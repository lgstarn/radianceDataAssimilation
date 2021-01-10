module netcdfDataArrayWriter_mod

    use iso_fortran_env
    use dataArrayWriter_mod
    use dataArray_mod
    use dataAttribute_mod
    use dataDimension_mod
    use dataVariable_mod
    use dataType_mod
    use dataShape_mod
    use dataGroup_mod
    use parallelConstants_mod
    use parallelInfo_mod
    use parallelDecomposition_mod
    use ncUtils_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    integer, parameter :: kindnum = MPI_OFFSET_KIND

    public :: NetcdfDataArrayWriter

    type, extends(DataArrayWriter) :: NetcdfDataArrayWriter
        contains
            procedure :: writeDimension
            procedure :: writeVariable

            procedure :: writeLocalVariable
            procedure :: writeMirroredVariable
            procedure :: writeDistributedVariable

            procedure :: getNetcdfType

            procedure :: netcdfDataArrayWriterConstructor
            final     :: netcdfDataArrayWriterDestructor
    end type

    contains

    subroutine netcdfDataArrayWriterConstructor(this,fileName)
        implicit none

        class(NetcdfDataArrayWriter)     :: this

        character(len=*),  intent(in) :: fileName

        call this%dataArrayWriterConstructor(fileName)
    end subroutine

    subroutine netcdfDataArrayWriterDestructor(this)
        implicit none

        type(NetcdfDataArrayWriter)  :: this

        call this%dataArrayWriterDestructor()
    end subroutine

    subroutine writeDimension(this,pinfo,ddim,locationInFile)

        use mpi
        use netcdf
        use pnetcdf

        implicit none

        class(NetcdfDataArrayWriter)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataDimension),       pointer     :: ddim
        character(len=*), optional, intent(in)  :: locationInFile

        integer :: fid
        integer :: dimid
        integer(kind=kindnum) :: dimlen
        integer :: rcode

        logical :: fileExists

        if (present(locationInFile)) then
            call debug('Now writing the dimension ' // trim(ddim%getName()) // ' to file ' // &
                & trim(this%getLocation()) // ' at location ' // trim(locationInFile))
        else
            call debug('Now writing the dimension ' // trim(ddim%getName()) // ' to file ' // &
                & trim(this%getLocation()))
        end if

        if (pinfo%getParallelType() == LOCAL_PARALLEL_TYPE) then
            inquire(file=this%getLocation(),exist=fileExists)
            if (fileExists) then
                rcode = nf90_open(this%getLocation(), NF_WRITE, fid)
                call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

                rcode = nf90_redef(fid)
                call ncCheck(rcode,'Putting the file ' // trim(this%getLocation()) // &
                    & ' back in redef mode')
            else
                rcode = nf90_create(this%getLocation(), NF_CLOBBER, fid)
                call ncCheck(rcode,'Creating the file ' // trim(this%getLocation()))
            end if

            if (present(locationInFile)) then
                rcode = nf90_def_dim(fid, locationInFile, ddim%getGlobalCount(), dimid)
                call ncCheck(rcode,'Defining the dimension ' // trim(locationInFile) // &
                    & ' in the file ' // trim(this%getLocation()))
            else
                rcode = nf90_def_dim(fid, ddim%getName(), ddim%getGlobalCount(), dimid)
                call ncCheck(rcode,'Defining the dimension ' // trim(ddim%getName()) // &
                    & ' in the file ' // trim(this%getLocation()))
            end if

            rcode = nf90_close(fid)
            call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
        else
            inquire(file=this%getLocation(),exist=fileExists)
            if (fileExists) then
                rcode = nfmpi_open(pinfo%getCommunicator(), this%getLocation(), &
                    nf_write, MPI_INFO_NULL, fid)
                call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

                rcode = nfmpi_redef(fid)
                call ncCheck(rcode,'Putting the file ' // trim(this%getLocation()) // &
                    & ' back in redef mode')
            else
                rcode = nfmpi_create(pinfo%getCommunicator(), this%getLocation(), &
                    nf_clobber, MPI_INFO_NULL, fid)
               call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))
            end if

            dimlen = ddim%getGlobalCount()

            if (present(locationInFile)) then
                rcode = nfmpi_def_dim(fid, locationInFile, dimlen, dimid)
                call ncCheck(rcode,'Defining the dimension ' // trim(locationInFile) // &
                    & ' in the file ' // trim(this%getLocation()))
            else
                rcode = nfmpi_def_dim(fid, ddim%getName(), dimlen, dimid)
                call ncCheck(rcode,'Defining the dimension ' // trim(ddim%getName()) // &
                    & ' in the file ' // trim(this%getLocation()))
            end if

            rcode = nfmpi_close(fid)
            call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
        end if
    end subroutine

    function getNetcdfType(this,dTypeNum) result(memType)
        use netcdf

        implicit none

        class(NetcdfDataArrayWriter) :: this

        integer,       intent(in) :: dTypeNum

        integer                   :: memType

        select case (dTypeNum)
            case (LOGICAL_TYPE_NUM)
                call error('NetCDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                memType = NF90_BYTE
            case (SHORT_TYPE_NUM)
                memType = NF90_SHORT
            case (INT_TYPE_NUM)
                memType = NF90_INT
            case (LONG_TYPE_NUM)
                memType = NF90_INT64
            case (REAL_TYPE_NUM)
                memType = NF90_FLOAT
            case (DOUBLE_TYPE_NUM)
                memType = NF90_DOUBLE
            case default
                memType = -999
        end select
    end function

    subroutine writeVariable(this,pinfo,var,locationInFile,writeDTypeNum)

        implicit none

        class(NetcdfDataArrayWriter)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataVariable),        pointer     :: var
        character(len=*), optional, intent(in)  :: locationInFile
        integer,          optional, intent(in)  :: writeDTypeNum

        call debug('Now writing variable ' // trim(var%getName()) // ' to file ' // &
            & trim(this%getLocation()))

        if (pinfo%getParallelType() == LOCAL_PARALLEL_TYPE) then
            call this%writeLocalVariable(var,locationInFile,writeDTypeNum)
        else if (pinfo%getParallelType() == MIRRORED_PARALLEL_TYPE) then
            call this%writeMirroredVariable(pinfo,var,locationInFile,writeDTypeNum)
        else if (pinfo%getParallelType() == DISTRIBUTED_PARALLEL_TYPE) then
            call this%writeDistributedVariable(pinfo,var,locationInFile,writeDTypeNum)
        else
            call error('Unknown parallel type ' // int2str(pinfo%getParallelType()))
        end if
    end subroutine

    subroutine writeLocalVariable(this,var,locationInFile,writeDTypeNum)
        use netcdf

        implicit none

        class(NetcdfDataArrayWriter)           :: this

        class(DataVariable),        pointer    :: var
        character(len=*), optional, intent(in) :: locationInFile
        integer,          optional, intent(in) :: writeDTypeNum

        class(DataArray),          pointer :: dArray
        class(DataShape),          pointer :: dShape
        class(DataDimension),      pointer :: ddim

        integer :: rcode
        integer :: fid
        integer :: varid
        integer :: xtype
        integer :: i

        logical :: fileExists

        integer :: dtypeToWrite

        integer, allocatable :: dimids(:)

        integer(int8),  pointer :: bptr(:)
        integer(int16), pointer :: sptr(:)
        integer(int32), pointer :: iptr(:)
        integer(int64), pointer :: lptr(:)
        real(real32),   pointer :: rptr(:)
        real(real64),   pointer :: dptr(:)

        inquire(file=this%getLocation(),exist=fileExists)
        if (fileExists) then
            rcode = nf90_open(this%getLocation(), NF90_WRITE, fid)
            call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

            rcode = nf90_redef(fid)
            call ncCheck(rcode,'Putting the file ' // trim(this%getLocation()) // &
                & ' back in redef mode')
        else
            rcode = nf90_create(this%getLocation(), NF90_CLOBBER, fid)
            call ncCheck(rcode,'Creating the file ' // trim(this%getLocation()))
        end if

        dShape => var%getDataShape()

        allocate(dimids(dShape%getNDimensions()))

        do i=1,dShape%getNDimensions()
            ddim => dShape%getDimensionNumber(i)
            rcode = nf90_inq_dimid(fid,ddim%getName(),dimids(i))
            call ncCheck(rcode,'Inquiring about the dimension ' // trim(ddim%getName()) // &
                & ' in the file ' // trim(this%getLocation()))
        end do

        dArray => var%getDataArray()
        xtype = this%getNetcdfType(dArray%getDataTypeNum())

        if (present(locationInFile)) then
            rcode = nf90_def_var(fid,locationInFile,xtype,dimids,varid)
            call ncCheck(rcode,'Defining the variable ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
        else
            rcode = nf90_def_var(fid,var%getName(),xtype,dimids,varid)
            call ncCheck(rcode,'Defining the variable ' // trim(var%getName()) // &
                & ' in the file ' // trim(this%getLocation()))
        end if

        rcode = nf90_enddef(fid)
        call ncCheck(rcode,'Taking the file ' // trim(this%getLocation()) // &
            & ' out of definition mode')

        if (present(writeDTypeNum)) then
            dtypeToWrite = writeDTypeNum
        else
            dtypeToWrite = dArray%getDataTypeNum()
        end if

        select case (dtypeToWrite)
            case (LOGICAL_TYPE_NUM)

                call error('NetCDF does not have a logical type at the current time.')

            case (BYTE_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    bptr => dArray%getDataPointer_byte()
                else
                    allocate(bptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(bptr)
                end if

                rcode = nf90_put_var(fid, varid, bptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local byte variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(bptr)
                end if

            case (SHORT_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    sptr => dArray%getDataPointer_short()
                else
                    allocate(sptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(sptr)
                end if

                rcode = nf90_put_var(fid, varid, sptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local short variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(sptr)
                end if

            case (INT_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    iptr => dArray%getDataPointer_int()
                else
                    allocate(iptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(iptr)
                end if

                rcode = nf90_put_var(fid, varid, iptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local int variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(iptr)
                end if

            case (LONG_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    lptr => dArray%getDataPointer_long()
                else
                    allocate(lptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(lptr)
                end if

                rcode = nf90_put_var(fid, varid, lptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local long variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(lptr)
                end if

            case (REAL_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    rptr => dArray%getDataPointer_real()
                else
                    allocate(rptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(rptr)
                end if

                rcode = nf90_put_var(fid, varid, rptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local real variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(rptr)
                end if

            case (DOUBLE_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    dptr => dArray%getDataPointer_double()
                else
                    allocate(dptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(dptr)
                end if

                rcode = nf90_put_var(fid, varid, dptr, &
                    & dShape%getGlobalStarts(), dShape%getGlobalCounts())

                call ncCheck(rcode,'Writing the local double variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(dptr)
                end if

            case default
                call error('Unknown data type ' // int2str(dArray%getDataTypeNum()))
        end select

        rcode = nf90_close(fid)
        call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
    end subroutine

    subroutine writeMirroredVariable(this,pinfo,var,locationInFile,writeDTypeNum)

        implicit none

        class(NetcdfDataArrayWriter)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataVariable),        pointer     :: var
        character(len=*), optional, intent(in)  :: locationInFile
        integer,          optional, intent(in)  :: writeDTypeNum

        ! read the data from the given location.
        ! note: currently only the root writes the data, leaving all of the processors
        ! sitting around doing nothing even though they have the data.
        ! it might be better (on parallel file systems) to split the writes across all
        ! members
        ! ideally, this would be an option the user could set through pinfo
        if (pinfo%getRank() == 0) then
            call this%writeLocalVariable(var,locationInFile,writeDTypeNum)
        end if
    end subroutine

    subroutine writeDistributedVariable(this,pinfo,var,locationInFile,writeDTypeNum)

        use pnetcdf
        use mpi

        implicit none

        class(NetcdfDataArrayWriter)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataVariable),        pointer     :: var
        character(len=*), optional, intent(in)  :: locationInFile
        integer,          optional, intent(in)  :: writeDTypeNum

        class(DataArray),           pointer :: dArray
        class(DataShape),           pointer :: dShape
        class(DataDimension),       pointer :: ddim

        integer :: rcode
        integer :: fid
        integer :: varid
        integer :: xtype
        integer :: i

        logical :: fileExists

        integer :: dtypeToWrite

        integer, allocatable :: dimids(:)

        integer(kindnum), allocatable :: data_count(:)
        integer(kindnum), allocatable :: data_offset(:)

        integer(int8),  pointer :: bptr(:)
        integer(int16), pointer :: sptr(:)
        integer(int32), pointer :: iptr(:)
        integer(int64), pointer :: lptr(:)
        real(real32),   pointer :: rptr(:)
        real(real64),   pointer :: dptr(:)

        inquire(file=this%getLocation(),exist=fileExists)
        if (fileExists) then
            rcode = nfmpi_open(pinfo%getCommunicator(), this%getLocation(), &
                nf_write, MPI_INFO_NULL, fid)
           call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

            rcode = nfmpi_redef(fid)
            call ncCheck(rcode,'Putting the file ' // trim(this%getLocation()) // &
                & ' back in redef mode')
        else
            rcode = nfmpi_create(pinfo%getCommunicator(), this%getLocation(), &
                nf_clobber, MPI_INFO_NULL, fid)
           call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))
        end if

        dShape => var%getDataShape()

        allocate(dimids(dShape%getNDimensions()))

        do i=1,dShape%getNDimensions()
            ddim => dShape%getDimensionNumber(i)
            rcode = nfmpi_inq_dimid(fid,ddim%getName(),dimids(i))
            call ncCheck(rcode,'Inquiring about the dimension ' // trim(ddim%getName()) // &
                & ' in the file ' // trim(this%getLocation()))
        end do

        dArray => var%getDataArray()
        xtype = this%getNetcdfType(dArray%getDataTypeNum())

        if (present(locationInFile)) then
            rcode = nfmpi_def_var(fid,locationInFile,xtype,size(dimids),dimids,varid)
            call ncCheck(rcode,'Defining the variable ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
        else
            rcode = nfmpi_def_var(fid,var%getName(),xtype,size(dimids),dimids,varid)
            call ncCheck(rcode,'Defining the variable ' // trim(var%getName()) // &
                & ' in the file ' // trim(this%getLocation()))
        end if

        rcode = nfmpi_enddef(fid)
        call ncCheck(rcode,'Taking the file ' // trim(this%getLocation()) // &
            & ' out of definition mode')

        data_offset = dShape%getLocalStarts()
        data_count  = dShape%getLocalCounts()

        if (present(writeDTypeNum)) then
            dtypeToWrite = writeDTypeNum
        else
            dtypeToWrite = dArray%getDataTypeNum()
        end if

        select case (dtypeToWrite)
            case (LOGICAL_TYPE_NUM)

                call error('NetCDF does not have a logical type at the current time.')

            case (BYTE_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    bptr => dArray%getDataPointer_byte()
                else
                    allocate(bptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(bptr)
                end if

                rcode = nfmpi_put_vara_int1_all(fid, varid, data_offset, &
                    & data_count, bptr)

                call ncCheck(rcode,'Writing the distributed byte variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(bptr)
                end if

            case (SHORT_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    sptr => dArray%getDataPointer_short()
                else
                    allocate(sptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(sptr)
                end if

                rcode = nfmpi_put_vara_int2_all(fid, varid, data_offset, &
                    & data_count, sptr)

                call ncCheck(rcode,'Writing the distributed short variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(sptr)
                end if

            case (INT_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    iptr => dArray%getDataPointer_int()
                else
                    allocate(iptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(iptr)
                end if

                rcode = nfmpi_put_vara_int_all(fid, varid, data_offset, &
                    & data_count, iptr)

                call ncCheck(rcode,'Writing the distributed int variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(iptr)
                end if

            case (LONG_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    lptr => dArray%getDataPointer_long()
                else
                    allocate(lptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(lptr)
                end if

                rcode = nfmpi_put_vara_int8_all(fid, varid, data_offset, &
                    & data_count, lptr)

                call ncCheck(rcode,'Writing the distributed long variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(lptr)
                end if

            case (REAL_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    rptr => dArray%getDataPointer_real()
                else
                    allocate(rptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(rptr)
                end if

                rcode = nfmpi_put_vara_real_all(ncid=fid, varid=varid, start=data_offset, &
                    & count=data_count, rvals=rptr)

                call ncCheck(rcode,'Writing the distributed real variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(rptr)
                end if

            case (DOUBLE_TYPE_NUM)

                if (dtypeToWrite == dArray%getDataTypeNum()) then
                    dptr => dArray%getDataPointer_double()
                else
                    allocate(dptr(dArray%getLocalTotalSize()))
                    call dArray%copyTo(dptr)
                end if

                rcode = nfmpi_put_vara_double_all(fid, varid, data_offset, &
                    & data_count, dptr)

                call ncCheck(rcode,'Writing the distributed double variable ' //      &
                    & trim(var%getName()) // ' to the file ' // trim(this%getLocation()))

                if (dtypeToWrite /= dArray%getDataTypeNum()) then
                    deallocate(dptr)
                end if

            case default
                call error('Unknown data type ' // int2str(dArray%getDataTypeNum()))
        end select

        rcode = nfmpi_close(fid)
        call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))


    end subroutine

end module
