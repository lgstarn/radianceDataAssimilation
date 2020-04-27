module netcdfDataArrayReader_mod
    use iso_fortran_env
    use dataArrayReader_mod
    use dataArray_mod
    use dataAttribute_mod
    use dataType_mod
    use dataShape_mod
    use dataGroup_mod
    use parallelConstants_mod
    use parallelInfo_mod
    use parallelDecomposition_mod
    use ncUtils_mod
    use asciiUtils_mod
    use mpiUtils_mod

    use mpi

    implicit none

    private

    integer, parameter :: kindnum = MPI_OFFSET_KIND

    public :: NetcdfDataArrayReader

    type, extends(DataArrayReader) :: NetcdfDataArrayReader
        contains
            procedure :: loadDimSizeFromVariable   => loadDimSizeFromVariable_netcdf
            procedure :: loadDataArray             => loadDataArray_netcdf
            procedure :: loadAttribute             => loadAttribute_netcdf

            procedure :: loadLocalArray
            procedure :: loadDistributedArray
            procedure :: loadMirroredArray

            procedure :: clone

            procedure :: netcdfDataArrayReaderConstructor
            final     :: netcdfDataArrayReaderDestructor
    end type

    contains

    subroutine netcdfDataArrayReaderConstructor(this,fileName)
        implicit none

        class(NetcdfDataArrayReader)     :: this

        character(len=*),  intent(in) :: fileName

        call this%dataArrayReaderConstructor(fileName)
    end subroutine

    subroutine netcdfDataArrayReaderDestructor(this)
        implicit none

        type(NetcdfDataArrayReader)  :: this

        call this%dataArrayReaderDestructor()
    end subroutine

    function loadDimSizeFromVariable_netcdf(this,pinfo,locationInFile,dimNum) result(dimn)

        use mpi
        use netcdf
        use pnetcdf

        implicit none

        class(NetcdfDataArrayReader)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        character(len=*),           intent(in)  :: locationInFile
        integer,                    intent(in)  :: dimNum

        integer                                 :: dimn
        integer(kind=kindnum)                   :: dimn_kind

        integer :: fid
        integer, allocatable :: dids(:)
        integer :: ndims
        integer :: ncerr
        integer :: vid, did, rcode

        ! assume initParallel() has already been called
        if (pinfo%getParallelType() == LOCAL_PARALLEL_TYPE) then
            rcode = nf90_open(this%getLocation(), nf_nowrite, fid)
            call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

            rcode = nf90_inq_varid(fid, locationInFile, vid)
            call ncCheck(rcode,'Reading the variable id for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            rcode = nf90_inquire_variable(fid, vid, ndims=ndims)
            call ncCheck(rcode,'Reading the number of dims for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            allocate(dids(ndims))

            rcode = nf90_inquire_variable(fid, vid, dimids=dids)
            call ncCheck(rcode,'Reading the dimension IDs for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            if (dimNum < 1 .or. dimNum > ndims) then
                call error('Invalid dimension number for ' // trim(locationInFile) // &
                    & ' in the file ' // trim(this%getLocation()) // ' - dim #' // &
                    & int2str(dimNum) // '/' // int2str(ndims))
            end if

            rcode = nf90_inquire_dimension(fid, dids(dimNum), len=dimn)
            call ncCheck(rcode,'Reading the dimension length for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
            ! dimn = int(dimn_kind)

            if (rcode /= nf_noerr) then
                dimn = -1
            endif

            rcode = nf90_close(fid)
            call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
        else
            rcode = nfmpi_open(pinfo%getCommunicator(), this%getLocation(), &
                nf_nowrite, MPI_INFO_NULL, fid)
            call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

            rcode = nfmpi_inq_varid(fid, locationInFile, vid)
            call ncCheck(rcode,'Reading the variable id for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            rcode = nfmpi_inq_varndims(fid, vid, ndims)
            call ncCheck(rcode,'Reading the number of dims for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            allocate(dids(ndims))

            rcode = nfmpi_inq_vardimid(fid, vid, dids)
            call ncCheck(rcode,'Reading the dimension IDs for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))

            if (dimNum < 1 .or. dimNum > ndims) then
                call error('Invalid dimension number for ' // trim(locationInFile) // &
                    & ' in the file ' // trim(this%getLocation()) // ' - dim #' // &
                    & int2str(dimNum) // '/' // int2str(ndims))
            end if

            rcode = nfmpi_inq_dimlen(fid, dids(dimNum), dimn_kind)
            call ncCheck(rcode,'Reading the dimension length for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
            dimn = int(dimn_kind)

            if (rcode /= nf_noerr) then
                dimn = -1
            endif

            rcode = nfmpi_close(fid)
            call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
        end if
    end function

    subroutine loadAttribute_netcdf(this,pinfo,attr,aname,locationInFile,required)
        use mpi
        use pnetcdf

        implicit none

        class(NetcdfDataArrayReader)        :: this

        class(ParallelInfo),        pointer    :: pinfo
        class(DataAttribute),       pointer    :: attr
        character(len=*),           intent(in) :: aname
        character(len=*), optional, intent(in) :: locationInFile
        logical,          optional, intent(in) :: required

        character(:), pointer :: attval

        integer(int8)  :: dbytes(1)
        integer(int16) :: dshorts(1)
        integer(int32) :: dints(1)
        integer(int64) :: dlongs(1)
        real(real32)   :: dreals(1)
        real(real64)   :: ddbles(1)

        integer :: fid
        integer :: attlen
        integer :: rcode
        integer(kind=kindnum) :: attlen_kind

        character(len=1), dimension(:), pointer :: data ! A pointer to a Fortran string

        integer :: memType

        logical :: isRequired

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        rcode = nfmpi_open(pinfo%getCommunicator(), this%getLocation(), &
            nf_nowrite, MPI_INFO_NULL, fid)
        call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

        rcode = nfmpi_inq_attlen(fid, nf_global, attr%getName(), attlen_kind)

        if (isRequired) then
            call ncCheck(rcode,'Reading the required attribute length for ' // trim(attr%getName()))
        end if

        ! if not required and not found, just return without doing anything

        if (rcode == nf_noerr) then
            attlen = int(attlen_kind)

            if (attr%getDataTypeNum() /= STRINGS_TYPE_NUM .and. &
                attr%getDataTypeNum() /= STRING_TYPE_NUM) then

                if (attlen /= 1) then
                    call error('Could not load a non-string attribute of length larger than 1: ' // &
                        & attr%getName() // ' / ' // int2str(attlen))
                endif
            end if

            select case (attr%getDataTypeNum())
                case(STRINGS_TYPE_NUM : STRING_TYPE_NUM)

                    allocate(character(len=attlen) :: attval)
                    rcode = nfmpi_get_att_text(fid, nf_global, attr%getName(), attval)
                    call ncCheck(rcode,'Reading the text attribute ' // trim(attr%getName()))
                    call attr%addString(attval)

                case (LOGICAL_TYPE_NUM)

                    call error('NetCDF does not have a logical type at this time')

                case (BYTE_TYPE_NUM)

                    rcode = nfmpi_get_att_int1(fid, nf_global, attr%getName(), dbytes)
                    call ncCheck(rcode,'Reading the byte attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(dbytes(1))

                case (SHORT_TYPE_NUM)

                    rcode = nfmpi_get_att_int2(fid, nf_global, attr%getName(), dshorts)
                    call ncCheck(rcode,'Reading the short attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(dshorts(1))

                case (INT_TYPE_NUM)

                    rcode = nfmpi_get_att_int(fid, nf_global, attr%getName(), dints)
                    call ncCheck(rcode,'Reading the int attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(dints(1))

                case (LONG_TYPE_NUM)

                    rcode = nfmpi_get_att_int8(fid, nf_global, attr%getName(), dlongs)
                    call ncCheck(rcode,'Reading the long attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(dlongs(1))

                case (REAL_TYPE_NUM)

                    rcode = nfmpi_get_att_real(fid, nf_global, attr%getName(), dreals)
                    call ncCheck(rcode,'Reading the real attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(dreals(1))

                case (DOUBLE_TYPE_NUM)

                    rcode = nfmpi_get_att_double(fid, nf_global, attr%getName(), ddbles)
                    call ncCheck(rcode,'Reading the double attribute ' // trim(attr%getName()))
                    call attr%setNumericValue(ddbles(1))

                case default
                    call error('Unknown attribute data type for netcdf data array reader: ' // &
                        attr%getName() // ' / ' // int2str(attr%getDataTypeNum()))
            end select
        end if

        rcode = nfmpi_close(fid)
        call ncCheck(rcode,'Closing the file ' // trim(this%getLocation()))
     end subroutine

    subroutine loadDataArray_netcdf(this,pinfo,dArray,locationInFile,required)

        implicit none

        class(NetcdfDataArrayReader)               :: this

        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        class(ParallelInfo),        pointer     :: pinfo
        logical,          optional, intent(in)  :: required

        if (pinfo%getParallelType() == LOCAL_PARALLEL_TYPE) then
            call this%loadLocalArray(dArray,locationInFile,required)
        else if (pinfo%getParallelType() == MIRRORED_PARALLEL_TYPE) then
            call this%loadMirroredArray(pinfo,dArray,locationInFile,required)
        else if (pinfo%getParallelType() == DISTRIBUTED_PARALLEL_TYPE) then
            call this%loadDistributedArray(pinfo,dArray,locationInFile,required)
        else
            call error('Unknown parallel type: ' // int2str(pinfo%getParallelType()))
        end if
    end subroutine

    subroutine loadLocalArray(this,dArray,locationInFile,required)
        use netcdf

        implicit none

        class(NetcdfDataArrayReader)               :: this

        class(DataArray),              pointer     :: dArray
        character(len=*),              intent(in)  :: locationInFile
        logical,             optional, intent(in)  :: required

        integer :: hdferr

        class(DataShape), pointer :: dShape

        integer :: fid
        integer :: rcode
        integer :: vid

        logical :: isRequired

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        dShape => dArray%getDataShape()

        rcode = nf90_open(this%getLocation(), nf90_nowrite, fid)
        call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

        rcode = nf90_inq_varid(fid, locationInFile, vid)

        if (isRequired) then
             call ncCheck(rcode,'Reading the required variable id for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
        end if

        if (rcode == nf90_noerr) then
            ! load the data according to type
            select case (dArray%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    call error('NetCDF does not have a logical type at the current time.')
                case (BYTE_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_byte(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local byte variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
                case (SHORT_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_short(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local short variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
                case (INT_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_int(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local int variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
                case (LONG_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_long(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local long variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
                case (REAL_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_real(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local real variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
                case (DOUBLE_TYPE_NUM)
                    rcode = nf90_get_var(fid, vid, dArray%getDataPointer_double(), &
                        dShape%getGlobalStarts(), dShape%getGlobalCounts())
                    call ncCheck(rcode,'Reading local double variable data for ' // &
                        & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            end select

            call dArray%setLoaded(.true.)
        end if
    end subroutine

    subroutine loadMirroredArray(this,pinfo,dArray,locationInFile,required)
        use hdf5

        implicit none

        class(NetcdfDataArrayReader)               :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        logical,          optional, intent(in)  :: required

        integer :: comm

        logical :: isRequired

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        ! assume initParallel() has already been called

        ! read the data from the given location.
        ! note: currently only the root reads the data, then broadcasts it to all members
        ! it might be better (on parallel file systems) to split the reads across all members
        ! then do an allgather
        ! ideally, this would be an option the user could set through pinfo
        if (pinfo%getRank() == 0) then
            call this%loadLocalArray(dArray,locationInFile)
        end if

        if (dArray%isLoaded()) then
            comm = pinfo%getCommunicator()

            ! now broadcast the data from the root to all members
            select case(dArray%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    call error('NetCDF does not have a logical type at the current time.')
                case (BYTE_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_byte(),   dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (byte): '   // locationInFile)
                case (SHORT_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_short(),  dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (short): '  // locationInFile)
                case (INT_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_int(),    dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (int): '    // locationInFile)
                case (LONG_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_long(),   dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (long): '   // locationInFile)
                case (REAL_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_real(),   dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (real): '   // locationInFile)
                case (DOUBLE_TYPE_NUM)
                    call bcast1d(dArray%getDataPointer_double(), dArray%getLocalTotalSize(), 0, comm, &
                        'NetCDF load mirrored array (double): ' // locationInFile)
            end select
        end if
    end subroutine

    subroutine loadDistributedArray(this,pinfo,dArray,locationInFile,required)

        use mpi
        use pnetcdf

        implicit none

        class(NetcdfDataArrayReader)            :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        logical,          optional, intent(in)  :: required

        integer(kindnum), allocatable :: data_count(:)
        integer(kindnum), allocatable :: data_offset(:)

        integer :: fid
        integer :: attlen
        integer :: rcode
        integer :: vid
        integer(kind=kindnum) :: attlen_kind

        character(len=1), dimension(:), pointer :: data ! A pointer to a Fortran string

        integer :: memType

        class(DataShape), pointer :: dShape

        logical :: isRequired

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        rcode = nfmpi_open(pinfo%getCommunicator(), this%getLocation(), &
            nf_nowrite, MPI_INFO_NULL, fid)
        call ncCheck(rcode,'Opening the file ' // trim(this%getLocation()))

        rcode = nfmpi_inq_varid(fid, locationInFile, vid)

        if (isRequired) then
            call ncCheck(rcode,'Reading the variable id for ' // trim(locationInFile) // &
                & ' in the file ' // trim(this%getLocation()))
        elseif (rcode < 0) then
            return
        end if

        dShape => dArray%getDataShape()

        data_offset = dShape%getLocalStarts()
        data_count = dShape%getLocalCounts()

        ! load the data in parallel according to type
        select case (dArray%getDataTypeNum())
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                rcode = nfmpi_get_vara_int1_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_byte())

                call ncCheck(rcode,'Reading MPI byte variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case (SHORT_TYPE_NUM)
                rcode = nfmpi_get_vara_int2_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_short())

                call ncCheck(rcode,'Reading MPI short variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case (INT_TYPE_NUM)
                rcode = nfmpi_get_vara_int_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_int())

                call ncCheck(rcode,'Reading MPI int variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case (LONG_TYPE_NUM)
                rcode = nfmpi_get_vara_int8_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_long())

                call ncCheck(rcode,'Reading MPI long variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case (REAL_TYPE_NUM)
                rcode = nfmpi_get_vara_real_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_real())

                call ncCheck(rcode,'Reading MPI real variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case (DOUBLE_TYPE_NUM)
                rcode = nfmpi_get_vara_double_all(fid, vid, data_offset, &
                    data_count, dArray%getDataPointer_double())

                call ncCheck(rcode,'Reading MPI real variable data for ' // &
                    & trim(locationInFile) // ' in the file ' // trim(this%getLocation()))
            case default
                call error('In load distributed array, unknown data type: ' // &
                    int2str(dArray%getDataTypeNum()))
        end select

        call dArray%setLoaded(.true.)
    end subroutine

    function clone(this) result(newReader)
        implicit none

        class(NetcdfDataArrayReader)          :: this

        class(DataArrayReader),       pointer :: newReader

        class(NetcdfDataArrayReader), pointer :: ndarptr

        allocate(ndarptr)
        call ndarptr%netcdfDataArrayReaderConstructor(this%getLocation())

        newReader => ndarptr
    end function

end module
