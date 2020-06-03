module hdfDataArrayReader_mod

    use parallelInfo_mod
    use parallelConstants_mod
    use parallelDecomposition_mod

    use iso_fortran_env

    use dataType_mod
    use dataArray_mod
    use dataShape_mod
    use dataGroup_mod
    use dataAttribute_mod
    use dataArrayReader_mod

    use mpiUtils_mod
    use hdfUtils_mod
    use asciiUtils_mod

    implicit none

    private

    public :: HdfDataArrayReader

    type, extends(DataArrayReader) :: HdfDataArrayReader
        logical :: littleEndian

        contains
            procedure :: isLittleEndian

            procedure :: loadDimSizeFromVariable   => loadDimSizeFromVariable_hdf
            procedure :: loadDataArray             => loadDataArray_hdf
            procedure :: loadAttribute             => loadAttribute_hdf

            procedure :: loadLocalArray
            procedure :: loadDistributedArray
            procedure :: loadMirroredArray

            procedure :: clone

            procedure, private :: getHdfType

            procedure :: hdfDataArrayReaderConstructor
            final     :: hdfDataArrayReaderDestructor
    end type

    contains

    subroutine hdfDataArrayReaderConstructor(this,fileName,littleEndian)
        implicit none

        class(HdfDataArrayReader)     :: this

        character(len=*),  intent(in) :: fileName
        logical,           intent(in) :: littleEndian

        call this%dataArrayReaderConstructor(fileName)
        this%littleEndian = littleEndian
    end subroutine

    subroutine hdfDataArrayReaderDestructor(this)
        implicit none

        type(HdfDataArrayReader)  :: this

        call this%dataArrayReaderDestructor()
    end subroutine

    function isLittleEndian(this) result(littleEndian)
        implicit none

        class(HdfDataArrayReader)                          :: this

        logical :: littleEndian

        littleEndian = this%littleEndian
    end function

    function loadDimSizeFromVariable_hdf(this,pinfo,locationInFile,dimNum,required) result(dimn)

        use hdf5

        implicit none

        class(HdfDataArrayReader)               :: this

        class(ParallelInfo),        pointer     :: pinfo
        character(len=*),           intent(in)  :: locationInFile
        integer,                    intent(in)  :: dimNum
        logical,         optional,  intent(in)  :: required

        integer                                 :: dimn

        integer :: ndims
        integer :: hdferr

        integer(HSIZE_T), allocatable :: dims(:), maxdims(:)

        integer(HID_T) :: plist_id
        integer(HID_T) :: file_id
        integer(HID_T) :: dspace_id
        integer(HID_T) :: dset_id

        logical :: isRequired

        integer :: commToUse

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        commToUse = pinfo%getCommunicator()

        ! assume initParallel() has already been called

        if (pinfo%getRank() == 0) then
            ! initialize hdf if necessary
            call h5open_f(hdferr); call h5check(hdferr,'h5open')

            ! create a new property list for parallel file access
            call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
            call h5check(hdferr,'h5pcreate')

            ! open the file with the given access group
            call h5fopen_f(this%getLocation(), H5F_ACC_RDONLY_F, file_id, hdferr)
            call h5check(hdferr,'h5fopen')

            ! open the data set
            call h5dopen_f(file_id, locationInFile, dset_id, hdferr); call h5check(hdferr,'h5dopen')

            ! open the dataspace for the data set
            call h5dget_space_f(dset_id,dspace_id,hdferr)

            if (hdferr /= 0) then
                if (isRequired) then
                    call h5check(hdferr,'h5dget_space')
                else
                    dimn = -1
                    return
                end if
            end if

            ! get the number of dimensions for the data space
            call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr)

            if (hdferr /= 0) then
                if (isRequired) then
                    call h5check(hdferr,'h5sget_simple_extent_ndims')
                else
                    dimn = -1
                    return
                end if
            end if

            ! allocate a place to store the dimensions
            allocate(dims(ndims),maxdims(ndims))

            ! get the actual dimensions
            call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr)

            if (hdferr /= 0) then
                if (isRequired) then
                    call h5check(hdferr,'h5sget_simple_extent_dims_f')
                else
                    dimn = -1
                    return
                end if
            end if

            if (dimNum < 1 .or. dimNum > ndims) then
                if (isRequired) then
                    call error('Illegal dimension number: ' // int2str(dimNum) // '(ndims: ' // &
                        int2str(ndims) // ')')
                else
                    dimn = -1
                    return
                end if
            end if

            dimn = dims(dimNum)
        end if

        if (pinfo%getParallelType() > LOCAL_PARALLEL_TYPE) then
            call bcast0d(dimn,0,commToUse,'dimension ' // locationInFile)
        end if
    end function

    subroutine loadAttribute_hdf(this,pinfo,attr,aname,locationInFile,required)
        use hdf5

        implicit none

        class(HdfDataArrayReader)        :: this

        class(ParallelInfo),        pointer    :: pinfo
        class(DataAttribute),       pointer    :: attr
        character(len=*),           intent(in) :: aname
        character(len=*), optional, intent(in) :: locationInFile
        logical,          optional, intent(in) :: required

        type(c_ptr), dimension(:), allocatable, target :: rdata

        integer(hid_t) :: size_val, file, dset_id, attr_id, space_id, type_id
        integer(int32) :: ndims

        integer(hsize_t), dimension(1:1) :: dims = (/1/)
        integer(hsize_t), dimension(1:1) :: maxdims
        integer(hid_t) :: atype

        integer(int8)  :: dbyte
        integer(int16) :: dshort
        integer(int32) :: dint
        integer(int64) :: dlong
        real(real32)   :: dreal
        real(real64)   :: ddble

        character(:), pointer :: bufr

        integer :: hdferr, i, j, length

        integer(size_t) :: isize

        integer(hsize_t) :: dims_i8(1)

        type(c_ptr) :: f_ptr

        logical :: isRequired

        ! character(len=maxLen, kind=c_char), pointer :: data ! A pointer to a Fortran string
        character(len=1), pointer :: data(:) ! A pointer to a Fortran string

        integer(hid_t) :: memType

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        ! Initialize fortran interface.
        call h5open_f(hdferr); call h5check(hdferr)

        ! Open file and attribute.
        CALL h5fopen_f(this%getLocation(), H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        if (present(locationInFile)) then
            call h5dopen_f(file, locationInFile, dset_id, hdferr); call h5check(hdferr)
        else
            dset_id = file
        end if

        call h5aopen_f(dset_id, aname, attr_id, hdferr)

        if (hdferr < 0 .and. isRequired) then
            call h5check(hdferr)
        else if (hdferr < 0) then
            return
        end if

        call h5aget_space_f(attr_id, space_id, hdferr); call h5check(hdferr)

        memType = this%getHdfType(attr%getDataTypeNum())

        dims_i8 = 1

        select case (attr%getDataTypeNum())
            case(STRING_TYPE_NUM : STRINGS_TYPE_NUM)

                call h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr)
                call h5check(hdferr,'h5sget_simple_extent_dims string')

                allocate(rdata(1:dims(1)))

                ! Read the data.
                f_ptr = C_LOC(rdata(1))
                call h5aread_f(attr_id, H5T_STRING, f_ptr, hdferr)
                call h5check(hdferr,'h5aread string')

                ! Put the variable data in bufr according to the f08 standard
                do i = 1, dims(1)
                    call c_f_pointer(rdata(i), data, (/attr%getMaxStringLength()/))
                    length = 0

                    do while(data(length+1) .ne. c_null_char)
                        length = length + 1
                    enddo

                    allocate(character(len=length) :: bufr)

                    do j=1, length
                        bufr(j:j) = data(j)
                    end do

                    call attr%addString(bufr)
                end do
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                call h5aread_f(attr_id, memType, dbyte,  dims_i8, hdferr)
                call h5check(hdferr,'h5aread byte')
                call attr%setNumericValue(dbyte)
            case (SHORT_TYPE_NUM)
                call h5aread_f(attr_id, memType, dshort, dims_i8, hdferr)
                call h5check(hdferr,'h5aread short')
                call attr%setNumericValue(dshort)
            case (INT_TYPE_NUM)
                call h5aread_f(attr_id, memType, dint,   dims_i8, hdferr)
                call h5check(hdferr,'h5aread int')
                call attr%setNumericValue(dint)
            case (LONG_TYPE_NUM)
                call h5aread_f(attr_id, memType, dlong,  dims_i8, hdferr)
                call h5check(hdferr,'h5aread long')
                call attr%setNumericValue(dlong)
            case (REAL_TYPE_NUM)
                call h5aread_f(attr_id, memType, dreal,  dims_i8, hdferr)
                call h5check(hdferr,'h5aread real')
                call attr%setNumericValue(dreal)
            case (DOUBLE_TYPE_NUM)
                call h5aread_f(attr_id, memType, ddble,  dims_i8, hdferr)
                call h5check(hdferr,'h5aread double')
                call attr%setNumericValue(ddble)
        end select

        call h5aclose_f(attr_id , hdferr); call h5check(hdferr)

        call h5sclose_f(space_id, hdferr); call h5check(hdferr)

        if (present(locationInFile)) then
            call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        end if

        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine loadDataArray_hdf(this,pinfo,dArray,locationInFile,required,loadDTypeNum)

        implicit none

        class(HdfDataArrayReader)               :: this

        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        class(ParallelInfo),        pointer     :: pinfo
        logical,          optional, intent(in)  :: required
        integer,          optional, intent(in)  :: loadDTypeNum

        if (pinfo%getParallelType() == LOCAL_PARALLEL_TYPE) then
            call this%loadLocalArray(dArray,locationInFile,required,loadDTypeNum)
        else if (pinfo%getParallelType() == MIRRORED_PARALLEL_TYPE) then
            call this%loadMirroredArray(pinfo,dArray,locationInFile,required,loadDTypeNum)
        else if (pinfo%getParallelType() == DISTRIBUTED_PARALLEL_TYPE) then
            call this%loadDistributedArray(pinfo,dArray,locationInFile,required,loadDTypeNum)
        else
            call error('Unknown parallel type: ' // int2str(pinfo%getParallelType()))
        end if
    end subroutine

    function getHdfType(this,dTypeNum) result(memType)
        use hdf5

        implicit none

        class(HdfDataArrayReader) :: this

        integer,       intent(in) :: dTypeNum

        integer(hid_t)            :: memType

        select case (dTypeNum)
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I8LE
                else
                    memType = H5T_STD_I8BE
                end if
            case (SHORT_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I16LE
                else
                    memType = H5T_STD_I16BE
                end if
            case (INT_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I32LE
                else
                    memType = H5T_STD_I32BE
                end if
            case (LONG_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I64LE
                else
                    memType = H5T_STD_I64BE
                end if
            case (REAL_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_IEEE_F32LE
                else
                    memType = H5T_IEEE_F32BE
                end if
            case (DOUBLE_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_IEEE_F64LE
                else
                    memType = H5T_IEEE_F64BE
                end if
            case default
                memType = -999
        end select
    end function

    subroutine loadLocalArray(this,dArray,locationInFile,required,loadDTypeNum)
        use hdf5

        implicit none

        class(HdfDataArrayReader)                  :: this

        class(DataArray),              pointer     :: dArray
        character(len=*),              intent(in)  :: locationInFile
        logical,             optional, intent(in)  :: required
        integer,             optional, intent(in)  :: loadDTypeNum
!        class(ParallelInfo), optional, pointer     :: pinfo

        integer :: hdferr

        integer(HID_T) :: file_id
        integer(HID_T) :: dset_id

        class(DataShape), pointer :: dShape

        integer(hid_t) :: memType

        integer,          allocatable :: dims(:)
        integer(hsize_t), allocatable :: dims_i8(:)

        integer(int8),  pointer :: bptr(:)
        integer(int16), pointer :: sptr(:)
        integer(int32), pointer :: iptr(:)
        integer(int64), pointer :: lptr(:)
        real(real32),   pointer :: rptr(:)
        real(real64),   pointer :: dptr(:)

        logical :: isRequired

        integer :: dtypeToLoad

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        dShape => dArray%getDataShape()

        ! initialize hdf if necessary
        call h5open_f(hdferr); call h5check(hdferr,'h5open')

        ! open the file with the given access group
        call h5fopen_f(this%getLocation(), H5F_ACC_RDONLY_F, file_id, hdferr)
        call h5check(hdferr,'h5fopen')

        ! open the data set
        call h5dopen_f(file_id, locationInFile, dset_id, hdferr)

        if (hdferr < 0 .and. isRequired) then
            call h5check(hdferr,'h5dopen')
        else if (hdferr < 0) then
            return
        end if

        memType = this%getHdfType(dArray%getDataTypeNum())

        dims    = dShape%getGlobalSizes()

        if (product(dims) == 0) then
            return
        end if

        allocate(dims_i8(size(dims)))
        dims_i8 = dims

        if (present(loadDTypeNum)) then
            dtypeToLoad = loadDTypeNum
        else
            dtypeToLoad = dArray%getDataTypeNum()
        end if

        ! load the data according to type and endianness
        select case (dtypeToLoad)
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    bptr => dArray%getDataPointer_byte()
                else
                    allocate(bptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, bptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(bptr)
                    deallocate(bptr)
                end if

            case (SHORT_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    sptr => dArray%getDataPointer_short()
                else
                    allocate(sptr(dArray%getLocalTotalSize()))
                end if
                call h5dread_f(dset_id, memType, sptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(sptr)
                    deallocate(sptr)
                end if
            case (INT_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    iptr => dArray%getDataPointer_int()
                else
                    allocate(iptr(dArray%getLocalTotalSize()))
                end if
                call h5dread_f(dset_id, memType, iptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(iptr)
                    deallocate(iptr)
                end if

            case (LONG_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    lptr => dArray%getDataPointer_long()
                else
                    allocate(lptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, lptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(lptr)
                    deallocate(lptr)
                end if

            case (REAL_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    rptr => dArray%getDataPointer_real()
                else
                    allocate(rptr(dArray%getLocalTotalSize()))
                end if
                call h5dread_f(dset_id, memType, rptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(rptr)
                    deallocate(rptr)
                end if

            case (DOUBLE_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    dptr => dArray%getDataPointer_double()
                else
                    allocate(dptr(dArray%getLocalTotalSize()))
                end if
                call h5dread_f(dset_id, memType, dptr, &
                    dims_i8, hdferr); call h5check(hdferr)

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(dptr)
                    deallocate(dptr)
                end if

        end select

        call dArray%setLoaded(.true.)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file_id, hdferr); call h5check(hdferr)
    end subroutine

    subroutine loadMirroredArray(this,pinfo,dArray,locationInFile,required,loadDTypeNum)
        use hdf5

        implicit none

        class(HdfDataArrayReader)               :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        logical,          optional, intent(in)  :: required
        integer,             optional, intent(in)  :: loadDTypeNum

        integer :: hdferr

        integer(HID_T) :: file_id
        integer(HID_T) :: dset_id

        class(DataShape), pointer :: dShape

        integer(hid_t) :: memType

        integer(int8),  pointer :: bptr(:)
        integer(int16), pointer :: sptr(:)
        integer(int32), pointer :: iptr(:)
        integer(int64), pointer :: lptr(:)
        real(real32),   pointer :: rptr(:)
        real(real64),   pointer :: dptr(:)

        integer :: comm

        logical :: loaded

        ! assume initParallel() has already been called

        ! read the data from the given location.
        ! note: currently only the root reads the data, then broadcasts it to all members
        ! it might be better (on parallel file systems) to split the reads across all members
        ! then do an allgather
        ! ideally, this would be an option the user could set through pinfo
        if (pinfo%getRank() == 0) then
            call this%loadLocalArray(dArray,locationInFile,required,loadDTypeNum)
            loaded = dArray%isLoaded()
        end if

        call bcast0d(loaded,0,pinfo%getCommunicator(),'HDF load mirrored is loaded: ')

        ! check if the loading was successful, otherwise just return without sharing
        if (loaded) then
            comm = pinfo%getCommunicator()

            ! now broadcast the data from the root to all members
            select case(dArray%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    call error('HDF does not have a logical type at the current time.')
                case (BYTE_TYPE_NUM)
                    bptr => dArray%getDataPointer_byte()
                    call bcast1d(bptr,   dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (byte): '   // locationInFile)
                case (SHORT_TYPE_NUM)
                    sptr => dArray%getDataPointer_short()
                    call bcast1d(sptr,  dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (short): '  // locationInFile)
                case (INT_TYPE_NUM)
                    iptr => dArray%getDataPointer_int()
                    call bcast1d(iptr,    dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (int): '    // locationInFile)
                case (LONG_TYPE_NUM)
                    lptr => dArray%getDataPointer_long()
                    call bcast1d(lptr,   dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (long): '   // locationInFile)
                case (REAL_TYPE_NUM)
                    rptr => dArray%getDataPointer_real()
                    call bcast1d(rptr,   dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (real): '   // locationInFile)
                case (DOUBLE_TYPE_NUM)
                    dptr => dArray%getDataPointer_double()
                    call bcast1d(dptr, dArray%getLocalTotalSize(), 0, comm, &
                        'HDF load mirrored array (double): ' // locationInFile)
            end select
        end if
    end subroutine

    subroutine loadDistributedArray(this,pinfo,dArray,locationInFile,required,loadDTypeNum)

        use hdf5
        use mpi

        implicit none

        class(HdfDataArrayReader)               :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        logical,          optional, intent(in)  :: required
        integer,             optional, intent(in)  :: loadDTypeNum

        integer(HID_T) :: plist_id
        integer(HID_T) :: file_id
        integer(HID_T) :: slabspace_id
        integer(HID_T) :: dspace_id
        integer(HID_T) :: memspace_id
        integer(HID_T) :: dset_id

        integer :: hdferr

        integer :: ndim

        integer,           allocatable :: data_count(:)
        integer,           allocatable :: data_offset(:)
        integer,           allocatable :: data_localEnd(:)

        integer(HSIZE_T),  allocatable :: data_count_i8(:)
        integer(HSSIZE_T), allocatable :: data_offset_i8(:)
        integer(HSSIZE_T), allocatable :: data_localEnd_i8(:)

        integer,          allocatable :: dims(:)
        integer(hsize_t), allocatable :: dims_i8(:)

        class(DataShape), pointer :: dShape

        integer(hid_t) :: memType

        integer(int8),  pointer :: bptr(:)
        integer(int16), pointer :: sptr(:)
        integer(int32), pointer :: iptr(:)
        integer(int64), pointer :: lptr(:)
        real(real32),   pointer :: rptr(:)
        real(real64),   pointer :: dptr(:)

        logical :: isRequired

        integer :: dtypeToLoad

        if (present(required)) then
            isRequired = required
        else
            isRequired = .true.
        end if

        ! assume initParallel() has already been called

        ! initialize hdf if necessary
        call h5open_f(hdferr); call h5check(hdferr,'h5open')

        ! create a new property list for parallel file access
        call h5pcreate_f(H5P_FILE_ACCESS_F, plist_id, hdferr)
        call h5check(hdferr,'h5pcreate')

        ! set the communicator
        call h5pset_fapl_mpio_f(plist_id, pinfo%getCommunicator(), mpi_info_null, hdferr)
        call h5check(hdferr,'h5pcreate')

        ! open the file with the given access group
        call h5fopen_f(this%getLocation(), H5F_ACC_RDONLY_F, file_id, hdferr, access_prp=plist_id)
        call h5check(hdferr,'h5fopen')

        ! open the data set
        call h5dopen_f(file_id, locationInFile, dset_id, hdferr)

        if (isRequired) then
            call h5check(hdferr,'h5dopen - location: ' // trim(locationInFile))
        else if (hdferr < 0) then
            return
        end if

        ! retrieve the count and offsets from the data shape
        dShape => dArray%getDataShape()
        call dShape%getLocalRanges(data_offset,data_localEnd,data_count)

        allocate(data_count_i8(size(data_count)))
        allocate(data_offset_i8(size(data_offset)))
        allocate(data_localEnd_i8(size(data_localEnd)))

        ! offsets in HDF are 0-based
        data_count_i8    = data_count
        data_offset_i8   = data_offset - 1
        data_localEnd_i8 = data_localEnd - 1

        ! create a simple data space with ndim dimensions and data_count elements
        ndim = size(data_offset_i8)
        call h5screate_simple_f(ndim, data_count_i8, memspace_id, hdferr)
        call h5check(hdferr,'h5screate_simple')

        ! create a copy of the dataspace that we will use for the parallel hyperslab
        call h5dget_space_f(dset_id, slabspace_id, hdferr)
        call h5check(hdferr,'h5dget_space_f')

        ! setup the hyperslab in the current region
        call h5sselect_hyperslab_f (slabspace_id, H5S_SELECT_SET_F, data_offset_i8, data_count_i8, &
            hdferr)
        call h5check(hdferr,'h5sselect_hyperslab')

        ! create a property list for setting up the transfer
        call h5pcreate_f(H5P_DATASET_XFER_F, plist_id, hdferr)
        call h5check(hdferr,'h5pcreate_f')

        ! on the property list, set collective IO
        call h5pset_dxpl_mpio_f(plist_id, H5FD_MPIO_COLLECTIVE_F, hdferr)
        call h5check(hdferr,'h5pset_dxpl_mpio')

        memType = this%getHdfType(dArray%getDataTypeNum())

        dims    = dShape%getGlobalSizes()
        allocate(dims_i8(size(dims)))
        dims_i8 = dims

        if (present(loadDTypeNum)) then
            dtypeToLoad = loadDTypeNum
        else
            dtypeToLoad = dArray%getDataTypeNum()
        end if

        ! load the data in parallel according to type and endianness
        select case (dtypeToLoad)
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    bptr => dArray%getDataPointer_byte()
                else
                    allocate(bptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, bptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id,  xfer_prp = plist_id); call h5check(hdferr, &
                    & 'byte read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(bptr)
                    deallocate(bptr)
                end if

            case (SHORT_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    sptr => dArray%getDataPointer_short()
                else
                    allocate(sptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, sptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id,  xfer_prp = plist_id); call h5check(hdferr, &
                    & 'short read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(sptr)
                    deallocate(sptr)
                end if

            case (INT_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    iptr => dArray%getDataPointer_int()
                else
                    allocate(iptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, iptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id,  xfer_prp = plist_id); call h5check(hdferr, &
                    & 'int read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(iptr)
                    deallocate(iptr)
                end if

            case (LONG_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    lptr => dArray%getDataPointer_long()
                else
                    allocate(lptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, lptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id,  xfer_prp = plist_id); call h5check(hdferr, &
                    & 'long read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(lptr)
                    deallocate(lptr)
                end if

            case (REAL_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    rptr => dArray%getDataPointer_real()
                else
                    allocate(rptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, rptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id,  xfer_prp = plist_id); call h5check(hdferr, &
                    & 'float read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(rptr)
                    deallocate(rptr)
                end if

            case (DOUBLE_TYPE_NUM)

                if (dtypeToLoad == dArray%getDataTypeNum()) then
                    dptr => dArray%getDataPointer_double()
                else
                    allocate(dptr(dArray%getLocalTotalSize()))
                end if

                call h5dread_f(dset_id, memType, dptr, &
                    & dims_i8, hdferr, file_space_id = slabspace_id, &
                    & mem_space_id = memspace_id, xfer_prp = plist_id); call h5check(hdferr, &
                    & 'double read from ' // trim(locationInFile))

                if (dtypeToLoad /= dArray%getDataTypeNum()) then
                    call dArray%copyData(dptr)
                    deallocate(dptr)
                end if
        end select

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file_id, hdferr); call h5check(hdferr)

        call dArray%setLoaded(.true.)
    end subroutine

    function clone(this) result(newReader)
        implicit none

        class(HdfDataArrayReader)       :: this

        class(DataArrayReader), pointer :: newReader

        class(HdfDataArrayReader), pointer :: hdarptr

        allocate(hdarptr)
        call hdarptr%hdfDataArrayReaderConstructor(this%getLocation(),this%littleEndian)

        newReader => hdarptr
    end function
end module
