module hdfUtils_mod
    use iso_c_binding
    use iso_fortran_env

    use cInterface_mod

    use mpiUtils_mod

    implicit none

    interface h5LoadAttribute
        module procedure h5LoadVarLenStringsAttribute
    end interface

    interface h5LoadData
        module procedure h5Load1DCharData
        module procedure h5Load1DByteData
        module procedure h5Load1DIntegerData
        module procedure h5Load2DIntegerData
        module procedure h5Load1DRealData
        module procedure h5Load2DRealData
        module procedure h5Load3DRealData
        module procedure h5Load1DDoubleData
        module procedure h5Load2DDoubleData
        module procedure h5Load3DDoubleData
    end interface

    contains

    subroutine h5LoadVarLenStringsAttribute(filename,aname,bufr,maxLen,location)
        use hdf5

        implicit none

        character(len=*), intent(in)            :: filename
        character(len=*), intent(in)            :: aname
        character(len=*), dimension(:), pointer :: bufr
        integer,          intent(in)            :: maxLen
        character(len=*), intent(in), optional  :: location

        type(c_ptr), dimension(:), allocatable, target :: rdata

        integer(hid_t) :: size_val, file, dset_id, attr_id, space_id, type_id
        integer(int32) :: ndims

        integer(hsize_t), dimension(1:1) :: dims = (/1/)
        integer(hsize_t), dimension(1:1) :: maxdims
        integer(hid_t) :: atype

        integer :: hdferr, i, j, length

        integer(size_t) :: isize

        type(c_ptr) :: f_ptr

        ! character(len=maxLen, kind=c_char), pointer :: data ! A pointer to a Fortran string
        character(len=1), dimension(:), pointer :: data ! A pointer to a Fortran string

        ! Initialize fortran interface.
        call h5open_f(hdferr); call h5check(hdferr)

        ! Open file and attribute.
        CALL h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        if (present(location)) then
            call h5dopen_f(file, location, dset_id, hdferr); call h5check(hdferr)
        else
            dset_id = file
        end if

        call h5aopen_f(dset_id, aname, attr_id, hdferr); call h5check(hdferr)

        call h5aget_space_f(attr_id, space_id, hdferr); call h5check(hdferr)

        call h5sget_simple_extent_dims_f(space_id, dims, maxdims, hdferr); call h5check(hdferr)

        allocate(rdata(1:dims(1)))

        ! Read the data.
        f_ptr = C_LOC(rdata(1))
        call h5aread_f(attr_id, H5T_STRING, f_ptr, hdferr); call h5check(hdferr)

        allocate(bufr(dims(1)))

        ! Put the variable data in bufr according to the f08 standard
        do i = 1, dims(1)
            call c_f_pointer(rdata(i), data, (/maxLen/))
            length = 0

            do while(data(length+1) .ne. c_null_char)
                length = length + 1
            enddo

            bufr(i) = ''

            do j=1, length
                bufr(i)(j:j) = data(j)
            end do
        end do

        ! another way to do it in case the above fails (e.g. when checking bounds)
        !do i = 1, dims(1)
        !     call c_f_pointer(rdata(i), data)
        !     length = 0
        ! 
        !    do while(data(length+1:length+1) .ne. c_null_char)
        !         length = length + 1
        !     enddo
        ! 
        !     bufr(i) = data(1:length)
        ! end do

        call h5aclose_f(attr_id , hdferr); call h5check(hdferr)

        call h5sclose_f(space_id, hdferr); call h5check(hdferr)

        if (present(location)) then
            call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        end if

        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load1DCharData(filename,dname,bufr)
        use hdf5

        implicit none

        character(len=*), intent(in) :: filename
        character(len=*), intent(in) :: dname
        character(len=*), dimension(:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr
        integer(size_t) :: isize

        integer(hid_t) :: atype

        call h5open_f(hdferr); call h5check(hdferr)

        call h5tcopy_f(H5T_NATIVE_CHARACTER,atype,hdferr); call h5check(hdferr)

        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id, dspace_id, hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))
        allocate(maxdims(ndims))
 
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)  

        allocate(bufr(dims(1)))
        isize = len(bufr)
        call h5tset_size_f(atype,isize,hdferr); call h5check(hdferr)

        call h5dread_f(dset_id, atype, bufr, dims, hdferr); call h5check(hdferr) !,H5S_ALL_F,H5S_ALL_F,H5P_DEFAULT_F)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load1DByteData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        integer(int8), dimension(:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims, maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))
        
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)  

        allocate(bufr(dims(1)))

        call h5dread_f(dset_id, H5T_STD_I8LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)

        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load1DIntegerData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        integer(int32), dimension(:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims, maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))
        
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)  

        allocate(bufr(dims(1)))

        call h5dread_f(dset_id, H5T_STD_I16LE, bufr, dims, hdferr)
        bufr = int(bufr,2); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)

        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load2DIntegerData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        integer, dimension(:,:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims, maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dread_f(dset_id, H5T_STD_I16LE, bufr, dims, hdferr)
        bufr = int(bufr,2); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)

        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load1DRealData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real32), dimension(:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))
        allocate(maxdims(ndims))
        
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)  

        allocate(bufr(dims(1)))

        call h5dread_f(dset_id, H5T_IEEE_F32LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load2DRealData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real32), dimension(:,:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))
        
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)  

        allocate(bufr(dims(1),dims(2)))

        call h5dread_f(dset_id, H5T_IEEE_F32LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load3DRealData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real32), dimension(:,:,:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))
        
        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)

        allocate(bufr(dims(1),dims(2),dims(3)))

        call h5dread_f(dset_id, H5T_IEEE_F32LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load1DDoubleData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real64), dimension(:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))

        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)

        allocate(bufr(dims(1)))

        call h5dread_f(dset_id, H5T_IEEE_F64LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load2DDoubleData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real64), dimension(:,:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))

        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)

        allocate(bufr(dims(1),dims(2)))

        call h5dread_f(dset_id, H5T_IEEE_F64LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)
    end subroutine

    subroutine h5Load3DDoubleData(filename,dname,bufr)
        use hdf5

        implicit none

        character(LEN=*), intent(in) :: filename
        character(LEN=*), intent(in) :: dname
        real(real64), dimension(:,:,:), pointer :: bufr

        integer(hid_t) :: file, dset_id, dspace_id
        integer(int32) :: ndims
        integer(hsize_t), dimension(:), allocatable :: dims,maxdims
        integer :: hdferr

        call h5open_f(hdferr); call h5check(hdferr)
        call h5fopen_f(filename, H5F_ACC_RDONLY_F, file, hdferr); call h5check(hdferr)

        call h5dopen_f(file, dname, dset_id, hdferr); call h5check(hdferr)

        call h5dget_space_f(dset_id,dspace_id,hdferr); call h5check(hdferr)

        call h5sget_simple_extent_ndims_f(dspace_id, ndims, hdferr); call h5check(hdferr)

        allocate(dims(ndims))

        allocate(maxdims(ndims))

        call h5sget_simple_extent_dims_f(dspace_id, dims, maxdims, hdferr); call h5check(hdferr)

        allocate(bufr(dims(1),dims(2),dims(3)))

        call h5dread_f(dset_id, H5T_IEEE_F64LE, bufr, dims, hdferr); call h5check(hdferr)

        deallocate(dims)
        deallocate(maxdims)

        call h5dclose_f(dset_id, hdferr); call h5check(hdferr)
        call h5fclose_f(file, hdferr); call h5check(hdferr)

    end subroutine

    subroutine h5check(hdferr, message)
        use hdf5

        implicit none

        integer,                    intent(in) :: hdferr
        character(len=*), optional, intent(in) :: message

        if (hdferr .lt. 0) then
            if (present(message)) then
                write(msgstr,*) trim(message),', HDF Error code: ',hdferr
            else
                write(msgstr,*) 'HDF Error code:',hdferr
            end if

            call error(msgstr)
        end if
    end subroutine
end module
