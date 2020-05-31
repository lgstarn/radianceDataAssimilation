module mpiUtils_mod
    use mpi
    use iso_fortran_env

    implicit none

    character(len=1024), public :: msgstr
!    integer,             public :: rank_world
!    integer,             public :: size_world

    integer,             public :: defaultStdOut = 6
    integer,             public :: defaultErrOut = 6

    logical,             public :: mpiInitialized = .false.

    logical :: printStackTrace = .true.

    interface bcast0d
        module procedure bcast0d_logical
        module procedure bcast0d_byte
        module procedure bcast0d_short
        module procedure bcast0d_int
        module procedure bcast0d_long
        module procedure bcast0d_real
        module procedure bcast0d_dble
    end interface

    interface bcast1d
        module procedure bcast1d_logical
        module procedure bcast1d_byte
        module procedure bcast1d_short
        module procedure bcast1d_int
        module procedure bcast1d_long
        module procedure bcast1d_real
        module procedure bcast1d_dble
        module procedure bcast1d_char
    end interface

    interface bcast1d_varLen
        module procedure bcast1d_varLen_logical
        module procedure bcast1d_varLen_byte
        module procedure bcast1d_varLen_short
        module procedure bcast1d_varLen_int
        module procedure bcast1d_varLen_long
        module procedure bcast1d_varLen_real
        module procedure bcast1d_varLen_dble
    end interface

    interface bcastnd
        module procedure bcast0d_logical
        module procedure bcast0d_byte
        module procedure bcast0d_short
        module procedure bcast0d_int
        module procedure bcast0d_long
        module procedure bcast0d_real
        module procedure bcast0d_dble

        module procedure bcast1d_logical
        module procedure bcast1d_byte
        module procedure bcast1d_short
        module procedure bcast1d_int
        module procedure bcast1d_long
        module procedure bcast1d_real
        module procedure bcast1d_dble
        module procedure bcast1d_char

        module procedure bcast2d_logical
        module procedure bcast2d_byte
        module procedure bcast2d_short
        module procedure bcast2d_int
        module procedure bcast2d_long
        module procedure bcast2d_real
        module procedure bcast2d_dble

        module procedure bcast3d_logical
        module procedure bcast3d_byte
        module procedure bcast3d_short
        module procedure bcast3d_int
        module procedure bcast3d_long
        module procedure bcast3d_real
        module procedure bcast3d_dble

        module procedure bcast4d_logical
        module procedure bcast4d_byte
        module procedure bcast4d_short
        module procedure bcast4d_int
        module procedure bcast4d_long
        module procedure bcast4d_real
        module procedure bcast4d_dble

        module procedure bcast5d_logical
        module procedure bcast5d_byte
        module procedure bcast5d_short
        module procedure bcast5d_int
        module procedure bcast5d_long
        module procedure bcast5d_real
        module procedure bcast5d_dble

        module procedure bcast6d_logical
        module procedure bcast6d_byte
        module procedure bcast6d_short
        module procedure bcast6d_int
        module procedure bcast6d_long
        module procedure bcast6d_real
        module procedure bcast6d_dble

        module procedure bcast7d_logical
        module procedure bcast7d_byte
        module procedure bcast7d_short
        module procedure bcast7d_int
        module procedure bcast7d_long
        module procedure bcast7d_real
        module procedure bcast7d_dble
    end interface

    interface bcastnd_varLen
        module procedure bcast2d_varLen_logical
        module procedure bcast2d_varLen_byte
        module procedure bcast2d_varLen_short
        module procedure bcast2d_varLen_int
        module procedure bcast2d_varLen_long
        module procedure bcast2d_varLen_real
        module procedure bcast2d_varLen_dble

        module procedure bcast3d_varLen_logical
        module procedure bcast3d_varLen_byte
        module procedure bcast3d_varLen_short
        module procedure bcast3d_varLen_int
        module procedure bcast3d_varLen_long
        module procedure bcast3d_varLen_real
        module procedure bcast3d_varLen_dble

        module procedure bcast4d_varLen_logical
        module procedure bcast4d_varLen_byte
        module procedure bcast4d_varLen_short
        module procedure bcast4d_varLen_int
        module procedure bcast4d_varLen_long
        module procedure bcast4d_varLen_real
        module procedure bcast4d_varLen_dble

        module procedure bcast5d_varLen_logical
        module procedure bcast5d_varLen_byte
        module procedure bcast5d_varLen_short
        module procedure bcast5d_varLen_int
        module procedure bcast5d_varLen_long
        module procedure bcast5d_varLen_real
        module procedure bcast5d_varLen_dble

        module procedure bcast6d_varLen_logical
        module procedure bcast6d_varLen_byte
        module procedure bcast6d_varLen_short
        module procedure bcast6d_varLen_int
        module procedure bcast6d_varLen_long
        module procedure bcast6d_varLen_real
        module procedure bcast6d_varLen_dble

        module procedure bcast7d_varLen_logical
        module procedure bcast7d_varLen_byte
        module procedure bcast7d_varLen_short
        module procedure bcast7d_varLen_int
        module procedure bcast7d_varLen_long
        module procedure bcast7d_varLen_real
        module procedure bcast7d_varLen_dble
    end interface

    interface doAllocateND
        module procedure doAllocateND_logical1d
        module procedure doAllocateND_byte1d
        module procedure doAllocateND_short1d
        module procedure doAllocateND_int1d
        module procedure doAllocateND_long1d
        module procedure doAllocateND_real1d
        module procedure doAllocateND_dble1d

        module procedure doAllocateND_logical2d
        module procedure doAllocateND_byte2d
        module procedure doAllocateND_short2d
        module procedure doAllocateND_int2d
        module procedure doAllocateND_long2d
        module procedure doAllocateND_real2d
        module procedure doAllocateND_dble2d

        module procedure doAllocateND_logical3d
        module procedure doAllocateND_byte3d
        module procedure doAllocateND_short3d
        module procedure doAllocateND_int3d
        module procedure doAllocateND_long3d
        module procedure doAllocateND_real3d
        module procedure doAllocateND_dble3d

        module procedure doAllocateND_logical4d
        module procedure doAllocateND_byte4d
        module procedure doAllocateND_short4d
        module procedure doAllocateND_int4d
        module procedure doAllocateND_long4d
        module procedure doAllocateND_real4d
        module procedure doAllocateND_dble4d

        module procedure doAllocateND_logical5d
        module procedure doAllocateND_byte5d
        module procedure doAllocateND_short5d
        module procedure doAllocateND_int5d
        module procedure doAllocateND_long5d
        module procedure doAllocateND_real5d
        module procedure doAllocateND_dble5d

        module procedure doAllocateND_logical6d
        module procedure doAllocateND_byte6d
        module procedure doAllocateND_short6d
        module procedure doAllocateND_int6d
        module procedure doAllocateND_long6d
        module procedure doAllocateND_real6d
        module procedure doAllocateND_dble6d

        module procedure doAllocateND_logical7d
        module procedure doAllocateND_byte7d
        module procedure doAllocateND_short7d
        module procedure doAllocateND_int7d
        module procedure doAllocateND_long7d
        module procedure doAllocateND_real7d
        module procedure doAllocateND_dble7d
    end interface

    contains

    subroutine initParallel()
        use mpi

        implicit none

        integer :: ierror

        if (.not. mpiInitialized) then
            call mpi_init(ierror)

            !call mpi_comm_size(MPI_COMM_WORLD, size_world, ierror)
            !call mpi_comm_rank(MPI_COMM_WORLD, rank_world, ierror)

            mpiInitialized = .true.
        end if
    end subroutine

    subroutine abortParallel(comm,rc)
        use mpi

        implicit none

        integer, optional, intent(in) :: comm
        integer, optional, intent(in) :: rc

        integer :: mycomm

        integer :: ierror

        if (present(comm)) then
            mycomm = comm
        else
            mycomm = MPI_COMM_WORLD
        end if

        if (present(rc)) then
            call mpi_abort(mycomm,rc,ierror)
        else
            call mpi_abort(mycomm,-999,ierror)
        end if
    end subroutine

    subroutine endParallel()
        use mpi

        implicit none

        integer :: ierror

        call mpi_finalize(ierror)
    end subroutine

    subroutine newline(unit,forceOutput,comm)
        use mpi

        integer, optional, intent(in) :: unit
        logical, optional, intent(in) :: forceOutput
        integer, optional, intent(in) :: comm

        call print('',unit,forceOutput,comm)
    end subroutine

    subroutine print(msg,unit,forceOutput,comm)
        use mpi

        implicit none

        character(len=*),  intent(in) :: msg
        integer, optional, intent(in) :: unit
        logical, optional, intent(in) :: forceOutput
        integer, optional, intent(in) :: comm

        integer :: mycomm, myrank, myunit, istat
        logical :: doForce

        if (present(forceOutput)) then
            doForce = forceOutput
        else
            doForce = .false.
        end if

        if (present(comm)) then
            mycomm = comm
        else
            mycomm = MPI_COMM_WORLD
        end if

        if (present(unit)) then
            myunit = unit
        else
            myunit = defaultStdOut
        end if

        if (doForce) then
            write(myunit,'(A)') trim(msg)
        else
            call MPI_Comm_rank(mycomm, myrank, istat); call mpichk(istat,&
                'In print message output MPI_Comm_rank:')

            if (myrank == 0) then
                write(myunit,'(A)') trim(msg)
            end if
        end if
    end subroutine

    subroutine debug(msg,unit,forceOutput,comm)
        use mpi

        implicit none

        character(len=*),  intent(in) :: msg
        integer, optional, intent(in) :: unit
        logical, optional, intent(in) :: forceOutput
        integer, optional, intent(in) :: comm

        integer :: mycomm, myrank, myunit, istat
        logical :: doForce

        if (present(forceOutput)) then
            doForce = forceOutput
        else
            doForce = .false.
        end if

        if (present(comm)) then
            mycomm = comm
        else
            mycomm = MPI_COMM_WORLD
        end if

        if (present(unit)) then
            myunit = unit
        else
            myunit = defaultStdOut
        end if

        if (doForce) then
            write(myunit,'(A)') trim(msg)
        else
            call MPI_Comm_rank(mycomm, myrank, istat); call mpichk(istat,&
                'In print message output MPI_Comm_rank:')

            if (myrank == 0) then
                write(myunit,'(A)') trim(msg)
            end if
        end if
    end subroutine

    subroutine error(msg,comm,rc,unit)
        use mpi

        implicit none

        character(len=*),  intent(in) :: msg
        integer, optional, intent(in) :: comm
        integer, optional, intent(in) :: rc
        integer, optional, intent(in) :: unit

        integer :: myrank, mysize, myunit, istat
        logical :: doForce

        real(8), pointer :: intentionalErrorToCauseStackTrace(:) => null()

        character(len=128) :: f

        doForce = .true.

        if (present(unit)) then
            myunit = unit
        else
            myunit = defaultErrOut
        end if

        call getRankAndSize(myrank,mysize,comm)

        ! print out the rank as one-based since this is Fortran...
        write(f,'(A,I0,A,I0)') 'Error encountered on rank ',myrank+1,' / ',mysize

        call print(trim(f) // ': ' // msg,unit,doForce,comm)

        if (printStackTrace) then
            allocate(intentionalErrorToCauseStackTrace(0))
            ! proposefully cause a null pointer exception to get a stack trace
            print *,intentionalErrorToCauseStackTrace(1)
        else
            call abortParallel(comm)
        end if
    end subroutine

    subroutine getArg(argNum,argStr,status)
        implicit none

        integer,                   intent(in)  :: argNum
        character(:), allocatable, intent(out) :: argStr
        integer,      optional,    intent(out) :: status

        integer :: strLen

        character(1) :: dummy

        call get_command_argument(argNum, dummy, length=strlen)
        allocate(character(len=strLen) :: argStr)

        if (present(status)) then
            call get_command_argument(argNum, argStr, status=status)
        else
            call get_command_argument(argNum, argStr)
        end if
    end subroutine

    subroutine getRankAndSize(rank_comm,size_comm,comm)
        use mpi

        implicit none

        integer,           intent(out) :: rank_comm
        integer,           intent(out) :: size_comm
        integer, optional, intent(in)  :: comm

        integer :: ierror
        integer :: commToUse

        if (present(comm)) then
            commToUse = comm
        else
            commToUse = MPI_COMM_WORLD
        end if

        call mpi_comm_size(commToUse, size_comm, ierror)

        call mpichk(ierror,'In getRankAndSize, comm_size:')
        call mpi_comm_rank(commToUse, rank_comm, ierror)

        call mpichk(ierror,'In getRankAndSize, comm_rank:')
    end subroutine

    subroutine decomposeElements(numElements, numPartitions, partitionIndex, numElemInPart, startIndexOfPart)
        implicit none

        integer, intent(in)  :: numElements
        integer, intent(in)  :: numPartitions
        integer, intent(in)  :: partitionIndex
        integer, intent(out) :: numElemInPart
        integer, intent(out) :: startIndexOfPart

        integer :: q, r

        q = numElements / numPartitions
        r = mod(numElements,numPartitions)

        numElemInPart = q
        if (r > partitionIndex) numElemInPart = numElemInPart + 1
        ! one-based indexing
        startIndexOfPart = q * partitionIndex + min(r, partitionIndex) + 1
    end subroutine

    subroutine createSubarray(datatype,ndims,sizes,axis,numPartitions,subarrays)
        use mpi

        implicit none

        integer, intent(in)  :: datatype
        integer, intent(in)  :: ndims
        integer, intent(in)  :: sizes(ndims)
        integer, intent(in)  :: axis
        integer, intent(in)  :: numPartitions
        integer, intent(out) :: subarrays(numPartitions)

        integer :: subsizes(ndims), substarts(ndims), n, s, p
        integer :: ierr

        subsizes = sizes
        substarts = 0

        do p=0,numPartitions-1
            call decomposeElements(sizes(axis),numPartitions,p,n,s)
            subsizes(axis) = n
            substarts(axis) = s-1

            call MPI_Type_create_subarray(ndims, sizes, subsizes, substarts, MPI_ORDER_FORTRAN, &
                datatype, subarrays(p+1), ierr)
            call mpichk(ierr,'In createSubarray, MPI_Type_create_subarray:')

            call MPI_Type_commit(subarrays(p+1),ierr)
            call mpichk(ierr,'In createSubarray, MPI_Type_commit:')
        end do
    end subroutine

    subroutine exchangeDouble3D(comm,sizes1,array1,axis1,sizes2,array2,axis2)
        use mpi

        implicit none

        integer, intent(in)  :: comm
        integer, intent(in)  :: sizes1(3)
        real(8), intent(in)  :: array1(sizes1(1),sizes1(2),sizes1(3))
        integer, intent(in)  :: axis1
        integer, intent(in)  :: sizes2(3)
        real(8), intent(out) :: array2(sizes2(1),sizes2(2),sizes2(3))
        integer, intent(in)  :: axis2

        integer, dimension(:), allocatable :: subarrays1, subarrays2, counts, displs

        integer :: i, nparts, ierr

        integer :: datatype = MPI_DOUBLE_PRECISION

        call MPI_Comm_size(comm,nparts,ierr)
        call mpichk(ierr,'In exchangeDouble3D, MPI_Comm_size:')

        ! TODO: only allocate these once per program execution
        allocate(subarrays1(nparts),subarrays2(nparts),counts(nparts),displs(nparts))

        counts = 1
        displs = 0

        call createSubarray(datatype, 3, sizes1, axis1, nparts, subarrays1)
        call createSubarray(datatype, 3, sizes2, axis2, nparts, subarrays2)

        call MPI_Alltoallw(array1, counts, displs, subarrays1, &
                           array2, counts, displs, subarrays2, comm, ierr)
        call mpichk(ierr,'In exchangeDouble3D, MPI_Alltoallw:')

        do i=1,nparts
            call MPI_type_free(subarrays1(i),ierr)
            call mpichk(ierr,'In exchangeDouble3D, MPI_type_free A:')
            call MPI_type_free(subarrays2(i),ierr)
            call mpichk(ierr,'In exchangeDouble3D, MPI_type_free B:')
        end do

        deallocate(subarrays1,subarrays2,counts,displs)
    end subroutine

    subroutine exchangeDoubleComplex3D(comm,sizes1,array1,axis1,sizes2,array2,axis2)
        use mpi
        use, intrinsic :: iso_c_binding

        implicit none

        integer, intent(in)                    :: comm
        integer, intent(in)                    :: sizes1(3)
        complex(C_DOUBLE_COMPLEX), intent(in)  :: array1(sizes1(1),sizes1(2),sizes1(3))
        integer, intent(in)                    :: axis1
        integer, intent(in)                    :: sizes2(3)
        complex(C_DOUBLE_COMPLEX), intent(out) :: array2(sizes2(1),sizes2(2),sizes2(3))
        integer, intent(in)                    :: axis2

        integer, dimension(:), allocatable :: subarrays1, subarrays2, counts, displs

        integer :: datatype = MPI_DOUBLE_COMPLEX

        integer :: i, nparts, ierr

        call MPI_Comm_size(comm,nparts,ierr)
        call mpichk(ierr,'In exchangeDoubleComplex3D, MPI_Comm_size:')

        ! TODO: only allocate these once per program execution
        allocate(subarrays1(nparts),subarrays2(nparts),counts(nparts),displs(nparts))

        counts = 1
        displs = 0

        call createSubarray(datatype, 3, sizes1, axis1, nparts, subarrays1)
        call createSubarray(datatype, 3, sizes2, axis2, nparts, subarrays2)

        call MPI_Alltoallw(array1, counts, displs, subarrays1, &
                           array2, counts, displs, subarrays2, comm, ierr)
        call mpichk(ierr,'In exchangeDoubleComplex3D, MPI_Alltoallw:')

        do i=1,nparts
            call MPI_type_free(subarrays1(i),ierr)
            call mpichk(ierr,'In exchangeDoubleComplex3D, MPI_type_free A:')
            call MPI_type_free(subarrays2(i),ierr)
            call mpichk(ierr,'In exchangeDoubleComplex3D, MPI_type_free B:')
        end do

        deallocate(subarrays1,subarrays2,counts,displs)
    end subroutine

    subroutine errmsg_concat(err_message,message)
        implicit none

        character(len=MPI_MAX_ERROR_STRING), intent(inout) :: err_message
        character(len=*),          optional, intent(in)    :: message

        if (present(message)) then
            err_message = trim(err_message) // ': ' // trim(message)
        end if
    end subroutine

    subroutine barrier(comm,message)
        use mpi

        implicit none

        integer,          optional, intent(in) :: comm
        character(len=*), optional, intent(in) :: message

        character(len=MPI_MAX_ERROR_STRING)    :: err_message

        integer :: ierr

        if (present(comm)) then
            call MPI_Barrier(comm,ierr)
        else
            call MPI_Barrier(MPI_COMM_WORLD,ierr)
        end if

        call errmsg_concat(err_message,message)

        call mpichk(ierr,message)
    end subroutine

    subroutine bcast0d_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,                    intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),              intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),             intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),             intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),             intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),               intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast0d_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),               intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcast0d.incl'
    end subroutine

    subroutine bcast1d_logical(val,n,root,comm,message)
        use mpi

        implicit none

        logical,                    intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_byte(val,n,root,comm,message)
        use mpi

        implicit none

        integer(int8),              intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_short(val,n,root,comm,message)
        use mpi

        implicit none

        integer(int16),             intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_int(val,n,root,comm,message)
        use mpi

        implicit none

        integer(int32),             intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_long(val,n,root,comm,message)
        use mpi

        implicit none

        integer(int64),             intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_real(val,n,root,comm,message)
        use mpi

        implicit none

        real(real32),               intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_dble(val,n,root,comm,message)
        use mpi

        implicit none

        real(real64),               intent(inout) :: val(n)
        integer,                    intent(in)    :: n
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_char(val,n,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: n
        character(len=n),           intent(inout) :: val
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_CHAR
        character(len=*), parameter :: typeName = 'char'

        include 'mpiUtils_bcast1d.incl'
    end subroutine

    subroutine bcast1d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast1d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcast1d_varLen.incl'
    end subroutine

    subroutine bcast2d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        logical,                    intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        integer(int8),              intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        integer(int16),             intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        integer(int32),             intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        integer(int64),             intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        real(real32),               intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(2)
        real(real64),               intent(inout) :: val(dims(1),dims(2))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast2d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast2d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        logical,                    intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        integer(int8),              intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        integer(int16),             intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        integer(int32),             intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        integer(int64),             intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        real(real32),               intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(3)
        real(real64),               intent(inout) :: val(dims(1),dims(2),dims(3))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast3d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast3d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        logical,                    intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        integer(int8),              intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        integer(int16),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        integer(int32),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        integer(int64),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        real(real32),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(4)
        real(real64),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast4d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast4d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        logical,                    intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        integer(int8),              intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        integer(int16),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        integer(int32),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        integer(int64),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        real(real32),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(5)
        real(real64),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast5d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast5d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        logical,                    intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        integer(int8),              intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        integer(int16),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        integer(int32),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        integer(int64),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        real(real32),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(6)
        real(real64),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast6d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,          pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast6d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_logical(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        logical,                    intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_LOGICAL
        character(len=*), parameter :: typeName = 'logical'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_byte(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        integer(int8),              intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER1
        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_short(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        integer(int16),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER2
        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_int(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        integer(int32),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER4
        character(len=*), parameter :: typeName = 'integer'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_long(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        integer(int64),             intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_INTEGER8
        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_real(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        real(real32),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_REAL
        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_dble(val,dims,root,comm,message)
        use mpi

        implicit none

        integer,                    intent(in)    :: dims(7)
        real(real64),               intent(inout) :: val(dims(1),dims(2),dims(3), &
                                                         dims(4),dims(5),dims(6), &
                                                         dims(7))
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        integer,          parameter :: type     = MPI_DOUBLE_PRECISION
        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd.incl'
    end subroutine

    subroutine bcast7d_varLen_logical(val,root,comm,message)
        use mpi

        implicit none

        logical,    pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_byte(val,root,comm,message)
        use mpi

        implicit none

        integer(int8),    pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'byte'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_short(val,root,comm,message)
        use mpi

        implicit none

        integer(int16),   pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'short'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_int(val,root,comm,message)
        use mpi

        implicit none

        integer(int32),   pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'int'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_long(val,root,comm,message)
        use mpi

        implicit none

        integer(int64),   pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'long'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_real(val,root,comm,message)
        use mpi

        implicit none

        real(real32),     pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'real'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    subroutine bcast7d_varLen_dble(val,root,comm,message)
        use mpi

        implicit none

        real(real64),     pointer,  intent(inout) :: val(:,:,:,:,:,:,:)
        integer,                    intent(in)    :: root
        integer,          optional, intent(in)    :: comm
        character(len=*), optional, intent(in)    :: message

        character(len=*), parameter :: typeName = 'double'

        include 'mpiUtils_bcastnd_varLen.incl'
    end subroutine

    ! Check the MPI error message with a friendly string. Also output an optional message about the
    ! error
    subroutine mpichk(mpiErrorCode, message)

        use mpi

        implicit none

        integer,                             intent(in) :: mpiErrorCode
        character(len=*),          optional, intent(in) :: message

        character(len=MPI_MAX_ERROR_STRING)             :: err_message

        integer :: length, temp

        ! check for an error
        if (mpiErrorCode /= MPI_SUCCESS) then
            ! get the string corresponding to the err code
            call MPI_Error_string (mpiErrorCode, err_message, length, temp )

            if (present(message)) then
                ! if the optional message is present, write it out.
                write(defaultErrOut,*) trim(message), ' ', err_message(1:length)
            else
                ! otherwise just write out the error message
                write(defaultErrOut,*) err_message(1:length)
            end if

            ! abort all processes
            call MPI_Abort(MPI_COMM_WORLD, 1, temp)
        end if
    end subroutine

    subroutine mpiUtils_read_file( filename, buffer, comm )
        use mpi

        implicit none

        character(*), intent(in) :: filename
        character(:), allocatable, intent(out) :: buffer
        integer, optional :: comm

        integer :: communicator, myrank, istat, s, u

        logical :: found

        if ( present( comm ) ) then
            communicator = comm
        else
            communicator = MPI_COMM_WORLD
        end if

        call mpi_comm_rank(communicator, myrank , istat)

        if ( myrank .eq. 0 ) then
            inquire(file=filename,size=s,exist=found)

            if (.not. found) then
                write(defaultStdOut,*) 'Error: could not find the file ',trim(filename)
                call MPI_Abort(communicator,1,istat)
            end if
        end if

        call bcast0d(s,0,communicator)

        allocate(character(len=s)::buffer)

        if ( myrank .eq. 0 ) then
            open( file=filename, newunit=u, access='STREAM')
            read(u) buffer
            close(u)
        end if

        call bcast1d(buffer,s,0,communicator)
    end subroutine

    subroutine doAllocateND_logical1d(val,dims)
        implicit none

        logical, pointer    :: val(:)
        integer, intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_byte1d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_short1d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_int1d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_long1d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_real1d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_dble1d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:)
        integer,       intent(in) :: dims(1)

        allocate(val(dims(1)))
    end subroutine

    subroutine doAllocateND_logical2d(val,dims)
        implicit none

        logical, pointer    :: val(:,:)
        integer, intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_byte2d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_short2d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_int2d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_long2d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_real2d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_dble2d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:)
        integer,       intent(in) :: dims(2)

        allocate(val(dims(1),dims(2)))
    end subroutine

    subroutine doAllocateND_logical3d(val,dims)
        implicit none

        logical, pointer    :: val(:,:,:)
        integer, intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_byte3d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_short3d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_int3d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_long3d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_real3d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_dble3d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:,:)
        integer,       intent(in) :: dims(3)

        allocate(val(dims(1),dims(2),dims(3)))
    end subroutine

    subroutine doAllocateND_logical4d(val,dims)
        implicit none

        logical, pointer    :: val(:,:,:,:)
        integer, intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_byte4d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_short4d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_int4d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_long4d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_real4d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_dble4d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:,:,:)
        integer,       intent(in) :: dims(4)

        allocate(val(dims(1),dims(2),dims(3),dims(4)))
    end subroutine

    subroutine doAllocateND_logical5d(val,dims)
        implicit none

        logical, pointer    :: val(:,:,:,:,:)
        integer, intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_byte5d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_short5d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_int5d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_long5d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_real5d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_dble5d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:,:,:,:)
        integer,       intent(in) :: dims(5)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5)))
    end subroutine

    subroutine doAllocateND_logical6d(val,dims)
        implicit none

        logical, pointer    :: val(:,:,:,:,:,:)
        integer, intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_byte6d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_short6d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_int6d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_long6d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_real6d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_dble6d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:,:,:,:,:)
        integer,       intent(in) :: dims(6)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6)))
    end subroutine

    subroutine doAllocateND_logical7d(val,dims)
        implicit none

        logical, pointer    :: val(:,:,:,:,:,:,:)
        integer, intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_byte7d(val,dims)
        implicit none

        integer(int8), pointer    :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_short7d(val,dims)
        implicit none

        integer(int16), pointer   :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_int7d(val,dims)
        implicit none

        integer(int32), pointer   :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_long7d(val,dims)
        implicit none

        integer(int64), pointer   :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_real7d(val,dims)
        implicit none

        real(real32),  pointer    :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine

    subroutine doAllocateND_dble7d(val,dims)
        implicit none

        real(real64),  pointer    :: val(:,:,:,:,:,:,:)
        integer,       intent(in) :: dims(7)

        allocate(val(dims(1),dims(2),dims(3),dims(4),dims(5),dims(6),dims(7)))
    end subroutine
end module
