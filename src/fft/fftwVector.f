module fftwVector_mod
    use mpiUtils_mod
    use abstractVector_mod
    use, intrinsic :: iso_c_binding
    implicit none

    private

    include 'fftw3.f03'

    type, public, extends(AbstractVector) :: FftwVector
        private
            complex(C_DOUBLE_COMPLEX), pointer, contiguous :: data1d(:)     ! only initialized if fftDim = 1
            complex(C_DOUBLE_COMPLEX), pointer, contiguous :: data2d(:,:)   ! only initialized if fftDim = 2
            complex(C_DOUBLE_COMPLEX), pointer, contiguous :: data3d(:,:,:) ! only initialized if fftDim = 3

            integer     :: L, M, N ! 3D coordinates. If 1D, only N is set. If 2D, only M and N are set.
            integer     :: fftDim  ! 1, 2 or 3
            type(C_PTR) :: dataptr ! C-style pointer to the data, used to interface with FFTW C-routines directly
            logical, private :: alloced ! whether the data was allocated or set as a pointer

        contains
            procedure :: fftwVectorConstructor_1d
            procedure :: fftwVectorConstructor_data1d
            procedure :: fftwVectorConstructor_2d
            procedure :: fftwVectorConstructor_data2d
            procedure :: fftwVectorConstructor_3d
            procedure :: fftwVectorConstructor_data3d

            procedure :: get1DPointer
            procedure :: get2DPointer
            procedure :: get3DPointer
            procedure :: getL
            procedure :: getM
            procedure :: getN
            procedure :: getFftDim

            procedure :: set1DArray
            procedure :: get1DArray
            procedure :: get1DArrayPtr
            procedure :: getSize
            procedure :: add
            procedure :: subtract
            procedure :: getClassName
            procedure :: scalarMultiply
            procedure :: elementwiseMultiply
            procedure :: clone

            final :: fftwVectorDestructor ! clean up all allocated variables
    end type

    contains

    ! 1D Constructor, no pointer
    subroutine fftwVectorConstructor_1d(this, N)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: N

        this%dataptr = fftw_alloc_complex(int(N, C_SIZE_T))
        call c_f_pointer(this%dataptr, this%data1d, [N])

        this%fftDim = 1
        this%L = 1
        this%M = 1
        this%N = N
        this%alloced = .true.
    end subroutine

    ! 1D Constructor, c-style pointer for preallocated memory
    subroutine fftwVectorConstructor_data1d(this, N, dataptr)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: N
        type(C_PTR) :: dataptr

        call c_f_pointer(dataptr, this%data1d, [N])

        this%fftDim = 1
        this%L = 1
        this%M = 1
        this%N = N
        this%alloced = .false.
    end subroutine

    ! 2D Constructor, no pointer
    subroutine fftwVectorConstructor_2d(this, M, N)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: M, N

        this%dataptr = fftw_alloc_complex(int(M * N, C_SIZE_T))
        call c_f_pointer(this%dataptr, this%data2d, [M,N])

        this%fftDim = 2
        this%L = 1
        this%M = M
        this%N = N
        this%alloced = .true.
    end subroutine

    ! 2D Constructor, c-style pointer for preallocated memory
    subroutine fftwVectorConstructor_data2d(this, M, N, dataptr)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: M, N
        type(C_PTR) :: dataptr

        call c_f_pointer(dataptr, this%data2d, [M,N])

        this%fftDim = 2
        this%L = 1
        this%M = M
        this%N = N
        this%alloced = .false.
    end subroutine

    ! 3D Constructor, no pointer
    subroutine fftwVectorConstructor_3d(this, L, M, N)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: L, M, N

        this%dataptr = fftw_alloc_complex(int(L * M * N, C_SIZE_T))
        call c_f_pointer(this%dataptr, this%data3d, [L,M,N])

        this%fftDim = 3
        this%L = L
        this%M = M
        this%N = N
        this%alloced = .true.
    end subroutine

    ! 3D Constructor, c-style pointer for preallocated memory
    subroutine fftwVectorConstructor_data3d(this, L, M, N, dataptr)
        implicit none

        class(FftwVector) :: this
        integer, intent(in) :: L, M, N
        type(C_PTR) :: dataptr

        call c_f_pointer(dataptr, this%data3d, [L,M,N])

        this%fftDim = 3
        this%L = L
        this%M = M
        this%N = N
        this%alloced = .false.
    end subroutine

    subroutine fftwVectorDestructor(this)
        implicit none

        type(FftwVector)  :: this

        if (this%alloced) then
            call fftw_free(this%dataptr)
        end if
    end subroutine

    function get1DPointer(this) result(data1d)
        implicit none

        class(FftwVector) :: this
        complex(C_DOUBLE_COMPLEX), dimension(:), pointer :: data1d

        data1d => this%data1d
    end function

    function get2DPointer(this) result(data2d)
        implicit none

        class(FftwVector) :: this
        complex(C_DOUBLE_COMPLEX), dimension(:,:), pointer :: data2d

        data2d => this%data2d
    end function

    function get3DPointer(this) result(data3d)
        implicit none

        class(FftwVector) :: this
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d

        data3d => this%data3d
    end function

    function getFftDim(this) result(fftDim)
        implicit none

        class(FftwVector) :: this
        integer :: fftDim

        fftDim = this%fftDim
    end function

    function getL(this) result(L)
        implicit none

        class(FftwVector) :: this
        integer :: L

        L = this%L
    end function

    function getM(this) result(M)
        implicit none

        class(FftwVector) :: this
        integer :: M

        M = this%M
    end function

    function getN(this) result(N)
        implicit none

        class(FftwVector) :: this
        integer :: N

        N = this%N
    end function

    subroutine set1DArray(this,array1d)
        implicit none

        class(FftwVector) :: this
        real(8), dimension(:) :: array1d
        real(8), dimension(:), pointer :: dataPtr

        select case(this%fftDim)
            case (1)
                call c_f_pointer(c_loc(this%data1d), dataPtr, [this%getSize()])
                dataPtr(:) = array1d(:)
            case (2)
                call c_f_pointer(c_loc(this%data2d), dataPtr, [this%getSize()])
                dataPtr(:) = array1d(:)
            case (3)
                call c_f_pointer(c_loc(this%data3d), dataPtr, [this%getSize()])
                dataPtr(:) = array1d(:)
        end select
    end subroutine

    subroutine get1DArray(this,array1d)
        implicit none

        class(FftwVector) :: this
        real(8), dimension(:) :: array1d
        real(8), dimension(:), pointer :: dataPtr

        select case(this%fftDim)
            case (1)
                call c_f_pointer(c_loc(this%data1d), dataPtr, [this%getSize()])
                array1d(:) = dataPtr(:)
            case (2)
                call c_f_pointer(c_loc(this%data2d), dataPtr, [this%getSize()])
                array1d(:) = dataPtr(:)
            case (3)
                call c_f_pointer(c_loc(this%data3d), dataPtr, [this%getSize()])
                array1d(:) = dataPtr(:)
        end select
    end subroutine

    function get1DArrayPtr(this) result(array1dPtr)
        implicit none

        class(FftwVector) :: this
        real(8), dimension(:), pointer :: array1dPtr

        select case(this%fftDim)
            case (1)
                call c_f_pointer(c_loc(this%data1d), array1dPtr, [this%getSize()])
            case (2)
                call c_f_pointer(c_loc(this%data2d), array1dPtr, [this%getSize()])
            case (3)
                call c_f_pointer(c_loc(this%data3d), array1dPtr, [this%getSize()])
        end select
    end function

    function getSize(this) result(vsize)
        implicit none

        class(FftwVector) :: this
        integer :: vsize

        integer :: L, M, N

        L = this%L
        M = this%M
        N = this%N

        select case(this%fftDim)
            case (1)
                vsize = 2*N
            case (2)
                vsize = 2*M*N
            case (3)
                vsize = 2*L*M*N
        end select
    end function

    subroutine add(this,other,result)
        implicit none

        class(FftwVector) :: this
        class(AbstractVector), pointer :: other, result

        class(FftwVector), pointer :: other_fftw, result_fftw

        select type(other)
            class is (FftwVector)
                other_fftw => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into add Fftw'
                call error(msgstr)
        end select

        select type(result)
            class is (FftwVector)
                result_fftw => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into add Fftw'
                call error(msgstr)
        end select

!        if (this%fftDim .ne. other%fftDim .or. this%fftDim .ne. result%fftDim) then
!            print ('A,I6,A,I6,A,I6)','Error: FFT vector addition dimension mismatch: ',&
!                this%fftDim,' / ',other%fftDim,' / ',result%fftDim
!            stop
!        end if
!
!        if (this%L .ne. other%L .or. this%M .ne. other%M .or. this%N .ne. other%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',other%L,', ',other%M,', ',other%N,')'
!            stop
!        end if
!
!        if (this%L .ne. result%L .or. this%M .ne. result%M .or. this%N .ne. result%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',result%L,', ',result%M,', ',result%N,')'
!            stop
!        end if

        select case(this%fftDim)
            case (1)
                result_fftw%data1d = this%data1d + other_fftw%data1d
            case (2)
                result_fftw%data2d = this%data2d + other_fftw%data2d
            case (3)
                result_fftw%data3d = this%data3d + other_fftw%data3d
        end select
    end subroutine

    subroutine subtract(this,other,result)
        implicit none

        class(FftwVector) :: this
        class(AbstractVector), pointer :: other, result

        class(FftwVector), pointer :: other_fftw, result_fftw

        select type(other)
            class is (FftwVector)
                other_fftw => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into subtract Fftw'
                call error(msgstr)
        end select

        select type(result)
            class is (FftwVector)
                result_fftw => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into subtract Fftw'
                call error(msgstr)
        end select

!        if (this%fftDim .ne. other%fftDim .or. this%fftDim .ne. result%fftDim) then
!            print ('A,I6,A,I6,A,I6)','Error: FFT vector addition dimension mismatch: ',&
!                this%fftDim,' / ',other%fftDim,' / ',result%fftDim
!            stop
!        end if
!
!        if (this%L .ne. other%L .or. this%M .ne. other%M .or. this%N .ne. other%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',other%L,', ',other%M,', ',other%N,')'
!            stop
!        end if
!
!        if (this%L .ne. result%L .or. this%M .ne. result%M .or. this%N .ne. result%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',result%L,', ',result%M,', ',result%N,')'
!            stop
!        end if

        select case(this%fftDim)
            case (1)
                result_fftw%data1d = this%data1d - other_fftw%data1d
            case (2)
                result_fftw%data2d = this%data2d - other_fftw%data2d
            case (3)
                result_fftw%data3d = this%data3d - other_fftw%data3d
        end select
    end subroutine

    subroutine scalarMultiply(this,alpha,result)
        implicit none

        class(FftwVector) :: this
        real(8) :: alpha
        class(AbstractVector), pointer :: result

        class(FftwVector), pointer :: result_fftw

        select type(result)
            class is (FftwVector)
                result_fftw => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into Fftw scalar mult'
                call error(msgstr)
        end select

!        if (this%fftDim .ne. other%fftDim .or. this%fftDim .ne. result%fftDim) then
!            print ('A,I6,A,I6,A,I6)','Error: FFT vector addition dimension mismatch: ',&
!                this%fftDim,' / ',other%fftDim,' / ',result%fftDim
!            stop
!        end if
!
!        if (this%L .ne. other%L .or. this%M .ne. other%M .or. this%N .ne. other%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',other%L,', ',other%M,', ',other%N,')'
!            stop
!        end if
!
!        if (this%L .ne. result%L .or. this%M .ne. result%M .or. this%N .ne. result%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',result%L,', ',result%M,', ',result%N,')'
!            stop
!        end if

        select case(this%fftDim)
            case (1)
                result_fftw%data1d = alpha*this%data1d
            case (2)
                result_fftw%data2d = alpha*this%data2d
            case (3)
                result_fftw%data3d = alpha*this%data3d
        end select
    end subroutine

    subroutine elementwiseMultiply(this,other,result)
        implicit none

        class(FftwVector) :: this
        class(AbstractVector), pointer :: other, result

        class(FftwVector), pointer :: other_fftw, result_fftw

        select type(other)
            class is (FftwVector)
                other_fftw => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into 1st argument of Fftw elementwise mult'
                call error(msgstr)
        end select

        select type(result)
            class is (FftwVector)
                result_fftw => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into 2nd argument of Fftw elementwise mult'
                call error(msgstr)
        end select

!        if (this%fftDim .ne. other%fftDim .or. this%fftDim .ne. result%fftDim) then
!            print ('A,I6,A,I6,A,I6)','Error: FFT vector addition dimension mismatch: ',&
!                this%fftDim,' / ',other%fftDim,' / ',result%fftDim
!            stop
!        end if
!
!        if (this%L .ne. other%L .or. this%M .ne. other%M .or. this%N .ne. other%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',other%L,', ',other%M,', ',other%N,')'
!            stop
!        end if
!
!        if (this%L .ne. result%L .or. this%M .ne. result%M .or. this%N .ne. result%N) then
!            print '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)','Error: Incompatible dimensions: (',&
!                & this%L,', ',this%M,', ',this%N,') vs (',result%L,', ',result%M,', ',result%N,')'
!            stop
!        end if

        select case(this%fftDim)
            case (1)
                ! * here is for complex elementwise multiplication
                result_fftw%data1d = this%data1d*other_fftw%data1d
            case (2)
                result_fftw%data2d = this%data2d*other_fftw%data2d
            case (3)
                result_fftw%data3d = this%data3d*other_fftw%data3d
        end select
    end subroutine

    function getClassName(this) result(className)
        implicit none

        class(FftwVector) :: this
        character(len=256)    :: className

        className = 'FftwVector'
    end function

    function clone(this) result(result_ptr)
        implicit none

        class(FftwVector) :: this
        class(AbstractVector), pointer :: result_ptr

        class(FftwVector), pointer :: cloneptr

        allocate(cloneptr)

        select case(this%fftDim)
            case (1)
                call cloneptr%fftwVectorConstructor_1d(this%N)
                cloneptr%data1d = this%data1d
            case (2)
                call cloneptr%fftwVectorConstructor_2d(this%M, this%N)
                cloneptr%data2d = this%data2d
            case (3)
                call cloneptr%fftwVectorConstructor_3d(this%L, this%M, this%N)
                cloneptr%data3d = this%data3d
        end select

        result_ptr => cloneptr
    end function
end module
