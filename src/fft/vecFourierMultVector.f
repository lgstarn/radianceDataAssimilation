module vecFourierMultVector_mod

    use abstractVector_mod

    use, intrinsic :: iso_c_binding

    use linkedList_mod

    use fftwVector_mod

    use mpiUtils_mod

    implicit none

    private

    include 'fftw3.f03'

    type, public, extends(AbstractVector) :: VecFourierMultVector
        private
            integer :: nr                    ! number of eofs
            integer :: nx, ny                ! size of the domain

            class(LinkedList), pointer                     :: fftwVectors
            complex(C_DOUBLE_COMPLEX), pointer, contiguous :: data3d(:,:,:) ! data for all the fftwVectors
            type(C_PTR)                                    :: dataptr       ! C-style pointer to the data

        contains
            procedure :: vecFourierMultVectorConstructor

            procedure :: firstVector
            procedure :: nextVector
            procedure :: numVectors
            procedure :: get3DArrayPtr
            procedure :: set1DArray
            procedure :: get1DArray
            procedure :: get1DArrayPtr
            procedure :: getSize
            procedure :: add
            procedure :: subtract
            procedure :: scalarMultiply
            procedure :: elementwiseMultiply
            procedure :: clone
            procedure :: getClassName

            final :: vecFourierMultVectorDestructor ! clean up all allocated variables
    end type

    contains

    subroutine vecFourierMultVectorConstructor(this, nx, ny, nr)
        implicit none

        class(VecFourierMultVector) :: this

        integer, intent(in) :: nx, ny, nr

        class(FftwVector), pointer :: vec => NULL()
        class(*), pointer :: optr

        integer :: i

        this%fftwVectors => LinkedList()

        this%nx  = nx
        this%ny  = ny
        this%nr  = nr

        this%dataptr = fftw_alloc_complex(int(this%nx*this%ny*this%nr, C_SIZE_T))
        call c_f_pointer(this%dataptr, this%data3d, [this%nx,this%ny,this%nr])

        this%data3d = 0.0d0

        do i=1,this%nr
            allocate(vec)
            call vec%fftwVectorConstructor_data2d(this%nx,this%ny,c_loc(this%data3d(:,:,i)))

            optr => vec

            call this%fftwVectors%add(optr)

            nullify(vec)
        end do

        this%nr = nr
    end subroutine

    subroutine vecFourierMultVectorDestructor(this)
        implicit none

        type(VecFourierMultVector)  :: this

        class(FftwVector), pointer :: vec

        integer :: i

        if (associated(this%fftwVectors)) then
            call this%fftwVectors%deleteAll()
            deallocate(this%fftwVectors)
        end if

        call fftw_free(this%dataptr)
    end subroutine

    subroutine firstVector(this)
        implicit none

        class(VecFourierMultVector) :: this

        call this%fftwVectors%first()
    end subroutine

    function nextVector(this) result(vec)
        implicit none

        class(VecFourierMultVector) :: this
        class(FftwVector), pointer :: vec
        class(*), pointer :: f_ptr

        f_ptr => this%fftwVectors%currentValue()
        select type(f_ptr)
            class is (FftwVector)
                vec => f_ptr
                call this%fftwVectors%next()
        end select
    end function

    function numVectors(this) result(num)
        implicit none

        class(VecFourierMultVector) :: this
        class(FftwVector), pointer :: vec
        integer :: num

        num = this%fftwVectors%getListSize()
    end function

    function get3DArrayPtr(this) result(dptr)
        implicit none

        class(VecFourierMultVector) :: this
        class(FftwVector), pointer :: vec
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: dptr

        dptr => this%data3D
    end function

    subroutine set1DArray(this,array1d)
        implicit none

        class(VecFourierMultVector) :: this
        real(8), dimension(:) :: array1d

        class(FftwVector), pointer :: vec

        integer :: i, cursor_start, cursor_end

        cursor_start = 1

        call this%firstVector()

        do i=1,this%numVectors()
            vec => this%nextVector()
            cursor_end = cursor_start + 2*this%nx*this%ny - 1
            call vec%set1DArray(array1d(cursor_start:cursor_end))
            cursor_start = cursor_end + 1
        end do
    end subroutine

    subroutine get1DArray(this,array1d)
        implicit none

        class(VecFourierMultVector) :: this
        real(8), dimension(:) :: array1d

        class(FftwVector), pointer :: vec

        integer :: i, cursor_start, cursor_end

        cursor_start = 1

        call this%firstVector()

        do i=1,this%numVectors()
            vec => this%nextVector()
            cursor_end = cursor_start + 2*this%nx*this%ny - 1

            if (cursor_end > size(array1d)) then
                write(msgstr,*) 'Error: attempted to go past end of array: ',cursor_end,' / ',size(array1d)
                call print(msgstr)
            end if

            call vec%get1DArray(array1d(cursor_start:cursor_end))
            cursor_start = cursor_end + 1
            call this%fftwVectors%next()
        end do
    end subroutine

    function get1DArrayPtr(this) result(array1dPtr)
        implicit none

        class(VecFourierMultVector) :: this
        real(8), dimension(:), pointer :: array1dPtr

        call c_f_pointer(c_loc(this%data3d), array1dPtr, [this%getSize()])
    end function

    function getSize(this) result(vsize)
        implicit none

        class(VecFourierMultVector) :: this
        integer :: vsize

        vsize = 2*this%nx*this%ny*this%nr
    end function

    subroutine add(this,other,result)
        implicit none

        class(VecFourierMultVector) :: this
        class(AbstractVector), pointer :: other, result

        class(FftwVector), pointer :: vec_this
        class(AbstractVector), pointer :: vec_other, vec_result

        class(VecFourierMultVector), pointer :: other_vehf, result_vehf

        integer :: i

        select type(other)
            class is (VecFourierMultVector)
                other_vehf => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector add'
                call error(msgstr)
        end select

        select type(result)
            class is (VecFourierMultVector)
                result_vehf => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector add'
                call error(msgstr)
        end select

        call this%firstVector()
        call other_vehf%firstVector()
        call result_vehf%firstVector()

        do i=1,this%numVectors()
            vec_this   => this%nextVector()
            vec_other  => other_vehf%nextVector()
            vec_result => result_vehf%nextVector()
            call vec_this%add(vec_other, vec_result)
        end do
    end subroutine

    subroutine subtract(this,other,result)
        implicit none

        class(VecFourierMultVector) :: this
        class(AbstractVector),       pointer :: other, result

        class(VecFourierMultVector), pointer :: other_vehf, result_vehf

        class(FftwVector), pointer :: vec_this
        class(AbstractVector), pointer :: vec_other, vec_result

        integer :: i

        select type(other)
            class is (VecFourierMultVector)
                other_vehf => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector subtract'
                call error(msgstr)
        end select

        select type(result)
            class is (VecFourierMultVector)
                result_vehf => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector subtract'
                call error(msgstr)
        end select

        call this%firstVector()
        call other_vehf%firstVector()
        call result_vehf%firstVector()

        do i=1,this%numVectors()
            vec_this   => this%nextVector()
            vec_other  => other_vehf%nextVector()
            vec_result => result_vehf%nextVector()
            call vec_this%subtract(vec_other, vec_result)
        end do
    end subroutine

    subroutine scalarMultiply(this,alpha,result)
        implicit none

        class(VecFourierMultVector) :: this
        real(8) :: alpha
        class(AbstractVector), pointer :: result

        class(VecFourierMultVector), pointer :: result_vehf

        class(FftwVector), pointer :: vec_this
        class(AbstractVector), pointer :: vec_result

        integer :: i

        select type(result)
            class is (VecFourierMultVector)
                result_vehf => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector scalar'
                call error(msgstr)
        end select

        call this%firstVector()
        call result_vehf%firstVector()

        do i=1,this%numVectors()
            vec_this   => this%nextVector()
            vec_result => result_vehf%nextVector()
            call vec_this%scalarMultiply(alpha, vec_result)
        end do
    end subroutine

    subroutine elementwiseMultiply(this,other,result)
        implicit none

        class(VecFourierMultVector) :: this
        class(AbstractVector), pointer :: other, result

        class(FftwVector), pointer :: vec_this
        class(AbstractVector), pointer :: vec_other, vec_result

        class(VecFourierMultVector), pointer :: other_vehf, result_vehf

        integer :: i

        select type(other)
            class is (VecFourierMultVector)
                other_vehf => other
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector elementwise'
                call error(msgstr)
        end select

        select type(result)
            class is (VecFourierMultVector)
                result_vehf => result
            class default
                write(msgstr,*) 'Incompatible input vector type passed into VecFourierMultVector elementwise'
                call error(msgstr)
        end select

        call this%firstVector()
        call other_vehf%firstVector()
        call result_vehf%firstVector()

        do i=1,this%numVectors()
            vec_this   => this%nextVector()
            vec_other  => other_vehf%nextVector()
            vec_result => result_vehf%nextVector()
            call vec_this%elementwiseMultiply(vec_other, vec_result)
        end do
    end subroutine

    function getClassName(this) result(className)
        implicit none

        class(VecFourierMultVector) :: this
        character(len=256)    :: className

        className = 'VecFourierMultVector'
    end function

    function clone(this) result(cloneptr)
        implicit none

        class(VecFourierMultVector)          :: this
        class(AbstractVector), pointer       :: cloneptr

        class(VecFourierMultVector), pointer :: clonevec
        class(FftwVector), pointer           :: vec_this, vec_clone
        complex(C_DOUBLE_COMPLEX),   pointer :: dptr1(:,:)
        complex(C_DOUBLE_COMPLEX),   pointer :: dptr2(:,:)

        integer :: i

        allocate(clonevec)
        call clonevec%vecFourierMultVectorConstructor(this%nx, this%ny, this%nr)

        call this%firstVector()
        call clonevec%firstVector()

        do i=1,this%numVectors()
            vec_this  => this%nextVector()
            vec_clone => clonevec%nextVector()

            dptr1 => vec_clone%get2DPointer()
            dptr2 => vec_this%get2DPointer()

            dptr1(:,:) = dptr2(:,:)
        end do

        cloneptr => clonevec
    end function
end module
