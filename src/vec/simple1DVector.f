module simple1DVector_mod
    use abstractVector_mod

    implicit none

    private

    type, extends(AbstractVector), public :: Simple1DVector
        private
            logical,                        private :: alloced
            real(8), dimension(:), pointer, private :: data1d
            integer,                        private :: ndata

        contains
            procedure :: simple1DVectorConstructor
            procedure :: simple1DVectorConstructor_data

            procedure :: set1DArray
            procedure :: get1DArray
            procedure :: get1DArrayPtr
            procedure :: add
            procedure :: subtract
            procedure :: scalarMultiply
            procedure :: elementwiseMultiply
            procedure :: getClassName
            procedure :: getSize
            procedure :: clone

            final :: simple1DVectorDestructor ! clean up all allocated variables
    end type

    contains

    subroutine simple1DVectorConstructor(this,ndata)
        implicit none

        class(Simple1DVector) :: this
        integer, intent(in)   :: ndata

        allocate(this%data1d(ndata))

        this%ndata = ndata
        this%alloced = .true.
    end subroutine

    subroutine simple1DVectorConstructor_data(this,data)
        implicit none

        class(Simple1DVector) :: this
        real(8), dimension(:), pointer :: data

        this%ndata = size(data)
        this%data1d => data
        this%alloced = .false.
    end subroutine

    subroutine simple1DVectorDestructor(this)
        implicit none

        type(Simple1DVector)  :: this

        if (this%alloced .and. associated(this%data1d)) then
            deallocate(this%data1d)
        end if
    end subroutine

    subroutine set1DArray(this,array1d)
        implicit none

        class(Simple1DVector)   :: this
        real(8), dimension(:) :: array1d

        this%data1d(1:this%ndata) = array1d(1:this%ndata)
    end subroutine

    subroutine get1DArray(this,array1d)
        implicit none

        class(Simple1DVector)   :: this
        real(8), dimension(:) :: array1d

        array1d(1:this%ndata) = this%data1d(1:this%ndata)
    end subroutine

    function get1DArrayPtr(this) result(array1d)
        implicit none

        class(Simple1DVector)            :: this
        real(8), dimension(:), pointer :: array1d

        array1d => this%data1d
    end function

    subroutine add(this,other,result)
        implicit none

        class(Simple1DVector)          :: this
        class(AbstractVector), pointer :: other
        class(AbstractVector), pointer :: result

        real(8), dimension(:), pointer :: a1, a2, a3

        a1 =>   this%get1DArrayPtr()
        a2 =>  other%get1DArrayPtr()
        a3 => result%get1DArrayPtr()

        a3(:) = a1(:) + a2(:)
    end subroutine

    subroutine subtract(this,other,result)
        implicit none

        class(Simple1DVector)          :: this
        class(AbstractVector), pointer :: other
        class(AbstractVector), pointer :: result

        real(8), dimension(:), pointer :: a1, a2, a3

        a1 =>   this%get1DArrayPtr()
        a2 =>  other%get1DArrayPtr()
        a3 => result%get1DArrayPtr()

        a3(:) = a1(:) - a2(:)
    end subroutine

    subroutine scalarMultiply(this,alpha,result)
        implicit none

        class(Simple1DVector)          :: this
        real(8)                        :: alpha
        class(AbstractVector), pointer :: result

        real(8), dimension(:), pointer :: a1, a2

        a1 =>   this%get1DArrayPtr()
        a2 => result%get1DArrayPtr()

        a2(:) = alpha*a1(:)
    end subroutine

    subroutine elementwiseMultiply(this,other,result)
        implicit none

        class(Simple1DVector)          :: this
        class(AbstractVector), pointer :: other
        class(AbstractVector), pointer :: result

        real(8), dimension(:), pointer :: a1, a2, a3

        a1 =>   this%get1DArrayPtr()
        a2 =>  other%get1DArrayPtr()
        a3 => result%get1DArrayPtr()

        a3(:) = a1(:)*a2(:)
    end subroutine

    function getSize(this) result(vecsize)
        implicit none

        class(Simple1DVector) :: this
        integer             :: vecsize

        vecsize = this%ndata
    end function

    function getClassName(this) result(className)
        implicit none

        class(Simple1DVector) :: this
        character(len=256)    :: className

        className = 'Simple1DVector'
    end function

    function clone(this) result(newptr)
        implicit none

        class(Simple1DVector) :: this
        class(AbstractVector), pointer :: newptr

        class(Simple1DVector), pointer :: other

        allocate(other)

        call other%simple1DVectorConstructor(this%ndata)
        other%data1d(:) = this%data1d(:)

        newptr => other
    end function
end module
