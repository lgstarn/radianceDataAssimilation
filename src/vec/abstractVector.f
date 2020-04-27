module abstractVector_mod
    implicit none

    private

    type, abstract, public :: AbstractVector
        contains
            procedure(set1DArrayAbstract),          deferred :: set1DArray
            procedure(get1DArrayAbstract),          deferred :: get1DArray
            procedure(get1DArrayPtrAbstract),       deferred :: get1DArrayPtr
            procedure(addAbstract),                 deferred :: add
            procedure(subtractAbstract),            deferred :: subtract
            procedure(scalarMultiplyAbstract),      deferred :: scalarMultiply
            procedure(elementwiseMultiplyAbstract), deferred :: elementwiseMultiply
            procedure(getSizeAbstract),             deferred :: getSize
            procedure(getClassName),                deferred :: getClassName

            ! abstract function to create a copy of this object
            procedure(cloneAbstract), deferred :: clone
    end type

    abstract interface

        ! Set the vector's internal representation from a one dimensional form of the data
        subroutine set1DArrayAbstract(this,array1d)
            import AbstractVector
            class(AbstractVector) :: this
            real(8), dimension(:) :: array1d
        end subroutine

        ! Put the vector's internal representation into a one dimensional form of the data
        ! Note this copies the data and requires array1d be allocated and have the correct size
        subroutine get1DArrayAbstract(this,array1d)
            import AbstractVector
            class(AbstractVector) :: this
            real(8), dimension(:) :: array1d
        end subroutine

        ! Return a direct pointer to a 1D vector representation of this data so that the user can
        ! read and write directly to the memory of this vector. If the subclass cannot implement this,
        ! it should set array1d to null.
        function get1DArrayPtrAbstract(this) result(array1d)
            import AbstractVector
            class(AbstractVector)          :: this
            real(8), dimension(:), pointer :: array1d
        end function

        subroutine addAbstract(this,other,result)
            import AbstractVector
            class(AbstractVector)          :: this
            class(AbstractVector), pointer :: other, result
        end subroutine

        subroutine subtractAbstract(this,other,result)
            import AbstractVector
            class(AbstractVector)          :: this
            class(AbstractVector), pointer :: other, result
        end subroutine

        subroutine scalarMultiplyAbstract(this,alpha,result)
            import AbstractVector
            class(AbstractVector)          :: this
            real(8)                        :: alpha
            class(AbstractVector), pointer :: result
        end subroutine

        subroutine elementwiseMultiplyAbstract(this,other,result)
            import AbstractVector
            class(AbstractVector)          :: this
            class(AbstractVector), pointer :: other, result
        end subroutine

        function getClassName(this) result(className)
            import AbstractVector
            class(AbstractVector) :: this
            character(len=256)    :: className
        end function

        function getSizeAbstract(this) result(vecsize)
            import AbstractVector
            class(AbstractVector) :: this
            integer               :: vecsize
        end function

        function cloneAbstract(this) result(newptr)
            import AbstractVector
            class(AbstractVector) :: this
            class(AbstractVector), pointer :: newptr
        end function
    end interface
end module
