module abstractVectorOperator_mod
    use abstractVector_mod

    implicit none

    private

    type, abstract, public :: AbstractVectorOperator
        contains
            procedure(applyOperator_abs),        deferred :: applyOperator
            procedure(applyTranspose_abs),       deferred :: applyTranspose
            procedure(applyOperatorAndAdd_abs),  deferred :: applyOperatorAndAdd
            procedure(applyTransposeAndAdd_abs), deferred :: applyTransposeAndAdd
    end type

    abstract interface
        subroutine applyOperator_abs(this, in, out)
            import AbstractVectorOperator
            import AbstractVector

            class(AbstractVectorOperator)  :: this
            class(AbstractVector), pointer :: in
            class(AbstractVector), pointer :: out
        end subroutine

        subroutine applyTranspose_abs(this, in, out)
            import AbstractVectorOperator
            import AbstractVector

            class(AbstractVectorOperator)  :: this
            class(AbstractVector), pointer :: in
            class(AbstractVector), pointer :: out
        end subroutine

        subroutine applyOperatorAndAdd_abs(this, in, out, c)
            import AbstractVectorOperator
            import AbstractVector

            class(AbstractVectorOperator)  :: this
            class(AbstractVector), pointer :: in
            class(AbstractVector), pointer :: out
            real(8), intent(in)            :: c
        end subroutine

        subroutine applyTransposeAndAdd_abs(this, in, out, c)
            import AbstractVectorOperator
            import AbstractVector

            class(AbstractVectorOperator)  :: this
            class(AbstractVector), pointer :: in
            class(AbstractVector), pointer :: out
            real(8), intent(in)            :: c
        end subroutine

    end interface

end module
