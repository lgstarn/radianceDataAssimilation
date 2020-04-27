module penalty_mod
    use abstractVector_mod
    use abstractVectorOperator_mod

    implicit none

    private

    type, abstract, public :: Penalty
        contains
            procedure(applyPenaltyAbstract),        deferred :: applyPenalty
    end type

    abstract interface

        subroutine applyPenaltyAbstract(this, control, state, bhalf, penval, pengrad)
            import Penalty
            import AbstractVector
            import AbstractVectorOperator

            class(Penalty)                         :: this
            class(AbstractVector),         pointer :: control
            class(AbstractVector),         pointer :: state
            class(AbstractVectorOperator), pointer :: bhalf
            real(8),                   intent(out) :: penval
            real(8),         target, intent(inout) :: pengrad(:)
        end subroutine
    end interface
end module
