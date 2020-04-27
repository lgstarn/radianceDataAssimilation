module AssimilationStrategy_mod
    use assimilationProblem_mod

    implicit none

    private

    public :: AssimilationStrategy

    type, abstract :: AssimilationStrategy
        contains
            procedure(assimilateAbstract), deferred :: assimilate
    end type

    abstract interface

        subroutine assimilateAbstract(this,problem)
            import AssimilationStrategy
            import AssimilationProblem
            class(AssimilationStrategy)         :: this
            class(AssimilationProblem), pointer :: problem
        end subroutine
    end interface
end module
