module AssimilationStrategy_mod
    use assimilationProblem_mod
    use parallelInfo_mod

    implicit none

    private

    public :: AssimilationStrategy

    type, abstract :: AssimilationStrategy
        contains
            procedure(assimilateAbstract), deferred :: assimilate
    end type

    abstract interface

        subroutine assimilateAbstract(this,pinfo,problem)
            import AssimilationStrategy
            import ParallelInfo
            import AssimilationProblem
            class(AssimilationStrategy)         :: this
            class(ParallelInfo),        pointer :: pinfo
            class(AssimilationProblem), pointer :: problem
        end subroutine
    end interface
end module
