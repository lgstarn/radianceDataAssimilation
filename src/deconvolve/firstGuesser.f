module FirstGuesser_mod
    use dataSet_mod
    use parallelInfo_mod
    use scannedObservationBundle_mod

    implicit none

    private

    type, abstract, public :: FirstGuesser
        contains
            procedure(populateFirstGuess_abs),  deferred :: populateFirstGuess
    end type

    abstract interface
        subroutine populateFirstGuess_abs(this,pinfo,obsBundle,firstGuess)

            import FirstGuesser
            import ParallelInfo
            import DataSet
            import ScannedObservationBundle

            class(FirstGuesser)                      :: this
            class(ParallelInfo),             pointer :: pinfo
            class(ScannedObservationBundle), pointer :: obsBundle
            class(DataSet),                  pointer :: firstGuess
        end subroutine
    end interface
end module
