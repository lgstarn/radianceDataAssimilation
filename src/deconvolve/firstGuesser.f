module FirstGuesser_mod
    use dataSet_mod
    use scannedObservationBundle_mod

    implicit none

    private

    type, abstract, public :: FirstGuesser
        contains
            procedure(populateFirstGuess_abs),  deferred :: populateFirstGuess
    end type

    abstract interface
        subroutine populateFirstGuess_abs(this,obsBundle,firstGuess)

            import FirstGuesser
            import DataSet
            import ScannedObservationBundle

            class(FirstGuesser)    :: this
            class(ScannedObservationBundle), pointer :: obsBundle
            class(DataSet),                  pointer :: firstGuess
        end subroutine
    end interface
end module
