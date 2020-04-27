module assimStrategyFactory_mod

    use assimilationStrategy_mod
    use ndvarAssimilationStrategy_mod
    use mpiUtils_mod

    implicit none

    private

    ! Observation operators
    character(len=256), parameter, public :: NDVAR_STRATEGY = 'NDVar'

    public :: getAssimilationStrategy

    contains

    function getAssimilationStrategy(strategyName) result(strategy)
        implicit none

        character(len=256), intent(in)       :: strategyName
        class(AssimilationStrategy), pointer :: strategy

        class(NDVarAssimilationStrategy), pointer :: ndvarStrategy

        select case(strategyName)
            case (NDVAR_STRATEGY)
                allocate(ndvarStrategy)
                call ndvarStrategy%nDVarAssimilationStrategyConstructor()
                strategy => ndvarStrategy
            case default
                write(msgstr,*) 'Unknown assimilation strategy ',trim(strategyName)
                call error(msgstr)
        end select
    end function
end module
