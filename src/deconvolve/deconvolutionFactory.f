module deconvolutionFactory_mod

    use deconvolutionConstants_mod
    use firstGuesser_mod
    use atmos3dDataSet_mod
    !use interpFirstGuesser_mod
    use adjointAveFirstGuesser_mod
    use mpiUtils_mod

    implicit none

    private

    public :: getFirstGuesser

    contains

    function getFirstGuesser(fgName,modelState) result(fg)
        implicit none

        character(len=256),              intent(in) :: fgName
        class(FirstGuesser),             pointer    :: fg
        class(Atmos3dDataSet), optional, pointer    :: modelState

!        class(InterpFirstGuesser),     pointer :: interpFg
        class(AdjointAveFirstGuesser), pointer :: adjointAveFg

        select case(fgName)
!            case (INTERP_FG)
!                allocate(interpFg)
!                call interpFg%interpFirstGuesserConstructor()
!                fg => interpFg
            case (ADJOINT_AVE_FG)
                allocate(adjointAveFg)
                call adjointAveFg%adjointAveFirstGuesserConstructor(modelState)
                fg => adjointAveFg
            case default
                write(msgstr,*) 'Unknown first guesser ',trim(fgName)
                call error(msgstr)
        end select
    end function
end module
