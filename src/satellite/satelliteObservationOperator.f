module satelliteObservationOperator_mod
    use observationOperator_mod
    use observation_mod
    use dataSet_mod
    use atmos3dDataSet_mod
    use satellitePlatformInfo_mod
    use satelliteObservation_mod
    use mpiUtils_mod

    implicit none

    private

    type, abstract, public, extends(ObservationOperator) :: SatelliteObservationOperator
        private

        contains
            procedure :: forward
            procedure :: tangentLinear
            procedure :: adjoint
            procedure(doForward_abs), deferred :: doForward
            procedure(doTLM_abs),     deferred :: doTLM
            procedure(doAdjoint_abs), deferred :: doAdjoint
    end type

    abstract interface

        subroutine doForward_abs(this, input, obs, output)
            import Atmos3DDataSet
            import SatelliteObservation
            import SatelliteObservationOperator

            class(SatelliteObservationOperator)  :: this
            class(Atmos3DDataSet),    pointer :: input
            class(SatelliteObservation), pointer :: obs
            real(8), dimension(:,:),     pointer :: output
        end subroutine

        subroutine doTLM_abs(this, baseState, obs, deltaX, deltaY)
            import Atmos3DDataSet
            import SatelliteObservation
            import SatelliteObservationOperator

            class(SatelliteObservationOperator)  :: this
            class(Atmos3DDataSet),    pointer :: baseState
            class(SatelliteObservation), pointer :: obs
            class(Atmos3DDataSet),    pointer :: deltaX
            real(8), dimension(:,:),     pointer :: deltaY
        end subroutine

        subroutine doAdjoint_abs(this, baseState, obs, deltaY, deltaX)
            import Atmos3DDataSet
            import SatelliteObservation
            import SatelliteObservationOperator

            class(SatelliteObservationOperator)  :: this
            class(Atmos3DDataSet),    pointer :: baseState
            class(SatelliteObservation), pointer :: obs
            real(8), dimension(:,:),     pointer :: deltaY
            class(Atmos3DDataSet),    pointer :: deltaX
        end subroutine
    end interface

    contains

    subroutine forward(this, input, obs, output)
        implicit none

        class(SatelliteObservationOperator) :: this

        class(DataSet),       pointer :: input
        class(Observation),      pointer :: obs
        real(8), dimension(:,:), pointer :: output

        class(Atmos3DDataSet),    pointer :: inputAtmos
        class(SatelliteObservation), pointer :: obsSat

        select type(input)
            class is (Atmos3DDataSet)
                inputAtmos => input
            class default
                write(msgstr,*) 'Incompatible model class in ',this%getName()
                call error(msgstr)
        end select

        select type(obs)
            class is (SatelliteObservation)
                obsSat => obs
            class default
                write(msgstr,*) 'Incompatible obs class in ',this%getName()
                call error(msgstr)
        end select

        if (.not. associated(obsSat)) then
            write(msgstr,*) 'In obsop ',trim(this%getName()),' the output was not associated.'
            call error(msgstr)
        end if

        call this%doForward(inputAtmos, obsSat, output)
    end subroutine

    subroutine tangentLinear(this, baseState, obs, deltaX, deltaY)
        implicit none

        class(SatelliteObservationOperator) :: this

        class(DataSet),       pointer :: baseState
        class(Observation),      pointer :: obs
        class(DataSet),       pointer :: deltaX
        real(8), dimension(:,:), pointer :: deltaY

        class(Atmos3DDataSet),    pointer :: baseStateAtmos
        class(SatelliteObservation), pointer :: obsSat
        class(Atmos3DDataSet),    pointer :: deltaXAtmos

        select type(baseState)
            class is (Atmos3DDataSet)
                baseStateAtmos => baseState
            class default
                write(msgstr,*) 'Incompatible base state class in ',this%getName()
                call error(msgstr)
        end select

        select type(deltaX)
            class is (Atmos3DDataSet)
                deltaXAtmos => deltaX
            class default
                write(msgstr,*) 'Incompatible delta X class in ',this%getName()
                call error(msgstr)
        end select

        select type(obs)
            class is (SatelliteObservation)
                obsSat => obs
            class default
                write(msgstr,*) 'Incompatible obs class in ',this%getName()
                call error(msgstr)
        end select

        call this%doTLM(baseStateAtmos, obsSat, deltaXAtmos, deltaY)
    end subroutine

    subroutine adjoint(this, baseState, obs, deltaY, deltaX)
        implicit none

        class(SatelliteObservationOperator)  :: this

        class(DataSet),       pointer :: baseState
        class(Observation),      pointer :: obs
        real(8), dimension(:,:), pointer :: deltaY
        class(DataSet),       pointer :: deltaX

        class(Atmos3DDataSet),    pointer :: baseStateAtmos
        class(SatelliteObservation), pointer :: obsSat
        class(Atmos3DDataSet),    pointer :: deltaXAtmos

        select type(baseState)
            class is (Atmos3DDataSet)
                baseStateAtmos => baseState
            class default
                write(msgstr,*) 'Incompatible base state class in ',this%getName()
                call error(msgstr)
        end select

        select type(obs)
            class is (SatelliteObservation)
                obsSat => obs
            class default
                write(msgstr,*) 'Incompatible obs class in ',this%getName()
                call error(msgstr)
        end select

        select type(deltaX)
            class is (Atmos3DDataSet)
                deltaXAtmos => deltaX
            class default
                write(msgstr,*) 'Incompatible delta X class in ',this%getName()
                call error(msgstr)
        end select

        call this%doAdjoint(baseStateAtmos, obsSat, deltaY, deltaXAtmos)
    end subroutine
end module
