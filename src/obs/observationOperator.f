module observationOperator_mod
    use dataSet_mod
    use observation_mod
    use obsQcCodes_mod

    implicit none

    private

    type, abstract, public :: ObservationOperator
        contains
            procedure(getName_abs),       deferred :: getName
            procedure(forward_abs),       deferred :: forward
            procedure(tangentLinear_abs), deferred :: tangentLinear
            procedure(adjoint_abs),       deferred :: adjoint
    end type

    abstract interface
        function getName_abs(this) result(name)
            import ObservationOperator

            class(ObservationOperator) :: this
            character(128)             :: name
        end function

        subroutine forward_abs(this, input, obs, output)
            import DataSet
            import Observation
            import ObservationOperator

            class(ObservationOperator)          :: this
            class(DataSet),             pointer :: input
            class(Observation),         pointer :: obs
            real(8), dimension(:,:),    pointer :: output
        end subroutine

        subroutine tangentLinear_abs(this, baseState, obs, deltaX, deltaY)
            import DataSet
            import Observation
            import ObservationOperator

            class(ObservationOperator)          :: this
            class(DataSet),             pointer :: baseState
            class(Observation),         pointer :: obs
            class(DataSet),             pointer :: deltaX
            real(8), dimension(:,:),    pointer :: deltaY
        end subroutine

        subroutine adjoint_abs(this, baseState, obs, deltaY, deltaX)
            import DataSet
            import Observation
            import ObservationOperator

            class(ObservationOperator)          :: this
            class(DataSet),             pointer :: baseState
            class(Observation),         pointer :: obs
            real(8), dimension(:,:),    pointer :: deltaY
            class(DataSet),             pointer :: deltaX
        end subroutine
    end interface
end module
