module observationBundle_mod
    use observation_mod
    use observationOperator_mod
    use abstractVectorOperator_mod

    implicit none

    private

    type, abstract, public :: observationBundle
        !private
            integer :: bundleSize

        contains
            procedure(getObservation_abs),  deferred :: getObservation
            procedure(getObsOp_abs),        deferred :: getObsOp
            procedure(getObsErrOp_abs),     deferred :: getObsErrOp

            procedure :: getBundleSize

            procedure :: observationBundleConstructor
            procedure :: observationBundleDestructor
    end type

    abstract interface
        function getBundleSize_abs(this) result(bsize)
            import ObservationBundle

            class(ObservationBundle)               :: this
            integer                                :: bsize
        end function

        function getObservation_abs(this,bundleNum) result(obs)
            import ObservationBundle
            import Observation

            class(ObservationBundle)               :: this
            class(Observation),            pointer :: obs
            integer, intent(in)                    :: bundleNum
        end function

        function getObsOp_abs(this,bundleNum) result(obsOp)
            import ObservationBundle
            import ObservationOperator

            class(ObservationBundle)               :: this
            class(ObservationOperator),    pointer :: obsOp
            integer, intent(in)                    :: bundleNum

        end function

        function getObsErrOp_abs(this,bundleNum) result(obsErrOp)
            import ObservationBundle
            import AbstractVectorOperator

            class(ObservationBundle)               :: this
            class(AbstractVectorOperator), pointer :: obsErrOp
            integer, intent(in)                    :: bundleNum
        end function
    end interface

    contains

    subroutine observationBundleConstructor(this,bsize)
        implicit none

        class(ObservationBundle) :: this
        integer, intent(in)      :: bsize

        this%bundleSize = bsize
    end subroutine

    subroutine observationBundleDestructor(this)
        implicit none

        class(ObservationBundle) :: this

    end subroutine

    function getBundleSize(this) result(bsize)
        implicit none

        class(ObservationBundle) :: this

        integer                           :: bsize

        bsize = this%bundleSize
    end function
end module
