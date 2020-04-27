module scannedObservationBundle_mod

    use observation_mod
    use observationOperator_mod
    use scannedObservation_mod
    use satelliteObservation_mod
    use scannedObservationOperator_mod
    use satelliteObservationBundle_mod
    use satellitePlatformInfo_mod
    use platformInfo_mod

    implicit none

    private

    type, extends(SatelliteObservationBundle), abstract, public :: ScannedObservationBundle
        private

        contains
            procedure :: scannedObservationBundleConstructor

            procedure :: getSatelliteObservation
            procedure :: getObsOp

            procedure(getScannedObservation_abs),     deferred :: getScannedObservation
            procedure(getAntennaPatternObsOp_abs),    deferred :: getAntennaPatternObsOp

            procedure :: scannedObservationBundleDestructor ! clean up all allocated variables
    end type

    abstract interface

        function getScannedObservation_abs(this,bundleNum) result(obs_so)
            import ScannedObservationBundle
            import ScannedObservation

            class(ScannedObservationBundle)    :: this
            integer, intent(in)                :: bundleNum
            class(ScannedObservation), pointer :: obs_so
        end function

        function getAntennaPatternObsOp_abs(this,bundleNum) result(obsOp)
            import ScannedObservationBundle
            import ScannedObservationOperator

            class(ScannedObservationBundle)            :: this
            integer, intent(in)                        :: bundleNum
            class(ScannedObservationOperator), pointer :: obsOp
        end function
    end interface

    contains

    subroutine scannedObservationBundleConstructor(this,bundleSize)

        implicit none

        class(ScannedObservationBundle)  :: this
        integer, intent(in)              :: bundleSize

        call this%satelliteObservationBundleConstructor(bundleSize)
    end subroutine

    subroutine scannedObservationBundleDestructor(this)
        implicit none

        class(ScannedObservationBundle)  :: this

        call this%satelliteObservationBundleDestructor()
    end subroutine

    function getSatelliteObservation(this,bundleNum) result (obs)
        implicit none

        class(ScannedObservationBundle) :: this
        integer, intent(in)           :: bundleNum
        class(SatelliteObservation), pointer   :: obs

        obs => this%getScannedObservation(bundleNum)
    end function

    function getObsOp(this,bundleNum) result (obsOp)
        implicit none

        class(ScannedObservationBundle) :: this
        integer, intent(in)           :: bundleNum
        class(ObservationOperator), pointer   :: obsOp

        obsOp => this%getAntennaPatternObsOp(bundleNum)
    end function
end module
