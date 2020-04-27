module satelliteObservationBundle_mod

    use observation_mod
    use satelliteObservation_mod
    use observationBundle_mod
    use satellitePlatformInfo_mod
    use platformInfo_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(ObservationBundle), abstract, public :: SatelliteObservationBundle
        private

        contains
            procedure :: satelliteObservationBundleConstructor

            procedure :: getObservation

            procedure(getSatelliteObservation_abs), deferred :: getSatelliteObservation
            procedure(getTotalNChannels_abs),       deferred :: getTotalNChannels

            procedure :: getFullSetChanNum

            procedure :: satelliteObservationBundleDestructor ! clean up all allocated variables
    end type

    abstract interface
        function getSatelliteObservation_abs(this,bundleNum) result(obs_so)
            import SatelliteObservationBundle
            import SatelliteObservation

            class(SatelliteObservationBundle)    :: this
            integer, intent(in)                  :: bundleNum
            class(SatelliteObservation), pointer :: obs_so
        end function

        function getTotalNChannels_abs(this,bundleNum) result(nchan)
            import SatelliteObservationBundle

            class(SatelliteObservationBundle) :: this
            integer, intent(in)               :: bundleNum

            integer                           :: nchan
        end function

    end interface

    contains

    subroutine satelliteObservationBundleConstructor(this,bundleSize)

        implicit none

        class(SatelliteObservationBundle)  :: this
        integer, intent(in)                :: bundleSize

        call this%observationBundleConstructor(bundleSize)
    end subroutine

    subroutine satelliteObservationBundleDestructor(this)
        implicit none

        class(SatelliteObservationBundle)  :: this

        call this%observationBundleDestructor()
    end subroutine

    function getObservation(this,bundleNum) result (obs)
        implicit none

        class(SatelliteObservationBundle) :: this
        integer, intent(in)               :: bundleNum

        class(Observation), pointer       :: obs

        obs => this%getSatelliteObservation(bundleNum)
    end function

    function getFullSetChanNum(this,bundleRelChnum) result(fullSetChNum)
        implicit none
        class(SatelliteObservationBundle) :: this
        integer, intent(in)               :: bundleRelChnum

        integer                           :: fullSetChNum

        class(SatelliteObservation), pointer :: satObs

        integer :: i
        integer :: bundleNum
        integer :: offsetIndex

        integer, dimension(:), pointer :: chsubset

        bundleNum = -1

        do i=1,this%getBundleSize()
            if (bundleRelChnum <= this%getTotalNChannels(i)) then
                bundleNum = i
                exit ! the loop
            end if
        end do

        if (bundleNum == -1) then
            write(msgstr,*) 'In getOffsetChanNum, chnum out of range:',bundleRelChnum,&
                &this%getTotalNChannels(this%getBundleSize())
            call print(msgstr)
        end if

        satObs => this%getSatelliteObservation(bundleNum)

        offsetIndex = bundleRelChnum - satObs%getChannelOffset()

        chsubset => satObs%getChannelSubset()

        if (offsetIndex < 1 .or. offsetIndex > size(chsubset,1)) then
            write(msgstr,*) 'In getOffsetChanNum, offsetIndex was larger than satObs%chsubset:',&
                &bundleRelChnum,satObs%getChannelOffset(),offsetIndex,size(chsubset,1)
            call print(msgstr)
            write(msgstr,*) 'chsubset:',chsubset
            call error(msgstr)
        end if

        fullSetChNum = chsubset(offsetIndex)
    end function
end module
