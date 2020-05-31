module conicalScanningObservation_mod

    use parallelInfo_mod

    use iso_fortran_env

    use observation_mod
    use scannedObservation_mod

    use platformInfo_mod
    use satellitePlatformInfo_mod

    use dataSet_mod
    use dataGroup_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataDimension_mod
    use dataArrayReader_mod
    use dataArrayWriter_mod

    use datetime_mod
    use mpiUtils_mod

    implicit none

    private

    character(*), public, parameter :: TB_VAR_NAME      = 'Brightness_Temperatures'
    character(*), public, parameter :: LAT_VAR_NAME     = 'Latitude'
    character(*), public, parameter :: LON_VAR_NAME     = 'Longitude'

    type, extends(ScannedObservation), public :: ConicalScanningObservation
        !private
            ! Spin-frequency in RPM
            real(8) :: spinFrequency

        contains
            procedure :: loadConicalScanningObservation

            procedure :: clone
            procedure :: cloneConicalScanningObs

            procedure :: conicalScanningObservationConstructor
            final     :: conicalScanningObservationDestructor
    end type

    contains

    subroutine conicalScanningObservationConstructor(this,platform,reader,&
        &fovAlong,fovCross,intTime,choffset,spinFrequency,obsErr,uniquechoffset)

        implicit none

        class(ConicalScanningObservation)           :: this

        class(SatellitePlatformInfo),   pointer     :: platform
        class(DataArrayReader),         pointer     :: reader

        real(8), dimension(:),          intent(in)  :: fovAlong
        real(8), dimension(:),          intent(in)  :: fovCross
        real(8), dimension(:),          intent(in)  :: intTime
        real(8),                        intent(in)  :: spinFrequency
        integer,                        intent(in)  :: choffset
        real(8), dimension(:),          intent(in)  :: obsErr

        !integer,          optional,     intent(in)  :: naux
        integer,          optional,     intent(in)  :: uniquechoffset

!        logical,          optional,     intent(in)  :: useQC
!        real(8),          optional,     intent(in)  :: minLat
!        real(8),          optional,     intent(in)  :: maxLat
!        real(8),          optional,     intent(in)  :: minLon
!        integer,          optional,     intent(in)  :: minScan
!        integer,          optional,     intent(in)  :: maxScan
!        real(8),          optional,     intent(in)  :: maxLon
!        type(datetime),   optional,     intent(in)  :: cdate
!        type(timedelta),  optional,     intent(in)  :: maxTimeDiff

        this%spinFrequency = spinFrequency

        call this%scannedObservationConstructor(platform,reader,&
            &fovAlong,fovCross,intTime,choffset,obsErr,uniquechoffset)

!        call this%scannedObservationConstructor(platform,reader,writer,lat,lon,&
!            &scLat,scLon,nchannels,tb,chsubset,fovAlong,fovCross,scanDistance,intTime,choffset,&
!            &obsErr,year,month,day,hour,minute,second,naux,uniquechoffset)

            !&minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff,&
    end subroutine

    subroutine conicalScanningObservationDestructor(this)
        implicit none

        type(ConicalScanningObservation)  :: this

        ! call this%scannedObservationDestructor()
    end subroutine

    subroutine loadConicalScanningObservation(this,pinfo,tbVar,latVar,lonVar,&
        & scLatVar,scLonVar,yearVar,monthVar,dayVar,hourVar,minuteVar,       &
        & secondVar)

        implicit none

        class(ConicalScanningObservation)  :: this

        class(ParallelInfo),            pointer    :: pinfo

        ! real(4) nchan, npix, nscan
        class(DataVariable),            pointer    :: tbVar
        ! real(4) npix, nscan
        class(DataVariable),            pointer    :: latVar
        ! real(4) npix, nscan
        class(DataVariable),            pointer    :: lonVar
        ! real(4) nscan
        class(DataVariable),  optional, pointer    :: scLatVar
        ! real(4) nscan
        class(DataVariable),  optional, pointer    :: scLonVar
        ! integer nscan
        class(DataVariable),  optional, pointer    :: yearVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: monthVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: dayVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: hourVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: minuteVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: secondVar

        call this%loadScannedObservation(pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
            & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar)
    end subroutine

    function cloneConicalScanningObs(this,shallow,copyData) result(csoptr)
        implicit none

        class(ConicalScanningObservation), target :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataSet),            pointer :: dsPtr
        class(DataGroup),          pointer :: dgPtr
        class(ConicalScanningObservation), pointer :: csoptr

        logical :: isShallow
        logical :: doCopy

        allocate(csoptr)
        call csoptr%conicalScanningObservationConstructor(this%getSatellitePlatform(),    &
            & this%reader%clone(),this%getFovAlongPointer(),this%getFovCrossPointer(),    &
            & this%getIntegrationTimePointer(),this%getChannelOffset(),this%spinFrequency,&
            & this%getObservationError(),this%getUniqueChannelOffset())

        if (present(shallow)) then
            isShallow = shallow
        else
            isShallow = .false.
        end if

        if (present(copyData)) then
            doCopy = copyData
        else
            doCopy = .false.
        end if

        if (.not. isShallow) then
            dsPtr => csoptr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(ConicalScanningObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),            pointer :: dsPtr
        class(ConicalScanningObservation), pointer :: csoptr

        csoptr => this%cloneConicalScanningObs(shallow,copyData)
        dsPtr => csoptr
    end function
end module
