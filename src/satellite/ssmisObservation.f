module ssmisObservation_mod

    use parallelInfo_mod

    use conicalScanningObservation_mod

    use platformInfo_mod

    use satelliteObservation_mod
    use satellitePlatformInfo_mod

    use dataSet_mod
    use dataGroup_mod
    use dataVariable_mod
    use dataDimension_mod

    use radianceAssimilationConstants_mod

    use dataArrayReader_mod
    use hdfDataArrayReader_mod

    use datetime_mod
    use geodesic_mod
    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    type, extends(ConicalScanningObservation), public :: SsmisObservation
        !private

        contains
            procedure :: ssmisObservationConstructor
            final     :: ssmisObservationDestructor

            procedure :: cloneSsmisObs
            procedure :: clone
    end type

    contains

    subroutine ssmisObservationConstructor(this,pinfo,filename,groupNum,fnumber)
        ! &minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)

        implicit none

        class(SsmisObservation)            :: this

        class(ParallelInfo),    pointer    :: pinfo
        character(len=*),       intent(in) :: filename
        integer,                intent(in) :: groupNum
        integer,                intent(in) :: fnumber

!        real(8), intent(in), optional :: minLat
!        real(8), intent(in), optional :: maxLat
!        real(8), intent(in), optional :: minLon
!        real(8), intent(in), optional :: maxLon
!        integer, intent(in), optional :: minScan
!        integer, intent(in), optional :: maxScan
!        type(datetime),      optional :: cdate
!        type(timedelta),     optional :: maxTimeDiff

        class(SatellitePlatformInfo), pointer :: satellitePlatform

        character(3) :: grp

        real(4), dimension(:,:), pointer :: lat
        real(4), dimension(:,:), pointer :: lon
        real(4), dimension(:),   pointer :: scLat
        real(4), dimension(:),   pointer :: scLon

        !real(4), dimension(:,:),   pointer :: quality
        !real(4), dimension(:,:,:), pointer :: incidenceAngle

        integer                           :: nchannels
        integer,    dimension(:), pointer :: chsubset

        integer,    dimension(:), pointer :: year
        integer(1), dimension(:), pointer :: month
        integer(1), dimension(:), pointer :: day
        integer(1), dimension(:), pointer :: hour
        integer(1), dimension(:), pointer :: minute
        integer(1), dimension(:), pointer :: second

        real(4), dimension(:,:,:), pointer :: tb
        real(4), dimension(:,:,:), pointer :: tb2

        real(8), dimension(:), pointer :: fovAlong
        real(8), dimension(:), pointer :: fovCross
        real(8), dimension(:), pointer :: intTime
        integer                        :: choffset
        real(8)                        :: spinFrequency = 31.6
        real(8), dimension(:), pointer :: obsErr

        integer :: i, j

        real(8) :: s12, azi1, azi2, s12min

        class(DataDimension), pointer :: pixDim, scanDim, chanDim

        class(DataVariable),  pointer :: tbVar
        class(DataVariable),  pointer :: latVar
        class(DataVariable),  pointer :: lonVar
        class(DataVariable),  pointer :: scLatVar
        class(DataVariable),  pointer :: scLonVar
        class(DataVariable),  pointer :: yearVar
        class(DataVariable),  pointer :: monthVar
        class(DataVariable),  pointer :: dayVar
        class(DataVariable),  pointer :: hourVar
        class(DataVariable),  pointer :: minuteVar
        class(DataVariable),  pointer :: secondVar

        class(HdfDataArrayReader), pointer :: hdfReader
        class(DataArrayReader),    pointer :: reader

        character(:), allocatable :: latVarName

        if (groupNum .eq. 1) then
            nchannels = 3
            choffset = 0

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))
            allocate(chsubset(nchannels))

            ! (19V 19H 22V)
            chsubset = (/ 13, 12, 14 /)
            ! Instantaneous half-power of the antenna along-direction FOV bandwidth, in km, of SSMIS channels
            fovAlong = (/ 70.1, 70.1, 70.1 /)
            ! Instantaneous half-power of the antenna cross-direction FOV bandwidth, in km, of SSMIS channels
            fovCross = (/ 42.4, 42.4, 42.4 /)
            ! Integration times for each channel in ms
            intTime  = (/ 4.22, 4.22, 4.22 /)
            ! Noise-equivalent temperature (K)
            obsErr   = (/ 0.34, 0.34, 0.45 /)
        else if (groupNum .eq. 2) then
            nchannels = 2
            choffset = 3

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))
            allocate(chsubset(nchannels))

            ! (37V 37H)
            chsubset = (/ 16, 15 /)
            ! Instaneous half-power of the antenna along-direction FOV bandwidth, in km, of SSMIS channels
            fovAlong = (/44.2, 44.2 /)
            ! Instaneous half-power of the antenna cross-direction FOV bandwidth, in km, of SSMIS channels
            fovCross = (/27.5, 27.5 /)
            ! Integration times for each channel in ms
            intTime = (/ 4.22, 4.22 /)
            ! Noise-equivalent temperature (K)
            obsErr = (/0.24, 0.24/)
        else if (groupNum .eq. 3) then
            nchannels = 0 !1 !4
            choffset = 5

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))
            allocate(chsubset(nchannels))

            ! (150H 183+/-1H 183+/-3H 183+/-7H)
            !chsubset = (/ 8 /)

!            allocate(tb2(nchannels,size(tb,2),size(tb,3)))
!
!            write(msgstr,*) 'Switching SSMIS S3 from size ',size(tb,1),size(tb,2),size(tb,3),'to',&
!                &nchannels,size(tb,2),size(tb,3)
!            call print(msgstr)
!
!            !tb2(1,:,:) = tb(1,:,:)
!
!            deallocate(tb)
!            allocate(tb(size(tb2,1),size(tb2,2),size(tb2,3)))
!            tb = tb2
!            deallocate(tb2)

            ! Instaneous half-power of the antenna along-direction FOV bandwidth, in km, of SSMIS channels
            !fovAlong = (/14.4/)!, 14, 14, 14/)
            ! Instaneous half-power of the antenna cross-direction FOV bandwidth, in km, of SSMIS channels
            !fovCross = (/13.1/)!, 13, 13, 13/)
            ! Integration times for each channel in ms
            !intTime = (/ 4.22/)!, 4.22, 4.22, 4.22/)
            ! Noise-equivalent temperature (K)
            !obsErr = (/0.53/)!, 1.2, 1.0, 1.25/)
        else if (groupNum .eq. 4) then
            nchannels = 2
            choffset = 5 !6 !9

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))
            allocate(chsubset(nchannels))

            ! (91V 91H)
            chsubset = (/ 17,  18 /)
            ! Instaneous half-power of the antenna along-direction FOV bandwidth, in km, of SSMIS channels
            fovAlong = (/14.4, 14.4/)
            ! Instaneous half-power of the antenna cross-direction FOV bandwidth, in km, of SSMIS channels
            fovCross = (/13.1, 13.1/)
            ! Integration times for each channel in ms
            intTime = (/ 4.22, 4.22/)
            ! Noise-equivalent temperature (K)
            obsErr  = (/ 0.19, 0.19/)
        else
            write(msgstr,*) 'Unknown SSMIS group number ',groupNum
            call error(msgstr)
        end if

        allocate(satellitePlatform)
        if (fnumber .eq. 16) then
            call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_SSMIS_F16,&
                &'F16','SSMIS',nchannels,chsubset)
        elseif (fnumber .eq. 17) then
            call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_SSMIS_F17,&
                &'F17','SSMIS',nchannels,chsubset)
        elseif (fnumber .eq. 18) then
            call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_SSMIS_F18,&
                &'F18','SSMIS',nchannels,chsubset)
        elseif (fnumber .eq. 19) then
            call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_SSMIS_F19,&
                &'F19','SSMIS',nchannels,chsubset)
        else
            write(msgstr,*) 'Unknown SSMIS platform number',fnumber
            call error(msgstr)
        end if

        allocate(hdfReader)
        call hdfReader%hdfDataArrayReaderConstructor(filename,littleEndian=.true.)
        reader => hdfReader

        print *,'Now calling SSMIS constructor'
        call barrier()

        call this%conicalScanningObservationConstructor(satellitePlatform,&
            & reader,fovAlong=fovAlong,fovCross=fovCross,intTime=intTime, &
            & choffset=choffset,spinFrequency=spinFrequency,obsErr=obsErr)

        print *,'Finished calling SSMIS constructor'

        write(grp,'(A,I1)') '/S',groupNum

        latVarName = grp // "/Latitude"

        pixDim  => this%loadDimensionFromVariable(pinfo,PIXELS_DIM_NAME,1,latVarName)
        scanDim => this%loadDimensionFromVariable(pinfo,SCANS_DIM_NAME, 2,latVarName)
        chanDim => this%addDimensionByName(CHANS_DIM_NAME,nchannels)

        latVar    => this%loadVariable(pinfo,LAT_VAR_NAME, lat, &
            & pixDim, scanDim, latVarName)

        lonVar    => this%loadVariable(pinfo,LON_VAR_NAME, lon, &
            & pixDim, scanDim, grp//"/Longitude")

        scLatVar  => this%loadVariable(pinfo, SC_LAT_VAR_NAME, scLat, &
            & scanDim, grp//"/SCstatus/SClatitude")

        scLonVar  => this%loadVariable(pinfo, SC_LON_VAR_NAME, scLon, &
            & scanDim, grp//"/SCstatus/SClongitude")

        tbVar => this%loadVariable(pinfo, TB_VAR_NAME, tb, &
            & chanDim, pixDim, scanDim, grp//"/Tc")

        yearVar   => this%loadVariable(pinfo,YEAR_VAR_NAME, year, &
            & scanDim, grp//"/ScanTime/Year")

        monthVar  => this%loadVariable(pinfo, MONTH_VAR_NAME, month, &
            & scanDim, grp//"/ScanTime/Month")

        dayVar    => this%loadVariable(pinfo, DAY_VAR_NAME, day, &
            & scanDim, grp//"/ScanTime/DayOfMonth")

        hourVar   => this%loadVariable(pinfo, HOUR_VAR_NAME, hour, &
            & scanDim, grp//"/ScanTime/Hour")

        minuteVar => this%loadVariable(pinfo, MINUTE_VAR_NAME, minute, &
            & scanDim, grp//"/ScanTime/Minute")

        secondVar => this%loadVariable(pinfo, SECOND_VAR_NAME, second, &
            & scanDim, grp//"/ScanTime/Second")

        call this%loadConicalScanningObservation(pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
            & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar)
    end subroutine

    subroutine ssmisObservationDestructor(this)
        implicit none

        type(SsmisObservation)  :: this

        ! called automatically
        ! call this%conicalScanningObservationDestructor()
    end subroutine

    function cloneSsmisObs(this,shallow,copyData) result(soptr)
        implicit none

        class(SsmisObservation), target :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataSet),          pointer :: dsPtr
        class(DataGroup),        pointer :: dgPtr
        class(SsmisObservation), pointer :: soptr

        logical :: isShallow
        logical :: doCopy

        allocate(soptr)
        call soptr%conicalScanningObservationConstructor(this%getSatellitePlatform(), &
            & this%reader,this%getFovAlongPointer(),this%getFovCrossPointer(),        &
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
            dsPtr => soptr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(SsmisObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),          pointer :: dsPtr
        class(SsmisObservation), pointer :: soptr

        soptr => this%cloneSsmisObs(shallow,copyData)
        dsPtr => soptr
    end function
end module
