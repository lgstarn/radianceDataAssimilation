module gmiObservation_mod

    use parallelInfo_mod

    use iso_fortran_env

    use platformInfo_mod

    use satelliteObservation_mod
    use satellitePlatformInfo_mod

    use conicalScanningObservation_mod
    use radianceAssimilationConstants_mod

    use dataSet_mod
    use dataGroup_mod
    use dataVariable_mod
    use dataDimension_mod
    use dataArrayReader_mod
    use hdfDataArrayReader_mod

    use datetime_mod
    use geodesic_mod
    use mpiUtils_mod
    use hdfUtils_mod

    implicit none

    private

    type, extends(ConicalScanningObservation), public :: GmiObservation
        contains
            procedure :: gmiObservationConstructor
            final     :: gmiObservationDestructor

            procedure :: clone
            procedure :: cloneGmiObs
    end type

    contains

    subroutine gmiObservationConstructor(this,pinfo,filename,postingNumber,naux)

        implicit none

        class(GmiObservation)              :: this

        class(ParallelInfo),    pointer    :: pinfo
        character(len=*),       intent(in) :: filename
        integer,                intent(in) :: postingNumber
        integer, optional,      intent(in) :: naux
!        character(len=*), intent(in)  :: filename

!        real(real64), intent(in), optional :: minLat
!        real(real64), intent(in), optional :: maxLat
!        real(real64), intent(in), optional :: minLon
!        real(real64), intent(in), optional :: maxLon
!        integer, intent(in), optional :: minScan
!        integer, intent(in), optional :: maxScan
!        type(datetime),      optional :: cdate
!        type(timedelta),     optional :: maxTimeDiff

        integer, dimension(:), pointer :: channelSubset

        class(DataDimension), pointer  :: pixDim, scanDim, chanDim

        class(DataVariable),  pointer  :: tbVar
        class(DataVariable),  pointer  :: latVar
        class(DataVariable),  pointer  :: lonVar
        class(DataVariable),  pointer  :: scLatVar
        class(DataVariable),  pointer  :: scLonVar
        class(DataVariable),  pointer  :: yearVar
        class(DataVariable),  pointer  :: monthVar
        class(DataVariable),  pointer  :: dayVar
        class(DataVariable),  pointer  :: hourVar
        class(DataVariable),  pointer  :: minuteVar
        class(DataVariable),  pointer  :: secondVar

        class(SatellitePlatformInfo), pointer :: satellitePlatform

        real(real32), dimension(:,:), pointer :: lat
        real(real32), dimension(:,:), pointer :: lon
        real(real32), dimension(:),   pointer :: scLat
        real(real32), dimension(:),   pointer :: scLon
        integer                               :: nchannels

        integer,       dimension(:), pointer :: year
        integer(int8), dimension(:), pointer :: month
        integer(int8), dimension(:), pointer :: day
        integer(int8), dimension(:), pointer :: hour
        integer(int8), dimension(:), pointer :: minute
        integer(int8), dimension(:), pointer :: second

        real(real32), dimension(:,:,:), pointer :: tb

        real(real64), dimension(:), pointer  :: fovAlong
        real(real64), dimension(:), pointer  :: fovCross
        real(real64), dimension(:), pointer  :: intTime
        real(real64), dimension(:), pointer  :: obsErr

        integer :: i, j, groupNum

        character(:), allocatable :: latVarName

        ! Spin-frequency in RPM
        real(real64) :: spinFrequency = 32
        integer      :: choffset

        character(3) :: grp

        class(DataArrayReader),    pointer :: reader
        class(HdfDataArrayReader), pointer :: hdfReader

        if (postingNumber == 1) then
            nchannels = 9
            choffset = 0

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))

            ! Instaneous half-power of the antenna along-direction FOV bandwidth, in km, of GMI channels
            fovAlong = (/19.4, 19.4, 11.2, 11.2,  9.2,  8.6,  8.6, 4.4, 4.4/)
            ! Instaneous half-power of the antenna cross-direction FOV bandwidth, in km, of GMI channels
            fovCross = (/32.2, 32.2, 18.3, 18.3, 15.0, 14.4, 14.4, 7.3, 7.3/)
            ! Integration times for each channel in ms
            intTime = (/9.7, 9.7, 5.3, 5.3, 5.0, 5.0, 5.0, 2.2, 2.2/)
            ! Noise-equivalent temperature (K)
            obsErr = (/0.96, 0.96, 0.84, 0.84, 1.05, 0.65, 0.65, 0.57, 0.57/)

            allocate(channelSubset(nchannels))

            do i=1,nchannels
                channelSubset(i) = i
            end do

            groupNum = 1
        elseif (postingNumber == 2) then
            nchannels = 4
            choffset = 9

            allocate(fovAlong(nchannels))
            allocate(fovCross(nchannels))
            allocate(intTime(nchannels))
            allocate(obsErr(nchannels))

            ! Instaneous half-power of the antenna along-direction FOV bandwidth, in km, of GMI channels
            fovAlong = (/4.4, 4.4, 4.4, 4.4/)
            ! Instaneous half-power of the antenna cross-direction FOV bandwidth, in km, of GMI channels
            fovCross = (/7.1, 7.1, 7.2, 7.2/)
            ! Integration times for each channel in ms
            intTime = (/3.6, 3.6, 3.6, 3.6/)
            ! Noise-equivalent temperature (K)
            obsErr = (/1.5, 1.5, 1.5, 1.5/)

            allocate(channelSubset(nchannels))

            do i=1,nchannels
                channelSubset(i) = choffset+i
            end do

            groupNum = 2
        else
            write(msgstr,*) 'Unknown posting number',postingNumber,'in GmiObservation constructor'
            call error(msgstr)
        end if

        allocate(satellitePlatform)
        call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_GPM_GMI,&
            &'GPM','GMI',nchannels,channelSubset)

        allocate(hdfReader)
        call hdfReader%hdfDataArrayReaderConstructor(filename,littleEndian=.true.)
        reader => hdfReader

        call this%conicalScanningObservationConstructor(satellitePlatform,      &
            & reader=reader,fovAlong=fovAlong,fovCross=fovCross,intTime=intTime,&
            & choffset=choffset,spinFrequency=spinFrequency,obsErr=obsErr)

        write(grp,'(A,I1)') '/S',groupNum
        latVarName = grp // "/Latitude"

        pixDim  => this%loadDimensionFromVariable(pinfo,PIXELS_DIM_NAME,1,latVarName)
        scanDim => this%loadDimensionFromVariable(pinfo,SCANS_DIM_NAME, 2,latVarName)
        chanDim => this%addDimensionByName(CHANS_DIM_NAME,nchannels)

        latVar    => this%loadVariable(pinfo, LAT_VAR_NAME,    lat, &
            & pixDim, scanDim, latVarName)

        lonVar    => this%loadVariable(pinfo, LON_VAR_NAME,    lon, &
            & pixDim, scanDim, grp // "/Longitude")

!        scLatVar  => this%loadVariable(pinfo, SC_LAT_VAR_NAME, scLat, &
!            & scanDim, grp//"/navigation/scLat")
!
!        scLonVar  => this%loadVariable(pinfo, SC_LON_VAR_NAME, scLon, &
!            & scanDim, grp//"/navigation/scLon")
        scLatVar  => this%loadVariable(pinfo, SC_LAT_VAR_NAME, scLat, &
            & scanDim, grp//"/SCstatus/SClatitude")

        scLonVar  => this%loadVariable(pinfo, SC_LON_VAR_NAME, scLon, &
            & scanDim, grp//"/SCstatus/SClongitude")

        yearVar  => this%loadVariable(pinfo,  YEAR_VAR_NAME,   year, &
            & scanDim, grp//"/ScanTime/Year")

        monthVar => this%loadVariable(pinfo,  MONTH_VAR_NAME,  month, &
            & scanDim, grp//"/ScanTime/Month")

        dayVar   => this%loadVariable(pinfo,  DAY_VAR_NAME,    day, &
            & scanDim, grp//"/ScanTime/DayOfMonth")

        hourVar   => this%loadVariable(pinfo, HOUR_VAR_NAME,   hour, &
            & scanDim, grp//"/ScanTime/Hour")

        minuteVar => this%loadVariable(pinfo, MINUTE_VAR_NAME, minute, &
            & scanDim, grp//"/ScanTime/Minute")

        secondVar => this%loadVariable(pinfo, SECOND_VAR_NAME, second, &
            & scanDim, grp//"/ScanTime/Second")

!        tbVar     => this%loadVariable(pinfo, TB_VAR_NAME,     tb, &
!            & scanDim, pixDim, chanDim, grp//"/Tb")
        tbVar     => this%loadVariable(pinfo, TB_VAR_NAME,     tb, &
            & chanDim, pixDim, scanDim, grp//"/Tc")

        call this%loadConicalScanningObservation(pinfo,tbVar,latVar,lonVar,scLatVar,&
            & scLonVar,yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar)
    end subroutine

    subroutine gmiObservationDestructor(this)
        implicit none

        type(GmiObservation)  :: this

        ! called automatically
        ! call this%conicalScanningObservationDestructor()
    end subroutine

    function cloneGmiObs(this,shallow,copyData) result(goptr)
        implicit none

        class(GmiObservation), target :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataSet),        pointer :: dsPtr
        class(DataGroup),      pointer :: dgPtr
        class(GmiObservation), pointer :: goptr

        logical :: isShallow
        logical :: doCopy

        allocate(goptr)
        call goptr%conicalScanningObservationConstructor(this%getSatellitePlatform(), &
            & this%reader%clone(),this%getFovAlongPointer(),this%getFovCrossPointer(),&
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
            dsPtr => goptr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(GmiObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),        pointer :: dsPtr
        class(GmiObservation), pointer :: goptr

        goptr => this%cloneGmiObs(shallow,copyData)
        dsPtr => goptr
    end function
end module
