module satelliteObservation_mod
    use parallelInfo_mod

    use iso_fortran_env

    use dictionary_mod

    use observation_mod

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
    character(*), public, parameter :: SC_LAT_VAR_NAME  = 'Spacecraft_Latitude'
    character(*), public, parameter :: SC_LON_VAR_NAME  = 'Spacecraft_Longitude'
    !character(*), public, parameter :: QUALITY_VAR_NAME = 'Quality'
    !character(*), public, parameter :: ANGLE_VAR_NAME   = 'Incidence Angle'
    character(*), public, parameter :: YEAR_VAR_NAME    = 'Observation_Year'
    character(*), public, parameter :: MONTH_VAR_NAME   = 'Observation_Month'
    character(*), public, parameter :: DAY_VAR_NAME     = 'Observation_Day'
    character(*), public, parameter :: HOUR_VAR_NAME    = 'Observation_Hour'
    character(*), public, parameter :: MINUTE_VAR_NAME  = 'Observation_Minute'
    character(*), public, parameter :: SECOND_VAR_NAME  = 'Observation_Second'

    character(*), public, parameter :: PIXELS_DIM_NAME  = 'pixels'
    character(*), public, parameter :: SCANS_DIM_NAME   = 'scans'
    character(*), public, parameter :: CHANS_DIM_NAME   = 'channels'

    integer,      public, parameter :: SO_LAT_DIM  = 1
    integer,      public, parameter :: SO_LON_DIM  = 2
    integer,      public, parameter :: SO_PIX_DIM  = 3
    integer,      public, parameter :: SO_SCAN_DIM = 4

    type, extends(Observation), public :: SatelliteObservation
        private
            class(SatellitePlatformInfo), pointer :: satellitePlatform => null()

            !real(real32),           dimension(:,:),   pointer :: quality
            !real(real32),           dimension(:,:,:), pointer :: incidenceAngle

            ! real(real32) nchan, npix, nscan
            class(DataVariable),  pointer     :: tbVar     => null()
            ! real(real32) npix, nscan
            class(DataVariable),  pointer     :: latVar    => null()
            ! real(real32) npix, nscan
            class(DataVariable),  pointer     :: lonVar    => null()
            ! real(real32) nscan
            class(DataVariable),  pointer     :: scLatVar  => null()
            ! real(real32) nscan
            class(DataVariable),  pointer     :: scLonVar  => null()
            ! integer    nscan
            class(DataVariable),  pointer     :: yearVar   => null()
            ! integer(1) nscan
            class(DataVariable),  pointer     :: monthVar  => null()
            ! integer(1) nscan
            class(DataVariable),  pointer     :: dayVar    => null()
            ! integer(1) nscan
            class(DataVariable),  pointer     :: hourVar   => null()
            ! integer(1) nscan
            class(DataVariable),  pointer     :: minuteVar => null()
            ! integer(1) nscan
            class(DataVariable),  pointer     :: secondVar => null()

            real(real64),         allocatable :: obsErr(:)

            integer :: choffset        = 0
            integer :: uniquechoffset  = 0

        contains
            procedure :: getSatellitePlatform

            procedure :: getPixelDim
            procedure :: getScanDim
            procedure :: getChannelDim

            procedure :: getPixelExtent
            procedure :: getScanExtent
            procedure :: getChannelExtent

            procedure :: getLatitudes
            procedure :: getLongitudes
            procedure :: getCraftLatitudes
            procedure :: getCraftLongitudes
            procedure :: getBrightnessTemps
            procedure :: getObservationError
            procedure :: getChannelOffset
            procedure :: getUniqueChannelOffset
            procedure :: getChannelSubset

            procedure :: getTbVar
            procedure :: getLatVar
            procedure :: getLonVar
            procedure :: getScLonVar
            procedure :: getScLatVar
            procedure :: getYearVar
            procedure :: getMonthVar
            procedure :: getDayVar
            procedure :: getHourVar
            procedure :: getMinuteVar
            procedure :: getSecondVar

            procedure :: clone
            procedure :: cloneSatObs
            procedure :: cloneSubset
            procedure :: cloneSatObsSubset

            generic   :: loadSatelliteObservation =>   &
                & loadSatelliteObservation_empty_real, &
                & loadSatelliteObservation_empty_dble, &
                & loadSatelliteObservation_vars

            procedure, private :: loadSatelliteObservation_empty_real
            procedure, private :: loadSatelliteObservation_empty_dble
            procedure, private :: loadSatelliteObservation_vars

            procedure :: writeSatObsToFile
            !procedure :: loadFromFile

            procedure :: satelliteObservationConstructor
            !procedure :: satelliteObservationConstructor_load

            final :: satelliteObservationDestructor ! clean up all allocated variables
    end type

    contains

    subroutine satelliteObservationConstructor(this,satellitePlatform,reader,&
        & choffset,obsErr,uniquechoffset)
        !&lat,lon,scLat,scLon,nchannels,tb,chsubset,choffset,obsErr,year,month,day,hour,&
        !&minute,second,naux,uniquechoffset)
        ! minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)

        implicit none

        class(SatelliteObservation)  :: this

        class(SatellitePlatformInfo),     pointer     :: satellitePlatform
        class(DataArrayReader), optional, pointer     :: reader
        integer,                optional, intent(in)  :: choffset
        real(real64),           optional, intent(in)  :: obsErr(:)
!        integer,                optional, intent(in)  :: naux
        integer,                optional, intent(in)  :: uniquechoffset
!        type(datetime),   optional,     intent(in)  :: cdate
!        type(timedelta),  optional,     intent(in)  :: maxTimeDiff

!        logical,          optional,     intent(in)  :: useQC
!        real(real64),          optional,     intent(in)  :: minLat
!        real(real64),          optional,     intent(in)  :: maxLat
!        real(real64),          optional,     intent(in)  :: minLon
!        real(real64),          optional,     intent(in)  :: maxLon
!        integer,          optional,     intent(in)  :: minScan
!        integer,          optional,     intent(in)  :: maxScan

        class(PlatformInfo), pointer  :: platform

        type(datetime)  :: ijtime
        type(timedelta) :: td

        integer :: nobs

        integer :: i, j, ind

        real(real64) :: maxsecs

        this%satellitePlatform => satellitePlatform

        platform => satellitePlatform

        if (present(choffset)) then
            this%choffset  =  choffset
        end if

        if (present(obsErr)) then
            this%obsErr    = obsErr
        end if

        if (present(uniquechoffset)) then
            this%uniquechoffset = uniquechoffset
        else if (present(choffset)) then
            this%uniquechoffset = choffset
        end if

        call this%observationConstructor(platform,reader)

!        if (present(maxTimeDiff)) then
!            maxsecs = abs(maxTimeDiff%total_seconds())
!        end if
!
!
!        nobs = 0
!
!        do j=1,size(this%lat,2)
!            do i=1,size(this%lat,1)
!                if (present(minLat)) then
!                    if (this%lat(i,j) .lt. minLat) then
!                        cycle
!                    end if
!                end if
!
!                if (present(maxLat)) then
!                    if (this%lat(i,j) .gt. maxLat) then
!                        cycle
!                    end if
!                end if
!
!                if (present(minLon)) then
!                    if (this%lon(i,j) .lt. minLon) then
!                        cycle
!                    end if
!                end if
!
!                 if (present(maxLon)) then
!                    if (this%lon(i,j) .gt. maxLon) then
!                        cycle
!                    end if
!                end if
!
!                if (present(cdate) .and. present(maxTimeDiff)) then
!                    ijtime = datetime(int(this%year(j)),int(this%month(j)),int(this%day(j)),&
!                        &int(this%hour(j)),int(this%minute(j)),int(this%second(j)))
!
!                    td = ijtime - cdate
!
!                    if (td%total_seconds() .gt. maxsecs) then
!                        cycle
!                    end if
!                end if
!
!                if (present(minScan)) then
!                    if (j < minScan) then
!                        cycle
!                    end if
!                end if
!
!                if (present(maxScan)) then
!                    if (j > maxScan) then
!                        cycle
!                    end if
!                end if
!
!                if (this%lon(i,j) < -180.d0 .or. this%lon(i,j) > 180.d0) then
!                    cycle
!                end if
!
!                if (this%lat(i,j) < -90.d0 .or. this%lat(i,j) > 90.d0) then
!                    cycle
!                end if
!
!                nobs = nobs + 1
!            end do
!        end do

!        ind = 0
!
!        do j=1,this%getNScans()
!            do i=1,this%getNPixels()
!                if (present(minLat)) then
!                    if (this%lat(i,j) .lt. minLat) then
!                        cycle
!                    end if
!                end if
!
!                if (present(maxLat)) then
!                    if (this%lat(i,j) .gt. maxLat) then
!                        cycle
!                    end if
!                end if
!
!                if (present(minLon)) then
!                    if (this%lon(i,j) .lt. minLon) then
!                        cycle
!                    end if
!                end if
!
!                 if (present(maxLon)) then
!                    if (this%lon(i,j) .gt. maxLon) then
!                        cycle
!                    end if
!                end if
!
!                if (present(cdate) .and. present(maxTimeDiff)) then
!                    ijtime = datetime(int(this%year(j)),int(this%month(j)),int(this%day(j)),&
!                        &int(this%hour(j)),int(this%minute(j)),int(this%second(j)))
!
!                    td = ijtime - cdate
!
!                    if (abs(td%total_seconds()) .gt. maxsecs) then
!                        cycle
!                    end if
!                end if
!
!                if (present(minScan)) then
!                    if (j < minScan) then
!                        cycle
!                    end if
!                end if
!
!                if (present(maxScan)) then
!                    if (j > maxScan) then
!                        cycle
!                    end if
!                end if
!
!                if (this%lon(i,j) < -180.d0 .or. this%lon(i,j) > 180.d0) then
!                    cycle
!                end if
!
!                if (this%lat(i,j) < -90.d0 .or. this%lat(i,j) > 90.d0) then
!                    cycle
!                end if
!
!                ind = ind + 1
!                this%obsData(1:this%nchannels,ind) = dble(this%tb(1:this%nchannels,i,j))
!                this%obsLoci(SO_LAT_DIM,ind) = dble(this%lat(i,j))
!                this%obsLoci(SO_LON_DIM,ind) = dble(this%lon(i,j))
!                this%obsLoci(SO_PIX_DIM,ind) = dble(i)
!                this%obsLoci(SO_SCAN_DIM,ind) = dble(j)
!            end do
!        end do
    end subroutine

!    subroutine satelliteObservationConstructor_load(this,satellitePlatform,reader,writer,&
!        nobs,fileName,useQC,naux)
!
!        implicit none
!
!        class(SatelliteObservation)            :: this
!
!        class(SatellitePlatformInfo), pointer  :: satellitePlatform
!        class(DataArrayReader),       pointer  :: reader
!        class(DataArrayWriter),       pointer  :: writer
!        integer, intent(in)                    :: nobs
!        character(len=*), intent(in)           :: fileName
!        logical, intent(in),          optional :: useQC
!        integer, intent(in),          optional :: naux
!        class(PlatformInfo),          pointer  :: platform
!
!        integer :: mobs, ndim
!
!        if (associated(satellitePlatform%channelSubset)) then
!            mobs = size(satellitePlatform%channelSubset)
!        else
!            mobs = satellitePlatform%mobs
!        end if
!
!        ! obsLoci stored as (i,j,lat,lon) as they are column observations. i/j will be scan/pixel, for example
!        ndim = 4
!
!        platform => satellitePlatform
!        call this%observationConstructor(platform,reader,writer,nobs,mobs,ndim,naux)
!
!        call this%loadFromFile(fileName)
!    end subroutine

    subroutine satelliteObservationDestructor(this)
        implicit none

        type(SatelliteObservation)  :: this

        ! all of the variables and dimensions will be destroyed in the data group / &
        ! data set classes
    end subroutine

    subroutine loadSatelliteObservation_empty_real(this,pinfo,pixDim,scanDim,latptr,lonptr)

        implicit none

        class(SatelliteObservation)           :: this

        class(ParallelInfo),        pointer   :: pinfo
        class(DataDimension),       pointer   :: pixDim
        class(DataDimension),       pointer   :: scanDim
        real(real32),               pointer   :: latptr(:,:)
        real(real32),               pointer   :: lonptr(:,:)

        integer :: nchan

        real(real32), pointer :: tbptr (:,:,:)
        real(real32), pointer :: latptr_var(:,:)
        real(real32), pointer :: lonptr_var(:,:)

        class(DataDimension), pointer :: chanDim

        class(DataVariable),  pointer :: tbVar
        class(DataVariable),  pointer :: latVar
        class(DataVariable),  pointer :: lonVar

        if (all(shape(latptr) /= shape(latptr_var)) .or. &
            all(shape(lonptr) /= shape(lonptr_var))) then
            write(msgstr,*) 'Incompatible shapes for lat ptr: ', &
                & shape(latptr), ' vs. ', shape(latptr_var), ' and/or lon ptr: ',&
                & shape(lonptr), ' vs. ', shape(lonptr_var)
            call error(msgstr)
        end if

        chanDim => this%addDimensionByName(CHANS_DIM_NAME, &
            & this%satellitePlatform%getNChannels())

        tbVar  => this%addVariable(pinfo, TB_VAR_NAME,  tbptr,  &
            & chanDim, pixDim, scanDim)

        latVar => this%addVariable(pinfo, LAT_VAR_NAME, latptr_var, &
            & pixDim, scanDim)

        lonVar => this%addVariable(pinfo, LON_VAR_NAME, lonptr_var, &
            & pixDim, scanDim)

        latptr_var(:,:) = latptr(:,:)
        lonptr_var(:,:) = lonptr(:,:)

        call this%loadSatelliteObservation_vars(pinfo,tbVar,latVar,lonVar)
    end subroutine

    subroutine loadSatelliteObservation_empty_dble(this,pinfo,pixDim,scanDim,latptr,lonptr)

        implicit none

        class(SatelliteObservation)           :: this

        class(ParallelInfo),        pointer   :: pinfo
        class(DataDimension),       pointer   :: pixDim
        class(DataDimension),       pointer   :: scanDim
        real(real64),               pointer   :: latptr(:,:)
        real(real64),               pointer   :: lonptr(:,:)

        integer :: nchan

        real(real32), pointer :: tbptr (:,:,:)
        real(real32), pointer :: latptr_var(:,:)
        real(real32), pointer :: lonptr_var(:,:)

        class(DataDimension), pointer :: chanDim

        class(DataVariable),  pointer :: tbVar
        class(DataVariable),  pointer :: latVar
        class(DataVariable),  pointer :: lonVar

        chanDim => this%addDimensionByName(CHANS_DIM_NAME, &
            & this%satellitePlatform%getNChannels())

        tbVar  => this%addVariable(pinfo, TB_VAR_NAME,  tbptr,  &
            & chanDim, pixDim, scanDim, initval = -999.0)

        latVar => this%addVariable(pinfo, LAT_VAR_NAME, latptr_var, &
            & pixDim, scanDim)

        lonVar => this%addVariable(pinfo, LON_VAR_NAME, lonptr_var, &
            & pixDim, scanDim)

        if (all(shape(latptr) /= shape(latptr_var)) .or. &
            all(shape(lonptr) /= shape(lonptr_var))) then
            write(msgstr,*) 'Incompatible shapes for lat ptr: ', &
                & shape(latptr), ' vs. ', shape(latptr_var), ' and/or lon ptr: ',&
                & shape(lonptr), ' vs. ', shape(lonptr_var)
            call error(msgstr)
        end if

        latptr_var = latptr
        lonptr_var = lonptr

        call this%loadSatelliteObservation_vars(pinfo,tbVar,latVar,lonVar)
    end subroutine

    subroutine loadSatelliteObservation_vars(this,pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
        & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar, mobsExtent, nobsExtent,        &
        & nlociExtent)

        implicit none

        class(SatelliteObservation)   :: this

        class(ParallelInfo),            pointer    :: pinfo

        ! real(real32) nchan, npix, nscan
        class(DataVariable),            pointer    :: tbVar
        ! real(real32) npix, nscan
        class(DataVariable),            pointer    :: latVar
        ! real(real32) npix, nscan
        class(DataVariable),            pointer    :: lonVar
        ! real(real32) nscan
        class(DataVariable),  optional, pointer    :: scLatVar
        ! real(real32) nscan
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

        class(DataExtent),    optional, pointer    :: mobsExtent
        class(DataExtent),    optional, pointer    :: nobsExtent
        class(DataExtent),    optional, pointer    :: nlociExtent

        class(DataDimension), pointer :: mobsDim
        class(DataDimension), pointer :: nobsDim
        class(DataDimension), pointer :: nlociDim
        class(DataDimension), pointer :: nauxDim

        class(DataExtent), pointer :: mobsExtentToUse
        class(DataExtent), pointer :: nobsExtentToUse
        class(DataExtent), pointer :: nlociExtentToUse
        class(DataExtent), pointer :: nauxExtent

        class(DataVariable), pointer :: obsDataVar
        class(DataVariable), pointer :: obsLociVar
        class(DataVariable), pointer :: auxDataVar

        integer :: npix
        integer :: nscan
        integer :: nchan

        integer :: i, j, ind
        integer :: xs, xe, ys, ye, nx, ny

        real(real32), dimension(:,:,:), pointer :: tb
        real(real32), dimension(:,:),   pointer :: lat
        real(real32), dimension(:,:),   pointer :: lon
        real(real64), dimension(:,:),   pointer :: obsData
        real(real64), dimension(:,:),   pointer :: obsLoci

        class(DataExtent),              pointer    :: pixExtent
        class(DataExtent),              pointer    :: scanExtent
        class(DataExtent),              pointer    :: chanExtent

        logical :: extentsProvided

        this%tbVar  => tbVar
        this%latVar => latVar
        this%lonVar => lonVar

        chanExtent => tbVar%getExtentNumber(1)
        pixExtent  => tbVar%getExtentNumber(2)
        scanExtent => tbVar%getExtentNumber(3)

        nchan = chanExtent%getGlobalCount()
        npix  =  pixExtent%getGlobalCount()
        nscan = scanExtent%getGlobalCount()

        if (present(scLatVar)) then
            this%scLatVar  => scLatVar
        end if

        if (present(scLonVar)) then
            this%scLonVar  => scLonVar
        end if

        if (present(yearVar)) then
            this%yearVar  => yearVar
        end if

        if (present(monthVar)) then
            this%monthVar  => monthVar
        end if

        if (present(dayVar)) then
            this%dayVar    => dayVar
        end if

        if (present(hourVar)) then
            this%hourVar   => hourVar
        end if

        if (present(minuteVar)) then
            this%minuteVar => minuteVar
        end if

        if (present(secondVar)) then
            this%secondVar => secondVar
        end if

        if (present(mobsExtent) .or. present(nobsExtent) .or. present(nlociExtent)) then
            if (.not. present(mobsExtent) .or. .not. present(nobsExtent) .or. &
               &.not. present(nlociExtent)) then
                call error('If any of mobsExtent, nobsExtent, or nlociExtent ' // &
                    & 'are present, all must be.')
            else
                extentsProvided = .true.
            end if
        else
                extentsProvided = .false.
        end if

        ! now take care of mapping to the observation (mobs x nobs) format
        if (extentsProvided) then
            mobsDim => mobsExtent%getDimension()
        else
            mobsDim => this%addDimensionByName(MOBS_DIM_NAME,nchan)
        end if

        if (extentsProvided) then
            nobsDim => nobsExtent%getDimension()
        else
            nobsDim => this%addDimensionByName(NOBS_DIM_NAME,npix*nscan)
        end if

        if (extentsProvided) then
            nlociDim => nlociExtent%getDimension()
        else
            nlociDim => this%addDimensionByName(NLOCI_DIM_NAME,4)
        end if

        nauxDim => null()

        ! initialize the obsData and obsLoci arrays / variable
        if (extentsProvided) then
            obsDataVar => this%addVariable(pinfo,OBS_DATA_VAR_NAME,obsData,&
                & mobsExtent,nobsExtent)
            mobsExtentToUse => mobsExtent
            nobsExtentToUse => nobsExtent
        else
            obsDataVar => this%addVariable(pinfo,OBS_DATA_VAR_NAME,obsData,&
                & mobsDim,nobsDim)
            mobsExtentToUse  => obsDataVar%getExtentNumber(1)
            nobsExtentToUse  => obsDataVar%getExtentNumber(2)
        end if

        print *,'now adding obsLociVar'

        if (extentsProvided) then
            obsLociVar => this%addVariable(pinfo,OBS_LOCI_VAR_NAME,obsLoci,&
                & nlociExtent,nobsExtent)
            nlociExtentToUse => nlociExtent
        else
            obsLociVar => this%addVariable(pinfo,OBS_LOCI_VAR_NAME,obsLoci,&
                & nlociDim,nobsDim)
            nlociExtentToUse => obsLociVar%getExtentNumber(1)
        end if

        auxDataVar => null()
        nauxExtent => null()

        ! get the data arrays for tb and lat/lon
        call tbVar %getArray(tb)
        call latVar%getArray(lat)
        call lonVar%getArray(lon)

        pixExtent  => tbVar%getExtentNumber(2)
        scanExtent => tbVar%getExtentNumber(3)

        ! get the local ranges
        call  pixExtent%getLocalRange(xs,xe,nx)
        call scanExtent%getLocalRange(ys,ye,ny)

        ind = 0

        if (.not. extentsProvided) then
            nchan = chanExtent%getLocalCount()

            ! copy the data from the TB into the format that the observation class wants (mobs x nobs)
            do j=1,ny
                do i=1,nx
                    ind = ind + 1
                    obsData(1:nchan,ind) = dble(tb(1:nchan,i,j))

                    obsLoci(SO_LAT_DIM,ind)  = dble(lat(i,j))
                    obsLoci(SO_LON_DIM,ind)  = dble(lon(i,j))
                    obsLoci(SO_PIX_DIM,ind)  = dble(xs+i-1)
                    obsLoci(SO_SCAN_DIM,ind) = dble(ys+j-1)
                end do
            end do
        end if

        ! now load the observation with the given variables
        call this%loadObservation(pinfo,mobsExtentToUse,nobsExtentToUse,nlociExtentToUse,&
            & nauxExtent, obsDataVar,obsLociVar,auxDataVar)
    end subroutine

    function getSatellitePlatform(this) result(satellitePlatform)
        implicit none

        class(SatelliteObservation)  :: this

        class(SatellitePlatformInfo), pointer :: satellitePlatform

        satellitePlatform => this%satellitePlatform

    end function

    function getBrightnessTemps(this) result(tbPtr)
        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:,:,:), pointer :: tbPtr

        call this%tbVar%getArray(tbPtr)
    end function

    function getLatitudes(this) result(lat)
        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:,:), pointer :: lat

        call this%latVar%getArray(lat)
    end function

    function getLongitudes(this) result(lon)

        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:,:), pointer :: lon

        call this%lonVar%getArray(lon)
    end function

    function getCraftLatitudes(this) result(scLat)

        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:), pointer :: scLat

        call this%scLatVar%getArray(scLat)
    end function

    function getCraftLongitudes(this) result(scLon)

        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:), pointer :: scLon

        call this%scLonVar%getArray(scLon)
    end function

    function getPixelDim(this) result(pixDim)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataDimension), pointer :: pixDim

        pixDim => this%getDimensionByName(PIXELS_DIM_NAME)
    end function

    function getScanDim(this) result(scanDim)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataDimension), pointer :: scanDim

        scanDim => this%getDimensionByName(SCANS_DIM_NAME)
    end function

    function getChannelDim(this) result(chanDim)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataDimension), pointer :: chanDim

        chanDim => this%getDimensionByName(CHANS_DIM_NAME)
    end function

    function getPixelExtent(this) result(pixExtent)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataExtent), pointer :: pixExtent

        pixExtent => this%tbVar%getExtentNumber(2)
    end function

    function getScanExtent(this) result(scanExtent)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataExtent), pointer :: scanExtent

        scanExtent => this%tbVar%getExtentNumber(3)
    end function

    function getChannelExtent(this) result(chanExtent)

        implicit none

        class(SatelliteObservation)   :: this

        class(DataExtent), pointer :: chanExtent

        chanExtent => this%tbVar%getExtentNumber(1)
    end function

    function getChannelOffset(this) result(choffset)

        implicit none

        class(SatelliteObservation)  :: this

        integer :: choffset

        choffset = this%choffset
    end function

    function getUniqueChannelOffset(this) result(uniquechoffset)

        implicit none

        class(SatelliteObservation)  :: this

        integer :: uniquechoffset

        uniquechoffset = this%uniquechoffset
    end function

    function getChannelSubset(this) result(chsubset)

        implicit none

        class(SatelliteObservation)  :: this

        integer, dimension(:), pointer :: chsubset

        chsubset => this%satellitePlatform%getChannelSubset()
    end function

    function getTbVar(this) result(tbVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: tbVar

        tbVar => this%tbVar
    end function

    function getLatVar(this) result(latVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: latVar

        latVar => this%latVar
    end function

    function getLonVar(this) result(lonVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: lonVar

        lonVar => this%lonVar
    end function

    function getScLatVar(this) result(scLatVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: scLatVar

        scLatVar => this%scLatVar
    end function

    function getScLonVar(this) result(scLonVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: scLonVar

        scLonVar => this%scLonVar
    end function

    function getYearVar(this) result(yearVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: yearVar

        yearVar => this%yearVar
    end function

    function getMonthVar(this) result(monthVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: monthVar

        monthVar => this%monthVar
    end function

    function getDayVar(this) result(dayVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: dayVar

        dayVar => this%dayVar
    end function

    function getHourVar(this) result(hourVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: hourVar

        hourVar => this%hourVar
    end function

    function getMinuteVar(this) result(minuteVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: minuteVar

        minuteVar => this%minuteVar
    end function

    function getSecondVar(this) result(secondVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: secondVar

        secondVar => this%secondVar
    end function

    function getObservationError(this) result(obsErr)

        implicit none

        class(SatelliteObservation)  :: this

        real(real64), dimension(:), allocatable :: obsErr

        obsErr = this%obsErr
    end function

    subroutine writeSatObsToFile(this,pinfo,writer,satRes)
        implicit none

        class(SatelliteObservation)        :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayWriter), pointer    :: writer
        logical,                intent(in) :: satRes

        class(DataDimension), pointer :: pixDim
        class(DataDimension), pointer :: scanDim
        class(DataDimension), pointer :: nobsDim

        class(DataExtent), pointer :: scanExtent
        class(DataExtent), pointer :: pixExtent

        class(DataVariable), pointer :: tbVar
        class(DataVariable), pointer :: obsLociVar
        class(DataVariable), pointer :: obsDataVar
        real(real32),        pointer :: tb(:,:,:)

        real(real64),        pointer :: obsLoci(:,:)
        real(real64),        pointer :: obsData(:,:)

        integer :: iobs, chnum, chseq, obsi, obsj
        integer :: xs, ys

        nobsDim => this%getNObsDim()
        pixDim  => this%getPixelDim()
        scanDim => this%getScanDim()

        if (satRes) then
            call writer%writeDimension(pinfo, this%getPixelDim())
            call writer%writeDimension(pinfo, this%getScanDim())
            call writer%writeDimension(pinfo, this%getChannelDim())

            tbVar => this%tbVar%clone(copyData=.false.)
            call tbVar%getArray(tb)

            tb = -999

            obsLociVar => this%getObsLociVar()

            if (.not. associated(obsLociVar)) then
                call error('The obs loci variable must be associated to write the satellite observation.')
            end if

            pixExtent  => this%getPixelExtent()
            scanExtent => this%getScanExtent()

            xs = pixExtent%getLocalStart()
            ys = scanExtent%getLocalStart()

            call obsLociVar%getArray(obsLoci)

            obsDataVar => this%getObsDataVar()
            call obsDataVar%getArray(obsData)

            print *,'xs, ys:',xs,ys,&
                & minval(obsLoci(SO_PIX_DIM,:)), maxval(obsLoci(SO_PIX_DIM,:)),&
                & minval(obsLoci(SO_SCAN_DIM,:)),maxval(obsLoci(SO_SCAN_DIM,:))

            do iobs=1,size(obsLoci,2)
                obsi = nint(obsLoci(SO_PIX_DIM,iobs)) -xs+1
                obsj = nint(obsLoci(SO_SCAN_DIM,iobs))-ys+1

                if (obsi < 1 .or. obsj < 1) then
                    !print *,'iobs/obsi/obsj was wrong:',iobs,obsi,obsj
                    cycle
                end if

                do chnum=1,size(obsData,1)
                    chseq = chnum+this%getUniqueChannelOffset()

                    tb(chseq,obsi,obsj) = obsData(chnum,iobs)
                end do
            end do

            call writer%writeVariable(pinfo, tbVar)
            call writer%writeVariable(pinfo, this%latVar)
            call writer%writeVariable(pinfo, this%lonVar)

            if (associated(this%scLatVar)) then
                call writer%writeVariable(pinfo, this%scLatVar)
            end if

            if (associated(this%scLonVar)) then
                call writer%writeVariable(pinfo, this%scLonVar)
            end if
        else
            call this%writeObsToFile(pinfo,writer)
        end if
    end subroutine
!
!    subroutine loadFromFile(this, fileName)
!        implicit none
!
!        class(SatelliteObservation)  :: this
!        character(len=*), intent(in) :: fileName
!
!        logical :: fileExists
!
!        real(real64), pointer :: lon(:), lat(:), x(:), y(:)
!        character(len=9) :: fieldName
!
!        real(real64), dimension(:,:), allocatable :: tmp
!
!        integer :: mobs, nobs, mobs2, nobs2
!
!        inquire(file=fileName,exist=fileExists)
!
!        if (.not. fileExists) then
!            write(msgstr,*) 'In satelliteObservation%loadFromFile could not load file ',fileName
!            call error(msgstr)
!        end if
!
!        mobs = this%getMObs()
!        nobs = this%getNObs()
!
!        call ncReadVariableDimensions2D(fileName,'OBS',nobs2,mobs2)
!
!        if (nobs .ne. nobs2 .or. mobs .ne. mobs2) then
!            write(msgstr,*) 'Mismatch in sizes in satelliteObservation%loadFromFile:',nobs,mobs,'vs',nobs2,mobs2
!            call error(msgstr)
!        end if
!
!        allocate(tmp(nobs,mobs))
!
!        call ncReadVariable2D(fileName,'OBS',tmp,nobs,mobs)
!        this%obsData = transpose(tmp)
!
!        deallocate(tmp)
!
!        x(1:nobs)   => this%obsLoci(SO_PIX_DIM,:)
!        y(1:nobs)   => this%obsLoci(SO_SCAN_DIM,:)
!        lat(1:nobs) => this%obsLoci(SO_LAT_DIM,:)
!        lon(1:nobs) => this%obsLoci(SO_LON_DIM,:)
!
!        call ncReadVariable1D(fileName,'X',  x,nobs)
!        call ncReadVariable1D(fileName,'Y',  y,nobs)
!        call ncReadVariable1D(fileName,'LAT',lat,nobs)
!        call ncReadVariable1D(fileName,'LON',lon,nobs)
!    end subroutine

    function cloneSatObs(this,shallow,copyData) result(soPtr)
        implicit none

        class(SatelliteObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),              pointer :: dsPtr
        class(DataGroup),            pointer :: dgPtr
        class(SatelliteObservation), pointer :: soPtr

        logical :: isShallow
        logical :: doCopy

        allocate(soptr)
        call soptr%satelliteObservationConstructor(this%satellitePlatform,&
            & this%reader%clone(),this%choffset,this%obsErr,this%uniquechoffset)
        dsPtr => soPtr

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
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(SatelliteObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),              pointer :: dsPtr
        class(SatelliteObservation), pointer :: soPtr

        soptr => this%cloneSatObs(shallow,copyData)
        dsPtr => soptr
    end function

    function cloneSatObsSubset(this,pinfo,nobsNewExtent,localInds) result(soptr)
        implicit none

        class(SatelliteObservation)      :: this

        class(ParallelInfo),  pointer    :: pinfo
        class(DataExtent),    pointer    :: nobsNewExtent
        integer,              intent(in) :: localInds(:)

        class(SatelliteObservation), pointer :: soptr

        class(DataDimension), pointer :: pixDim, scanDim, chanDim

        class(DataVariable),  pointer :: tbVar  => null()
        class(DataVariable),  pointer :: latVar => null()
        class(DataVariable),  pointer :: lonVar => null()

        class(DataVariable),  pointer :: scLatVar  => null()
        class(DataVariable),  pointer :: scLonVar  => null()
        class(DataVariable),  pointer :: yearVar   => null()
        class(DataVariable),  pointer :: monthVar  => null()
        class(DataVariable),  pointer :: dayVar    => null()
        class(DataVariable),  pointer :: hourVar   => null()
        class(DataVariable),  pointer :: minuteVar => null()
        class(DataVariable),  pointer :: secondVar => null()

        class(DataExtent), pointer :: pixExtent, scanExtent, chanExtent

        class(DataSet),     pointer :: dsptr
        class(Observation), pointer :: optr

        dsPtr => this%clone(shallow=.true.,copyData=.false.)

        select type (dsPtr)
            class is (SatelliteObservation)
                soPtr => dsPtr
            class default
                call error('Unknown satellite observation type')
        end select

        pixDim  => this%getPixelDim()
        scanDim => this%getScanDim()
        chanDim => this%getChannelDim()

        call soptr%addDimension(pixDim%clone())
        call soptr%addDimension(scanDim%clone())
        call soptr%addDimension(chanDim%clone())

        tbVar     => this%getTbVar()
        latVar    => this%getLatVar()
        lonVar    => this%getLonVar()
        scLatVar  => this%getScLatVar()
        scLonVar  => this%getScLonVar()
        yearVar   => this%getYearVar()
        monthVar  => this%getMonthVar()
        dayVar    => this%getDayVar()
        hourVar   => this%getHourVar()
        minuteVar => this%getMinuteVar()
        secondVar => this%getSecondVar()

        tbVar     =>     tbVar%clone(copyData=.true.)
        call soptr%addVariablePointer(    tbVar)

        chanExtent => tbVar%getExtentNumber(1)
        pixExtent  => tbVar%getExtentNumber(2)
        scanExtent => tbVar%getExtentNumber(3)

        latVar    =>    latVar%clone(copyData=.true.)
        call soptr%addVariablePointer(   latVar)

        lonVar    =>    lonVar%clone(copyData=.true.)
        call soptr%addVariablePointer(   lonVar)

        if (associated(scLatVar)) then
            scLatVar  =>  scLatVar%clone(copyData=.true.)
            call soptr%addVariablePointer( scLatVar)
        end if

        if (associated(scLonVar)) then
            scLonVar  =>  scLonVar%clone(copyData=.true.)
            call soptr%addVariablePointer( scLonVar)
        end if

        if (associated(yearVar)) then
            yearVar   =>   yearVar%clone(copyData=.true.)
            call soptr%addVariablePointer(  yearVar)
        end if

        if (associated(monthVar)) then
            monthVar  =>  monthVar%clone(copyData=.true.)
            call soptr%addVariablePointer( monthVar)
        end if

        if (associated(dayVar)) then
            dayVar    =>    dayVar%clone(copyData=.true.)
            call soptr%addVariablePointer(   dayVar)
        end if

        if (associated(hourVar)) then
            hourVar   =>   hourVar%clone(copyData=.true.)
            call soptr%addVariablePointer(  hourVar)
        end if

        if (associated(minuteVar)) then
            minuteVar => minuteVar%clone(copyData=.true.)
            call soptr%addVariablePointer(minuteVar)
        end if

        if (associated(secondVar)) then
            secondVar => secondVar%clone(copyData=.true.)
            call soptr%addVariablePointer(secondVar)
        end if

        call soptr%loadSatelliteObservation(pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
            & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar,this%getMObsExtent(),&
            & nobsNewExtent,this%getNLociExtent())

        optr => soptr

        call this%copySubset(optr,localInds)
    end function

    function cloneSubset(this,pinfo,nobsNewExtent,localInds) result(optr)
        implicit none

        class(SatelliteObservation)     :: this

        class(ParallelInfo), pointer    :: pinfo
        class(DataExtent),   pointer    :: nobsNewExtent
        integer,             intent(in) :: localInds(:)

        class(Observation),  pointer    :: optr

        class(SatelliteObservation),  pointer :: soptr

        print *,'now cloning subset'

        soptr => this%cloneSatObsSubset(pinfo,nobsNewExtent,localInds)
        optr => soptr
    end function
end module
