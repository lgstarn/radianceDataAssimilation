module satelliteObservation_mod
    use parallelInfo_mod

    use iso_fortran_env

    use dictionary_mod

    use observation_mod

    use platformInfo_mod
    use satellitePlatformInfo_mod

    use dataSet_mod
    use dataType_mod
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

    !character(*), public, parameter :: TB_VAR_NAME      = 'Brightness_Temperatures'
    !character(*), public, parameter :: LAT_VAR_NAME     = 'Latitude'
    !character(*), public, parameter :: LON_VAR_NAME     = 'Longitude'
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
            !class(DataVariable),  pointer     :: tbVar     => null()
            ! real(real32) npix, nscan
            !class(DataVariable),  pointer     :: latVar    => null()
            ! real(real32) npix, nscan
            !class(DataVariable),  pointer     :: lonVar    => null()
            ! real(real32) nscan
            !class(DataVariable),  pointer     :: scLatVar  => null()
            ! real(real32) nscan
            !class(DataVariable),  pointer     :: scLonVar  => null()
            ! integer    nscan
            !class(DataVariable),  pointer     :: yearVar   => null()
            ! integer(1) nscan
            !class(DataVariable),  pointer     :: monthVar  => null()
            ! integer(1) nscan
            !class(DataVariable),  pointer     :: dayVar    => null()
            ! integer(1) nscan
            !class(DataVariable),  pointer     :: hourVar   => null()
            ! integer(1) nscan
            !class(DataVariable),  pointer     :: minuteVar => null()
            ! integer(1) nscan
            !class(DataVariable),  pointer     :: secondVar => null()

            integer :: choffset        = 0
            integer :: uniquechoffset  = 0

        contains
            procedure :: getSatellitePlatform

            !procedure :: getPixelDim
            !procedure :: getScanDim
            !procedure :: getChannelDim

            !procedure :: getPixelExtent
            !procedure :: getScanExtent
            !procedure :: getChannelExtent

            !procedure :: getLatitudes
            !procedure :: getLongitudes
            procedure :: getCraftLatitudes
            procedure :: getCraftLongitudes
            !procedure :: getBrightnessTemps
            procedure :: getChannelOffset
            procedure :: getUniqueChannelOffset
            procedure :: getChannelSubset

            !procedure :: getTbVar
            !procedure :: getLatVar
            !procedure :: getLonVar
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

            procedure :: loadSatObsFromFile

            procedure :: loadSatelliteObservation
            procedure :: loadSatelliteObservation_tb3d

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

        if (present(uniquechoffset)) then
            this%uniquechoffset = uniquechoffset
        else if (present(choffset)) then
            this%uniquechoffset = choffset
        end if

        call this%observationConstructor(platform,obserr,reader)

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

    ! Migrate the lat/lon/tb format to the obsData/obsLoci format. NOTE: latVar, lonVar, and tbVar
    ! will be deleted if they exist!
    subroutine loadSatelliteObservation_tb3d(this, pinfo, latVar, lonVar, tbVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(ParallelInfo), pointer :: pinfo

        class(DataVariable), pointer :: tbVar
        class(DataVariable), pointer :: latVar
        class(DataVariable), pointer :: lonVar

        integer :: nobs_l(1)
        integer, allocatable :: allnobs(:)

        real(real64), pointer :: obsData(:,:)
        real(real64), pointer :: obsLoci(:,:)

        real(real32), pointer :: lat_real(:,:)
        real(real32), pointer :: lon_real(:,:)
        real(real32), pointer :: tb_real(:,:,:)

        real(real64), pointer :: lat_dble(:,:)
        real(real64), pointer :: lon_dble(:,:)
        real(real64), pointer :: tb_dble(:,:,:)

        integer :: i, j, ierr
        integer :: pixs, pixe
        integer :: scans, scane
        integer :: nchan_g, npix_g, nscan_g
        integer :: nchan_l, npix_l, nscan_l
        integer :: chnum, iobs, nobs_s

        logical :: isReal

        class(DataDimension), pointer :: mobsDim
        class(DataDimension), pointer :: nobsDim
        class(DataDimension), pointer :: nlociDim

        class(DataExtent), pointer :: mobsExtent
        class(DataExtent), pointer :: nobsExtent
        class(DataExtent), pointer :: nlociExtent

        class(DataVariable), pointer    :: obsDataVar
        class(DataVariable), pointer    :: obsLociVar

        if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
            isReal = .true.
        else
            isReal = .false.
        end if

        if (isReal) then
            call latVar%getArray(lat_real)
            call lonVar%getArray(lon_real)
            call tbVar%getArray(tb_real)

            if (size(tb_real,2) /= size(lat_real,1) .or. size(tb_real,3) /= size(lat_real,2) .or. &
                size(tb_real,2) /= size(lon_real,1) .or. size(tb_real,3) /= size(lon_real,2)) then

                write(msgstr,*) 'Incompatible tb/lat/lon sizes: ',shape(tb_real),' vs ', &
                    shape(lat_real),' vs', shape(lon_real)
                call error(msgstr)
            end if
        else
            call latVar%getArray(lat_dble)
            call lonVar%getArray(lon_dble)
            call tbVar%getArray(tb_dble)

            if (size(tb_dble,2) /= size(lat_dble,1) .or. size(tb_dble,3) /= size(lat_dble,2) .or. &
                size(tb_dble,2) /= size(lon_dble,1) .or. size(tb_dble,3) /= size(lon_dble,2)) then

                write(msgstr,*) 'Incompatible tb/lat/lon sizes: ',shape(tb_dble),' vs ', &
                    shape(lat_dble),' vs', shape(lon_dble)
                call error(msgstr)
            end if
        end if

        nchan_g = tbVar%getGlobalDimCount(1)
        npix_g  = tbVar%getGlobalDimCount(2)
        nscan_g = tbVar%getGlobalDimCount(3)

        nchan_l = tbVar%getLocalExtentCount(1)
        call tbVar%getLocalExtentRange(2,pixs,pixe,npix_l)
        call tbVar%getLocalExtentRange(3,scans,scane,nscan_l)

        if (nchan_g /= nchan_l) then
            call error('Cannot currently handle the distributed channel case')
        end if

        ! create the global dimensions
        mobsDim  => this%addDimensionByName(MOBS_DIM_NAME,nchan_g)
        nobsDim  => this%addDimensionByName(NOBS_DIM_NAME,globalSize=npix_g*nscan_g)
        nlociDim => this%addDimensionByName(NLOCI_DIM_NAME,4)

        ! FIXME: refactor this code to bring these details into the DataSet framework in an intuitive way
        ! now use MPI to allgather the offsets
        allocate(allnobs(pinfo%getCommSize()))

        nobs_l(1) = npix_l*nscan_l

        ! gather every processes' nobs_l into allnobs to be shared
        call mpi_allgather(nobs_l, 1, MPI_INTEGER, allnobs, 1, &
            MPI_INTEGER, pinfo%getCommunicator(), ierr)

        allocate(mobsExtent)
        call mobsExtent%dataExtentConstructor(mobsDim)
        allocate(nlociExtent)
        call nlociExtent%dataExtentConstructor(nlociDim)

        ! the local offset (zero-based)
        nobs_s = 0

        if (all(allnobs == npix_g*nscan_g)) then
            ! the TB data is shared globally
            allocate(nobsExtent)
            call nobsExtent%dataExtentConstructor(nobsDim)
        else
            ! TB already decomped. Migrate the 3D tb/lat/lon to the 2D obsData/obsLoci.

            ! pinfo%getRank() is zero-based, which is equivalent to doing rank - 1
            do i=1,pinfo%getRank()
                nobs_s = nobs_s + allnobs(i)
            end do

            allocate(nobsExtent)
            call nobsExtent%dataExtentConstructor(nobsDim,localCount=nobs_l(1),&
                localStart=nobs_s+1)
        end if

        obsDataVar => this%addVariable(pinfo,OBS_DATA_VAR_NAME, obsData, &
            & mobsExtent, nobsExtent)
        obsLociVar => this%addVariable(pinfo,OBS_LOCI_VAR_NAME, obsLoci, &
            & nlociExtent, nobsExtent)

        iobs = 0

        do j=1,nscan_l
            do i=1,npix_l
                iobs = iobs + 1
                if (isReal) then
                    obsLoci(SO_LAT_DIM,iobs)  = lat_real(i,j)
                    obsLoci(SO_LON_DIM,iobs)  = lon_real(i,j)
                else
                    obsLoci(SO_LAT_DIM,iobs)  = lat_dble(i,j)
                    obsLoci(SO_LON_DIM,iobs)  = lon_dble(i,j)
                end if
                obsLoci(SO_PIX_DIM,iobs)  = pixs + i - 1
                obsLoci(SO_SCAN_DIM,iobs) = scans + j - 1

                do chnum=1,nchan_l
                    if (isReal) then
                        obsData(chnum,iobs) = tb_real(chnum,i,j)
                    else
                        obsData(chnum,iobs) = tb_dble(chnum,i,j)
                    end if
                end do
            end do
        end do

        ! now that the lat/lon/tb have been migrated to the 2d obsLoci/obsData format, remove them
        if (this%hasVariable(latVar%getName())) then
            call this%deleteVariable(latVar)
        end if

        if (this%hasVariable(lonVar%getName())) then
            call this%deleteVariable(lonVar)
        end if

        if (this%hasVariable(tbVar%getName())) then
            call this%deleteVariable(tbVar)
        end if

        call this%loadSatelliteObservation(pinfo,obsDataVar,obsLociVar)
    end subroutine

    subroutine loadSatelliteObservation(this, pinfo, obsDataVar, obsLociVar, &
        auxDataVar, qcCodesVar, ownerVar, contribVar)

        implicit none

        class(SatelliteObservation)   :: this

        class(ParallelInfo),            pointer    :: pinfo
        class(DataVariable),            pointer    :: obsDataVar
        class(DataVariable),            pointer    :: obsLociVar
        class(DataVariable), optional,  pointer    :: auxDataVar
        class(DataVariable), optional,  pointer    :: qcCodesVar
        class(DataVariable), optional,  pointer    :: ownerVar
        class(DataVariable), optional,  pointer    :: contribVar

        ! now load the observation with the given variables
        call this%loadObservation(pinfo,obsDataVar,obsLociVar,auxDataVar,qcCodesVar,&
            ownerVar,contribVar)
    end subroutine

    function getSatellitePlatform(this) result(satellitePlatform)
        implicit none

        class(SatelliteObservation)  :: this

        class(SatellitePlatformInfo), pointer :: satellitePlatform

        satellitePlatform => this%satellitePlatform

    end function

!    function getBrightnessTemps(this) result(tbPtr)
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        real(real32), dimension(:,:,:), pointer :: tbPtr
!
!        class(DataVariable), pointer :: tbVar
!
!        tbVar => this%getTbVar()
!
!        call tbVar%getArray(tbPtr)
!    end function
!
!    function getLatitudes(this) result(lat)
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        real(real32), dimension(:,:), pointer :: lat
!
!        class(DataVariable), pointer :: latVar
!
!        latVar => this%getLatVar()
!
!        call latVar%getArray(lat)
!    end function
!
!    function getLongitudes(this) result(lon)
!
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        real(real32), dimension(:,:), pointer :: lon
!
!        class(DataVariable), pointer :: lonVar
!
!        lonVar => this%getLonVar()
!
!        call lonVar%getArray(lon)
!    end function

    function getCraftLatitudes(this) result(scLat)

        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:), pointer :: scLat

        class(DataVariable), pointer :: scLatVar

        scLatVar => this%getScLatVar()

        call scLatVar%getArray(scLat)
    end function

    function getCraftLongitudes(this) result(scLon)

        implicit none

        class(SatelliteObservation)  :: this

        real(real32), dimension(:), pointer :: scLon

        class(DataVariable), pointer :: scLonVar

        scLonVar => this%getScLonVar()

        call scLonVar%getArray(scLon)
    end function

!    function getPixelDim(this) result(pixDim)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataDimension), pointer :: pixDim
!
!        pixDim => this%getDimensionByName(PIXELS_DIM_NAME)
!    end function
!
!    function getScanDim(this) result(scanDim)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataDimension), pointer :: scanDim
!
!        scanDim => this%getDimensionByName(SCANS_DIM_NAME)
!    end function
!
!    function getChannelDim(this) result(chanDim)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataDimension), pointer :: chanDim
!
!        chanDim => this%getDimensionByName(CHANS_DIM_NAME)
!    end function
!
!    function getPixelExtent(this) result(pixExtent)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataExtent), pointer :: pixExtent
!
!        class(DataVariable), pointer :: tbVar
!
!        tbVar => this%getTbVar()
!
!        pixExtent => tbVar%getExtentNumber(2)
!    end function
!
!    function getScanExtent(this) result(scanExtent)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataExtent), pointer :: scanExtent
!
!        class(DataVariable), pointer :: tbVar
!
!        tbVar => this%getTbVar()
!
!        scanExtent => tbVar%getExtentNumber(3)
!    end function
!
!    function getChannelExtent(this) result(chanExtent)
!
!        implicit none
!
!        class(SatelliteObservation)   :: this
!
!        class(DataExtent), pointer :: chanExtent
!
!        class(DataVariable), pointer :: tbVar
!
!        tbVar => this%getTbVar()
!
!        chanExtent => tbVar%getExtentNumber(1)
!    end function

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

!    function getTbVar(this) result(tbVar)
!
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        class(DataVariable), pointer :: tbVar
!
!        tbVar => this%getVariableByName(TB_VAR_NAME)
!    end function
!
!    function getLatVar(this) result(latVar)
!
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        class(DataVariable), pointer :: latVar
!
!        latVar => this%getVariableByName(LAT_VAR_NAME)
!    end function
!
!    function getLonVar(this) result(lonVar)
!
!        implicit none
!
!        class(SatelliteObservation)  :: this
!
!        class(DataVariable), pointer :: lonVar
!
!        lonVar => this%getVariableByName(LON_VAR_NAME)
!    end function

    function getScLatVar(this) result(scLatVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: scLatVar

        if (this%hasVariable(SC_LAT_VAR_NAME)) then
            scLatVar => this%getVariableByName(SC_LAT_VAR_NAME)
        else
            scLatVar => null()
        end if
    end function

    function getScLonVar(this) result(scLonVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: scLonVar

        if (this%hasVariable(SC_LON_VAR_NAME)) then
            scLonVar => this%getVariableByName(SC_LON_VAR_NAME)
        else
            scLonVar => null()
        end if
    end function

    function getYearVar(this) result(yearVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: yearVar

        if (this%hasVariable(YEAR_VAR_NAME)) then
            yearVar => this%getVariableByName(YEAR_VAR_NAME)
        else
            yearVar => null()
        end if
    end function

    function getMonthVar(this) result(monthVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: monthVar

        if (this%hasVariable(MONTH_VAR_NAME)) then
            monthVar => this%getVariableByName(MONTH_VAR_NAME)
        else
            monthVar => null()
        end if
    end function

    function getDayVar(this) result(dayVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: dayVar

        if (this%hasVariable(DAY_VAR_NAME)) then
            dayVar => this%getVariableByName(DAY_VAR_NAME)
        else
            dayVar => null()
        end if
    end function

    function getHourVar(this) result(hourVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: hourVar

        if (this%hasVariable(HOUR_VAR_NAME)) then
            hourVar => this%getVariableByName(HOUR_VAR_NAME)
        else
            hourVar => null()
        end if
    end function

    function getMinuteVar(this) result(minuteVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: minuteVar

        if (this%hasVariable(MINUTE_VAR_NAME)) then
            minuteVar => this%getVariableByName(MINUTE_VAR_NAME)
        else
            minuteVar => null()
        end if
    end function

    function getSecondVar(this) result(secondVar)

        implicit none

        class(SatelliteObservation)  :: this

        class(DataVariable), pointer :: secondVar

        if (this%hasVariable(SECOND_VAR_NAME)) then
            secondVar => this%getVariableByName(SECOND_VAR_NAME)
        else
            secondVar => null()
        end if
    end function

    subroutine writeSatObsToFile(this,pinfo,writer)
        implicit none

        class(SatelliteObservation)        :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayWriter), pointer    :: writer

        call this%writeObsToFile(pinfo,writer)

!        integer, optional,      intent(in) :: offset
!        logical, optional,      intent(in) :: copyObsToTb
!
!        class(DataDimension), pointer :: pixDim
!        class(DataDimension), pointer :: scanDim
!        class(DataDimension), pointer :: nobsDim
!
!        class(DataExtent), pointer :: scanExtent
!        class(DataExtent), pointer :: pixExtent
!
!        class(DataVariable), pointer :: tbVar
!        class(DataVariable), pointer :: obsLociVar
!        class(DataVariable), pointer :: obsDataVar
!        real(real32),        pointer :: tb(:,:,:)
!
!        real(real64),        pointer :: obsLoci(:,:)
!        real(real64),        pointer :: obsData(:,:)
!
!        integer :: iobs, chnum, chseq, obsi, obsj
!        integer :: xs, ys
!
!        integer :: choffset
!        logical :: doCopy
!
!        if (present(offset)) then
!            choffset = offset
!        else
!            choffset = this%getUniqueChannelOffset()
!        end if
!
!        if (present(copyObsToTb)) then
!            doCopy = copyObsToTb
!        else
!            doCopy = .true.
!        end if
!
!        nobsDim => this%getNObsDim()
!        pixDim  => this%getPixelDim()
!        scanDim => this%getScanDim()
!
!        call writer%writeDimension(pinfo, this%getPixelDim())
!        call writer%writeDimension(pinfo, this%getScanDim())
!        call writer%writeDimension(pinfo, this%getChannelDim())

!        tbVar => this%getTbVar()

!        if (doCopy) then
!            !tbVar => tbVar%clone(copyData=.false.)
!            call tbVar%getArray(tb)
!
!            tb = -999
!
!            obsLociVar => this%getObsLociVar()
!
!            if (.not. associated(obsLociVar)) then
!                call error('The obs loci variable must be associated to write the satellite observation.')
!            end if
!
!            pixExtent  => this%getPixelExtent()
!            scanExtent => this%getScanExtent()
!
!            xs = pixExtent%getLocalStart()
!            ys = scanExtent%getLocalStart()
!
!            print *,'xs/ys is:',pinfo%getRank(),xs,ys,shape(tb)
!
!            call obsLociVar%getArray(obsLoci)
!
!            obsDataVar => this%getObsDataVar()
!            call obsDataVar%getArray(obsData)
!
!            do iobs=1,size(obsLoci,2)
!                obsi = nint(obsLoci(SO_PIX_DIM,iobs)) !-xs+1
!                obsj = nint(obsLoci(SO_SCAN_DIM,iobs))!-ys+1
!
!                if (obsi < 1 .or. obsj < 1) then
!                    ! FIXME: apparently too many observations are being copied in somewhere
!                    !print *,'iobs/obsi/obsj was wrong:',pinfo%getRank(),iobs,obsi,obsj
!                    cycle
!                end if
!
!                do chnum=1,size(obsData,1)
!                    chseq = chnum+choffset
!
!                    tb(chseq,obsi,obsj) = obsData(chnum,iobs)
!                end do
!            end do
!        end if

!        call writer%writeVariable(pinfo, tbVar)
!        call writer%writeVariable(pinfo, this%getLatVar())
!        call writer%writeVariable(pinfo, this%getLonVar())
!
!        if (associated(this%hasVariable(SC_LAT_VAR_NAME))) then
!            call writer%writeVariable(pinfo, this%getScLatVar())
!        end if
!
!        if (associated(this%hasVariable(SC_LON_VAR_NAME))) then
!            call writer%writeVariable(pinfo, this%getScLonVar())
!        end if
    end subroutine

    ! Note that this subroutine only is for loading from the format in writeSatObsToFile.
    ! Observations corresponding to real satellite obs should be loaded in a subclass
    subroutine loadSatObsFromFile(this,pinfo)
        implicit none

        class(SatelliteObservation)     :: this
        class(ParallelInfo),    pointer :: pinfo

        class(DataVariable), pointer :: obsDataVar
        class(DataVariable), pointer :: obsLociVar

        class(DataDimension), pointer :: mobsDim
        class(DataDimension), pointer :: nobsDim

        call this%loadObsFromFile(pinfo)

    end subroutine

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
        if (associated(this%reader)) then
            call soptr%satelliteObservationConstructor(this%satellitePlatform,&
                & reader=this%reader%clone(),choffset=this%choffset,&
                & obsErr=this%obsErr,uniqueChoffset=this%uniquechoffset)
        else
            call soptr%satelliteObservationConstructor(this%satellitePlatform,&
                & choffset=this%choffset,obserr=this%obsErr,&
                & uniqueChoffset=this%uniquechoffset)
        end if
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

        !class(DataVariable),  pointer :: tbVar  => null()
        !class(DataVariable),  pointer :: latVar => null()
        !class(DataVariable),  pointer :: lonVar => null()

        class(DataVariable),  pointer :: scLatVar  => null()
        class(DataVariable),  pointer :: scLonVar  => null()
        class(DataVariable),  pointer :: yearVar   => null()
        class(DataVariable),  pointer :: monthVar  => null()
        class(DataVariable),  pointer :: dayVar    => null()
        class(DataVariable),  pointer :: hourVar   => null()
        class(DataVariable),  pointer :: minuteVar => null()
        class(DataVariable),  pointer :: secondVar => null()

        !class(DataExtent), pointer :: pixExtent, scanExtent, chanExtent
        class(Observation), pointer :: optr

        optr => this%cloneObsSubset(pinfo,nobsNewExtent,localInds)

        select type (optr)
            class is (SatelliteObservation)
                soPtr => optr
            class default
                call error('Unknown satellite observation type')
        end select

        scLatVar  => this%getScLatVar()
        scLonVar  => this%getScLonVar()
        yearVar   => this%getYearVar()
        monthVar  => this%getMonthVar()
        dayVar    => this%getDayVar()
        hourVar   => this%getHourVar()
        minuteVar => this%getMinuteVar()
        secondVar => this%getSecondVar()

        ! just dump all of these variables into the clone - in general the 1D nobsExtent
        ! is not compatible with these other 2D and 3D variables.
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
    end function

    function cloneSubset(this,pinfo,nobsNewExtent,localInds) result(optr)
        implicit none

        class(SatelliteObservation)     :: this

        class(ParallelInfo), pointer    :: pinfo
        class(DataExtent),   pointer    :: nobsNewExtent
        integer,             intent(in) :: localInds(:)

        class(Observation),  pointer    :: optr

        class(SatelliteObservation),  pointer :: soptr

        soptr => this%cloneSatObsSubset(pinfo,nobsNewExtent,localInds)
        optr => soptr
    end function
end module
