module amsrObservation_mod

    use parallelInfo_mod

    use iso_fortran_env

    use linkedList_mod

    use observation_mod
    use platformInfo_mod

    use satelliteObservation_mod
    use satellitePlatformInfo_mod
    use conicalScanningObservation_mod

    use dataSet_mod
    use dataGroup_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataAttribute_mod
    use dataDimension_mod
    use dataArrayReader_mod
    use hdfDataArrayReader_mod

    use radianceAssimilationConstants_mod

    use datetime_mod
    use strUtils_mod
    use hdfUtils_mod
    use geodesic_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(ConicalScanningObservation), public :: AmsrObservation
        contains
            procedure :: amsrObservationConstructor
            final     :: amsrObservationDestructor

            procedure :: cloneAmsrObs
            procedure :: clone
    end type

    contains

    subroutine amsrObservationConstructor(this,pinfo,filename,postingNum,amsrType)

        implicit none

        class(AmsrObservation)             :: this

        class(ParallelInfo),    pointer    :: pinfo
        character(len=*),       intent(in) :: filename
        integer,                intent(in) :: postingNum
        integer,                intent(in) :: amsrType

        class(DataDimension), pointer    :: pixDim,    scanDim,    chanDim
        class(DataDimension), pointer    :: pix89aDim, scan89aDim, chan89aDim

        class(DataExtent), pointer :: pixExtent, scanExtent

        class(DataVariable),  pointer    :: tbVar
        class(DataVariable),  pointer    :: ch1Var
        class(DataVariable),  pointer    :: ch2Var
        class(DataVariable),  pointer    :: latVar
        class(DataVariable),  pointer    :: lonVar
        class(DataVariable),  pointer    :: lat89aVar
        class(DataVariable),  pointer    :: lon89aVar
        class(DataVariable),  pointer    :: navDataVar
        class(DataVariable),  pointer    :: scLatVar
        class(DataVariable),  pointer    :: scLonVar
        class(DataVariable),  pointer    :: scanTimeVar
        class(DataVariable),  pointer    :: yearVar
        class(DataVariable),  pointer    :: monthVar
        class(DataVariable),  pointer    :: dayVar
        class(DataVariable),  pointer    :: hourVar
        class(DataVariable),  pointer    :: minuteVar
        class(DataVariable),  pointer    :: secondVar

        class(DataAttribute), pointer :: attr

        class(LinkedList),    pointer :: crParamsList

        !integer, intent(in), optional :: naux

        integer,      dimension(:),     pointer :: channelSubset

        class(SatellitePlatformInfo),   pointer :: satellitePlatform

        real(real32), dimension(:,:),   pointer :: lat89a
        real(real32), dimension(:,:),   pointer :: lon89a
        real(real32), dimension(:,:),   pointer :: lat89b
        real(real32), dimension(:,:),   pointer :: lon89b

!        integer,      dimension(:,:),   pointer :: azimuth_int

        real(real32), dimension(:),     pointer :: scLat
        real(real32), dimension(:),     pointer :: scLon
        integer                                 :: nchannels
        integer,      dimension(:),     pointer :: year
        integer(1),   dimension(:),     pointer :: month
        integer(1),   dimension(:),     pointer :: day
        integer(1),   dimension(:),     pointer :: hour
        integer(1),   dimension(:),     pointer :: minute
        integer(1),   dimension(:),     pointer :: second

        real(real32), dimension(:,:,:), pointer :: tb
        real(real32), dimension(:,:),   pointer :: ch1
        real(real32), dimension(:,:),   pointer :: ch2

        real(real64), dimension(:),     pointer :: fovAlong
        real(real64), dimension(:),     pointer :: fovCross
        real(real64), dimension(:),     pointer :: intTime
        real(real64), dimension(:),     pointer :: obsErr

        class(ParallelInfo),            pointer    :: pinfo_copy

        type(datetime)  :: tai93, scanDt
        type(timedelta) :: tai93diff

        integer :: i, j, i2, j2, ierr

        character(:), allocatable :: loadFrom

        ! Spin-frequency in RPM
        real(real64) :: spinFrequency = 40.d0
        integer :: choffset
        integer :: uniquechoffset

        character(len=1024), pointer :: cr_params(:)

        integer :: nparam, nsplit, firstIndex

        integer, parameter :: maxAttrStrLen = 255

        character(maxAttrStrLen), dimension(:), pointer :: cr_splits_str
        real(real64),             dimension(:), pointer :: a1_params
        real(real64),             dimension(:), pointer :: a2_params
        character(maxAttrStrLen), dimension(:), pointer :: satAltitude_str

        integer :: npix, nscan
        integer :: xs, xe, nx, ys, ye, ny

        real(real64) :: satAltitude

        real(real64), parameter :: deg2rad = acos(-1.0d0)/180.d0
        real(real64), parameter :: rad2deg = 180.d0/acos(-1.0d0)

        real(real32), dimension(:,:),   pointer :: lat1, lon1, lat2, lon2
        real(real64), dimension(:,:),   pointer :: theta, tmp2d
        real(real64), dimension(:,:,:), pointer :: p1, p2, pt, ex, ey, ez

        real(real32), dimension(:,:),   pointer :: lat, lon

        real(real32), dimension(:,:),   pointer :: navData
        real(real64), dimension(:),     pointer :: scanTime

        real(real32), dimension(:,:), pointer :: singleChTb

        real(real64) :: s12, azi1, azi2, s12min, s12minAll

        real(real64) :: prm1, prm2, nadirAngle, scLonr8, scLatr8

        character(len=255) :: ch1Str, ch2Str

        class(HdfDataArrayReader), pointer :: hdfReader
        class(DataArrayReader),    pointer :: reader

        nchannels = 2

        allocate(fovAlong     (nchannels))
        allocate(fovCross     (nchannels))
        allocate(intTime      (nchannels))
        allocate(obsErr       (nchannels))
        allocate(channelSubset(nchannels))

        select case(postingNum)
            case (1)
                ch1Str = "/Brightness Temperature (6.9GHz,V)"
                ch2Str = "/Brightness Temperature (6.9GHz,H)"
                fovCross = (/35.0,  35.0/)
                fovAlong = (/62.0,  62.0/)
                intTime  = (/ 2.5,   2.5/)
                obsErr   = (/ 0.34,  0.34/)

                channelSubset = (/1, 2/)
                choffset = 0
                uniquechoffset = 0
            case (2)
                ch1Str = "/Brightness Temperature (7.3GHz,V)"
                ch2Str = "/Brightness Temperature (7.3GHz,H)"
                fovCross = (/35.0,  35.0/)
                fovAlong = (/62.0,  62.0/)
                intTime  = (/ 2.5,   2.5/)
                obsErr   = (/ 0.43,  0.43/)

                channelSubset = (/3, 4/)
                choffset = 2
                uniquechoffset = 2
            case (3)
                ch1Str = "/Brightness Temperature (10.7GHz,V)"
                ch2Str = "/Brightness Temperature (10.7GHz,H)"
                fovCross = (/24.0, 24.0/)
                fovAlong = (/42.0, 42.0/)
                intTime  = (/ 2.5,  2.5/)
                obsErr   = (/ 0.7,  0.7/)

                channelSubset = (/5, 6/)
                choffset = 4
                uniquechoffset = 4
            case (4)
                ch1Str = "/Brightness Temperature (18.7GHz,V)"
                ch2Str = "/Brightness Temperature (18.7GHz,H)"
                fovCross = (/14.0, 14.0/)
                fovAlong = (/22.0, 22.0/)
                intTime  = (/ 2.5,  2.5/)
                obsErr   = (/ 0.7,  0.7/)

                channelSubset = (/7, 8/)
                choffset = 6
                uniquechoffset = 6
            case (5)
                ch1Str = "/Brightness Temperature (23.8GHz,V)"
                ch2Str = "/Brightness Temperature (23.8GHz,H)"
                fovCross = (/15.0, 15.0/)
                fovAlong = (/26.0, 26.0/)
                intTime  = (/ 2.5,  2.5/)
                obsErr   = (/ 0.6,  0.6/)

                channelSubset = (/9, 10/)
                choffset = 8
                uniquechoffset = 8
            case (6)
                ch1Str = "/Brightness Temperature (36.5GHz,V)"
                ch2Str = "/Brightness Temperature (36.5GHz,H)"
                fovCross = (/ 7.0,  7.0/)
                fovAlong = (/12.0, 12.0/)
                intTime  = (/ 2.5,  2.5/)
                obsErr   = (/ 0.7,  0.7/)

                channelSubset = (/11, 12/)
                choffset = 10
                uniquechoffset = 10
            case (7)
                ch1Str = "/Brightness Temperature (89.0GHz-A,V)"
                ch2Str = "/Brightness Temperature (89.0GHz-A,H)"
                fovCross = (/ 3.0,  3.0/)
                fovAlong = (/ 5.0,  5.0/)
                intTime  = (/ 1.2,  1.2/)
                obsErr   = (/ 1.2,  1.2/)

                channelSubset = (/13, 14/)
                choffset = 12
                uniquechoffset = 12
            case (8)
                ch1Str = "/Brightness Temperature (89.0GHz-B,V)"
                ch2Str = "/Brightness Temperature (89.0GHz-B,H)"
                fovCross = (/ 3.0,  3.0/)
                fovAlong = (/ 5.0,  5.0/)
                intTime  = (/ 1.2,  1.2/)
                obsErr   = (/ 1.4,  1.4/)

                channelSubset = (/13, 14/)
                choffset = 14
                uniquechoffset = 12
            case default
                write(msgstr,*) 'Error in AMSR constructor: unknown posting ',postingNum
                call error(msgstr)
        end select

        allocate(satellitePlatform)
        call satellitePlatform%satellitePlatformInfoConstructor(PLATFORM_AMSR_2,'GCOM-W',&
            & 'AMSR-2',nchannels,channelSubset)

        allocate(hdfReader)
        call hdfReader%hdfDataArrayReaderConstructor(filename,littleEndian=.true.)
        reader => hdfReader

        call this%conicalScanningObservationConstructor(satellitePlatform, &
            & reader,fovAlong=fovAlong,fovCross=fovCross,intTime=intTime,  &
            & choffset=choffset,spinFrequency=spinFrequency,obsErr=obsErr)

        attr => this%loadAttribute(pinfo,'CoRegistrationParameterA1',crParamsList, &
            & maxAttrStrLen)

        call strsplit(trim(attr%getStringValue(1)),',',cr_splits_str,nparam)

        allocate(a1_params(nparam))

        do i=1,nparam
            firstIndex = index(trim(cr_splits_str(i)),'-')
            if (firstIndex == 0) then
                write(msgstr,*) 'Error: invalid A1 param string: ',trim(cr_splits_str(i))
                call print(msgstr)
            end if
            a1_params(i) = str2double(cr_splits_str(i)(firstIndex+1:len_trim(cr_splits_str(i))))
        end do

        deallocate(cr_params)
        deallocate(cr_splits_str)

        attr => this%loadAttribute(pinfo,'CoRegistrationParameterA2',crParamsList,&
            & maxAttrStrLen)

        call strsplit(trim(attr%getStringValue(1)),',',cr_splits_str,nparam)

        allocate(a2_params(nparam))

        do i=1,nparam
            firstIndex = index(trim(cr_splits_str(i)),'-')
            a2_params(i) = str2double(cr_splits_str(i)(firstIndex+1:len_trim(cr_splits_str(i))))
        end do

        if (postingNum == nparam+1) then
            ! 89A
            loadFrom = "/Latitude of Observation Point for 89A"

            pixDim  => this%loadDimensionFromVariable(pinfo,PIXELS_DIM_NAME,1,loadFrom)
            scanDim => this%loadDimensionFromVariable(pinfo,SCANS_DIM_NAME, 2,loadFrom)
            chanDim => this%addDimensionByName(CHANS_DIM_NAME,2)

            latVar => this%loadVariable(pinfo,LAT_VAR_NAME,lat,&
                & pixDim,scanDim,loadFrom)
            lonVar => this%loadVariable(pinfo,LON_VAR_NAME,lon,&
                & pixDim,scanDim,"/Longitude of Observation Point for 89A")
        else if (postingNum == nparam+2) then
            ! 89B
            loadFrom = "/Latitude of Observation Point for 89B"

            pixDim  => this%loadDimensionFromVariable(pinfo,PIXELS_DIM_NAME,1,loadFrom)
            scanDim => this%loadDimensionFromVariable(pinfo,SCANS_DIM_NAME, 2,loadFrom)
            chanDim => this%addDimensionByName(CHANS_DIM_NAME,2)

            latVar => this%loadVariable(pinfo,LAT_VAR_NAME,lat,&
                & pixDim,scanDim,loadFrom)
            lonVar => this%loadVariable(pinfo,LON_VAR_NAME,lon,&
                & pixDim,scanDim,"/Longitude of Observation Point for 89B")
        else
            ! all other channels in order of registration
            if (postingNum < 1 .or. postingNum > nparam + 2) then
                write(msgstr,*) 'Invalid AMSR A2 posting: ',postingNum,nparam,trim(cr_params(1))
                call error(msgstr)
            end if

            prm1 = a1_params(postingNum)
            prm2 = a2_params(postingNum)

            loadFrom = "/Latitude of Observation Point for 89A"

            ! get a parallel info to get a complete copy of lat/lon89a (either local or mirrored)
            pinfo_copy => pinfo%getMirror()

            pix89aDim  => this%loadDimensionFromVariable(pinfo_copy,'89A pixels',1,loadFrom)
            scan89aDim => this%loadDimensionFromVariable(pinfo_copy,'89A scans', 2,loadFrom)
            chan89aDim => this%addDimensionByName('89A TB',2)

            lat89aVar => this%loadVariable(pinfo_copy,'LAT89A',lat89a,&
                & pixDim,scanDim,"/Latitude of Observation Point for 89A")
            lon89aVar => this%loadVariable(pinfo_copy,'LON89A',lon89a,&
                & pixDim,scanDim,"/Longitude of Observation Point for 89A")

            pixDim  => this%addDimensionByName(PIXELS_DIM_NAME, pix89aDim%getGlobalCount()/2)
            scanDim => this%addDimensionByName(SCANS_DIM_NAME, scan89aDim%getGlobalCount())
            chanDim => this%addDimensionByName(CHANS_DIM_NAME, 2)

            latVar => this%addVariable(pinfo,LAT_VAR_NAME,lat,pixDim,scanDim)
            lonVar => this%addVariable(pinfo,LON_VAR_NAME,lon,pixDim,scanDim)

            npix = size(lat89a,1)

            lat1 => lat89a(  1:npix-1:2,:)
            lat2 => lat89a(1+1:npix  :2,:)

            lon1 => lon89a(  1:npix-1:2,:)
            lon2 => lon89a(1+1:npix  :2,:)

            pixExtent  => latVar%getExtentNumber(1)
            scanExtent => latVar%getExtentNumber(2)

            call  pixExtent%getLocalTotalRange(xs,xe,nx)
            call scanExtent%getLocalTotalRange(ys,ye,ny)

            allocate(p1(nx,ny,3))
            allocate(p2(nx,ny,3))

            do j=1,ny
                do i=1,nx
                    i2 = i+xs-1
                    j2 = j+ys-1
                    p1(i,j,1) = cos(lon1(i2,j2)*deg2rad)*cos(lat1(i2,j2)*deg2rad)
                    p1(i,j,2) = sin(lon1(i2,j2)*deg2rad)*cos(lat1(i2,j2)*deg2rad)
                    p1(i,j,3) = sin(lat1(i2,j2)*deg2rad)

                    p2(i,j,1) = cos(lon2(i2,j2)*deg2rad)*cos(lat2(i2,j2)*deg2rad)
                    p2(i,j,2) = sin(lon2(i2,j2)*deg2rad)*cos(lat2(i2,j2)*deg2rad)
                    p2(i,j,3) = sin(lat2(i2,j2)*deg2rad)
                end do
            end do

            allocate(theta(nx,ny))

            theta = acos(p1(:,:,1)*p2(:,:,1)+p1(:,:,2)*p2(:,:,2)+p1(:,:,3)*p2(:,:,3))

            allocate(ex(nx,ny,3))
            ex = p1

            allocate(tmp2d(nx,ny))

            tmp2d = sqrt(p1(:,:,1)*p1(:,:,1)+p1(:,:,2)*p1(:,:,2)+p1(:,:,3)*p1(:,:,3))* &
                    sqrt(p2(:,:,1)*p2(:,:,1)+p2(:,:,2)*p2(:,:,2)+p2(:,:,3)*p2(:,:,3))* &
                    sin(theta)

            allocate(ez(nx,ny,3))

            ez(:,:,1) = (p1(:,:,2)*p2(:,:,3)-p1(:,:,3)*p2(:,:,2))/tmp2d
            ez(:,:,2) = (p1(:,:,3)*p2(:,:,1)-p1(:,:,1)*p2(:,:,3))/tmp2d
            ez(:,:,3) = (p1(:,:,1)*p2(:,:,2)-p1(:,:,2)*p2(:,:,1))/tmp2d

            allocate(ey(nx,ny,3))

            ey(:,:,1) = ez(:,:,2)*ex(:,:,3)-ez(:,:,3)*ex(:,:,2)
            ey(:,:,2) = ez(:,:,3)*ex(:,:,1)-ez(:,:,1)*ex(:,:,3)
            ey(:,:,3) = ez(:,:,1)*ex(:,:,2)-ez(:,:,2)*ex(:,:,1)

            allocate(pt(nx,ny,3))

            tmp2d = cos(prm1*theta)

            pt(:,:,1) = tmp2d*ex(:,:,1)
            pt(:,:,2) = tmp2d*ex(:,:,2)
            pt(:,:,3) = tmp2d*ex(:,:,3)

            tmp2d = sin(prm1*theta)

            pt(:,:,1) = pt(:,:,1) + tmp2d*ey(:,:,1)
            pt(:,:,2) = pt(:,:,2) + tmp2d*ey(:,:,2)
            pt(:,:,3) = pt(:,:,3) + tmp2d*ey(:,:,3)

            tmp2d = cos(prm2*theta)

            pt(:,:,1) = tmp2d*pt(:,:,1)
            pt(:,:,2) = tmp2d*pt(:,:,2)
            pt(:,:,3) = tmp2d*pt(:,:,3)

            tmp2d = sin(prm2*theta)

            pt(:,:,1) = pt(:,:,1) + tmp2d*ez(:,:,1)
            pt(:,:,2) = pt(:,:,2) + tmp2d*ez(:,:,2)
            pt(:,:,3) = pt(:,:,3) + tmp2d*ez(:,:,3)

            tmp2d  = sqrt(pt(:,:,1)*pt(:,:,1) + pt(:,:,2)*pt(:,:,2))

            lat = atan2(pt(:,:,3),    tmp2d)*rad2deg
            lon = atan2(pt(:,:,2),pt(:,:,1))*rad2deg

            !deallocate(lat1)
            !deallocate(lat2)
            !deallocate(lon1)
            !deallocate(lon2)
            deallocate(p1)
            deallocate(p2)
            deallocate(ex)
            deallocate(tmp2d)
            deallocate(ez)
            deallocate(ey)
            deallocate(pt)
            deallocate(theta)
        end if

        deallocate(a1_params)
        deallocate(a2_params)

        ! todo: use this, possible to verify the locations. note if 89b the nadir angle is actually 47.
        nadirAngle = 47.5d0

        scLatVar  => this%addVariable(pinfo,SC_LAT_VAR_NAME, scLat, scanDim)
        scLonVar  => this%addVariable(pinfo,SC_LON_VAR_NAME, scLon, scanDim)

        navDataVar => this%loadVariable(pinfo,'/Navigation Data',navData,&
            & pixDim,scanDim)

        do i=1,ny
            call cartesianToGeodetic(dble(navData(1,i)),dble(navData(2,i)),dble(navData(3,i)),&
                &satAltitude, scLonr8, scLatr8,1)

            scLon(i) = scLonr8
            scLat(i) = scLatr8
        end do

        call this%deleteVariable(navDataVar)

        scanTimeVar => this%loadVariable(pinfo, '/Scan Time', scanTime, scanDim)

        yearVar   => this%addVariable(pinfo, YEAR_VAR_NAME,   year,   scanDim)
        monthVar  => this%addVariable(pinfo, MONTH_VAR_NAME,  month,  scanDim)
        dayVar    => this%addVariable(pinfo, DAY_VAR_NAME,    day,    scanDim)
        hourVar   => this%addVariable(pinfo, HOUR_VAR_NAME,   hour,   scanDim)
        minuteVar => this%addVariable(pinfo, MINUTE_VAR_NAME, minute, scanDim)
        secondVar => this%addVariable(pinfo, SECOND_VAR_NAME, second, scanDim)

        tai93 = datetime(1993,1,1,0,0,0)

        ! TODO: also take care of leap seconds.
        ! TODO: may need to worry about fractions of a second (currently rounded to the nearest second)
        do i=1,ny
            tai93diff = timedelta(seconds=nint(scanTime(i)))
            scanDt    = tai93 + tai93diff

            year(i)   = scanDt%getYear()
            month(i)  = scanDt%getMonth()
            day(i)    = scanDt%getDay()
            hour(i)   = scanDt%getHour()
            minute(i) = scanDt%getMinute()
            second(i) = scanDt%getSecond()
        end do

        call this%deleteVariable(scanTimeVar)

        nchannels = 2

        tbVar  => this%addVariable( pinfo, TB_VAR_NAME, tb,  pixDim, scanDim, chanDim)
        ch1Var => this%loadVariable(pinfo, 'Ch1',       ch1, pixDim, scanDim, ch1Str)
        ch2Var => this%loadVariable(pinfo, 'Ch2',       ch2, pixDim, scanDim, ch2Str)
        tb(1,:,:) = ch1*0.01
        tb(2,:,:) = ch2*0.01

        call this%deleteVariable(ch1Var)
        call this%deleteVariable(ch2Var)

        call this%loadConicalScanningObservation(pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
            & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar)

!        call this%conicalScanningObservationConstructor(satellitePlatform,finalLat,finalLon,scLat,scLon,&
!            &nchannels,tb,channelSubset,fovAlong,fovCross,s12min,intTime,choffset,&
!            &spinFrequency,obsErr,year,month,day,hour,minute,second,naux,uniquechoffset)
    end subroutine

    subroutine amsrObservationDestructor(this)
        implicit none

        type(AmsrObservation)  :: this

        ! called automatically as conicalScanningObservation is not abstract
        ! call this%conicalScanningObservationDestructor()
    end subroutine

    function cloneAmsrObs(this,shallow,copyData) result(aoptr)
        implicit none

        class(AmsrObservation), target :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataSet),         pointer :: dsPtr
        class(DataGroup),       pointer :: dgPtr
        class(AmsrObservation), pointer :: aoptr

        logical :: isShallow
        logical :: doCopy

        allocate(aoptr)
        call aoptr%conicalScanningObservationConstructor(this%getSatellitePlatform(), &
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
            dsPtr => aoptr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(AmsrObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),         pointer :: dsPtr
        class(AmsrObservation), pointer :: aoptr

        aoptr => this%cloneAmsrObs(shallow,copyData)
        dsPtr => aoptr
    end function
end module
