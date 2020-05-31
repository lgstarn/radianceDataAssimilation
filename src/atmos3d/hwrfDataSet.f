module HwrfDataSet_mod

    use parallelInfo_mod
    use parallelConstants_mod

    use iso_fortran_env

    use dataSet_mod
    use wrfDataSet_mod
    use atmos3dDataSet_mod

    use dataGrid_mod
    use regularTriangularLatLonGrid_mod

    use dataGroup_mod
    use dataVariable_mod
    use dataAttribute_mod
    use dataDimension_mod
    use dataArrayReader_mod

    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    public :: HwrfDataSet

    type, extends(WrfDataSet) :: HwrfDataSet
        private
            integer(int32) :: time
            real(real64)   :: dx_deg,dy_deg,lat1,lon1,offsetx,offsety
            real(real64)   :: cen_lat,cen_lon
            integer(int32) :: i_parent_start, j_parent_start
            integer(int32) :: num_land_cat

        contains
            procedure :: getLandCatCount => getLandCatCount_hwrf
            procedure :: clone
            procedure :: cloneHwrf
            procedure :: convertLatLonToIJ

            generic   :: hwrfDataSetConstructor => &
                & hwrfDataSetConstructor_file, &
                & hwrfDataSetConstructor_class
            procedure, private :: hwrfDataSetConstructor_file
            procedure, private :: hwrfDataSetConstructor_class
            procedure :: loadHwrfDataSet

            !procedure, private :: loadLevelThickness
            final :: hwrfDataSetDestructor ! clean up all allocated variables

    end type

!    interface HwrfDataSet
!        procedure hwrfDataSetConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine hwrfDataSetConstructor_file(this,pinfo,inputFile,time)
        implicit none

        class(HwrfDataSet)                 :: this

        class(ParallelInfo),    pointer    :: pinfo
        character(*), optional, intent(in) :: inputFile
        integer,      optional, intent(in) :: time

        call this%wrfDataSetConstructor(inputFile)

        if (present(time)) then
            this%time = time
        else
            this%time = 1
        end if

        if (associated(pinfo)) then
            call this%loadHwrfDataSet(pinfo)
        end if
    end subroutine

    subroutine hwrfDataSetConstructor_class(this,pinfo,reader,time)
        implicit none

        class(HwrfDataSet)                 :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayReader), pointer    :: reader
        integer,      optional, intent(in) :: time

        call this%wrfDataSetConstructor(reader)

        if (present(time)) then
            this%time = time
        else
            this%time = 1
        end if

        if (associated(pinfo)) then
            call this%loadHwrfDataSet(pinfo)
        end if
    end subroutine

    subroutine loadHwrfDataSet(this,pinfo)

        implicit none

        class(HwrfDataSet)  :: this
        class(ParallelInfo), pointer  :: pinfo

        class(DataDimension), pointer :: westEastDim
        class(DataDimension), pointer :: southNorthDim
        class(DataDimension), pointer :: bottomTopDim
        class(DataDimension), pointer :: bottomTopStagDim
        class(DataDimension), pointer :: timeDim

        class(DataVariable),  pointer :: pLevelVar
        class(DataVariable),  pointer :: tVar
        class(DataVariable),  pointer :: qvaporVar
        class(DataVariable),  pointer :: uVar
        class(DataVariable),  pointer :: vVar
        class(DataVariable),  pointer :: wVar
        class(DataVariable),  pointer :: cldfraVar
        class(DataVariable),  pointer :: cwmVar
        class(DataVariable),  pointer :: qcloudVar
        class(DataVariable),  pointer :: qiceVar
        class(DataVariable),  pointer :: qrainVar
        class(DataVariable),  pointer :: qsnowVar
        class(DataVariable),  pointer :: qgraupVar
        class(DataVariable),  pointer :: qhailVar
        class(DataVariable),  pointer :: fIceVar
        class(DataVariable),  pointer :: fRainVar
        class(DataVariable),  pointer :: fRimeFacVar
        class(DataVariable),  pointer :: dzVar
        class(DataVariable),  pointer :: u10Var
        class(DataVariable),  pointer :: v10Var
        class(DataVariable),  pointer :: tSurfVar
        class(DataVariable),  pointer :: soilTypeVar
        class(DataVariable),  pointer :: vegTypeVar
        class(DataVariable),  pointer :: luIndexVar
        class(DataVariable),  pointer :: sfcZVar
        class(DataVariable),  pointer :: latVar
        class(DataVariable),  pointer :: lonVar
        class(DataVariable),  pointer :: pSfcVar
        class(DataVariable),  pointer :: t2Var
        class(DataVariable),  pointer :: q2Var
        class(DataVariable),  pointer :: landMaskVar
        class(DataVariable),  pointer :: tLandVar
        class(DataVariable),  pointer :: tWaterVar

        class(DataAttribute), pointer :: attr

        integer :: nt,nzl
        real(8) :: x, y

        real(8), parameter :: RD  = 286.9968933d0  ! DRY GAS CONSTANT, J/(kg*degK)
        real(8), parameter :: Re  = 6371000.0d0    ! EARTH RADIUS, m
        real(8), parameter :: G0  = 9.80616d0      ! GRAVITY m/s**2
        real(8), parameter :: EPS = 0.621970585d0  ! RATIO OF MOLEC WT OF WATER AND DRY AIR
        real(8), parameter :: X_C = (1.0d0 - EPS) / EPS ! unitless

        integer :: t, z

        ! The 3d arrays (plus time, which will be squeezed out)
        real(real32), pointer :: cwm    (:,:,:,:)
        real(real32), pointer :: f_rain (:,:,:,:)
        real(real32), pointer :: f_ice  (:,:,:,:)
        real(real32), pointer :: f_rimef(:,:,:,:)
        real(real32), pointer :: q      (:,:,:,:)
        real(real32), pointer :: temp   (:,:,:,:)
        real(real32), pointer :: p      (:,:,:,:)
        real(real32), pointer :: cldfra (:,:,:,:)
        real(real32), pointer :: qcloud (:,:,:,:)
        real(real32), pointer :: qice   (:,:,:,:)
        real(real32), pointer :: qrain  (:,:,:,:)
        real(real32), pointer :: qsnow  (:,:,:,:)
        real(real32), pointer :: qgraup (:,:,:,:)
        real(real32), pointer :: qhail  (:,:,:,:)
        real(real32), pointer :: u      (:,:,:,:)
        real(real32), pointer :: v      (:,:,:,:)
        real(real32), pointer :: w      (:,:,:,:)
        real(real32), pointer :: dz     (:,:,:,:)

        ! The 2d arrays (plus time, which will be squeezed out)
        real(real32), pointer :: sfc_z   (:,:,:)
        real(real32), pointer :: u10     (:,:,:)
        real(real32), pointer :: v10     (:,:,:)
        real(real32), pointer :: tLand   (:,:,:)
        real(real32), pointer :: tWater  (:,:,:)
        real(real32), pointer :: tSurf   (:,:,:)
        integer,      pointer :: landmask(:,:,:)
        integer,      pointer :: soilType(:,:,:)
        integer,      pointer :: vegType (:,:,:)
        integer,      pointer :: luIndex (:,:,:)
        real(real32), pointer :: lat     (:,:,:)
        real(real32), pointer :: lon     (:,:,:)
        real(real32), pointer :: pSfc    (:,:,:)
        real(real32), pointer :: t2      (:,:,:)
        real(real32), pointer :: q2      (:,:,:)

        real(real64), pointer :: lat_global(:,:,:) => null()
        real(real64), pointer :: lon_global(:,:,:) => null()

        character(:), allocatable :: loadFrom

        class(DataGrid),                    pointer :: grid         => null()
        class(RegularTriangularLatLonGrid), pointer :: triGrid      => null()

        class(DataVariable),  pointer :: latVar_global => null()
        class(DataVariable),  pointer :: lonVar_global => null()

        loadFrom = 'CWM'

        westEastDim   => this%loadDimensionFromVariable(pinfo,WEST_EAST_DIM_NAME,   1, loadFrom)
        southNorthDim => this%loadDimensionFromVariable(pinfo,SOUTH_NORTH_DIM_NAME, 2, loadFrom)
        bottomTopDim  => this%loadDimensionFromVariable(pinfo,BOTTOM_TOP_DIM_NAME,  3, loadFrom)
        timeDim       => this%loadDimensionFromVariable(pinfo,TIME_DIM_NAME,        4, loadFrom)

        bottomTopStagDim => this%loadDimensionFromVariable(pinfo,BOTTOM_TOP_DIM_NAME,  3, 'PINT')

        call bottomTopStagDim%setStagger(1)

        nt = timeDim      %getGlobalSize()

        if (this%time > nt) then
            call error('The requested time is larger than the number of time steps in the file: ' // &
                int2str(this%time) // ' vs. ' // int2str(nt))
        else
            call timeDim%setGlobalRange(this%time,1)
        end if

        cwmVar   => this%loadVariable(pinfo,CWM_VAR,cwm,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'CWM')
        cwm = cwm*1000.d0 ! convert from kg/kg to g/kg

        fRainVar => this%loadVariable(pinfo,'F_RAIN',f_rain,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim)

        fIceVar => this%loadVariable(pinfo,'F_ICE',f_ice,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim)

        fRimeFacVar => this%loadVariable(pinfo,'F_RIMEF',f_rimef,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim)

        qvaporVar => this%loadVariable(pinfo,QVAPOR_VAR,q,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'Q')
        q = q*1000.d0 ! convert from kg/kg to g/kg

        tVar => this%loadVariable(pinfo,T_VAR,temp,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'T')

        pLevelVar => this%loadVariable(pinfo,P_LEVEL_VAR,p,&
            & westEastDim,southNorthDim,bottomTopStagDim,timeDim,'PINT')
        p = p/100.d0 ! convert from Pa to hPa

        sfcZVar => this%loadVariable(pinfo,SFC_HGT_VAR,sfc_z,&
            & westEastDim,southNorthDim,timeDim,'FIS')
        sfc_z = sfc_z/9.8d0 ! convert from geopotential height to m

        cldfraVar =>  this%loadVariable(pinfo,CLDFRA_VAR,cldfra,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'CLDFRA')

        qcloudVar => this%addVariable(pinfo, QCLOUD_VAR, qcloud, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qcloud = cwm*(1 - f_rain)*(1 - f_ice)

        qiceVar => this%addVariable(pinfo, QICE_VAR, qice, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qice = 0.d0; where (f_rimef <= 1.0001d0) qice = cwm*f_ice

        qrainVar => this%addVariable(pinfo, QRAIN_VAR, qrain, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qrain = cwm*f_rain*(1 - f_ice)

        qsnowVar => this%addVariable(pinfo, QSNOW_VAR, qsnow, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qsnow = 0; where(f_rimef > 1.0001d0 .and. f_rimef <= 5.d0) qsnow = cwm*f_ice

        qgraupVar => this%addVariable(pinfo, QGRAUP_VAR, qgraup, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qgraup = 0; where(f_rimef > 5.d0 .and. f_rimef <= 10.d0) qgraup = cwm*f_ice

        qhailVar => this%addVariable(pinfo, QHAIL_VAR, qhail, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)
        qhail = 0; where(f_rimef > 10.d0) qhail = cwm*f_ice

        uVar => this%loadVariable(pinfo,U_VAR,u,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'U')

        vVar => this%loadVariable(pinfo,V_VAR,v,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'V')

        wVar => this%loadVariable(pinfo,W_VAR,w,&
            & westEastDim,southNorthDim,bottomTopDim,timeDim,'W')

        dzVar => this%addVariable(pinfo, DZ_VAR, dz, &
            & westEastDim,southNorthDim,bottomTopDim,timeDim)

        ! z local count. Note that because p is staggered, it is length nzl+1 in that dimension
        nzl = size(u,3)

        ! Virtual T (K)
        ! tv = t*(1.0 + X_C*q)
        ! thickness of layer in meters
        ! RD  = 286.9968933  ! J/(kg*degK)
        ! G0  = 9.80616      ! GRAVITY m/s**2

        ! K * RD / G0
        ! K * J s^2 / (kg * K * m)
        ! kg m^2 s^2 / (s^2 kg * m)
        ! m
        dz = log(p(:,:,1:nzl,1:1)/p(:,:,2:nzl+1,1:1))*RD/G0
        dz = dz*temp*(1.0 + X_C*q)

        ! correct for gravity differences as a function of height
        do z =1,nzl
            ! 1st order correction for gravity fn of height
            dz(:,:,z,1:1) = dz(:,:,z,1:1) * ((Re+sfc_z + SUM(dz(:,:,1:z,1:1),3))/Re)**2
        end do

        u10Var => this%loadVariable(pinfo,U10_VAR,u10,&
            & westEastDim,southNorthDim,timeDim,'U10')

        v10Var => this%loadVariable(pinfo,V10_VAR,v10,&
            & westEastDim,southNorthDim,timeDim,'V10')

        tLandVar => this%loadVariable(pinfo,'TSHLTR',tLand,&
            & westEastDim,southNorthDim,timeDim)

        tWaterVar => this%loadVariable(pinfo,'SST',tWater,&
            & westEastDim,southNorthDim,timeDim)

        landMaskVar => this%loadVariable(pinfo,'LANDMASK',landmask,&
            & westEastDim,southNorthDim,timeDim)

        tSurfVar => this%addVariable(pinfo, T_SURF_VAR, tSurf, &
            & westEastDim,southNorthDim,timeDim)

        where(landMask == 1) tSurf = tLand
        where(landMask == 0) tSurf = tWater

        soilTypeVar => this%loadVariable(pinfo,SOIL_TYPE_VAR,soilType,&
            & westEastDim,southNorthDim,timeDim,'ISLTYP')

        vegTypeVar => this%loadVariable(pinfo,VEG_TYPE_VAR,vegType,&
            & westEastDim,southNorthDim,timeDim,'IVGTYP')

        luIndexVar => this%loadVariable(pinfo,LU_INDEX_VAR,luIndex,&
            & westEastDim,southNorthDim,timeDim,'LU_INDEX')

        latVar => this%loadVariable(pinfo,A3D_LAT_VAR,lat,&
            & westEastDim,southNorthDim,timeDim,'HLAT')

        lonVar => this%loadVariable(pinfo,A3D_LON_VAR,lon,&
            & westEastDim,southNorthDim,timeDim,'HLON')

        tSurfVar => this%addVariable(pinfo, T_SURF_VAR, tSurf, &
            & westEastDim,southNorthDim,timeDim)

        psfcVar => this%addVariable(pinfo, P_SFC_VAR, psfc, &
            & westEastDim,southNorthDim,timeDim)
        psfc(:,:,1) = p(:,:,1,1)

        ! since the output from MYJ is unreliable, just use the first model layer
        t2Var => this%addVariable(pinfo, T2_VAR, t2, westEastDim, southNorthDim, timeDim)
        t2(:,:,1) = temp(:,:,1,1)

        ! since the output from MYJ is unreliable, just use the first model layer
        q2Var => this%addVariable(pinfo, Q2_VAR, q2, westEastDim, southNorthDim, timeDim)
        q2(:,:,1) = q(:,:,1,1)

        allocate(triGrid)
        call triGrid%regularTriangularLatLonGridConstructor(latVar,lonVar)

        grid => triGrid

        call this%loadWrfDataSet(pinfo,grid,&!westEastDim,southNorthDim,bottomTopDim,
            & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,qrainVar,&
            & qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,vegTypeVar,    &
            & luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)

!        wrfEarthRadius = 6370d0*1000.d0
!        mToDegrees = 360.d0/2.0d0/3.141593d0/wrfEarthRadius

        attr => this%loadAttribute(pinfo, 'DX', this%dx_deg)
        attr => this%loadAttribute(pinfo, 'DY', this%dy_deg)

        attr => this%loadAttribute(pinfo, 'I_PARENT_START', this%i_parent_start)
        attr => this%loadAttribute(pinfo, 'J_PARENT_START', this%j_parent_start)

        attr => this%loadAttribute(pinfo, 'CEN_LAT', this%cen_lat)
        attr => this%loadAttribute(pinfo, 'CEN_LON', this%cen_lon)

        attr => this%loadAttribute(pinfo, 'NUM_LAND_CAT', this%num_land_cat)

!        dx = this%dx_deg/mToDegrees
!        dy = this%dy_deg/mToDegrees
!        latinc = dy*mToDegrees
!        loninc = dy*mToDegrees

        this%lat1 = lat(1,1,1)
        this%lon1 = lon(1,1,1)

        ! HWRF is a strange beast. The grid spacing is based on the parent, which might not be available.
        ! This is just a linear correction, so we compute the i/j location of the lat/lon pair of (1,1).
        ! Obviously i/j should be (1,1) for this, but it isn't due to the grid offset. The next lines of code
        ! therefore fix that problem.
        call this%convertLatLonToIJ(this%lat1,this%lon1,x,y)

        this%offsety = (y-1.d0)*this%dy_deg
        this%offsetx = (x-1.0d0)*this%dx_deg*2.d0
    end subroutine

    subroutine hwrfDataSetDestructor(this)
        implicit none

        ! this is commented out because it isn't a real(8) destructor - but subclasses should still call it
        type(HwrfDataSet)  :: this

        call this%wrfDataSetDestructor()
    end subroutine

    function getLandCatCount_hwrf(this) result(numLandCat)
        implicit none

        class(HwrfDataSet) :: this

        integer            :: numLandCat

        numLandCat = this%num_land_cat
    end function

    function clone(this,shallow,copyData) result(dgPtr)
        implicit none

        class(HwrfDataSet), target     :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(HwrfDataSet), pointer    :: hwrfPtr
        class(DataGroup),   pointer    :: dgPtr

        hwrfPtr => this%cloneHwrf(copyData)
        dgPtr => hwrfPtr

    end function

    function cloneHwrf(this,shallow,copyData) result(hptr)
        implicit none

        class(HwrfDataSet), target     :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataGroup),   pointer    :: dgPtr
        class(HwrfDataSet), pointer    :: hptr

        class(DataDimension), pointer :: westEastDim
        class(DataDimension), pointer :: southNorthDim
        class(DataDimension), pointer :: bottomTopDim
        class(DataDimension), pointer :: timeDim

        class(DataVariable),  pointer :: pLevelVar
        class(DataVariable),  pointer :: tVar
        class(DataVariable),  pointer :: qvaporVar
        class(DataVariable),  pointer :: uVar
        class(DataVariable),  pointer :: vVar
        class(DataVariable),  pointer :: wVar
        class(DataVariable),  pointer :: cldfraVar
        class(DataVariable),  pointer :: cwmVar
        class(DataVariable),  pointer :: qcloudVar
        class(DataVariable),  pointer :: qiceVar
        class(DataVariable),  pointer :: qrainVar
        class(DataVariable),  pointer :: qsnowVar
        class(DataVariable),  pointer :: qgraupVar
        class(DataVariable),  pointer :: qhailVar
        class(DataVariable),  pointer :: dzVar
        class(DataVariable),  pointer :: u10Var
        class(DataVariable),  pointer :: v10Var
        class(DataVariable),  pointer :: tSurfVar
        class(DataVariable),  pointer :: soilTypeVar
        class(DataVariable),  pointer :: vegTypeVar
        class(DataVariable),  pointer :: luIndexVar
        class(DataVariable),  pointer :: sfcZVar
        class(DataVariable),  pointer :: latVar
        class(DataVariable),  pointer :: lonVar
        class(DataVariable),  pointer :: pSfcVar
        class(DataVariable),  pointer :: t2Var
        class(DataVariable),  pointer :: q2Var

        class(DataGrid),      pointer :: grid

        logical :: isShallow
        logical :: doCopy

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

        allocate(hptr)
        call hptr%hwrfDataSetConstructor(null(),this%getDataArrayReader(),this%time)

        if (.not. isShallow) then
            dgPtr => this
            call hptr%copy(dgPtr,doCopy)

            westEastDim   => hptr%getDimensionByName(WEST_EAST_DIM_NAME)
            southNorthDim => hptr%getDimensionByName(SOUTH_NORTH_DIM_NAME)
            bottomTopDim  => hptr%getDimensionByName(BOTTOM_TOP_DIM_NAME)

            pLevelVar   => hptr%getVariableByName(P_LEVEL_VAR)
            tVar        => hptr%getVariableByName(T_VAR)
            qvaporVar   => hptr%getVariableByName(QVAPOR_VAR)
            uVar        => hptr%getVariableByName(U_VAR)
            vVar        => hptr%getVariableByName(V_VAR)
            wVar        => hptr%getVariableByName(W_VAR)
            cldfraVar   => hptr%getVariableByName(CLDFRA_VAR)
            cwmVar      => hptr%getVariableByName(CWM_VAR)
            qcloudVar   => hptr%getVariableByName(QCLOUD_VAR)
            qiceVar     => hptr%getVariableByName(QICE_VAR)
            qrainVar    => hptr%getVariableByName(QRAIN_VAR)
            qsnowVar    => hptr%getVariableByName(QSNOW_VAR)
            qgraupVar   => hptr%getVariableByName(QGRAUP_VAR)
            qhailVar    => hptr%getVariableByName(QHAIL_VAR)
            dzVar       => hptr%getVariableByName(DZ_VAR)
            u10Var      => hptr%getVariableByName(U10_VAR)
            v10Var      => hptr%getVariableByName(V10_VAR)
            tSurfVar    => hptr%getVariableByName(T_SURF_VAR)
            soilTypeVar => hptr%getVariableByName(SOIL_TYPE_VAR)
            vegTypeVar  => hptr%getVariableByName(VEG_TYPE_VAR)
            luIndexVar  => hptr%getVariableByName(LU_INDEX_VAR)
            sfcZVar     => hptr%getVariableByName(SFC_HGT_VAR)
            latVar      => hptr%getVariableByName(A3D_LAT_VAR)
            lonVar      => hptr%getVariableByName(A3D_LON_VAR)
            pSfcVar     => hptr%getVariableByName(P_SFC_VAR)
            t2Var       => hptr%getVariableByName(T2_VAR)
            q2Var       => hptr%getVariableByName(Q2_VAR)
            grid        => this%getGrid()
            grid        => grid%clone(copyData=doCopy)

            call hptr%loadWrfDataSet(null(),grid,&!westEastDim,southNorthDim,bottomTopDim,
                & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,&
                & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,&
                & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)
        end if

        hptr%dx_deg  = this%dx_deg
        hptr%dy_deg  = this%dy_deg
        hptr%lat1    = this%lat1
        hptr%lon1    = this%lon1
        hptr%offsetx = this%offsetx
        hptr%offsety = this%offsety
        hptr%cen_lat = this%cen_lat
        hptr%cen_lon = this%cen_lon
        hptr%i_parent_start = this%i_parent_start
        hptr%j_parent_start = this%j_parent_start
    end function

!    subroutine loadHwrfField(this,fieldName,data3D,nx,ny,nz_var,t)
!        implicit none
!        class(HwrfDataSet) :: this
!        character(len=256) :: fieldName
!        real(8), dimension(:,:,:) :: data3D
!        integer, intent(in) :: nx,ny,nz_var,t
!        real(8), dimension(:,:,:), allocatable :: tmp1, tmp2
!
!        select case (fieldName)
!            case (P_LEVEL_NAME)
!                data3D(:,:,1:nz_var) = this%p/100.d0 ! convert from Pa to hPa
!            case (T_NAME)
!                data3D(:,:,1:nz_var) = this%t
!            case (CLDFRA_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'CLDFRA',data3D,nx,ny,nz_var,t)
!            case (QVAPOR_NAME)
!                data3D(:,:,1:nz_var) = 1000.d0*this%q ! convert from kg/kg to g/kg
!            case (CWM_NAME)
!                data3D(:,:,1:nz_var) = 1000.d0*this%cwm ! convert from kg/kg to g/kg
!            case (QCLOUD_NAME)
!                data3D(:,:,1:nz_var) = 1000.d0*this%cwm*(1 - this%f_rain)*(1 - this%f_ice) ! convert from kg/kg to g/kg
!            case (QICE_NAME)
!                data3D(:,:,1:nz_var) = 0.d0
!                where (f_rimef .le. 1.0001d0) &
!                    data3D(:,:,1:nz_var) = 1000.d0*this%cwm*this%f_ice ! convert from kg/kg to g/kg
!            case (QRAIN_NAME)
!                data3D(:,:,1:nz_var) = 1000.d0*this%cwm*this%f_rain*(1 - this%f_ice) ! convert from kg/kg to g/kg
!            case (QSNOW_NAME)
!                data3D(:,:,1:nz_var) = 0.d0
!                where(f_rimef .gt. 1.0001d0 .and. f_rimef .le. 5.d0) &
!                    data3D(:,:,1:nz_var) = 1000.d0*this%cwm*this%f_ice ! convert from kg/kg to g/kg
!            case (QGRAUP_NAME)
!                data3D(:,:,1:nz_var) = 0.
!                where(f_rimef .gt. 5.d0 .and. f_rimef .le. 10.d0) &
!                    data3D(:,:,1:nz_var) = 1000.*this%cwm*this%f_ice ! convert from kg/kg to g/kg
!            case (QHAIL_NAME)
!                data3D(:,:,1:nz_var) = 0.d0
!                where(f_rimef .gt. 10.d0) &
!                    data3D(:,:,1:nz_var) = 1000.d0*this%cwm*this%f_ice ! convert from kg/kg to g/kg
!            case (UV_NAME)
!                allocate(tmp1(nx,ny,nz_var))
!                allocate(tmp2(nx,ny,nz_var))
!                call ncReadVariable4DAtTime(this%fileName,'U',tmp1(:,:,1:nz_var),nx,ny,nz_var,t)
!                call ncReadVariable4DAtTime(this%fileName,'V',tmp2(:,:,1:nz_var),nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = sqrt(tmp1**2 + tmp2**2)
!                deallocate(tmp1)
!                deallocate(tmp2)
!            case (W_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'W',data3D,nx,ny,nz_var,t)
!            case (DZ_NAME)
!                call this%loadLevelThickness(data3D,nz_var)
!            case (U10_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'U10',data3D(:,:,1),nx,ny,t)
!            case (V10_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'V10',data3D(:,:,1),nx,ny,t)
!            case (UV10_NAME)
!                allocate(tmp1(nx,ny,1))
!                allocate(tmp2(nx,ny,1))
!
!                call ncReadVariable3DAtTime(this%fileName,'U10',tmp1(:,:,1),nx,ny,t)
!                call ncReadVariable3DAtTime(this%fileName,'V10',tmp2(:,:,1),nx,ny,t)
!
!                data3D(:,:,1) = sqrt(tmp1(:,:,1)**2+tmp2(:,:,1)**2)
!                deallocate(tmp1,tmp2)
!            case (T_LAND_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'TSHLTR',data3D(:,:,1),nx,ny,t)
!            case (T_WATER_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'SST',data3D(:,:,1),nx,ny,t)
!            case (SOIL_TYPE_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'ISLTYP',data3D(:,:,1),nx,ny,t)
!            case (VEG_TYPE_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'IVGTYP',data3D(:,:,1),nx,ny,t)
!            case (LU_INDEX_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'LU_INDEX',data3D(:,:,1),nx,ny,t)
!            case (SURFACE_HEIGHT_NAME)
!                data3D(:,:,1) = this%sfc_z
!            case (LAT_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'HLAT',data3D(:,:,1),nx,ny,t)
!            case (LON_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'HLON',data3D(:,:,1),nx,ny,t)
!            case (P_SFC_NAME)
!                data3D(:,:,1) = this%p(:,:,1)/100.d0 ! convert from Pa to hPa
!            case (T2_NAME)
!                ! since the output from MYJ is unreliable, just use the first model layer
!                data3D(:,:,1) = this%t(:,:,1)
!            case (Q2_NAME)
!                ! since the output from MYJ is unreliable, just use the first model layer
!                data3D(:,:,1) = 1000.d0*this%q(:,:,1)
!            case default
!                write(msgstr,*) 'Unknown field name ',trim(fieldName)
!                call error(msgstr)
!        end select
!    end subroutine

    ! get the nlayers thickness of each level in m
!    subroutine loadLevelThickness(this,dz,nz)
!        implicit none
!        class(HwrfDataSet) :: this
!        real(8), dimension(:,:,:) :: dz
!        integer, intent(in) :: nz
!
!        real(8), dimension(size(dz,1),size(dz,2)) :: height
!
!        integer :: z
!
!        ! Virtual T (K)
!        ! tv = t*(1.0 + X_C*q)
!        ! thickness of layer in meters
!        ! RD  = 286.9968933  ! J/(kg*degK)
!        ! G0  = 9.80616      ! GRAVITY m/s**2
!
!        ! K * RD / G0
!        ! K * J s^2 / (kg * K * m)
!        ! kg m^2 s^2 / (s^2 kg * m)
!        ! m
!        dz(:,:,1:nz) = log(this%p(:,:,1:nz)/this%p(:,:,2:nz+1))*RD/G0
!        dz(:,:,1:nz) = dz(:,:,1:nz)*this%t*(1.0 + X_C*this%q)
!
!        ! correct for gravity differences as a function of height
!        do z =1,nz
!            height = this%sfc_z(:,:) + SUM(dz(:,:,1:z),3)
!            dz(:,:,z) = dz(:,:,z) * ((Re+height)/Re)**2 ! 1st order correction for gravity fn of height
!        end do
!    end subroutine

    subroutine convertLatLonToIJ(this,lat,lon,x,y)
        implicit none
        class(HwrfDataSet) :: this
        real(8), intent(in) :: lat, lon
        real(8), intent(out) :: x, y

        real(8) :: wbd, sbd

        class(DataDimension), pointer :: xdim, ydim

        integer :: nx, ny

        xdim => this%getDimensionByName(WEST_EAST_DIM_NAME)
        ydim => this%getDimensionByName(SOUTH_NORTH_DIM_NAME)

        nx = xdim%getGlobalSize()
        ny = ydim%getGlobalSize()

        wbd   = -((nx-2)*this%dx_deg)+this%offsetx
        sbd   = -(((ny-1)/2)*this%dy_deg)+this%offsety

        call llij_rotlatlon(lat,lon,x,y,sbd,wbd,this%cen_lat,this%cen_lon,&
            this%dy_deg,this%dx_deg,nx,ny)
    end subroutine

    SUBROUTINE llij_rotlatlon(lat, lon, i_real, j_real, sbd, wbd, central_lat, central_lon, dlambdad, dphid, nx, ny)

        implicit none

        real(8), intent(in)  :: lat, lon        ! lat/lon: desired lat/lon in degrees
        real(8), intent(out) :: i_real, j_real  ! i/j:     grid points corresponding to lat/lon
        real(8), intent(in)  :: sbd             ! sbd:     southern boundary (lat) in degrees
        real(8), intent(in)  :: wbd             ! wbd:     western boundary (lon) in degrees
        real(8), intent(in)  :: central_lat     ! cen_lat: central (true) latitude in degrees
        real(8), intent(in)  :: central_lon     ! cen_lon: southern (true) longitude in degrees
        real(8), intent(in)  :: dlambdad        ! dy:      delta lambda in degrees
        real(8), intent(in)  :: dphid           ! dx:      delta phi in degrees
        integer, intent(in)  :: nx              ! nx:      number of x (longitude) points
        integer, intent(in)  :: ny              ! ny:      number of y (latitude) points

        ! local variables
        REAL(8)                             :: wb,sb,dlm,dph,tph0,stph0,ctph0
        REAL(8)                             :: tdlm,tdph,tlmh,tlmh0,tphh,tphv,d2r
        REAL(8)                             :: stph,ctph,ctphctlm,pi_2
        REAL(8)                             :: sphh,clmh
        REAL(8)                             :: glat,glon

        real(8) :: facth

        pi_2 = acos(0.)
        d2r  = pi_2/90.
        glat = lat * d2r
        glon = lon * d2r
        wb   = wbd * d2r                  ! WB:   western boundary in radians
        sb   = sbd * d2r                  ! SB:   southern boundary in radians
        dlm  = dlambdad * d2r             ! DLM:  dlamda in radians
        dph  = dphid * d2r                ! DPH:  dphi   in radians
        tdlm = dlm + dlm                  ! TDLM: 2.0*dlamda
        tdph = dph + dph                  ! TDPH: 2.0*DPH

        tph0  = central_lat*d2r           ! TPH0: central lat in radians
        stph0 = SIN(tph0)
        ctph0 = COS(tph0)

        clmh = cos(glon - central_lon*d2r)

        ctphctlm = (clmh + tan(glat)*tan(tph0))*cos(glat)*ctph0

        sphh = sin(glat)
        stph = (sphh - stph0 * ctphctlm)/ctph0
        tphh = asin(stph)
        j_real = (tphh - sb)/dph + 1
        ctph = cos(tphh)
        ctphctlm = (clmh + tan(glat)*tan(tph0))*cos(glat)*ctph0/ctph
        tlmh = acos((clmh + tan(glat)*tan(tph0))*cos(glat)*ctph0/ctph)

        tlmh0 = wb - tdlm + mod(j_real+1.d0,2.0d0) * dlm   ! TLMH (rotated lats at H points)

!       problem: which hemisphere is tlmh in? domain of acos(t) is [-1,1], range is (0,\pi)
!       so t = cos(x), for x \in (-\pi,0), will not be the same when acos, i.e. x \neq acos(cos(x)).
!       Need to correct for this.
!       Problem: x is unknown, as x=acos(t), so t doesn't tell you the hemisphere. Need info from somewhere else.
!
!       Solution: the hemisphere of glon - central_lon*d2r is the same as tlmh

        if (glon - central_lon*d2r < 0.d0) then
            tlmh = -tlmh
        end if

        i_real = (tlmh - tlmh0)/tdlm
   END SUBROUTINE llij_rotlatlon
end module
