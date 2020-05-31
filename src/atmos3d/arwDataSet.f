module ArwDataSet_mod

    use parallelInfo_mod
    use parallelConstants_mod

    use iso_fortran_env

    use dataSet_mod
    use wrfDataSet_mod
    use atmos3dDataSet_mod

    use dataGrid_mod
    use regularTriangularLatLonGrid_mod

    use dataGroup_mod
    use dataAttribute_mod
    use dataDimension_mod
    use dataVariable_mod
    use dataArrayReader_mod

    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    character(*), public, parameter :: WEST_EAST_STAG_DIM_NAME   = 'west_east_stag'
    character(*), public, parameter :: SOUTH_NORTH_STAG_DIM_NAME = 'south_north_stag'
    character(*), public, parameter :: BOTTOM_TOP_STAG_DIM_NAME  = 'bottom_top_stag'

    type, extends(WrfDataSet), public :: ArwDataSet
        private

            integer      :: time
            integer      :: map_proj
            real(real64) :: truelat1,truelat2,stdlon
            real(real64) :: pole_lat,pole_lon
            real(real64) :: dx,dy,latinc,loninc
            real(real64) :: cen_lat,cen_lon
            integer      :: num_land_cat

        contains
            procedure :: loadArwDataSet
            procedure :: getLandCatCount   => getLandCatCount_arw
            procedure :: clone
            procedure :: cloneArw
            procedure :: convertLatLonToIJ => convertLatLonToIJ_arw

            generic   :: arwDataSetConstructor => &
                arwDataSetConstructor_file, &
                arwDataSetConstructor_class

            procedure, private :: arwDataSetConstructor_file
            procedure, private :: arwDataSetConstructor_class
            final :: arwDataSetDestructor ! clean up all allocated variables

    end type

!    interface ArwDataSet
!        procedure arwDataSetConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine arwDataSetConstructor_file(this,pinfo,inputFile,time)

        implicit none

        class(ArwDataSet)  :: this

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
            call this%loadArwDataSet(pinfo)
        end if
    end subroutine

    subroutine arwDataSetConstructor_class(this,pinfo,reader,time)

        implicit none

        class(ArwDataSet)  :: this

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
            call this%loadArwDataSet(pinfo)
        end if
    end subroutine

    subroutine loadArwDataSet(this,pinfo)

        implicit none

        class(ArwDataSet)  :: this

        class(ParallelInfo),    pointer    :: pinfo

        integer :: nxl,nyl,nzl,nt,z

        integer :: i,j,k,cursor,ndims,nz_val,si,ei

        class(DataDimension), pointer :: westEastDim       => null()
        class(DataDimension), pointer :: westEastStagDim   => null()
        class(DataDimension), pointer :: southNorthDim     => null()
        class(DataDimension), pointer :: southNorthStagDim => null()
        class(DataDimension), pointer :: bottomTopDim      => null()
        class(DataDimension), pointer :: bottomTopStagDim  => null()
        class(DataDimension), pointer :: timeDim           => null()

        class(DataDimension), pointer :: westEastGlobalDim   => null()
        class(DataDimension), pointer :: southNorthGlobalDim => null()

        class(DataVariable),  pointer :: pLevelVar   => null()
        class(DataVariable),  pointer :: pVar        => null()
        class(DataVariable),  pointer :: pbVar       => null()
        class(DataVariable),  pointer :: pTopVar     => null()
        class(DataVariable),  pointer :: psfcVar     => null()
        class(DataVariable),  pointer :: phVar       => null()
        class(DataVariable),  pointer :: phbVar      => null()
        class(DataVariable),  pointer :: tVar        => null()
        class(DataVariable),  pointer :: qvaporVar   => null()
        class(DataVariable),  pointer :: uVar        => null()
        class(DataVariable),  pointer :: vVar        => null()
        class(DataVariable),  pointer :: wVar        => null()
        class(DataVariable),  pointer :: uStagVar    => null()
        class(DataVariable),  pointer :: vStagVar    => null()
        class(DataVariable),  pointer :: wStagVar    => null()
        class(DataVariable),  pointer :: cldfraVar   => null()
        class(DataVariable),  pointer :: cwmVar      => null()
        class(DataVariable),  pointer :: qcloudVar   => null()
        class(DataVariable),  pointer :: qiceVar     => null()
        class(DataVariable),  pointer :: qrainVar    => null()
        class(DataVariable),  pointer :: qsnowVar    => null()
        class(DataVariable),  pointer :: qgraupVar   => null()
        class(DataVariable),  pointer :: qhailVar    => null()
        class(DataVariable),  pointer :: dzVar       => null()
        class(DataVariable),  pointer :: u10Var      => null()
        class(DataVariable),  pointer :: v10Var      => null()
        class(DataVariable),  pointer :: tSurfVar    => null()
        class(DataVariable),  pointer :: soilTypeVar => null()
        class(DataVariable),  pointer :: vegTypeVar  => null()
        class(DataVariable),  pointer :: luIndexVar  => null()
        class(DataVariable),  pointer :: sfcZVar     => null()
        class(DataVariable),  pointer :: latVar      => null()
        class(DataVariable),  pointer :: lonVar      => null()
        class(DataVariable),  pointer :: t2Var       => null()
        class(DataVariable),  pointer :: q2Var       => null()
        class(DataVariable),  pointer :: landMaskVar => null()
        class(DataVariable),  pointer :: tLandVar    => null()
        class(DataVariable),  pointer :: tWaterVar   => null()

        class(DataGrid),                   pointer :: grid        => null()
        class(RegularTriangularLatLonGrid), pointer :: triGrid    => null()

        class(DataVariable),  pointer :: latVar_global => null()
        class(DataVariable),  pointer :: lonVar_global => null()

        class(DataAttribute), pointer :: attr => null()

        character(:), allocatable :: loadFrom

        ! The 3d arrays (plus time, which will be squeezed out)
        real(real64), pointer :: cwm    (:,:,:,:) => null()
        real(real64), pointer :: f_rain (:,:,:,:) => null()
        real(real64), pointer :: f_ice  (:,:,:,:) => null()
        real(real64), pointer :: f_rimef(:,:,:,:) => null()
        real(real64), pointer :: q      (:,:,:,:) => null()
        real(real64), pointer :: t      (:,:,:,:) => null()
        real(real64), pointer :: pLevel (:,:,:,:) => null()
        real(real64), pointer :: pLevel2(:,:,:,:) => null()
        real(real64), pointer :: p      (:,:,:,:) => null()
        real(real64), pointer :: pb     (:,:,:,:) => null()
        real(real64), pointer :: ph     (:,:,:,:) => null()
        real(real64), pointer :: phb    (:,:,:,:) => null()
        real(real64), pointer :: cldfra (:,:,:,:) => null()
        real(real64), pointer :: qcloud (:,:,:,:) => null()
        real(real64), pointer :: qice   (:,:,:,:) => null()
        real(real64), pointer :: qrain  (:,:,:,:) => null()
        real(real64), pointer :: qsnow  (:,:,:,:) => null()
        real(real64), pointer :: qgraup (:,:,:,:) => null()
        real(real64), pointer :: qhail  (:,:,:,:) => null()
        real(real64), pointer :: u_stag (:,:,:,:) => null()
        real(real64), pointer :: v_stag (:,:,:,:) => null()
        real(real64), pointer :: w_stag (:,:,:,:) => null()
        real(real64), pointer :: u      (:,:,:,:) => null()
        real(real64), pointer :: v      (:,:,:,:) => null()
        real(real64), pointer :: w      (:,:,:,:) => null()
        real(real64), pointer :: dz     (:,:,:,:) => null()

        ! The 2d arrays (plus time, which will be squeezed out)
        real(real64), pointer :: sfc_z   (:,:,:) => null()
        real(real64), pointer :: u10     (:,:,:) => null()
        real(real64), pointer :: v10     (:,:,:) => null()
        real(real64), pointer :: tLand   (:,:,:) => null()
        real(real64), pointer :: tWater  (:,:,:) => null()
        real(real64), pointer :: tSurf   (:,:,:) => null()
        real(real64), pointer :: landmask(:,:,:) => null()
        real(real64), pointer :: soilType(:,:,:) => null()
        real(real64), pointer :: vegType (:,:,:) => null()
        real(real64), pointer :: luIndex (:,:,:) => null()
        real(real64), pointer :: lat     (:,:,:) => null()
        real(real64), pointer :: lon     (:,:,:) => null()
        real(real64), pointer :: psfc    (:,:,:) => null()
        real(real64), pointer :: t2      (:,:,:) => null()
        real(real64), pointer :: q2      (:,:,:) => null()

        real(real64), pointer :: lat_global(:,:,:) => null()
        real(real64), pointer :: lon_global(:,:,:) => null()

        ! 1d array (including time)
        real(real64), dimension(:), pointer :: p_top  => null()

        integer :: dimMapping(3)

        !allocate(this)

        call debug('Now loading the ARW attributes')

        attr => this%loadAttribute(pinfo, 'MAP_PROJ',     this%map_proj)
        attr => this%loadAttribute(pinfo, 'TRUELAT1',     this%truelat1)
        attr => this%loadAttribute(pinfo, 'TRUELAT2',     this%truelat2)
        attr => this%loadAttribute(pinfo, 'STAND_LON',    this%stdlon)
        attr => this%loadAttribute(pinfo, 'POLE_LAT',     this%pole_lat)
        attr => this%loadAttribute(pinfo, 'POLE_LON',     this%pole_lon)
        attr => this%loadAttribute(pinfo, 'CEN_LAT',      this%cen_lat)
        attr => this%loadAttribute(pinfo, 'CEN_LON',      this%cen_lon)
        attr => this%loadAttribute(pinfo, 'DX',           this%dx)
        attr => this%loadAttribute(pinfo, 'DY',           this%dy)
        ! only lat/lon grids will have this attribute
        attr => this%loadAttribute(pinfo, 'LATINC',       this%latinc, -999d9)
        attr => this%loadAttribute(pinfo, 'LONINC',       this%loninc, -999d9)
        attr => this%loadAttribute(pinfo, 'NUM_LAND_CAT', this%num_land_cat)

        call debug('Now loading the ARW dimensions')

        loadFrom = 'QVAPOR'

        westEastDim   => this%loadDimensionFromVariable(pinfo, WEST_EAST_DIM_NAME,   1, loadFrom)
        southNorthDim => this%loadDimensionFromVariable(pinfo, SOUTH_NORTH_DIM_NAME, 2, loadFrom)
        bottomTopDim  => this%loadDimensionFromVariable(pinfo, BOTTOM_TOP_DIM_NAME,  3, loadFrom)
        timeDim       => this%loadDimensionFromVariable(pinfo, TIME_DIM_NAME,        4, loadFrom)

!        nx = westEastDim  %getGlobalSize()
!        ny = southNorthDim%getGlobalSize()
!        nz = bottomTopDim %getGlobalSize()
        nt = timeDim      %getGlobalSize()

        if (this%time > nt) then
            call error('The requested time is larger than the number of time steps in the file: ' // &
                int2str(this%time) // ' vs. ' // int2str(nt))
        else
            call timeDim%setGlobalRange(this%time,1)
        end if

        westEastStagDim   => this%loadDimensionFromVariable(pinfo,WEST_EAST_STAG_DIM_NAME,   &
            & 1, 'U', compareToDim=WEST_EAST_DIM_NAME,stagger=1)

        southNorthStagDim => this%loadDimensionFromVariable(pinfo,SOUTH_NORTH_STAG_DIM_NAME, &
            & 2, 'V', compareToDim=SOUTH_NORTH_DIM_NAME,stagger=1)

        bottomTopStagDim  => this%loadDimensionFromVariable(pinfo,BOTTOM_TOP_STAG_DIM_NAME,  &
            & 3, 'PHB', compareToDim=BOTTOM_TOP_DIM_NAME,stagger=1)

        call debug('Now loading the ARW variables')

        qvaporVar => this%loadVariable(pinfo, QVAPOR_VAR, q, &
            & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QVAPOR', squeeze=.true.)
        ! convert from kg/kg to g/kg
        q = q*1000.d0

        tVar => this%loadVariable(pinfo, T_VAR, t, &
            & westEastDim, southNorthDim, bottomTopDim, timeDim,'T', squeeze=.true.)

        pVar => this%loadVariable(pinfo, 'P', p, &
            & westEastDim, southNorthDim, bottomTopDim, timeDim, squeeze=.true.)

        pbVar => this%loadVariable(pinfo, 'PB', pb, &
            & westEastDim, southNorthDim, bottomTopDim, timeDim, squeeze=.true.)

        ! convert p and t to Pa and K, respectively
        p = p + pb
        t = t + 300.
        t = t*(p/100000.)**(2.0/7.0)

        psfcVar => this%loadVariable(pinfo, P_SFC_VAR, psfc, &
            & westEastDim, southNorthDim, timeDim,'PSFC', squeeze=.true.)

        ! WRF doesn't have a p level variable, so we'll need to fudge one
        pLevelVar => this%addVariable(pinfo, P_LEVEL_VAR, pLevel, &
            & westEastDim, southNorthDim, bottomTopStagDim, timeDim)
        call pLevelVar%squeeze()

        ! we'll use psfc to begin
        pLevel(:,:,1,1) = psfc(:,:,1)

        ! z local count. Note that because p is staggered, it is length nzl+1 in that dimension
        nzl =size(t,3)

        if (nzl /= bottomTopDim%getGlobalCount()) then
            call error('Cannot reconstruct pressure levels on a split z-dimension')
        end if

        do z=2,nzl
            ! iteratively add the delta pressure to the previous level
            pLevel(:,:,z,1) = exp(2*log(p(:,:,z-1,1)) - log(pLevel(:,:,z-1,1)))
        end do

        ptopVar => this%loadVariable(pinfo, 'P_TOP', p_top, timeDim, squeeze=.true.)

        ! end with the model-top pressure
        pLevel (:,:,nzl+1,1) = p_top(1)

        ! now do a second logarithmic interpolation in reverse so we can take the average
        allocate(pLevel2,mold=plevel)

        pLevel2(:,:,nzl+1,1) = p_top(1)

        do z=nzl,2,-1
            pLevel2(:,:,z,1) = exp(2*log(p(:,:,z,1)) - log(pLevel(:,:,z+1,1)))
        end do

        ! now smooth out the pLevel. TODO: was this actually a good idea?
        pLevel(:,:,2:nzl,1) = 0.5*(pLevel(:,:,2:nzl,  1) + pLevel2(:,:,2:nzl,1))
        pLevel(:,:,2:nzl,1) =     (pLevel(:,:,1:nzl-1,1) + pLevel (:,:,2:nzl,1) + pLevel(:,:,3:nzl+1,1))/3.d0

        deallocate(pLevel2)

        ! now convert pressures from Pa to hPa
        psfc = psfc/100.
        p_top = p_top/100.
        pLevel = pLevel/100.

        phVar => this%loadVariable(pinfo, 'PH', ph, &
           & westEastDim, southNorthDim, bottomTopStagDim, timeDim, squeeze=.true.)
        phbVar => this%loadVariable(pinfo, 'PHB', phb, &
           & westEastDim, southNorthDim, bottomTopStagDim, timeDim, squeeze=.true.)

        ph = ph+phb

        dzVar => this%addVariable(pinfo, DZ_VAR, dz, &
            & westEastDim, southNorthDim, bottomTopDim, timeDim)
        call dzVar%squeeze()

        do z=1,nzl
            dz(:,:,z,1) = (ph(:,:,z+1,1)-ph(:,:,z,1))/9.81 ! convert from m^2/s^2 to m
        end do

        cldfraVar => this%loadVariable(pinfo, CLDFRA_VAR, cldfra, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'CLDFRA', squeeze=.true.)

        qcloudVar => this%loadVariable(pinfo, QCLOUD_VAR, qcloud, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QCLOUD', squeeze=.true.)
        ! convert from kg/kg to g/kg
        qcloud = qcloud*1000.

        qiceVar => this%loadVariable(pinfo, QICE_VAR, qice, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QICE', squeeze=.true.)
        ! convert from kg/kg to g/kg
        qice = qice*1000.

        qrainVar => this%loadVariable(pinfo, QRAIN_VAR, qrain, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QRAIN', squeeze=.true.)
        ! convert from kg/kg to g/kg
        qrain = qrain*1000.

        qsnowVar => this%loadVariable(pinfo, QSNOW_VAR, qsnow, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QSNOW', squeeze=.true.)
        ! convert from kg/kg to g/kg
        qsnow = qsnow*1000.

        ! TODO: add the option to find the integral of the exponential distribution between
        ! 0 and 5 mm for graupel, and 5 to infinity for hail
        qgraupVar => this%loadVariable(pinfo, QGRAUP_VAR, qgraup, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QGRAUP', squeeze=.true.)
        ! convert from kg/kg to g/kg
        qgraup = qgraup*1000.

        qhailVar => this%loadVariable(pinfo, QHAIL_VAR, qhail, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim, 'QHAIL', &
           & required=.false., squeeze=.true.)
        ! convert from kg/kg to g/kg
        qhail = qhail*1000.

        cwmVar => this%addVariable(pinfo, CWM_VAR, cwm, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim)
        call cwmVar%squeeze()

        cwm = qcloud + qice + qrain + qsnow + qgraup + qhail

        uStagVar => this%loadVariable(pinfo, 'U_STAG', u_stag, &
           & westEastStagDim, southNorthDim, bottomTopDim, timeDim, 'U', squeeze=.true.)

        uVar => this%addVariable(pinfo, U_VAR, u, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim)
        call uVar%squeeze()

        nxl = size(u,1)

        u(:,:,:,1:1) = 0.5*(u_stag(1:nxl,:,:,1:1) + u_stag(2:nxl+1,:,:,1:1))

        vStagVar => this%loadVariable(pinfo, 'V_STAG', v_stag, &
           & westEastDim, southNorthStagDim, bottomTopDim, timeDim, 'V', squeeze=.true.)

        vVar => this%addVariable(pinfo, V_VAR, v, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim)
        call vVar%squeeze()

        nyl = size(v,2)

        v(:,:,:,1:1) = 0.5*(v_stag(:,1:nyl,:,1:1) + v_stag(:,2:nyl+1,:,1:1))

        wStagVar => this%loadVariable(pinfo, 'W_STAG', w_stag, &
           & westEastDim, southNorthDim, bottomTopStagDim, timeDim, 'W', squeeze=.true.)

        wVar => this%addVariable(pinfo, W_VAR, w, &
           & westEastDim, southNorthDim, bottomTopDim, timeDim)
        call wVar%squeeze()

        w(:,:,:,1:1) = 0.5*(w_stag(:,:,1:nzl,1:1) + w_stag(:,:,2:nzl+1,1:1))

        u10Var => this%loadVariable(pinfo, U10_VAR, u10, &
           & westEastDim, southNorthDim, timeDim, 'U10', squeeze=.true.)

        v10Var => this%loadVariable(pinfo, V10_VAR, v10, &
           & westEastDim, southNorthDim, timeDim, 'V10', squeeze=.true.)

        tSurfVar => this%loadVariable(pinfo, T_SURF_VAR, tSurf, &
           & westEastDim, southNorthDim, timeDim, 'TSK', squeeze=.true.)

        soilTypeVar => this%loadVariable(pinfo, SOIL_TYPE_VAR, soilType, &
           & westEastDim, southNorthDim, timeDim, 'ISLTYP', squeeze=.true.)

        vegTypeVar => this%loadVariable(pinfo, VEG_TYPE_VAR, vegType, &
           & westEastDim, southNorthDim, timeDim, 'IVGTYP', squeeze=.true.)

        luIndexVar => this%loadVariable(pinfo, LU_INDEX_VAR, luIndex, &
           & westEastDim, southNorthDim, timeDim, 'LU_INDEX', squeeze=.true.)

        sfcZVar => this%loadVariable(pinfo, SFC_HGT_VAR, sfc_z, &
            & westEastDim, southNorthDim, timeDim,'HGT', squeeze=.true.)

        latVar => this%loadVariable(pinfo, A3D_LAT_VAR, lat, &
            & westEastDim, southNorthDim, timeDim, 'XLAT', squeeze=.true.)

        lonVar => this%loadVariable(pinfo, A3D_LON_VAR, lon, &
            & westEastDim, southNorthDim, timeDim, 'XLONG', squeeze=.true.)

        t2Var => this%loadVariable(pinfo, T2_VAR, t2, &
            & westEastDim, southNorthDim, timeDim, 'T2', squeeze=.true.)

        q2Var => this%loadVariable(pinfo, Q2_VAR, q2, &
            & westEastDim, southNorthDim, timeDim, 'Q2', squeeze=.true.)

        call debug('Now transposing the WRF variables to z-x-y order')

        dimMapping = (/3,1,2/)

        call pLevelVar%transpose(dimMapping)
        call      tVar%transpose(dimMapping)
        call qvaporVar%transpose(dimMapping)
        call      uVar%transpose(dimMapping)
        call      vVar%transpose(dimMapping)
        call      wVar%transpose(dimMapping)
        call cldfraVar%transpose(dimMapping)
        call    cwmVar%transpose(dimMapping)
        call qcloudVar%transpose(dimMapping)
        call   qiceVar%transpose(dimMapping)
        call  qrainVar%transpose(dimMapping)
        call  qsnowVar%transpose(dimMapping)
        call qgraupVar%transpose(dimMapping)
        call  qhailVar%transpose(dimMapping)

        call debug('Now creating the input grid')

        allocate(triGrid)
        call triGrid%regularTriangularLatLonGridConstructor(latVar,lonVar)
        grid => triGrid

        call debug('Now finalizing the WRF data set')

        call this%loadWrfDataSet(pinfo,grid,&!westEastDim,southNorthDim,bottomTopDim,         &
            & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,   &
            & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,&
            & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)

        call debug('Finished loading the WRF data set')

        call barrier(pinfo%getCommunicator())
    end subroutine

    subroutine arwDataSetDestructor(this)
        implicit none

        type(ArwDataSet)  :: this

        call this%wrfDataSetDestructor()
    end subroutine

    function clone(this,shallow,copyData) result(dgPtr)
        implicit none

        class(ArwDataSet),  target     :: this

        logical, optional,  intent(in) :: shallow
        logical, optional,  intent(in) :: copyData

        class(ArwDataSet),  pointer    :: aptr
        class(DataGroup),   pointer    :: dgPtr

        aptr => this%cloneArw(copyData)
        dgptr => aptr
    end function

    function cloneArw(this,shallow,copyData) result(aPtr)
        implicit none

        class(ArwDataSet),  target     :: this

        logical, optional,  intent(in) :: shallow
        logical, optional,  intent(in) :: copyData

        class(DataGroup),   pointer    :: dgPtr
        class(ArwDataSet),  pointer    :: aptr

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

        allocate(aptr)
        call aptr%arwDataSetConstructor(null(),this%getDataArrayReader(),this%time)

        if (.not. isShallow) then
            dgPtr => this
            call aptr%copy(dgPtr,doCopy)

            westEastDim   => aptr%getDimensionByName(WEST_EAST_DIM_NAME)
            southNorthDim => aptr%getDimensionByName(SOUTH_NORTH_DIM_NAME)
            bottomTopDim  => aptr%getDimensionByName(BOTTOM_TOP_DIM_NAME)

            pLevelVar   => aptr%getVariableByName(P_LEVEL_VAR)
            tVar        => aptr%getVariableByName(T_VAR)
            qvaporVar   => aptr%getVariableByName(QVAPOR_VAR)
            uVar        => aptr%getVariableByName(U_VAR)
            vVar        => aptr%getVariableByName(V_VAR)
            wVar        => aptr%getVariableByName(W_VAR)
            cldfraVar   => aptr%getVariableByName(CLDFRA_VAR)
            cwmVar      => aptr%getVariableByName(CWM_VAR)
            qcloudVar   => aptr%getVariableByName(QCLOUD_VAR)
            qiceVar     => aptr%getVariableByName(QICE_VAR)
            qrainVar    => aptr%getVariableByName(QRAIN_VAR)
            qsnowVar    => aptr%getVariableByName(QSNOW_VAR)
            qgraupVar   => aptr%getVariableByName(QGRAUP_VAR)
            qhailVar    => aptr%getVariableByName(QHAIL_VAR)
            dzVar       => aptr%getVariableByName(DZ_VAR)
            u10Var      => aptr%getVariableByName(U10_VAR)
            v10Var      => aptr%getVariableByName(V10_VAR)
            tSurfVar    => aptr%getVariableByName(T_SURF_VAR)
            soilTypeVar => aptr%getVariableByName(SOIL_TYPE_VAR)
            vegTypeVar  => aptr%getVariableByName(VEG_TYPE_VAR)
            luIndexVar  => aptr%getVariableByName(LU_INDEX_VAR)
            sfcZVar     => aptr%getVariableByName(SFC_HGT_VAR)
            latVar      => aptr%getVariableByName(A3D_LAT_VAR)
            lonVar      => aptr%getVariableByName(A3D_LON_VAR)
            pSfcVar     => aptr%getVariableByName(P_SFC_VAR)
            t2Var       => aptr%getVariableByName(T2_VAR)
            q2Var       => aptr%getVariableByName(Q2_VAR)
            grid        => this%getGrid()
            grid        => grid%clone(copyData=doCopy)

            call aptr%loadWrfDataSet(null(),grid,&! westEastDim,southNorthDim,bottomTopDim,     &
                & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,   &
                & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,&
                & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)
        end if

        aptr%map_proj  = this%map_proj
        aptr%truelat1  = this%truelat1
        aptr%truelat2  = this%truelat2
        aptr%pole_lat  = this%pole_lat
        aptr%dx        = this%dx
        aptr%dy        = this%dy
        aptr%latinc    = this%latinc
        aptr%loninc    = this%loninc
        aptr%cen_lat   = this%cen_lat
        aptr%cen_lon   = this%cen_lon
        aptr%num_land_cat = this%num_land_cat
    end function

!    ! get the height of the surface from sea-level in meters
!    subroutine loadArwField(this,fieldName,data3D,nx,ny,nz_var,t)
!        implicit none
!        class(ArwDataSet) :: this
!        character(len=256) :: fieldName
!        real(real64), dimension(:,:,:) :: data3D
!        integer, intent(in) :: nx,ny,nz_var,t
!        real(real64), dimension(:,:,:), allocatable :: tmp1, tmp2
!
!        select case (fieldName)
!            case (P_LEVEL_NAME)
!                data3D(:,:,1:nz_var) = this%pLevel/100.d0 ! convert from Pa to hPa
!            case (T_NAME)
!                data3D(:,:,1:nz_var) = this%t
!            case (CLDFRA_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'CLDFRA',data3D,nx,ny,nz_var,t)
!            case (QVAPOR_NAME)
!                data3D(:,:,1:nz_var) = 1000.d0*this%q ! convert from kg/kg to g/kg
!            case (CWM_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'QCLOUD',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                call ncReadVariable4DAtTime(this%fileName,'QICE',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = data3D(:,:,1:nz_var) + 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                call ncReadVariable4DAtTime(this%fileName,'QRAIN',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = data3D(:,:,1:nz_var) + 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                call ncReadVariable4DAtTime(this%fileName,'QSNOW',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = data3D(:,:,1:nz_var) + 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                call ncReadVariable4DAtTime(this%fileName,'QGRAUP',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = data3D(:,:,1:nz_var) + 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                call ncReadVariable4DAtTime(this%fileName,'QHAIL',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = data3D(:,:,1:nz_var) + 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!            case (QCLOUD_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'QCLOUD',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                where (data3D .lt. 0.0d0) data3D = 0.0d0
!            case (QICE_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'QICE',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                where (data3D .lt. 0.0d0) data3D = 0.0d0
!            case (QRAIN_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'QRAIN',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                where (data3D .lt. 0.0d0) data3D = 0.0d0
!            case (QSNOW_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'QSNOW',data3D,nx,ny,nz_var,t)
!                data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                where (data3D .lt. 0.0d0) data3D = 0.0d0
!            case (QGRAUP_NAME)
!                data3D(:,:,1:nz_var) = 0.d0
!                !call ncReadVariable4DAtTime(this%fileName,'QGRAUP',data3D,nx,ny,nz_var,t)
!                !data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!                !where (data3D .lt. 0.0d0) data3D = 0.0d0
!            !case (QHAIL_NAME)
!            !    ! TODO: find the integral of the exponential distribution between
!            !    ! 0 and 5 mm for graupel, and 5 to infinity for hail
!            !    call ncReadVariable4DAtTime(this%fileName,'QHAIL',data3D,nx,ny,nz_var,t)
!            !    data3D(:,:,1:nz_var) = 1000.d0*data3D(:,:,1:nz_var) ! convert from kg/kg to g/kg
!            !    where (data3D .lt. 0.0d0) data3D = 0.0d0
!            case (UV_NAME)
!                allocate(tmp1(nx+1,ny,nz_var))
!                allocate(tmp2(nx,ny+1,nz_var))
!                ! read in staggered U and V
!                call ncReadVariable4DAtTime(this%fileName,'U',tmp1(:,:,1:nz_var),nx,ny,nz_var,t,1,nx+1,1,ny,1,nz_var)
!                call ncReadVariable4DAtTime(this%fileName,'V',tmp2(:,:,1:nz_var),nx,ny,nz_var,t,1,nx,1,ny+1,1,nz_var)
!                ! unstagger
!                data3D(:,:,1:nz_var) = sqrt(0.5*(tmp1(1:nx,1:ny,1:nz_var) + tmp1(2:nx+1,1:ny,1:nz_var))**2 + &
!                                            0.5*(tmp2(1:nx,1:ny,1:nz_var) + tmp2(1:nx,2:ny+1,1:nz_var))**2)
!                deallocate(tmp1)
!                deallocate(tmp2)
!            case (W_NAME)
!                call ncReadVariable4DAtTime(this%fileName,'W',data3D,nx,ny,nz_var,t)
!            case (DZ_NAME)
!                data3D(:,:,1:nz_var) = this%dz
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
!                call ncReadVariable3DAtTime(this%fileName,'TSK',data3D(:,:,1),nx,ny,t)
!            case (T_WATER_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'TSK',data3D(:,:,1),nx,ny,t)
!            case (SOIL_TYPE_NAME)
!                data3D(:,:,1:nz_var) = -1.
!            case (VEG_TYPE_NAME)
!                data3D(:,:,1:nz_var) = -1.
!            case (LU_INDEX_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'LU_INDEX',data3D(:,:,1),nx,ny,t)
!            case (SURFACE_HEIGHT_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'HGT',data3D(:,:,1),nx,ny,t)
!            case (LAT_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'XLAT',data3D(:,:,1),nx,ny,t)
!            case (LON_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'XLONG',data3D(:,:,1),nx,ny,t)
!            case (P_SFC_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'PSFC',data3D(:,:,1),nx,ny,t)
!                ! convert to hPa
!                data3D(:,:,1) = data3D(:,:,1)/100.
!            case (T2_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'T2',data3D(:,:,1),nx,ny,t)
!            case (Q2_NAME)
!                call ncReadVariable3DAtTime(this%fileName,'Q2',data3D(:,:,1),nx,ny,t)
!            case default
!                call error('Unknown field name ' // trim(fieldName))
!        end select
!    end subroutine

    function getLandCatCount_arw(this) result(numLandCat)
        implicit none

        class(ArwDataSet) :: this

        integer           :: numLandCat

        numLandCat = this%num_land_cat
    end function

    subroutine convertLatLonToIJ_arw(this,lat,lon,x,y)

        implicit none

        class(ArwDataSet) :: this

        real(real64), intent(in)   :: lat, lon
        real(real64), intent(out)  :: x, y

        real(real64) :: loc(2)

        real(real64), pointer :: latPtr(:,:)
        real(real64), pointer :: lonPtr(:,:)

        latPtr => this%getVariable2D(A3D_LAT_VAR)
        lonPtr => this%getVariable2D(A3D_LON_VAR)

        call dllToIJ(this%map_proj,this%truelat1,this%truelat2,this%stdlon,&
                     & lonPtr(1,1),latPtr(1,1),this%pole_lat,this%pole_lon,1.0d0,1.0d0,&
                     & this%dx,this%dy,this%latinc,this%loninc,this%cen_lat,this%cen_lon,lat,lon,loc)

        y = loc(1)
        x = loc(2)
    end subroutine

    include 'wrf_user_latlon_routines.f'
end module
