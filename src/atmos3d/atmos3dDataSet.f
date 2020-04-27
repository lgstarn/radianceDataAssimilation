module atmos3dDataSet_mod
    use parallelInfo_mod

    use iso_fortran_env

    use dataSet_mod
    use dataGrid_mod
    use dataArray_mod
    use dataVariable_mod
    use dataArrayReader_mod
    use dataExtent_mod
    use dataDimension_mod

    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    ! the east/west dimension
    character(*), parameter, public :: WEST_EAST_DIM_NAME   = 'west_east'
    ! the north/south dimension
    character(*), parameter, public :: SOUTH_NORTH_DIM_NAME = 'south_north'
    ! the north/south dimension
    character(*), parameter, public :: BOTTOM_TOP_DIM_NAME  = 'bottom_top'

    ! the nlevels (nlayers+1) pressure in hPa
    character(*), parameter, public :: P_LEVEL_VAR   = 'P_LEVEL'
    ! the nlayers temperature in K
    character(*), parameter, public :: T_VAR         = 'T'
    ! the nlayers water vapor in g/kg
    character(*), parameter, public :: QVAPOR_VAR    = 'QVAPOR'
    ! the "west-east" component of the wind speed in m/s
    character(*), parameter, public :: U_VAR         = 'U'
    ! the "south-north" component of the wind speed in m/s
    character(*), parameter, public :: V_VAR         = 'V'
    ! the vertical velocity in m/s
    character(*), parameter, public :: W_VAR         = 'W'
    ! the fraction of cloud in the volume (fuzzily defined), 0-1.
    character(*), parameter, public :: CLDFRA_VAR    = 'CLDFRA'
    ! the nlayers of all species of condensed water in g/kg
    character(*), parameter, public :: CWM_VAR       = 'CWM'
    ! the nlayers water (non-precipitating) cloud in g/kg
    character(*), parameter, public :: QCLOUD_VAR    = 'QCLOUD'
    ! the nlayers ice (non-precipitating) cloud in g/kg
    character(*), parameter, public :: QICE_VAR      = 'QICE'
    ! the nlayers rain (precipitating water) cloud in g/kg
    character(*), parameter, public :: QRAIN_VAR     = 'QRAIN'
    ! the nlayers "snow" cloud (lightly rimed precipitating ice) in g/kg
    character(*), parameter, public :: QSNOW_VAR     = 'QSNOW'
    ! the nlayers "graupel" cloud (rimed precipitating ice) in g/kg
    character(*), parameter, public :: QGRAUP_VAR    = 'QGRAUP'
    ! the nlayers "hail" cloud (heavily rimed precipitating ice) in g/kg
    character(*), parameter, public :: QHAIL_VAR     = 'QHAIL'
    ! the nlayers thickness of each level in m
    character(*), parameter, public :: DZ_VAR        = 'DZ'
    ! the eastern-component of the 10m wind speed in m/s
    character(*), parameter, public :: U10_VAR       = 'U10'
    ! the northern-component of the 10m wind speed in m/s
    character(*), parameter, public :: V10_VAR       = 'V10'
    ! the temperature of the land or water or ice in K
    character(*), parameter, public :: T_SURF_VAR    = 'T_SURF'
    ! the type of soil (TODO: define this more clearly)
    character(*), parameter, public :: SOIL_TYPE_VAR = 'SOIL_TYPE'
    ! the type of vegetation (TODO: define this more clearly)
    character(*), parameter, public :: VEG_TYPE_VAR  = 'VEG_TYPE'
    ! the NPOESS land use category
    character(*), parameter, public :: LU_INDEX_VAR  = 'LU_INDEX'
    ! the height of the surface from sea-level in meters
    character(*), parameter, public :: SFC_HGT_VAR   = 'SFC_Z'
    ! the latitude in degrees
    character(*), parameter, public :: A3D_LAT_VAR   = 'LAT'
    ! the longitude in degrees
    character(*), parameter, public :: A3D_LON_VAR   = 'LON'
    ! the latitude of all points across the domain in degrees
    character(*), parameter, public :: A3D_LAT_GLOBAL_VAR = 'LAT_GLOBAL'
    ! the longitude of all points across the domain in degrees
    character(*), parameter, public :: A3D_LON_GLOBAL_VAR = 'LON_GLOBAL'
    ! the pressure at the surface in hPa
    character(*), parameter, public :: P_SFC_VAR     = 'PSFC'
    ! the temperature at 2 meters above the surface in K
    character(*), parameter, public :: T2_VAR        = 'T2'
    ! the water vapor at 2 meters above the surface in g/kg
    character(*), parameter, public :: Q2_VAR        = 'Q2'

    type, public, abstract, extends(DataSet) :: Atmos3DDataSet
        !private

        class(DataGrid),     pointer :: grid

        ! Pressure (hPa) on the levels (nz+1, nwe, nsn)
        class(DataVariable), pointer :: pLevelVar
        ! Temperature (K) (nz, nwe, nsn)
        class(DataVariable), pointer :: tVar
        ! Water vapor mixing ratio (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qvaporVar
        ! "West-east" component of the wind speed in m/s
        class(DataVariable), pointer :: uVar
        ! "North-south" component of the wind speed in m/s
        class(DataVariable), pointer :: vVar
        ! Vertical velocity of the points (m/s) (nz, nwe, nsn)
        class(DataVariable), pointer :: wVar
        ! Cloud fraction (0-1) (nz, nwe, nsn)
        class(DataVariable), pointer :: cldfraVar
        ! Total cloud water (all species including ice) mixing ratio (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: cwmVar
        ! Non-precipitating water cloud (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qcloudVar
        ! Non-precipitating ice cloud (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qiceVar
        ! Precipitating ice cloud (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qrainVar
        ! Precipitating "snow" cloud (lightly rimed precipitating ice) (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qsnowVar
        ! Precipitating "graupel" cloud (rimed precipitating ice) (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qgraupVar
        ! Precipitating "hail" cloud (heavily precipitating ice) (g/kg) (nz, nwe, nsn)
        class(DataVariable), pointer :: qhailVar
        ! Thickness of each layer (m) (nz, nwe, nsn)
        class(DataVariable), pointer :: dzVar
        ! 10 m wind speed along the west-east dimension (m/s) (nz, nwe, nsn)
        class(DataVariable), pointer :: u10Var
        ! 10 m wind speed along the south-north dimension (m/s) (nz, nwe, nsn)
        class(DataVariable), pointer :: v10Var
        ! Temperature of the surface (K) (nwe, nsn)
        class(DataVariable), pointer :: tSurfVar
        ! Type of soil (TODO: define more clearly) (nwe, nsn)
        class(DataVariable), pointer :: soilTypeVar
        ! Type of vegetation (TODO: define more clearly) (nwe, nsn)
        class(DataVariable), pointer :: vegTypeVar
        ! NPOESS land use category (nwe, nsn)
        class(DataVariable), pointer :: luIndexVar
        ! Height of the sfc from sea-level (m) (nwe, nsn)
        class(DataVariable), pointer :: sfcZVar
        ! Latitude of the points (degress) (nwe, nsn)
        class(DataVariable), pointer :: latVar
        ! Longitude of the points (degress) (nwe, nsn)
        class(DataVariable), pointer :: lonVar
        ! Pressure at the surface (hPa) (nwe, nsn)
        class(DataVariable), pointer :: pSfcVar
        ! Temperature at 2 meters above the surface (K) (nwe, nsn)
        class(DataVariable), pointer :: t2Var
        ! Water-vapor mixing ratio at 2 meters above the surface (g/kg) (nwe, nsn)
        class(DataVariable), pointer :: q2Var

        contains
            procedure :: getWestEastExtent
            procedure :: getSouthNorthExtent
            procedure :: getBottomTopExtent
            !procedure :: getDx
            !procedure :: getDy

            procedure :: getGrid

            procedure :: getVariable2d
            procedure :: getVariable3d
            procedure :: getColumn
            procedure :: getColumnFields
            procedure :: spreadColumn
            procedure :: setColumn
            procedure :: setColumnFields
            procedure :: getValue2D
            procedure :: spreadValue2D
            procedure :: getValue3D

            procedure :: loadAtmos3dDataSet

            procedure, private :: getPointsAndWeight

            ! convert a given lat lon to i/j coordinates
            procedure(getLandCatCountAbstract), deferred :: getLandCatCount

            ! convert a given lat lon to i/j coordinates
            procedure(convertLatLonToIJAbstract), deferred :: convertLatLonToIJ

            ! get the 10m wind direction in degrees east from north
            procedure :: getWind10mDirection

            ! get the 10m wind speed in m/s
            procedure :: getWind10mSpeed

            procedure :: atmos3DDataSetConstructor
            procedure :: atmos3DDataSetDestructor
    end type

    abstract interface

        function getLandCatCountAbstract(this) result(numLandCat)
            import Atmos3DDataSet

            class(Atmos3DDataSet) :: this

            integer               :: numLandCat
        end function

        subroutine convertLatLonToIJAbstract(this,lat,lon,x,y)
            import Atmos3DDataSet
            import real64

            class(Atmos3DDataSet)      :: this

            real(real64), intent(in)   :: lat, lon
            real(real64), intent(out)  :: x, y
        end subroutine
    end interface

    contains

    subroutine atmos3DDataSetConstructor(this,reader)

        implicit none

        class(Atmos3DDataSet)                      :: this

        class(DataArrayReader), optional, pointer  :: reader

        call this%dataSetConstructor(reader)
    end subroutine

    subroutine loadAtmos3dDataSet(this,pinfo,grid,& !westEastDim,southNorthDim,bottomTopDim,&
        & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,    &
        & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar, &
        & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)
!,dx,dy,map_proj,truelat1,truelat2,stdlon,pole_lat,pole_lon,   &
!        & latinc,loninc,cen_lat,cen_lon,num_land_cat)

        implicit none

        class(Atmos3dDataSet)               :: this

        class(ParallelInfo),    pointer     :: pinfo

        class(DataGrid),        pointer     :: grid

        class(DataVariable),    pointer     :: pLevelVar
        class(DataVariable),    pointer     :: tVar
        class(DataVariable),    pointer     :: qvaporVar
        class(DataVariable),    pointer     :: uVar
        class(DataVariable),    pointer     :: vVar
        class(DataVariable),    pointer     :: wVar
        class(DataVariable),    pointer     :: cldfraVar
        class(DataVariable),    pointer     :: cwmVar
        class(DataVariable),    pointer     :: qcloudVar
        class(DataVariable),    pointer     :: qiceVar
        class(DataVariable),    pointer     :: qrainVar
        class(DataVariable),    pointer     :: qsnowVar
        class(DataVariable),    pointer     :: qgraupVar
        class(DataVariable),    pointer     :: qhailVar
        class(DataVariable),    pointer     :: dzVar
        class(DataVariable),    pointer     :: u10Var
        class(DataVariable),    pointer     :: v10Var
        class(DataVariable),    pointer     :: tSurfVar
        class(DataVariable),    pointer     :: soilTypeVar
        class(DataVariable),    pointer     :: vegTypeVar
        class(DataVariable),    pointer     :: luIndexVar
        class(DataVariable),    pointer     :: sfcZVar
        class(DataVariable),    pointer     :: latVar
        class(DataVariable),    pointer     :: lonVar
        class(DataVariable),    pointer     :: pSfcVar
        class(DataVariable),    pointer     :: t2Var
        class(DataVariable),    pointer     :: q2Var

!        real(real64), optional, intent(in)  :: dx
!        real(real64), optional, intent(in)  :: dy
!        integer,      optional, intent(in)  :: map_proj
!        real(real64), optional, intent(in)  :: truelat1
!        real(real64), optional, intent(in)  :: truelat2
!        real(real64), optional, intent(in)  :: stdlon
!        real(real64), optional, intent(in)  :: pole_lat
!        real(real64), optional, intent(in)  :: pole_lon
!        real(real64), optional, intent(in)  :: latinc
!        real(real64), optional, intent(in)  :: loninc
!        real(real64), optional, intent(in)  :: cen_lat
!        real(real64), optional, intent(in)  :: cen_lon
!        integer,      optional, intent(in)  :: num_land_cat

!        this%westEastDim   => westEastDim
!        this%southNorthDim => southNorthDim
!        this%bottomTopDim  => bottomTopDim

        this%grid        => grid
        this%pLevelVar   => pLevelVar
        this%tVar        => tVar
        this%qvaporVar   => qvaporVar
        this%uVar        => uVar
        this%vVar        => vVar
        this%wVar        => wVar
        this%cldfraVar   => cldfraVar
        this%cwmVar      => cwmVar
        this%qcloudVar   => qcloudVar
        this%qiceVar     => qiceVar
        this%qrainVar    => qrainVar
        this%qsnowVar    => qsnowVar
        this%qgraupVar   => qgraupVar
        this%qhailVar    => qhailVar
        this%dzVar       => dzVar
        this%u10Var      => u10Var
        this%v10Var      => v10Var
        this%tSurfVar    => tSurfVar
        this%soilTypeVar => soilTypeVar
        this%vegTypeVar  => vegTypeVar
        this%luIndexVar  => luIndexVar
        this%sfcZVar     => sfcZVar
        this%latVar      => latVar
        this%lonVar      => lonVar
        this%pSfcVar     => pSfcVar
        this%t2Var       => t2Var
        this%q2Var       => q2Var

!        if (present(map_proj)) then
!            this%map_proj = map_proj
!        end if
!
!        if (present(truelat1)) then
!            this%truelat1 = truelat1
!        end if
!
!        if (present(truelat2)) then
!            this%truelat2 = truelat2
!        end if
!
!        if (present(stdlon)) then
!            this%stdlon   = stdlon
!        end if
!
!        if (present(pole_lat)) then
!            this%pole_lat = pole_lat
!        end if
!
!        if (present(pole_lon)) then
!            this%pole_lon = pole_lon
!        end if
!
!        if (present(dx)) then
!            this%dx       = dx
!        end if
!
!        if (present(dy)) then
!            this%dy       = dy
!        end if
!
!        if (present(latinc)) then
!            this%latinc   = latinc
!        end if
!
!        if (present(loninc)) then
!            this%loninc   = loninc
!        end if
!
!        if (present(cen_lat)) then
!            this%cen_lat  = cen_lat
!        end if
!
!        if (present(cen_lon)) then
!            this%cen_lon  = cen_lon
!        end if
!
!        if (present(num_land_cat)) then
!            this%num_land_cat = num_land_cat
!        end if
    end subroutine

    subroutine atmos3DDataSetDestructor(this)
        implicit none

        class(Atmos3DDataSet) :: this

        if (associated(this%grid)) then
            deallocate(this%grid)
        end if

        ! all other dimensions and variables will be destroyed in the data set / group
    end subroutine

!    function getDx(this) result(dx)
!        implicit none
!
!        class(Atmos3DDataSet)  :: this
!        real(real64)           :: dx
!
!        dx = this%dx
!    end function
!
!    function getDy(this) result(dy)
!        implicit none
!
!        class(Atmos3DDataSet)  :: this
!        real(real64)           :: dy
!
!        dy = this%dy
!    end function

    function getWestEastDim(this,varName) result(ddim)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataDimension),   pointer     :: ddim

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        ddim => dvar%getDimensionNumber(2)
    end function

    function getSouthNorthDim(this,varName) result(ddim)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataDimension),   pointer     :: ddim

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        ddim => dvar%getDimensionNumber(3)
    end function

    function getBottomTopDim(this,varName) result(ddim)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataDimension),   pointer     :: ddim

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        ddim => dvar%getDimensionNumber(1)
    end function

    function getWestEastExtent(this,varName) result(dextent)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataExtent),   pointer     :: dextent

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        dextent => dvar%getExtentNumber(2)
    end function

    function getSouthNorthExtent(this,varName) result(dextent)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataExtent),   pointer     :: dextent

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        dextent => dvar%getExtentNumber(3)
    end function

    function getBottomTopExtent(this,varName) result(dextent)
        implicit none

        class(Atmos3DDataSet)               :: this

        character(*), optional, intent(in)  :: varName

        class(DataExtent),   pointer     :: dextent

        class(DataVariable), pointer :: dvar

        if (present(varName)) then
            dvar => this%getVariableByName(varName)
        else
            dvar => this%getVariableByName(QVAPOR_VAR)
        end if

        dextent => dvar%getExtentNumber(1)
    end function

    function getGrid(this) result(grid)
        implicit none

        class(Atmos3DDataSet) :: this

        class(DataGrid), pointer :: grid

        grid => this%grid
    end function

    function getVariable3D(this,fieldName,reverse) result(dptr)
        implicit none

        class(Atmos3DDataSet) :: this

        character(len=*), intent(in) :: fieldName
        logical,          intent(in) :: reverse

        real(real64), dimension(:,:,:), pointer :: dptr

        real(real64), dimension(:,:,:), pointer :: dptrTmp

        class(DataArray), pointer :: dArray

        integer :: nz

        dArray => this%getDataArrayByName(fieldName)

        if (associated(dArray)) then
            if (reverse) then
                call dArray%getArray(dptrTmp)

                nz = size(dptrTmp,1)

                ! use pointer remapping
                dptr => dptrTmp(nz:1:-1,:,:)
            else
                call dArray%getArray(dptr)
            end if
        else
            dptr => null()
        end if
    end function

    function getVariable2D(this,fieldName) result(dptr)
        implicit none

        class(Atmos3DDataSet)            :: this

        character(len=*), intent(in) :: fieldName

        real(real64),     pointer    :: dptr(:,:)

        class(DataArray), pointer :: dArray

        dArray => this%getDataArrayByName(fieldName)

        call dArray%getArray(dptr)
    end function

    subroutine getPointsAndWeight(this,x,y,x_corners,y_corners,x_weight,y_weight,varName)
        implicit none

        class(Atmos3DDataSet)              :: this

        real(real64),               intent(in)  :: x,y
        integer,      dimension(4), intent(out) :: x_corners, y_corners
        real(real64), dimension(4), intent(out) :: x_weight, y_weight
        character(*), optional,     intent(in)  :: varName

        integer :: x_f, x_c, y_f, y_c

        real(real64) :: x_p, y_p

        integer :: xs, xe, nx
        integer :: ys, ye, ny

        class(DataExtent), pointer :: westEastExtent
        class(DataExtent), pointer :: southNorthExtent

        westEastExtent   => this%getWestEastExtent(varName)
        southNorthExtent => this%getSouthNorthExtent(varName)

        call   westEastExtent%getLocalRange(xs, xe, nx)
        call southNorthExtent%getLocalRange(ys, ye, ny)

        if (x .lt. xs .or. y .lt. ys .or. x .gt. xe .or. y .gt. ye) then
            write(msgstr,*) 'In atmos3DDataSet, x or y (',x,',',y,') out of range: (',&
                xs,',',ys,') to (',xe,',',ye,')'
            call error(msgstr)
        end if

        x_f = floor(x)
        x_c = ceiling(x)
        y_f = floor(y)
        y_c = ceiling(y)

        x_p = x - dble(x_f)
        y_p = y - dble(y_f)

        x_corners(1) = x_f - xs + 1; y_corners(1) = y_f - ys + 1
        x_corners(2) = x_f - xs + 1; y_corners(2) = y_c - ys + 1
        x_corners(3) = x_c - xs + 1; y_corners(3) = y_f - ys + 1
        x_corners(4) = x_c - xs + 1; y_corners(4) = y_c - ys + 1

        x_weight(1)  = (1.0d0-x_p); y_weight(1)  = (1.0d0-y_p);
        x_weight(2)  = (1.0d0-x_p); y_weight(2)  =        y_p;
        x_weight(3)  =        x_p;  y_weight(3)  = (1.0d0-y_p);
        x_weight(4)  =        x_p;  y_weight(4)  =        y_p;
    end subroutine

    subroutine getColumn(this,fieldName,x,y,reverse,column)
        implicit none

        class(Atmos3DDataSet) :: this

        real(real64),     intent(in)  :: x,y
        character(len=*), intent(in)  :: fieldName
        logical,          intent(in)  :: reverse
        real(real64),     intent(out) :: column(:)

        integer :: i, nz

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        real(real64), dimension(:,:,:), pointer :: dptr3d

        dptr3d => this%getVariable3d(fieldName,reverse)

        if (.not. associated(dptr3d)) then
            call error('A3D: could not find the field name ' // fieldName)
        end if

        call this%getPointsAndWeight(x,y,x_corners,y_corners,x_weight,y_weight)

        nz = size(dptr3d,1)

        column(1:nz) = 0.0d0

        do i=1,4
            column(1:nz) = column(1:nz) + x_weight(i)*y_weight(i)*dptr3d(1:nz,&
                &x_corners(i),y_corners(i))
        end do
    end subroutine

    subroutine spreadColumn(this,fieldName,x,y,reverse,column)
        implicit none

        class(Atmos3DDataSet) :: this

        real(real64),    intent(in)  :: x, y
        character(len=*),intent(in)  :: fieldName
        logical,         intent(in)  :: reverse
        real(real64),    intent(in)  :: column(:)

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        real(real64), dimension(:,:,:), pointer :: dptr3d

        integer :: i, nz

        dptr3d => this%getVariable3d(fieldName,reverse)

        if (.not. associated(dptr3d)) then
            call error('A3D: could not find the field name ' // fieldName)
        end if

        call this%getPointsAndWeight(x,y,x_corners,y_corners,x_weight,y_weight)

        nz = size(dptr3d,1)

        do i=1,4
            dptr3d(1:nz   ,x_corners(i),y_corners(i)) = dptr3d(1:nz   ,x_corners(i),y_corners(i)) + &
                x_weight(i)*y_weight(i)*column(1:nz)
        end do
    end subroutine

    subroutine setColumn(this,fieldName,x,y,reverse,column)
        implicit none

        class(Atmos3DDataSet) :: this

        integer, intent(in) :: x,y
        character(len=*), intent(in) :: fieldName
        logical,          intent(in) :: reverse
        real(real64),     intent(in) :: column(:)

        integer :: nz

        real(real64), dimension(:,:,:), pointer :: dptr3d

        dptr3d => this%getVariable3d(fieldName,reverse)

        if (.not. associated(dptr3d)) then
            call error('A3D: could not find the field name ' // fieldName)
        end if

        nz = size(dptr3d,1)

        dptr3d( 1:nz  ,x,y) = column(1:nz)
    end subroutine

    subroutine getColumnFields(this,x,y,reverse,column,requestedFields)
        implicit none

        class(Atmos3DDataSet) :: this

        real(real64), intent(in)  :: x,y
        logical,      intent(in)  :: reverse
        real(real64), intent(out) :: column(:)
        character(*), intent(in)  :: requestedFields(:)

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        integer :: i, cstart, cend, nz
        integer :: fnum
        character(:), allocatable :: fieldName

        real(real64), dimension(:,:,:), pointer :: dptr3d

        cstart = 1

        do fnum=1,size(requestedFields)
            fieldName = requestedFields(fnum)

            dptr3d => this%getVariable3d(fieldName,reverse)

            if (.not. associated(dptr3d)) then
                call error('A3D: could not find the field name ' // fieldName)
            end if

            nz   = size(dptr3d,1)
            cend = cstart + nz - 1

            if (cend > size(column)) then
                call error('The size of the column was not large enough to hold all the data: ' // &
                    int2str(cend) // ' vs. ' // int2str(size(column)))
            end if

            call this%getPointsAndWeight(x,y,x_corners,y_corners,x_weight,y_weight)

            column(cstart:cend) = 0.0d0

            do i=1,4
                column(cstart:cend) = column(cstart:cend) + &
                    &x_weight(i)*y_weight(i)*dptr3d(1:nz   ,x_corners(i),y_corners(i))
            end do

            cstart = cend + 1
        end do
    end subroutine

    subroutine setColumnFields(this,x,y,reverse,column,requestedFields)
        implicit none

        class(Atmos3DDataSet) :: this

        integer,      intent(in) :: x,y
        logical,      intent(in) :: reverse
        real(real64), intent(in) :: column(:)
        character(*), intent(in) :: requestedFields(:)

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        integer :: i, cstart, cend, nz
        integer :: fnum
        character(:), allocatable :: fieldName

        real(real64), dimension(:,:,:), pointer :: dptr3d

        cstart = 1

        do fnum=1,size(requestedFields)
            fieldName = requestedFields(fnum)

            dptr3d => this%getVariable3d(fieldName,reverse)

            if (.not. associated(dptr3d)) then
                call error('A3D: could not find the field name ' // fieldName)
            end if

            nz   = size(dptr3d,1)
            cend = cstart + nz - 1

            if (cend > size(column)) then
                call error('The size of the column was not large enough to hold all the data: ' // &
                    int2str(cend) // ' vs. ' // int2str(size(column)))
            end if

            dptr3d( 1:nz,  x,y) = column(cstart:cend)

            cstart = cend + 1
        end do
    end subroutine

    function getValue2D(this,fieldName,x,y) result(value)
        implicit none

        class(Atmos3DDataSet) :: this

        real(real64),    intent(in) :: x,y
        character(len=*),intent(in) :: fieldName

        real(real64) :: value

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        real(real64), pointer :: dptr2d(:,:)

        integer :: i

        call this%getPointsAndWeight(x,y,x_corners,y_corners,x_weight,y_weight)

        dptr2d => this%getVariable2d(fieldName)

        value = 0.0d0

        do i=1,4
            value = value + x_weight(i)*y_weight(i)*dptr2d(x_corners(i),y_corners(i))
        end do
    end function

    subroutine spreadValue2D(this,fieldName,x,y,value)
        implicit none

        class(Atmos3DDataSet) :: this

        character(len=*),intent(in) :: fieldName
        real(real64),    intent(in) :: x,y
        real(real64),    intent(in) :: value

        integer,      dimension(4) :: x_corners, y_corners
        real(real64), dimension(4) :: x_weight, y_weight

        real(real64), pointer :: dptr2d(:,:)

        integer :: i

        dptr2d => this%getVariable2d(fieldName)

        call this%getPointsAndWeight(x,y,x_corners,y_corners,x_weight,y_weight)

        do i=1,4
            dptr2d(x_corners(i),y_corners(i)) = dptr2d(x_corners(i),y_corners(i)) + &
                x_weight(i)*y_weight(i)*value
        end do
    end subroutine

    function getValue3D(this,fieldName,x,y,z,reverse) result(value)
        implicit none

        class(Atmos3DDataSet) :: this

        integer,         intent(in) :: x,y,z
        character(len=*),intent(in) :: fieldName
        logical,         intent(in) :: reverse

        real(real64) :: value

        real(real64), pointer :: dptr3d(:,:,:)

        integer :: nz

        dptr3d => this%getVariable3d(fieldName,reverse)

        if (.not. associated(dptr3d)) then
            call error('A3D: could not find the field name ' // fieldName)
        end if

        nz = size(dptr3d,1)

!        if (z .gt. nz) then
!            write(msgstr, '(A,I4,A,A,A,I4)') 'Cannot get z ',z,' of field ',fieldName,' as length is',nz
!            call error(msgstr)
!        end if

        value = dptr3d(z,x,y)
    end function

    function getWind10mDirection(this,x,y) result(wdir)
        implicit none

        class(Atmos3DDataSet) :: this
        real(real64), intent(in) :: x,y

        real(real64) :: wdir

        wdir = 180.d0*atan2(this%getValue2d(U10_VAR,x,y),this%getValue2d(V10_VAR,x,y))/(4.d0*atan(1.d0))

        if (wdir .lt. 0) then
            wdir = wdir + 360.d0
        end if
    end function

    function getWind10mSpeed(this,x,y) result(uv10)
        implicit none

        class(Atmos3DDataSet) :: this
        real(real64), intent(in) :: x,y

        real(real64) :: uv10

        uv10 = sqrt(this%getValue2d(U10_VAR,x,y)**2+this%getValue2d(V10_VAR,x,y)**2)
    end function

end module
