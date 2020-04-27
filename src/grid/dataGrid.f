module dataGrid_mod

    use parallelInfo_mod

    use iso_fortran_env

    use linkedList_mod

    use tiling_mod

    use dataType_mod
    use dataArray_mod
    use dataShape_mod
    use dataVariable_mod
    use dataExtent_mod

    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    public :: DataGrid

    type, abstract   :: DataGrid
        private
            class(LinkedList),   pointer :: globalCoordinateVars => null()
            class(LinkedList),   pointer ::  localCoordinateVars => null()
            class(DataVariable), pointer :: globalCoordOwnersVar => null()
            class(LinkedList),   pointer ::  localTilings        => null()
            class(Tiling),       pointer :: globalTiling         => null()
            logical                      ::  tiled               = .false.

        integer, pointer :: dimsToTile(:) => null()

        contains
            procedure :: addCoordinateVar

            procedure :: getNDimensions

            procedure :: getGlobalGridShape
            procedure :: getLocalGridShape

            procedure :: getGlobalCoordinateVar
            procedure :: getLocalCoordinateVar

            procedure :: getGlobalCoordinateExtent
            procedure :: getLocalCoordinateExtent

            procedure :: getGlobalCoordOwnersVar
            !procedure :: setGlobalCoordOwnersVar

            procedure :: getGlobalCoordinateDimCount
            procedure :: getLocalCoordinateExtentCount
            procedure :: getGlobalCoordinateDimRange
            procedure :: getLocalCoordinateExtentRange

            procedure :: getDimsToTile

            procedure ::  getLocalTilings
            procedure :: getGlobalTiling

            procedure :: tile
            procedure :: isTiled

            procedure(getDistanceToCenterAbstract), deferred :: getDistanceToCenter
            procedure(doTileAbstract),              deferred :: doTile
            procedure(cloneAbstract),               deferred :: clone

            generic :: dataGridConstructor => &
                dataGridConstructor_normal, &
                dataGridConstructor_set

            procedure, private :: dataGridConstructor_normal
            procedure, private :: dataGridConstructor_set

            procedure :: dataGridDestructor ! clean up all allocated variables
    end type

    abstract interface
!        subroutine getNearestPointAbstract(this,ndim,nloci,lociVals,nearestPts)
!            import DataGrid
!            import real64
!
!            class(DataGrid)           :: this
!
!            integer,      intent(in)  :: ndim
!            integer,      intent(in)  :: nloci
!            real(real64), intent(in)  :: lociVals(ndim,nloci)
!            integer,      intent(out) :: nearestPts(nloci)
!        end subroutine

        subroutine getDistanceToCenterAbstract(this,ndim,nloci,lociVals,distances)
            import DataGrid
            import real64

            class(DataGrid)           :: this

            integer,             intent(in)  :: ndim
            integer,             intent(in)  :: nloci
            real(real64),        intent(in)  :: lociVals(ndim,nloci)
            real(real64),        intent(out) :: distances(nloci)
        end subroutine

        subroutine doTileAbstract(this,pinfo,localTilings,globalTiling)
            import DataGrid
            import ParallelInfo
            import LinkedList
            import Tiling

            class(DataGrid)               :: this

            class(ParallelInfo),              pointer  :: pinfo
            class(LinkedList),   intent(out), pointer  ::  localTilings
            class(Tiling),       intent(out), pointer  :: globalTiling
        end subroutine

        function cloneAbstract(this,copyData) result(dgptr)
            import DataGrid

            class(DataGrid)             :: this
            logical,         intent(in) :: copyData

            class(DataGrid), pointer    :: dgptr
        end function
    end interface

    contains

    subroutine dataGridConstructor_normal(this,xcv_local,ycv_local,dimsToTile)

        implicit none

        class(DataGrid)               :: this

        class(DataVariable), pointer  :: xcv_local
        class(DataVariable), pointer  :: ycv_local
        integer,             optional :: dimsToTile(2)

        class(*), pointer :: optr

        integer :: nx, ny

        this%globalCoordinateVars => LinkedList()
        this% localCoordinateVars => LinkedList()
        this%localTilings         => LinkedList()

        ! add xcv, ycv to local coordinate vars
        optr => xcv_local; call this%localCoordinateVars%add(optr)
        optr => ycv_local; call this%localCoordinateVars%add(optr)

        if (present(dimsToTile)) then
            allocate(this%dimsToTile(2))
            this%dimsToTile = dimsToTile
        end if
    end subroutine

    subroutine dataGridConstructor_set(this,localTilings,globalTiling,xcv_local,ycv_local,&
        & xcv_global,ycv_global,globalCoordOwnersVar,dimsToTile)

        implicit none

        class(DataGrid)                   :: this

        class(LinkedList),   pointer  :: localTilings
        class(Tiling),       pointer  :: globalTiling
        class(DataVariable), pointer  :: xcv_local
        class(DataVariable), pointer  :: ycv_local
        class(DataVariable), pointer  :: xcv_global
        class(DataVariable), pointer  :: ycv_global
        class(DataVariable), pointer  :: globalCoordOwnersVar
        integer,             optional :: dimsToTile(2)

        this%globalCoordinateVars => LinkedList()
        this%localCoordinateVars  => LinkedList()

        if (associated(localTilings) .and. associated(globalTiling)) then
            this%localTilings =>  localTilings
            this%globalTiling => globalTiling
            this%tiled = .true.
        end if

        call this%addCoordinateVar(xcv_global,xcv_local)
        call this%addCoordinateVar(ycv_global,ycv_local)

        this%globalCoordOwnersVar => globalCoordOwnersVar

        if (present(dimsToTile)) then
            allocate(this%dimsToTile(2))
            this%dimsToTile = dimsToTile
        end if
    end subroutine

    subroutine dataGridDestructor(this)
        implicit none

        class(DataGrid)  :: this

        if (associated(this%globalCoordinateVars)) then
            deallocate(this%globalCoordinateVars)
            nullify(this%globalCoordinateVars)
        end if

        if (associated(this%localCoordinateVars)) then
            deallocate(this%localCoordinateVars)
            nullify(this%localCoordinateVars)
        end if

        if (associated(this%globalCoordOwnersVar)) then
            deallocate(this%globalCoordOwnersVar)
            nullify(this%globalCoordOwnersVar)
        end if

        if (associated(this%dimsToTile)) then
            deallocate(this%dimsToTile)
            nullify(this%dimsToTile)
        end if

        if (associated(this%localTilings)) then
            deallocate(this%localTilings)
            nullify(this%localTilings)
        end if

        if (associated(this%globalTiling)) then
            deallocate(this%globalTiling)
            nullify(this%globalTiling)
        end if
    end subroutine

    subroutine addCoordinateVar(this,cv_global,cv_local)
        implicit none

        class(DataGrid)    :: this

        class(DataVariable),           pointer :: cv_global
        class(DataVariable),           pointer :: cv_local

        class(*), pointer :: optr

        optr => cv_global; call this%globalCoordinateVars%add(optr)
        optr => cv_local;  call this%localCoordinateVars%add(optr)
    end subroutine

    function getNDimensions(this) result(ndim)
        implicit none

        class(DataGrid) :: this

        integer         :: ndim

        class(DataVariable), pointer :: cv1

        ! all coordinate variables should have the same size and number of dims
        cv1 => this%getGlobalCoordinateVar(1)

        ndim = cv1%getNDimensions()
    end function

    function getGlobalCoordinateVar(this,varNum) result(cvar)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: varNum

        class(DataVariable), pointer :: cvar

        class(*),            pointer :: optr

        integer :: i

        if (varNum > this%globalCoordinateVars%getListSize()) then
            write(msgstr,*) 'Error: in dataGrid, requested variable number larger than number of variables:', &
                & varNum,this%getNDimensions()
            call error(msgstr)
        end if

        call this%globalCoordinateVars%first()

        optr => this%globalCoordinateVars%get(varNum)

        select type(optr)
            class is (DataVariable)
                ! cast down
                cvar => optr
            class default
                call error('Unknown class in global coords list')
        end select
    end function

    function getLocalCoordinateVar(this,varNum) result(cvar)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: varNum

        class(DataVariable), pointer :: cvar

        class(*), pointer :: optr

        integer :: i

        if (varNum > this%localCoordinateVars%getListSize()) then
            write(msgstr,*) 'Error: in dataGrid, requested variable number larger than number of variables:',&
                &varNum,this%getNDimensions()
            call error(msgstr)
        end if

        optr => this%localCoordinateVars%get(varNum)

        select type(optr)
            class is (DataVariable)
                ! cast down
                cvar => optr
            class default
                call error('Unknown class in local coords list')
        end select
    end function

    function getGlobalCoordinateExtent(this,extentNum) result(dextent)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: extentNum

        class(DataExtent), pointer :: dextent

        class(DataVariable), pointer :: cvar

        cvar => this%getGlobalCoordinateVar(1)
        dextent => cvar%getExtentNumber(extentNum)
    end function

    function getLocalCoordinateExtent(this,extentNum) result(dextent)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: extentNum

        class(DataExtent), pointer :: dextent

        class(DataVariable), pointer :: cvar

        cvar => this%getLocalCoordinateVar(1)
        dextent => cvar%getExtentNumber(extentNum)
    end function

    function getGlobalCoordinateDimCount(this,dimNum) result(dimCount)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: dimNum

        integer :: dimCount

        class(DataVariable), pointer :: cvar

        cvar => this%getGlobalCoordinateVar(1)
        dimCount = cvar%getGlobalDimCount(dimNum)
    end function

    function getLocalCoordinateExtentCount(this,dimNum) result(dimCount)
        implicit none

        class(DataGrid) :: this

        integer, intent(in) :: dimNum

        integer :: dimCount

        class(DataVariable), pointer :: cvar

        cvar => this%getLocalCoordinateVar(1)
        dimCount = cvar%getLocalExtentCount(dimNum)
    end function

    subroutine getLocalCoordinateExtentRange(this,dimNum,xs,xe,nx)
        implicit none

        class(DataGrid) :: this

        integer,           intent(in)  :: dimNum
        integer,           intent(out) :: xs, xe, nx

        class(DataVariable), pointer :: cvar

        cvar => this%getLocalCoordinateVar(1)
        call cvar%getLocalExtentRange(dimNum,xs,xe,nx)
    end subroutine

    subroutine getGlobalCoordinateDimRange(this,dimNum,xs,xe,nx)
        implicit none

        class(DataGrid) :: this

        integer,              intent(in)  :: dimNum
        integer,              intent(out) :: xs, xe, nx

        class(DataVariable),  pointer     :: cvar

        cvar => this%getGlobalCoordinateVar(1)
        call cvar%getGlobalDimRange(dimNum,xs,xe,nx)
    end subroutine

    function getDimsToTile(this) result(dimsToTile)
        implicit none

        class(DataGrid) :: this

        integer, pointer :: dimsToTile(:)

        dimsToTile => this%dimsToTile
    end function

    function getGlobalGridShape(this) result(dShape)
        implicit none

        class(DataGrid) :: this

        class(DataShape), pointer :: dShape

        class(DataVariable), pointer :: cv1

        cv1 => this%getGlobalCoordinateVar(1)

        dShape => cv1%getDataShape()
    end function

    function getLocalGridShape(this) result(dShape)
        implicit none

        class(DataGrid) :: this

        class(DataShape), pointer :: dShape

        class(DataVariable), pointer :: cv1

        cv1 => this%getLocalCoordinateVar(1)

        dShape => cv1%getDataShape()
    end function

!    function findOwners(this,pinfo,dShape_global,dShape_local) result(globalCoordOwnersVar)
!        implicit none
!
!        class(DataGrid)                  :: this
!
!        class(ParallelInfo),     pointer :: pinfo
!
!        class(DataShape),        pointer :: dShape_global
!        class(DataShape),        pointer :: dShape_local
!
!        integer, allocatable :: minIndexes(:)
!        integer, allocatable :: maxIndexes(:)
!        integer, allocatable :: counts(:)
!
!        integer, allocatable :: ranges(:,:)
!
!        class(MirroredVariable), pointer :: globalCoordOwnersVar
!
!        integer :: ndim
!
!        allocate(globalCoordOwnersVar)
!        call globalCoordOwnersVar%mirroredVariableConstructor('grid_point_owners',&
!            & INT_TYPE_NUM,dShape_global%clone())
!
!        ndim = dShape_global%getNDimensions()
!
!        call dShape_local%getLocalRanges(minIndexes,maxIndexes,counts)
!
!        allocate(ranges(ndim,2))
!        ranges(:,1) = minIndexes(:)
!        ranges(:,2) = maxIndexes(:)
!
!        call globalCoordOwnersVar%addChangeRange(ranges, pinfo%getRank(), &
!            & dShape_global%getNDimensions())
!
!        call globalCoordOwnersVar%synchronize(pinfo)
!
!        deallocate(ranges)
!    end function

    function getGlobalCoordOwnersVar(this) result(ownersVar)
        implicit none

        class(DataGrid) :: this

        class(DataVariable), pointer :: ownersVar

        if (.not. this%tiled) then
            call error('Cannot find the owners if the data grid has not been tiled; call tile() first')
        end if

        ownersVar => this%globalCoordOwnersVar
    end function

    function getLocalTilings(this) result(localTilings)
        implicit none

        class(DataGrid) :: this

        class(LinkedList), pointer :: localTilings

        if (.not. this%tiled) then
            call error('The grid has not yet been tiled')
        end if

        localTilings => this%localTilings
    end function

    function getGlobalTiling(this) result(globalTiling)
        implicit none

        class(DataGrid) :: this

        class(Tiling), pointer :: globalTiling

        if (.not. this%tiled) then
            call error('The grid has not yet been tiled')
        end if

        globalTiling => this%globalTiling
    end function

    subroutine tile(this,pinfo)
        implicit none

        class(DataGrid)              :: this

        class(ParallelInfo), pointer :: pinfo

        class(DataVariable), pointer :: cv_local
        class(DataVariable), pointer :: cv_global

        class(DataVariable), pointer :: owner_local
        class(DataVariable), pointer :: owner_global

        class(DataShape), pointer :: lshape
        class(DataArray), pointer :: dArray

        integer, dimension(:), pointer :: owner1d

        character(*), parameter :: NODE_OWNERS = 'nodeOwners'

        class(*), pointer :: optr

        integer :: i

        call this%localCoordinateVars%first()

        cv_local => this%getLocalCoordinateVar(1)
        lshape   => cv_local%getDataShape()

        ! create a new variable with the same shape as the coordinate variable
        allocate(owner_local)
        call owner_local%dataVariableConstructor(NODE_OWNERS,INT_TYPE_NUM,lshape,&
            collective=.true.,distributed=.true.)

        ! get the data array and set it to this processor's rank
        dArray  => owner_local%getDataArray()
        owner1d => dArray%getDataPointer_int()
        owner1d(:) = pinfo%getRank()

        ! now gather the variable to global in order to have all the owners together
        this%globalCoordOwnersVar => owner_local%gatherToGlobal(pinfo,NODE_OWNERS // '_GLOBAL')

        ! gather all of the coordinate variables to global and add them to the list
        do i=1,this%localCoordinateVars%getListSize()
            cv_local  => this%getLocalCoordinateVar(i)
            cv_global => cv_local%gatherToGlobal(pinfo,cv_local%getName() // '_GLOBAL')

            optr => cv_global; call this%globalCoordinateVars%add(optr)
        end do

        call this%doTile(pinfo,this%localTilings,this%globalTiling)

!        nx = xcv_global%getGlobalDimCount(1)
!        ny = xcv_global%getGlobalDimCount(2)
!
!        if (nx /= ycv_global%getGlobalDimCount(1) .or. &
!            ny /= ycv_global%getGlobalDimCount(2)) then
!
!            call error('The x and y global dimensions in scattered grid should have the same sizes: ' // &
!                int2str(nx) // ' and ' // int2str(ny) // ' for x versus ' // &
!                int2str(ycv_global%getGlobalDimCount(1)) // ' and ' // &
!                int2str(ycv_global%getGlobalDimCount(2)) // ' for y')
!        end if

        ! here we assume all coordinate variables have the same data shape
!        this%globalCoordOwnersVar => this%findOwners(pinfo,cv_global%getDataShape(),&
!            & cv_local%getDataShape())

        this%tiled = .true.
    end subroutine

    function isTiled(this) result(tiled)
        implicit none

        class(DataGrid) :: this

        logical         :: tiled

        tiled = this%tiled
    end function
end module
