module regularTriangularLatLonGrid_mod

    use iso_fortran_env

    use parallelInfo_mod

    use dataGrid_mod

    use linkedList_mod
    use dictionary_mod

    use tiling_mod
    use tripack_mod
    use regularTriangularTiling_mod

    use dataType_mod
    use dataArray_mod
    use dataShape_mod
    use dataVariable_mod
    use dataExtent_mod

    use geodesic_mod

    use sortUtils_mod
    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    real(real64), parameter :: d2r = dacos(0.d0)/90.d0

    integer, parameter :: X_START = 1
    integer, parameter :: X_END   = 2
    integer, parameter :: Y_START = 3
    integer, parameter :: Y_END   = 4
    integer, parameter :: LSIZE   = 5

    integer, parameter :: MIN_LAT = 1
    integer, parameter :: MIN_LON = 2
    integer, parameter :: MAX_LAT = 3
    integer, parameter :: MAX_LON = 4

    public :: RegularTriangularLatLonGrid

    type, extends(DataGrid)   :: RegularTriangularLatLonGrid
        private
            !real(real64), pointer :: allXYBounds(:,:,:)
            !real(real64), pointer :: allXYCenter(:,:)
            real(real64) :: latc
            real(real64) :: lonc
            integer      :: myproc

            integer,      allocatable :: localBounds(:,:)
            real(real64), allocatable ::    xyBounds(:,:)

            class(RegularTriangularTiling), pointer :: localTriTil => null()

        contains
            procedure :: getDistanceToCenter
            procedure :: doTile
            procedure :: cloneRegularTriangularLatLonGrid
            procedure :: clone

            generic   :: regularTriangularLatLonGridConstructor => &
                         regularTriangularLatLonGridConstructor_local, &
                         regularTriangularLatLonGridConstructor_all

            procedure, private :: regularTriangularLatLonGridConstructor_local
            procedure, private :: regularTriangularLatLonGridConstructor_all

            final     :: regularTriangularLatLonGridDestructor ! clean up all allocated variables
    end type

    contains

    subroutine regularTriangularLatLonGridConstructor_local(this,xcv_local,ycv_local,dimsToTriangulate)

        implicit none

        class(RegularTriangularLatLonGrid) :: this

        class(ParallelInfo), pointer  :: pinfo
        class(DataVariable), pointer  :: xcv_local
        class(DataVariable), pointer  :: ycv_local
        integer,             optional :: dimsToTriangulate(2)

        call this%dataGridConstructor(xcv_local,ycv_local,dimsToTriangulate)
    end subroutine

    subroutine regularTriangularLatLonGridConstructor_all(this,localTilings,globalTiling, &
        & xcv_local,ycv_local,xcv_global,ycv_global,globalCoordOwnersVar,dimsToTile)

        implicit none

        class(RegularTriangularLatLonGrid) :: this

        class(LinkedList),   pointer  ::  localTilings
        class(Tiling),       pointer  :: globalTiling
        class(DataVariable), pointer  :: xcv_local
        class(DataVariable), pointer  :: ycv_local
        class(DataVariable), pointer  :: xcv_global
        class(DataVariable), pointer  :: ycv_global
        class(DataVariable), pointer  :: globalCoordOwnersVar
        integer,             optional :: dimsToTile(2)

        call this%dataGridConstructor(localTilings,globalTiling,xcv_local,ycv_local, &
            & xcv_global,ycv_global,globalCoordOwnersVar,dimsToTile)
    end subroutine

    subroutine regularTriangularLatLonGridDestructor(this)
        implicit none

        type(RegularTriangularLatLonGrid)  :: this

        call this%dataGridDestructor()
    end subroutine

    subroutine getDistanceToCenter(this,ndim,nloci,lociVals,distances)
        implicit none

        class(RegularTriangularLatLonGrid)      :: this

        integer,      intent(in)  :: ndim
        integer,      intent(in)  :: nloci
        real(real64), intent(in)  :: lociVals(ndim,nloci)
        !integer,      intent(out) :: nearestPts(nloci)
        real(real64), intent(out) :: distances(nloci)

        class(DataArray), pointer :: dArray

        integer,      pointer :: node_ij(:,:)
        real(real64), pointer :: node_xy(:,:)

        integer :: i

        real(real64) :: azi1, azi2

        real(real64) :: latcr
        real(real64) :: loncr

        if (ndim /= 2) then
            call error('The data grid class currently only supports 2D.')
        end if

        if (.not. this%isTiled()) then
            call error('The nearest points can only be found after tiling the grid')
        end if

        latcr = this%latc*d2r
        loncr = this%lonc*d2r

        do i=1,nloci

            if (lociVals(1,i) < this%xyBounds(MIN_LAT,this%myproc) .or. &
                lociVals(1,i) > this%xyBounds(MAX_LAT,this%myproc) .or. &
                lociVals(2,i) < this%xyBounds(MIN_LON,this%myproc) .or. &
                lociVals(2,i) > this%xyBounds(MAX_LON,this%myproc)) then

                distances(i) = 1d99
            else
                call geodesic_inverse(lociVals(1,i), lociVals(2,i), this%latc, this%lonc, &
                    distances(i), azi1, azi2, 1)
            end if
        end do
    end subroutine

    ! Note: This method is collective over pinfo%getCommunicator()
    !
    ! On entry into this method, the local and global coordinate vars have been set.
    ! The local coordinate var is the tiling region on this particular processor.
    ! The first two coordinate variables should be latitude (degrees) and longitude (degrees),
    ! in that order. The variables can be either real or double precision.
    subroutine doTile(this,pinfo,localTilings,globalTiling)
        implicit none

        class(RegularTriangularLatLonGrid)               :: this

        class(ParallelInfo),              pointer  :: pinfo
        class(LinkedList),   intent(out), pointer  ::  localTilings
        class(Tiling),       intent(out), pointer  :: globalTiling

        class(RegularTriangularTiling), pointer :: localTriTile
        class(RegularTriangularTiling), pointer :: globalTriTile

        class(DataVariable), pointer :: latGlobal, lonGlobal
        class(DataVariable), pointer :: latLocal,  lonLocal

        class(DataShape), pointer :: latShape

        real(real32), pointer :: latptr_real(:,:)
        real(real32), pointer :: lonptr_real(:,:)
        real(real64), pointer :: latptr_dble(:,:)
        real(real64), pointer :: lonptr_dble(:,:)

        class(Dictionary), pointer :: triDict
        class(Dictionary), pointer :: nbrDict

        logical :: isReal
        integer :: typeNum

        integer :: xls, xle, nlx
        integer :: yls, yle, nly
        integer :: xbs, xbe, nbx
        integer :: ybs, ybe, nby
        integer :: xgs, xge, ngx
        integer :: ygs, yge, ngy
        integer :: xgc, ygc
        integer :: xis, xie, nix
        integer :: yis, yie, niy

        integer :: nodeNum
        integer, parameter :: buffer  = 20
        integer, parameter :: buffer2 = 5
        integer :: numtot
        integer :: ierr

        integer,      dimension(:,:), pointer :: node_ij => NULL()
        real(real64), dimension(:,:), pointer :: node_xy => NULL()

        real(real64) :: latc, lonc, lat, lon, k, x, y
        real(real64) :: latd, lond, latr, lonr, londiff
        real(real64) :: minLatd, minLond
        real(real64) :: maxLatd, maxLond

        integer :: i, j, ind, nind, tind

        real(real64), pointer :: dist(:)
        integer :: ier
        integer :: nit, nlist
        integer :: nlist_all

        integer,      pointer :: allLptr(:)
        integer,      pointer :: allLend(:)
        integer,      pointer :: allList(:)
        integer,      pointer :: allNodeij(:,:)
        real(real64), pointer :: allNodexy(:,:)
        integer :: allLnew

        integer :: iv(3), jv(3)

        type triData
            integer :: triInd
        end type

        integer, allocatable :: nodes(:)

        type(triData), pointer :: tdat

        class(*), pointer :: optr

        integer, allocatable :: lcc(:)
        integer, allocatable :: lct(:)
        integer :: lwk
        integer, allocatable :: iwk(:)

        integer, pointer :: listv(:)
        integer, pointer :: lptrv(:)
        integer, pointer :: lendv(:)
        integer, pointer :: elementsv(:,:)
        integer, pointer :: neighborsv(:,:)
        integer, pointer :: nodeijv(:,:)
        real(real64), pointer :: nodexyv(:,:)

        integer :: xs, xe
        integer :: ys, ye
        integer :: proc, indAll

        logical :: nbrb, nodeb
        integer :: listFac

        integer :: node, lpl, lp, n1, na, nb, nt

        integer, dimension(:,:), pointer :: tri_i_node_ij

        character(DICT_KEY_LENGTH) :: triKey, nbrKey

        integer, parameter :: HASH_SIZE = 2895134 ! a prime number about 20% larger than largest estimated data set

        integer :: allTriInd, i1, j1, gind, l, m, loop, nbr, nodeInd, triInd

        logical :: allIn

        class(RegularTriangularTiling), pointer :: localTriTil

        latLocal => this%getLocalCoordinateVar(1)
        lonLocal => this%getLocalCoordinateVar(2)

        latGlobal => this%getGlobalCoordinateVar(1)
        lonGlobal => this%getGlobalCoordinateVar(2)

        ! give the local data plus a buffer to each process
        latShape => latLocal%getDataShape()

        typeNum = latGlobal%getDataTypeNum()

        if (typeNum == REAL_TYPE_NUM) then
            call latGlobal%getArray(latptr_real)
            call lonGlobal%getArray(lonptr_real)

            isReal = .true.
        elseif (typeNum == DOUBLE_TYPE_NUM) then
            call latGlobal%getArray(latptr_dble)
            call lonGlobal%getArray(lonptr_dble)

            isReal = .false.
        else
            call error('The lat/lon coordinate var type must be either real or double.')
        end if

        call latShape%getLocalRange(1,xls,xle,nlx)
        call latShape%getLocalRange(2,yls,yle,nly)
        call latShape%getGlobalRange(1,xgs,xge,ngx)
        call latShape%getGlobalRange(2,ygs,yge,ngy)

        xbs = max(xgs,xls-buffer)
        xbe = min(xge,xle+buffer)
        ybs = max(ygs,yls-buffer)
        ybe = min(yge,yle+buffer)

        nbx = xbe-xbs+1
        nby = ybe-ybs+1

        nodenum = nbx*nby

        allocate(node_ij(2,nodenum))
        allocate(node_xy(2,nodenum))
        !allocate(node_xpyp(2,nodenum))

        ! find the xcenter of each local data
        if (mod(nbx,2) == 0) then
            xgc = (xbs+xbe+1)/2
        else
            xgc = (xbs+xbe)/2
        end if

        ! find the ycenter of each local data
        if (mod(nbx,2) == 0) then
            ygc = (ybs+ybe+1)/2
        else
            ygc = (ybs+ybe)/2
        end if

        if (isReal) then
            latc = d2r*latptr_real(xgc,ygc)
            lonc = d2r*lonptr_real(xgc,ygc)
        else
            latc = d2r*latptr_dble(xgc,ygc)
            lonc = d2r*lonptr_dble(xgc,ygc)
        end if

        ind = 0

        minLond =  1d99
        minLatd =  1d99
        maxLond = -1d99
        maxLatd = -1d99

        ! project the lat/lon data to a plane with a stereographic projection
        do j=1,nby
            do i=1,nbx
                ind = ind + 1
                node_ij(1,ind) = xbs+i-1
                node_ij(2,ind) = ybs+j-1

                if (isReal) then
                    latd = dble(latptr_real(node_ij(1,ind),node_ij(2,ind)))
                    lond = dble(lonptr_real(node_ij(1,ind),node_ij(2,ind)))
                else
                    latd = latptr_dble(node_ij(1,ind),node_ij(2,ind))
                    lond = lonptr_dble(node_ij(1,ind),node_ij(2,ind))
                end if

                londiff = lond - lonc/d2r

                if (abs(londiff) > 180.d0) then
                    lond = lond - sign(360.d0,londiff)
                end if

                if (lond < minLond) minLond = lond
                if (latd < minLatd) minLatd = latd
                if (lond > maxLond) maxLond = lond
                if (latd > maxLatd) maxLatd = latd

                node_xy(1,ind)   = latd
                node_xy(2,ind)   = lond
            end do
        end do

        allocate(allNodeij(2,ngx*ngy))
        allocate(allNodexy(2,ngx*ngy))

        ind = 0

        do j=1,ngy
            do i=1,ngx
                ind = ind + 1
                allNodeij(1,ind) = i
                allNodeij(2,ind) = j

                if (isReal) then
                    latd = dble(latptr_real(i,j))
                    lond = dble(lonptr_real(i,j))
                else
                    latd = latptr_dble(i,j)
                    lond = lonptr_dble(i,j)
                end if

                allNodexy(1,ind)   = latd
                allNodexy(2,ind)   = lond
            end do
        end do

        indAll  = 0

        allocate(this%localBounds(4,pinfo%getCommSize()))
        allocate(this%xyBounds(4,pinfo%getCommSize()))

        do proc=0,pinfo%getCommSize()-1
            if (pinfo%getRank() == proc) then
                ! store the proc (1-inded) for later to aid access to localBounds
                this%myproc = proc+1

                nodeijv    => node_ij
                nodexyv    => node_xy

                allocate(localTriTil)
                call localTriTil%regularTriangularTilingConstructor(nbx, nby, &
                    & nodeijv, nodexyv, deallocNode_ij=.true.,deallocNode_xy=.true.)

                elementsv  => localTriTil%elements
                neighborsv => localTriTil%elementNeighbor

                this%localBounds(X_START,this%myproc) = xls
                this%localBounds(X_END,  this%myproc) = xle
                this%localBounds(Y_START,this%myproc) = yls
                this%localBounds(Y_END,  this%myproc) = yle

                this%   xyBounds(MIN_LAT,this%myproc) = minLatd
                this%   xyBounds(MIN_LON,this%myproc) = minLond
                this%   xyBounds(MAX_LAT,this%myproc) = maxLatd
                this%   xyBounds(MAX_LON,this%myproc) = maxLond

                this%latc = latc/d2r
                this%lonc = lonc/d2r
            end if

            call bcastnd_varlen(elementsv,proc,pinfo%getCommunicator(),'bcasting the elements from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(neighborsv,proc,pinfo%getCommunicator(),'bcasting the neighbors from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(nodeijv,proc,pinfo%getCommunicator(),'bcasting the nodei from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(nodexyv, proc,pinfo%getCommunicator(),'bcasting the nodexy from rank ' // &
                & int2str(proc))
            call bcast1d(this%localBounds(:,proc+1),5,proc,pinfo%getCommunicator(),'bcasting the localBounds from rank ' // &
                & int2str(proc))
            call bcast1d(this%xyBounds(:,proc+1),4,proc,pinfo%getCommunicator(),'bcasting the xyBounds from rank ' // &
                & int2str(proc))

            xs = this%localBounds(X_START,proc+1)
            xe = this%localBounds(X_END,proc+1)
            ys = this%localBounds(Y_START,proc+1)
            ye = this%localBounds(Y_END,proc+1)

            if (pinfo%getRank() == proc) then
                this%localTriTil => localTriTil
            else
                allocate(localTriTil)
                call localTriTil%regularTriangularTilingConstructor(xe-xs+1,ye-ys+1,nodeijv,nodexyv, &
                    & elementsv,neighborsv,deallocNode_ij=.true.,deallocNode_xy=.true.)
            end if

            optr => localTriTil
            call localTilings%add(optr)

            ! The following were allocated by bcast_varLen.
            ! However, the allocated pointers will be kept in localTilings.

            !if (pinfo%getRank() /= proc) then
            !    deallocate(listv)
            !    deallocate(lptrv)
            !    deallocate(elementsv)
            !    deallocate(neighborsv)
            !    deallocate(nodeiv)
            !    deallocate(nodejv)
            !    deallocate(nodexv)
            !    deallocate(nodeyv)
            !    deallocate(lendv)
            !end if
        end do

        allocate(globalTriTile)
        call globalTriTile%regularTriangularTilingConstructor(ngx,ngy,allNodeij,allNodexy)

        globalTiling => globalTriTile
    end subroutine

    function cloneRegularTriangularLatLonGrid(this,copyData) result(tgptr)
        implicit none

        class(RegularTriangularLatLonGrid)             :: this

        logical,              intent(in) :: copyData

        class(RegularTriangularLatLonGrid),   pointer    :: tgptr

        class(DataVariable), pointer :: xGlobal, yGlobal
        class(DataVariable), pointer :: xLocal,  yLocal
        class(DataVariable), pointer :: xGlobalClone, yGlobalClone
        class(DataVariable), pointer :: xLocalClone,  yLocalClone
        class(DataVariable), pointer :: ownerVar

        class(Tiling),       pointer :: thisGlobalTiling
        class(Tiling),       pointer :: otherGlobalTiling

        class(LinkedList),   pointer :: thisLocalTilings
        class(LinkedList),   pointer :: otherLocalTilings

        class(Tiling),       pointer :: localTiling

        class(*), pointer :: optr

        integer :: i, ndim

        xGlobal => this%getGlobalCoordinateVar(1)
        yGlobal => this%getGlobalCoordinateVar(2)
        xLocal  => this%getLocalCoordinateVar(1)
        yLocal  => this%getLocalCoordinateVar(2)

        if (associated(xGlobal)) then
            xGlobalClone => xGlobal%clone(copyData)
        end if

        if (associated(yGlobal)) then
            yGlobalClone => yGlobal%clone(copyData)
        end if

        xLocalClone => xLocal%clone(copyData)
        yLocalClone => yLocal%clone(copyData)

        ownerVar => this%getGlobalCoordOwnersVar()

        thisGlobalTiling => this%getGlobalTiling()
        otherGlobalTiling => otherGlobalTiling%clone(copyData)

        ! copy the local tilings, copying the data if copyData
        thisLocalTilings => this%getLocalTilings()
        otherLocalTilings => LinkedList()

        call thisLocalTilings%first()

        do i=1,thisLocalTilings%getListSize()
            optr => thisLocalTilings%currentValue()

            select type(optr)
                class is (Tiling)
                    ! cast down
                    localTiling => optr
                class default
                    call error('Unknown class in global coords list')
            end select

            ! reuse the optr pointer for the cloned tiling
            optr => localTiling%clone(copyData)

            call otherLocalTilings%add(optr)

            call thisLocalTilings%next()
        end do

        allocate(tgptr)

        call tgptr%regularTriangularLatLonGridConstructor(otherLocalTilings,otherGlobalTiling,&
            & xLocalClone, yLocalClone, xGlobalClone, yGlobalClone, &
            & ownerVar%clone(copyData), this%getDimsToTile())
    end function

    function clone(this,copyData) result(dgptr)
        implicit none

        class(RegularTriangularLatLonGrid)             :: this

        logical,               intent(in) :: copyData

        class(DataGrid),       pointer    :: dgptr

        class(RegularTriangularLatLonGrid), pointer    :: tgptr

        tgptr => this%cloneRegularTriangularLatLonGrid(copyData)

        dgptr => tgptr
    end function

    function getTriangleKey(triInd,triangles,node_ij,nx,doDebug,forceOutput) result(triKey)
        implicit none

        integer, intent(in) :: triInd
        integer, intent(in) :: triangles(:,:)
        integer, intent(in) :: node_ij(:,:)
        integer, intent(in) :: nx
        logical, optional, intent(in) :: doDebug
        logical, optional, intent(in) :: forceOutput

        character(DICT_KEY_LENGTH) :: triKey
        integer :: l
        integer :: i, j, ind, gind

        logical :: doPrint

        integer :: tri(3)

        if (present(doDebug)) then
            doPrint = doDebug
        else
            doPrint = .false.
        end if

        triKey = ''

        tri(:) = triangles(:,triInd)

        ! sort the indes so that all orders are considered the same
        call quicksort(tri,1,3)

        do l=1,3
            ind = tri(l)

            i = node_ij(1,ind)
            j = node_ij(2,ind)

            gind = i + (j-1)*nx

            if (doPrint) then
                write(msgstr,*) 'index',l,'(',ind,'/',size(node_ij,2),'):',i,j,'/',gind
                call print(msgstr,forceOutput=forceOutput)
            end if

            if (l > 1) then
                triKey = trim(triKey) // ','
            end if

            triKey = trim(triKey) // int2str(gind)
        end do
    end function

end module
