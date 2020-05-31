module triangularLatLonGrid_mod

    use iso_fortran_env

    use parallelInfo_mod

    use dataGrid_mod

    use linkedList_mod
    use dictionary_mod

    use tiling_mod
    use tripack_mod
    use triangularTiling_mod

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

    public :: TriangularLatLonGrid

    type, extends(DataGrid)   :: TriangularLatLonGrid
        private
            !real(real64), pointer :: allXYBounds(:,:,:)
            !real(real64), pointer :: allXYCenter(:,:)
            real(real64) :: latc
            real(real64) :: lonc
            integer      :: myproc

            integer,      allocatable :: localBounds(:,:)
            real(real64), allocatable ::    xyBounds(:,:)

            class(TriangularTiling), pointer :: localTriTil => NULL()

        contains
            procedure :: getDistanceToCenter
            procedure :: doTile
            procedure :: cloneTriangularLatLonGrid
            procedure :: clone

            generic   :: triangularLatLonGridConstructor => &
                         triangularLatLonGridConstructor_local, &
                         triangularLatLonGridConstructor_all

            procedure, private :: triangularLatLonGridConstructor_local
            procedure, private :: triangularLatLonGridConstructor_all

            final     :: triangularLatLonGridDestructor ! clean up all allocated variables
    end type

    contains

    subroutine triangularLatLonGridConstructor_local(this,xcv_local,ycv_local,dimsToTriangulate)

        implicit none

        class(TriangularLatLonGrid)         :: this

        class(ParallelInfo), pointer  :: pinfo
        class(DataVariable), pointer  :: xcv_local
        class(DataVariable), pointer  :: ycv_local
        integer,             optional :: dimsToTriangulate(2)

        call this%dataGridConstructor(xcv_local,ycv_local,dimsToTriangulate)
    end subroutine

    subroutine triangularLatLonGridConstructor_all(this,localTilings,globalTiling, &
        & xcv_local,ycv_local,xcv_global,ycv_global,globalCoordOwnersVar,dimsToTile)

        implicit none

        class(TriangularLatLonGrid)             :: this

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

    subroutine triangularLatLonGridDestructor(this)
        implicit none

        type(TriangularLatLonGrid)  :: this

        call this%dataGridDestructor()
    end subroutine

    subroutine getDistanceToCenter(this,ndim,nloci,lociVals,distances)
        implicit none

        class(TriangularLatLonGrid)      :: this

        integer,      intent(in)  :: ndim
        integer,      intent(in)  :: nloci
        real(real64), intent(in)  :: lociVals(ndim,nloci)
        !integer,      intent(out) :: nearestPts(nloci)
        real(real64), intent(out) :: distances(nloci)

        class(DataArray), pointer :: dArray

        integer,      pointer :: node_ij(:,:)
        real(real64), pointer :: node_xy(:,:)

        real(real64) :: baryCoords(3)
        integer      :: nodeIndices(3)

        real(real64) :: alpha
        real(real64) :: beta
        real(real64) :: gamma

        integer :: i, j, edge
        integer :: ind

        !class(LinkedList),       pointer :: localTilings
        !class(TriangularTiling), pointer :: localTriTil

        !real(real64) :: latd(nloci)
        !real(real64) :: lond(nloci)
        real(real64) :: londiff, latr, lonr, k
        !real(real64) :: minLatd, minLond, maxLatd, maxLond
        real(real64) :: lociValsP(nloci)

        real(real64) :: azi1, azi2

        real(real64) :: latcr
        real(real64) :: loncr

        integer :: oi, oj, nproc

        class(*), pointer :: optr

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

!        localTilings => this%getLocalTilings()
!
!        nproc = localTilings%getListSize()
!
!        call localTilings%first()
!
!        nearestPts(:) = -1
!
!        do i=1,nproc
!            optr => localTilings%currentValue()
!
!            select type(optr)
!
!                type is (TriangularTiling)
!                    localTriTil => optr
!                class default
!                    call error('Unknown type in localTilings.')
!            end select
!
!            latc = this%allXYCenter(1,i)
!            lonc = this%allXYCenter(2,i)
!            minLatd = this%allXYBounds(1,1,i)
!            minLond = this%allXYBounds(2,1,i)
!            maxLatd = this%allXYBounds(1,2,i)
!            maxLond = this%allXYBounds(2,2,i)
!            node_ij => localTriTil%getNodeLociIndices()
!            ngx     = localTriTil%getNFirstIndex()
!
!            latd(:) = lociVals(1,:)
!            lond(:) = lociVals(2,:)
!
!            do j=1,nloci
!                ! check
!                if (nearestPts(j) > 0) cycle
!
!                londiff = lond(j) - lonc/d2r
!
!                if (abs(londiff) > 180.d0) lond(j) = lond(j) - sign(360.d0,londiff)
!
!                if (minLatd <= latd(j) .and. latd(j) <= maxLatd .and. &
!                  & minLond <= lond(j) .and. lond(j) <= maxLond) then
!
!                    latr = d2r*latd(j)
!                    lonr = d2r*lond(j)
!
!                    k = 2.d0/(1.d0+sin(latc)*sin(latr)+cos(latc)*cos(latr)*cos(lonr-lonc))
!
!                    lociValsP(1) = k*cos(latr)*sin(lonr-lonc)
!                    lociValsP(2) = k*(cos(latc)*sin(latr) - sin(latc)*cos(latr)*cos(lonr-lonc))
!
!                    call localTriTil%search(lociValsP,nodeIndices,baryCoords,edge)
!
!                    ! check if the point is inside of the convex hull
!                    if (edge > 0) then
!                        alpha = baryCoords(1)
!                        beta  = baryCoords(2)
!                        gamma = baryCoords(3)
!
!                        ! take the largest coordinate, as that is the largest area of the triangle
!                        ! (see Barycentric coordinates wiki page, e.g.). For a equilateral triangle,
!                        ! that is also the closest point. For other "nice" triangles, the largest area is
!                        ! close to the closest vertex. Anyways it doesn't really matter, just need to be
!                        ! consistent.
!                        if (alpha >= beta .and. alpha >= gamma) then
!                            ind = nodeIndices(1)
!                        else if (beta >= alpha .and. beta >= gamma) then
!                            ind = nodeIndices(2)
!                        else
!                            ind = nodeIndices(3)
!                        end if
!
!                        oi = node_ij(1,ind)
!                        oj = node_ij(2,ind)
!
!                        nearestPts(j) = oi + (oj-1)*ngx
!                    end if
!                end if
!            end do
!
!            call localTilings%next()
!
!            print *,'finished proc ',i
!        end do
    end subroutine

    ! Note: This method is collective over pinfo%getCommunicator()
    !
    ! On entry into this method, the local and global coordinate vars have been set.
    ! The local coordinate var is the tiling region on this particular processor.
    ! The first two coordinate variables should be latitude (degrees) and longitude (degrees),
    ! in that order. The variables can be either real or double precision.
    subroutine doTile(this,pinfo,localTilings,globalTiling)
        implicit none

        class(TriangularLatLonGrid)               :: this

        class(ParallelInfo),              pointer  :: pinfo
        class(LinkedList),   intent(out), pointer  ::  localTilings
        class(Tiling),       intent(out), pointer  :: globalTiling

        class(TriangularTiling), pointer :: localTriTile
        class(TriangularTiling), pointer :: globalTriTile

        class(DataVariable), pointer :: latGlobal, lonGlobal
        class(DataVariable), pointer :: latLocal,  lonLocal

        class(DataShape), pointer :: latShape

        class(Dictionary), pointer :: triDict
        class(Dictionary), pointer :: nbrDict

        real(real32), pointer :: latptr_real(:,:)
        real(real32), pointer :: lonptr_real(:,:)
        real(real64), pointer :: latptr_dble(:,:)
        real(real64), pointer :: lonptr_dble(:,:)

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
        integer, parameter :: buffer  = 5
        integer, parameter :: buffer2 = 2
        integer :: numtot
        integer :: ierr

        integer,      dimension(:,:), pointer :: node_ij => NULL()
        real(real64), dimension(:,:), pointer :: node_xy => NULL()
        real(real64), dimension(:,:), pointer :: node_xpyp => NULL()

        real(real64) :: latc, lonc, lat, lon, k, x, y
        real(real64) :: latd, lond, latr, lonr, londiff
        real(real64) :: minLatd, minLond
        real(real64) :: maxLatd, maxLond

        integer :: i, j, ind

        integer, pointer :: lptr(:)
        integer, pointer :: lend(:)
        integer, pointer :: list(:)
        integer, pointer :: near(:)
        integer, pointer :: next(:)
        real(real64), pointer :: dist(:)
        integer :: lnew
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
        real(real64), pointer :: nodexpypv(:,:)
        integer          :: lnewv

        integer :: xs, xe
        integer :: ys, ye
        integer :: proc, indAll, lnewAll, loldAll

        logical :: nbrb, nodeb
        integer :: listFac

        integer :: node, lpl, lp, n1, na, nb, nt

        integer, pointer :: triangles(:,:)
        integer, pointer :: neighbors(:,:)
        integer, pointer :: triv(:,:)
        integer, pointer :: nbrv(:,:)
        integer, pointer :: allTriangles(:,:)
        integer, pointer :: allNeighbors(:,:)
        integer, pointer :: ltri(:,:)

        integer, dimension(:,:), pointer :: tri_i_node_ij

        character(DICT_KEY_LENGTH) :: triKey, nbrKey

        integer, parameter :: HASH_SIZE = 2895134 ! a prime number about 20% larger than largest estimated data set

        integer :: allTriInd, i1, j1, gind, l, m, loop, nbr, nodeInd, triInd

        logical :: allIn

        class(TriangularTiling), pointer :: localTriTil

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
        allocate(node_xpyp(2,nodenum))

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

                ! The lon center could be either
                ! a) Around -179 or something
                ! b) Around 179 or something
                ! In case a), find any points that are more than 180 degrees away
                ! Subtract 360 from those points (so 179 becomes -181)
                ! In case b), find any points thare are more than 180 degrees away
                ! Add 360 to those points (so -179 becomes 181)
                ! Later, whenever comparing to the min/max, check distance to center
                ! If more than 180 degrees away, add/subtract 360 to the offending points
                ! Case a) lonc -179, lond = 179
                ! londiff = lond - lonc = 358
                ! lond = lond - 1*360 = -181
                ! Case b) lonc 179, lond = -179
                ! londiff = lond - lonc = -358
                ! lond = lond + 360 = 181

                londiff = lond - lonc/d2r

                if (abs(londiff) > 180.d0) then
                    lond = lond - sign(360.d0,londiff)
                end if

                if (lond < minLond) minLond = lond
                if (latd < minLatd) minLatd = latd
                if (lond > maxLond) maxLond = lond
                if (latd > maxLatd) maxLatd = latd

                latr = d2r*latd
                lonr = d2r*lond

                k   = 2.d0/(1.d0+sin(latc)*sin(latr)+cos(latc)*cos(latr)*cos(lonr-lonc))
                x   = k*cos(latr)*sin(lonr-lonc)
                y   = k*(cos(latc)*sin(latr) - sin(latc)*cos(latr)*cos(lonr-lonc))

                node_xy(1,ind)   = latd
                node_xy(2,ind)   = lond
                node_xpyp(1,ind) = x
                node_xpyp(2,ind) = y
            end do
        end do

        nit = 0
        nlist = 6*nodenum
        allocate(list(nlist))
        allocate(lptr(nlist))
        allocate(lend(nodenum))

        !call trmshr ( nbx*nby, nbx, node_xpyp(1,:), node_xpyp(2,:), nit, &
        !    & list, lptr, lend, lnew, ier )
        allocate(near(nodenum))
        allocate(next(nodenum))
        allocate(dist(nodenum))

        call trmesh( nodenum, node_xpyp(1,:), node_xpyp(2,:), list, lptr, lend, lnew, &
            & near, next, dist, ier )

        deallocate(near)
        deallocate(next)
        deallocate(dist)

        if (ier /= 0) then
            !call error('trmshr returned a non-zero error: ' // int2str(ier) // ', nit: ' // int2str(nit))
            call error('trmesh returned a non-zero error: ' // int2str(ier))
        end if

        allocate(lcc(1))
        allocate(lct(1))
        allocate(ltri(6,2*nbx*nby))

        call trlist ( 0, lcc, nbx*nby, list, lptr, lend, 6, nt, ltri, lct, ier )

        if (ier /= 0) then
            call error('trlist returned a non-zero error: ' // int2str(ier))
        end if

        allocate(triangles(3,nt))
        allocate(neighbors(3,nt))

        triangles(:,:) = ltri(1:3,1:nt)
        neighbors(:,:) = ltri(4:6,1:nt)

        deallocate(ltri)

        !allocate(this%allXYBounds(2,2,pinfo%getCommSize()))
        !allocate(this%allXYCenter(2,pinfo%getCommSize()))
        allocate(allList(6*ngx*ngy))
        allocate(allLptr(6*ngx*ngy))
        allocate(allLend(ngx*ngy))
        allocate(allNodeij(2,ngx*ngy))
        allocate(allNodexy(2,ngx*ngy))

        indAll  = 0
        lnewAll = 1
        loldAll = 1

        allocate(this%localBounds(5,pinfo%getCommSize()))
        allocate(this%xyBounds(4,pinfo%getCommSize()))

        do proc=0,pinfo%getCommSize()-1
            if (pinfo%getRank() == proc) then
                ! store the proc (1-indexed) for later to aid access to localBounds
                this%myproc = proc+1

                listv      => list
                lptrv      => lptr
                nodeijv    => node_ij
                nodexyv    => node_xy
                nodexpypv  => node_xpyp
                lendv      => lend
                elementsv  => triangles
                neighborsv => neighbors

                this%localBounds(X_START,this%myproc) = xls
                this%localBounds(X_END,  this%myproc) = xle
                this%localBounds(Y_START,this%myproc) = yls
                this%localBounds(Y_END,  this%myproc) = yle
                this%localBounds(LSIZE,  this%myproc) = lnew

                this%   xyBounds(MIN_LAT,this%myproc) = minLatd
                this%   xyBounds(MIN_LON,this%myproc) = minLond
                this%   xyBounds(MAX_LAT,this%myproc) = maxLatd
                this%   xyBounds(MAX_LON,this%myproc) = maxLond

                this%latc = latc/d2r
                this%lonc = lonc/d2r
                !this%allXYCenter(1,proc+1) = latc
                !this%allXYCenter(2,proc+1) = lonc
                !this%allXYCenter(1,proc+1) = latc
                !this%allXYCenter(2,proc+1) = lonc
                !this%allXYBounds(1,1,proc+1) = minLatd
                !this%allXYBounds(2,1,proc+1) = minLond
                !this%allXYBounds(1,2,proc+1) = maxLatd
                !this%allXYBounds(2,2,proc+1) = maxLond
            end if

            call bcast1d_varlen(listv, proc,pinfo%getCommunicator(),'bcasting the list from rank ' // &
                & int2str(proc))
            call bcast1d_varlen(lptrv, proc,pinfo%getCommunicator(),'bcasting the lptr from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(elementsv,proc,pinfo%getCommunicator(),'bcasting the nodei from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(neighborsv,proc,pinfo%getCommunicator(),'bcasting the nodei from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(nodeijv,proc,pinfo%getCommunicator(),'bcasting the nodei from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(nodexyv, proc,pinfo%getCommunicator(),'bcasting the nodexy from rank ' // &
                & int2str(proc))
            call bcastnd_varlen(nodexpypv, proc,pinfo%getCommunicator(),'bcasting the nodexpyp from rank ' // &
                & int2str(proc))
            call bcast1d_varlen(lendv, proc,pinfo%getCommunicator(),'bcasting the lend from rank ' // &
                & int2str(proc))
            call bcast1d(this%localBounds(:,proc+1),5,proc,pinfo%getCommunicator(),'bcasting the localBounds from rank ' // &
                & int2str(proc))
            call bcast1d(this%xyBounds(:,proc+1),4,proc,pinfo%getCommunicator(),'bcasting the xyBounds from rank ' // &
                & int2str(proc))
            !call bcast1d(this%allXYCenter(:,proc+1),2,proc,pinfo%getCommunicator(),'bcasting the xc from rank ' // &
            !    & int2str(proc))
            !call bcastnd(this%allXYBounds(:,:,proc+1), (/2,2/),proc,pinfo%getCommunicator(),'bcasting the xybounds from rank ' // &
            !    & int2str(proc))

            xs = this%localBounds(X_START,proc+1)
            xe = this%localBounds(X_END,proc+1)
            ys = this%localBounds(Y_START,proc+1)
            ye = this%localBounds(Y_END,proc+1)
            lnewv = this%localBounds(LSIZE,proc+1)

            allocate(localTriTil)
            call localTriTil%triangularTilingConstructor(size(nodeijv,2),nodeijv,nodexpypv, &
                & elementsv,neighborsv,size(elementsv,2),listv,lptrv,lendv,lnewv,             &
                & deallocNode_ij=.true.,deallocNode_xy=.true.)

            if (pinfo%getRank() == proc) then
                this%localTriTil => localTriTil
            end if

            optr => localTriTil
            call localTilings%add(optr)

            ! now all processes will add this to their global tiling
            do node=1,size(nodeijv,2)
                ! check if this node is in our region
                if (nodeijv(1,node) < xs .or. nodeijv(1,node) > xe .or. &
                   &nodeijv(2,node) < ys .or. nodeijv(2,node) > ye) then
                    cycle
                end if

                indAll = indAll + 1

                allNodeij(:,indAll) = nodeijv(:,node)
                allNodexy(:,indAll) = nodexyv(:,node)

                if (nodeijv(1,node) == xgs .or. nodeijv(1,node) == xge .or. &
                    nodeijv(2,node) == ygs .or. nodeijv(2,node) == yge) then
                    nodeb = .true.
                else
                    nodeb = .false.
                end if

                ! for each node, loop through all of the neighbors and add them
                lpl = lendv(node)
                lp  = lpl

                n1 = abs ( listv(lp) )

                ! get the pointer that will be used for the last member to wrap around
                loldAll = lnewAll

                ! fprintf('The proc #%d last neighbor of %d is: %d\n',proc,node,lpl)
                ! j = 0

                do while (.true.)
                    lp = lptrv(lp)
                    n1 = abs ( listv(lp) )

                    if (nodeijv(1,n1) == xgs .or. nodeijv(1,n1) == xge .or. &
                        nodeijv(2,n1) == ygs .or. nodeijv(2,n1) == yge) then

                        nbrb = .true.
                    else
                        nbrb = .false.
                    end if

                    ! when trying to connect two boundary points - only do so if
                    ! both the x and y values are 0 or 1 apart; otherwise it's an
                    ! exterior connection of an exterior triangle (bad)
!                    if (nodeb .and. nbrb .and. ((abs(nodeijv(2,n1) - nodeijv(2,node)) > 1) .or. &
!                        (abs(nodeijv(1,n1) - nodeijv(1,node)) > 1))) then
!
!                        !fprintf('Skipping proc %d node %d (%d/%d) and (%d/%d)\n',proc,node,&
!                        !    nodeijv(1,node),nodeijv(2,node),nodeijv(1,n1),nodeijv(2,n1))
!                        if (lp == lpl) then
!                            allLend(indAll) = lnewAll-1
!                            allLptr(lnewAll-1) = loldAll
!                            allList(lnewAll-1) = listFac*allList(lnewAll-1)
!                            !fprintf('Stopping proc %d node %d (%d/%d) at #%d\n',proc,node,&
!                            !    nodeijv(1,node),nodeijv(2,node),j)
!                            exit
!                        end if
!                    else
                        if (nbrb  .and. nodeb) then
                            listFac = -1
                        else
                            listFac = 1
                        end if

                        lnewAll = lnewAll + 1

                        if (lp == lpl) then
                            allList(lnewAll-1) = listFac*(nodeijv(1,n1) + (nodeijv(2,n1)-1)*ngx)
                            allLend(indAll)    = lnewAll-1
                            allLptr(lnewAll-1) = loldAll
                            !fprintf('Stopping proc %d node %d (%d/%d) at #%d\n',proc,node,&
                            !    nodeijv(1,node),nodeijv(2,node),j)
                            exit
                        else
                            allList(lnewAll-1) = nodeijv(1,n1) + (nodeijv(2,n1)-1)*ngx
                            allLptr(lnewAll-1) = lnewAll
                            !fprintf('The proc %d #%d neighbor of node (%d/%d) is: (%d/%d)\n',proc,j,&
                            !    nodeijv(1,node),nodeijv(2,node),nodeijv(1,n1),nodeijv(2,n1))
                        end if
!                    end if
                end do
            end do

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

        ! Now we loop through the list of tilings

        ! first reduce the number of triangles across all processors
        ! Note this is a maximum, as some are redundant and will not be included
        call MPI_AllReduce(nt,numtot,1,MPI_INTEGER,MPI_SUM,pinfo%getCommunicator(),ierr)

        allocate(allTriangles(3,numtot))
        allocate(allNeighbors(3,numtot))

        triDict => Dictionary(HASH_SIZE)
        nbrDict => Dictionary(HASH_SIZE)

        ! loop through this process twice - once for the triangles, then for the neighbors
        ! this is because the neighbors need to have all of the triangles ready before proceeding
        do loop=1,2
            ! counter for the global triangles
            allTriInd = 0

            call localTilings%first()

            ! we will send the triangles from each process
            do i=0,pinfo%getCommSize()-1
                optr => localTilings%currentValue()

                ! cast down to the tiling
                select type(optr)
                    type is (TriangularTiling)
                        localTriTil => optr
                    class default
                        call error('Unknown type in localTilings.')
                end select

                call localTilings%next()

                triv    => localTriTil%getElementIndices()
                tri_i_node_ij => localTriTil%getNodeLociIndices()

                if (loop == 2) then
                    nbrv => localTriTil%getElementNeighbors()
                end if

                ! and pull the data out
                xis = this%localBounds(1,i+1)
                xie = this%localBounds(2,i+1)
                yis = this%localBounds(3,i+1)
                yie = this%localBounds(4,i+1)

                nix = xie-xie+1
                niy = yie-yis+1

                ! now set the second (smaller) buffer region
                ! any triangles inside here are considered good enough to add
                ! the triangles outside will be discarded for now (but added later)
                xbs = max(xgs,xis-buffer2)
                xbe = min(xge,xie+buffer2)
                ybs = max(ygs,yis-buffer2)
                ybe = min(yge,yie+buffer2)

                ! now loop through all of the triangles for process i
                do j=1,size(triv,2)
                    allIn = .true.

                    ! make sure the triangle has all points inside the buffer region
                    do l=1,3
                        ind = triv(l,j)

                        iv(l) = tri_i_node_ij(1,ind)
                        jv(l) = tri_i_node_ij(2,ind)

                        if (iv(l) < xbs .or. iv(l) > xbe .and. &
                            jv(l) < ybs .or. jv(l) > ybe) then
                            allIn = .false.
                        end if
                    end do

                    !if (.not. allIn) then
                    !    ! not all of the indexes are in the buffer region so the triangle is suspect
                    !    cycle ! the j loop
                    !end if

                    ! also exclude any purely boundary triangles that fill in the convex hull
                    !if(all(iv == xgs) .or. all(iv == xge) .or. &
                    !   all(jv == ygs) .or. all(jv == yge)) then
                    !    cycle ! the j loop
                    !end if

                    ! get the comma-separated key of all of the nodes
                    triKey = getTriangleKey(j,triv,tri_i_node_ij,ngx)

                    ! check if this key has already been added to the triDict or nbrDict
                    if (loop == 1) then
                        if (triDict%hasKey(triKey)) then
                            cycle
                        end if
                    else
                        if (nbrDict%hasKey(triKey)) then
                            cycle
                        end if
                    end if

                    ! we now have a good triangle to add to the global list (tri or nbr)
                    allTriInd = allTriInd + 1

                    if (loop == 1) then
                        ! prepare to add this triangle to allTriangles
                        do l=1,3
                            ! move the local node indexes to global node indexes
                            gind = iv(l) + (jv(l)-1)*ngx

                            allTriangles(l,allTriInd) = gind
                        end do

                        allocate(tdat)
                        tdat%triInd = allTriInd
                        optr => tdat
                        call triDict%add(triKey,optr)
                    else
                        ! need to find all the neighbors of the triangle

                        do l=1,3
                            nbr = nbrv(l,j)

                            if (nbr <= 0) then
                                ! this is a point on the boundary
                                allNeighbors(l,allTriInd) = 0
                                cycle ! the l=1,3 loop
                            end if

                            if (nbr > 0) then
                                ! if positive, this is a (local) triangle index
                                ! need to convert it to the global triangle index
                                triInd = nbr
                            else
                                ! if negative, this is a point on the convex hull
                                ! in this case, LINK = -(3*I + J-1) where I, J = triangle, edge index
                                ! need to convert I here to the global index
                                nodeInd = mod(-nbr,3)+1
                                triInd  = (-nbr-nodeInd+1)/3
                            end if

    !                        if (triInd == 0) then
    !                            ! nbr should not be zero
    !
    !                            call print('Could not find the triangle corresponding to the key ' // trim(nbrKey) // &
    !                                & ' in the dictionary of triangles: ' // int2str(nbr))
    !
    !                            call print('triangle:')
    !
    !                            nbrKey = getTriangleKey(j,triv,tri_i_node_ij,ngx,doDebug=.true.)
    !
    !                            allNeighbors(l,allTriInd) = 0
    !                            cycle ! the l loop
    !                        end if

                            ! first get the triangle key
                            nbrKey = getTriangleKey(triInd,triv,tri_i_node_ij,ngx)

                            ! check if we have the key in the triDict, otherwise error
                            if (.not. triDict%hasKey(nbrKey)) then
                                allNeighbors(l,allTriInd) = 0

                                cycle

    !                            do m=1,3
    !                                ind = triv(m,triInd)
    !
    !                                iv(m) = tri_i_node_ij(1,ind)
    !                                jv(m) = tri_i_node_ij(2,ind)
    !                            end do
    !
    !                            if(all(iv == xgs) .or. all(iv == xge) .or. &
    !                               all(jv == ygs) .or. all(jv == yge)) then
    !                                allNeighbors(l,allTriInd) = 0
    !                                cycle ! the l loop
    !                            end if
    !
    !                            call print('Could not find the triangle corresponding to the key ' // trim(nbrKey) // &
    !                                & ' in the dictionary of triangles: ' // int2str(nbr))
    !
    !                            call print('triangle:')
    !
    !                            nbrKey = getTriangleKey(j,triv,tri_i_node_ij,ngx,doDebug=.true.)
    !
    !                            call print('neighbor:')
    !
    !                            nbrKey = getTriangleKey(triInd,triv,tri_i_node_ij,ngx,doDebug=.true.)
    !
    !                            allNeighbors(l,allTriInd) = 0
    !
    !                            cycle
                            end if

                            ! now look-up the key to get the triData wrapper
                            optr => triDict%get(nbrKey)

                            ! cast down to the correct type
                            select type(optr)
                                type is (TriData)
                                    tdat => optr
                                class default
                                    call error('Unknown type in triDict.')
                            end select

                            if (nbr > 0) then
                                ! now set the global triangle index from the triData
                                allNeighbors(l,allTriInd) = tdat%triInd
                            else
                                ! now set the (negative) global link index from the triData
                                allNeighbors(l,allTriInd) = -(3*tdat%triInd + nodeInd - 1)
                            end if
                        end do

                        allocate(tdat)
                        tdat%triInd = allTriInd
                        optr => tdat
                        call nbrDict%add(triKey,optr)
                    end if
                end do ! the "j = 1,numtri" loop

                ! if (i /= pinfo%getRank()) then
                !    deallocate(triv)
                !    deallocate(tri_i_node_ij)
                !
                !    if (loop == 2) then
                !        deallocate(nbrv)
                !    end if
                ! end if
            end do ! the "i = 0,comm_size-1" loop
        end do ! the "loop = 1,2" loop

        if (pinfo%getRank() == 0) then
            open(unit=42,file="node_xy.txt",status="replace")

            write(42,*) ngx
            write(42,*) ngy

            ind = 0

            do j=1,ngy
                do i=1,ngx
                    ind = ind + 1
                    write(42,*) allNodeij(1,ind),allNodeij(2,ind),&
                        allNodexy(1,ind),allNodexy(2,ind)
                end do
            end do
            close(unit=42)

            open(unit=42,file="allTriangles.txt",status="replace")

            write(42,*) numtot

            do j=1,numtot
                do i=1,3
                    write(42,*) i,j,allTriangles(i,j)
                end do
            end do
            close(unit=42)

            call abortParallel()
        end if

        allocate(globalTriTile)
        call globalTriTile%triangularTilingConstructor(size(allNodeij,2),allNodeij,allNodexy, &
            & allTriangles,allNeighbors,allTriInd,allList,allLptr,allLend,lnewAll,     &
            & deallocNode_ij=.true.,deallocNode_xy=.true.)

        globalTiling => globalTriTile

        deallocate(triDict)
        deallocate(nbrDict)
    end subroutine

    function cloneTriangularLatLonGrid(this,copyData) result(tgptr)
        implicit none

        class(TriangularLatLonGrid)             :: this

        logical,              intent(in) :: copyData

        class(TriangularLatLonGrid),   pointer    :: tgptr

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

        call tgptr%triangularLatLonGridConstructor(otherLocalTilings,otherGlobalTiling,&
            & xLocalClone, yLocalClone, xGlobalClone, yGlobalClone, &
            & ownerVar%clone(copyData), this%getDimsToTile())
    end function

    function clone(this,copyData) result(dgptr)
        implicit none

        class(TriangularLatLonGrid)             :: this

        logical,               intent(in) :: copyData

        class(DataGrid),       pointer    :: dgptr

        class(TriangularLatLonGrid), pointer    :: tgptr

        tgptr => this%cloneTriangularLatLonGrid(copyData)

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

        ! sort the indexes so that all orders are considered the same
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
