module conicalScanningObsOperator_mod

    use parallelInfo_mod

    use iso_fortran_env

    use scannedObservation_mod
    use scannedObservationOperator_mod

    use petscMat
    use petscVec

    use linkedList_mod

    use triangularTiling_mod

    use dataSet_mod
    use dataGrid_mod
    use dataType_mod
    use dataExtent_mod
    use dataVariable_mod

    use observation_mod
    use observationOperator_mod
    use obsQcCodes_mod

    use satelliteObservation_mod
    use satellitePlatformInfo_mod
    use conicalScanningObservation_mod

    use tiling_mod

    use geodesic_mod
    use quadUtils_mod

    use mpiUtils_mod

    implicit none

#include <petsc/finclude/petscsys.h>

    private

    ! number of integration points in time for the time sweep of the instantaneous antenna pattern
    integer, parameter :: NT_POINTS = 5
    ! integration factor to convert from fov (in half power) to standard deviation of a Gaussian
    real(real64), parameter :: SIGMA_FACTOR = 2.d0*sqrt(2.d0*log(2.d0))
    ! everyone's favorite constant
    real(real64), parameter :: PI = 4.d0*datan(1.d0)
    ! convert from degrees to radians
    real(real64), parameter :: DTOR = PI/180.d0
    ! convert from radians to degrees
    real(real64), parameter :: RTOD = 180.d0/PI
    ! minimum percentage of "good" (not out of domain, etc.) points to allow a scan
    real(real64), parameter :: DEFAULT_MIN_GOOD_RATIO = 0.7

    ! approximate the number of km per degree, neglecting latitude dependency
    real(real64), parameter :: earthRadius = 6370d0
    real(real64), parameter :: kmToDegrees = 360.d0/2.0d0/3.141593d0/earthRadius

    public :: ConicalScanningObsOperator

    type, extends(ScannedObservationOperator) :: ConicalScanningObsOperator
        !private
            real(real64), dimension(NT_POINTS)  :: tg
            real(real64), dimension(NT_POINTS)  :: wt
            class(DataGrid),            pointer :: inputGrid
            integer, dimension(:),      pointer :: stateTriIndices => NULL()
            real(real64), dimension(:), pointer :: maxdistsqr      => NULL()
            real(real64), dimension(:), pointer :: xkvals          => NULL()
            real(real64), dimension(:), pointer :: ykvals          => NULL()
            real(real64), dimension(:), pointer :: fkvals          => NULL()
            integer,      dimension(:), pointer :: usedInd         => NULL()
            integer,      dimension(:), pointer :: offsets         => NULL()
            class(DataVariable),        pointer :: indVarGlobal    => NULL()
            integer                             :: myoffset
            integer                             :: lastindex

        contains
            procedure :: conicalScanningObsOperatorConstructor
            !procedure :: conicalScanningObsOperatorConstructor_load

            procedure :: getName
            procedure :: getMaxStateIndices
            procedure :: getRowValues
            procedure :: getNumberOfNonzeros
            procedure :: isSameOperator

            procedure, private :: getMinMaxLatLons
            procedure, private :: getRowData

            procedure, private :: doLoad

            final :: conicalScanningObsOperatorDestructor ! clean up all allocated variables
    end type

    contains

    subroutine conicalScanningObsOperatorConstructor(this,pinfo,obs,inputGrid, &
        & minGoodRatio,columnNormsVar)

        implicit none

        class(ConicalScanningObsOperator)             :: this

        class(ParallelInfo),               pointer    :: pinfo

        class(ConicalScanningObservation), pointer    :: obs
        class(DataGrid),                   pointer    :: inputGrid
        real(real64),            optional, intent(in) :: minGoodRatio
        class(DataVariable),     optional, pointer    :: columnNormsVar

        class(ScannedObservation),         pointer    :: obs_so

        integer, allocatable :: domainSizes(:)

        class(DataVariable), pointer :: xCoord
        class(DataVariable), pointer :: indVarLocal

        class(DataExtent), pointer :: xExtent
        class(DataExtent), pointer :: yExtent

        integer, pointer :: linds(:,:)

        integer :: i, ind, j
        integer :: nx_local, ny_local

        call this%doLoad(obs,inputGrid)

        xExtent => inputGrid%getLocalCoordinateExtent(1)
        yExtent => inputGrid%getLocalCoordinateExtent(2)

        nx_local = xExtent%getLocalCount()
        ny_local = yExtent%getLocalCount()

        allocate(domainSizes(pinfo%getCommSize()))
        do i=1,pinfo%getCommSize()
            if (i-1 == pinfo%getRank()) then
                domainSizes(i) = nx_local*ny_local
            end if
            call bcast0d(domainSizes(i),i-1,pinfo%getCommunicator(),'sharing rank sizes')
        end do

        allocate(this%offsets(pinfo%getCommSize()))
        do i=1,pinfo%getCommSize()
            if (i == 1) then
                this%offsets(i) = 0
            else
                this%offsets(i) = this%offsets(i-1) + domainSizes(i-1)
            end if
        end do

        this%myoffset  = this%offsets(pinfo%getRank()+1)
        this%lastIndex = sum(domainSizes(1:pinfo%getRank()+1)) - 1

        xCoord => inputGrid%getLocalCoordinateVar(1)

        allocate(indVarLocal)
        call indVarLocal%dataVariableConstructor('PETSC_INDEX_LOCAL',INT_TYPE_NUM,&
            & xCoord%getDataShape(),collective=.true.,distributed=.true.)

        call indVarLocal%getArray(linds)

        ind = 0
        do j=1,ny_local
            do i=1,nx_local
                linds(i,j) = ind + this%myoffset
                ind = ind + 1
            end do
        end do

        this%indVarGlobal => indVarLocal%gatherToGlobal(pinfo,'PETSC_INDEX_GLOBAL')

        obs_so => obs

        call this%scannedObservationOperatorConstructor(pinfo, obs_so, inputGrid, &
            & minGoodRatio,columnNormsVar)
    end subroutine

    subroutine doLoad(this, obs, inputGrid)

        implicit none

        class(ConicalScanningObsOperator)          :: this
        class(ConicalScanningObservation), pointer :: obs
        class(DataGrid),                   pointer :: inputGrid

        integer :: totalNz

        integer :: i, ind, j, nprofile

        integer :: nodeNum
        integer :: elementNum
        integer :: nloc

        real(real64), dimension(:,:),   pointer :: node_xy_obs

        real(real64), dimension(:,:),   pointer :: obsLoci

        real(real64), dimension(:,:),   pointer :: latptr, lonptr

        class(Tiling),  pointer :: stateTri

        integer, pointer :: triangle(:,:)

        call getGaussLegendreQuadPts(NT_POINTS,-0.5d0,0.5d0,this%tg,this%wt)

        stateTri => inputGrid%getGlobalTiling()

        triangle => stateTri%getElementIndices()

        allocate(this%stateTriIndices(size(triangle,2)))
    end subroutine

    subroutine conicalScanningObsOperatorDestructor(this)
        implicit none

        type(ConicalScanningObsOperator)  :: this

        integer :: i

        if (associated(this%inputGrid)) then
            deallocate(this%inputGrid)
        end if

        if (associated(this%stateTriIndices)) then
            deallocate(this%stateTriIndices)
        end if

        if (associated(this%maxdistsqr)) then
            deallocate(this%maxdistsqr)
        end if

        if (associated(this%xkvals)) then
            deallocate(this%xkvals)
        end if

        if (associated(this%ykvals)) then
            deallocate(this%ykvals)
        end if

        if (associated(this%fkvals)) then
            deallocate(this%fkvals)
        end if

        if (associated(this%usedInd)) then
            deallocate(this%usedInd)
        end if

        ! note that node_xy is deallocated in tri
    end subroutine

    function getName(this) result(name)
        implicit none

        class(ConicalScanningObsOperator) :: this
        character(128) :: name

        name = 'ConicalScanningObsOperator'
    end function

    subroutine getMinMaxLatLons(this, pinfo, obs_cs, inputGrid, &
        & minLat, maxLat, minLon, maxLon)

        implicit none

        class(ConicalScanningObsOperator)              :: this

        class(ParallelInfo),               pointer     :: pinfo
        class(ConicalScanningObservation), pointer     :: obs_cs
        class(DataGrid),                   pointer     :: inputGrid
        real(real64), dimension(:,:),      intent(out) :: minLat, maxLat, minLon, maxLon

        class(DataExtent), pointer :: xExtent
        class(DataExtent), pointer :: yExtent

        real(real64), dimension(:,:), pointer :: obsLoci

        class(Tiling), pointer :: stateTri

        real(real64) :: thetap
        real(real64) :: lat, lon, lat2, lon2, scLat, scLon
        real(real64) :: arclen, arclen2, angle1, angle2, angle3

        integer :: chind
        integer :: i, il, iz, itime, k
        integer :: scanNum, pixelNum
        integer :: xs, xe, nx_local
        integer :: ys, ye, ny_local

        real(real64) :: distsqr, latdist, londist, coslat

        integer, pointer :: owners(:)

        real(real32), pointer :: scLatPtr(:)
        real(real32), pointer :: scLonPtr(:)

        real(real64), pointer :: node_xy(:,:)

        real(real64) :: maxdist

        minLat =  999
        maxLat = -999
        minLon =  999
        maxLon = -999

        obsLoci => obs_cs%getObsLoci()
        owners  => obs_cs%getObsOwners()

        scLatPtr => obs_cs%getCraftLatitudes()
        scLonPtr => obs_cs%getCraftLongitudes()

        stateTri => inputGrid%getGlobalTiling()

        node_xy => stateTri%getNodeLoci()

        il = 0

        ! for performance, here we need to loop through the list of observations and find the
        ! minimum and maximum lat/lon combos. Then we need to loop through the list of node
        ! points and find all nodes that can potentially be affected by any observation in the
        ! list, which will be the only points investigated below. "potentially affected" means
        ! considering the largest maxdistsqr of any channel, then multiplying by some factor
        ! to account for the antenna pattern scan.

        ! do a quick run through the obs to approximate the min/max lat/lon
        do i=1,size(obsLoci,2)
            if (owners(i) /= pinfo%getRank()) then
                cycle
            end if
            il = il+1
            lat = obsLoci(SO_LAT_DIM,i)
            lon = obsLoci(SO_LON_DIM,i)

            pixelNum = anint(obsLoci(SO_PIX_DIM,i))
            scanNum  = anint(obsLoci(SO_SCAN_DIM,i))

            scLat = dble(scLatPtr(scanNum))
            scLon = dble(scLonPtr(scanNum))

            call geodesic_inverse(scLat,scLon,lat,lon,arclen,angle1,angle2,1)

            do chind=1,this%nposting_channels
                ! total angular offset in degrees
                ! (360 degrees, 1000 ms per second, 60 seconds per minute)
                thetap = 360.d0*obs_cs%getIntegrationTime(chind)*obs_cs%spinFrequency/(1000.d0*60.d0)

                this%usedInd = 0

                maxdist = sqrt(this%maxdistsqr(chind))

                ! go through the points within the time integral
                do itime = 1,NT_POINTS
                    angle3 = angle1 + thetap*this%tg(itime)
                    ! compute the center of the instantaneous scan
                    call geodesic_direct(scLat,scLon,angle3,arclen,lat2,lon2,angle2,1)

                    ! if this instantaneous scan is out of the domain, disqualify it
!                    call state_gtb%convertLatLonToIJ(lat2,lon2,x2,y2)

!                    if (x2 .lt. 1 .or. y2 .lt. 1 .or. x2 .gt. nx .or. y2 .gt. ny) then
!                        call obs_cs%setQCCode(chind, i, QC_UPDT_FWDOP)
!                        exit ! the loop
!                    end if

                    coslat = max(cos(DTOR*lat2),0.1d0)
                    !latw = stateTri%wrapX(lat2)
                    !lonw = stateTri%wrapY(lon2)
                    minLat(il,chind) = min(minLat(il,chind),lat2-maxdist)
                    maxLat(il,chind) = max(maxLat(il,chind),lat2+maxdist)
                    minLon(il,chind) = min(minLon(il,chind),lon2-maxdist*coslat)
                    maxLon(il,chind) = max(maxLon(il,chind),lon2+maxdist*coslat)

                    ! run through all the nodes and see which points are within range
                    !do k=1,size(node_xy,2)
                    !    latdist = node_xy(1,k) - lat2
                    !    londist = (node_xy(2,k) - lon2)
                    !    !londist = londist - 360.d0*anint(londist/360.d0)
                    !
                    !    if (londist > 180.d0) then
                    !        londist = 360.d0 - londist
                    !    else if (londist < -180.d0) then
                    !        londist = londist + 360.d0
                    !    end if
                    !
                    !    distsqr = latdist*latdist + londist*londist*coslat*coslat
                    !
                    !    ! update usedInd if in range
                    !    if (distsqr <= this%maxdistsqr(chind)) then
                    !        this%usedInd(k) = 1
                    !    end if
                    !end do
                end do

                ! now run through again and update nnzs and min/max lat/lon
                !do k=1,size(node_xy,2)
                !    if (this%usedInd(k) == 1) then
                !        ! update min/max lat/lons as well
                !        minLat(il,chind) = min(minLat(il,chind),node_xy(1,k))
                !        maxLat(il,chind) = max(maxLat(il,chind),node_xy(1,k))
                !        minLon(il,chind) = min(minLon(il,chind),node_xy(2,k))
                !        maxLon(il,chind) = max(maxLon(il,chind),node_xy(2,k))
                !    end if
                !end do
            end do
        end do
    end subroutine

    subroutine getRowData(this,pinfo,doCount,obs_so,inputGrid,obsnum,il,chind,scLat,scLon,&
        & lat,lon,arclen,angle,minLat,maxLat,minLon,maxLon,d_nnz,o_nnz,rowSum,nind,xinds, &
        & fvals)

        implicit none

        class(ConicalScanningObsOperator)       :: this

        class(ParallelInfo),        pointer     :: pinfo
        logical,                    intent(in)  :: doCount
        class(ScannedObservation),  pointer     :: obs_so
        class(DataGrid),            pointer     :: inputGrid
        integer,                    intent(in)  :: obsnum
        PetscInt,                   intent(in)  :: il
        integer,                    intent(in)  :: chind
        real(real64),               intent(in)  :: scLat
        real(real64),               intent(in)  :: scLon
        real(real64),               intent(in)  :: lat
        real(real64),               intent(in)  :: lon
        real(real64),               intent(in)  :: arclen
        real(real64),               intent(in)  :: angle
        real(real64),               intent(in)  :: minLat
        real(real64),               intent(in)  :: maxLat
        real(real64),               intent(in)  :: minLon
        real(real64),               intent(in)  :: maxLon
        PetscInt,         optional, intent(out) :: d_nnz
        PetscInt,         optional, intent(out) :: o_nnz
        real(real64),     optional, intent(out) :: rowSum
        PetscInt,         optional, intent(out) :: nind
        PetscInt,         optional, intent(out) :: xinds(:)
        real(real64),     optional, intent(out) :: fvals(:)

        class(ConicalScanningObservation), pointer :: obs

        real(real64) :: an, anglek1, anglek2, anglek3, areak, ax, ay, bn, bx, by, costheta, f1, f2, f3
        real(real64) :: lat2, lon2, latk1, latk2, latk3, lonk1, lonk2, lonk3
        real(real64) :: xf, xk1, xk2, xk3, yf, yk1, yk2, yk3, zf

        real(real64) :: thetap, sang, cang, latw, lonw
        real(real64) :: angle2, angle3, angle4, arclen2, arclen3, arclen4
        real(real64) :: fval, distsqr, latdist, londist, coslat, earthradius, kmtodegrees

        integer :: i, ind1, ind2, ind3, iz, itime, k, ki, kj, l, numtri, triInd
        integer :: i1, i2, i3
        integer :: j1, j2, j3

        real(real64) :: acostheta

        integer :: nx_global, ny_global

        integer, pointer :: ginds(:,:)

        real(real64), dimension(2,3) :: xyi
        logical,      dimension(3)   :: inside

        class(Tiling), pointer :: stateTri

        real(real64) :: minAngle
        real(real64) :: maxAngle

        integer, pointer :: triangle(:,:)
        integer, pointer :: node_ij(:,:)
        real(real64), pointer :: node_xy(:,:)

        integer :: rejected

        minAngle = 5.0d0*DTOR
        maxAngle = 175.d0*DTOR

        if (doCount) then
            if (.not. present(d_nnz) .or. .not. present(o_nnz)) then
                call error('Both d_nnz and o_nnz must be present for doCount = .true.')
            end if
        else
            if (.not. present(rowSum) .or. .not. present(nind) .or. &
                .not. present(xinds)  .or. .not. present(fvals)) then
                call error('rowSum, nind, xinds, and fvals must be present for doCount = .false.')
            end if
        end if

        stateTri => inputGrid%getGlobalTiling()
        nx_global = inputGrid%getGlobalCoordinateDimCount(1)
        ny_global = inputGrid%getGlobalCoordinateDimCount(2)

        triangle => stateTri%getElementIndices()
        node_ij  => stateTri%getNodeLociIndices()
        node_xy  => stateTri%getNodeLoci()

        call this%indVarGlobal%getArray(ginds)

        select type(obs_so)
            class is (ConicalScanningObservation)
                obs => obs_so
            class default
                write(msgstr,*) 'Incompatible base observation class in ',this%getName()
                call error(msgstr)
        end select

        if (.not. doCount) then
            rowSum = 0.d0
            xinds  = -1
            nind   = 0
        end if

        if (present(d_nnz)) then
            d_nnz = 0
        end if

        if (present(o_nnz)) then
            o_nnz = 0
        end if

        iz = obsnum
        i  = iz + 1

        if (.not. obs_so%passesQC(chind, i)) then
            return
        end if

        ! total angular offset in degrees of the pixel
        ! (360 degrees, 1000 ms per second, 60 seconds per minute)
        thetap = 360.d0*obs%getIntegrationTime(chind)*obs%spinFrequency/(1000.d0*60.d0)

        xf = SIGMA_FACTOR/obs%getFovCross(chind)/1000.d0
        yf = SIGMA_FACTOR/obs%getFovAlong(chind)/1000.d0

        ! get all of the triangles in range of the given min/max lat/lon
        call stateTri%getAllElementsInRange(minLat,maxLat,minLon,maxLon,&
            & this%stateTriIndices,numTri)

        ! reset the used indexes to 0
        this%usedInd = 0

        rejected = 0

        ! go through the points within the time integral
        do itime = 1,NT_POINTS
            ! the instantaneous angle
            angle3 = angle + thetap*this%tg(itime)
            ! compute the center of the instantaneous scan
            call geodesic_direct(scLat,scLon,angle3,arclen,lat2,lon2,angle2,1)

            ! cosine of the latitude, used for distance calculations
            coslat = cos(DTOR*lat2)
            ! wrapped latitude and longitude
            !latw = stateTri%wrapX(lat2)
            !lonw = stateTri%wrapY(lon2)

            if (.not. doCount) then
                ! loop through all the triangles in range
                do k=1,numTri
                    ! now need to compute the contribution of this triangle to the overall sum
                    ! use truncated triangular prism
                    triInd = this%stateTriIndices(k)
                    ! the vertexes of the triangle
                    do l=1,3
                        ind1 = triangle(l,triInd)

                        ! get the distance between the instantaneous obs and the node
                        call geodesic_inverse(lat2,lon2,&
                            & node_xy(1,ind1),node_xy(2,ind1),&
                            & arclen2,angle2,angle4,1)

                        ! angle of the bearing versus the x-axis
                        anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
                        ! the normalized x and y coordinates
                        this%xkvals(ind1) = arclen2*cos(anglek1)*xf
                        this%ykvals(ind1) = arclen2*sin(anglek1)*yf
                        ! the value of f for this point
                        this%fkvals(ind1) = exp(-0.5*(this%xkvals(ind1)**2+&
                                                    & this%ykvals(ind1)**2))
                    end do
                end do
            end if

            ! loop through all the triangles in range
            do k=1,numTri
                ! now need to compute the contribution of this triangle to the overall sum
                ! use truncated triangular prism
                triInd = this%stateTriIndices(k)
                ! the vertexes of the triangle
                ind1   = triangle(1,triInd)
                ind2   = triangle(2,triInd)
                ind3   = triangle(3,triInd)

                this%usedInd(ind1) = 1
                this%usedInd(ind2) = 1
                this%usedInd(ind3) = 1

                if (.not. doCount) then
                    xk1 = this%xkvals(ind1)
                    yk1 = this%ykvals(ind1)
                    f1  = this%fkvals(ind1)

                    xk2 = this%xkvals(ind2)
                    yk2 = this%ykvals(ind2)
                    f2  = this%fkvals(ind2)

                    xk3 = this%xkvals(ind3)
                    yk3 = this%ykvals(ind3)
                    f3  = this%fkvals(ind3)

                    ! compute the area of the triangle in x/y space
                    ax = xk1-xk2
                    ay = yk1-yk2
                    bx = xk1-xk3
                    by = yk1-yk3

                    an = dsqrt(ax*ax + ay*ay)
                    bn = dsqrt(bx*bx + by*by)

                    if (abs(an*bn) < 1d-16) then
                        rejected = rejected + 1
                        cycle
                    end if

                    costheta = (ax*bx + ay*by)/(an*bn)

                    !if (costheta >= 1.d0 .or. costheta <= -1.d0) then
                    !    rejected = rejected + 1
                    !    cycle
                    !end if

                    ! also check if the angle is unreasonably large or small
                    acostheta = dacos(costheta)

                    !if (acostheta < minAngle .or. acostheta > maxAngle) then
                    !    ! this is a degenerate triangle, ignore it
                    !    rejected = rejected + 1
                    !    cycle
                    !end if

                    areak = 0.5d0*an*bn*dsqrt(1.d0-costheta*costheta)

                    !if (areak > 0.6d0) then
                    !    print *,'areak:',areak
                    !    rejected = rejected + 1
                    !    cycle
                    !end if

                    ! and complete the summed portion of the integral
                    zf = this%wt(itime)*areak/(6.0d0*PI)

                    if (zf*f1 > 1d-10) then
                        nind = nind + 1
                        ki = node_ij(1,ind1)
                        kj = node_ij(2,ind1)
                        xinds(nind) = ginds(ki,kj)
                        fvals(nind) = zf*f1
                        rowSum = rowSum + fvals(nind)
                    end if

                    if (zf*f2 > 1d-10) then
                        nind = nind + 1
                        ki = node_ij(1,ind2)
                        kj = node_ij(2,ind2)
                        xinds(nind) = ginds(ki,kj)
                        fvals(nind) = zf*f2
                        rowSum = rowSum + fvals(nind)
                    end if

                    if (zf*f3 > 1d-10) then
                        nind = nind + 1
                        ki = node_ij(1,ind3)
                        kj = node_ij(2,ind3)
                        xinds(nind) = ginds(ki,kj)
                        fvals(nind) = zf*f3
                        rowSum = rowSum + fvals(nind)
                    end if
                end if
            end do
        end do

        if (present(d_nnz) .and. present(o_nnz)) then
            do k=1,size(node_xy,2)
                if (this%usedInd(k) == 1) then
                    ki = node_ij(1,k)
                    kj = node_ij(2,k)
                    if (ginds(ki,kj) >= this%myoffset .and. &
                      & ginds(ki,kj) <= this%lastIndex) then
                        ! this counts as an "on-diagonal" non-zero point
                        d_nnz = d_nnz + 1
                    else
                        ! need a point outside of the local data, it's "off-diagonal"
                        o_nnz = o_nnz + 1
                    end if
                end if
            end do
        end if

        if (rejected > 0) then
            print *,'avg rejected:',rejected/dble(NT_POINTS)
        end if
    end subroutine

    subroutine getNumberOfNonzeros(this, pinfo, obs_so, inputGrid, minLat, maxLat, &
        & minLon, maxLon, d_nnz, o_nnz)

        implicit none

        class(ConicalScanningObsOperator)         :: this

        class(ParallelInfo),          pointer     :: pinfo
        class(ScannedObservation),    pointer     :: obs_so
        class(DataGrid),              pointer     :: inputGrid
        real(real64), dimension(:,:), intent(out) :: minLat, maxLat, minLon, maxLon
        PetscInt,     dimension(:,:), intent(out) :: d_nnz
        PetscInt,     dimension(:,:), intent(out) :: o_nnz

        integer :: minj, mink, maxj, maxk

        real(real64) :: thetap, sang, cang
        real(real64) :: lat, lon, lat2, lon2, scLat, scLon
        real(real64) :: arclen, arclen2, angle1, angle2, angle3, angle4, fval, hval
        real(real64) :: min_good_ratio

        integer :: chind, i, iz, itime, j, k, ind, numTri

        integer :: scanNum, pixelNum
        integer :: xs, xe, nx_local
        integer :: ys, ye, ny_local

        integer, pointer :: owners(:)

        class(ConicalScanningObservation), pointer :: obs_cs

        real(real64) :: latvals(4)
        real(real64) :: lonvals(4)

        real(real64) :: distsqr, latdist, londist, coslat

        real(real32), pointer :: scLatPtr(:)
        real(real32), pointer :: scLonPtr(:)

        class(DataExtent), pointer :: xExtent
        class(DataExtent), pointer :: yExtent

        real(real64), dimension(:,:), pointer :: obsLoci

        class(Tiling), pointer :: stateTri
        real(real64),  pointer :: node_xy(:,:)

        PetscInt :: il

        call debug('Now beginning the count of non-zeroes')

        stateTri => inputGrid%getGlobalTiling()
        node_xy => stateTri%getNodeLoci()

        d_nnz = 0
        o_nnz = 0

        xExtent => inputGrid%getLocalCoordinateExtent(1)
        yExtent => inputGrid%getLocalCoordinateExtent(2)

        call xExtent%getLocalRange(xs,xe,nx_local)
        call yExtent%getLocalRange(ys,ye,ny_local)

        allocate(this%maxdistsqr(this%nposting_channels))
        allocate(this%xkvals(size(node_xy,2)))
        allocate(this%ykvals(size(node_xy,2)))
        allocate(this%fkvals(size(node_xy,2)))
        allocate(this%usedInd(size(node_xy,2)))

        select type(obs_so)
            class is (ConicalScanningObservation)
                obs_cs => obs_so
            class default
                write(msgstr,*) 'Incompatible base observation class in ',this%getName()
                call error(msgstr)
        end select

        ! get the maximum distance squared, in degrees, of each channel's instantaneous fov
        do i=1,this%nposting_channels
            this%maxdistsqr(i) = (2.5*max(obs_cs%getFovAlong(i),obs_cs%getFovCross(i))*kmToDegrees)**2

            write(msgstr,*) 'Found the maximum distance (degrees) for channel ', i, &
                & ':', this%maxdistsqr(i)
            call debug(msgstr)
        end do


        ! set the min/max lat/lons to a missing value; those that aren't set will retain this value.
        ! temporarily make minLat and minLon positive 999 to make min calculations easier
        minLat =  999
        maxLat = -999
        minLon =  999
        maxLon = -999

        call debug('Now checking the min/max lons')

        call this%getMinMaxLatLons(pinfo, obs_cs, inputGrid, minLat, maxLat, minLon, maxLon)

        ! here we should prune the input grid to stay within min/max lat/lon
        obsLoci => obs_cs%getObsLoci()

        scLatPtr => obs_cs%getCraftLatitudes()
        scLonPtr => obs_cs%getCraftLongitudes()

        call debug('Now running through each observation to find the number of non-zeroes')

        owners => obs_so%getObsOwners()

        il = 0

        ! do a quick run through the obs to approximate the number of non-zeros in each row
        do iz=0,size(obsLoci,2)-1
            i = iz+1
            if (owners(i) /= pinfo%getRank()) then
                cycle
            end if
            il = il+1
            lat = obsLoci(SO_LAT_DIM,i)
            lon = obsLoci(SO_LON_DIM,i)

            pixelNum = anint(obsLoci(SO_PIX_DIM,i))
            scanNum  = anint(obsLoci(SO_SCAN_DIM,i))

            scLat = dble(scLatPtr(scanNum))
            scLon = dble(scLonPtr(scanNum))

            call geodesic_inverse(scLat,scLon,lat,lon,arclen,angle1,angle2,1)

            do chind=1,this%nposting_channels
                call this%getRowData(pinfo,.true.,obs_so,inputGrid,iz,il,chind,&
                    & scLat,scLon,lat,lon,arclen,angle1,&
                    & minLat(il,chind),maxLat(il,chind),minLon(il,chind),maxLon(il,chind),&
                    & d_nnz(il,chind),o_nnz(il,chind))
!
!                ! total angular offset in degrees
!                ! (360 degrees, 1000 ms per second, 60 seconds per minute)
!                thetap = 360.d0*obs_cs%getIntegrationTime(chind)*obs_cs%spinFrequency/(1000.d0*60.d0)
!
!                this%usedInd = 0
!
!                ! go through the points within the time integral
!                do itime = 1,NT_POINTS
!                    angle3 = angle1 + thetap*this%tg(itime)
!                    ! compute the center of the instantaneous scan
!                    call geodesic_direct(scLat,scLon,angle3,arclen,lat2,lon2,angle2,1)
!
!                    ! if this instantaneous scan is out of the domain, disqualify it
!!                    call state_gtb%convertLatLonToIJ(lat2,lon2,x2,y2)
!
!!                    if (x2 .lt. 1 .or. y2 .lt. 1 .or. x2 .gt. nx .or. y2 .gt. ny) then
!!                        call obs_cs%setQCCode(chind, i, QC_UPDT_FWDOP)
!!                        exit ! the loop
!!                    end if
!
!                    coslat = cos(DTOR*lat2)
!                    latw = stateTri%wrapX(lat2)
!                    lonw = stateTri%wrapY(lon2)
!
!                    ! run through all the nodes and see which points are within range
!                    do k=1,size(node_xy,2)
!                        latdist = node_xy(1,k) - latw
!                        londist = (node_xy(2,k) - lonw)*coslat
!                        distsqr = latdist*latdist + londist*londist
!
!                        ! update usedInd if in range
!                        if (distsqr <= this%maxdistsqr(chind)) then
!                            this%usedInd(k) = 1
!                        end if
!                    end do
!                end do
!
!                ! now run through again and update nnzs and min/max lat/lon
!                do k=1,size(node_xy,2)
!                    if (this%usedInd(k) == 1) then
!                        ! update min/max lat/lons as well
!                        minLat(il,chind) = min(minLat(il,chind),node_xy(1,k))
!                        maxLat(il,chind) = max(maxLat(il,chind),node_xy(1,k))
!                        minLon(il,chind) = min(minLon(il,chind),node_xy(2,k))
!                        maxLon(il,chind) = max(maxLon(il,chind),node_xy(2,k))
!
!                        ! update the number of diagonal or non-diagonal non-zeros
!                        if (node_ij(1,k) >= xs .and. node_ij(1,k) <= xe .and. &
!                           &node_ij(2,k) >= ys .and. node_ij(2,k) <= ye) then
!
!                            ! this counts as an "on-diagonal" non-zero point
!                            d_nnz(il,chind) = d_nnz(il,chind) + 1
!                        else
!                            ! need a point outside of the local data, it's "off-diagonal"
!                            o_nnz(il,chind) = o_nnz(il,chind) + 1
!                        end if
!                    end if
!                end do
            end do
        end do

        !print *,'found nnz:',pinfo%getRank(),minval(d_nnz),maxval(d_nnz),minval(o_nnz),maxval(o_nnz)

        ! switch back the minLat/minLon to be consistent with maxLat/maxLon
        where(minLat == 999) minLat = -999
        where(minLon == 999) minLon = -999
    end subroutine

    subroutine getRowValues(this,pinfo,obs_so,inputGrid,obsnum,il,chind,rowSum,nind,&
        & xinds,fvals,scLat,scLon,lat,lon,arclen,angle,minLat,maxLat,minLon,maxLon, &
        & d_nnz_actual, o_nnz_actual)

        implicit none

        class(ConicalScanningObsOperator)       :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(ScannedObservation),  pointer     :: obs_so
        class(DataGrid),            pointer     :: inputGrid
        integer,                    intent(in)  :: obsnum
        PetscInt,                   intent(in)  :: il
        integer,                    intent(in)  :: chind
        real(real64),               intent(out) :: rowSum
        PetscInt,                   intent(out) :: nind
        PetscInt,     dimension(:), intent(out) :: xinds
        real(real64), dimension(:), intent(out) :: fvals
        real(real64),               intent(in)  :: scLat
        real(real64),               intent(in)  :: scLon
        real(real64),               intent(in)  :: lat
        real(real64),               intent(in)  :: lon
        real(real64),               intent(in)  :: arclen
        real(real64),               intent(in)  :: angle
        real(real64),               intent(in)  :: minLat
        real(real64),               intent(in)  :: maxLat
        real(real64),               intent(in)  :: minLon
        real(real64),               intent(in)  :: maxLon
        PetscInt,                   intent(out) :: d_nnz_actual
        PetscInt,                   intent(out) :: o_nnz_actual

!        class(ConicalScanningObservation), pointer :: obs
!
!        real(real64) :: an, anglek1, anglek2, anglek3, areak, ax, ay, bn, bx, by, costheta, f1, f2, f3
!        real(real64) :: lat2, lon2, latk1, latk2, latk3, lonk1, lonk2, lonk3
!        real(real64) :: xf, xk1, xk2, xk3, yf, yk1, yk2, yk3, zf
!
!        real(real64) :: thetap, sang, cang, latw, lonw
!        real(real64) :: angle2, angle3, angle4, arclen2, arclen3, arclen4
!        real(real64) :: fval, distsqr, latdist, londist, coslat, earthradius, kmtodegrees
!
!        integer :: i, ind1, ind2, ind3, iz, itime, k, ki, kj, numtri, triInd
!
!        integer :: nx_global, ny_global
!
!        real(real64), dimension(2,3) :: xyi
!        logical, dimension(3) :: inside
!
!        class(Triangulation), pointer :: stateTri
!
!        stateTri => inputGrid%getTriangulation()
!        nx_global = inputGrid%getGlobalDimCount(1)
!        ny_global = inputGrid%getGlobalDimCount(2)

        call this%getRowData(pinfo,.false.,obs_so,inputGrid,obsnum,il,chind,scLat,scLon,&
            & lat,lon,arclen,angle,minLat,maxLat,minLon,maxLon,&
            & rowSum=rowSum,nind=nind,xinds=xinds,fvals=fvals, &
            & d_nnz=d_nnz_actual,o_nnz=o_nnz_actual)

!        select type(obs_so)
!            class is (ConicalScanningObservation)
!                obs => obs_so
!            class default
!                write(msgstr,*) 'Incompatible base observation class in ',this%getName()
!                call error(msgstr)
!        end select
!
!        rowSum = 0.d0
!        xinds  = -1
!        fvals  = 0.d0
!        nind   = 0
!
!        iz = obsnum
!        i  = iz + 1
!
!        if (.not. obs_so%passesQC(chind, i)) then
!            return
!        end if
!
!        ! total angular offset in degrees of the pixel
!        ! (360 degrees, 1000 ms per second, 60 seconds per minute)
!        thetap = 360.d0*obs%getIntegrationTime(chind)*obs%spinFrequency/(1000.d0*60.d0)
!
!        xf = SIGMA_FACTOR/obs%getFovCross(chind)/1000.d0
!        yf = SIGMA_FACTOR/obs%getFovAlong(chind)/1000.d0
!
!        ! get all of the triangles in range of the given min/max lat/lon
!        call stateTri%getAllTrianglesInRange(minLat,maxLat,minLon,maxLon,&
!            & this%stateTriIndices,numTri)
!
!        ! go through the points within the time integral
!        do itime = 1,NT_POINTS
!            ! the instantaneous angle
!            angle3 = angle + thetap*this%tg(itime)
!            ! compute the center of the instantaneous scan
!            call geodesic_direct(scLat,scLon,angle3,arclen,lat2,lon2,angle2,1)
!
!            ! reset the used indexes to 0
!            this%usedInd = 0
!
!            ! cosine of the latitude, used for distance calculations
!            coslat = cos(DTOR*lat2)
!            ! wrapped latitude and longitude
!            latw = stateTri%wrapX(lat2)
!            lonw = stateTri%wrapY(lon2)
!
!            ! run through all the nodes and set the fvals if in range
!            do k=1,size(node_xy,2)
!                latdist = node_xy(1,k) - latw
!                londist = (node_xy(2,k) - lonw)*coslat
!                distsqr = latdist*latdist + londist*londist
!
!                ! see if this node is within range
!                if (distsqr <= this%maxdistsqr(chind)) then
!                    ! get the distance between the instantaneous obs and the node
!                    call geodesic_inverse(latw,lonw,node_xy(1,k),node_xy(2,k),&
!                        &arclen2,angle2,angle4,1)
!
!                    ! angle of the bearing versus the x-axis
!                    anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                    ! the normalized x and y coordinates
!                    this%xkvals(k) = arclen2*cos(anglek1)*xf
!                    this%ykvals(k) = arclen2*sin(anglek1)*yf
!                    ! the value of f for this point
!                    this%fkvals(k) = exp(-0.5*(this%xkvals(k)**2+this%ykvals(k)**2))
!
!                    ! mark the point as used during this time integral
!                    this%usedInd(k) = 1
!                end if
!            end do
!
!            ! loop through all the triangles in range
!            do k=1,numTri
!                ! now need to compute the contribution of this triangle to the overall sum
!                ! use truncated triangular prism
!                triInd = this%stateTriIndices(k)
!                ! the vertexes of the triangle
!                ind1   = triangle(1,triInd)
!                ind2   = triangle(2,triInd)
!                ind3   = triangle(3,triInd)
!
!                ! check if, for some reason, none of the vertexes were used
!                if (this%usedInd(ind1) + this%usedInd(ind2) + this%usedInd(ind3) == 0) then
!                    cycle
!                end if
!
!                ! for the first vertex, if it was used, get the x/y/f value
!                if (this%usedInd(ind1) == 1) then
!                    xk1 = this%xkvals(ind1)
!                    yk1 = this%ykvals(ind1)
!
!                    f1  = this%fkvals(ind1)
!                else
!                    ! otherwise, we still need the distance from the obs to the point
!                    call geodesic_inverse(latw,lonw,node_xy(1,ind1),node_xy(2,ind1),&
!                        &arclen2,angle2,angle4,1)
!
!                    ! get the normalized x/y coordinates
!                    anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                    xk1 = arclen2*cos(anglek1)*xf
!                    yk1 = arclen2*sin(anglek1)*yf
!                    !f1  = exp(-0.5*(xk1**2+yk1**2))
!                end if
!
!                ! for the second vertex, if it was used, get the x/y/f value
!                if (this%usedInd(ind2) == 1) then
!                    xk2 = this%xkvals(ind2)
!                    yk2 = this%ykvals(ind2)
!
!                    f2  = this%fkvals(ind2)
!                else
!                    ! otherwise, we still need the distance from the obs to the point
!                    call geodesic_inverse(latw,lonw,node_xy(1,ind2),node_xy(2,ind2),&
!                        &arclen2,angle2,angle4,1)
!
!                    ! get the normalized x/y coordinates
!                    anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                    xk2 = arclen2*cos(anglek1)*xf
!                    yk2 = arclen2*sin(anglek1)*yf
!                    !f2  = exp(-0.5*(xk2**2+yk2**2))
!                end if
!
!                ! for the third vertex, if it was used, get the x/y/f value
!                if (this%usedInd(ind3) == 1) then
!                    xk3 = this%xkvals(ind3)
!                    yk3 = this%ykvals(ind3)
!
!                    f3  = this%fkvals(ind3)
!                else
!                    ! otherwise, we still need the distance from the obs to the point
!                    call geodesic_inverse(latw,lonw,node_xy(1,ind3),node_xy(2,ind3),&
!                        &arclen2,angle2,angle4,1)
!
!                    anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                    xk3 = arclen2*cos(anglek1)*xf
!                    yk3 = arclen2*sin(anglek1)*yf
!                    !f3  = exp(-0.5*(xk3**2+yk3**2))
!                end if
!
!                ! compute the area of the triangle in x/y space
!                ax = xk1-xk2
!                ay = yk1-yk2
!                bx = xk1-xk3
!                by = yk1-yk3
!
!                an = dsqrt(ax*ax + ay*ay)
!                bn = dsqrt(bx*bx + by*by)
!
!                if (abs(an*bn) < 1d-16) then
!                    cycle
!                end if
!
!                costheta = (ax*bx + ay*by)/(an*bn)
!
!                if (costheta >= 1.d0 .or. costheta <= -1.d0) then
!                    cycle
!                end if
!
!                areak = 0.5*an*bn*dsqrt(1.d0-costheta*costheta)
!
!                ! and complete the summed portion of the integral
!                zf = this%wt(itime)*areak/(6.0d0*PI)
!
!                !xyi(1,1) = node_xy(1,ind1)
!                !xyi(2,1) = node_xy(2,ind1)
!                !xyi(1,2) = node_xy(1,ind2)
!                !xyi(2,2) = node_xy(2,ind2)
!                !xyi(1,3) = node_xy(1,ind3)
!                !xyi(2,3) = node_xy(2,ind3)
!
!                ! check that these points are in the obs domain convex hull before adding
!                !call this%obsTri%isInsideConvexHull(3, xyi, inside)
!
!                !if (inside(1) .and. this%usedInd(ind1) == 1) then
!                if (this%usedInd(ind1) == 1 .and. zf*f1 > 1d-10) then
!                    nind = nind + 1
!                    ki = node_ij(1,ind1)
!                    kj = node_ij(2,ind1)
!                    xinds(nind) = ki + (kj-1)*nx_global - 1
!                    fvals(nind) = zf*f1
!                    rowSum = rowSum + fvals(nind)
!                end if
!
!                !if (inside(2) .and. this%usedInd(ind2) == 1) then
!                if (this%usedInd(ind2) == 1 .and. zf*f2 > 1d-10) then
!                    nind = nind + 1
!                    ki = node_ij(1,ind2)
!                    kj = node_ij(2,ind2)
!                    xinds(nind) = ki + (kj-1)*nx_global - 1
!                    fvals(nind) = zf*f2
!                    rowSum = rowSum + fvals(nind)
!                end if
!
!                !if (inside(3) .and. this%usedInd(ind3) == 1) then
!                if (this%usedInd(ind3) == 1 .and. zf*f3 > 1d-10) then
!                    nind = nind + 1
!                    ki = node_ij(1,ind3)
!                    kj = node_ij(2,ind3)
!                    xinds(nind) = ki + (kj-1)*nx_global - 1
!                    fvals(nind) = zf*f3
!                    rowSum = rowSum + fvals(nind)
!                end if
!            end do
!        end do

!        iz = obsnum
!        i  = iz + 1
!
!        ! total angular offset in degrees
!        ! (360 degrees, 1000 ms per second, 60 seconds per minute)
!        thetap = 360.d0*obs%intTime(chind)*obs%spinFrequency/(1000.d0*60.d0)
!
!        xf = SIGMA_FACTOR/obs%fovCross(chind)/1000.d0
!        yf = SIGMA_FACTOR/obs%fovAlong(chind)/1000.d0
!
!        rowSum = 0.d0
!        fvals = 0.d0
!        nind = 0
!
!        call stateTri%getAllTrianglesInRange(minLat,maxLat,minLon,maxLon,stateTriIndices,numTri)
!
!        do itime=1,NT_POINTS
!            angle3 = angle + thetap*this%tg(itime)
!            call geodesic_direct(scLat,scLon,angle3,arclen,lat2,lon2,angle2,1)
!
!            !sang = sin(DTOR*angle3)
!            !cang = cos(DTOR*angle3)
!
!            do k=1,numTri
!                ! now need to compute the contribution of this triangle to the overall sum
!                ! use truncated triangular prism
!                triInd = stateTriIndices(k)
!                ind1   = triangle(1,triInd)
!                ind2   = triangle(2,triInd)
!                ind3   = triangle(3,triInd)
!
!                latk1 = node_xy(1,ind1)
!                latk2 = node_xy(1,ind2)
!                latk3 = node_xy(1,ind3)
!
!                lonk1 = node_xy(2,ind1)
!                lonk2 = node_xy(2,ind2)
!                lonk3 = node_xy(2,ind3)
!
!                call geodesic_inverse(lat2,lon2,latk1,lonk1,arclen2,angle2,angle4,1)
!                ! want range in (-180,180), and need to rotate as azimuth from N = 0 to N = 90 degrees.
!                anglek1 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                xk1 = arclen2*cos(anglek1)*xf
!                yk1 = arclen2*sin(anglek1)*yf
!
!                call geodesic_inverse(lat2,lon2,latk2,lonk2,arclen2,angle2,angle4,1)
!                anglek2 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                xk2 = arclen2*cos(anglek2)*xf
!                yk2 = arclen2*sin(anglek2)*yf
!
!                call geodesic_inverse(lat2,lon2,latk3,lonk3,arclen2,angle2,angle4,1)
!                anglek3 = DTOR*(mod(angle3-angle2+180.d0+90.d0,360.d0)-180.d0)
!                xk3 = arclen2*cos(anglek3)*xf
!                yk3 = arclen2*sin(anglek3)*yf
!
!                f1 = exp(-0.5*(xk1**2+yk1**2))
!                f2 = exp(-0.5*(xk2**2+yk2**2))
!                f3 = exp(-0.5*(xk3**2+yk3**2))
!
!                ax = xk1-xk2
!                ay = yk1-yk2
!                bx = xk1-xk3
!                by = yk1-yk3
!
!                an = dsqrt(ax*ax + ay*ay)
!                bn = dsqrt(bx*bx + by*by)
!
!                costheta = (ax*bx + ay*by)/(an*bn)
!
!                areak = 0.5*an*bn*dsqrt(1.d0-costheta*costheta)
!
!                zf = this%wt(itime)*areak/(6.0d0*PI)
!
!                fval = zf*(f1 + f2 + f3)
!
!                if (fval .gt. 1.d-8) then
!                    xyi(1,1) = node_xy(1,ind1)
!                    xyi(2,1) = node_xy(2,ind1)
!                    xyi(1,2) = node_xy(1,ind2)
!                    xyi(2,2) = node_xy(2,ind2)
!                    xyi(1,3) = node_xy(1,ind3)
!                    xyi(2,3) = node_xy(2,ind3)
!
!                    ! check that these points are in the obs domain convex hull before adding
!                    call this%obsTri%isInsideConvexHull(3, xyi, inside)
!
!                    if (inside(1) .and. f1 > 1d-8) then
!                        nind = nind + 1
!                        ki = node_ij(1,ind1)
!                        kj = node_ij(2,ind1)
!                        xinds(nind) = ki + (kj-1)*this%nx - 1
!                        fvals(nind) = zf*f1
!                        rowSum = rowSum + fvals(nind)
!                    end if
!
!                    if (inside(2) .and. f2 > 1d-8) then
!                        nind = nind + 1
!                        ki = node_ij(1,ind2)
!                        kj = node_ij(2,ind2)
!                        xinds(nind) = ki + (kj-1)*this%nx - 1
!                        fvals(nind) = zf*f2
!                        rowSum = rowSum + fvals(nind)
!                    end if
!
!                    if (inside(3) .and. f3 > 1d-8) then
!                        nind = nind + 1
!                        ki = node_ij(1,ind3)
!                        kj = node_ij(2,ind3)
!                        xinds(nind) = ki + (kj-1)*this%nx - 1
!                        fvals(nind) = zf*f3
!                        rowSum = rowSum + fvals(nind)
!                    end if
!                end if
!            end do
!        end do
    end subroutine

    function getMaxStateIndices(this,inputGrid) result(maxInd)
        implicit none

        class(ConicalScanningObsOperator) :: this

        class(DataGrid), pointer :: inputGrid

        integer :: maxInd

        class(Tiling), pointer :: stateTri
        integer,       pointer :: triangle(:,:)

        stateTri => inputGrid%getGlobalTiling()
        triangle => stateTri%getElementIndices()

        maxInd = 3*size(triangle,2)
    end function

    function isSameOperator(this,obs_so,ch1,ch2) result(canReuse)

        class(ConicalScanningObsOperator) :: this

        class(ScannedObservation),               pointer :: obs_so
        integer, intent(in)                              :: ch1
        integer, intent(in)                              :: ch2

        logical                                          :: canReuse

        class(ConicalScanningObservation),       pointer :: obs_cs

        class(DataExtent), pointer :: dextent

        class(DataVariable), pointer :: obsDataVar

        if (ch1 < 1 .or. ch2 < 1) then
            canReuse = .false.
            return
        end if

        select type(obs_so)
            class is (ConicalScanningObservation)
                obs_cs => obs_so
            class default
                write(msgstr,*) 'Incompatible base observation class in ',this%getName()
                call error(msgstr)
        end select

        obsDataVar => obs_cs%getObsDataVar()

        dextent => obsDataVar%getExtentNumber(1)

        if (ch1 > dextent%getLocalCount() .or. ch2 > dextent%getLocalCount()) then
            canReuse = .false.
            return
        end if

        ! check if we can reuse the values from a previous channel to improve performance
        canReuse = obs_cs%getFovCross(ch1) == obs_cs%getFovCross(ch2) .and. &
                  &obs_cs%getFovAlong(ch1) == obs_cs%getFovAlong(ch2)
    end function
end module
