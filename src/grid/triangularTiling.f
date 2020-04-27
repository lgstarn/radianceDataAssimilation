module triangularTiling_mod

    use iso_fortran_env

    use tiling_mod

    use tripack_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(Tiling), public :: TriangularTiling
        !private
            integer, pointer :: lptr(:)  => null()
            integer, pointer :: lend(:)  => null()
            integer, pointer :: list(:)  => null()
            integer          :: lnew     =  1
            integer          :: nst      =  1

        contains
            !procedure :: setNumTriangles

            procedure :: doInterpolate

            procedure :: search
            procedure :: isInside

            procedure :: cloneTriangularTiling
            procedure :: clone

            generic   :: triangularTilingConstructor => &
                       & triangularTilingConstructor_tile, &
                       & triangularTilingConstructor_set

            procedure, private :: triangularTilingConstructor_tile
            procedure, private :: triangularTilingConstructor_set

            final :: triangularTilingDestructor ! clean up all allocated variables
    end type

    contains

    subroutine triangularTilingConstructor_tile(this,nodeNum,ngx,node_ij,node_xy,&
        node_owner,deallocNode_ij,deallocNode_xy,regularGrid,nx)

        implicit none

        class(TriangularTiling) :: this

        integer,             intent(in) :: nodeNum
        integer,             intent(in) :: ngx
        integer,             pointer    :: node_ij(:,:)
        real(real64),        pointer    :: node_xy(:,:)
        integer,   optional, pointer    :: node_owner(:)
        logical,   optional, intent(in) :: deallocNode_ij
        logical,   optional, intent(in) :: deallocNode_xy
        logical,   optional, intent(in) :: regularGrid
        integer,   optional, intent(in) :: nx

        integer :: i, ierr, nt, nit, nlist

        integer,        allocatable :: lcc(:)
        integer,        allocatable :: lct(:)
        integer,        allocatable :: ltri(:,:)
        real(real64),   allocatable :: dist(:)
        integer(int32), allocatable :: near(:)
        integer(int32), allocatable :: next(:)

        logical :: doRegularGrid

        if (size(node_xy,1) .ne. 2 .or. size(node_xy,2) .ne. nodeNum) then
            write(msgstr,*) 'Incompatible size of node_xy in triangularTilingConstructor:',&
                &size(node_xy,1),size(node_xy,2),nodenum
            call error(msgstr)
        end if

        if (present(regularGrid)) then
            if (regularGrid) then
                if (.not. present(nx)) then
                    call error('Nx is not optional if regularGrid is true')
                end if
            end if

            doRegularGrid = regularGrid
        else
            doRegularGrid = .false.
        end if

        nlist = 6*nodenum
        allocate(this%list(nlist))
        allocate(this%lptr(nlist))
        allocate(this%lend(nodenum))

        if (doRegularGrid) then
            nit = 1000
            call trmshr ( nodenum, nx, node_xy(1,:), node_xy(2,:), nit, this%list, this%lptr, &
                & this%lend, this%lnew, ierr )

            if (ierr /= 0) then
                call error('trmshr returned a non-zero error: ' // int2str(ierr) // &
                    & ', nit: ' // int2str(nit))
            end if
        else
            allocate(near(nodenum))
            allocate(next(nodenum))
            allocate(dist(nodenum))
            call trmesh( nodenum, node_xy(1,:), node_xy(2,:), this%list, this%lptr, this%lend, &
                this%lnew, near, next, dist, ierr )

            if (ierr /= 0) then
                call error('trmesh returned a non-zero error: ' // int2str(ierr))
            end if
            deallocate(near)
            deallocate(next)
            deallocate(dist)
        end if

        allocate(lcc(1))
        allocate(lct(1))
        allocate(ltri(6,2*nodenum))

        call trlist ( 0, lcc, nodenum, this%list, this%lptr, this%lend, 6, nt, ltri, lct, ierr )

        allocate(this%elements(3,nt))
        allocate(this%elementNeighbor(3,nt))

        this%elements(:,:)        = ltri(1:3,1:nt)
        this%elementNeighbor(:,:) = ltri(4:6,1:nt)

        call this%tilingConstructor(nodeNum,ngx,node_ij,node_xy,this%elements,&
            & this%elementNeighbor,2,nt,node_owner,deallocNode_ij,deallocNode_xy)

        deallocate(lcc)
        deallocate(lct)
        deallocate(ltri)
    end subroutine

    subroutine triangularTilingConstructor_set(this,nodeNum,ngx,node_ij,node_xy,&
        & elements,elementNeighbor,numElements,list,lptr,lend,lnew,node_owner,&
        & deallocNode_ij,deallocNode_xy)

        implicit none

        class(TriangularTiling) :: this

        integer,                intent(in) :: nodeNum
        integer,                intent(in) :: ngx
        integer,                pointer    :: node_ij(:,:)
        real(real64),           pointer    :: node_xy(:,:)
        integer,                pointer    :: elements(:,:)
        integer,                pointer    :: elementNeighbor(:,:)
        integer,                intent(in) :: numElements
        integer,                pointer    :: list(:)
        integer,                pointer    :: lptr(:)
        integer,                pointer    :: lend(:)
        integer,                intent(in) :: lnew
        integer,      optional, pointer    :: node_owner(:)
        logical,      optional, intent(in) :: deallocNode_ij
        logical,      optional, intent(in) :: deallocNode_xy

        call this%tilingConstructor(nodeNum,ngx,node_ij,node_xy,elements, &
            & elementNeighbor,2,numElements,node_owner,deallocNode_ij,&
            & deallocNode_xy)

        this%list     => list
        this%lptr     => lptr
        this%lend     => lend
        this%lnew     =  lnew
    end subroutine

    subroutine triangularTilingDestructor(this)

        implicit none

        type(TriangularTiling)  :: this

        call this%tilingDestructor()

        if (associated(this%lptr)) then
            deallocate(this%lptr)
        end if

        if (associated(this%lend)) then
            deallocate(this%lend)
        end if

        if (associated(this%list)) then
            deallocate(this%list)
        end if
    end subroutine

!    subroutine setNumTriangles(this,numTriangles)
!        implicit none
!
!        class(TriangularTiling) :: this
!
!        integer,    intent(in)  :: numTriangles
!
!        if (associated(this%triangle)) then
!            deallocate(this%triangle)
!        end if
!
!        if (associated(this%elementNeighbor)) then
!            deallocate(this%elementNeighbor)
!        end if
!
!        allocate(this%triangle(3,numTriangles))
!        allocate(this%elementNeighbor(3,numTriangles))
!
!        this%numElements = numTriangles
!    end subroutine

    subroutine doInterpolate(this, zd, ni, xyi, zi)

        implicit none

        class(TriangularTiling)   :: this

        real(real64), intent(in)  :: zd(:)
        integer,      intent(in)  :: ni
        real(real64), intent(in)  :: xyi(2,ni)
        real(real64), intent(out) :: zi(ni)

        integer :: i, i1, i2, i3, edge

        integer :: nodeIndices(3)

        real(real64) :: baryCoords(3)

        real(real64) :: xyi2(2)

        if (size(zd) /= size(this%node_xy)) then
            call error('The size of the input data did not match the size of the grid: ' // &
                & int2str(size(zd)) // ' vs. ' // int2str(size(this%node_xy)))
        end if

         do i=1,ni
            xyi2(1:2) = xyi(1:2,i)

            call this%search(xyi2,nodeIndices,baryCoords,edge)

            i1 = nodeIndices(1)
            i2 = nodeIndices(2)
            i3 = nodeIndices(3)

            if (edge > 0) then
                zi(i) = baryCoords(1)*zd(i1) + baryCoords(2)*zd(i2) + baryCoords(3)*zd(i3)
            else
                zi(i) = -999.d0
            end if
        end do
    end subroutine

    subroutine search(this,xyi,nodeIndices,baryCoords,edge)

        implicit none

        class(TriangularTiling)    :: this
        real(real64), intent(in)   :: xyi(2)
        integer,      intent(out)  :: nodeIndices(:)
        real(real64), intent(out)  :: baryCoords(:)
        integer,      intent(out)  :: edge

        real(real64) :: alpha
        real(real64) :: beta
        real(real64) :: gamma

        integer :: i1, i2, i3

        real(real64) :: x1, y1, x2, y2, x3, y3
        real(real64) :: dx21, dy21
        real(real64) :: dx31, dy31
        real(real64) :: dxp1, dyp1
        real(real64) :: d00, d01, d11, d20, d21
        real(real64) :: denom
        real(real64) :: u, v, w

        real(real64) :: px, py

        px = xyi(1)
        py = xyi(2)

        call trfind ( this%nst, px, py, this%nodeNum, this%node_xy(1,:), this%node_xy(2,:), &
            & this%list, this%lptr, this%lend, i1, i2, i3 )

        nodeIndices(1) = i1
        nodeIndices(2) = i2
        nodeIndices(3) = i3

        baryCoords(:) = 0.d0

        if (i1 > 0 .and. i2 > 0 .and. i3 > 0) then
            ! save the found index #2 for starting the search next time
            this%nst = i2

            x1 = this%node_xy(1,i1)
            y1 = this%node_xy(2,i1)
            x2 = this%node_xy(1,i2)
            y2 = this%node_xy(2,i2)
            x3 = this%node_xy(1,i3)
            y3 = this%node_xy(2,i3)

            dx21 = x2-x1
            dy21 = y2-y1
            dx31 = x3-x1
            dy31 = y3-y1
            dxp1 = px-x1
            dyp1 = py-y1

            d00 = dx21*dx21 + dy21*dy21
            d01 = dx21*dx31 + dy21*dy31
            d11 = dx31*dx31 + dy31*dy31
            d20 = dx21*dxp1 + dy21*dyp1
            d21 = dx31*dxp1 + dy31*dyp1

            denom = d00*d11 - d01*d01
            beta  = (d11*d20 - d01*d21) / denom
            gamma = (d00*d21 - d01*d20) / denom
            alpha = 1.0d0 - beta - gamma

            baryCoords(1) = alpha
            baryCoords(2) = beta
            baryCoords(3) = gamma

            edge = 1
        else
            edge = -1
        end if
    end subroutine

    subroutine isInside(this, ni, xyi, inside)

        implicit none

        class(TriangularTiling)   :: this

        integer,      intent(in)  :: ni
        real(real64), intent(in)  :: xyi(2,ni)
        logical,      intent(out) :: inside(ni)

        integer :: i, i1, i2, i3

        real(real64) :: px, py
        real(real64) :: xyi2(2)

        do i = 1, ni
            px = xyi(1,i)
            py = xyi(2,i)

            call trfind ( this%nst, px, py, this%nodeNum, this%node_xy(1,:), this%node_xy(2,:), &
                & this%list, this%lptr, this%lend, i1, i2, i3 )

            if ( i1 <= 0 .or. i2 <= 0 .or. i3 <= 0) then
                inside(i) = .false.
            else
                this%nst = i2
                inside(i) = .true.
            end if
        end do
    end subroutine

    function cloneTriangularTiling(this,copyData) result(other)

        implicit none

        class(TriangularTiling)             :: this

        logical,                 intent(in) :: copyData

        class(TriangularTiling), pointer    :: other

        integer :: nx, ny

        integer,      pointer    :: node_ij(:,:)
        real(real64), pointer    :: node_xy(:,:)
        integer,      pointer    :: list(:)
        integer,      pointer    :: lptr(:)
        integer,      pointer    :: lend(:)
        integer,      pointer    :: elements(:,:)
        integer,      pointer    :: elementNeighbor(:,:)
        integer,      pointer    :: node_owner(:)

        allocate(node_ij(size(this%node_ij,1),size(this%node_ij,2)))
        allocate(node_xy(size(this%node_xy,1),size(this%node_xy,2)))
        allocate(list(size(this%list,1)))
        allocate(lptr(size(this%lptr,1)))
        allocate(lend(size(this%lend,1)))
        allocate(elements(size(this%elements,1),size(this%elements,2)))
        allocate(elementNeighbor(size(this%elementNeighbor,1),size(this%elementNeighbor,2)))

        if (associated(this%node_owner)) then
            allocate(node_owner(size(this%node_owner,1)))
        end if

        if (copyData) then
            node_ij(:,:) = this%node_ij(:,:)
            node_xy(:,:) = this%node_xy(:,:)
            list(:) = this%list(:)
            lend(:) = this%lend(:)
            elements(:,:) = this%elements(:,:)
            elementNeighbor(:,:) = this%elementNeighbor(:,:)

            if (associated(this%node_owner)) then
                node_owner(:) = this%node_owner(:)
            end if
        end if

        allocate(other)
        call other%triangularTilingConstructor(this%nodeNum,this%ngx,node_ij,node_xy,     &
            & elements,elementNeighbor,this%numElements,list,lptr,lend,this%lnew,&
            & node_owner,deallocNode_ij=.true.,deallocNode_xy=.true.)
    end function

    function clone(this,copyData) result(other)

        implicit none

        class(TriangularTiling)   :: this

        logical,       intent(in) :: copyData

        class(Tiling),           pointer    :: other

        class(TriangularTiling), pointer    :: til

        til => this%cloneTriangularTiling(copyData)

        other => til
    end function
end module
