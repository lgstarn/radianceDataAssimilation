module regularTriangularTiling_mod

    use iso_fortran_env

    use tiling_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(Tiling), public :: RegularTriangularTiling
        !private
            integer :: nst      =  1
            integer :: nx
            integer :: ny

        contains
            !procedure :: setNumTriangles

            procedure :: doInterpolate

            procedure :: search
            procedure :: isInside

            procedure :: cloneRegularTriangularTiling
            procedure :: clone

            generic   :: regularTriangularTilingConstructor => &
                         regularTriangularTilingConstructor_tile, &
                         regularTriangularTilingConstructor_set

            procedure, private :: regularTriangularTilingConstructor_tile
            procedure, private :: regularTriangularTilingConstructor_set


            final :: regularTriangularTilingDestructor ! clean up all allocated variables
    end type

    contains

    subroutine regularTriangularTilingConstructor_tile(this,nx,ny,node_ij,node_xy,&
        deallocNode_ij,deallocNode_xy)

        implicit none

        class(RegularTriangularTiling)      :: this
        integer,             intent(in)     :: nx
        integer,             intent(in)     :: ny
        integer,             pointer        :: node_ij(:,:)
        real(real64),        pointer        :: node_xy(:,:)
        logical,   optional, intent(in)     :: deallocNode_ij
        logical,   optional, intent(in)     :: deallocNode_xy

        integer,             pointer        :: triangles(:,:)
        integer,             pointer        :: neighbors(:,:)

        integer :: i, j
        integer :: tind

        allocate(triangles(3,2*(nx-1)*(ny-1)))
        allocate(neighbors(3,2*(nx-1)*(ny-1)))

        this%nx = nx
        this%ny = ny

        tind = 0

        do j=1,ny-1
            do i=1,nx-1
                ! add the two triangles making up the box from i to i+1 / j to j +1
                tind = tind + 1
                triangles(1,tind) = indx(i,  j,  nx)
                triangles(2,tind) = indx(i+1,j,  nx)
                triangles(3,tind) = indx(i,  j+1,nx)

                ! the left neighbor between 3 and 1
                if (i > 1) then
                    neighbors(1,tind) = 2*indx(i-1,j,nx)
                else
                    neighbors(1,tind) = -1
                end if

                ! the bottom neighbor between 1 and 2
                if (j > 1) then
                    neighbors(2,tind) = 2*indx(i,j-1,nx)
                else
                    neighbors(2,tind) = -1
                end if

                ! the right neighbor between 2 and 3
                neighbors(2,tind) = 2*indx(i,j,nx)

                tind = tind + 1
                triangles(1,tind) = indx(i+1,j,  nx)
                triangles(2,tind) = indx(i+1,j+1,nx)
                triangles(3,tind) = indx(i,  j+1,nx)

                ! the left neighbor between 3 and 1
                neighbors(1,tind) = 2*indx(i,j,nx)-1

                ! the right neighbor between 1 and 2
                if (i < nx) then
                    neighbors(2,tind) = 2*indx(i+1,j,nx)-1
                else
                    neighbors(2,tind) = -1
                end if

                ! the top neighbor between 2 and 3
                if (j < ny) then
                    neighbors(3,tind) = 2*indx(i,j+1,nx)-1
                else
                    neighbors(3,tind) = -1
                end if
            end do
        end do

        call this%regularTriangularTilingConstructor_set(nx,ny,node_ij,node_xy,&
            triangles,neighbors,deallocNode_ij,deallocNode_xy)
    end subroutine

    subroutine regularTriangularTilingConstructor_set(this,nx,ny,node_ij,node_xy,&
        triangles,neighbors,deallocNode_ij,deallocNode_xy)

        class(RegularTriangularTiling)      :: this
        integer,             intent(in)     :: nx
        integer,             intent(in)     :: ny
        integer,             pointer        :: node_ij(:,:)
        real(real64),        pointer        :: node_xy(:,:)
        integer,             pointer        :: triangles(:,:)
        integer,             pointer        :: neighbors(:,:)
        logical,   optional, intent(in)     :: deallocNode_ij
        logical,   optional, intent(in)     :: deallocNode_xy

        call this%tilingConstructor(nx*ny,node_ij,node_xy,triangles,neighbors,&
            2,size(triangles,2),deallocNode_ij,deallocNode_xy)
    end subroutine

    function indx(i,j,nx)
        integer, intent(in) :: i
        integer, intent(in) :: j
        integer, intent(in) :: nx

        integer :: indx

        indx = i + (j-1)*nx
    end function

    subroutine regularTriangularTilingDestructor(this)

        implicit none

        type(RegularTriangularTiling)  :: this

        call this%tilingDestructor()
    end subroutine

    subroutine doInterpolate(this, zd, ni, xyi, zi)

        implicit none

        class(RegularTriangularTiling)   :: this

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

        class(RegularTriangularTiling)    :: this
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

        integer :: iter, maxIter

        logical :: found

        real(real64) :: px, py

        call error('This function is not yet implemented')

        px = xyi(1)
        py = xyi(2)

        ! we are in big trouble if we iterate more times than there are elements
        maxIter = size(this%elements,2)

        ! do a greedy search starting from nst
        !do iter=1,MAX_ITER

        !end do

        !call trfind ( this%nst, px, py, this%nodeNum, this%node_xy(1,:), this%node_xy(2,:), &
        !    & this%list, this%lptr, this%lend, i1, i2, i3 )

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

        class(RegularTriangularTiling)   :: this

        integer,      intent(in)  :: ni
        real(real64), intent(in)  :: xyi(2,ni)
        logical,      intent(out) :: inside(ni)

        integer :: i, i1, i2, i3

        real(real64) :: px, py
        real(real64) :: xyi2(2)

        call error('isInside not yet implemented')

        do i = 1, ni
            px = xyi(1,i)
            py = xyi(2,i)

            !call trfind ( this%nst, px, py, this%nodeNum, this%node_xy(1,:), this%node_xy(2,:), &
            !    & this%list, this%lptr, this%lend, i1, i2, i3 )

            if ( i1 <= 0 .or. i2 <= 0 .or. i3 <= 0) then
                inside(i) = .false.
            else
                this%nst = i2
                inside(i) = .true.
            end if
        end do
    end subroutine

    function cloneRegularTriangularTiling(this,copyData) result(other)

        implicit none

        class(RegularTriangularTiling)             :: this

        logical,                 intent(in) :: copyData

        class(RegularTriangularTiling), pointer    :: other

        integer :: nx, ny

        integer,      pointer    :: node_ij(:,:)
        real(real64), pointer    :: node_xy(:,:)

        allocate(node_ij(size(this%node_ij,1),size(this%node_ij,2)))
        allocate(node_xy(size(this%node_xy,1),size(this%node_xy,2)))

        if (copyData) then
            node_ij(:,:) = this%node_ij(:,:)
            node_xy(:,:) = this%node_xy(:,:)
        end if

        allocate(other)
        call other%regularTriangularTilingConstructor(this%nx,this%ny,node_ij,node_xy)

    end function

    function clone(this,copyData) result(other)

        implicit none

        class(RegularTriangularTiling)   :: this

        logical,       intent(in) :: copyData

        class(Tiling),           pointer    :: other

        class(RegularTriangularTiling), pointer    :: til

        til => this%cloneRegularTriangularTiling(copyData)

        other => til
    end function
end module
