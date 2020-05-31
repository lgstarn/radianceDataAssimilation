module tiling_mod

    use iso_fortran_env

    use tripack_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    type, abstract, public :: Tiling
        !private
            integer               :: nodeNum               = -1
            integer,      pointer :: node_ij(:,:)          => NULL()
            real(real64), pointer :: node_xy(:,:)          => NULL()
            integer,      pointer :: elements(:,:)         => NULL()
            integer,      pointer :: elementNeighbor(:,:)  => NULL()
            logical               :: deallocNode_ij        = .true.
            logical               :: deallocNode_xy        = .true.
            integer               :: ndim                  = -1
            integer               :: numElements           = -1

        contains
            procedure :: tilingConstructor

            procedure :: getNumElements
!            procedure :: setNumElements

            procedure :: getNumNodes
            procedure :: getNodeLoci
            procedure :: getNodeLociIndices
            procedure :: getElementIndices
            procedure :: getElementNeighbors
            generic   :: interpolate => &
                & interpolate_byte,  &
                & interpolate_short, &
                & interpolate_int,   &
                & interpolate_long,  &
                & interpolate_real,  &
                & interpolate_dble

            procedure :: interpolate_byte
            procedure :: interpolate_short
            procedure :: interpolate_int
            procedure :: interpolate_long
            procedure :: interpolate_real
            procedure :: interpolate_dble

            procedure :: getAllElementsInRange

            procedure(doInterpolateAbstract), deferred :: doInterpolate
            procedure(searchAbstract),        deferred :: search
            procedure(isInsideAbstract),      deferred :: isInside
            procedure(cloneAbstract),         deferred :: clone

            procedure :: tilingDestructor ! clean up all allocated variables
    end type

    abstract interface
        subroutine doInterpolateAbstract(this, zd, ni, xyi, zi)
            import Tiling
            import real64

            class(Tiling)             :: this

            real(real64), intent(in)  :: zd(:)
            integer,      intent(in)  :: ni
            real(real64), intent(in)  :: xyi(2,ni)
            real(real64), intent(out) :: zi(ni)
        end subroutine

        subroutine searchAbstract(this,xyi,nodeIndices,baryCoords,edge)
            import Tiling
            import real64

            class(Tiling)              :: this

            real(real64), intent(in)   :: xyi(2)
            integer,      intent(out)  :: nodeIndices(:)
            real(real64), intent(out)  :: baryCoords(:)
            integer,      intent(out)  :: edge
        end subroutine

        subroutine isInsideAbstract(this,ni,xyi,inside)
            import Tiling
            import real64

            class(Tiling)             :: this

            integer,      intent(in)  :: ni
            real(real64), intent(in)  :: xyi(2,ni)
            logical,      intent(out) :: inside(ni)
        end subroutine

        function cloneAbstract(this,copyData) result(tptr)
            import Tiling

            class(Tiling)             :: this

            logical,       intent(in) :: copyData
            class(Tiling), pointer    :: tptr
        end function
    end interface

    contains

    subroutine tilingConstructor(this,nodenum,node_ij,node_xy,elements,&
        & elementNeighbor,ndim,nelements,deallocNode_ij,deallocNode_xy)

        implicit none

        class(Tiling)                 :: this

        integer,           intent(in) :: nodenum
        integer,           intent(in) :: ndim
        integer,           intent(in) :: nelements
        integer,           pointer    :: node_ij(:,:)         ! (2,nodenum)
        real(real64),      pointer    :: node_xy(:,:)         ! (2,nodenum)
        integer,           pointer    :: elements(:,:)        ! (ndim,nelements)
        integer,           pointer    :: elementNeighbor(:,:) ! (ndim,nelements)
        logical, optional, intent(in) :: deallocNode_ij
        logical, optional, intent(in) :: deallocNode_xy

        if (present(deallocNode_ij)) then
            this%deallocNode_ij = deallocNode_ij
        else
            this%deallocNode_ij = .true.
        end if

        if (present(deallocNode_xy)) then
            this%deallocNode_xy = deallocNode_xy
        else
            this%deallocNode_xy = .true.
        end if

        this%nodeNum         =  nodenum
        this%ndim            =  ndim
        this%numElements     =  nelements
        this%node_ij         => node_ij
        this%node_xy         => node_xy
        this%elements        => elements
        this%elementNeighbor => elementNeighbor
    end subroutine

    subroutine tilingDestructor(this)
        implicit none

        class(Tiling)  :: this

        if (this%deallocNode_ij .and. associated(this%node_ij)) then
            deallocate(this%node_ij)
            nullify(this%node_ij)
        end if

        if (this%deallocNode_xy .and. associated(this%node_xy)) then
            deallocate(this%node_xy)
            nullify(this%node_xy)
        end if

        if (associated(this%elements)) then
            deallocate(this%elements)
            nullify(this%elements)
        end if

        if (associated(this%elementNeighbor)) then
            deallocate(this%elementNeighbor)
            nullify(this%elementNeighbor)
        end if
    end subroutine

    function getNumNodes(this) result(numNodes)
        implicit none

        class(Tiling) :: this

        integer       :: numNodes

        numNodes = this%nodenum
    end function

    function getNumElements(this) result(numElements)
        implicit none

        class(Tiling) :: this

        integer       :: numElements

        numElements = this%numElements
    end function

    function getNodeLoci(this) result(node_xy)
        implicit none

        class(Tiling)         :: this

        real(real64), pointer :: node_xy(:,:)

        node_xy => this%node_xy
    end function

    function getNodeLociIndices(this) result(node_ij)
        implicit none

        class(Tiling)    :: this

        integer, pointer :: node_ij(:,:)

        node_ij => this%node_ij
    end function

    function getElementIndices(this) result(elements)
        implicit none

        class(Tiling)    :: this

        integer, pointer :: elements(:,:)

        elements => this%elements
    end function

    function getElementNeighbors(this) result(neighbors)
        implicit none

        class(Tiling)    :: this

        integer, pointer :: neighbors(:,:)

        neighbors => this%elementNeighbor
    end function

    subroutine getAllElementsInRange(this, xmin, xmax, ymin, ymax, indices, numInRange)
        implicit none

        class(Tiling)             :: this

        real(real64), intent(in)  :: xmin
        real(real64), intent(in)  :: xmax
        real(real64), intent(in)  :: ymin
        real(real64), intent(in)  :: ymax
        integer,      intent(out) :: indices(:)
        integer,      intent(out) :: numInRange

        integer :: i, l, ind, maxind

        real(real64) :: x, y

        real(real64) :: xminw, xmaxw, yminw, ymaxw

        logical :: outside

        xminw = xmin
        xmaxw = xmax
        yminw = ymin
        ymaxw = ymax

        maxind = size(indices,1)

        numInRange = 0

        do i=1,this%numElements
            outside = .false.

            do l=1,size(this%elements,1)
                ind = this%elements(l,i)

                x = this%node_xy(1,ind)
                y = this%node_xy(2,ind)

                if (x .lt. xminw .or. x .gt. xmaxw) then
                    outside = .true.
                    exit
                end if

                if (y .lt. yminw .or. y .gt. ymaxw) then
                    outside = .true.
                    exit
                end if
            end do

            if (outside) then
                cycle
            end if

            numInRange = numInRange + 1

            indices(numInRange) = i
        end do

        if (numInRange > maxind) then
            write(msgstr,*) 'Error in getAllTrianglesInRange: outind is larger than maxind:',numInRange,maxind
            call error(msgstr)
        end if
    end subroutine

    subroutine interpolate_byte(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        integer(int8), intent(in)  :: zd(this%nodeNum)

        integer,       intent(in)  :: ni
        real(real64),  intent(in)  :: xyi(2,ni)
        real(real64),  intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine

    subroutine interpolate_short(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        integer(int16), intent(in)  :: zd(this%nodeNum)

        integer,        intent(in)  :: ni
        real(real64),   intent(in)  :: xyi(2,ni)
        real(real64),   intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine

    subroutine interpolate_int(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        integer(int32), intent(in)  :: zd(this%nodeNum)

        integer,        intent(in)  :: ni
        real(real64),   intent(in)  :: xyi(2,ni)
        real(real64),   intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine

    subroutine interpolate_long(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        integer(int64), intent(in)  :: zd(this%nodeNum)

        integer,        intent(in)  :: ni
        real(real64),   intent(in)  :: xyi(2,ni)
        real(real64),   intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine

    subroutine interpolate_real(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        real(real32), intent(in)  :: zd(this%nodeNum)

        integer,      intent(in)  :: ni
        real(real64), intent(in)  :: xyi(2,ni)
        real(real64), intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine

    subroutine interpolate_dble(this, zd, ni, xyi, zi)
        implicit none

        class(Tiling) :: this

        real(real64), intent(in)  :: zd(this%nodeNum)

        integer,      intent(in)  :: ni
        real(real64), intent(in)  :: xyi(2,ni)
        real(real64), intent(out) :: zi(ni)

        include 'tiling_interpolate.incl'
    end subroutine
end module
