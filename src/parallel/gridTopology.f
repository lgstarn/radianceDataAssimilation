module gridTopology_mod

    use mpiUtils_mod
    use parallelTopology_mod

    implicit none

    private

    type, public, extends(ParallelTopology) :: GridTopology
        private
            integer                        :: ndims          ! number of dimensions in this topology
            integer, dimension(:), pointer :: subcomms       ! the split communicators (1...ndims)

        contains
            procedure :: getNumDims
            procedure :: getSubCommunicators

            procedure :: gridTopologyConstructor
            final     :: gridTopologyDestructor
    end type

    contains

    subroutine gridTopologyConstructor(this,rootComm,ndims)

        implicit none

        class(GridTopology) :: this

        integer, intent(in) :: rootComm
        integer, intent(in) :: ndims

        integer :: comm
        integer :: comm_cart
        integer :: nprocs
        integer :: i
        integer :: ierr
        integer, dimension(ndims) :: dims
        logical, dimension(ndims) :: periods, remdims

        call this%parallelTopologyConstructor(rootComm)

        this%ndims            = ndims

        allocate(this%subcomms(ndims))

        ! set them to zero
        dims = 0
        periods = .false.
        remdims = .false.

        ! divide the processors in the rootComm into a cartesian grid
        call MPI_Dims_create(this%rootCommSize,ndims,dims,ierr)
        call mpichk(ierr,'In GridTopology constructor MPI_Dims_create:')

        ! make a new communicator comm_cart with the cartesian grid attached
        call MPI_Cart_create(rootComm,ndims,dims,periods,.true.,comm_cart,ierr)
        call mpichk(ierr,'In GridTopology MPI_Cart_create:')

        do i=1,ndims
            ! we will only keep one dimension per subcomm
            remdims(i) = .true.

            ! Further divide the comm_cart into subgroups with only one dimension each
            call MPI_Cart_sub(comm_cart,remdims,this%subcomms(i),ierr)
            call mpichk(ierr,'In GridTopology MPI_cart_sub:')

            ! reset the dimension
            remdims(i) = .false.
        end do

        ! free the cartesian-grid communicator as we will only use the single subcomm
        call MPI_Comm_free(comm_cart,ierr)
        call mpichk(ierr,'In GridTopology MPI_Comm_free:')
    end subroutine

    subroutine gridTopologyDestructor(this)
        implicit none

        type(GridTopology) :: this

        integer :: i, ierr

        if (associated(this%subcomms)) then
            do i=1,size(this%subcomms)
                call MPI_Comm_free(this%subcomms(i),ierr)
                call mpichk(ierr,'In GridTopology destructor MPI_Comm_free:')
            end do

            deallocate(this%subcomms)
        end if
    end subroutine

    function getNumDims(this) result(ndims)
        implicit none

        class(GridTopology) :: this

        integer :: ndims

        ndims = this%ndims
    end function

    function getSubCommunicators(this) result(subcomms)
        implicit none

        class(GridTopology) :: this

        integer, allocatable :: subcomms(:)

        subcomms = this%subcomms
    end function
end module
