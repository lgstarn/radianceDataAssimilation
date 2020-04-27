module parallelTopology_mod
    use mpiUtils_mod

    implicit none

    private

    type, public :: ParallelTopology
!        private
            integer                        :: rootComm       ! root communicator (e.g. world)
            integer                        :: rootCommRank   ! the rank of this PE in root
            integer                        :: rootCommSize   ! the number of PEs in root

        contains
            procedure :: getRootComm
            procedure :: getRootCommRank
            procedure :: getRootCommSize

            procedure :: parallelTopologyConstructor
            final     :: parallelTopologyDestructor
    end type

    contains

    subroutine parallelTopologyConstructor(this,rootComm)

        class(ParallelTopology) :: this

        integer, intent(in) :: rootComm

        integer :: ierr

        this%rootComm  = rootComm

        call MPI_Comm_rank(rootComm,this%rootCommRank,ierr); call mpichk(ierr,&
            'In ParallelTopology constructor MPI_Comm_rank:')

        call MPI_Comm_size(rootComm,this%rootCommSize,ierr); call mpichk(ierr,&
            'In ParallelTopology constructor MPI_Comm_size:')
    end subroutine

    subroutine parallelTopologyDestructor(this)
        type(ParallelTopology) :: this

    end subroutine

    function getRootComm(this) result(rootComm)
        implicit none

        class(ParallelTopology) :: this

        integer :: rootComm

        rootComm = this%rootComm
    end function

    function getRootCommRank(this) result(rootCommRank)
        implicit none

        class(ParallelTopology) :: this

        integer :: rootCommRank

        rootCommRank = this%rootCommRank
    end function

    function getRootCommSize(this) result(rootCommSize)
        implicit none

        class(ParallelTopology) :: this

        integer :: rootCommSize

        rootCommSize = this%rootCommSize
    end function

end module
