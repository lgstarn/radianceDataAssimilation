module parallelInfo_mod
    use mpi
    use parallelDecomposition_mod
    use parallelConstants_mod
    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    type, public   :: ParallelInfo
        private
            integer                               :: parallelType
            integer                               :: comm
            integer                               :: rank_comm
            integer                               :: size_comm
            class(ParallelDecomposition), pointer :: decomp => null()

        contains
            procedure :: getParallelType
            procedure :: setParallelType
            procedure :: getParallelDecomposition
            procedure :: setParallelDecomposition
            procedure :: getCommunicator
            procedure :: setCommunicator
            procedure :: getRank
            procedure :: getCommSize

            procedure :: getMirror

            procedure :: clone

            procedure :: parallelInfoConstructor
            final     :: parallelInfoDestructor
    end type

    contains

    subroutine parallelInfoConstructor(this,parallelType,comm,decomp)
        implicit none

        class(ParallelInfo)              :: this

        integer,                                intent(in) :: parallelType
        integer,                      optional, intent(in) :: comm
        class(ParallelDecomposition), optional, pointer    :: decomp

        this%parallelType = parallelType

        if (present(comm)) then
            this%comm = comm
        else
            this%comm = MPI_COMM_WORLD
        end if

        if (parallelType == LOCAL_PARALLEL_TYPE) then
            this%rank_comm = 0
            this%size_comm = 1
        else
            ! from mpi utils
            call getRankAndSize(this%rank_comm,this%size_comm,this%comm)
        end if

        if (present(decomp)) then
            this%decomp => decomp
        else if (parallelType == DISTRIBUTED_PARALLEL_TYPE) then
            call error('The parallel decomposition is required with the distributed parallel type.')
        else
            this%decomp => null()
        end if
     end subroutine

    subroutine parallelInfoDestructor(this)
        implicit none

        type(ParallelInfo)  :: this

        ! don't deallocate the decomp as it might be shared
    end subroutine

    function getParallelType(this) result(parallelType)
        implicit none

        class(ParallelInfo)   :: this

        integer :: parallelType

        parallelType = this%parallelType
    end function

    subroutine setParallelType(this,parallelType)
        implicit none

        class(ParallelInfo)   :: this

        integer, intent(in) :: parallelType

        this%parallelType = parallelType
    end subroutine

    function getParallelDecomposition(this) result(decomp)
        implicit none

        class(ParallelInfo)   :: this

        class(ParallelDecomposition), pointer :: decomp

        decomp => this%decomp
    end function

    subroutine setParallelDecomposition(this,decomp)
        implicit none

        class(ParallelInfo)   :: this

        class(ParallelDecomposition), pointer :: decomp

        this%decomp => decomp
    end subroutine

    function getCommunicator(this) result(comm)
        implicit none

        class(ParallelInfo)   :: this

        integer :: comm

        comm = this%comm
    end function

    subroutine setCommunicator(this,comm)
        implicit none

        class(ParallelInfo)   :: this

        integer, intent(in) :: comm

        this%comm = comm

        if (this%parallelType > LOCAL_PARALLEL_TYPE) then
            call getRankAndSize(this%rank_comm,this%size_comm,comm)
        end if
    end subroutine

    function getRank(this) result(rank)
        implicit none

        class(ParallelInfo), target :: this

        integer :: rank

        rank = this%rank_comm
    end function

    function getCommSize(this) result(size)
        implicit none

        class(ParallelInfo) :: this

        integer :: size

        size = this%size_comm
    end function

    function getMirror(this) result(pinfo)
        implicit none

        class(ParallelInfo), target :: this

        class(ParallelInfo), pointer :: pinfo

        if (this%getParallelType() == LOCAL_PARALLEL_TYPE .or. &
            this%getParallelType() == MIRRORED_PARALLEL_TYPE) then
            pinfo => this
        else ! distributed type
            allocate(pinfo)
            call pinfo%parallelInfoConstructor(MIRRORED_PARALLEL_TYPE,this%comm)
        end if
    end function

    function clone(this) result(piptr)
        implicit none

        class(ParallelInfo) :: this
        class(ParallelInfo), pointer :: piptr

        allocate(piptr)

        call piptr%parallelInfoConstructor(this%parallelType,&
            this%comm,this%decomp)
    end function
end module
