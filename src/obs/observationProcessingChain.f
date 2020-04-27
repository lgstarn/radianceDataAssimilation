module observationProcessingChain_mod
    use linkedList_mod

    use observation_mod
    use observationProcessor_mod

    use parallelInfo_mod

    use mpiUtils_mod

    implicit none

    private

    type, public :: ObservationProcessingChain
        class(LinkedList), pointer :: obsProcessors => null()

        contains
            procedure :: applyChain
            procedure :: addProcessor

            procedure, private :: nextProcessor

            procedure :: observationProcessingChainConstructor

            final :: observationProcessingChainDestructor
    end type

    contains

    subroutine observationProcessingChainConstructor(this)
        implicit none

        class(ObservationProcessingChain) :: this

        this%obsProcessors      => LinkedList()
    end subroutine

    subroutine observationProcessingChainDestructor(this)
        implicit none

        type(ObservationProcessingChain)  :: this

        class(*), pointer           :: upointer

        if (associated(this%obsProcessors)) then
            deallocate(this%obsProcessors)
        end if
    end subroutine

    subroutine applyChain(this,pinfo,obsIn,obsOut,stage)
        implicit none

        class(ObservationProcessingChain) :: this

        class(ParallelInfo),   pointer    :: pinfo
        class(Observation),    pointer    :: obsIn
        class(Observation),    pointer    :: obsOut
        integer,               intent(in) :: stage

        class(ObservationProcessor), pointer :: obsProc

        logical :: newObject

        integer :: i

        class(Observation), pointer :: cursor1, cursor2

        call this%obsProcessors%first()

        cursor1 => obsIn
        cursor2 => null()

        do i=1,this%obsProcessors%getListSize()
            obsProc => this%nextProcessor()

            call obsProc%process(pinfo,cursor1,cursor2,stage,newObject)

            if (newObject) then
                deallocate(cursor1)
                nullify(cursor1)
                cursor1 => cursor2
            end if
        end do

        obsOut => cursor1
    end subroutine

    function nextProcessor(this) result(obsProc)
        implicit none

        class(ObservationProcessingChain)   :: this

        class(ObservationProcessor), pointer :: obsProc
        class(*),                    pointer :: o_ptr

        o_ptr => this%obsProcessors%currentValue()

        if (associated(o_ptr)) then
            select type(o_ptr)
                class is (ObservationProcessor)
                    obsProc => o_ptr
                    call this%obsProcessors%next()
                class default
                    call error('Unknown class in obsProcessors list')
            end select
        end if
    end function

    subroutine addProcessor(this,obsProc)
        implicit none

        class(ObservationProcessingChain)   :: this

        class(ObservationProcessor), pointer :: obsProc
        class(*),                    pointer :: o_ptr

        class(*), pointer :: optr

        optr => obsProc
        call this%obsProcessors%add(optr)
    end subroutine
end module
