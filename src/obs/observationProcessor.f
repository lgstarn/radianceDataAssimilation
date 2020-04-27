module observationProcessor_mod
    use observation_mod
    use linkedList_mod
    use parallelInfo_mod

    implicit none

    private

    type, abstract, public :: ObservationProcessor
        contains
            procedure(processAbstract), deferred :: process
    end type

    abstract interface
        subroutine processAbstract(this,pinfo,obsIn,obsOut,stage,newObject)
            import ObservationProcessor
            import Observation
            import ParallelInfo

            class(ObservationProcessor)      :: this

            class(ParallelInfo), pointer     :: pinfo
            class(Observation),  pointer     :: obsIn
            class(Observation),  pointer     :: obsOut
            integer,             intent(in)  :: stage
            logical,             intent(out) :: newObject
        end subroutine
    end interface
end module
