module parallelConstants_mod
    implicit none

    integer, public, parameter :: LOCAL_PARALLEL_TYPE       = 1
    integer, public, parameter :: MIRRORED_PARALLEL_TYPE    = 2
    integer, public, parameter :: DISTRIBUTED_PARALLEL_TYPE = 3
end module
