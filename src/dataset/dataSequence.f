module dataSequence_mod

    implicit none

    private

    public :: DataSequence

    ! TODO: flesh out this type
    type :: DataSequence
        private
            integer :: test

        contains
            procedure :: dataSequenceConstructor

            final     :: dataSequenceDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataSequenceConstructor(this)
        implicit none

        class(DataSequence) :: this

    end subroutine

    subroutine dataSequenceDestructor(this)
        implicit none

        type(DataSequence) :: this
    end subroutine
end module
