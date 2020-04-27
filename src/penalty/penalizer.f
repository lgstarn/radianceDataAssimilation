module penalizer_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use penalty_mod
    use linkedList_mod
    use mpiUtils_mod

    implicit none

    private

    type, public :: Penalizer
        private
            real(8),           pointer :: weights(:) => null()
            class(LinkedList), pointer :: penalties  => null()

        contains
            procedure :: penalizerConstructor

            procedure :: addPenalty
            procedure :: getPenalty

            procedure, private :: nextPenalty

            final :: PenalizerDestructor
    end type

    contains

    subroutine penalizerConstructor(this, nweights)
        implicit none

        class(Penalizer)           :: this
        integer,        intent(in) :: nweights

        this%penalties => LinkedList()

        allocate(this%weights(nweights))
        this%weights = 0.d0
    end subroutine

    subroutine penalizerDestructor(this)
        implicit none

        type(Penalizer)  :: this

        class(Penalty), pointer :: pen

        integer :: i

        call this%penalties%deleteAll()
        deallocate(this%penalties)

        deallocate(this%weights)
    end subroutine

    subroutine addPenalty(this, pen, weight)
        implicit none

        class(Penalizer)        :: this
        class(Penalty), pointer :: pen
        real(8),     intent(in) :: weight

        class(*), pointer :: optr

        integer :: ind

        optr => pen

        call this%penalties%add(optr)

        ind = this%penalties%getListSize()

        if (ind > size(this%weights)) then
            write(msgstr,*) 'More penalties added ',this%penalties%getListSize(),'than nweights',size(this%weights)
            call print(msgstr)
        end if

        this%weights(ind) = weight
    end subroutine

    function nextPenalty(this) result(pen)
        implicit none

        class(Penalizer)        :: this

        class(Penalty), pointer :: pen
        class(*),       pointer :: o_ptr

        o_ptr => this%penalties%currentValue()

        if (associated(o_ptr)) then
            select type(o_ptr)
                class is (Penalty)
                    pen => o_ptr
                    call this%penalties%next()
                class default
                    call error('Unknown class in penalties list')
            end select
        end if
    end function

    subroutine getPenalty(this, control, state, bhalf, pen, grad)
        implicit none

        class(Penalizer) :: this

        class(AbstractVector),         pointer :: control
        class(AbstractVector),         pointer :: state
        class(AbstractVectorOperator), pointer :: bhalf
        real(8),                   intent(out) :: pen
        real(8),   dimension(:), intent(inout) :: grad

        class(Penalty), pointer :: penptr
        real(8) :: penval
        real(8), dimension(:), pointer :: pengrad

        integer :: i

        grad = 0.d0
        pen  = 0.d0

        allocate(pengrad(size(grad)))

        call this%penalties%first()

        do i=1,this%penalties%getListSize()
            pengrad = 0.d0

            penptr => this%nextPenalty()

            call penptr%applyPenalty(control, state, bhalf, penval, pengrad)

            pen  = pen  + this%weights(i)*penval**2

            write(msgstr,*) 'penalty',i,this%weights(i)*penval**2
            call print(msgstr)

            grad = grad + 2.0d0*this%weights(i)*pengrad*penval
        end do

        deallocate(pengrad)
    end subroutine
end module
