!! list.f90
module linkedList_mod
    implicit none

    private

    type link
        private
            class(*),    pointer :: value    => null() ! value stored in link
            class(link), pointer :: previous => null() ! previous value in the list
            class(link), pointer :: next     => null() ! next link in list
            logical :: shouldDeallocate ! if the value should be deallocated on finalization
        contains
            final :: linkDestructor
    end type

    interface link
        procedure linkConstructor ! construct/initialize a link
    end interface

    public :: LinkedList

    type LinkedList
        private
            integer              :: listSize = 0
            class(link), pointer :: firstLink => null() ! first link in list
            class(link), pointer :: lastLink => null()  ! last link in list
            class(link), pointer :: currLink => null()  ! list iterator
            logical              :: shouldDeallocate    ! if the values should be deallocated on finalization
        contains
            procedure :: add         ! add a value to the list after the current link
            procedure :: addBefore   ! add a value to the list before the current link
            procedure :: getListSize ! get the size of the list
            procedure :: delete      ! delete the current pointer from the list
            procedure :: deleteAll   ! delete all of the links ***and values*** from the list
            procedure :: first       ! move the list iterator to the beginning
            procedure :: next        ! increment list iterator
            procedure :: previous    ! decrement list iterator
            procedure :: last        ! move the list iterator to the end
            procedure :: currentValue   ! get value from currLink
            procedure :: get            ! get value from index (leaves iterator at index if no err)
            procedure :: moreValues     ! more values for iterator?
            procedure :: isEmpty        ! return true if list is empty
            final     :: listDestructor ! destroy this list
    end type

    interface LinkedList
        procedure linkedListConstructor ! construct/initialize a link
    end interface

    contains

    function linkedListConstructor(shouldDeallocate) result(this)
        implicit none

        logical, optional, intent(in) :: shouldDeallocate

        class(LinkedList), pointer :: this

        allocate(this)
        this%listSize = 0

        if (present(shouldDeallocate)) then
            this%shouldDeallocate = shouldDeallocate
        else
            this%shouldDeallocate = .true.
        end if
    end function

    subroutine listDestructor(this)
        implicit none

        type(LinkedList) :: this
        class(link), pointer :: next

        call this%first()

        do while (associated(this%currLink))
            next => this%currLink%next
            deallocate(this%currLink)
            this%currLink => next
        end do
    end subroutine

    ! add a value after the current link
    subroutine add(this, value, shouldDeallocate)
        implicit none

        class(LinkedList)              :: this
        class(*), pointer              :: value
        logical,  optional, intent(in) :: shouldDeallocate

        class(link), pointer :: newLink
        class(link), pointer :: cursorLink

        if (.not. associated(this%firstLink)) then
            if (present(shouldDeallocate)) then
                this%firstLink => link(value, null(), null(), shouldDeallocate)
            else
                this%firstLink => link(value, null(), null(), this%shouldDeallocate)
            end if
            this%currLink => this%firstLink
            this%lastLink => this%firstLink
        else
            ! check for the case where the currLink has gone off the end
            if (.not. associated(this%currLink)) then
                this%currLink => this%lastLink
            endif

            ! create a new link that will go between the current link and the next link
            if (present(shouldDeallocate)) then
                newLink => link(value, this%currLink, this%currLink%next, shouldDeallocate)
            else
                newLink => link(value, this%currLink, this%currLink%next, this%shouldDeallocate)
            end if

            if (associated(this%currLink%next)) then
                this%currLink%next%previous => newLink
            else
                this%lastLink => newLink
            endif

            this%currLink%next => newLink

            this%currLink => newLink
        end if

        this%listSize = this%listSize + 1
    end subroutine

    ! add a value before the current link
    subroutine addBefore(this, value, shouldDeallocate)
        implicit none

        class(LinkedList)             :: this
        class(*),          pointer    :: value
        logical, optional, intent(in) :: shouldDeallocate

        class(link), pointer :: newLink
        class(link), pointer :: cursorLink

        if (.not. associated(this%firstLink)) then
            if (present(shouldDeallocate)) then
                this%firstLink => link(value, null(), null(), shouldDeallocate)
            else
                this%firstLink => link(value, null(), null(), this%shouldDeallocate)
            end if
            this%currLink => this%firstLink
            this%lastLink => this%firstLink
        else
            ! deal with the case where the iterator has gone off the list
            if (.not. associated(this%currLink)) then
                this%currLink => this%firstLink
            endif

            ! create a new link that will go between the previous link and the current link
            if (present(shouldDeallocate)) then
                newLink => link(value, this%currLink%previous, this%currLink, shouldDeallocate)
            else
                newLink => link(value, this%currLink%previous, this%currLink, this%shouldDeallocate)
            end if

            if (associated(this%currLink%previous)) then
                this%currLink%previous%next => newLink
            else
                this%firstLink => newLink
            endif

            this%currLink%previous => newLink

            this%currLink => newLink
        end if

        this%listSize = this%listSize + 1
    end subroutine

    function delete(this)
        implicit none

        class(LinkedList) :: this

        class(Link), pointer :: temp

        logical :: delete

        if (associated(this%currLink)) then
            delete = .true.

            if (associated(this%currLink%previous)) then
                this%currLink%previous%next => this%currLink%next
            else
                this%firstLink => this%currLink%next
            endif

            if (associated(this%currLink%next)) then
                this%currLink%next%previous => this%currLink%previous
            else
                this%lastLink => this%currLink%previous
            endif

            temp => this%currLink%next

            deallocate(this%currLink)

            this%currLink => temp

            this%listSize = this%listSize - 1
        else
            delete = .false.
        endif
    end function

    ! NOTE: this function deletes the stored values in the linked list as well.
    subroutine deleteAll(this)
        implicit none

        class(LinkedList) :: this

        class(*), pointer :: currentValue
        class(link), pointer :: next

        call this%first()

        do while (this%moreValues())
            currentValue => this%currentValue()

            deallocate(currentValue)

            call this%next()
        enddo

        do while (associated(this%currLink))
            next => this%currLink%next
            deallocate(this%currLink)
            this%currLink => next
        end do
    end subroutine

    function isEmpty(this)
        implicit none

        class(LinkedList) :: this

        logical :: isEmpty

        if (associated(this%firstLink)) then
            isEmpty = .false.
        else
            isEmpty = .true.
        endif
    end function

    subroutine next(this)
        implicit none

        class(LinkedList) :: this

        if (associated(this%currLink)) then
            this%currLink => this%currLink%next
        endif
    end subroutine

    subroutine previous(this)
        implicit none

        class(LinkedList) :: this

        if (associated(this%currLink)) then
            this%currLink => this%currLink%previous
        endif
    end subroutine

    function moreValues(this)
        implicit none

        class(LinkedList) :: this
        logical :: moreValues

        moreValues = associated(this%currLink)
    end function

    subroutine first(this)
        implicit none

        class(LinkedList) :: this

        this%currLink => this%firstLink
    end subroutine

    subroutine last(this)
        implicit none

        class(LinkedList) :: this

        this%currLink => this%lastLink
    end subroutine

    function currentValue(this)
        implicit none

        class(LinkedList) :: this
        class(*), pointer :: currentValue

        if (associated(this%currLink)) then
            currentValue => this%currLink%value
        else
            currentvalue => null()
        endif
    end function

    function get(this,ind) result(optr)
        implicit none

        class(LinkedList) :: this

        integer, intent(in) :: ind

        class(*), pointer :: optr

        integer :: i

        call this%first()

        do i=1,ind
            if (i == ind) then
                optr => this%currentValue()
            end if

            call this%next()
        end do
    end function

    function getListSize(this)
        implicit none

        class(LinkedList) :: this
        integer           :: getListSize

        getListSize = this%listSize
    end function

    function linkConstructor(value, previous, next, shouldDeallocate) result(linkptr)
        implicit none

        class(*), pointer :: value
        class(link), pointer :: previous
        class(link), pointer :: next
        logical, optional, intent(in) :: shouldDeallocate

        class(link), pointer :: linkptr

        linkptr => null()

        allocate(linkptr)

        linkptr%value => value
        linkptr%next => next
        linkptr%previous => previous

        if (present(shouldDeallocate)) then
            linkptr%shouldDeallocate = shouldDeallocate
        else
            linkptr%shouldDeallocate = .true.
        end if
    end function

    subroutine linkDestructor(this)
        implicit none

        type(link) :: this

        if (associated(this%value) .and. this%shouldDeallocate) then
            deallocate(this%value)
        end if
    end subroutine
end module
