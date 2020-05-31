module dictionary_mod
    use linkedList_mod

    implicit none

    private

    integer, public, parameter :: DICT_KEY_LENGTH = 256

    integer, parameter :: defaultHashSize  = 4993
    integer, parameter :: multiplier = 31

    type HashBin
        type(LinkedList), pointer :: list => null()
    end type

    type DictionaryItem
        character(len=:), allocatable  :: key
        class(*), pointer              :: value
        logical                        :: shouldDeallocate

        contains
            final     :: dictionaryItemDestructor
    end type

    interface DictionaryItem
        procedure :: dictionaryItemConstructor ! allow generic instantiation
    end interface

    public :: Dictionary

    type Dictionary
        private
            type(HashBin), pointer, dimension(:) :: table => null()

            integer                              :: hashSize = 0
            integer                              :: nkeys = 0

        contains

            procedure, private :: getItem ! get the DictionaryItem holding the key/value pair

            procedure :: add ! add an element associated with the key
            procedure :: get ! get the element associated with a key
            procedure :: keys ! get the keys in this Dictionary
            procedure :: numkeys ! get the keys in this Dictionary
            procedure :: delete ! delete the element associated with a key
            procedure :: hasKey ! is an element associated with a key?

            procedure, private :: hashkey ! get the hashkey associated with a string

            final :: dictionaryDestructor ! clean up all allocated variables

    end type

    interface Dictionary
        procedure dictionaryConstructor ! allow generic instantiation
    end interface

    contains

    function dictionaryItemConstructor(key, value, shouldDeallocate) result(ditem)
        implicit none

        character(len=*),  intent(in) :: key
        class(*), pointer, intent(in) :: value
        logical, optional, intent(in) :: shouldDeallocate

        class(DictionaryItem), pointer :: ditem

        allocate(ditem)
        allocate(character(len=len_trim(adjustl(key))) :: ditem%key)
        ditem%key = adjustl(trim(key))
        ditem%value => value

        if (present(shouldDeallocate)) then
            ditem%shouldDeallocate = shouldDeallocate
        else
            ditem%shouldDeallocate = .true.
        end if
    end function

    subroutine dictionaryItemDestructor(this)
        implicit none

        type(DictionaryItem) :: this

        if (associated(this%value) .and. this%shouldDeallocate) then
            deallocate(this%value)
        end if

    end subroutine

    function dictionaryConstructor(hashSize) result(this)
        implicit none
        integer, intent(in), optional :: hashSize

        class(Dictionary), pointer :: this

        integer :: i

        allocate(this)
        if (present(hashSize)) then
            this%hashSize = hashSize
        else
            this%hashSize = defaultHashSize
        endif

        allocate(this%table(this%hashSize))
    end function

    subroutine dictionaryDestructor(this)
        implicit none

        type(Dictionary)               :: this

        type(HashBin),         pointer :: bin
        type(LinkedList),      pointer :: list
        class(*),              pointer :: upointer
        class(DictionaryItem), pointer :: item

        integer :: i

        do i = 1,size(this%table)
            if (associated(this%table(i)%list)) then
                list => this%table(i)%list
                call list%first()

                do while (list%moreValues())
                    upointer => list%currentValue()

                    ! item value will be deallocated in DictionaryItem destructor if needed
                    deallocate(upointer)

                    call list%next()
                end do

                bin => this%table(i)

                if (associated(bin%list)) then
                    deallocate(bin%list)
                end if
            end if
        end do

        deallocate(this%table)
    end subroutine

    subroutine add(this, key, value, shouldDeallocate)
        implicit none

        class(Dictionary)             :: this
        character(len=*),  intent(in) :: key
        class(*), pointer, intent(in) :: value
        logical, optional, intent(in) :: shouldDeallocate

        class(DictionaryItem), pointer :: item
        class(*),              pointer :: upointer
        integer                        :: hash

        item => this%getItem(key)

        if (associated(item)) then
            if (associated(item%value) .and. item%shouldDeallocate) then
                deallocate(item%value)
            end if
            item%value => value
        else
            item => DictionaryItem(key,value,shouldDeallocate)

            upointer => item

            hash = this%hashkey(key)
            if (.not. associated(this%table(hash)%list)) then
                allocate(this%table(hash)%list)
            endif

            call this%table(hash)%list%add(upointer)

            this%nkeys = this%nkeys + 1
        end if
    end subroutine

    subroutine keys(this,keyNames)
        implicit none

        class(Dictionary)            :: this
        class(DictionaryItem), pointer :: item
        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames
        type(LinkedList), pointer    :: list
        class(*), pointer            :: upointer

        integer :: hashIndex, keyIndex

        allocate(keyNames(this%numkeys()))

        keyIndex = 1

        do hashIndex = 1,size(this%table)
            if (associated(this%table(hashIndex)%list)) then
                list => this%table(hashIndex)%list
                call list%first()

                do while (list%moreValues())
                    upointer => list%currentValue()

                    select type(upointer)
                       class is (DictionaryItem)
                          item => upointer
                    end select

                    keyNames(keyIndex) = item%key
                    keyIndex = keyIndex + 1

                    call list%next()
                end do
            end if
        end do
    end subroutine

    function numkeys(this)
        implicit none

        class(Dictionary) :: this
        integer           :: numkeys

        numkeys = this%nkeys

    end function

    function delete(this, key)
        implicit none

        class(Dictionary)            :: this
        character(len=*), intent(in) :: key
        logical :: delete

        type(LinkedList), pointer    :: list
        class(*), pointer            :: upointer
        class(DictionaryItem), pointer :: item
        integer                      :: hash

        hash = this%hashkey(key)

        delete = .false.

        if (associated(this%table(hash)%list)) then
            list => this%table(hash)%list

            call list%first()

            do while (list%moreValues())
                upointer => list%currentValue()

                select type(upointer)
                   class is (DictionaryItem)
                      item => upointer
                end select

                if (item%key .eq. key) then
                    delete = list%delete()
                    deallocate(upointer)
                    this%nkeys = this%nkeys - 1
                    exit
                endif

                call list%next()
            enddo
        endif
    end function

    function get(this, key) result(value)
        implicit none

        class(Dictionary)            :: this
        character(len=*), intent(in) :: key
        class(*), pointer            :: value

        type(DictionaryItem), pointer    :: item

        item => this%getItem(key)

        if (associated(item)) then
            value => item%value
        else
            value => null()
        endif
    end function

    function hasKey( this, key ) result(has)
        implicit none

        class(Dictionary)            :: this
        character(len=*), intent(in) :: key
        logical                      :: has

        type(DictionaryItem), pointer   :: item

        item => this%getItem(key)

        has = associated(item)
    end function

    function getItem(this, key)
        implicit none

        class(Dictionary)             :: this
        character(len=*),  intent(in) :: key
        type(DictionaryItem), pointer :: getItem

        type(LinkedList), pointer     :: list
        integer                       :: hash
        class(*), pointer             :: upointer
        type(DictionaryItem), pointer :: item

        character(len=:), allocatable :: shortKey

        getItem => null()

        allocate(character(len=len_trim(adjustl(key))) :: shortKey)

        shortKey = adjustl(trim(key))

        hash = this%hashkey(shortKey)

        if (associated(this%table(hash)%list)) then
            list => this%table(hash)%list

            call list%first()

            do while (list%moreValues())
                upointer => list%currentValue()

                select type(upointer)
                   class is (DictionaryItem)
                      item => upointer
                end select

                if (trim(item%key) == shortKey) then
                    getItem => item
                    exit
                endif

                call list%next()
            enddo
        endif
    end function

    integer function hashkey(this,key)
        implicit none

        class(Dictionary)            :: this

        character(len=*), intent(in) :: key

        integer                      :: hash
        integer                      :: i

        character(len=len(key)) :: shortKey

        hashkey = 0

        shortKey = adjustl(trim(key))

        do i = 1,min(len(shortKey),DICT_KEY_LENGTH)
            hashkey = multiplier * hashkey + ichar(shortKey(i:i))
        enddo

        hashkey = 1 + modulo( hashkey-1, this%hashSize )
    end function
end module
