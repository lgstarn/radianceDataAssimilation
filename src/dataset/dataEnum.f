module dataEnum_mod
    use dictionary_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DataEnum

    type   :: DataEnum
        private
            class(Dictionary), pointer :: enumDict => NULL()
            character(len=1024)        :: name     =  ''

        contains
            procedure :: getName
            procedure :: setName

            procedure :: addEnumStringValuePair
            procedure :: lookupEnumString

            procedure :: clone

            procedure :: dataEnumConstructor
            final     :: dataEnumDestructor
    end type

    type   :: EnumName
        private
            character(:), allocatable :: ename

        contains
            procedure :: clone => cloneEnumName
            final :: enumNameDestructor
    end type

    contains

    subroutine dataEnumConstructor(this,name,numberOfItems)
        implicit none

        class(DataEnum)                 :: this
        character(len=1024), intent(in) :: name
        integer, optional,   intent(in) :: numberOfItems

        call this%setName(name)

        if (present(numberOfItems)) then
            this%enumDict => Dictionary(numberOfItems)
        else
            ! just use a default hash size that isn't too large
            this%enumDict => Dictionary(10)
        end if
    end subroutine

    subroutine dataEnumDestructor(this)
        implicit none

        type(DataEnum)  :: this

        if (associated(this%enumDict)) then
            deallocate(this%enumDict)  ! deallocates all of the contents as well
        end if

    end subroutine

    subroutine enumNameDestructor(this)
        implicit none

        type(EnumName)  :: this

        if (allocated(this%ename)) then
            deallocate(this%ename)
        end if

    end subroutine

    subroutine addEnumStringValuePair(this,enumValue,enumStr)
        implicit none

        class(DataEnum)      :: this
        integer, intent(in)  :: enumValue
        character(len=*), intent(in) :: enumStr

        character(len=256) :: key

        class(EnumName), pointer :: enumNamePtr

        class(*), pointer :: objptr

        allocate(enumNamePtr)

        enumNamePtr%ename = enumStr

        write(key,'(i10)') enumValue

        objptr => enumNamePtr

        call this%enumDict%add(key,objptr)
    end subroutine

    function lookupEnumString(this,enumValue) result(enumStr)
        implicit none

        class(DataEnum)           :: this
        integer, intent(in)       :: enumValue

        character(:), allocatable :: enumStr

        character(len=256) :: key

        class(EnumName), pointer :: enumNamePtr

        class(*), pointer :: objptr

        write(key,'(i10)') enumValue

        if (.not. this%enumDict%hasKey(key)) then
            enumStr = 'Could not find the enum value named ' // adjustl(trim(key))
        end if

        objptr => this%enumDict%get(key)

        select type(objptr)
            class is (EnumName)
                enumNamePtr => objptr
            class default
                call error('Unknown class in enum dictionary.')
        end select

        enumStr = enumNamePtr%ename
    end function

    function getName(this) result(name)
        implicit none

        class(DataEnum)     :: this
        character(len=1024) :: name

        name = this%name
    end function

    subroutine setName(this,name)
        implicit none

        class(DataEnum)     :: this
        character(len=1024) :: name

        this%name = name
    end subroutine

    function clone(this) result(deptr)
        implicit none

        class(DataEnum) :: this
        class(DataEnum), pointer :: deptr
        class(*), pointer :: optr

        class(EnumName), pointer :: enptr

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        integer :: i

        allocate(deptr)

        call deptr%dataEnumConstructor(this%name,this%enumDict%numkeys())

        call this%enumDict%keys(keyNames)

        do i=1,size(keyNames)
            optr => this%enumDict%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (EnumName)
                        ! cast down
                        enptr => optr
                    class default
                        call error('A non EnumName class was found in the attributes dict.')
                end select

                call deptr%enumDict%add(enptr%ename,optr)
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in enum dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)
    end function

    function cloneEnumName(this) result(enptr)
        implicit none

        class(EnumName) :: this
        class(EnumName), pointer :: enptr

        allocate(enptr)
        enptr%ename = this%ename ! automatically allocates enptr%ename
    end function

end module
