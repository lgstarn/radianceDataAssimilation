module dataAttribute_mod
    use dataType_mod
    use dataEnum_mod
    use linkedList_mod ! needed because Fortran doesn't work well with arrays of strings
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    character, dimension(1), parameter :: void = [achar(0)]

    public :: DataAttribute

    type   :: DataAttribute
        private
            logical                              :: loaded  = .false.
            character(:),            allocatable :: name
            character, dimension(:), allocatable :: rawValue ! transfered to/from according to dataType
!            character(:),            allocatable :: strValue
            class(LinkedList),       pointer     :: stringList
            integer                              :: maxStrLen = -1

            class(DataType), pointer :: dType

            integer(4)               :: enumValue
            class(DataEnum), pointer :: enumPtr

        contains
            procedure           :: getName

            procedure           :: getDataType
            procedure           :: getDataTypeNum

            procedure           :: isLoaded

            generic, public     :: getNumericValue =>    &
                                       & getLogicalValue,&
                                       & getByteValue,   &
                                       & getShortValue,  &
                                       & getIntValue,    &
                                       & getLongValue,   &
                                       & getRealValue,   &
                                       & getDoubleValue

            procedure, private  :: getLogicalValue
            procedure, private  :: getByteValue
            procedure, private  :: getShortValue
            procedure, private  :: getIntValue
            procedure, private  :: getLongValue
            procedure, private  :: getRealValue
            procedure, private  :: getDoubleValue

            generic, public     :: setNumericValue =>    &
                                       & setLogicalValue,&
                                       & setByteValue,   &
                                       & setShortValue,  &
                                       & setIntValue,    &
                                       & setLongValue,   &
                                       & setRealValue,   &
                                       & setDoubleValue

            procedure, private  :: setLogicalValue
            procedure, private  :: setByteValue
            procedure, private  :: setShortValue
            procedure, private  :: setIntValue
            procedure, private  :: setLongValue
            procedure, private  :: setRealValue
            procedure, private  :: setDoubleValue

            procedure, private  :: setRawValue

            procedure           :: getStringValue
            procedure           :: getStringCount
            procedure           :: getStringList

            procedure           :: addString
!            procedure           :: setStringValue
!            procedure           :: setStringsValue

            procedure           :: getMaxStringLength
            procedure           :: setMaxStringLength

            procedure           :: getEnumValue
            procedure           :: getEnumString

            procedure           :: setName

            generic, public     :: dataAttributeConstructor => &
                & dataAttributeConstructor_main,   &
                & dataAttributeConstructor_byte,   &
                & dataAttributeConstructor_short,  &
                & dataAttributeConstructor_int,    &
                & dataAttributeConstructor_long,   &
                & dataAttributeConstructor_real,   &
                & dataAttributeConstructor_double, &
                & dataAttributeConstructor_str,    &
                & dataAttributeConstructor_enum

            procedure, private  :: dataAttributeConstructor_main
            procedure, private  :: dataAttributeConstructor_byte
            procedure, private  :: dataAttributeConstructor_short
            procedure, private  :: dataAttributeConstructor_int
            procedure, private  :: dataAttributeConstructor_long
            procedure, private  :: dataAttributeConstructor_real
            procedure, private  :: dataAttributeConstructor_double
            procedure, private  :: dataAttributeConstructor_str
            procedure, private  :: dataAttributeConstructor_enum

            procedure           :: clone

            final               :: dataAttributeDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataAttributeConstructor_main(this,dTypeNum,name,dval)
        implicit none

        class(DataAttribute)         :: this

        integer,             intent(in) :: dTypeNum
        character(*),        intent(in) :: name
        character, optional, intent(in) :: dval(:)

        class(DataType),     pointer    :: dType

        call this%setName(name)

        allocate(dType)
        call dType%dataTypeConstructor(dTypeNum)
        this%dtype => dtype

        if (present(dval)) then
            call this%setRawValue(dval)
        end if
    end subroutine

    subroutine dataAttributeConstructor_byte(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        integer(1),       intent(in) :: value

        call this%dataAttributeConstructor(BYTE_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_short(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        integer(2),       intent(in) :: value

        call this%dataAttributeConstructor(SHORT_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_int(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        integer(4),       intent(in) :: value

        call this%dataAttributeConstructor(INT_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_long(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        integer(8),       intent(in) :: value

        call this%dataAttributeConstructor(LONG_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_real(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        real(4),          intent(in) :: value

        call this%dataAttributeConstructor(REAL_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_double(this,name,value)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        real(8),          intent(in) :: value

        call this%dataAttributeConstructor(DOUBLE_TYPE_NUM,name,transfer(value,void))
    end subroutine

    subroutine dataAttributeConstructor_str(this,name,value)
        implicit none

        class(DataAttribute)          :: this

        character(len=*), intent(in)  :: name
        character(len=*), intent(in)  :: value

        class(DataType),  pointer     :: dType

        character(:), pointer :: cptr
        class(*),     pointer :: optr

        allocate(dType)
        call dType%dataTypeConstructor(STRING_TYPE_NUM)

        call this%setName(name)
        this%dtype => dtype
        this%stringList => LinkedList()

        allocate(character(len=len(value)) :: cptr)
        cptr = value
        optr => cptr

        call this%stringList%add(optr)
    end subroutine

    subroutine dataAttributeConstructor_enum(this,name,dtype,enumValue,enumPtr)
        implicit none

        class(DataAttribute)         :: this

        character(len=*), intent(in) :: name
        class(DataType),  pointer    :: dType
        integer(4),       intent(in) :: enumValue
        class(DataEnum),  pointer    :: enumPtr

        call this%setName(name)
        this%dtype => dtype

        this%enumValue =  enumValue
        this%enumPtr   => enumPtr
    end subroutine

    subroutine dataAttributeDestructor(this)
        implicit none

        type(DataAttribute)  :: this

        if (allocated(this%name)) then
            deallocate(this%name)
        end if

        if (allocated(this%rawValue)) then
            deallocate(this%rawValue)
        end if

        if (associated(this%stringList)) then
            deallocate(this%stringList)
        end if

        if (associated(this%dType)) then
            deallocate(this%dType)
        end if
    end subroutine

    function isLoaded(this) result(loaded)
        implicit none

        class(DataAttribute)  :: this
        logical               :: loaded

        loaded = this%loaded
    end function

    function getName(this) result(vname)
        implicit none

        class(DataAttribute)       :: this
        character(:), allocatable  :: vname

        vname = this%name
    end function

    function getDataType(this) result(dType)
        implicit none

        class(DataAttribute)     :: this
        class(DataType), pointer :: dType

        dType => this%dType
    end function

    function getDataTypeNum(this) result(dTypeNum)
        implicit none

        class(DataAttribute) :: this
        integer              :: dTypeNum

        dTypeNum = this%dType%getDataTypeNum()
    end function

    subroutine getLogicalValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        logical, intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getByteValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getByteValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(1), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getByteValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getShortValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(2), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getShortValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getIntValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(4), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getIntValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getLongValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(8), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getLongValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getRealValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        real(4), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getRealValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine getDoubleValue(this,val)
        implicit none

        class(DataAttribute) :: this

        real(8), intent(out) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getDoubleValue for a non-numeric attribute.')
        end if

        call this%dType%transferFromChar(this%rawValue,val)
    end subroutine

    subroutine setLogicalValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        logical, intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getByteValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setByteValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(1), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getByteValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setShortValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(2), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getShortValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setIntValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(4), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getIntValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setLongValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        integer(8), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getLongValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setRealValue(this,val)
        implicit none

        class(DataAttribute)    :: this

        real(4), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getRealValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setDoubleValue(this,val)
        implicit none

        class(DataAttribute) :: this

        real(8), intent(in) :: val

        if (.not. this%dType%isNumeric()) then
            call error('Called getDoubleValue for a non-numeric attribute.')
        end if

        call this%setRawValue(transfer(val,void))
    end subroutine

    subroutine setRawValue(this,rawValue)
        implicit none

        class(DataAttribute) :: this

        character, intent(in) :: rawValue(:)

        this%rawValue = rawValue
        this%loaded   = .true.
    end subroutine

    function getStringValue(this,num) result(val)
        implicit none

        class(DataAttribute) :: this

        integer, optional, intent(in) :: num

        character(:),     allocatable :: val

        class(*),     pointer :: optr
        character(:), pointer :: cptr

        integer :: i, numToUse

        if (present(num)) then
            numToUse = num
        else
            numToUse = 1
        end if

        call this%stringList%first()

        if (i < 1 .or. i > this%stringList%getListSize()) then
            call error('Requested an invalid string number: ' // int2str(i) // '/' // &
                int2str(this%stringList%getListSize()))
        end if

        optr => this%stringList%get(numToUse)

        select type(optr)
            type is (character(len=*))
                cptr => optr
            class default
                call error('Unknown class in strings list')
        end select

        val = cptr
    end function

    function getStringCount(this) result(nstr)
        implicit none

        class(DataAttribute) :: this

        integer :: nstr

        nstr = this%stringList%getListSize()
    end function

    function getStringList(this) result(stringList)
        implicit none

        class(DataAttribute) :: this

        class(LinkedList), pointer :: stringList

        stringList => this%stringList
    end function

    subroutine addString(this,str)
        implicit none

        class(DataAttribute) :: this
        character(:), pointer :: str

        class(*), pointer :: optr

        optr => str

        if (.not. associated(this%stringList)) then
            this%stringList => LinkedList()
        end if

        call this%stringList%add(optr)

        this%loaded   = .true.
    end subroutine

!    subroutine setStringValue(this,val)
!        implicit none
!
!        class(DataAttribute) :: this
!
!        character(*), intent(in) :: val
!
!        if (.not. associated(this%stringsValue)) then
!            allocate(character(len(val)) :: this%stringsValue(1))
!        end if
!
!        this%stringsValue(1) = val
!    end subroutine
!
!    subroutine setStringsValue(this,vals)
!        implicit none
!
!        class(DataAttribute) :: this
!
!        character(:), pointer :: vals(:)
!
!        this%stringsValue => vals
!    end subroutine

    function getMaxStringLength(this) result(maxStrLen)
        implicit none

        class(DataAttribute) :: this

        integer :: maxStrLen

        maxStrLen = this%maxStrLen
    end function

    subroutine setMaxStringLength(this,maxStrLen)
        implicit none

        class(DataAttribute) :: this

        integer, intent(in) :: maxStrLen

        this%maxStrLen = maxStrLen
    end subroutine

    function getEnumValue(this) result(enumVal)
         implicit none

         class(DataAttribute) :: this

         integer :: enumVal

         enumVal = this%enumValue
     end function

    subroutine setEnumValue(this,enumVal)
        implicit none

        class(DataAttribute) :: this

        integer, intent(in) :: enumVal

        this%enumValue = enumVal
    end subroutine

    function getEnumString(this) result(enumStr)
        implicit none

        class(DataAttribute) :: this

        character(:), allocatable :: enumStr

        if (associated(this%enumPtr)) then
            enumStr = this%enumPtr%lookupEnumString(this%enumValue)
        else
            call error('Error: enum pointer was not associated in data attribute ' // &
                & trim(this%name))
        end if
    end function

    subroutine setName(this,name)
        implicit none

        class(DataAttribute) :: this

        character(len=*) :: name

        if (allocated(this%name)) then
            deallocate(this%name)
        end if
        this%name = name
    end subroutine

    function clone(this) result(daptr)
        implicit none

        class(DataAttribute) :: this
        class(DataAttribute), pointer :: daptr

        class(*), pointer :: optr
        character(:), pointer :: cptr, cptr2

        integer :: i

        allocate(daptr)
        call daptr%dataAttributeConstructor(this%dtype%getDataTypeNum(),&
            this%name,this%rawValue)

        if (associated(this%stringList)) then
            daptr%stringList => LinkedList()

            call this%stringList%first()

            do i=1,this%stringList%getListSize()
                optr => this%stringList%currentValue()

                select type(optr)
                    type is (character(len=*))
                        cptr => optr
                    class default
                        call error('Unknown class in strings list')
                end select

                allocate(cptr2,source=cptr)

                call daptr%addString(cptr2)

                call this%stringList%next()
            end do
        end if
        daptr%enumValue = this%enumValue

        if (associated(this%enumPtr)) then
            daptr%enumPtr  => this%enumPtr%clone()
        end if
    end function

end module
