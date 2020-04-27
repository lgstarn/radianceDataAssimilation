module dataType_mod
    use iso_fortran_env
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    character, dimension(1), parameter :: void = [achar(0)]

    logical,         parameter :: logicalType = .false.
    integer(int8),   parameter :: byteType    = 0
    integer(int16),  parameter :: shortType   = 0
    integer(int32),  parameter :: intType     = 0
    integer(int64),  parameter :: longType    = 0
    real(real32),    parameter :: realType    = 0.0
    real(real64),    parameter :: doubleType  = 0.0d0

    integer, public, parameter :: LOGICAL_TYPE_NUM = 1
    integer, public, parameter :: BYTE_TYPE_NUM    = 2
    integer, public, parameter :: SHORT_TYPE_NUM   = 3
    integer, public, parameter :: INT_TYPE_NUM     = 4
    integer, public, parameter :: LONG_TYPE_NUM    = 5
    integer, public, parameter :: REAL_TYPE_NUM    = 6
    integer, public, parameter :: DOUBLE_TYPE_NUM  = 7
    integer, public, parameter :: STRING_TYPE_NUM  = 8
    integer, public, parameter :: ENUM_TYPE_NUM    = 9
    integer, public, parameter :: STRINGS_TYPE_NUM = 10

    character(*), public, parameter :: LOGICAL_TYPE_NAME = "logical"
    character(*), public, parameter :: BYTE_TYPE_NAME    = "byte"
    character(*), public, parameter :: SHORT_TYPE_NAME   = "short"
    character(*), public, parameter :: INT_TYPE_NAME     = "integer"
    character(*), public, parameter :: LONG_TYPE_NAME    = "long"
    character(*), public, parameter :: REAL_TYPE_NAME    = "real"
    character(*), public, parameter :: DOUBLE_TYPE_NAME  = "double"
    character(*), public, parameter :: STRING_TYPE_NAME  = "string"
    character(*), public, parameter :: ENUM_TYPE_NAME    = "enum"
    character(*), public, parameter :: STRINGS_TYPE_NAME = "strings"

    public :: findTypeNum
    public :: DataType

    type   :: DataType
        private
            integer                   :: dtypeNum
            character(:), allocatable :: dtypeName

        contains

            generic :: transferToChar =>  &
                & transferToChar_logical, &
                & transferToChar_byte,    &
                & transferToChar_short,   &
                & transferToChar_int,     &
                & transferToChar_long,    &
                & transferToChar_real,    &
                & transferToChar_double

            procedure, private :: transferToChar_logical
            procedure, private :: transferToChar_byte
            procedure, private :: transferToChar_short
            procedure, private :: transferToChar_int
            procedure, private :: transferToChar_long
            procedure, private :: transferToChar_real
            procedure, private :: transferToChar_double

            generic :: transferFromChar =>  &
                & transferFromChar_logical, &
                & transferFromChar_byte,    &
                & transferFromChar_short,   &
                & transferFromChar_int,     &
                & transferFromChar_long,    &
                & transferFromChar_real,    &
                & transferFromChar_double

            procedure, private :: transferFromChar_logical
            procedure, private :: transferFromChar_byte
            procedure, private :: transferFromChar_short
            procedure, private :: transferFromChar_int
            procedure, private :: transferFromChar_long
            procedure, private :: transferFromChar_real
            procedure, private :: transferFromChar_double

            generic :: checkType =>  &
                & checkType_logical, &
                & checkType_byte,    &
                & checkType_short,   &
                & checkType_int,     &
                & checkType_long,    &
                & checkType_real,    &
                & checkType_double

            procedure, private :: checkType_logical
            procedure, private :: checkType_byte
            procedure, private :: checkType_short
            procedure, private :: checkType_int
            procedure, private :: checkType_long
            procedure, private :: checkType_real
            procedure, private :: checkType_double

            procedure, nopass, private :: lookupDataTypeName

            procedure :: compareTypes

            procedure :: isNumeric
            procedure :: getDataTypeNum
            procedure :: getDataTypeName

            procedure :: clone

            procedure :: dataTypeConstructor
            final     :: dataTypeDestructor
    end type

    interface findTypeNum
        module procedure :: findTypeNum_logical
        module procedure :: findTypeNum_byte
        module procedure :: findTypeNum_short
        module procedure :: findTypeNum_int
        module procedure :: findTypeNum_long
        module procedure :: findTypeNum_real
        module procedure :: findTypeNum_double
    end interface

    contains

    subroutine dataTypeConstructor(this,dtypeNum)
        implicit none

        class(DataType)              :: this

        integer,          intent(in) :: dtypeNum

        this%dtypeNum  = dtypeNum
        this%dtypeName = lookupDataTypeName(dtypeNum)
    end subroutine

    subroutine dataTypeDestructor(this)
        implicit none

        type(DataType)  :: this

    end subroutine

    function transferToChar_logical(this,value) result(dval)
        implicit none

        class(DataType)       :: this
        logical, intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_byte(this,value) result(dval)
        implicit none

        class(DataType)          :: this
        integer(int8), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_short(this,value) result(dval)
        implicit none

        class(DataType)          :: this
        integer(int16), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_int(this,value) result(dval)
        implicit none

        class(DataType)          :: this
        integer(int32), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_long(this,value) result(dval)
        implicit none

        class(DataType)          :: this
        integer(int64), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_real(this,value) result(dval)
        implicit none

        class(DataType)       :: this
        real(real32), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    function transferToChar_double(this,value) result(dval)
        implicit none

        class(DataType)       :: this

        real(real64), intent(in)   :: value

        character, dimension(:), allocatable :: dval

        dval = transfer(value,void)
    end function

    subroutine transferFromChar_logical(this,dval,value)
        implicit none

        class(DataType)      :: this

        logical, intent(out) :: value

        character, dimension(:), intent(in)  :: dval

        select case (this%dtypeNum)
            case (LOGICAL_TYPE_NUM)
                value = transfer(dval,logicalType)
            case default
                call error('Unable to transfer to logical from type ' // this%getDataTypeName())
        end select
    end subroutine

    subroutine transferFromChar_byte(this,dval,value)
        implicit none

        class(DataType)           :: this

        integer(int8), intent(out)   :: value

        character, dimension(:), intent(in)  :: dval

        character(*), parameter :: typeName = 'byte'

        include 'dataType_transferFromChar.incl'
    end subroutine

    subroutine transferFromChar_short(this,dval,value)
        implicit none

        class(DataType)     :: this

        integer(int16), intent(out)   :: value

        character, dimension(:)  :: dval

        character(*), parameter :: typeName = 'short'

        include 'dataType_transferFromChar.incl'
    end subroutine

    subroutine transferFromChar_int(this,dval,value)
        implicit none

        class(DataType)     :: this
        integer(int32), intent(out)   :: value

        character, dimension(:)  :: dval

        character(*), parameter :: typeName = 'int'

        include 'dataType_transferFromChar.incl'
    end subroutine

    subroutine transferFromChar_long(this,dval,value)
        implicit none

        class(DataType)     :: this
        integer(int64), intent(out)   :: value

        character, dimension(:)  :: dval

        character(*), parameter :: typeName = 'long'

        include 'dataType_transferFromChar.incl'
    end subroutine

    subroutine transferFromChar_real(this,dval,value)
        implicit none

        class(DataType)  :: this
        real(real32), intent(out)   :: value

        character, dimension(:)  :: dval

        character(*), parameter :: typeName = 'real'

        include 'dataType_transferFromChar.incl'
    end subroutine

    subroutine transferFromChar_double(this,dval,value)
        implicit none

        class(DataType)  :: this
        real(real64), intent(out)   :: value

        character, dimension(:)  :: dval

        character(*), parameter :: typeName = 'double'

        include 'dataType_transferFromChar.incl'
    end subroutine

    function findTypeNum_logical(value) result(typeNum)
        implicit none

        logical, intent(in) :: value

        integer :: typeNum

        typeNum = LOGICAL_TYPE_NUM
    end function

    function findTypeNum_byte(value) result(typeNum)
        implicit none

        integer(int8), intent(in)   :: value

        integer :: typeNum

        typeNum = BYTE_TYPE_NUM
    end function

    function findTypeNum_short(value) result(typeNum)
        implicit none

        integer(int16), intent(in)   :: value

        integer :: typeNum

        typeNum = SHORT_TYPE_NUM
    end function

    function findTypeNum_int(value) result(typeNum)
        implicit none

        integer(int32), intent(in)   :: value

        integer :: typeNum

        typeNum = INT_TYPE_NUM
    end function

    function findTypeNum_long(value) result(typeNum)
        implicit none

        integer(int64), intent(in)   :: value

        integer :: typeNum

        typeNum = LONG_TYPE_NUM
    end function

    function findTypeNum_real(value) result(typeNum)
        implicit none

        real(real32), intent(in) :: value

        integer :: typeNum

        typeNum = REAL_TYPE_NUM
    end function

    function findTypeNum_double(value) result(typeNum)
        implicit none

        real(real64), intent(in) :: value

        integer :: typeNum

        typeNum = DOUBLE_TYPE_NUM
    end function

    subroutine checkType_logical(this,value)
        implicit none

        class(DataType)  :: this
        logical, intent(in)   :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_byte(this,value)
        implicit none

        class(DataType)  :: this
        integer(int8), intent(in)   :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_short(this,value)
        implicit none

        class(DataType)  :: this
        integer(int16), intent(in)   :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_int(this,value)
        implicit none

        class(DataType) :: this
        integer(int32), intent(in) :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_long(this,value)
        implicit none

        class(DataType) :: this
        integer(int64), intent(in) :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_real(this,value)
        implicit none

        class(DataType) :: this
        real(real32), intent(in) :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine checkType_double(this,value)
        implicit none

        class(DataType) :: this
        real(real64), intent(in) :: value

        call this%compareTypes(findTypeNum(value))
    end subroutine

    subroutine compareTypes(this,typeNum)
        implicit none

        class(DataType)     :: this
        integer, intent(in) :: typeNum

        if (this%dtypeNum /= typeNum) then
            call error('Data type mismatch: ' // lookupDataTypeName(this%dtypeNum) // &
                ' vs. ' // lookupDataTypeName(typeNum))
        end if
    end subroutine

    function isNumeric(this) result(numeric)
        implicit none

        class(DataType)  :: this

        logical :: numeric

        numeric = this%dtypeNum > LOGICAL_TYPE_NUM .and. this%dtypeNum <= DOUBLE_TYPE_NUM
    end function

    function getDataTypeNum(this) result(dtypeNum)
        implicit none

        class(DataType)  :: this

        integer :: dtypeNum

        dtypeNum = this%dtypeNum
    end function

    function getDataTypeName(this) result(dtypeName)
        implicit none

        class(DataType) :: this

        character(:), allocatable :: dtypeName

        dtypeName = this%dtypeName
    end function

    function lookupDataTypeName(dtypeNum) result(dtypeName)
        implicit none

        integer :: dtypeNum
        character(:), allocatable :: dTypeName

        select case (dtypeNum)
            case (LOGICAL_TYPE_NUM)
                dtypeName = LOGICAL_TYPE_NAME
            case (BYTE_TYPE_NUM)
                dtypeName = BYTE_TYPE_NAME
            case (SHORT_TYPE_NUM)
                dtypeName = SHORT_TYPE_NAME
            case (INT_TYPE_NUM)
                dtypeName = INT_TYPE_NAME
            case (LONG_TYPE_NUM)
                dtypeName = LONG_TYPE_NAME
            case (REAL_TYPE_NUM)
                dtypeName = REAL_TYPE_NAME
            case (DOUBLE_TYPE_NUM)
                dtypeName = DOUBLE_TYPE_NAME
            case (STRING_TYPE_NUM)
                dtypeName = STRING_TYPE_NAME
            case (ENUM_TYPE_NUM)
                dtypeName = ENUM_TYPE_NAME
            case default
                dtypeName = "unknown"
        end select
    end function

    function clone(this) result(ddptr)
        implicit none

        class(DataType) :: this
        class(DataType), pointer :: ddptr

        allocate(ddptr)

        call ddptr%dataTypeConstructor(this%dtypeNum)
    end function
end module
