module dataDimension_mod
    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    type, public   :: DataDimension
        private
            character(:), pointer :: name
            character(:), pointer :: compareToDim

            integer             :: globalSize       ! Total dimension size independent of chosen start
            integer             :: globalCount      ! The user-selected count (start + end - 1)
            integer             :: globalStart      ! The user-selected start for all processors
            integer             :: stagger          ! If this dimension is staggered (by 0 or 1)
            ! logical             :: unlimited
            ! logical             :: shared           ! Whether the dimension is shared with other objs

        contains
            procedure           :: getName
            procedure           :: setName
            procedure           :: getComparisonDimName
            procedure           :: setComparisonDimName
            procedure           :: getGlobalSize
            procedure           :: getGlobalCount
            procedure           :: getGlobalStart
            procedure           :: getGlobalEnd
            procedure           :: getGlobalRange
            procedure           :: getStagger
            ! procedure           :: isUnlimited
            ! procedure           :: isShared
            procedure           :: isReadOnly
            ! procedure           :: isVariableCount

            procedure           :: setGlobalRange
            procedure           :: setStagger

            procedure           :: clone

            procedure           :: dataDimensionConstructor

            final               :: dataDimensionDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataDimensionConstructor(this,name,globalSize,globalCount,globalStart,&
        &stagger,compareToDim)

        implicit none

        class(DataDimension)             :: this

        character(len=*),           intent(in) :: name
        integer,                    intent(in) :: globalSize
        integer,          optional, intent(in) :: globalCount
        integer,          optional, intent(in) :: globalStart
        integer,          optional, intent(in) :: stagger
!        logical,          optional, intent(in) :: shared
        character(len=*), optional, intent(in) :: compareToDim

!        logical, optional,   intent(in) :: unlimited
!        logical, optional,   intent(in) :: variableCount

        allocate(character(len=len(name)) :: this%name)
        this%name           = name
        this%globalSize     = globalSize

        if     (present(globalCount) .and. present(globalStart)) then
            call this%setGlobalRange(globalCount,globalStart)
        elseif (present(globalCount) .or.  present(globalStart)) then
            call error('The global count and global start must both be present.')
        else
            this%globalStart = 1
            this%globalCount = globalSize
        end if

        if (present(stagger)) then
            this%stagger = stagger
        else
            this%stagger = 0
        end if

!        if (present(unlimited)) then
!            this%unlimited = unlimited
!        else
!            this%unlimited = .false.
!        end if

!        if (present(shared)) then
!            this%shared         = shared
!        else
!            this%shared         = .false.
!        end if

        if (present(compareToDim)) then
            allocate(character(len=len(compareToDim)) :: this%compareToDim)

            this%compareToDim = compareToDim
        else
            allocate(character(len=len(name)) :: this%compareToDim)

            this%compareToDim = name
        end if

!        if (present(variableCount)) then
!            this%variableCount = variableCount
!        else
!            this%variableCount = .false.
!        end if
    end subroutine

    subroutine dataDimensionDestructor(this)
        implicit none

        type(DataDimension)  :: this
    end subroutine

    function getName(this) result(vname)
        implicit none

        class(DataDimension) :: this

        character(:),     pointer  :: vname

        vname => this%name
    end function

    subroutine setName(this,vname)
        implicit none

        class(DataDimension)     :: this

        character(*),       intent(in) :: vname

        if (this%isReadOnly()) then
            call error('Cannot set the name on a read-only data dimension')
        end if

        if (associated(this%name)) then
            deallocate(this%name)
        end if

        allocate(character(len=len(vname)) :: this%name)

        this%name = vname
    end subroutine

    function getComparisonDimName(this) result(compareToDim)
        implicit none

        class(DataDimension) :: this

        character(:),     pointer  :: compareToDim

        compareToDim => this%compareToDim
    end function

    subroutine setComparisonDimName(this,compareToDim)
        implicit none

        class(DataDimension)  :: this
        character(:), pointer :: compareToDim

        if (this%isReadOnly()) then
            call error('Cannot set the comparison name on a read-only data dimension')
        end if

        if (associated(this%compareToDim)) then
            deallocate(this%compareToDim)
        end if

        allocate(character(len=len(compareToDim)) :: this%compareToDim)

        this%compareToDim = compareToDim
    end subroutine

    function getGlobalSize(this) result(globalSize)
        implicit none

        class(DataDimension) :: this

        integer                    :: globalSize

        globalSize = this%globalSize
    end function

    function getGlobalCount(this) result(globalCount)
        implicit none

        class(DataDimension) :: this

        integer                    :: globalCount

        globalCount = this%globalCount
    end function

    function getGlobalStart(this) result(globalStart)
        implicit none

        class(DataDimension) :: this

        integer                    :: globalStart

        globalStart = this%globalStart
    end function

    function getGlobalEnd(this) result(globalEnd)
        implicit none

        class(DataDimension) :: this

        integer                    :: globalEnd

        globalEnd = this%globalStart + this%globalCount - 1
    end function

    subroutine getGlobalRange(this,xs,xe,nx)
        implicit none

        class(DataDimension) :: this

        integer,       intent(out) :: xs, xe, nx

        xs  = this%getGlobalStart()
        nx  = this%getGlobalCount()
        xe  = this%getGlobalEnd()
    end subroutine

    function getStagger(this) result(stagger)
        implicit none

        class(DataDimension) :: this

        integer :: stagger

        stagger = this%stagger
    end function

    subroutine setStagger(this,stagger)
        implicit none

        class(DataDimension) :: this

        integer,        intent(in) :: stagger

        if (this%isReadOnly()) then
            call error('Cannot set the stagger on a read-only data dimension')
        end if

        this%stagger = stagger
    end subroutine

    subroutine setGlobalRange(this,nx,xs)
        implicit none

        class(DataDimension) :: this

        integer,       intent(in)  :: xs, nx

        if (this%isReadOnly()) then
            call error('Cannot set the global range on a read-only data dimension')
        end if

        if (nx == 0) then
            this%globalStart = 1
            this%globalCount = 0
        else
            if (xs > nx .or. xs < 1) then
                call error('Invalid global range for dimension ' // trim(this%getName()) // &
                    ': ' // int2str(xs) // '/' // int2str(nx))
            end if

            this%globalStart = xs
            this%globalCount = nx
        end if
    end subroutine

!    function isUnlimited(this) result(unlim)
!        implicit none
!
!        class(DataDimension) :: this
!        logical              :: unlim
!
!        unlim = this%unlimited
!    end function
!
!    function isShared(this) result(shared)
!        implicit none
!
!        class(DataDimension) :: this
!
!        logical                    :: shared
!
!        shared = this%shared
!    end function

    function isReadOnly(this) result(readOnly)
        implicit none

        class(DataDimension) :: this

        logical                    :: readOnly

!        readOnly = .not. this%shared
        readOnly = .false.
    end function

!    function isVariableCount(this) result(varLen)
!        implicit none
!
!        class(DataDimension) :: this
!        logical              :: varLen
!
!        varLen = this%variableCount
!    end function

    function clone(this) result(ddptr)
        implicit none

        class(DataDimension) :: this

        class(DataDimension), pointer :: ddptr

        allocate(ddptr)

        call ddptr%dataDimensionConstructor(this%name, this%globalSize, &
            &globalCount=this%globalCount, globalStart=this%globalStart,&
            &    stagger=this%stagger,    compareToDim=this%compareToDim)
    end function
end module
