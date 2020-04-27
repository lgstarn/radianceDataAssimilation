module dataExtent_mod
    use mpiUtils_mod
    use asciiUtils_mod
    use dataDimension_mod

    implicit none

    private

    type, public   :: DataExtent
        private
            class(DataDimension), pointer :: ddim

            integer             :: localCount       ! The decomposed number of elements on this processor
            integer             :: localStart       ! Where local data starts relative to globalStart
            ! logical             :: unlimited
            ! logical             :: shared           ! Whether the dimension is shared with other objs
            ! logical             :: variableCount

        contains
            procedure           :: getDimension
            procedure           :: getName
            procedure           :: getComparisonDimName
            procedure           :: getGlobalSize
            procedure           :: getGlobalCount
            procedure           :: getGlobalStart
            procedure           :: getGlobalEnd
            procedure           :: getGlobalRange
            procedure           :: getLocalCount
            procedure           :: getLocalStart
            procedure           :: getLocalEnd
            procedure           :: getLocalRange
            procedure           :: getLocalTotalStart
            procedure           :: getLocalTotalEnd
            procedure           :: getLocalTotalRange
            procedure           :: getStagger
            ! procedure           :: isUnlimited
            ! procedure           :: isShared
            ! procedure           :: isReadOnly
            ! procedure           :: isVariableCount

            procedure           :: setLocalRange

            procedure           :: clone

            procedure           :: dataExtentConstructor

            final               :: dataExtentDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataExtentConstructor(this,ddim,localCount,localStart)

        implicit none

        class(DataExtent)                      :: this

        class(DataDimension),       pointer    :: ddim
        integer,          optional, intent(in) :: localCount
        integer,          optional, intent(in) :: localStart
        ! logical,          optional, intent(in) :: shared

        ! logical, optional,   intent(in) :: unlimited
        ! logical, optional,   intent(in) :: variableCount

        this%ddim => ddim

        if     (present(localCount) .and. present(localStart)) then
            call this%setLocalRange(localStart,localCount)
        elseif (present(localCount) .or.  present(localStart)) then
            call error('The local count and local start must both be present.')
        else
            this%localStart = 1
            this%localCount = this%getGlobalCount()
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
    end subroutine

    subroutine dataExtentDestructor(this)
        implicit none

        type(DataExtent)  :: this
    end subroutine

    function getDimension(this) result(ddim)
        implicit none

        class(DataExtent)     :: this

        class(DataDimension), pointer :: ddim

        ddim => this%ddim
    end function

    function getName(this) result(vname)
        implicit none

        class(DataExtent) :: this

        character(:),    pointer  :: vname

        vname => this%ddim%getName()
    end function

    function getComparisonDimName(this) result(compareToDim)
        implicit none

        class(DataExtent) :: this

        character(:),    pointer  :: compareToDim

        compareToDim => this%ddim%getComparisonDimName()
    end function

    function getGlobalSize(this) result(globalSize)
        implicit none

        class(DataExtent) :: this

        integer                   :: globalSize

        globalSize = this%ddim%getGlobalSize()
    end function

    function getGlobalCount(this) result(globalCount)
        implicit none

        class(DataExtent) :: this

        integer                   :: globalCount

        globalCount = this%ddim%getGlobalCount()
    end function

    function getGlobalStart(this) result(globalStart)
        implicit none

        class(DataExtent) :: this

        integer                   :: globalStart

        globalStart = this%ddim%getGlobalStart()
    end function

    function getGlobalEnd(this) result(globalEnd)
        implicit none

        class(DataExtent) :: this

        integer                   :: globalEnd

        globalEnd = this%ddim%getGlobalEnd()
    end function

    subroutine getGlobalRange(this,xs,xe,nx)
        implicit none

        class(DataExtent) :: this

        integer,      intent(out) :: xs, xe, nx

        xs  = this%ddim%getGlobalStart()
        nx  = this%ddim%getGlobalCount()
        xe  = this%ddim%getGlobalEnd()
    end subroutine

    function getLocalCount(this) result(localCount)
        implicit none

        class(DataExtent) :: this

        integer                   :: localCount

        localCount = this%localCount
    end function

    function getLocalStart(this) result(localStart)
        implicit none

        class(DataExtent) :: this

        integer                   :: localStart

        localStart = this%localStart
    end function

    function getLocalEnd(this) result(localEnd)
        implicit none

        class(DataExtent) :: this

        integer                   :: localEnd

        localEnd = this%getLocalStart() + this%localCount - 1
    end function

    subroutine getLocalRange(this,xs,xe,nx)
        implicit none

        class(DataExtent) :: this

        integer,      intent(out) :: xs, xe, nx

        xs  = this%getLocalStart()
        nx  = this%getLocalCount()
        xe  = this%getLocalEnd()
    end subroutine

    function getLocalTotalStart(this) result(localStart)
        implicit none

        class(DataExtent) :: this

        integer                   :: localStart

        localStart = this%localStart + this%getGlobalStart() - 1
    end function

    function getLocalTotalEnd(this) result(localEnd)
        implicit none

        class(DataExtent) :: this

        integer                   :: localEnd

        localEnd = this%getLocalTotalStart() + this%localCount - 1
    end function

    subroutine getLocalTotalRange(this,xs,xe,nx)
        implicit none

        class(DataExtent) :: this

        integer,      intent(out) :: xs, xe, nx

        xs  = this%getLocalTotalStart()
        nx  = this%getLocalCount()
        xe  = this%getLocalTotalEnd()
    end subroutine

    function getStagger(this) result(stagger)
        implicit none

        class(DataExtent) :: this

        integer                   :: stagger

        stagger = this%ddim%getStagger()
    end function

    subroutine setLocalRange(this,xs,nx)
        implicit none

        class(DataExtent) :: this
        integer,      intent(in)  :: xs, nx

        integer :: gst, gnd
        integer :: lgs, lgd

        gst = this%ddim%getGlobalStart()
        gnd = gst + this%ddim%getGlobalCount() - 1
        lgs = gst + xs - 1
        lgd = gst + nx - 1

        if (this%ddim%getGlobalCount() == 0) then
            this%localStart = 1
            this%localCount = 0
        elseif (lgs < 1 .or. &
                lgd < 1 .or. &
                lgs > gnd .or. &
                lgd > gnd) then

            call error('For the extent named ' // trim(this%ddim%getName()) // &
                ', invalid local start/count: ' // int2str(xs) // '/' // int2str(nx) // &
                '. Global start/count: ' // int2str(gst) // '/' // int2str(gnd) // &
                '. Local+global start/end: ' // int2str(lgs) // '/' // int2str(lgd))
        end if

        this%localStart = xs
        this%localCount = nx
    end subroutine

!    function isUnlimited(this) result(unlim)
!        implicit none
!
!        class(DataExtent) :: this
!        logical              :: unlim
!
!        unlim = this%unlimited
!    end function
!
!    function isShared(this) result(shared)
!        implicit none
!
!        class(DataExtent) :: this
!
!        logical                   :: shared
!
!        shared = this%shared
!    end function

!    function isReadOnly(this) result(readOnly)
!        implicit none
!
!        class(DataExtent) :: this
!
!        logical                   :: readOnly
!
!        readOnly = .not. this%shared
!    end function

!    function isVariableCount(this) result(varLen)
!        implicit none
!
!        class(DataExtent) :: this
!        logical              :: varLen
!
!        varLen = this%variableCount
!    end function

    function clone(this) result(ddptr)
        implicit none

        class(DataExtent)          :: this

        class(DataExtent), pointer :: ddptr

        allocate(ddptr)

        call ddptr%dataExtentConstructor(this%ddim%clone(), &
            & localCount=this%localCount,  localStart=this%localStart)
    end function
end module
