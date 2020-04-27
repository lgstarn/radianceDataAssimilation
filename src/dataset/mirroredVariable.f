module mirroredVariable_mod

    use dictionary_mod

    use dataType_mod
    use dataArray_mod
    use dataShape_mod
    use dataVariable_mod
    use dataAttribute_mod

    use mirroredArray_mod

    use parallelInfo_mod

    use mpiUtils_mod

    implicit none

    private

    public :: MirroredVariable

    type, extends(DataVariable) :: MirroredVariable

        private

        class(MirroredArray), pointer :: mArray => null()

        contains
            procedure :: getMirroredArray

            generic :: addChange =>           &
                                & addChange_logical, &
                                & addChange_byte,    &
                                & addChange_short,   &
                                & addChange_int,     &
                                & addChange_long,    &
                                & addChange_real,    &
                                & addChange_dble

            procedure, private :: addChange_logical
            procedure, private :: addChange_byte
            procedure, private :: addChange_short
            procedure, private :: addChange_int
            procedure, private :: addChange_long
            procedure, private :: addChange_real
            procedure, private :: addChange_dble

            generic :: addChangeRange =>           &
                                & addChangeRange_logical, &
                                & addChangeRange_byte,    &
                                & addChangeRange_short,   &
                                & addChangeRange_int,     &
                                & addChangeRange_long,    &
                                & addChangeRange_real,    &
                                & addChangeRange_dble

            procedure, private :: addChangeRange_logical
            procedure, private :: addChangeRange_byte
            procedure, private :: addChangeRange_short
            procedure, private :: addChangeRange_int
            procedure, private :: addChangeRange_long
            procedure, private :: addChangeRange_real
            procedure, private :: addChangeRange_dble

            generic   :: mirroredVariableConstructor => &
                                & mirroredVariableConstructor_array, &
                                & mirroredVariableConstructor_shape

            procedure, private :: mirroredVariableConstructor_array
            procedure, private :: mirroredVariableConstructor_shape

            procedure :: clone
            procedure :: cloneMirrored
            procedure :: synchronize

            final     :: mirroredVariableDestructor
    end type

    contains

    subroutine mirroredVariableConstructor_array(this,name,mArray)

        implicit none

        class(MirroredVariable)           :: this

        character(len=*),      intent(in) :: name
        class(MirroredArray),  pointer    :: mArray

        class(DataArray),      pointer    :: dArray

        this%mArray => mArray

        dArray => mArray

        call this%dataVariableConstructor(name,dArray,collective=.true.,distributed=.false.)
    end subroutine

    subroutine mirroredVariableConstructor_shape(this,name,dTypeNum,dShape)

        implicit none

        class(MirroredVariable)           :: this

        character(len=*),  intent(in)  :: name
        integer,           intent(in)  :: dTypeNum
        class(DataShape),  pointer     :: dShape

        class(MirroredArray), pointer  :: mArray
        class(DataArray),     pointer  :: dArray

        allocate(mArray)
        call mArray%mirroredArrayConstructor(dtypeNum,dShape)

        this%mArray => mArray

        dArray => mArray

        call this%dataVariableConstructor(name,dArray,collective=.true.,&
            & distributed=.false.)
    end subroutine

    subroutine mirroredVariableDestructor(this)
        implicit none

        type(MirroredVariable)  :: this

        ! dArray and attributes are deallocated in DataVariable
    end subroutine

    function getMirroredArray(this) result(mArray)
        implicit none

        class(MirroredVariable) :: this

        class(MirroredArray), pointer :: mArray

        mArray => this%mArray
    end function

    subroutine addChange_logical(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                         :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim),  intent(in)  :: ind
        logical, dimension(nchanges),       intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_byte(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                         :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim),  intent(in)  :: ind
        integer(int8), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_short(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                          :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim),   intent(in)  :: ind
        integer(int16), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_int(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                          :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim),   intent(in)  :: ind
        integer(int32), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_long(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                          :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim),   intent(in)  :: ind
        integer(int64), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_real(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                        :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim), intent(in)  :: ind
        real(real32), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChange_dble(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredVariable)                        :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(nchanges,ndim), intent(in)  :: ind
        real(real64), dimension(nchanges), intent(in)  :: value

        call this%mArray%addChange(ind,nchanges,ndim,value)
    end subroutine

    subroutine addChangeRange_logical(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        logical,                            intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_byte(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int8),                      intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_short(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int16),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_int(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int32),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_long(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int64),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_real(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real32),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    subroutine addChangeRange_dble(this,ranges,value,ndim)
        implicit none

        class(MirroredVariable)                         :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real64),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        call this%mArray%addChangeRange(ranges,value,ndim)
    end subroutine

    function clone(this,copyData) result(dvptr)
        implicit none

        class(MirroredVariable) :: this

        logical, intent(in) :: copyData

        class(DataVariable), pointer :: dvptr

        class(MirroredVariable), pointer :: mvptr

        mvptr => this%cloneMirrored(copyData)
        dvptr => mvptr
    end function

    function cloneMirrored(this,copyData) result(mvptr)
        implicit none

        class(MirroredVariable) :: this

        logical, intent(in) :: copyData

        class(MirroredVariable), pointer :: mvptr

        class(*), pointer :: optr

        class(DataAttribute), pointer :: daptr

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        integer :: i, ierr

        allocate(mvptr)

        call mvptr%mirroredVariableConstructor(this%getName(),this%mArray%cloneMirrored(copyData))

        call this%getAttributeNames(keyNames)

        do i=1,size(keyNames)
            daPtr => this%getAttribute(keyNames(i),ierr)

            if (ierr == 0) then
                call mvptr%addAttribute(daPtr%clone())
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)
    end function

    subroutine synchronize(this,pinfo)
        implicit none

        class(MirroredVariable)          :: this
        class(ParallelInfo), pointer :: pinfo

        call this%mArray%synchronize(pinfo)
    end subroutine
end module
