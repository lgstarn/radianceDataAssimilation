module mirroredArray_mod

    use iso_fortran_env
    use parallelInfo_mod
    use dataArray_mod
    use dataType_mod
    use dataShape_mod
    use dataExtent_mod
    use linkedList_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(DataArray), public :: MirroredArray
        private
            class(LinkedList), pointer :: changes
            integer                    :: totalChanges
            integer, allocatable       :: lsizes(:)

!            logical,        dimension(:), pointer :: dptr1d_logical => NULL()
!            integer(int8),  dimension(:), pointer :: dptr1d_byte    => NULL()
!            integer(int16), dimension(:), pointer :: dptr1d_short   => NULL()
!            integer(int32), dimension(:), pointer :: dptr1d_int     => NULL()
!            integer(int64), dimension(:), pointer :: dptr1d_long    => NULL()
!            real(real32),   dimension(:), pointer :: dptr1d_real    => NULL()
!            real(real64),   dimension(:), pointer :: dptr1d_dble    => NULL()

        contains

            procedure :: addChange_logical => addMirroredChange_logical
            procedure :: addChange_byte    => addMirroredChange_byte
            procedure :: addChange_short   => addMirroredChange_short
            procedure :: addChange_int     => addMirroredChange_int
            procedure :: addChange_long    => addMirroredChange_long
            procedure :: addChange_real    => addMirroredChange_real
            procedure :: addChange_dble    => addMirroredChange_dble

            procedure :: addChangeRange_logical => addMirroredChangeRange_logical
            procedure :: addChangeRange_byte    => addMirroredChangeRange_byte
            procedure :: addChangeRange_short   => addMirroredChangeRange_short
            procedure :: addChangeRange_int     => addMirroredChangeRange_int
            procedure :: addChangeRange_long    => addMirroredChangeRange_long
            procedure :: addChangeRange_real    => addMirroredChangeRange_real
            procedure :: addChangeRange_dble    => addMirroredChangeRange_dble

            generic            :: combineChanges =>            &
                                      combineChanges_logical,  &
                                      combineChanges_byte,     &
                                      combineChanges_short,    &
                                      combineChanges_int,      &
                                      combineChanges_long,     &
                                      combineChanges_real,     &
                                      combineChanges_dble

            procedure, private :: combineChanges_logical
            procedure, private :: combineChanges_byte
            procedure, private :: combineChanges_short
            procedure, private :: combineChanges_int
            procedure, private :: combineChanges_long
            procedure, private :: combineChanges_real
            procedure, private :: combineChanges_dble

            procedure, private :: nextMessage

            procedure          :: synchronize => synchronize_mirroredArray

            procedure, private :: doSynchronize_logical
            procedure, private :: doSynchronize_byte
            procedure, private :: doSynchronize_short
            procedure, private :: doSynchronize_int
            procedure, private :: doSynchronize_long
            procedure, private :: doSynchronize_real
            procedure, private :: doSynchronize_dble

            generic            :: mirroredArrayConstructor =>                  &
                                      mirroredArrayConstructor_dtypenum,       &
                                      mirroredArrayConstructor_noptr,          &
                                      mirroredArrayConstructor_logical1d_dims, &
                                      mirroredArrayConstructor_byte1d_dims,    &
                                      mirroredArrayConstructor_short1d_dims,   &
                                      mirroredArrayConstructor_int1d_dims,     &
                                      mirroredArrayConstructor_long1d_dims,    &
                                      mirroredArrayConstructor_real1d_dims,    &
                                      mirroredArrayConstructor_double1d_dims,  &
                                      mirroredArrayConstructor_logical2d_dims, &
                                      mirroredArrayConstructor_byte2d_dims,    &
                                      mirroredArrayConstructor_short2d_dims,   &
                                      mirroredArrayConstructor_int2d_dims,     &
                                      mirroredArrayConstructor_long2d_dims,    &
                                      mirroredArrayConstructor_real2d_dims,    &
                                      mirroredArrayConstructor_double2d_dims,  &
                                      mirroredArrayConstructor_logical3d_dims, &
                                      mirroredArrayConstructor_byte3d_dims,    &
                                      mirroredArrayConstructor_short3d_dims,   &
                                      mirroredArrayConstructor_int3d_dims,     &
                                      mirroredArrayConstructor_long3d_dims,    &
                                      mirroredArrayConstructor_real3d_dims,    &
                                      mirroredArrayConstructor_double3d_dims,  &
                                      mirroredArrayConstructor_logical4d_dims, &
                                      mirroredArrayConstructor_byte4d_dims,    &
                                      mirroredArrayConstructor_short4d_dims,   &
                                      mirroredArrayConstructor_int4d_dims,     &
                                      mirroredArrayConstructor_long4d_dims,    &
                                      mirroredArrayConstructor_real4d_dims,    &
                                      mirroredArrayConstructor_double4d_dims,  &
                                      mirroredArrayConstructor_logical5d_dims, &
                                      mirroredArrayConstructor_byte5d_dims,    &
                                      mirroredArrayConstructor_short5d_dims,   &
                                      mirroredArrayConstructor_int5d_dims,     &
                                      mirroredArrayConstructor_long5d_dims,    &
                                      mirroredArrayConstructor_real5d_dims,    &
                                      mirroredArrayConstructor_double5d_dims,  &
                                      mirroredArrayConstructor_logical6d_dims, &
                                      mirroredArrayConstructor_byte6d_dims,    &
                                      mirroredArrayConstructor_short6d_dims,   &
                                      mirroredArrayConstructor_int6d_dims,     &
                                      mirroredArrayConstructor_long6d_dims,    &
                                      mirroredArrayConstructor_real6d_dims,    &
                                      mirroredArrayConstructor_double6d_dims,  &
                                      mirroredArrayConstructor_logical7d_dims, &
                                      mirroredArrayConstructor_byte7d_dims,    &
                                      mirroredArrayConstructor_short7d_dims,   &
                                      mirroredArrayConstructor_int7d_dims,     &
                                      mirroredArrayConstructor_long7d_dims,    &
                                      mirroredArrayConstructor_real7d_dims,    &
                                      mirroredArrayConstructor_double7d_dims,  &
                                      mirroredArrayConstructor_logical1d_dshp, &
                                      mirroredArrayConstructor_byte1d_dshp,    &
                                      mirroredArrayConstructor_short1d_dshp,   &
                                      mirroredArrayConstructor_int1d_dshp,     &
                                      mirroredArrayConstructor_long1d_dshp,    &
                                      mirroredArrayConstructor_real1d_dshp,    &
                                      mirroredArrayConstructor_double1d_dshp,  &
                                      mirroredArrayConstructor_logical2d_dshp, &
                                      mirroredArrayConstructor_byte2d_dshp,    &
                                      mirroredArrayConstructor_short2d_dshp,   &
                                      mirroredArrayConstructor_int2d_dshp,     &
                                      mirroredArrayConstructor_long2d_dshp,    &
                                      mirroredArrayConstructor_real2d_dshp,    &
                                      mirroredArrayConstructor_double2d_dshp,  &
                                      mirroredArrayConstructor_logical3d_dshp, &
                                      mirroredArrayConstructor_byte3d_dshp,    &
                                      mirroredArrayConstructor_short3d_dshp,   &
                                      mirroredArrayConstructor_int3d_dshp,     &
                                      mirroredArrayConstructor_long3d_dshp,    &
                                      mirroredArrayConstructor_real3d_dshp,    &
                                      mirroredArrayConstructor_double3d_dshp,  &
                                      mirroredArrayConstructor_logical4d_dshp, &
                                      mirroredArrayConstructor_byte4d_dshp,    &
                                      mirroredArrayConstructor_short4d_dshp,   &
                                      mirroredArrayConstructor_int4d_dshp,     &
                                      mirroredArrayConstructor_long4d_dshp,    &
                                      mirroredArrayConstructor_real4d_dshp,    &
                                      mirroredArrayConstructor_double4d_dshp,  &
                                      mirroredArrayConstructor_logical5d_dshp, &
                                      mirroredArrayConstructor_byte5d_dshp,    &
                                      mirroredArrayConstructor_short5d_dshp,   &
                                      mirroredArrayConstructor_int5d_dshp,     &
                                      mirroredArrayConstructor_long5d_dshp,    &
                                      mirroredArrayConstructor_real5d_dshp,    &
                                      mirroredArrayConstructor_double5d_dshp,  &
                                      mirroredArrayConstructor_logical6d_dshp, &
                                      mirroredArrayConstructor_byte6d_dshp,    &
                                      mirroredArrayConstructor_short6d_dshp,   &
                                      mirroredArrayConstructor_int6d_dshp,     &
                                      mirroredArrayConstructor_long6d_dshp,    &
                                      mirroredArrayConstructor_real6d_dshp,    &
                                      mirroredArrayConstructor_double6d_dshp,  &
                                      mirroredArrayConstructor_logical7d_dshp, &
                                      mirroredArrayConstructor_byte7d_dshp,    &
                                      mirroredArrayConstructor_short7d_dshp,   &
                                      mirroredArrayConstructor_int7d_dshp,     &
                                      mirroredArrayConstructor_long7d_dshp,    &
                                      mirroredArrayConstructor_real7d_dshp,    &
                                      mirroredArrayConstructor_double7d_dshp

            procedure, private :: mirroredArrayConstructor_dtypenum
            procedure, private :: mirroredArrayConstructor_noptr
            procedure, private :: mirroredArrayConstructor_logical1d_dims
            procedure, private :: mirroredArrayConstructor_byte1d_dims
            procedure, private :: mirroredArrayConstructor_short1d_dims
            procedure, private :: mirroredArrayConstructor_int1d_dims
            procedure, private :: mirroredArrayConstructor_long1d_dims
            procedure, private :: mirroredArrayConstructor_real1d_dims
            procedure, private :: mirroredArrayConstructor_double1d_dims
            procedure, private :: mirroredArrayConstructor_logical2d_dims
            procedure, private :: mirroredArrayConstructor_byte2d_dims
            procedure, private :: mirroredArrayConstructor_short2d_dims
            procedure, private :: mirroredArrayConstructor_int2d_dims
            procedure, private :: mirroredArrayConstructor_long2d_dims
            procedure, private :: mirroredArrayConstructor_real2d_dims
            procedure, private :: mirroredArrayConstructor_double2d_dims
            procedure, private :: mirroredArrayConstructor_logical3d_dims
            procedure, private :: mirroredArrayConstructor_byte3d_dims
            procedure, private :: mirroredArrayConstructor_short3d_dims
            procedure, private :: mirroredArrayConstructor_int3d_dims
            procedure, private :: mirroredArrayConstructor_long3d_dims
            procedure, private :: mirroredArrayConstructor_real3d_dims
            procedure, private :: mirroredArrayConstructor_double3d_dims
            procedure, private :: mirroredArrayConstructor_logical4d_dims
            procedure, private :: mirroredArrayConstructor_byte4d_dims
            procedure, private :: mirroredArrayConstructor_short4d_dims
            procedure, private :: mirroredArrayConstructor_int4d_dims
            procedure, private :: mirroredArrayConstructor_long4d_dims
            procedure, private :: mirroredArrayConstructor_real4d_dims
            procedure, private :: mirroredArrayConstructor_double4d_dims
            procedure, private :: mirroredArrayConstructor_logical5d_dims
            procedure, private :: mirroredArrayConstructor_byte5d_dims
            procedure, private :: mirroredArrayConstructor_short5d_dims
            procedure, private :: mirroredArrayConstructor_int5d_dims
            procedure, private :: mirroredArrayConstructor_long5d_dims
            procedure, private :: mirroredArrayConstructor_real5d_dims
            procedure, private :: mirroredArrayConstructor_double5d_dims
            procedure, private :: mirroredArrayConstructor_logical6d_dims
            procedure, private :: mirroredArrayConstructor_byte6d_dims
            procedure, private :: mirroredArrayConstructor_short6d_dims
            procedure, private :: mirroredArrayConstructor_int6d_dims
            procedure, private :: mirroredArrayConstructor_long6d_dims
            procedure, private :: mirroredArrayConstructor_real6d_dims
            procedure, private :: mirroredArrayConstructor_double6d_dims
            procedure, private :: mirroredArrayConstructor_logical7d_dims
            procedure, private :: mirroredArrayConstructor_byte7d_dims
            procedure, private :: mirroredArrayConstructor_short7d_dims
            procedure, private :: mirroredArrayConstructor_int7d_dims
            procedure, private :: mirroredArrayConstructor_long7d_dims
            procedure, private :: mirroredArrayConstructor_real7d_dims
            procedure, private :: mirroredArrayConstructor_double7d_dims
            procedure, private :: mirroredArrayConstructor_logical1d_dshp
            procedure, private :: mirroredArrayConstructor_byte1d_dshp
            procedure, private :: mirroredArrayConstructor_short1d_dshp
            procedure, private :: mirroredArrayConstructor_int1d_dshp
            procedure, private :: mirroredArrayConstructor_long1d_dshp
            procedure, private :: mirroredArrayConstructor_real1d_dshp
            procedure, private :: mirroredArrayConstructor_double1d_dshp
            procedure, private :: mirroredArrayConstructor_logical2d_dshp
            procedure, private :: mirroredArrayConstructor_byte2d_dshp
            procedure, private :: mirroredArrayConstructor_short2d_dshp
            procedure, private :: mirroredArrayConstructor_int2d_dshp
            procedure, private :: mirroredArrayConstructor_long2d_dshp
            procedure, private :: mirroredArrayConstructor_real2d_dshp
            procedure, private :: mirroredArrayConstructor_double2d_dshp
            procedure, private :: mirroredArrayConstructor_logical3d_dshp
            procedure, private :: mirroredArrayConstructor_byte3d_dshp
            procedure, private :: mirroredArrayConstructor_short3d_dshp
            procedure, private :: mirroredArrayConstructor_int3d_dshp
            procedure, private :: mirroredArrayConstructor_long3d_dshp
            procedure, private :: mirroredArrayConstructor_real3d_dshp
            procedure, private :: mirroredArrayConstructor_double3d_dshp
            procedure, private :: mirroredArrayConstructor_logical4d_dshp
            procedure, private :: mirroredArrayConstructor_byte4d_dshp
            procedure, private :: mirroredArrayConstructor_short4d_dshp
            procedure, private :: mirroredArrayConstructor_int4d_dshp
            procedure, private :: mirroredArrayConstructor_long4d_dshp
            procedure, private :: mirroredArrayConstructor_real4d_dshp
            procedure, private :: mirroredArrayConstructor_double4d_dshp
            procedure, private :: mirroredArrayConstructor_logical5d_dshp
            procedure, private :: mirroredArrayConstructor_byte5d_dshp
            procedure, private :: mirroredArrayConstructor_short5d_dshp
            procedure, private :: mirroredArrayConstructor_int5d_dshp
            procedure, private :: mirroredArrayConstructor_long5d_dshp
            procedure, private :: mirroredArrayConstructor_real5d_dshp
            procedure, private :: mirroredArrayConstructor_double5d_dshp
            procedure, private :: mirroredArrayConstructor_logical6d_dshp
            procedure, private :: mirroredArrayConstructor_byte6d_dshp
            procedure, private :: mirroredArrayConstructor_short6d_dshp
            procedure, private :: mirroredArrayConstructor_int6d_dshp
            procedure, private :: mirroredArrayConstructor_long6d_dshp
            procedure, private :: mirroredArrayConstructor_real6d_dshp
            procedure, private :: mirroredArrayConstructor_double6d_dshp
            procedure, private :: mirroredArrayConstructor_logical7d_dshp
            procedure, private :: mirroredArrayConstructor_byte7d_dshp
            procedure, private :: mirroredArrayConstructor_short7d_dshp
            procedure, private :: mirroredArrayConstructor_int7d_dshp
            procedure, private :: mirroredArrayConstructor_long7d_dshp
            procedure, private :: mirroredArrayConstructor_real7d_dshp
            procedure, private :: mirroredArrayConstructor_double7d_dshp

            procedure, private :: doConstructor

            procedure :: cloneMirrored

            final     :: mirroredArrayDestructor
    end type

    type, private :: MessagePart
        private
            integer,    dimension(:,:), allocatable :: ind

            logical,        dimension(:),   allocatable :: value_logical
            integer(int8),  dimension(:),   allocatable :: value_byte
            integer(int16), dimension(:),   allocatable :: value_short
            integer(int32), dimension(:),   allocatable :: value_int
            integer(int64), dimension(:),   allocatable :: value_long
            real(real32),   dimension(:),   allocatable :: value_real
            real(real64),   dimension(:),   allocatable :: value_dble

        contains
            generic,   private :: getValuesAtIndices => &
                                    getValuesAtIndices_logical, &
                                    getValuesAtIndices_byte, &
                                    getValuesAtIndices_short, &
                                    getValuesAtIndices_int, &
                                    getValuesAtIndices_long, &
                                    getValuesAtIndices_real, &
                                    getValuesAtIndices_dble

            procedure, private :: getValuesAtIndices_logical
            procedure, private :: getValuesAtIndices_byte
            procedure, private :: getValuesAtIndices_short
            procedure, private :: getValuesAtIndices_int
            procedure, private :: getValuesAtIndices_long
            procedure, private :: getValuesAtIndices_real
            procedure, private :: getValuesAtIndices_dble

            generic,   private :: setValues => &
                                    setValues_logical, &
                                    setValues_byte, &
                                    setValues_short, &
                                    setValues_int, &
                                    setValues_long, &
                                    setValues_real, &
                                    setValues_dble

            procedure, private :: setValues_logical
            procedure, private :: setValues_byte
            procedure, private :: setValues_short
            procedure, private :: setValues_int
            procedure, private :: setValues_long
            procedure, private :: setValues_real
            procedure, private :: setValues_dble

    end type

    contains

    subroutine doConstructor(this)
        implicit none

        class(MirroredArray) :: this

        class(DataShape), pointer :: dShape

        this%changes      => LinkedList()
        this%totalChanges =  0

        dShape            => this%getDataShape()

        this%lsizes       = dShape%getLocalCounts()
    end subroutine

    subroutine mirroredArrayConstructor_dtypenum(this,dTypeNum,dShape,alloc)
        implicit none

        class(MirroredArray)            :: this

        integer,             intent(in) :: dTypeNum
        class(DataShape),    pointer    :: dShape
        logical, optional,   intent(in) :: alloc

        call this%dataArrayConstructor(dTypeNum,dShape,alloc)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_noptr(this,dType,dShape,alloc)
        implicit none

        class(MirroredArray)                :: this

        class(DataType),     pointer    :: dType
        class(DataShape),    pointer    :: dShape
        logical, optional,   intent(in) :: alloc

        call this%dataArrayConstructor(dType,dShape,alloc)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        logical,        dimension(:), pointer    :: dptr
        logical,        optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int8),  dimension(:), pointer    :: dptr
        integer(int8),  optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int16), dimension(:), pointer    :: dptr
        integer(int16), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int32), dimension(:), pointer    :: dptr
        integer(int32), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int64), dimension(:), pointer    :: dptr
        integer(int64), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)

        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        real(real32),   dimension(:), pointer    :: dptr
        real(real32),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double1d_dims(this,dim1,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1
        real(real64),   dimension(:), pointer    :: dptr
        real(real64),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,        dimension(:), pointer    :: dptr
        logical,        optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int8),  dimension(:), pointer    :: dptr
        integer(int8),  optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int16), dimension(:), pointer    :: dptr
        integer(int16), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int32), dimension(:), pointer    :: dptr
        integer(int32), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int64), dimension(:), pointer    :: dptr
        integer(int64), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        real(real32),   dimension(:), pointer    :: dptr
        real(real32),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double1d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        real(real64),   dimension(:), pointer    :: dptr
        real(real64),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1, dim2
        logical,      dimension(:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),             pointer    :: dim1, dim2
        integer(int8), dimension(:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int16), dimension(:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int32), dimension(:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int64), dimension(:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        real(real32),   dimension(:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double2d_dims(this,dim1,dim2,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        real(real64),   dimension(:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        call this%dataArrayConstructor(dim1,dim2,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double2d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        logical,      dimension(:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),             pointer    :: dim1, dim2, dim3
        integer(int8), dimension(:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        integer(int16), dimension(:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        integer(int32), dimension(:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        integer(int64), dimension(:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        real(real32),   dimension(:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double3d_dims(this,dim1,dim2,dim3,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3
        real(real64),   dimension(:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double3d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1, dim2, dim3, dim4
        logical,      dimension(:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        integer(int8), dimension(:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        integer(int16), dimension(:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        integer(int32), dimension(:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        integer(int64), dimension(:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        real(real32),   dimension(:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double4d_dims(this,dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4
        real(real64),   dimension(:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        logical,      dimension(:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double4d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)

        implicit none

        class(MirroredArray) :: this

        class(DataExtent),            pointer    :: dim1, dim2, dim3, dim4, dim5
        logical,      dimension(:,:,:,:,:), pointer  :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int8), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int16), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int32), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int64), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        real(real32),   dimension(:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5
        real(real64),   dimension(:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double5d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        logical,      dimension(:,:,:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int8), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int16), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int32), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int64), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        real(real32),   dimension(:,:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        real(real64),   dimension(:,:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double6d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        logical,      dimension(:,:,:,:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer(int8), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer(int16), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer(int32), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer(int64), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        real(real32),   dimension(:,:,:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        real(real64),   dimension(:,:,:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_logical7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:,:,:,:,:), pointer    :: dptr
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_byte7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_short7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_int7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_long7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:,:,:), pointer    :: dptr
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_real7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:,:,:), pointer    :: dptr
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayConstructor_double7d_dshp(this,dShape,dptr,initVal,copyData)
        implicit none

        class(MirroredArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:,:,:), pointer    :: dptr
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        call this%dataArrayConstructor(dShape,dptr,initVal,copyData)
        call this%doConstructor()
    end subroutine

    subroutine mirroredArrayDestructor(this)
        implicit none

        type(MirroredArray) :: this

        if (associated(this%changes)) then
            ! also deallocates all message parts contained inside
            deallocate(this%changes)
        end if
    end subroutine

    subroutine checkDimensions(this,ndim)
        implicit none

        class(MirroredArray)                      :: this

        integer, intent(in) :: ndim

        if (ndim .ne. size(this%lsizes)) then
            call error('Incompatible ndims for mirrored array: ' // &
                & int2str(ndim) // ' vs. ' // int2str(size(this%lsizes)))
        end if
    end subroutine

    subroutine addMirroredChange_logical(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                       :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),  intent(in)  :: ind
        logical, dimension(nchanges),       intent(in)  :: value

        logical, dimension(:), pointer :: dptr1d, valueptr

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_byte(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                       :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),  intent(in)  :: ind
        integer(int8), dimension(nchanges), intent(in)  :: value

        integer(int8), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_short(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int16), dimension(nchanges), intent(in)  :: value

        integer(int16), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_int(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int32), dimension(nchanges), intent(in)  :: value

        integer(int32), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_long(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int64), dimension(nchanges), intent(in)  :: value

        integer(int64), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_real(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                      :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges), intent(in)  :: ind
        real(real32), dimension(nchanges), intent(in)  :: value

        real(real32), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChange_dble(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)                      :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges), intent(in)  :: ind
        real(real64), dimension(nchanges), intent(in)  :: value

        real(real64), dimension(:), pointer :: dptr1d

        integer :: i, j, indval

        class(*),           pointer :: optr
        class(MessagePart), pointer :: mpart

        call this%getArray(dptr1d)

        include 'mirroredArray_addMirroredChange.incl'
    end subroutine

    subroutine addMirroredChangeRange_logical(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        logical,                            intent(in)  :: value
        integer,                            intent(in)  :: ndim

        logical, dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_byte(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int8),                      intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int8), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_short(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int16),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int16), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_int(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int32),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int32), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_long(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int64),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int64), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_real(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real32),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        real(real32), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    subroutine addMirroredChangeRange_dble(this,ranges,value,ndim)
        implicit none

        class(MirroredArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real64),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        real(real64), dimension(:), allocatable :: values

        include 'mirroredArray_addMirroredChangeRange.incl'
    end subroutine

    function nextMessage(this) result(mpart)
        implicit none

        class(MirroredArray)   :: this
        class(MessagePart), pointer :: mpart
        class(*),           pointer :: optr

        optr => this%changes%currentValue()

        select type(optr)
            class is (MessagePart)
                mpart => optr
            class default
                call error('Unknown class in distributed messages changes list')
        end select

        call this%changes%next()
    end function

    subroutine combineChanges_logical(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)              :: this

        integer,      dimension(:,:),  pointer :: ind
        integer, intent(out)                   :: nchanges
        integer, intent(out)                   :: ndim
        logical,      dimension(:),    pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_byte(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)              :: this

        integer,      dimension(:,:),  pointer :: ind
        integer, intent(out)                   :: nchanges
        integer, intent(out)                   :: ndim
        integer(int8), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_short(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)               :: this

        integer,        dimension(:,:), pointer :: ind
        integer, intent(out)                    :: nchanges
        integer, intent(out)                    :: ndim
        integer(int16), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_int(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)               :: this

        integer,        dimension(:,:), pointer :: ind
        integer, intent(out)                    :: nchanges
        integer, intent(out)                    :: ndim
        integer(int32), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_long(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)               :: this

        integer,        dimension(:,:), pointer :: ind
        integer, intent(out)                    :: nchanges
        integer, intent(out)                    :: ndim
        integer(int64), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_real(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)             :: this

        integer,      dimension(:,:), pointer :: ind
        integer, intent(out)                  :: nchanges
        integer, intent(out)                  :: ndim
        real(real32), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine combineChanges_dble(this,ind,nchanges,ndim,value)
        implicit none

        class(MirroredArray)             :: this

        integer,      dimension(:,:), pointer :: ind
        integer, intent(out)                  :: nchanges
        integer, intent(out)                  :: ndim
        real(real64), dimension(:),   pointer :: value

        include 'mirroredArray_combineChanges.incl'
    end subroutine

    subroutine getValuesAtIndices_logical(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        logical,        dimension(:), pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_logical(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_byte(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        integer(int8),  dimension(:), pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_byte(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_short(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        integer(int16), dimension(:), pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_short(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_int(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        integer(int32), dimension(:), pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_int(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_long(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        integer(int64), dimension(:), pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_long(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_real(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        real(real32), dimension(:),   pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_real(1:nmsg)
    end subroutine

    subroutine getValuesAtIndices_dble(this,value,cursor,nmsg)
        implicit none

        class(MessagePart)                    :: this
        real(real64), dimension(:),   pointer :: value
        integer, intent(in)                   :: cursor
        integer, intent(in)                   :: nmsg

        value(cursor:cursor+nmsg-1) = this%value_dble(1:nmsg)
    end subroutine

    subroutine setValues_logical(this,value)
        implicit none

        class(MessagePart)     :: this
        logical,  dimension(:) :: value

        allocate(this%value_logical(size(value)))
        this%value_logical = value
    end subroutine

    subroutine setValues_byte(this,value)
        implicit none

        class(MessagePart)           :: this
        integer(int8),  dimension(:) :: value

        allocate(this%value_byte(size(value)))
        this%value_byte = value
    end subroutine

    subroutine setValues_short(this,value)
        implicit none

        class(MessagePart)           :: this
        integer(int16), dimension(:) :: value

        allocate(this%value_short(size(value)))
        this%value_short = value
    end subroutine

    subroutine setValues_int(this,value)
        implicit none

        class(MessagePart)           :: this
        integer(int32), dimension(:) :: value

        allocate(this%value_int(size(value)))
        this%value_int = value
    end subroutine

    subroutine setValues_long(this,value)
        implicit none

        class(MessagePart)           :: this
        integer(int64), dimension(:) :: value

        allocate(this%value_long(size(value)))
        this%value_long = value
    end subroutine

    subroutine setValues_real(this,value)
        implicit none

        class(MessagePart)         :: this
        real(real32), dimension(:) :: value

        allocate(this%value_real(size(value)))
        this%value_real = value
    end subroutine

    subroutine setValues_dble(this,value)
        implicit none

        class(MessagePart)         :: this
        real(real64), dimension(:) :: value

        allocate(this%value_dble(size(value)))
        this%value_dble = value
    end subroutine

    subroutine synchronize_mirroredArray(this,pinfo)
        implicit none

        class(MirroredArray)          :: this

        class(ParallelInfo), pointer  :: pinfo

        class(DataType), pointer :: dtype

        dtype => this%getDataType()

        select case(dtype%getDataTypeNum())
            case (LOGICAL_TYPE_NUM)
                call this%doSynchronize_logical(pinfo)
            case (BYTE_TYPE_NUM)
                call this%doSynchronize_byte(pinfo)
            case (SHORT_TYPE_NUM)
                call this%doSynchronize_short(pinfo)
            case (INT_TYPE_NUM)
                call this%doSynchronize_int(pinfo)
            case (LONG_TYPE_NUM)
                call this%doSynchronize_long(pinfo)
            case (REAL_TYPE_NUM)
                call this%doSynchronize_real(pinfo)
            case (DOUBLE_TYPE_NUM)
                call this%doSynchronize_dble(pinfo)
            case default
                call error('In message synchronize, unknown type ' // &
                    int2str(dtype%getDataTypeNum()))
        end select
    end subroutine

    subroutine doSynchronize_logical(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        logical, dimension(:), pointer        :: dptr1d

        logical, dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_byte(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        integer(int8), dimension(:), pointer  :: dptr1d

        integer(int8), dimension(:), pointer  :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_short(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        integer(int16), dimension(:), pointer :: dptr1d

        integer(int16), dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_int(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        integer(int32), dimension(:), pointer :: dptr1d

        integer(int32), dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_long(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        integer(int64), dimension(:), pointer :: dptr1d

        integer(int64), dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_real(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        real(real32), dimension(:), pointer :: dptr1d

        real(real32), dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    subroutine doSynchronize_dble(this,pinfo)
        implicit none

        class(MirroredArray)          :: this
        class(ParallelInfo), pointer  :: pinfo

        real(real64), dimension(:), pointer :: dptr1d

        real(real64), dimension(:), pointer :: values

        include 'mirroredArray_synchronize.incl'
    end subroutine

    function clone(this,copyData) result(daptr)

        class(MirroredArray)             :: this

        logical,              intent(in) :: copyData

        class(DataArray),     pointer    :: daptr

        class(MirroredArray), pointer    :: maptr

        maptr => this%cloneMirrored(copyData)
        daptr => maptr
    end function

    function cloneMirrored(this,copyData) result(mArray)

        class(MirroredArray)         :: this

        logical,          intent(in) :: copyData

        class(MirroredArray), pointer    :: mArray

        class(DataShape), pointer    :: dsptr
        class(DataType),  pointer    :: dtptr

        dsptr => this%getDataShape()
        dtptr => this%getDataType()

        allocate(mArray)

        select case(dtptr%getDataTypeNum())
            case (LOGICAL_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_logical(),copyData=copyData)
            case (BYTE_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_byte(),   copyData=copyData)
            case (SHORT_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_short(),  copyData=copyData)
            case (INT_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_int(),    copyData=copyData)
            case (LONG_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_long(),   copyData=copyData)
            case (REAL_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_real(),   copyData=copyData)
            case (DOUBLE_TYPE_NUM)
                call mArray%mirroredArrayConstructor(dsptr%clone(),&
                    & this%getDataPointer_double(), copyData=copyData)
            case default
                call error('In clone, unknown data type:' // dtptr%getDataTypeName())
        end select
    end function
end module
