module dataArray_mod
    use parallelInfo_mod
    use parallelDecomposition_mod

    use iso_fortran_env
    use, intrinsic :: iso_c_binding

    use dataType_mod
    use dataShape_mod
    use dataExtent_mod
    use dataDimension_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DataArray

    type   :: DataArray
        private
            logical,        dimension(:), pointer :: dptr1d_logical => NULL()
            integer(int8),  dimension(:), pointer :: dptr1d_byte    => NULL()
            integer(int16), dimension(:), pointer :: dptr1d_short   => NULL()
            integer(int32), dimension(:), pointer :: dptr1d_int     => NULL()
            integer(int64), dimension(:), pointer :: dptr1d_long    => NULL()
            real(real32),   dimension(:), pointer :: dptr1d_real    => NULL()
            real(real64),   dimension(:), pointer :: dptr1d_dble    => NULL()

            logical                   :: loaded = .false.

            class(DataType),  pointer :: dType          => null()
            class(DataShape), pointer :: dShape         => null()

        contains
            procedure :: getDataType
            procedure :: getDataTypeNum
            procedure :: getDataShape

            procedure :: getNDimensions
            procedure :: getLocalExtentCount
            procedure :: getGlobalDimCount
            procedure :: getLocalTotalSize
            procedure :: getExtentNumber
            procedure :: getDimensionNumber

            procedure :: isLoaded
            procedure :: setLoaded

            procedure :: squeeze
            procedure :: transpose

            procedure :: getDataPointer_logical
            procedure :: getDataPointer_byte
            procedure :: getDataPointer_short
            procedure :: getDataPointer_int
            procedure :: getDataPointer_long
            procedure :: getDataPointer_real
            procedure :: getDataPointer_double

            generic   :: getArray =>                      &
                                      getArray1D_logical, &
                                      getArray1D_byte,    &
                                      getArray1D_short,   &
                                      getArray1D_int,     &
                                      getArray1D_long,    &
                                      getArray1D_real,    &
                                      getArray1D_double,  &
                                      getArray2D_logical, &
                                      getArray2D_byte,    &
                                      getArray2D_short,   &
                                      getArray2D_int,     &
                                      getArray2D_long,    &
                                      getArray2D_real,    &
                                      getArray2D_double,  &
                                      getArray3D_logical, &
                                      getArray3D_byte,    &
                                      getArray3D_short,   &
                                      getArray3D_int,     &
                                      getArray3D_long,    &
                                      getArray3D_real,    &
                                      getArray3D_double,  &
                                      getArray4D_logical, &
                                      getArray4D_byte,    &
                                      getArray4D_short,   &
                                      getArray4D_int,     &
                                      getArray4D_long,    &
                                      getArray4D_real,    &
                                      getArray4D_double,  &
                                      getArray5D_logical, &
                                      getArray5D_byte,    &
                                      getArray5D_short,   &
                                      getArray5D_int,     &
                                      getArray5D_long,    &
                                      getArray5D_real,    &
                                      getArray5D_double,  &
                                      getArray6D_logical, &
                                      getArray6D_byte,    &
                                      getArray6D_short,   &
                                      getArray6D_int,     &
                                      getArray6D_long,    &
                                      getArray6D_real,    &
                                      getArray6D_double,  &
                                      getArray7D_logical, &
                                      getArray7D_byte,    &
                                      getArray7D_short,   &
                                      getArray7D_int,     &
                                      getArray7D_long,    &
                                      getArray7D_real,    &
                                      getArray7D_double

            procedure, private :: getArray1D_logical
            procedure, private :: getArray1D_byte
            procedure, private :: getArray1D_short
            procedure, private :: getArray1D_int
            procedure, private :: getArray1D_long
            procedure, private :: getArray1D_real
            procedure, private :: getArray1D_double
            procedure, private :: getArray2D_logical
            procedure, private :: getArray2D_byte
            procedure, private :: getArray2D_short
            procedure, private :: getArray2D_int
            procedure, private :: getArray2D_long
            procedure, private :: getArray2D_real
            procedure, private :: getArray2D_double
            procedure, private :: getArray3D_logical
            procedure, private :: getArray3D_byte
            procedure, private :: getArray3D_short
            procedure, private :: getArray3D_int
            procedure, private :: getArray3D_long
            procedure, private :: getArray3D_real
            procedure, private :: getArray3D_double
            procedure, private :: getArray4D_logical
            procedure, private :: getArray4D_byte
            procedure, private :: getArray4D_short
            procedure, private :: getArray4D_int
            procedure, private :: getArray4D_long
            procedure, private :: getArray4D_real
            procedure, private :: getArray4D_double
            procedure, private :: getArray5D_logical
            procedure, private :: getArray5D_byte
            procedure, private :: getArray5D_short
            procedure, private :: getArray5D_int
            procedure, private :: getArray5D_long
            procedure, private :: getArray5D_real
            procedure, private :: getArray5D_double
            procedure, private :: getArray6D_logical
            procedure, private :: getArray6D_byte
            procedure, private :: getArray6D_short
            procedure, private :: getArray6D_int
            procedure, private :: getArray6D_long
            procedure, private :: getArray6D_real
            procedure, private :: getArray6D_double
            procedure, private :: getArray7D_logical
            procedure, private :: getArray7D_byte
            procedure, private :: getArray7D_short
            procedure, private :: getArray7D_int
            procedure, private :: getArray7D_long
            procedure, private :: getArray7D_real
            procedure, private :: getArray7D_double

            generic            :: copyData =>            &
                                      copyData_logical,  &
                                      copyData_byte,     &
                                      copyData_short,    &
                                      copyData_int,      &
                                      copyData_long,     &
                                      copyData_real,     &
                                      copyData_double

            procedure, private :: copyData_logical
            procedure, private :: copyData_byte
            procedure, private :: copyData_short
            procedure, private :: copyData_int
            procedure, private :: copyData_long
            procedure, private :: copyData_real
            procedure, private :: copyData_double

            generic,   private :: setDataPointer =>            &
                                      setDataPointer_logical,  &
                                      setDataPointer_byte,     &
                                      setDataPointer_short,    &
                                      setDataPointer_int,      &
                                      setDataPointer_long,     &
                                      setDataPointer_real,     &
                                      setDataPointer_double

            procedure, private :: setDataPointer_logical
            procedure, private :: setDataPointer_byte
            procedure, private :: setDataPointer_short
            procedure, private :: setDataPointer_int
            procedure, private :: setDataPointer_long
            procedure, private :: setDataPointer_real
            procedure, private :: setDataPointer_double

            generic            :: checkType =>           &
                                      checkType_logical, &
                                      checkType_byte,    &
                                      checkType_short,   &
                                      checkType_int,     &
                                      checkType_long,    &
                                      checkType_real,    &
                                      checkType_dble

            procedure, private :: checkType_logical
            procedure, private :: checkType_byte
            procedure, private :: checkType_short
            procedure, private :: checkType_int
            procedure, private :: checkType_long
            procedure, private :: checkType_real
            procedure, private :: checkType_dble

            procedure          :: checkDimensions

            generic            :: addChange =>           &
                                      addChange_logical, &
                                      addChange_byte,    &
                                      addChange_short,   &
                                      addChange_int,     &
                                      addChange_long,    &
                                      addChange_real,    &
                                      addChange_dble

            procedure :: addChange_logical
            procedure :: addChange_byte
            procedure :: addChange_short
            procedure :: addChange_int
            procedure :: addChange_long
            procedure :: addChange_real
            procedure :: addChange_dble

            generic            :: addChangeRange =>           &
                                      addChangeRange_logical, &
                                      addChangeRange_byte,    &
                                      addChangeRange_short,   &
                                      addChangeRange_int,     &
                                      addChangeRange_long,    &
                                      addChangeRange_real,    &
                                      addChangeRange_dble

            procedure, private :: addChangeRange_logical
            procedure, private :: addChangeRange_byte
            procedure, private :: addChangeRange_short
            procedure, private :: addChangeRange_int
            procedure, private :: addChangeRange_long
            procedure, private :: addChangeRange_real
            procedure, private :: addChangeRange_dble

            procedure :: clone
            procedure :: zeroAll

            generic            :: dataArrayConstructor =>                  &
                                      dataArrayConstructor_dtypenum,       &
                                      dataArrayConstructor_noptr,          &
                                      dataArrayConstructor_logical1d_dims, &
                                      dataArrayConstructor_byte1d_dims,    &
                                      dataArrayConstructor_short1d_dims,   &
                                      dataArrayConstructor_int1d_dims,     &
                                      dataArrayConstructor_long1d_dims,    &
                                      dataArrayConstructor_real1d_dims,    &
                                      dataArrayConstructor_double1d_dims,  &
                                      dataArrayConstructor_logical2d_dims, &
                                      dataArrayConstructor_byte2d_dims,    &
                                      dataArrayConstructor_short2d_dims,   &
                                      dataArrayConstructor_int2d_dims,     &
                                      dataArrayConstructor_long2d_dims,    &
                                      dataArrayConstructor_real2d_dims,    &
                                      dataArrayConstructor_double2d_dims,  &
                                      dataArrayConstructor_logical3d_dims, &
                                      dataArrayConstructor_byte3d_dims,    &
                                      dataArrayConstructor_short3d_dims,   &
                                      dataArrayConstructor_int3d_dims,     &
                                      dataArrayConstructor_long3d_dims,    &
                                      dataArrayConstructor_real3d_dims,    &
                                      dataArrayConstructor_double3d_dims,  &
                                      dataArrayConstructor_logical4d_dims, &
                                      dataArrayConstructor_byte4d_dims,    &
                                      dataArrayConstructor_short4d_dims,   &
                                      dataArrayConstructor_int4d_dims,     &
                                      dataArrayConstructor_long4d_dims,    &
                                      dataArrayConstructor_real4d_dims,    &
                                      dataArrayConstructor_double4d_dims,  &
                                      dataArrayConstructor_logical5d_dims, &
                                      dataArrayConstructor_byte5d_dims,    &
                                      dataArrayConstructor_short5d_dims,   &
                                      dataArrayConstructor_int5d_dims,     &
                                      dataArrayConstructor_long5d_dims,    &
                                      dataArrayConstructor_real5d_dims,    &
                                      dataArrayConstructor_double5d_dims,  &
                                      dataArrayConstructor_logical6d_dims, &
                                      dataArrayConstructor_byte6d_dims,    &
                                      dataArrayConstructor_short6d_dims,   &
                                      dataArrayConstructor_int6d_dims,     &
                                      dataArrayConstructor_long6d_dims,    &
                                      dataArrayConstructor_real6d_dims,    &
                                      dataArrayConstructor_double6d_dims,  &
                                      dataArrayConstructor_logical7d_dims, &
                                      dataArrayConstructor_byte7d_dims,    &
                                      dataArrayConstructor_short7d_dims,   &
                                      dataArrayConstructor_int7d_dims,     &
                                      dataArrayConstructor_long7d_dims,    &
                                      dataArrayConstructor_real7d_dims,    &
                                      dataArrayConstructor_double7d_dims,  &
                                      dataArrayConstructor_logical1d_dshp, &
                                      dataArrayConstructor_byte1d_dshp,    &
                                      dataArrayConstructor_short1d_dshp,   &
                                      dataArrayConstructor_int1d_dshp,     &
                                      dataArrayConstructor_long1d_dshp,    &
                                      dataArrayConstructor_real1d_dshp,    &
                                      dataArrayConstructor_double1d_dshp,  &
                                      dataArrayConstructor_logical2d_dshp, &
                                      dataArrayConstructor_byte2d_dshp,    &
                                      dataArrayConstructor_short2d_dshp,   &
                                      dataArrayConstructor_int2d_dshp,     &
                                      dataArrayConstructor_long2d_dshp,    &
                                      dataArrayConstructor_real2d_dshp,    &
                                      dataArrayConstructor_double2d_dshp,  &
                                      dataArrayConstructor_logical3d_dshp, &
                                      dataArrayConstructor_byte3d_dshp,    &
                                      dataArrayConstructor_short3d_dshp,   &
                                      dataArrayConstructor_int3d_dshp,     &
                                      dataArrayConstructor_long3d_dshp,    &
                                      dataArrayConstructor_real3d_dshp,    &
                                      dataArrayConstructor_double3d_dshp,  &
                                      dataArrayConstructor_logical4d_dshp, &
                                      dataArrayConstructor_byte4d_dshp,    &
                                      dataArrayConstructor_short4d_dshp,   &
                                      dataArrayConstructor_int4d_dshp,     &
                                      dataArrayConstructor_long4d_dshp,    &
                                      dataArrayConstructor_real4d_dshp,    &
                                      dataArrayConstructor_double4d_dshp,  &
                                      dataArrayConstructor_logical5d_dshp, &
                                      dataArrayConstructor_byte5d_dshp,    &
                                      dataArrayConstructor_short5d_dshp,   &
                                      dataArrayConstructor_int5d_dshp,     &
                                      dataArrayConstructor_long5d_dshp,    &
                                      dataArrayConstructor_real5d_dshp,    &
                                      dataArrayConstructor_double5d_dshp,  &
                                      dataArrayConstructor_logical6d_dshp, &
                                      dataArrayConstructor_byte6d_dshp,    &
                                      dataArrayConstructor_short6d_dshp,   &
                                      dataArrayConstructor_int6d_dshp,     &
                                      dataArrayConstructor_long6d_dshp,    &
                                      dataArrayConstructor_real6d_dshp,    &
                                      dataArrayConstructor_double6d_dshp,  &
                                      dataArrayConstructor_logical7d_dshp, &
                                      dataArrayConstructor_byte7d_dshp,    &
                                      dataArrayConstructor_short7d_dshp,   &
                                      dataArrayConstructor_int7d_dshp,     &
                                      dataArrayConstructor_long7d_dshp,    &
                                      dataArrayConstructor_real7d_dshp,    &
                                      dataArrayConstructor_double7d_dshp

            procedure, private :: dataArrayConstructor_dtypenum
            procedure, private :: dataArrayConstructor_noptr
            procedure, private :: dataArrayConstructor_logical1d_dims
            procedure, private :: dataArrayConstructor_byte1d_dims
            procedure, private :: dataArrayConstructor_short1d_dims
            procedure, private :: dataArrayConstructor_int1d_dims
            procedure, private :: dataArrayConstructor_long1d_dims
            procedure, private :: dataArrayConstructor_real1d_dims
            procedure, private :: dataArrayConstructor_double1d_dims
            procedure, private :: dataArrayConstructor_logical2d_dims
            procedure, private :: dataArrayConstructor_byte2d_dims
            procedure, private :: dataArrayConstructor_short2d_dims
            procedure, private :: dataArrayConstructor_int2d_dims
            procedure, private :: dataArrayConstructor_long2d_dims
            procedure, private :: dataArrayConstructor_real2d_dims
            procedure, private :: dataArrayConstructor_double2d_dims
            procedure, private :: dataArrayConstructor_logical3d_dims
            procedure, private :: dataArrayConstructor_byte3d_dims
            procedure, private :: dataArrayConstructor_short3d_dims
            procedure, private :: dataArrayConstructor_int3d_dims
            procedure, private :: dataArrayConstructor_long3d_dims
            procedure, private :: dataArrayConstructor_real3d_dims
            procedure, private :: dataArrayConstructor_double3d_dims
            procedure, private :: dataArrayConstructor_logical4d_dims
            procedure, private :: dataArrayConstructor_byte4d_dims
            procedure, private :: dataArrayConstructor_short4d_dims
            procedure, private :: dataArrayConstructor_int4d_dims
            procedure, private :: dataArrayConstructor_long4d_dims
            procedure, private :: dataArrayConstructor_real4d_dims
            procedure, private :: dataArrayConstructor_double4d_dims
            procedure, private :: dataArrayConstructor_logical5d_dims
            procedure, private :: dataArrayConstructor_byte5d_dims
            procedure, private :: dataArrayConstructor_short5d_dims
            procedure, private :: dataArrayConstructor_int5d_dims
            procedure, private :: dataArrayConstructor_long5d_dims
            procedure, private :: dataArrayConstructor_real5d_dims
            procedure, private :: dataArrayConstructor_double5d_dims
            procedure, private :: dataArrayConstructor_logical6d_dims
            procedure, private :: dataArrayConstructor_byte6d_dims
            procedure, private :: dataArrayConstructor_short6d_dims
            procedure, private :: dataArrayConstructor_int6d_dims
            procedure, private :: dataArrayConstructor_long6d_dims
            procedure, private :: dataArrayConstructor_real6d_dims
            procedure, private :: dataArrayConstructor_double6d_dims
            procedure, private :: dataArrayConstructor_logical7d_dims
            procedure, private :: dataArrayConstructor_byte7d_dims
            procedure, private :: dataArrayConstructor_short7d_dims
            procedure, private :: dataArrayConstructor_int7d_dims
            procedure, private :: dataArrayConstructor_long7d_dims
            procedure, private :: dataArrayConstructor_real7d_dims
            procedure, private :: dataArrayConstructor_double7d_dims
            procedure, private :: dataArrayConstructor_logical1d_dshp
            procedure, private :: dataArrayConstructor_byte1d_dshp
            procedure, private :: dataArrayConstructor_short1d_dshp
            procedure, private :: dataArrayConstructor_int1d_dshp
            procedure, private :: dataArrayConstructor_long1d_dshp
            procedure, private :: dataArrayConstructor_real1d_dshp
            procedure, private :: dataArrayConstructor_double1d_dshp
            procedure, private :: dataArrayConstructor_logical2d_dshp
            procedure, private :: dataArrayConstructor_byte2d_dshp
            procedure, private :: dataArrayConstructor_short2d_dshp
            procedure, private :: dataArrayConstructor_int2d_dshp
            procedure, private :: dataArrayConstructor_long2d_dshp
            procedure, private :: dataArrayConstructor_real2d_dshp
            procedure, private :: dataArrayConstructor_double2d_dshp
            procedure, private :: dataArrayConstructor_logical3d_dshp
            procedure, private :: dataArrayConstructor_byte3d_dshp
            procedure, private :: dataArrayConstructor_short3d_dshp
            procedure, private :: dataArrayConstructor_int3d_dshp
            procedure, private :: dataArrayConstructor_long3d_dshp
            procedure, private :: dataArrayConstructor_real3d_dshp
            procedure, private :: dataArrayConstructor_double3d_dshp
            procedure, private :: dataArrayConstructor_logical4d_dshp
            procedure, private :: dataArrayConstructor_byte4d_dshp
            procedure, private :: dataArrayConstructor_short4d_dshp
            procedure, private :: dataArrayConstructor_int4d_dshp
            procedure, private :: dataArrayConstructor_long4d_dshp
            procedure, private :: dataArrayConstructor_real4d_dshp
            procedure, private :: dataArrayConstructor_double4d_dshp
            procedure, private :: dataArrayConstructor_logical5d_dshp
            procedure, private :: dataArrayConstructor_byte5d_dshp
            procedure, private :: dataArrayConstructor_short5d_dshp
            procedure, private :: dataArrayConstructor_int5d_dshp
            procedure, private :: dataArrayConstructor_long5d_dshp
            procedure, private :: dataArrayConstructor_real5d_dshp
            procedure, private :: dataArrayConstructor_double5d_dshp
            procedure, private :: dataArrayConstructor_logical6d_dshp
            procedure, private :: dataArrayConstructor_byte6d_dshp
            procedure, private :: dataArrayConstructor_short6d_dshp
            procedure, private :: dataArrayConstructor_int6d_dshp
            procedure, private :: dataArrayConstructor_long6d_dshp
            procedure, private :: dataArrayConstructor_real6d_dshp
            procedure, private :: dataArrayConstructor_double6d_dshp
            procedure, private :: dataArrayConstructor_logical7d_dshp
            procedure, private :: dataArrayConstructor_byte7d_dshp
            procedure, private :: dataArrayConstructor_short7d_dshp
            procedure, private :: dataArrayConstructor_int7d_dshp
            procedure, private :: dataArrayConstructor_long7d_dshp
            procedure, private :: dataArrayConstructor_real7d_dshp
            procedure, private :: dataArrayConstructor_double7d_dshp

            procedure          :: synchronize

            final :: dataArrayDestructor
    end type

    contains

    subroutine dataArrayConstructor_dtypenum(this,dTypeNum,dShape,alloc)
        implicit none

        class(DataArray)                :: this

        integer,             intent(in) :: dTypeNum
        class(DataShape),    pointer    :: dShape
        logical, optional,   intent(in) :: alloc

        class(DataType), pointer :: dType

        allocate(dType)
        call dType%dataTypeConstructor(dTypeNum)

        call this%dataArrayConstructor(dType,dShape,alloc)
    end subroutine

    subroutine dataArrayConstructor_noptr(this,dType,dShape,alloc)
        implicit none

        class(DataArray)                :: this

        class(DataType),     pointer    :: dType
        class(DataShape),    pointer    :: dShape
        logical, optional,   intent(in) :: alloc

        logical :: doAlloc

        integer              :: ndim, ntot
        integer, allocatable :: dims(:)

        !allocate(this)
        if (.not. associated(dType)) then
            write(msgstr,*) 'Error: data type not specified.'
            call error(msgstr)
        end if

        if (.not. associated(dShape)) then
            write(msgstr,*) 'Error: data shape not specified.'
            call error(msgstr)
        end if

        this%dType  => dType
        this%dShape => dShape

        if (present(alloc)) then
            doAlloc = alloc
        else
            doAlloc = .true.
        end if

        if (doAlloc) then
            ntot = dShape%getLocalTotalSize()

            select case (this%dtype%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    allocate(this%dptr1d_logical(ntot))
                case (BYTE_TYPE_NUM)
                    allocate(this%dptr1d_byte(ntot))
                case (SHORT_TYPE_NUM)
                    allocate(this%dptr1d_short(ntot))
                case (INT_TYPE_NUM)
                    allocate(this%dptr1d_int(ntot))
                case (LONG_TYPE_NUM)
                    allocate(this%dptr1d_long(ntot))
                case (REAL_TYPE_NUM)
                    allocate(this%dptr1d_real(ntot))
                case (DOUBLE_TYPE_NUM)
                    allocate(this%dptr1d_dble(ntot))
                case default
                    call error('In dataArrayConstructor, unknown data type:' // &
                        &this%dtype%getDataTypeName())
            end select
        end if
    end subroutine

    subroutine dataArrayConstructor_logical1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        logical,        dimension(:), pointer    :: dptrnd
        logical,        optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int8),  dimension(:), pointer    :: dptrnd
        integer(int8),  optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int16), dimension(:), pointer    :: dptrnd
        integer(int16), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int32), dimension(:), pointer    :: dptrnd
        integer(int32), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        integer(int64), dimension(:), pointer    :: dptrnd
        integer(int64), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        real(real32),   dimension(:), pointer    :: dptrnd
        real(real32),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double1d_dims(this,dim1,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1
        real(real64),   dimension(:), pointer    :: dptrnd
        real(real64),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(1,dim1)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,        dimension(:), pointer    :: dptr1d
        logical,        optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        logical :: defaultInitVal = .false.

        integer, parameter :: typeNum = LOGICAL_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int8),  dimension(:), pointer    :: dptr1d
        integer(int8),  optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        integer(int8) :: defaultInitVal = 0

        integer, parameter :: typeNum = BYTE_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int16), dimension(:), pointer    :: dptr1d
        integer(int16), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        integer(int16) :: defaultInitVal = 0

        integer, parameter :: typeNum = SHORT_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int32), dimension(:), pointer    :: dptr1d
        integer(int32), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        integer(int32) :: defaultInitVal = 0

        integer, parameter :: typeNum = INT_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        integer(int64), dimension(:), pointer    :: dptr1d
        integer(int64), optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        integer(int64) :: defaultInitVal = 0

        integer, parameter :: typeNum = LONG_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        real(real32),   dimension(:), pointer    :: dptr1d
        real(real32),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        real(real32) :: defaultInitVal = 0.

        integer, parameter :: typeNum = REAL_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double1d_dshp(this,dShape,dptr1d,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        real(real64),   dimension(:), pointer    :: dptr1d
        real(real64),   optional,     intent(in) :: initVal
        logical,        optional,     intent(in) :: copyData

        real(real64) :: defaultInitVal = 0.d0

        integer, parameter :: typeNum = DOUBLE_TYPE_NUM

        include 'dataArray_dataArrayConstructor1d_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_logical2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1, dim2
        logical,      dimension(:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int8), dimension(:,:),  pointer    :: dptrnd
        integer(int8), optional,        intent(in) :: initVal
        logical,       optional,        intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int16), dimension(:,:), pointer    :: dptrnd
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int32), dimension(:,:), pointer    :: dptrnd
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        integer(int64), dimension(:,:), pointer    :: dptrnd
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        real(real32),   dimension(:,:), pointer    :: dptrnd
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double2d_dims(this,dim1,dim2,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),              pointer    :: dim1, dim2
        real(real64),   dimension(:,:), pointer    :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(2,dim1,dim2)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:), pointer    :: dptrnd
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:), pointer    :: dptrnd
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:), pointer    :: dptrnd
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:), pointer    :: dptrnd
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:), pointer    :: dptrnd
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double2d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:), pointer    :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_logical3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),      pointer    :: dim1, dim2, dim3
        logical,      dimension(:,:,:), pointer    :: dptrnd
        logical,      optional,         intent(in) :: initVal
        logical,      optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),       pointer    :: dim1, dim2, dim3
        integer(int8), dimension(:,:,:), pointer    :: dptrnd
        integer(int8), optional,         intent(in) :: initVal
        logical,       optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3
        integer(int16), dimension(:,:,:), pointer    :: dptrnd
        integer(int16), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3
        integer(int32), dimension(:,:,:), pointer    :: dptrnd
        integer(int32), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3
        integer(int64), dimension(:,:,:), pointer    :: dptrnd
        integer(int64), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3
        real(real32),   dimension(:,:,:), pointer    :: dptrnd
        real(real32),   optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double3d_dims(this,dim1,dim2,dim3,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        real(real64),   dimension(:,:,:), pointer    :: dptrnd
        real(real64),   optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(3,dim1,dim2,dim3)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        logical,      dimension(:,:,:), pointer    :: dptrnd
        logical,      optional,         intent(in) :: initVal
        logical,      optional,         intent(in) :: copyData

        logical,      dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                pointer    :: dShape
        integer(int8), dimension(:,:,:), pointer    :: dptrnd
        integer(int8), optional,         intent(in) :: initVal
        logical,       optional,         intent(in) :: copyData

        integer(int8), dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                 pointer    :: dShape
        integer(int16), dimension(:,:,:), pointer    :: dptrnd
        integer(int16), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        integer(int16), dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                 pointer    :: dShape
        integer(int32), dimension(:,:,:), pointer    :: dptrnd
        integer(int32), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        integer(int32), dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                 pointer    :: dShape
        integer(int64), dimension(:,:,:), pointer    :: dptrnd
        integer(int64), optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        integer(int64), dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                 pointer    :: dShape
        real(real32),   dimension(:,:,:), pointer    :: dptrnd
        real(real32),   optional,         intent(in) :: initVal
        logical,        optional,         intent(in) :: copyData

        real(real32),   dimension(:),     pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double3d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:), pointer  :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_logical4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                pointer    :: dim1, dim2, dim3, dim4
        logical,      dimension(:,:,:,:), pointer    :: dptrnd
        logical,      optional,           intent(in) :: initVal
        logical,      optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                 pointer    :: dim1, dim2, dim3, dim4
        integer(int8), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,           intent(in) :: initVal
        logical,       optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4
        integer(int16), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,           intent(in) :: initVal
        logical,        optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4
        integer(int32), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,           intent(in) :: initVal
        logical,        optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4
        integer(int64), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,           intent(in) :: initVal
        logical,        optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4
        real(real32),   dimension(:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,           intent(in) :: initVal
        logical,        optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double4d_dims(this,dim1,dim2,dim3,dim4,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4
        real(real64),   dimension(:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,           intent(in) :: initVal
        logical,        optional,           intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(4,dim1,dim2,dim3,dim4)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double4d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_logical5d_dims(this,dim1,dim2,dim3,dim4,dim5,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                  pointer    :: dim1, dim2, dim3, dim4, dim5
        logical,      dimension(:,:,:,:,:), pointer    :: dptrnd
        logical,      optional,             intent(in) :: initVal
        logical,      optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                   pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int8), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,             intent(in) :: initVal
        logical,       optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                    pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int16), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,             intent(in) :: initVal
        logical,        optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                    pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int32), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,             intent(in) :: initVal
        logical,        optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                    pointer    :: dim1, dim2, dim3, dim4, dim5
        integer(int64), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,             intent(in) :: initVal
        logical,        optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                    pointer    :: dim1, dim2, dim3, dim4, dim5
        real(real32),   dimension(:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,             intent(in) :: initVal
        logical,        optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double5d_dims(this,dim1,dim2,dim3,dim4,dim5,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                    pointer    :: dim1, dim2, dim3, dim4, dim5
        real(real64),   dimension(:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,             intent(in) :: initVal
        logical,        optional,             intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(5,dim1,dim2,dim3,dim4,dim5)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        logical,      dimension(:,:,:,:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double5d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_logical6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),            pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        logical,      dimension(:,:,:,:,:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                     pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        integer(int8), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,               intent(in) :: initVal
        logical,       optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                      pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int16), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                      pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int32), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                      pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        integer(int64), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                      pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        real(real32),   dimension(:,:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double6d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                      pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        real(real64),   dimension(:,:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(6,dim1,dim2,dim3,dim4,dim5,dim6)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                     pointer    :: dShape
        logical,      dimension(:,:,:,:,:,:), pointer    :: dptrnd
        logical,      optional,               intent(in) :: initVal
        logical,      optional,               intent(in) :: copyData

        logical,      dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                      pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,               intent(in) :: initVal
        logical,       optional,               intent(in) :: copyData

        integer(int8), dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                       pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        integer(int16), dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                       pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        integer(int32), dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                       pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        integer(int64), dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                       pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        real(real32),   dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double6d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),                       pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,               intent(in) :: initVal
        logical,        optional,               intent(in) :: copyData

        real(real64),   dimension(:),           pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine


    subroutine dataArrayConstructor_logical7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                 pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical, dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        logical, optional,                 intent(in) :: initVal
        logical, optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_byte7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                       pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,                 intent(in) :: initVal
        logical,       optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_short7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                        pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,                 intent(in) :: initVal
        logical,        optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_int7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                        pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,                 intent(in) :: initVal
        logical,        optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_long7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                        pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,                 intent(in) :: initVal
        logical,        optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_real7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                        pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),   dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,                 intent(in) :: initVal
        logical,        optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_double7d_dims(this,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            &dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataExtent),                        pointer    :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),   dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,                 intent(in) :: initVal
        logical,        optional,                 intent(in) :: copyData

        class(DataShape), pointer :: dShape

        allocate(dShape)
        call dShape%dataShapeConstructor(7,dim1,dim2,dim3,dim4,dim5,dim6,dim7)

        call this%dataArrayConstructor(dShape,dptrnd,initVal,copyData)
    end subroutine

    subroutine dataArrayConstructor_logical7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),             pointer    :: dShape
        logical,      dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        logical,      optional,       intent(in) :: initVal
        logical,      optional,       intent(in) :: copyData

        logical,      dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_byte7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),              pointer    :: dShape
        integer(int8), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int8), optional,       intent(in) :: initVal
        logical,       optional,       intent(in) :: copyData

        integer(int8), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_short7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int16), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int16), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int16), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_int7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int32), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int32), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int32), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_long7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        integer(int64), dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        integer(int64), optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        integer(int64), dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_real7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real32),   dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        real(real32),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real32),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayConstructor_double7d_dshp(this,dShape,dptrnd,initVal,copyData)
        implicit none

        class(DataArray) :: this

        class(DataShape),               pointer    :: dShape
        real(real64),   dimension(:,:,:,:,:,:,:), pointer    :: dptrnd
        real(real64),   optional,       intent(in) :: initVal
        logical,        optional,       intent(in) :: copyData

        real(real64),   dimension(:),   pointer    :: dptr1d

        include 'dataArray_dataArrayConstructornd_dshp.incl'
    end subroutine

    subroutine dataArrayDestructor(this)
        implicit none

        type(DataArray)  :: this

        if (associated(this%dptr1d_logical)) then
            deallocate(this%dptr1d_logical)
            nullify(this%dptr1d_logical)
        end if

        if (associated(this%dptr1d_byte)) then
            deallocate(this%dptr1d_byte)
            nullify(this%dptr1d_byte)
        end if

        if (associated(this%dptr1d_short)) then
            deallocate(this%dptr1d_short)
            nullify(this%dptr1d_short)
        end if

        if (associated(this%dptr1d_int)) then
            deallocate(this%dptr1d_int)
            nullify(this%dptr1d_int)
        end if

        if (associated(this%dptr1d_long)) then
            deallocate(this%dptr1d_long)
            nullify(this%dptr1d_long)
        end if

        if (associated(this%dptr1d_real)) then
            deallocate(this%dptr1d_real)
            nullify(this%dptr1d_real)
        end if

        if (associated(this%dptr1d_dble)) then
            deallocate(this%dptr1d_dble)
            nullify(this%dptr1d_dble)
        end if

        if (associated(this%dType)) then
            deallocate(this%dType)
            nullify   (this%dType)
        end if

        if (associated(this%dShape)) then
            deallocate(this%dShape)
            nullify   (this%dShape)
        end if
    end subroutine

    subroutine squeeze(this)
        implicit none

        class(DataArray)                  :: this

        class(DataShape), pointer :: dShapeNew

        if (associated(this%dShape)) then
            dShapeNew => this%dShape%squeeze()

            deallocate(this%dShape)
            nullify(this%dShape)

            this%dShape => dShapeNew
        end if
    end subroutine

    subroutine transpose(this,dimMapping)
        implicit none

        class(DataArray)    :: this

        integer, intent(in) :: dimMapping(:)

        logical,        dimension(:), pointer :: dptr1d_logical => NULL()
        integer(int8),  dimension(:), pointer :: dptr1d_byte    => NULL()
        integer(int16), dimension(:), pointer :: dptr1d_short   => NULL()
        integer(int32), dimension(:), pointer :: dptr1d_int     => NULL()
        integer(int64), dimension(:), pointer :: dptr1d_long    => NULL()
        real(real32),   dimension(:), pointer :: dptr1d_real    => NULL()
        real(real64),   dimension(:), pointer :: dptr1d_dble    => NULL()

        class(DataShape), pointer :: dShapeNew

        class(DataExtent), pointer :: oldExtent

        integer, allocatable :: coordsOld(:)
        integer, allocatable :: coordsNew(:)

        integer, allocatable :: counts_old(:)
        integer, allocatable :: counts_new(:)

        integer :: indOld
        integer :: indNew

        integer :: ndim, ntot

        integer :: i, j

        ndim = size(dimMapping)

        if (ndim /= this%dShape%getNDimensions()) then
            call error('The number of dimensions for transpose did not match: ' // &
                int2str(ndim) // ' and ' // int2str(this%dShape%getNDimensions()))
        end if

        allocate(dshapeNew)
        call dshapeNew%dataShapeConstructor()

        allocate(coordsNew(ndim))
        allocate(coordsOld(ndim))

        counts_old = this%dShape%getLocalCounts()

        ntot = this%dShape%getLocalTotalSize()

        do i=1,ndim
            oldExtent => this%dShape%getExtentNumber(dimMapping(i))
            call dShapeNew%addExtent(oldExtent%clone())
        end do

        counts_new = dShapeNew%getLocalCounts()

        select case (this%dtype%getDataTypeNum())
            case (LOGICAL_TYPE_NUM)
                allocate(dptr1d_logical(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_logical(indNew) = this%dptr1d_logical(indOld)
                end do

                deallocate(this%dptr1d_logical)
                this%dptr1d_logical => dptr1d_logical
            case (BYTE_TYPE_NUM)
                allocate(dptr1d_byte(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_byte(indNew) = this%dptr1d_byte(indOld)
                end do

                deallocate(this%dptr1d_byte)
                this%dptr1d_byte => dptr1d_byte
            case (SHORT_TYPE_NUM)
                allocate(dptr1d_short(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_short(indNew) = this%dptr1d_short(indOld)
                end do

                deallocate(this%dptr1d_short)
                this%dptr1d_short => dptr1d_short
            case (INT_TYPE_NUM)
                allocate(dptr1d_int(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_int(indNew) = this%dptr1d_int(indOld)
                end do

                deallocate(this%dptr1d_int)
                this%dptr1d_int => dptr1d_int
            case (LONG_TYPE_NUM)
                allocate(dptr1d_long(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_long(indNew) = this%dptr1d_long(indOld)
                end do

                deallocate(this%dptr1d_long)
                this%dptr1d_long => dptr1d_long
            case (REAL_TYPE_NUM)
                allocate(dptr1d_real(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_real(indNew) = this%dptr1d_real(indOld)
                end do

                deallocate(this%dptr1d_real)
                this%dptr1d_real => dptr1d_real
            case (DOUBLE_TYPE_NUM)
                allocate(dptr1d_dble(ntot))

                do indOld=1,ntot
                    call this%dShape%localInd2Coord(indOld,counts_old,coordsOld)
                    do j=1,ndim
                        coordsNew(j) = coordsOld(dimMapping(j))
                    end do
                    indNew  = dShapeNew%localCoord2Ind(coordsNew,counts_new)

                    dptr1d_dble(indNew) = this%dptr1d_dble(indOld)
                end do

                deallocate(this%dptr1d_dble)
                this%dptr1d_dble => dptr1d_dble
        end select

        deallocate(this%dShape)
        this%dShape => dShapeNew
    end subroutine

    function getDataPointer_logical(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        logical, pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(LOGICAL_TYPE_NUM)

        dptr1d => this%dptr1d_logical
    end function

    function getDataPointer_byte(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        integer(int8), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(BYTE_TYPE_NUM)

        dptr1d => this%dptr1d_byte
    end function

    function getDataPointer_short(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        integer(int16), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(SHORT_TYPE_NUM)

        dptr1d => this%dptr1d_short
    end function

    function getDataPointer_int(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        integer(int32), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(INT_TYPE_NUM)

        dptr1d => this%dptr1d_int
    end function

    function getDataPointer_long(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        integer(int64), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(LONG_TYPE_NUM)

        dptr1d => this%dptr1d_long
    end function

    function getDataPointer_real(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        real(real32), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(REAL_TYPE_NUM)

        dptr1d => this%dptr1d_real
    end function

    function getDataPointer_double(this) result(dptr1d)
        implicit none

        class(DataArray)                  :: this

        real(real64), pointer, dimension(:) :: dptr1d

        call this%dtype%compareTypes(DOUBLE_TYPE_NUM)

        dptr1d => this%dptr1d_dble
    end function

    subroutine copyData_logical(this,dptr1d)
        implicit none

        class(DataArray)                  :: this
        logical, dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(LOGICAL_TYPE_NUM)

        this%dptr1d_logical(:) = dptr1d(:)
    end subroutine

    subroutine copyData_byte(this,dptr1d)
        implicit none

        class(DataArray)                        :: this
        integer(int8), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(BYTE_TYPE_NUM)

        this%dptr1d_byte(:) = dptr1d(:)
    end subroutine

    subroutine copyData_short(this,dptr1d)
        implicit none

        class(DataArray)                         :: this
        integer(int16), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(SHORT_TYPE_NUM)

        this%dptr1d_short(:) = dptr1d(:)
    end subroutine

    subroutine copyData_int(this,dptr1d)
        implicit none

        class(DataArray)                        :: this
        integer(int32), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(INT_TYPE_NUM)

        this%dptr1d_int(:) = dptr1d(:)
    end subroutine

    subroutine copyData_long(this,dptr1d)
        implicit none

        class(DataArray)                        :: this
        integer(int64), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(LONG_TYPE_NUM)

        this%dptr1d_long(:) = dptr1d(:)
    end subroutine

    subroutine copyData_real(this,dptr1d)
        implicit none

        class(DataArray)                       :: this
        real(real32), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(REAL_TYPE_NUM)

        this%dptr1d_real(:) = dptr1d(:)
    end subroutine

    subroutine copyData_double(this,dptr1d)
        implicit none

        class(DataArray)                       :: this
        real(real64), dimension(:), intent(in) :: dptr1d

        call this%dtype%compareTypes(DOUBLE_TYPE_NUM)

        this%dptr1d_dble(:) = dptr1d(:)
    end subroutine

    subroutine setDataPointer_logical(this,dptr1d)
        implicit none

        class(DataArray)               :: this
        logical, dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(LOGICAL_TYPE_NUM)

        this%dptr1d_logical => dptr1d
    end subroutine

    subroutine setDataPointer_byte(this,dptr1d)
        implicit none

        class(DataArray)                     :: this
        integer(int8), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(BYTE_TYPE_NUM)

        this%dptr1d_byte => dptr1d
    end subroutine

    subroutine setDataPointer_short(this,dptr1d)
        implicit none

        class(DataArray)                      :: this
        integer(int16), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(SHORT_TYPE_NUM)

        this%dptr1d_short => dptr1d
    end subroutine

    subroutine setDataPointer_int(this,dptr1d)
        implicit none

        class(DataArray)                      :: this
        integer(int32), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(INT_TYPE_NUM)

        this%dptr1d_int => dptr1d
    end subroutine

    subroutine setDataPointer_long(this,dptr1d)
        implicit none

        class(DataArray)                      :: this
        integer(int64), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(LONG_TYPE_NUM)

        this%dptr1d_long => dptr1d
    end subroutine

    subroutine setDataPointer_real(this,dptr1d)
        implicit none

        class(DataArray)                    :: this
        real(real32), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(REAL_TYPE_NUM)

        this%dptr1d_real => dptr1d
    end subroutine

    subroutine setDataPointer_double(this,dptr1d)
        implicit none

        class(DataArray)                    :: this
        real(real64), dimension(:), pointer :: dptr1d

        call this%dtype%compareTypes(DOUBLE_TYPE_NUM)

        this%dptr1d_dble => dptr1d
    end subroutine

    subroutine checkType_logical(this,value)
        implicit none

        class(DataArray)                :: this
        logical,       dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(LOGICAL_TYPE_NUM)
    end subroutine

    subroutine checkType_byte(this,value)
        implicit none

        class(DataArray)                :: this
        integer(int8), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(BYTE_TYPE_NUM)
    end subroutine

    subroutine checkType_short(this,value)
        implicit none

        class(DataArray)                 :: this
        integer(int16), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(SHORT_TYPE_NUM)
    end subroutine

    subroutine checkType_int(this,value)
        implicit none

        class(DataArray)                 :: this
        integer(int32), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(INT_TYPE_NUM)
    end subroutine

    subroutine checkType_long(this,value)
        implicit none

        class(DataArray)                 :: this
        integer(int64), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(LONG_TYPE_NUM)
    end subroutine

    subroutine checkType_real(this,value)
        implicit none

        class(DataArray)               :: this
        real(real32), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(REAL_TYPE_NUM)
    end subroutine

    subroutine checkType_dble(this,value)
        implicit none

        class(DataArray)               :: this
        real(real64), dimension(:), intent(in)  :: value

        call this%dtype%compareTypes(DOUBLE_TYPE_NUM)
    end subroutine

    subroutine checkDimensions(this,ndim)
        implicit none

        class(DataArray)    :: this

        integer, intent(in) :: ndim

        call this%dshape%compareNDimensions(ndim)
    end subroutine

    subroutine getArray1d_logical(this,array1d)
        implicit none

        class(DataArray)               :: this
        logical, dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_logical
    end subroutine

    subroutine getArray1d_byte(this,array1d)
        implicit none

        class(DataArray)                     :: this
        integer(int8), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_byte
    end subroutine

    subroutine getArray1d_short(this,array1d)
        implicit none

        class(DataArray)                      :: this
        integer(int16), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_short
    end subroutine

    subroutine getArray1d_int(this,array1d)
        implicit none

        class(DataArray)                      :: this
        integer(int32), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_int
    end subroutine

    subroutine getArray1d_long(this,array1d)
        implicit none

        class(DataArray)                      :: this
        integer(int64), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_long
    end subroutine

    subroutine getArray1d_real(this,array1d)
        implicit none

        class(DataArray)                    :: this
        real(real32), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_real
    end subroutine

    subroutine getArray1d_double(this,array1d)
        implicit none

        class(DataArray)                    :: this
        real(real64), dimension(:), pointer :: array1d

        call this%checkType(array1d)

        array1d => this%dptr1d_dble
    end subroutine

    subroutine getArray2d_logical(this,array2d)
        implicit none

        class(DataArray)                 :: this
        logical, dimension(:),   pointer :: array1d
        logical, dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_byte(this,array2d)
        implicit none

        class(DataArray)                :: this
        integer(int8), dimension(:),   pointer :: array1d
        integer(int8), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_short(this,array2d)
        implicit none

        class(DataArray)                        :: this
        integer(int16), dimension(:),   pointer :: array1d
        integer(int16), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_int(this,array2d)
        implicit none

        class(DataArray)                :: this
        integer(int32), dimension(:),   pointer :: array1d
        integer(int32), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_long(this,array2d)
        implicit none

        class(DataArray)                :: this
        integer(int64), dimension(:),   pointer :: array1d
        integer(int64), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_real(this,array2d)
        implicit none

        class(DataArray)                :: this
        real(real32), dimension(:),   pointer :: array1d
        real(real32), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray2d_double(this,array2d)
        implicit none

        class(DataArray)                      :: this
        real(real64), dimension(:),   pointer :: array1d
        real(real64), dimension(:,:), pointer :: array2d

        call this%getArray(array1d)
        call this%checkDimensions(2)

        array2d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_logical(this,array3d)
        implicit none

        class(DataArray)                  :: this
        logical, dimension(:),     pointer :: array1d
        logical, dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_byte(this,array3d)
        implicit none

        class(DataArray)                  :: this
        integer(int8), dimension(:),     pointer :: array1d
        integer(int8), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_short(this,array3d)
        implicit none

        class(DataArray)                  :: this
        integer(int16), dimension(:),     pointer :: array1d
        integer(int16), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_int(this,array3d)
        implicit none

        class(DataArray)                  :: this
        integer(int32), dimension(:),     pointer :: array1d
        integer(int32), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_long(this,array3d)
        implicit none

        class(DataArray)                  :: this
        integer(int64), dimension(:),     pointer :: array1d
        integer(int64), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_real(this,array3d)
        implicit none

        class(DataArray)                  :: this
        real(real32), dimension(:),     pointer :: array1d
        real(real32), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray3d_double(this,array3d)
        implicit none

        class(DataArray)                  :: this
        real(real64), dimension(:),     pointer :: array1d
        real(real64), dimension(:,:,:), pointer :: array3d

        call this%getArray(array1d)
        call this%checkDimensions(3)

        array3d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_logical(this,array4d)
        implicit none

        class(DataArray)                     :: this
        logical, dimension(:),       pointer :: array1d
        logical, dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_byte(this,array4d)
        implicit none

        class(DataArray)                           :: this
        integer(int8), dimension(:),       pointer :: array1d
        integer(int8), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_short(this,array4d)
        implicit none

        class(DataArray)                            :: this
        integer(int16), dimension(:),       pointer :: array1d
        integer(int16), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_int(this,array4d)
        implicit none

        class(DataArray)                            :: this
        integer(int32), dimension(:),       pointer :: array1d
        integer(int32), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_long(this,array4d)
        implicit none

        class(DataArray)                            :: this
        integer(int64), dimension(:),       pointer :: array1d
        integer(int64), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_real(this,array4d)
        implicit none

        class(DataArray)                            :: this
        real(real32), dimension(:),       pointer :: array1d
        real(real32), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray4d_double(this,array4d)
        implicit none

        class(DataArray)                            :: this
        real(real64), dimension(:),       pointer :: array1d
        real(real64), dimension(:,:,:,:), pointer :: array4d

        call this%getArray(array1d)
        call this%checkDimensions(4)

        array4d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_logical(this,array5d)
        implicit none

        class(DataArray)                       :: this
        logical, dimension(:),         pointer :: array1d
        logical, dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_byte(this,array5d)
        implicit none

        class(DataArray)                              :: this
        integer(int8), dimension(:),          pointer :: array1d
        integer(int8), dimension(:,:,:,:,:),  pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_short(this,array5d)
        implicit none

        class(DataArray)                              :: this
        integer(int16), dimension(:),         pointer :: array1d
        integer(int16), dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_int(this,array5d)
        implicit none

        class(DataArray)                              :: this
        integer(int32), dimension(:),         pointer :: array1d
        integer(int32), dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_long(this,array5d)
        implicit none

        class(DataArray)                              :: this
        integer(int64), dimension(:),         pointer :: array1d
        integer(int64), dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_real(this,array5d)
        implicit none

        class(DataArray)                            :: this
        real(real32), dimension(:),         pointer :: array1d
        real(real32), dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray5d_double(this,array5d)
        implicit none

        class(DataArray)                            :: this
        real(real64), dimension(:),         pointer :: array1d
        real(real64), dimension(:,:,:,:,:), pointer :: array5d

        call this%getArray(array1d)
        call this%checkDimensions(5)

        array5d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_logical(this,array6d)
        implicit none

        class(DataArray)                         :: this
        logical, dimension(:),           pointer :: array1d
        logical, dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_byte(this,array6d)
        implicit none

        class(DataArray)                               :: this
        integer(int8), dimension(:),           pointer :: array1d
        integer(int8), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_short(this,array6d)
        implicit none

        class(DataArray)                                :: this
        integer(int16), dimension(:),           pointer :: array1d
        integer(int16), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_int(this,array6d)
        implicit none

        class(DataArray)                                :: this
        integer(int32), dimension(:),           pointer :: array1d
        integer(int32), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_long(this,array6d)
        implicit none

        class(DataArray)                                :: this
        integer(int64), dimension(:),           pointer :: array1d
        integer(int64), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_real(this,array6d)
        implicit none

        class(DataArray)                              :: this
        real(real32), dimension(:),           pointer :: array1d
        real(real32), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray6d_double(this,array6d)
        implicit none

        class(DataArray)                              :: this
        real(real64), dimension(:),           pointer :: array1d
        real(real64), dimension(:,:,:,:,:,:), pointer :: array6d

        call this%getArray(array1d)
        call this%checkDimensions(6)

        array6d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_logical(this,array7d)
        implicit none

        class(DataArray)                           :: this
        logical, dimension(:),             pointer :: array1d
        logical, dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_byte(this,array7d)
        implicit none

        class(DataArray)                                 :: this
        integer(int8), dimension(:),             pointer :: array1d
        integer(int8), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_short(this,array7d)
        implicit none

        class(DataArray)                                  :: this
        integer(int16), dimension(:),             pointer :: array1d
        integer(int16), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_int(this,array7d)
        implicit none

        class(DataArray)                                  :: this
        integer(int32), dimension(:),             pointer :: array1d
        integer(int32), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_long(this,array7d)
        implicit none

        class(DataArray)                                  :: this
        integer(int64), dimension(:),             pointer :: array1d
        integer(int64), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_real(this,array7d)
        implicit none

        class(DataArray)                                :: this
        real(real32), dimension(:),             pointer :: array1d
        real(real32), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine getArray7d_double(this,array7d)
        implicit none

        class(DataArray)                                :: this
        real(real64), dimension(:),             pointer :: array1d
        real(real64), dimension(:,:,:,:,:,:,:), pointer :: array7d

        call this%getArray(array1d)
        call this%checkDimensions(7)

        array7d(1:this%getLocalExtentCount(1),&
                1:this%getLocalExtentCount(2),&
                1:this%getLocalExtentCount(3),&
                1:this%getLocalExtentCount(4),&
                1:this%getLocalExtentCount(5),&
                1:this%getLocalExtentCount(6),&
                1:this%getLocalExtentCount(7))   => array1d(1:this%getLocalTotalSize())
    end subroutine

    subroutine addChange_logical(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                       :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),  intent(in)  :: ind
        logical, dimension(nchanges),       intent(in)  :: value

        logical, dimension(:), pointer :: dptr1d, valueptr

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_byte(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                       :: this
        integer,                            intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),  intent(in)  :: ind
        integer(int8), dimension(nchanges), intent(in)  :: value

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_short(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int16), dimension(nchanges), intent(in)  :: value

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_int(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int32), dimension(nchanges), intent(in)  :: value

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_long(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                        :: this
        integer,                             intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges),   intent(in)  :: ind
        integer(int64), dimension(nchanges), intent(in)  :: value

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_real(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                      :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges), intent(in)  :: ind
        real(real32), dimension(nchanges), intent(in)  :: value

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChange_dble(this,ind,nchanges,ndim,value)
        implicit none

        class(DataArray)                      :: this
        integer,                           intent(in)  :: nchanges, ndim
        integer, dimension(ndim,nchanges), intent(in)  :: ind
        real(real64), dimension(nchanges), intent(in)  :: value

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataArray_addChange.incl'
    end subroutine

    subroutine addChangeRange_logical(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        logical,                            intent(in)  :: value
        integer,                            intent(in)  :: ndim

        logical, dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_byte(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int8),                      intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int8), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_short(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int16),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int16), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_int(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int32),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int32), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_long(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        integer(int64),                     intent(in)  :: value
        integer,                            intent(in)  :: ndim

        integer(int64), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_real(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real32),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        real(real32), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    subroutine addChangeRange_dble(this,ranges,value,ndim)
        implicit none

        class(DataArray)                            :: this

        integer, dimension(ndim,2),         intent(in)  :: ranges
        real(real64),                       intent(in)  :: value
        integer,                            intent(in)  :: ndim

        real(real64), dimension(:), allocatable :: values

        include 'dataArray_addChangeRange.incl'
    end subroutine

    function getDataShape(this) result(dShape)
        implicit none

        class(DataArray)          :: this
        class(DataShape), pointer :: dShape

        dShape => this%dShape
    end function

    function getNDimensions(this) result(ndim)
        implicit none

        class(DataArray)           :: this

        integer                    :: ndim

        ndim = this%dShape%getNDimensions()
    end function

    function getLocalExtentCount(this,dimNum) result(ndim)
        implicit none

        class(DataArray)           :: this

        integer,        intent(in) :: dimNum

        integer                    :: ndim

        ndim = this%dShape%getLocalCount(dimNum)
    end function

    function getGlobalDimCount(this,dimNum) result(ndim)
        implicit none

        class(DataArray)           :: this

        integer,        intent(in) :: dimNum

        integer                    :: ndim

        ndim = this%dShape%getGlobalCount(dimNum)
    end function

    function getLocalTotalSize(this) result(ntot)

        implicit none

        class(DataArray)           :: this

        integer                    :: ntot

        ntot = this%dShape%getLocalTotalSize()
    end function

    function getExtentNumber(this,dimNum) result(ddim)

        implicit none

        class(DataArray)              :: this

        integer,           intent(in) :: dimNum

        class(DataExtent), pointer    :: ddim

        ddim => this%dShape%getExtentNumber(dimNum)
    end function

    function getDimensionNumber(this,dimNum) result(ddim)

        implicit none

        class(DataArray)                      :: this

        integer,                   intent(in) :: dimNum

        class(DataDimension), pointer    :: ddim

        ddim => this%dShape%getDimensionNumber(dimNum)
    end function

    function getDataType(this) result(dType)
        implicit none

        class(DataArray)         :: this
        class(DataType), pointer :: dType

        dType => this%dType
    end function

    function getDataTypeNum(this) result(dTypeNum)
        implicit none

        class(DataArray)         :: this
        integer                  :: dTypeNum

        dTypeNum = this%dType%getDataTypeNum()
    end function

    function isLoaded(this) result(loaded)
        implicit none

        class(DataArray)         :: this
        logical                  :: loaded

        loaded = this%loaded
    end function

    subroutine setLoaded(this,loaded)
        implicit none

        class(DataArray)    :: this
        logical, intent(in) :: loaded

        this%loaded = loaded
    end subroutine

    function clone(this,copyData) result(daptr)

        class(DataArray)          :: this
        class(DataArray), pointer :: daptr
        logical,       intent(in) :: copyData

        class(DataShape), pointer :: dsptr

        logical,        dimension(:), pointer :: new_dptr1d_logical => NULL()
        integer(int8),  dimension(:), pointer :: new_dptr1d_byte    => NULL()
        integer(int16), dimension(:), pointer :: new_dptr1d_short   => NULL()
        integer(int32), dimension(:), pointer :: new_dptr1d_int     => NULL()
        integer(int64), dimension(:), pointer :: new_dptr1d_long    => NULL()
        real(real32),   dimension(:), pointer :: new_dptr1d_real    => NULL()
        real(real64),   dimension(:), pointer :: new_dptr1d_dble    => NULL()

        dsptr => this%dShape

        allocate(daptr)

        if (copyData) then
            select case(this%dtype%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_logical,copyData=.true.)
                case (BYTE_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_byte,   copyData=.true.)
                case (SHORT_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_short,  copyData=.true.)
                case (INT_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_int,    copyData=.true.)
                case (LONG_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_long,   copyData=.true.)
                case (REAL_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_real,   copyData=.true.)
                case (DOUBLE_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),this%dptr1d_dble,   copyData=.true.)
                case default
                    call error('In clone, unknown data type:' // this%dtype%getDataTypeName())
            end select
        else
            select case(this%dtype%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_logical,copyData=.false.)
                case (BYTE_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_byte,   copyData=.false.)
                case (SHORT_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_short,  copyData=.false.)
                case (INT_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_int,    copyData=.false.)
                case (LONG_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_long,   copyData=.false.)
                case (REAL_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_real,   copyData=.false.)
                case (DOUBLE_TYPE_NUM)
                    call daptr%dataArrayConstructor(dsptr%clone(),new_dptr1d_dble,   copyData=.false.)
                case default
                    call error('In clone, unknown data type:' // this%dtype%getDataTypeName())
            end select
        end if
    end function

    subroutine zeroAll(this)
        implicit none

        class(DataArray)          :: this

        select case(this%dtype%getDataTypeNum())
            case (LOGICAL_TYPE_NUM)
                this%dptr1d_logical = .false.
            case (BYTE_TYPE_NUM)
                this%dptr1d_byte = 0
            case (SHORT_TYPE_NUM)
                this%dptr1d_short = 0
            case (INT_TYPE_NUM)
                this%dptr1d_int = 0
            case (LONG_TYPE_NUM)
                this%dptr1d_long = 0
            case (REAL_TYPE_NUM)
                this%dptr1d_real = 0.
            case (DOUBLE_TYPE_NUM)
                this%dptr1d_dble = 0.d0
            case default
                call error('In zeroAll, unknown data type:' // this%dtype%getDataTypeName())
        end select
    end subroutine

    subroutine synchronize(this,pinfo)
        implicit none

        class(DataArray)             :: this

        class(ParallelInfo), pointer :: pinfo

        ! by default do nothing
    end subroutine
end module
