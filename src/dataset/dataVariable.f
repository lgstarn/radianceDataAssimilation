module dataVariable_mod

    use parallelInfo_mod

    use dictionary_mod

    use dataType_mod
    use dataShape_mod
    use dataArray_mod
    use dataAttribute_mod
    use dataExtent_mod
    use dataDimension_mod

    use mpiUtils_mod

    implicit none

    private

    public :: DataVariable

    ! A class that represents actual data, usually stored as an N-dimensional array. Variables have
    ! a name, a data type, as well as a shape, which is a collection of data dimensions/extents.
    ! These dimensions can be shared, in which case the shared dimension must be in the same group or a
    ! parent group (however, this behavior is currently not implemented).
    type :: DataVariable

        private

        logical                    :: collective              ! can we do collective operations?
        logical                    :: distributed             ! is the data distributed?
        character(:), allocatable  :: name
        class(DataArray),  pointer :: dArray         => null()
        class(Dictionary), pointer :: attributes     => null()

        contains
            procedure :: isCollective
!            procedure :: setCollective

            procedure :: isDistributed
!            procedure :: setDistributed

            procedure :: getName
            procedure :: setName

            procedure :: getLocalExtentCount
            procedure :: getGlobalDimCount
            procedure :: getLocalTotalSize
            procedure :: getNDimensions
            procedure :: getExtentNumber
            procedure :: getDimensionNumber
            procedure :: getLocalExtentRange
            procedure :: getGlobalDimRange

            procedure :: getDataType
            procedure :: getDataTypeNum
            procedure :: getDataShape

            procedure :: getDataArray

            procedure :: getAttribute
            procedure :: getAttributeNames
            procedure :: addAttribute

            procedure :: squeeze
            procedure :: transpose
            procedure :: gatherToGlobal

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

            procedure :: synchronize

            procedure :: zeroAll
            procedure :: clone

            generic   :: dataVariableConstructor => &
                &dataVariableConstructor_array, &
                &dataVariableConstructor_shape

            procedure, private :: dataVariableConstructor_array
            procedure, private :: dataVariableConstructor_shape

            final     :: dataVariableDestructor
    end type

    contains

    subroutine dataVariableConstructor_array(this,name,dArray,collective,distributed)

        implicit none

        class(DataVariable)           :: this

        character(len=*),  intent(in)  :: name
        class(DataArray),  pointer     :: dArray
        logical,           intent(in)  :: collective
        logical,           intent(in)  :: distributed

        this%name = name

        this%dArray => dArray

        this%attributes => Dictionary(10)

        this%collective  = collective
        this%distributed = distributed
    end subroutine

    subroutine dataVariableConstructor_shape(this,name,dTypeNum,dShape,collective,&
        & distributed)

        implicit none

        class(DataVariable)           :: this

        character(len=*),  intent(in)  :: name
        integer,           intent(in)  :: dTypeNum
        class(DataShape),  pointer     :: dShape
        logical,           intent(in)  :: collective
        logical,           intent(in)  :: distributed

        class(DataArray),    pointer  :: dArray

        allocate(dArray)
        call dArray%dataArrayConstructor(dtypeNum,dShape)

        call this%dataVariableConstructor_array(name,dArray,collective,distributed)
    end subroutine

    subroutine dataVariableDestructor(this)
        implicit none

        type(DataVariable)  :: this

        if (associated(this%dArray)) then
            deallocate(this%dArray)
        end if

        if (associated(this%attributes)) then
            deallocate(this%attributes) ! also deallocates the contained objects
        end if
    end subroutine

    function isCollective(this) result(collective)
        implicit none

        class(DataVariable) :: this

        logical :: collective

        collective = this%collective
    end function

!    subroutine setCollective(this,collective)
!        implicit none
!
!        class(DataVariable) :: this
!        logical, intent(in) :: collective
!
!        this%collective = collective
!    end subroutine

    function isDistributed(this) result(distributed)
        implicit none

        class(DataVariable) :: this

        logical             :: distributed

        distributed = this%distributed
    end function

    function getName(this) result(gname)
        implicit none

        class(DataVariable)       :: this

        character(:), allocatable :: gname

        gname = this%name
    end function

    subroutine setName(this,name)
        implicit none

        class(DataVariable)       :: this
        character(:), allocatable :: name

        this%name = name
    end subroutine

    function getLocalExtentCount(this,dimNum) result(dimCount)
        implicit none

        class(DataVariable) :: this

        integer, intent(in) :: dimNum

        integer             :: dimCount

        dimCount = this%dArray%getLocalExtentCount(dimNum)
    end function

    function getGlobalDimCount(this,dimNum) result(dimCount)
        implicit none

        class(DataVariable) :: this

        integer, intent(in) :: dimNum

        integer             :: dimCount

        dimCount = this%dArray%getGlobalDimCount(dimNum)
    end function

    function getLocalTotalSize(this) result(ntot)
        implicit none

        class(DataVariable) :: this

        integer             :: ntot

        ntot = this%dArray%getLocalTotalSize()
    end function

    function getNDimensions(this) result(ndim)
        implicit none

        class(DataVariable) :: this

        integer             :: ndim

        ndim = this%dArray%getNDimensions()
    end function

    function getExtentNumber(this,dimNum) result(dextent)
        implicit none

        class(DataVariable)           :: this

        integer,           intent(in) :: dimNum

        class(DataExtent), pointer    :: dextent

        dextent => this%dArray%getExtentNumber(dimNum)
    end function

    function getDimensionNumber(this,dimNum) result(ddim)
        implicit none

        class(DataVariable)              :: this

        integer,              intent(in) :: dimNum

        class(DataDimension), pointer    :: ddim

        ddim => this%dArray%getDimensionNumber(dimNum)
    end function

    subroutine getLocalExtentRange(this,dimNum,xs,xe,nx)
        implicit none

        class(DataVariable) :: this

        integer,           intent(in)  :: dimNum
        integer,           intent(out) :: xs, xe, nx

        class(DataExtent), pointer     :: dextent

        dextent => this%dArray%getExtentNumber(dimNum)

        xs  = dextent%getLocalStart()
        nx  = dextent%getLocalCount()
        xe  = dextent%getLocalEnd()
    end subroutine

    subroutine getGlobalDimRange(this,dimNum,xs,xe,nx)
        implicit none

        class(DataVariable) :: this

        integer,              intent(in)  :: dimNum
        integer,              intent(out) :: xs, xe, nx

        class(DataDimension), pointer     :: ddim

        ddim => this%getDimensionNumber(dimNum)

        xs  = ddim%getGlobalStart()
        nx  = ddim%getGlobalCount()
        xe  = ddim%getGlobalEnd()
    end subroutine

    function getDataType(this) result(dtype)
        implicit none

        class(DataVariable)      :: this
        class(DataType), pointer :: dtype

        if (associated(this%dArray)) then
            dtype => this%dArray%getDataType()
        else
            call error('Data array in data variable not initialized')
        end if
    end function

    function getDataTypeNum(this) result(dtypenum)
        implicit none

        class(DataVariable) :: this

        integer             :: dtypenum

        class(DataType), pointer :: dtype

        if (associated(this%dArray)) then
            dtype => this%dArray%getDataType()
        else
            call error('Data array in data variable not initialized')
        end if

        dtypenum = dtype%getDataTypeNum()
    end function

    function getDataShape(this) result(dshape)
        implicit none

        class(DataVariable)       :: this
        class(DataShape), pointer :: dshape

        if (associated(this%dArray)) then
            dshape => this%dArray%getDataShape()
        else
            call error('Error: data array in data variable not initialized')
        end if
    end function

    function getDataArray(this) result(daPtr)
        implicit none

        class(DataVariable)       :: this
        class(DataArray), pointer :: daPtr

        daPtr => this%dArray
    end function

    function getAttribute(this,attributeName,ierr) result(attr)
        implicit none

        class(DataVariable)               :: this
        character(len=*),    intent(in)   :: attributeName
        integer,             intent(out)  :: ierr

        class(DataAttribute), pointer     :: attr

        class(DataAttribute), pointer     :: tmpattr  => null()

        class(*),             pointer     :: optr     => null()

        integer :: nstr
        integer :: attrInd

        logical :: hasAtSymbol ! allow for a single @ symbol at the beginning of the name

        character(:), allocatable :: substr1, substr2, substr3

        integer :: i

        substr1 = trim(attributeName)
        nstr = len(substr1)

        hasAtSymbol = .false.

        ! assume no error for now
        ierr = 0

        do i=1,nstr
            if (substr1(i:i) == '@') then
                if (hasAtSymbol) then
                    ! already had an attribute! Error.
                    ierr = 3
                else
                    attrInd = i
                    hasAtSymbol = .true.
                end if
            end if
        end do

        if (hasAtSymbol) then
            ! trim off the attribute symbol
            if (attrInd+1 <= nstr) then
                ! check for the tricky case where @ is at the end of the string
                substr1 = substr1(attrInd+1:nstr)
            else
                ierr = 3
            end if
        end if

        if (ierr == 0) then
            attr => null()
            ierr = 3

            if (this%attributes%hasKey(substr1)) then
                optr => this%attributes%get(substr1)

                select type(optr)
                    class is (DataAttribute)
                        ! cast down
                        attr => optr
                    class default
                        call error('Unknown class in attributes dictionary')
                end select

                ierr = 0
            end if
        end if
    end function

    subroutine getAttributeNames(this,attrNames)
        implicit none

        class(DataVariable)           :: this

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: attrNames

        call this%attributes%keys(attrNames)
    end subroutine

    subroutine addAttribute(this,attr)
        implicit none

        class(DataVariable)           :: this
        class(DataAttribute), pointer :: attr

        class(*), pointer :: optr

        if (associated(this%attributes)) then
            optr => attr
            call this%attributes%add(attr%getName(),optr)
        else
            call error('Variable attributes dictionary was not associated!')
        end if
    end subroutine

    subroutine squeeze(this)
        implicit none

        class(DataVariable) :: this

        if (associated(this%dArray)) then
            call this%dArray%squeeze()
        end if
    end subroutine

    subroutine transpose(this,dimMapping)
        implicit none

        class(DataVariable) :: this

        integer, intent(in) :: dimMapping(:)

        call this%dArray%transpose(dimMapping)
    end subroutine

    ! gather all local elements from all processors into a global data variable
    ! this is a collective operation (all processors in the communicator must participate)
    function gatherToGlobal(this,pinfo,newName) result(gvar)
        implicit none

        class(DataVariable)             :: this

        class(ParallelInfo), pointer    :: pinfo
        character(len=*),    intent(in) :: newName

        class(DataVariable), pointer :: gvar

        class(DataShape), pointer :: gshape
        class(DataArray), pointer :: gArray

        integer :: i, proc

        integer, pointer :: displacements_local(:)
        integer, pointer :: displacements_other(:)

        integer, dimension(:), allocatable ::  localStarts,  localCounts
        integer, dimension(:), allocatable :: globalStarts, globalCounts

        integer, dimension(:), allocatable :: coords

        logical,        pointer, dimension(:) :: values_logical, gptr_logical
        integer(int8),  pointer, dimension(:) :: values_byte,    gptr_byte
        integer(int16), pointer, dimension(:) :: values_short,   gptr_short
        integer(int32), pointer, dimension(:) :: values_int,     gptr_int
        integer(int64), pointer, dimension(:) :: values_long,    gptr_long
        real(real32),   pointer, dimension(:) :: values_real,    gptr_real
        real(real64),   pointer, dimension(:) :: values_dble,    gptr_dble

        class(DataShape), pointer :: dShape
        class(DataType),  pointer :: dType

        dShape => this%dArray%getDataShape()
        dType  => this%getDataType()

        gshape => dShape%getGlobalShape()

        allocate(gvar)
        call gvar%dataVariableConstructor(newName,this%dArray%getDataTypeNum(),gshape, &
            & collective=.true.,distributed=.false.)

        gArray => gvar%getDataArray()

        ! now gather the data into the data array

        ! first we will need the displacements for this processor
        allocate(displacements_local(this%dArray%getLocalTotalSize()))

        allocate(coords(this%getNDimensions()))

        ! subtract 1 from the starts to make it simpler to add them
         localStarts = dShape%getLocalStarts() - 1
         localCounts = dShape%getLocalCounts()
        globalStarts = dShape%getGlobalStarts() - 1
        globalCounts = dShape%getGlobalCounts()

        ! find the global coordinates for each local point
        do i=1,this%dArray%getLocalTotalSize()
            call dShape%localInd2Coord(i,localCounts,coords)

            coords = coords + localStarts + globalStarts
            displacements_local(i) = dShape%globalCoord2Ind(coords,globalCounts)
        end do

        do proc=0,pinfo%getCommSize()-1

            ! broadcast processor # proc's displacements into displacement_other
            if (proc == pinfo%getRank()) then
                displacements_other => displacements_local
            end if

            call bcast1d_varLen(displacements_other,proc,pinfo%getCommunicator())

            ! now we must handle each data type separately
            select case (this%dArray%getDataTypeNum())
                case (LOGICAL_TYPE_NUM)
                    gptr_logical => gArray%getDataPointer_logical()

                    if (proc == pinfo%getRank()) then
                        values_logical => this%dArray%getDataPointer_logical()
                    end if

                    call bcast1d_varLen(values_logical,proc,pinfo%getCommunicator())

                    do i=1,size(values_logical)
                        gptr_logical(displacements_other(i)) = values_logical(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_logical)
                    end if
                case (BYTE_TYPE_NUM)
                    gptr_byte => gArray%getDataPointer_byte()

                    if (proc == pinfo%getRank()) then
                        values_byte => this%dArray%getDataPointer_byte()
                    end if

                    call bcast1d_varLen(values_byte,proc,pinfo%getCommunicator())

                    do i=1,size(values_byte)
                        gptr_byte(displacements_other(i)) = values_byte(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_byte)
                    end if
                case (SHORT_TYPE_NUM)
                    gptr_short => gArray%getDataPointer_short()

                    if (proc == pinfo%getRank()) then
                        values_short => this%dArray%getDataPointer_short()
                    end if

                    call bcast1d_varLen(values_short,proc,pinfo%getCommunicator())

                    do i=1,size(values_byte)
                        gptr_short(displacements_other(i)) = values_short(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_short)
                    end if
                case (INT_TYPE_NUM)
                    gptr_int => gArray%getDataPointer_int()

                    if (proc == pinfo%getRank()) then
                        values_int => this%dArray%getDataPointer_int()
                    end if

                    call bcast1d_varLen(values_int,proc,pinfo%getCommunicator())

                    do i=1,size(values_int)
                        gptr_int(displacements_other(i)) = values_int(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_int)
                    end if
                case (LONG_TYPE_NUM)
                    gptr_long => gArray%getDataPointer_long()

                    if (proc == pinfo%getRank()) then
                        values_long => this%dArray%getDataPointer_long()
                    end if

                    call bcast1d_varLen(values_long,proc,pinfo%getCommunicator())

                    do i=1,size(values_long)
                        gptr_long(displacements_other(i)) = values_long(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_long)
                    end if
                case (REAL_TYPE_NUM)
                    gptr_real => gArray%getDataPointer_real()

                    if (proc == pinfo%getRank()) then
                        values_real => this%dArray%getDataPointer_real()
                    end if

                    call bcast1d_varLen(values_real,proc,pinfo%getCommunicator())

                    do i=1,size(values_real)
                        gptr_real(displacements_other(i)) = values_real(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_real)
                    end if
                case (DOUBLE_TYPE_NUM)
                    gptr_dble => gArray%getDataPointer_double()

                    if (proc == pinfo%getRank()) then
                        values_dble => this%dArray%getDataPointer_double()
                    end if

                    call bcast1d_varLen(values_dble,proc,pinfo%getCommunicator())

                    do i=1,size(values_dble)
                        gptr_dble(displacements_other(i)) = values_dble(i)
                    end do

                    if (proc /= pinfo%getRank()) then
                        deallocate(values_dble)
                    end if
                case default
                    call error('In data variable gatherToGlobal, unknown data type:' // &
                        & dType%getDataTypeName())
            end select

            if (proc /= pinfo%getRank()) then
                deallocate(displacements_other)
            end if
        end do
    end function

    subroutine getArray1d_logical(this,array)
        implicit none

        class(DataVariable)            :: this
        logical, dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_logical(this,array)
        implicit none

        class(DataVariable)              :: this
        logical, dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_logical(this,array)
        implicit none

        class(DataVariable)                :: this
        logical, dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_logical(this,array)
        implicit none

        class(DataVariable)                  :: this
        logical, dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_logical(this,array)
        implicit none

        class(DataVariable)                    :: this
        logical, dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_logical(this,array)
        implicit none

        class(DataVariable)                      :: this
        logical, dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_logical(this,array)
        implicit none

        class(DataVariable)                        :: this
        logical, dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_byte(this,array)
        implicit none

        class(DataVariable)                  :: this
        integer(int8), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_byte(this,array)
        implicit none

        class(DataVariable)                    :: this
        integer(int8), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_byte(this,array)
        implicit none

        class(DataVariable)                      :: this
        integer(int8), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_byte(this,array)
        implicit none

        class(DataVariable)                        :: this
        integer(int8), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_byte(this,array)
        implicit none

        class(DataVariable)                          :: this
        integer(int8), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_byte(this,array)
        implicit none

        class(DataVariable)                            :: this
        integer(int8), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_byte(this,array)
        implicit none

        class(DataVariable)                              :: this
        integer(int8), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_short(this,array)
        implicit none

        class(DataVariable)                   :: this
        integer(int16), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_short(this,array)
        implicit none

        class(DataVariable)                     :: this
        integer(int16), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_short(this,array)
        implicit none

        class(DataVariable)                       :: this
        integer(int16), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_short(this,array)
        implicit none

        class(DataVariable)                         :: this
        integer(int16), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_short(this,array)
        implicit none

        class(DataVariable)                           :: this
        integer(int16), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_short(this,array)
        implicit none

        class(DataVariable)                             :: this
        integer(int16), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_short(this,array)
        implicit none

        class(DataVariable)                               :: this
        integer(int16), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_int(this,array)
        implicit none

        class(DataVariable)                   :: this
        integer(int32), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_int(this,array)
        implicit none

        class(DataVariable)                     :: this
        integer(int32), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_int(this,array)
        implicit none

        class(DataVariable)                       :: this
        integer(int32), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_int(this,array)
        implicit none

        class(DataVariable)                         :: this
        integer(int32), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_int(this,array)
        implicit none

        class(DataVariable)                           :: this
        integer(int32), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_int(this,array)
        implicit none

        class(DataVariable)                             :: this
        integer(int32), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_int(this,array)
        implicit none

        class(DataVariable)                               :: this
        integer(int32), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_long(this,array)
        implicit none

        class(DataVariable)                   :: this
        integer(int64), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_long(this,array)
        implicit none

        class(DataVariable)                     :: this
        integer(int64), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_long(this,array)
        implicit none

        class(DataVariable)                       :: this
        integer(int64), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_long(this,array)
        implicit none

        class(DataVariable)                         :: this
        integer(int64), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_long(this,array)
        implicit none

        class(DataVariable)                           :: this
        integer(int64), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_long(this,array)
        implicit none

        class(DataVariable)                             :: this
        integer(int64), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_long(this,array)
        implicit none

        class(DataVariable)                               :: this
        integer(int64), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_real(this,array)
        implicit none

        class(DataVariable)                 :: this
        real(real32), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_real(this,array)
        implicit none

        class(DataVariable)                   :: this
        real(real32), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_real(this,array)
        implicit none

        class(DataVariable)                     :: this
        real(real32), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_real(this,array)
        implicit none

        class(DataVariable)                       :: this
        real(real32), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_real(this,array)
        implicit none

        class(DataVariable)                         :: this
        real(real32), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_real(this,array)
        implicit none

        class(DataVariable)                           :: this
        real(real32), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_real(this,array)
        implicit none

        class(DataVariable)                             :: this
        real(real32), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray1d_double(this,array)
        implicit none

        class(DataVariable)                 :: this
        real(real64), dimension(:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray2d_double(this,array)
        implicit none

        class(DataVariable)                   :: this
        real(real64), dimension(:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray3d_double(this,array)
        implicit none

        class(DataVariable)                     :: this
        real(real64), dimension(:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray4d_double(this,array)
        implicit none

        class(DataVariable)                       :: this
        real(real64), dimension(:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray5d_double(this,array)
        implicit none

        class(DataVariable)                         :: this
        real(real64), dimension(:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray6d_double(this,array)
        implicit none

        class(DataVariable)                           :: this
        real(real64), dimension(:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine getArray7d_double(this,array)
        implicit none

        class(DataVariable)                             :: this
        real(real64), dimension(:,:,:,:,:,:,:), pointer :: array

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray()

        call dArray%getArray(array)
    end subroutine

    subroutine zeroAll(this)
        implicit none

        class(DataVariable) :: this

        call this%dArray%zeroAll()
    end subroutine

    subroutine synchronize(this,pinfo)
        implicit none

        class(DataVariable)          :: this
        class(ParallelInfo), pointer :: pinfo

        call this%dArray%synchronize(pinfo)
    end subroutine

    function clone(this,copyData) result(dvptr)
        implicit none

        class(DataVariable) :: this
        logical, intent(in) :: copyData
        class(DataVariable), pointer :: dvptr

        class(DataAttribute), pointer :: daptr

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        integer :: i, ierr

        allocate(dvptr)

        call dvptr%dataVariableConstructor(this%name,this%dArray%clone(copyData),&
            this%collective,this%distributed)

        call this%getAttributeNames(keyNames)

        do i=1,size(keyNames)
            daPtr => this%getAttribute(keyNames(i),ierr)

            if (ierr == 0) then
                call dvptr%addAttribute(daPtr%clone())
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)
    end function
end module
