module dataShape_mod

    use parallelDecomposition_mod

    use iso_fortran_env

    use linkedList_mod

    use dataExtent_mod
    use dataDimension_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DataShape

    type   :: DataShape
        private

        class(LinkedList), pointer :: extentList  => null()

        contains
            procedure :: decompose
            procedure :: squeeze

            procedure :: getNDimensions

            procedure :: globalCoord2Ind
            procedure :: globalInd2Coord

            procedure :: localCoord2Ind
            procedure :: localInd2Coord

            procedure :: getGlobalShape

            procedure :: getGlobalSize
            procedure :: getGlobalSizes
            procedure :: getGlobalCount
            procedure :: getGlobalCounts
            procedure :: getGlobalStart
            procedure :: getGlobalStarts
            procedure :: getGlobalRange
            procedure :: getGlobalRanges
            procedure :: getLocalCount
            procedure :: getLocalCounts
            procedure :: getLocalStart
            procedure :: getLocalStarts
            procedure :: getLocalEnd
            procedure :: getLocalEnds
            procedure :: getLocalRange
            procedure :: getLocalRanges
            procedure :: getLocalTotalSize

            procedure :: getMaxDimNameLength

            ! procedure :: isUnlimited

            procedure :: resetToFirstExtent
            procedure :: nextExtent
            procedure :: getCurrentExtent
            procedure :: getExtentNumber
            procedure :: getDimensionNumber
            procedure :: addExtent

            procedure :: checkLocalTotalSize
            procedure :: compareNDimensions

            generic            :: datashapeConstructor =>     &
                                  dataShapeConstructor_empty, &
!                                  dataShapeConstructor_dims,  &
                                  dataShapeConstructor_global, &
                                  dataShapeConstructor_local

            procedure, private :: dataShapeConstructor_empty
!            procedure, private :: dataShapeConstructor_dims
            procedure, private :: dataShapeConstructor_global
            procedure, private :: dataShapeConstructor_local

            procedure :: clone

            final     :: dataShapeDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataShapeConstructor_empty(this)
        implicit none

        class(DataShape)              :: this

        this%extentList => LinkedList()
    end subroutine

!    subroutine dataShapeConstructor_globalSizes(this,dims,decomp)
!        implicit none
!
!        class(DataShape)                                   :: this
!        integer,                                intent(in) :: globalSizes(:)
!        class(ParallelDecomposition), optional, pointer    :: decomp
!
!        class(DataExtent), pointer :: dextent
!        character(len=6) :: dimName
!
!        integer, allocatable :: localCounts(:)
!        integer, allocatable :: localStarts(:)
!
!        integer :: i, ndims
!
!        this%extentList => LinkedList()
!
!        ndims = size(dims)
!
!        if (present(decomp)) then
!            call this%dataShapeConstructor(.true.)
!
!            allocate(localCounts(ndims),localStarts(ndims))
!
!            call decomp%decompose(ndims,dims,localCounts,localStarts)
!
!            do i=1,ndims
!                write(dimName,'(A,I0)') 'DIM',i
!                allocate(dextent)
!
!                call dextent%dataExtentConstructor(dimName,globalSizes(i),&
!                       localCount=localCounts(i),localStart=localStarts(i),&
!                    shared=.false.)
!
!                call this%addExtent(dDim)
!                nullify(dDim)
!            end do
!        else
!            do i=1,size(dims)
!                write(dimName,'(A,I0)') 'DIM',i
!                allocate(dextent)
!                call dextent%dataExtentConstructor(dimName,globalSizes(i),shared=.false.)
!                call this%addExtent(dextent)
!                nullify(dextent)
!            end do!
!        end if
!    end subroutine

    subroutine dataShapeConstructor_global(this,ndim,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
        decomp)

        implicit none

        class(DataShape)                                   :: this

        integer,                                intent(in) :: ndim
        class(DataDimension),                   pointer    :: dim1
        class(DataDimension),         optional, pointer    :: dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelDecomposition), optional, pointer    :: decomp

        class(DataExtent),              pointer    :: ldim1, ldim2, ldim3, ldim4
        class(DataExtent),              pointer    :: ldim5, ldim6, ldim7

        integer, allocatable :: globalCounts(:)
        integer, allocatable :: staggers(:)
        integer, allocatable :: localCounts(:)
        integer, allocatable :: localStarts(:)

        character(:), pointer :: dimNames(:)

        integer :: maxName

        class(DataExtent), pointer :: dext

        this%extentList => LinkedList()

        allocate(globalCounts(ndim))
        allocate(staggers(ndim))

        if (.not. associated(dim1)) then
            call error('Dimension 1 was not associated in the data shape constructor ' // &
                & ' - ndim: ' // int2str(ndim))
        end if

        globalCounts(1) = dim1%getGlobalCount()
        staggers(1)     = dim1%getStagger()
        maxName         = len(dim1%getComparisonDimName())

        if (ndim >= 2) then
            if (.not. associated(dim2)) then
                call error('Dimension 2 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(2) = dim2%getGlobalCount()
            staggers(2)     = dim2%getStagger()
            maxName         = max(maxName,len(dim2%getComparisonDimName()))
        end if

        if (ndim >= 3) then
            if (.not. associated(dim3)) then
                call error('Dimension 3 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(3) = dim3%getGlobalCount()
            staggers(3)     = dim3%getStagger()
            maxName         = max(maxName,len(dim3%getComparisonDimName()))
        end if

        if (ndim >= 4) then
            if (.not. associated(dim4)) then
                call error('Dimension 4 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(4) = dim4%getGlobalCount()
            staggers(4)     = dim4%getStagger()
            maxName         = max(maxName,len(dim4%getComparisonDimName()))
        end if

        if (ndim >= 5) then
            if (.not. associated(dim5)) then
                call error('Dimension 5 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(5) = dim5%getGlobalCount()
            staggers(5)     = dim5%getStagger()
            maxName         = max(maxName,len(dim5%getComparisonDimName()))
        end if

        if (ndim >= 6) then
            if (.not. associated(dim6)) then
                call error('Dimension 6 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(6) = dim6%getGlobalCount()
            staggers(6)     = dim6%getStagger()
            maxName         = max(maxName,len(dim6%getComparisonDimName()))
        end if

        if (ndim == 7) then
            if (.not. associated(dim7)) then
                call error('Dimension 7 was not associated in the data shape constructor ' // &
                    & ' - ndim: ' // int2str(ndim))
            end if
            globalCounts(7) = dim7%getGlobalCount()
            staggers(7)     = dim7%getStagger()
            maxName         = max(maxName,len(dim7%getComparisonDimName()))
        end if

        if (present(decomp)) then
            allocate(character(len=maxName) :: dimNames(ndim))

            dimNames(1) = dim1%getComparisonDimName()

            if (ndim >= 2) then
                dimNames(2) = dim2%getComparisonDimName()
            end if

            if (ndim >= 3) then
                dimNames(3) = dim3%getComparisonDimName()
            end if

            if (ndim >= 4) then
                dimNames(4) = dim4%getComparisonDimName()
            end if

            if (ndim >= 5) then
                dimNames(5) = dim5%getComparisonDimName()
            end if

            if (ndim >= 6) then
                dimNames(6) = dim6%getComparisonDimName()
            end if

            if (ndim == 7) then
                dimNames(7) = dim7%getComparisonDimName()
            end if

            allocate(localCounts(ndim))
            allocate(localStarts(ndim))

            call decomp%decompose(dimNames,ndim,globalCounts,staggers,&
                & localCounts,localStarts)

            allocate(ldim1)
            call ldim1%dataExtentConstructor(dim1,localCounts(1),localStarts(1))

            if (ndim >= 2) then
                allocate(ldim2)
                call ldim2%dataExtentConstructor(dim2,localCounts(2),localStarts(2))
            end if

            if (ndim >= 3) then
                allocate(ldim3)
                call ldim3%dataExtentConstructor(dim3,localCounts(3),localStarts(3))
            end if

            if (ndim >= 4) then
                allocate(ldim4)
                call ldim4%dataExtentConstructor(dim4,localCounts(4),localStarts(4))
            end if

            if (ndim >= 5) then
                allocate(ldim5)
                call ldim5%dataExtentConstructor(dim5,localCounts(5),localStarts(5))
            end if

            if (ndim >= 6) then
                allocate(ldim6)
                call ldim6%dataExtentConstructor(dim6,localCounts(6),localStarts(6))
            end if

            if (ndim >= 7) then
                allocate(ldim7)
                call ldim7%dataExtentConstructor(dim7,localCounts(7),localStarts(7))
            end if
        else
            ! set the local ranges to the global ranges on the dimensions
            allocate(ldim1)
            call ldim1%dataExtentConstructor(dim1,globalCounts(1),1)

            if (ndim >= 2) then
                allocate(ldim2)
                call ldim2%dataExtentConstructor(dim2,globalCounts(2),1)
            end if

            if (ndim >= 3) then
                allocate(ldim3)
                call ldim3%dataExtentConstructor(dim3,globalCounts(3),1)
            end if

            if (ndim >= 4) then
                allocate(ldim4)
                call ldim4%dataExtentConstructor(dim4,globalCounts(4),1)
            end if

            if (ndim >= 5) then
                allocate(ldim5)
                call ldim5%dataExtentConstructor(dim5,globalCounts(5),1)
            end if

            if (ndim >= 6) then
                allocate(ldim6)
                call ldim6%dataExtentConstructor(dim6,globalCounts(6),1)
            end if

            if (ndim >= 7) then
                allocate(ldim7)
                call ldim7%dataExtentConstructor(dim7,globalCounts(7),1)
            end if
        end if

        call this%dataShapeConstructor(ndim,ldim1,ldim2,ldim3,ldim4,ldim5,ldim6,ldim7)
    end subroutine

    subroutine dataShapeConstructor_local(this,ndim,ldim1,ldim2,ldim3,ldim4,ldim5,ldim6,ldim7)
        implicit none

        class(DataShape)  :: this

        integer,                             intent(in) :: ndim
        class(DataExtent),           pointer    :: ldim1
        class(DataExtent), optional, pointer    :: ldim2, ldim3, ldim4
        class(DataExtent), optional, pointer    :: ldim5, ldim6, ldim7

        call this%dataShapeConstructor()

        if (.not. associated(ldim1)) then
            call error('Dimension 1 is not associated with a pointer in dataShape constructor')
        end if

        call this%addExtent(ldim1)

        if (ndim >= 2) then
            if (.not. associated(ldim2)) then
                call error('Dimension 2 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim2)
        end if

        if (ndim >= 3) then
            if (.not. associated(ldim3)) then
                call error('Dimension 3 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim3)
        end if

        if (ndim >= 4) then
            if (.not. associated(ldim4)) then
                call error('Dimension 4 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim4)
        end if

        if (ndim >= 5) then
            if (.not. associated(ldim5)) then
                call error('Dimension 5 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim5)
        end if

        if (ndim >= 6) then
            if (.not. associated(ldim6)) then
                call error('Dimension 6 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim6)
        end if

        if (ndim >= 7) then
            if (.not. associated(ldim7)) then
                call error('Dimension 7 was not associated in the data shape constructor')
            end if
            call this%addExtent(ldim7)
        end if
    end subroutine

    subroutine dataShapeDestructor(this)
        implicit none

        type(DataShape)  :: this

        if (associated(this%extentList)) then
            deallocate(this%extentList)
        end if
    end subroutine

    function decompose(this,decomp) result(dShapeNew)
        implicit none

        class(DataShape)    :: this
        class(ParallelDecomposition), pointer :: decomp

        class(DataShape), pointer :: dShapeNew

        integer, dimension(:), allocatable :: globalCounts, globalStarts
        integer, dimension(:), allocatable :: localCounts, localStarts
        integer, dimension(:), allocatable :: staggers

        character(:), pointer :: dimNames(:)

        integer :: i, ndims, maxName

        class(DataExtent), pointer :: dExtOld, dext

        ndims = this%getNDimensions()
        maxName = this%getMaxDimNameLength()

        allocate(globalCounts(ndims))
        allocate(globalStarts(ndims))
        allocate(staggers(ndims))
        allocate(character(len=maxName) :: dimNames(ndims))

        call this%resetToFirstExtent()

        do i=1,ndims
            dExtOld => this%getCurrentExtent()

            globalCounts(i) = dExtOld%getGlobalCount()
            globalStarts(i) = dExtOld%getGlobalStart()
            staggers(i)     = dExtOld%getStagger()
            dimNames(i)     = dExtOld%getComparisonDimName()

            call this%nextExtent()
        end do

        allocate(localCounts(ndims),localStarts(ndims))

        call decomp%decompose(dimNames,ndims,globalCounts,staggers,&
            & localCounts,localStarts)

        allocate(dShapeNew)

        call dShapeNew%dataShapeConstructor()

        call this%resetToFirstExtent()

        do i=1,ndims
            dExtOld => this%getCurrentExtent()

            allocate(dext)
            call dext%dataExtentConstructor(dExtOld%getDimension(), &
                & localCount =localCounts(i),  localStart =localStarts(i))

            call dShapeNew%addExtent(dext)

            ! nullify the pointer in order to reuse it, just to be safe
            nullify(dext)

            call this%nextExtent()
        end do
    end function

    function squeeze(this) result(dShapeNew)
        implicit none

        class(DataShape)    :: this

        class(DataShape), pointer :: dShapeNew

        integer :: i, ndims

        class(DataExtent), pointer :: dExtOld

        ndims = this%getNDimensions()

        call this%resetToFirstExtent()

        allocate(dShapeNew)
        call dShapeNew%dataShapeConstructor()

        do i=1,ndims
            dExtOld => this%getCurrentExtent()

            if (dExtOld%getGlobalCount() /= 1) then
                call dShapeNew%addExtent(dExtOld%clone())
            end if

            call this%nextExtent()
        end do
    end function

    function globalCoord2Ind(this,coords,globalCounts) result(indval)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: coords(:)
        integer, intent(in) :: globalCounts(:)
        integer             :: indval

        integer :: i

        ! commented out for speed
!        if (size(coords) /= this%getNDimensions()) then
!            call error('The number of global indexes did not match the number of dimensions:' //
!                int2str(size(coords)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

!        if (size(globalCounts) /= this%getNDimensions()) then
!            call error('The number of global counts did not match the number of dimensions:' //
!                int2str(size(globalCounts)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

        indval = 0
        do i=1,this%getNDimensions()
            if (i == 1) then
                indval = coords(i)
            else
                indval = indval + (coords(i)-1)*product(globalCounts(1:i-1))
            end if
        end do
    end function

    subroutine globalInd2Coord(this,ind,globalCounts,coords)
        implicit none

        class(DataShape)    :: this

        integer, intent(in)  :: ind
        integer, intent(in)  :: globalCounts(:)
        integer, intent(out) :: coords(:)

        integer :: indCursor
        integer :: i

!        if (size(coords) /= this%getNDimensions()) then
!            call error('The number of global indexes did not match the number of dimensions:' //
!                int2str(size(coords)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

        indCursor = ind

        do i=1,this%getNDimensions()
            coords(i) = mod(indCursor-1, globalCounts(i)) + 1

            indCursor =  (indCursor-1)/globalCounts(i)  + 1
        end do
    end subroutine


    function localCoord2Ind(this,coords,localCounts) result(indval)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: coords(:)
        integer, intent(in) :: localCounts(:)
        integer             :: indval

        integer :: i

        ! commented out for speed
!        if (size(coords) /= this%getNDimensions()) then
!            call error('The number of local indexes did not match the number of dimensions:' //
!                int2str(size(coords)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

!        if (size(localCounts) /= this%getNDimensions()) then
!            call error('The number of local counts did not match the number of dimensions:' //
!                int2str(size(localCounts)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

        indval = 0
        do i=1,this%getNDimensions()
            if (i == 1) then
                indval = coords(i)
            else
                indval = indval + (coords(i)-1)*product(localCounts(1:i-1))
            end if
        end do
    end function

    subroutine localInd2Coord(this,ind,localCounts,coords)
        implicit none

        class(DataShape)    :: this

        integer, intent(in)  :: ind
        integer, intent(in)  :: localCounts(:)
        integer, intent(out) :: coords(:)

        integer :: indCursor
        integer :: i

!        if (size(coords) /= this%getNDimensions()) then
!            call error('The number of local indexes did not match the number of dimensions:' //
!                int2str(size(coords)) ' vs. ' // int2str(this%getNDimensions()))
!        end if

        indCursor = ind

        do i=1,this%getNDimensions()
            coords(i) = mod(indCursor-1, localCounts(i)) + 1

            indCursor =  (indCursor-1)/localCounts(i)  + 1
        end do
    end subroutine

    function getGlobalShape(this) result(gshape)
        implicit none

        class(DataShape)    :: this

        class(DataShape), pointer :: gshape

        integer :: i

        class(*), pointer :: optr

        class(DataExtent), pointer :: dextent

        allocate(gshape)
        call gshape%dataShapeConstructor()

        call this%extentList%first()

        do i=1,this%getNDimensions()
            optr => this%extentList%currentValue()

            if (associated(optr)) then
                select type(optr)
                    class is (DataExtent)
                        ! cast down
                        dextent => optr
                        call this%extentList%next()
                    class default
                        call error('Unknown class in data shape dims list')
                end select
            else
                call error('Dimensions list in data shape is in an inconsistent state')
            end if

            dextent => dextent%clone()

            call dextent%setLocalRange(dextent%getGlobalStart(),dextent%getGlobalCount())

            call gshape%addExtent(dextent)
        end do
    end function

    function getGlobalSize(this,dimNum) result(globalSize)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: globalSize

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        globalSize = dextent%getGlobalSize()
    end function

    function getGlobalSizes(this) result(globalSizes)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: globalSizes(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(globalSizes(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            globalSizes(i) = dextent%getGlobalSize()

            call this%nextExtent()
        end do
    end function

    function getGlobalCount(this,dimNum) result(globalCount)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: globalCount

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        globalCount = dextent%getGlobalCount()
    end function

    function getGlobalCounts(this) result(globalCounts)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: globalCounts(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(globalCounts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            globalCounts(i) = dextent%getGlobalCount()

            call this%nextExtent()
        end do
    end function

    function getGlobalStart(this,dimNum) result(globalStart)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: globalStart

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        globalStart = dextent%getGlobalStart()
    end function

    function getGlobalStarts(this) result(globalStarts)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: globalStarts(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(globalStarts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            globalStarts(i) = dextent%getGlobalStart()

            call this%nextExtent()
        end do
    end function

    subroutine getGlobalRange(this,dimNum,minIndex,maxIndex,nind)
        implicit none

        class(DataShape)    :: this

        integer, intent(in)  :: dimNum
        integer, intent(out) :: minIndex
        integer, intent(out) :: maxIndex
        integer, intent(out) :: nind

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        call dextent%getGlobalRange(minIndex,maxIndex,nind)
    end subroutine

    subroutine getGlobalRanges(this,minIndexes,maxIndexes,counts)
        implicit none

        class(DataShape)    :: this

        integer, intent(out), allocatable :: minIndexes(:)
        integer, intent(out), allocatable :: maxIndexes(:)
        integer, intent(out), allocatable :: counts(:)

        class(DataExtent), pointer :: dextent

        integer :: i, ndim

        ndim = this%getNDimensions()

        allocate(minIndexes(ndim),maxIndexes(ndim),counts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            call dextent%getGlobalRange(minIndexes(i),maxIndexes(i),counts(i))

            call this%nextExtent()
        end do
    end subroutine

    function getLocalCount(this,dimNum) result(localCount)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: localCount

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        localCount = dextent%getLocalCount()
    end function

    function getLocalCounts(this) result(lcounts)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: lcounts(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(lcounts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            lcounts(i) = dextent%getLocalCount()

            call this%nextExtent()
        end do
    end function

    function getLocalStart(this,dimNum) result(localStart)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: localStart

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        localStart = dextent%getLocalStart()
    end function

    function getLocalStarts(this) result(lstarts)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: lstarts(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(lstarts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            lstarts(i) = dextent%getLocalStart()

            call this%nextExtent()
        end do
    end function

    function getLocalEnd(this,dimNum) result(localEnd)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: dimNum

        integer :: localEnd

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        localEnd = dextent%getLocalEnd()
    end function

    function getLocalEnds(this) result(lends)
        implicit none

        class(DataShape)    :: this

        integer, allocatable :: lends(:)

        integer :: i, ndim

        class(DataExtent), pointer :: dextent

        ndim = this%getNDimensions()

        allocate(lends(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            lends(i) = dextent%getLocalEnd()

            call this%nextExtent()
        end do
    end function

    subroutine getLocalRange(this,dimNum,minIndex,maxIndex,nind)
        implicit none

        class(DataShape)    :: this

        integer, intent(in)  :: dimNum
        integer, intent(out) :: minIndex
        integer, intent(out) :: maxIndex
        integer, intent(out) :: nind

        class(DataExtent), pointer :: dextent

        dextent => this%getExtentNumber(dimNum)

        call dextent%getLocalRange(minIndex,maxIndex,nind)
    end subroutine

    subroutine getLocalRanges(this,minIndexes,maxIndexes,counts)
        implicit none

        class(DataShape)    :: this

        integer, intent(out), allocatable :: minIndexes(:)
        integer, intent(out), allocatable :: maxIndexes(:)
        integer, intent(out), allocatable :: counts(:)

        class(DataExtent), pointer :: dextent

        integer :: i, ndim

        ndim = this%getNDimensions()

        allocate(minIndexes(ndim),maxIndexes(ndim),counts(ndim))

        call this%resetToFirstExtent()

        do i=1,ndim
            dextent => this%getCurrentExtent()

            call dextent%getLocalRange(minIndexes(i),maxIndexes(i),counts(i))

            call this%nextExtent()
        end do
    end subroutine

    subroutine resetToFirstExtent(this)
        implicit none

        class(DataShape)    :: this

        call this%extentList%first()
    end subroutine

    subroutine nextExtent(this)
        implicit none

        class(DataShape)    :: this

        class(DataExtent), pointer :: dextent

        call this%extentList%next()
    end subroutine

    function getCurrentExtent(this) result(dextent)
        implicit none

        class(DataShape)    :: this

        class(DataExtent), pointer :: dextent

        class(*), pointer :: optr

        optr => this%extentList%currentValue()

        select type(optr)
            class is (DataExtent)
                ! cast down to DataExtent
                dextent => optr
            class default
                call error('Unknown class in data shape dims list')
        end select
    end function

    function getExtentNumber(this,dimNum) result(dextent)
        implicit none

        class(DataShape)    :: this
        integer, intent(in) :: dimNum

        class(DataExtent), pointer :: dextent

        integer :: i

        if (dimNum > this%getNDimensions()) then
            write(msgstr,*) 'Error: in dataShape, requested dimension larger than number of dimensions:',&
                &dimNum,this%getNDimensions()
            call error(msgstr)
        end if

        call this%resetToFirstExtent()

        do i=1,dimNum
            dextent => this%getCurrentExtent()

            call this%nextExtent()
        end do
    end function

    function getDimensionNumber(this,dimNum) result(dextent)
        implicit none

        class(DataShape)                 :: this

        integer,              intent(in) :: dimNum

        class(DataDimension), pointer    :: dextent

        class(DataExtent), pointer :: dextentLocal

        dextentLocal => this%getExtentNumber(dimNum)

        dextent => dextentLocal%getDimension()
    end function

    subroutine addExtent(this,dextent)
        implicit none

        class(DataShape)    :: this

        class(DataExtent), pointer :: dextent

        class(*), pointer :: optr

        if (.not. associated(dextent)) then
            call error('Cannot add an unassociated dimension to the dataShape')
        end if

        call this%extentList%last()

        optr => dextent

        ! data set is the location-of-record for dimensions, only it can deallocate
        ! non-shared dimensions so as to avoid multiple deallocations
        call this%extentList%add(optr,shouldDeallocate=.false.)
    end subroutine

    function getNDimensions(this) result(ndim)

        implicit none

        class(DataShape) :: this

        integer          :: ndim

        ndim = this%extentList%getListSize()
    end function

    function getLocalTotalSize(this) result(ntot)

        implicit none

        class(DataShape) :: this

        integer          :: i, ntot

        if (this%getNDimensions() == 0) then
            ntot = 0
        else
            ntot = product(this%getLocalCounts())
        end if
    end function

    function getMaxDimNameLength(this) result(maxDimNameLength)
        implicit none

        class(DataShape) :: this

        class(DataExtent), pointer :: dextent

        integer :: maxDimNameLength

        integer :: i, ndim

        ndim = this%getNDimensions()

        call this%resetToFirstExtent()

        maxDimNameLength = 0

        do i=1,ndim
            dextent => this%getCurrentExtent()

            maxDimNameLength = max(maxDimNameLength,len(dextent%getName()))

            call this%nextExtent()
        end do
    end function

    subroutine checkLocalTotalSize(this,ntot)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: ntot

        if (this%getLocalTotalSize() /= ntot) then
            call error('The total sizes did not match: ' // &
                int2str(this%getLocalTotalSize()) // ' vs. ' // int2str(ntot))
        end if
    end subroutine

    subroutine compareNDimensions(this,ndims)
        implicit none

        class(DataShape)    :: this

        integer, intent(in) :: ndims

        if (this%getNDimensions() /= ndims) then
            call error('Number of dimensions: ' // int2str(this%getNDimensions()) // &
                ' vs. ' // int2str(ndims))
        end if
    end subroutine


!    function isUnlimited(this,dimNum) result(unlim)
!        implicit none
!
!        class(DataShape)    :: this
!        integer, intent(in) :: dimNum
!
!        logical             :: unlim
!
!        class(DataExtent), pointer :: dextent
!
!        dextent => this%getDimension(dimNum)
!
!        unlim = dextent%isUnlimited()
!    end function

    function clone(this) result(dsptr)
        implicit none

        class(DataShape) :: this
        class(DataShape), pointer :: dsptr

        class(DataExtent), pointer :: dextent

        class(*), pointer :: optr

        integer :: i, ndim

        allocate(dsptr)

        call dsptr%dataShapeConstructor()

        call this%extentList%first()

        do i=1,this%getNDimensions()
            optr => this%extentList%currentValue()

            if (associated(optr)) then
                select type(optr)
                    class is (DataExtent)
                        ! cast down
                        dextent => optr
                        call this%extentList%next()
                    class default
                        call error('Unknown class in data shape dims list')
                end select
            else
                call error('Dimensions list in data shape is in an inconsistent state')
            end if

            call dsptr%addExtent(dextent%clone())
        end do
    end function
end module
