module observationSplitter_mod

    use observation_mod
    use observationProcessor_mod

    use dataGrid_mod
    use dataArray_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataDimension_mod

    use parallelInfo_mod

    use mpiUtils_mod

    implicit none

    private

    public :: ObservationSplitter

    type, extends(ObservationProcessor) :: ObservationSplitter

        private

        integer, allocatable :: lociDims(:)

        class(DataGrid),   pointer :: inputGrid => NULL()

        contains
            procedure :: process
            procedure :: getPointOwners

            procedure :: observationSplitterConstructor

            final     :: observationSplitterDestructor
    end type

    contains

    subroutine observationSplitterConstructor(this,inputGrid,lociDims)
        implicit none

        class(ObservationSplitter)  :: this

        class(DataGrid), pointer    :: inputGrid
        integer,         intent(in) :: lociDims(:)

        this%inputGrid => inputGrid
        this%lociDims = lociDims
    end subroutine

    subroutine observationSplitterDestructor(this)
        implicit none

        type(ObservationSplitter)  :: this

        ! don't deallocate the input grid as it could be used in other places
    end subroutine

    subroutine process(this,pinfo,obsIn,obsOut,stage,newObject)
        implicit none

        class(ObservationSplitter)       :: this

        class(ParallelInfo), pointer     :: pinfo
        class(Observation),  pointer     :: obsIn
        class(Observation),  pointer     :: obsOut
        integer,             intent(in)  :: stage
        logical,             intent(out) :: newObject

        real(real64), pointer :: obsLoci(:,:)

        real(real64), allocatable :: lociVals(:,:)
        integer,      allocatable :: owners(:)
        integer,      allocatable :: localInds(:)

        integer :: i, inew, ndim, nobs, nobsnew_local, nobsnew_global, rp1

        integer, allocatable, dimension(:) :: obs_sizes, obs_offsets

        integer :: rank

        class(DataDimension), pointer :: nobsNewDim
        class(DataExtent),    pointer :: nobsNewExtent
        class(DataVariable),  pointer :: obsLociVar
        class(DataVariable),  pointer :: obsLociGlobalVar

        print *,'obsplit 1'

        rank = pinfo%getRank()

        obsLociVar => obsIn%getObsLociVar()
        !obsLociGlobalVar => obsLociVar%gatherToGlobal(pinfo,obsLociVar%getName() // '_GLOBAL')

        call obsLociVar%getArray(obsLoci)

        nobs = size(obsLoci,2)
        ndim = size(this%lociDims)

        allocate(lociVals(ndim,nobs))
        allocate(owners(nobs))

        do i=1,ndim
            lociVals(i,:) = obsLoci(this%lociDims(i),:)
        end do

        print *,'obsplit 3'

        print *,'min/max loci vals:',pinfo%getRank(),minval(lociVals(1,:)),&
            & maxval(lociVals(1,:)),&
            & minval(lociVals(2,:)),maxval(lociVals(2,:))

        call this%getPointOwners(pinfo,ndim,nobs,lociVals,owners)

        print *,'obsplit 4'

        nobsnew_local  = 0

        do i=1,size(owners)
            if (owners(i) == rank) then
                nobsnew_local = nobsnew_local + 1
            end if
        end do

        print *,'size of owners is:',shape(owners),nobs,nobsnew_local,rank,&
            & minval(obsLoci(1,:)),maxval(obsLoci(1,:)),&
            & minval(obsLoci(2,:)),maxval(obsLoci(2,:)),&
            & minval(obsLoci(3,:)),maxval(obsLoci(3,:)),&
            & minval(obsLoci(4,:)),maxval(obsLoci(4,:)),&
            & minval(owners),maxval(owners)

        print *,'obsplit 5'

        allocate(obs_sizes(pinfo%getCommSize()))

        do i=1,pinfo%getCommSize()
            if (i-1 == pinfo%getRank()) then
                obs_sizes(i) = nobsnew_local
            end if
            call bcast0d(obs_sizes(i),i-1,pinfo%getCommunicator(),'sharing obs local sizes')
        end do

        nobsnew_global = sum(obs_sizes)
        print *,'on rank',pinfo%getRank(),'obs_sizes:',obs_sizes,sum(obs_sizes),nobsnew_global

        print *,'obsplit 6'

        allocate(obs_offsets(pinfo%getCommSize()))
        do i=1,pinfo%getCommSize()
            if (i == 1) then
                obs_offsets(i) = 0
            else
                obs_offsets(i) = obs_offsets(i-1) + obs_sizes(i-1)
            end if
        end do

        allocate(localinds(nobsnew_local))

        inew = 0

        do i=1,nobs
            if (owners(i) == rank) then
                inew = inew + 1
                localinds(inew) = i
            end if
        end do

        rp1 = pinfo%getRank() + 1

        print *,'obsplit 7'

        allocate(nobsNewDim)
        call nobsNewDim%dataDimensionConstructor(NOBS_DIM_NAME,nobsnew_global)

        allocate(nobsNewExtent)
        call nobsNewExtent%dataExtentConstructor(nobsNewDim, &
                & localCount = obs_sizes(rp1),  localStart = obs_offsets(rp1) + 1)

        obsOut => obsIn%cloneSubset(pinfo,nobsNewExtent,localInds)
        call obsOut%setObsOwnershipRange(1,nobsnew_local,pinfo%getRank())

        print *,'obsplit 8'

        print *,'finished splitting the observations on rank',pinfo%getRank()

        deallocate(lociVals)
        deallocate(owners)
        deallocate(obs_sizes)
        deallocate(obs_offsets)
        deallocate(localinds)
        !deallocate(obsLociGlobalVar)

        newObject = .true.
    end subroutine

    subroutine getPointOwners(this,pinfo,ndim,nloci,lociVals,owners)
        implicit none

        class(ObservationSplitter)       :: this

        class(ParallelInfo), pointer     :: pinfo
        integer,             intent(in)  :: ndim
        integer,             intent(in)  :: nloci
        real(real64),        intent(in)  :: lociVals(ndim,nloci)
        integer,             intent(out) :: owners(nloci)

        real(real64) :: distances(2,nloci)
        real(real64) :: mindist(2,nloci)
        integer :: i, ierror

        print *,'gpo 1'

        call this%inputGrid%getDistanceToCenter(ndim,nloci,lociVals,distances(1,:))

        print *,'gpo 2'

        distances(2,:) = pinfo%getRank()

        call mpi_allreduce(distances,mindist,nloci,MPI_2DOUBLE_PRECISION,MPI_MINLOC,&
            pinfo%getCommunicator(),ierror)

        do i=1,nloci
            if (mindist(1,i) < 1d99) then
                owners(i) = nint(mindist(2,i))
            else
                owners(i) = -1
            end if
        end do

        print *,'sample owners:',owners(1:5)

!        call this%inputGrid%getNearestPoint(ndim,nloci,lociVals,nearestPts)
!
!        print *,'gpo 2'
!
!        ownersVar => this%inputGrid%getGlobalCoordOwnersVar()
!        dArray    => ownersVar%getDataArray()
!        owners1d  => dArray%getDataPointer_int()
!
!        do i=1,nloci
!            if (nearestPts(i) > 0) then
!                owners(i) = owners1d(nearestPts(i))
!            else
!                owners(i) = -1
!            end if
!        end do
    end subroutine
end module
