module parallelDecomposition_mod
    use linkedList_mod ! needed because Fortran doesn't handle arrays of strings well
    use asciiUtils_mod
    use mpiUtils_mod
    use gridTopology_mod

    implicit none

    private

    ! This is a class that distributes an n-dimensional grid into subcommunicators. These
    ! sub-communicators can be used to divide work among multiple processors according to a
    ! grid topology.
    !
    ! Some of the code in this class was adapted from the paper
    ! Fast parallel multidimensional FFT using advanced MPI by Dalcin et al,
    ! https://arxiv.org/pdf/1804.09536
    !
    ! Jeff Steward, December 2018
    type, public :: ParallelDecomposition
        private

        class(GridTopology),  pointer     :: topology  => null()
        class(LinkedList),    pointer     :: splitDims => null()

        contains
            procedure :: getRootComm
            procedure :: getRootCommRank
            procedure :: getRootCommSize
            procedure :: getNumSplitDimensions
            procedure :: getSplitDimensions
            procedure :: decompose
            procedure :: getTopology

            procedure :: parallelDecompositionConstructor
            final     :: parallelDecompositionDestructor
    end type

    contains

    subroutine parallelDecompositionConstructor(this,dim1Name,dim2Name,dim3Name,dim4Name,&
        dim5name,dim6name,dim7name,comm)

        implicit none

        class(ParallelDecomposition) :: this

        character(*), optional, intent(in) :: dim1Name
        character(*), optional, intent(in) :: dim2Name
        character(*), optional, intent(in) :: dim3Name
        character(*), optional, intent(in) :: dim4Name
        character(*), optional, intent(in) :: dim5Name
        character(*), optional, intent(in) :: dim6Name
        character(*), optional, intent(in) :: dim7Name
        integer,      optional, intent(in) :: comm

        character(:), pointer :: cptr

        class(*), pointer :: optr

        integer :: ndim

        allocate(this%topology)
        this%splitDims => LinkedList()

        ndim = 0

        if (present(dim1Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim1Name)) :: cptr)
            cptr = dim1Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim2Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim2Name)) :: cptr)
            cptr = dim2Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim3Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim3Name)) :: cptr)
            cptr = dim3Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim4Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim4Name)) :: cptr)
            cptr = dim4Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim5Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim5Name)) :: cptr)
            cptr = dim5Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim6Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim6Name)) :: cptr)
            cptr = dim6Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(dim7Name)) then
            ndim = ndim + 1
            allocate(character(len=len(dim7Name)) :: cptr)
            cptr = dim7Name
            optr => cptr
            call this%splitDims%add(optr)
        end if

        if (present(comm)) then
            call this%topology%gridTopologyConstructor(comm,ndim)
        else
            call this%topology%gridTopologyConstructor(MPI_COMM_WORLD,ndim)
        end if
    end subroutine

    subroutine parallelDecompositionDestructor(this)

        implicit none

        type(ParallelDecomposition) :: this

        if (associated(this%splitDims)) then
            deallocate(this%splitDims)
        end if
    end subroutine

    function getRootComm(this) result(comm)
        implicit none

        class(ParallelDecomposition)         :: this

        integer :: comm

        comm = this%topology%getRootComm()
    end function

    function getRootCommRank(this) result(rank)
        implicit none

        class(ParallelDecomposition)         :: this

        integer :: rank

        rank = this%topology%getRootCommRank()
    end function

    function getRootCommSize(this) result(csize)
        implicit none

        class(ParallelDecomposition)         :: this

        integer :: csize

        csize = this%topology%getRootCommSize()
    end function

    subroutine decompose(this,dimNames,ntotalDims,globalCounts,staggers,&
        & localCounts,localStarts)

        implicit none

        class(ParallelDecomposition)     :: this

        character(:),        pointer     :: dimNames(:)
        integer,             intent(in)  :: nTotalDims
        integer,             intent(in)  :: globalCounts(nTotalDims)
        integer,             intent(in)  :: staggers(nTotalDims)
        integer,             intent(out) :: localCounts(nTotalDims)
        integer,             intent(out) :: localStarts(nTotalDims)

        integer :: i, j
        integer :: ierr
        integer :: subcommCounter,subCommSize,subCommRank
        integer, allocatable, dimension(:) :: subcomms

        logical :: splitDim

        class(*),     pointer :: optr
        character(:), pointer :: cptr

        if (size(dimNames) .ne. nTotalDims) then
            write(msgstr,*) 'Incompatible dimension sizes in decompose:',&
                &nTotalDims,size(dimNames),globalCounts
            call error(msgstr)
        end if

        subcomms = this%topology%getSubCommunicators()

        subcommCounter = 0

        do i=1,nTotalDims
            splitDim = .false.
            call this%splitDims%first()

            do j=1,this%splitDims%getListSize()
                optr => this%splitDims%currentValue()

                select type(optr)
                    type is (character(len=*))
                        cptr => optr
                    class default
                        call error('Unknown class in splitDims list')
                end select

                if (trim(dimNames(i)) == trim(cptr)) then
                    splitDim = .true.
                    exit ! the loop
                end if

                call this%splitDims%next()
            end do

            if (splitDim) then
                subcommCounter = subcommCounter + 1

                call MPI_Comm_size(subcomms(subcommCounter),subCommSize,ierr); call mpichk(ierr,&
                    & 'In parallelDecomposition constructor subcomm MPI_Comm_size:')

                call MPI_Comm_rank(subcomms(subcommCounter),subCommRank,ierr); call mpichk(ierr,&
                    & 'In parallelDecomposition constructor subcomm MPI_Comm_rank:')

                if (globalCounts(i)-staggers(i) < 1) then
                    call error('Invalid global count and stagger combination for dimension ' // &
                        int2str(i) // ' - ' // int2str(globalCounts(i)) // ' vs. ' // &
                        int2str(staggers(i)))
                end if

                ! in mpiutils
                call decomposeElements(globalCounts(i)-staggers(i),subCommSize,subCommRank,&
                    & localCounts(i),localStarts(i))

                ! add the stagger back in
                localCounts(i) = localCounts(i) + staggers(i)
            else
                localCounts(i) = globalCounts(i)
                localStarts(i) = 1
            end if
        end do
    end subroutine

    function getNumSplitDimensions(this) result(nSplitDims)
        implicit none

        class(ParallelDecomposition) :: this

        integer :: nSplitDims

        nSplitDims = this%splitDims%getListSize()
    end function

    function getSplitDimensions(this) result(splitDims)
        implicit none

        class(ParallelDecomposition) :: this

        class(LinkedList), pointer :: splitDims

        splitDims => this%splitDims
    end function

    function getTopology(this) result(topology)
        implicit none

        class(ParallelDecomposition) :: this

        class(GridTopology), pointer :: topology

        topology => this%topology
    end function
end module
