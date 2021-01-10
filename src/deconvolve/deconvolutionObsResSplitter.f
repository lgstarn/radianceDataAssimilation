module deconvolutionObsResSplitter_mod

    use observation_mod
    use observationSplitter_mod

    use satelliteObservation_mod

    use dataGrid_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataDimension_mod
    use mirroredVariable_mod

    use parallelInfo_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DeconvolutionObsResSplitter

    type, extends(ObservationSplitter) :: DeconvolutionObsResSplitter

        private

        integer, pointer :: scanRanges(:)

        contains
            procedure :: getPointOwners

            procedure :: deconvolutionObsResSplitterConstructor

            final     :: deconvolutionObsResSplitterDestructor
    end type

    contains

    subroutine deconvolutionObsResSplitterConstructor(this,inputGrid,scanRanges)
        implicit none

        class(DeconvolutionObsResSplitter)  :: this

        class(DataGrid), pointer :: inputGrid
        integer,         pointer :: scanRanges(:)

        integer :: lociDims(1)

        lociDims(1) = SO_SCAN_DIM

        call this%observationSplitterConstructor(inputGrid,lociDims)

        this%scanRanges => scanRanges
    end subroutine

    subroutine deconvolutionObsResSplitterDestructor(this)
        implicit none

        type(DeconvolutionObsResSplitter)  :: this
    end subroutine

    subroutine getPointOwners(this,pinfo,ndim,nloci,lociVals,owners)
        implicit none

        class(DeconvolutionObsResSplitter)       :: this

        class(ParallelInfo), pointer :: pinfo

        integer,      intent(in)  :: ndim
        integer,      intent(in)  :: nloci
        real(real64), intent(in)  :: lociVals(ndim,nloci)
        integer,      intent(out) :: owners(nloci)

        integer :: i, scanInd, scannum, minScan, maxScan

        if (ndim /= 1) then
            call error('deconvolutionObsResSplitter only applicable for ndim = 1, not for ' // &
                int2str(ndim))
        end if

        scanInd = 1
        ! scanRanges of 0 has the global start minus 1
        minScan = this%scanRanges(0)
        maxScan = maxval(this%scanRanges)

        ! scanRanges is sorted, so we can speed things up quite a bit by caching the index
        do i=1,nloci
            scannum = lociVals(1,i)

            if (scannum <= minScan .or. scannum > maxScan) then
                ! out of global range
                owners(i) = -1
            elseif (scannum > this%scanRanges(scanInd-1) .and. scannum <= this%scanRanges(scanInd)) then
                ! subtract 1 as MPI rank is zero-based
                owners(i) = scanInd - 1
            else
                ! not in range of scanInd, but not out of global range. Increment to next scanInd
                scanInd = scanInd + 1

                ! and don't forget to update this owner
                owners(i) = scanInd - 1
            end if
        end do
    end subroutine
end module
