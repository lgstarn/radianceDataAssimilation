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

        integer :: i, scanInd

        if (ndim /= 1) then
            call error('deconvolutionObsResSplitter only applicable for ndim = 1, not for ' // &
                int2str(ndim))
        end if

        scanInd = 1

        ! lociVals will come in sorted, and scanRanges is sorted, so we can speed things
        ! up quite a bit by caching the index
        do i=1,nloci
            if (lociVals(1,i) <= this%scanRanges(scanInd)) then
                ! subtract 1 as MPI rank is zero-based
                owners(i) = scanInd - 1
            else
                ! check if we would go beyond the edge of scanRanges
                if (scanInd + 1 > size(this%scanRanges)) then
                    call error('Invalid scanRanges: ' // int2str(nint(lociVals(1,i))) // ' was larger than ' // &
                        int2str(this%scanRanges(scanInd)) // ', but there are only ' // &
                        int2str(size(this%scanRanges)) // ' total scanRanges and cannot increment.')
                end if

                ! since no, time to increment scanInd
                scanInd = scanInd + 1
            end if
        end do
    end subroutine
end module
