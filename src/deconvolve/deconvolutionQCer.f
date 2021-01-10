module deconvolutionQCer_mod

    use parallelInfo_mod

    use iso_fortran_env

    use observationProcessor_mod

    use obsQCCodes_mod
    use observation_mod

    use satelliteObservation_mod

    use mpiUtils_mod

    implicit none

    private

    type, extends(ObservationProcessor), public :: DeconvolutionQCer
        contains
            procedure :: deconvolutionQCerConstructor

            procedure :: process

            final :: deconvolutionQCerDestructor
    end type

    contains

    subroutine deconvolutionQCerConstructor(this)
        class(DeconvolutionQCer) :: this

        ! for now, do nothing
    end subroutine

    subroutine deconvolutionQCerDestructor(this)
        implicit none

        type(DeconvolutionQCer)  :: this

    end subroutine

    subroutine process(this,pinfo,obsIn,obsOut,stage,newObject)

        class(DeconvolutionQCer)         :: this

        class(ParallelInfo), pointer     :: pinfo
        class(Observation),  pointer     :: obsIn
        class(Observation),  pointer     :: obsOut
        integer,             intent(in)  :: stage
        logical,             intent(out) :: newObject

        real(real64), pointer :: obsData(:,:)
        integer, pointer :: owners(:)

        integer :: j, k, ierr

        class(SatelliteObservation), pointer :: satObs

        select type(obsIn)
            class is (SatelliteObservation)
                satObs => obsIn
            class default
                write(msgstr,*) 'Unknown type in DeconvolutionQCer qualityCheck'
                call error(msgstr)
        end select

        obsData => obsIn%getObsData()
        owners => obsIn%getObsOwners()

        do k=1,size(obsData,2)
            if (owners(k) == pinfo%getRank()) then
                do j=1,size(obsData,1)
                    if (obsData(j,k) > 300.d0 .or. obsData(j,k) < 10.d0) then
                        call obsIn%setQCCode(j,k,QC_BADOBERROR)
                    end if
                end do
            end if
        end do

        obsOut => obsIn
        newObject = .false.
    end subroutine
end module
