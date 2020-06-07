module adjointAveFirstGuesser_mod

    use iso_fortran_env

    use firstGuesser_mod

    use dataSet_mod
    use dataExtent_mod
    use dataVariable_mod

    use obsQCCodes_mod
    use observation_mod

    use satelliteObservation_mod

    use scannedObservation_mod
    use scannedObservationOperator_mod
    use scannedObservationBundle_mod

    use mpiUtils_mod

    implicit none

    private

    public :: AdjointAveFirstGuesser

    type, extends(FirstGuesser) :: AdjointAveFirstGuesser
        contains
            procedure :: adjointAveFirstGuesserConstructor

            procedure :: populateFirstGuess

            final :: adjointAveFirstGuesserDestructor ! clean up all allocated variables
    end type

    contains

    subroutine adjointAveFirstGuesserConstructor(this)
        implicit none

        class(AdjointAveFirstGuesser) :: this
    end subroutine

    subroutine adjointAveFirstGuesserDestructor(this)
        implicit none

        type(AdjointAveFirstGuesser)  :: this
    end subroutine

    subroutine populateFirstGuess(this,obsBundle,firstGuess)
        implicit none

        class(AdjointAveFirstGuesser) :: this

        class(ScannedObservationBundle),   pointer :: obsBundle
        class(DataSet),                    pointer :: firstGuess

        real(real64), dimension(:,:),      pointer :: lat, lon

        real(real64) :: xv,yv

        class(ScannedObservation),         pointer :: obs_so

        class(ScannedObservationOperator), pointer :: obsOp_conv
        class(Observation),                pointer :: obs

        class(DataSet),                    pointer :: tmpState
        class(DataExtent),                 pointer :: mobsExtent
        class(DataExtent),                 pointer :: nobsExtent
        class(DataVariable),               pointer :: tbVar1
        class(DataVariable),               pointer :: tbVar2
        class(DataVariable),               pointer :: obsQcVar

        integer :: i, ind1, ind2, j, nchan, npts

        integer      :: nloc
        real(real64) :: mslon


        real(real64), pointer :: output(:,:)

        real(real64), pointer :: obsData(:,:)

        real(real64), pointer :: tb1(:,:)
        real(real64), pointer :: tb2(:,:)

        integer,      pointer :: qcCodes(:,:)

        class(SatelliteObservation), pointer :: firstGuess_so
        class(SatelliteObservation), pointer :: tmpState_so

        select type (firstGuess)
            class is (SatelliteObservation)
                firstGuess_so => firstGuess
            class default
                call error('Unknown satellite observation type')
        end select

        tmpState_so => firstGuess_so%cloneSatObs(shallow=.false.,copyData=.true.)

        tmpState => tmpState_so

        tbVar1 => firstGuess_so%getObsDataVar()
        tbVar2 =>   tmpState_so%getObsDataVar()

        call tbVar1%getArray(tb1)
        call tbVar2%getArray(tb2)

        tb1 = 0.0d0
        tb2 = 0.0d0

        do i=1,obsBundle%getBundleSize()
            obs_so => obsBundle%getScannedObservation(i)

            obsOp_conv => obsBundle%getAntennaPatternObsOp(i)

            obs => obs_so

            mobsExtent => obs%getMObsExtent()
            nobsExtent => obs%getNObsExtent()

            nchan = mobsExtent%getLocalCount()
            npts  = nobsExtent%getLocalCount()

            if (nchan == 0) then
                cycle
            end if

            write(msgstr,*) 'in bundle ',i,'passes qc:',obs%getNPassesQC(),&
                nchan*npts
            call print(msgstr)

            allocate(output(nchan,npts))

            obsData => obs%getObsData()

            output(:,:) = obsData(:,:)

            ! do ind2=1,npts
            !     do ind1=1,nchan
            !         if (obsData(ind1,ind2) > 0) then !obs_so%passesQC(ind1,ind2)) then
            !             output(ind1,ind2) = obsData(ind1,ind2)
            !         else
            !             output(ind1,ind2) = 0.d0
            !         end if
            !     end do
            ! end do

            do ind1=1,nchan
                write(msgstr,*) 'min/max value for channel ',ind1+obs_so%getChannelOffset(),&
                    ':',minval(output(ind1,:)),maxval(output(ind1,:))
                call print(msgstr)
            end do

            call obsOp_conv%adjoint(firstGuess, obs, output, firstGuess)

            output(:,:) = 1.0d0

            ! do ind2=1,npts
            !     do ind1=1,nchan
            !         if (obsData(ind1,ind2) > 0) then !obs_so%passesQC(ind1, ind2)) then
            !             output(ind1,ind2) = 1.d0
            !         else
            !             output(ind1,ind2) = 0.d0
            !         end if
            !     end do
            ! end do

            call obsOp_conv%adjoint(tmpState, obs, output, tmpState)

            deallocate(output)
        end do

        do i=1,size(tb1,1)
            write(msgstr,*) 'min/max value for channel ',i,':',minval(tb1(i,:)),&
                maxval(tb1(i,:)),minval(tb2(i,:)),maxval(tb2(i,:))
            call print(msgstr)
        end do

        where (tb2 > 0.d0) tb1 = tb1/tb2

        do i=1,size(tb1,1)
            write(msgstr,*) 'min/max value for channel ',i,':',minval(tb1(i,:)),&
                maxval(tb1(i,:))
            call print(msgstr)
        end do

        qcCodes => firstGuess_so%getQcCodes()

        do j=1,size(qcCodes,2)
            do i=1,size(qcCodes,1)
                if (tb2(i,j) > 0.d0) then
                    qcCodes(i,j) = QC_NOERR
                else
                    qcCodes(i,j) = QC_OUTDOM
                end if
            end do
        end do

        deallocate(tmpState)
    end subroutine
end module
