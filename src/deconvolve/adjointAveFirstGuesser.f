module adjointAveFirstGuesser_mod

    use iso_fortran_env

    use firstGuesser_mod

    use dataSet_mod
    use dataExtent_mod
    use dataVariable_mod

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

        class(ScannedObservationBundle), pointer :: obsBundle
        class(DataSet),      pointer :: firstGuess

        real(real64), dimension(:,:),         pointer :: lat, lon

        real(real64) :: xv,yv

        integer                            :: nloc
        real(real64)                            :: mslon

        class(ScannedObservation),         pointer :: obs_so

        class(ScannedObservationOperator), pointer :: obsOp_conv
        class(Observation),                pointer :: obs

        class(DataSet),                    pointer :: tmpState
        class(DataExtent),                 pointer :: mobsExtent
        class(DataExtent),                 pointer :: nobsExtent
        class(DataVariable),               pointer :: tbVar1
        class(DataVariable),               pointer :: tbVar2

        integer :: i, ind1, ind2, nx, ny

        real(real64), pointer :: output(:,:)

        real(real64), pointer :: obsData(:,:)

        real(real64), pointer :: tb1(:,:,:)
        real(real64), pointer :: tb2(:,:,:)

        tmpState => firstGuess%clone(.true.)

        call firstGuess%zeroAll()
        call tmpState%zeroAll()

        do i=1,obsBundle%getBundleSize()
            obs_so => obsBundle%getScannedObservation(i)

            obsOp_conv => obsBundle%getAntennaPatternObsOp(i)

            obs => obs_so

            mobsExtent => obs%getMObsExtent()
            nobsExtent => obs%getNObsExtent()

            nx = mobsExtent%getLocalCount()
            ny = nobsExtent%getLocalCount()

            write(msgstr,*) 'in bundle ',i,'passes qc:',obs%getNPassesQC(),&
                nx*ny
            call print(msgstr)

            allocate(output(nx,ny))

            obsData => obs%getObsData()

            do ind2=1,ny
                do ind1=1,nx
                    if (obs%passesQC(ind1, ind2)) then
                        output(ind1,ind2) = obsData(ind1,ind2)
                    else
                        output(ind1,ind2) = 0.d0
                    end if
                end do
            end do

            call obsOp_conv%adjoint(firstGuess, obs, output, firstGuess)

            do ind2=1,ny
                do ind1=1,nx
                    if (obs_so%passesQC(ind1, ind2)) then
                        output(ind1,ind2) = 1.d0
                    else
                        output(ind1,ind2) = 0.d0
                    end if
                end do
            end do
            call obsOp_conv%adjoint(firstGuess, obs, output, tmpState)

            deallocate(output)
        end do

        tbVar1 => firstGuess%getVariableByName(TB_VAR_NAME)
        tbVar2 => tmpState%getVariableByName(TB_VAR_NAME)

        call tbVar1%getArray(tb1)
        call tbVar2%getArray(tb2)

        where (tb2 > 0.d0) tb1 = tb1/tb2

        do i=1,size(tb1,1)
            write(msgstr,*) 'min/max value for channel ',i,':',minval(tb1(i,:,:)),&
                maxval(tb1(i,:,:))
            call print(msgstr)
        end do

        deallocate(tmpState)
    end subroutine
end module
