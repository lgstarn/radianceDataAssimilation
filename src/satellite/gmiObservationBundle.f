module gmiObservationBundle_mod
    use parallelInfo_mod

    use iso_fortran_env

    use gmiObservation_mod

    use obsQCCodes_mod
    use observation_mod
    use observationOperator_mod
    use observationProcessingChain_mod

    use scannedObservation_mod
    use scannedObservationBundle_mod
    use scannedObservationOperator_mod

    use conicalScanningObservation_mod
    use conicalScanningObsOperator_mod

    use abstractVectorOperator_mod
    use diagonalMatrixOperator_mod

    use dataGrid_mod
    use dataVariable_mod
    use dataDimension_mod

    use mpiUtils_mod

    implicit none

    private

    type, extends(ScannedObservationBundle), public :: GmiObservationBundle
        !private
            class(GmiObservation),           pointer :: gmiObs1     => NULL()
            class(GmiObservation),           pointer :: gmiObs2     => NULL()

            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag1 => NULL()
            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag2 => NULL()

            class(ConicalScanningObsOperator), pointer :: antennaObsOp1 => NULL()
            class(ConicalScanningObsOperator), pointer :: antennaObsOp2 => NULL()

        contains
            procedure :: gmiObservationBundleConstructor

            procedure :: getScannedObservation
            procedure :: getAntennaPatternObsOp
            procedure :: getObsErrOp

            procedure :: getGmiObs
            procedure :: getDiagObsErrOp
            procedure :: getNChannels
            !procedure :: getNObs
            procedure :: getTotalNChannels

            final :: gmiObservationBundleDestructor ! clean up all allocated variables
    end type

    contains

    subroutine gmiObservationBundleConstructor(this,pinfo,orbitFile,inputGrid, &
        & obsProcessor,minGoodRatio,obsErrInflation_opt,columnNormsVar)

        implicit none

        class(GmiObservationBundle)                   :: this

        class(ParallelInfo),               pointer    :: pinfo
        character(len=*),                  intent(in) :: orbitFile
        class(DataGrid),                   pointer    :: inputGrid
        class(ObservationProcessingChain), pointer    :: obsProcessor

        real(real64),            optional, intent(in) :: minGoodRatio
        real(real64),            optional, intent(in) :: obsErrInflation_opt
        class(DataVariable),     optional, pointer    :: columnNormsVar

        real(real64) :: obsErrInflation

        class(GmiObservation), pointer :: tmpobs1     => NULL()
        class(GmiObservation), pointer :: tmpobs2     => NULL()

        class(Observation), pointer :: obsptr1     => NULL()
        class(Observation), pointer :: obsptr2     => NULL()

        class(ConicalScanningObservation), pointer :: obs_cs1 => NULL()
        class(ConicalScanningObservation), pointer :: obs_cs2 => NULL()

        if (present(obsErrInflation_opt)) then
            obsErrInflation = obsErrInflation_opt
            if (obsErrInflation <= 0.d0) then
                write(msgstr,*) 'Warning: in gmiObservationBundle obsErrInflation was',obsErrInflation,', resetting to 1'
                call print(msgstr)
                obsErrInflation = 1.d0
            end if
        else
            obsErrInflation = 1.d0
        end if

        allocate(tmpobs1)
        call tmpobs1%gmiObservationConstructor(pinfo,orbitFile,1)
        obsptr1 => tmpobs1; obsptr2 => this%gmiObs1
        call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
        deallocate(tmpobs1)

        allocate(tmpobs2)
        call tmpobs2%gmiObservationConstructor(pinfo,orbitFile,2)
        obsptr1 => tmpobs2; obsptr2 => this%gmiObs2
        call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
        deallocate(tmpobs2)

        obs_cs1 => this%gmiObs1
        obs_cs2 => this%gmiObs2

        allocate(this%antennaObsOp1)
        call this%antennaObsOp1%conicalScanningObsOperatorConstructor(pinfo,obs_cs1,&
            & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
        !call this%antennaObsOp1%conicalScanningObsOperatorConstructor(obs_cs1,&
        !    &dataSet,'gmiObsOp1',minGoodRatio,columnNorms)
        !call this%antennaObsOp1%conicalScanningObsOperatorConstructor_load(obs_cs1,dataSet,&
        !   'gmiObsOp1',columnNorms)

        allocate(this%antennaObsOp2)
        call this%antennaObsOp2%conicalScanningObsOperatorConstructor(pinfo,obs_cs2,&
            & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
        !call this%antennaObsOp2%conicalScanningObsOperatorConstructor(obs_cs2,&
        !    &dataSet,'gmiObsOp2',minGoodRatio,columnNorms)
        !call this%antennaObsOp2%conicalScanningObsOperatorConstructor_load(obs_cs2,dataSet,&
        !   &'gmiObsOp2',columnNorms)

        allocate(this%rInvHalfDiag1)
        call this%rInvHalfDiag1%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%gmiObs1%getObservationError())/obsErrInflation)

        allocate(this%rInvHalfDiag2)
        call this%rInvHalfDiag2%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%gmiObs2%getObservationError())/obsErrInflation)

        call this%scannedObservationBundleConstructor(2)
    end subroutine

    subroutine gmiObservationBundleDestructor(this)
        implicit none

        type(GmiObservationBundle) :: this

        if (associated(this%gmiObs1)) then
            deallocate(this%gmiObs1)
        end if
        if (associated(this%gmiObs2)) then
            deallocate(this%gmiObs2)
        end if

        if (associated(this%antennaObsOp1)) then
            deallocate(this%antennaObsOp1)
        end if
        if (associated(this%antennaObsOp2)) then
            deallocate(this%antennaObsOp2)
        end if

        if (associated(this%rInvHalfDiag1)) then
            deallocate(this%rInvHalfDiag1)
        end if
        if (associated(this%rInvHalfDiag2)) then
            deallocate(this%rInvHalfDiag2)
        end if
    end subroutine

    function getScannedObservation(this,bundleNum) result (obs)
        implicit none

        class(GmiObservationBundle)        :: this
        integer, intent(in)                :: bundleNum
        class(ScannedObservation), pointer :: obs

        obs => this%getGmiObs(bundleNum)
    end function

    function getObsErrOp(this,bundleNum) result (obsErrOp)
        implicit none

        class(GmiObservationBundle)   :: this
        integer, intent(in)           :: bundleNum
        class(AbstractVectorOperator), pointer   :: obsErrOp

        obsErrOp => this%getDiagObsErrOp(bundleNum)
    end function

    function getGmiObs(this,bundleNum) result (gmiObs)
        implicit none

        class(GmiObservationBundle)   :: this
        integer, intent(in)           :: bundleNum
        class(GmiObservation), pointer :: gmiObs

        select case(bundleNum)
            case (1)
                gmiObs => this%gmiObs1
            case (2)
                gmiObs => this%gmiObs2
            case default
                write(msgstr,*) 'Unknown GMI obs number in getGmiObs',bundleNum
                call error(msgstr)
        end select
    end function

    function getAntennaPatternObsOp(this,bundleNum) result (obsOp)
        implicit none

        class(GmiObservationBundle)   :: this
        integer, intent(in)           :: bundleNum
        class(ScannedObservationOperator), pointer :: obsOp

        select case(bundleNum)
            case (1)
                obsOp => this%antennaObsOp1
            case (2)
                obsOp => this%antennaObsOp2
            case default
                write(msgstr,*) 'Unknown GMI obs number in getAntennaPatternObsOp',bundleNum
                call error(msgstr)
        end select
    end function

    function getDiagObsErrOp(this,bundleNum) result (diagObsErr)
        implicit none

        class(GmiObservationBundle)   :: this
        integer, intent(in)           :: bundleNum
        class(DiagonalMatrixOperator), pointer :: diagObsErr

        select case(bundleNum)
            case (1)
                diagObsErr => this%rInvHalfDiag1
            case (2)
                diagObsErr => this%rInvHalfDiag2
            case default
                write(msgstr,*) 'Unknown GMI obs number in getDiagObsErrOp',bundleNum
                call error(msgstr)
        end select
    end function

    function getNChannels(this,bundleNum) result(nchan)
        implicit none

        class(GmiObservationBundle)   :: this
        integer, intent(in)           :: bundleNum
        integer                       :: nchan

        class(GmiObservation), pointer :: gmiObs

        class(DataDimension),  pointer :: ddim

        gmiObs => this%getGmiObs(bundleNum)

        ddim => gmiObs%getChannelDim()

        nchan = ddim%getGlobalCount()
    end function

!    function getNObs(this,bundleNum) result(nobs)
!        implicit none
!
!        class(GmiObservationBundle)   :: this
!        integer, intent(in)           :: bundleNum
!        integer                       :: nobs
!
!        class(GmiObservation), pointer :: gmiObs
!
!        class(DataDimension),  pointer :: ddim
!
!        gmiObs => this%getGmiObs(bundleNum)
!
!        nobs = gmiObs%getNObs()
!    end function

    function getNPassesQC(this,bundleNum) result(npassesqc)
        implicit none

        class(GmiObservationBundle)    :: this
        integer, intent(in)            :: bundleNum
        integer                        :: npassesqc

        class(GmiObservation), pointer :: gmiObs

        gmiObs => this%getGmiObs(bundleNum)

        npassesqc = gmiObs%getNPassesQc()
    end function

    function getTotalNChannels(this,bundleNum) result(nchan)
        implicit none

        class(GmiObservationBundle) :: this
        integer, intent(in)         :: bundleNum
        integer                     :: nchan

        class(GmiObservation), pointer :: gmiObs

        class(DataDimension),  pointer :: ddim

        gmiObs => this%getGmiObs(bundleNum)

        ddim => gmiObs%getChannelDim()

        nchan = ddim%getGlobalCount() + gmiObs%getChannelOffset()
    end function
end module
