module ssmisObservationBundle_mod

    use parallelInfo_mod

    use iso_fortran_env

    use scannedObservationBundle_Mod

    use abstractVectorOperator_mod
    use diagonalMatrixOperator_mod

    use obsQCCodes_mod
    use observation_mod
    use observationOperator_mod
    use observationProcessingChain_mod

    use ssmisObservation_mod
    use scannedObservation_mod
    use scannedObservationOperator_mod
    use conicalScanningObservation_mod
    use conicalScanningObsOperator_mod

    use dataGrid_mod
    use dataDimension_mod
    use dataVariable_mod

    use mpiUtils_mod

    implicit none

    private

    type, extends(ScannedObservationBundle), public :: SsmisObservationBundle
        !private
            class(SsmisObservation),           pointer :: ssmisObs1     => NULL()
            class(SsmisObservation),           pointer :: ssmisObs2     => NULL()
            class(SsmisObservation),           pointer :: ssmisObs3     => NULL()
            class(SsmisObservation),           pointer :: ssmisObs4     => NULL()

            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag1 => NULL()
            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag2 => NULL()
            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag3 => NULL()
            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiag4 => NULL()

            class(ConicalScanningObsOperator), pointer :: antennaObsOp1 => NULL()
            class(ConicalScanningObsOperator), pointer :: antennaObsOp2 => NULL()
            class(ConicalScanningObsOperator), pointer :: antennaObsOp3 => NULL()
            class(ConicalScanningObsOperator), pointer :: antennaObsOp4 => NULL()

            ! this parameter controls the behavior of the observation operator antenna matrices.
            ! 1: recompute, don't save. 2: recompute, save. 3: load from file
            integer, private :: saveFile = 1

        contains
            procedure :: ssmisObservationBundleConstructor

            procedure :: getScannedObservation
            procedure :: getAntennaPatternObsOp
            procedure :: getObsErrOp

            procedure :: getSsmisObs
            procedure :: getDiagObsErrOp
            procedure :: getNChannels
            !procedure :: getNObs
            procedure :: getTotalNChannels

            final :: ssmisObservationBundleDestructor ! clean up all allocated variables
    end type

    contains

    subroutine ssmisObservationBundleConstructor(this,pinfo,orbitFile, &
        & inputGrid,fnumber,obsProcessor,minGoodRatio,obsErrInflation_opt,columnNormsVar)
       !& minLat,maxLat,minScan,maxScan,cdate,maxTimeDiff,&

        implicit none

        class(SsmisObservationBundle)                 :: this
        class(ParallelInfo),               pointer    :: pinfo
        character(len=*),                  intent(in) :: orbitFile
        class(DataGrid),                   pointer    :: inputGrid
        integer,                           intent(in) :: fnumber
!        real(real64),                 optional, intent(in) :: minLat
!        real(real64),                 optional, intent(in) :: maxLat
!        real(real64),                 optional, intent(in) :: minLon
!        real(real64),                 optional, intent(in) :: maxLon
!        integer,                 optional, intent(in) :: minScan
!        integer,                 optional, intent(in) :: maxScan
!        type(datetime),          optional, intent(in) :: cdate
!        type(timedelta),         optional, intent(in) :: maxTimeDiff
        class(ObservationProcessingChain), optional, pointer    :: obsProcessor

        real(real64),            optional, intent(in) :: minGoodRatio
        real(real64),            optional, intent(in) :: obsErrInflation_opt
        class(DataVariable),     optional, pointer    :: columnNormsVar

        real(real64)                                       :: obsErrInflation

        class(SsmisObservation), pointer    :: tmpObs1     => NULL()
        class(SsmisObservation), pointer    :: tmpObs2     => NULL()
        class(SsmisObservation), pointer    :: tmpObs3     => NULL()
        class(SsmisObservation), pointer    :: tmpObs4     => NULL()

        class(ConicalScanningObservation), pointer    :: obs_cs1 => NULL()
        class(ConicalScanningObservation), pointer    :: obs_cs2 => NULL()
        class(ConicalScanningObservation), pointer    :: obs_cs3 => NULL()
        class(ConicalScanningObservation), pointer    :: obs_cs4 => NULL()

        class(Observation), pointer    :: obsptr1
        class(Observation), pointer    :: obsptr2

        class(SsmisObservation), pointer :: obs_s

        logical :: newObject

        integer            :: i
        character(len=4)   :: filenum
        character(len=256) :: filename

        if (present(obsErrInflation_opt)) then
            obsErrInflation = obsErrInflation_opt
            if (obsErrInflation <= 0.d0) then
                write(msgstr,*) 'Warning: in ssmisObservationBundle obsErrInflation was',obsErrInflation,', resetting to 1'
                call print(msgstr)
                obsErrInflation = 1.d0
            end if
        else
            obsErrInflation = 1.d0
        end if

!        allocate(this%ssmisObs1,this%ssmisObs2,this%ssmisObs3,this%ssmisObs4)

        ! send each observation through the processing chain, the observation that comes out
        ! is the one to keep

!        call this%ssmisObs1%ssmisObservationConstructor(orbitFile,1,fnumber,.true.,&
!            & 0,minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)
!
!        call this%ssmisObs2%ssmisObservationConstructor(orbitFile,2,fnumber,.true.,&
!            & 0,minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)
!
!        call this%ssmisObs3%ssmisObservationConstructor(orbitFile,3,fnumber,.true.,&
!            & 0,minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)
!
!        call this%ssmisObs4%ssmisObservationConstructor(orbitFile,4,fnumber,.true.,&
!            & 0,minLat,maxLat,minLon,maxLon,minScan,maxScan,cdate,maxTimeDiff)

        allocate(tmpObs1,tmpObs2,tmpObs3,tmpObs4)

        call tmpObs1%ssmisObservationConstructor(pinfo,orbitFile,1,fnumber)
        call tmpObs2%ssmisObservationConstructor(pinfo,orbitFile,2,fnumber)
        call tmpObs3%ssmisObservationConstructor(pinfo,orbitFile,3,fnumber)
        call tmpObs4%ssmisObservationConstructor(pinfo,orbitFile,4,fnumber)

        if (present(obsProcessor)) then
            ! assign observation ownership, apply preliminary QC, etc.

            obsptr1 => tmpobs1
            call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
            select type(obsptr2)
                type is (SsmisObservation)
                    this%ssmisObs1 => obsptr2
            end select

            obsptr1 => tmpobs2
            call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
            select type(obsptr2)
                type is (SsmisObservation)
                    this%ssmisObs2 => obsptr2
            end select

            obsptr1 => tmpobs3
            call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
            select type(obsptr2)
                type is (SsmisObservation)
                    this%ssmisObs3 => obsptr2
            end select

            obsptr1 => tmpobs4
            call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)
            select type(obsptr2)
                type is (SsmisObservation)
                    this%ssmisObs4 => obsptr2
            end select
        else
            this%ssmisObs1 => tmpobs1
            this%ssmisObs2 => tmpobs2
            this%ssmisObs3 => tmpobs3
            this%ssmisObs4 => tmpobs4
        end if

        obs_cs1 => this%ssmisObs1
        obs_cs2 => this%ssmisObs2
        obs_cs3 => this%ssmisObs3
        obs_cs4 => this%ssmisObs4

        call print('Now creating the SSMIS antenna operator 1')

        allocate(this%antennaObsOp1)

!        if (this%saveFile == 1) then
            call this%antennaObsOp1%conicalScanningObsOperatorConstructor(pinfo,obs_cs1,&
                & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!        elseif (this%saveFile == 2) then
!            call this%antennaObsOp1%conicalScanningObsOperatorConstructor(obs_cs1,&
!                &dataSet,'ssmisObsOp1',minGoodRatio,columnNorms)
!        else
!            call this%antennaObsOp1%conicalScanningObsOperatorConstructor_load(obs_cs1,dataSet,&
!                &'ssmisObsOp1',columnNorms)
!        end if

        call print('Now creating the SSMIS antenna operator 2')

        allocate(this%antennaObsOp2)
!        if (this%saveFile == 1) then
            call this%antennaObsOp2%conicalScanningObsOperatorConstructor(pinfo,obs_cs2,&
                & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!        elseif (this%saveFile == 2) then
!            call this%antennaObsOp2%conicalScanningObsOperatorConstructor(obs_cs2,&
!                &dataSet,'ssmisObsOp2',minGoodRatio,columnNorms)
!        else
!            call this%antennaObsOp2%conicalScanningObsOperatorConstructor_load(obs_cs2,dataSet,&
!                &'ssmisObsOp2',columnNorms)
!        end if

        call print('Now creating the SSMIS antenna operator 3')

        allocate(this%antennaObsOp3)
!        if (this%saveFile == 1) then
            call this%antennaObsOp3%conicalScanningObsOperatorConstructor(pinfo,obs_cs3,&
                & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!        elseif (this%saveFile == 2) then
!            call this%antennaObsOp3%conicalScanningObsOperatorConstructor(obs_cs3,&
!                &dataSet,'ssmisObsOp3',minGoodRatio,columnNorms)
!        else
!            call this%antennaObsOp3%conicalScanningObsOperatorConstructor_load(obs_cs3,dataSet,&
!                &'ssmisObsOp3',columnNorms)
!        end if

        call print('Now creating the SSMIS antenna operator 4')

        allocate(this%antennaObsOp4)
!        if (this%saveFile == 1) then
            call this%antennaObsOp4%conicalScanningObsOperatorConstructor(pinfo,obs_cs4,&
                & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!        elseif (this%saveFile == 2) then
!            call this%antennaObsOp4%conicalScanningObsOperatorConstructor(obs_cs4,&
!                &dataSet,'ssmisObsOp4',minGoodRatio,columnNorms)
!        else
!            call this%antennaObsOp4%conicalScanningObsOperatorConstructor_load(obs_cs4,dataSet,&
!                &'ssmisObsOp4',columnNorms)
!        end if

        call print('Now creating the SSMIS obs error covs')

        allocate(this%rInvHalfDiag1)
        call this%rInvHalfDiag1%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%ssmisObs1%getObservationError())/obsErrInflation)

        allocate(this%rInvHalfDiag2)
        call this%rInvHalfDiag2%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%ssmisObs2%getObservationError())/obsErrInflation)

        allocate(this%rInvHalfDiag3)
        call this%rInvHalfDiag3%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%ssmisObs3%getObservationError())/obsErrInflation)

        allocate(this%rInvHalfDiag4)
        call this%rInvHalfDiag4%diagonalMatrixOperatorConstructor(&
            1.d0/dble(this%ssmisObs4%getObservationError())/obsErrInflation)

        call print('Now creating the scanned observation bundle')

        call this%scannedObservationBundleConstructor(4)
    end subroutine

    subroutine ssmisObservationBundleDestructor(this)
        implicit none

        type(SsmisObservationBundle) :: this

        if (associated(this%ssmisObs1)) then
            deallocate(this%ssmisObs1)
        end if
        if (associated(this%ssmisObs2)) then
            deallocate(this%ssmisObs2)
        end if
        if (associated(this%ssmisObs3)) then
            deallocate(this%ssmisObs3)
        end if
        if (associated(this%ssmisObs4)) then
            deallocate(this%ssmisObs4)
        end if

        if (associated(this%antennaObsOp1)) then
            deallocate(this%antennaObsOp1)
        end if
        if (associated(this%antennaObsOp2)) then
            deallocate(this%antennaObsOp2)
        end if
        if (associated(this%antennaObsOp3)) then
            deallocate(this%antennaObsOp3)
        end if
        if (associated(this%antennaObsOp4)) then
            deallocate(this%antennaObsOp4)
        end if

        if (associated(this%rInvHalfDiag1)) then
            deallocate(this%rInvHalfDiag1)
        end if
        if (associated(this%rInvHalfDiag2)) then
            deallocate(this%rInvHalfDiag2)
        end if
        if (associated(this%rInvHalfDiag3)) then
            deallocate(this%rInvHalfDiag3)
        end if
        if (associated(this%rInvHalfDiag4)) then
            deallocate(this%rInvHalfDiag4)
        end if
    end subroutine

    function getScannedObservation(this,bundleNum) result (obs)
        implicit none

        class(SsmisObservationBundle)      :: this
        integer, intent(in)                :: bundleNum

        class(ScannedObservation), pointer :: obs

        obs => this%getSsmisObs(bundleNum)
    end function

    function getObsErrOp(this,bundleNum) result (obsErrOp)
        implicit none

        class(SsmisObservationBundle)            :: this
        integer, intent(in)                      :: bundleNum

        class(AbstractVectorOperator), pointer   :: obsErrOp

        obsErrOp => this%getDiagObsErrOp(bundleNum)
    end function

    function getSsmisObs(this,bundleNum) result (ssmisObs)
        implicit none

        class(SsmisObservationBundle)    :: this
        integer, intent(in)              :: bundleNum

        class(SsmisObservation), pointer :: ssmisObs

        select case(bundleNum)
            case (1)
                ssmisObs => this%ssmisObs1
            case (2)
                ssmisObs => this%ssmisObs2
            case (3)
                ssmisObs => this%ssmisObs3
            case (4)
                ssmisObs => this%ssmisObs4
            case default
                write(msgstr,*) 'Unknown SSMIS obs number in getSsmisObs',bundleNum
                call error(msgstr)
        end select
    end function

    function getAntennaPatternObsOp(this,bundleNum) result (obsOp)
        implicit none

        class(SsmisObservationBundle)                 :: this
        integer,                           intent(in) :: bundleNum

        class(ScannedObservationOperator), pointer    :: obsOp

        select case(bundleNum)
            case (1)
                obsOp => this%antennaObsOp1
            case (2)
                obsOp => this%antennaObsOp2
            case (3)
                obsOp => this%antennaObsOp3
            case (4)
                obsOp => this%antennaObsOp4
            case default
                write(msgstr,*) 'Unknown SSMIS obs number in getAntennaPatternObsOp',bundleNum
                call error(msgstr)
        end select
    end function

    function getDiagObsErrOp(this,bundleNum) result (diagObsErr)
        implicit none

        class(SsmisObservationBundle)          :: this
        integer, intent(in)                    :: bundleNum

        class(DiagonalMatrixOperator), pointer :: diagObsErr

        select case(bundleNum)
            case (1)
                diagObsErr => this%rInvHalfDiag1
            case (2)
                diagObsErr => this%rInvHalfDiag2
            case (3)
                diagObsErr => this%rInvHalfDiag3
            case (4)
                diagObsErr => this%rInvHalfDiag4
            case default
                write(msgstr,*) 'Unknown SSMIS obs number in getDiagObsErrOp',bundleNum
                call error(msgstr)
        end select
    end function

!    function getNObs(this,bundleNum) result(nobs)
!        implicit none
!
!        class(SsmisObservationBundle) :: this
!        integer, intent(in)           :: bundleNum
!
!        integer                       :: nobs
!
!        class(SsmisObservation), pointer :: ssmisObs
!
!        ssmisObs => this%getSsmisObs(bundleNum)
!
!        nobs = ssmisObs%getNObs()
!    end function

!    function getNPassesQC(this,bundleNum) result(npassesqc)
!        implicit none
!
!        class(SsmisObservationBundle) :: this
!        integer, intent(in)           :: bundleNum
!
!        integer                       :: npassesqc
!
!        class(SsmisObservation), pointer :: ssmisObs
!
!        ssmisObs => this%getSsmisObs(bundleNum)
!
!        npassesqc = ssmisObs%getNPassesQc()
!    end function

    function getNChannels(this,bundleNum) result(nchan)
        implicit none

        class(SsmisObservationBundle) :: this
        integer, intent(in)           :: bundleNum

        integer                       :: nchan

        class(SsmisObservation), pointer :: ssmisObs

        class(DataDimension),    pointer :: ddim

        ssmisObs => this%getSsmisObs(bundleNum)

        ddim => ssmisObs%getMObsDim()

        nchan = ddim%getGlobalCount()
    end function

    function getTotalNChannels(this,bundleNum) result(nchan)
        implicit none

        class(SsmisObservationBundle) :: this
        integer, intent(in)           :: bundleNum

        integer                       :: nchan

        class(SsmisObservation), pointer :: ssmisObs

        class(DataDimension),    pointer :: ddim

        ssmisObs => this%getSsmisObs(bundleNum)

        ddim => ssmisObs%getMObsDim()

        nchan = ddim%getGlobalCount() + ssmisObs%getChannelOffset()
    end function
end module
