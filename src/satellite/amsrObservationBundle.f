module amsrObservationBundle_mod

    use parallelInfo_mod

    use iso_fortran_env

    use linkedList_mod

    use obsQCCodes_mod
    use observation_mod
    use observationOperator_mod

    use scannedObservationBundle_Mod

    use amsrObservation_mod

    use abstractVectorOperator_mod
    use diagonalMatrixOperator_mod

    use scannedObservation_mod
    use scannedObservationOperator_mod
    use conicalScanningObservation_mod
    use conicalScanningObsOperator_mod
    use observationProcessingChain_mod

    use dataGrid_mod
    use dataVariable_mod
    use dataDimension_mod

    use mpiUtils_mod

    implicit none

    private

    type, extends(ScannedObservationBundle), public :: AmsrObservationBundle
        !private
            ! TODO: rather than have obs, obsop, and obserr, put them together in a triplet class
            class(AmsrObservation),            pointer :: amsrObs89A         => NULL()
            class(AmsrObservation),            pointer :: amsrObs89B         => NULL()
            class(LinkedList),                 pointer :: otherObs           => NULL()

            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiagA      => NULL()
            class(DiagonalMatrixOperator),     pointer :: rInvHalfDiagB      => NULL()
            class(LinkedList),                 pointer :: otherRInvHalfDiags => NULL()

            class(ConicalScanningObsOperator), pointer :: antennaObsOp89A      => NULL()
            class(ConicalScanningObsOperator), pointer :: antennaObsOp89B      => NULL()
            class(LinkedList),                 pointer :: otherAntennaObsOps => NULL()

            ! this parameter controls the behavior of the observation operator antenna matrices.
            ! 1: recompute, don't save. 2: recompute, save. 3: load from file
            ! integer, private :: saveFile = 3

        contains
            procedure :: amsrObservationBundleConstructor

            procedure :: getScannedObservation
            procedure :: getAntennaPatternObsOp
            procedure :: getObsErrOp

            procedure :: getAmsrObs
            procedure :: getDiagObsErrOp
            procedure :: getNChannels
!            procedure :: getNObs
            procedure :: getTotalNChannels

            final :: amsrObservationBundleDestructor ! clean up all allocated variables
    end type

    contains

    subroutine amsrObservationBundleConstructor(this,pinfo,orbitFile,inputGrid, &
        & amsrNum,obsProcessor,minGoodRatio,obsErrInflation_opt,columnNormsVar)

        implicit none

        class(AmsrObservationBundle)                  :: this
        class(ParallelInfo),               pointer    :: pinfo
        character(len=*),                  intent(in) :: orbitFile
        class(DataGrid),                   pointer    :: inputGrid
!        character(len=1024),               intent(in) :: orbitFile
        integer,                           intent(in) :: amsrNum
        class(ObservationProcessingChain), pointer    :: obsProcessor
!        real(real64),            optional, intent(in) :: minLat
!        real(real64),            optional, intent(in) :: maxLat
!        real(real64),            optional, intent(in) :: minLon
!        real(real64),            optional, intent(in) :: maxLon
!        integer,                 optional, intent(in) :: minScan
!        integer,                 optional, intent(in) :: maxScan
!        type(datetime),          optional, intent(in) :: cdate
!        type(timedelta),         optional, intent(in) :: maxTimeDiff

        real(real64),            optional, intent(in) :: minGoodRatio
        real(real64),            optional, intent(in) :: obsErrInflation_opt
        class(DataVariable),     optional, pointer    :: columnNormsVar

        real(real64) :: obsErrInflation

        class(ConicalScanningObservation), pointer    :: obs89a            => NULL()
        class(ConicalScanningObservation), pointer    :: obs89b            => NULL()
        class(ConicalScanningObservation), pointer    :: obsOther          => NULL()

        class(AmsrObservation),            pointer    :: tmpobs1           => NULL()
        class(AmsrObservation),            pointer    :: tmpobs2           => NULL()

        class(AmsrObservation),            pointer    :: obs_a             => NULL()
        class(ConicalScanningObsOperator), pointer    :: antennaObsOpOther => NULL()
        class(DiagonalMatrixOperator),     pointer    :: rInvHalfDiagOther => NULL()

        class(Observation),                pointer    :: obsptr1
        class(Observation),                pointer    :: obsptr2

        class(*),                          pointer    :: optr

        integer            :: i
        integer            :: nother
!        character(len=4)   :: filenum

        if (present(obsErrInflation_opt)) then
            obsErrInflation = obsErrInflation_opt
            if (obsErrInflation <= 0.d0) then
                write(msgstr,*) 'Warning: in amsrObservationBundle obsErrInflation was',obsErrInflation,&
                    &', resetting to 1'
                call print(msgstr)
                obsErrInflation = 1.d0
            end if
        else
            obsErrInflation = 1.d0
        end if

        if (amsrNum == 1) then
            ! AMSR
            nother = 7
        else if (amsrNum == 2) then
            ! AMSR-E
            nother = 5
        else if (amsrNum == 3) then
            ! AMSR-2
            nother = 6
        end if

        this%otherObs           => LinkedList()
        this%otherRInvHalfDiags => LinkedList()
        this%otherAntennaObsOps => LinkedList()

        do i=1,nother
            allocate(tmpobs1)

            call tmpobs1%amsrObservationConstructor(pinfo,orbitFile,i,amsrNum)

            ! assign observation ownership, apply preliminary QC, etc.
            obsptr1 => tmpobs1; obsptr2 => obs_a
            call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)

            deallocate(tmpObs1)
            nullify(tmpobs1)

            optr     => obs_a

            call this%otherObs%add(optr)

            obsOther => obs_a

            allocate(antennaObsOpOther)

!            write(filenum,'(I4)') i+1000

!            if (this%saveFile == 1) then
            call antennaObsOpOther%conicalScanningObsOperatorConstructor(pinfo,obsOther,&
                & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!            elseif (this%saveFile == 2) then
!                call antennaObsOpOther%conicalScanningObsOperatorConstructor(obsOther,&
!                    &dataSet,'amsrObsOp' // filenum(2:4),minGoodRatio,columnNormsVar)
!            else
!                call antennaObsOpOther%conicalScanningObsOperatorConstructor_load(obsOther,&
!                    &dataSet,'amsrObsOp' // filenum(2:4),columnNorms)
!            end if

            optr => antennaObsOpOther

            call this%otherAntennaObsOps%add(optr)

            allocate(rInvHalfDiagOther)
            call rInvHalfDiagOther%diagonalMatrixOperatorConstructor(&
                &1.d0/dble(obs_a%getObservationError())/obsErrInflation)

            optr => rInvHalfDiagOther

            call this%otherRInvHalfDiags%add(optr)

            nullify(obs_a)
            nullify(rInvHalfDiagOther)
            nullify(antennaObsOpOther)
        end do

        allocate(this%amsrObs89A,this%amsrObs89B)

        allocate(tmpobs1)
        call tmpobs1%amsrObservationConstructor(pinfo,orbitFile,nother+1,amsrNum)
        obsptr1 => tmpobs1; obsptr2 => this%amsrObs89A
        call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)

        allocate(tmpobs2)
        call tmpobs2%amsrObservationConstructor(pinfo,orbitFile,nother+2,amsrNum)
        obsptr1 => tmpobs2; obsptr2 => this%amsrObs89B
        call obsProcessor%applyChain(pinfo,obsptr1,obsptr2,PRIOR_STAGE)

        obs89a  => this%amsrObs89A
        obs89b  => this%amsrObs89B

        allocate(this%antennaObsOp89A)
        allocate(this%antennaObsOp89B)

!        if (this%saveFile == 1) then
        call this%antennaObsOp89A%conicalScanningObsOperatorConstructor(pinfo,obs89a,&
            & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
        call this%antennaObsOp89B%conicalScanningObsOperatorConstructor(pinfo,obs89b,&
            & inputGrid,minGoodRatio=minGoodRatio,columnNormsVar=columnNormsVar)
!        elseif (this%saveFile == 2) then
!            call this%antennaObsOp89A%conicalScanningObsOperatorConstructor(pinfo,obs89a,&
!                &dataSet,'amsrObsOp007',minGoodRatio,columnNorms)
!            call this%antennaObsOp89B%conicalScanningObsOperatorConstructor(pinfo,obs89b,&
!                &dataSet,'amsrObsOp008',minGoodRatio,columnNorms)
!        else
!            call error('Need to make some changes to load the operator from disk.')
!            call this%antennaObsOp89A%conicalScanningObsOperatorConstructor_load(obs89a,&
!                &dataSet,'amsrObsOp007',columnNorms)
!            call this%antennaObsOp89B%conicalScanningObsOperatorConstructor_load(obs89b,&
!                &dataSet,'amsrObsOp008',columnNorms)
!        end if

        allocate(this%rInvHalfDiagA)
        call this%rInvHalfDiagA%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%amsrObs89A%getObservationError())/obsErrInflation)

        allocate(this%rInvHalfDiagB)
        call this%rInvHalfDiagB%diagonalMatrixOperatorConstructor(&
            &1.d0/dble(this%amsrObs89B%getObservationError())/obsErrInflation)

        call this%scannedObservationBundleConstructor(2+nother)
    end subroutine

    subroutine amsrObservationBundleDestructor(this)
        implicit none

        type(AmsrObservationBundle) :: this

        if (associated(this%amsrObs89A)) then
            deallocate(this%amsrObs89A)
        end if

        if (associated(this%amsrObs89B)) then
            deallocate(this%amsrObs89B)
        end if

        if (associated(this%otherObs)) then
            deallocate(this%otherObs)
        end if

        if (associated(this%antennaObsOp89A)) then
            deallocate(this%antennaObsOp89A)
        end if

        if (associated(this%antennaObsOp89B)) then
            deallocate(this%antennaObsOp89B)
        end if

        if (associated(this%otherAntennaObsOps)) then
            deallocate(this%otherAntennaObsOps)
        end if

        if (associated(this%rInvHalfDiagA)) then
            deallocate(this%rInvHalfDiagA)
        end if

        if (associated(this%rInvHalfDiagB)) then
            deallocate(this%rInvHalfDiagB)
        end if
        if (associated(this%otherRInvHalfDiags)) then
            deallocate(this%otherRInvHalfDiags)
        end if
    end subroutine

    function getScannedObservation(this,bundleNum) result (obs)
        implicit none

        class(AmsrObservationBundle)       :: this
        integer, intent(in)                :: bundleNum

        class(ScannedObservation), pointer :: obs

        obs => this%getAmsrObs(bundleNum)
    end function

    function getObsErrOp(this,bundleNum) result (obsErrOp)
        implicit none

        class(AmsrObservationBundle)             :: this
        integer, intent(in)                      :: bundleNum

        class(AbstractVectorOperator), pointer   :: obsErrOp

        obsErrOp => this%getDiagObsErrOp(bundleNum)
    end function

    function getAmsrObs(this,bundleNum) result (amsrObs)
        implicit none

        class(AmsrObservationBundle)    :: this
        integer, intent(in)             :: bundleNum

        class(AmsrObservation), pointer :: amsrObs

        class(*), pointer :: optr
        integer :: i

        if (bundleNum > 0 .and. bundleNum <= this%getBundleSize() - 2) then
            call this%otherObs%first()

            optr => this%otherObs%get(bundleNum)

            select type(optr)
                class is (AmsrObservation)
                    amsrObs => optr
                class default
                    write(msgstr,*) 'Unknown class in AmsrObservation observation list'
                    call error(msgstr)
            end select
        elseif (bundleNum == this%getBundleSize() - 1) then
            amsrObs => this%amsrObs89A
        elseif (bundleNum == this%getBundleSize()) then
            amsrObs => this%amsrObs89B
        else
            write(msgstr,*) 'Unknown AMSR observation:',bundleNum,this%getBundleSize()
            call error(msgstr)
        end if
    end function

    function getAntennaPatternObsOp(this,bundleNum) result (obsOp)
        implicit none

        class(AmsrObservationBundle)                  :: this
        integer,                           intent(in) :: bundleNum

        class(ScannedObservationOperator), pointer    :: obsOp

        class(*), pointer :: optr
        integer :: i

        if (bundleNum > 0 .and. bundleNum <= this%getBundleSize() - 2) then
            call this%otherAntennaObsOps%first()

            optr => this%otherAntennaObsOps%get(bundleNum)

            select type(optr)
                class is (ScannedObservationOperator)
                    obsOp => optr
                class default
                    call error('Unknown class in AmsrObservation obsop list')
            end select
        elseif (bundleNum == this%getBundleSize() - 1) then
                obsOp => this%antennaObsOp89A
        elseif (bundleNum == this%getBundleSize()) then
                obsOp => this%antennaObsOp89B
        else
            write(msgstr,*) 'Unknown AMSR observation:',bundleNum,this%getBundleSize()
            call error(msgstr)
        end if
    end function

    function getDiagObsErrOp(this,bundleNum) result (diagObsErr)
        implicit none

        class(AmsrObservationBundle)           :: this
        integer, intent(in)                    :: bundleNum

        class(DiagonalMatrixOperator), pointer :: diagObsErr

        class(*), pointer :: optr
        integer :: i

        if (bundleNum > 0 .and. bundleNum <= this%getBundleSize() - 2) then
            call this%otherRInvHalfDiags%first()

            optr => this%otherRInvHalfDiags%get(bundleNum)

            select type(optr)
                class is (DiagonalMatrixOperator)
                    diagObsErr => optr
                class default
                    call error('Unknown class in AmsrObservation obserr list')
            end select
        elseif (bundleNum == this%getBundleSize() - 1) then
            diagObsErr => this%rInvHalfDiagA
        elseif (bundleNum == this%getBundleSize()) then
            diagObsErr => this%rInvHalfDiagB
        else
            write(msgstr,*) 'Unknown AMSR observation:',bundleNum,this%getBundleSize()
            call error(msgstr)
        end if
    end function

!    function getNObs(this,bundleNum) result(nobs)
!        implicit none
!
!        class(AmsrObservationBundle)    :: this
!        integer, intent(in)             :: bundleNum
!
!        integer                         :: nobs
!
!        class(AmsrObservation), pointer :: amsrObs
!
!        amsrObs => this%getAmsrObs(bundleNum)
!
!        nobs = amsrObs%getNObs()
!    end function

    function getNPassesQC(this,bundleNum) result(npassesqc)
        implicit none

        class(AmsrObservationBundle) :: this
        integer, intent(in)          :: bundleNum

        integer                      :: npassesqc

        class(AmsrObservation), pointer :: amsrObs

        print *,'inside amsr obs getNPassesQC'

        amsrObs => this%getAmsrObs(bundleNum)

        print *,'inside amsr obs getNPassesQC 2',associated(amsrObs)

        npassesqc = amsrObs%getNPassesQc()
    end function

    function getNChannels(this,bundleNum) result(nchan)
        implicit none

        class(AmsrObservationBundle) :: this
        integer, intent(in)          :: bundleNum

        integer                      :: nchan

        class(AmsrObservation), pointer :: amsrObs

        class(DataDimension),   pointer :: ddim

        amsrObs => this%getAmsrObs(bundleNum)

        ddim => amsrObs%getMObsDim()

        nchan = ddim%getGlobalCount()
    end function

    function getTotalNChannels(this,bundleNum) result(nchan)
        implicit none

        class(AmsrObservationBundle) :: this
        integer, intent(in)          :: bundleNum

        integer                      :: nchan

        class(AmsrObservation), pointer :: amsrObs
        class(DataDimension),   pointer :: ddim

        amsrObs => this%getAmsrObs(bundleNum)

        ddim => amsrObs%getMObsDim()

        nchan = ddim%getGlobalCount() + amsrObs%getChannelOffset()
    end function
end module
