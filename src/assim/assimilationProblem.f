module assimilationProblem_mod
    use parallelInfo_mod

    use observation_mod
    use observationOperator_mod
    use observer_mod
    use penalizer_mod
    use datasetVectorConverter_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use dataSet_mod
    use simple1DVector_mod
    use observationBundle_mod

    implicit none

    private

    type, public :: assimilationProblem
        private

            integer                                :: nctrl       ! number of control variables
            logical                                :: alphaTest   ! do alpha test?

            class(ParallelInfo),           pointer :: pinfo            => NULL() ! Passed in
            real(8), dimension(:),         pointer :: initGuess        => NULL() ! dimension: nctrl x 1. Passed in
            class(DatasetVectorConverter), pointer :: converter        => NULL() ! Passed in
            class(Observer),               pointer :: obsvr            => NULL() ! Passed in
            class(DataSet),                pointer :: background       => NULL() ! Passed in
            class(DataSet),                pointer :: baseState        => NULL() ! Passed in
            class(DataSet),                pointer :: deltaX           => NULL() ! Passed in
            class(AbstractVectorOperator), pointer :: bHalf            => NULL() ! Passed in
            !class(Optimizer),              pointer :: opt              => NULL() ! Passed in; optional
            class(Penalizer),              pointer :: penmgr           => NULL() ! Passed in; optional
            real(8), dimension(:),         pointer :: finalGrad        => NULL() ! nctrl x 1. Alloced in variational solvers
            class(AbstractVector),         pointer :: ydiff            => NULL() ! Alloced based on observer
            class(AbstractVector),         pointer :: controlVector    => NULL() ! Alloced based on nctrl
            class(AbstractVector),         pointer :: backgroundVector => NULL() ! Alloced based on converter
            class(AbstractVector),         pointer :: stateVector      => NULL() ! Alloced based on converter

        contains

            procedure :: assimilationProblemConstructor

            procedure :: getParallelInfo
            procedure :: getNControl
            procedure :: getInitialGuess
            !procedure :: getOptimizer
            procedure :: getBaseDataSet
            procedure :: getDeltaXDataSet
            procedure :: getDatasetVectorConverter
            procedure :: getObserver
            procedure :: getPenalizer
            procedure :: getControlVector
            procedure :: getBackgroundVector
            procedure :: getStateVector
            procedure :: getObsDiffVector
            procedure :: getBHalfOperator
            procedure :: getFinalGrad
            procedure :: shouldDoAlphaTest

            procedure :: addObservationBundle

            final :: assimilationProblemDestructor ! clean up all allocated variables
    end type

    contains

    subroutine assimilationProblemConstructor(this,pinfo,nctrl,initGuess,converter,obsvr,&
        &background,bHalf,penmgr,alphaTest)

        implicit none

        class(AssimilationProblem)              :: this
        class(ParallelInfo),           pointer  :: pinfo
        integer, intent(in)                     :: nctrl
        real(8), dimension(:),         pointer  :: initGuess
        class(DatasetVectorConverter), pointer  :: converter
        class(Observer),               pointer  :: obsvr
        class(DataSet),                pointer  :: background
        class(AbstractVectorOperator), pointer  :: bHalf
        class(Penalizer),    optional, pointer  :: penmgr
        logical, intent(in), optional           :: alphaTest

        class(Simple1DVector), pointer :: bvec, cvec, svec, ydvec

        integer :: nobs, nstate

        this%nctrl  = nctrl

        nstate = converter%getLocalStateVectorSize(background)
        nobs   = obsvr%getTotalObs()

        this%pinfo      => pinfo
        this%initGuess  => initGuess
        this%converter  => converter
        this%obsvr      => obsvr
        this%background => background
        this%bHalf      => bHalf

        ! clone the background into a baseState and deltaX
        this%baseState  => background%clone(shallow=.false.,copyData=.true.)
        this%deltaX     => background%clone(shallow=.false.,copyData=.false.)

        allocate(bvec)
        allocate(cvec)
        allocate(svec)
        allocate(ydvec)

        call bvec%simple1DVectorConstructor(nstate)
        call cvec%simple1DVectorConstructor(nctrl)
        call svec%simple1DVectorConstructor(nstate)
        call ydvec%simple1DVectorConstructor(nobs)

        allocate(this%finalGrad(nctrl))

        this%backgroundVector => bvec
        this%controlVector    => cvec
        this%stateVector      => svec
        this%yDiff            => ydvec

        call converter%convertFromState(background,this%backgroundVector)

        if (present(alphaTest)) then
            this%alphaTest = alphaTest
        else
            this%alphaTest = .false.
        end if

        if (present(penmgr)) then
            this%penmgr => penmgr
        end if
    end subroutine

    subroutine assimilationProblemDestructor(this)
        implicit none

        type(AssimilationProblem) :: this

        if (associated(this%baseState)) then
            deallocate(this%baseState)
        end if

        if (associated(this%deltaX)) then
            deallocate(this%deltaX)
        end if

        if (associated(this%backgroundVector)) then
            deallocate(this%backgroundVector)
        end if

        if (associated(this%controlVector)) then
            deallocate(this%controlVector)
        end if

        if (associated(this%stateVector)) then
            deallocate(this%stateVector)
        end if

        if (associated(this%yDiff)) then
            deallocate(this%yDiff)
        end if

        if (associated(this%finalGrad)) then
            deallocate(this%finalGrad)
        end if
    end subroutine

    function getParallelInfo(this) result(pinfo)
        implicit none

        class(AssimilationProblem)    :: this
        class(ParallelInfo), pointer  :: pinfo

        pinfo => this%pinfo
    end function

    function getNControl(this) result(nctrl)
        implicit none

        class(AssimilationProblem)  :: this
        integer                     :: nctrl

        nctrl = this%nctrl
    end function

    function getInitialGuess(this) result(initGuess)
        implicit none

        class(AssimilationProblem)     :: this
        real(8), dimension(:), pointer :: initGuess

        initGuess => this%initGuess
    end function

    function getBaseDataSet(this) result(baseState)
        implicit none

        class(AssimilationProblem) :: this
        class(DataSet), pointer :: baseState

        baseState => this%baseState
    end function

    function getDeltaXDataSet(this) result(deltaX)
        implicit none

        class(AssimilationProblem) :: this
        class(DataSet), pointer :: deltaX

        deltaX => this%deltaX
    end function

    function getDatasetVectorConverter(this) result(converter)
        implicit none

        class(AssimilationProblem)           :: this
        class(DatasetVectorConverter), pointer :: converter

        converter => this%converter
    end function

    function getObserver(this) result(obsvr)
        implicit none

        class(AssimilationProblem) :: this
        class(Observer), pointer   :: obsvr

        obsvr => this%obsvr
    end function

    function getPenalizer(this) result(penmgr)
        implicit none

        class(AssimilationProblem) :: this
        class(Penalizer), pointer  :: penmgr

        penmgr => this%penmgr
    end function

    function getControlVector(this) result(controlVector)
        implicit none

        class(AssimilationProblem)     :: this
        class(AbstractVector), pointer :: controlVector

        controlVector => this%controlVector
    end function

    function getBackgroundVector(this) result(backgroundVector)
        implicit none

        class(AssimilationProblem)     :: this
        class(AbstractVector), pointer :: backgroundVector

        backgroundVector => this%backgroundVector
    end function

    function getStateVector(this) result(stateVector)
        implicit none

        class(AssimilationProblem)     :: this
        class(AbstractVector), pointer :: stateVector

        stateVector => this%stateVector
    end function

    function getObsDiffVector(this) result(yDiff)
        implicit none

        class(AssimilationProblem)     :: this
        class(AbstractVector), pointer :: yDiff

        yDiff => this%yDiff
    end function

    function getBHalfOperator(this) result(bHalf)
        implicit none

        class(AssimilationProblem)             :: this
        class(AbstractVectorOperator), pointer :: bHalf

        bHalf => this%bHalf
    end function

    function getFinalGrad(this) result(finalGrad)
        implicit none

        class(AssimilationProblem)     :: this
        real(8), dimension(:), pointer :: finalGrad

        finalGrad => this%finalGrad
    end function

    function shouldDoAlphaTest(this) result(alphaTest)
        implicit none

        class(AssimilationProblem)  :: this
        logical                     :: alphaTest

        alphaTest = this%alphaTest
    end function

    subroutine addObservationBundle(this,bundle)
        implicit none

        class(AssimilationProblem)             :: this
        class(ObservationBundle)               :: bundle
        class(Observation),            pointer :: obs
        class(AbstractVectorOperator), pointer :: rInvHalf
        class(ObservationOperator),    pointer :: obsOp

        integer :: i

        do i=1,bundle%getBundleSize()
            obs => bundle%getObservation(i)
            rInvHalf => bundle%getObsErrOp(i)
            obsOp => bundle%getObsOp(i)

            call this%obsvr%addObservation(obs,obsOp,rInvHalf)
        end do
    end subroutine
end module
