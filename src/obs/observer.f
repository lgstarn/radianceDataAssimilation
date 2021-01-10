module observer_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use simple1DVector_mod
    use dataSet_mod
    use linkedList_mod
    use observation_mod
    use observationOperator_mod
    use observationBundle_mod
    use dataExtent_mod
    use mpiUtils_mod

    implicit none

    private

    type, public :: Observer
        private
            integer                              :: totalObs          = 0
            class(LinkedList),           pointer :: observations      => null()
            class(LinkedList),           pointer :: obsOperators      => null()
            class(LinkedList),           pointer :: rInvHalfOperators => null()

        contains
            procedure :: ObserverConstructor

            procedure, private :: nextObservation
            procedure, private :: nextObsOperator
            procedure, private :: nextRInvHalf

            procedure :: addObservation
            procedure :: addObservationBundle
            procedure :: getTotalObs
            procedure :: getYDiff
            procedure :: runObservationOperators
            procedure :: applyObsAdjoint
            procedure :: applyRInvHalf

            final :: ObserverDestructor
    end type

    contains

    subroutine observerConstructor(this)
        implicit none

        class(Observer)                                :: this

        this%observations      => LinkedList()
        this%obsOperators      => LinkedList()
        this%rInvHalfOperators => LinkedList()
    end subroutine

    subroutine observerDestructor(this)
        implicit none

        type(Observer)  :: this

        class(*), pointer           :: upointer

        if (associated(this%observations)) then
            call this%observations%deleteAll()
            deallocate(this%observations)
        end if

        if (associated(this%obsOperators)) then
            call this%rInvHalfOperators%deleteAll()
            deallocate(this%obsOperators)
        end if

        if (associated(this%rInvHalfOperators)) then
            call this%rInvHalfOperators%deleteAll()
            deallocate(this%rInvHalfOperators)
        end if
    end subroutine

    subroutine addObservation(this, obs, obsOperator, rInvHalfOperator)
        implicit none

        class(Observer)                        :: this
        class(Observation),            pointer :: obs
        class(ObservationOperator),    pointer :: obsOperator
        class(AbstractVectorOperator), pointer :: rInvHalfOperator
        class(DataExtent),             pointer :: mobsExtent
        class(DataExtent),             pointer :: nobsExtent

        class(*), pointer :: optr

        optr => obs
        call this%observations%add(optr)
        optr => obsOperator
        call this%obsOperators%add(optr)
        optr => rInvHalfOperator
        call this%rInvHalfOperators%add(optr)

        mobsExtent => obs%getMObsExtent()
        nobsExtent => obs%getNObsExtent()

        !print *,'Adding ',obs%getMObs()*obs%getNObs(),'obs'

        this%totalObs = this%totalObs + &
            mobsExtent%getLocalCount()*nobsExtent%getLocalCount()
    end subroutine

    subroutine addObservationBundle(this, obsBundle)
        implicit none

        class(Observer)                        :: this
        class(ObservationBundle),      pointer :: obsBundle

        integer :: i

        do i=1,obsBundle%getBundleSize()
            call this%addObservation(obsBundle%getObservation(i),obsBundle%getObsOp(i),&
                &obsBundle%getObsErrOp(i))
        end do
    end subroutine

    function nextObservation(this) result(obs)
        implicit none

        class(Observer)        :: this

        class(Observation), pointer :: obs
        class(*),           pointer :: o_ptr

        o_ptr => this%observations%currentValue()

        if (associated(o_ptr)) then
            select type(o_ptr)
                class is (Observation)
                    obs => o_ptr
                    call this%observations%next()
                class default
                    call error('Unknown class in observation list')
            end select
        end if
    end function

    function nextObsOperator(this) result(obsOp)
        implicit none

        class(Observer)        :: this

        class(ObservationOperator), pointer :: obsOp
        class(*),                   pointer :: o_ptr

        o_ptr => this%obsOperators%currentValue()

        if (associated(o_ptr)) then
            select type(o_ptr)
                class is (ObservationOperator)
                    obsOp => o_ptr
                    call this%obsOperators%next()
                class default
                    call error('Unknown class in obs op list')
            end select
        end if
    end function

    function nextRInvHalf(this) result(rInvHalf)
        implicit none

        class(Observer)        :: this

        class(AbstractVectorOperator), pointer :: rInvHalf
        class(*),                      pointer :: o_ptr

        o_ptr => this%rInvHalfOperators%currentValue()

        if (associated(o_ptr)) then
            select type(o_ptr)
                class is (AbstractVectorOperator)
                    rInvHalf => o_ptr
                    call this%rInvHalfOperators%next()
                class default
                    call error('Unknown class in rinvhalf list')
            end select
        end if
    end function

    function getTotalObs(this) result(totalObs)
        implicit none

        class(Observer) :: this

        integer :: totalObs

        totalObs = this%totalObs
    end function

    subroutine getYDiff(this, state, ydiff)
        implicit none

        class(Observer) :: this

        class(DataSet),        pointer :: state
        class(AbstractVector), pointer :: ydiff

        call this%runObservationOperators(state,ydiff,.true.)
    end subroutine

    subroutine runObservationOperators(this, state, yOut, applyDiff)
        implicit none

        class(Observer)                 :: this

        class(DataSet),        pointer  :: state
        class(AbstractVector), pointer  :: yOut
        logical,               optional :: applydiff

        class(Observation),         pointer :: obs
        class(ObservationOperator), pointer :: obsOp

        real(8),             dimension(:),   pointer :: yout_ptr, yptr
        real(8),             dimension(:,:), pointer :: hxData
        real(8), contiguous, dimension(:,:), pointer :: obsData
        real(8), contiguous, dimension(:,:), pointer :: obsLoci

        integer :: i, j, k
        integer :: lcursor
        integer :: cursorStart
        integer :: cursorEnd
        integer :: cursorLen
        integer :: nobs
        integer :: mobs
        logical :: doApplyDiff

        class(DataExtent), pointer :: mobsExtent
        class(DataExtent), pointer :: nobsExtent

        if (yout%getSize() .ne. this%totalObs) then
            write(msgstr,*) 'Size of yout vs totalObs mismatch in observer: ',yout%getSize(),this%totalObs
            call error(msgstr)
        end if

        if (present(applyDiff)) then
            doApplydiff = applyDiff
        else
            doApplyDiff = .false.
        end if

        yout_ptr => yout%get1DArrayPtr()

        cursorStart = 1

        call this%observations%first()
        call this%obsOperators%first()

        !open(unit=993,file='yout.txt')

        do i=1,this%observations%getListSize()
            obs     => this%nextObservation()
            obsOp   => this%nextObsOperator()

            mobsExtent => obs%getMObsExtent()
            nobsExtent => obs%getNObsExtent()

            mobs = mobsExtent%getLocalCount()
            nobs = nobsExtent%getLocalCount()

            ! the cursorLen is the number of total observations in this obs
            cursorLen = mobs*nobs

            if (cursorLen == 0) then
                cycle
            end if

            ! end of cursor range in yout
            cursorEnd = cursorStart + cursorLen - 1

            ! set the obsData 2D pointer to the yout array range
            hxData(1:mobs,1:nobs) => yout_ptr(cursorStart:cursorEnd)

            ! call the observation operator H(x) with obsData (which is pointing to yout)
            call obsOp%forward(state, obs, hxData)

            ! now set the yptr 1D pointer to the actual observation data
            obsData => obs%getObsData()
            obsLoci => obs%getObsLoci()
            yptr(1:cursorLen) => obsData(:,:)

            lcursor = 0

            do k=1,nobs
                do j=1,mobs
                    lcursor = lcursor + 1
                    ! for obs that pass QC
                    if (obs%passesQC(j, k)) then
                        ! set yout = y - H(x) for the observation data yptr and H(x) as yout

                        !write(993,'(F12.6,F12.6,I8,F12.6,F12.6,F12.6,F12.6)') yptr(lcursor),&
                        !    yout_ptr(cursorStart),j,obsLoci(:,k)

                        if (doApplyDiff) then
                            yout_ptr(cursorStart) = yptr(lcursor) - yout_ptr(cursorStart)
                        end if
                    else
                        yout_ptr(cursorStart) = 0.d0
                    end if

                    cursorStart = cursorStart + 1
                end do
            end do
        end do

        !close(993)
    end subroutine

    subroutine applyObsAdjoint(this, deltaY, baseState, deltax)
        implicit none

        class(Observer)                :: this
        class(AbstractVector), pointer :: deltaY
        class(DataSet),        pointer :: baseState
        class(DataSet),        pointer :: deltax

        real(8), dimension(:),      pointer :: deltaY_ptr
        class(Observation),         pointer :: obs
        class(ObservationOperator), pointer :: obsOp
        real(8), dimension(:),      pointer :: obsData

        integer :: i, j, k
        integer :: cursorStart
        integer :: cursorEnd
        integer :: cursorLen
        integer :: nobs
        integer :: mobs
        integer :: lcursor

        real(8), dimension(:),   pointer :: yptr
        real(8), dimension(:,:), pointer :: deltaY2D

        class(DataExtent), pointer :: mobsExtent
        class(DataExtent), pointer :: nobsExtent

        if (deltaY%getSize() .ne. this%totalObs) then
            write(msgstr,*) 'Size of ydiff vs totalObs mismatch in observer: ',deltaY%getSize(),this%totalObs
            call error(msgstr)
        end if

        deltaY_ptr => deltaY%get1DArrayPtr()

        call deltaX%zeroAll()

        cursorStart = 1

        call this%observations%first()
        call this%obsOperators%first()

        do i=1,this%observations%getListSize()
            obs   => this%nextObservation()
            obsOp => this%nextObsOperator()

            mobsExtent => obs%getMObsExtent()
            nobsExtent => obs%getNObsExtent()

            mobs = mobsExtent%getLocalCount()
            nobs = nobsExtent%getLocalCount()

            ! the cursorLen is the number of total observations in this obs
            cursorLen = mobs*nobs

            if (cursorLen == 0) then
                cycle
            end if

            lcursor = cursorStart

            ! zero out any values that don't pass QC
            do k=1,nobs
                do j=1,mobs
                    ! for obs that pass QC
                    if (.not. obs%passesQC(j, k)) then
                        deltaY_ptr(lcursor) = 0.d0
                    end if

                    !write(993,'(F12.6,F12.6,I8,F12.6,F12.6,F12.6,F12.6)') &
                    !    &yout_ptr(cursorStart)+yptr(lcursor),yptr(lcursor),j,obs%obsLoci(:,k)

                    lcursor = lcursor + 1
                end do
            end do

            ! end of cursor range in deltaY
            cursorEnd = cursorStart + cursorLen - 1

            ! set the deltaY2D pointer to the 1D deltaY array range
            deltaY2D(1:mobs,1:nobs) => deltaY_ptr(cursorStart:cursorEnd)

            ! call the observation adjoint, which sets deltaY 1D through the pointer
            call obsOp%adjoint(baseState, obs, deltaY2D, deltaX)

            ! move the cursor in ydiff forward for the next obs
            cursorStart = cursorEnd + 1
        end do
    end subroutine

    subroutine applyRInvHalf(this, input, output)
        implicit none

        class(Observer)       :: this
        class(AbstractVector), pointer :: input
        class(AbstractVector), pointer :: output

        real(8), dimension(:),         pointer :: input_ptr
        real(8), dimension(:),         pointer :: output_ptr
        class(Observation),            pointer :: obs
        class(AbstractVectorOperator), pointer :: rInvHalf

        class(AbstractVector), pointer :: inputl
        class(AbstractVector), pointer :: outputl

        class(Simple1DVector), pointer :: inputs
        class(Simple1DVector), pointer :: outputs

        integer :: i
        integer :: cursorStart
        integer :: cursorEnd
        integer :: cursorLen
        integer :: nobs
        integer :: mobs

        real(8), dimension(:), pointer :: yptr1, yptr2
        real(8), dimension(:), pointer :: lptr1, lptr2

        class(DataExtent),  pointer :: mobsExtent
        class(DataExtent),  pointer :: nobsExtent

        if (input%getSize() .ne. this%totalObs) then
            write(msgstr,*) 'Size of input vs totalObs mismatch in observer: ',input%getSize(),this%totalObs
            call error(msgstr)
        end if

        if (output%getSize() .ne. this%totalObs) then
            write(msgstr,*) 'Size of output vs totalObs mismatch in observer: ',output%getSize(),this%totalObs
            call error(msgstr)
        end if

        input_ptr => input%get1DArrayPtr()
        output_ptr => output%get1DArrayPtr()

        cursorStart = 1

        call this%observations%first()
        call this%rInvHalfOperators%first()

        do i=1,this%observations%getListSize()
            obs      => this%nextObservation()
            rInvHalf => this%nextRInvHalf()

            mobsExtent => obs%getMObsExtent()
            nobsExtent => obs%getNObsExtent()

            mobs = mobsExtent%getLocalCount()
            nobs = nobsExtent%getLocalCount()

            ! the cursorLen is the number of total observations in this obs
            cursorLen = mobs*nobs

            if (cursorLen == 0) then
                cycle
            end if

            ! end of cursor range in deltaY
            cursorEnd = cursorStart + cursorLen - 1

            ! set the input/output pointers
            yptr1(1:cursorLen) =>  input_ptr(cursorStart:cursorEnd)
            yptr2(1:cursorLen) => output_ptr(cursorStart:cursorEnd)

            ! create new simple vectors with these arrays, an inexpensive operation as nothing is copied
            allocate(inputs)
            allocate(outputs)

            call inputs%simple1DVectorConstructor_data(yptr1)
            call outputs%simple1DVectorConstructor_data(yptr2)

            inputl  => inputs
            outputl => outputs

            call rInvHalf%applyOperator(inputl, outputl)

            ! move the cursor in ydiff forward for the next obs
            cursorStart = cursorEnd + 1

            deallocate(inputs)
            deallocate(outputs)
        end do
    end subroutine

end module
