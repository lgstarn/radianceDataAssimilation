module vecFourierMultOperator_mod
    use, intrinsic :: iso_c_binding
    use abstractVector_mod
    use abstractVectorOperator_mod
    use vecFourierMultVector_mod
    use linkedList_mod
    use fftwVector_mod
    use fftwVectorOperator_mod

    implicit none

    private

    include 'fftw3.f03'

    public :: VecFourierMultOperator

    type, extends(AbstractVectorOperator), abstract :: VecFourierMultOperator
        !private
            class(VecFourierMultVector),   pointer :: fftVec1   => NULL()
            class(VecFourierMultVector),   pointer :: fftVec2   => NULL()
            class(VecFourierMultVector),   pointer :: fftMult   => NULL()
            class(LinkedList),             pointer :: vecOps    => NULL()
            logical                                :: runFwd    = .false.
            logical                                :: runInv    = .true.

        contains
            procedure :: vecFourierMultOperatorConstructor
            procedure :: vecFourierMultOperatorDestructor

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd
            procedure :: doApplyOperator
            procedure :: getNumControl

            procedure :: createVecOperator
            procedure :: destroyVecOperator
            procedure :: executeVecOperator
            procedure :: executeVecTranspose

            procedure(copyFromDomainAbstract),     deferred :: copyFromDomain
            procedure(copyToDomainAbstract),       deferred :: copyToDomain
            procedure(copyFromFreqSpaceAbstract),  deferred :: copyFromFreqSpace
            procedure(copyToFreqSpaceAbstract),    deferred :: copyToFreqSpace
    end type

    abstract interface

        subroutine copyFromDomainAbstract(this, in, out)
            import VecFourierMultOperator
            import AbstractVector
            import VecFourierMultVector

            class(VecFourierMultOperator)         :: this
            class(AbstractVector),        pointer :: in
            class(VecFourierMultVector),  pointer :: out
        end subroutine

        subroutine copyToDomainAbstract(this, in, out, c)
            import VecFourierMultOperator
            import VecFourierMultVector
            import AbstractVector

            class(VecFourierMultOperator)         :: this
            class(VecFourierMultVector),  pointer :: in
            class(AbstractVector),        pointer :: out
            real(8),         optional, intent(in) :: c
        end subroutine

        subroutine copyFromFreqSpaceAbstract(this, in, out)
            import VecFourierMultOperator
            import AbstractVector
            import VecFourierMultVector

            class(VecFourierMultOperator)         :: this
            class(AbstractVector),        pointer :: in
            class(VecFourierMultVector),  pointer :: out
        end subroutine

        subroutine copyToFreqSpaceAbstract(this, in, out, c)
            import VecFourierMultOperator
            import VecFourierMultVector
            import AbstractVector

            class(VecFourierMultOperator)         :: this
            class(VecFourierMultVector),  pointer :: in
            class(AbstractVector),        pointer :: out
            real(8),         optional, intent(in) :: c
        end subroutine
    end interface

    contains

    subroutine vecFourierMultOperatorConstructor(this,fftVec1,fftVec2,fftMult,runInv,runFwd)
        implicit none

        class(VecFourierMultOperator)                    :: this
        class(VecFourierMultVector),             pointer :: fftVec1
        class(VecFourierMultVector),             pointer :: fftVec2
        class(VecFourierMultVector),   optional, pointer :: fftMult
        logical,                       optional          :: runInv
        logical,                       optional          :: runFwd

        class(AbstractVector), pointer :: vptr

        if (present(fftMult)) then
            this%fftMult => fftMult
        end if

        if (present(runFwd)) then
            this%runFwd = runFwd
        end if

        if (present(runInv)) then
            this%runInv = runInv
        end if

        this%fftVec1 => fftVec1
        this%fftVec2 => fftVec2

        call this%createVecOperator(this%vecOps,this%fftVec1,this%fftVec2)
    end subroutine

    ! this can't be a finalizer since the type is abstract and Fortran doesn't
    ! allow that. So subclasses should call this method.
    subroutine vecFourierMultOperatorDestructor(this)
        implicit none

        class(VecFourierMultOperator)  :: this

        call this%destroyVecOperator(this%vecOps,this%fftVec1,this%fftVec2)

        if (associated(this%fftVec1)) then
            deallocate(this%fftVec1)
        end if

        if (associated(this%fftVec2)) then
            deallocate(this%fftVec2)
        end if
    end subroutine

    subroutine createVecOperator(this,opList,in,out)
        implicit none

        class(VecFourierMultOperator)        :: this
        class(LinkedList),           pointer :: opList
        class(VecFourierMultVector), pointer :: in
        class(VecFourierMultVector), pointer :: out

        class(FftwVector),           pointer :: inv, outv
        class(FftwVectorOperator),   pointer :: opv
        class(*),                    pointer :: optr

        integer :: i

        opList => LinkedList()

        call in%firstVector()
        call out%firstVector()

        do i=1,in%numVectors()
            inv => in%nextVector()
            outv => out%nextVector()
            allocate(opv)
            call opv%fftwVectorOperatorConstructor(inv,outv)

            optr => opv

            call opList%add(optr)

            nullify(opv)
        end do
    end subroutine

    subroutine destroyVecOperator(this,op,in,out)
        implicit none

        class(VecFourierMultOperator)        :: this
        class(LinkedList),           pointer :: op
        class(VecFourierMultVector), pointer :: in
        class(VecFourierMultVector), pointer :: out

        class(FftwVector),           pointer :: inv, outv
        class(FftwVectorOperator),   pointer :: opv
        class(*),                    pointer :: optr

        integer :: i

        if (associated(op)) then
            call op%deleteAll()
            deallocate(op)
        end if

        if (associated(in)) then
            deallocate(in)
        end if

        if (associated(out)) then
            deallocate(out)
        end if
    end subroutine

    subroutine executeVecOperator(this,op,in,out)
        implicit none

        class(VecFourierMultOperator)        :: this
        class(LinkedList),           pointer :: op
        class(VecFourierMultVector), pointer :: in
        class(VecFourierMultVector), pointer :: out

        class(FftwVector),           pointer :: inv, outv
        class(FftwVectorOperator),   pointer :: opv
        class(*),                    pointer :: optr
        class(AbstractVector),       pointer :: vec1, vec2

        integer :: i

        call in%firstVector()
        call out%firstVector()
        call op%first()

        do i=1,in%numVectors()
            inv => in%nextVector()
            outv => out%nextVector()

            optr => op%currentValue()
            select type(optr)
                class is (FftwVectorOperator)
                    opv => optr
                    call op%next()
            end select

            vec1 => inv
            vec2 => outv

            call opv%applyOperator(vec1, vec2)
        end do
    end subroutine

    subroutine executeVecTranspose(this,op,in,out)
        implicit none

        class(VecFourierMultOperator)        :: this
        class(LinkedList),           pointer :: op
        class(VecFourierMultVector), pointer :: in
        class(VecFourierMultVector), pointer :: out

        class(FftwVector),           pointer :: inv, outv
        class(FftwVectorOperator),   pointer :: opv
        class(*),                    pointer :: optr
        class(AbstractVector),       pointer :: vec1, vec2

        integer :: i

        call in%firstVector()
        call out%firstVector()
        call op%first()

        do i=1,in%numVectors()
            inv => in%nextVector()
            outv => out%nextVector()

            optr => op%currentValue()
            select type(optr)
                class is (FftwVectorOperator)
                    opv => optr
                    call op%next()
            end select

            vec1 => inv
            vec2 => outv

            call opv%applyTranspose(vec1, vec2)
        end do
    end subroutine

    function getNumControl(this) result(nctrl)
        implicit none

        class(VecFourierMultOperator) :: this

        integer :: nctrl

        nctrl = this%fftVec1%getSize()
    end function

    subroutine doApplyOperator(this, in, out, first, second, c)
        implicit none

        class(VecFourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        logical,            intent(in) :: first
        logical,            intent(in) :: second
        real(8), optional,  intent(in) :: c

        class(AbstractVector), pointer :: p1, p2

        real(8), pointer :: dptr(:)

        ! if a multiplier is associated, do component-wise multipliciation to end up in fftVec1 (in freq space)
        if (associated(this%fftMult)) then
            if (first) then
                ! copy the input vector into fftVec1
                call this%copyFromDomain(in,this%fftVec1)

                ! compute the fft z = Fx from fftVec1 into fftVec2
                call this%executeVecOperator(this%vecOps,this%fftVec1,this%fftVec2)
            else
                ! else set z = fftVec2 from the input vector
                call this%copyFromFreqSpace(in,this%fftVec2)
            end if

            p1 => this%fftVec2
            p2 => this%fftVec1

            ! compute fftVec1 = m \circ z (in freq space)
            call this%fftMult%elementwiseMultiply(p1,p2)
        else ! end up in fftVec1/freq space without doing the elementwise multiply
            if (first) then
                ! copy the input vector into fftVec2
                call this%copyFromDomain(in,this%fftVec2)

                ! compute the fft z = Fx from fftVec2 into fftVec1
                call this%executeVecOperator(this%vecOps,this%fftVec2,this%fftVec1)
            else
                ! else just set z = fftVec1 from the input vector
                call this%copyFromFreqSpace(in,this%fftVec1)
            end if
        end if

        ! if running the second (inverse) transform, we compute x = F^{-1} z
        if (second) then
            ! compute the ifft x = F^{-1} z from fftVec1 into fftVec2
            call this%executeVecTranspose(this%vecOps,this%fftVec1,this%fftVec2)

            ! copy fftVec2 to the domain
            call this%copyToDomain(this%fftVec2,out,c)
        else
            ! otherwise copy the output from fftVec1 in freq space
            call this%copyToFreqSpace(this%fftVec1,out,c)
        end if
    end subroutine

    subroutine applyOperator(this, in, out)
        implicit none

        class(VecFourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        call this%doApplyOperator(in,out,this%runFwd,this%runInv)
    end subroutine

    ! if runInv = runFwd, the transpose of the fft mult operator is identical
    ! to the forward (i.e. the operator is self-adjoint). otherwise, the order
    ! of the transforms gets switched
    subroutine applyTranspose(this, in, out)
        implicit none

        class(VecFourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        call this%doApplyOperator(in,out,this%runInv,this%runFwd)
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(VecFourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        call this%doApplyOperator(in,out,this%runFwd,this%runInv, c)
    end subroutine

    ! if runInv = runFwd, the transpose of the fft mult operator is identical
    ! to the forward (i.e. the operator is self-adjoint). otherwise, the order
    ! of the transforms gets switched
    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(VecFourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        call this%doApplyOperator(in,out,this%runInv,this%runFwd, c)
    end subroutine
end module
