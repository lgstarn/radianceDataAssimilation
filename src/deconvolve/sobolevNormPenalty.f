module sobolevNormPenalty_mod

    use penalty_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use simple1DVector_mod
    use vecFftDerivativeOperator_mod
    use vecFftDerivativeOperator_mod

    implicit none

    private

    type, extends(Penalty), public :: SobolevNormPenalty
        private
            class(VecFftDerivativeOperator), pointer :: gSobolevPenalty => null()
            class(Simple1DVector),           pointer :: gtmp1           => null()
            class(Simple1DVector),           pointer :: gtmp2           => null()
            integer                                  :: offset          = 0
            integer                                  :: nctrl           = 0
            real(8)                                  :: s               = 0
            real(8)                                  :: pnorm           = 1

        contains
            procedure :: sobolevNormPenaltyConstructor

            procedure :: applyPenalty

            final :: sobolevNormPenaltyDestructor ! clean up all allocated variables
    end type

    contains

    subroutine sobolevNormPenaltyConstructor(this,nx,ny,nr,nctrl,s,pnorm,offset)
        implicit none

        class(SobolevNormPenalty)     :: this

        integer,           intent(in) :: nx
        integer,           intent(in) :: ny
        integer,           intent(in) :: nr
        integer,           intent(in) :: nctrl
        real(8),           intent(in) :: s
        real(8),           intent(in) :: pnorm
        integer, optional, intent(in) :: offset

        real(8), dimension(3)   :: avals
        real(8), dimension(2,3) :: svals

        if (present(offset)) then
            this%offset = offset
        else
            this%offset = 0
        end if

        this%nctrl   = nctrl
        this%s       = s
        this%pnorm   = pnorm

        allocate(this%gtmp1)
        allocate(this%gtmp2)

        call this%gtmp1%simple1DVectorConstructor(nx*ny*nr)
        call this%gtmp2%simple1DVectorConstructor(nctrl)

        avals = 1.d0
        svals = 0.d0

        svals(1,1) = 0.d0
        svals(1,2) = 2.d0
        svals(2,3) = 2.d0

        allocate(this%gSobolevPenalty)
        call this%gSobolevPenalty%vecFftDerivativeOperatorConstructor(nx,ny,0,nr)
        call this%gSobolevPenalty%addDerivatives(avals,svals,1)
        !call this%gSobolevPenalty%absoluteValue()
        call this%gSobolevPenalty%exponentiate(0.5d0*s)
        call this%gSobolevPenalty%normalize()
    end subroutine

    subroutine sobolevNormPenaltyDestructor(this)
        implicit none

        type(SobolevNormPenalty)  :: this

        deallocate(this%gSobolevPenalty)
        deallocate(this%gtmp1)
        deallocate(this%gtmp2)
    end subroutine

    subroutine applyPenalty(this, control, state, bhalf, penval, pengrad)
        class(SobolevNormPenalty)        :: this
        class(AbstractVector),         pointer :: control
        class(AbstractVector),         pointer :: state
        class(AbstractVectorOperator), pointer :: bhalf
        real(8),                   intent(out) :: penval
        real(8),   dimension(:), intent(inout) :: pengrad

        real(8), dimension(:), pointer :: dptr, dptr1

        class(AbstractVector), pointer :: vec1, vec2

        class(Simple1DVector), pointer :: g

        real(8) :: powval

        integer :: i, v1, v2

        dptr => control%get1DArrayPtr()

        v1 = this%offset+1
        v2 = this%offset+this%nctrl

        dptr1 => dptr(v1:v2)

        allocate(g)
        call g%simple1DVectorConstructor_data(dptr1)
        vec1 => g
        vec2 => this%gtmp1
        call this%gSobolevPenalty%applyOperator(vec1,vec2)
        deallocate(g)

        dptr => this%gtmp1%get1DArrayPtr()

        penval  = 0.d0

        do i=1,size(dptr)
            penval  = penval + abs(dptr(i))**this%pnorm
            if (abs(dptr(i)) < 1.d-16) then
                dptr(i) = 0.
            else
                dptr(i) = abs(dptr(i))**(this%pnorm-1.d0)/dptr(i)
            end if
        end do

        if (abs(penval) > 1.d-16) then
            dptr   = (penval**(1.0d0/this%pnorm-1.d0))*dptr
            penval = penval**(1.0d0/this%pnorm)
        else
            dptr = 0.d0
            penval = 0.d0
        end if

        vec1 => this%gtmp1
        vec2 => this%gtmp2
        call this%gSobolevPenalty%applyTranspose(vec1,vec2)

        dptr => this%gtmp2%get1DArrayPtr()

        pengrad(v1:v2) = pengrad(v1:v2) + dptr(1:this%nctrl)
    end subroutine
end module
