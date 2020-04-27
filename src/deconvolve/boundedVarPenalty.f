module boundedVarPenalty_mod

    use penalty_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use simple1DVector_mod
    use vecFftDerivativeOperator_mod
    use vecFftDerivativeOperator_mod

    implicit none

    private

    type, extends(Penalty), public :: BoundedVarPenalty
        private
            class(VecFftDerivativeOperator), pointer :: xDerivative => null()
            class(VecFftDerivativeOperator), pointer :: yDerivative => null()

            class(Simple1DVector),           pointer :: utmp1      => null()
            class(Simple1DVector),           pointer :: utmp2      => null()
            class(Simple1DVector),           pointer :: utmp3      => null()

            integer                                  :: offset = 0
            integer                                  :: nctrl  = 0
            real(8)                                  :: pnorm
            integer                                  :: deriv
            logical                                  :: penalizeState

        contains
            procedure :: boundedVarPenaltyConstructor

            procedure :: applyPenalty

            final :: boundedVarPenaltyDestructor ! clean up all allocated variables
    end type

    contains

    subroutine boundedVarPenaltyConstructor(this,nx,ny,nz,nr,nctrl,offset,pnorm,deriv,penalizeState)
        implicit none

        class(BoundedVarPenalty) :: this
        integer,             intent(in) :: nx
        integer,             intent(in) :: ny
        integer,             intent(in) :: nz
        integer,             intent(in) :: nr
        integer,             intent(in) :: nctrl
        integer,   optional, intent(in) :: offset
        real(8),   optional, intent(in) :: pnorm
        integer,   optional, intent(in) :: deriv
        logical,   optional, intent(in) :: penalizeState

        real(8), dimension(1)   :: avals
        real(8), dimension(2,1) :: svals

        this%nctrl = nctrl
        if (present(offset)) then
            this%offset = offset
        else
            this%offset = 0
        end if

        if (present(pnorm)) then
            this%pnorm = pnorm
        else
            this%pnorm = 2.d0
        end if

        if (present(deriv)) then
            this%deriv = deriv
        else
            this%deriv = 1
        end if

        if (present(penalizeState)) then
            this%penalizeState = penalizeState
        else
            this%penalizeState = .false.
        end if

        allocate(this%utmp1)
        allocate(this%utmp2)

        call this%utmp1%simple1DVectorConstructor(nx*ny*nr)
        call this%utmp2%simple1DVectorConstructor(nx*ny*nr)

        if (this%penalizeState) then
            allocate(this%utmp3)
            call this%utmp3%simple1DVectorConstructor(nx*ny*nz)
        end if

        avals = 1.d0

        svals = 0.d0
        svals(1,1) = dble(this%deriv)

        allocate(this%xDerivative)
        if (this%penalizeState) then
            call this%xDerivative%vecFftDerivativeOperatorConstructor(nx,ny,nz,nr,doForward=.true.)
        else
            call this%xDerivative%vecFftDerivativeOperatorConstructor(nx,ny,0,nr)
        end if
        call this%xDerivative%addDerivatives(avals,svals,1)
        call this%xDerivative%normalize()

        svals = 0.d0
        svals(2,1) = dble(this%deriv)

        allocate(this%yDerivative)
        if (this%penalizeState) then
            call this%yDerivative%vecFftDerivativeOperatorConstructor(nx,ny,nz,nr,doForward=.true.)
        else
            call this%yDerivative%vecFftDerivativeOperatorConstructor(nx,ny,0,nr)
        end if
        call this%yDerivative%addDerivatives(avals,svals,1)
        call this%yDerivative%normalize()
    end subroutine

    subroutine boundedVarPenaltyDestructor(this)
        implicit none

        type(BoundedVarPenalty)  :: this

        if (associated(this%xDerivative)) deallocate(this%xDerivative)
        if (associated(this%yDerivative)) deallocate(this%yDerivative)
        if (associated(this%utmp1)) deallocate(this%utmp1)
        if (associated(this%utmp2)) deallocate(this%utmp2)
        if (associated(this%utmp3)) deallocate(this%utmp3)
    end subroutine

    subroutine applyPenalty(this, control, state, bhalf, penval, pengrad)
        class(BoundedVarPenalty)               :: this
        class(AbstractVector),         pointer :: control
        class(AbstractVector),         pointer :: state
        class(AbstractVectorOperator), pointer :: bhalf
        real(8),                   intent(out) :: penval
        real(8),         target, intent(inout) :: pengrad(:)

        real(8), dimension(:), pointer :: dptr, dptr1, dptr2

        class(Simple1DVector), pointer :: u, gradvec
        class(AbstractVector), pointer :: vec1, vec2

        real(8) :: pval1, pval2

        integer :: i, v1, v2

        v1 = this%offset+1
        v2 = this%offset+this%nctrl

        if (this%penalizeState) then
            dptr1  => state%get1DArrayPtr()
        else
            dptr  => control%get1DArrayPtr()
            dptr1 => dptr(v1:v2)
        end if

        allocate(u)
        call u%simple1DVectorConstructor_data(dptr1)
        vec1 => u
        vec2 => this%utmp1
        call this%xDerivative%applyOperator(vec1,vec2)

        vec1 => u
        vec2 => this%utmp2
        call this%yDerivative%applyOperator(vec1,vec2)
        deallocate(u)

        dptr1 => this%utmp1%get1DArrayPtr()
        dptr2 => this%utmp2%get1DArrayPtr()

        penval  = 0.d0

        ! g_p = ||f(Z)||_p
        ! \nabla g_p = \frac{f(Z) \circ abs(f(Z))^{p-2}}{g_p^{p-1}}

        if (abs(this%pnorm - 1.0d0) < 1d-16) then
            do i=1,size(dptr1)
                penval = penval + abs(dptr1(i)) + abs(dptr2(i))
                dptr1(i) = (-1.d0)**this%deriv*sign(1.0d0,dptr1(i))
                dptr2(i) = (-1.d0)**this%deriv*sign(1.0d0,dptr2(i))
            end do
        else
            do i=1,size(dptr1)
                penval = penval + abs(dptr1(i))**this%pnorm + abs(dptr2(i))**this%pnorm
                dptr1(i) = dptr1(i)*abs(dptr1(i))**(this%pnorm-2.d0)
                dptr2(i) = dptr2(i)*abs(dptr2(i))**(this%pnorm-2.d0)
            end do

            penval = penval**(1.d0/this%pnorm)

            if (penval .lt. 2.3d-12) then
                penval = 0.d0
                dptr1  = 0.d0
                dptr2  = 0.d0
            else
                dptr1  = (-1.d0)**this%deriv*dptr1*(penval**(1.d0-this%pnorm))
                dptr2  = (-1.d0)**this%deriv*dptr2*(penval**(1.d0-this%pnorm))
            end if
        end if

        dptr1(1:this%nctrl) => pengrad(v1:v2)

        allocate(gradvec)
        call gradvec%simple1DVectorConstructor_data(dptr1)

        if (this%penalizeState) then
            vec1 => this%utmp1
            vec2 => this%utmp3
            call this%xDerivative%applyTranspose(vec1,vec2)

            vec1 => this%utmp2
            vec2 => this%utmp3
            call this%yDerivative%applyTransposeAndAdd(vec1,vec2,1.0d0)

            dptr1(1:this%nctrl) => pengrad(v1:v2)

            vec1 => this%utmp3
            vec2 => gradvec
            call bHalf%applyTranspose(vec1,vec2)
        else
            vec1 => this%utmp1
            vec2 => gradvec
            call this%xDerivative%applyTranspose(vec1,vec2)

            vec1 => this%utmp2
            vec2 => gradvec
            call this%yDerivative%applyTransposeAndAdd(vec1,vec2,1.0d0)
        end if

        deallocate(gradvec)
    end subroutine
end module
