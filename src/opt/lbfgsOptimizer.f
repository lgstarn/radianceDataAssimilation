module LbfgsOptimizer_mod
    use Optimizer_mod

    implicit none

    private

    integer, parameter :: mp = 6, lp = 6
    real(8)            :: gtol

    type, extends(Optimizer), public :: LbfgsOptimizer
        private
            integer :: ntrav, msave, nxm, nwork, maxiter

            real(8) :: eps = 0.5d0**(52)

            real(8) :: oldcost

            real(8), allocatable, dimension(:) :: diag, ss, yy, ww
            real(8) :: dzs(10), apd

            integer :: iprint(2), point
            logical :: diagco

        contains

            procedure :: optimize => lbfgsOptimize
            procedure :: lbfgsOptimizerConstructor
            final :: lbfgsOptimizerDestructor ! clean up all allocated variables
    end type

!    interface LbfgsOptimizer
!        procedure lbfgsOptimizerConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine lbfgsOptimizerConstructor(this,nctr,maxiter)
        implicit none

        class(LbfgsOptimizer)  :: this
        integer, intent(in) :: nctr, maxiter

        integer :: ierr

        this%maxiter = maxiter
        this%ntrav = 25*nctr
        this%msave = 9
        this%nxm = this%msave*nctr
        this%nwork = nctr+2*this%msave

        allocate(this%diag(nctr))
        allocate(this%ss(this%nxm))
        allocate(this%yy(this%nxm))
        allocate(this%ww(this%nwork))

        this%diagco=.false.
        this%iprint(1)=1
        this%iprint(2)=0
        this%eps=1.0d-8
        this%point=0.

        ! use the square-root of double machine precision, 2^-52
        gtol = sqrt(2.220446049250313d-16)

        call this%optimizerConstructor('LBFGS')
    end subroutine

    subroutine lbfgsOptimizerDestructor(this)
        implicit none

        type(LbfgsOptimizer)  :: this

        deallocate(this%diag)
        deallocate(this%ss)
        deallocate(this%yy)
        deallocate(this%ww)
    end subroutine

    subroutine lbfgsOptimize(this,funder_ptr,nctrl,z0,costval,grad,iflag)
        class(LbfgsOptimizer) :: this
        procedure (funder_func), pointer :: funder_ptr
        integer, intent(in) :: nctrl
        real(8), intent(inout), dimension(nctrl) :: z0
        real(8), intent(out) :: costval
        real(8), intent(out), dimension(nctrl) :: grad
        integer, intent(out) :: iflag

        integer :: icall

        iflag = 0

        do icall=0,this%maxiter
            call funder_ptr(nctrl,z0,costval,grad,iflag)
            call va15ad(nctrl,this%msave,z0,costval,grad,this%diagco,this%diag, &
                  this%iprint,this%eps,this%ss,this%yy,this%point,this%ww,iflag)

            if(iflag .le. 0) exit !the loop
        enddo

        call funder_ptr(nctrl,z0,costval,grad,iflag)
    end subroutine

    INCLUDE 'va15ad.incl'
end module
