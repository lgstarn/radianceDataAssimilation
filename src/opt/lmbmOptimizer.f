module LmbmOptimizer_mod
    use optimizer_mod
    use numericalUtils_mod

    implicit none

    private

    type, extends(Optimizer), public :: LmbmOptimizer
        private
            integer :: na, mcu, nw
            integer :: ipar(7),iout(3)

            real(8), allocatable, dimension(:) :: w
            real(8) :: rpar(8)
            real :: time,rtim(2)
            integer :: mc, mcinit

        contains

            procedure :: optimize => lmbmOptimize
            procedure :: lmbmOptimizerConstructor
            final :: lmbmOptimizerDestructor ! clean up all allocated variables
    end type

!    interface LmbmOptimizer
!        procedure lmbmOptimizerConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine lmbmOptimizerConstructor(this,nctr,maxiter,gtol)
        implicit none

        class(LmbmOptimizer)          :: this
        integer, intent(in)           :: nctr, maxiter
        real(8), intent(in), optional :: gtol

        integer :: ierr

        !allocate(this)

        this%na = 4
        this%mcu = 9
        this%nw = 1 + 9*nctr + 2*nctr*this%na + 3*this%na + 2*nctr*(this%mcu+1) + &
                  3*(this%mcu+2)*(this%mcu+1)/2 + 9*(this%mcu+1)

        allocate (this%w(this%nw), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for w"

        this%mc = this%mcu

        this%ipar = 0

        ! printout specification
        this%ipar(5) = 4

        ! selection of the method
        this%ipar(6) = 0

        ! selection of the scaling
        this%ipar(7) = 1

        ! maximum numbers of iterations and function evaluations
        this%ipar(2) = maxiter
        this%ipar(3) = maxiter
        this%ipar(1) = 1
        this%ipar(4) = 5

        ! choice of real(8) parameters
        this%rpar = 0.0d0

        this%rpar(1) = 1.0d-8
        this%rpar(2) = 1.0d-8

        ! desired accuracy
        this%rpar(3) = 0.0d0
        if (present(gtol)) then
            this%rpar(4) = gtol
            this%rpar(5) = gtol
        else
            this%rpar(4) = 1.0d-8
            this%rpar(5) = 1.0d-8
        end if

        ! locality measure
        this%rpar(6) = 1.0

        ! line search parameter
        this%rpar(7) = 1d-8

        ! max stepsize
        this%rpar(8) = 100.0d0

        call this%optimizerConstructor('LMBM')
    end subroutine

    subroutine lmbmOptimizerDestructor(this)
        implicit none

        type(LmbmOptimizer)  :: this
    end subroutine


    subroutine lmbmOptimize(this,funder_ptr,nctrl,z0,costval,grad,iflag)
        class(LmbmOptimizer) :: this
        procedure (funder_func), pointer :: funder_ptr
        integer, intent(in) :: nctrl
        real(8), intent(inout), dimension(nctrl) :: z0
        real(8), intent(out) :: costval
        real(8), intent(out), dimension(nctrl) :: grad
        integer, intent(out) :: iflag

        call funder_ptr(nctrl,z0,costval,grad,iflag)
        call lmbmu(nctrl,this%na,this%mcu,this%mc,this%nw,z0,costval,&
            this%rpar,this%ipar,this%iout,this%time,this%rtim,this%w,funder_ptr)
        call funder_ptr(nctrl,z0,costval,grad,iflag)
    end subroutine

include 'lmbm.incl'

include 'lmsub.incl'

include 'lmbmMatcal.incl'
end module
