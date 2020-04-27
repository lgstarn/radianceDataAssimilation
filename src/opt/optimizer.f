module Optimizer_mod

    implicit none

    private

    public :: funder_func

    type, abstract, public :: Optimizer
        private
            character (len=256)              :: optimizerName

        contains
            procedure(optimizeAbstract), deferred :: optimize
            !procedure(getFinalCost_abs), deferred :: getFinalCost
            !procedure(getFinalGrad_abs), deferred :: getFinalGrad
            !procedure(getFinalFlag_abs), deferred :: getFinalFlag
            procedure :: optimizerConstructor
            !procedure :: optimizerDestructor
            !final :: optimizerDestructor ! clean up all allocated variables
    end type

    abstract interface
        subroutine funder_func(nctrl,z,costval,grad,iflag)
            integer, intent(in) :: nctrl
            double precision, intent(in), dimension(nctrl) :: z
            double precision, intent(out) :: costval
            double precision, intent(out), dimension(nctrl) :: grad
            integer, intent(out) :: iflag
        end subroutine
    end interface

    abstract interface
        subroutine optimizeAbstract(this,funder_ptr,nctrl,z0,costval,grad,iflag)
            import Optimizer
            import funder_func
            class(Optimizer) :: this
            procedure (funder_func), pointer :: funder_ptr
            integer, intent(in) :: nctrl
            double precision, intent(inout), dimension(nctrl) :: z0
            double precision, intent(out) :: costval
            double precision, intent(out), dimension(nctrl) :: grad
            integer, intent(out) :: iflag
        end subroutine
    end interface

    contains

    subroutine optimizerConstructor(this,optimizerName)
        implicit none

        class(Optimizer)             :: this
        character(len=*), intent(in) :: optimizerName

        !allocate(this)
        this%optimizerName = optimizerName
    end subroutine

!    subroutine optimizerDestructor(this)
!        implicit none
!
!        class(Optimizer) :: this
!    end subroutine
end module
