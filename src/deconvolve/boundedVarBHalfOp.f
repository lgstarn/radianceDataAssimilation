module boundedVarBHalfOp_mod

    use iso_fortran_env

    use abstractVector_mod
    use abstractVectorOperator_mod

    use simple1DVector_mod

    use vecFftDerivativeOperator_mod

    implicit none

    private

    type, extends(AbstractVectorOperator), public :: BoundedVarBHalfOp
        private
            class(VecFftDerivativeOperator), pointer :: BHalf_bv => null()
            integer                                  :: nctrl = 0

        contains
            procedure :: getNumControl

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd

            procedure :: boundedVarBHalfOpConstructor

            final :: boundedVarBHalfOpDestructor
    end type

    contains

    subroutine boundedVarBHalfOpConstructor(this,nx,ny,nz,nr,veofs)
        implicit none

        class(BoundedVarBHalfOp)          :: this

        integer,              intent(in)  :: nx
        integer,              intent(in)  :: ny
        integer,              intent(in)  :: nz
        integer,              intent(in)  :: nr
        real(real64),         optional    :: veofs(nz,nr)

        real(real64), dimension(1)   :: avals
        real(real64), dimension(2,1) :: svals

        avals = 1.d0

        allocate(this%BHalf_bv)
        call this%BHalf_bv%vecFftDerivativeOperatorConstructor(nx,ny,nz,nr,veofs=veofs)
        svals = 0.d0
        call this%BHalf_bv%addDerivatives(avals,svals,1)
        call this%BHalf_bv%normalize()
        call this%BHalf_bv%exponentiate(0.5d0)

        this%nctrl = this%BHalf_bv%getNumControl()
    end subroutine

    subroutine boundedVarBHalfOpDestructor(this)
        implicit none

        type(BoundedVarBHalfOp)  :: this

        deallocate(this%BHalf_bv)
    end subroutine

    function getNumControl(this) result(nctrl)
        implicit none

        class(BoundedVarBHalfOp) :: this

        integer :: nctrl

        nctrl = this%BHalf_bv%getNumControl()
    end function

    subroutine applyOperator(this, in, out)
        implicit none

        class(BoundedVarBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        class(Simple1DVector), pointer :: u

        real(real64), dimension(:), pointer :: dptr1, dptr2

        class(AbstractVector), pointer :: vec1, vec2

        dptr1 => in%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl)

        allocate(u)

        call u%simple1DVectorConstructor_data(dptr2)

        vec1 => u
        vec2 => out
        call this%BHalf_bv%applyOperator(vec1,vec2)

        deallocate(u)
    end subroutine

    subroutine applyTranspose(this, in, out)
        implicit none

        class(BoundedVarBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        class(Simple1DVector), pointer :: u

        real(real64), dimension(:), pointer :: dptr1, dptr2

        class(AbstractVector), pointer :: vec1, vec2

        dptr1 => out%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl)

        allocate(u)

        call u%simple1DVectorConstructor_data(dptr2)

        vec1 => in
        vec2 => u
        call this%BHalf_bv%applyTranspose(vec1,vec2)

        deallocate(u)
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(BoundedVarBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(real64),            intent(in) :: c

        class(Simple1DVector), pointer :: u
        class(Simple1DVector), pointer :: v

        real(real64), dimension(:), pointer :: dptr1, dptr2

        class(AbstractVector), pointer :: vec1, vec2

        dptr1 => in%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl)

        allocate(u)

        call u%simple1DVectorConstructor_data(dptr2)

        vec1 => u
        vec2 => out
        call this%BHalf_bv%applyOperatorAndAdd(vec1,vec2,c)

        deallocate(u)
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(BoundedVarBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(real64),            intent(in) :: c

        class(Simple1DVector), pointer :: u

        real(real64), dimension(:), pointer :: dptr1, dptr2

        class(AbstractVector), pointer :: vec1, vec2

        dptr1 => out%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl)

        allocate(u)
        call u%simple1DVectorConstructor_data(dptr2)

        vec1 => in
        vec2 => u
        call this%BHalf_bv%applyTransposeAndAdd(vec1, vec2, c)

        deallocate(u)
    end subroutine
end module
