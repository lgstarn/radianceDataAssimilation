module ctnPlusTexBHalfOp_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use vecFftDerivativeOperator_mod
    use simple1DVector_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(AbstractVectorOperator), public :: CtnPlusTexBHalfOp
        !private
            class(VecFftDerivativeOperator), pointer :: BHalf_u => null()
            class(VecFftDerivativeOperator), pointer :: BHalf_v => null()
            integer :: nctrl_u
            integer :: nctrl_v
            real(8) :: v_factor

        contains
            procedure :: getNumControlU
            procedure :: getNumControlV
            procedure :: getNumControl

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd

            procedure :: ctnPlusTexBHalfOpConstructor

            final :: ctnPlusTexBHalfOpDestructor
    end type

    contains

    subroutine ctnPlusTexBHalfOpConstructor(this,nx,ny,nz,nr,veofs)
        implicit none

        class(CtnPlusTexBHalfOp)         :: this
        integer,              intent(in) :: nx
        integer,              intent(in) :: ny
        integer,              intent(in) :: nz
        integer,              intent(in) :: nr
        real(8),       dimension(nz,nr)  :: veofs

        real(8), dimension(2) :: avals
        real(8), dimension(2,2) :: svals

        this%v_factor = 5d2

        avals = 1.d0

        allocate(this%BHalf_u)
        call this%BHalf_u%vecFftDerivativeOperatorConstructor(nx,ny,nz,nr,veofs=veofs)
        svals = 0.d0
        call this%BHalf_u%addDerivatives(avals,svals,1)
        call this%BHalf_u%normalize()
        call this%BHalf_u%exponentiate(0.5d0)

        this%nctrl_u = this%BHalf_u%getNumControl()

        allocate(this%BHalf_v)
        call this%BHalf_v%vecFftDerivativeOperatorConstructor(nx,ny,nz,nr,veofs=veofs)
        svals = 0.d0
        svals(1,1) = 2.0d0
        svals(2,2) = 2.0d0
        call this%BHalf_v%addDerivatives(avals,svals,2)
        !call this%BHalf_v%absoluteValue()
        call this%BHalf_v%normalize()
        !call this%BHalf_v%exponentiate(0.5d0)

        this%nctrl_v = this%BHalf_v%getNumControl()
    end subroutine

    subroutine ctnPlusTexBHalfOpDestructor(this)
        implicit none

        type(CtnPlusTexBHalfOp)  :: this

        deallocate(this%BHalf_u)
        deallocate(this%BHalf_v)
    end subroutine

    function getNumControlU(this) result(nctrl)
        implicit none

        class(CtnPlusTexBHalfOp) :: this

        integer :: nctrl

        nctrl = this%BHalf_u%getNumControl()
    end function

    function getNumControlV(this) result(nctrl)
        implicit none

        class(CtnPlusTexBHalfOp) :: this

        integer :: nctrl

        nctrl = this%BHalf_v%getNumControl()
    end function

    function getNumControl(this) result(nctrl)
        implicit none

        class(CtnPlusTexBHalfOp) :: this

        integer :: nctrl

        nctrl = this%getNumControlU() + this%getNumControlV()
    end function

    subroutine applyOperator(this, in, out)
        implicit none

        class(CtnPlusTexBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        class(Simple1DVector), pointer :: u
        class(Simple1DVector), pointer :: v

        real(8), dimension(:), pointer :: dptr1, dptr2, dptr3

        class(AbstractVector), pointer :: vec1, vec2

        if (in%getSize() /= this%nctrl_u + this%nctrl_v) then
            write(msgstr,*) 'CtnPlusTexBHalfOp 1: input size',in%getSize(),'did not match u',this%nctrl_u,'+ v',this%nctrl_v
            call error(msgstr)
        end if

        dptr1 => in%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl_u)
        dptr3 => dptr1(this%nctrl_u+1:this%nctrl_u+this%nctrl_v)

        allocate(u)
        allocate(v)

        call u%simple1DVectorConstructor_data(dptr2)
        call v%simple1DVectorConstructor_data(dptr3)

        vec1 => u
        vec2 => out
        call this%BHalf_u%applyOperator(vec1,vec2)

        dptr1 => vec2%get1DArrayPtr()
        write(msgstr,*) 'u norm:',sqrt(dot_product(dptr1,dptr1))
        call print(msgstr)

        vec1 => v
        vec2 => out
        call this%BHalf_v%applyOperatorAndAdd(vec1,vec2,this%v_factor)

         write(msgstr,*) 'u+v norm:',sqrt(dot_product(dptr1,dptr1))
         call print(msgstr)

        deallocate(u)
        deallocate(v)
    end subroutine

    subroutine applyTranspose(this, in, out)
        implicit none

        class(CtnPlusTexBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        class(Simple1DVector), pointer :: u
        class(Simple1DVector), pointer :: v

        real(8), dimension(:), pointer :: dptr1, dptr2, dptr3

        class(AbstractVector), pointer :: vec1, vec2

        if (out%getSize() /= this%nctrl_u + this%nctrl_v) then
            write(msgstr,*) 'CtnPlusTexBHalfOp 2: input size',in%getSize(),'did not match u',this%nctrl_u,'+ v',this%nctrl_v
            call error(msgstr)
        end if

        dptr1 => out%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl_u)
        dptr3 => dptr1(this%nctrl_u+1:this%nctrl_u+this%nctrl_v)

        allocate(u)
        allocate(v)

        call u%simple1DVectorConstructor_data(dptr2)
        call v%simple1DVectorConstructor_data(dptr3)

        vec1 => in
        vec2 => u
        call this%BHalf_u%applyTranspose(vec1,vec2)

        vec1 => in
        vec2 => v
        call this%BHalf_v%applyTranspose(vec1,vec2)
        call vec2%scalarMultiply(this%v_factor,vec2)

        deallocate(u)
        deallocate(v)
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(CtnPlusTexBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        class(Simple1DVector), pointer :: u
        class(Simple1DVector), pointer :: v

        real(8), dimension(:), pointer :: dptr1, dptr2, dptr3

        class(AbstractVector), pointer :: vec1, vec2

        if (in%getSize() /= this%nctrl_u + this%nctrl_v) then
            write(msgstr,*) 'CtnPlusTexBHalfOp 3: input size',in%getSize(),'did not match u',this%nctrl_u,'+ v',this%nctrl_v
            call error(msgstr)
        end if

        dptr1 => in%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl_u)
        dptr3 => dptr1(this%nctrl_u+1:this%nctrl_u+this%nctrl_v)

        allocate(u)
        allocate(v)

        call u%simple1DVectorConstructor_data(dptr2)
        call v%simple1DVectorConstructor_data(dptr3)

        vec1 => u
        vec2 => out
        call this%BHalf_u%applyOperatorAndAdd(vec1,vec2,c)

        vec1 => v
        vec2 => out
        call this%BHalf_v%applyOperatorAndAdd(vec1,vec2,this%v_factor*c)

        deallocate(u)
        deallocate(v)
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(CtnPlusTexBHalfOp) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        class(Simple1DVector), pointer :: u
        class(Simple1DVector), pointer :: v

        real(8), dimension(:), pointer :: dptr1, dptr2, dptr3

        class(AbstractVector), pointer :: vec1, vec2

        if (out%getSize() /= this%nctrl_u + this%nctrl_v) then
            write(msgstr,*) 'CtnPlusTexBHalfOp 4: input size',in%getSize(),'did not match u',this%nctrl_u,'+ v',this%nctrl_v
            call error(msgstr)
        end if

        dptr1 => out%get1DArrayPtr()
        dptr2 => dptr1(1:this%nctrl_u)
        dptr3 => dptr1(this%nctrl_u+1:this%nctrl_u+this%nctrl_v)

        allocate(u)
        allocate(v)

        call u%simple1DVectorConstructor_data(dptr2)
        call v%simple1DVectorConstructor_data(dptr3)

        vec1 => in
        vec2 => u
        call this%BHalf_u%applyTransposeAndAdd(vec1, vec2, c)

        vec1 => in
        vec2 => v
        call this%BHalf_v%applyTransposeAndAdd(vec1, vec2, this%v_factor*c)

        deallocate(u)
        deallocate(v)
    end subroutine
end module
