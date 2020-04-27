module fftwVectorOperator_mod
    use fftwVector_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use, intrinsic :: iso_c_binding
    use mpiUtils_mod
    implicit none

    private 

    include 'fftw3.f03'

    public :: FftwVectorOperator

    type, extends(AbstractVectorOperator) :: FftwVectorOperator
        private
            type(C_PTR) :: planfwd, plantrans
            real(8), dimension(:), pointer :: fwdtmp => NULL()
            real(8), dimension(:), pointer :: invtmp => NULL()

        contains
            procedure :: fftwVectorOperatorConstructor

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd

            final :: fftwVectorOperatorDestructor ! clean up all allocated variables
    end type

    contains

    subroutine fftwVectorOperatorConstructor(this,inv,outv)
        implicit none

        class(FftwVectorOperator) :: this
        class(FftwVector), pointer :: inv, outv

        integer :: inType, outType

        complex(C_DOUBLE_COMPLEX), dimension(:),     pointer :: data1d_in
        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: data2d_in
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d_in

        complex(C_DOUBLE_COMPLEX), dimension(:),     pointer :: data1d_out
        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: data2d_out
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d_out

        ! to rewrite this for MPI: http://www.fftw.org/fftw2_doc/fftw_4.html#SEC55
        !   - the transforms will need to be in place
        !   - need to work with the data in transformed order (FFTW_TRANSPOSED_ORDER) for performance?
        !   -

        if (inv%getFftDim() .ne. outv%getFftDim()) then
            write(msgstr, '(A,I6,A,I6)') 'Error: FFT in/out dimension mismatch: ',inv%getFftDim(),' / ',outv%getFftDim()
            call error(msgstr)
        end if

        if (inv%getL() .ne. outv%getL() .or. inv%getM() .ne. outv%getM() .or. inv%getN() .ne. outv%getN()) then
            write(msgstr, '(A,I6,A,I6,A,I6,A,I6,A,I6,A,I6,A)') 'Error: Incompatible dimensions: (',&
                & inv%getL(),', ',inv%getM(),', ',inv%getN(),') vs (',outv%getL(),', ',outv%getM(),', ',outv%getN(),')'
            call error(msgstr)
        end if

        select case(inv%getFftDim())
            case (1)
                data1d_in  => inv%get1DPointer()
                data1d_out => outv%get1DPointer()
                ! reverse the order in Fortran, see FFTW documentation
                this%planfwd   = fftw_plan_dft_1d(inv%getN(), data1d_in, data1d_out, FFTW_FORWARD, FFTW_ESTIMATE)
                this%plantrans = fftw_plan_dft_1d(inv%getN(), data1d_in, data1d_out, FFTW_BACKWARD, FFTW_ESTIMATE)
            case (2)
                data2d_in  => inv%get2DPointer()
                data2d_out => outv%get2DPointer()
                ! reverse the order in Fortran, see FFTW documentation
                this%planfwd   = fftw_plan_dft_2d(inv%getN(), inv%getM(), data2d_in, data2d_out, FFTW_FORWARD, FFTW_ESTIMATE)
                this%plantrans = fftw_plan_dft_2d(inv%getN(), inv%getM(), data2d_in, data2d_out, FFTW_BACKWARD, FFTW_ESTIMATE)
            case (3)
                data3d_in  => inv%get3DPointer()
                data3d_out => outv%get3DPointer()
                ! reverse the order in Fortran, see FFTW documentation
                this%planfwd   = fftw_plan_dft_3d(inv%getL(), inv%getN(), inv%getM(), data3d_in, data3d_out, &
                    &FFTW_FORWARD, FFTW_ESTIMATE)
                this%plantrans = fftw_plan_dft_3d(inv%getL(), inv%getN(), inv%getM(), data3d_in, data3d_out, &
                    &FFTW_BACKWARD, FFTW_ESTIMATE)
        end select
    end subroutine

    subroutine fftwVectorOperatorDestructor(this)
        implicit none

        type(FftwVectorOperator)  :: this

        call fftw_destroy_plan(this%planfwd)
        call fftw_destroy_plan(this%plantrans)

        if (associated(this%fwdtmp)) then
            deallocate(this%fwdtmp)
        end if

        if (associated(this%invtmp)) then
            deallocate(this%invtmp)
        end if
    end subroutine

    subroutine applyOperator(this, in, out)
        implicit none

        class(FftwVectorOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        call doApply(this%planfwd, in, out)
    end subroutine

    subroutine applyTranspose(this, in, out)
        implicit none

        class(FftwVectorOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        call doApply(this%plantrans, in, out)
    end subroutine

    subroutine doApply(plan, in_abs, out_abs)
        implicit none

        type(C_PTR) :: plan
        class(AbstractVector), pointer :: in_abs
        class(AbstractVector), pointer :: out_abs

        class(FftwVector), pointer :: inv, outv

        complex(C_DOUBLE_COMPLEX), dimension(:),     pointer :: data1d_in
        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: data2d_in
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d_in

        complex(C_DOUBLE_COMPLEX), dimension(:),     pointer :: data1d_out
        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: data2d_out
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d_out

        select type(in_abs)
            class is (FftwVector)
                inv => in_abs
            class default
                write(msgstr,*) 'Incompatible input vector type passed into Fftw operator'
                call error(msgstr)
        end select

        select type(out_abs)
            class is (FftwVector)
                outv => out_abs
            class default
                write(msgstr,*) 'Incompatible output vector type passed into Fftw operator'
                call error(msgstr)
        end select

        ! Assume either inv and outv data are the same as what was planned for in the constructor
        ! or that all of the rules according to the FFTW manual are followed
        select case(inv%getFftDim())
            case (1)
                data1d_in  => inv%get1DPointer() 
                data1d_out => outv%get1DPointer() 
                call fftw_execute_dft(plan, data1d_in, data1d_out)
            case (2)
                data2d_in  => inv%get2DPointer() 
                data2d_out => outv%get2DPointer() 
                call fftw_execute_dft(plan, data2d_in, data2d_out)
            case (3)
                data3d_in  => inv%get3DPointer() 
                data3d_out => outv%get3DPointer() 
                call fftw_execute_dft(plan, data3d_in, data3d_out)
        end select
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(FftwVectorOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        real(8), dimension(:), pointer :: dptr

        dptr => out%get1DArrayPtr()

        if (.not. associated(this%fwdtmp)) then
            allocate(this%fwdtmp(size(dptr,1)))
        else if (size(this%fwdtmp,1) /= size(dptr,1)) then
            deallocate(this%fwdtmp)
            allocate(this%fwdtmp(size(dptr,1)))
        end if

        this%fwdtmp(:) = dptr(:)

        call doApply(this%planfwd, in, out)

        dptr(:) = this%fwdtmp(:) + c*dptr(:)
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(FftwVectorOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        real(8), dimension(:), pointer :: dptr

        dptr => out%get1DArrayPtr()

        if (.not. associated(this%invtmp)) then
            allocate(this%invtmp(size(dptr,1)))
        else if (size(this%invtmp,1) /= size(dptr,1)) then
            deallocate(this%invtmp)
            allocate(this%invtmp(size(dptr,1)))
        end if

        this%invtmp(:) = dptr(:)

        call doApply(this%plantrans, in, out)

        dptr(:) = this%invtmp(:) + c*dptr(:)
    end subroutine
end module
