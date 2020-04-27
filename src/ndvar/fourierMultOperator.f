module fourierMultOperator_mod
    use, intrinsic :: iso_c_binding
    use abstractVector_mod
    use abstractVectorOperator_mod
    use vertEofHorzFftVector_mod
    use linkedList_mod
    use fftwVector_mod
    use fftwVectorOperator_mod
    use localization_mod

    implicit none

    private

    public :: FourierMultOperator

    type, extends(AbstractVectorOperator) :: FourierMultOperator
        !private
            class(FftwVector), pointer :: fftVec
            class(FftwVector), pointer :: fftMult

        contains
            procedure :: fourierMultOperatorConstructor
            final     :: fourierMultOperatorDestructor

            procedure :: applyOperator => applyOperator_fmo
            procedure :: applyTranspose => applyTranspose_fmo
            procedure :: getNumControl

            procedure, private :: createVecOperator
            procedure, private :: destroyVecOperator
            procedure, private :: executeVecOperator
            procedure, private :: executeVecTranspose
    end type

    contains

    subroutine fourierMultOperatorConstructor(this,fftVec,fftMult)
        implicit none

        class(FourierMultOperator) :: this
        class(FftwVector), pointer :: fftVec
        class(FftwVector), pointer :: fftMult

        this%fftVec  => fftVec
        this%fftMult => fftMult
    end subroutine

    subroutine fourierMultOperatorDestructor(this)
        implicit none

        type(FourierMultOperator)  :: this

        deallocate(this%fftVec)
        deallocate(this%fftMult)
    end subroutine

    subroutine applyOperator_fmo(this, in, out)
        implicit none

        class(FourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1, dptr2
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1, cptr2, cptr3

        class(AbstractVector), pointer :: p1, p2

        integer :: i, j, k, psize, fsize, ind1

        ! get the 3D representation of the padded FFT vector of Z
        cptr1 => this%Zv%get3DArrayPtr()

        ! get the 1D representation of the input pcdata
        optr => in%get1DArrayPtr()

        ind1 = 1

        ! copy all of the data from the input vector into Z
        call this%Zv%set1DArray(optr)

        p1 => this%C1pv
        p2 => this%Cpxv

        ! now element-wise multiply Zv by the fft of C1
        call this%Zv%elementwiseMultiply(p1,p2)

        ! and finally compute the ifft to get C^{1/2} z
        call this%executeVecTranspose(this%Cxop,this%Cpxv,this%Cxv)

        ! get the pointer to the output result in pc space
        cptr1 => this%Cxv%get3DArrayPtr()

        ! get the 1D representation of the input full data
        optr => out%get1DArrayPtr()

        fsize = this%ny*this%nx*this%nz

        dptr1(1:this%nx,1:this%ny,1:this%nz) => optr(1:fsize)

        ! now apply the vertical EOFs to the PCs to get the full output
        do j=1,this%ny
            do i=1,this%nx
                dptr1(i,j,1:this%nz) = matmul(this%veofs,real(cptr1(i,j,1:this%nr)))
            end do
        end do
    end subroutine

    subroutine applyTranspose_fmo(this, in, out)
        implicit none

        class(FourierMultOperator) :: this

        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1, dptr2
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1, cptr2, cptr3

        class(AbstractVector), pointer :: p1, p2

        integer :: i, j, k, ind1, psize, fsize

        ! get the 3D representation of the padded Cpxv vector to move the input in to
        cptr1 => this%Cpxv%get3DArrayPtr()

        ! get the 1D representation of the input full data
        optr => in%get1DArrayPtr()

        ! the size of the input full data 1d array
        fsize = this%nx*this%ny*this%nz

        ! convert the full input data 1d array into a 3d array with Fortran pointer slices
        dptr2(1:this%nx,1:this%ny,1:this%nz) => optr(1:fsize)

        cptr1 = 0.

        ! now apply the transpose of the vertical EOFs to full input to get the PCs
        do i=1,this%nx
            do j=1,this%ny
                cptr1(i,j,1:this%nr) = matmul(transpose(this%veofs),dptr2(i,j,1:this%nz))
            end do
        end do

        ! now execute the fft of Cpxv into Cxv
        call this%executeVecOperator(this%Cxop,this%Cpxv,this%Cxv)

        ! now element-wise multiply of Cxv by the fft of C1
        p1 => this%C1pv
        p2 => this%Zv

        call this%Cxv%elementwiseMultiply(p1,p2)

        ! Now place Zv into the output array.

        ! get the 1D representation of the input pcdata
        optr => out%get1DArrayPtr()

        ! the size of the pcdata 1d array
        ! get the padded FFT data from the Zv output
        cptr2 => this%Zv%get3DArrayPtr()

        call out%set1DArray(this%Zv%get1DArrayPtr())
    end subroutine
end module
