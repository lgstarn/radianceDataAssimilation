module bHalf3dFftOperator_mod
    use, intrinsic :: iso_c_binding
    use abstractVector_mod
    use abstractVectorOperator_mod
    use vecFourierMultOperator_mod
    use vecFourierMultVector_mod
    use localization_mod

    implicit none

    private

    public :: BHalf3dFftOperator

    type, extends(VecFourierMultOperator) :: BHalf3dFftOperator
        !private
            integer                          :: nx = 0
            integer                          :: ny = 0
            integer                          :: nz = 0
            integer                          :: nr = 0

            real(8), dimension(:,:), pointer :: transform => NULL()

        contains
            procedure :: bHalf3dFftOperatorConstructor
            final     :: bHalf3dFftOperatorDestructor

            procedure :: copyFromDomain
            procedure :: copyToDomain
            procedure :: copyFromFreqSpace
            procedure :: copyToFreqSpace
    end type

    contains

    subroutine bHalf3dFftOperatorConstructor(this,nx,ny,nz,nr,vec1,vec2,mult,transform,doForward)
        implicit none

        class(BHalf3dFftOperator)            :: this
        integer, intent(in)                  :: nx, ny, nz, nr
        class(VecFourierMultVector), pointer :: vec1
        class(VecFourierMultVector), pointer :: vec2
        class(VecFourierMultVector), pointer :: mult
        real(8),  dimension(nz,nr), optional :: transform
        logical, intent(in),        optional :: doForward

        logical :: fwd

        this%nx = nx
        this%ny = ny
        this%nz = nz
        this%nr = nr

        if (present(transform)) then
            allocate(this%transform(nz,nr))
            this%transform = transform
        end if

        if (present(doForward)) then
            fwd = doForward
        else
            fwd = .false.
        end if

        call this%vecFourierMultOperatorConstructor(vec1,vec2,mult,.true.,fwd)
    end subroutine

    subroutine bHalf3dFftOperatorDestructor(this)
        implicit none

        type(BHalf3dFftOperator)  :: this

        if (associated(this%transform)) then
            deallocate(this%transform)
        end if

        call this%vecFourierMultOperatorDestructor()
    end subroutine

    subroutine copyFromFreqSpace(this, in, out)
        implicit none

        class(BHalf3dFftOperator)             :: this
        class(AbstractVector),        pointer :: in
        class(VecFourierMultVector),  pointer :: out

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1

        integer :: i, j, k, ind1, fsize

        ! complex pointer to the output array
        cptr1 => out%get3DArrayPtr()

        ! get the 1D representation of the input full data
        optr => in%get1DArrayPtr()

        ! index into optr
        ind1 = 1

        ! copy all of the data from the input vector into cptr1
        do k=1,size(cptr1,3)
            do j=1,size(cptr1,2)
                do i=1,size(cptr1,1)
                    cptr1(i,j,k) = cmplx(optr(ind1),optr(ind1+1),kind=kind(1.0d0))
                    ind1 = ind1 + 2
                end do
            end do
        end do
    end subroutine

    subroutine copyToFreqSpace(this, in, out, c)
        implicit none

        class(BHalf3dFftOperator)         :: this
        class(VecFourierMultVector),  pointer :: in
        class(AbstractVector),        pointer :: out
        real(8), optional,         intent(in) :: c

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1

        integer :: i, j, k, ind1

        ! complex pointer to input data
        cptr1 => in%get3DArrayPtr()

        ! 1D representation of the full output data
        optr => out%get1DArrayPtr()

        ! index into optr
        ind1 = 1

        if (present(c)) then
            do k=1,size(cptr1,3)
                do j=1,size(cptr1,2)
                    do i=1,size(cptr1,1)
                        optr(ind1)   = optr(ind1)   +  c*dble(cptr1(i,j,k))
                        optr(ind1+1) = optr(ind1+1) + c*aimag(cptr1(i,j,k))
                        ind1 = ind1 + 2
                    end do
                end do
            end do
        else
            do k=1,size(cptr1,3)
                do j=1,size(cptr1,2)
                    do i=1,size(cptr1,1)
                        optr(ind1)   =  dble(cptr1(i,j,k))
                        optr(ind1+1) = aimag(cptr1(i,j,k))
                        ind1 = ind1 + 2
                    end do
                end do
            end do
        end if
    end subroutine

    subroutine copyFromDomain(this, in, out)
        implicit none

        class(BHalf3dFftOperator)       :: this
        class(AbstractVector),        pointer :: in
        class(VecFourierMultVector),  pointer :: out

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1

        integer :: i, j, fsize

        ! 1D representation of the domain input; only real, size of domain.
        optr => in%get1DArrayPtr()

        ! complex pointer to input data
        cptr1 => out%get3DArrayPtr()

        if (associated(this%transform)) then
            ! the size of the input full data 1d array
            fsize = this%nx*this%ny*this%nz

            ! 3D representation of input array (real)
            dptr1(1:this%nx,1:this%ny,1:this%nz) => optr(1:fsize)
        else
            ! the size of the input full data 1d array
            fsize = this%nx*this%ny*this%nr

            ! 3D representation of input array (real)
            dptr1(1:this%nx,1:this%ny,1:this%nr) => optr(1:fsize)
        end if

        cptr1 = 0.

        if (associated(this%transform)) then
            ! now apply the transpose of the vertical EOFs to full input to get the PCs
            do j=1,this%ny
                do i=1,this%nx
                    cptr1(i,j,1:this%nr) = matmul(transpose(this%transform),dptr1(i,j,1:this%nz))
                end do
            end do
        else
            ! now apply the transpose of the vertical EOFs to full input to get the PCs
            do i=1,this%nx
                do j=1,this%ny
                    cptr1(i,j,1:this%nr) = dptr1(i,j,1:this%nr)
                end do
            end do
        end if
    end subroutine

    subroutine copyToDomain(this, in, out, c)
        implicit none

        class(BHalf3dFftOperator)             :: this
        class(VecFourierMultVector),  pointer :: in
        class(AbstractVector),        pointer :: out
        real(8), optional,         intent(in) :: c

        real(8),                   dimension(:),     pointer :: optr
        real(8),                   dimension(:,:,:), pointer :: dptr1
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: cptr1

        integer :: i, j, fsize

        ! complex pointer to input data
        cptr1 => in%get3DArrayPtr()

        ! 1D representation of the full output data
        optr => out%get1DArrayPtr()

        if (associated(this%transform)) then
            ! the size of the output full data 1d array
            fsize = this%nx*this%ny*this%nz

            ! 3D representation of input array (real)
            dptr1(1:this%nx,1:this%ny,1:this%nz) => optr(1:fsize)
        else
            ! the size of the input full data 1d array
            fsize = this%nx*this%ny*this%nr

            ! 3D representation of output array (real)
            dptr1(1:this%nx,1:this%ny,1:this%nr) => optr(1:fsize)
        end if

        if (associated(this%transform)) then
            ! now apply the vertical EOFs to the PCs to get the full output
            if (present(c)) then
                do j=1,this%ny
                    do i=1,this%nx
                        dptr1(i,j,1:this%nz) = dptr1(i,j,1:this%nz) + c*matmul(this%transform,real(cptr1(i,j,1:this%nr)))
                    end do
                end do
            else
                do j=1,this%ny
                    do i=1,this%nx
                        dptr1(i,j,1:this%nz) = matmul(this%transform,real(cptr1(i,j,1:this%nr)))
                    end do
                end do
            end if
        else
            if (present(c)) then
                do j=1,this%ny
                    do i=1,this%nx
                        dptr1(i,j,1:this%nr) = dptr1(i,j,1:this%nr) + c*real(cptr1(i,j,1:this%nr))
                    end do
                end do
            else
                do j=1,this%ny
                    do i=1,this%nx
                        dptr1(i,j,1:this%nr) = real(cptr1(i,j,1:this%nr))
                    end do
                end do
            end if
        end if
    end subroutine
end module
