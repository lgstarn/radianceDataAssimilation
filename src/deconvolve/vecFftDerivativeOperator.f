module vecFftDerivativeOperator_mod
    use, intrinsic :: iso_c_binding
    use abstractVector_mod
    use abstractVectorOperator_mod
    use vecFourierMultOperator_mod
    use vecFourierMultVector_mod
    use linkedList_mod
    use fftwVector_mod
    use fftwVectorOperator_mod
    use localization_mod
    use bHalf3DFftOperator_mod
    use mpiUtils_mod

    implicit none

    real(8), parameter :: pi = 4.d0*datan(1.D0)

    private

    public :: VecFftDerivativeOperator

    type, extends(BHalf3DFftOperator) :: VecFftDerivativeOperator
        !private
            logical :: normalized = .false.

        contains
            procedure :: vecFftDerivativeOperatorConstructor
            final     :: vecFftDerivativeOperatorDestructor

            procedure :: addDerivatives
            procedure :: absoluteValue
            procedure :: exponentiate
            procedure :: normalize
            procedure, nopass :: pow
    end type

    contains

    subroutine vecFftDerivativeOperatorConstructor(this,nx,ny,nz,nr,veofs,bypass,doForward)
        implicit none

        class(VecFftDerivativeOperator)     :: this
        integer,       intent(in)           :: nx, ny, nz, nr
        real(8),       intent(in), optional :: veofs(nz,nr)
        logical,       intent(in), optional :: bypass
        logical,       intent(in), optional :: doForward

        integer :: nxl, nyl

        logical :: dobypass

        this%nx = nx
        this%ny = ny
        this%nr = nr

!        nxl = nx + 10
!        nyl = ny + 10

        nxl = nx
        nyl = ny

!        nxl = 2*nx
!        nyl = 2*ny

        ! make sure the sizes are odd
        nxl = nxl + mod(nxl+1,2)
        nyl = nyl + mod(nyl+1,2)

        if (present(bypass)) then
            dobypass = bypass
        else
            dobypass = .false.
        end if

        allocate(this%fftVec1)
        call this%fftVec1%vecFourierMultVectorConstructor(nxl, nyl, nr)

        allocate(this%fftvec2)
        call this%fftVec2%vecFourierMultVectorConstructor(nxl, nyl, nr)

        if (.not. dobypass) then
            allocate(this%fftMult)
            call this%fftMult%vecFourierMultVectorConstructor(nxl,nyl,nr)
        end if

        call this%bHalf3dFftOperatorConstructor(nx,ny,nz,nr, &
            & this%fftVec1,this%fftVec2,this%fftMult,veofs,doForward)
    end subroutine

    subroutine vecFftDerivativeOperatorDestructor(this)
        implicit none

        type(VecFftDerivativeOperator)  :: this

        if (associated(this%fftMult)) then
            deallocate(this%fftMult)
        end if

        if (associated(this%fftVec1)) then
            deallocate(this%fftVec1)
        end if

        if (associated(this%fftVec2)) then
            deallocate(this%fftVec2)
        end if
    end subroutine

    subroutine addDerivatives(this,avals,svals,nind)
        implicit none

        class(VecFftDerivativeOperator)        :: this
        integer,                    intent(in) :: nind
        real(8), dimension(nind),   intent(in) :: avals
        real(8), dimension(2,nind), intent(in) :: svals

        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: C1

        class(FftwVector),           pointer :: inv

        integer :: i, j, r, ind
        integer :: width, height
        integer :: w2, h2
        real(8) :: k1, k2
        complex(C_DOUBLE_COMPLEX) :: sqrtm1 = (0,1)
        complex(C_DOUBLE_COMPLEX) :: wf, hf

        if (this%normalized) then
            write(msgstr,*) 'Error in vecFftDerivativeOperator addDerivative: ' // &
                & 'cannot change derivatives once normalized'
            call error(msgstr)
        end if

        call this%fftMult%firstVector()

        do r=1,this%fftMult%numVectors()
            inv => this%fftMult%nextVector()

            C1 => inv%get2DPointer()

            width  = size(C1,1)
            height = size(C1,2)

            w2 = width/2.d0
            h2 = height/2.d0
            wf = 2.d0*pi*sqrtm1/dble(width)
            hf = 2.d0*pi*sqrtm1/dble(height)

            do j=0,height-1
                if (j <= h2) then
                    k2 = j
                else
                    k2 = j - height
                end if

                do i=0,width-1
                    if (i <= w2) then
                        k1 = i
                    else
                        k1 = i - width
                    end if

                    do ind=1,nind
                        C1(i+1,j+1) = C1(i+1,j+1) + avals(ind)*pow(wf*k1,svals(1,ind))*pow(hf*k2,svals(2,ind))
                    end do
                end do
            end do
        end do
    end subroutine

    subroutine absoluteValue(this)
        implicit none

        class(VecFftDerivativeOperator)        :: this

        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: C1

        class(FftwVector),           pointer :: inv

        integer :: i, j, r
        real(8) :: zi, zr

        complex(C_DOUBLE_COMPLEX), dimension(:,:), allocatable :: svalsc

        if (this%normalized) then
            write(msgstr,*) 'Error in vecFftDerivativeOperator absoluteValue: cannot change derivatives once normalized'
            call error(msgstr)
        end if

        call this%fftMult%firstVector()

        do r=1,this%fftMult%numVectors()
            inv => this%fftMult%nextVector()

            C1 => inv%get2DPointer()

            do j=1,size(C1,2)
                do i=1,size(C1,1)
                    zr = real(C1(i,j))
                    zi = aimag(C1(i,j))
                    C1(i,j) = sqrt(zr*zr+zi*zi)
                end do
            end do
        end do
    end subroutine

    function pow(z, p) result(c)
        implicit none

        complex(C_DOUBLE_COMPLEX), intent(in) :: z
        real(8), intent(in) :: p

        complex(C_DOUBLE_COMPLEX) :: c

        real(8) :: rz, iz, absz, anglz

        rz = real(z)
        iz = aimag(z)
        absz = sqrt(rz*rz+iz*iz)**p
        anglz = atan2(iz, rz)*p
        c = cmplx(absz*cos(anglz),absz*sin(anglz))
    end function

    subroutine exponentiate(this,kval)
        implicit none

        class(VecFftDerivativeOperator) :: this
        real(8),             intent(in) :: kval

        class(FftwVector), pointer :: inv

        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: C1

        integer :: i, j, r

        call this%fftMult%firstVector()

        do r=1,this%fftMult%numVectors()
            inv => this%fftMult%nextVector()

            C1 => inv%get2DPointer()

            do j=1,size(C1,2)
                do i=1,size(C1,1)
                    C1(i,j) = pow(C1(i,j),kval)
                end do
            end do
        end do
    end subroutine

    subroutine normalize(this)
        implicit none

        class(VecFftDerivativeOperator)         :: this
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d

        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: C1

        if (this%normalized) then
            write(msgstr,*) 'Error in vecFftDerivativeOperator normalize: cannot normalize more than once'
            call error(msgstr)
        end if

        ! now normalize and take the square-root
        data3d => this%fftMult%get3DArrayPtr()
        data3d = data3d/dble(size(data3d,1)*size(data3d,2))

        this%normalized = .true.
    end subroutine
end module
