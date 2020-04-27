module vertEofHorzFftOperator_mod
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

    implicit none

    private

    public :: VertEofHorzFftOperator

    type, extends(BHalf3DFftOperator) :: VertEofHorzFftOperator
        !private
            class(VecFourierMultVector), pointer :: C1v, C1pv
            class(LinkedList),           pointer :: C1op
            real(8),      dimension(:),  pointer :: lvals

        contains
            procedure :: vertEofHorzFftOperatorConstructor
            final     :: vertEofHorzFftOperatorDestructor

            procedure, private :: setCorrelationsFromL
    end type

    contains

    subroutine vertEofHorzFftOperatorConstructor(this,nx,ny,nz,nr,veofs,lvals)
        implicit none

        class(VertEofHorzFftOperator) :: this
        integer, intent(in) :: nx, ny, nz, nr
        real(8), dimension(nz,nr) :: veofs
        real(8), dimension(nr)    :: lvals

        real(8) :: maxLval
        integer :: maxLceil, nxl, nyl

        class(VecFourierMultVector), pointer :: vec1
        class(VecFourierMultVector), pointer :: vec2

        allocate(this%lvals(nr))

        this%lvals = lvals

        maxLval  = maxval(lvals)
        maxLceil = ceiling(maxLval)

        nxl = nx+2*maxLceil
        nyl = ny+2*maxLceil

        ! make nxl and nyl odd
        nxl = nxl + mod(nxl+1,2)
        nyl = nyl + mod(nyl+1,2)

        allocate(this%C1v,this%C1pv)

        call this%C1v%vecFourierMultVectorConstructor(nxl, nyl, nr)
        call this%C1pv%vecFourierMultVectorConstructor(nxl, nyl, nr)

        call this%createVecOperator(this%C1op,this%C1v,this%C1pv)

        call this%setCorrelationsFromL(this%C1op,this%C1v,this%C1pv,nr,lvals)

        allocate(this%fftVec1)
        call this%fftVec1%vecFourierMultVectorConstructor(nxl, nyl, nr)

        allocate(this%fftvec2)
        call this%fftVec2%vecFourierMultVectorConstructor(nxl, nyl, nr)

        call this%bHalf3dFftOperatorConstructor(nx,ny,nz,nr,this%fftVec1,this%fftVec2,this%C1pv,veofs)
    end subroutine

    subroutine vertEofHorzFftOperatorDestructor(this)
        implicit none

        type(VertEofHorzFftOperator)  :: this

        deallocate(this%lvals)

        call this%destroyVecOperator(this%C1op,this%C1v,this%C1pv)

        if (associated(this%fftVec1)) then
            deallocate(this%fftVec1)
        end if

        if (associated(this%fftVec2)) then
            deallocate(this%fftVec2)
        end if
    end subroutine

    subroutine setCorrelationsFromL(this,C1op,in,out,nr,lvals)
        implicit none

        class(VertEofHorzFftOperator)        :: this
        class(LinkedList),           pointer :: C1op
        class(VecFourierMultVector), pointer :: in
        class(VecFourierMultVector), pointer :: out
        integer, intent(in)                  :: nr
        real(8), dimension(nr)               :: lvals

        class(FftwVector),           pointer :: inv
        class(FftwVector),           pointer :: outv
        class(FftwVectorOperator),   pointer :: opv
        class(*),                    pointer :: optr

        complex(C_DOUBLE_COMPLEX), dimension(:,:),   pointer :: C1
        complex(C_DOUBLE_COMPLEX), dimension(:,:,:), pointer :: data3d

        integer :: i,j,k,ll,r
        integer :: dx, dy, width, height
        real(8) :: d

        call in%firstVector()

        ! first column in matrix; calculate distance from (i,j) to (1,1)
        k = 1
        ll = 1

        do r=1,in%numVectors()
            inv => in%nextVector()

            C1 => inv%get2DPointer()

            width = size(C1,1)
            height = size(C1,2)

            do i=1,width
                do j=1,height

                    ! x periodic boundary distance from (i,j) to (k,ll)
                    dx = abs(i - k)

                    if (dx .gt. width/2) then
                        dx = width - dx
                    end if

                    ! y periodic boundary distance from (i,j) to (k,ll)
                    dy = abs(j - ll)

                    if (dy .gt. height/2) then
                        dy = height - dy
                    end if

                    d = sqrt(dble(dx**2 + dy**2))

                    C1(i,j) = covfac_gaspari_cohn(d,lvals(r))
                end do
            end do
        end do

        call this%executeVecOperator(C1op,in,out)

        ! now normalize and take the square-root
        data3d => out%get3DArrayPtr()
        data3d = sqrt(data3d)/(sqrt(dble(size(data3d,1)*size(data3d,2))))
    end subroutine
end module
