module vertEofOperator_mod
    use iso_fortran_env
    use, intrinsic :: iso_c_binding

    use :: mpiUtils_mod
    use :: abstractVector_mod
    use :: abstractVectorOperator_mod

    implicit none

    private

    public :: VertEofOperator

    type, extends(AbstractVectorOperator) :: VertEofOperator
        !private
            integer               :: nz
            integer               :: nr
            integer               :: stride
            real(real64), pointer :: veofs(:,:) => NULL()
            real(real64), pointer :: vecz(:) => NULL()
            real(real64), pointer :: vecr(:) => NULL()

        contains
            procedure :: vertEofOperatorConstructor
            final     :: vertEofOperatorDestructor

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd
    end type

    contains

    subroutine vertEofOperatorConstructor(this,stride,nz,nr,veofs)
        implicit none

        class(VertEofOperator) :: this
        integer, intent(in) :: stride
        integer, intent(in) :: nz, nr
        real(8), dimension(nz,nr) :: veofs

        real(8) :: maxLval
        integer :: maxLceil, nxl, nyl

        this%stride = stride
        this%nz     = nz
        this%nr     = nr

        allocate(this%veofs(nz,nr))
        this%veofs(:,:) = veofs(:,:)

        allocate(this%vecz(nz))
        allocate(this%vecr(nr))
    end subroutine

    subroutine vertEofOperatorDestructor(this)
        implicit none

        type(VertEofOperator)  :: this

        if (associated(this%veofs)) then
            deallocate(this%veofs)
        end if

        if (associated(this%vecz)) then
            deallocate(this%vecz)
        end if

        if (associated(this%vecr)) then
            deallocate(this%vecr)
        end if
    end subroutine

    subroutine applyOperator(this, in, out)

        class(VertEofOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        real(real64), pointer :: inptr(:)
        real(real64), pointer :: outptr(:)

        integer :: n_in, n_out, n_all
        integer :: i, j, ind_in, ind_out

        inptr  => in%get1DArrayPtr()
        outptr => out%get1DArrayPtr()

        n_in = size(inptr)
        n_out = size(outptr)
        n_all = n_in/this%nr

        ! apply the operator to go from n_all x nr to n_all x nz
        if (mod(n_in,this%nr) /= 0 .or. mod(n_out,this%nz) /= 0 .or. &
            n_all /= size(outptr)/this%nz) then

            write(msgstr,*) 'Incompatible sizes in applyOperator:',&
                this%nz,this%nr,n_in,n_out
            call error(msgstr)
        end if

        ind_in = 1
        ind_out = 1

        do i=1,n_all
            do j=1,this%nr
                this%vecr(j) = inptr(ind_in)
                ind_in = ind_in + this%stride
            end do

            this%vecz = matmul(this%veofs,this%vecr)

            do j=1,this%nz
                outptr(ind_out) = this%vecz(j)
                ind_out = ind_out + this%stride
            end do
        end do
    end subroutine

    subroutine applyTranspose(this, in, out)

        class(VertEofOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        integer :: n_in, n_out, n_all
        integer :: i, j, ind_in, ind_out

        real(real64), pointer :: inptr(:)
        real(real64), pointer :: outptr(:)

        inptr  => in%get1DArrayPtr()
        outptr => out%get1DArrayPtr()

        n_in = size(inptr)
        n_out = size(outptr)
        n_all = n_in/this%nz

        ! apply the operator to go from n_all x nr to n_all x nz
        if (mod(n_in,this%nz) /= 0 .or. mod(n_out,this%nr) /= 0 .or. &
            n_all /= size(outptr)/this%nr) then

            write(msgstr,*) 'Incompatible sizes in applyTranpose:',&
                this%nz,this%nr,n_in,n_out
            call error(msgstr)
        end if

        ind_in = 1
        ind_out = 1

        do i=1,n_all
            do j=1,this%nz
                this%vecz(j) = inptr(ind_in)
                ind_in = ind_in + this%stride
            end do

            this%vecr = matmul(transpose(this%veofs),this%vecz)

            do j=1,this%nr
                outptr(ind_out) = this%vecr(j)
                ind_out = ind_out + this%stride
            end do
        end do

    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)

        class(VertEofOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8), intent(in)            :: c

        integer :: n_in, n_out, n_all
        integer :: i, j, ind_in, ind_out

        real(real64), pointer :: inptr(:)
        real(real64), pointer :: outptr(:)

        inptr  => in%get1DArrayPtr()
        outptr => out%get1DArrayPtr()

        n_in = size(inptr)
        n_out = size(outptr)
        n_all = n_in/this%nr

        ! apply the operator to go from n_all x nr to n_all x nz
        if (mod(n_in,this%nr) /= 0 .or. mod(n_out,this%nz) /= 0 .or. &
            n_all /= size(outptr)/this%nz) then

            write(msgstr,*) 'Incompatible sizes in applyOperator:',&
                this%nz,this%nr,n_in,n_out
            call error(msgstr)
        end if

        ind_in = 1
        ind_out = 1

        do i=1,n_all
            do j=1,this%nr
                this%vecr(j) = inptr(ind_in)
                ind_in = ind_in + this%stride
            end do

            this%vecz = matmul(this%veofs,this%vecr)

            do j=1,this%nz
                outptr(ind_out) = outptr(ind_out) + c*this%vecz(j)
                ind_out = ind_out + this%stride
            end do
        end do
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)

        class(VertEofOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8), intent(in)            :: c

        integer :: n_in, n_out, n_all
        integer :: i, j, ind_in, ind_out

        real(real64), pointer :: inptr(:)
        real(real64), pointer :: outptr(:)

        inptr  => in%get1DArrayPtr()
        outptr => out%get1DArrayPtr()

        n_in = size(inptr)
        n_out = size(outptr)
        n_all = n_in/this%nr

        ! apply the operator to go from n_all x nr to n_all x nz
        if (mod(n_in,this%nr) /= 0 .or. mod(n_out,this%nz) /= 0 .or. &
            n_all /= size(outptr)/this%nz) then

            write(msgstr,*) 'Incompatible sizes in applyTranpose:',&
                this%nz,this%nr,n_in,n_out
            call error(msgstr)
        end if

        ind_in = 1
        ind_out = 1

        do i=1,n_all
            do j=1,this%nz
                this%vecz(j) = inptr(ind_in)
                ind_in = ind_in + this%stride
            end do

            this%vecr = matmul(transpose(this%veofs),this%vecz)

            do j=1,this%nr
                outptr(ind_out) = outptr(ind_out) + c*this%vecr(j)
                ind_out = ind_out + this%stride
            end do
        end do
    end subroutine

end module
