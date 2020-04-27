module diagonalMatrixOperator_mod
    use abstractVectorOperator_mod
    use abstractVector_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(AbstractVectorOperator), public :: DiagonalMatrixOperator
        private
            integer :: m, n
            double precision, dimension(:), pointer :: data1d => NULL()

        contains
            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd

            procedure :: diagonalMatrixOperatorConstructor
            final     :: diagonalMatrixOperatorDestructor
    end type

    contains

    subroutine diagonalMatrixOperatorConstructor(this,data1d)
        implicit none

        class(DiagonalMatrixOperator) :: this
        double precision, dimension(:) :: data1d

        integer :: m

        m = size(data1d)

        this%m = m
        this%n = m
        allocate(this%data1d(m))
        this%data1d(:) = data1d(:)
    end subroutine

    subroutine diagonalMatrixOperatorDestructor(this)
        implicit none

        type(DiagonalMatrixOperator)  :: this

        deallocate(this%data1d)
    end subroutine

    subroutine applyOperator(this, in, out)
        implicit none

        class(DiagonalMatrixOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        integer :: i, isize, osize, m, o

        ! out   = this%data1d * in
        ! m x o   m x m         m x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        ! the diagonal matrix operator is m x m, i.e. square
        m = size(this%data1d)
        ! which must match some multiple of the input size
        if (mod(isize,m) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with diagonal operator of size',m
            call error(msgstr)
        end if

        if (m == 0) then
            write(msgstr,*) 'Not applying size zero data:',m,isize
            call print(msgstr)
            return
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (isize .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in diagonal operator.'
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Diagonal matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! diagonal matrix multiplication is quite simple. yay for getting here eventually.
        do i=1,m
            optr2(i,:) = this%data1d(i)*iptr2(i,:)
        end do
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(DiagonalMatrixOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        integer :: i, isize, osize, m, o

        ! out   = this%data1d * in
        ! m x o   m x m         m x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        ! the diagonal matrix operator is m x m, i.e. square
        m = size(this%data1d)
        ! which must match some multiple of the input size
        if (mod(isize,m) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with diagonal operator of size',m
            call error(msgstr)
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (isize .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in diagonal operator.'
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Diagonal matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! diagonal matrix multiplication is quite simple. yay for getting here eventually.
        do i=1,m
            optr2(i,:) = optr2(i,:) + c*this%data1d(i)*iptr2(i,:)
        end do
    end subroutine

    subroutine applyTranspose(this, in, out)
        implicit none

        class(DiagonalMatrixOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        ! transpose of a diagonal matrix is the same operator
        call this%applyOperator(in,out)
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(DiagonalMatrixOperator)  :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        ! transpose of a diagonal matrix is the same operator
        call this%applyOperatorAndAdd(in,out,c)
    end subroutine
end module
