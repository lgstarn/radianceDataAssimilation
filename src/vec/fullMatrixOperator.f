module fullMatrixOperator_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(AbstractVectorOperator), public :: FullMatrixOperator
        private
            integer                          :: m
            integer                          :: n
            real(8), dimension(:,:), pointer :: data2d

        contains
            procedure :: fullMatrixOperatorConstructor_mndata
            procedure :: fullMatrixOperatorConstructor_data

            procedure :: applyOperator
            procedure :: applyTranspose
            procedure :: applyOperatorAndAdd
            procedure :: applyTransposeAndAdd

            final :: fullMatrixOperatorDestructor ! clean up all allocated variables
    end type

!    interface FullMatrixOperator
!        procedure fullMatrixOperatorConstructor_mndata, fullMatrixOperatorConstructor_data ! allow generic instantiation
!    end interface

    contains

    subroutine fullMatrixOperatorConstructor_mndata(this,m,n)
        implicit none

        class(FullMatrixOperator) :: this
        integer, intent(in)       :: m, n

        this%m = m
        this%n = n

        allocate(this%data2d(m,n))
    end subroutine

    subroutine fullMatrixOperatorConstructor_data(this,data2d)
        implicit none

        class(FullMatrixOperator) :: this
        real(8), dimension(:,:), intent(in) :: data2d

        this%m = size(data2D,1)
        this%n = size(data2D,2)

        allocate(this%data2d(this%m,this%n))

        this%data2d(:,:) = data2d(:,:)
    end subroutine

    subroutine fullMatrixOperatorDestructor(this)
        implicit none

        type(FullMatrixOperator)  :: this

        deallocate(this%data2d)
    end subroutine

    subroutine applyOperator(this, in, out)
        implicit none

        class(FullMatrixOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        integer :: isize, osize
        integer :: m, n, o

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        ! out   = this%matrixobj * in%matrixobj
        ! m x o    m x n            n x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        m = size(this%data2d,1)
        n = size(this%data2d,2)

        ! the matrix operator is m x n, which must match some multiple of the input size
        if (mod(isize,n) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with full matrix operator of size',m,n
            call error(msgstr)
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (m*o .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in full operator',m,n
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Full matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! use the intrinsic matmul function for (hopefully) better performance
        optr2(:,:) = matmul(this%data2d,iptr2)
    end subroutine

    subroutine applyTranspose(this, in, out)
        implicit none

        class(FullMatrixOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out

        integer :: isize, osize
        integer :: m, n, o

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        ! out   = this%matrixobj * in%matrixobj
        ! m x o    m x n            n x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        m = size(this%data2d,1)
        n = size(this%data2d,2)

        ! the matrix operator is m x n, which must match some multiple of the input size
        if (mod(isize,n) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with full matrix operator of size',m,n
            call error(msgstr)
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (m*o .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in full operator',m,n
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Full matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! use the intrinsic matmul function for (hopefully) better performance
        optr2(:,:) = matmul(transpose(this%data2d),iptr2)
    end subroutine

    subroutine applyOperatorAndAdd(this, in, out, c)
        implicit none

        class(FullMatrixOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        integer :: isize, osize
        integer :: m, n, o

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        ! out   = this%matrixobj * in%matrixobj
        ! m x o    m x n            n x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        m = size(this%data2d,1)
        n = size(this%data2d,2)

        ! the matrix operator is m x n, which must match some multiple of the input size
        if (mod(isize,n) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with full matrix operator of size',m,n
            call error(msgstr)
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (m*o .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in full operator',m,n
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Full matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! use the intrinsic matmul function for (hopefully) better performance
        optr2(:,:) = optr2(:,:) + c*matmul(this%data2d,iptr2)
    end subroutine

    subroutine applyTransposeAndAdd(this, in, out, c)
        implicit none

        class(FullMatrixOperator) :: this
        class(AbstractVector), pointer :: in
        class(AbstractVector), pointer :: out
        real(8),            intent(in) :: c

        integer :: isize, osize
        integer :: m, n, o

        real(8), dimension(:),   pointer :: iptr, optr
        real(8), dimension(:,:), pointer :: iptr2, optr2

        ! out   = this%matrixobj * in%matrixobj
        ! m x o    m x n            n x o

        ! match up the sizes to what makes sense for a diagonal matrix.
        ! assume the caller knew what they were doing (can only check if the mod of the size is not the same)
        ! we'll also assume that if this is a 2D vector (i.e. the length of the vector in array form is an integer
        ! multiple of the size m of the diagonal in this matrix) it is stored in fortran format

        isize = in%getSize()
        osize = out%getSize()

        m = size(this%data2d,1)
        n = size(this%data2d,2)

        ! the matrix operator is m x n, which must match some multiple of the input size
        if (mod(isize,n) .ne. 0) then
            write(msgstr,*) 'Error: size of input vector',isize,'incompatiable with full matrix operator of size',m,n
            call error(msgstr)
        end if

        ! which gives us the size of the input
        o = isize/m

        ! this must match the size of the output (m x o)
        if (m*o .ne. osize) then
            write(msgstr,*) 'Error: sizes of input vector',isize,'incompatiable with output size',osize,'in full operator',m,n
            call error(msgstr)
        end if

        iptr =>  in%get1DArrayPtr()
        optr => out%get1DArrayPtr()

        if (.not. associated(iptr) .or. .not. associated(optr)) then
            write(msgstr,*) 'Full matrix operator requires direct 1D access to the data.'
            call error(msgstr)
        end if

        ! conform the arrays through the beauty of Fortran pointers
        iptr2(1:m,1:o) => iptr(1:isize)
        optr2(1:m,1:o) => optr(1:osize)

        ! use the intrinsic matmul function for (hopefully) better performance
        optr2(:,:) = optr2(:,:) + c*matmul(this%data2d,iptr2)
    end subroutine
end module
