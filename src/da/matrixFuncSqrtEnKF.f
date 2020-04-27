program matrixFuncSqrtEnKF
    use mpi
    use mpiUtils_mod
!    use slabDecomposition_mod

    implicit none

    class(SlabDecomposition),   pointer :: decomp_slab

    character(len=256)                  :: namelistFileName = 'mfnSqrtEnKF.input'
    character(:), allocatable           :: namelistFileContents

    integer                             :: ndims
    integer, dimension(:), allocatable  :: dims, dims2
    integer                             :: pencilAxis

    namelist /mfnSqrtEnkfOptions/ ndims, dims, pencilAxis

    integer :: ierr

    ! initialize MPI
    call MPI_Init(ierr); call mpichk(ierr,'In MatrixFuncSqrtEnKF MPI_Init:')

    allocate(dims(100))

    ! load the namelist in an MPI-friendly way
    call mpiUtils_read_file(namelistFileName, namelistFileContents)
    read(namelistFileContents, nml=mfnSqrtEnkfOptions)

    allocate(dims2(ndims))

    dims2(:) = dims(1:ndims)

    ! initialize the grid decomposition
!    allocate(decomp_slab)
!    call decomp_slab%slabDecompositionConstructor(ndims,pencilAxis)
    ! call decomp%decompose(dims,localCounts,localStarts)

    ! setup the decomposition - done

    ! load and decompose the ensemble from disk

    ! determine ownership of observations

    ! compute the forward observations, QC, etc.

    ! setup the C_{Hx,Hx} matrix function operator

    ! compute the matrix function solution for the mean

    ! compute the matrix function solution for each ensemble member

    ! write the updated ensemble back to disk

    ! finalize MPI
    call MPI_finalize(ierr)
end program MatrixFuncSqrtEnKF
