module testmod

    use, intrinsic :: iso_c_binding
    use mpiUtils_mod
    implicit none

    private

    include 'fftw3.f03'

    public :: seqxfft3, allocArray, check

    public :: FFTW_FORWARD, FFTW_BACKWARD

    complex(C_DOUBLE_COMPLEX), pointer, contiguous, dimension(:,:,:), public :: arrayA, arrayB, arrayC

    contains

    subroutine allocArray(ndims,sizes,array,name)
        implicit none

        integer, intent(in) :: ndims
        integer, intent(in) :: sizes(ndims)
        complex(C_DOUBLE_COMPLEX), pointer, contiguous, dimension(:,:,:) :: array
        character(1) :: name

        type(C_PTR) :: dataptr ! C-style pointer to the data, used to interface with FFTW C-routines directly

        dataptr = fftw_alloc_complex(int(product(sizes), C_SIZE_T))
        call c_f_pointer(dataptr, array, [sizes(1),sizes(2),sizes(3)])

        !print *,'size of array',name,' ',shape(array)
        !print *,'size of array',name,' ',sizes
    end subroutine

    subroutine seqxfft3(sizes, array, axis, esign)
        implicit none

        integer, intent(in) :: sizes(3)
        complex(C_DOUBLE_COMPLEX), pointer, contiguous, dimension(:,:,:) :: array
        integer, intent(in) :: axis
        integer, intent(in) :: esign

        type(C_PTR) :: plan

        integer :: i, j

        if (axis == 1) then
            plan =  fftw_plan_many_dft(1, sizes(1), sizes(2)*sizes(3), &
                array, sizes, 1, sizes(1), &
                array, sizes, 1, sizes(1), &
                esign, FFTW_ESTIMATE)

            call fftw_execute_dft(plan, array, array)

            call fftw_destroy_plan(plan)

        else if (axis == 2) then
            plan =  fftw_plan_many_dft(1, sizes(2), sizes(3), &
                array(1,:,:), sizes, 1, sizes(2), &
                array(1,:,:), sizes, 1, sizes(2), &
                esign, FFTW_ESTIMATE)

            do i=1,size(array,1)
                call fftw_execute_dft(plan, array(i,:,:), array(i,:,:))
            end do

            call fftw_destroy_plan(plan)
        else
            plan =  fftw_plan_many_dft(1, sizes(3), sizes(1)*sizes(2), &
                array, sizes, sizes(1)*sizes(2), 1, &
                array, sizes, sizes(1)*sizes(2), 1, &
                esign, FFTW_ESTIMATE)

            call fftw_execute_dft(plan, array, array)

            call fftw_destroy_plan(plan)
        end if

        if (esign == FFTW_BACKWARD) then
            array = array/dble(sizes(axis))
        end if
        array = array
    end subroutine

    subroutine check(err, message)
       use mpi
       use netcdf
       implicit none
       integer err
       character(len=*), optional :: message

       ! It is a good idea to check returned value for possible error
       if (err .NE. NF90_NOERR) then
        if (present(message)) then
           write(6,*) trim(message), trim(nf90_strerror(err))
        else
           write(6,*) trim(nf90_strerror(err))
        end if
           call MPI_Abort(MPI_COMM_WORLD, -1, err)
       end if
    end subroutine
end module

program testRoundTripParallelFft

    use testmod
    use mpiUtils_mod
    use parallelDecomposition_mod
    use gridTopology_mod
    use, intrinsic :: iso_c_binding

    implicit none

    integer :: numElemInPart, npart, numElements, startIndexOfPart

    integer, parameter :: nx = 3
    integer, parameter :: ny = 4
    integer, parameter :: nz = 5

    character(len=*), parameter :: dim1 = "DIM1"
    character(len=*), parameter :: dim2 = "DIM2"
    character(len=*), parameter :: dim3 = "DIM3"

    integer, dimension(3) :: N = (/nz,ny,nx/)
    integer, dimension(3) :: staggers = (/0,0,0/)

    integer :: ierr

    integer, dimension(2) :: subcomms

    integer, dimension(3) :: sizesA,  sizesB,  sizesC
    integer, dimension(3) :: startsA, startsB, startsC

    real(8) :: testMatrix(nz,ny,nx)

    integer :: i, j, k, cursor, rank, il, jl, kl

    integer :: cdisp1, cdisp2, cdisp3

    integer :: ncid, nprocs

    character(:), pointer :: dimNames(:)

    class(ParallelDecomposition), pointer :: decompA, decompB, decompC
    class(GridTopology),          pointer :: topology

    call MPI_Init(ierr); call mpichk(ierr,'In testRoundTripParallelFft MPI_Init:')

     call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr); call mpichk(ierr,&
                     'In print message output MPI_Comm_rank:')

    allocate(decompA)
    call decompA%parallelDecompositionConstructor(dim2,dim3)
    allocate(decompB)
    call decompB%parallelDecompositionConstructor(dim1,dim3)
    allocate(decompC)
    call decompC%parallelDecompositionConstructor(dim1,dim2)

    allocate(character(len=4) :: dimNames(3))

    dimNames(1) = dim1
    dimNames(2) = dim2
    dimNames(3) = dim3

    call decompA%decompose(dimNames,3,N,staggers,sizesA,startsA)
    call decompB%decompose(dimNames,3,N,staggers,sizesB,startsB)
    call decompC%decompose(dimNames,3,N,staggers,sizesC,startsC)

    allocate(topology)
    call topology%gridTopologyConstructor(MPI_COMM_WORLD,2)

    subcomms = topology%getSubCommunicators()

    call allocArray(3,sizesA,arrayA,'A')
    call allocArray(3,sizesB,arrayB,'B')
    call allocArray(3,sizesC,arrayC,'C')

    do k=1,sizesA(3)
        do j=1,sizesA(2)
            do i=1,sizesA(1)
                il = i+startsA(1)-1
                jl = j+startsA(2)-1
                kl = k+startsA(3)-1
                cursor = il+jl*N(1)+kl*N(2)*N(1)
                arrayA(i,j,k) = cmplx(dble(cursor),dble(cursor),kind=8)
                !write(*,'(I12,1X,I12,1X,I12,1X,F14.6,1X,F14.6)') &
                !    & il+1,jl+1,kl+1,real(arrayA(i,j,k)),imag(arrayA(i,j,k))
            end do
        end do
    end do

    call seqxfft3(sizesA, arrayA, 1, FFTW_FORWARD)
    call exchangeDoubleComplex3D(subcomms(1), sizesA, arrayA, 1, sizesB, arrayB, 2)
    call seqxfft3(sizesB, arrayB, 2, FFTW_FORWARD)
    call exchangeDoubleComplex3D(subcomms(2), sizesB, arrayB, 2, sizesC, arrayC, 3)
    call seqxfft3(sizesC, arrayC, 3, FFTW_FORWARD)

    do k=1,sizesC(3)
        do j=1,sizesC(2)
            do i=1,sizesC(1)
                il = i+startsC(1)-1
                jl = j+startsC(2)-1
                kl = k+startsC(3)-1

                !write(msgstr,*)  il+1,jl+1,kl+1,real(arrayC(i,j,k)),imag(arrayC(i,j,k))
                !call print(msgstr)
            end do
        end do
    end do

    call seqxfft3(sizesC, arrayC, 3, FFTW_BACKWARD)
    call exchangeDoubleComplex3D(subcomms(2), sizesC, arrayC, 3, sizesB, arrayB, 2)
    call seqxfft3(sizesB, arrayB, 2, FFTW_BACKWARD)
    call exchangeDoubleComplex3D(subcomms(1), sizesB, arrayB, 2, sizesA, arrayA, 1)
    call seqxfft3(sizesA, arrayA, 1, FFTW_BACKWARD)

    do k=1,sizesA(3)
        do j=1,sizesA(2)
            do i=1,sizesA(1)
                il = i+startsA(1)-1
                jl = j+startsA(2)-1
                kl = k+startsA(3)-1
                cursor = il+jl*N(1)+kl*N(2)*N(1)

                !write(*,'(I12,1X,I12,1X,I12,1X,F14.6,1X,F14.6)') &
                !    & il+1,jl+1,kl+1,real(arrayA(i,j,k)),imag(arrayA(i,j,k))

                if (abs(arrayA(i,j,k) - cmplx(dble(cursor),dble(cursor),kind=8)) > 1d-8) then
                    write(msgstr,*) 'Error1: ',i,j,k,abs(arrayA(i,j,k) - cmplx(dble(cursor),dble(cursor),kind=8)),&
                        arrayA(i,j,k),cmplx(dble(cursor),dble(cursor),kind=8)
                    call error(msgstr)
                end if

                ! print *, il+1,jl+1,kl+1,real(arrayA(i,j,k)),imag(arrayA(i,j,k))
            end do
        end do
    end do

    call writePNetcdf3dDoubleArray(MPI_COMM_WORLD,real(arrayA),sizesA,startsA,N)

    deallocate(arrayA)
    deallocate(arrayB)
    deallocate(arrayC)
    deallocate(decompA)
    deallocate(decompB)
    deallocate(decompC)
    call MPI_FINALIZE(ierr)
    call check(ierr)
end program

subroutine writePNetcdf3dDoubleArray(comm,array3d,lsizes,lstarts,dims)
    use mpi
    use netcdf
    use pnetcdf, only : nf90mpi_put_varn_all, nf90mpi_create
    use testmod
    use, intrinsic :: iso_c_binding

    implicit none

    integer, intent(in) :: comm
    integer, intent(in) :: lsizes(3)
    integer, intent(in) :: lstarts(3)
    real(8), dimension(lsizes(1),lsizes(2),lsizes(3)) :: array3d
    integer, intent(in) :: dims(3)

    integer :: cmode, ncid, varid, ierr
    real buffer(13)
    integer pnc_starts(3)
    integer pnc_counts(3)

    integer :: nx, ny, nz

    integer :: dimid(3), rank

    integer :: i

    nx = dims(3)
    ny = dims(2)
    nz = dims(1)

    call MPI_Comm_rank(comm,rank,ierr)

    cmode = IOR(IOR(nf90_netcdf4, NF90_MPIIO),NF90_CLOBBER)

    ierr = nf90_create_par(path='testPNC.nc', cmode=cmode, comm=MPI_COMM_WORLD, ncid=ncid, &
                     info=MPI_INFO_NULL)
    call check(ierr, 'In nf90_create_par: ')

    ! create a global array of size NZ * NY * NX
    ierr = nf90_def_dim(ncid, "Z", NZ, dimid(1))
    call check(ierr, 'In nf90_def_dim X: ')
    ierr = nf90_def_dim(ncid, "Y", NY, dimid(2))
    call check(ierr, 'In nf90_def_dim Y: ')
    ierr = nf90_def_dim(ncid, "X", NX, dimid(3))
    call check(ierr, 'In nf90_def_dim Z: ')
    ierr = nf90_def_var(ncid, "var", NF90_DOUBLE, dimid, varid)
    call check(ierr, 'In nf90_def_var var: ')
    ierr = nf90_enddef(ncid)
    call check(ierr, 'In nf90_enddef: ')
    pnc_counts(:) = lsizes
    pnc_starts(:) = lstarts

    ! set the buffer pointers to different offsets to the I/O buffer
    ierr = nf90_put_var(ncid=ncid, varid=varid, values=array3d, &
        start=pnc_starts, count=pnc_counts) !, &
    call check(ierr, 'In nf90_put_var: ')
    ierr = nf90_close(ncid);
    call check(ierr, 'In nf90_close: ')
end subroutine

!subroutine check(ierr)
!    use mpi
!
!    implicit none
!
!    integer, intent(in) :: ierr
!
!    integer :: rc, ierr2
!
!    if (ierr .ne. MPI_SUCCESS) then
!        print *,'There was an MPI error!'
!        rc = 5
!        call mpi_abort(MPI_COMM_WORLD, rc, ierr2)
!        write(*,'(A,I8)') 'Error starting MPI program. Terminating. RC:',rc
!    end if
!
!end subroutine



