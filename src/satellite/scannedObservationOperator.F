module scannedObservationOperator_mod

    use iso_fortran_env

    use linkedList_mod

    use observation_mod
    use observationOperator_mod
    use obsQcCodes_mod

    use scannedObservation_mod

    use satellitePlatformInfo_mod
    use satelliteObservation_mod

    use dataSet_mod
    use dataExtent_mod
    use dataVariable_mod

    use dataGrid_mod

    use parallelInfo_mod

    use geodesic_mod
    use quadUtils_mod
    use asciiUtils_mod
    use mpiUtils_mod

    use petscMat
    use petscVec

    implicit none

!#include <petsc/finclude/petscmatdef.h>
!#include <petsc/finclude/petscvecdef.h>
!#include <petsc/finclude/petscsysdef.h>
#include <petsc/finclude/petscmat.h>
#include <petsc/finclude/petscvec.h>
#include <petsc/finclude/petscsys.h>

    private

    ! minimum percentage of "good" (not out of domain, etc.) points to allow a scan
    real(real64), parameter :: DEFAULT_MIN_GOOD_RATIO = 0.7d0

    public :: ScannedObservationOperator

    type :: SOO_data
        !private
            class(LinkedList), pointer :: hMats
            Vec :: xvec, yvec

        contains
            final :: soo_dataDestructor
    end type

    ! holder class as ifort does not allow Mat pointers to be added to a linked list
    type :: mat_data
        Mat, pointer :: hmat => NULL()

        contains
            final :: mat_dataDestructor
    end type

    type, extends(ObservationOperator), abstract :: ScannedObservationOperator
        !private
            character(len=:), allocatable :: xVarName
            integer :: chstart
            integer :: chend
            integer :: nposting_channels
            type(SOO_data) :: sooData
            integer, allocatable :: obs_offsets(:)
            integer, allocatable :: obs_sizes(:)

        contains
            procedure :: scannedObservationOperatorConstructor
            !procedure :: scannedObservationOperatorConstructor_load

            procedure :: forward
            procedure :: tangentLinear
            procedure :: adjoint
            procedure :: firstMatrix
            procedure :: nextMatrix

            procedure(getNumberOfNonzeros_abs), deferred :: getNumberOfNonzeros
            procedure(getMaxStateIndices_abs),  deferred :: getMaxStateIndices
            procedure(getRowValues_abs),        deferred :: getRowValues
            procedure(isSameOperator_abs),      deferred :: isSameOperator

            ! Fortran standard disallows an abstract final, so the data is moved into SOO_data
            ! See https://gcc.gnu.org/ml/fortran/2013-12/msg00101.html for more info
            !final :: scannedObservationOperatorDestructor ! clean up all allocated variables
    end type

    abstract interface
        subroutine getNumberOfNonzeros_abs(this,pinfo,obs_so,inputGrid,minLat,maxLat,&
            & minLon,maxLon,d_nnz,o_nnz)

            import ParallelInfo
            import ScannedObservationOperator
            import ScannedObservation
            import DataGrid
            import real64

            class(ScannedObservationOperator)         :: this

            class(ParallelInfo),          pointer     :: pinfo
            class(ScannedObservation),    pointer     :: obs_so
            class(DataGrid),              pointer     :: inputGrid
            real(real64), dimension(:,:), intent(out) :: minLat, maxLat, minLon, maxLon
            PetscInt, dimension(:,:),      intent(out) :: d_nnz
            PetscInt, dimension(:,:),      intent(out) :: o_nnz
        end subroutine

        subroutine getRowValues_abs(this,pinfo,obs_so,inputGrid,obsnum,il,chind,rowSum,&
            &nind,xinds,fvals,scLat,scLon,lat,lon,arclen,angle,minLat,maxLat,minLon,maxLon, &
            &d_nnz_actual,o_nnz_actual)

            import ParallelInfo
            import ScannedObservationOperator
            import ScannedObservation
            import DataGrid
            import real64

            class(ScannedObservationOperator)       :: this

            class(ParallelInfo),        pointer     :: pinfo
            class(ScannedObservation),  pointer     :: obs_so
            class(DataGrid),            pointer     :: inputGrid
            integer,                    intent(in)  :: obsnum
            PetscInt,                   intent(in)  :: il
            integer,                    intent(in)  :: chind
            real(real64),               intent(out) :: rowSum
            PetscInt,                   intent(out) :: nind
            PetscInt,     dimension(:), intent(out) :: xinds
            real(real64), dimension(:), intent(out) :: fvals
            real(real64),               intent(in)  :: scLat
            real(real64),               intent(in)  :: scLon
            real(real64),               intent(in)  :: lat
            real(real64),               intent(in)  :: lon
            real(real64),               intent(in)  :: arclen
            real(real64),               intent(in)  :: angle
            real(real64),               intent(in)  :: minLat
            real(real64),               intent(in)  :: maxLat
            real(real64),               intent(in)  :: minLon
            real(real64),               intent(in)  :: maxLon
            PetscInt,                   intent(out) :: d_nnz_actual
            PetscInt,                   intent(out) :: o_nnz_actual
        end subroutine

        function getMaxStateIndices_abs(this,inputGrid) result(maxInd)
            import ScannedObservationOperator
            import DataGrid

            class(ScannedObservationOperator) :: this
            integer                           :: maxInd
            class(DataGrid),     pointer :: inputGrid
        end function

        function isSameOperator_abs(this,obs_so,ch1,ch2) result(canReuse)
            import ScannedObservationOperator
            import ScannedObservation

            class(ScannedObservationOperator) :: this
            class(ScannedObservation),       pointer :: obs_so
            integer, intent(in)                      :: ch1
            integer, intent(in)                      :: ch2
            logical                                  :: canReuse
        end function

    end interface

    contains

    subroutine scannedObservationOperatorConstructor(this, pinfo, obs, inputGrid, &
        & minGoodRatio, columnNormsVar, xVarName)

        implicit none

        class(ScannedObservationOperator)             :: this

        class(ParallelInfo),               pointer    :: pinfo
        class(ScannedObservation),         pointer    :: obs
        class(DataGrid),                   pointer    :: inputGrid
        real(real64),            optional, intent(in) :: minGoodRatio
        class(DataVariable),     optional, pointer    :: columnNormsVar
        character(len=*),        optional, intent(in) :: xVarName

        integer :: totalNz

        real(real64) :: lat, lon, scLat, scLon
        real(real64) :: arclen, angle1, angle2
        real(real64) :: min_good_ratio

        integer :: chind, i, ind, iz, itime, j, k, ki, kj
        integer :: scanNum, pixelNum
        integer :: ierr
        integer :: nx_global, ny_global
        integer :: nobs_global
        integer :: xs, xe, ys, ye
        integer :: nx_local,  ny_local
        integer :: nobs_local
        PetscInt :: oind
        PetscInt :: s1_l, s1_g, s2_l, s2_g

        PetscInt, dimension(1) :: izs

        PetscInt, dimension(:,:), pointer :: d_nnz, o_nnz

        PetscInt, dimension(:),   pointer :: xinds
        real(real64), dimension(:),   pointer :: fvals

        real(real64), dimension(:),     allocatable :: colNorms
        real(real64), dimension(:,:,:), pointer     :: colNormsData

        Mat, pointer :: hmat
        class(mat_data), pointer :: hmat_data
        class(*), pointer :: optr

        real(real64) :: tin
        real(real64), dimension(:,:), pointer :: minLat, maxLat, minLon, maxLon

        PetscInt :: il
        integer :: ncols, maxnz
        integer :: minj, mink, maxj, maxk

        character(4)   :: filenum
        character(256) :: filename

        integer :: ch1, ch2
        integer :: nloc
        logical :: rowComputed, reuseMat

        real(real64),              pointer :: obsLociPtr(:,:)
        real(real32),              pointer :: scLatPtr(:)
        real(real32),              pointer :: scLonPtr(:)
        integer,                   pointer :: owners(:)

        class(DataExtent), pointer :: chanExtent
        class(DataExtent), pointer :: nobsExtent

        class(DataVariable), pointer :: tbVar
        class(DataVariable), pointer :: obsData

        PetscInt :: d_nnz_actual
        PetscInt :: o_nnz_actual
        PetscInt, parameter :: one = 1

        PetscInt :: maxondiag, maxoffdiag

        PetscViewer :: viewer

        real(real64) :: info(MAT_INFO_SIZE)

        if (present(minGoodRatio)) then
            min_good_ratio = minGoodRatio
        else
            min_good_ratio = DEFAULT_MIN_GOOD_RATIO
        end if

        if (present(xVarName)) then
            allocate(character(len=len_trim(xVarName)) :: this%xVarName)
            this%xVarName = trim(xVarName)
        else
            allocate(character(len=len_trim(OBS_DATA_VAR_NAME)) :: this%xVarName)
            this%xVarName = trim(OBS_DATA_VAR_NAME)
        end if

        chanExtent => obs%getMObsExtent()

        call chanExtent%getLocalRange(this%chstart,this%chend,this%nposting_channels)

        obsData => obs%getObsDataVar()

        nobsExtent     => obsData%getExtentNumber(2)
        nobs_global =  nobsExtent%getGlobalCount()

        owners => obs%getObsOwners()

        nobs_local = nobsExtent%getLocalCount()

        allocate(this%obs_sizes(pinfo%getCommSize()))

        do i=1,pinfo%getCommSize()
            if (i-1 == pinfo%getRank()) then
                this%obs_sizes(i) = nobs_local
            end if
            call bcast0d(this%obs_sizes(i),i-1,pinfo%getCommunicator(),'sharing obs local sizes')
        end do

        allocate(this%obs_offsets(pinfo%getCommSize()))
        do i=1,pinfo%getCommSize()
            if (i == 1) then
                this%obs_offsets(i) = 0
            else
                this%obs_offsets(i) = this%obs_offsets(i-1) + this%obs_sizes(i-1)
            end if
        end do

        this%sooData%hmats => LinkedList()

        call debug('Initializing PETSc. Rank: ' // int2str(pinfo%getRank()) // &
            ', nobs_local:  ' // int2str(nobs_local)  // &
            ', nobs_global: ' // int2str(nobs_global))

        !call PetscOptionsSetValue(PETSC_NULL_OPTIONS,'-no_signal_handler','true',&
        !    & ierr)
        call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
        call PetscPopSignalHandler(ierr)

        allocate(d_nnz(nobs_local,this%nposting_channels))
        allocate(o_nnz(nobs_local,this%nposting_channels))

        allocate(minLat(nobs_local,this%nposting_channels))
        allocate(maxLat(nobs_local,this%nposting_channels))
        allocate(minLon(nobs_local,this%nposting_channels))
        allocate(maxLon(nobs_local,this%nposting_channels))

        call debug('Getting number of non-zeroes for the scanned obs op...')

        call this%getNumberOfNonzeros(pinfo, obs, inputGrid, minLat, maxLat, &
            & minLon, maxLon, d_nnz, o_nnz)

        call debug('Got number of non-zeroes for the scanned obs op.')

        obsLociPtr => obs%getObsLoci()
        scLatPtr   => obs%getCraftLatitudes()
        scLonPtr   => obs%getCraftLongitudes()

        nx_global = inputGrid%getGlobalCoordinateDimCount(1)
        ny_global = inputGrid%getGlobalCoordinateDimCount(2)
        call inputGrid%getLocalCoordinateExtentRange(1,xs,xe,nx_local)
        call inputGrid%getLocalCoordinateExtentRange(2,ys,ye,ny_local)

        do chind=1,this%nposting_channels
            reuseMat = .false.
            if (chind > 1) then
                if (this%isSameOperator(obs,chind-1,chind)) then
                    reuseMat = .true.
                end if
            end if
            nullify(hmat_data)
            allocate(hmat_data)

            if (.not. reuseMat) then
                nullify(hMat)
                allocate(hMat)

                maxondiag = 0

                do i=1,size(d_nnz,1)
                    if (d_nnz(i,chind) > maxondiag) then
                        maxondiag = d_nnz(i,chind)
                    end if
                end do

                maxoffdiag = 0

                do i=1,size(d_nnz,1)
                    if (o_nnz(i,chind) > maxoffdiag) then
                        maxoffdiag = o_nnz(i,chind)
                    end if
                end do

                write(msgstr,*) 'Now creating the PETSc matrices... ch ', &
                    & chind+obs%getChannelOffset(), ' max on/off diag: ', &
                    & maxondiag, '/', maxoffdiag
                call debug(msgstr)

                call MatCreate(pinfo%getCommunicator(),hMat,ierr)
                s1_l = nobs_local
                s1_g = nobs_global
                s2_l = nx_local*ny_local
                s2_g = nx_global*ny_global

                call MatSetSizes(hMat, s1_l, s2_l, s1_g, s2_g, ierr)
                if (ierr /= 0) then
                    stop
                end if
                call MatSetType(hMat,MATMPIAIJ,ierr)
                call MatMPIAIJSetPreallocation(hMat, -1, d_nnz(:,chind), &
                                                   & -1, o_nnz(:,chind),ierr)
                call MatSetFromOptions( hMat, ierr )
                call MatSetOption(hMat,MAT_IGNORE_ZERO_ENTRIES,.true., ierr)
            end if

            call MatSetOption(hmat, MAT_NEW_NONZERO_ALLOCATION_ERR, PETSC_TRUE, ierr)
            hmat_data%hmat => hmat
            optr => hmat_data
            call this%sooData%hmats%add(optr)
        end do

        maxnz = this%getMaxStateIndices(inputGrid)

        allocate(xinds(maxnz))
        allocate(fvals(maxnz))

        il = 0

        do iz=0,size(owners)-1
            i = iz + 1

            if (owners(i) /= pinfo%getRank()) then
                cycle
            end if

            il = il + 1
            lat = obsLociPtr(SO_LAT_DIM,i)
            lon = obsLociPtr(SO_LON_DIM,i)

            scanNum = anint(obsLociPtr(SO_SCAN_DIM,i))

            scLat = dble(scLatPtr(scanNum))
            scLon = dble(scLonPtr(scanNum))

            ! get the distance and bearing from the SC to the point
            call geodesic_inverse(scLat,scLon,lat,lon,arclen,angle1,angle2,1)

            call this%firstMatrix()

            ch1 = -1
            rowComputed = .false.

            do chind=1,this%nposting_channels
                hmat => this%nextMatrix()

                if (.not. obs%passesQC(chind+this%chstart-1, i)) then
                    cycle
                end if

                if (chind > 1) then
                    if (this%isSameOperator(obs,chind-1,chind)) then
                        cycle
                    end if
                end if

                call this%getRowValues(pinfo,obs,inputGrid,iz,il,chind,tin,oind,xinds,fvals,&
                    & scLat,scLon,lat,lon,arclen,angle1,minLat(il,chind),maxLat(il,chind), &
                    & minLon(il,chind),maxLon(il,chind),d_nnz_actual,o_nnz_actual)
                rowComputed = .true.
                ch1 = chind

                if (d_nnz_actual > d_nnz(il,chind)) then
                    write(msgstr,*) 'The actual d_nnz was larger than the predicted size', &
                        & pinfo%getRank(),il,chind,d_nnz_actual,d_nnz(il,chind),&
                        & o_nnz_actual,o_nnz(il,chind)
                    call error(msgstr)
                end if

                if (o_nnz_actual > o_nnz(il,chind)) then
                    write(msgstr,*) 'The actual o_nnz was larger than the predicted size', &
                        & pinfo%getRank(),il,chind,o_nnz_actual,o_nnz(il,chind),&
                        & d_nnz_actual,d_nnz(il,chind)
                    call error(msgstr)
                end if

                !print *,'tin:',iz,chind,tin,tin < MIN_GOOD_RATIO,scLat,scLon,lat,lon

                if (tin < MIN_GOOD_RATIO) then
                    !write(msgstr,*) 'The computed row sum was too small:', tin,&
                    !    pinfo%getRank(),il,chind,d_nnz(il,chind),o_nnz(il,chind),lat,lon
                    !call print(msgstr,forceOutput=.true.)

                    rowComputed = .false.
                    call obs%setQCCode(chind, i, QC_UPDT_FWDOP)
                    cycle
                else if (tin > 3.0) then
                    !write(msgstr,*) 'The computed row sum was too large:', tin,&
                    !    pinfo%getRank(),il,chind,d_nnz(il,chind),o_nnz(il,chind),lat,lon
                    !call print(msgstr,forceOutput=.true.)
                    rowComputed = .false.
                    call obs%setQCCode(chind, i, QC_UPDT_FWDOP)
                    cycle
                end if

                izs(1) = il-1 + this%obs_offsets(pinfo%getRank()+1)
                fvals  = fvals/tin

                call MatSetValues(hmat,one,izs,oind,xinds(1:oind),fvals(1:oind),ADD_VALUES,ierr)

                if (mod(il,1000) == 0) then
                    write(msgstr,*) 'Rank',pinfo%getRank(),'chnum:',chind+obs%getChannelOffset(),&
                        & 'completed obs',il,'/',nobs_local
                    call print(msgstr,forceOutput=.true.)
                    !call MatAssemblyBegin(hmat,MAT_FLUSH_ASSEMBLY,ierr)
                end if
            end do
        end do

        call this%firstMatrix()

        call debug('Now assembling the matrix')

        do i=1,this%nposting_channels
            hmat => this%nextMatrix()
            call MatAssemblyBegin(hmat,MAT_FINAL_ASSEMBLY,ierr)
            call MatAssemblyEnd(hmat,MAT_FINAL_ASSEMBLY,ierr)

            chind = i + obs%getChannelOffset()

            call debug('Finished assembling the channel ' // int2str(chind))

!            if (present(outfile)) then
!                reuseMat = .false.
!                if (i > 1) then
!                    if (this%isSameOperator(obs,i-1,i)) then
!                        reuseMat = .true.
!                    end if
!                end if
!
!                if (.not. reuseMat) then
!                    write(filenum,'(I4)') i+1000
!                    filename = trim(outfile)//'_'//filenum(2:4)
!                    call PetscViewerBinaryOpen(PETSC_COMM_WORLD,filename,FILE_MODE_WRITE,viewer,ierr)
!                    call MatView(hmat,viewer,ierr)
!                    call PetscViewerDestroy(viewer,ierr)
!                end if
!            end if

            if (present(columnNormsVar)) then
                call debug('Now working on the column norms for channel ' // int2str(chind))

                ! column norms can only be found globally.
                allocate(colNorms(nx_global*ny_global))

                call MatGetColumnNorms(hmat,NORM_2,colNorms,ierr)

                call debug('Now setting the column norms for channel ' // int2str(chind))

                ind = 0

                call columnNormsVar%getArray(colNormsData)

                do kj=1,ny_global
                    do ki=1,nx_global
                        ind = ind + 1
                        if (ki >= xs .and. ki <= xe) then
                            if (kj >= ys .and. kj <= ye) then
                                colNormsData(chind,ki-xs+1,kj-ys+1) = colNorms(ind)
                            end if
                        end if
                    end do
                end do

                call debug('Finished setting the column norms for channel ' // int2str(chind))

                deallocate(colNorms)
            end if
        end do

!        call VecCreateMPIWithArray(pinfo%getCommunicator(),1,&
!            nx_local*ny_local,nx_global*ny_global,PETSC_NULL_SCALAR,this%sooData%xvec,ierr)
        s1_l = nx_local*ny_local
        s1_g = nx_global*ny_global
        call VecCreateMPI(pinfo%getCommunicator(),s1_l,s1_g,this%sooData%xvec,ierr)
        s1_l = nobs_local
        s1_g = nobs_global
        call VecCreateMPI(pinfo%getCommunicator(),s1_l,s1_g,this%sooData%yvec,ierr)

        deallocate(maxLat)
        deallocate(minLat)
        deallocate(minLon)
        deallocate(maxLon)
    end subroutine

!    subroutine scannedObservationOperatorConstructor_load(this,obs,state_gtb,infile,columnNorms)
!        implicit none
!
!        class(ScannedObservationOperator) :: this
!        class(ScannedObservation), pointer :: obs
!        class(GriddedTbDataSet), pointer :: state_gtb
!        character(*), intent(in) :: infile
!        class(GriddedTbDataSet), optional, pointer :: columnNorms
!
!        integer :: chind, k
!        class(mat_data), pointer :: hmat_data
!        Mat, pointer :: hmat
!        class(*), pointer :: optr
!
!        character(4) :: filenum
!        character(256) :: filename
!        PetscViewer :: viewer
!
!        real(real64) :: info(MAT_INFO_SIZE)
!
!        integer :: ierr, ind, ki, kj, n, ncols, nx, ny, m, rank
!
!        real(real64), dimension(:), pointer :: dptr1d
!
!        real(real64), dimension(:), allocatable :: colNorms
!
!        logical :: reuseMat
!
!        Vec :: good
!
!        this%nposting_channels = obs%getMObs()
!
!        this%nobs = size(obs%obsLoci,2)
!
!        nx = state_gtb%getNX()
!        ny = state_gtb%getNY()
!
!        this%sooData%hmats => LinkedList()
!
!        call PetscInitialize(PETSC_NULL_CHARACTER,ierr)
!        call MPI_Comm_rank(PETSC_COMM_WORLD,rank,ierr)
!
!        !call VecCreateSeq(PETSC_COMM_SELF,nx*ny,this%sooData%xvec,ierr)
!        call VecCreateMPIWithArray(PETSC_COMM_WORLD,1,nx*ny,PETSC_NULL_SCALAR,this%sooData%xvec,ierr)
!        call VecCreateSeq(PETSC_COMM_SELF,this%nobs,this%sooData%yvec,ierr)
!
!        call VecDuplicate(this%sooData%yvec,good,ierr)
!
!        do chind=1,this%nposting_channels
!
!            reuseMat = .false.
!            if (chind > 1) then
!                if (this%isSameOperator(obs,chind-1,chind)) then
!                    reuseMat = .true.
!                end if
!            end if
!
!            nullify(hmat_data)
!            allocate(hmat_data)
!
!            if (.not. reuseMat) then
!                nullify(hMat)
!                allocate(hMat)
!                call MatCreate(PETSC_COMM_WORLD,hMat,ierr)
!                call MatSetType(hMat,MATAIJ,ierr)
!                call MatSetOption(hMat,MAT_IGNORE_ZERO_ENTRIES,.true.,ierr)
!
!                write(filenum,'(I4)') chind+1000
!                filename = trim(infile)//'_'//filenum(2:4)
!
!                write(msgstr,*) 'Loading scannedObservationOperator matrix from file ',trim(filename)
!                call print(msgstr)
!
!                call PetscViewerBinaryOpen(PETSC_COMM_WORLD,filename,FILE_MODE_READ,viewer,ierr)
!                call MatLoad(hmat,viewer,ierr)
!            end if
!
!            call MatGetRowMaxAbs(hmat,good,PETSC_NULL_INTEGER,ierr)
!
!            call VecGetArrayF90(good,dptr1d,ierr)
!
!            do k=1,size(dptr1d)
!                if (abs(dptr1d(k)) < 1d-16) then
!                    call obs%setQCCode(chind, k, QC_UPDT_FWDOP)
!                end if
!            end do
!
!            call VecRestoreArrayF90(good,dptr1d,ierr)
!
!            ind = 0
!
!            if (present(columnNorms)) then
!                if (chind+obs%getChannelOffset() > size(columnNorms%data3d,3)) then
!                    write(msgstr,*) 'Warning: columnNorms data not large enough to store chind:',&
!                        &chind+obs%getUniqueChannelOffset(),size(columnNorms%data3d,3)
!                    call print(msgstr)
!                else
!                    allocate(colNorms(nx*ny))
!
!                    call MatGetColumnNorms(hmat,NORM_2,colNorms,ierr)
!
!                    do kj=1,ny
!                        do ki=1,nx
!                            ind = ind + 1
!                            columnNorms%data3d(ki,kj,chind+obs%getUniqueChannelOffset()) = &
!                                & columnNorms%data3d(ki,kj,chind+obs%getUniqueChannelOffset()) + &
!                                & colNorms(ind)
!                        end do
!                    end do
!
!                    deallocate(colNorms)
!                end if
!            end if
!
!            hmat_data%hmat => hmat
!            optr => hmat_data
!            call this%sooData%hmats%add(optr)
!        end do
!    end subroutine

    subroutine soo_dataDestructor(this)
        implicit none

        type(SOO_data)  :: this

        integer :: ierr

        call this%hmats%deleteAll()
        deallocate(this%hmats)

        call VecDestroy(this%xvec,ierr)
        call VecDestroy(this%yvec,ierr)
    end subroutine

    subroutine mat_dataDestructor(this)
        implicit none

        type(mat_data)  :: this

        if (associated(this%hmat)) then
            deallocate(this%hmat)
        end if
    end subroutine

    subroutine firstMatrix(this)
        implicit none

        class(ScannedObservationOperator) :: this

        call this%sooData%hMats%first()
    end subroutine

    function nextMatrix(this) result(hmat)
        implicit none

        class(ScannedObservationOperator) :: this
        Mat, pointer :: hmat
        class(*), pointer :: f_ptr
        class(mat_data), pointer :: hmat_data

        f_ptr => this%sooData%hmats%currentValue()
        select type(f_ptr)
            type is (mat_data)
                hmat_data => f_ptr
                hmat => hmat_data%hmat
                call this%sooData%hMats%next()
        end select
    end function

    subroutine forward(this, input, obs, output)
        implicit none

        class(ScannedObservationOperator) :: this

        class(DataSet),               pointer :: input
        class(Observation),           pointer :: obs
        real(real64), dimension(:,:), pointer :: output

        Mat,                          pointer :: hmat

        real(real64), dimension(:,:), pointer :: inputData

        class(DataVariable),  pointer :: inputDataVar
        class(DataVariable),  pointer :: obsDataVar

        class(DataExtent), pointer :: chanExtent
        class(DataExtent), pointer :: nobsExtent

        real(real64), pointer :: dptr1d(:)
        integer,      pointer :: qcCodes(:,:)

        class(ScannedObservation), pointer :: obs_so

        integer :: chind, ierr
        integer :: i

        integer :: nobs_local
        integer :: nobs_global

        integer :: choffset

        real(real64) :: mval, minv, maxv

        select type(obs)
            class is (ScannedObservation)
                obs_so => obs
            class default
                call error('The observation type was not ScannedObservation')
        end select

        qcCodes => obs%getQCCodes()

        inputDataVar => input%getVariableByName(this%xVarName)
        obsDataVar   => obs_so%getObsDataVar()
        chanExtent   => obsDataVar%getExtentNumber(1)

        if (.not. associated(chanExtent)) then
            call error('Could not find the dimension ' // CHANS_DIM_NAME)
        end if

!        if (chanExtent%getLocalCount() /= this%nposting_channels) then
!            write(msgstr,*) 'nchannels does not match in scannedObservationOperator',&
!                &chanExtent%getLocalCount(),this%nposting_channels
!            call error(msgstr)
!        end if

        nobsExtent  => obsDataVar%getExtentNumber(2)
        nobs_local  =  nobsExtent%getLocalCount()
        nobs_global =  nobsExtent%getGlobalCount()

        if (nobs_local /= size(output,2)) then
            call error('nobs does not match in scannedObservationOperator ' // &
                int2str(nobs_local) // ' vs. ' // int2str(size(output,2)))
        end if

        if (size(qcCodes,1) /= size(output,1)) then
            call error('nchan does not match in scannedObservationOperator ' // &
                int2str(size(qcCodes,1)) // ' vs. ' // int2str(size(output,1)))
        end if

        call this%firstMatrix()

        call inputDataVar%getArray(inputData)

!        do chind=1,this%nposting_channels
!            minv =  999.
!            maxv = -999.
!            do i=1,size(inputData,2)
!                if (qcCodes(chind,i) <= QC_OBEVAL .and. &
!                    inputData(chind+obs_so%getChannelOffset(),i) > 0) then
!
!                    minv = min(minv,inputData(chind+obs_so%getChannelOffset(),i))
!                    maxv = max(maxv,inputData(chind+obs_so%getChannelOffset(),i))
!                end if
!            end do
!
!            print *,'the min/max for chind',chind,'is ',minv,maxv
!        end do

!        print *,'computing the xvar:',this%xVarName,minval(inputData),maxval(inputData)

        do chind=1,this%nposting_channels
            hmat => this%nextMatrix()

            call VecGetArrayF90(this%sooData%xvec,dptr1d,ierr)
            dptr1d(:) = inputData(chind+obs_so%getChannelOffset(),:)
            call VecRestoreArrayF90(this%sooData%xvec,dptr1d,ierr)

            call MatMult(hMat, this%sooData%xvec, this%sooData%yvec, ierr)

            call VecGetArrayReadF90(this%sooData%yvec,dptr1d,ierr)
            do i=1,size(qcCodes,2)
                 if (qcCodes(chind,i) <= QC_OBEVAL) then
                     output(chind,i) = dptr1d(i)
                 else
                     output(chind,i) = QC_NOVALUE
                 end if
            end do
            call VecRestoreArrayReadF90(this%sooData%yvec,dptr1d,ierr)
        end do

!        minv =  999.
!        maxv = -999.
!
!        do chind=1,this%nposting_channels
!            minv =  999.
!            maxv = -999.
!            do i=1,size(output,2)
!                if (output(chind,i) /= QC_NOVALUE) then
!                    minv = min(minv,output(chind,i))
!                    maxv = max(maxv,output(chind,i))
!                end if
!            end do
!
!            print *,'the min/max output for chind',chind,'is ',minv,maxv
!        end do

    end subroutine

    subroutine tangentLinear(this, baseState, obs, deltaX, deltaY)
        implicit none

        class(ScannedObservationOperator) :: this

        class(DataSet),               pointer :: baseState
        class(Observation),           pointer :: obs
        class(DataSet),               pointer :: deltaX
        real(real64), dimension(:,:), pointer :: deltaY

        ! tlm of a linear model is the same as the forward
        call this%forward(deltaX, obs, deltaY)
    end subroutine

    subroutine adjoint(this, baseState, obs, deltaY, deltaX)
        implicit none

        class(ScannedObservationOperator)    :: this

        class(DataSet),               pointer :: baseState
        class(Observation),           pointer :: obs
        real(real64), dimension(:,:), pointer :: deltaY
        class(DataSet),               pointer :: deltaX

!        class(SatelliteObservation),  pointer :: satObs
!        class(GriddedTbDataSet),      pointer :: state_gtb

        Mat,                          pointer :: hmat

        real(real64), dimension(:,:), pointer :: obsOutData

        real(real64), dimension(:),   pointer :: xptr
        real(real64), dimension(:),   pointer :: yptr

        integer,      pointer :: qcCodes(:,:)

        integer :: chind, ierr
        integer :: i, nobs_local

        class(ScannedObservation), pointer :: obs_so

        class(DataVariable), pointer :: obsDataVar
        class(DataVariable), pointer :: outDataVar

        class(DataExtent), pointer :: chanExtent
        class(DataExtent), pointer :: nobsExtent

        PetscInt :: vsize

        select type(obs)
            class is (ScannedObservation)
                obs_so => obs
            class default
                call error('The observation type was not ScannedObservation')
        end select

        chanExtent => obs%getMObsExtent()

        outDataVar => deltaX%getVariableByName(this%xVarName)

!        if (chanExtent%getLocalCount() /= this%nposting_channels) then
!            write(msgstr,*) 'Error: nchannels does not match in scannedObservationOperator: ', &
!                & chanExtent%getLocalCount(), ' vs. ',this%nposting_channels
!            call error(msgstr)
!        end if

        obsDataVar  => obs%getObsDataVar()
        nobsExtent  => obsDataVar%getExtentNumber(2)
        nobs_local  =  nobsExtent%getLocalCount()
        qcCodes     => obs%getQCCodes()

        if (nobs_local /= size(deltaY,2)) then
            call error('nobs does not match in scannedObservationOperator adjoint')
        end if

        if (size(qcCodes,1) /= size(deltaY,1)) then
            call error('nchan does not match in scannedObservationOperator adjoint ' // &
                int2str(size(qcCodes,1)) // ' vs. ' // int2str(size(deltaY,1)))
        end if

        call outDataVar%getArray(obsOutData)

        call this%firstMatrix()

        call VecGetArrayF90(this%sooData%yvec,yptr,ierr)

        do chind=1,this%nposting_channels
            hmat => this%nextMatrix()

            do i=1,size(qcCodes,2)
                if (qcCodes(chind,i) <= QC_OBEVAL .and. deltaY(chind,i) /= QC_NOVALUE) then
                    yptr(i) = deltaY(chind,i)
                else
                    yptr(i) = 0.d0
                end if
            end do

            call VecGetLocalSize(this%sooData%xvec,vsize,ierr)

            if (vsize /= size(obsOutData,2)) then
                write (msgstr,*) 'Incompatible adjoint sizes ',vsize,' vs. ',size(obsOutData,2)
                call error(msgstr)
            end if

            ! it would be nice if VecPlaceArray could work here, but it segfaults on some systems
            call VecGetArrayF90(this%sooData%xvec,xptr,ierr)
            xptr(1:vsize) = obsOutData(chind+obs_so%getChannelOffset(),1:vsize)
            call MatMultTransposeAdd(hMat, this%sooData%yvec, this%sooData%xvec, this%sooData%xvec, ierr)
            obsOutData(chind+obs_so%getChannelOffset(),1:vsize) = xptr(1:vsize)
            call VecRestoreArrayF90(this%sooData%xvec,xptr,ierr)

            ! call VecPlaceArray(this%sooData%xvec, xptr, ierr)
            ! call MatMultTransposeAdd(hMat, this%sooData%yvec, this%sooData%xvec, this%sooData%xvec, ierr)
            ! call VecResetArray(this%sooData%xvec, ierr)
        end do

        call VecRestoreArrayF90(this%sooData%yvec,yptr,ierr)
    end subroutine
end module
