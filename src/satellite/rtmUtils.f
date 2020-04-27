module rtmUtils_mod
    use iso_fortran_env

    use linkedList_mod

    use atmos3dDataSet_mod

    use observation_mod
    use observationBundle_mod

    use scannedObservation_mod
    use scannedObservationBundle_mod
    use scannedObservationOperator_mod

    use satelliteObservation_mod
    use satelliteObservationOperator_mod
    use satellitePlatformInfo_mod

    use dataSet_mod
    use dataVariable_mod
    use dataDimension_mod
    use dataExtent_mod
    use dataArrayWriter_mod

    use netcdfDataArrayWriter_mod

    use parallelInfo_mod
    use parallelConstants_mod
    use mpiUtils_mod

    implicit none

    private

    public :: doRtmForward
    public :: doRtmAdjointMatrix
    public :: doTlmKMatrix
    public :: sweepAntennaPattern
    public :: getOrbitInfo

    contains

    subroutine doRtmForward(pinfo,platform,state,rtmObsOp,obs,startx,endx,starty,endy,&
        & xBatchStride)

        implicit none

        class(ParallelInfo),                 pointer :: pinfo
        class(SatellitePlatformInfo),        pointer :: platform
        class(Atmos3DDataSet),               pointer :: state
        class(SatelliteObservationOperator), pointer :: rtmObsOp
        class(SatelliteObservation),         pointer :: obs
        integer,          intent(in), optional       :: startx,endx,starty,endy,xBatchStride

        class(SatelliteObservation), pointer :: obsSubset

        integer :: startxval, endxval, startyval, endyval, xstrideval
        integer :: batch, numBatch, ind, x, xl, y, allocStat, profile
        integer :: xs_batch, xe_batch, nx, nxl, ny

        real(real32), dimension(:,:,:), pointer :: tb
        real(real64), dimension(:,:),   pointer :: obsData
        real(real64), dimension(:,:),   pointer :: lat, lon
        real(real64), dimension(:,:),   pointer :: obsDataLocal
        real(real64), dimension(:,:),   pointer :: obsLociLocal
        real(real64), dimension(:,:),   pointer :: latLocal, lonLocal

        class(DataExtent),    pointer :: xExtent, yExtent, chanExtent
        class(DataDimension), pointer :: pixDim, scanDim
        class(DataVariable),  pointer :: qvVar, tbVar

        class(ParallelInfo), pointer :: pinfo_local

        integer :: nchan

        qvVar => state%getVariableByName(QVAPOR_VAR)
        xExtent => state%getWestEastExtent()
        yExtent => state%getSouthNorthExtent()

        if (present(startx)) then
            startxval = startx
        else
            startxval = xExtent%getLocalTotalStart()
        end if

        if (present(endx)) then
            endxval = endx
        else
            endxval = xExtent%getLocalTotalEnd()
        end if

        if (present(starty)) then
            startyval = starty
        else
            startyval = yExtent%getLocalTotalStart()
        end if

        if (present(endy)) then
            endyval = endy
        else
            endyval = yExtent%getLocalTotalEnd()
        end if

        if (present(xBatchStride)) then
            xstrideval = xBatchStride
        else
            xstrideval = 4
        end if

        lon => state%getVariable2D(A3D_LON_VAR)
        lat => state%getVariable2D(A3D_LAT_VAR)

        if (startxval > endxval .or. startyval > endyval .or. &
            startxval < xExtent%getLocalTotalStart() .or. &
            startxval > xExtent%getLocalTotalEnd()   .or. &
            startyval < yExtent%getLocalTotalStart() .or. &
            startyval > yExtent%getLocalTotalEnd()   .or. &
            endxval   > xExtent%getLocalTotalEnd()   .or. &
            endyval   > yExtent%getLocalTotalEnd()) then

            write(msgstr,*) 'The (startx,endx) or (starty,endy) of (',startxval,',',endxval,'),', &
                '(',startyval,',',endyval,') was ','invalid - x: (',&
                xExtent%getLocalTotalStart(),',',xExtent%getLocalTotalEnd(),'), y: (',&
                yExtent%getLocalTotalStart(),',',yExtent%getLocalTotalEnd(),')'

            call error(msgstr)
        end if

        nx = endxval-startxval+1
        ny = endyval-startyval+1

        chanExtent => obs%getChannelExtent()
        nchan   = chanExtent%getLocalCount()

        allocate(pinfo_local)
        call pinfo_local%parallelInfoConstructor(LOCAL_PARALLEL_TYPE)

        tbVar => obs%getTbVar()
        call tbVar%getArray(tb)

        obsData => obs%getObsData()

        numBatch = nx/xstrideval

        if (numBatch*xstrideval /= nx) then
            numBatch = numBatch + 1
        end if

        do batch=1,numBatch
            xs_batch = ((batch-1)*xstrideval) + 1
            xe_batch = min(batch*xstrideval,nx)
            nxl = xe_batch - xs_batch  + 1

            allocate(obsSubset)
            call obsSubset%satelliteObservationConstructor(platform)

            pixDim  => obsSubset%addDimensionByName(PIXELS_DIM_NAME,nxl)
            scanDim => obsSubset%addDimensionByName(SCANS_DIM_NAME, ny)

            allocate(latLocal(nxl,ny))
            allocate(lonLocal(nxl,ny))

            ! it isn't really necessary to copy here, can just use pointers
            do y=1,ny
                do xl=1,nxl
                    x = xs_batch+xl-1
                    latLocal(xl,y) = lat(x,y)
                    lonLocal(xl,y) = lon(x,y)
                end do
            end do

            call obsSubset%loadSatelliteObservation(pinfo,pixDim,scanDim,&
                latLocal,lonLocal)

            deallocate(latLocal,lonLocal)

            obsLociLocal => obsSubset%getObsLoci()

            profile = 1

            do y=1,ny
                do xl=1,nxl
                    x = xs_batch+xl-1
                    obsLociLocal(SO_PIX_DIM,profile) = x + startxval - 1
                    obsLociLocal(SO_SCAN_DIM,profile) = y + startyval - 1
                    profile = profile + 1
                end do
            end do

            call rtmObsOp%doForward(state, obsSubset, obsSubset%getObsData())

            ! copy the observation data from the obsSubset to the observation
            obsDataLocal => obsSubset%getObsData()

            profile = 0

            do y=1,ny
                do xl=1,nxl
                    profile = profile + 1
                    x = xs_batch+xl-1
                    ind = (y-1)*nx + x
                    obsData(:,ind) = obsDataLocal(:,profile)
                    tb(:,x,y)      = obsDataLocal(:,profile)
                end do
            end do

            deallocate(obsSubset)
        end do
    end subroutine

    subroutine doRtmAdjointMatrix(state_ptr, state_adj_ptr, rtmObsOp, x_int, y_int, time, &
            &outputPrefix, platform, k_matrix_in)

        implicit none

        class(Atmos3DDataSet), pointer :: state_ptr, state_adj_ptr
        class(SatelliteObservationOperator), pointer :: rtmObsOp
        integer, intent(in) :: x_int, y_int
        integer, intent(in) :: time
        character(len=*), intent(in) :: outputPrefix
        class(SatellitePlatformInfo), pointer :: platform
        real(real64), dimension(:,:), optional, target, intent(out) :: k_matrix_in

        character(len=256) :: key
        real(real64), dimension(:), pointer :: dptr, optr, dptr2
        real(real64) :: u10column, v10column, totalCwm, minDiff, diffp, minDiffI, minDiffJ
        integer :: i, j, z, nz, nz_all
        class(SatelliteObservation), pointer :: obsData, perturb

        real(real64), dimension(:), allocatable :: profile
        real(real64), dimension(:), allocatable :: bt_profile
        real(real64), dimension(:,:), pointer   :: k_matrix

        real(real64), dimension(:), pointer :: obsx, obsy

        real(real64) :: x, y

        class(ParallelInfo),  pointer :: pinfo

        class(DataVariable),  pointer :: tbVar      => null()
        class(DataVariable),  pointer :: latVar     => null()
        class(DataVariable),  pointer :: lonVar     => null()

        class(DataExtent),    pointer :: zExtent
        class(DataDimension), pointer :: pixDim
        class(DataDimension), pointer :: scanDim

        real(real32), pointer :: latptr(:,:)
        real(real32), pointer :: lonptr(:,:)

        real(real64), pointer :: obsLoci(:,:)

        allocate(pinfo)
        call pinfo%parallelInfoConstructor(LOCAL_PARALLEL_TYPE)

        ! only makes sense to run this single column test on one processor
        if (pinfo%getRank() == 0) then
            x = dble(x_int)
            y = dble(y_int)

            allocate(obsx(1),obsy(1))
            allocate(obsData)
            allocate(latptr(1,1))
            allocate(lonptr(1,1))

            call obsData%satelliteObservationConstructor(platform)

            pixDim   => obsData%addDimensionByName(PIXELS_DIM_NAME,1)
            scanDim  => obsData%addDimensionByName(SCANS_DIM_NAME,1)

            latptr(1,1) = state_ptr%getValue2D(A3D_LAT_VAR,x,y)
            lonptr(1,1) = state_ptr%getValue2D(A3D_LON_VAR,x,y)

            call obsData%loadSatelliteObservation(pinfo,pixDim,scanDim, &
                & latptr, lonptr)

            allocate(perturb)
            call perturb%satelliteObservationConstructor(platform)
            call perturb%loadSatelliteObservation(pinfo,pixDim,scanDim, &
                & latptr, lonptr)

            zExtent => state_ptr%getBottomTopExtent()

            nz = zExtent%getLocalCount()
            !nz_all = 7*nz+3
            nz_all = 9*nz+3

            allocate(profile(nz_all))
            allocate(dptr(nz))
            allocate(dptr2(nz+1))

            call state_ptr%getColumn(P_LEVEL_VAR,x,y,.false.,dptr2)
            ! logarithmic average
            profile(0*nz+1:1*nz) = exp((log(dptr2(2:nz+1))+log(dptr2(1:nz)))/2.)
            call state_ptr%getColumn(T_VAR,x,y,.false.,dptr)
            profile(1*nz+1:2*nz) = dptr
            call state_ptr%getColumn(QVAPOR_VAR,x,y,.false.,dptr)
            profile(2*nz+1:3*nz) = dptr
            call state_ptr%getColumn(QCLOUD_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            where(dptr .lt. 1d-4) dptr = 1d-4
            profile(3*nz+1:4*nz) = dptr
            call state_ptr%setColumn(QCLOUD_VAR,x_int,y_int,.false.,dptr)
            call state_ptr%getColumn(QICE_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            where(dptr .lt. 1d-4) dptr = 1d-4
            profile(4*nz+1:5*nz) = dptr
            call state_ptr%setColumn(QICE_VAR,x_int,y_int,.false.,dptr)
            call state_ptr%getColumn(QRAIN_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            where(dptr .lt. 1d-4) dptr = 1d-4
            profile(5*nz+1:6*nz) = dptr
            call state_ptr%setColumn(QRAIN_VAR,x_int,y_int,.false.,dptr)
            call state_ptr%getColumn(QSNOW_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            where(dptr .lt. 1d-4) dptr = 1d-4
            profile(6*nz+1:7*nz) = dptr
            call state_ptr%setColumn(QSNOW_VAR,x_int,y_int,.false.,dptr)
            !call state_ptr%getColumn(QGRAUP_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            !where(dptr .lt. 1d-4) dptr = 1d-4
            !profile(7*nz+1:8*nz) = dptr
            !call state_ptr%setColumn(QGRAUP_VAR,x_int,y_int,.false.,dptr)
            !call state_ptr%getColumn(QHAIL_VAR,x,y,.false.,dptr)
            ! add some cloud in so the adjoint gets hit
            !where(dptr .lt. 1d-4) dptr = 1d-4
            !profile(8*nz+1:9*nz) = dptr
            !call state_ptr%setColumn(QHAIL_VAR,x_int,y_int,.false.,dptr)
            profile(7*nz+1) = state_ptr%getValue2D(U10_VAR,x,y)
            profile(7*nz+2) = state_ptr%getValue2D(V10_VAR,x,y)
            profile(7*nz+3) = state_ptr%getValue2D(T_SURF_VAR,x,y)

            if (present(k_matrix_in)) then
                k_matrix => k_matrix_in
            else
                allocate(k_matrix(nz_all,platform%getNChannels()))
                k_matrix = 0.0d0
            end if

            obsLoci => obsData%getObsLoci()

            obsLoci(SO_PIX_DIM,1) = x
            obsLoci(SO_SCAN_DIM,1) = y
            optr => perturb%getObservation(1)

            optr = 0.d0

            do z=1,size(k_matrix,2)
                call state_adj_ptr%zeroAll()
                optr(z) = 1.0d0

                call rtmObsOp%doAdjoint(state_ptr,obsData,perturb%getObsData(),state_adj_ptr)

                optr(z) = 0.0d0
                k_matrix(0*nz+1:2*nz,z) = 0.d0
                call state_adj_ptr%getColumn(T_VAR,      x,y,.false.,dptr)
                k_matrix(1*nz+1:2*nz,z) = dptr
                call state_adj_ptr%getColumn(QVAPOR_VAR, x,y,.false.,dptr)
                k_matrix(2*nz+1:3*nz,z) = dptr
                call state_adj_ptr%getColumn(QCLOUD_VAR, x,y,.false.,dptr)
                k_matrix(3*nz+1:4*nz,z) = dptr
                call state_adj_ptr%getColumn(QICE_VAR,   x,y,.false.,dptr)
                k_matrix(4*nz+1:5*nz,z) = dptr
                call state_adj_ptr%getColumn(QRAIN_VAR,  x,y,.false.,dptr)
                k_matrix(5*nz+1:6*nz,z) = dptr
                call state_adj_ptr%getColumn(QSNOW_VAR,  x,y,.false.,dptr)
                k_matrix(6*nz+1:7*nz,z) = dptr
                !call state_adj_ptr%getColumn(QGRAUP_VAR,x,y,.false.,dptr)
                !k_matrix(7*nz+1:8*nz,z) = dptr
                !call state_adj_ptr%getColumn(QHAIL_VAR,x,y,.false.,dptr)
                !k_matrix(8*nz+1:9*nz,z) = dptr
                k_matrix(7*nz+1,z) = state_adj_ptr%getValue2D(U10_VAR,x,y)
                k_matrix(7*nz+2,z) = state_adj_ptr%getValue2D(V10_VAR,x,y)
                k_matrix(7*nz+3,z) = state_adj_ptr%getValue2D(T_SURF_VAR,x,y)
            end do

            if (.not. present(k_matrix_in)) then
               open (unit=42,file=trim(outputPrefix) // "_k_matrix_adj.txt",action="write",status="replace")
               do i=1,nz_all
                   write(42,*) (k_matrix(i,j),j=1,size(k_matrix,2))
               end do
               close(unit=42)
            end if

            !open (unit=42,file=trim(outputPrefix) // "_profile.txt",action="write",status="replace")
            !do i=1,nz_all
            !    write(42,*) profile(i)
            !end do
            !close(unit=42)

            if (.not. present(k_matrix_in)) then
                deallocate(k_matrix)
            end if

            deallocate(pinfo)
            deallocate(obsx,obsy)
            deallocate(obsData)
            deallocate(latptr)
            deallocate(lonptr)
            deallocate(pixDim)
            deallocate(scanDim)
            deallocate(perturb)
            deallocate(profile)
            deallocate(dptr)
            deallocate(dptr2)
        end if
    end subroutine

    subroutine doTlmKMatrix(state_ptr, perturb_ptr, rtmObsOp, x_int, y_int, time, &
            &outputPrefix, platform)

        implicit none

        class(Atmos3DDataSet), pointer :: state_ptr, perturb_ptr
        integer, intent(in) :: x_int, y_int
        integer, intent(in) :: time
        character(len=*), intent(in) :: outputPrefix
        class(SatellitePlatformInfo), pointer :: platform

        class(SatelliteObservationOperator),  pointer :: rtmObsOp
        class(SatelliteObservation),          pointer :: obsdata, obsdata_tl

        real(real64), dimension(:),   allocatable :: tl_profile
        real(real64), dimension(:),   allocatable :: bt_profile
        real(real64), dimension(:,:), allocatable :: k_matrix

        real(real64), dimension(:), pointer   :: dptr, optr, dptr2

        real(real64), dimension(:), pointer :: obsx, obsy

        real(real64), pointer :: obsLoci(:,:)

        integer :: i, j, z, nz, nz_all, mobs

        real(real64) :: x, y

        class(DataExtent),    pointer :: zExtent
        class(DataDimension), pointer :: pixDim
        class(DataDimension), pointer :: scanDim

        class(ParallelInfo),  pointer :: pinfo

        real(real32), pointer :: latptr(:,:)
        real(real32), pointer :: lonptr(:,:)

        allocate(pinfo)
        call pinfo%parallelInfoConstructor(LOCAL_PARALLEL_TYPE)

        ! it currently only makes sense to run this single column utility on one processor
        if (pinfo%getRank() == 0) then

            x = real(x_int)
            y = real(y_int)

            allocate(obsx(1),obsy(1))

            allocate(obsData)
            allocate(obsData_tl)

            allocate(latptr(1,1))
            allocate(lonptr(1,1))

            ! temporary, single column sat obs objects
            call    obsData%satelliteObservationConstructor(platform)
            pixDim   => obsData%addDimensionByName(PIXELS_DIM_NAME,1)
            scanDim  => obsData%addDimensionByName(SCANS_DIM_NAME,1)

            latptr(1,1) = state_ptr%getValue2D(A3D_LAT_VAR,x,y)
            lonptr(1,1) = state_ptr%getValue2D(A3D_LON_VAR,x,y)

            call obsData%loadSatelliteObservation(pinfo,pixDim,scanDim, &
                & latptr, lonptr)

            call obsData_tl%satelliteObservationConstructor(platform)

            call obsData_tl%loadSatelliteObservation(pinfo,pixDim,scanDim, &
                & latptr, lonptr)

            zExtent => state_ptr%getBottomTopExtent()

            nz = zExtent%getLocalCount()
            nz_all = 9*nz+3

            allocate(dptr(nz))
            allocate(dptr2(nz+1))

            allocate(tl_profile(nz_all))
            call state_ptr%getColumn(P_LEVEL_VAR,x,y,.false.,dptr2)
            ! logarithmic average
            tl_profile(0*nz+1:1*nz) = exp((log(dptr2(2:nz+1))+log(dptr2(1:nz)))/2.)

            call state_ptr%getColumn(T_VAR,x,y,.false.,dptr)
            tl_profile(1*nz+1:2*nz) = dptr

            call state_ptr%getColumn(QVAPOR_VAR,x,y,.false.,dptr)
            tl_profile(2*nz+1:3*nz) = dptr

            call state_ptr%getColumn(QCLOUD_VAR,x,y,.false.,dptr)
            ! add in some qcloud so the adjoint gets invoked
            where(dptr .lt. 1d-4) dptr = 1d-4
            call state_ptr%setColumn(QCLOUD_VAR,x_int,y_int,.false.,dptr)
            tl_profile(3*nz+1:4*nz) = dptr

            call state_ptr%getColumn(QICE_VAR,x,y,.false.,dptr)
            ! add in some qice so the adjoint gets invoked
            where(dptr .lt. 1d-4) dptr = 1d-4
            call state_ptr%setColumn(QICE_VAR,x_int,y_int,.false.,dptr)
            tl_profile(4*nz+1:5*nz) = dptr

            call state_ptr%getColumn(QRAIN_VAR,x,y,.false.,dptr)
            ! add in some qrain so the adjoint gets invoked
            where(dptr .lt. 1d-4) dptr = 1d-4
            call state_ptr%setColumn(QRAIN_VAR,x_int,y_int,.false.,dptr)
            tl_profile(5*nz+1:6*nz) = dptr

            call state_ptr%getColumn(QSNOW_VAR,x,y,.false.,dptr)
            where(dptr .lt. 1d-4) dptr = 1d-4
            ! add in some qsnow so the adjoint gets invoked
            call state_ptr%setColumn(QSNOW_VAR,x_int,y_int,.false.,dptr)
            tl_profile(6*nz+1:7*nz) = dptr

            !call state_ptr%getColumn(QGRAUP_VAR,x,y,.false.,dptr)
            !where(dptr .lt. 1d-4) dptr = 1d-4
            ! add in some qgraup so the adjoint gets invoked
            !call state_ptr%setColumn(QGRAUP_VAR,x_int,y_int,.false.,dptr)
            !tl_profile(7*nz+1:8*nz) = dptr

            !call state_ptr%getColumn(QHAIL_VAR,x,y,.false.,dptr)
            ! add in some qhail so the adjoint gets invoked
            !where(dptr .lt. 1d-4) dptr = 1d-4
            !call state_ptr%setColumn(QHAIL_VAR,x_int,y_int,.false.,dptr)
            !tl_profile(8*nz+1:9*nz) = dptr

            tl_profile(9*nz+1) = state_ptr%getValue2D(U10_VAR,x,y)
            tl_profile(9*nz+2) = state_ptr%getValue2D(V10_VAR,x,y)
            tl_profile(9*nz+3) = state_ptr%getValue2D(T_SURF_VAR,x,y)

            obsLoci => obsData%getObsLoci()

            obsLoci(SO_PIX_DIM,1) = x
            obsLoci(SO_SCAN_DIM,1) = y

            call perturb_ptr%zeroAll()

            dptr = 0.0d0

            do z=1,nz
                if (z .eq. 1) then
                    allocate(k_matrix(nz_all,mobs))
                    k_matrix = 0.0d0
                end if

                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(T_VAR,x_int,y_int,.false.,dptr)

                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(1*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(T_VAR,x_int,y_int,.false.,dptr)
            end do

            do z=1,nz
                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(QVAPOR_VAR,x_int,y_int,.false.,dptr)
                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(2*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(QVAPOR_VAR,x_int,y_int,.false.,dptr)
            end do

            do z=1,nz
                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(QCLOUD_VAR,x_int,y_int,.false.,dptr)
                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(3*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(QCLOUD_VAR,x_int,y_int,.false.,dptr)
            end do

            do z=1,nz
                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(QICE_VAR,x_int,y_int,.false.,dptr)
                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(4*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(QICE_VAR,x_int,y_int,.false.,dptr)
            end do

            do z=1,nz
                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(QRAIN_VAR,x_int,y_int,.false.,dptr)
                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(5*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(QRAIN_VAR,x_int,y_int,.false.,dptr)
            end do

            do z=1,nz
                dptr(z) = 1.0d0
                call perturb_ptr%setColumn(QSNOW_VAR,x_int,y_int,.false.,dptr)
                call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
                optr => obsData_tl%getObservation(1)
                k_matrix(6*nz+z,:) = optr(:)
                dptr = 0.0d0
                call perturb_ptr%setColumn(QSNOW_VAR,x_int,y_int,.false.,dptr)
            end do

            !do z=1,nz
            !    dptr(z) = 1.0d0
            !    call perturb_ptr%setColumn(QGRAUP_VAR,x_int,y_int,.false.,dptr)
            !    call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
            !    optr => obsData_tl%getObservation(1)
            !    k_matrix(7*nz+z,:) = optr(:)
            !    dptr = 0.0d0
            !    call perturb_ptr%setColumn(QGRAUP_VAR,x_int,y_int,.false.,dptr)
            !end do

            !do z=1,nz
            !    dptr(z) = 1.0d0
            !    call perturb_ptr%setColumn(QHAIL_VAR,x_int,y_int,.false.,dptr)
            !    call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
            !    optr => obsData_tl%getObservation(1)
            !    k_matrix(8*nz+z,:) = optr(:)
            !    dptr = 0.0d0
            !    call perturb_ptr%setColumn(QHAIL_VAR,x_int,y_int,.false.,dptr)
            !end do

            dptr(1) = 1.0d0
            call perturb_ptr%setColumn(U10_VAR,x_int,y_int,.false.,dptr)
            call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
            optr => obsData_tl%getObservation(1)
            k_matrix(9*nz+1,:) = optr(:)
            dptr(1) = 0.0d0
            call perturb_ptr%setColumn(U10_VAR,x_int,y_int,.false.,dptr)

            dptr(1) = 1.0d0
            call perturb_ptr%setColumn(V10_VAR,x_int,y_int,.false.,dptr)
            call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
            optr => obsData_tl%getObservation(1)
            k_matrix(9*nz+2,:) = optr(:)
            dptr(1) = 0.0d0
            call perturb_ptr%setColumn(V10_VAR,x_int,y_int,.false.,dptr)

            dptr(1) = 1.0d0
            call perturb_ptr%setColumn(T_SURF_VAR,x_int,y_int,.false.,dptr)
            call rtmObsOp%doTLM(state_ptr, obsData, perturb_ptr, obsData_tl%getObsData())
            optr => obsData_tl%getObservation(1)
            k_matrix(9*nz+3,:) = optr(:)
            dptr(1) = 0.0d0
            call perturb_ptr%setColumn(T_SURF_VAR,x_int,y_int,.false.,dptr)

            optr => obsData%getObservation(1)
            allocate(bt_profile(mobs))
            bt_profile(:) = optr(:)

            open (unit=42,file=trim(outputPrefix) // "_k_matrix_tl.txt",action="write",status="replace")
            do i=1,nz_all
                write(42,*) (k_matrix(i,j),j=1,mobs)
            end do
            close(unit=42)

            open (unit=42,file=trim(outputPrefix) // "_profile.txt",action="write",status="replace")
            do i=1,nz_all
                write(42,*) tl_profile(i)
            end do
            close(unit=42)

            open (unit=42,file=trim(outputPrefix) // "_bt_profile.txt",action="write",status="replace")
            do i=1,mobs
                write(42,*) bt_profile(i)
            end do
            close(unit=42)

            deallocate(pinfo)
            deallocate(obsx)
            deallocate(obsy)
            deallocate(obsData)
            deallocate(obsData_tl)
            deallocate(latptr)
            deallocate(lonptr)
            deallocate(pixDim)
            deallocate(scanDim)
            deallocate(dptr)
            deallocate(dptr2)
            deallocate(tl_profile)
            deallocate(k_matrix)
            deallocate(bt_profile)
        end if
    end subroutine

    subroutine sweepAntennaPattern(pinfo,tbDataset,scannedObsBundleIn,scannedObsBundleOut,&
        outputFileNamePrefix,comm)

        implicit none

        class(ParallelInfo),                pointer    :: pinfo
        class(DataSet),                     pointer    :: tbDataset
        class(ScannedObservationBundle),    pointer    :: scannedObsBundleIn
        class(ScannedObservationBundle),    pointer    :: scannedObsBundleOut
        character(len=*), optional,         intent(in) :: outputFileNamePrefix
        integer,          optional,         intent(in) :: comm

        integer :: chnum, i, ind, obsi, obsj

        class(ObservationBundle),           pointer     :: obsBundle    => NULL()

        class(Observation),                 pointer     :: obs          => NULL()
        class(ScannedObservation),          pointer     :: obs_so       => NULL()
        class(ScannedObservationOperator),  pointer     :: obsOpConv    => NULL()

        real(real64), dimension(:,:),       pointer     :: output       => NULL()
        real(real64), dimension(:),         allocatable :: obsErr

        real(real32), dimension(:,:,:),     pointer     :: obsTb        => NULL()

        character(len=4)    :: filenum
        character(len=256)  :: filename

        real(real64), dimension(:,:), pointer :: obsLoci, obsData

        class(DataExtent), pointer :: mObsExtent
        class(DataExtent), pointer :: nObsExtent

        class(DataArrayWriter),       pointer :: writer
        class(NetcdfDataArrayWriter), pointer :: ncWriter

        do i=1,scannedObsBundleIn%getBundleSize()
            obs_so    => scannedObsBundleIn%getScannedObservation(i)
            obs       => obs_so
            obsOpConv => scannedObsBundleIn%getAntennaPatternObsOp(i)

            mObsExtent => obs%getMObsExtent()
            nObsExtent => obs%getNObsExtent()

            allocate(output(mObsExtent%getLocalCount(),nObsExtent%getLocalCount()))
            call obsOpConv%forward(tbDataset, obs, output)

            obsErr = obs_so%getObservationError()

            ! switch the pointer to the output bundle (which could be the same pointer as the input)
            obs_so => scannedObsBundleOut%getScannedObservation(i)

            obsTb => obs_so%getBrightnessTemps()

            obsLoci => obs%getObsLoci()
            obsData => obs_so%getObsData()

            ! copy the swept data into the output observation
            do ind=1,nobsExtent%getLocalCount()
                obsi = anint(obsLoci(SO_PIX_DIM,ind))
                obsj = anint(obsLoci(SO_SCAN_DIM,ind))
                obsTb(:,obsi,obsj) = output(:,ind)
                obsData(:,ind)     = output(:,ind)
            end do

            if (present(outputFileNamePrefix) .and. size(obsData,1) /= 0) then
                write(filenum,'(I4)') i+1000
                fileName = trim(outputFileNamePrefix) //filenum(2:4)// '.nc'
                allocate(ncWriter)
                call ncWriter%netcdfDataArrayWriterConstructor(fileName)
                writer => ncWriter

                call obs_so%writeObsToFile(pinfo,writer)
            end if

            deallocate(output)
            nullify(output)
        end do
    end subroutine

    subroutine getOrbitInfo()
        implicit none

    end subroutine
end module
