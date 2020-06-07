program compareIdeal1DVar

    use assimilationStrategy_mod
    use assimilationProblem_mod
    use observer_mod
    use observation_mod
    use observationOperator_mod
    use satelliteObservation_mod
    use ccvObservation_mod
    use satelliteObservationOperator_mod
    use satellitePlatformInfo_mod
    use abstractVectorFactory_mod
    use abstractVectorOperator_mod
    use abstractVector_mod
    use dataSetVectorConverter_mod
    use atmos3DDataSet_mod
    use radianceCcv_mod
    use dataSet_mod
    use rtmUtils_mod
    use rtmOptions_mod
    use dataSetFactory_mod
    use radianceAssimilationFactory_mod
    use assimStrategyFactory_mod
    use fullMatrixOperator_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    integer, parameter            :: PLATFORM_TO_TEST = PLATFORM_TRMM_TMI
    character(len=256), parameter :: IDEAL_OBS_OP = RTTOV_MW_OBS_OP
    character(len=256), parameter :: ASSIM_OBS_OP = CRTM_OBS_OP

    ! the CCV observation type, 1 = linear, 2 = non-linear
    integer, parameter :: hType = 2
    ! for the CCV observation type, should we use global or regional statistics?
    logical, parameter :: useGlobal = .false.
    ! the number of CCV platforms to initialize
    integer, parameter :: nplatforms = 1
    ! the configuration path for the CCVs
    character(len=1024), parameter :: ccvConfigPath = '/Users/steward/research/workspace_photran/radiancesDA/radianceCcvConfig/'
    character(len=1024), parameter :: ccvCoeffsPath = '/Users/steward/research/workspace_photran/radiancesDA/radianceCcvCoeffs/'

    character(len=1024) :: inputFile

    class(AssimilationStrategy), pointer :: strategy
    class(DataSet), pointer :: state
    class(DataSet), pointer :: background
    class(Atmos3DDataSet), pointer :: state_a3d
    class(AbstractVectorOperator), pointer :: BHalf
    class(AbstractVectorOperator), pointer :: rInvHalf
    class(FullMatrixOperator), pointer :: BHalf_full
    class(FullMatrixOperator), pointer :: rInvHalf_full
    class(SatelliteObservationOperator), pointer :: obsOpIn, obsOpOut
    class(CcvObservation), pointer :: ccvObs
    class(SatelliteObservation),  pointer :: obsData
    class(SatellitePlatformInfo), pointer :: idealPlatform, assimPlatform, ccvPlatform
    class(Observation), pointer :: obs
    class(ObservationOperator), pointer :: obsOp

    class(Observer),   pointer :: observer_ptr
    class(DataSetVectorConverter), pointer :: converter

    class(AssimilationProblem), pointer :: problem

    logical :: alphaTest = .true.

    ! number of control variables per column, here the number of principal components
    integer :: nctrl = 40
    ! maximum number of iterations per 1DVar retrieval
    integer :: maxiter = 1000

    integer :: time = 1

    integer, parameter :: nz = 60

    integer, parameter :: mobs = 9

    integer, parameter :: nfields=6
    ! choose some default fields
    character(len=256), dimension(nfields), target :: requestedfields = (/ &
            UV10_NAME, &
            P_SFC_NAME, &
            T_NAME,  &
            QVAPOR_NAME, &
            W_NAME, &
            CWM_NAME/)
!    character(len=256), dimension(nfields), target :: requestedfields = (/ &
!            UV_NAME, &
!            T_NAME,  &
!            QVAPOR_NAME, &
!            W_NAME, &
!            P_SFC_NAME, &
!            QCLOUD_NAME, &
!            QRAIN_NAME, &
!            QICE_NAME, &
!            QSNOW_NAME, &
!            QGRAUP_NAME, &
!            QHAIL_NAME/)

    integer, dimension(nfields), target :: fieldNz = (/ &
            1,  & ! UV10
            1,  & ! PSFC
            nz, & ! T
            nz, & ! QVAPOR
            nz+1, & ! W
            nz/)   ! CWM
!    integer, dimension(nfields), target :: fieldNz = (/ &
!            nz,  & ! UV
!            nz, & ! T
!            nz, & ! QVAPOR
!            nz+1, & ! W
!               1, & ! P_SFC
!              nz, & ! QCLOUD
!              nz, & ! QRAIN
!              nz, & ! QICE
!              nz, & ! QSNOW
!              nz, & ! QGRAUP
!              nz/)  ! QHAIL

    class(RtmOptions), pointer :: rtmOpts

    character(len=1024) :: b_half_file_name = 'BHalf.txt'
    character(len=1024) :: rInvHalf_file_name = 'RInvHalf.txt'

    real(8), dimension(:), pointer :: xmean
    real(8), dimension(:), pointer :: initGuess

    real(8), dimension(:,:), pointer :: data2d

    integer :: nzfile,nctrl_file,mobs_file

    integer :: nz_total, i, j, startx, endx, starty, endy, xstride

    inputFile = '/Users/steward/research/satellite/projects/20160320_jacobianForBjorn/wrfout_d03_2005-08-01_18:30:00'

    nz_total = sum(fieldNz)

    startx  = 1
    endx    = 2
    starty  = 1
    endy    = 2
    xstride = 1

    if (IDEAL_OBS_OP .eq. CCV_OBS_OP .or. ASSIM_OBS_OP .eq. CCV_OBS_OP) then
        call radCcvManager%initialize(nplatforms,ccvConfigPath,ccvCoeffsPath,1,6)
    end if

    ! strategy   => getAssimilationStrategy(ONE_D_VAR_STRATEGY,requestedFields,nfields,fieldNz,opt)
    strategy   => getAssimilationStrategy(NDVAR_STRATEGY) !,requestedFields,nfields,fieldNz,opt)
    state      => getDataSet(WRF_ARW_MODEL,inputFile,time,.true.,requestedFields,fieldNz)
    background => getDataSet(WRF_ARW_MODEL,inputFile,time,.true.,requestedFields,fieldNz)

    select type(state)
        class is (Atmos3DDataSet)
            state_a3d => state
        class default
            write(msgstr,*) 'Incompatible model class in compareIdeal1DVar'
            call error(msgstr)
    end select

    ! read in the mean
    allocate(xmean(243))
    open(unit=42,file='xmean.txt')
    read(42,*) xmean(:)
    close(42)

!    print *,'Background before mean:',background%data(:,1,1)
!
!    ! throw away the background and use just a mean field
!    do i=1,background%nx
!        do j=1,background%ny
!            call background%setColumnFields(i,j,.false.,xmean,requestedfields)
!        end do
!    end do
!
!    print *,'Background after mean:',background%data(:,1,1)

    call load2DRealFile(b_half_file_name,nzfile,nctrl_file,data2d)

    if (nzfile /= nz_total .or. nctrl_file /= nctrl) then
        write(msgstr,*) 'Error: the nz_total and nctrl did not match the file:',trim(b_half_file_name),&
            &nz_total,nctrl,'vs',nzfile,nctrl_file
        call error(msgstr)
    end if

    allocate(BHalf_full)
    call BHalf_full%fullMatrixOperatorConstructor_data(data2d)
    BHalf      => BHalf_full
    deallocate(data2d)

    call load2DRealFile(rInvHalf_file_name,mobs_file,mobs_file,data2d)

    if (mobs_file /= mobs) then
        write(msgstr,*) 'Error: the mobs did not match the file:',trim(rInvHalf_file_name),&
            &mobs,'vs',mobs_file
        call error(msgstr)
    end if

    allocate(rInvHalf_full)
    call rInvHalf_full%fullMatrixOperatorConstructor_data(data2d)
    rInvHalf      => rInvHalf_full
    deallocate(data2d)

    rtmOpts => getRtmOptions()

    rtmOpts%modelVarNames => requestedFields
    rtmOpts%modelVarNz    => fieldNz

    idealPlatform => getSatellitePlatform(PLATFORM_TO_TEST,IDEAL_OBS_OP)
    assimPlatform => getSatellitePlatform(PLATFORM_TO_TEST,ASSIM_OBS_OP)
    obsOpIn       => getSatelliteObservationOperator(IDEAL_OBS_OP,rtmOpts,idealPlatform)
    obsOpOut      => getSatelliteObservationOperator(ASSIM_OBS_OP,rtmOpts,assimPlatform)

    allocate(obsData)
    call obsData%satelliteObservationConstructor_empty(assimPlatform,(endx-startx+1)*(endy-starty+1))

    call doRtmForward(idealPlatform,state_a3d,obsOpIn,obsData,startx=startx,endx=endx,&
        starty=starty,endy=endy,xstride=xstride)

    if (ASSIM_OBS_OP .eq. CCV_OBS_OP) then
        allocate(ccvObs)
        ccvPlatform => getSatellitePlatform(PLATFORM_TO_TEST,CCV_OBS_OP)
        call ccvObs%ccvObservationConstructor(ccvPlatform,hType,useGlobal,obsData)
        obs => ccvObs
    else
        nullify(ccvPlatform) ! just so we don't deallocate it
        obs => obsData
    end if

    allocate(observer_ptr)
    call observer_ptr%observerConstructor()
    obsOp => obsOpOut
    call observer_ptr%addObservation(obs, obsOp, rInvHalf)

    allocate(initGuess(nctrl))
    initGuess = 0.

    allocate(problem)
    call problem%assimilationProblemConstructor(nctrl,initGuess,converter,observer_ptr,&
        &background,bHalf,alphaTest=alphaTest)

    call strategy%assimilate(problem)

    deallocate(strategy)
    deallocate(state)
    deallocate(background)
    deallocate(BHalf)
    deallocate(observer_ptr)
    deallocate(rtmOpts)
    deallocate(obsOpIn)
    deallocate(obsOpOut)
    deallocate(rInvHalf)
    deallocate(idealPlatform)
    deallocate(assimPlatform)
    if (associated(ccvPlatform)) then
        deallocate(ccvPlatform)
    end if
    deallocate(obsData)
    deallocate(initGuess)

end program
