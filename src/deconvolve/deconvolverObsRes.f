! The DeconvolverObsRes program takes a set of passive satellite observations with a given observing platform
! with given antenna patterns on a given geometrical grid and solves a minimization problem to find the
! optimal deconvolved analysis that matches these observations penalized by departures from a first-guess.
! Both terms (observation departure and background departure penalty) are scaled by their respective covariances.
!
! In short, the DeconvolverObsRes program solves a 3DVar-style cost function J
!   J(z) = 1/2 \delta_x(z)^T \delta_x(z) + 1/2 \delta_y(z)^T \delta_y(z)
! where
!   z is the control variable
!   \delta_x(z) measures the lack of fit between a first-guess background and state
!   \delta_y(z) measures the lack of fit between forward-synthesized observations and actual observations
!
! While the computation of \delta_x(z) and \delta_y(z) involves detailed calculations, the solution of the
! cost function itself is relatively straight-forward once these two terms are available.
!
! January 2017, Jeff Steward
!
program deconvolverObsRes

    use parallelInfo_mod
    use parallelConstants_mod
    use parallelDecomposition_mod

    use, intrinsic :: iso_c_binding
    use iso_fortran_env

    use dataSet_mod
    use dataGrid_mod
    use dataExtent_mod
    use dataDimension_mod
    use datasetVectorConverter_mod

    use dataArrayWriter_mod
    use netcdfDataArrayWriter_mod

    use observer_mod
    use observation_mod
    use observationBundle_mod
    use observationOperator_mod
    use deconvolutionObsResSplitter_mod
    !use observationSplitter_mod
    use observationProcessor_mod
    use observationProcessingChain_mod

    use satelliteObservation_mod
    use scannedObservationBundle_mod

    use radianceAssimilationFactory_mod

    use diagonalMatrixOperator_mod
    use abstractVectorOperator_mod
    use abstractVector_mod
    use simple1DVector_mod

!    use localization_mod

    use assimilationProblem_mod
    use assimilationStrategy_mod
    use nDVarAssimilationStrategy_mod
    use optimizerFactory_mod
    use assimStrategyFactory_mod

    use firstGuesser_mod
    use deconvolutionQCer_mod
    use deconvolutionFactory_mod
    use deconvolutionConstants_mod
    use deconvolutionConverter_mod

    use vertEofHorzFftOperator_mod
    use boundedVarBHalfOp_mod

    use penalizer_mod
!    use penalty_mod
!    use boundedVarPenalty_mod

!    use ctnPlusTexBHalfOp_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    class(ParallelInfo),              pointer :: pinfo_state  => null()
    class(ParallelInfo),              pointer :: pinfo_obs    => null()
    class(ParallelDecomposition),     pointer :: decomp_state => null()
    class(ParallelDecomposition),     pointer :: decomp_obs   => null()

    class(ScannedObservation),        pointer :: scannedObs

    class(NDVarAssimilationStrategy), pointer :: deconvstrategy
    class(AssimilationStrategy),      pointer :: strategy
    class(Optimizer),                 pointer :: opt
    class(DataSet),                   pointer :: state
    class(DataSet),                   pointer :: background
    class(DataSet),                   pointer :: finalState
    class(DataSet),                   pointer :: finalStatePC
    class(DataSet),                   pointer :: columnNorms
    class(AbstractVectorOperator),    pointer :: BHalf
    class(ObservationBundle),         pointer :: obsBundle
    class(ScannedObservationBundle),  pointer :: scannedObsBundle
    class(Observer),                  pointer :: obsvr
    class(DataGrid),                  pointer :: inputGrid
    class(Penalizer),                 pointer :: penmgr
!    class(BoundedVarPenalty),         pointer :: upen
!    class(BoundedVarPenalty),         pointer :: gpen
!    class(Penalty),                   pointer :: pen1
!    class(Penalty),                   pointer :: pen2
    class(DatasetVectorConverter),    pointer :: converter
    class(DeconvolutionConverter),    pointer :: converter_dcnv
    class(ObservationProcessingChain),pointer :: obsProcessor
    class(DeconvolutionQCer),         pointer :: qchecker
    class(DeconvolutionObsResSplitter), pointer :: obsSplitter
    class(ObservationProcessor),      pointer :: obsProc

    class(FirstGuesser),              pointer :: fg

    !class(VertEofHorzFftOperator),    pointer :: fftOp
    !class(CtnPlusTexBHalfOp),         pointer :: fftOp
    class(BoundedVarBHalfOp),         pointer :: fftOp
    class(BoundedVarBHalfOp),         pointer :: fftOpNoVeofs

    class(DataDimension), pointer :: pcDim
    class(DataDimension), pointer :: scanDim
    class(DataDimension), pointer :: pixDim
    class(DataDimension), pointer :: chanDim

    class(DataExtent), pointer :: pixExtent
    class(DataExtent), pointer :: scanExtent
    class(DataExtent), pointer :: pcExtent

    class(DataVariable), pointer :: backgroundVar
    class(DataVariable), pointer :: colNormsVar
    class(DataVariable), pointer :: finalStatePCVar

    class(DataArrayWriter),       pointer :: dawriter
    class(NetcdfDataArrayWriter), pointer :: ncWriter

    character(len=*), parameter :: BACKGROUND_TB_VAR = 'TB_FirstGuess'
    character(len=*), parameter :: COLNORMS_TB_VAR   = 'TB_ColNorms'
    character(len=*), parameter :: FINAL_PC_VAR      = 'FinalPC'

    class(AssimilationProblem), pointer :: problem

    integer, parameter :: maxiter = 1000

    integer :: nctrl
    real(real64), dimension(:), pointer :: initGuess

    integer :: i !, N, M
    integer :: nctrl_u, nctrl_v
!    real(real64) :: dx, dy

    real(real64) :: L

    real(real64), dimension(:,:),   pointer :: veofs
    real(real64), dimension(:),     pointer :: lvals

    real(real64) :: d

    integer, dimension(3) :: dims

    character(len=:), allocatable :: orbitFile
    character(len=:), allocatable :: platformNumberString
    character(len=:), allocatable :: postingNumberString
    character(len=:), allocatable :: outputPrefix
    character(len=:), allocatable :: bhalfFile

    character(len=:), allocatable :: splitFile
    character(len=:), allocatable :: columnNormsFile
    character(len=:), allocatable :: firstGuessFile
    character(len=:), allocatable :: pcStarFile

    character(len=5) :: procNumString

    integer :: platformNumber
    integer :: postingNumber

    integer :: nchans
    integer :: npc
    integer :: nscan
    integer :: npix

    logical :: fileExists

    real(real64), parameter :: s               =  0.0d0
    real(real64), parameter :: pnorm           = 25.0d0
    real(real64), parameter :: obserrInflation =  1.0d0
    logical                 :: alphaTest       = .false.

    character(256) :: obsMatFilename

    ! real(real64) :: buff, startv, endv, startvr, endvr
    !real(real64) :: startvr, endvr
    ! integer :: scans, scane, scansr, scaner
    !integer :: scansr, scaner
    ! integer :: nbuffer, nscanr
    !integer :: nscanr

    real(real64), dimension(:,:),   pointer :: lat
    real(real64), dimension(:,:),   pointer :: lon
    real(real64), dimension(:,:,:), pointer :: backgroundTb
    real(real64), dimension(:,:,:), pointer :: colNormsTb
    real(real64), dimension(:,:,:), pointer :: finalStateTb
    real(real64), dimension(:,:,:), pointer :: finalStatePCPtr

    class(ScannedObservation), pointer :: obs_so

    character(len=1024) :: pcStarOutputName

    integer :: ind1, j, k, ncid, dimids(3), latid, lonid, nx, ny, pcid
    real(real64), dimension(:),     pointer :: optr
    real(real64), dimension(:,:),   pointer :: latptr, lonptr
    real(real64), dimension(:,:,:), pointer :: dptr
    class(AbstractVector),     pointer :: finalControl
    class(AbstractVector),     pointer :: finalStateVector
    class(Simple1DVector),     pointer :: finalStateVector_s1d
    class(DataSet),            pointer :: finalDataSetPC
    complex(C_DOUBLE_COMPLEX)          :: cval

    integer, pointer :: scanRanges(:)

!        class(Simple1DVector), pointer :: u
!        class(Simple1DVector), pointer :: v
!
!        real(real64), dimension(:), pointer :: dptr1, dptr2, dptr3
!
!        class(AbstractVector), pointer :: vec1, vec2

    call initParallel()

    if (command_argument_count() .ne. 5) then
        call print('Usage: deconvolverObsRes.exe [Orbit file] [Platform #] [Posting #] [Output prefix] ' // &
            &'[Split #] [NSplit] [BHalf file]')
        call print('')
        call print('where')
        call print('')
        call print('    [Orbit file] is the orbit file, must be of file type corresponding [platform number]')
        write(msgstr,'(A,I4,A)') '    [platform #] is the number of the platform, e.g. "',PLATFORM_GPM_GMI,&
            '" for GPM/GMI (see radianceAssimilationConstants.f03).'
        call print(msgstr)
        call print('    [Posting #] is the posting to interpolate at; e.g. for GPM/GMI "2" denotes the HR channels.')
        call print('    [Output prefix] is the prefix to add to the split output files.')
        !call print('    [Split #] is number of the split between 1 and NSplit, inclusive, to compute and output.')
        !call print('    [NSplit] is total # of split files, of which this program will compute a single file.')
        call print('    [BHalf file] is the square-root (SVD) of the single column first guess error covariance matrix.')
        call endParallel()
        return
    end if

    call getArg(1, orbitFile)
    call getArg(2, platformNumberString)
    call getArg(3, postingNumberString)
    call getArg(4, outputPrefix)
    call getArg(5, bhalfFile)

    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    call print('Running observation-resolution deconvolution.')
    call print('   Orbit file: '      // trim(orbitFile))
    call print('   Platform number: ' // trim(platformNumberString))
    call print('   Posting number: '  // trim(postingNumberString))
    call print('   Output prefix: '   // trim(outputPrefix))
    call print('   BHalf file: '      // trim(bhalfFile))

    allocate(decomp_state)
    call decomp_state%parallelDecompositionConstructor(dim1Name=SCANS_DIM_NAME)

    allocate(pinfo_state)
    call pinfo_state%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp_state)

    write(msgstr,'(I6)') pinfo_state%getCommSize()
    call print('   Number of processors: ' // trim(adjustl(msgstr)))

    read( platformNumberString, '(i10)' ) platformNumber
    read( postingNumberString,  '(i10)' ) postingNumber

    write(procNumString,'(i5)') pinfo_state%getRank() + 1 + 10000 ! make a zero padded string

    splitFile       = 'tbDeconv_'   // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    columnNormsFile = 'colNorms_'   // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    firstGuessFile  = 'firstguess_' // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    pcStarFile      = 'pcStar_'     // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'

    call print('----------------------------------------------------')
    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    call print('Output deconvolution file: ' // trim(splitFile))
    call print('Output column norms file: '  // trim(columnNormsFile))
    call print('Output first guess file: '   // trim(firstGuessFile))
    call print('Output PC solutions file: '  // trim(pcStarFile))
    call print('')

    nchans = 0

    inquire(file=trim(BHalfFile),exist=fileExists)

    if (.not. fileExists) then
        call error('Could not load the ' // trim(BHalfFile) // ' file!')
    end if

    call print('Now reading BHalf file' // trim(BHalfFile))
    call load2DRealFile(BHalfFile,npc,nchans,veofs)
    write(msgstr,*) 'Read ',nchans,'chans /',npc,'PCs from the BHalf file ',trim(BHalfFile)
    call print(msgstr)
    call print('')

    if (nchans <= 0 .or. npc <= 0) then
        write(msgstr,*) 'Invalid BHalfFile ',trim(BHalfFile),'. Dims:',nchans,npc
        call error(msgstr)
    end if

    write(msgstr,*) 'Now reading orbit file ',trim(orbitFile), ' for deconvolution at posting:', &
        postingNumber
    call print(msgstr)

    !allocate(decomp_obs)
    !call decomp_obs%parallelDecompositionConstructor()

    allocate(pinfo_obs)
    call pinfo_obs%parallelInfoConstructor(MIRRORED_PARALLEL_TYPE)
    !call pinfo_obs%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp_obs)

    scannedObs => getScannedObservation(pinfo_state,orbitFile,platformNumber,postingNumber)

    call print('Finished reading orbit file ' // trim(orbitFile))

    pixExtent  => scannedObs%getPixelExtent()
    scanExtent => scannedObs%getScanExtent()
    pixDim     => pixExtent%getDimension()
    scanDim    => scanExtent%getDimension()

    allocate(scanRanges(pinfo_state%getCommSize()))
    scanRanges(pinfo_state%getRank()+1) = scanExtent%getLocalEnd()

    do i=1,pinfo_state%getCommSize()
        call bcast0d(scanRanges(i),i-1,pinfo_state%getCommunicator(),'bcasting the scan range')
    end do

    print *,'scan ranges:',scanRanges

    npix  =  pixExtent%getLocalCount()
    nscan = scanExtent%getLocalCount()

    write(msgstr,*) 'Read orbit file on rank ' // int2str(pinfo_obs%getRank()) // '. NScans:' // &
        & int2str(nscan) // ', npixels:' // int2str(npix) // ', nchannels:' // int2str(nchans)
    call print(msgstr,forceOutput=.true.)
    call print('')

    ! nbuffer = nsplit - 1

    ! buff = 0.5d0/dble(nsplit*nbuffer)

    ! startv = max(dble(splitNum-1)/dble(nsplit)-buff,0.d0)
    ! endv = min(dble(splitNum)/dble(nsplit)+buff,1.d0)

    ! scans = max(ceiling(startv*(nscan-1))+1,1)
    ! scane = floor(endv*(nscan-1))+1

    !startvr = dble(pinfo%getRank())/dble(size_world)
    !endvr = dble(pinfo%getRank()+1)/dble(size_world)

    !scansr = ceiling(startvr*(nscan-1))+1
    !scaner = floor(endvr*(nscan-1))+1

    !write(msgstr,'(A,I4,A,F8.3,A,F8.3,A,I6,A,I6,A)') ' Processor #'&
    !    &,pinfo%getRank()+1,' goes from (',100*startvr,'%,',100*endvr,&
    !    &'%), scans (',scansr,',',scaner,')'
    !call print(msgstr,forceOutput=.true.)
    !call print('')

    !nscanr = scansr - scaner + 1

!    deallocate(scannedObs)
!
!    write(msgstr,*) 'Now re-reading orbit file ',trim(orbitFile), &
!        &' for deconvolution at posting:', postingNumber
!    call print(msgstr)
!
!    scannedObs => getScannedObservation(orbitFile,platformNumber,postingNumber,&
!        &minScan=scansr,maxScan=scaner)

    allocate(background)
    call background%dataSetConstructor(null())
    chanDim => background%addDimensionByName(CHANS_DIM_NAME,nchans)
    call background%addDimension(scanDim%clone())
    call background%addDimension(pixDim%clone())

    backgroundVar => background%addVariable(pinfo_state,BACKGROUND_TB_VAR,backgroundTb,&
        & chanDim,pixDim,scanDim,initVal=-999.d0)

    call scannedObs%findGrid(pinfo_obs)
    inputGrid => scannedObs%getGrid()

    allocate(columnNorms)
    call columnNorms%dataSetConstructor(null())
    call columnNorms%addDimension(chanDim%clone())
    call columnNorms%addDimension(scanDim%clone())
    call columnNorms%addDimension(pixDim%clone())

    colNormsVar => columnNorms%addVariable(pinfo_state,COLNORMS_TB_VAR,colNormsTb,&
        & chanDim, pixDim, scanDim)

    write(msgstr,*) 'size of colNormsTb:',shape(colNormsTb)
    call print(msgstr,forceOutput=.true.)

    ! now every PE has the observations available. It is time to partition ownership
    ! and perform basic quality control

    allocate(obsProcessor)
    call obsProcessor%observationProcessingChainConstructor()

    allocate(obsSplitter)
    call obsSplitter%deconvolutionObsResSplitterConstructor(inputGrid,scanRanges)
    obsProc => obsSplitter
    call obsProcessor%addProcessor(obsProc)

    allocate(qchecker)
    call qchecker%deconvolutionQCerConstructor()
    obsProc => qchecker
    call obsProcessor%addProcessor(obsProc)

    call print('Now loading the scannedObsBundle from ' // trim(orbitFile))
    scannedObsBundle => getScannedObservationBundle(pinfo_obs,orbitFile,inputGrid,platformNumber,&
        &obsProcessor,minGoodRatio=0.01d0,obsErrInflation=5.0d0,columnNormsVar=colNormsVar)

    call print('Loaded the scannedObsBundle from ' // trim(orbitFile))
    call print('')

    allocate(obsvr)
    call obsvr%observerConstructor()

    obsBundle => scannedObsBundle

    ! also runs QC
    call obsvr%addObservationBundle(obsBundle)

    fg => getFirstGuesser(ADJOINT_AVE_FG)

    call print('Now populating the first guess.')
    call fg%populateFirstGuess(scannedObsBundle,background)
    call print('Finished populating the first guess.')
    call print('')

    call print('Now writing the first guess data to ' // trim(firstGuessFile))
    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(firstGuessFile)
    dawriter => ncWriter
    call background%writeToFile(pinfo_state,dawriter)
    deallocate(ncWriter)
    call print('Successfully wrote the first guess data to ' // trim(firstGuessFile))
    call print('')

    call print('Now writing the column norms data to ' // trim(columnNormsFile))
    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(columnNormsFile)
    dawriter => ncWriter
    call columnNorms%writeToFile(pinfo_state,dawriter)
    deallocate(ncWriter)
    call print('Successfully wrote the column norms data to ' // trim(columnNormsFile))
    call print('----------------------------------------------------')

    allocate(fftOp)

    nx = size(backgroundTb,2)
    ny = size(backgroundTb,3)

!    dx = background%getDX()/1000.d0
!    dy = background%getDY()/1000.d0

!    allocate(lvals(npc))
!    open(unit=92, file='lvals.txt')
!    do i = 1,npc
!        read(92,*) lvals(i)
!        lvals(i) = lvals(i)/dx
!    end do
!    close(92)
!
!    lvals = lvals
!
!    call fftOp%vertEofHorzFftOperatorConstructor(N,M,nchans,npc,veofs,lvals)

!    call fftOp%ctnPlusTexBHalfOpConstructor(N,M,nchans,npc,veofs)
!    nctrl_u = fftOp%getNumControlU()
!    nctrl_v = fftOp%getNumControlV()

    call fftOp%boundedVarBHalfOpConstructor(nx,ny,nchans,npc,veofs)

    allocate(problem)

    nctrl = fftOp%getNumControl()
    opt => getOptimizer(LBFGS_OPTIMIZER,nctrl,maxiter)

    allocate(initGuess(nctrl))
    initGuess = 0.

    allocate(converter_dcnv)

    converter => converter_dcnv

    !allocate(upen)
    !call upen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl_u)
    !call upen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl)
    !pen1 => upen

    !allocate(gpen)
    !call gpen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl_v,nctrl_u,pnorm,0)
    !pen2 => gpen

    allocate(penmgr)

    call penmgr%penalizerConstructor(0)
!    call penmgr%addPenalty(pen1,1.0d5)
!    call penmgr%addPenalty(pen2,1.0d7)

    bHalf => fftOp

    call problem%assimilationProblemConstructor(nctrl,initGuess,opt,converter,obsvr,&
        &background,bHalf,penmgr,alphaTest)

    allocate(deconvstrategy)
    call deconvstrategy%nDVarAssimilationStrategyConstructor()

    call print('Now beginning deconvolution minimization.')
    call deconvstrategy%assimilate(problem)

    finalState => problem%getBaseDataSet()

    call print('Finished the deconvolution minimization.')
    call print('----------------------------------------------------')
    call print('Now writing the deconvolution output to ' // trim(splitFile))
    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(splitFile)
    dawriter => ncWriter
    call finalState%writeToFile(pinfo_state,dawriter)
    deallocate(ncWriter)
    call print('Successfully wrote the deconvolution output to ' // trim(splitFile))
    call print('')

    finalControl => problem%getControlVector()

    allocate(fftOpNoVeofs)

    call fftOpNoVeofs%boundedVarBHalfOpConstructor(nx,ny,npc,npc)

    pcDim => finalStatePC%addDimensionByName('PC',npc)
    allocate(pcExtent)
    call pcExtent%dataExtentConstructor(pcDim)

    allocate(finalStatePC)
    call finalStatePC%dataSetConstructor(null())
    finalStatePCVar => finalStatePC%addVariable(pinfo_state,FINAL_PC_VAR,finalStatePCPtr,&
        & pcExtent, scannedObs%getPixelExtent(), scannedObs%getScanExtent(),       &
        & initVal=-999.d0)

    allocate(finalStateVector_s1d)
    call finalStateVector_s1d%simple1DVectorConstructor(nx*ny*npc)

    finalStateVector => finalStateVector_s1d

    call fftOpNoVeofs%applyOperator(finalControl,finalStateVector)

    call converter%convertToState(finalStateVector, finalDataSetPC)

    call print('Now writing the PC solution data to ' // trim(pcStarFile))
    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(pcStarFile)
    dawriter => ncWriter
    call finalStatePC%writeToFile(pinfo_state,dawriter)
    deallocate(ncWriter)

    call print('Successfully wrote the PC solution data to ' // trim(pcStarFile))
    call print('')
    call print('----------------------------------------------------')
    call print('Program finished successfully.')

!        dptr1 => problem%getInitialGuess()
!        dptr2 => dptr1(1:nctrl_u)
!        dptr3 => dptr1(nctrl_u+1:nctrl_u+nctrl_v)
!
!        allocate(u)
!        allocate(v)
!
!        call u%simple1DVectorConstructor_data(dptr2)
!        call v%simple1DVectorConstructor_data(dptr3)
!
!        vec1 => u
!        vec2 => problem%getStateVector()
!        call fftOp%BHalf_u%applyOperator(vec1,vec2)
!
!        dptr1 => vec2%get1DArrayPtr()
!        call print('u norm:',sqrt(dot_product(dptr1,dptr1)))
!
!        call converter%convertToState(vec2, finalState)
!        call finalState_ores%writeToFile('u_vec.nc')
!
!        call print('size of x vector:',size(dptr1),N*M*nchans)
!
!        vec1 => v
!        vec2 => problem%getStateVector()
!        call fftOp%BHalf_v%applyOperator(vec1,vec2)
!        call vec2%scalarMultiply(fftOp%v_factor,vec2)
!
!        dptr1 => vec2%get1DArrayPtr()
!        call print('v norm:',sqrt(dot_product(dptr1,dptr1)))
!
!        call converter%convertToState(vec2, finalState)
!        call finalState_ores%writeToFile('v_vec.nc')
!
!        deallocate(u)
!        deallocate(v)

end program
