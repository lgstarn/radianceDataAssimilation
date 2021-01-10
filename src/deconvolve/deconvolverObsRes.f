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
    use dataExtent_mod
    use dataDimension_mod
    use datasetVectorConverter_mod

    use dataGrid_mod
    use regularTriangularLatLonGrid_mod

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
    use assimStrategyFactory_mod

    use firstGuesser_mod
    use deconvolutionQCer_mod
    use deconvolutionFactory_mod
    use deconvolutionConstants_mod
    use deconvolutionConverter_mod

    use vertEofOperator_mod
    !use vertEofHorzFftOperator_mod
    !use boundedVarBHalfOp_mod

    ! use penalizer_mod
    ! use penalty_mod
    ! use boundedVarPenalty_mod

!    use ctnPlusTexBHalfOp_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    class(ParallelInfo),              pointer :: pinfo_state  => null()
    class(ParallelInfo),              pointer :: pinfo_obs    => null()
    class(ParallelDecomposition),     pointer :: decomp_state => null()
    class(ParallelDecomposition),     pointer :: decomp_obs   => null()

    class(ScannedObservation),        pointer :: scannedObs => null()

    class(NDVarAssimilationStrategy), pointer :: deconvstrategy => null()
    class(AssimilationStrategy),      pointer :: strategy => null()
    class(DataSet),                   pointer :: state => null()
    class(SatelliteObservation),      pointer :: background => null()
    class(DataSet),                   pointer :: background_ds => null()

    class(DataSet),                   pointer :: finalState => null()
    class(SatelliteObservation),      pointer :: finalState_so

    class(DataSet),                   pointer :: finalStatePC => null()
    class(DataSet),                   pointer :: columnNorms => null()
    class(AbstractVectorOperator),    pointer :: BHalf => null()
    class(ObservationBundle),         pointer :: obsBundle => null()
    class(ScannedObservationBundle),  pointer :: scannedObsBundle => null()
    class(Observer),                  pointer :: obsvr => null()
    class(RegularTriangularLatLonGrid), pointer :: inputGrid => null()
    class(DataGrid),                  pointer :: inputGrid_ds => null()
    class(VertEofOperator),           pointer :: veofOp

!    class(Penalizer),                 pointer :: penmgr => null()
!    class(BoundedVarPenalty),         pointer :: upen
!    class(BoundedVarPenalty),         pointer :: gpen
!    class(Penalty),                   pointer :: pen1
!    class(Penalty),                   pointer :: pen2
    class(DatasetVectorConverter),    pointer :: converter => null()
    class(DeconvolutionConverter),    pointer :: converter_dcnv => null()
    class(ObservationProcessingChain),pointer :: obsProcessor => null()
    class(DeconvolutionQCer),         pointer :: qchecker => null()
    class(DeconvolutionObsResSplitter), pointer :: obsSplitter => null()
    class(ObservationProcessor),      pointer :: obsProc => null()
    class(SatellitePlatformInfo),     pointer :: platform => null()

    class(FirstGuesser),              pointer :: fg => null()

    !class(VertEofHorzFftOperator),    pointer :: fftOp
    !class(CtnPlusTexBHalfOp),         pointer :: fftOp
    !class(BoundedVarBHalfOp),         pointer :: fftOp        => null()
    !class(BoundedVarBHalfOp),         pointer :: fftOpNoVeofs => null()

    class(DataDimension), pointer :: pcDim
    class(DataDimension), pointer :: chanDim
    class(DataDimension), pointer :: nobsDim

    class(DataDimension), pointer :: scanDim
    class(DataDimension), pointer :: pixDim

    class(DataExtent), pointer :: nobsExtent  => null()
    class(DataExtent), pointer :: chanExtent  => null()
    class(DataExtent), pointer :: nLociExtent => null()
    class(DataExtent), pointer :: pcExtent    => null()
    class(DataExtent), pointer :: scanExtent  => null()
    class(DataExtent), pointer :: pixExtent   => null()

    class(DataVariable), pointer :: tbVar   => null()
    class(DataVariable), pointer :: obsLociVar      => null()
    class(DataVariable), pointer :: colNormsVar     => null()
    class(DataVariable), pointer :: finalStatePCVar => null()
    class(DataVariable), pointer :: latVar => null()
    class(DataVariable), pointer :: lonVar => null()
    class(DataVariable), pointer :: obsDataVar => null()
    class(DataVariable), pointer :: qc3dVar => null()
    class(DataVariable), pointer :: qcCodesVar => null()
    class(DataVariable), pointer :: qcCodesVarGlobal => null()
    class(DataArrayWriter),       pointer :: dawriter => null()
    class(NetcdfDataArrayWriter), pointer :: ncWriter => null()

    real(real64), pointer :: tbptr(:,:,:)      => null()
    real(real64), pointer :: latPtr(:,:)      => null()
    real(real64), pointer :: lonPtr(:,:)      => null()
    real(real64), pointer :: obsData(:,:)      => null()

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
    character(len=:), allocatable :: startScanString
    character(len=:), allocatable :: endScanString
    character(len=:), allocatable :: outputPrefix
    character(len=:), allocatable :: bhalfFile

    character(len=:), allocatable :: deconvolvedOutputFile
    character(len=:), allocatable :: columnNormsFile
    character(len=:), allocatable :: firstGuessFile
    character(len=:), allocatable :: pcStarFile

    character(len=5) :: procNumString

    integer :: platformNumber
    integer :: postingNumber

    integer :: scans, scane
    real(real64) :: startv, endv
    integer :: nchans
    integer :: npc
    integer :: nobs

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

    real(real64), dimension(:,:),   pointer :: obsLoci
    real(real64), dimension(:,:),   pointer :: lat
    real(real64), dimension(:,:),   pointer :: lon
    real(real64), dimension(:,:,:), pointer :: backgroundTb
    real(real64), dimension(:,:,:), pointer :: colNormsTb
    real(real64), dimension(:,:,:), pointer :: finalStateTb
    real(real64), dimension(:,:),   pointer :: finalStatePCPtr
    real(real64), dimension(:),     pointer :: finalControlPtr

    class(ScannedObservation), pointer :: obs_so

    character(len=1024) :: pcStarOutputName

    integer :: ind1, j, k, ncid, dimids(3), latid, lonid, nx, ny, pcid
    real(real64), dimension(:),     pointer :: optr
    real(real64), dimension(:,:,:), pointer :: dptr
    class(AbstractVector),     pointer :: finalControl
    class(AbstractVector),     pointer :: finalStateVector
    class(Simple1DVector),     pointer :: finalStateVector_s1d
    class(DataSet),            pointer :: finalDataSetPC
    complex(C_DOUBLE_COMPLEX)          :: cval

    integer :: npixg, nscang, nscanl, oli, olj, startScan, endScan, ind

    integer, pointer :: scanRanges(:)
    integer, pointer :: qcCodes(:,:)
    integer, pointer :: qc3d(:,:,:)

!        class(Simple1DVector), pointer :: u
!        class(Simple1DVector), pointer :: v
!
!        real(real64), dimension(:), pointer :: dptr1, dptr2, dptr3
!
!        class(AbstractVector), pointer :: vec1, vec2

    call initParallel()

    if (command_argument_count() .ne. 7) then
        call print('Usage: deconvolverObsRes.exe [Orbit file] [Platform #] [Posting #] [Start scan] [End scan] ' // &
            '[Output prefix] [Split #] [NSplit] [BHalf file]')
        call print('')
        call print('where')
        call print('')
        call print('    [Orbit file] is the orbit file, must be of file type corresponding [platform number]')
        write(msgstr,'(A,I4,A)') '    [platform #] is the number of the platform, e.g. "',PLATFORM_GPM_GMI,&
            '" for GPM/GMI (see radianceAssimilationConstants.f03).'
        call print(msgstr)
        call print('    [Posting #] is the posting to interpolate at; e.g. for GPM/GMI "2" denotes the HR channels.')
        call print('    [Start scan] is the first scan for the domain used for the obs-res deconvolution.')
        call print('    [End scan] is the last scan for the domain used for the obs-res deconvolution.')
        call print('    [Output prefix] is the prefix to add to the split output files.')
        !call print('    [Split #] is number of the split between 1 and NSplit, inclusive, to compute and output.')
        !call print('    [NSplit] is total # of split files, of which this program will compute a single file.')
        call print('    [BHalf file] is the square-root (SVD) of the single column first guess error covariance matrix.')
        call endParallel()
    end if

    call getArg(1, orbitFile)
    call getArg(2, platformNumberString)
    call getArg(3, postingNumberString)
    call getArg(4, startScanString)
    call getArg(5, endScanString)
    call getArg(6, outputPrefix)
    call getArg(7, bhalfFile)

    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    call print('Running observation-resolution deconvolution.')
    call print('   Orbit file:      ' // trim(orbitFile))
    call print('   Platform number: ' // trim(platformNumberString))
    call print('   Start scan:      ' // trim(startScanString))
    call print('   End scan:        ' // trim(endScanString))
    call print('   Posting number:  ' // trim(postingNumberString))
    call print('   Output prefix:   ' // trim(outputPrefix))
    call print('   BHalf file:      ' // trim(bhalfFile))

    allocate(decomp_state)
    call decomp_state%parallelDecompositionConstructor(dim1Name=SCANS_DIM_NAME)

    allocate(pinfo_state)
    call pinfo_state%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp_state)

    write(msgstr,'(I6)') pinfo_state%getCommSize()
    call print('   Number of processors: ' // trim(adjustl(msgstr)))

    read( platformNumberString, '(i10)' ) platformNumber
    ! use CRTM as default just to get proper number of channels
    platform => getSatellitePlatform(platformNumber,CRTM_OBS_OP)

    read( postingNumberString,  '(i10)' ) postingNumber
    read( startScanString,  '(i10)' ) startScan
    read( endScanString,  '(i10)' ) endScan

    write(procNumString,'(i5)') pinfo_state%getRank() + 1 + 10000 ! make a zero padded string

    deconvolvedOutputFile       = 'tbDeconv_'   // trim(outputPrefix) // '.nc'
    columnNormsFile = 'colNorms_'   // trim(outputPrefix) // '.nc'
    firstGuessFile  = 'firstguess_' // trim(outputPrefix) // '.nc'
    pcStarFile      = 'pcStar_'     // trim(outputPrefix) // '.nc'

    call print('----------------------------------------------------')
    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    call print('Output deconvolution file: ' // trim(deconvolvedOutputFile))
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

    allocate(pinfo_obs)
    call pinfo_obs%parallelInfoConstructor(MIRRORED_PARALLEL_TYPE)

    scannedObs => getScannedObservation(pinfo_obs,orbitFile,platformNumber,postingNumber)

    call print('Finished reading orbit file ' // trim(orbitFile))

    nobsExtent  => scannedObs%getNobsExtent()
    nobsDim     => nobsExtent%getDimension()
    nlociExtent => scannedObs%getNLociExtent()

    obsLoci => scannedObs%getObsLoci()

    ! global npix and nscan
    npixg  = maxval(obsLoci(SO_PIX_DIM,:))
    nscang = maxval(obsLoci(SO_SCAN_DIM,:))

    call barrier()

    write(msgstr,*) 'Read orbit file. NScans:' // &
        & int2str(nscang) // ', npixels:' // int2str(npixg) // ', nchannels:' // int2str(nchans)
    call print(msgstr) !,forceOutput=.true.)
    call print('')

    call decomposeElements(endScan-startScan+1, pinfo_obs%getCommSize(), pinfo_obs%getRank(), nscanl, scans)

    scans = scans + startScan - 1
    scane = scans + nscanl - 1

    write(msgstr,'(A,I0,A,I0,A,I0,A)') ' Processor #'&
        &,pinfo_obs%getRank()+1,' has scans (',scans,',',scane,')'
    call print(msgstr,forceOutput=.true.)

    allocate(scanRanges(0:pinfo_obs%getCommSize()))
    scanRanges(0) = startScan-1
    scanRanges(pinfo_state%getRank()+1) = scane

    do i=1,pinfo_state%getCommSize()
        call bcast0d(scanRanges(i),i-1,pinfo_state%getCommunicator(),'bcasting the scan range')
    end do


    allocate(background)
    call background%satelliteObservationConstructor(platform)

    chanDim => background%addDimensionByName(CHANS_DIM_NAME,nchans)
    scanDim => background%addDimensionByName(SCANS_DIM_NAME,endScan-startScan+1)
    pixDim => background%addDimensionByName(PIXELS_DIM_NAME,npixg)

    allocate(chanExtent)
    call chanExtent%dataExtentConstructor(chanDim)
    allocate(scanExtent)
    call scanExtent%dataExtentConstructor(scanDim,nscanl,scans-startScan+1)
    allocate(pixExtent)
    call pixExtent%dataExtentConstructor(pixDim)

    tbVar => background%addVariable(pinfo_state,TB_VAR_NAME,tbPtr,&
        & chanExtent,pixExtent,scanExtent,initVal=-999.d0)
    latVar => background%addVariable(pinfo_state,LAT_VAR_NAME,latPtr,&
        & pixExtent,scanExtent,initVal=-999.d0)
    lonVar => background%addVariable(pinfo_state,LON_VAR_NAME,lonPtr,&
        & pixExtent,scanExtent,initVal=-999.d0)
    qc3dVar => background%addVariable(pinfo_state,'qcCodes_3d',qc3d,&
        & chanExtent,pixExtent,scanExtent,initVal=0)

    ! copy over the lat/lon values
    do j=scans,scane
        do i=1,npixg
            latptr(i,j-scans+1) = obsLoci(SO_LAT_DIM,i+(j-1)*npixg)
            lonptr(i,j-scans+1) = obsLoci(SO_LON_DIM,i+(j-1)*npixg)
        end do
    end do

    allocate(inputGrid)
    call inputGrid%regularTriangularLatLonGridConstructor(latVar,lonVar)
    call inputGrid%tile(pinfo_obs)

!    allocate(columnNorms)
!    call columnNorms%dataSetConstructor(null())
!    call columnNorms%addDimension(chanDim%clone())
!    call columnNorms%addDimension(scanDim%clone())
!    call columnNorms%addDimension(pixDim%clone())
!
!    colNormsVar => columnNorms%addVariable(pinfo_state,COLNORMS_TB_VAR,colNormsTb,&
!        & chanDim, pixDim, scanDim)
!
!    write(msgstr,*) 'size of colNormsTb:',shape(colNormsTb)
!    call print(msgstr,forceOutput=.true.)

    ! now every PE has the observations available. It is time to partition ownership
    ! and perform basic quality control

    allocate(obsProcessor)
    call obsProcessor%observationProcessingChainConstructor()

    inputGrid_ds => inputGrid

    allocate(obsSplitter)
    call obsSplitter%deconvolutionObsResSplitterConstructor(inputGrid_ds,scanRanges)
    obsProc => obsSplitter
    call obsProcessor%addProcessor(obsProc)

    allocate(qchecker)
    call qchecker%deconvolutionQCerConstructor()
    obsProc => qchecker
    call obsProcessor%addProcessor(obsProc)

    call print('Now loading the scannedObsBundle from ' // trim(orbitFile))
    scannedObsBundle => getScannedObservationBundle(pinfo_obs,orbitFile,inputGrid_ds,platformNumber,&
        &obsProcessor,minGoodRatio=0.01d0,obsErrInflation=1d0)

    call print('Loaded the scannedObsBundle from ' // trim(orbitFile))
    call print('')

    allocate(obsvr)
    call obsvr%observerConstructor()

    obsBundle => scannedObsBundle

    ! also runs QC
    call obsvr%addObservationBundle(obsBundle)

    ! the TB values will be set by the first guesser
    tbPtr = 0.

    call background%loadSatelliteObservation_tb3d(pinfo_state, latVar, lonVar, tbVar, deleteVars=.false.)

    fg => getFirstGuesser(ADJOINT_AVE_FG)

    background_ds => background

    call print('Now populating the first guess.')
    call fg%populateFirstGuess(pinfo_state,scannedObsBundle,background_ds)
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

!    call print('Now writing the column norms data to ' // trim(columnNormsFile))
!    allocate(ncWriter)
!    call ncWriter%netcdfDataArrayWriterConstructor(columnNormsFile)
!    dawriter => ncWriter
!    call columnNorms%writeToFile(pinfo_state,dawriter)
!    deallocate(ncWriter)
!    call print('Successfully wrote the column norms data to ' // trim(columnNormsFile))
!    call print('----------------------------------------------------')

    obsDataVar => background%getObsDataVar()

    call obsDataVar%getArray(obsData)

    allocate(veofOp)
    call veofOp%vertEofOperatorConstructor(1,nchans,npc,veofs)

    allocate(converter_dcnv)
    call converter_dcnv%deconvolutionConverterConstructor(OBS_DATA_VAR_NAME,background%getQCCodes())

    nctrl = converter_dcnv%getLocalStateVectorSize(background_ds)
    if (mod(nctrl,size(obsData,1)) /= 0) then
        call error('The number of control variables was not compatible with the obsData size: ' // &
            int2str(nctrl) // ' vs. ' // int2str(size(obsData,1)))
    end if

    ! the number of control variables is the local size with channels swapped for PCs
    nctrl = npc*(nctrl/size(obsData,1))

    allocate(initGuess(nctrl))
    initGuess = 0.

    converter => converter_dcnv

    allocate(obsvr)
    call obsvr%observerConstructor()

    obsBundle => scannedObsBundle
    call obsvr%addObservationBundle(obsBundle)

    bHalf => veofOp

    allocate(problem)
    ! FIXME: remove pinfo from here, only pass in during assimilate. Stale pointers are bad.
    call problem%assimilationProblemConstructor(pinfo_state,nctrl,initGuess,converter,obsvr,&
        &background_ds,bHalf)

    allocate(deconvstrategy)
    call deconvstrategy%nDVarAssimilationStrategyConstructor()

    write(msgstr,*) 'Now beginning deconvolution minimization...'
    call print(msgstr)

    call deconvstrategy%assimilate(pinfo_state,problem)

    finalState => problem%getBaseDataSet()

    select type (finalState)
        class is (SatelliteObservation)
            finalState_so => finalState
        class default
            call error('Unknown dataset type')
    end select

    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(deconvolvedOutputFile)
    dawriter => ncWriter

    call finalState_so%writeSatObsToFile(pinfo_state,dawriter)

    deallocate(ncWriter)

    write(msgstr,*) 'Finalized deconvolution and wrote output to ',trim(deconvolvedOutputFile)
    call print(msgstr)

    finalControl => problem%getControlVector()

    allocate(finalStatePC)
    call finalStatePC%dataSetConstructor(null())

    pcDim => finalStatePC%addDimensionByName('PC',npc)
    allocate(pcExtent)
    call pcExtent%dataExtentConstructor(pcDim)

    nobsExtent => finalState_so%getNObsExtent()

    finalStatePCVar => finalStatePC%addVariable(pinfo_state,FINAL_PC_VAR,finalStatePCPtr,&
        & pcExtent, nobsExtent, initVal=-999.d0)

    obsLociVar => finalState_so%getObsLociVar()
    qcCodesVar => finalState_so%getQcCodesVar()

    call finalStatePC%addVariablePointer(obsLociVar%clone(copyData=.true.))
    call finalStatePC%addVariablePointer(qcCodesVar%clone(copyData=.true.))

    call qcCodesVar%getArray(qcCodes)

    finalControlPtr => finalControl%get1DArrayPtr()

    if (mod(size(finalControlPtr),npc) /= 0) then
        write(msgstr,*) 'The size of the final control pointer was not as expected on rank ',pinfo_state%getRank(),':',&
            size(finalControlPtr),npc
        call error(msgstr)
    end if

    ind = 0

    do j=1,size(finalStatePCPtr,2)
        if (all(qcCodes(:,j) == 0)) then
            do i=1,npc
                ind = ind + 1
                finalStatePCPtr(i,j) = finalControlPtr(ind)
            end do
        end if
    end do

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

    call endParallel()

!    allocate(fftOp)
!
!    nx = size(backgroundTb,2)
!    ny = size(backgroundTb,3)
!
!!    dx = background%getDX()/1000.d0
!!    dy = background%getDY()/1000.d0
!
!!    allocate(lvals(npc))
!!    open(unit=92, file='lvals.txt')
!!    do i = 1,npc
!!        read(92,*) lvals(i)
!!        lvals(i) = lvals(i)/dx
!!    end do
!!    close(92)
!!
!!    lvals = lvals
!!
!!    call fftOp%vertEofHorzFftOperatorConstructor(N,M,nchans,npc,veofs,lvals)
!
!!    call fftOp%ctnPlusTexBHalfOpConstructor(N,M,nchans,npc,veofs)
!!    nctrl_u = fftOp%getNumControlU()
!!    nctrl_v = fftOp%getNumControlV()
!
!    call fftOp%boundedVarBHalfOpConstructor(nx,ny,nchans,npc,veofs)
!
!    allocate(problem)
!
!    nctrl = fftOp%getNumControl()
!
!    allocate(initGuess(nctrl))
!    initGuess = 0.
!
!    allocate(converter_dcnv)
!
!    converter => converter_dcnv
!
!    !allocate(upen)
!    !call upen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl_u)
!    !call upen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl)
!    !pen1 => upen
!
!    !allocate(gpen)
!    !call gpen%boundedVarPenaltyConstructor(N,M,nchans,npc,nctrl_v,nctrl_u,pnorm,0)
!    !pen2 => gpen
!
!    allocate(penmgr)
!
!    call penmgr%penalizerConstructor(0)
!!    call penmgr%addPenalty(pen1,1.0d5)
!!    call penmgr%addPenalty(pen2,1.0d7)
!
!    bHalf => fftOp
!
!    call problem%assimilationProblemConstructor(pinfo_state,nctrl,initGuess,converter,obsvr,&
!        &background,bHalf,penmgr,alphaTest)
!
!    allocate(deconvstrategy)
!    call deconvstrategy%nDVarAssimilationStrategyConstructor()
!
!    call print('Now beginning deconvolution minimization.')
!    call deconvstrategy%assimilate(pinfo_state,problem)
!
!    finalState => problem%getBaseDataSet()
!
!    call print('Finished the deconvolution minimization.')
!    call print('----------------------------------------------------')
!    call print('Now writing the deconvolution output to ' // trim(deconvolvedOutputFile))
!    allocate(ncWriter)
!    call ncWriter%netcdfDataArrayWriterConstructor(deconvolvedOutputFile)
!    dawriter => ncWriter
!    call finalState%writeToFile(pinfo_state,dawriter)
!    deallocate(ncWriter)
!    call print('Successfully wrote the deconvolution output to ' // trim(deconvolvedOutputFile))
!    call print('')
!
!    finalControl => problem%getControlVector()
!
!    allocate(fftOpNoVeofs)
!
!    call fftOpNoVeofs%boundedVarBHalfOpConstructor(nx,ny,npc,npc)
!
!    pcDim => finalStatePC%addDimensionByName('PC',npc)
!    allocate(pcExtent)
!    call pcExtent%dataExtentConstructor(pcDim)
!
!    allocate(finalStatePC)
!    call finalStatePC%dataSetConstructor(null())
!    finalStatePCVar => finalStatePC%addVariable(pinfo_state,FINAL_PC_VAR,finalStatePCPtr,&
!        & pcExtent, scannedObs%getPixelExtent(), scannedObs%getScanExtent(),       &
!        & initVal=-999.d0)
!
!    allocate(finalStateVector_s1d)
!    call finalStateVector_s1d%simple1DVectorConstructor(nx*ny*npc)
!
!    finalStateVector => finalStateVector_s1d
!
!    call fftOpNoVeofs%applyOperator(finalControl,finalStateVector)
!
!    call converter%convertToState(finalStateVector, finalDataSetPC)
!
!    call print('Now writing the PC solution data to ' // trim(pcStarFile))
!    allocate(ncWriter)
!    call ncWriter%netcdfDataArrayWriterConstructor(pcStarFile)
!    dawriter => ncWriter
!    call finalStatePC%writeToFile(pinfo_state,dawriter)
!    deallocate(ncWriter)
!
!    call print('Successfully wrote the PC solution data to ' // trim(pcStarFile))
!    call print('')
!    call print('----------------------------------------------------')
!    call print('Program finished successfully.')
!
!!        dptr1 => problem%getInitialGuess()
!!        dptr2 => dptr1(1:nctrl_u)
!!        dptr3 => dptr1(nctrl_u+1:nctrl_u+nctrl_v)
!!
!!        allocate(u)
!!        allocate(v)
!!
!!        call u%simple1DVectorConstructor_data(dptr2)
!!        call v%simple1DVectorConstructor_data(dptr3)
!!
!!        vec1 => u
!!        vec2 => problem%getStateVector()
!!        call fftOp%BHalf_u%applyOperator(vec1,vec2)
!!
!!        dptr1 => vec2%get1DArrayPtr()
!!        call print('u norm:',sqrt(dot_product(dptr1,dptr1)))
!!
!!        call converter%convertToState(vec2, finalState)
!!        call finalState_ores%writeToFile('u_vec.nc')
!!
!!        call print('size of x vector:',size(dptr1),N*M*nchans)
!!
!!        vec1 => v
!!        vec2 => problem%getStateVector()
!!        call fftOp%BHalf_v%applyOperator(vec1,vec2)
!!        call vec2%scalarMultiply(fftOp%v_factor,vec2)
!!
!!        dptr1 => vec2%get1DArrayPtr()
!!        call print('v norm:',sqrt(dot_product(dptr1,dptr1)))
!!
!!        call converter%convertToState(vec2, finalState)
!!        call finalState_ores%writeToFile('v_vec.nc')
!!
!!        deallocate(u)
!!        deallocate(v)

end program
