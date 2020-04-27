program roundTripTest

    ! from package parallel
    use mpiUtils_mod
    use parallelInfo_mod
    use parallelConstants_mod
    use parallelDecomposition_mod

    ! from package dataset
    use dataSet_mod
    use dataGrid_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataDimension_mod

    use datasetVectorConverter_mod

    use dataArrayWriter_mod
    use netcdfDataArrayWriter_mod

    ! from package vec
    use simple1DVector_mod
    use abstractVectorOperator_mod

    ! from package obs
    use observer_mod
    use observation_mod
    use observationBundle_mod
    use observationOperator_mod
    use observationSplitter_mod
    use observationProcessor_mod
    use observationProcessingChain_mod

    ! from package satellite
    use satelliteObservation_mod
    use satellitePlatformInfo_mod
    use satelliteObservationOperator_mod

    use scannedObservation_mod
    use scannedObservationOperator_mod
    use scannedObservationBundle_mod

    use radianceAssimilationFactory_mod

    use rtmUtils_mod
    use rtmOptions_mod

    ! from package atmos3d
    use atmos3dDataSet_mod
    use atmos3dDataSetFactory_mod
        
    ! from package deconvolve
    use firstGuesser_mod
    use boundedVarBHalfOp_mod
    use deconvolutionFactory_mod
    use deconvolutionConstants_mod
    use deconvolutionConverter_mod

    ! from package math
    use noiseUtils_mod

    ! from package assim
    use assimilationProblem_mod
    use assimilationStrategy_mod
    use assimStrategyFactory_mod

    ! from package ndvar
    use nDVarAssimilationStrategy_mod

    ! from package opt
    use optimizerFactory_mod
    
    ! from package str
    use asciiUtils_mod

    implicit none

    integer, parameter :: MAX_ITER = 1000

    class(ParallelInfo),                 pointer :: pinfo      => null()
    class(ParallelDecomposition),        pointer :: decomp     => null()

    class(SatellitePlatformInfo),        pointer :: platform

    integer :: time = 1
    class(Atmos3dDataSet),               pointer :: trueAtmosState

    class(RtmOptions),                   pointer :: rtmOpts
    class(SatelliteObservationOperator), pointer :: obsOpRtm
    class(SatelliteObservation),         pointer :: obs_modelRes

    class(Atmos3dDataSet),               pointer :: trueState => null()

    class(DataVariable),                 pointer :: columnNormsVar
    class(DataSet),                      pointer :: columnNorms
    class(DataSet),                      pointer :: firstGuess

    real(8), dimension(:,:),             pointer :: veofs

    class(ObservationBundle),            pointer :: obsBundle        => NULL()
    class(ScannedObservationBundle),     pointer :: scannedObsBundle => NULL()

    class(Observation),                  pointer :: obs              => NULL()
    class(ScannedObservation),           pointer :: obs_so           => NULL()
    class(ScannedObservationOperator),   pointer :: obsOpConv        => NULL()

    real(8), dimension(:,:),             pointer :: output           => NULL()

    class(FirstGuesser),                 pointer :: fg

    real(8), dimension(:,:),             pointer :: node_xy      => NULL()
    real(8), dimension(:),               pointer :: node_tb_fg   => NULL()
    real(8), dimension(:),               pointer :: node_tb_dc   => NULL()
    real(8), dimension(:),               pointer :: node_tb_true => NULL()
    real(8), dimension(:,:),             pointer :: latptr       => NULL()
    real(8), dimension(:,:),             pointer :: lonptr       => NULL()

    real(4), dimension(:,:,:),           pointer :: obsTb        => NULL()
    real(8), dimension(:),               pointer :: obsErr       => NULL()

    class(NDVarAssimilationStrategy),    pointer :: deconvstrategy
    class(AssimilationStrategy),         pointer :: strategy
    class(Optimizer),                    pointer :: opt

    class(AssimilationProblem),          pointer :: problem

    class(BoundedVarBHalfOp),            pointer :: fftOp

    class(DatasetVectorConverter),       pointer :: converter
    class(DeconvolutionConverter),       pointer :: converter_dcnv
    class(AbstractVectorOperator),       pointer :: BHalf
    class(Observer),                     pointer :: obsvr
    class(DataSet),                      pointer :: finalState

    class(DataGrid),                     pointer :: inputGrid  => null()

    real(8),               dimension(:), pointer :: initGuess

    class(SatelliteObservation),         pointer :: fgObsInterp
    class(SatelliteObservation),         pointer :: dcObsInterp
    class(SatelliteObservation),         pointer :: trueObsInterp

    class(ObservationSplitter),          pointer :: obsSplitter
    class(ObservationProcessor),         pointer :: obsProcessor
    class(ObservationProcessingChain),   pointer :: obsProcessingChain

    class(DataExtent),    pointer :: xExtent    => null()
    class(DataExtent),    pointer :: yExtent    => null()
    class(DataDimension), pointer :: pixDim     => null()
    class(DataDimension), pointer :: scanDim    => null()

    class(SatelliteObservation), pointer :: satObs     => null()
    class(DataSet),              pointer :: satObs_ds  => null()

    class(NetcdfDataArrayWriter), pointer :: ncWriter   => null()
    class(DataArrayWriter),       pointer :: writer     => null()

    integer, pointer :: channelSubset(:)

    integer :: chnum, chseq, ind, nprofile, nodenum, obsi, obsj, x, y
    integer :: i, narg, nchans, nctrl, npc, ntokens, platformNumber

    real(8), dimension(1)   :: tbi_fg, tbi_dc, tbi_true
    real(8), dimension(2,1) :: loctb

    character(len=4096) :: cmdlineArg
    character(len=1024) :: modelInputFile
    character(len=256)  :: modelName
    character(len=256)  :: platformNumberName
    character(len=256)  :: obsOpName
    character(len=1024) :: orbitFile
    character(len=1024) :: bhalfFile
    character(len=1024) :: rtmOutputFile
    character(len=1024) :: noNoiseScannedOutputPrefix
    character(len=1024) :: noisyScannedOutputPrefix
    character(len=1024) :: fgOutputPrefix
    character(len=1024) :: deconvolvedOutputPrefix

    character(len=1024) :: fgOutputFile
    character(len=1024) :: columnNormOutputFile
    character(len=1024) :: deconvolvedOutputFile

    character(len=4)    :: filenum
    character(len=256)  :: filename

    logical :: rtmOutExists

    call initParallel()

    narg = command_argument_count()

    if (narg .ne. 11) then
        call print('Usage:')
        call get_command_argument(0, cmdlineArg)
        write(msgstr,*) '    ',adjustl(trim(cmdlineArg)),' [model input file] [model string] [platform #] ',&
            &'[obs op string] [satellite orbit file] [BHalf file]'
        call print(msgstr)
        call print('         [RTM output file] [no noise obs output prefix] [noisy obs output prefix] ' // &
            &'[fg output prefix] [deconvolved output prefix]')
        call newline()
        call print('where')
        call newline()
        call print('    [model input file] is the model file to read (must be of type [model string])')
        call print('    [model string] is the code for the state, e.g. "' // trim(WRF_ARW_MODEL) // &
            '" for the WRF ARW model (see radianceAssimilationFactory.f)')
        write(msgstr,'(A,I4,A)') '     [platform #] is the number of the platform, e.g. "',PLATFORM_TRMM_TMI,&
            '" for TRMM/TMI'
        call print(msgstr)
        call print('    [obs op string] is the code for the observation operator, e.g. "' // &
            trim(CRTM_OBS_OP) // '" for the Community Radiative Transfer Model (see radianceAssimilationFactory.f)')
        call print('    [satellite orbit file] is the orbit file to use for convolving/deconvolving')
        call print('    [RTM output file] is the location to output the RTM (truth) pre-convolution TBs')
        call print('    [BHalf file] is the square-root of the error covariance to use for deconvolution')
        call print('    [no noise obs output file] is the location to output the pre-noise convolved TBs')
        call print('    [noisy obs output prefix] is the location to output the post-noise convolved TBs')
        call print('    [fg output prefix] is the location to output the first guess')
        call print('    [deconvolved output file] is the location to output the final deconvolved solution')
        call error('')
        call abortParallel()
    end if

    call get_command_argument(1,  modelInputFile)
    call get_command_argument(2,  modelName)
    call get_command_argument(3,  platformNumberName)
    call get_command_argument(4,  obsOpName)
    call get_command_argument(5,  orbitFile)
    call get_command_argument(6,  bhalfFile)
    call get_command_argument(7,  rtmOutputFile)
    call get_command_argument(8,  noNoiseScannedOutputPrefix)
    call get_command_argument(9,  noisyScannedOutputPrefix)
    call get_command_argument(10, fgOutputPrefix)
    call get_command_argument(11, deconvolvedOutputPrefix)

    read( platformNumberName, '(i10)' ) platformNumber

    allocate(decomp)
    call decomp%parallelDecompositionConstructor(WEST_EAST_DIM_NAME,SOUTH_NORTH_DIM_NAME)

    call debug('Now setting the parallel info')

    allocate(pinfo)
    call pinfo%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp)

    call newline()
    call newline()
    call newline()
    call print('----------------------------------------------------')
    call print('Running round-trip error test.')
    call print('   Input file:        ' // trim(modelInputFile))
    call print('   Model type:        ' // trim(modelName))
    call print('   Platform:          ' // trim(platform%platformName) // ' (sensor name: ' // trim(platform%sensorName) // ')')
    call print('   Obs op:            ' // trim(obsOpName))
    call print('   Orbit file:        ' // trim(orbitFile))
    call print('   BHalf file:        ' // trim(bhalfFile))
    call print('   RTM output file:   ' // trim(rtmOutputFile))
    call print('   No noise obs:      ' // trim(noNoiseScannedOutputPrefix))
    call print('   Noise obs:         ' // trim(noisyScannedOutputPrefix))
    call print('   First guess:       ' // trim(fgOutputPrefix))
    call print('   Deconvolution:     ' // trim(deconvolvedOutputPrefix))
    call print('----------------------------------------------------')
    call newline()
    call newline()
    call newline()
    call print('----------------------------------------------------')
    call print('Now loading the ' // trim(modelName) // ' model file ' // trim(modelInputFile))

    trueAtmosState => getAtmos3dDataSet(pinfo,modelName,time,modelInputFile)

    call print('----------------------------------------------------')
    call newline()
    call newline()
    call newline()
    call print('----------------------------------------------------')
    call print('Now running the ' // trim(obsOpName) // ' forward radiative transfer.')
    call newline()

    inquire(file = rtmOutputFile, exist=rtmOutExists)
!    if (rtmOutExists) then
!        call obs_modelRes%satelliteObservationConstructor_load(platform,null(),null(),&
!            trueAtmosState%getNX()*trueAtmosState%getNY(),rtmOutputFile)
!        write(msgstr,*) '   Loaded the existing RTM file from ',trim(rtmOutputFile)
!        call print(msgstr)
!    else
!        call obs_modelRes%satelliteObservationConstructor_empty(platform,null(),null(),&
!            trueAtmosState%getNX()*trueAtmosState%getNY())
!        call doRtmForward(platform,trueAtmosState,obsOp,obs_modelRes,rtmOutputFile)
!        write(msgstr,*) '   Ran the RTM and saved the output to ',trim(rtmOutputFile)
!        call print(msgstr)
!    end if

    call print('----------------------------------------------------')
    call newline()
    call newline()
    call newline()
    call print('----------------------------------------------------')
    call print('Now loading the platform specific parameters.')

    platform => getSatellitePlatform(platformNumber,obsOpName)

    call load2DRealFile(BHalfFile,npc,nchans,veofs)
    write(msgstr,*) 'Read ',nchans,'chans /',npc,'PCs from the BHalf file ',trim(BHalfFile)
    call print(msgstr)

    if (nchans == 0) then
        call error('Error: could not read the file ' // trim(BHalfFile))
    end if

    call print('Now loading the scannedObsBundle from ' // trim(orbitFile))

    allocate(obsProcessingChain)
    call obsProcessingChain%observationProcessingChainConstructor()

    allocate(obsSplitter)
    call obsSplitter%observationSplitterConstructor(inputGrid,(/SO_LAT_DIM,SO_LON_DIM/))

    obsProcessor => obsSplitter

    call obsProcessingChain%addProcessor(obsProcessor)

    scannedObsBundle => getScannedObservationBundle(pinfo,orbitFile,inputGrid,     &
        platformNumber,obsProcessingChain,minGoodRatio=0.7d0,obsErrInflation=1.0d0) !,&
        !columnNorms=columnNormsVar)

    call print('Loaded the scannedObsBundle from ' // trim(orbitFile))
    call debug('Now setting the channel subset for platform ' // trim(platformNumberName))

    nchans = scannedObsBundle%getTotalNChannels(scannedObsBundle%getBundleSize())

    allocate(channelSubset(nchans))

    do chnum=1,nchans
        channelSubset(chnum) = scannedObsBundle%getFullSetChanNum(chnum)
    end do

    platform   => getSatellitePlatform(platformNumber,obsOpName,channelSubset=channelSubset)

    call debug('Created the satellite platform for ' // trim(platform%platformName))

    xExtent => trueAtmosState%getWestEastExtent()
    yExtent => trueAtmosState%getSouthNorthExtent()

    ! now creating "pixel" and "scan" dimensions from the x and y model dimensions
    pixDim  => xExtent%getDimension()
    pixDim  => pixDim%clone()
    scanDim => yExtent%getDimension()
    scanDim => scanDim%clone()

    call  pixDim%setName(PIXELS_DIM_NAME)
    call scanDim%setName(SCANS_DIM_NAME)

    allocate(obs_modelRes)
    call obs_modelRes%satelliteObservationConstructor(platform)
    call obs_modelRes%addDimension(pixDim)
    call obs_modelRes%addDimension(scanDim)
    call obs_modelRes%loadSatelliteObservation(pinfo,pixDim,scanDim, &
        & trueAtmosState%getVariable2D(A3D_LAT_VAR),&
        & trueAtmosState%getVariable2D(A3D_LON_VAR))

    call debug('Now setting up the RTM options')

    rtmOpts    => getRtmOptions()
    obsOpRtm   => getSatelliteObservationOperator(obsOpName,rtmOpts,platform)

    call debug('Now running the RTM forward calculations...')

    call doRtmForward(pinfo,platform,trueAtmosState,obsOpRtm,satObs)

    call barrier(pinfo%getCommunicator())

    call debug('Successfully completed the RTM forward calculations.')

    call debug('Now writing the model resolution output to ' // &
        & trim(rtmOutputFile))

    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(rtmOutputFile)
    writer => ncWriter
    call satObs%writeSatObsToFile(pinfo,writer,.true.)
    deallocate(ncWriter)

    satObs_ds => satObs

    ! now sweep the antenna pattern
    call sweepAntennaPattern(pinfo,satObs_ds,scannedObsBundle,scannedObsBundle,&
        noNoiseScannedOutputPrefix)

!    allocate(columnNorms)
!    call columnNorms%dataSetConstructor()
!    call columnNorms%addDimension(pixDim)
!    call columnNorms%addDimension(scanDim)
!    call columnNorms%addVariable(pinfo,'Column_Norms',cnorms,pixDim,scanDim)
!
!    do i=1,scannedObsBundle%getBundleSize()
!        obs_so  => scannedObsBundle%getScannedObservation(i)
!
!        obsErr => obs_so%getObservationError()
!
!        do chnum=1,obs_so%getMObs()
!            chseq = chnum+obs_so%getUniqueChannelOffset()
!            columnNorms_mres%data3d(:,:,chseq) = columnNorms_mres%data3d(:,:,chseq)/obsErr(chnum)
!        end do
!    end do
!
!    columnNormOutputFile = trim(deconvolvedOutputPrefix) // '_colNorms.nc'
!
!    call columnNorms_mres%writeToFile(columnNormOutputFile,'ColumnNorms')
!
!    write(msgstr,*) 'Wrote the column norms file to',trim(columnNormOutputFile)
!    call print(msgstr)

!    ! copy the data from the model-res obs to the model state as part of the round-trip
!    nprofile = 0
!    do x=1,trueState_mres%getNX()
!        do y=1,trueState_mres%getNY()
!            nprofile = nprofile + 1
!            do chnum=1,nchans
!                chseq = scannedObsBundle%getFullSetChanNum(chnum)
!                trueState_mres%data3d(x,y,chnum) = obs_modelRes%obsData(chseq,nprofile)
!            end do
!        end do
!    end do

    call random_seed()

    ! now run the antenna convolution to get the obs-res synthetic obs
!    do i=1,scannedObsBundle%getBundleSize()
!        obs_so    => scannedObsBundle%getScannedObservation(i)
!        obs       => obs_so
!        obsOpConv => scannedObsBundle%getAntennaPatternObsOp(i)
!
!        allocate(output(obs%getMObs(),obs%getNObs()))
!        call obsOpConv%forward(trueState, obs, output)
!
!        obsErr => obs_so%getObservationError()
!
!        ! add noise to the observation
!        do chnum=1,obs%getMObs()
!            call addGaussianNoise(obs%getNObs(),obs_so%obsData(chnum,:),obsErr(chnum),0.d0)
!        end do
!
!        write(filenum,'(I4)') i+1000
!        fileName = trim(noNoiseScannedOutputPrefix) //filenum(2:4)
!
!        call obs_so%writeToFile(fileName)
!
!        obsTb => obs_so%getTB()
!
!        ! copy the data back into the observation
!        do ind=1,obs%getNObs()
!            obsi = aint(obs%obsLoci(SO_PIX_DIM,ind))
!            obsj = aint(obs%obsLoci(SO_SCAN_DIM,ind))
!            obsTb(:,obsi,obsj) = output(:,ind)
!            obs_so%obsData(:,ind)  = output(:,ind)
!        end do
!
!        filename = trim(noisyScannedOutputPrefix) //filenum(2:4)
!
!        call obs_so%writeToFile(fileName)
!
!        deallocate(output)
!        nullify(output)
!    end do

!    write(msgstr,*) 'Wrote the ',scannedObsBundle%getBundleSize(),' noise-free observation bundles to',&
!        &trim(noNoiseScannedOutputPrefix)
!    call print(msgstr)
!    write(msgstr,*) 'Wrote the ',scannedObsBundle%getBundleSize(),' noisy observation bundles to',&
!        &trim(noNoiseScannedOutputPrefix)
!    call print(msgstr)

! the rest is real

!    ! now get the first guess
!    allocate(firstGuess_mres)
!    call firstGuess_mres%modelResGriddedTbDataSetConstructor(trueAtmosState,nchans,&
!        null(),null())
!
!    fg => getFirstGuesser(ADJOINT_AVE_FG)
!    firstGuess_gtb => firstGuess_mres
!
!    call fg%populateFirstGuess(scannedObsBundle,firstGuess_gtb)
!
!    fgOutputFile = trim(fgOutputPrefix) // '_fg.nc'
!
!    call firstGuess_gtb%writeToFile(fgOutputFile,'TB_FirstGuess')
!
!    write(msgstr,*) 'Wrote the first guess to',trim(fgOutputFile)
!    call print(msgstr)
!
!    latptr => firstGuess_gtb%getLatPtr()
!    lonptr => firstGuess_gtb%getLonPtr()
!
!    allocate(problem)
!
!    allocate(fftOp)
!
!    call fftOp%boundedVarBHalfOpConstructor(firstGuess_gtb%getNX(),firstGuess_gtb%getNY(),&
!        nchans,npc,veofs)
!
!    nctrl = fftOp%getNumControl()
!    opt => getOptimizer(LBFGS_OPTIMIZER,nctrl,MAX_ITER)
!
!    allocate(initGuess(nctrl))
!    initGuess = 0.
!
!    allocate(converter_dcnv)
!
!    converter => converter_dcnv
!
!    allocate(obsvr)
!    call obsvr%observerConstructor()
!
!    obsBundle => scannedObsBundle
!    call obsvr%addObservationBundle(obsBundle)
!
!    bHalf => fftOp
!
!    firstGuess => firstGuess_gtb
!
!    call problem%assimilationProblemConstructor(nctrl,initGuess,opt,converter,obsvr,&
!        &firstGuess,bHalf)
!
!    allocate(deconvstrategy)
!    call deconvstrategy%nDVarAssimilationStrategyConstructor()
!
!    write(msgstr,*) 'Now beginning deconvolution minimization...'
!    call print(msgstr)
!
!    call deconvstrategy%assimilate(problem)
!
!    finalState => problem%getBaseDataSet()
!
!    select type(finalState)
!        class is (GriddedTbDataSet)
!            finalState_gtb => finalState
!        class default
!            write(msgstr,*) 'Unknown model state in roundTripTest base state.'
!            call error(msgstr)
!    end select
!
!    deconvolvedOutputFile = trim(deconvolvedOutputPrefix) // '_dc.nc'
!
!    call finalState_gtb%writeToFile(deconvolvedOutputFile,'TB_Deconv')
!
!    write(msgstr,*) 'Finalized deconvolution and wrote output to ',trim(deconvolvedOutputFile)
!    call print(msgstr)
!
!    ! now interpolate the first guess, deconv, and true state to the observations
!    do i=1,scannedObsBundle%getBundleSize()
!        obs_so  => scannedObsBundle%getScannedObservation(i)
!
!        allocate(fgObsInterp)
!        call fgObsInterp%satelliteObservationConstructor_empty(obs_so%satellitePlatform,&
!            &null(),null(),obs_so%getNPassesQC(mstart=1,mend=obs_so%getMObs()))
!
!        allocate(dcObsInterp)
!        call dcObsInterp%satelliteObservationConstructor_empty(obs_so%satellitePlatform,&
!            &null(),null(),obs_so%getNPassesQC(mstart=1,mend=obs_so%getMObs()))
!
!        allocate(trueObsInterp)
!        call trueObsInterp%satelliteObservationConstructor_empty(obs_so%satellitePlatform,&
!            &null(),null(),obs_so%getNPassesQC(mstart=1,mend=obs_so%getMObs()))
!
!        do chnum=1,obs_so%getMObs()
!            chseq = chnum+obs_so%getUniqueChannelOffset()
!
!            nodenum = count(firstGuess_gtb%data3d(:,:,chseq) > 0 .and. finalState_gtb%data3d(:,:,chseq) > 0)
!
!            allocate(node_xy(2,nodenum))
!
!            allocate(node_tb_fg(nodenum))
!            allocate(node_tb_dc(nodenum))
!            allocate(node_tb_true(nodeNum))
!
!            ind = 0
!
!            do x=1,firstGuess_gtb%getNX()
!                do y=1,firstGuess_gtb%getNY()
!                    if (firstGuess_gtb%data3d(x,y,chseq) > 0 .and. &
!                       &finalState_gtb%data3d(x,y,chseq) > 0) then
!
!                        ind = ind + 1
!                        node_xy(1,ind) = latptr(x,y)
!                        node_xy(2,ind) = lonptr(x,y)
!
!                        node_tb_fg(ind)   = firstGuess_gtb%data3d(x,y,chseq)
!                        node_tb_dc(ind)   = finalState_gtb%data3d(x,y,chseq)
!                        node_tb_true(ind) =  trueState_gtb%data3d(x,y,chseq)
!                    end if
!                end do
!            end do
!
!            allocate(tri)
!            call tri%triangulationConstructor(nodeNum,node_xy)
!
!            nprofile = 0
!            do ind=1,obs_so%getNObs()
!                if (obs_so%passesQC(1,ind,obs_so%getMObs())) then
!                    loctb(1,1) = obs_so%obsLoci(SO_LAT_DIM,ind)     ! lat
!                    loctb(2,1) = obs_so%obsLoci(SO_LON_DIM,ind)     ! lon
!                    obsi       = obs_so%obsLoci(SO_PIX_DIM,ind)     ! i
!                    obsj       = obs_so%obsLoci(SO_SCAN_DIM,ind)    ! j
!
!                    call tri%scatteredInterpolation(node_tb_fg,   1, loctb, tbi_fg)
!                    call tri%scatteredInterpolation(node_tb_dc,   1, loctb, tbi_dc)
!                    call tri%scatteredInterpolation(node_tb_true, 1, loctb, tbi_true)
!
!                    if (tbi_true(1) > 0 .and. tbi_fg(1) > 0 .and. tbi_dc(1) > 0) then
!                        nprofile = nprofile + 1
!                        fgObsInterp%obsLoci(SO_LAT_DIM,nprofile)  = loctb(1,1)
!                        fgObsInterp%obsLoci(SO_LON_DIM,nprofile)  = loctb(2,1)
!                        fgObsInterp%obsLoci(SO_PIX_DIM,nprofile)  = obsi
!                        fgObsInterp%obsLoci(SO_SCAN_DIM,nprofile) = obsj
!                        fgObsInterp%obsData(chnum,nprofile) = tbi_fg(1)
!
!                        dcObsInterp%obsLoci(SO_LAT_DIM,nprofile)  = loctb(1,1)
!                        dcObsInterp%obsLoci(SO_LON_DIM,nprofile)  = loctb(2,1)
!                        dcObsInterp%obsLoci(SO_PIX_DIM,nprofile)  = obsi
!                        dcObsInterp%obsLoci(SO_SCAN_DIM,nprofile) = obsj
!                        dcObsInterp%obsData(chnum,nprofile) = tbi_dc(1)
!
!                        trueObsInterp%obsLoci(SO_LAT_DIM,nprofile)  = loctb(1,1)
!                        trueObsInterp%obsLoci(SO_LON_DIM,nprofile)  = loctb(2,1)
!                        trueObsInterp%obsLoci(SO_PIX_DIM,nprofile)  = obsi
!                        trueObsInterp%obsLoci(SO_SCAN_DIM,nprofile) = obsj
!                        trueObsInterp%obsData(chnum,nprofile) = tbi_true(1)
!                    end if
!                end if
!            end do
!
!            deallocate(tri)
!            deallocate(node_tb_fg)
!            deallocate(node_tb_dc)
!            deallocate(node_tb_true)
!
!            nullify(tri)
!            nullify(node_tb_fg)
!            nullify(node_tb_dc)
!            nullify(node_tb_true)
!        end do
!
!        write(filenum,'(I4)') i+1000
!        filename = trim(fgOutputPrefix) //'_fgObsInterp_'//filenum(2:4)
!        call fgObsInterp%writeToFile(fileName)
!
!        write(msgstr,*) 'Wrote first guess interpolated to obs locations to ',trim(filename)
!        call print(msgstr)
!
!        filename = trim(deconvolvedOutputPrefix) //'_dcObsInterp_'//filenum(2:4)
!        call dcObsInterp%writeToFile(fileName)
!
!        write(msgstr,*) 'Wrote deconvolution interpolated to obs locations to ',trim(filename)
!        call print(msgstr)
!
!        filename = trim(deconvolvedOutputPrefix) //'_truthObsInterp_'//filenum(2:4)
!        call trueObsInterp%writeToFile(fileName)
!
!        write(msgstr,*) 'Wrote truth interpolated to obs locations to ',trim(filename)
!        call print(msgstr)
!
!        deallocate(fgObsInterp)
!        deallocate(dcObsInterp)
!        deallocate(trueObsInterp)
!    end do

!        allocate(background_ave_mres)
!        allocate(background_interp_mres)
!        ! only using the domain of the true state here
!        call background_ave_mres%modelResGriddedTbDataSetConstructor(trueAtmosState,nchans)
!        ! only using the domain of the true state here
!        call background_interp_mres%modelResGriddedTbDataSetConstructor(trueAtmosState,nchans)
!        allocate(tmpState_mres)
!        ! only using the domain of the true state here
!        call tmpState_mres%modelResGriddedTbDataSetConstructor(trueAtmosState,nchans)

!        background_ave_gtb => background_ave_mres
!        background_interp_gtb => background_interp_mres
!        tmpState_gtb   => tmpState_mres
!
!        lat => trueState_mres%getLatPtr()
!        lon => trueState_mres%getLonPtr()
!
!        background_ave => background_ave_gtb
!        background_interp => background_interp_gtb
!        tmpState => tmpState_gtb
!

!        call background_interp_mres%writeToFile("testSsmisObs_interp.nc")
!        stop

!        call antennaObsOp1%adjoint(background_ave, obs1, output, background_ave)
!
!        output = 1.d0
!        call antennaObsOp1%adjoint(background_ave, obs1, output, tmpState)
!
!        deallocate(output)
!
!        allocate(output(ssmisObs2%nchannels,ssmisObs2%getNObs()))
!
!        call antennaObsOp2%adjoint(background_ave, obs2, output, background_ave)
!
!        output = 1.d0
!        call antennaObsOp2%adjoint(background_ave, obs2, output, tmpState)
!
!        deallocate(output)
!
!        allocate(output(ssmisObs3%nchannels,ssmisObs3%getNObs()))
!        call antennaObsOp3%forward(trueState, obs3, output)
!
!        open(unit=971,file="ssmisObs3_obs.txt")
!        do nprofile=1,ssmisObs3%getNObs()
!            write(971,*) ssmisObs3%obsLoci(:,nprofile),output(:,nprofile)
!        end do
!        close(971)
!
!        call antennaObsOp3%adjoint(background_ave, obs3, output, background_ave)
!
!        output = 1.d0
!        call antennaObsOp3%adjoint(background_ave, obs3, output, tmpState)
!
!        deallocate(output)
!
!        allocate(output(ssmisObs4%nchannels,ssmisObs4%getNObs()))
!        call antennaObsOp4%forward(trueState, obs4, output)
!
!        open(unit=971,file="ssmisObs4_obs.txt")
!        do nprofile=1,ssmisObs4%getNObs()
!            write(971,*) ssmisObs4%obsLoci(:,nprofile),output(:,nprofile)
!        end do
!        close(971)
!
!        call antennaObsOp4%adjoint(background_ave, obs4, output, background_ave)
!
!        output = 1.d0
!        call antennaObsOp4%adjoint(background_ave, obs4, output, tmpState)
!
!        deallocate(output)
!
!        where (tmpState_gtb%data3d > 0.d0) background_ave_gtb%data3d = background_ave_gtb%data3d/tmpState_gtb%data3d
!        call background_ave_gtb%writeToFile("testSsmisObs_ave.nc")
!        stop
end program
