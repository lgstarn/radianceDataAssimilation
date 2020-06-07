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

    ! from package io
    use dataArrayReader_mod
    use dataArrayWriter_mod
    use netcdfDataArrayReader_mod
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
    use vertEofOperator_mod
    !use boundedVarBHalfOp_mod
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
    class(SatellitePlatformInfo),        pointer :: platformWithChanSubset

    integer :: time = 1
    class(Atmos3dDataSet),               pointer :: trueAtmosState

    class(RtmOptions),                   pointer :: rtmOpts
    class(SatelliteObservationOperator), pointer :: obsOpRtm
    class(SatelliteObservation),         pointer :: obs_modelRes
    class(DataSet),                      pointer :: obs_dataSet

    class(Atmos3dDataSet),               pointer :: trueState => null()

    class(DataVariable),                 pointer :: columnNormsVar
    class(DataSet),                      pointer :: columnNorms
    class(SatelliteObservation),         pointer :: firstGuess
    class(DataSet),                      pointer :: firstGuess_ds

    class(DataVariable),                 pointer :: latVar
    class(DataVariable),                 pointer :: lonVar

    real(real64), dimension(:,:),        pointer :: veofs

    class(ObservationBundle),            pointer :: obsBundle        => NULL()
    class(ScannedObservationBundle),     pointer :: scannedObsBundle => NULL()

    class(Observation),                  pointer :: obs              => NULL()
    class(ScannedObservation),           pointer :: obs_so           => NULL()
    class(ScannedObservationOperator),   pointer :: obsOpConv        => NULL()

    class(FirstGuesser),                 pointer :: fg

    class(DataVariable),                 pointer :: obsDataVar       => NULL()
    real(real64), dimension(:,:),        pointer :: obsData          => NULL()
    class(DataVariable),                 pointer :: tbVar            => NULL()
    real(real64), dimension(:,:,:),      pointer :: tb               => NULL()
    real(real64), dimension(:),          pointer :: obsErr           => NULL()

    class(NDVarAssimilationStrategy),    pointer :: deconvstrategy
    class(AssimilationStrategy),         pointer :: strategy
    class(Optimizer),                    pointer :: opt

    class(AssimilationProblem),          pointer :: problem

    !class(BoundedVarBHalfOp),            pointer :: fftOp
    class(VertEofOperator),              pointer :: veofOp

    class(DatasetVectorConverter),       pointer :: converter
    class(DeconvolutionConverter),       pointer :: converter_dcnv
    class(AbstractVectorOperator),       pointer :: BHalf
    class(Observer),                     pointer :: obsvr

    class(DataSet),                      pointer :: finalState
    class(SatelliteObservation),         pointer :: finalState_so

    class(DataGrid),                     pointer :: inputGrid  => null()

    real(real64),                        pointer :: initGuess(:)

    class(SatelliteObservation),         pointer :: fgObsInterp
    class(SatelliteObservation),         pointer :: dcObsInterp
    class(SatelliteObservation),         pointer :: trueObsInterp

    class(ObservationSplitter),          pointer :: obsSplitter
    class(ObservationProcessor),         pointer :: obsProcessor
    class(ObservationProcessingChain),   pointer :: obsProcessingChain

    class(DataExtent),    pointer :: mobsExtent  => null()
    class(DataExtent),    pointer :: nobsExtent  => null()
    class(DataExtent),    pointer :: nLociExtent => null()
    class(DataDimension), pointer :: mobsDim     => null()
    class(DataDimension), pointer :: nobsDim     => null()
    class(DataDimension), pointer :: nLociDim    => null()

    class(DataDimension), pointer :: chanDim     => null()
    class(DataExtent),    pointer :: chanExtent  => null()

    class(NetcdfDataArrayReader), pointer :: ncReader   => null()
    class(DataArrayReader),       pointer :: reader     => null()

    class(NetcdfDataArrayWriter), pointer :: ncWriter   => null()
    class(DataArrayWriter),       pointer :: writer     => null()

    integer, pointer :: channelSubset(:)

    integer :: chnum, chseq, ind, nprofile, nodenum, obsi, obsj, pixnum, x, y
    integer :: i, narg, nchans, nctrl, npc, ntokens, platformNumber

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
        write(msgstr,'(A,I4,A)') '    [platform #] is the number of the platform, e.g. "',PLATFORM_TRMM_TMI,&
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
        call endParallel()
        stop
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

    allocate(pinfo)
    call pinfo%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp)

    platform => getSatellitePlatform(platformNumber,obsOpName)

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
    call print('Now loading the platform specific parameters.')

    call load2DRealFile(BHalfFile,npc,nchans,veofs)
    write(msgstr,*) 'Read ',nchans,'chans /',npc,'PCs from the BHalf file ',trim(BHalfFile)
    call print(msgstr)

    if (nchans == 0) then
        call error('Error: could not read the file ' // trim(BHalfFile))
    end if

    call print('Now loading the scannedObsBundle from ' // trim(orbitFile))

    inputGrid => trueAtmosState%getGrid()
    call inputGrid%tile(pinfo)

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

    allocate(obs_modelRes)

    inquire(file = rtmOutputFile, exist=rtmOutExists)
    if (rtmOutExists) then
        call debug('RTM output file ' // trim(rtmOutputFile) // &
            & ' already exists - reusing it.')

        allocate(ncReader)
        call ncReader%netcdfDataArrayReaderConstructor(rtmOutputFile)

        reader => ncReader

        call obs_modelRes%satelliteObservationConstructor(platform,reader)

        call obs_modelRes%loadSatObsFromFile(pinfo)

        write(msgstr,*) '   Loaded the existing RTM file from ',trim(rtmOutputFile)
        call print(msgstr)
    else
        call debug('The RTM output file does not exist - regenerating.')

        call obs_modelRes%satelliteObservationConstructor(platform)

        latVar => trueAtmosState%getVariable(A3D_LAT_VAR)
        lonVar => trueAtmosState%getVariable(A3D_LON_VAR)

        latVar => latVar%clone(copyData=.true.)
        lonVar => lonVar%clone(copyData=.true.)

        if (associated(platform%channelSubset)) then
            chanDim => obs_modelRes%addDimensionByName('Channels',&
                size(platform%channelSubset))
        else
            chanDim => obs_modelRes%addDimensionByName('Channels',platform%mobs)
        end if

        allocate(chanExtent)
        call chanExtent%dataExtentConstructor(chanDim)

        tbVar => obs_modelRes%addVariable(pinfo,'Brightness_Temperatures',tb,&
            chanExtent,latVar%getExtentNumber(1),latVar%getExtentNumber(2))

        tb = -999.

        call obs_modelRes%loadSatelliteObservation_tb3d(pinfo,latVar,lonVar,tbVar)

        call debug('Now setting up the RTM options')

        rtmOpts    => getRtmOptions()
        obsOpRtm   => getSatelliteObservationOperator(obsOpName,rtmOpts,platform)

        call debug('Now running the RTM forward calculations...')

        call doRtmForward(pinfo,platform,trueAtmosState,obsOpRtm,obs_modelRes)

        write(msgstr,*) 'Ran the RTM and saved the output to ',trim(rtmOutputFile)
        call print(msgstr)

        call debug('Now writing the model resolution output to ' // &
            & trim(rtmOutputFile))

        allocate(ncWriter)
        call ncWriter%netcdfDataArrayWriterConstructor(rtmOutputFile)
        writer => ncWriter
        call obs_modelRes%writeSatObsToFile(pinfo,writer)
        deallocate(ncWriter)
    end if

    obs_dataSet => obs_modelRes

    ! now sweep the antenna pattern
    call sweepAntennaPattern(pinfo,obs_dataSet,scannedObsBundle,scannedObsBundle,&
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

    call random_seed()

    ! now add the observation error for noisy observations
    do i=1,scannedObsBundle%getBundleSize()
        obs_so => scannedObsBundle%getScannedObservation(i)

        obsErr     => obs_so%getObservationError()

        obsDataVar => obs_so%getObsDataVar()

        call obsDataVar%getArray(obsData)

        ! add noise to the observation
        do chnum=1,size(obsData,1)
            call addStandardGaussianNoise(size(obsData,2),obsData(chnum,:),&
                obsErr(chnum),0.d0)
        end do

        if (size(obsData,1) /= 0) then
            write(filenum,'(I4)') i+1000
            filename = trim(noisyScannedOutputPrefix) // filenum(2:4) // '.nc'

            allocate(ncWriter)
            call ncWriter%netcdfDataArrayWriterConstructor(filename)
            writer => ncWriter

            call obs_so%writeSatObsToFile(pinfo,writer)
            deallocate(ncWriter)

            write(msgstr,'(A,I0,A,A)') 'Wrote noisy observation bundle number ',i,&
                & ' to ', filename
            call print(msgstr)
        end if
    end do

    ! now get the first guess
    firstGuess_ds => obs_modelRes%clone(shallow=.false.,copyData=.true.)

    fg => getFirstGuesser(ADJOINT_AVE_FG)

    select type (firstGuess_ds)
        class is (SatelliteObservation)
            firstGuess => firstGuess_ds
        class default
            call error('Unknown first guess type')
    end select

    call fg%populateFirstGuess(scannedObsBundle,firstGuess_ds)

    fgOutputFile = trim(fgOutputPrefix) // '_fg.nc'

    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(fgOutputFile)
    writer => ncWriter

    call firstGuess%writeSatObsToFile(pinfo,writer)
    deallocate(ncWriter)

    write(msgstr,*) 'Wrote the first guess to',trim(fgOutputFile)
    call print(msgstr)

    obsDataVar => firstGuess%getObsDataVar()

    call obsDataVar%getArray(obsData)

    allocate(veofOp)
    call veofOp%vertEofOperatorConstructor(1,nchans,npc,veofs)

    allocate(converter_dcnv)
    call converter_dcnv%deconvolutionConverterConstructor(OBS_DATA_VAR_NAME)

    nctrl = npc*size(obsData,2)

    !opt => getOptimizer(LBFGS_OPTIMIZER,nctrl,MAX_ITER)

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
    call problem%assimilationProblemConstructor(pinfo,nctrl,initGuess,opt,converter,obsvr,&
        &firstGuess_ds,bHalf)

    allocate(deconvstrategy)
    call deconvstrategy%nDVarAssimilationStrategyConstructor()

    write(msgstr,*) 'Now beginning deconvolution minimization...'
    call print(msgstr)

    call deconvstrategy%assimilate(pinfo,problem)

    finalState => problem%getBaseDataSet()

    select type (finalState)
        class is (SatelliteObservation)
            finalState_so => finalState
        class default
            call error('Unknown dataset type')
    end select

    deconvolvedOutputFile = trim(deconvolvedOutputPrefix) // '_dc.nc'

    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(deconvolvedOutputFile)
    writer => ncWriter

    call finalState_so%writeSatObsToFile(pinfo,writer)

    deallocate(ncWriter)

    write(msgstr,*) 'Finalized deconvolution and wrote output to ',trim(deconvolvedOutputFile)
    call print(msgstr)

    call endParallel()

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
