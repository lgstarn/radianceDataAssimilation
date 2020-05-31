program runRtmForwardWithSweep

    use parallelInfo_mod
    use parallelConstants_mod
    use parallelDecomposition_mod

    use iso_fortran_env

    use atmos3dDataSet_mod
    use atmos3dDataSetFactory_mod

    use rtmUtils_mod
    use rtmOptions_mod

    use radianceAssimilationFactory_mod

    use satelliteObservation_mod
    use satelliteObservationOperator_mod
    use satellitePlatformInfo_mod

    use dataSet_mod
    use dataGrid_mod
    use dataExtent_mod
    use dataDimension_mod
    use dataArrayWriter_mod
    use netcdfDataArrayWriter_mod

    use obsQCCodes_mod
    use observation_mod
    use observationSplitter_mod
    use observationProcessor_mod

    use mpiUtils_mod

    implicit none

    integer                       :: narg
    character(len=:), allocatable :: cmdlineArg

    character(len=:), allocatable :: inputFile
    character(len=:), allocatable :: outputPrefix
    character(len=:), allocatable :: platformNumberName
    character(len=:), allocatable :: modelName
    character(len=:), allocatable :: obsOpName
    character(len=:), allocatable :: orbitFile

    integer          :: time = 1
    integer          :: platformNumber
    integer          :: chnum
    integer          :: nchans
    integer, pointer :: channelSubset(:)

    logical          :: newObj

    class(ParallelInfo),                 pointer :: pinfo      => null()
    class(ParallelDecomposition),        pointer :: decomp     => null()

    class(Atmos3dDataSet),               pointer :: modelState => null()

    class(DataGrid),                     pointer :: inputGrid  => null()

    class(ObservationSplitter),          pointer :: obsSplitter
    class(ObservationProcessor),         pointer :: obsProcessor
    class(ObservationProcessingChain),   pointer :: obsProcessorChain

    class(ScannedObservationBundle),     pointer :: scannedObsBundle => NULL()

    class(SatellitePlatformInfo),        pointer :: platform => null()

    class(SatelliteObservation),         pointer :: satObs     => null()
    class(SatelliteObservationOperator), pointer :: obsOpRtm   => null()

    class(DataDimension), pointer :: chanDim     => null()
    class(DataExtent),    pointer :: chanExtent  => null()

    real(real64), pointer :: tb(:,:,:)

    class(DataVariable), pointer :: latVar
    class(DataVariable), pointer :: lonVar
    class(DataVariable), pointer :: tbVar

    class(RtmOptions),    pointer :: rtmOpts    => null()

    class(DataSet),       pointer :: satObs_ds  => null()

    class(NetcdfDataArrayWriter), pointer :: ncWriter   => null()
    class(DataArrayWriter),       pointer :: writer     => null()

    class(Observation),   pointer :: satObs_o
    class(Observation),   pointer :: satObs_split

    integer :: nchan

    call initParallel()

    narg = command_argument_count()

    if (narg .lt. 6) then
        call print('Usage:')
        call getArg(0, cmdlineArg)
        call print('    ' // adjustl(trim(cmdlineArg)) // ' [inputFile] [outputPrefix] [model string] [platform #] ' // &
                                                 ' [obs op string] [orbit file]')
        call print('')
        call print('where')
        call print('')
        call print('    [inputFile] is the model file to read (must be of type [model string])')
        call print('    [outputPrefix] is the prefix of the output (one file will be generated per posting)')
        call print('    [model string] is the code for the state, e.g. "' // trim(WRF_ARW_MODEL) // &
            '" for the WRF ARW model (see radianceAssimilationFactory.f03)')
        call print('    [platform #] is the number of the platform, e.g. "' // int2str(PLATFORM_TRMM_TMI) // &
            '" for TRMM/TMI')
        call print('    [obs op string] is the code for the observation operator, e.g. "' // &
            trim(CRTM_OBS_OP) // '" for the Community Radiative Transfer Model (see radianceAssimilationFactory.f)')
        call print('    [orbit file] is the orbit file to utilize for the satellite geometry.')

        call endParallel()
        return
    end if

    call getArg(1, inputFile)
    call getArg(2, outputPrefix)
    call getArg(3, modelName)
    call getArg(4, platformNumberName)
    call getArg(5, obsOpName)
    call getArg(6, orbitFile)

    read( platformNumberName, '(i10)' ) platformNumber

    call print('Running obs op ' // trim(obsOpName) // ' and platform number ' // &
        trim(platformNumberName) // ' for model type ' // trim(modelName))
    call print('    on input file ' // trim(inputFile))
    call print('    and outputting the results to files beginning with the prefix ' // trim(outputPrefix))
    call print('    for the orbit file: '// trim(orbitFile))

    call debug('Now setting the parallel decomposition')

    allocate(decomp)
    call decomp%parallelDecompositionConstructor(WEST_EAST_DIM_NAME,SOUTH_NORTH_DIM_NAME)

    call debug('Now setting the parallel info')

    allocate(pinfo)
    call pinfo%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp)

    call debug('Now loading the data set from ' // trim(inputFile))

    modelState => getAtmos3dDataSet(pinfo,modelName,time,inputFile)

    call debug('Now creating the obs processor and splitter')

    allocate(obsProcessorChain)
    call obsProcessorChain%observationProcessingChainConstructor()

    inputGrid => modelState%getGrid()

    call inputGrid%tile(pinfo)

    allocate(obsSplitter)
    call obsSplitter%observationSplitterConstructor(inputGrid,(/SO_LAT_DIM,SO_LON_DIM/))

    obsProcessor => obsSplitter

    call obsProcessorChain%addProcessor(obsProcessor)

    call debug('Now loading the scanned obs bundle')

    scannedObsBundle => getScannedObservationBundle(pinfo,orbitFile,inputGrid,&
        &platformNumber,obsProcessorChain)

    call debug('Now setting the channel subset for platform ' // trim(platformNumberName))

    nchans = scannedObsBundle%getTotalNChannels(scannedObsBundle%getBundleSize())

    allocate(channelSubset(nchans))

    do chnum=1,nchans
        channelSubset(chnum) = scannedObsBundle%getFullSetChanNum(chnum)
    end do

    platform   => getSatellitePlatform(platformNumber,obsOpName,channelSubset=channelSubset)

    call debug('Created the satellite platform for ' // trim(platform%platformName))

    ! now creating "pixel" and "scan" dimensions from the x and y model dimensions
    allocate(satObs)
    call satObs%satelliteObservationConstructor(platform)

    latVar => modelState%getVariable(A3D_LAT_VAR)
    lonVar => modelState%getVariable(A3D_LON_VAR)

    latVar => latVar%clone(copyData=.true.)
    lonVar => lonVar%clone(copyData=.true.)

    if (associated(platform%channelSubset)) then
        nchan = size(platform%channelSubset)
    else
        nchan = platform%mobs
    end if

    chanDim => satObs%addDimensionByName('Channels',nchan)
    allocate(chanExtent)
    call chanExtent%dataExtentConstructor(chanDim)

    tbVar => satObs%addVariable(pinfo,'Brightness_Temperatures',tb,&
        chanExtent,latVar%getExtentNumber(1),latVar%getExtentNumber(2))

    tb = -999.

    call satObs%loadSatelliteObservation_tb3d(pinfo,latVar,lonVar,tbVar)

    call debug('Now setting up the RTM options')

    rtmOpts    => getRtmOptions()
    obsOpRtm   => getSatelliteObservationOperator(obsOpName,rtmOpts,platform)

    call debug('Now running the RTM forward calculations...')

    call doRtmForward(pinfo,platform,modelState,obsOpRtm,satObs)

    call barrier(pinfo%getCommunicator())

    call debug('Successfully completed the RTM forward calculations.')

    call debug('Now writing the model resolution output to ' // &
        & trim(outputPrefix) // '_mr.nc')

    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(trim(outputPrefix) // '_mr.nc')
    writer => ncWriter
    call satObs%writeSatObsToFile(pinfo,writer)
    deallocate(ncWriter)

    satObs_ds => satObs

    ! now sweep the antenna pattern
    call sweepAntennaPattern(pinfo,satObs_ds,scannedObsBundle,scannedObsBundle,outputPrefix)

    deallocate(decomp)
    deallocate(pinfo)
    deallocate(obsProcessorChain)
    ! FIXME: deallocating model state is causing a "munmap_chunk(): invalid pointer" error
    !deallocate(modelState)
    deallocate(rtmOpts)
    deallocate(obsOpRtm)
    deallocate(platform)
    deallocate(satObs)

    call barrier()
    call endParallel()
end program
