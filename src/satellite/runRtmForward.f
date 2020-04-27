program runRtmForward

    use iso_fortran_env

    use atmos3dDataSet_mod
    use atmos3dDataSetFactory_mod

    use rtmUtils_mod
    use rtmOptions_mod

    use radianceAssimilationFactory_mod

    use satelliteObservationOperator_mod
    use satellitePlatformInfo_mod
    use satelliteObservation_mod

    use netcdfDataArrayReader_mod
    use dataArrayReader_mod
    use netcdfDataArrayWriter_mod
    use dataArrayWriter_mod

    use dataExtent_mod
    use dataDimension_mod
    use dataVariable_mod

    use parallelConstants_mod
    use parallelInfo_mod
    use parallelDecomposition_mod

    use mpiUtils_mod

    implicit none

    character(len=:), allocatable :: inputFile
    character(len=:), allocatable :: outputFile
    character(len=:), allocatable :: platformNumberName
    character(len=:), allocatable :: modelName
    character(len=:), allocatable :: obsOpName

    class(Atmos3dDataSet),               pointer :: modelState => null()

    class(SatelliteObservation),         pointer :: satObs     => null()
    class(SatelliteObservationOperator), pointer :: obsOp      => null()

    class(ParallelInfo),                 pointer :: pinfo      => null()
    class(ParallelDecomposition),        pointer :: decomp     => null()

    class(NetcdfDataArrayWriter),        pointer :: ncWriter   => null()
    class(DataArrayWriter),              pointer :: writer     => null()

    class(DataExtent),    pointer :: xExtent    => null()
    class(DataExtent),    pointer :: yExtent    => null()
    class(DataDimension), pointer :: pixDim     => null()
    class(DataDimension), pointer :: scanDim    => null()
    class(DataDimension), pointer :: chanDim    => null()

    class(DataVariable),  pointer :: tbVar      => null()
    class(DataVariable),  pointer :: latVar     => null()
    class(DataVariable),  pointer :: lonVar     => null()

    integer :: time = 1
    integer :: platformNumber

    class(RtmOptions), pointer :: rtmOpts => null()

    integer :: narg
    character(len=1024) :: cmdlineArg

    integer :: nprofile, x, y

    class(SatellitePlatformInfo), pointer :: platform => null()

    call initParallel()

    narg = command_argument_count()

    if (narg .lt. 5) then
        call print('Usage:')
        call get_command_argument(0, cmdlineArg)
        call print('    ' // adjustl(trim(cmdlineArg)) // ' [inputFile] [outputFile] [model string] [platform #] ' // &
                                                 ' [obs op string]')
        call print('')
        call print('where')
        call print('')
        call print('    [inputFile] is the model file to read (must be of type [model string])')
        call print('    [outputFile] is the location to place the output from the observation operator')
        call print('    [model string] is the code for the state, e.g. "' // trim(WRF_ARW_MODEL) // &
            '" for the WRF ARW model (see radianceAssimilationFactory.f03)')
        call print('    [platform #] is the number of the platform, e.g. "' // int2str(PLATFORM_TRMM_TMI) // &
            '" for TRMM/TMI')
        call print('    [obs op string] is the code for the observation operator, e.g. "' // &
            trim(CRTM_OBS_OP) // '" for the Community Radiative Transfer Model (see radianceAssimilationFactory.f)')
        call endParallel()
        return
    end if

    call getArg(1, inputFile)
    call getArg(2, outputFile)
    call getArg(3, modelName)
    call getArg(4, platformNumberName)
    call getArg(5, obsOpName)

    read( platformNumberName, '(i10)' ) platformNumber

    call print('Running obs op ' // trim(obsOpName) // ' and platform number ' // &
        trim(platformNumberName) // ' for model type ' // trim(modelName))
    call print('    on input file ' // trim(inputFile))
    call print('    and outputting the results to ' // trim(outputFile))

    call debug('Now setting the parallel decomposition')

    allocate(decomp)
    call decomp%parallelDecompositionConstructor(WEST_EAST_DIM_NAME,SOUTH_NORTH_DIM_NAME)

    call debug('Now setting the parallel info')

    allocate(pinfo)
    call pinfo%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp)

    call debug('Now loading the data set from ' // trim(inputFile))

    modelState => getAtmos3dDataSet(pinfo,modelName,time,inputFile)

    call debug('Now setting up the RTM options and platform')

    xExtent => modelState%getWestEastExtent()
    yExtent => modelState%getSouthNorthExtent()

    rtmOpts  => getRtmOptions()
    platform => getSatellitePlatform(platformNumber,obsOpName)
    obsOp    => getSatelliteObservationOperator(obsOpName,rtmOpts,platform)

    call debug('Created the RTM options for satellite ' // trim(platform%platformName))

    call debug('Now setting the observation parallel info')

    ! now creating "pixel" and "scan" dimensions from the x and y model dimensions
    pixDim  => xExtent%getDimension()
    pixDim  => pixDim%clone()

    scanDim => yExtent%getDimension()
    scanDim => scanDim%clone()
    call pixDim%setName(PIXELS_DIM_NAME)
    call scanDim%setName(SCANS_DIM_NAME)

    allocate(satObs)
    call satObs%satelliteObservationConstructor(platform)
    call satObs%addDimension(pixDim)
    call satObs%addDimension(scanDim)
    call satObs%loadSatelliteObservation(pinfo,pixDim,scanDim, &
        & modelState%getVariable2D(A3D_LAT_VAR),&
        & modelState%getVariable2D(A3D_LON_VAR))

    call debug('Now running the RTM forward calculations...')

    call doRtmForward(pinfo,platform,modelState,obsOp,satObs)

    call barrier(pinfo%getCommunicator())

    call debug('Successfully completed the RTM forward calculations.')

    call debug('Now writing the output to ' // trim(outputFile))
    allocate(ncWriter)
    call ncWriter%netcdfDataArrayWriterConstructor(outputFile)
    writer => ncWriter
    call satObs%writeSatObsToFile(pinfo,writer,.true.)
    deallocate(ncWriter)
    deallocate(modelState)
    deallocate(rtmOpts)
    deallocate(obsOp)
    deallocate(platform)
    deallocate(satObs)

    call endParallel()
end program
