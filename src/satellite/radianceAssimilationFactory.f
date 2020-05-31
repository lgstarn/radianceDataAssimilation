module radianceAssimilationFactory_mod

    use radianceAssimilationConstants_mod
    use rtmOptions_mod

    use satelliteObservationOperator_mod

    use crtmObservationOperator_mod
    use rttovObservationOperator_mod
    use ccvObservationOperator_mod

    use satellitePlatformInfo_mod

    use scannedObservation_mod

    use amsrObservation_mod
    use gmiObservation_mod
    use ssmisObservation_mod

    use observationProcessingChain_mod

    use dataVariable_mod

    use dataGrid_mod

    use scannedObservationBundle_mod

    use gmiObservationBundle_mod
    use amsrObservationBundle_mod
    use ssmisObservationBundle_mod

    use parallelInfo_mod

    use datetime_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    !private

    public :: getRtmOptions
    public :: getSatelliteObservationOperator
    public :: getSatellitePlatform
    public :: getScannedObservationBundle

    contains

    function getRtmOptions() result(rtmOpts)
        implicit none

        class(RtmOptions), pointer :: rtmOpts

        rtmOpts => RtmOptions()
    end function

    function getSatelliteObservationOperator(operatorName,rtmOpts,platform) result(obsOp)
        implicit none

        character(len=*),                    intent(in) :: operatorName
        class(RtmOptions),                   pointer    :: rtmOpts
        class(SatelliteObservationOperator), pointer    :: obsOp
        class(SatellitePlatformInfo),        pointer    :: platform

        class(CrtmObservationOperator),  pointer :: crtmObsOp
        class(RttovObservationOperator), pointer :: rttovObsOp
        class(CcvObservationOperator),   pointer :: ccvObsOp

        select case(operatorName)
            case (CRTM_OBS_OP)
                crtmObsOp => CrtmObservationOperator(rtmOpts,platform)
                obsOp => crtmObsOp
            case (RTTOV_IR_OBS_OP)
                rttovObsOp => RttovObservationOperator(rtmOpts,.false.,platform)
                obsOp => rttovObsOp
            case (RTTOV_MW_OBS_OP)
                rttovObsOp => RttovObservationOperator(rtmOpts,.true.,platform)
                obsOp => rttovObsOp
            case (CCV_OBS_OP)
                ccvObsOp => CcvObservationOperator(rtmOpts%modelVarNames,rtmOpts%modelVarNz,platform)
                obsOp => ccvObsOp
            case default
                write(msgstr,*) 'Unknown observation operator ',trim(operatorName)
                call error(msgstr)
        end select
    end function

    ! get the platform  for a given number and observation operator combination
    ! this method takes observation operator because the sensor name is different for each RTM
    ! this function will get messy eventually, at which time it should be refactored, e.g. to read
    ! from a namelist file
    function getSatellitePlatform(platformNumber,obsOpName,channelSubset) result(platform)

        implicit none

        integer, intent(in)        :: platformNumber
        character(len=*)           :: obsOpName
        integer, optional, pointer :: channelSubset(:)

        class(SatellitePlatformInfo), pointer :: platform

        character(:), allocatable :: platformName, sensorName
        integer :: mobs

        ! set defaults to make it obvious if the values were not set
        platformName = 'N/A'
        sensorName = 'N/A'
        mobs = 0

        select case (platformNumber)
            case (PLATFORM_TRMM_TMI)
                platformName = 'TRMM TMI'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'tmi_trmm'
                        mobs = 9
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'trmm_1_tmi'
                        mobs = 9
                    case (CCV_OBS_OP)
                        sensorName = 'tmi_trmm_ccv'
                        mobs = 3
                end select
            case (PLATFORM_OSCAT)
                platformName = 'Oceansat-2'
                select case(obsOpName)
                    case (CCV_OBS_OP)
                        sensorName = 'oscat_ccv'
                        mobs = 1
                    case default
                        call error('Unsupported platform number/obs op combination (' // &
                            int2str(platformNumber) // ',' // trim(obsOpName) // ')')
                end select
            case (PLATFORM_AIRS)
                platformName = 'Aqua AIRS'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'airs324_aqua'
                        mobs = 2378
                    case (RTTOV_IR_OBS_OP)
                        sensorName = 'eos_2_airs'
                        mobs = 2378
                    case (CCV_OBS_OP)
                        sensorName = 'airs_ccv'
                        mobs = 9
                end select
            case (PLATFORM_HAMSR)
                platformName = 'HAMSR'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'hamsr_grip'
                        mobs = 25
                    case (RTTOV_MW_OBS_OP)
                        call error('Do not have RTTOV coefficients for HAMSR')
                    case (CCV_OBS_OP)
                        sensorName = 'hamsr_ccv'
                        mobs = 3
                end select
            case (PLATFORM_GEOSTORM)
                platformName = 'GeoStorm'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'geostorm'
                        mobs = 10
                    case (RTTOV_MW_OBS_OP)
                        call error('Do not have RTTOV coefficients for GeoStorm')
                    case (CCV_OBS_OP)
                        sensorName = 'gs_ccv'
                        mobs = 3
                end select
            case (PLATFORM_MHS_N19)
                platformName = 'MHS N19'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'mhs_n19'
                        mobs = 5
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'noaa_19_mhs'
                        mobs = 5
                    case (CCV_OBS_OP)
                        sensorName = 'mhsn19_ccv'
                        mobs = 3
                end select
            case (PLATFORM_GPM_GMI)
                platformName = 'GPM GMI'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'gmi_gpm'
                        mobs = 13
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'gpm_1_gmi'
                        mobs = 13
                    case (CCV_OBS_OP)
                        sensorName = 'gmigpm_ccv'
                        mobs = 5
                end select
            case (PLATFORM_AMSUA_AQUA)
                platformName = 'Aqua AMSU/A'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'amsua_aqua'
                        mobs = 15
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'eos_2_amsua'
                        mobs = 15
                    case (CCV_OBS_OP)
                        sensorName = 'amsuaaqua_ccv'
                        mobs = 3
                end select
            case (PLATFORM_SSMIS_F16)
                platformName = 'DMSP-16 Ssmis'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ssmis_f16'
                        mobs = 24
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'dmsp_16_ssmis'
                        mobs = 24
                    case (CCV_OBS_OP)
                        sensorName = 'ssmis_16_ccv'
                        mobs = 3
                end select
            case (PLATFORM_SSMIS_F17)
                platformName = 'DMSP-17 Ssmis'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ssmis_f17'
                        mobs = 24
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'dmsp_17_ssmis'
                        mobs = 24
                    case (CCV_OBS_OP)
                        sensorName = 'ssmis_17_ccv'
                        mobs = 3
                end select
            case (PLATFORM_SSMIS_F18)
                platformName = 'DMSP-18 Ssmis'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ssmis_f18'
                        mobs = 24
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'dmsp_18_ssmis'
                        mobs = 24
                    case (CCV_OBS_OP)
                        sensorName = 'ssmis_18_ccv'
                        mobs = 3
                end select
            case (PLATFORM_SSMIS_F19)
                platformName = 'DMSP-19 Ssmis'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ssmis_f19'
                        mobs = 24
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'dmsp_19_ssmis'
                        mobs = 24
                    case (CCV_OBS_OP)
                        sensorName = 'ssmis_19_ccv'
                        mobs = 3
                end select
            case (PLATFORM_AMSR_E)
                platformName = 'Aqua AMSR-E'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'amsre_aqua'
                        mobs = 12
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'eos_2_amsre'
                        mobs = 12
                    case (CCV_OBS_OP)
                        sensorName = 'amsre_ccv'
                        mobs = 4
                end select
            case (PLATFORM_AMSR_2)
                platformName = 'GCOM-W1 AMSR-2'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'amsr2_gcom-w1'
                        mobs = 14
                    case (RTTOV_MW_OBS_OP)
                        sensorName = 'gcom-w_1_amsr2'
                        mobs = 14
                    case (CCV_OBS_OP)
                        sensorName = 'amsr2_ccv'
                        mobs = 4
                end select
            case (PLATFORM_MTSAT_2)
                platformName = 'MTSAT 2'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'imgr_mt2'
                        mobs = 5
                    case (RTTOV_IR_OBS_OP)
                        sensorName = 'mtsat_2_imager'
                        mobs = 5
                    case (CCV_OBS_OP)
                        sensorName = 'mtsat2_ccv'
                        mobs = 2
                end select
            case (PLATFORM_HIMAWARI8_HMI)
                platformName = 'Himawari 8'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ahi_himawari8'
                        mobs = 16
                    case (RTTOV_IR_OBS_OP)
                        sensorName = 'himawari_8_ahi'
                        mobs = 16
                    case (CCV_OBS_OP)
                        sensorName = 'himawari8_ccv'
                        mobs = 2
                end select
            case (PLATFORM_HIMAWARI9_HMI)
                platformName = 'Himawari 9'
                select case(obsOpName)
                    case (CRTM_OBS_OP)
                        sensorName = 'ahi_himawari9'
                        mobs = 16
                    case (RTTOV_IR_OBS_OP)
                        sensorName = 'himawari_9_ahi'
                        mobs = 16
                    case (CCV_OBS_OP)
                        sensorName = 'himawari9_ccv'
                        mobs = 2
                end select
            case default
                write(msgstr,'(A,I0)') 'Unknown platform number ',platformNumber
                call error(msgstr)
        end select

        if (sensorName == 'N/A' .or. mobs < 0) then
            write(msgstr, '(A,I0,A,A,A)') 'Unsupported platform number/obs op combination (',&
                platformNumber,', ',trim(obsOpName),')'
            call error(msgstr)
        end if

        allocate(platform)
        call platform%satellitePlatformInfoConstructor(platformNumber, platformName, &
            & sensorName, mobs, channelSubset=channelSubset)
    end function

    function getScannedObservation(pinfo,orbitFile,platformNumber,postingNumber) &
        result(scannedObs)

        implicit none

        class(ParallelInfo),    pointer    :: pinfo
        character(len=*),       intent(in) :: orbitFile
        integer,                intent(in) :: platformNumber
        integer,                intent(in) :: postingNumber

        class(ScannedObservation), pointer :: scannedObs

        class(GmiObservation),     pointer :: gmiObs
        class(SsmisObservation),   pointer :: ssmisObs
        class(AmsrObservation),    pointer :: amsrObs

        if (platformNumber == PLATFORM_GPM_GMI) then
            allocate(gmiObs)

            call gmiObs%gmiObservationConstructor(pinfo,orbitFile,postingNumber)

            scannedObs => gmiObs
        else if (platformNumber >= PLATFORM_SSMIS_F16 .and. platformNumber <= PLATFORM_SSMIS_F19) then
            allocate(ssmisObs)

            call debug('Now loading ssmis obs')

            call ssmisObs%ssmisObservationConstructor(pinfo,orbitFile,postingNumber,&
                & platformNumber+7)

            call debug('Finished loading ssmis obs')

            scannedObs => ssmisObs
        else if (platformNumber == PLATFORM_AMSR_2) then
            allocate(amsrObs)

            call amsrObs%amsrObservationConstructor(pinfo,orbitFile,postingNumber,3)

            scannedObs => amsrObs
        else
            write(msgstr,*) 'Unknown platformNumber for getScannedObservation ', platformNumber
            call error(msgstr)
        end if
    end function

    function getScannedObservationBundle(pinfo,orbitFile,inputGrid,platformNumber,obsProcessor,&
        & minGoodRatio,obsErrInflation,columnNormsVar) result(scannedObsBundle)

        implicit none

        class(ParallelInfo),               pointer    :: pinfo
        character(len=*),                  intent(in) :: orbitfile
        class(DataGrid),                   pointer    :: inputGrid
        integer, intent(in)                           :: platformNumber
        class(ObservationProcessingChain), pointer    :: obsProcessor
        real(8), intent(in),     optional             :: minGoodRatio
        real(8), intent(in),     optional             :: obsErrInflation
        class(DataVariable),     optional, pointer    :: columnNormsVar

        class(ScannedObservationBundle),   pointer    :: scannedObsBundle

        class(SsmisObservationBundle),     pointer    :: ssmisObsBundle
        class(AmsrObservationBundle),      pointer    :: amsrObsBundle
        class(GmiObservationBundle),       pointer    :: gmiObsBundle

        if (platformNumber >= PLATFORM_SSMIS_F16 .and. platformNumber <= PLATFORM_SSMIS_F19) then
            allocate(ssmisObsBundle)
            call ssmisObsBundle%ssmisObservationBundleConstructor(pinfo,orbitFile,      &
                & inputGrid,platformNumber+7,obsProcessor,minGoodRatio,obsErrInflation, &
                & columnNormsVar)
            scannedObsBundle => ssmisObsBundle
        else if (platformNumber >= PLATFORM_AMSR_E .and. platformNumber <= PLATFORM_AMSR_2) then
            allocate(amsrObsBundle)
            call amsrObsBundle%amsrObservationBundleConstructor(pinfo,orbitFile,            &
                & inputGrid,platformNumber-11,obsProcessor,minGoodRatio,obsErrInflation, &
                & columnNormsVar)

            scannedObsBundle => amsrObsBundle
        else if (platformNumber == PLATFORM_GPM_GMI) then
            allocate(gmiObsBundle)
            call gmiObsBundle%gmiObservationBundleConstructor(pinfo,orbitFile,          &
                & inputGrid,obsProcessor,minGoodRatio,obsErrInflation,columnNormsVar)

            scannedObsBundle => gmiObsBundle
        else
            write(msgstr,*) 'Unknown platformNumber for getScannedObservationBundle', platformNumber
            call error(msgstr)
        end if
    end function
end module
