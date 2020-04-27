module radianceAssimilationConstants_mod

    public

    ! Observation operators
    character(len=256), parameter, public :: CRTM_OBS_OP = 'CRTM'
    character(len=256), parameter, public :: RTTOV_IR_OBS_OP = 'RTTOV-IR'
    character(len=256), parameter, public :: RTTOV_MW_OBS_OP = 'RTTOV-MW'
    character(len=256), parameter, public :: CCV_OBS_OP = 'CCV'

    ! Platforms
    integer, parameter, public :: PLATFORM_TRMM_TMI     = 1
    integer, parameter, public :: PLATFORM_OSCAT        = 2
    integer, parameter, public :: PLATFORM_AIRS         = 3
    integer, parameter, public :: PLATFORM_HAMSR        = 4
    integer, parameter, public :: PLATFORM_GEOSTORM     = 5
    integer, parameter, public :: PLATFORM_MHS_N19      = 6
    integer, parameter, public :: PLATFORM_GPM_GMI      = 7
    integer, parameter, public :: PLATFORM_AMSUA_AQUA   = 8
    integer, parameter, public :: PLATFORM_SSMIS_F16    = 9
    integer, parameter, public :: PLATFORM_SSMIS_F17    = 10
    integer, parameter, public :: PLATFORM_SSMIS_F18    = 11
    integer, parameter, public :: PLATFORM_SSMIS_F19    = 12
    integer, parameter, public :: PLATFORM_AMSR_E       = 13
    integer, parameter, public :: PLATFORM_AMSR_2       = 14
    integer, parameter, public :: PLATFORM_MTSAT_2      = 15
    integer, parameter, public :: PLATFORM_HIMAWARI7_HMI = 16
    integer, parameter, public :: PLATFORM_HIMAWARI8_HMI = 17
    integer, parameter, public :: PLATFORM_HIMAWARI9_HMI = 18
end module
