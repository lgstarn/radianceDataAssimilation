module obsQcCodes_mod

    implicit none

    ! Model-independent quality control codes. These should be supplemented with domain-specific error codes.
    !
    ! Note that an observation is considered to pass QC if the code is greater than 1, meaning
    ! any QC_CODE 2 or above will be rejected.
    !
    ! Adapted from HEDAS error codes

    integer, public, parameter  :: QC_NOERR                 = 0   ! No error detected, assimiate observation
    integer, public, parameter  :: QC_OBEVAL                = 1   ! Only evaluate this observation, don't assim
    integer, public, parameter  :: QC_OBSKIP                = 2   ! Skip this observation for any reason
    integer, public, parameter  :: QC_BADTIME               = 3   ! The time provided was invalid
    integer, public, parameter  :: QC_OUTOFTIME             = 4   ! The time provided was out of the simulation range
    integer, public, parameter  :: QC_BADOBVALUE            = 5   ! An invalid observation value was encountered
    integer, public, parameter  :: QC_BADOBERROR            = 6   ! An invalid observation error was encountered
    integer, public, parameter  :: QC_BADLOCI               = 11  ! One or more loci values provided were invalid
    integer, public, parameter  :: QC_OUTDOM                = 12  ! The observation was outside of the model domain
    integer, public, parameter  :: QC_PRIOR_FWDOP           = 21  ! Eval error in prior
    integer, public, parameter  :: QC_UPDT_FWDOP            = 31  ! Eval error during update
    integer, public, parameter  :: QC_POSTR_FWDOP           = 41  ! Eval error in posterior

    real(8), public, parameter  :: QC_NOVALUE               = -9.99d99

    integer, public, parameter :: PRIOR_STAGE         = 1
    integer, public, parameter :: ASSIMILATION_STAGE  = 2
    integer, public, parameter :: POSTERIOR_STAGE     = 3
end module
