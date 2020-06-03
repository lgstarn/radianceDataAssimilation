module rttovObservationOperator_mod
    use satelliteObservationOperator_mod
    use observationOperator_mod
    use satelliteObservation_mod
    use satellitePlatformInfo_mod
    use platformInfo_mod
    use observation_mod
    use standardAtmosphere_mod
    use rtmOptions_mod
    use dataSet_mod
    use dataExtent_mod
    use atmos3DDataSet_mod
    use mpiUtils_mod

    Use rttov_types, Only :  &
        & rttov_coefs,       &
        & rttov_scatt_coef     ,&
        & rttov_chanprof,    &
        & rttov_options,     &
        & rttov_options_scatt  ,&
        & rttov_profile_cloud   ,&
        & rttov_profile,      &
        & rttov_transmission, &
        & rttov_radiance,     &
        & rttov_emissivity,  &
        & rttov_reflectance, &
        & rttov_traj

    Use rttov_const, Only :   &
        & errorstatus_success,&
        & errorstatus_fatal

    Use parkind1, Only : jpim, jprb, jplm

    USE rttov_unix_env, ONLY : rttov_exit

    implicit none

    private

#ifdef _RTTOV_single_layer_cloud_plot_PARALLEL
#include "rttov_parallel_direct.interface"
#define rttov_direct rttov_parallel_direct
#else
#include "rttov_direct.interface"
#endif
#include "rttov_ad.interface"
#include "rttov_read_coefs.interface"
#include "rttov_copy_prof.interface"
#include "rttov_dealloc_coefs.interface"
#include "rttov_alloc_rad.interface"
#include "rttov_alloc_transmission.interface"
#include "rttov_alloc_prof.interface"
#include "rttov_init_prof.interface"
#include "rttov_init_rad.interface"
#include "rttov_init_transmission.interface"
#include "rttov_errorreport.interface"
#include "rttov_print_opts.interface"
#include "rttov_print_profile.interface"
#include "rttov_alloc_traj.interface"
#include "rttov_read_scattcoeffs.interface"
#include "rttov_dealloc_scattcoeffs.interface"
#include "rttov_scatt_setupindex.interface"
#include "rttov_scatt.interface"
#include "rttov_scatt_tl.interface"
#include "rttov_scatt_ad.interface"
#include "rttov_alloc_scatt_prof.interface"

    !--------------------------
    !
    INTEGER(KIND=jpim), PARAMETER :: mxchn = 9000 ! max number of channels

    type, extends(SatelliteObservationOperator), public :: RttovObservationOperator
        private

        ! RTTOV_errorhandling interface
        !====================
        integer(kind=jpim) :: err_unit        ! Logical error unit (<0 for default)
        integer(kind=jpim) :: verbosity_level ! (<0 for default)

        ! RTTOV fwd variables/structures
        !====================
        type(rttov_options)                    :: opts           ! Options structure
        type(rttov_options_scatt)              :: opts_scatt     ! Scattering options
        type(rttov_coefs)                      :: coefs          ! Coefficients structure
        type(rttov_scatt_coef)                 :: coefs_scatt    ! Scattering coefficients structure
        integer (kind=jpim),       allocatable :: frequencies(:) ! Index used by rttov_scatt
        type(rttov_chanprof),      allocatable :: chanprof(:)    ! Input channel/profile list
        type(rttov_profile),       allocatable :: profiles(:)    ! Input profiles
        type(rttov_profile_cloud), allocatable :: cld_profiles(:)! Cloud profiles
        logical(kind=jplm),        allocatable :: calcemis(:)    ! Flag to indicate calculation of emissivity within RTTOV
        type(rttov_emissivity),    allocatable :: emissivity(:)  ! Input/output surface emissivity
        logical(kind=jplm),        allocatable :: calcrefl(:)    ! Flag to indicate calculation of BRDF within RTTOV
        type(rttov_reflectance),   allocatable :: reflectance(:) ! Input/output surface BRDF
        type(rttov_transmission)               :: transmission   ! Output transmittances
        type(rttov_radiance)                   :: radiance       ! Output radiances
        type(rttov_traj)                       :: traj           ! Trajectory, useful for storing data between repeated calls

        ! RTTOV TL variables/structures
        !====================
        type(rttov_profile),       allocatable :: profiles_tl(:)    ! TL input profiles
        type(rttov_profile_cloud), allocatable :: cld_profiles_tl(:)! TL cloud profiles
        type(rttov_transmission)               :: transmission_tl   ! TL output transmittances
        type(rttov_radiance)                   :: radiance_tl       ! TL output radiances
        type(rttov_emissivity),    allocatable :: emissivity_tl(:)  ! TL input/output surface emissivity
        type(rttov_reflectance),   allocatable :: reflectance_tl(:) ! TL input/output surface BRDF
        type(rttov_traj)                       :: traj_tl           ! TL trajectory, for performance

        ! RTTOV TL variables/structures
        !====================
        type(rttov_profile),      allocatable  :: profiles_ad(:)    ! AD output profiles
        type(rttov_profile_cloud), allocatable :: cld_profiles_ad(:)! AD cloud profiles
        type(rttov_transmission)               :: transmission_ad   ! AD input transmittances
        type(rttov_radiance)                   :: radiance_ad       ! AD input radiances
        type(rttov_emissivity),  allocatable   :: emissivity_ad(:)  ! AD input/output surface emissivity
        type(rttov_reflectance), allocatable   :: reflectance_ad(:) ! AD input/output surface BRDF
        type(rttov_traj)                       :: traj_ad           ! AD trajectory, for performance

        integer(kind=jpim)                     :: errorstatus       ! Return error status of RTTOV subroutine calls

        integer(kind=jpim) :: alloc_status(10)


        ! variables for input
        !====================
        character(len=256) :: coef_filename
        character(len=256) :: cld_coef_filename
        integer(kind=jpim) :: dosolar
        integer(kind=jpim) :: nprof
        integer(kind=jpim) :: nchanprof
        integer(kind=jpim) :: ivch, ich
        real(kind=jprb)    :: ems_val, brdf_val
        integer(kind=jpim) :: asw
        integer(kind=jpim) :: aer_clim_index

        real(kind=jprb),    allocatable :: emis(:), brdf(:)
        integer(kind=jpim), allocatable :: nchan(:)
        real(kind=jprb),    allocatable :: aerprof(:,:,:)

        ! loop variables
        integer(kind=jpim) :: jch, lay
        integer(kind=jpim) :: nch
        integer(kind=jpim) :: nprint
        integer(kind=jpim) :: iprof, joff
        integer            :: ios

        ! options
        class(RtmOptions), pointer :: rtmOpt
        class(SatellitePlatformInfo), pointer :: platform

        !- End of RTTOV header --------------------------------------------------------
        logical :: initializedFwd, initializedTL, initializedAdj, isMW
        integer :: nprofiles,nz,nchannels
        real(real64), dimension(:), pointer :: dptr, cptr, levptr, levptr2

        contains
            procedure, private :: setRtmOptions
            procedure, private :: setRttovAtmosphere
            procedure, private :: initializeRttovFwd
            procedure, private :: initializeRttovTL
            procedure, private :: initializeRttovAdj
            procedure, private :: finalizeRttovFwd
            procedure, private :: finalizeRttovTL
            procedure, private :: finalizeRttovAdj

            procedure :: getName
            procedure :: doForward
            procedure :: doTLM
            procedure :: doAdjoint
            final     :: rttovObsOpDestructor ! clean up all allocated variables
    end type

    interface RttovObservationOperator
        procedure rttovObservationOperatorConstructor ! allow generic instantiation
    end interface

    contains

    function rttovObservationOperatorConstructor(rtmOpt,isMW,platform) result(this)
        implicit none

        class(RttovObservationOperator), pointer :: this

        class(RtmOptions), pointer               :: rtmOpt
        logical, intent(in)                      :: isMW
        class(SatellitePlatformInfo),    pointer :: platform

        allocate(this)

        this%initializedFwd = .false.
        this%initializedTL  = .false.
        this%initializedAdj = .false.
        this%isMW           = isMW

        this%rtmOpt   => rtmOpt
        this%platform => platform
    end function

    subroutine rttovObsOpDestructor(this)
        implicit none

        type(RttovObservationOperator)  :: this

        if (this%initializedFwd) then
            call this%finalizeRttovFwd()
        end if
        if (this%initializedTL) then
            call this%finalizeRttovTL()
        end if
        if (this%initializedAdj) then
            call this%finalizeRttovAdj()
        end if
    end subroutine

    function getName(this) result(name)
        implicit none

        class(RttovObservationOperator) :: this
        character(128) :: name

        name = 'RTTOV'
    end function

    subroutine setRtmOptions(this)
        implicit none

        class(RttovObservationOperator) :: this

        this%opts%rt_ir%addsolar           = this%rtmOpt%includeSolar ! include solar radiation?
        this%opts%interpolation%addinterp  = .true.        ! Allow interpolation of input profile
        this%opts%config%apply_reg_limits  = .true.
        this%opts%interpolation%reg_limit_extrap = .true.
        this%opts%config%do_checkinput     = .true. !this%rtmOpt%checkInput ! Check the input?
        this%opts%rt_all%addrefrac         = this%rtmOpt%includeRefraction  ! Include refraction in path calc?
        this%opts%rt_all%use_q2m           = .false.  ! Don't use surface humidity
        this%opts%rt_ir%addclouds          = .true. !this%rtmOpt%includeClouds ! Include cloud effects?
        this%opts%rt_ir%addaerosl          = this%rtmOpt%includeAerosols ! Don't include aerosol effects

        this%opts%rt_ir%ozone_data         = this%rtmOpt%includeOzone
        this%opts%rt_ir%co2_data           = this%rtmOpt%includeCO2
        this%opts%rt_ir%n2o_data           = this%rtmOpt%includeN2O
        this%opts%rt_ir%ch4_data           = this%rtmOpt%includeCH4
        this%opts%rt_ir%co_data            = this%rtmOpt%includeCO
        this%opts%rt_ir%user_cld_opt_param = .false. !this%rtmOpt%directlySpecifyOpticalParameters
        this%opts%rt_all%switchrad         = .true.

        if (this%isMW) then
            this%opts%rt_mw%clw_data           = .TRUE.  !
            this%opts%rt_mw%fastem_version     = 6       ! default is 5

            this%opts_scatt%config%do_checkinput    = .false. ! this%rtmOpt%checkInput
            this%opts_scatt%config%apply_reg_limits = .true.

            this%opts_scatt%reg_limit_extrap = .true.
            this%opts_scatt%use_q2m          = .false.
            this%opts_scatt%lusercfrac       = .false.
            this%opts%rt_ir%addclouds        = .false. 
        end if

    end subroutine

    subroutine doForward(this, input, obs, output)
        implicit none

        class(RttovObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: input
        class(SatelliteObservation), pointer :: obs
        real(real64), dimension(:,:),     pointer :: output

        real(real64), dimension(:), pointer :: obsx, obsy

        integer :: nprofiles  ! m

        integer :: maxi,maxj,maxk,obsnum

        character(len=20) :: sensorId

        real(real64) :: x, y

        real(real64), pointer :: obsptr(:)

        real(real64), pointer :: dimension(:,:)

        real :: start, finish

        real(real64) :: lat, lon, lu_index

        integer :: j, z, nz, chanprofnum, prof

        integer :: water_class

        real(real64), pointer :: obsLoci(:,:)

        class(DataExtent), pointer :: zExtent

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx,1)

        zExtent => input%getBottomTopExtent()

        nz = zExtent%getLocalCount()

        ! check if we can reuse the initialized RTTOV arrays
        if (this%initializedFwd .and. &
               (nprofiles .ne. this%nprofiles .or. nz .ne. this%nz)) then

            ! if not, need to finalize then reinitialize
            call this%finalizeRttovFwd()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeRttovFwd(nprofiles,nz)
        end if

        prof = 0

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            prof = prof + 1

            call this%setRttovAtmosphere(input, x, y, &
                & this%profiles(prof), this%cld_profiles(prof))
        end do

            ! Call RTTOV forward model
        if (this%isMW) then
            call rttov_scatt (                &
                  & this%errorstatus,         &! out
                  & this%opts_scatt,          &! in
                  & this%nz,                  &! in
                  & this%chanprof,            &! in
                  & this%frequencies,         &! in
                  & this%profiles,            &! in
                  & this%cld_profiles,        &! in
                  & this%coefs,               &! in
                  & this%coefs_scatt,         &! in
                  & this%calcemis,            &! in
                  & this%emissivity,          &! inout
                  & this%radiance)             ! inout
        else
            call rttov_direct(                   &
                & this%errorstatus,              &! out error flag
                & this%chanprof,                 &! in channel and profile index structure
                & this%opts,                     &! in options structure
                & this%profiles,                 &! in profile array
                & this%coefs,                    &! in coefficients strucutre
                & this%transmission,             &! inout computed transmittances
                & this%radiance,                 &! inout computed radiances
                & calcemis = this%calcemis,      &! in flag for internal emissivity calcs
                & emissivity = this%emissivity,  &! inout input/output emissivities per channel
                & calcrefl = this%calcrefl,      &! in flag for internal BRDF calcs
                & reflectance = this%reflectance) ! inout input/output BRDFs per channel
        end if

        if ( this%errorstatus /= errorstatus_success ) Then
            Write ( 0, * ) 'rttov_direct error'
            call abortParallel()
        end If

        chanprofnum = 0

        do obsnum=1,nprofiles
            obsptr(1:this%nchannels) => output(1:this%nchannels,obsnum)

            do z=1,this%nchannels
                chanprofnum = chanprofnum + 1
                obsptr(z) = this%radiance%bt(chanprofnum)
            end do
        end do

        call cpu_time(finish)

        write(msgstr,'(A,I6,A,I4,A,I4,A,I4,A,I4,A,F8.2,A)') &
            & 'Successfully ran RTTOV for ',nprofiles, &
            &' profiles (',nint(minval(obsx)),',',nint(minval(obsy)),&
            & ') to (',    nint(maxval(obsx)),',',nint(maxval(obsy)),&
            & ') in ',finish-start,' seconds'
        call print(msgstr)
    end subroutine

    subroutine doTLM(this, baseState, obs, deltaX, deltaY)
        implicit none

        class(RttovObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: baseState
        class(SatelliteObservation), pointer :: obs
        class(Atmos3DDataSet),    pointer :: deltaX
        real(real64), dimension(:,:),     pointer :: deltaY

        real(real64), dimension(:), pointer :: obsx, obsy
        integer :: nz,nabsorbers,nclouds,naerosols,nchannels
        real(real64), pointer :: obsptr(:), obsptr_tl(:)

        integer :: errStat, prof, z
        integer :: obsnum, chanprofnum, nprofiles

        real(real64) :: x, y

        logical :: tl_has_cloud
        character(len=20) :: sensorId

        real(real64), pointer :: obsLoci(:,:)
        class(DataExtent), pointer :: zExtent

        real :: start, finish

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx,1)

        zExtent => baseState%getBottomTopExtent()

        nz = zExtent%getLocalCount()

        ! check if we can reuse the initialized RTTOV arrays
        if (this%initializedFwd .and. (nprofiles .ne. this%nprofiles .or. nz .ne. this%nz)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeRttovFwd()
        end if

        if (this%initializedTL .and. (nprofiles .ne. this%nprofiles .or. nz .ne. this%nz)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeRttovTL()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeRttovFwd(nprofiles,nz)
        end if

        if (.not. this%initializedTL) then
            call this%initializeRttovTL()
        end if

        prof = 0

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            prof = prof + 1

            call this%setRttovAtmosphere(baseState, x, y, this%profiles(prof),    this%cld_profiles(prof))
            call this%setRttovAtmosphere(deltaX,    x, y, this%profiles_tl(prof), this%cld_profiles_tl(prof))
        end do

        ! Call RTTOV TLM
        if (this%isMW) then
            call rttov_scatt_tl (           &
                & this%errorstatus,         &! out
                & this%opts_scatt,          &! in
                & this%nz,                  &! in
                & this%chanprof,            &! in
                & this%frequencies,         &! in
                & this%profiles,            &! in
                & this%cld_profiles,        &! in
                & this%coefs,               &! in
                & this%coefs_scatt,         &! in
                & this%calcemis,            &! in
                & this%emissivity,          &! inout
                & this%profiles_tl,         &! in
                & this%cld_profiles_tl,     &! in
                & this%emissivity_tl,       &! in out
                & this%radiance,            &! inout
                & this%radiance_tl)          ! inout
        else
            call rttov_tl(                       &
                & this%errorstatus,              &! out error flag
                & this%chanprof,                 &! in channel and profile index structure
                & this%opts,                     &! in options structure
                & this%profiles,                 &! in profile array
                & this%profiles_tl,              &! in profile TL array
                & this%coefs,                    &! in coefficients strucutre
                & this%transmission,             &! inout computed transmittances
                & this%transmission_tl,          &! inout computed transmittances TL
                & this%radiance,                 &! inout computed radiances
                & this%radiance_tl,              &! inout computed TL radiances
                & this%calcemis,                 &! in flag for internal emissivity calcs
                & this%emissivity,               &! inout input/output emissivities per channel
                & this%emissivity_tl,            &! inout input/output TL emissivities per channel
                & this%calcrefl,                 &! in flag for internal BRDF calcs
                & this%reflectance,              &! inout input/output BRDFs per channel
                & this%reflectance_tl)            ! inout TL input/output BRDFs per channel
        end if

        chanprofnum = 0

        do obsnum=1,nprofiles
            obsptr(1:this%nchannels) => deltaY(1:this%nchannels,obsnum)

            do z=1,this%nchannels
                chanprofnum = chanprofnum + 1
                obsptr(z) = this%radiance_tl%bt(chanprofnum)
            end do
        end do

        call cpu_time(finish)

        print '(A,I6,A,I4,A,I4,A,I4,A,I4,A,F8.2,A)','Successfully ran RTTOV TL for ',nprofiles,&
            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'

    end subroutine

    subroutine doAdjoint(this, baseState, obs, deltaY, deltaX)
        implicit none

        class(RttovObservationOperator) :: this

        class(Atmos3DDataSet), pointer    :: baseState
        class(SatelliteObservation), pointer :: obs
        real(real64), dimension(:,:), pointer     :: deltaY
        class(Atmos3DDataSet), pointer    :: deltaX

        real(real64), dimension(:), pointer :: obsx, obsy

        integer          :: nprofiles,nz,nabsorbers
        real(real64), pointer :: obsptr(:), obsptr_tl(:)

        integer :: errStat, prof, z, obsnum, chanprofnum

        real(real64) :: x, y

        character(len=20) :: sensorId

        real(real64), pointer :: obsLoci(:,:)

        class(DataExtent), pointer :: zExtent

        real :: start, finish

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx,1)

        zExtent => baseState%getBottomTopExtent()

        nz = zExtent%getLocalCount()

        ! check if we can reuse the initialized RTTOV arrays
        if (this%initializedFwd .and. (nprofiles .ne. this%nprofiles .or. nz .ne. this%nz)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeRttovFwd()
        end if

        if (this%initializedAdj .and. (nprofiles .ne. this%nprofiles .or. nz .ne. this%nz)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeRttovAdj()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeRttovFwd(nprofiles,nz)
        end if

        if (.not. this%initializedAdj) then
            call this%initializeRttovAdj()
        end if

        ! call this before setting perturbations
        call rttov_init_rad(this%radiance_ad)

        chanprofnum = 0

        do obsnum=1,nprofiles
            obsptr(1:this%nchannels) => deltaY(1:this%nchannels,obsnum)

            do z=1,this%nchannels
                chanprofnum = chanprofnum + 1
                this%radiance_ad%bt(chanprofnum) = obsptr(z)
                this%radiance_ad%total(chanprofnum) = obsptr(z)
                this%emissivity_ad(chanprofnum)%emis_in = 0.d0
                this%emissivity_ad(chanprofnum)%emis_out = 0.d0
                this%reflectance_ad(chanprofnum)%refl_in = 0.d0
                this%reflectance_ad(chanprofnum)%refl_out = 0.d0
            end do
        end do

        prof = 0

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            prof = prof + 1

            call this%setRttovAtmosphere(baseState, x, y, this%profiles(prof), this%cld_profiles(prof))
        end do

        call rttov_init_prof(this%profiles_ad)

        call rttov_init_transmission(this%transmission_ad)

        if (this%isMW) then
            call rttov_scatt_ad ( &
                & this%errorstatus,         &! out
                & this%opts_scatt,          &! in
                & this%nz,                  &! in
                & this%chanprof,            &! in
                & this%frequencies,         &! in
                & this%profiles,            &! in
                & this%cld_profiles,        &! in
                & this%coefs,               &! in
                & this%coefs_scatt,         &! in
                & this%calcemis,            &! in
                & this%emissivity,          &! inout
                & this%profiles_ad,         &! inout
                & this%cld_profiles_ad,     &! inout
                & this%emissivity_ad,       &! inout
                & this%radiance,            &! inout
                & this%radiance_ad)          ! inout
        else
            ! Call RTTOV adjoint
            CALL rttov_ad(                       &
                & this%errorstatus,              &! out error flag
                & this%chanprof,                 &! in channel and profile index structure
                & this%opts,                     &! in options structure
                & this%profiles,                 &! in profile array
                & this%profiles_ad,              &! inout profile adjoint array
                & this%coefs,                    &! in coefficients strucutre
                & this%transmission,             &! inout computed transmittances
                & this%transmission_ad,          &! inout computed transmittances adjoint
                & this%radiance,                 &! inout computed radiances
                & this%radiance_ad,              &! inout computed adjoint radiances
                & this%calcemis,                 &! in flag for internal emissivity calcs
                & this%emissivity,               &! inout input/output emissivities per channel
                & this%emissivity_ad,            &! inout input/output adjoint emissivities per channel
                & this%calcrefl,                 &! in flag for internal BRDF calcs
                & this%reflectance,              &! inout input/output BRDFs per channel
                & this%reflectance_ad)            ! inout adjoint input/output BRDFs per channel
        end if

        call this%setRttovAtmosphere(baseState, x, y, this%profiles_ad(prof), &
            & this%cld_profiles_ad(prof), .true., deltaX)

        call cpu_time(finish)

        print '(A,I6,A,I4,A,I4,A,I4,A,I4,A,F7.2,A)','Successfully ran RTTOV adjoint for ',nprofiles,&
            &' profiles (',nint(obsx(1)),',',nint(obsy(1)),') to (',nint(obsx(nprofiles)),',',&
            &nint(obsy(nprofiles)),') in ',finish-start,' seconds'

    end subroutine

    subroutine setRttovAtmosphere(this, baseState, x, y, profile, cld_profile, adjoint, deltaXopt)

        implicit none

        class(RttovObservationOperator)   :: this
        class(Atmos3DDataSet), pointer :: baseState
        real(real64), intent(in) :: x, y
        class(rttov_profile), intent(inout) :: profile
        class(rttov_profile_cloud), intent(inout) :: cld_profile
        logical, intent(in), optional :: adjoint
        class(Atmos3DDataSet), pointer, optional :: deltaXOpt
        class(Atmos3DDataSet), pointer :: deltaX

        integer :: j, nz, allocstat

        real(real64) :: lu_index

        integer :: water_class
        logical :: isAdjoint

        class(DataExtent), pointer :: zExtent

        if (present(adjoint)) then
            isAdjoint = adjoint
        else
            isAdjoint = .false.
        end if

        if (isAdjoint) then
            if (.not. present(deltaXOpt)) then
                write(msgstr,*) 'Delta X must be present for the',this%getName(),'adjoint calculation.'
                call error(msgstr)
            else
                deltaX => deltaXOpt
            end if
        end if

        zExtent => baseState%getBottomTopExtent()

        nz = zExtent%getLocalCount()

        if (isAdjoint) then
            ! adjoint of logarithmic average
            call baseState%getColumn(P_LEVEL_VAR,x,y,.true.,this%levptr)
            this%levptr2(1:nz) = exp((log(this%levptr(2:nz+1))+log(this%levptr(1:nz)))/2.)*profile%p(1:nz)/2.

            this%levptr2(2:nz+1) = this%levptr2(2:nz+1)/this%levptr(2:nz+1)
            this%levptr2(1:nz)   = this%levptr2(1:nz)  /this%levptr(1:nz)

            call deltaX%spreadColumn(P_LEVEL_VAR,x,y,.true.,this%levptr2)
        else
            ! logarithmic average
            call baseState%getColumn(P_LEVEL_VAR,x,y,.true.,this%levptr)
            print *,'p level var is:',this%levptr
            profile%p(1:nz) = exp((log(this%levptr(2:nz+1)) + log(this%levptr(1:nz)))/2.)
        end if

        if (isAdjoint) then
            call deltaX%spreadColumn(T_VAR,x,y,.true.,profile%t)
        else
            call baseState%getColumn(T_VAR,x,y,.true.,profile%t)
        end if

        if (isAdjoint) then
            !do j=1,nz
                ! function converts to kg/kg
            !    call convertPpmvtoSpecificHumidity(profile%q(j), this%dptr(j))

            !    this%dptr(j) = this%dptr(j)/1000.d0
            !enddo

            ! Convert from g/kg to kg/kg again (weird, but this is the way adjoints work)
            this%dptr = profile%q/1000.d0

            call deltaX%spreadColumn(QVAPOR_VAR,x,y,.true.,this%dptr)
        else
            call baseState%getColumn(QVAPOR_VAR,x,y,.true.,this%dptr)

            ! Convert from g/kg to ppmv
            !do j=1,nz
                ! function converts from kg/kg, => divide by 1000.
                !call convertSpecificHumidityToPpmv(this%dptr(j)/1000., profile%q(j))
            !end do

            ! convert from g/kg to kg/kg
            profile%q = max(this%dptr/1000.d0,1d-8)
            profile%gas_units = 1
        end if

        if (this%isMW) then
            if (isAdjoint) then
                this%levptr(1:nz) = cld_profile%ph(1:nz)
                call deltaX%spreadColumn(P_LEVEL_VAR,x,y,.true.,this%levptr)
                call deltaX%spreadValue2D(P_SFC_VAR,x,y,cld_profile%ph(nz+1))
            else
                call baseState%getColumn(P_LEVEL_VAR,x,y,.true.,this%levptr)
                cld_profile%ph(1:nz) = this%levptr(1:nz)
                cld_profile%ph(nz+1) = baseState%getValue2D(P_SFC_VAR,x,y)
            end if

            if (isAdjoint) then
                call deltaX%spreadColumn(CLDFRA_VAR,x,y,.true.,cld_profile%cc)
            else
                call baseState%getColumn(CLDFRA_VAR,x,y,.true.,cld_profile%cc)
            end if

            if (isAdjoint) then
                ! Convert from g/kg to kg/kg (weird, but this is the way adjoints work)
                this%dptr(1:nz) = cld_profile%clw(1:nz)/1000.d0
                call deltaX%spreadColumn(QCLOUD_VAR,x,y,.true.,this%dptr)
            else
                call baseState%getColumn(QCLOUD_VAR,x,y,.true.,this%dptr)
                cld_profile%clw(:) = this%dptr(:)/1000.d0 ! liquid water (kg/kg)
            end if

            if (isAdjoint) then
                this%dptr(1:nz) = cld_profile%ciw(1:nz)/1000.d0
                call deltaX%spreadColumn(QICE_VAR,x,y,.true.,this%dptr)
            else
                call baseState%getColumn(QICE_VAR,x,y,.true.,this%dptr)
                cld_profile%ciw(:) = this%dptr(:)/1000.d0      ! ice water(kg/kg)
            end if

            if (isAdjoint) then
                this%dptr(1:nz) = cld_profile%rain(1:nz)/1000.d0
                call deltaX%spreadColumn(QRAIN_VAR,x,y,.true.,cld_profile%rain)
            else
                call baseState%getColumn(QRAIN_VAR,x,y,.true.,this%dptr)
                cld_profile%rain(:) = this%dptr(:)/1000.d0     ! rain(kg/kg)
            end if

            if (isAdjoint) then
                !call deltaX%spreadColumn(QGRAUP_VAR,x,y,.true.,cld_profile%sp/1000.d0)
                call deltaX%spreadColumn(QSNOW_VAR, x,y,.true.,cld_profile%sp/1000.d0)
                !call deltaX%spreadColumn(QHAIL_VAR, x,y,.true.,cld_profile%sp/1000.d0)
            else
                !call baseState%getColumn(QGRAUP_VAR,x,y,.true.,this%dptr)
                call baseState%getColumn(QSNOW_VAR,x,y,.true.,this%cptr)
                !cld_profile%sp(:) = this%dptr(:)+this%cptr(:)  ! frozen precip.(kg/kg)
                cld_profile%sp(:) = this%cptr(:)/1000.d0  ! frozen precip.(kg/kg)
                !call baseState%getColumn(QHAIL_VAR,x,y,.true.,this%dptr)
                ! add in hail and convert to kg/kg
                !cld_profile%sp(:) = (cld_profile%sp(:)+this%dptr(:))/1000.d0
                cld_profile%use_totalice = .false.
            end if
        else
            if (associated(profile%cloud)) then
                if (isAdjoint) then
                    this%dptr(nz)   = 0.d0
                    ! Convert from g/kg to kg/kg again (weird, but this is the way adjoints work)
                    this%dptr(1:nz-1) = 0.5d0*profile%cloud(1,1:nz-1)/1000.d0
                    this%dptr(2:nz) = this%dptr(2:nz) + 0.5d0*profile%cloud(1,1:nz-1)/1000.d0
                    call deltaX%spreadColumn(QCLOUD_VAR,x,y,.true.,this%dptr)
                    call deltaX%spreadColumn(QRAIN_VAR,x,y,.true.,this%dptr)
                else
                    call baseState%getColumn(QCLOUD_VAR,x,y,.true.,this%dptr)
                    call baseState%getColumn(QRAIN_VAR,x,y,.true.,this%cptr)
                    profile%cloud(1,1:nz-1) = 0.5d0*(this%dptr(1:nz-1)+this%cptr(1:nz-1)+ &
                                                     this%dptr(2:nz)  +this%cptr(2:nz))/1000.d0 ! liquid water (kg/kg)
                    profile%clw_scheme = 1
                end if

                if (isAdjoint) then
                    this%dptr(nz)   = 0.d0
                    this%dptr(1:nz-1) = 0.5d0*profile%cloud(6,1:nz-1)/1000.0d0
                    this%dptr(2:nz) = this%dptr(2:nz) + 0.5d0*profile%cloud(6,1:nz-1)/1000.d0

                    call deltaX%spreadColumn(QICE_VAR, x,y,.true.,this%dptr)
                    call deltaX%spreadColumn(QSNOW_VAR, x,y,.true.,this%dptr)
                    !call deltaX%spreadColumn(QGRAUP_VAR, x,y,.true.,this%dptr)
                else
                    !call baseState%getColumn(QGRAUP_VAR,x,y,.true.,this%dptr)
                    call baseState%getColumn(QICE_VAR,x,y,.true.,this%dptr)
                    call baseState%getColumn(QSNOW_VAR,x,y,.true.,this%cptr)
                    !profile%cloud(6,:) = this%dptr(:)+this%cptr(:)  ! frozen precip.(kg/kg)
                    profile%cloud(6,1:nz-1) = 0.5d0*(this%dptr(1:nz-1)+this%cptr(1:nz-1) + &
                                                     this%dptr(2:nz)  +this%cptr(2:nz))/1000.d0  ! frozen precip.(kg/kg)
                    !call baseState%getColumn(QHAIL_VAR,x,y,.true.,this%dptr)
                    ! add in hail and convert to kg/kg
                    !profile%cloud(6,:) = (profile%cloud(6,:)+this%dptr(:))/1000.d0

                    profile%ice_scheme = 2
                end if
            end if

            if (associated(profile%cfrac)) then
                if (isAdjoint) then
                    this%dptr(nz)   = 0.d0
                    this%dptr(1:nz-1) = 0.5d0*profile%cfrac(1:nz-1)
                    this%dptr(2:nz) = this%dptr(2:nz) + 0.5d0*profile%cfrac(1:nz-1)
                    call deltaX%getColumn(CLDFRA_VAR,x,y,.true.,this%dptr)
                else
                    call baseState%getColumn(CLDFRA_VAR,x,y,.true.,this%dptr)
                    profile%cfrac(1:nz-1) = 0.5d0*(this%dptr(1:nz-1) + this%dptr(2:nz))
                end if
            end if
        end if

        ! 2 meter air variables
        if (isAdjoint) then
            call deltaX%spreadValue2D(T2_VAR,x,y,profile%s2m%t)
        else
            profile%s2m%t = baseState%getValue2D(T2_VAR,x,y)
        end if

        if (isAdjoint) then
            call deltaX%spreadValue2D(P_SFC_VAR,x,y,profile%s2m%p)
        else
            profile%s2m%p = baseState%getValue2D(P_SFC_VAR,x,y)
        end if

        if (isAdjoint) then
            call deltaX%spreadValue2D(U10_VAR,x,y,profile%s2m%u)
        else
            profile%s2m%u = baseState%getValue2D(U10_VAR,x,y)
        end if

        if (isAdjoint) then
            call deltaX%spreadValue2D(V10_VAR,x,y,profile%s2m%v)
        else
            profile%s2m%v = baseState%getValue2D(V10_VAR,x,y)
        end if

        ! Surface type and water type
        lu_index = baseState%getValue2D(LU_INDEX_VAR,x,y)

        if (baseState%getLandCatCount() .eq. 20 .or. baseState%getLandCatCount() .eq. 21) then
           water_class = 17
        elseif (baseState%getLandCatCount() .eq. 24) then
           water_class = 16
        else
            write(msgstr,*) 'Unknown number of land categories: ', &
                baseState%getLandCatCount(),', RTTOV obs op cannot continue.'
            call error(msgstr)
        endif

        profile%skin%fastem(:) = 0.0
        ! rttov,surftype 0=land, 1=ocean, 2=sea-ice
        if (nint(lu_index) .eq. water_class) then
            profile%skin%surftype = 1
            profile%skin%watertype = 1 ! 0=fresh water, 1=ocean water

            if (isAdjoint) then
                call deltaX%spreadValue2D(T_SURF_VAR,x,y,profile%skin%t)
            else
                profile%skin%t = baseState%getValue2D(T_SURF_VAR,x,y)
            end if

            profile%elevation  = 0.0
        else
            profile%skin%surftype = 0

            if (isAdjoint) then
                call deltaX%spreadValue2D(T_SURF_VAR,x,y,profile%skin%t)
            else
                profile%skin%t = baseState%getValue2D(T_SURF_VAR,x,y)
            end if

            profile%elevation = 0.001*baseState%getValue2D(SFC_HGT_VAR,x,y) ! km
            ! some random values the cat dragged in
            profile%skin%fastem (1) = 3.0
            profile%skin%fastem (2) = 5.0
            profile%skin%fastem (3) = 15.0
            profile%skin%fastem (4) = 0.1
            profile%skin%fastem (5) = 0.3
        end if

        profile%ctp  = 500.0_JPRB   ! Not used but still required by RTTOV

        ! Elevation, latitude and longitude
        profile%latitude    = baseState%getValue2D(A3D_LAT_VAR,x,y)
        profile%longitude   = baseState%getValue2D(A3D_LON_VAR,x,y)
       !profile%cfraction = 0.0
!                cumulativeCloudVapor = 0.0

        ! Other variables from interactive baseStates
        ! TODO: calculate these as appropriate for the type of scanner
        profile%zenangle    = this%rtmOpt%zenithAngle
        profile%azangle     = this%rtmOpt%azimuthAngle
        profile%sunzenangle = this%rtmOpt%sunZenithAngle
        profile%sunazangle  = this%rtmOpt%sunAzimuthAngle
    end subroutine

    subroutine convertSpecificHumidityToPpmv(q, converted)

        implicit none

        double precision, intent(in) :: q
        double precision, intent(out) :: converted
        ! molecular weight of dry air
        double precision, parameter :: M_air = 28.9644
        ! molecular weight of water vapor
        double precision, parameter :: M_wv = 18.0152833

        converted = (q*M_air)/((1.0-q)*M_wv + q*M_air) * 1.0e+06
        converted = max(0.0d0, converted)
    end subroutine

    subroutine convertPpmvToSpecificHumidity(ppmv, converted)

        implicit none

        double precision, intent(in) :: ppmv
        double precision, intent(out) :: converted
        ! molecular weight of dry air
        double precision, parameter :: M_air = 28.9644
        ! molecular weight of water vapor
        double precision, parameter :: M_wv = 18.0152833

        converted = M_wv*ppmv/((M_wv-M_air)*ppmv + 1e6*M_air)
        converted = max(0.0d0, converted)
    end subroutine

    subroutine initializeRttovFwd(this,nprof,nz)
        implicit none

        class(RttovObservationOperator) :: this
        integer, intent(in) :: nprof, nz
        character(256) :: coef_filename, cld_filename

        integer :: ich, j, jch, nch, asw

        this%nz = nz

        this%nprofiles    = nprof
        this%alloc_status = 0

        this%errorstatus     = 0_jpim
        this%alloc_status(:) = 0_jpim

        ! --------------------------------------------------------------------------
        ! 1. Set the options
        ! --------------------------------------------------------------------------
        call this%setRtmOptions()

        ! --------------------------------------------------------------------------
        ! 2. Read coefficients
        ! --------------------------------------------------------------------------

        ! Read optical depth and aerosol coefficient files together
        if (this%isMW) then
            coef_filename = 'rtcoef_' // trim(this%platform%sensorName) // '.dat'
            if (associated(this%platform%getChannelSubset())) then
                call rttov_read_coefs(this%errorstatus, this%coefs, this%opts, file_coef=coef_filename, &
                    channels=this%platform%getChannelSubset())
            else
                call rttov_read_coefs(this%errorstatus, this%coefs, this%opts, file_coef=coef_filename)
            end if
        else
            !coef_filename = 'rtcoef_' // trim(this%platform%sensorName) // '.H5'
            !cld_filename  = 'sccldcoef_' // trim(this%platform%sensorName) // '.H5'
            coef_filename = 'rtcoef_' // trim(this%platform%sensorName) // '.dat'
            cld_filename  = 'sccldcoef_' // trim(this%platform%sensorName) // '.dat'
            if (associated(this%platform%getChannelSubset())) then
                call rttov_read_coefs(this%errorstatus, this%coefs, this%opts, form_coef='formatted', &
                    file_coef=coef_filename, file_sccld=cld_filename,&
                    channels=this%platform%getChannelSubset())
            else
                call rttov_read_coefs(this%errorstatus, this%coefs, this%opts, form_coef='formatted', &
                    file_coef=coef_filename, file_sccld=cld_filename)
            end if
        end if
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'fatal error reading coefficients'
            call rttov_exit(this%errorstatus)
        endif

        this%nchannels = this%coefs % coef % fmv_chn

        if (this%isMW) then
            call rttov_read_scattcoeffs (this%errorstatus, this%opts_scatt, this%coefs,&
                                         this%coefs_scatt)
            if (this%errorstatus /= errorstatus_success) then
                write(*,*) 'fatal error reading scatt coefficients'
                call rttov_exit(this%errorstatus)
            endif
        end if

        ! Ensure the options and coefficients are consistent
        call rttov_user_options_checkinput(this%errorstatus, this%opts, this%coefs)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'error in rttov options'
            call rttov_exit(this%errorstatus)
        endif

        ! --------------------------------------------------------------------------
        ! 3. Build the list of profile/channel indices in chanprof
        ! --------------------------------------------------------------------------

        ! In general the number of channels simulated per profile can vary, but for
        ! this example we simulate all specified channels for each profile.

        allocate(this%nchan(nprof))     ! Number of channels per profile
        this%nchan(:) = this%nchannels
        this%nchanprof = SUM(this%nchan(:))  ! Size of chanprof array is total number of channels over all profiles

        ! Pack channels and input emissivity arrays
        allocate(this%chanprof(this%nchanprof))
        allocate(this%emis(this%nchanprof))
        allocate(this%brdf(this%nchanprof))

        if (this%isMW) then
            allocate (this%frequencies(this%nchanprof))
        end if

        ! don't allow user to specify emissivity or brdf yet
        this%emis = 0.0d0
        this%brdf = 0.0d0

        nch = 0_jpim
        do j = 1, nprof
            do jch = 1, this%nchan(j)
                nch = nch + 1_jpim
                this%chanprof(nch)%prof = j
                this%chanprof(nch)%chan = jch
            enddo
        enddo

        ! --------------------------------------------------------------------------
        ! 4. Allocate profiles, radiance and transmittance structures
        ! --------------------------------------------------------------------------

        asw = 1 ! Switch for allocation passed into RTTOV subroutines

        ! Allocate input profile arrays
        allocate(this%profiles(nprof), stat=this%alloc_status(1))
        allocate (this%cld_profiles (nprof), stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem allocation error for profile array'
            call rttov_exit(errorstatus_fatal)
        endif

        call rttov_alloc_prof(              &
            & this%errorstatus,             &
            & nprof,                        &
            & this%profiles,                &
            & this%nz,                      &
            & this%opts,                    &
            & asw,                          &
            & coefs=this%coefs,             &
            & init=.true._jplm         )
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'allocation error for profile arrays'
            call rttov_exit(this%errorstatus)
        endif

        call rttov_alloc_scatt_prof(            &
                             this%errorstatus,  &
                             nprof,             &
                             this%cld_profiles, &
                             this%nz,           &
                             .false._jplm,      &
                             1_jpim,            &
                             init = .true._jplm)

        ! Allocate output radiance arrays
        call rttov_alloc_rad(      &
            & this%errorstatus,    &
            & this%nchanprof,      &
            & this%radiance,       &
            & this%nz,             &
            & asw)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'allocation error for radiance arrays'
            call rttov_exit(this%errorstatus)
        endif

        ! Allocate transmittance structure
        call rttov_alloc_transmission( &
            & this%errorstatus,             &
            & this%transmission,            &
            & this%nz,                      &
            & this%nchanprof,               &
            & asw,                          &
            & init=.true._jplm)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'allocation error for transmission arrays'
            call rttov_exit(this%errorstatus)
        endif

        call rttov_alloc_traj(                          &
            & this%errorstatus,          nprof,              &
            & this%nchannels,            this%opts,               &
            & this%profiles(1)%nlevels,  this%coefs,              &
            & asw,               traj = this%traj)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'allocation error for transmission arrays'
            call rttov_exit(this%errorstatus)
        endif

        ! Allocate arrays for surface emissivity
        allocate(this%calcemis(this%nchanprof), stat=this%alloc_status(1))
        allocate(this%emissivity(this%nchanprof), stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem allocation error for emissivity arrays'
            call rttov_exit(errorstatus_fatal)
        endif

        ! Allocate arrays for surface reflectance
        allocate(this%calcrefl(this%nchanprof), stat=this%alloc_status(1))
        allocate(this%reflectance(this%nchanprof), stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'memallocation error for reflectance arrays'
            call rttov_exit(errorstatus_fatal)
        endif

        ! --------------------------------------------------------------------------
        ! 6. Specify surface emissivity and reflectance
        ! --------------------------------------------------------------------------

        ! Copy emissivities into RTTOV input emissivity array
        this%emissivity(:) % emis_in = this%emis(:)

        ! Calculate emissivity within RTTOV where the input emissivity value is
        ! zero or less
        this%calcemis(:) = this%emis(:) <= 0._jprb

        ! Copy BRDFs into RTTOV input reflectance array
        this%reflectance(:)%refl_in = this%brdf(:)

        ! Calculate BRDF within RTTOV where the input BRDF value is zero or less
        this%calcrefl(:) = this%brdf(:) <= 0._jprb

        ! Use default cloud top BRDF for simple cloud in VIS/NIR channels
        this%reflectance(:) % refl_cloud_top = 0._jprb

        if (this%isMW) then
            call rttov_scatt_setupindex ( &
                & nprof,                & ! in
                & this%nchannels,       & ! in
                & this%coefs,           & ! in
                & this%nchanprof,       & ! in
                & this%chanprof,        & ! out
                & this%frequencies)       ! out
        end if

        allocate(this%dptr(nz),this%cptr(nz),this%levptr(nz+1),this%levptr2(nz+1))

        this%initializedFwd = .true.

        write(msgstr,*) 'RTTOV fwd obs op succesfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine initializeRttovTL(this)
        implicit none

        class(RttovObservationOperator) :: this

        integer :: asw

        if (this%initializedTL) return

        asw = 1 ! Switch for allocation passed into RTTOV subroutines

        print '(A,I6,A)','Initializing RTTOV TL for ',this%nprofiles,' profiles'

        ! Allocate input profile arrays
        allocate(this%profiles_tl(this%nprofiles), stat=this%alloc_status(1))
        if (ANY(this%alloc_status /= 0)) THEN
            write(*,*) 'mem allocation error for profile_tl array'
            call rttov_exit(errorstatus_fatal)
        end if

        call rttov_alloc_prof(         &
            & this%errorstatus,        &
            & this%nprofiles,          &
            & this%profiles_tl,        &
            & this%nz,                 &
            & this%opts,               &
            & asw,                     &
            & coefs=this%coefs,        &
            & init=.TRUE._jplm         )
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for profile arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        allocate (this%cld_profiles_tl(this%nprofiles), stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem allocation error for cloud profile array'
            call rttov_exit(errorstatus_fatal)
        endif

        call rttov_alloc_scatt_prof(                 &
                             & this%errorstatus,     &
                             & this%nprofiles,       &
                             & this%cld_profiles_tl, &
                             & this%nz,              &
                             & .false._jplm,         &
                             & 1_jpim,               &
                             & init = .true._jplm)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for radiance arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        ! Allocate output radiance arrays
        CALL rttov_alloc_rad(      &
            & this%errorstatus,    &
            & this%nchanprof,      &
            & this%radiance_tl,    &
            & this%nz,      &
            & asw)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for radiance arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        ! Allocate transmittance structure
        CALL rttov_alloc_transmission( &
            & this%errorstatus,        &
            & this%transmission_tl,    &
            & this%nz,                 &
            & this%nchanprof,          &
            & asw,                     &
            & init=.TRUE._jplm)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for transmission arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        ALLOCATE(this%emissivity_tl(this%nchanprof), stat=this%alloc_status(1))
        ALLOCATE(this%reflectance_tl(this%nchanprof), stat=this%alloc_status(2))
        IF (ANY(this%alloc_status /= 0)) THEN
            WRITE(*,*) 'memallocation error for tl emissivity/reflectance arrays'
            CALL rttov_exit(errorstatus_fatal)
        ENDIF

        CALL rttov_alloc_traj(                            &
            & this%errorstatus,          this%nprofiles,  &
            & this%nchannels,            this%opts,       &
            & this%profiles(1)%nlevels,  this%coefs,      &
            & asw,               traj_tl = this%traj_tl )
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for transmission arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        this%initializedTL = .true.

        write(msgstr,*) 'RTTOV TL obs op succesfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine initializeRttovAdj(this)
        implicit none

        class(RttovObservationOperator) :: this

        integer :: asw

        if (this%initializedAdj) return

        call this%setRtmOptions()

        asw = 1

        ! Allocate input profile arrays
        ALLOCATE(this%profiles_ad(this%nprofiles), stat=this%alloc_status(1))
        IF (ANY(this%alloc_status /= 0)) THEN
            WRITE(*,*) 'mem allocation error for profile_tl array'
            CALL rttov_exit(errorstatus_fatal)
        ENDIF
        CALL rttov_alloc_prof(         &
            & this%errorstatus,        &
            & this%nprofiles,          &
            & this%profiles_ad,        &
            & this%nz,                 &
            & this%opts,               &
            & asw,                &
            & coefs=this%coefs,        &
            & init=.TRUE._jplm         )
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for profile arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        allocate (this%cld_profiles_ad(this%nprofiles), stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem allocation error for cloud profile array'
            call rttov_exit(errorstatus_fatal)
        endif

        call rttov_alloc_scatt_prof(                 &
                             & this%errorstatus,     &
                             & this%nprofiles,       &
                             & this%cld_profiles_ad, &
                             & this%nz,              &
                             & .false._jplm,         &
                             & 1_jpim,               &
                             & init = .true._jplm)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for cloud profile arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF


        ! Allocate output radiance arrays
        CALL rttov_alloc_rad(   &
            & this%errorstatus, &
            & this%nchanprof,   &
            & this%radiance_ad, &
            & this%nz,        &
            & asw)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for radiance arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        ! Allocate transmittance structure
        CALL rttov_alloc_transmission( &
            & this%errorstatus,        &
            & this%transmission_ad,    &
            & this%nz,                 &
            & this%nchanprof,          &
            & asw,                     &
            & init=.TRUE._jplm)
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for transmission arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        ALLOCATE(this%emissivity_ad(this%nchanprof), stat=this%alloc_status(1))
        ALLOCATE(this%reflectance_ad(this%nchanprof), stat=this%alloc_status(2))
        IF (ANY(this%alloc_status /= 0)) THEN
            WRITE(*,*) 'memallocation error for tl emissivity/reflectance arrays'
            CALL rttov_exit(errorstatus_fatal)
        ENDIF

        CALL rttov_alloc_traj(                           &
            & this%errorstatus,          this%nprofiles, &
            & this%nchannels,            this%opts,      &
            & this%profiles(1)%nlevels,  this%coefs,     &
            & asw,               traj_ad = this%traj_ad )
        IF (this%errorstatus /= errorstatus_success) THEN
            WRITE(*,*) 'allocation error for transmission arrays'
            CALL rttov_exit(this%errorstatus)
        ENDIF

        this%initializedAdj = .true.

        write(msgstr,*) 'RTTOV Adj obs op succesfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine finalizeRttovFwd(this)
        implicit none

        class(RttovObservationOperator) :: this

        integer :: asw

        if (.not. this%initializedFwd) return

        write(msgstr,*) 'Finalizing RTTOV fwd'
        call print(msgstr)

        deallocate (this%emis,        stat=this%alloc_status(1))
        deallocate (this%brdf,        stat=this%alloc_status(2))
        deallocate (this%nchan,       stat=this%alloc_status(3))
        deallocate (this%chanprof,    stat=this%alloc_status(4))
        deallocate (this%emissivity,  stat=this%alloc_status(5))
        deallocate (this%calcemis,    stat=this%alloc_status(6))
        deallocate (this%reflectance, stat=this%alloc_status(7))
        deallocate (this%calcrefl,    stat=this%alloc_status(8))

        if (this%isMW) then
            deallocate (this%frequencies,stat=this%alloc_status(9))
        end if

        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem dellocation error'
        endif

        asw = 0 ! Switch for deallocation passed into RTTOV subroutines

        call rttov_alloc_traj(                          &
            & this%errorstatus,         this%nprofiles,          &
            & this%nchannels,           this%opts,               &
            & this%profiles(1)%nlevels,  this%coefs,              &
            & asw,               traj = this%traj)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'traj deallocation error'
        endif

        ! Deallocate radiance arrays
        call rttov_alloc_rad(this%errorstatus, this%nchannels, this%radiance, this%nz, asw)
        if(this%errorstatus /= errorstatus_success) then
            write(*,*) 'radiance deallocation error'
        endif

        ! Deallocate transmission arrays
        call rttov_alloc_transmission(this%errorstatus, this%transmission, this%nz, &
            this%nchannels, asw)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'transmission deallocation error'
        endif

        ! Deallocate profile arrays
        call rttov_alloc_prof(this%errorstatus, this%nprofiles, this%profiles, this%nz, this%opts, asw)
        deallocate(this%profiles, stat=this%alloc_status(1))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'profile deallocation error'
        endif

        call rttov_alloc_scatt_prof(this%errorstatus,this%nprofiles,this%cld_profiles,this%nz,.false.,asw)
        deallocate (this%cld_profiles, stat=this%alloc_status(2))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(2) /= 0) then
            write(*,*) 'mem deallocation error for profile/cld_profile array'
        endif

        call rttov_dealloc_coefs(this%errorstatus, this%coefs)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'coefs deallocation error'
        endif

        if (this%isMW) then
            call rttov_dealloc_scattcoeffs( this%coefs_scatt )
        end if

        deallocate(this%dptr,this%cptr,this%levptr,this%levptr2)

        this%initializedFwd = .false.
    end subroutine

    subroutine finalizeRttovTL(this)
        implicit none

        class(RttovObservationOperator) :: this

        integer :: asw

        if (.not. this%initializedTL) return

        write(msgstr,*) 'Finalizing RTTOV TL'
        call print(msgstr)

        deallocate (this%emissivity_tl,  stat=this%alloc_status(1))
        deallocate (this%reflectance_tl, stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem dellocation error'
        endif

        asw = 0 ! Switch for deallocation passed into RTTOV subroutines

        ! Deallocate radiance arrays
        call rttov_alloc_rad(this%errorstatus, this%nchannels, this%radiance_tl, this%nz, asw)
        if(this%errorstatus /= errorstatus_success) then
            write(*,*) 'radiance_tl deallocation error'
        endif

        ! Deallocate transmission arrays
        call rttov_alloc_transmission(this%errorstatus, this%transmission_tl, this%nz, this%nchannels, asw)
        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'transmission_tl deallocation error'
        endif

        ! Deallocate profile arrays
        call rttov_alloc_prof(this%errorstatus, this%nprofiles, this%profiles_tl, this%nz, this%opts, asw)
        deallocate(this%profiles_tl, stat=this%alloc_status(2))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'profile_tl deallocation error'
        endif

        ! Deallocate cld_profile array
        call rttov_alloc_scatt_prof(this%errorstatus,this%nprofiles,this%cld_profiles_tl,&
            &this%nz,.false.,asw)
        deallocate(this%cld_profiles_tl, stat=this%alloc_status(1))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'mem deallocation error for profile/cld_profile TL array'
        endif

        call rttov_alloc_traj(                               &
            & this%errorstatus,     this%nprofiles,          &
            & this%nchannels,       this%opts,               &
            & this%profiles(1)%nlevels,  this%coefs,         &
            & asw,               traj_tl = this%traj_tl)

        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'traj_tl deallocation error'
        endif
        this%initializedTL = .false.
    end subroutine

    subroutine finalizeRttovAdj(this)
        implicit none

        class(RttovObservationOperator) :: this

        integer :: asw

        if (.not. this%initializedAdj) return

        deallocate (this%emissivity_ad,  stat=this%alloc_status(1))
        deallocate (this%reflectance_ad, stat=this%alloc_status(2))
        if (ANY(this%alloc_status /= 0)) then
            write(*,*) 'mem dellocation error'
        endif

        asw = 0 ! Switch for deallocation passed into RTTOV subroutines

        ! Deallocate radiance arrays
        call rttov_alloc_rad(this%errorstatus, this%nchannels, this%radiance_ad, this%nz, asw)
        if(this%errorstatus /= errorstatus_success) then
            write(*,*) 'radiance_ad deallocation error'
        endif

        ! Deallocate transmission arrays
        call rttov_alloc_transmission(this%errorstatus, this%transmission_ad, this%nz, &
            this%nchannels, asw)

        if (this%errorstatus /= errorstatus_success) then
            write(*,*) 'transmission_ad deallocation error'
        endif

        ! Deallocate profile arrays
        call rttov_alloc_prof(this%errorstatus, this%nprofiles, this%profiles_ad, this%nz, this%opts, asw)
        deallocate(this%profiles_ad, stat=this%alloc_status(1))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'profile_ad deallocation error'
        endif

        call rttov_alloc_scatt_prof(this%errorstatus,this%nprofiles,&
            &this%cld_profiles_ad,this%nz,.false.,asw)
        deallocate(this%cld_profiles_ad, stat=this%alloc_status(1))
        if (this%errorstatus /= errorstatus_success .or. this%alloc_status(1) /= 0) then
            write(*,*) 'mem deallocation error for cloud profile array'
        endif

        call rttov_alloc_traj(                          &
            & this%errorstatus,     this%nprofiles,     &
            & this%nchannels,            this%opts,          &
            & this%nz,  this%coefs,    &
            & asw,               traj_ad = this%traj_ad)

        this%initializedAdj = .false.
    end subroutine

end module
