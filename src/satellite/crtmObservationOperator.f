module crtmObservationOperator_mod
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

    use crtm_module

    implicit none

    private

    public :: CrtmObservationOperator

    type, extends(SatelliteObservationOperator) :: CrtmObservationOperator
        private

        type(CRTM_ChannelInfo_type) :: chinfo
        type(CRTM_Geometry_type), allocatable :: geo(:) ! m
        type(CRTM_Options_type), allocatable :: opt(:) ! m

        ! Forward declarations
        type(CRTM_Atmosphere_type), allocatable :: atm(:) ! m
        type(CRTM_Surface_type), allocatable :: sfc(:) ! m
        type(CRTM_RTSolution_type), allocatable :: rts(:,:) ! l x m

        ! Tangent linear declarations
        type(CRTM_Atmosphere_type), allocatable :: atm_tl(:) ! m
        type(CRTM_Surface_type), allocatable :: sfc_tl(:) ! m
        type(CRTM_RTSolution_type), allocatable :: rts_tl(:,:) ! l x m

        ! Adjoint declarations
        type(CRTM_Atmosphere_type), allocatable :: atm_adj(:) ! m
        type(CRTM_Surface_type), allocatable :: sfc_adj(:) ! m
        type(CRTM_RTSolution_type), allocatable :: rts_adj(:,:) ! l x m

!        type(CRTM_Atmosphere_type)  , allocatable :: atm_K(:,:)  ! l x m
!        type(CRTM_Surface_type)     , allocatable :: sfc_K(:,:)  ! l x m
!        type(CRTM_RTSolution_type)  , allocatable :: rts_K(:,:)  ! l x m

        real(8), dimension(:), pointer :: dptr, cptr

        logical :: initializedFwd, initializedTL, initializedAdj
        integer :: nprofiles,nlayers,nabsorbers,nclouds,naerosols,nchannels

        class(RtmOptions), pointer   :: rtmOpts

        class(SatellitePlatformInfo), pointer :: platform

        contains
            procedure, private :: setCrtmAtmosphere
            procedure, private :: setCrtmSurface
            procedure, private :: setCrtmGeometry
            procedure, private :: setCrtmOptions
            procedure :: initializeCrtmFwd
            procedure :: initializeCrtmTL
            procedure :: initializeCrtmAdj
            procedure :: finalizeCrtmFwd
            procedure :: finalizeCrtmTL
            procedure :: finalizeCrtmAdj

            procedure :: getName
            procedure :: doForward
            procedure :: doTLM
            procedure :: doAdjoint
            final :: crtmObsOpDestructor ! clean up all allocated variables
    end type

    interface CrtmObservationOperator
        procedure crtmObservationOperatorConstructor ! allow generic instantiation
    end interface

    contains

    function crtmObservationOperatorConstructor(rtmOpts,platform) result(this)
        implicit none

        class(CrtmObservationOperator), pointer :: this
        class(RtmOptions), pointer              :: rtmOpts
        class(SatellitePlatformInfo),   pointer :: platform

        allocate(this)
        this%initializedFwd = .false.
        this%initializedTL  = .false.
        this%initializedAdj = .false.
        this%rtmOpts        => rtmOpts
        this%platform       => platform
    end function

    subroutine crtmObsOpDestructor(this)
        implicit none

        type(CrtmObservationOperator)  :: this

        if (this%initializedFwd) then
            call this%finalizeCrtmFwd()
        end if
        if (this%initializedTL) then
            call this%finalizeCrtmTL()
        end if
        if (this%initializedAdj) then
            call this%finalizeCrtmAdj()
        end if
    end subroutine

    function getName(this) result(name)
        implicit none

        class(CrtmObservationOperator) :: this
        character(128) :: name

        name = 'CRTM'
    end function

    subroutine doForward(this, input, obs, output)
        implicit none

        class(CrtmObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: input
        class(SatelliteObservation), pointer :: obs
        real(8), dimension(:,:),     pointer :: output

        real(8), dimension(:,:),     pointer :: obsLoci

        class(DataExtent),   pointer :: zExtent

        real(8), dimension(:),   pointer :: obsx, obsy
        integer :: nprofiles  ! m

        integer :: maxi,maxj,maxk,obsnum

        character(len=20) :: sensorId

        integer :: allocStat, errStat

        integer :: nlayers, nabsorbers, nclouds, naerosols, profile, isltyp, ivgtyp

        real(8) :: x, y

        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        real(8), pointer, dimension(:) :: obsptr, latptr, lonptr, sfctmpptr, emisptr, odptr

        real :: start, finish

        real(8) :: lat, lon

        integer :: w, z

        character(len=256) :: fieldName

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx,1)

        zExtent => input%getBottomTopExtent()

        nlayers = zExtent%getLocalCount()
        nabsorbers = 2
        nclouds = 6
        naerosols = 0

        ! check if we can reuse the initialized CRTM arrays
        if (this%initializedFwd .and. (nprofiles .ne. this%nprofiles .or. nlayers .ne. this%nlayers .or. &
            nabsorbers .ne. this%nabsorbers .or. naerosols .ne. this%naerosols)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeCrtmFwd()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeCrtmFwd(sensorId,nprofiles,nlayers,nabsorbers,&
                                     nclouds,naerosols)
        end if

        profile = 1

        call CRTM_Surface_Zero(this%sfc)
        call CRTM_Atmosphere_Zero(this%atm)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)
            call this%setCrtmAtmosphere(this%atm,input,x,y,obsnum)
            call this%setCrtmSurface(this%sfc,input,x,y,obsnum)
            call this%setCrtmGeometry(input,x,y,obsnum)
            call this%setCrtmOptions(input,x,y,obsnum)
        end do

!        errStat = CRTM_Surface_WriteFile('SURF.bin',this%sfc(1:nprofiles))
!        errStat = CRTM_Atmosphere_WriteFile('ATM.bin', this%atm(1:nprofiles),QUIET=.false.)
!        errStat = CRTM_Options_WriteFile('OPT.bin', this%opt(1:nprofiles),QUIET=.false.)
!        errStat = CRTM_Geometry_WriteFile('GEO.bin', this%geo(1:nprofiles),QUIET=.false.)
!        stop

!        print *,'Now invoking CRTM_Forward'

!        call CRTM_Geometry_Inspect(this%geo(1))
!        call CRTM_Options_Inspect(this%opt(1))
!        call CRTM_Surface_Inspect(this%Sfc(1))
!        call CRTM_Atmosphere_Inspect(this%Atm(1))
!        stop
        chInfos(1) = this%chInfo

        errStat = CRTM_Forward(this%atm, this%sfc, this%geo, chInfos, this%rts, Options=this%opt)

        if (errStat /= SUCCESS) then
            call error('Error invoking CRTM_Forward')
        endif

        do obsnum=1,nprofiles
            obsptr => output(:,obsnum)

            do z=1,this%nchannels
                obsptr(z) = this%rts(z,obsnum)%Brightness_Temperature
            end do
        end do

        call cpu_time(finish)

        write(msgstr, '(A,I6,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)') 'Successfully ran CRTM for ',nprofiles,&
            ' profiles (',obsx(1),',',obsx(nprofiles),') to (',obsy(1),',',obsy(nprofiles),') in ',finish-start,' seconds'
        call print(msgstr)

    end subroutine

    subroutine doTLM(this, baseState, obs, deltaX, deltaY)
        implicit none

        class(CrtmObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: baseState
        class(SatelliteObservation), pointer :: obs
        class(Atmos3DDataSet),    pointer :: deltaX
        real(8), dimension(:,:),     pointer :: deltaY

        real(8), dimension(:), pointer :: obsx, obsy
        integer :: nprofiles,nlayers,nabsorbers,nclouds,naerosols,nchannels
        real(8), dimension(:), pointer :: obsptr, obsptr_tl, latptr, lonptr

        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        integer :: errStat, profile, z, fwd_cloud, tl_cloud, obsnum

        real(8) :: x, y

        logical :: tl_has_cloud
        character(len=20) :: sensorId

        real :: start, finish

        real(8), dimension(:,:),     pointer :: obsLoci

        class(DataExtent),   pointer :: zExtent

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx)

        zExtent => baseState%getBottomTopExtent()

        nlayers = zExtent%getLocalCount()
        nabsorbers = 2
        nclouds = 6
        naerosols = 0

        ! check if we can reuse the initialized CRTM arrays
        if (this%initializedFwd .and. (nprofiles .gt. this%nprofiles .or. nlayers .gt. this%nlayers .or. &
            nabsorbers .ne. this%nabsorbers .or. naerosols .ne. this%naerosols)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeCrtmFwd()
        end if

        if (this%initializedTl .and. (nprofiles .ne. this%nprofiles .or. nlayers .ne. this%nlayers .or. &
            nabsorbers .ne. this%nabsorbers .or. naerosols .ne. this%naerosols)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeCrtmTl()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeCrtmFwd(sensorId,nprofiles,nlayers,nabsorbers,&
                                     nclouds,naerosols)
        end if

        if (.not. this%initializedTL) then
            call this%initializeCrtmTL(sensorId,nprofiles,nlayers,nabsorbers,&
                                       nclouds,naerosols)
        end if

        profile = 1

        call CRTM_Surface_Zero(this%sfc)
        call CRTM_Atmosphere_Zero(this%atm)

        call CRTM_Atmosphere_Zero(this%atm_tl)
        call CRTM_Surface_Zero(this%sfc_tl)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)
            call this%setCrtmAtmosphere(this%atm,baseState,x,y,profile)

            call this%setCrtmSurface(this%sfc,baseState,x,y,profile)
            call this%setCrtmGeometry(baseState,x,y,profile)
            call this%setCrtmOptions(baseState,x,y,profile)

            call this%setCrtmAtmosphere(this%atm_tl,deltaX,x,y,profile)
            call this%setCrtmSurface(this%sfc_tl,deltaX,x,y,profile)

            this%atm_tl(profile)%n_clouds = nclouds

            ! match up the fwd and tl clouds
            ! assuming all cloud species in the forward - otherwise the
            ! adjoint won't get invoked!
            do fwd_cloud=nclouds,1,-1
                if (fwd_cloud .gt. this%atm(profile)%n_clouds) then
                    write(msgstr,*) 'Error: can only assume all cloud species are filled for TL:',x,y
                    call error(msgstr)
                else
                    tl_has_cloud = .false.
                    do tl_cloud=1,fwd_cloud
                        if (this%atm(profile)%Cloud(fwd_cloud)%Type .eq. &
                            this%atm_tl(profile)%Cloud(tl_cloud)%Type) then

                            tl_has_cloud = .true.
                            exit
                        end if
                    end do

                    if (tl_has_cloud) then
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Type = &
                            this%atm_tl(profile)%Cloud(tl_cloud)%Type
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Water_Content = &
                            this%atm_tl(profile)%Cloud(tl_cloud)%Water_Content
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Effective_Radius = &
                            this%atm_tl(profile)%Cloud(tl_cloud)%Effective_Radius
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Effective_Variance = &
                            this%atm_tl(profile)%Cloud(tl_cloud)%Effective_Variance
                    else
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Type = &
                            this%atm(profile)%Cloud(fwd_cloud)%Type
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Water_Content = 0.0
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Effective_Radius = 0.0
                        this%atm_tl(profile)%Cloud(fwd_cloud)%Effective_Variance = 0.0
                    end if
                end if
            end do

            profile = profile + 1
        end do

!        errStat = CRTM_Surface_WriteFile('SURF.bin',this%sfc(1:nprofiles))
!        errStat = CRTM_Atmosphere_WriteFile('ATM.bin', this%atm(1:nprofiles),QUIET=.false.)
!        errStat = CRTM_Options_WriteFile('OPT.bin', this%opt(1:nprofiles),QUIET=.false.)
!        errStat = CRTM_Geometry_WriteFile('GEO.bin', this%geo(1:nprofiles),QUIET=.false.)
!        stop

!        print *,'Now invoking CRTM_Tangent_Linear'

!        call CRTM_Geometry_Inspect(this%geo(1))
!        call CRTM_Options_Inspect(this%opt(1))
!        call CRTM_Surface_Inspect(this%Sfc(1))
!        call CRTM_Atmosphere_Inspect(this%Atm(1))
!        call CRTM_Atmosphere_Inspect(this%Atm_tl(1))

        chInfos(1) = this%chInfo

        this%rts_tl%radiance = 0.0
        this%rts_tl%brightness_temperature = 1.0

        errStat = CRTM_Tangent_Linear(this%atm, this%sfc, this%atm_tl, this%sfc_tl, this%geo, chInfos, &
            this%rts, this%rts_tl,Options=this%opt)

        if (errStat /= SUCCESS) then
            call error('Error invoking CRTM_Tangent_Linear')
        endif

        do obsnum=1,nprofiles
            obsptr_tl => deltaY(:,obsnum)

            do z=1,this%nchannels
                obsptr_tl(z) = this%rts_tl(z,obsnum)%Brightness_Temperature
            end do
        end do

        call cpu_time(finish)

!        print '(A,I6,A,I4,A,I4,A,I4,A,I4,A,F6.2,A)','Successfully ran CRTM TL for ',nprofiles,&
!            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'

    end subroutine

    subroutine doAdjoint(this, baseState, obs, deltaY, deltaX)
        implicit none

        class(CrtmObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: baseState
        class(SatelliteObservation), pointer :: obs
        real(8), dimension(:,:),     pointer :: deltaY
        class(Atmos3DDataSet),    pointer :: deltaX

        real(8), dimension(:), pointer :: obsx, obsy

        integer :: nprofiles,nlayers,nabsorbers,nclouds,naerosols,nchannels
        real(8), pointer :: obsptr(:), obsptr_tl(:), latptr(:), lonptr(:)

        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        real(8), dimension(:,:),     pointer :: obsLoci

        class(DataExtent),   pointer :: zExtent

        integer :: errStat, profile, z, fwd_cloud, obsnum

        real(8) :: x, y

        character(len=20) :: sensorId

        real :: start, finish

        call cpu_time(start)

        obsLoci => obs%getObsLoci()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx,1)

        zExtent => baseState%getBottomTopExtent()

        nlayers = zExtent%getLocalCount()
        nabsorbers = 2
        nclouds = 6
        naerosols = 0

        ! check if we can reuse the initialized CRTM arrays
        if (this%initializedFwd .and. (nprofiles .ne. this%nprofiles .or. nlayers .ne. this%nlayers .or. &
            nabsorbers .ne. this%nabsorbers .or. naerosols .ne. this%naerosols)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeCrtmFwd()
        end if

        if (this%initializedAdj .and. (nprofiles .ne. this%nprofiles .or. nlayers .ne. this%nlayers .or. &
            nabsorbers .ne. this%nabsorbers .or. naerosols .ne. this%naerosols)) then
            ! if not, need to finalize then reinitialize
            call this%finalizeCrtmAdj()
        end if

        sensorId = trim(obs%getSensorName())

        if (.not. this%initializedFwd) then
            call this%initializeCrtmFwd(sensorId,nprofiles,nlayers,nabsorbers,&
                                     nclouds,naerosols)
        end if

        if (.not. this%initializedAdj) then
            call this%initializeCrtmAdj(sensorId,nprofiles,nlayers,nabsorbers,&
                                       nclouds,naerosols)
        end if

        call CRTM_Surface_Zero(this%sfc)
        call CRTM_Atmosphere_Zero(this%atm)

        call CRTM_RTSolution_Zero(this%rts_adj)
        call CRTM_Atmosphere_Zero(this%atm_adj)
        call CRTM_Surface_Zero(this%sfc_adj)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)
            call this%setCrtmAtmosphere(this%atm,baseState,x,y,obsnum)
            call this%setCrtmSurface(this%sfc,baseState,x,y,obsnum)
            call this%setCrtmGeometry(baseState,x,y,obsnum)
            call this%setCrtmOptions(baseState,x,y,obsnum)

            obsptr(1:this%nchannels) => deltaY(1:this%nchannels,obsnum)
            this%rts_adj(:,obsnum)%Radiance = 0.0
            this%rts_adj(:,obsnum)%Brightness_Temperature = obsptr(:)

            this%atm_adj(obsnum)%n_clouds = nclouds

            ! match up the fwd and adj clouds
            ! assuming all cloud species in the forward - otherwise the
            ! adjoint won't get invoked!

!                do fwd_cloud=1,nclouds
!                    if (fwd_cloud .gt. this%atm(profile)%n_clouds) then
!                        print *,'Error: can only assume all cloud species are filled for TL:',x,y
!                        stop
!                    else
!                        this%atm_adj(profile)%Cloud(fwd_cloud)%Type = &
!                            this%atm(profile)%Cloud(fwd_cloud)%Type
!                        this%atm_adj(profile)%Cloud(fwd_cloud)%Water_Content = 0.0
!                        this%atm_adj(profile)%Cloud(fwd_cloud)%Effective_Radius = 0.0
!                        this%atm_adj(profile)%Cloud(fwd_cloud)%Effective_Variance = 0.0
!                    end if
!                end do
        end do

!        print *,'Now invoking CRTM_Adjoint'

        chInfos(1) = this%chInfo

        errStat = CRTM_Adjoint(this%atm, this%sfc, this%rts_adj, this%geo, chInfos, &
            this%atm_adj, this%sfc_adj, this%rts, Options=this%opt)

        if (errStat /= SUCCESS) then
            call error('Error invoking CRTM_Adjoint')
        endif

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            call this%setCrtmAtmosphere(this%atm_adj,deltaX,x,y,obsnum,.true.)
            call this%setCrtmSurface(this%sfc_adj,deltaX,x,y,obsnum,.true.)
        end do

        call cpu_time(finish)

!        print '(A,I6,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)','Successfully ran CRTM adjoint for ',nprofiles,&
!            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'

    end subroutine

    subroutine setCrtmAtmosphere(this, atm, input, x, y, profile, adjoint, limitCloud)
        implicit none

        class(CrtmObservationOperator)    :: this

        type(CRTM_Atmosphere_type)        :: atm(:)
        class(Atmos3DDataSet), pointer    :: input
        real(8),               intent(in) :: x, y
        integer,               intent(in) :: profile
        logical,               optional   :: adjoint, limitCloud

        ! number of hydrometer classes
        integer, parameter :: nhydro = 6

        integer, dimension(nhydro) :: hydroclass

        real(8), dimension(nhydro) :: nu = (/6.0,6.0,1.0,1.0,1.0,1.0/)  ! Gamma distribution width parameter
        real(8), dimension(nhydro) :: alph1 = (/0.5236,0.00739,0.5236,0.003,0.049,0.466/) ! Alpha in mass-dimensional relationship
        real(8), dimension(nhydro) :: numberConcentration = (/1.7d+08,1.0d+06,5.0d+02,1.0d+05,1.0d+05,5.0d+03/)  ! number/kg
        real(8), dimension(nhydro) :: beta1 = (/3.0,2.45,3.0,2.2,2.8,3.0/)  ! Beta in mass-dimensional relationship

        real(8), parameter :: RD  = 286.9968933d0  ! DRY GAS CONSTANT, J/(kg*degK)
        real(8), parameter :: Re  = 6371000.0d0    ! EARTH RADIUS, m
        real(8), parameter :: G0  = 9.80616d0      ! GRAVITY m/s**2
        real(8), parameter :: EPS = 0.621970585d0  ! RATIO OF MOLEC WT OF WATER AND DRY AIR
        real(8), parameter :: X_C = (1.0d0 - EPS) / EPS ! unitless

        real(8) :: f_rain, f_ice, f_rimef, cwm, lat, lon, tv, dz, multiplier, height
        real(8) :: arg1, arg2, numerator, denominator, MeanDiameter, EffectiveDiameter

        integer :: icloud, kcloud, z, nz

        logical :: isAdjoint
        logical :: doLimitCloud

        class(DataExtent), pointer :: zExtent

        if (present(adjoint)) then
            isAdjoint = adjoint
        else
            isAdjoint = .false.
        end if

        if (present(limitCloud)) then
            doLimitCloud = limitCloud
        else
            doLimitCloud = .false.
        end if

        zExtent => input%getBottomTopExtent()

        nz = zExtent%getLocalCount()

        if (.not. isAdjoint) then
            atm(profile)%Climatology         = TROPICAL
            atm(profile)%Absorber_Id(1:2)    = (/ H2O_ID, O3_ID /)
            atm(profile)%Absorber_Units(1:2) = (/ MASS_MIXING_RATIO_UNITS,  VOLUME_MIXING_RATIO_UNITS /)
        end if

        if (isAdjoint) then
            this%dptr(1:nz+1) = atm(profile)%Level_Pressure(0:nz)
            call input%spreadColumn(P_LEVEL_VAR,x,y,.true.,this%dptr)
        else
            call input%getColumn(P_LEVEL_VAR,x,y,.true.,this%dptr)
            atm(profile)%Level_Pressure(0:nz) = this%dptr(1:nz+1)
            ! use logarithmic average for layers
            atm(profile)%Pressure(1:nz) = exp((log(this%dptr(2:nz+1))+log(this%dptr(1:nz)))/2.)
        end if

        if (isAdjoint) then
            this%dptr(1:nz) = atm(profile)%Temperature(1:nz)
            call input%spreadColumn(T_VAR,x,y,.true.,this%dptr)
        else
            call input%getColumn(T_VAR,x,y,.true.,this%dptr)
            atm(profile)%Temperature(1:nz) = this%dptr(1:nz)
        end if

        if (isAdjoint) then
            this%dptr(1:nz) = atm(profile)%Absorber(1:nz,1)
            call input%spreadColumn(QVAPOR_VAR,x,y,.true.,this%dptr)
        else
            call input%getColumn(QVAPOR_VAR,x,y,.true.,this%dptr)
            atm(profile)%Absorber(1:nz,1) = this%dptr(1:nz)
            where(atm(profile)%Absorber .lt. 0.0d0) atm(profile)%Absorber = 0.0d0
        end if

        if (.not. isAdjoint .and. all(atm(profile)%Pressure(1:nz) .gt. 0)) then
            call interpolateToStandardAtmosphere(atm(profile)%Pressure(1:nz),atm(profile)%Absorber(1:nz,2),&
                TROPICAL_ATM,O3_INDEX)
        end if

        hydroclass = -1
        kcloud = 0
        if (isAdjoint) then
            ! adjoint can't deal with doLimitCloud
            if (doLimitCloud) then
                write(msgstr,*) 'Adjoint cannot handle doLimitCloud as it would introduce a discontinuity'
                call error(msgstr)
            end if

            do icloud=1,atm(profile)%n_clouds
                this%dptr(1:nz) = atm(profile)%Cloud(icloud)%Water_Content

                select case(atm(profile)%Cloud(icloud)%type)
                    case(WATER_CLOUD)
                        call input%spreadColumn(QCLOUD_VAR,x,y,.true.,this%dptr)
                    case(ICE_CLOUD)
                        call input%spreadColumn(QICE_VAR,x,y,.true.,this%dptr)
                    case(RAIN_CLOUD)
                        call input%spreadColumn(QRAIN_VAR,x,y,.true.,this%dptr)
                    case(SNOW_CLOUD)
                        call input%spreadColumn(QSNOW_VAR,x,y,.true.,this%dptr)
                    !case(GRAUPEL_CLOUD)
                    !    call input%spreadColumn(QGRAUP_VAR,x,y,.true.,this%dptr)
                    !case(HAIL_CLOUD)
                    !    call input%spreadColumn(QHAIL_VAR,x,y,.true.,this%dptr)
                end select
            end do
        else
            call input%getColumn(CLDFRA_VAR,x,y,.true.,this%cptr)

            do icloud=1,nhydro
                select case (icloud)
                    case (1)
                        call input%getColumn(QCLOUD_VAR,x,y,.true.,this%dptr)

                        if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                            kcloud = kcloud + 1
                            atm(profile)%Cloud(kcloud)%Type = WATER_CLOUD
                            atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                            hydroclass(icloud) = kcloud
                        end if
                    case (2)
                        call input%getColumn(QICE_VAR,x,y,.true.,this%dptr)

                        if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                            kcloud = kcloud + 1
                            atm(profile)%Cloud(kcloud)%Type = ICE_CLOUD
                            atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                            hydroclass(icloud) = kcloud
                        end if
                    case (3)
                        call input%getColumn(QRAIN_VAR,x,y,.true.,this%dptr)

                        if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                            kcloud = kcloud + 1
                            atm(profile)%Cloud(kcloud)%Type = RAIN_CLOUD
                            atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                            hydroclass(icloud) = kcloud
                        end if
                    case (4)
                        call input%getColumn(QSNOW_VAR,x,y,.true.,this%dptr)

                        if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                            kcloud = kcloud + 1
                            atm(profile)%Cloud(kcloud)%Type = SNOW_CLOUD
                            atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                            hydroclass(icloud) = kcloud
                        end if
                    !case (5)
                    !    call input%getColumn(QGRAUP_VAR,x,y,.true.,this%dptr)
                    !
                    !    if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                    !        kcloud = kcloud + 1
                    !        atm(profile)%Cloud(kcloud)%Type = GRAUPEL_CLOUD
                    !        atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                    !        hydroclass(icloud) = kcloud
                    !    end if
                    !case (6)
                    !    call input%getColumn(QHAIL_VAR,x,y,.true.,this%dptr)
                    ! 
                    !    if (.not. doLimitCloud .or. sum(this%dptr(1:nz)) .ge. 1d-8) then
                    !        kcloud = kcloud + 1
                    !        atm(profile)%Cloud(kcloud)%Type = HAIL_CLOUD
                    !        atm(profile)%Cloud(kcloud)%Water_Content = this%cptr*this%dptr
                    !        hydroclass(icloud) = kcloud
                    !    end if
                end select
            end do

            atm(profile)%n_clouds=kcloud
        end if

        if (isAdjoint) then
            ! compute the dz from the atmosphere
            this%dptr(1:nz) = log(this%atm(profile)%Level_Pressure(1:nz)/this%atm(profile)%Level_Pressure(0:nz-1))*RD/G0
            this%dptr(1:nz) = this%dptr(1:nz)*this%atm(profile)%Temperature(1:nz)*(1.0 + &
                X_C*this%atm(profile)%Absorber(1:nz,1)/1000.) ! convert to kg/kg

            ! correct for gravity differences as a function of height
            do z =1,nz
                height = this%geo(profile)%Surface_Altitude + SUM(this%dptr(z+1:nz))
                this%dptr(z) = this%dptr(z) * ((Re+height)/Re)**2 ! 1st order correction for gravity fn of height
            end do

            call input%spreadColumn(DZ_VAR,x,y,.true.,this%dptr)
        else
            call input%getColumn(DZ_VAR,x,y,.true.,this%dptr)
        end if

        do z=1,nz
            ! Pa / K * m = kg/(m s^2) / K * m
            if (this%atm(profile)%Pressure(z) .gt. 0.0 .and. &
                this%atm(profile)%Temperature(z) .gt. 0.0 .and. this%dptr(z) .gt. 0.0) then
                multiplier = this%atm(profile)%Pressure(z) / this%atm(profile)%Temperature(z) * this%dptr(z)
                ! (kg^2 * K m)/(J K m s^2)
                ! = kg^2 s^2 /(kg m^2 s^2)
                ! = 1/(m^2)
                multiplier = multiplier / 286.9968933d0 / 10.d0 ! DRY GAS CONSTANT, J/(kg*degK)
            else
                multiplier = 0.0
            end if

            do icloud=1,nhydro
                if (hydroclass(icloud) .gt. 0) then
                    kcloud = hydroclass(icloud)
                    arg1 = nu(icloud) + 1.0d0
                    arg2 = beta1(icloud) + nu(icloud) + 1.0d0
                    numerator = gf( arg1 ) *atm(profile)%Cloud(kcloud)%Water_Content(z)
                    denominator = alph1(icloud) * NumberConcentration(icloud) * gf( arg2 )
                    MeanDiameter = arg1 * ( numerator / denominator ) ** ( 1.0d0 / beta1(icloud) )
                    EffectiveDiameter = 1.0d+04 * MeanDiameter * ( nu(icloud) + 3.0d0 ) / arg1   ! cm to micrometers

                    select case (atm(profile)%Cloud(kcloud)%Type)
                        case(WATER_CLOUD)
                            call input%getColumn(QCLOUD_VAR,x,y,.true.,this%cptr)
                        case(ICE_CLOUD)
                            call input%getColumn(QICE_VAR,x,y,.true.,this%cptr)
                        case(RAIN_CLOUD)
                            call input%getColumn(QRAIN_VAR,x,y,.true.,this%cptr)
                        case(SNOW_CLOUD)
                            call input%getColumn(QSNOW_VAR,x,y,.true.,this%cptr)
                        !case(GRAUPEL_CLOUD)
                        !    call input%getColumn(QGRAUP_VAR,x,y,.true.,this%cptr)
                        !case(HAIL_CLOUD)
                        !    call input%getColumn(QHAIL_VAR,x,y,.true.,this%cptr)
                    end select

                    ! convert from kg/kg to kg/m^2
                    if (multiplier .gt. 0) then
!                        if (isAdjoint) then
!                            cptr(:) = cptr(:)/multiplier
!                            select case (atm(profile)%Cloud(kcloud)%Type)
!                                case(WATER_CLOUD)
!                                    call input%getColumn(QCLOUD_VAR,x,y,.true.,this%cptr)
!                                case(ICE_CLOUD)
!                                    call input%getColumn(QICE_VAR,x,y,.true.,this%cptr)
!                                case(RAIN_CLOUD)
!                                    call input%getColumn(QRAIN_VAR,x,y,.true.,this%cptr)
!                                case(SNOW_CLOUD)
!                                    call input%getColumn(QSNOW_VAR,x,y,.true.,this%cptr)
!                                case(GRAUPEL_CLOUD)
!                                    call input%getColumn(QGRAUP_VAR,x,y,.true.,this%cptr)
!                                case(HAIL_CLOUD)
!                                    call input%getColumn(QHAIL_VAR,x,y,.true.,this%cptr)
!                            end select
!
!                        else
                            atm(profile)%Cloud(kcloud)%Water_Content(z) = &
                                atm(profile)%Cloud(kcloud)%Water_Content(z)*multiplier
                            atm(profile)%Cloud(kcloud)%Effective_Radius(z) = EffectiveDiameter/2.
                            if (atm(profile)%Cloud(kcloud)%Water_Content(z) .lt. 0.0d0) then
                                atm(profile)%Cloud(kcloud)%Water_Content(z) = 0.0d0
                            end if
!                        end if
                    end if

                    !atm(profile)%Cloud(kcloud)%Effective_Variance = 0.0d0
                end if
            end do
        end do
    end subroutine

    subroutine setCrtmSurface(this,sfc,input,x,y,profile,adjoint)
        implicit none

        class(CrtmObservationOperator) :: this
        type(CRTM_Surface_type) :: sfc(:)
        class(Atmos3DDataSet), pointer :: input
        real(8), intent(in) :: x, y
        integer, intent(in) :: profile
        logical, optional :: adjoint
        real(8), dimension(:), pointer :: dptr

        real(8) :: lu_index

        logical :: isAdjoint

        integer :: water_class, snow_class

        if (present(adjoint)) then
            isAdjoint = adjoint
        else
            isAdjoint = .false.
        end if

!                !  soil types according to the TBL file + description
!                !          (description based on loam = 40% 40% 20% (silt,sand,clay))
!                !
!                !             1          SAND              (coarse)
!                !             2          LOAMY SAND        (more coarse + silt + some clay)
!                !             3          SANDY LOAM        (more coarse + silt + some clay)
!                !             4          SILT LOAM         (some coarse + medium + some clay)
!                !             5          SILT              (medium)
!                !             6          LOAM              (coarse + medium + some fine)
!                !             7          SANDY CLAY LOAM   (coarse + fine + some medium)
!                !             8          SILTY CLAY LOAM   (less coarse + medium + fine)
!                !             9          CLAY LOAM         (coarse + medium + fine)
!                !            10          SANDY CLAY        (coarse + fine)
!                !            11          SILTY CLAY        (medium + fine)
!                !            12          CLAY              (fine)
!                !            13          ORGANIC MATERIALS
!                !            14          WATER
!                !            15          BEDROCK
!                !            16          OTHER (land-ice)
!                !            17          Playa
!                !            18          Lava
!                !            19          White Sand
!
!
!                ! Microwave CRTM soil types, 2.1.3 user guide page 33
!                !
!                ! Index         Texture                Description
!                !
!                !     1          coarse                 loamy sand
!                !     2          medium                 silty clay loam
!                !     3          fine                   light clay
!                !     4          coarse-medium          sandy loam
!                !     5          coarse-fine            sandy clay
!                !     6          medium-fine            clay loam
!                !     7          coarse-med-fine        sandy clay loam
!                !     8          organic                farmland
!                !     9          glacial land ice       ice over land
!!                isltyp = input%getValue2D('ISLTYP',x,y)
!!
!!                ! approximate mapping between the two
!!                select case(isltyp)
!!                    case (1:2) ! sand, loamy sand
!!                        this%sfc(profile)%Soil_Type = 1 ! loamy sand
!!                    case (3) ! sandy loam
!!                        this%sfc(profile)%Soil_Type = 4 ! sandy loam
!!                    case (4) ! silt loam
!!                        this%sfc(profile)%Soil_Type = 6 ! clay loam
!!                    case (5) ! silt
!!                        this%sfc(profile)%Soil_Type = 2 ! silty clay loam
!!                    case (6) ! loam
!!                        this%sfc(profile)%Soil_Type = 8 ! farmland
!!                    case (7) ! sandy clay loam
!!                        this%sfc(profile)%Soil_Type = 7 ! sandy clay loam
!!                    case (8:9) ! silty clay loam, clay loam
!!                        this%sfc(profile)%Soil_Type = 6 ! clay loam
!!                    case (10) ! sandy clay
!!                        this%sfc(profile)%Soil_Type = 5 ! sandy clay
!!                    case (11) ! silty clay
!!                        this%sfc(profile)%Soil_Type = 6 ! clay loam
!!                    case (12) ! clay
!!                        this%sfc(profile)%Soil_Type = 3 ! light clay
!!                    case (13) ! organic
!!                        this%sfc(profile)%Soil_Type = 8 ! farmland
!!                    case (14) ! water
!!                        ! do nothing
!!                    case (16) ! other (land-ice)
!!                        this%sfc(profile)%Soil_Type = 9 ! ice over land
!!                    case default
!!                        print *,'Unknown soil type ',isltyp
!!                        stop
!!                end select
!
!                ! WRF vegetation parameters from the TBL file
!                !
!                !  1     Urban and Built-Up Land
!                !  2     Dryland Cropland and Pasture
!                !  3     Irrigated Cropland and Pasture
!                !  4     Mixed Dryland/Irrigated Cropland and Pasture
!                !  5     Cropland/Grassland Mosaic
!                !  6     Cropland/Woodland Mosaic
!                !  7     Grassland
!                !  8     Shrubland
!                !  9     Mixed Shrubland/Grassland
!                ! 10     Savanna
!                ! 11     Deciduous Broadleaf Forest
!                ! 12     Deciduous Needleleaf Forest
!                ! 13     Evergreen Broadleaf Forest
!                ! 14     Evergreen Needleleaf Forest
!                ! 15     Mixed Forest
!                ! 16     Water Bodies
!                ! 17     Herbaceous Wetland
!                ! 18     Wooded Wetland
!                ! 19     Barren or Sparsely Vegetated
!                ! 20     Herbaceous Tundra
!                ! 21     Wooded Tundra
!                ! 22     Mixed Tundra
!                ! 23     Bare Ground Tundra
!                ! 24     Snow or Ice
!                ! 25     Playa
!                ! 26     Lava
!                ! 27     White Sand
!
!                ! Microwave CRTM vegetation Types (2.1.3 user guide page 33)
!                !
!                ! Vegetation Type                       Classification Index
!                !
!                ! broadleaf-evergreen (tropical forest)                    1
!                ! broad-deciduous trees                                    2
!                ! broadleaf and needleleaf trees (mixed forest)            3
!                ! needleleaf-evergreen trees                               4
!                ! needleleaf-deciduous trees (larch)                       5
!                ! broadleaf trees with ground cover (savanna)              6
!                ! ground cover only (perennial)                            7
!                ! broad leaf shrubs w/ ground cover                        8
!                ! broadleaf shrubs with bare soil                          9
!                ! dwarf trees & shrubs w/ground cover (tundra)            10
!                ! bare soil                                               11
!                ! cultivations                                            12
!                ! glacial                                                 13
!!                ivgtyp = input%getValue2D('IVGTYP',x,y)
!!
!!                ! approximate mapping between the two
!!                select case(ivgtyp)
!!                    case (1) ! Urban and Built-Up Land
!!                        this%sfc(profile)%Vegetation_Type = 11 ! bare soil (?)
!!                    case (2:6) ! Croplands
!!                        this%sfc(profile)%Vegetation_Type = 12 ! cultivations
!!                    case (7) ! Grassland
!!                        this%sfc(profile)%Vegetation_Type = 7 ! ground cover only (perennial
!!                    case (8) ! Shrubland
!!                        this%sfc(profile)%Vegetation_Type = 9 ! broadleaf shrubs with bare soil
!!                    case (9) ! Mixed Shrubland/Grassland
!!                        this%sfc(profile)%Vegetation_Type = 8 ! broad leaf shrubs w/ ground cover
!!                    case (10) ! Savanna
!!                        this%sfc(profile)%Vegetation_Type = 6 ! broadleaf trees with ground cover (savanna)
!!                    case (11) ! Deciduous Broadleaf Forest
!!                        this%sfc(profile)%Vegetation_Type = 2 ! broad-deciduous trees
!!                    case (12) ! Deciduous Needleleaf Forest
!!                        this%sfc(profile)%Vegetation_Type = 5 ! needleleaf-deciduous trees (larch)
!!                    case (13) ! Evergreen Broadleaf Forest
!!                        this%sfc(profile)%Vegetation_Type = 1 ! broadleaf-evergreen (tropical forest)
!!                    case (14) ! Evergreen Needleleaf Forest
!!                        this%sfc(profile)%Vegetation_Type = 4 ! needleleaf-evergreen trees
!!                    case (15) ! Mixed Forest
!!                        this%sfc(profile)%Vegetation_Type = 3 ! broadleaf and needleleaf trees (mixed forest)
!!                    case (16) ! Water Bodies
!!                        this%sfc(profile)%Vegetation_Type = 11 ! bare soil (?) - probably doesn't matter, it's water not land
!!                    case (17) ! Herbaceous Wetland
!!                        this%sfc(profile)%Vegetation_Type = 12 ! cultivations (?) - best of the bad choices?
!!                    case (18) ! Wooded Wetland
!!                        this%sfc(profile)%Vegetation_Type = 1 ! broadleaf-evergreen (tropical forest) (?)
!!                    case (19) ! Barren or Sparsely Vegetated
!!                        this%sfc(profile)%Vegetation_Type = 11 ! bare soil
!!                    case (20:23) ! Herbaceous Tundra
!!                        this%sfc(profile)%Vegetation_Type = 10 ! dwarf trees & shrubs w/ground cover (tundra)
!!                    case (24) ! Snow or Ice
!!                        this%sfc(profile)%Vegetation_Type = 13 ! glacial
!!                    case (25) ! Playa
!!                        this%sfc(profile)%Vegetation_Type = 11 ! bare soil (?)
!!!                    case (26) ! Lava
!!!                        this%sfc(profile)%Vegetation_Type =
!!!                    case (27) ! White Sand
!!!                        this%sfc(profile)%Vegetation_Type =
!!                    case default
!!                        print *,'Unknown soil type ',isltyp
!!                        stop
!!                end select
!
        if (isAdjoint) then
            lu_index = input%getValue2D(LU_INDEX_VAR,x,y)

            if (input%getLandCatCount() .eq. 20) then
                water_class = 17
                snow_class  = 15
            elseif (input%getLandCatCount() .eq. 24) then
                water_class = 16
                snow_class  = 24
            else
                write(msgstr,*) 'Unknown number of land categories: ',input%getLandCatCount(), &
                    ', CRTM obs op cannot continue.'
               call error(msgstr)
            endif

            if (nint(lu_index) .eq. water_class) then
                call input%spreadValue2D(T_SURF_VAR, x,y,sfc(profile)%Water_Temperature)
            else if (nint(lu_index) .eq. snow_class) then
                call input%spreadValue2D(T_SURF_VAR, x,y,sfc(profile)%Snow_Temperature)
            else
                call input%spreadValue2D(T_SURF_VAR, x,y,sfc(profile)%Land_Temperature)
            end if
        else
            lu_index = input%getValue2D(LU_INDEX_VAR,x,y)

            if (input%getLandCatCount() .eq. 20) then
                water_class = 17
                snow_class  = 15
            elseif (input%getLandCatCount() .eq. 24) then
                water_class = 16
                snow_class  = 24
            else
                write(msgstr,*) 'Unknown number of land categories: ',input%getLandCatCount(), &
                    ', CRTM obs op cannot continue.'
               call error(msgstr)
            endif

            if (nint(lu_index) .eq. water_class) then
                sfc(profile)%Water_Coverage = 1.0
                sfc(profile)%Land_Coverage  = 0.0
                sfc(profile)%Snow_Coverage  = 0.0
                sfc(profile)%Ice_Coverage   = 0.0
            else if (nint(lu_index) .eq. snow_class) then
                sfc(profile)%Snow_Coverage  = 1.0
                sfc(profile)%Water_Coverage = 0.0
                sfc(profile)%Land_Coverage  = 0.0
                sfc(profile)%Ice_Coverage   = 0.0
            else
                sfc(profile)%Land_Coverage  = 1.0
                sfc(profile)%Water_Coverage = 0.0
                sfc(profile)%Snow_Coverage  = 0.0
                sfc(profile)%Ice_Coverage   = 0.0

                ! this needs to be mapped, but it's super complicated
                ! see WRF user guide chapter 3, CRTM user guide pg 29-32
                !sfc(profile)%Land_Type = nint(lu_index)
            end if

            sfc(profile)%Land_Temperature  = input%getValue2D(T_SURF_VAR,x,y)
            sfc(profile)%Water_Temperature = input%getValue2D(T_SURF_VAR,x,y)
            sfc(profile)%Snow_Temperature  = input%getValue2D(T_SURF_VAR,x,y)
            sfc(profile)%Ice_Temperature   = input%getValue2D(T_SURF_VAR,x,y)
        end if

        if (isAdjoint) then
            call input%spreadValue2D(V10_VAR,x,y,&
                sfc(profile)%Wind_Speed*cos(4.0*atan(1.d0)*sfc(profile)%Wind_Direction/180.))

            call input%spreadValue2D(U10_VAR,x,y,&
                sfc(profile)%Wind_Speed*sin(-4.0*atan(1.d0)*sfc(profile)%Wind_Direction/180.))
        else
            sfc(profile)%Wind_Speed = input%getWind10mSpeed(x,y)
            sfc(profile)%Wind_Direction = input%getWind10mDirection(x,y)
        end if
    end subroutine

    subroutine setCrtmGeometry(this,input,x,y,profile)
        implicit none

        class(CrtmObservationOperator) :: this
        class(Atmos3DDataSet), pointer :: input
        real(8), intent(in) :: x, y
        integer, intent(in) :: profile

        real(8) :: lat, lon, sfc_z

        sfc_z = input%getValue2D(SFC_HGT_VAR,x,y)

        lon = input%getValue2D(A3D_LON_VAR,x,y)
        lat = input%getValue2D(A3D_LAT_VAR,x,y)

        if (lon .lt. 0) then
            lon = lon + 360.d0
        end if

        if (lat .lt. 0) then
            lat = lat + 360.d0
        end if

        this%geo(profile)%Longitude = lon
        this%geo(profile)%Latitude = lat
        this%geo(profile)%Surface_Altitude = sfc_z
        this%geo(profile)%Sensor_Scan_Angle   = 48.5
        this%geo(profile)%Sensor_Zenith_Angle = 52.821
        this%geo(profile)%year = 2010
        this%geo(profile)%month = 8
        this%geo(profile)%day = 31
    end subroutine

    subroutine setCrtmOptions(this,input,x,y,profile)
        implicit none

        class(CrtmObservationOperator) :: this
        class(Atmos3DDataSet), pointer :: input
        real(8), intent(in) :: x, y
        integer, intent(in) :: profile

        this%opt(profile)%Check_Input = .true.
    end subroutine


    subroutine initializeCrtmFwd(this,sensorId,nprofiles,nlayers,nabsorbers,nclouds,naerosols)
        implicit none

        class(CrtmObservationOperator) :: this

        integer, intent(in) :: nprofiles
        character(len=20), intent(in) :: sensorId
        integer, intent(in) :: nlayers, nabsorbers, nclouds, naerosols

        character(len=20), dimension(1) :: sensorIds
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        integer :: allocStat, errStat, m

        if (this%initializedFwd) return

        allocate(this%dptr(nlayers+1),this%cptr(nlayers+1))

        sensorIds(1) = sensorId

        write(msgstr, '(A,I6,A)') 'Initializing CRTM fwd for ',nprofiles,' profiles'
        call print(msgstr)

        errStat = CRTM_init(sensorIds, chinfos, Load_CloudCoeff = .true., Load_AerosolCoeff = .false., Quiet = .true., &
            IRwaterCoeff_File='IR_WATER.bin', IRlandCoeff_File='IR_LAND.bin', &
            VISlandCoeff_File='VIS_LAND.bin', IRsnowCoeff_File='IR_SNOW.bin', &
            IRiceCoeff_File='IR_ICE.bin',     VISwaterCoeff_File='VIS_WATER.bin', &
            VISsnowCoeff_File='VIS_SNOW.bin', VISiceCoeff_File='VIS_ICE.bin', &
            MWwaterCoeff_File='MW_WATER.bin')

        if (errStat /= SUCCESS) then
            write(msgstr,*) 'CRTM_init errStat:',errStat,' for sensor: ',sensorId
            call error(msgstr)
        endif

        if (associated(this%platform%getChannelSubset())) then
            errStat = CRTM_ChannelInfo_Subset(chInfos(1),this%platform%getChannelSubset())
        else
            ! reset the channels so all are used
            errStat = CRTM_ChannelInfo_Subset(chInfos(1),RESET=.true.)
        end if

        this%chinfo = chinfos(1)

        allocate(this%geo(nprofiles), &
            this%opt(nprofiles), &
            this%atm(nprofiles), &
            this%sfc(nprofiles), &
            stat = allocStat)

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM arrays: ',allocStat
            call error(msgstr)
        end if

        this%nchannels = CRTM_ChannelInfo_n_Channels( chinfos(1) )

        ! Allocate channel-dependent arrays
        allocate( this%rts(this%nchannels, nprofiles),STAT = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM result matrix: ',allocStat
            call error(msgstr)
        end if

        call CRTM_Atmosphere_Create(this%atm  , &
                                    nlayers   , &
                                    nabsorbers, &
                                    nclouds   , &
                                    naerosols)

        if (any(.not. CRTM_Atmosphere_Associated(this%atm))) then
            write(msgstr,*) 'Error initializing CRTM atmosphere object'
            call error(msgstr)
        end if

        call CRTM_RTSolution_Create(this%rts, nlayers)

        if (any(.not. CRTM_RTSolution_Associated(this%rts))) then
            write(msgstr,*) 'Error initializing CRTM RT Solution'
            call error(msgstr)
        end if

        call CRTM_Options_Create(this%opt, this%nchannels)

        IF ( ANY(.NOT. CRTM_Options_Associated( this%opt )) ) THEN
            write(msgstr,*) 'Error initializing CRTM Options'
            call error(msgstr)
        END IF

        this%nprofiles = nprofiles
        this%nlayers = nlayers
        this%nabsorbers = nabsorbers
        this%nclouds = nclouds
        this%naerosols = naerosols
        this%initializedFwd = .true.

        write(msgstr,*) 'CRTM_init successfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine initializeCrtmTL(this,sensorId,nprofiles,nlayers,nabsorbers,nclouds,naerosols)
        implicit none

        class(CrtmObservationOperator) :: this

        integer, intent(in) :: nprofiles
        character(len=20), intent(in) :: sensorId
        integer, intent(in) :: nlayers, nabsorbers, nclouds, naerosols

        character(len=20), dimension(1) :: sensorIds
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        integer :: allocStat, errStat, m

        if (this%initializedTL) return

        write(msgstr,'(A,I6,A)') 'Initializing CRTM TL for ',nprofiles,' profiles'
        call print(msgstr)

        allocate(this%atm_tl(nprofiles), &
            this%sfc_tl(nprofiles), &
            stat = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM TL arrays: ',allocStat
            call error(msgstr)
        end if

        ! Allocate channel-dependent arrays
        allocate(this%rts_tl(this%nchannels, nprofiles),STAT = allocStat)

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM result matrix: ',allocStat
            call error(msgstr)
        end if

        call CRTM_Atmosphere_Create(this%atm_tl, &
                                    nlayers   , &
                                    nabsorbers, &
                                    nclouds   , &
                                    naerosols)

        if (any(.not. CRTM_Atmosphere_Associated(this%atm_tl))) then
            write(msgstr,*) 'Error initializing CRTM atmosphere TL object'
            call error(msgstr)
        end if

        call CRTM_RTSolution_Create(this%rts_tl, nlayers)

        if (any(.not. CRTM_RTSolution_Associated(this%rts_tl))) then
            write(msgstr,*) 'Error initializing CRTM RT TL Solution'
            call error(msgstr)
        end if

        this%initializedTL = .true.

        write(msgstr,*) 'CRTM_init TL succesfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine initializeCRTMAdj(this,sensorId,nprofiles,nlayers,nabsorbers,nclouds,naerosols)
        implicit none

        class(CrtmObservationOperator) :: this

        integer, intent(in) :: nprofiles
        character(len=20), intent(in) :: sensorId
        integer, intent(in) :: nlayers, nabsorbers, nclouds, naerosols

        character(len=20), dimension(1) :: sensorIds
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        integer :: allocStat, errStat, m

        if (this%initializedAdj) return

        write(msgstr, '(A,I6,A)') 'Initializing CRTM adj for ',nprofiles,' profiles'
        call print(msgstr)

        allocate(this%atm_adj(nprofiles), &
            this%sfc_adj(nprofiles), &
            stat = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM Adj arrays: ',allocStat
            call error(msgstr)
        end if

        ! Allocate channel-dependent arrays
        allocate(this%rts_adj(this%nchannels, nprofiles),STAT = allocStat)

        if (allocStat /= 0) then
            write(msgstr,*) 'Error initializing CRTM adj result matrix: ',allocStat
            call error(msgstr)
        end if

        call CRTM_Atmosphere_Create(this%atm_adj, &
                                    nlayers   , &
                                    nabsorbers, &
                                    nclouds   , &
                                    naerosols)

        if (any(.not. CRTM_Atmosphere_Associated(this%atm_adj))) then
            write(msgstr,*) 'Error initializing CRTM atmosphere adj object'
            call error(msgstr)
        end if

        call CRTM_RTSolution_Create(this%rts_adj, nlayers)

        if (any(.not. CRTM_RTSolution_Associated(this%rts_adj))) then
            write(msgstr,*) 'Error initializing CRTM RT adj Solution'
            call error(msgstr)
        end if

        this%initializedAdj = .true.

        write(msgstr,*) 'CRTM_init Adj succesfully initialized.'
        call print(msgstr)
    end subroutine

    subroutine finalizeCrtmFwd(this)
        implicit none

        class(CrtmObservationOperator) :: this

        integer :: allocStat, errStat
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        write(msgstr,*) 'Finalizing CRTM'
        call print(msgstr)

        chinfos(1) = this%chinfo

        errStat = CRTM_Destroy( chinfos )

        if ( errStat /= SUCCESS ) then
            write(msgstr,*) 'Error destroying CRTM'
            call error(msgstr)
        endif

        deallocate( this%geo, this%opt, this%atm, this%sfc, this%rts, stat = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error deallocating CRTM arrays: ',allocStat
            call error(msgstr)
        end if

        this%initializedFwd = .false.
    end subroutine

    subroutine finalizeCrtmTL(this)
        implicit none

        class(CrtmObservationOperator) :: this

        integer :: allocStat
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        write(msgstr,*) 'Finalizing CRTM TL'
        call print(msgstr)

        deallocate( this%atm_tl, this%sfc_tl, stat = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error deallocating CRTM TL arrays: ',allocStat
            call error(msgstr)
        end if

        this%initializedTL = .false.
    end subroutine

    subroutine finalizeCrtmAdj(this)
        implicit none

        class(CrtmObservationOperator) :: this

        integer :: allocStat
        type(CRTM_ChannelInfo_type), dimension(1) :: chinfos

        write(msgstr,*) 'Finalizing CRTM adj'
        call print(msgstr)

        deallocate( this%atm_adj, this%sfc_adj, stat = allocStat )

        if (allocStat /= 0) then
            write(msgstr,*) 'Error deallocating CRTM adj arrays: ',allocStat
            call error(msgstr)
        end if

        this%initializedAdj = .false.
    end subroutine

    ! gamma function
    function gf(xx)
        implicit none

        integer i
        real(8) gf
        real(8) cof(6),xx,tmp,ser,x
        data cof/76.18009d0,-86.50532d0,24.01410d0,-1.231740d0,.1208580d-2, &
            -.536382d-5/

        x=xx-1.d0

        tmp=(x+5.5d0)**(x+0.5d0)*exp(-(x+5.5d0))*sqrt(6.283185d0)
        ser=1.
        do i=1,6
            x=x+1.
            ser=ser+cof(i)/x
        end do

        gf=tmp*ser
        return
    end function gf

end module
