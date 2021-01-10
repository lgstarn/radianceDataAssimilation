module scannedObservation_mod

    use parallelInfo_mod
    use parallelConstants_mod

    use iso_fortran_env

    use platformInfo_mod

    use observation_mod
    use satelliteObservation_mod
    use satellitePlatformInfo_mod

    !use dataGrid_mod
    !use regularTriangularLatLonGrid_mod

    use dataSet_mod
    use dataGroup_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataDimension_mod
    use dataArrayReader_mod

    use triangularTiling_mod

    use geodesic_mod
    use datetime_mod
    use mpiUtils_mod

    implicit none

    private

    type, extends(SatelliteObservation), public :: ScannedObservation
        private
            real(real64), dimension(:),  pointer :: fovAlong
            real(real64), dimension(:),  pointer :: fovCross
            real(real64), dimension(:),  pointer :: intTime
            !logical                              :: gridded  = .false.
            !class(RegularTriangularLatLonGrid), pointer :: grid     => null()

            ! minimum distance between scans (km), used for quick distance approximations
            real(real64)                            :: scanDistance
        contains
            procedure :: loadScannedObservation
            procedure :: getFovAlong
            procedure :: getFovAlongPointer
            procedure :: getFovCross
            procedure :: getFovCrossPointer
            procedure :: getMaxFovAlong
            procedure :: getMaxFovCross
            procedure :: getIntegrationTime
            procedure :: getIntegrationTimePointer
            procedure :: getScanDistance
            !procedure :: getGrid
            !procedure :: findGrid

            procedure :: clone
            procedure :: cloneScannedObs

            procedure :: scannedObservationConstructor
            final     :: scannedObservationDestructor
    end type

    contains

    subroutine scannedObservationConstructor(this,platform,reader,&
        &fovAlong,fovCross,intTime,choffset,obsErr,uniquechoffset)

        implicit none

        class(ScannedObservation)                :: this

        class(SatellitePlatformInfo), pointer    :: platform
        class(DataArrayReader),       pointer    :: reader
        real(real64), dimension(:),   intent(in) :: fovAlong
        real(real64), dimension(:),   intent(in) :: fovCross
        real(real64), dimension(:),   intent(in) :: intTime
        integer,                      intent(in) :: choffset
        real(real64), dimension(:),   intent(in) :: obsErr

!        type(datetime),   optional,     intent(in)  :: cdate
!        integer,          optional,     intent(in)  :: naux
        integer,          optional,     intent(in)  :: uniquechoffset

        allocate(this%fovAlong(size(fovAlong)))
        allocate(this%fovCross(size(fovCross)))
        allocate(this%intTime (size(intTime)))

        this%fovAlong(:) = fovAlong(:)
        this%fovCross(:) = fovCross(:)
        this%intTime (:) = intTime (:)

        call this%satelliteObservationConstructor(platform,reader,&
            &choffset,obsErr,uniquechoffset)
    end subroutine

    subroutine scannedObservationDestructor(this)
        implicit none

        type(ScannedObservation)  :: this

        if (associated(this%fovAlong)) then
            deallocate(this%fovAlong)
        end if

        if (associated(this%fovCross)) then
            deallocate(this%fovCross)
        end if

        if (associated(this%intTime)) then
            deallocate(this%intTime)
        end if

        !if (associated(this%grid)) then
        !    deallocate(this%grid)
        !end if
    end subroutine

    subroutine loadScannedObservation(this,pinfo,tbVar,latVar,lonVar,scLatVar,scLonVar,&
        & yearVar,monthVar,dayVar,hourVar,minuteVar,secondVar)

        use mpi

        implicit none

        class(ScannedObservation)  :: this

        class(ParallelInfo),            pointer    :: pinfo

        ! real(real32) nchan, npix, nscan
        class(DataVariable),            pointer    :: tbVar
        ! real(real32) npix, nscan
        class(DataVariable),            pointer    :: latVar
        ! real(real32) npix, nscan
        class(DataVariable),            pointer    :: lonVar
        ! real(real32) nscan
        class(DataVariable),  optional, pointer    :: scLatVar
        ! real(real32) nscan
        class(DataVariable),  optional, pointer    :: scLonVar
        ! integer nscan
        class(DataVariable),  optional, pointer    :: yearVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: monthVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: dayVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: hourVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: minuteVar
        ! integer(1) nscan
        class(DataVariable),  optional, pointer    :: secondVar

        real(real64) :: s12, azi1, azi2
        real(real64) :: s12min, s12minall

        integer :: i, j, ierr

        real(real32), pointer :: lat(:,:)
        real(real32), pointer :: lon(:,:)

        call latVar%getArray(lat)
        call lonVar%getArray(lon)

        s12min = 1.d99

        do i=1,size(lat,2)-1
            do j=1,size(lat,1)
                call geodesic_inverse(dble(lat(j,i)),dble(lon(j,i)),&
                    &dble(lat(j,i+1)),dble(lon(j,i+1)), s12, azi1, azi2, 2)

                ! convert to km
                s12 = s12/1000.d0

                s12min = min(s12,s12min)
            end do
        end do

        if (pinfo%getParallelType() == DISTRIBUTED_PARALLEL_TYPE) then
            call MPI_AllReduce(s12min, s12minAll, 1, MPI_DOUBLE_PRECISION, MPI_MIN, &
                pinfo%getCommunicator(), ierr)

            call mpichk(ierr,'Scanned obs all reduce')
        else
            s12minAll = s12min
        end if

        ! this is a quick estimate of the minimum distance between scans
        this%scanDistance = s12minAll

        call this%loadSatelliteObservation_tb3d(pinfo,latVar,lonVar,tbVar)
    end subroutine

    function getFovAlong(this,channel) result(fovAlong)

        implicit none

        class(ScannedObservation)  :: this
        integer, intent(in)        :: channel

        real(real64) :: fovAlong

        fovAlong = this%fovAlong(channel)
    end function

    function getFovAlongPointer(this) result(fovAlong)

        implicit none

        class(ScannedObservation)  :: this

        real(real64), pointer :: fovAlong(:)

        fovAlong => this%fovAlong
    end function

    function getFovCross(this,channel) result(fovCross)

        implicit none

        class(ScannedObservation)  :: this
        integer, intent(in)        :: channel

        real(real64) :: fovCross

        fovCross = this%fovCross(channel)
    end function

    function getFovCrossPointer(this) result(fovCross)

        implicit none

        class(ScannedObservation)  :: this

        real(real64), pointer :: fovCross(:)

        fovCross => this%fovCross
    end function

    function getMaxFovAlong(this) result(fovAlong)

        implicit none

        class(ScannedObservation)  :: this

        real(real64) :: fovAlong

        fovAlong = maxval(this%fovAlong)
    end function

    function getMaxFovCross(this) result(fovCross)

        implicit none

        class(ScannedObservation)  :: this

        real(real64) :: fovCross

        fovCross = maxval(this%fovCross)
    end function

    function getIntegrationTime(this,channel) result(intTime)

        implicit none

        class(ScannedObservation)  :: this
        integer, intent(in)        :: channel

        real(real64) :: intTime

        intTime = this%intTime(channel)
    end function

    function getIntegrationTimePointer(this) result(intTimes)
        implicit none

        class(ScannedObservation)  :: this

        real(real64), pointer :: intTimes(:)

        intTimes => this%intTime
    end function

    function getScanDistance(this) result(scanDistance)

        implicit none

        class(ScannedObservation)  :: this

        real(real64) :: scanDistance

        scanDistance = this%scanDistance
    end function

    function cloneScannedObs(this,shallow,copyData) result(soptr)
        implicit none

        class(ScannedObservation), target :: this

        logical, optional, intent(in) :: shallow
        logical, optional, intent(in) :: copyData

        class(DataSet),            pointer :: dsPtr
        class(DataGroup),          pointer :: dgPtr
        class(ScannedObservation), pointer :: soPtr

        logical :: isShallow
        logical :: doCopy

        allocate(soptr)
        if (associated(this%reader)) then
            call soptr%scannedObservationConstructor(this%getSatellitePlatform(), &
                & this%reader%clone(),this%fovAlong,this%fovCross,this%intTime,   &
                & this%getChannelOffset(),this%getObservationError(),             &
                & this%getUniqueChannelOffset())
        else
            call soptr%scannedObservationConstructor(this%getSatellitePlatform(), &
                & this%reader,        this%fovAlong,this%fovCross,this%intTime,   &
                & this%getChannelOffset(),this%getObservationError(),             &
                & this%getUniqueChannelOffset())
        end if

        if (present(shallow)) then
            isShallow = shallow
        else
            isShallow = .false.
        end if

        if (present(copyData)) then
            doCopy = copyData
        else
            doCopy = .false.
        end if

        if (.not. isShallow) then
            dsPtr => soPtr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if

!        if (this%gridded) then
!            soptr%grid => this%grid%cloneRegularTriangularLatLonGrid(doCopy)
!            soptr%gridded = this%gridded
!        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(ScannedObservation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),            pointer :: dsPtr
        class(ScannedObservation), pointer :: soPtr

        soPtr => this%cloneScannedObs(shallow,copyData)
        dsPtr => soPtr
    end function
end module
