module ccvObservationOperator_mod
    use satelliteObservationOperator_mod
    use atmos3DDataSet_mod
    use rtmOptions_mod
    use satellitePlatformInfo_mod
    use satelliteObservation_mod
    use dataVariable_mod

    use radianceCcv_mod

    implicit none

    private

    public :: CcvObservationOperator

    type, extends(SatelliteObservationOperator) :: CcvObservationOperator
        character(len=256), dimension(:), allocatable :: modelVarNames
        integer, dimension(:), allocatable            :: modelVarNz
        class(SatellitePlatformInfo), pointer         :: platform

        real(8), pointer, dimension(:) :: dptr, dptr2

        contains
            procedure :: doForward
            procedure :: doTLM
            procedure :: doAdjoint
            procedure :: getName
            final :: ccvObservationOperatorDestructor ! clean up all allocated variables
    end type

    interface CcvObservationOperator
        procedure ccvObservationOperatorConstructor ! allow generic instantiation
    end interface

    contains

    function ccvObservationOperatorConstructor(modelVarNames,modelVarNz,platform) result(this)
        implicit none

        class(CcvObservationOperator), pointer :: this

        character(len=256), dimension(:)      :: modelVarNames
        integer, dimension(:)                 :: modelVarNz
        class(SatellitePlatformInfo), pointer :: platform

        integer :: totalNz

        allocate(this)
        allocate(this%modelVarNames(size(modelVarNames)))
        this%modelVarNames(:) = modelVarNames(:)

        allocate(this%modelVarNz(size(modelVarNz)))
        this%modelVarNz(:) = modelVarNz(:)

        totalNz = sum(modelVarNz(:))

        allocate(this%dptr(totalNz))
        allocate(this%dptr2(totalNz))

        this%platform => platform
    end function

    subroutine ccvObservationOperatorDestructor(this)
        implicit none

        type(CcvObservationOperator)  :: this
        deallocate(this%modelVarNames)
        deallocate(this%dptr)
        deallocate(this%dptr2)
    end subroutine

    function getName(this) result(name)
        implicit none

        class(CcvObservationOperator) :: this
        character(128) :: name

        !name = 'Canonical-Correlations-Based Satellite Operator'
        name = 'CCV'
    end function

    subroutine doForward(this, input, obs, output)
        implicit none

        class(CcvObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: input
        class(SatelliteObservation), pointer :: obs
        real(8), dimension(:,:),     pointer :: output

        real(8), dimension(:),   pointer :: obsx, obsy

        real(8), dimension(:,:), pointer :: obsData
        real(8), dimension(:,:), pointer :: obsLoci
        real(8), dimension(:,:), pointer :: auxData

        integer :: nprofiles ! m

        integer :: maxi,maxj,maxk,obsnum

        character(len=20) :: sensorId

        integer :: allocStat, errStat

        real(8) :: x, y

        real(8), dimension(:), pointer :: obsptr, auxptr
        integer, dimension(:), allocatable :: ccvNums

        class(DataVariable), pointer :: obsDataVar

        real :: start, finish

        real(8) :: lat, lon

        integer :: z, nloc

!        call cpu_time(start)

        obsLoci => obs%getObsLoci()
        auxData => obs%getAuxData()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)

        nprofiles = size(obsx)

        allocate(ccvNums(obs%platform%mobs))

        do z=1,obs%platform%mobs
            ccvNums(z) = z
        end do

        obsDataVar => obs%getObsDataVar()

        call obsDataVar%getArray(obsData)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            call input%getColumnFields(x,y,.false.,this%dptr,this%modelVarNames)

            obsptr => obsData(:,obsnum)
            auxptr => auxData(:,obsnum)

            call radCcvManager%forward(nint(auxptr(1)),nint(auxptr(2)),ccvNums,this%dptr,nint(auxptr(3)),obsptr)
        end do

        call cpu_time(finish)

        deallocate(ccvNums)

!        print '(A,I6,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)','Successfully ran CCV for ',nprofiles,&
!            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'

    end subroutine

    subroutine doTLM(this, baseState, obs, deltaX, deltaY)
        implicit none

        class(CcvObservationOperator) :: this

        class(Atmos3DDataSet),    pointer :: baseState
        class(SatelliteObservation), pointer :: obs
        class(Atmos3DDataSet),    pointer :: deltaX
        real(8), dimension(:,:),     pointer :: deltaY

        integer :: nprofiles,nchannels
        real(8), pointer :: obsx(:), obsy(:)
        real(8), pointer, dimension(:) :: obsptr, obsptr_tl, auxptr

        real(8), dimension(:,:), pointer :: obsData
        real(8), dimension(:,:), pointer :: obsLoci
        real(8), dimension(:,:), pointer :: auxData

        integer, dimension(:), allocatable :: ccvNums

        integer :: obsnum, z

        real(8) :: x, y
        real(8) :: lat, lon

        logical :: tl_has_cloud
        character(len=20) :: sensorId

        real :: start, finish

!        call cpu_time(start)

        allocate(ccvNums(obs%platform%mobs))

        do z=1,obs%platform%mobs
            ccvNums(z) = z
        end do

        obsLoci => obs%getObsLoci()
        auxData => obs%getAuxData()
        obsData => obs%getObsData()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)
        nprofiles = size(obsx,1)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            call baseState%getColumnFields(x,y,.false.,this%dptr,this%modelVarNames)
            call deltaX%getColumnFields(x,y,.false.,this%dptr2,this%modelVarNames)

            obsptr    => obsData(:,obsnum)
            obsptr_tl => deltaY(:,obsnum)
            auxptr    => auxData(:,obsnum)

            call radCcvManager%tangentLinear(nint(auxptr(1)),nint(auxptr(2)),ccvNums,this%dptr,this%dptr2,&
                nint(auxptr(3)),obsptr,obsptr_tl)
        end do

        call cpu_time(finish)

        deallocate(ccvNums)

!        print '(A,I6,A,I4,A,I4,A,I4,A,I4,A,F6.2,A)','Successfully ran CCV TL for ',nprofiles,&
!            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'

    end subroutine

    subroutine doAdjoint(this, baseState, obs, deltaY, deltaX)
        implicit none

        class(CcvObservationOperator)  :: this

        class(Atmos3DDataSet),    pointer :: baseState
        class(SatelliteObservation), pointer :: obs
        real(8), dimension(:,:),     pointer :: deltaY
        class(Atmos3DDataSet),    pointer :: deltaX

        real(8), dimension(:,:), pointer :: obsData
        real(8), dimension(:,:), pointer :: obsLoci
        real(8), dimension(:,:), pointer :: auxData

        real(8), pointer :: obsx(:), obsy(:)
        real(8), dimension(:), pointer :: obsptr, auxptr, obsptr_adj

        integer, dimension(:), allocatable :: ccvNums

        integer :: obsnum, z

        real :: start, finish

        real(8) :: x, y

        integer :: nprofiles

!        call cpu_time(start)

        allocate(ccvNums(obs%platform%mobs))

        do z=1,obs%platform%mobs
            ccvNums(z) = z
        end do

        obsLoci => obs%getObsLoci()
        auxData => obs%getAuxData()
        obsData => obs%getObsData()

        obsx => obsLoci(SO_PIX_DIM,:)
        obsy => obsLoci(SO_SCAN_DIM,:)
        nprofiles = size(obsx)

        do obsnum=1,nprofiles
            x = obsx(obsnum)
            y = obsy(obsnum)

            call baseState%getColumnFields(x,y,.false.,this%dptr,this%modelVarNames)
            call baseState%getColumnFields(x,y,.false.,this%dptr2,this%modelVarNames)

            obsptr     => obsData(:,obsnum)
            obsptr_adj => deltaY(:,obsnum)
            auxptr     => auxData(:,obsnum)

            call radCcvManager%adjoint(nint(auxptr(1)),nint(auxptr(2)),ccvNums,&
                this%dptr,obsptr_adj,nint(auxptr(3)),obsptr,this%dptr2)
        end do

!        print '(A,I6,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A,F6.2,A)','Successfully ran CCV adjoint for ',nprofiles,&
!            ' profiles (',obsx(1),',',obsy(1),') to (',obsx(nprofiles),',',obsy(nprofiles),') in ',finish-start,' seconds'
    end subroutine
end module
