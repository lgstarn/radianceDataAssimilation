module ccvObservation_mod
    use satelliteObservation_mod
    use radianceCcv_mod
    use satellitePlatformInfo_mod
    use platformInfo_mod
    use dictionary_mod
    use diagonalMatrixOperator_mod
    use mpiUtils_mod

    implicit none

    private

    public :: CcvObservation

    type, extends(SatelliteObservation) :: CcvObservation
        private

        integer                              :: hType
        logical                              :: useGlobal
        integer, dimension(:), allocatable   :: ccvNums
        class(Dictionary), pointer           :: regionsDict

        contains
            procedure :: setObservation
            !   procedure :: getRInvHalf => getRInvHalfCcv
            procedure :: ccvObservationConstructor
            final :: ccvObservationDestructor ! clean up all allocated variables
    end type

!    interface CcvObservation
!        procedure ccvObservationConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine ccvObservationConstructor(this,platform,hType,useGlobal,fullObs,useQC)
        implicit none

        class(CcvObservation) :: this

        class(SatellitePlatformInfo), pointer :: platform
        integer, intent(in)           :: hType
        logical, intent(in)           :: useGlobal
        logical, intent(in), optional :: useQC

        class(SatelliteObservation), pointer :: fullObs

        integer :: i, j
        real(8), dimension(:), pointer :: optr, dptr1, dptr2

        real(8), dimension(platform%mobs) :: rInv

        class(DiagonalMatrixOperator), pointer :: rInvHalf_diag
        class(*), pointer :: objptr

        character(len=256) :: key

        !allocate(this)
        !call this%observationConstructor(obsData,obsOp,rInvHalf,3,.true.)

        this%regionsDict => Dictionary(10)
        this%hType     = hType
        this%useGlobal = useGlobal

        call this%satelliteObservationConstructor_empty(platform,fullObs%getNObs(),useQC,3)

        allocate(this%ccvNums(platform%mobs))
        do i=1,platform%mobs
            this%ccvNums(i) = i
        end do

        do i=1,fullObs%getNObs()
            optr => fullObs%getObservation(i)
            call this%setObservation(i,optr)
        end do

        if (this%useGlobal) then
            call radCcvManager%getRSquared(platform%platformNumber,0,this%ccvNums,rInv)
            do i=1,platform%mobs
                rInv(i) = 1.0d0/sqrt(1-rInv(i))
            end do

            allocate(rInvHalf_diag)
            call rInvHalf_diag%diagonalMatrixOperatorConstructor(rInv)
            objptr => rInvHalf_diag

            write(key,'(i10)') 0

            call this%regionsDict%add(key,objptr)
        else
            do i=1,radCcvManager%getNumberOfRegions(platform%platformNumber)
                call radCcvManager%getRSquared(platform%platformNumber,i,this%ccvNums,rInv)
                do j=1,platform%mobs
                    rInv(j) = 1.0d0/sqrt(1-rInv(j))
                end do

                allocate(rInvHalf_diag)
                call rInvHalf_diag%diagonalMatrixOperatorConstructor(rInv)
                objptr => rInvHalf_diag

                write(key,'(i10)') i

                call this%regionsDict%add(key,objptr)
            end do
        end if

        do i=1,fullObs%getNObs()
            dptr1 => this%getObsLoci(i)
            dptr2 => fullObs%getObsLoci(i)

            if (associated(dptr1) .and. associated(dptr2)) then
                dptr1(:) = dptr2(:)
            end if
        end do
    end subroutine

    subroutine ccvObservationDestructor(this)
        implicit none

        type(CcvObservation)  :: this

        deallocate(this%ccvNums)
        deallocate(this%regionsDict) ! deallocates all of the contents as well

        !call this%observationDestructor()
    end subroutine

    ! overridden method to get the matrix operator for the given observation; look up the
    ! region this is associated with and return that pointer
!    function getRInvHalfCcv(this,n) result(rInvHalf)
!        implicit none
!
!        class(CcvObservation) :: this
!        integer, intent(in) :: n
!
!        class(*), pointer :: optr
!        class(AbstractVectorOperator), pointer :: rInvHalf
!        character(len=256) :: key
!        integer :: region
!        region = this%obsData%auxData(2,n)
!        write(key,'(i10)') region
!        if (.not. this%regionsDict%hasKey(key)) then
!            print *,'Could not find the region named ',adjustl(trim(key))
!        end if
!
!        optr => this%regionsDict%get(key)
!        select type(optr)
!            class is (AbstractVectorOperator)
!                rInvHalf => optr
!            class default
!                print *,'Unknown class in ccvObs dictionary.'
!                stop
!        end select
!    end function

    subroutine setObservation(this,n,odata)
        implicit none

        class(CcvObservation) :: this
        integer, intent(in) :: n
        real(8), pointer :: odata(:)

        integer :: regionNum
        real(8), pointer :: obsptr(:), auxptr(:)
        class(PlatformInfo), pointer :: platform

        if (this%useGlobal) then
            regionNum = 0
        else
            regionNum = radCcvManager%getRegionNumber(this%platform%platformNumber,odata)
        end if

        obsptr => this%getObservation(n)

        call radCcvManager%getObsCcv(this%platform%platformNumber,regionNum,this%ccvNums,odata,obsptr)

        auxptr => this%getAuxData(n)

        auxptr(1) = platform%platformNumber
        auxptr(2) = regionNum
        auxptr(3) = this%hType
    end subroutine

end module
