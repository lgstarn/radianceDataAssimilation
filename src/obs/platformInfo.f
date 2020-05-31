module platformInfo_mod

    implicit none

    private

    type, public :: PlatformInfo
        integer            :: platformNumber ! a number corresponding to the platform, see radianceAssimilationFactory
        character(len=256) :: platformName   ! the name of the platform
        character(len=256) :: sensorName     ! the name of the sensor, e.g. TMI. Platforms with multiple
                                             ! sensors will have one instance per sensor, and transformed
                                             ! observations will have their own sensorName
        integer :: mobs                      ! number of individual observations (channels, CCVs, etc.)
                                             ! per observation location

        contains
            procedure :: platformInfoConstructor
            final :: platformInfoDestructor ! clean up all allocated variables
    end type

    contains

    subroutine platformInfoConstructor(this,platformNumber,platformName,sensorName,mobs)
        implicit none

        class(PlatformInfo) :: this
        integer, intent(in)          :: platformNumber
        character(len=*), intent(in) :: platformName
        character(len=*), intent(in) :: sensorName
        integer, intent(in)          :: mobs

        this%platformNumber = platformNumber
        this%platformName   = platformName
        this%sensorName     = sensorName
        this%mobs           = mobs
    end subroutine

    subroutine platformInfoDestructor(this)
        implicit none

        type(PlatformInfo)  :: this

    end subroutine

end module
