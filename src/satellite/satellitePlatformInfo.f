module satellitePlatformInfo_mod
    use platformInfo_mod

    implicit none

    private

    type, public, extends(PlatformInfo) :: SatellitePlatformInfo
        !private

        integer, dimension(:), pointer :: channelSubset => NULL() ! a list of the subset of channels to use, or null if none 
        contains
            procedure :: getNChannels
            procedure :: getChannelSubset

            procedure :: satellitePlatformInfoConstructor
            final :: satellitePlatformInfoDestructor ! clean up all allocated variables
    end type

    contains

    subroutine satellitePlatformInfoConstructor(this,platformNumber,platformName,&
        & sensorName,nchan,channelSubset)
        implicit none

        class(SatellitePlatformInfo) :: this
        integer, intent(in)          :: platformNumber
        character(len=*), intent(in) :: platformName, sensorName
        integer, intent(in)          :: nchan
        integer, dimension(:), pointer, optional :: channelSubset

        integer :: nchanOther

        call this%platformInfoConstructor(platformNumber,platformName,sensorName,nchan)

        if (present(channelSubset)) then
            this%channelSubset => channelSubset
        end if
    end subroutine

    subroutine satellitePlatformInfoDestructor(this)
        implicit none

        type(SatellitePlatformInfo)  :: this

        if (associated(this%channelSubset)) then
            deallocate(this%channelSubset)
            nullify(this%channelSubset)
        end if
    end subroutine

    function getNChannels(this) result(nchan)
        implicit none

        class(SatellitePlatformInfo)  :: this

        integer :: nchan

        if (associated(this%channelSubset)) then
            nchan = size(this%channelSubset)
        else
            nchan = this%mobs
        end if

    end function

    function getChannelSubset(this) result(chsubset)
        implicit none

        class(SatellitePlatformInfo)  :: this

        integer, pointer :: chsubset(:)

        chsubset => this%channelSubset
    end function
end module
