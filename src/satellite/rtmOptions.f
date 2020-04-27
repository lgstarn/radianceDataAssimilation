module rtmOptions_mod
    implicit none

    private

    public :: RtmOptions

    type :: RtmOptions
        logical, public :: includeSolar
        logical, public :: checkInput
        logical, public :: includeRefraction
        logical, public :: includeClouds
        logical, public :: includeAerosols

        logical, public :: includeOzone
        logical, public :: includeCO2
        logical, public :: includeN2O
        logical, public :: includeCH4
        logical, public :: includeCO
        logical, public :: directlySpecifyOpticalParameters

        real, public :: zenithAngle
        real, public :: azimuthAngle
        real, public :: sunZenithAngle
        real, public :: sunAzimuthAngle

        character(len=256), dimension(:), pointer, public :: modelVarNames
        integer, dimension(:), pointer, public :: modelVarNz

        contains
            final :: rtmOptionsDestructor ! clean up all allocated variables
    end type

    interface RtmOptions
        procedure rtmOptionsConstructor ! allow generic instantiation
    end interface

    contains

    function rtmOptionsConstructor() result(this)
        implicit none

        class(RtmOptions), pointer  :: this

        allocate(this)

        ! set defaults
        this%includeSolar  = .false.
        this%checkInput  = .true.
        this%includeRefraction   = .false.
        this%includeClouds  = .false.
        this%includeAerosols  = .false.

        this%includeOzone = .false.
        this%includeCO2 = .false.
        this%includeN2O = .false.
        this%includeCH4 = .false.
        this%includeCO = .false.
        this%directlySpecifyOpticalParameters = .false.

        this%zenithAngle = 49.195
        this%azimuthAngle = 0.0
        this%sunZenithAngle = 0.0
        this%sunAzimuthAngle = 0.0

    end function

    subroutine rtmOptionsDestructor(this)
        implicit none

        type(RtmOptions)  :: this

    end subroutine

end module
