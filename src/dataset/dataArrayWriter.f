module dataArrayWriter_mod
    use dataArray_mod
    use parallelInfo_mod
    use dataVariable_mod
    use dataDimension_mod

    implicit none

    private

    public :: DataArrayWriter

    ! An abstract class that represents some underlying model of data. All data is assumed to
    ! have a dimensionality (e.g. 1D, 2D, 3D, etc.) as well as named variables.
    type, abstract :: DataArrayWriter
        character(:), allocatable :: location

        contains

            procedure(writeDimensionAbstract), deferred :: writeDimension
            procedure(writeVariableAbstract),  deferred :: writeVariable

            procedure :: getLocation

            procedure :: dataArrayWriterConstructor
            procedure :: dataArrayWriterDestructor
    end type

    abstract interface
        subroutine writeDimensionAbstract(this,pinfo,ddim,locationInFile)

            import DataArrayWriter
            import ParallelInfo
            import DataDimension

            class(DataArrayWriter)                  :: this

            class(ParallelInfo),        pointer     :: pinfo
            class(DataDimension),       pointer     :: ddim
            character(len=*), optional, intent(in)  :: locationInFile
        end subroutine

        subroutine writeVariableAbstract(this,pinfo,var,locationInFile)

            import DataArrayWriter
            import ParallelInfo
            import DataVariable

            class(DataArrayWriter)                          :: this

            class(ParallelInfo),        pointer     :: pinfo
            class(DataVariable),        pointer     :: var
            character(len=*), optional, intent(in)  :: locationInFile
        end subroutine
    end interface

    contains

    subroutine dataArrayWriterConstructor(this,location)
        implicit none

        class(DataArrayWriter)  :: this

        character(len=*),  intent(in) :: location

        this%location = location
    end subroutine

    subroutine dataArrayWriterDestructor(this)
        implicit none

        class(DataArrayWriter)  :: this
    end subroutine

    function getLocation(this) result(location)
        implicit none

        class(DataArrayWriter)  :: this

        character(:), allocatable :: location

        location = this%location
    end function
end module
