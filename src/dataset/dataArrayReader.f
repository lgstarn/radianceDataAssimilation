module dataArrayReader_mod

    use parallelInfo_mod

    use dataArray_mod
    use dataAttribute_mod

    implicit none

    private

    public :: DataArrayReader

    ! An abstract class that represents some underlying model of data. All data is assumed to
    ! have a dimensionality (e.g. 1D, 2D, 3D, etc.) as well as named variables.
    type, abstract :: DataArrayReader
        character(:), allocatable :: location

        contains
            procedure(loadDimSizeFromVariableAbstract),  deferred :: loadDimSizeFromVariable
            procedure(loadAttributeAbstract),            deferred :: loadAttribute
            procedure(cloneAbstract),                    deferred :: clone
            procedure(loadDataArrayAbstract),            deferred :: loadDataArray

            procedure :: getLocation

            procedure :: dataArrayReaderConstructor
            procedure :: dataArrayReaderDestructor
    end type

    abstract interface
        function loadDimSizeFromVariableAbstract(this,pinfo,locationInFile,dimNum) &
            result(dimn)

            import DataArrayReader
            import ParallelInfo

            class(DataArrayReader)           :: this

            class(ParallelInfo), pointer     :: pinfo
            character(len=*),    intent(in)  :: locationInFile
            integer,             intent(in)  :: dimNum

            integer :: dimn
        end function

        subroutine loadAttributeAbstract(this,pinfo,attr,aname,locationInFile,required)

            import DataArrayReader
            import ParallelInfo
            import DataAttribute

            class(DataArrayReader)                 :: this

            class(ParallelInfo),        pointer    :: pinfo
            class(DataAttribute),       pointer    :: attr
            character(len=*),           intent(in) :: aname
            character(len=*), optional, intent(in) :: locationInFile
            logical,          optional, intent(in) :: required
        end subroutine

        subroutine loadDataArrayAbstract(this,pinfo,dArray,locationInFile,required)

            import DataArrayReader
            import ParallelInfo
            import DataArray

            class(DataArrayReader)          :: this

            class(ParallelInfo), pointer    :: pinfo
            class(DataArray),    pointer    :: dArray
            character(len=*),    intent(in) :: locationInFile
            logical,   optional, intent(in) :: required
        end subroutine

        function cloneAbstract(this) result(newReader)

            import DataArrayReader

            class(DataArrayReader)          :: this

            class(DataArrayReader), pointer :: newReader
        end function
    end interface

    contains

    subroutine dataArrayReaderConstructor(this,location)
        implicit none

        class(DataArrayReader)  :: this

        character(len=*),  intent(in) :: location

        ! set the name of the top group to an empty string
        this%location = location
    end subroutine

    subroutine dataArrayReaderDestructor(this)
        implicit none

        class(DataArrayReader)  :: this
    end subroutine

    function getLocation(this) result(location)
        implicit none

        class(DataArrayReader)  :: this

        character(:), allocatable :: location

        location = this%location
    end function
end module
