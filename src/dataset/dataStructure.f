module dataStructure_mod

    use dataDimension_mod
    use linkedList_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DataStructure

    type   :: DataStructure
        private

        class(LinkedList), pointer :: dims  => null()

        contains
            procedure :: getNDimensions
            procedure :: getDimensionLength
            procedure :: getTotalSize
            procedure :: isUnlimited

            procedure :: getDimension

            procedure :: dataStructureConstructor

            final     :: dataStructureDestructor ! clean up all allocated variables
    end type

    contains

    subroutine dataStructureConstructor(this)
        implicit none

        class(DataStructure) :: this

        allocate(this%dims)
    end subroutine

    function getDimension(this,dimNum) result(ddim)
        implicit none

        class(DataStructure)    :: this
        integer, intent(in) :: dimNum

        class(DataDimension), pointer :: ddim

        class(*), pointer :: optr

        call this%dims%first()

        if (dimNum > this%getNDimensions()) then
            write(msgstr,*) 'Error: in dataStructure, requested dimension larger than number of dimensions:',&
                &dimNum,this%getNDimensions()
            call error(msgstr)
        end if

        optr => this%dims%get(dimNum)

        select type(optr)
            class is (DataDimension)
                ! cast down to DataGroup
                ddim => optr
            class default
                call error('Unknown class in data shape dims list')
        end select
    end function

    function getDimensionLength(this,dimNum) result(ndim)
        implicit none

        class(DataStructure)    :: this
        integer, intent(in) :: dimNum

        integer :: ndim

        class(DataDimension), pointer :: ddim

        ddim => this%getDimension(dimNum)

        ndim = ddim%getLength()
    end function

    function getNDimensions(this) result(ndim)

        implicit none

        class(DataStructure) :: this

        integer          :: ndim

        ndim = this%dims%getListSize()
    end function

    function getTotalSize(this) result(ntot)

        implicit none

        class(DataStructure) :: this

        integer          :: i, ntot

        if (this%getNDimension() == 0) then
            ntot = 0
        else
            ntot = 1

            do i=1,this%getNDimensions()
                ntot = ntot*this%getDimensionLength(i)
            end do
        end if
    end function

    function isUnlimited(this,dimNum) result(unlim)
        implicit none

        class(DataStructure)    :: this
        integer, intent(in) :: dimNum

        logical             :: unlim

        class(DataDimension), pointer :: ddim

        ddim => this%getDimension(dimNum)

        unlim = ddim%isUnlimited()
    end function
end module
