module deconvolutionConverter_mod
    use iso_fortran_env

    use datasetVectorConverter_mod
    use dataType_mod

    use abstractVector_mod
    use simple1DVector_mod

    use dataSet_mod
    use dataVariable_mod

    use satelliteObservation_mod

    use mpiUtils_mod

    implicit none

    private

    type, public, extends(DatasetVectorConverter) :: DeconvolutionConverter
        character(len=:), allocatable :: stateVarName

        contains
            procedure :: deconvolutionConverterConstructor
            final     :: deconvolutionConverterDestructor

            procedure :: convertToState
            procedure :: convertFromState
            procedure :: getStateVectorSize
    end type

    contains

    subroutine deconvolutionConverterConstructor(this,stateVarName)

        class(DeconvolutionConverter) :: this

        character(len=*),  intent(in) :: stateVarName

        allocate(character(len=len_trim(stateVarName)) :: this%stateVarName)

        this%stateVarName = trim(stateVarName)
    end subroutine

    subroutine deconvolutionConverterDestructor(this)

        type(DeconvolutionConverter)  :: this

        if (allocated(this%stateVarName)) then
            deallocate(this%stateVarName)
        end if

    end subroutine

    subroutine convertToState(this, vector, state)

        class(DeconvolutionConverter)  :: this
        class(AbstractVector), pointer :: vector
        class(DataSet),        pointer :: state

        real(real64), dimension(:),   pointer :: data1d
        real(real64), dimension(:,:), pointer :: tbDble
        real(real32), dimension(:,:), pointer :: tbReal

        class(DataVariable), pointer :: tbVar

        integer :: i, j, k, ind

        data1d => vector%get1DArrayPtr()

        tbVar => state%getVariableByName(this%stateVarName)

        ind = 0

        if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
            call tbVar%getArray(tbReal)

            do j=1,size(tbReal,2)
                do i=1,size(tbReal,1)
                    ind = ind + 1
                    tbReal(i,j) = data1d(ind)
                end do
            end do
        else
            call tbVar%getArray(tbDble)

            do j=1,size(tbDble,2)
                do i=1,size(tbDble,1)
                    ind = ind + 1
                    tbDble(i,j) = data1d(ind)
                end do
            end do
        end if
    end subroutine

    subroutine convertFromState(this, state, vector)

        class(DeconvolutionConverter)  :: this
        class(DataSet),        pointer :: state
        class(AbstractVector), pointer :: vector

        real(real64), dimension(:),     pointer :: data1d
        real(real64), dimension(:,:), pointer :: tbDble
        real(real32), dimension(:,:), pointer :: tbReal

        class(DataVariable), pointer :: tbVar

        integer :: i, j, k, ind

        data1d => vector%get1DArrayPtr()
        tbVar => state%getVariableByName(this%stateVarName)

        ind = 0

        if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
            call tbVar%getArray(tbReal)

            do j=1,size(tbReal,2)
                do i=1,size(tbReal,1)
                    ind = ind + 1
                    data1d(ind) = tbReal(i,j)
                end do
            end do
        else
            call tbVar%getArray(tbDble)

            do j=1,size(tbDble,2)
                do i=1,size(tbDble,1)
                    ind = ind + 1
                    data1d(ind) = tbDble(i,j)
                end do
            end do
        end if
    end subroutine

    function getStateVectorSize(this,state) result(svsize)

        class(DeconvolutionConverter)  :: this
        class(DataSet),        pointer :: state

        integer                        :: svsize

        real(real64), dimension(:,:,:), pointer :: tb

        class(DataVariable), pointer :: tbVar

        tbVar => state%getVariableByName(this%stateVarName)

        svsize = tbVar%getLocalTotalSize()
    end function
end module
