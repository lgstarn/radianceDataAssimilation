module deconvolutionConverter_mod
    use iso_fortran_env

    use datasetVectorConverter_mod
    use dataType_mod

    use abstractVector_mod
    use simple1DVector_mod

    use dataSet_mod
    use dataArray_mod
    use dataVariable_mod

    use satelliteObservation_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    type, public, extends(DatasetVectorConverter) :: DeconvolutionConverter
        logical :: hasMask

        character(len=:), allocatable :: stateVarName
        character(len=:), allocatable :: maskVarName

        contains
            procedure :: deconvolutionConverterConstructor
            final     :: deconvolutionConverterDestructor

            procedure :: convertToState
            procedure :: convertFromState
            procedure :: getLocalStateVectorSize
    end type

    contains

    subroutine deconvolutionConverterConstructor(this,stateVarName,maskVarName)

        class(DeconvolutionConverter) :: this

        character(len=*),           intent(in) :: stateVarName
        character(len=*), optional, intent(in) :: maskVarName

        allocate(character(len=len_trim(stateVarName)) :: this%stateVarName)
        this%stateVarName = trim(stateVarName)

        if (present(maskVarName)) then
            this%hasMask = .true.

            allocate(character(len=len_trim(maskVarName)) :: this%maskVarName)
            this%maskVarName = trim(maskVarName)
        end if
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
        class(DataVariable), pointer :: maskVar

        integer, pointer :: mask(:,:)

        integer :: i, j, k, ind

        data1d => vector%get1DArrayPtr()

        tbVar => state%getVariableByName(this%stateVarName)

        ind = 0

        if (this%hasMask) then
            maskVar => state%getVariableByName(this%maskVarName)
            call maskVar%getArray(mask)
        end if

        if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
            call tbVar%getArray(tbReal)

            if (this%hasMask) then
                do j=1,size(mask,2)
                    if (all(mask(:,j) == 0)) then
                        do i=1,size(tbReal,1)
                            ind = ind + 1
                            tbReal(i,j) = data1d(ind)
                        end do
                    end if
                end do
            else
                do j=1,size(tbReal,2)
                    do i=1,size(tbReal,1)
                        ind = ind + 1
                        tbReal(i,j) = data1d(ind)
                    end do
                end do
            end if
        else
            call tbVar%getArray(tbDble)

            if (this%hasMask) then
                do j=1,size(mask,2)
                    if (all(mask(:,j) == 0)) then
                        do i=1,size(tbDble,1)
                            ind = ind + 1
                            tbDble(i,j) = data1d(ind)
                        end do
                    end if
                end do
            else
                do j=1,size(tbDble,2)
                    do i=1,size(tbDble,1)
                        ind = ind + 1
                        tbDble(i,j) = data1d(ind)
                    end do
                end do
            end if
        end if
    end subroutine

    subroutine convertFromState(this, state, vector)

        class(DeconvolutionConverter)  :: this
        class(DataSet),        pointer :: state
        class(AbstractVector), pointer :: vector

        real(real64), dimension(:),   pointer :: data1d
        real(real64), dimension(:,:), pointer :: tbDble
        real(real32), dimension(:,:), pointer :: tbReal

        class(DataVariable), pointer :: tbVar
        class(DataVariable), pointer :: maskVar

        integer, pointer :: mask(:,:)

        integer :: i, j, k, ind

        data1d  => vector%get1DArrayPtr()

        tbVar   => state%getVariableByName(this%stateVarName)

        if (this%hasMask) then
            maskVar => state%getVariableByName(this%maskVarName)
            call maskVar%getArray(mask)
        end if

        ind = 0

        if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
            call tbVar%getArray(tbReal)

            if (this%hasMask) then
                do j=1,size(mask,2)
                    if (all(mask(:,j) == 0)) then
                        do i=1,size(tbReal,1)
                            ind = ind + 1
                            data1d(ind) = tbReal(i,j)
                        end do
                    end if
                end do
            else
                do j=1,size(tbReal,2)
                    do i=1,size(tbReal,1)
                        ind = ind + 1
                        data1d(ind) = tbReal(i,j)
                    end do
                end do
            end if
        else
            call tbVar%getArray(tbDble)

            if (this%hasMask) then
                do j=1,size(tbDble,2)
                    if (all(mask(:,j) == 0)) then
                        do i=1,size(tbDble,1)
                            ind = ind + 1
                            data1d(ind) = tbDble(i,j)
                        end do
                    end if
                end do
            else
                do j=1,size(tbDble,2)
                    do i=1,size(tbDble,1)
                        ind = ind + 1
                        data1d(ind) = tbDble(i,j)
                    end do
                end do
            end if
        end if
    end subroutine

    function getLocalStateVectorSize(this,state) result(svsize)

        class(DeconvolutionConverter)  :: this
        class(DataSet),        pointer :: state

        integer                        :: svsize

        integer :: i

        integer,      pointer :: mask(:,:)

        class(DataVariable), pointer :: tbVar
        class(DataVariable), pointer :: maskVar
        class(DataArray),    pointer :: maskArray

        real(real32), dimension(:,:), pointer :: tbReal
        real(real64), dimension(:,:), pointer :: tbDble

        tbVar => state%getVariableByName(this%stateVarName)

        if (this%hasMask) then
            maskVar => state%getVariableByName(this%maskVarName)
            call maskVar%getArray(mask)

            svsize = 0

            if (tbVar%getDataTypeNum() == REAL_TYPE_NUM) then
                call tbVar%getArray(tbReal)

                if (size(mask,2) /= size(tbReal,2)) then
                    call error('The size of the mask did not match the size of real tb: ' // &
                        int2str(size(mask,2)) // ' vs. ' // int2str(size(tbReal,2)))
                end if

                do i=1,size(mask,2)
                    if (all(mask(:,i) == 0)) then
                        svsize = svsize + size(tbReal,1)
                    end if
                end do
            else
                call tbVar%getArray(tbDble)

                if (size(mask,2) /= size(tbDble,2)) then
                    call error('The size of the mask did not match the size of double tb: ' // &
                        int2str(size(mask,2)) // ' vs. ' // int2str(size(tbDble,2)))
                end if

                do i=1,size(mask,2)
                    if (all(mask(:,i) == 0)) then
                        svsize = svsize + size(tbDble,1)
                    end if
                end do
            end if
        else
            svsize = tbVar%getLocalTotalSize()
        end if
    end function
end module
