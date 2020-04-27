module deconvolutionConverter_mod
    use iso_fortran_env

    use datasetVectorConverter_mod

    use abstractVector_mod
    use simple1DVector_mod

    use dataSet_mod
    use dataVariable_mod

    use satelliteObservation_mod

    use mpiUtils_mod

    implicit none

    private

    type, public, extends(DatasetVectorConverter) :: DeconvolutionConverter
        contains
            procedure :: convertToState
            procedure :: convertFromState
            procedure :: getStateVectorSize
    end type

    contains

    subroutine convertToState(this, vector, state)
        implicit none

        class(DeconvolutionConverter)  :: this
        class(AbstractVector), pointer :: vector
        class(DataSet),        pointer :: state

        real(real64), dimension(:),     pointer :: data1d
        real(real64), dimension(:,:,:), pointer :: tb

        class(DataVariable), pointer :: tbVar

        integer :: i, j, k, ind

        data1d => vector%get1DArrayPtr()

        tbVar => state%getVariableByName(TB_VAR_NAME)

        call tbVar%getArray(tb)

        ind = 0

        do k=1,size(tb,3)
            do j=1,size(tb,2)
                do i=1,size(tb,1)
                    ind = ind + 1
                    tb(i,j,k) = data1d(ind)
                end do
            end do
        end do
    end subroutine

    subroutine convertFromState(this, state, vector)
        implicit none

        class(DeconvolutionConverter)  :: this
        class(DataSet),        pointer :: state
        class(AbstractVector), pointer :: vector

        real(real64), dimension(:),     pointer :: data1d
        real(real64), dimension(:,:,:), pointer :: tb

        class(DataVariable), pointer :: tbVar

        integer :: i, j, k, ind

        data1d => vector%get1DArrayPtr()
        tbVar => state%getVariableByName(TB_VAR_NAME)

        call tbVar%getArray(tb)

        ind = 0

        do k=1,size(tb,3)
            do j=1,size(tb,2)
                do i=1,size(tb,1)
                    ind = ind + 1
                    data1d(ind) = tb(i,j,k)
                end do
            end do
        end do
    end subroutine

    function getStateVectorSize(this,state) result(svsize)
        implicit none

        class(DeconvolutionConverter)  :: this
        class(DataSet),     pointer :: state

        integer                        :: svsize

        real(real64), dimension(:,:,:), pointer :: tb

        class(DataVariable), pointer :: tbVar

        tbVar => state%getVariableByName(TB_VAR_NAME)

        call tbVar%getArray(tb)

        svsize = size(tb,1)*size(tb,2)*size(tb,3)
    end function
end module
