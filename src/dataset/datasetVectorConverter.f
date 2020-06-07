module datasetVectorConverter_mod
    use abstractVector_mod
    use dataset_mod

    implicit none

    private

    type, abstract, public :: DatasetVectorConverter
        contains
            procedure(convertToState_abs),          deferred :: convertToState
            procedure(convertFromState_abs),        deferred :: convertFromState
            procedure(getLocalStateVectorSize_abs), deferred :: getLocalStateVectorSize
    end type

    abstract interface
        subroutine convertToState_abs(this, vector, state)
            import DatasetVectorConverter
            import AbstractVector
            import Dataset

            class(DatasetVectorConverter)    :: this
            class(AbstractVector),   pointer :: vector
            class(Dataset),          pointer :: state
        end subroutine

        subroutine convertFromState_abs(this, state, vector)
            import DatasetVectorConverter
            import Dataset
            import AbstractVector

            class(DatasetVectorConverter)    :: this
            class(Dataset),          pointer :: state
            class(AbstractVector),   pointer :: vector
        end subroutine

        function getLocalStateVectorSize_abs(this, state) result(svsize)
            import DatasetVectorConverter
            import Dataset

            class(DatasetVectorConverter)    :: this
            class(Dataset),          pointer :: state
            integer                          :: svsize
        end function
    end interface
end module
