module atmos3dDataSetFactory_mod

    use parallelInfo_mod

    use dataSet_mod
    use atmos3dDataSet_mod
    use arwDataSet_mod
    use hwrfDataSet_mod

    use mpiUtils_mod

    implicit none

    private

    ! Model dataSets
    character(len=256), parameter, public :: WRF_ARW_MODEL = 'WRF-ARW'
    character(len=256), parameter, public :: HWRF_MODEL    = 'HWRF'

    public :: getAtmos3dDataSet

    contains

    function getAtmos3dDataSet(pinfo,modelName,time,inputFile) result(dataSet)

        implicit none

        class(ParallelInfo),              pointer    :: pinfo
        character(len=*),                 intent(in) :: modelName
        integer,                          intent(in) :: time
        character(len=*),       optional, intent(in) :: inputFile

        class(Atmos3dDataSet),  pointer    :: dataSet
        class(ArwDataSet),      pointer    :: dataSetArw
        class(HwrfDataSet),     pointer    :: dataSetHwrf

        select case (modelName)
            case (WRF_ARW_MODEL)
                allocate(dataSetArw)
                call dataSetArw%arwDataSetConstructor(pinfo,inputFile=inputFile,time=time)
                dataSet => dataSetArw
            case (HWRF_MODEL)
                allocate(dataSetHwrf)
                call dataSetHwrf%hwrfDataSetConstructor(pinfo,inputFile=inputFile,time=time)
                dataSet => dataSetHwrf
            case default
                call error('Unknown model name' // trim(modelName))
        end select
    end function
end module
