module hdfDataArrayWriter_mod
    use iso_fortran_env
    use dataArrayReader_mod
    use dataArray_mod
    use dataAttribute_mod
    use dataType_mod
    use dataShape_mod
    use dataGroup_mod
    use parallelConstants_mod
    use parallelInfo_mod
    use parallelDecomposition_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    public :: HdfDataArrayWriter

    type, extends(DataArrayWriter) :: HdfDataArrayWriter
        logical :: littleEndian

        contains
            procedure :: isLittleEndian

            procedure :: writeDimension => writeDimension_hdf
            procedure :: writeVariable  => writeVariable_hdf

            procedure, private :: getHdfType

            procedure :: hdfDataArrayWriterConstructor
            final     :: hdfDataArrayWriterDestructor
    end type

    contains

    subroutine hdfDataArrayWriterConstructor(this,fileName,littleEndian)
        implicit none

        class(HdfDataArrayWriter)     :: this

        character(len=*),  intent(in) :: fileName
        logical,           intent(in) :: littleEndian

        call this%dataArrayWriterConstructor(fileName)
        this%littleEndian = littleEndian
    end subroutine

    subroutine hdfDataArrayWriterDestructor(this)
        implicit none

        type(HdfDataArrayWriter)  :: this

        call this%dataArrayReaderDestructor()
    end subroutine

    function isLittleEndian(this) result(littleEndian)
        implicit none

        class(HdfDataArrayWriter)                          :: this

        logical :: littleEndian

        littleEndian = this%littleEndian
    end function

    subroutine writeDimension(this,pinfo,ddim,locationInFile)

        use hdf5

        implicit none

        class(HdfDataArrayWriter)               :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataDimension),       pointer     :: ddim
        character(len=*), optional, intent(in)  :: locationInFile

    end subroutine

    subroutine writeVariable(this,pinfo,var,locationInFile)

        use hdf5

        implicit none

        class(HdfDataArrayWriter)        :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataVariable),        pointer     :: var
        character(len=*), optional, intent(in)  :: locationInFile

    end subroutine

    function getHdfType(this,dTypeNum) result(memType)

        use hdf5

        implicit none

        class(HdfDataArrayWriter) :: this

        integer,       intent(in) :: dTypeNum

        integer(hid_t)            :: memType

        select case (dTypeNum)
            case (LOGICAL_TYPE_NUM)
                call error('HDF does not have a logical type at the current time.')
            case (BYTE_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I8LE
                else
                    memType = H5T_STD_I8BE
                end if
            case (SHORT_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I16LE
                else
                    memType = H5T_STD_I16BE
                end if
            case (INT_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I32LE
                else
                    memType = H5T_STD_I32BE
                end if
            case (LONG_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_STD_I64LE
                else
                    memType = H5T_STD_I64BE
                end if
            case (REAL_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_IEEE_F32LE
                else
                    memType = H5T_IEEE_F32BE
                end if
            case (DOUBLE_TYPE_NUM)
                if (this%littleEndian) then
                    memType = H5T_IEEE_F64LE
                else
                    memType = H5T_IEEE_F64BE
                end if
            case default
                memType = -999
        end select
    end function
end module
