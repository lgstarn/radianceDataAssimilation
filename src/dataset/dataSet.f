module dataSet_mod

    use parallelInfo_mod
    use parallelConstants_mod

    use linkedList_mod
    use dictionary_mod

    use dataType_mod
    use dataGroup_mod
    use dataShape_mod
    use dataArray_mod
    use dataVariable_mod
    use dataAttribute_mod
    use dataDimension_mod
    use dataArrayReader_mod
    use dataArrayWriter_mod

    use mirroredArray_mod

    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    private

    public :: DataSet

    ! A type that represents a readable/writeable collection of data. It "is a"
    ! DataGroup as well, meaning it inherits all of that functionality, but
    ! adds the ability to "load" variables (using the DataArrayReader reader)
    ! and also to write the entire data set to file (using the DataArrayWriter).
    ! This is a loose implementation/extention of the UCAR Unidata Common Data Model.
    type, extends(DataGroup) :: DataSet

        class(DataArrayReader), pointer :: reader   => NULL()

        contains
            procedure :: clone

            procedure :: getDataArrayReader
            procedure :: setDataArrayReader

            generic   :: loadAttribute =>              &
                             loadAttribute_varLenStrs, &
                             loadAttribute_logical,    &
                             loadAttribute_byte,       &
                             loadAttribute_short,      &
                             loadAttribute_int,        &
                             loadAttribute_long,       &
                             loadAttribute_real,       &
                             loadAttribute_dble

            procedure, private :: loadAttribute_varLenStrs
            procedure, private :: loadAttribute_logical
            procedure, private :: loadAttribute_byte
            procedure, private :: loadAttribute_short
            procedure, private :: loadAttribute_int
            procedure, private :: loadAttribute_long
            procedure, private :: loadAttribute_real
            procedure, private :: loadAttribute_dble

            procedure :: loadDimensionFromVariable

            generic   :: loadVariable =>                 &
                            loadVariable_dim1,           &
                            loadVariable_dim2,           &
                            loadVariable_dim3,           &
                            loadVariable_dim4,           &
                            loadVariable_dim5,           &
                            loadVariable_dim6,           &
                            loadVariable_dim7,           &
                            loadVariable_logical1d_dims, &
                            loadVariable_byte1d_dims,    &
                            loadVariable_short1d_dims,   &
                            loadVariable_int1d_dims,     &
                            loadVariable_long1d_dims,    &
                            loadVariable_real1d_dims,    &
                            loadVariable_dble1d_dims,    &
                            loadVariable_logical2d_dims, &
                            loadVariable_byte2d_dims,    &
                            loadVariable_short2d_dims,   &
                            loadVariable_int2d_dims,     &
                            loadVariable_long2d_dims,    &
                            loadVariable_real2d_dims,    &
                            loadVariable_dble2d_dims,    &
                            loadVariable_logical3d_dims, &
                            loadVariable_byte3d_dims,    &
                            loadVariable_short3d_dims,   &
                            loadVariable_int3d_dims,     &
                            loadVariable_long3d_dims,    &
                            loadVariable_real3d_dims,    &
                            loadVariable_dble3d_dims,    &
                            loadVariable_logical4d_dims, &
                            loadVariable_byte4d_dims,    &
                            loadVariable_short4d_dims,   &
                            loadVariable_int4d_dims,     &
                            loadVariable_long4d_dims,    &
                            loadVariable_real4d_dims,    &
                            loadVariable_dble4d_dims,    &
                            loadVariable_logical5d_dims, &
                            loadVariable_byte5d_dims,    &
                            loadVariable_short5d_dims,   &
                            loadVariable_int5d_dims,     &
                            loadVariable_long5d_dims,    &
                            loadVariable_real5d_dims,    &
                            loadVariable_dble5d_dims,    &
                            loadVariable_logical6d_dims, &
                            loadVariable_byte6d_dims,    &
                            loadVariable_short6d_dims,   &
                            loadVariable_int6d_dims,     &
                            loadVariable_long6d_dims,    &
                            loadVariable_real6d_dims,    &
                            loadVariable_dble6d_dims,    &
                            loadVariable_logical7d_dims, &
                            loadVariable_byte7d_dims,    &
                            loadVariable_short7d_dims,   &
                            loadVariable_int7d_dims,     &
                            loadVariable_long7d_dims,    &
                            loadVariable_real7d_dims,    &
                            loadVariable_dble7d_dims,    &
                            loadVariable_logical1d_dshp, &
                            loadVariable_byte1d_dshp,    &
                            loadVariable_short1d_dshp,   &
                            loadVariable_int1d_dshp,     &
                            loadVariable_long1d_dshp,    &
                            loadVariable_real1d_dshp,    &
                            loadVariable_dble1d_dshp,    &
                            loadVariable_logical2d_dshp, &
                            loadVariable_byte2d_dshp,    &
                            loadVariable_short2d_dshp,   &
                            loadVariable_int2d_dshp,     &
                            loadVariable_long2d_dshp,    &
                            loadVariable_real2d_dshp,    &
                            loadVariable_dble2d_dshp,    &
                            loadVariable_logical3d_dshp, &
                            loadVariable_byte3d_dshp,    &
                            loadVariable_short3d_dshp,   &
                            loadVariable_int3d_dshp,     &
                            loadVariable_long3d_dshp,    &
                            loadVariable_real3d_dshp,    &
                            loadVariable_dble3d_dshp,    &
                            loadVariable_logical4d_dshp, &
                            loadVariable_byte4d_dshp,    &
                            loadVariable_short4d_dshp,   &
                            loadVariable_int4d_dshp,     &
                            loadVariable_long4d_dshp,    &
                            loadVariable_real4d_dshp,    &
                            loadVariable_dble4d_dshp,    &
                            loadVariable_logical5d_dshp, &
                            loadVariable_byte5d_dshp,    &
                            loadVariable_short5d_dshp,   &
                            loadVariable_int5d_dshp,     &
                            loadVariable_long5d_dshp,    &
                            loadVariable_real5d_dshp,    &
                            loadVariable_dble5d_dshp,    &
                            loadVariable_logical6d_dshp, &
                            loadVariable_byte6d_dshp,    &
                            loadVariable_short6d_dshp,   &
                            loadVariable_int6d_dshp,     &
                            loadVariable_long6d_dshp,    &
                            loadVariable_real6d_dshp,    &
                            loadVariable_dble6d_dshp,    &
                            loadVariable_logical7d_dshp, &
                            loadVariable_byte7d_dshp,    &
                            loadVariable_short7d_dshp,   &
                            loadVariable_int7d_dshp,     &
                            loadVariable_long7d_dshp,    &
                            loadVariable_real7d_dshp,    &
                            loadVariable_dble7d_dshp

            procedure, private :: loadVariable_dim1
            procedure, private :: loadVariable_dim2
            procedure, private :: loadVariable_dim3
            procedure, private :: loadVariable_dim4
            procedure, private :: loadVariable_dim5
            procedure, private :: loadVariable_dim6
            procedure, private :: loadVariable_dim7

            procedure, private :: loadVariable_logical1d_dims
            procedure, private :: loadVariable_byte1d_dims
            procedure, private :: loadVariable_short1d_dims
            procedure, private :: loadVariable_int1d_dims
            procedure, private :: loadVariable_long1d_dims
            procedure, private :: loadVariable_real1d_dims
            procedure, private :: loadVariable_dble1d_dims

            procedure, private :: loadVariable_logical2d_dims
            procedure, private :: loadVariable_byte2d_dims
            procedure, private :: loadVariable_short2d_dims
            procedure, private :: loadVariable_int2d_dims
            procedure, private :: loadVariable_long2d_dims
            procedure, private :: loadVariable_real2d_dims
            procedure, private :: loadVariable_dble2d_dims

            procedure, private :: loadVariable_logical3d_dims
            procedure, private :: loadVariable_byte3d_dims
            procedure, private :: loadVariable_short3d_dims
            procedure, private :: loadVariable_int3d_dims
            procedure, private :: loadVariable_long3d_dims
            procedure, private :: loadVariable_real3d_dims
            procedure, private :: loadVariable_dble3d_dims

            procedure, private :: loadVariable_logical4d_dims
            procedure, private :: loadVariable_byte4d_dims
            procedure, private :: loadVariable_short4d_dims
            procedure, private :: loadVariable_int4d_dims
            procedure, private :: loadVariable_long4d_dims
            procedure, private :: loadVariable_real4d_dims
            procedure, private :: loadVariable_dble4d_dims

            procedure, private :: loadVariable_logical5d_dims
            procedure, private :: loadVariable_byte5d_dims
            procedure, private :: loadVariable_short5d_dims
            procedure, private :: loadVariable_int5d_dims
            procedure, private :: loadVariable_long5d_dims
            procedure, private :: loadVariable_real5d_dims
            procedure, private :: loadVariable_dble5d_dims

            procedure, private :: loadVariable_logical6d_dims
            procedure, private :: loadVariable_byte6d_dims
            procedure, private :: loadVariable_short6d_dims
            procedure, private :: loadVariable_int6d_dims
            procedure, private :: loadVariable_long6d_dims
            procedure, private :: loadVariable_real6d_dims
            procedure, private :: loadVariable_dble6d_dims

            procedure, private :: loadVariable_logical7d_dims
            procedure, private :: loadVariable_byte7d_dims
            procedure, private :: loadVariable_short7d_dims
            procedure, private :: loadVariable_int7d_dims
            procedure, private :: loadVariable_long7d_dims
            procedure, private :: loadVariable_real7d_dims
            procedure, private :: loadVariable_dble7d_dims

            procedure, private :: loadVariable_logical1d_dshp
            procedure, private :: loadVariable_byte1d_dshp
            procedure, private :: loadVariable_short1d_dshp
            procedure, private :: loadVariable_int1d_dshp
            procedure, private :: loadVariable_long1d_dshp
            procedure, private :: loadVariable_real1d_dshp
            procedure, private :: loadVariable_dble1d_dshp

            procedure, private :: loadVariable_logical2d_dshp
            procedure, private :: loadVariable_byte2d_dshp
            procedure, private :: loadVariable_short2d_dshp
            procedure, private :: loadVariable_int2d_dshp
            procedure, private :: loadVariable_long2d_dshp
            procedure, private :: loadVariable_real2d_dshp
            procedure, private :: loadVariable_dble2d_dshp

            procedure, private :: loadVariable_logical3d_dshp
            procedure, private :: loadVariable_byte3d_dshp
            procedure, private :: loadVariable_short3d_dshp
            procedure, private :: loadVariable_int3d_dshp
            procedure, private :: loadVariable_long3d_dshp
            procedure, private :: loadVariable_real3d_dshp
            procedure, private :: loadVariable_dble3d_dshp

            procedure, private :: loadVariable_logical4d_dshp
            procedure, private :: loadVariable_byte4d_dshp
            procedure, private :: loadVariable_short4d_dshp
            procedure, private :: loadVariable_int4d_dshp
            procedure, private :: loadVariable_long4d_dshp
            procedure, private :: loadVariable_real4d_dshp
            procedure, private :: loadVariable_dble4d_dshp

            procedure, private :: loadVariable_logical5d_dshp
            procedure, private :: loadVariable_byte5d_dshp
            procedure, private :: loadVariable_short5d_dshp
            procedure, private :: loadVariable_int5d_dshp
            procedure, private :: loadVariable_long5d_dshp
            procedure, private :: loadVariable_real5d_dshp
            procedure, private :: loadVariable_dble5d_dshp

            procedure, private :: loadVariable_logical6d_dshp
            procedure, private :: loadVariable_byte6d_dshp
            procedure, private :: loadVariable_short6d_dshp
            procedure, private :: loadVariable_int6d_dshp
            procedure, private :: loadVariable_long6d_dshp
            procedure, private :: loadVariable_real6d_dshp
            procedure, private :: loadVariable_dble6d_dshp

            procedure, private :: loadVariable_logical7d_dshp
            procedure, private :: loadVariable_byte7d_dshp
            procedure, private :: loadVariable_short7d_dshp
            procedure, private :: loadVariable_int7d_dshp
            procedure, private :: loadVariable_long7d_dshp
            procedure, private :: loadVariable_real7d_dshp
            procedure, private :: loadVariable_dble7d_dshp

            generic   :: loadLocalVariable =>                 &
                            loadLocalVariable_dim1,           &
                            loadLocalVariable_dim2,           &
                            loadLocalVariable_dim3,           &
                            loadLocalVariable_dim4,           &
                            loadLocalVariable_dim5,           &
                            loadLocalVariable_dim6,           &
                            loadLocalVariable_dim7,           &
                            loadLocalVariable_logical1d_dims, &
                            loadLocalVariable_byte1d_dims,    &
                            loadLocalVariable_short1d_dims,   &
                            loadLocalVariable_int1d_dims,     &
                            loadLocalVariable_long1d_dims,    &
                            loadLocalVariable_real1d_dims,    &
                            loadLocalVariable_dble1d_dims,    &
                            loadLocalVariable_logical2d_dims, &
                            loadLocalVariable_byte2d_dims,    &
                            loadLocalVariable_short2d_dims,   &
                            loadLocalVariable_int2d_dims,     &
                            loadLocalVariable_long2d_dims,    &
                            loadLocalVariable_real2d_dims,    &
                            loadLocalVariable_dble2d_dims,    &
                            loadLocalVariable_logical3d_dims, &
                            loadLocalVariable_byte3d_dims,    &
                            loadLocalVariable_short3d_dims,   &
                            loadLocalVariable_int3d_dims,     &
                            loadLocalVariable_long3d_dims,    &
                            loadLocalVariable_real3d_dims,    &
                            loadLocalVariable_dble3d_dims,    &
                            loadLocalVariable_logical4d_dims, &
                            loadLocalVariable_byte4d_dims,    &
                            loadLocalVariable_short4d_dims,   &
                            loadLocalVariable_int4d_dims,     &
                            loadLocalVariable_long4d_dims,    &
                            loadLocalVariable_real4d_dims,    &
                            loadLocalVariable_dble4d_dims,    &
                            loadLocalVariable_logical5d_dims, &
                            loadLocalVariable_byte5d_dims,    &
                            loadLocalVariable_short5d_dims,   &
                            loadLocalVariable_int5d_dims,     &
                            loadLocalVariable_long5d_dims,    &
                            loadLocalVariable_real5d_dims,    &
                            loadLocalVariable_dble5d_dims,    &
                            loadLocalVariable_logical6d_dims, &
                            loadLocalVariable_byte6d_dims,    &
                            loadLocalVariable_short6d_dims,   &
                            loadLocalVariable_int6d_dims,     &
                            loadLocalVariable_long6d_dims,    &
                            loadLocalVariable_real6d_dims,    &
                            loadLocalVariable_dble6d_dims,    &
                            loadLocalVariable_logical7d_dims, &
                            loadLocalVariable_byte7d_dims,    &
                            loadLocalVariable_short7d_dims,   &
                            loadLocalVariable_int7d_dims,     &
                            loadLocalVariable_long7d_dims,    &
                            loadLocalVariable_real7d_dims,    &
                            loadLocalVariable_dble7d_dims,    &
                            loadLocalVariable_logical1d_dshp, &
                            loadLocalVariable_byte1d_dshp,    &
                            loadLocalVariable_short1d_dshp,   &
                            loadLocalVariable_int1d_dshp,     &
                            loadLocalVariable_long1d_dshp,    &
                            loadLocalVariable_real1d_dshp,    &
                            loadLocalVariable_dble1d_dshp,    &
                            loadLocalVariable_logical2d_dshp, &
                            loadLocalVariable_byte2d_dshp,    &
                            loadLocalVariable_short2d_dshp,   &
                            loadLocalVariable_int2d_dshp,     &
                            loadLocalVariable_long2d_dshp,    &
                            loadLocalVariable_real2d_dshp,    &
                            loadLocalVariable_dble2d_dshp,    &
                            loadLocalVariable_logical3d_dshp, &
                            loadLocalVariable_byte3d_dshp,    &
                            loadLocalVariable_short3d_dshp,   &
                            loadLocalVariable_int3d_dshp,     &
                            loadLocalVariable_long3d_dshp,    &
                            loadLocalVariable_real3d_dshp,    &
                            loadLocalVariable_dble3d_dshp,    &
                            loadLocalVariable_logical4d_dshp, &
                            loadLocalVariable_byte4d_dshp,    &
                            loadLocalVariable_short4d_dshp,   &
                            loadLocalVariable_int4d_dshp,     &
                            loadLocalVariable_long4d_dshp,    &
                            loadLocalVariable_real4d_dshp,    &
                            loadLocalVariable_dble4d_dshp,    &
                            loadLocalVariable_logical5d_dshp, &
                            loadLocalVariable_byte5d_dshp,    &
                            loadLocalVariable_short5d_dshp,   &
                            loadLocalVariable_int5d_dshp,     &
                            loadLocalVariable_long5d_dshp,    &
                            loadLocalVariable_real5d_dshp,    &
                            loadLocalVariable_dble5d_dshp,    &
                            loadLocalVariable_logical6d_dshp, &
                            loadLocalVariable_byte6d_dshp,    &
                            loadLocalVariable_short6d_dshp,   &
                            loadLocalVariable_int6d_dshp,     &
                            loadLocalVariable_long6d_dshp,    &
                            loadLocalVariable_real6d_dshp,    &
                            loadLocalVariable_dble6d_dshp,    &
                            loadLocalVariable_logical7d_dshp, &
                            loadLocalVariable_byte7d_dshp,    &
                            loadLocalVariable_short7d_dshp,   &
                            loadLocalVariable_int7d_dshp,     &
                            loadLocalVariable_long7d_dshp,    &
                            loadLocalVariable_real7d_dshp,    &
                            loadLocalVariable_dble7d_dshp

            procedure, private :: loadLocalVariable_dim1
            procedure, private :: loadLocalVariable_dim2
            procedure, private :: loadLocalVariable_dim3
            procedure, private :: loadLocalVariable_dim4
            procedure, private :: loadLocalVariable_dim5
            procedure, private :: loadLocalVariable_dim6
            procedure, private :: loadLocalVariable_dim7

            procedure, private :: loadLocalVariable_logical1d_dims
            procedure, private :: loadLocalVariable_byte1d_dims
            procedure, private :: loadLocalVariable_short1d_dims
            procedure, private :: loadLocalVariable_int1d_dims
            procedure, private :: loadLocalVariable_long1d_dims
            procedure, private :: loadLocalVariable_real1d_dims
            procedure, private :: loadLocalVariable_dble1d_dims

            procedure, private :: loadLocalVariable_logical2d_dims
            procedure, private :: loadLocalVariable_byte2d_dims
            procedure, private :: loadLocalVariable_short2d_dims
            procedure, private :: loadLocalVariable_int2d_dims
            procedure, private :: loadLocalVariable_long2d_dims
            procedure, private :: loadLocalVariable_real2d_dims
            procedure, private :: loadLocalVariable_dble2d_dims

            procedure, private :: loadLocalVariable_logical3d_dims
            procedure, private :: loadLocalVariable_byte3d_dims
            procedure, private :: loadLocalVariable_short3d_dims
            procedure, private :: loadLocalVariable_int3d_dims
            procedure, private :: loadLocalVariable_long3d_dims
            procedure, private :: loadLocalVariable_real3d_dims
            procedure, private :: loadLocalVariable_dble3d_dims

            procedure, private :: loadLocalVariable_logical4d_dims
            procedure, private :: loadLocalVariable_byte4d_dims
            procedure, private :: loadLocalVariable_short4d_dims
            procedure, private :: loadLocalVariable_int4d_dims
            procedure, private :: loadLocalVariable_long4d_dims
            procedure, private :: loadLocalVariable_real4d_dims
            procedure, private :: loadLocalVariable_dble4d_dims

            procedure, private :: loadLocalVariable_logical5d_dims
            procedure, private :: loadLocalVariable_byte5d_dims
            procedure, private :: loadLocalVariable_short5d_dims
            procedure, private :: loadLocalVariable_int5d_dims
            procedure, private :: loadLocalVariable_long5d_dims
            procedure, private :: loadLocalVariable_real5d_dims
            procedure, private :: loadLocalVariable_dble5d_dims

            procedure, private :: loadLocalVariable_logical6d_dims
            procedure, private :: loadLocalVariable_byte6d_dims
            procedure, private :: loadLocalVariable_short6d_dims
            procedure, private :: loadLocalVariable_int6d_dims
            procedure, private :: loadLocalVariable_long6d_dims
            procedure, private :: loadLocalVariable_real6d_dims
            procedure, private :: loadLocalVariable_dble6d_dims

            procedure, private :: loadLocalVariable_logical7d_dims
            procedure, private :: loadLocalVariable_byte7d_dims
            procedure, private :: loadLocalVariable_short7d_dims
            procedure, private :: loadLocalVariable_int7d_dims
            procedure, private :: loadLocalVariable_long7d_dims
            procedure, private :: loadLocalVariable_real7d_dims
            procedure, private :: loadLocalVariable_dble7d_dims

            procedure, private :: loadLocalVariable_logical1d_dshp
            procedure, private :: loadLocalVariable_byte1d_dshp
            procedure, private :: loadLocalVariable_short1d_dshp
            procedure, private :: loadLocalVariable_int1d_dshp
            procedure, private :: loadLocalVariable_long1d_dshp
            procedure, private :: loadLocalVariable_real1d_dshp
            procedure, private :: loadLocalVariable_dble1d_dshp

            procedure, private :: loadLocalVariable_logical2d_dshp
            procedure, private :: loadLocalVariable_byte2d_dshp
            procedure, private :: loadLocalVariable_short2d_dshp
            procedure, private :: loadLocalVariable_int2d_dshp
            procedure, private :: loadLocalVariable_long2d_dshp
            procedure, private :: loadLocalVariable_real2d_dshp
            procedure, private :: loadLocalVariable_dble2d_dshp

            procedure, private :: loadLocalVariable_logical3d_dshp
            procedure, private :: loadLocalVariable_byte3d_dshp
            procedure, private :: loadLocalVariable_short3d_dshp
            procedure, private :: loadLocalVariable_int3d_dshp
            procedure, private :: loadLocalVariable_long3d_dshp
            procedure, private :: loadLocalVariable_real3d_dshp
            procedure, private :: loadLocalVariable_dble3d_dshp

            procedure, private :: loadLocalVariable_logical4d_dshp
            procedure, private :: loadLocalVariable_byte4d_dshp
            procedure, private :: loadLocalVariable_short4d_dshp
            procedure, private :: loadLocalVariable_int4d_dshp
            procedure, private :: loadLocalVariable_long4d_dshp
            procedure, private :: loadLocalVariable_real4d_dshp
            procedure, private :: loadLocalVariable_dble4d_dshp

            procedure, private :: loadLocalVariable_logical5d_dshp
            procedure, private :: loadLocalVariable_byte5d_dshp
            procedure, private :: loadLocalVariable_short5d_dshp
            procedure, private :: loadLocalVariable_int5d_dshp
            procedure, private :: loadLocalVariable_long5d_dshp
            procedure, private :: loadLocalVariable_real5d_dshp
            procedure, private :: loadLocalVariable_dble5d_dshp

            procedure, private :: loadLocalVariable_logical6d_dshp
            procedure, private :: loadLocalVariable_byte6d_dshp
            procedure, private :: loadLocalVariable_short6d_dshp
            procedure, private :: loadLocalVariable_int6d_dshp
            procedure, private :: loadLocalVariable_long6d_dshp
            procedure, private :: loadLocalVariable_real6d_dshp
            procedure, private :: loadLocalVariable_dble6d_dshp

            procedure, private :: loadLocalVariable_logical7d_dshp
            procedure, private :: loadLocalVariable_byte7d_dshp
            procedure, private :: loadLocalVariable_short7d_dshp
            procedure, private :: loadLocalVariable_int7d_dshp
            procedure, private :: loadLocalVariable_long7d_dshp
            procedure, private :: loadLocalVariable_real7d_dshp
            procedure, private :: loadLocalVariable_dble7d_dshp

            generic   :: loadMirroredVariable =>                 &
                            loadMirroredVariable_dim1,           &
                            loadMirroredVariable_dim2,           &
                            loadMirroredVariable_dim3,           &
                            loadMirroredVariable_dim4,           &
                            loadMirroredVariable_dim5,           &
                            loadMirroredVariable_dim6,           &
                            loadMirroredVariable_dim7,           &
                            loadMirroredVariable_logical1d_dims, &
                            loadMirroredVariable_byte1d_dims,    &
                            loadMirroredVariable_short1d_dims,   &
                            loadMirroredVariable_int1d_dims,     &
                            loadMirroredVariable_long1d_dims,    &
                            loadMirroredVariable_real1d_dims,    &
                            loadMirroredVariable_dble1d_dims,    &
                            loadMirroredVariable_logical2d_dims, &
                            loadMirroredVariable_byte2d_dims,    &
                            loadMirroredVariable_short2d_dims,   &
                            loadMirroredVariable_int2d_dims,     &
                            loadMirroredVariable_long2d_dims,    &
                            loadMirroredVariable_real2d_dims,    &
                            loadMirroredVariable_dble2d_dims,    &
                            loadMirroredVariable_logical3d_dims, &
                            loadMirroredVariable_byte3d_dims,    &
                            loadMirroredVariable_short3d_dims,   &
                            loadMirroredVariable_int3d_dims,     &
                            loadMirroredVariable_long3d_dims,    &
                            loadMirroredVariable_real3d_dims,    &
                            loadMirroredVariable_dble3d_dims,    &
                            loadMirroredVariable_logical4d_dims, &
                            loadMirroredVariable_byte4d_dims,    &
                            loadMirroredVariable_short4d_dims,   &
                            loadMirroredVariable_int4d_dims,     &
                            loadMirroredVariable_long4d_dims,    &
                            loadMirroredVariable_real4d_dims,    &
                            loadMirroredVariable_dble4d_dims,    &
                            loadMirroredVariable_logical5d_dims, &
                            loadMirroredVariable_byte5d_dims,    &
                            loadMirroredVariable_short5d_dims,   &
                            loadMirroredVariable_int5d_dims,     &
                            loadMirroredVariable_long5d_dims,    &
                            loadMirroredVariable_real5d_dims,    &
                            loadMirroredVariable_dble5d_dims,    &
                            loadMirroredVariable_logical6d_dims, &
                            loadMirroredVariable_byte6d_dims,    &
                            loadMirroredVariable_short6d_dims,   &
                            loadMirroredVariable_int6d_dims,     &
                            loadMirroredVariable_long6d_dims,    &
                            loadMirroredVariable_real6d_dims,    &
                            loadMirroredVariable_dble6d_dims,    &
                            loadMirroredVariable_logical7d_dims, &
                            loadMirroredVariable_byte7d_dims,    &
                            loadMirroredVariable_short7d_dims,   &
                            loadMirroredVariable_int7d_dims,     &
                            loadMirroredVariable_long7d_dims,    &
                            loadMirroredVariable_real7d_dims,    &
                            loadMirroredVariable_dble7d_dims,    &
                            loadMirroredVariable_logical1d_dshp, &
                            loadMirroredVariable_byte1d_dshp,    &
                            loadMirroredVariable_short1d_dshp,   &
                            loadMirroredVariable_int1d_dshp,     &
                            loadMirroredVariable_long1d_dshp,    &
                            loadMirroredVariable_real1d_dshp,    &
                            loadMirroredVariable_dble1d_dshp,    &
                            loadMirroredVariable_logical2d_dshp, &
                            loadMirroredVariable_byte2d_dshp,    &
                            loadMirroredVariable_short2d_dshp,   &
                            loadMirroredVariable_int2d_dshp,     &
                            loadMirroredVariable_long2d_dshp,    &
                            loadMirroredVariable_real2d_dshp,    &
                            loadMirroredVariable_dble2d_dshp,    &
                            loadMirroredVariable_logical3d_dshp, &
                            loadMirroredVariable_byte3d_dshp,    &
                            loadMirroredVariable_short3d_dshp,   &
                            loadMirroredVariable_int3d_dshp,     &
                            loadMirroredVariable_long3d_dshp,    &
                            loadMirroredVariable_real3d_dshp,    &
                            loadMirroredVariable_dble3d_dshp,    &
                            loadMirroredVariable_logical4d_dshp, &
                            loadMirroredVariable_byte4d_dshp,    &
                            loadMirroredVariable_short4d_dshp,   &
                            loadMirroredVariable_int4d_dshp,     &
                            loadMirroredVariable_long4d_dshp,    &
                            loadMirroredVariable_real4d_dshp,    &
                            loadMirroredVariable_dble4d_dshp,    &
                            loadMirroredVariable_logical5d_dshp, &
                            loadMirroredVariable_byte5d_dshp,    &
                            loadMirroredVariable_short5d_dshp,   &
                            loadMirroredVariable_int5d_dshp,     &
                            loadMirroredVariable_long5d_dshp,    &
                            loadMirroredVariable_real5d_dshp,    &
                            loadMirroredVariable_dble5d_dshp,    &
                            loadMirroredVariable_logical6d_dshp, &
                            loadMirroredVariable_byte6d_dshp,    &
                            loadMirroredVariable_short6d_dshp,   &
                            loadMirroredVariable_int6d_dshp,     &
                            loadMirroredVariable_long6d_dshp,    &
                            loadMirroredVariable_real6d_dshp,    &
                            loadMirroredVariable_dble6d_dshp,    &
                            loadMirroredVariable_logical7d_dshp, &
                            loadMirroredVariable_byte7d_dshp,    &
                            loadMirroredVariable_short7d_dshp,   &
                            loadMirroredVariable_int7d_dshp,     &
                            loadMirroredVariable_long7d_dshp,    &
                            loadMirroredVariable_real7d_dshp,    &
                            loadMirroredVariable_dble7d_dshp

            procedure, private :: loadMirroredVariable_dim1
            procedure, private :: loadMirroredVariable_dim2
            procedure, private :: loadMirroredVariable_dim3
            procedure, private :: loadMirroredVariable_dim4
            procedure, private :: loadMirroredVariable_dim5
            procedure, private :: loadMirroredVariable_dim6
            procedure, private :: loadMirroredVariable_dim7

            procedure, private :: loadMirroredVariable_logical1d_dims
            procedure, private :: loadMirroredVariable_byte1d_dims
            procedure, private :: loadMirroredVariable_short1d_dims
            procedure, private :: loadMirroredVariable_int1d_dims
            procedure, private :: loadMirroredVariable_long1d_dims
            procedure, private :: loadMirroredVariable_real1d_dims
            procedure, private :: loadMirroredVariable_dble1d_dims

            procedure, private :: loadMirroredVariable_logical2d_dims
            procedure, private :: loadMirroredVariable_byte2d_dims
            procedure, private :: loadMirroredVariable_short2d_dims
            procedure, private :: loadMirroredVariable_int2d_dims
            procedure, private :: loadMirroredVariable_long2d_dims
            procedure, private :: loadMirroredVariable_real2d_dims
            procedure, private :: loadMirroredVariable_dble2d_dims

            procedure, private :: loadMirroredVariable_logical3d_dims
            procedure, private :: loadMirroredVariable_byte3d_dims
            procedure, private :: loadMirroredVariable_short3d_dims
            procedure, private :: loadMirroredVariable_int3d_dims
            procedure, private :: loadMirroredVariable_long3d_dims
            procedure, private :: loadMirroredVariable_real3d_dims
            procedure, private :: loadMirroredVariable_dble3d_dims

            procedure, private :: loadMirroredVariable_logical4d_dims
            procedure, private :: loadMirroredVariable_byte4d_dims
            procedure, private :: loadMirroredVariable_short4d_dims
            procedure, private :: loadMirroredVariable_int4d_dims
            procedure, private :: loadMirroredVariable_long4d_dims
            procedure, private :: loadMirroredVariable_real4d_dims
            procedure, private :: loadMirroredVariable_dble4d_dims

            procedure, private :: loadMirroredVariable_logical5d_dims
            procedure, private :: loadMirroredVariable_byte5d_dims
            procedure, private :: loadMirroredVariable_short5d_dims
            procedure, private :: loadMirroredVariable_int5d_dims
            procedure, private :: loadMirroredVariable_long5d_dims
            procedure, private :: loadMirroredVariable_real5d_dims
            procedure, private :: loadMirroredVariable_dble5d_dims

            procedure, private :: loadMirroredVariable_logical6d_dims
            procedure, private :: loadMirroredVariable_byte6d_dims
            procedure, private :: loadMirroredVariable_short6d_dims
            procedure, private :: loadMirroredVariable_int6d_dims
            procedure, private :: loadMirroredVariable_long6d_dims
            procedure, private :: loadMirroredVariable_real6d_dims
            procedure, private :: loadMirroredVariable_dble6d_dims

            procedure, private :: loadMirroredVariable_logical7d_dims
            procedure, private :: loadMirroredVariable_byte7d_dims
            procedure, private :: loadMirroredVariable_short7d_dims
            procedure, private :: loadMirroredVariable_int7d_dims
            procedure, private :: loadMirroredVariable_long7d_dims
            procedure, private :: loadMirroredVariable_real7d_dims
            procedure, private :: loadMirroredVariable_dble7d_dims

            procedure, private :: loadMirroredVariable_logical1d_dshp
            procedure, private :: loadMirroredVariable_byte1d_dshp
            procedure, private :: loadMirroredVariable_short1d_dshp
            procedure, private :: loadMirroredVariable_int1d_dshp
            procedure, private :: loadMirroredVariable_long1d_dshp
            procedure, private :: loadMirroredVariable_real1d_dshp
            procedure, private :: loadMirroredVariable_dble1d_dshp

            procedure, private :: loadMirroredVariable_logical2d_dshp
            procedure, private :: loadMirroredVariable_byte2d_dshp
            procedure, private :: loadMirroredVariable_short2d_dshp
            procedure, private :: loadMirroredVariable_int2d_dshp
            procedure, private :: loadMirroredVariable_long2d_dshp
            procedure, private :: loadMirroredVariable_real2d_dshp
            procedure, private :: loadMirroredVariable_dble2d_dshp

            procedure, private :: loadMirroredVariable_logical3d_dshp
            procedure, private :: loadMirroredVariable_byte3d_dshp
            procedure, private :: loadMirroredVariable_short3d_dshp
            procedure, private :: loadMirroredVariable_int3d_dshp
            procedure, private :: loadMirroredVariable_long3d_dshp
            procedure, private :: loadMirroredVariable_real3d_dshp
            procedure, private :: loadMirroredVariable_dble3d_dshp

            procedure, private :: loadMirroredVariable_logical4d_dshp
            procedure, private :: loadMirroredVariable_byte4d_dshp
            procedure, private :: loadMirroredVariable_short4d_dshp
            procedure, private :: loadMirroredVariable_int4d_dshp
            procedure, private :: loadMirroredVariable_long4d_dshp
            procedure, private :: loadMirroredVariable_real4d_dshp
            procedure, private :: loadMirroredVariable_dble4d_dshp

            procedure, private :: loadMirroredVariable_logical5d_dshp
            procedure, private :: loadMirroredVariable_byte5d_dshp
            procedure, private :: loadMirroredVariable_short5d_dshp
            procedure, private :: loadMirroredVariable_int5d_dshp
            procedure, private :: loadMirroredVariable_long5d_dshp
            procedure, private :: loadMirroredVariable_real5d_dshp
            procedure, private :: loadMirroredVariable_dble5d_dshp

            procedure, private :: loadMirroredVariable_logical6d_dshp
            procedure, private :: loadMirroredVariable_byte6d_dshp
            procedure, private :: loadMirroredVariable_short6d_dshp
            procedure, private :: loadMirroredVariable_int6d_dshp
            procedure, private :: loadMirroredVariable_long6d_dshp
            procedure, private :: loadMirroredVariable_real6d_dshp
            procedure, private :: loadMirroredVariable_dble6d_dshp

            procedure, private :: loadMirroredVariable_logical7d_dshp
            procedure, private :: loadMirroredVariable_byte7d_dshp
            procedure, private :: loadMirroredVariable_short7d_dshp
            procedure, private :: loadMirroredVariable_int7d_dshp
            procedure, private :: loadMirroredVariable_long7d_dshp
            procedure, private :: loadMirroredVariable_real7d_dshp
            procedure, private :: loadMirroredVariable_dble7d_dshp

            generic   :: loadDistributedVariable =>                 &
                            loadDistributedVariable_dim1,           &
                            loadDistributedVariable_dim2,           &
                            loadDistributedVariable_dim3,           &
                            loadDistributedVariable_dim4,           &
                            loadDistributedVariable_dim5,           &
                            loadDistributedVariable_dim6,           &
                            loadDistributedVariable_dim7,           &
                            loadDistributedVariable_logical1d_dims, &
                            loadDistributedVariable_byte1d_dims,    &
                            loadDistributedVariable_short1d_dims,   &
                            loadDistributedVariable_int1d_dims,     &
                            loadDistributedVariable_long1d_dims,    &
                            loadDistributedVariable_real1d_dims,    &
                            loadDistributedVariable_dble1d_dims,    &
                            loadDistributedVariable_logical2d_dims, &
                            loadDistributedVariable_byte2d_dims,    &
                            loadDistributedVariable_short2d_dims,   &
                            loadDistributedVariable_int2d_dims,     &
                            loadDistributedVariable_long2d_dims,    &
                            loadDistributedVariable_real2d_dims,    &
                            loadDistributedVariable_dble2d_dims,    &
                            loadDistributedVariable_logical3d_dims, &
                            loadDistributedVariable_byte3d_dims,    &
                            loadDistributedVariable_short3d_dims,   &
                            loadDistributedVariable_int3d_dims,     &
                            loadDistributedVariable_long3d_dims,    &
                            loadDistributedVariable_real3d_dims,    &
                            loadDistributedVariable_dble3d_dims,    &
                            loadDistributedVariable_logical4d_dims, &
                            loadDistributedVariable_byte4d_dims,    &
                            loadDistributedVariable_short4d_dims,   &
                            loadDistributedVariable_int4d_dims,     &
                            loadDistributedVariable_long4d_dims,    &
                            loadDistributedVariable_real4d_dims,    &
                            loadDistributedVariable_dble4d_dims,    &
                            loadDistributedVariable_logical5d_dims, &
                            loadDistributedVariable_byte5d_dims,    &
                            loadDistributedVariable_short5d_dims,   &
                            loadDistributedVariable_int5d_dims,     &
                            loadDistributedVariable_long5d_dims,    &
                            loadDistributedVariable_real5d_dims,    &
                            loadDistributedVariable_dble5d_dims,    &
                            loadDistributedVariable_logical6d_dims, &
                            loadDistributedVariable_byte6d_dims,    &
                            loadDistributedVariable_short6d_dims,   &
                            loadDistributedVariable_int6d_dims,     &
                            loadDistributedVariable_long6d_dims,    &
                            loadDistributedVariable_real6d_dims,    &
                            loadDistributedVariable_dble6d_dims,    &
                            loadDistributedVariable_logical7d_dims, &
                            loadDistributedVariable_byte7d_dims,    &
                            loadDistributedVariable_short7d_dims,   &
                            loadDistributedVariable_int7d_dims,     &
                            loadDistributedVariable_long7d_dims,    &
                            loadDistributedVariable_real7d_dims,    &
                            loadDistributedVariable_dble7d_dims,    &
                            loadDistributedVariable_logical1d_dshp, &
                            loadDistributedVariable_byte1d_dshp,    &
                            loadDistributedVariable_short1d_dshp,   &
                            loadDistributedVariable_int1d_dshp,     &
                            loadDistributedVariable_long1d_dshp,    &
                            loadDistributedVariable_real1d_dshp,    &
                            loadDistributedVariable_dble1d_dshp,    &
                            loadDistributedVariable_logical2d_dshp, &
                            loadDistributedVariable_byte2d_dshp,    &
                            loadDistributedVariable_short2d_dshp,   &
                            loadDistributedVariable_int2d_dshp,     &
                            loadDistributedVariable_long2d_dshp,    &
                            loadDistributedVariable_real2d_dshp,    &
                            loadDistributedVariable_dble2d_dshp,    &
                            loadDistributedVariable_logical3d_dshp, &
                            loadDistributedVariable_byte3d_dshp,    &
                            loadDistributedVariable_short3d_dshp,   &
                            loadDistributedVariable_int3d_dshp,     &
                            loadDistributedVariable_long3d_dshp,    &
                            loadDistributedVariable_real3d_dshp,    &
                            loadDistributedVariable_dble3d_dshp,    &
                            loadDistributedVariable_logical4d_dshp, &
                            loadDistributedVariable_byte4d_dshp,    &
                            loadDistributedVariable_short4d_dshp,   &
                            loadDistributedVariable_int4d_dshp,     &
                            loadDistributedVariable_long4d_dshp,    &
                            loadDistributedVariable_real4d_dshp,    &
                            loadDistributedVariable_dble4d_dshp,    &
                            loadDistributedVariable_logical5d_dshp, &
                            loadDistributedVariable_byte5d_dshp,    &
                            loadDistributedVariable_short5d_dshp,   &
                            loadDistributedVariable_int5d_dshp,     &
                            loadDistributedVariable_long5d_dshp,    &
                            loadDistributedVariable_real5d_dshp,    &
                            loadDistributedVariable_dble5d_dshp,    &
                            loadDistributedVariable_logical6d_dshp, &
                            loadDistributedVariable_byte6d_dshp,    &
                            loadDistributedVariable_short6d_dshp,   &
                            loadDistributedVariable_int6d_dshp,     &
                            loadDistributedVariable_long6d_dshp,    &
                            loadDistributedVariable_real6d_dshp,    &
                            loadDistributedVariable_dble6d_dshp,    &
                            loadDistributedVariable_logical7d_dshp, &
                            loadDistributedVariable_byte7d_dshp,    &
                            loadDistributedVariable_short7d_dshp,   &
                            loadDistributedVariable_int7d_dshp,     &
                            loadDistributedVariable_long7d_dshp,    &
                            loadDistributedVariable_real7d_dshp,    &
                            loadDistributedVariable_dble7d_dshp

            procedure, private :: loadDistributedVariable_dim1
            procedure, private :: loadDistributedVariable_dim2
            procedure, private :: loadDistributedVariable_dim3
            procedure, private :: loadDistributedVariable_dim4
            procedure, private :: loadDistributedVariable_dim5
            procedure, private :: loadDistributedVariable_dim6
            procedure, private :: loadDistributedVariable_dim7

            procedure, private :: loadDistributedVariable_logical1d_dims
            procedure, private :: loadDistributedVariable_byte1d_dims
            procedure, private :: loadDistributedVariable_short1d_dims
            procedure, private :: loadDistributedVariable_int1d_dims
            procedure, private :: loadDistributedVariable_long1d_dims
            procedure, private :: loadDistributedVariable_real1d_dims
            procedure, private :: loadDistributedVariable_dble1d_dims

            procedure, private :: loadDistributedVariable_logical2d_dims
            procedure, private :: loadDistributedVariable_byte2d_dims
            procedure, private :: loadDistributedVariable_short2d_dims
            procedure, private :: loadDistributedVariable_int2d_dims
            procedure, private :: loadDistributedVariable_long2d_dims
            procedure, private :: loadDistributedVariable_real2d_dims
            procedure, private :: loadDistributedVariable_dble2d_dims

            procedure, private :: loadDistributedVariable_logical3d_dims
            procedure, private :: loadDistributedVariable_byte3d_dims
            procedure, private :: loadDistributedVariable_short3d_dims
            procedure, private :: loadDistributedVariable_int3d_dims
            procedure, private :: loadDistributedVariable_long3d_dims
            procedure, private :: loadDistributedVariable_real3d_dims
            procedure, private :: loadDistributedVariable_dble3d_dims

            procedure, private :: loadDistributedVariable_logical4d_dims
            procedure, private :: loadDistributedVariable_byte4d_dims
            procedure, private :: loadDistributedVariable_short4d_dims
            procedure, private :: loadDistributedVariable_int4d_dims
            procedure, private :: loadDistributedVariable_long4d_dims
            procedure, private :: loadDistributedVariable_real4d_dims
            procedure, private :: loadDistributedVariable_dble4d_dims

            procedure, private :: loadDistributedVariable_logical5d_dims
            procedure, private :: loadDistributedVariable_byte5d_dims
            procedure, private :: loadDistributedVariable_short5d_dims
            procedure, private :: loadDistributedVariable_int5d_dims
            procedure, private :: loadDistributedVariable_long5d_dims
            procedure, private :: loadDistributedVariable_real5d_dims
            procedure, private :: loadDistributedVariable_dble5d_dims

            procedure, private :: loadDistributedVariable_logical6d_dims
            procedure, private :: loadDistributedVariable_byte6d_dims
            procedure, private :: loadDistributedVariable_short6d_dims
            procedure, private :: loadDistributedVariable_int6d_dims
            procedure, private :: loadDistributedVariable_long6d_dims
            procedure, private :: loadDistributedVariable_real6d_dims
            procedure, private :: loadDistributedVariable_dble6d_dims

            procedure, private :: loadDistributedVariable_logical7d_dims
            procedure, private :: loadDistributedVariable_byte7d_dims
            procedure, private :: loadDistributedVariable_short7d_dims
            procedure, private :: loadDistributedVariable_int7d_dims
            procedure, private :: loadDistributedVariable_long7d_dims
            procedure, private :: loadDistributedVariable_real7d_dims
            procedure, private :: loadDistributedVariable_dble7d_dims

            procedure, private :: loadDistributedVariable_logical1d_dshp
            procedure, private :: loadDistributedVariable_byte1d_dshp
            procedure, private :: loadDistributedVariable_short1d_dshp
            procedure, private :: loadDistributedVariable_int1d_dshp
            procedure, private :: loadDistributedVariable_long1d_dshp
            procedure, private :: loadDistributedVariable_real1d_dshp
            procedure, private :: loadDistributedVariable_dble1d_dshp

            procedure, private :: loadDistributedVariable_logical2d_dshp
            procedure, private :: loadDistributedVariable_byte2d_dshp
            procedure, private :: loadDistributedVariable_short2d_dshp
            procedure, private :: loadDistributedVariable_int2d_dshp
            procedure, private :: loadDistributedVariable_long2d_dshp
            procedure, private :: loadDistributedVariable_real2d_dshp
            procedure, private :: loadDistributedVariable_dble2d_dshp

            procedure, private :: loadDistributedVariable_logical3d_dshp
            procedure, private :: loadDistributedVariable_byte3d_dshp
            procedure, private :: loadDistributedVariable_short3d_dshp
            procedure, private :: loadDistributedVariable_int3d_dshp
            procedure, private :: loadDistributedVariable_long3d_dshp
            procedure, private :: loadDistributedVariable_real3d_dshp
            procedure, private :: loadDistributedVariable_dble3d_dshp

            procedure, private :: loadDistributedVariable_logical4d_dshp
            procedure, private :: loadDistributedVariable_byte4d_dshp
            procedure, private :: loadDistributedVariable_short4d_dshp
            procedure, private :: loadDistributedVariable_int4d_dshp
            procedure, private :: loadDistributedVariable_long4d_dshp
            procedure, private :: loadDistributedVariable_real4d_dshp
            procedure, private :: loadDistributedVariable_dble4d_dshp

            procedure, private :: loadDistributedVariable_logical5d_dshp
            procedure, private :: loadDistributedVariable_byte5d_dshp
            procedure, private :: loadDistributedVariable_short5d_dshp
            procedure, private :: loadDistributedVariable_int5d_dshp
            procedure, private :: loadDistributedVariable_long5d_dshp
            procedure, private :: loadDistributedVariable_real5d_dshp
            procedure, private :: loadDistributedVariable_dble5d_dshp

            procedure, private :: loadDistributedVariable_logical6d_dshp
            procedure, private :: loadDistributedVariable_byte6d_dshp
            procedure, private :: loadDistributedVariable_short6d_dshp
            procedure, private :: loadDistributedVariable_int6d_dshp
            procedure, private :: loadDistributedVariable_long6d_dshp
            procedure, private :: loadDistributedVariable_real6d_dshp
            procedure, private :: loadDistributedVariable_dble6d_dshp

            procedure, private :: loadDistributedVariable_logical7d_dshp
            procedure, private :: loadDistributedVariable_byte7d_dshp
            procedure, private :: loadDistributedVariable_short7d_dshp
            procedure, private :: loadDistributedVariable_int7d_dshp
            procedure, private :: loadDistributedVariable_long7d_dshp
            procedure, private :: loadDistributedVariable_real7d_dshp
            procedure, private :: loadDistributedVariable_dble7d_dshp

            procedure :: doLoadArray
            procedure :: doLoadDimSizeFromVariable

            procedure :: writeToFile

            procedure :: dataSetConstructor
            final     :: dataSetDestructor
    end type

    contains

    subroutine dataSetConstructor(this,reader)
        implicit none

        class(DataSet)  :: this

        class(DataArrayReader), optional, pointer    :: reader

        if (present(reader)) then
            this%reader  => reader
        end if

        ! set the name of the top group to an empty string
        call this%dataGroupConstructor('')
    end subroutine

    ! clean-up allocated variables
    subroutine dataSetDestructor(this)
        implicit none

        type(DataSet)  :: this

        if (associated(this%reader)) then
            deallocate(this%reader)
            nullify(this%reader)
        end if
    end subroutine

    function getDataArrayReader(this) result(reader)
        implicit none

        class(DataSet)         :: this

        class(DataArrayReader), pointer :: reader

        reader => this%reader
    end function

    subroutine setDataArrayReader(this,reader)
        implicit none

        class(DataSet)         :: this

        class(DataArrayReader), pointer :: reader

        if (associated(this%reader)) then
            deallocate(this%reader)
        end if
        this%reader => reader
    end subroutine

    function loadAttribute_varLenStrs(this,pinfo,attrName,stringList,maxStrLen,&
        locationInFile) result(attr)

        implicit none

        class(DataSet)                      :: this

        class(ParallelInfo),    pointer     :: pinfo

        character(*),           intent(in)  :: attrName
        class(LinkedList),      pointer     :: stringList
        integer,                intent(in)  :: maxStrLen
        character(*), optional, intent(in)  :: locationInFile

        class(DataAttribute),   pointer     :: attr

        class(DataArrayReader), pointer     :: dReader

        integer :: i

        allocate(attr)
        call attr%dataAttributeConstructor(STRINGS_TYPE_NAME,attrName)
        call attr%setMaxStringLength(maxStrLen)

        dReader => this%getDataArrayReader()

        if (.not. associated(dReader)) then
            call error('The reader was not associated for the call to loadAttribute for attr ' // attrName)
        end if

        if (present(locationInFile)) then
            call dReader%loadAttribute(pinfo,attr,locationInFile)
        else
            call dReader%loadAttribute(pinfo,attr,attrName)
        end if

        if (attr%isLoaded()) then
            ! load the value from the attr for convenience
            stringList => attr%getStringList()
        else
            stringList => null()
        end if
    end function

    function loadAttribute_logical(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                      :: this

        class(ParallelInfo),    pointer     :: pinfo

        character(*),           intent(in)  :: attrName
        logical,                intent(out) :: value
        logical,      optional, intent(in)  :: defaultValue
        character(*), optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_byte(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                       :: this

        class(ParallelInfo),     pointer     :: pinfo

        character(*),            intent(in)  :: attrName
        integer(int8),           intent(out) :: value
        integer(int8), optional, intent(in)  :: defaultValue
        character(*),  optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_short(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                        :: this

        class(ParallelInfo),      pointer     :: pinfo

        character(*),             intent(in)  :: attrName
        integer(int16),           intent(out) :: value
        integer(int16), optional, intent(in)  :: defaultValue
        character(*),   optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_int(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                        :: this

        class(ParallelInfo),      pointer     :: pinfo

        character(*),             intent(in)  :: attrName
        integer(int32),           intent(out) :: value
        integer(int32), optional, intent(in)  :: defaultValue
        character(*),   optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_long(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                        :: this

        class(ParallelInfo),      pointer     :: pinfo

        character(*),             intent(in)  :: attrName
        integer(int64),           intent(out) :: value
        integer(int64), optional, intent(in)  :: defaultValue
        character(*),   optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_real(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                      :: this

        class(ParallelInfo),    pointer     :: pinfo

        character(*),           intent(in)  :: attrName
        real(real32),           intent(out) :: value
        real(real32), optional, intent(in)  :: defaultValue
        character(*), optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadAttribute_dble(this,pinfo,attrName,value,defaultValue,locationInFile) result(attr)
        implicit none

        class(DataSet)                      :: this

        class(ParallelInfo),    pointer     :: pinfo

        character(len=*),       intent(in)  :: attrName
        real(real64),           intent(out) :: value
        real(real64), optional, intent(in)  :: defaultValue
        character(*), optional, intent(in)  :: locationInFile

        include 'dataSet_loadAttribute.incl'
    end function

    function loadDimensionFromVariable(this,pinfo,dimName,dimNum,locationInFile,&
        globalCount,globalStart,stagger,compareToDim,required) result(ddim)

        implicit none

        class(DataSet)                          :: this

        class(ParallelInfo),        pointer     :: pinfo
        character(len=*),           intent(in)  :: dimName
        integer,                    intent(in)  :: dimNum
        character(len=*), optional, intent(in)  :: locationInFile
        integer,          optional, intent(in)  :: globalCount
        integer,          optional, intent(in)  :: globalStart
        integer,          optional, intent(in)  :: stagger
        character(len=*), optional, intent(in)  :: compareToDim
        logical,          optional, intent(in)  :: required

        class(DataDimension),       pointer :: ddim
        class(DataArrayReader),     pointer :: dReader

        integer :: dimn

        dReader => this%getDataArrayReader()

        if (.not. associated(dReader)) then
            call error('The reader was not associated for the call to loadDimension for dim ' // dimName)
        end if

        allocate(ddim)
        if (present(locationInFile)) then
            dimn = dReader%loadDimSizeFromVariable(pinfo,locationInFile,dimNum,required)
        else
            dimn = dReader%loadDimSizeFromVariable(pinfo,dimName,dimNum,required)
        end if

        if (dimn >= 0) then
            ddim => this%addDimensionByName(dimName,dimn,globalCount,globalStart,&
                & stagger,compareToDim)
        else
            ddim => null()
        end if
    end function

    function loadVariable_dim1(this,pinfo,dTypeNum,variableName,dim1,locationInFile,&
        & required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                                     :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim2 => null()
        class(Datadimension),                   pointer    :: dim3 => null()
        class(Datadimension),                   pointer    :: dim4 => null()
        class(Datadimension),                   pointer    :: dim5 => null()
        class(Datadimension),                   pointer    :: dim6 => null()
        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 1

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim2(this,pinfo,dTypeNum,variableName,dim1,dim2,locationInFile, &
        & required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim3 => null()
        class(Datadimension),                   pointer    :: dim4 => null()
        class(Datadimension),                   pointer    :: dim5 => null()
        class(Datadimension),                   pointer    :: dim6 => null()
        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 2

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim3(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,&
        & locationInFile,required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                                     :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2, dim3
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim4 => null()
        class(Datadimension),                   pointer    :: dim5 => null()
        class(Datadimension),                   pointer    :: dim6 => null()
        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 3

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim4(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,&
        & locationInFile,required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                                     :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2, dim3, dim4
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim5 => null()
        class(Datadimension),                   pointer    :: dim6 => null()
        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 4

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim5(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,&
        & locationInFile,required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                                     :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2, dim3, dim4, dim5
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim6 => null()
        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 5

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim6(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,&
        locationInFile,required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        class(Datadimension),                   pointer    :: dim7 => null()

        integer, parameter :: ndim = 6

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_dim7(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            locationInFile,required,squeeze,loadDTypeNum) result(var)

        implicit none

        class(DataSet)                                     :: this

        character(len=*),                       intent(in) :: variableName
        integer,                                intent(in) :: dTypeNum
        class(DataDimension),                   pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),                    pointer    :: pinfo
        character(len=*),             optional, intent(in) :: locationInFile
        logical,                      optional, intent(in) :: required
        logical,                      optional, intent(in) :: squeeze
        integer,                      optional, intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer    :: var

        integer, parameter :: ndim = 7

        include 'dataSet_loadVariable.incl'
    end function

    function loadVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze,loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                     pointer    :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function


    function loadVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_byte7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_short7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_int7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_long7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_real7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_dble7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dims.incl'
    end function

    function loadVariable_logical1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function


    function loadVariable_byte4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_logical7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_byte7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        class(DataVariable),                     pointer     :: var
        integer,                       optional, intent(in)  :: loadDTypeNum

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_short7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_int7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_long7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_real7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadVariable_dble7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadVariable_dshp.incl'
    end function

    function loadLocalVariable_dim1(this,dTypeNum,variableName,dim1,locationInFile,pinfo,&
        required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 1

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim2(this,dTypeNum,variableName,dim1,dim2,locationInFile,&
        & pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 2

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim3(this,dTypeNum,variableName,dim1,dim2,dim3,&
        locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 3

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim4(this,dTypeNum,variableName,dim1,dim2,dim3,dim4,&
            locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 4

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim5(this,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,&
        & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 5

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim6(this,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,&
        locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 6

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_dim7(this,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                            :: this

        character(len=*),              intent(in) :: variableName
        integer,                       intent(in) :: dTypeNum
        class(DataDimension),          pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        character(len=*),    optional, intent(in) :: locationInFile
        class(ParallelInfo), optional, pointer    :: pinfo
        logical,             optional, intent(in) :: required
        logical,             optional, intent(in) :: squeeze
        integer,             optional, intent(in) :: loadDTypeNum

        class(DataVariable),           pointer    :: var

        integer, parameter :: ndim = 7

        include 'dataSet_loadLocalVariable.incl'
    end function

    function loadLocalVariable_logical1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble1d_dims(this,variableName,dataptr,dim1, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble2d_dims(this,variableName,dataptr,dim1,dim2, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function


    function loadLocalVariable_byte4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_byte7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),           optional, pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_short7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_int7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_long7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_real7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_dble7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dims.incl'
    end function

    function loadLocalVariable_logical1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble1d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble2d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble3d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function


    function loadLocalVariable_byte4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble4d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble5d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble6d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_logical7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_byte7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        class(ParallelInfo),           optional, pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_short7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_int7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_long7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),            optional, pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_real7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadLocalVariable_dble7d_dshp(this,variableName,dataptr,dShape, &
            & locationInFile,pinfo,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),          optional, pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        include 'dataSet_loadLocalVariable_dshp.incl'
    end function

    function loadMirroredVariable_dim1(this,pinfo,dTypeNum,variableName,dim1,locationInFile, &
        required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 1

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim2(this,pinfo,dTypeNum,variableName,dim1,dim2,&
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 2

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim3(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,&
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 3

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim4(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,&
            locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 4

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim5(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,&
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 5

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim6(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,&
        locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 6

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_dim7(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                          :: this

        character(len=*),            intent(in) :: variableName
        integer,                     intent(in) :: dTypeNum
        class(DataDimension),        pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),         pointer    :: pinfo
        character(len=*),  optional, intent(in) :: locationInFile
        logical,           optional, intent(in) :: required
        logical,           optional, intent(in) :: squeeze
        integer,           optional, intent(in) :: loadDTypeNum

        class(DataVariable),         pointer    :: var

        integer, parameter :: ndim = 7

        include 'dataSet_loadMirroredVariable.incl'
    end function

    function loadMirroredVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function


    function loadMirroredVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_byte7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_short7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_int7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_long7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_real7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_dble7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dims.incl'
    end function

    function loadMirroredVariable_logical1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function


    function loadMirroredVariable_byte4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_logical7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_byte7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_short7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_int7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_long7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                        optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_real7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadMirroredVariable_dble7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadMirroredVariable_dshp.incl'
    end function

    function loadDistributedVariable_dim1(this,pinfo,dTypeNum,variableName,dim1, &
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataDimension),         pointer    :: dim2 => null()
        class(DataDimension),         pointer    :: dim3 => null()
        class(DataDimension),         pointer    :: dim4 => null()
        class(DataDimension),         pointer    :: dim5 => null()
        class(DataDimension),         pointer    :: dim6 => null()
        class(DataDimension),         pointer    :: dim7 => null()

        class(DataVariable),          pointer    :: var

        integer, parameter :: ndim = 1

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim2(this,pinfo,dTypeNum,variableName,dim1,dim2,&
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        class(DataDimension),         pointer    :: dim3 => null()
        class(DataDimension),         pointer    :: dim4 => null()
        class(DataDimension),         pointer    :: dim5 => null()
        class(DataDimension),         pointer    :: dim6 => null()
        class(DataDimension),         pointer    :: dim7 => null()

        integer, parameter :: ndim = 2

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim3(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,&
        locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2, dim3
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        class(DataDimension),         pointer    :: dim4 => null()
        class(DataDimension),         pointer    :: dim5 => null()
        class(DataDimension),         pointer    :: dim6 => null()
        class(DataDimension),         pointer    :: dim7 => null()

        integer, parameter :: ndim = 3

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim4(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,&
            locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2, dim3, dim4
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        class(DataDimension),         pointer    :: dim5 => null()
        class(DataDimension),         pointer    :: dim6 => null()
        class(DataDimension),         pointer    :: dim7 => null()

        integer, parameter :: ndim = 4

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim5(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,&
        & locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2, dim3, dim4, dim5
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        class(DataDimension),         pointer    :: dim6 => null()
        class(DataDimension),         pointer    :: dim7 => null()

        integer, parameter :: ndim = 5

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim6(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,&
        locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2, dim3, dim4, dim5, dim6
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        integer, parameter :: ndim = 6

        class(DataDimension),         pointer    :: dim7 => null()

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_dim7(this,pinfo,dTypeNum,variableName,dim1,dim2,dim3,dim4,dim5,dim6,dim7,&
            locationInFile,required,squeeze, loadDTypeNum) result(var)

        implicit none

        class(DataSet)                           :: this

        character(len=*),             intent(in) :: variableName
        integer,                      intent(in) :: dTypeNum
        class(DataDimension),         pointer    :: dim1, dim2, dim3, dim4, dim5, dim6, dim7
        class(ParallelInfo),          pointer    :: pinfo
        character(len=*),  optional,  intent(in) :: locationInFile
        logical,           optional,  intent(in) :: required
        logical,           optional,  intent(in) :: squeeze
        integer,           optional,  intent(in) :: loadDTypeNum

        class(DataVariable),          pointer    :: var

        integer, parameter :: ndim = 7

        include 'dataSet_loadDistributedVariable.incl'
    end function

    function loadDistributedVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional,  intent(in) :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function


    function loadDistributedVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_byte7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                       optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_short7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),              optional,  intent(in)  :: locationInFile
        logical,                       optional,  intent(in)  :: required
        logical,                       optional,  intent(in)  :: squeeze
        integer,                       optional,  intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_int7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),              optional,  intent(in)  :: locationInFile
        logical,                       optional,  intent(in)  :: required
        logical,                       optional,  intent(in)  :: squeeze
        integer,                       optional,  intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_long7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),              optional,  intent(in)  :: locationInFile
        logical,                       optional,  intent(in)  :: required
        logical,                       optional,  intent(in)  :: squeeze
        integer,                       optional,  intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_real7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_dble7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dims.incl'
    end function

    function loadDistributedVariable_logical1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble1d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble2d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble3d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function


    function loadDistributedVariable_byte4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble4d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble5d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble6d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_logical7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_byte7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                       :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        class(ParallelInfo),                     pointer     :: pinfo
        character(len=*),              optional, intent(in)  :: locationInFile
        logical,                       optional, intent(in)  :: required
        logical,                       optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_short7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_int7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_long7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                        :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        class(ParallelInfo),                      pointer     :: pinfo
        character(len=*),               optional, intent(in)  :: locationInFile
        logical,                        optional, intent(in)  :: required
        logical,                        optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_real7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function loadDistributedVariable_dble7d_dshp(this,pinfo,variableName,dataptr,dShape, &
            & locationInFile,required,squeeze, loadDTypeNum) result(var)
        implicit none

        class(DataSet)                                      :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        class(ParallelInfo),                    pointer     :: pinfo
        character(len=*),             optional, intent(in)  :: locationInFile
        logical,                      optional, intent(in)  :: required
        logical,                      optional, intent(in)  :: squeeze
        integer,                      optional, intent(in)  :: loadDTypeNum

        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataSet_loadDistributedVariable_dshp.incl'
    end function

    function doLoadDimSizeFromVariable(this,pinfo,locationInFile,dimNum) result(dimn)

        implicit none

        class(DataSet)                          :: this

        class(ParallelInfo),        pointer     :: pinfo
        character(len=*),           intent(in)  :: locationInFile
        integer,                    intent(in)  :: dimNum

        integer                                 :: dimn

        class(DataArrayReader), pointer :: reader

        reader => this%getDataArrayReader()

        if (associated(reader)) then
            dimn = reader%loadDimSizeFromVariable(pinfo,locationInFile,dimNum)
        else
            call error('The reader was not associated for loadDimSizeFromVariable')
        end if
    end function

    subroutine doLoadArray(this,pinfo,dArray,locationInFile,required,loadDTypeNum)

        implicit none

        class(DataSet)                          :: this

        class(ParallelInfo),        pointer     :: pinfo
        class(DataArray),           pointer     :: dArray
        character(len=*),           intent(in)  :: locationInFile
        logical,          optional, intent(in)  :: required
        integer,          optional, intent(in)  :: loadDTypeNum

        class(DataArrayReader), pointer :: reader

        reader => this%getDataArrayReader()

        if (associated(reader)) then
            call reader%loadDataArray(pinfo,dArray,locationInFile,required,loadDTypeNum)
        else
            call error('The reader was not associated for loadLocalArray')
        end if
    end subroutine

    subroutine writeToFile(this,pinfo,writer,writeDTypeNum)
        implicit none

        class(DataSet)                     :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayWriter), pointer    :: writer
        integer,  optional,     intent(in) :: writeDTypeNum

        class(DataDimension), pointer :: ddim
        class(DataVariable),  pointer :: dvar

        integer :: i

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: dimNames
        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: varNames

        call this%getDimensionNames(dimNames)

        do i=1,size(dimNames)
            ddim => this%getDimensionByName(dimNames(i))
            call writer%writeDimension(pinfo,ddim)
        end do

        call this%getVariableNames(varNames)

        do i=1,size(varNames)
            dvar => this%getVariableByName(varNames(i))
            call writer%writeVariable(pinfo,dvar,writeDTypeNum=writeDTypeNum)
        end do
    end subroutine

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(DataSet), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),          pointer :: dsPtr
        class(DataGroup),        pointer :: dgPtr

        logical :: isShallow
        logical :: doCopy

        allocate(dsPtr)
        call dsPtr%dataSetConstructor(this%getDataArrayReader())

        if (present(shallow)) then
            isShallow = shallow
        else
            isShallow = .false.
        end if

        if (present(copyData)) then
            doCopy = copyData
        else
            doCopy = .false.
        end if

        if (.not. isShallow) then
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function
end module
