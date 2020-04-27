module dataGroup_mod

    use linkedList_mod
    use dictionary_mod

    use dataEnum_mod
    use dataType_mod
    use dataArray_mod
    use dataShape_mod
    use dataExtent_mod
    use dataVariable_mod
    use dataAttribute_mod
    use dataDimension_mod

    use mirroredVariable_mod
    use mirroredArray_mod

    use parallelConstants_mod
    use parallelInfo_mod

    use asciiUtils_mod

    use mpiUtils_mod

    implicit none

    private

    public :: DataGroup

    ! A class that represents a group of data along with descriptions of that data. Groups have a
    ! name and zero or more:
    ! subgroups (allowing for a recursive data tree), variables, attributes, and dimensions.
    ! The actual data is stored in variable objects, while attributes and dimensions describe the data.
    ! The data and meta data can be accessed from this group using a string syntax to navigate the
    ! tree structure. This is similar to HDF5, where a "/" separates group names, and an @ symbol
    ! is used for attribute names. This is a loose implementation/extention of
    ! the UCAR Unidata Common Data Model.
    type :: DataGroup
        character(:), allocatable  :: name
        class(Dictionary), pointer :: subgroups      => null()
        class(Dictionary), pointer :: variables      => null()
        class(Dictionary), pointer :: attributes     => null()
        class(Dictionary), pointer :: dims           => null()
        class(Dictionary), pointer :: enums          => null()

        contains
            procedure :: getName
            procedure :: setName

            procedure :: getGroup
            procedure :: getGroupByName
            procedure :: getVariable
            procedure :: getVariableByName
            procedure :: getVariableNames
            procedure :: getDataArray
            procedure :: getDataArrayByName
            procedure :: getMirroredArray
            procedure :: getMirroredArrayByName
            procedure :: getAttribute
            procedure :: getAttributeByName
            procedure :: getDimension
            procedure :: getDimensionByName
            procedure :: getDimensionNames
            procedure :: getGlobalDimCount
            procedure :: getEnum

            procedure :: getNDimensions

            procedure :: addGroup
            procedure :: addGroupByName

            procedure :: addAttribute
            ! easier to just add the attribute object
            ! procedure :: addAttributeByName

            procedure :: addDimension
            procedure :: addDimensionByName

            procedure :: addEnum

            procedure :: copy

            procedure, private :: trimSlashes
            procedure, private :: getOwnerGroup

            procedure :: deleteVariable

            procedure :: addVariablePointer

            generic   :: addVariable => &
                            addVariable_logical1d,      &
                            addVariable_byte1d,         &
                            addVariable_short1d,        &
                            addVariable_int1d,          &
                            addVariable_long1d,         &
                            addVariable_real1d,         &
                            addVariable_dble1d,         &
                            addVariable_logical2d,      &
                            addVariable_byte2d,         &
                            addVariable_short2d,        &
                            addVariable_int2d,          &
                            addVariable_long2d,         &
                            addVariable_real2d,         &
                            addVariable_dble2d,         &
                            addVariable_logical3d,      &
                            addVariable_byte3d,         &
                            addVariable_short3d,        &
                            addVariable_int3d,          &
                            addVariable_long3d,         &
                            addVariable_real3d,         &
                            addVariable_dble3d,         &
                            addVariable_logical4d,      &
                            addVariable_byte4d,         &
                            addVariable_short4d,        &
                            addVariable_int4d,          &
                            addVariable_long4d,         &
                            addVariable_real4d,         &
                            addVariable_dble4d,         &
                            addVariable_logical5d,      &
                            addVariable_byte5d,         &
                            addVariable_short5d,        &
                            addVariable_int5d,          &
                            addVariable_long5d,         &
                            addVariable_real5d,         &
                            addVariable_dble5d,         &
                            addVariable_logical6d,      &
                            addVariable_byte6d,         &
                            addVariable_short6d,        &
                            addVariable_int6d,          &
                            addVariable_long6d,         &
                            addVariable_real6d,         &
                            addVariable_dble6d,         &
                            addVariable_logical7d,      &
                            addVariable_byte7d,         &
                            addVariable_short7d,        &
                            addVariable_int7d,          &
                            addVariable_long7d,         &
                            addVariable_real7d,         &
                            addVariable_dble7d,         &
                            addVariable_logical1d_dims, &
                            addVariable_byte1d_dims,    &
                            addVariable_short1d_dims,   &
                            addVariable_int1d_dims,     &
                            addVariable_long1d_dims,    &
                            addVariable_real1d_dims,    &
                            addVariable_dble1d_dims,    &
                            addVariable_logical2d_dims, &
                            addVariable_byte2d_dims,    &
                            addVariable_short2d_dims,   &
                            addVariable_int2d_dims,     &
                            addVariable_long2d_dims,    &
                            addVariable_real2d_dims,    &
                            addVariable_dble2d_dims,    &
                            addVariable_logical3d_dims, &
                            addVariable_byte3d_dims,    &
                            addVariable_short3d_dims,   &
                            addVariable_int3d_dims,     &
                            addVariable_long3d_dims,    &
                            addVariable_real3d_dims,    &
                            addVariable_dble3d_dims,    &
                            addVariable_logical4d_dims, &
                            addVariable_byte4d_dims,    &
                            addVariable_short4d_dims,   &
                            addVariable_int4d_dims,     &
                            addVariable_long4d_dims,    &
                            addVariable_real4d_dims,    &
                            addVariable_dble4d_dims,    &
                            addVariable_logical5d_dims, &
                            addVariable_byte5d_dims,    &
                            addVariable_short5d_dims,   &
                            addVariable_int5d_dims,     &
                            addVariable_long5d_dims,    &
                            addVariable_real5d_dims,    &
                            addVariable_dble5d_dims,    &
                            addVariable_logical6d_dims, &
                            addVariable_byte6d_dims,    &
                            addVariable_short6d_dims,   &
                            addVariable_int6d_dims,     &
                            addVariable_long6d_dims,    &
                            addVariable_real6d_dims,    &
                            addVariable_dble6d_dims,    &
                            addVariable_logical7d_dims, &
                            addVariable_byte7d_dims,    &
                            addVariable_short7d_dims,   &
                            addVariable_int7d_dims,     &
                            addVariable_long7d_dims,    &
                            addVariable_real7d_dims,    &
                            addVariable_dble7d_dims,    &
                            addVariable_logical1d_extent, &
                            addVariable_byte1d_extent,    &
                            addVariable_short1d_extent,   &
                            addVariable_int1d_extent,     &
                            addVariable_long1d_extent,    &
                            addVariable_real1d_extent,    &
                            addVariable_dble1d_extent,    &
                            addVariable_logical2d_extent, &
                            addVariable_byte2d_extent,    &
                            addVariable_short2d_extent,   &
                            addVariable_int2d_extent,     &
                            addVariable_long2d_extent,    &
                            addVariable_real2d_extent,    &
                            addVariable_dble2d_extent,    &
                            addVariable_logical3d_extent, &
                            addVariable_byte3d_extent,    &
                            addVariable_short3d_extent,   &
                            addVariable_int3d_extent,     &
                            addVariable_long3d_extent,    &
                            addVariable_real3d_extent,    &
                            addVariable_dble3d_extent,    &
                            addVariable_logical4d_extent, &
                            addVariable_byte4d_extent,    &
                            addVariable_short4d_extent,   &
                            addVariable_int4d_extent,     &
                            addVariable_long4d_extent,    &
                            addVariable_real4d_extent,    &
                            addVariable_dble4d_extent,    &
                            addVariable_logical5d_extent, &
                            addVariable_byte5d_extent,    &
                            addVariable_short5d_extent,   &
                            addVariable_int5d_extent,     &
                            addVariable_long5d_extent,    &
                            addVariable_real5d_extent,    &
                            addVariable_dble5d_extent,    &
                            addVariable_logical6d_extent, &
                            addVariable_byte6d_extent,    &
                            addVariable_short6d_extent,   &
                            addVariable_int6d_extent,     &
                            addVariable_long6d_extent,    &
                            addVariable_real6d_extent,    &
                            addVariable_dble6d_extent,    &
                            addVariable_logical7d_extent, &
                            addVariable_byte7d_extent,    &
                            addVariable_short7d_extent,   &
                            addVariable_int7d_extent,     &
                            addVariable_long7d_extent,    &
                            addVariable_real7d_extent,    &
                            addVariable_dble7d_extent

            procedure, private :: addVariable_logical1d
            procedure, private :: addVariable_byte1d
            procedure, private :: addVariable_short1d
            procedure, private :: addVariable_int1d
            procedure, private :: addVariable_long1d
            procedure, private :: addVariable_real1d
            procedure, private :: addVariable_dble1d

            procedure, private :: addVariable_logical2d
            procedure, private :: addVariable_byte2d
            procedure, private :: addVariable_short2d
            procedure, private :: addVariable_int2d
            procedure, private :: addVariable_long2d
            procedure, private :: addVariable_real2d
            procedure, private :: addVariable_dble2d

            procedure, private :: addVariable_logical3d
            procedure, private :: addVariable_byte3d
            procedure, private :: addVariable_short3d
            procedure, private :: addVariable_int3d
            procedure, private :: addVariable_long3d
            procedure, private :: addVariable_real3d
            procedure, private :: addVariable_dble3d

            procedure, private :: addVariable_logical4d
            procedure, private :: addVariable_byte4d
            procedure, private :: addVariable_short4d
            procedure, private :: addVariable_int4d
            procedure, private :: addVariable_long4d
            procedure, private :: addVariable_real4d
            procedure, private :: addVariable_dble4d

            procedure, private :: addVariable_logical5d
            procedure, private :: addVariable_byte5d
            procedure, private :: addVariable_short5d
            procedure, private :: addVariable_int5d
            procedure, private :: addVariable_long5d
            procedure, private :: addVariable_real5d
            procedure, private :: addVariable_dble5d

            procedure, private :: addVariable_logical6d
            procedure, private :: addVariable_byte6d
            procedure, private :: addVariable_short6d
            procedure, private :: addVariable_int6d
            procedure, private :: addVariable_long6d
            procedure, private :: addVariable_real6d
            procedure, private :: addVariable_dble6d

            procedure, private :: addVariable_logical7d
            procedure, private :: addVariable_byte7d
            procedure, private :: addVariable_short7d
            procedure, private :: addVariable_int7d
            procedure, private :: addVariable_long7d
            procedure, private :: addVariable_real7d
            procedure, private :: addVariable_dble7d

            procedure, private :: addVariable_logical1d_dims
            procedure, private :: addVariable_byte1d_dims
            procedure, private :: addVariable_short1d_dims
            procedure, private :: addVariable_int1d_dims
            procedure, private :: addVariable_long1d_dims
            procedure, private :: addVariable_real1d_dims
            procedure, private :: addVariable_dble1d_dims

            procedure, private :: addVariable_logical2d_dims
            procedure, private :: addVariable_byte2d_dims
            procedure, private :: addVariable_short2d_dims
            procedure, private :: addVariable_int2d_dims
            procedure, private :: addVariable_long2d_dims
            procedure, private :: addVariable_real2d_dims
            procedure, private :: addVariable_dble2d_dims

            procedure, private :: addVariable_logical3d_dims
            procedure, private :: addVariable_byte3d_dims
            procedure, private :: addVariable_short3d_dims
            procedure, private :: addVariable_int3d_dims
            procedure, private :: addVariable_long3d_dims
            procedure, private :: addVariable_real3d_dims
            procedure, private :: addVariable_dble3d_dims

            procedure, private :: addVariable_logical4d_dims
            procedure, private :: addVariable_byte4d_dims
            procedure, private :: addVariable_short4d_dims
            procedure, private :: addVariable_int4d_dims
            procedure, private :: addVariable_long4d_dims
            procedure, private :: addVariable_real4d_dims
            procedure, private :: addVariable_dble4d_dims

            procedure, private :: addVariable_logical5d_dims
            procedure, private :: addVariable_byte5d_dims
            procedure, private :: addVariable_short5d_dims
            procedure, private :: addVariable_int5d_dims
            procedure, private :: addVariable_long5d_dims
            procedure, private :: addVariable_real5d_dims
            procedure, private :: addVariable_dble5d_dims

            procedure, private :: addVariable_logical6d_dims
            procedure, private :: addVariable_byte6d_dims
            procedure, private :: addVariable_short6d_dims
            procedure, private :: addVariable_int6d_dims
            procedure, private :: addVariable_long6d_dims
            procedure, private :: addVariable_real6d_dims
            procedure, private :: addVariable_dble6d_dims

            procedure, private :: addVariable_logical7d_dims
            procedure, private :: addVariable_byte7d_dims
            procedure, private :: addVariable_short7d_dims
            procedure, private :: addVariable_int7d_dims
            procedure, private :: addVariable_long7d_dims
            procedure, private :: addVariable_real7d_dims
            procedure, private :: addVariable_dble7d_dims

            procedure, private :: addVariable_logical1d_extent
            procedure, private :: addVariable_byte1d_extent
            procedure, private :: addVariable_short1d_extent
            procedure, private :: addVariable_int1d_extent
            procedure, private :: addVariable_long1d_extent
            procedure, private :: addVariable_real1d_extent
            procedure, private :: addVariable_dble1d_extent

            procedure, private :: addVariable_logical2d_extent
            procedure, private :: addVariable_byte2d_extent
            procedure, private :: addVariable_short2d_extent
            procedure, private :: addVariable_int2d_extent
            procedure, private :: addVariable_long2d_extent
            procedure, private :: addVariable_real2d_extent
            procedure, private :: addVariable_dble2d_extent

            procedure, private :: addVariable_logical3d_extent
            procedure, private :: addVariable_byte3d_extent
            procedure, private :: addVariable_short3d_extent
            procedure, private :: addVariable_int3d_extent
            procedure, private :: addVariable_long3d_extent
            procedure, private :: addVariable_real3d_extent
            procedure, private :: addVariable_dble3d_extent

            procedure, private :: addVariable_logical4d_extent
            procedure, private :: addVariable_byte4d_extent
            procedure, private :: addVariable_short4d_extent
            procedure, private :: addVariable_int4d_extent
            procedure, private :: addVariable_long4d_extent
            procedure, private :: addVariable_real4d_extent
            procedure, private :: addVariable_dble4d_extent

            procedure, private :: addVariable_logical5d_extent
            procedure, private :: addVariable_byte5d_extent
            procedure, private :: addVariable_short5d_extent
            procedure, private :: addVariable_int5d_extent
            procedure, private :: addVariable_long5d_extent
            procedure, private :: addVariable_real5d_extent
            procedure, private :: addVariable_dble5d_extent

            procedure, private :: addVariable_logical6d_extent
            procedure, private :: addVariable_byte6d_extent
            procedure, private :: addVariable_short6d_extent
            procedure, private :: addVariable_int6d_extent
            procedure, private :: addVariable_long6d_extent
            procedure, private :: addVariable_real6d_extent
            procedure, private :: addVariable_dble6d_extent

            procedure, private :: addVariable_logical7d_extent
            procedure, private :: addVariable_byte7d_extent
            procedure, private :: addVariable_short7d_extent
            procedure, private :: addVariable_int7d_extent
            procedure, private :: addVariable_long7d_extent
            procedure, private :: addVariable_real7d_extent
            procedure, private :: addVariable_dble7d_extent

            generic   :: addLocalVariable => &
                            addLocalVariable_logical1d,      &
                            addLocalVariable_byte1d,         &
                            addLocalVariable_short1d,        &
                            addLocalVariable_int1d,          &
                            addLocalVariable_long1d,         &
                            addLocalVariable_real1d,         &
                            addLocalVariable_dble1d,         &
                            addLocalVariable_logical2d,      &
                            addLocalVariable_byte2d,         &
                            addLocalVariable_short2d,        &
                            addLocalVariable_int2d,          &
                            addLocalVariable_long2d,         &
                            addLocalVariable_real2d,         &
                            addLocalVariable_dble2d,         &
                            addLocalVariable_logical3d,      &
                            addLocalVariable_byte3d,         &
                            addLocalVariable_short3d,        &
                            addLocalVariable_int3d,          &
                            addLocalVariable_long3d,         &
                            addLocalVariable_real3d,         &
                            addLocalVariable_dble3d,         &
                            addLocalVariable_logical4d,      &
                            addLocalVariable_byte4d,         &
                            addLocalVariable_short4d,        &
                            addLocalVariable_int4d,          &
                            addLocalVariable_long4d,         &
                            addLocalVariable_real4d,         &
                            addLocalVariable_dble4d,         &
                            addLocalVariable_logical5d,      &
                            addLocalVariable_byte5d,         &
                            addLocalVariable_short5d,        &
                            addLocalVariable_int5d,          &
                            addLocalVariable_long5d,         &
                            addLocalVariable_real5d,         &
                            addLocalVariable_dble5d,         &
                            addLocalVariable_logical6d,      &
                            addLocalVariable_byte6d,         &
                            addLocalVariable_short6d,        &
                            addLocalVariable_int6d,          &
                            addLocalVariable_long6d,         &
                            addLocalVariable_real6d,         &
                            addLocalVariable_dble6d,         &
                            addLocalVariable_logical7d,      &
                            addLocalVariable_byte7d,         &
                            addLocalVariable_short7d,        &
                            addLocalVariable_int7d,          &
                            addLocalVariable_long7d,         &
                            addLocalVariable_real7d,         &
                            addLocalVariable_dble7d,         &
                            addLocalVariable_logical1d_dims, &
                            addLocalVariable_byte1d_dims,    &
                            addLocalVariable_short1d_dims,   &
                            addLocalVariable_int1d_dims,     &
                            addLocalVariable_long1d_dims,    &
                            addLocalVariable_real1d_dims,    &
                            addLocalVariable_dble1d_dims,    &
                            addLocalVariable_logical2d_dims, &
                            addLocalVariable_byte2d_dims,    &
                            addLocalVariable_short2d_dims,   &
                            addLocalVariable_int2d_dims,     &
                            addLocalVariable_long2d_dims,    &
                            addLocalVariable_real2d_dims,    &
                            addLocalVariable_dble2d_dims,    &
                            addLocalVariable_logical3d_dims, &
                            addLocalVariable_byte3d_dims,    &
                            addLocalVariable_short3d_dims,   &
                            addLocalVariable_int3d_dims,     &
                            addLocalVariable_long3d_dims,    &
                            addLocalVariable_real3d_dims,    &
                            addLocalVariable_dble3d_dims,    &
                            addLocalVariable_logical4d_dims, &
                            addLocalVariable_byte4d_dims,    &
                            addLocalVariable_short4d_dims,   &
                            addLocalVariable_int4d_dims,     &
                            addLocalVariable_long4d_dims,    &
                            addLocalVariable_real4d_dims,    &
                            addLocalVariable_dble4d_dims,    &
                            addLocalVariable_logical5d_dims, &
                            addLocalVariable_byte5d_dims,    &
                            addLocalVariable_short5d_dims,   &
                            addLocalVariable_int5d_dims,     &
                            addLocalVariable_long5d_dims,    &
                            addLocalVariable_real5d_dims,    &
                            addLocalVariable_dble5d_dims,    &
                            addLocalVariable_logical6d_dims, &
                            addLocalVariable_byte6d_dims,    &
                            addLocalVariable_short6d_dims,   &
                            addLocalVariable_int6d_dims,     &
                            addLocalVariable_long6d_dims,    &
                            addLocalVariable_real6d_dims,    &
                            addLocalVariable_dble6d_dims,    &
                            addLocalVariable_logical7d_dims, &
                            addLocalVariable_byte7d_dims,    &
                            addLocalVariable_short7d_dims,   &
                            addLocalVariable_int7d_dims,     &
                            addLocalVariable_long7d_dims,    &
                            addLocalVariable_real7d_dims,    &
                            addLocalVariable_dble7d_dims

            procedure, private :: addLocalVariable_logical1d
            procedure, private :: addLocalVariable_byte1d
            procedure, private :: addLocalVariable_short1d
            procedure, private :: addLocalVariable_int1d
            procedure, private :: addLocalVariable_long1d
            procedure, private :: addLocalVariable_real1d
            procedure, private :: addLocalVariable_dble1d

            procedure, private :: addLocalVariable_logical2d
            procedure, private :: addLocalVariable_byte2d
            procedure, private :: addLocalVariable_short2d
            procedure, private :: addLocalVariable_int2d
            procedure, private :: addLocalVariable_long2d
            procedure, private :: addLocalVariable_real2d
            procedure, private :: addLocalVariable_dble2d

            procedure, private :: addLocalVariable_logical3d
            procedure, private :: addLocalVariable_byte3d
            procedure, private :: addLocalVariable_short3d
            procedure, private :: addLocalVariable_int3d
            procedure, private :: addLocalVariable_long3d
            procedure, private :: addLocalVariable_real3d
            procedure, private :: addLocalVariable_dble3d

            procedure, private :: addLocalVariable_logical4d
            procedure, private :: addLocalVariable_byte4d
            procedure, private :: addLocalVariable_short4d
            procedure, private :: addLocalVariable_int4d
            procedure, private :: addLocalVariable_long4d
            procedure, private :: addLocalVariable_real4d
            procedure, private :: addLocalVariable_dble4d

            procedure, private :: addLocalVariable_logical5d
            procedure, private :: addLocalVariable_byte5d
            procedure, private :: addLocalVariable_short5d
            procedure, private :: addLocalVariable_int5d
            procedure, private :: addLocalVariable_long5d
            procedure, private :: addLocalVariable_real5d
            procedure, private :: addLocalVariable_dble5d

            procedure, private :: addLocalVariable_logical6d
            procedure, private :: addLocalVariable_byte6d
            procedure, private :: addLocalVariable_short6d
            procedure, private :: addLocalVariable_int6d
            procedure, private :: addLocalVariable_long6d
            procedure, private :: addLocalVariable_real6d
            procedure, private :: addLocalVariable_dble6d

            procedure, private :: addLocalVariable_logical7d
            procedure, private :: addLocalVariable_byte7d
            procedure, private :: addLocalVariable_short7d
            procedure, private :: addLocalVariable_int7d
            procedure, private :: addLocalVariable_long7d
            procedure, private :: addLocalVariable_real7d
            procedure, private :: addLocalVariable_dble7d

            procedure, private :: addLocalVariable_logical1d_dims
            procedure, private :: addLocalVariable_byte1d_dims
            procedure, private :: addLocalVariable_short1d_dims
            procedure, private :: addLocalVariable_int1d_dims
            procedure, private :: addLocalVariable_long1d_dims
            procedure, private :: addLocalVariable_real1d_dims
            procedure, private :: addLocalVariable_dble1d_dims

            procedure, private :: addLocalVariable_logical2d_dims
            procedure, private :: addLocalVariable_byte2d_dims
            procedure, private :: addLocalVariable_short2d_dims
            procedure, private :: addLocalVariable_int2d_dims
            procedure, private :: addLocalVariable_long2d_dims
            procedure, private :: addLocalVariable_real2d_dims
            procedure, private :: addLocalVariable_dble2d_dims

            procedure, private :: addLocalVariable_logical3d_dims
            procedure, private :: addLocalVariable_byte3d_dims
            procedure, private :: addLocalVariable_short3d_dims
            procedure, private :: addLocalVariable_int3d_dims
            procedure, private :: addLocalVariable_long3d_dims
            procedure, private :: addLocalVariable_real3d_dims
            procedure, private :: addLocalVariable_dble3d_dims

            procedure, private :: addLocalVariable_logical4d_dims
            procedure, private :: addLocalVariable_byte4d_dims
            procedure, private :: addLocalVariable_short4d_dims
            procedure, private :: addLocalVariable_int4d_dims
            procedure, private :: addLocalVariable_long4d_dims
            procedure, private :: addLocalVariable_real4d_dims
            procedure, private :: addLocalVariable_dble4d_dims

            procedure, private :: addLocalVariable_logical5d_dims
            procedure, private :: addLocalVariable_byte5d_dims
            procedure, private :: addLocalVariable_short5d_dims
            procedure, private :: addLocalVariable_int5d_dims
            procedure, private :: addLocalVariable_long5d_dims
            procedure, private :: addLocalVariable_real5d_dims
            procedure, private :: addLocalVariable_dble5d_dims

            procedure, private :: addLocalVariable_logical6d_dims
            procedure, private :: addLocalVariable_byte6d_dims
            procedure, private :: addLocalVariable_short6d_dims
            procedure, private :: addLocalVariable_int6d_dims
            procedure, private :: addLocalVariable_long6d_dims
            procedure, private :: addLocalVariable_real6d_dims
            procedure, private :: addLocalVariable_dble6d_dims

            procedure, private :: addLocalVariable_logical7d_dims
            procedure, private :: addLocalVariable_byte7d_dims
            procedure, private :: addLocalVariable_short7d_dims
            procedure, private :: addLocalVariable_int7d_dims
            procedure, private :: addLocalVariable_long7d_dims
            procedure, private :: addLocalVariable_real7d_dims
            procedure, private :: addLocalVariable_dble7d_dims

            generic   :: addMirroredVariable => &
                            addMirroredVariable_logical1d,      &
                            addMirroredVariable_byte1d,         &
                            addMirroredVariable_short1d,        &
                            addMirroredVariable_int1d,          &
                            addMirroredVariable_long1d,         &
                            addMirroredVariable_real1d,         &
                            addMirroredVariable_dble1d,         &
                            addMirroredVariable_logical2d,      &
                            addMirroredVariable_byte2d,         &
                            addMirroredVariable_short2d,        &
                            addMirroredVariable_int2d,          &
                            addMirroredVariable_long2d,         &
                            addMirroredVariable_real2d,         &
                            addMirroredVariable_dble2d,         &
                            addMirroredVariable_logical3d,      &
                            addMirroredVariable_byte3d,         &
                            addMirroredVariable_short3d,        &
                            addMirroredVariable_int3d,          &
                            addMirroredVariable_long3d,         &
                            addMirroredVariable_real3d,         &
                            addMirroredVariable_dble3d,         &
                            addMirroredVariable_logical4d,      &
                            addMirroredVariable_byte4d,         &
                            addMirroredVariable_short4d,        &
                            addMirroredVariable_int4d,          &
                            addMirroredVariable_long4d,         &
                            addMirroredVariable_real4d,         &
                            addMirroredVariable_dble4d,         &
                            addMirroredVariable_logical5d,      &
                            addMirroredVariable_byte5d,         &
                            addMirroredVariable_short5d,        &
                            addMirroredVariable_int5d,          &
                            addMirroredVariable_long5d,         &
                            addMirroredVariable_real5d,         &
                            addMirroredVariable_dble5d,         &
                            addMirroredVariable_logical6d,      &
                            addMirroredVariable_byte6d,         &
                            addMirroredVariable_short6d,        &
                            addMirroredVariable_int6d,          &
                            addMirroredVariable_long6d,         &
                            addMirroredVariable_real6d,         &
                            addMirroredVariable_dble6d,         &
                            addMirroredVariable_logical7d,      &
                            addMirroredVariable_byte7d,         &
                            addMirroredVariable_short7d,        &
                            addMirroredVariable_int7d,          &
                            addMirroredVariable_long7d,         &
                            addMirroredVariable_real7d,         &
                            addMirroredVariable_dble7d,         &
                            addMirroredVariable_logical1d_dims, &
                            addMirroredVariable_byte1d_dims,    &
                            addMirroredVariable_short1d_dims,   &
                            addMirroredVariable_int1d_dims,     &
                            addMirroredVariable_long1d_dims,    &
                            addMirroredVariable_real1d_dims,    &
                            addMirroredVariable_dble1d_dims,    &
                            addMirroredVariable_logical2d_dims, &
                            addMirroredVariable_byte2d_dims,    &
                            addMirroredVariable_short2d_dims,   &
                            addMirroredVariable_int2d_dims,     &
                            addMirroredVariable_long2d_dims,    &
                            addMirroredVariable_real2d_dims,    &
                            addMirroredVariable_dble2d_dims,    &
                            addMirroredVariable_logical3d_dims, &
                            addMirroredVariable_byte3d_dims,    &
                            addMirroredVariable_short3d_dims,   &
                            addMirroredVariable_int3d_dims,     &
                            addMirroredVariable_long3d_dims,    &
                            addMirroredVariable_real3d_dims,    &
                            addMirroredVariable_dble3d_dims,    &
                            addMirroredVariable_logical4d_dims, &
                            addMirroredVariable_byte4d_dims,    &
                            addMirroredVariable_short4d_dims,   &
                            addMirroredVariable_int4d_dims,     &
                            addMirroredVariable_long4d_dims,    &
                            addMirroredVariable_real4d_dims,    &
                            addMirroredVariable_dble4d_dims,    &
                            addMirroredVariable_logical5d_dims, &
                            addMirroredVariable_byte5d_dims,    &
                            addMirroredVariable_short5d_dims,   &
                            addMirroredVariable_int5d_dims,     &
                            addMirroredVariable_long5d_dims,    &
                            addMirroredVariable_real5d_dims,    &
                            addMirroredVariable_dble5d_dims,    &
                            addMirroredVariable_logical6d_dims, &
                            addMirroredVariable_byte6d_dims,    &
                            addMirroredVariable_short6d_dims,   &
                            addMirroredVariable_int6d_dims,     &
                            addMirroredVariable_long6d_dims,    &
                            addMirroredVariable_real6d_dims,    &
                            addMirroredVariable_dble6d_dims,    &
                            addMirroredVariable_logical7d_dims, &
                            addMirroredVariable_byte7d_dims,    &
                            addMirroredVariable_short7d_dims,   &
                            addMirroredVariable_int7d_dims,     &
                            addMirroredVariable_long7d_dims,    &
                            addMirroredVariable_real7d_dims,    &
                            addMirroredVariable_dble7d_dims

            procedure, private :: addMirroredVariable_logical1d
            procedure, private :: addMirroredVariable_byte1d
            procedure, private :: addMirroredVariable_short1d
            procedure, private :: addMirroredVariable_int1d
            procedure, private :: addMirroredVariable_long1d
            procedure, private :: addMirroredVariable_real1d
            procedure, private :: addMirroredVariable_dble1d

            procedure, private :: addMirroredVariable_logical2d
            procedure, private :: addMirroredVariable_byte2d
            procedure, private :: addMirroredVariable_short2d
            procedure, private :: addMirroredVariable_int2d
            procedure, private :: addMirroredVariable_long2d
            procedure, private :: addMirroredVariable_real2d
            procedure, private :: addMirroredVariable_dble2d

            procedure, private :: addMirroredVariable_logical3d
            procedure, private :: addMirroredVariable_byte3d
            procedure, private :: addMirroredVariable_short3d
            procedure, private :: addMirroredVariable_int3d
            procedure, private :: addMirroredVariable_long3d
            procedure, private :: addMirroredVariable_real3d
            procedure, private :: addMirroredVariable_dble3d

            procedure, private :: addMirroredVariable_logical4d
            procedure, private :: addMirroredVariable_byte4d
            procedure, private :: addMirroredVariable_short4d
            procedure, private :: addMirroredVariable_int4d
            procedure, private :: addMirroredVariable_long4d
            procedure, private :: addMirroredVariable_real4d
            procedure, private :: addMirroredVariable_dble4d

            procedure, private :: addMirroredVariable_logical5d
            procedure, private :: addMirroredVariable_byte5d
            procedure, private :: addMirroredVariable_short5d
            procedure, private :: addMirroredVariable_int5d
            procedure, private :: addMirroredVariable_long5d
            procedure, private :: addMirroredVariable_real5d
            procedure, private :: addMirroredVariable_dble5d

            procedure, private :: addMirroredVariable_logical6d
            procedure, private :: addMirroredVariable_byte6d
            procedure, private :: addMirroredVariable_short6d
            procedure, private :: addMirroredVariable_int6d
            procedure, private :: addMirroredVariable_long6d
            procedure, private :: addMirroredVariable_real6d
            procedure, private :: addMirroredVariable_dble6d

            procedure, private :: addMirroredVariable_logical7d
            procedure, private :: addMirroredVariable_byte7d
            procedure, private :: addMirroredVariable_short7d
            procedure, private :: addMirroredVariable_int7d
            procedure, private :: addMirroredVariable_long7d
            procedure, private :: addMirroredVariable_real7d
            procedure, private :: addMirroredVariable_dble7d

            procedure, private :: addMirroredVariable_logical1d_dims
            procedure, private :: addMirroredVariable_byte1d_dims
            procedure, private :: addMirroredVariable_short1d_dims
            procedure, private :: addMirroredVariable_int1d_dims
            procedure, private :: addMirroredVariable_long1d_dims
            procedure, private :: addMirroredVariable_real1d_dims
            procedure, private :: addMirroredVariable_dble1d_dims

            procedure, private :: addMirroredVariable_logical2d_dims
            procedure, private :: addMirroredVariable_byte2d_dims
            procedure, private :: addMirroredVariable_short2d_dims
            procedure, private :: addMirroredVariable_int2d_dims
            procedure, private :: addMirroredVariable_long2d_dims
            procedure, private :: addMirroredVariable_real2d_dims
            procedure, private :: addMirroredVariable_dble2d_dims

            procedure, private :: addMirroredVariable_logical3d_dims
            procedure, private :: addMirroredVariable_byte3d_dims
            procedure, private :: addMirroredVariable_short3d_dims
            procedure, private :: addMirroredVariable_int3d_dims
            procedure, private :: addMirroredVariable_long3d_dims
            procedure, private :: addMirroredVariable_real3d_dims
            procedure, private :: addMirroredVariable_dble3d_dims

            procedure, private :: addMirroredVariable_logical4d_dims
            procedure, private :: addMirroredVariable_byte4d_dims
            procedure, private :: addMirroredVariable_short4d_dims
            procedure, private :: addMirroredVariable_int4d_dims
            procedure, private :: addMirroredVariable_long4d_dims
            procedure, private :: addMirroredVariable_real4d_dims
            procedure, private :: addMirroredVariable_dble4d_dims

            procedure, private :: addMirroredVariable_logical5d_dims
            procedure, private :: addMirroredVariable_byte5d_dims
            procedure, private :: addMirroredVariable_short5d_dims
            procedure, private :: addMirroredVariable_int5d_dims
            procedure, private :: addMirroredVariable_long5d_dims
            procedure, private :: addMirroredVariable_real5d_dims
            procedure, private :: addMirroredVariable_dble5d_dims

            procedure, private :: addMirroredVariable_logical6d_dims
            procedure, private :: addMirroredVariable_byte6d_dims
            procedure, private :: addMirroredVariable_short6d_dims
            procedure, private :: addMirroredVariable_int6d_dims
            procedure, private :: addMirroredVariable_long6d_dims
            procedure, private :: addMirroredVariable_real6d_dims
            procedure, private :: addMirroredVariable_dble6d_dims

            procedure, private :: addMirroredVariable_logical7d_dims
            procedure, private :: addMirroredVariable_byte7d_dims
            procedure, private :: addMirroredVariable_short7d_dims
            procedure, private :: addMirroredVariable_int7d_dims
            procedure, private :: addMirroredVariable_long7d_dims
            procedure, private :: addMirroredVariable_real7d_dims
            procedure, private :: addMirroredVariable_dble7d_dims


            generic  :: addDistributedVariable => &
                            addDistributedVariable_dtype,          &
                            addDistributedVariable_logical1d,      &
                            addDistributedVariable_byte1d,         &
                            addDistributedVariable_short1d,        &
                            addDistributedVariable_int1d,          &
                            addDistributedVariable_long1d,         &
                            addDistributedVariable_real1d,         &
                            addDistributedVariable_dble1d,         &
                            addDistributedVariable_logical2d,      &
                            addDistributedVariable_byte2d,         &
                            addDistributedVariable_short2d,        &
                            addDistributedVariable_int2d,          &
                            addDistributedVariable_long2d,         &
                            addDistributedVariable_real2d,         &
                            addDistributedVariable_dble2d,         &
                            addDistributedVariable_logical3d,      &
                            addDistributedVariable_byte3d,         &
                            addDistributedVariable_short3d,        &
                            addDistributedVariable_int3d,          &
                            addDistributedVariable_long3d,         &
                            addDistributedVariable_real3d,         &
                            addDistributedVariable_dble3d,         &
                            addDistributedVariable_logical4d,      &
                            addDistributedVariable_byte4d,         &
                            addDistributedVariable_short4d,        &
                            addDistributedVariable_int4d,          &
                            addDistributedVariable_long4d,         &
                            addDistributedVariable_real4d,         &
                            addDistributedVariable_dble4d,         &
                            addDistributedVariable_logical5d,      &
                            addDistributedVariable_byte5d,         &
                            addDistributedVariable_short5d,        &
                            addDistributedVariable_int5d,          &
                            addDistributedVariable_long5d,         &
                            addDistributedVariable_real5d,         &
                            addDistributedVariable_dble5d,         &
                            addDistributedVariable_logical6d,      &
                            addDistributedVariable_byte6d,         &
                            addDistributedVariable_short6d,        &
                            addDistributedVariable_int6d,          &
                            addDistributedVariable_long6d,         &
                            addDistributedVariable_real6d,         &
                            addDistributedVariable_dble6d,         &
                            addDistributedVariable_logical7d,      &
                            addDistributedVariable_byte7d,         &
                            addDistributedVariable_short7d,        &
                            addDistributedVariable_int7d,          &
                            addDistributedVariable_long7d,         &
                            addDistributedVariable_real7d,         &
                            addDistributedVariable_dble7d,         &
                            addDistributedVariable_logical1d_dims, &
                            addDistributedVariable_byte1d_dims,    &
                            addDistributedVariable_short1d_dims,   &
                            addDistributedVariable_int1d_dims,     &
                            addDistributedVariable_long1d_dims,    &
                            addDistributedVariable_real1d_dims,    &
                            addDistributedVariable_dble1d_dims,    &
                            addDistributedVariable_logical2d_dims, &
                            addDistributedVariable_byte2d_dims,    &
                            addDistributedVariable_short2d_dims,   &
                            addDistributedVariable_int2d_dims,     &
                            addDistributedVariable_long2d_dims,    &
                            addDistributedVariable_real2d_dims,    &
                            addDistributedVariable_dble2d_dims,    &
                            addDistributedVariable_logical3d_dims, &
                            addDistributedVariable_byte3d_dims,    &
                            addDistributedVariable_short3d_dims,   &
                            addDistributedVariable_int3d_dims,     &
                            addDistributedVariable_long3d_dims,    &
                            addDistributedVariable_real3d_dims,    &
                            addDistributedVariable_dble3d_dims,    &
                            addDistributedVariable_logical4d_dims, &
                            addDistributedVariable_byte4d_dims,    &
                            addDistributedVariable_short4d_dims,   &
                            addDistributedVariable_int4d_dims,     &
                            addDistributedVariable_long4d_dims,    &
                            addDistributedVariable_real4d_dims,    &
                            addDistributedVariable_dble4d_dims,    &
                            addDistributedVariable_logical5d_dims, &
                            addDistributedVariable_byte5d_dims,    &
                            addDistributedVariable_short5d_dims,   &
                            addDistributedVariable_int5d_dims,     &
                            addDistributedVariable_long5d_dims,    &
                            addDistributedVariable_real5d_dims,    &
                            addDistributedVariable_dble5d_dims,    &
                            addDistributedVariable_logical6d_dims, &
                            addDistributedVariable_byte6d_dims,    &
                            addDistributedVariable_short6d_dims,   &
                            addDistributedVariable_int6d_dims,     &
                            addDistributedVariable_long6d_dims,    &
                            addDistributedVariable_real6d_dims,    &
                            addDistributedVariable_dble6d_dims,    &
                            addDistributedVariable_logical7d_dims, &
                            addDistributedVariable_byte7d_dims,    &
                            addDistributedVariable_short7d_dims,   &
                            addDistributedVariable_int7d_dims,     &
                            addDistributedVariable_long7d_dims,    &
                            addDistributedVariable_real7d_dims,    &
                            addDistributedVariable_dble7d_dims

            procedure, private :: addDistributedVariable_dtype

            procedure, private :: addDistributedVariable_logical1d
            procedure, private :: addDistributedVariable_byte1d
            procedure, private :: addDistributedVariable_short1d
            procedure, private :: addDistributedVariable_int1d
            procedure, private :: addDistributedVariable_long1d
            procedure, private :: addDistributedVariable_real1d
            procedure, private :: addDistributedVariable_dble1d

            procedure, private :: addDistributedVariable_logical2d
            procedure, private :: addDistributedVariable_byte2d
            procedure, private :: addDistributedVariable_short2d
            procedure, private :: addDistributedVariable_int2d
            procedure, private :: addDistributedVariable_long2d
            procedure, private :: addDistributedVariable_real2d
            procedure, private :: addDistributedVariable_dble2d

            procedure, private :: addDistributedVariable_logical3d
            procedure, private :: addDistributedVariable_byte3d
            procedure, private :: addDistributedVariable_short3d
            procedure, private :: addDistributedVariable_int3d
            procedure, private :: addDistributedVariable_long3d
            procedure, private :: addDistributedVariable_real3d
            procedure, private :: addDistributedVariable_dble3d

            procedure, private :: addDistributedVariable_logical4d
            procedure, private :: addDistributedVariable_byte4d
            procedure, private :: addDistributedVariable_short4d
            procedure, private :: addDistributedVariable_int4d
            procedure, private :: addDistributedVariable_long4d
            procedure, private :: addDistributedVariable_real4d
            procedure, private :: addDistributedVariable_dble4d

            procedure, private :: addDistributedVariable_logical5d
            procedure, private :: addDistributedVariable_byte5d
            procedure, private :: addDistributedVariable_short5d
            procedure, private :: addDistributedVariable_int5d
            procedure, private :: addDistributedVariable_long5d
            procedure, private :: addDistributedVariable_real5d
            procedure, private :: addDistributedVariable_dble5d

            procedure, private :: addDistributedVariable_logical6d
            procedure, private :: addDistributedVariable_byte6d
            procedure, private :: addDistributedVariable_short6d
            procedure, private :: addDistributedVariable_int6d
            procedure, private :: addDistributedVariable_long6d
            procedure, private :: addDistributedVariable_real6d
            procedure, private :: addDistributedVariable_dble6d

            procedure, private :: addDistributedVariable_logical7d
            procedure, private :: addDistributedVariable_byte7d
            procedure, private :: addDistributedVariable_short7d
            procedure, private :: addDistributedVariable_int7d
            procedure, private :: addDistributedVariable_long7d
            procedure, private :: addDistributedVariable_real7d
            procedure, private :: addDistributedVariable_dble7d

            procedure, private :: addDistributedVariable_logical1d_dims
            procedure, private :: addDistributedVariable_byte1d_dims
            procedure, private :: addDistributedVariable_short1d_dims
            procedure, private :: addDistributedVariable_int1d_dims
            procedure, private :: addDistributedVariable_long1d_dims
            procedure, private :: addDistributedVariable_real1d_dims
            procedure, private :: addDistributedVariable_dble1d_dims

            procedure, private :: addDistributedVariable_logical2d_dims
            procedure, private :: addDistributedVariable_byte2d_dims
            procedure, private :: addDistributedVariable_short2d_dims
            procedure, private :: addDistributedVariable_int2d_dims
            procedure, private :: addDistributedVariable_long2d_dims
            procedure, private :: addDistributedVariable_real2d_dims
            procedure, private :: addDistributedVariable_dble2d_dims

            procedure, private :: addDistributedVariable_logical3d_dims
            procedure, private :: addDistributedVariable_byte3d_dims
            procedure, private :: addDistributedVariable_short3d_dims
            procedure, private :: addDistributedVariable_int3d_dims
            procedure, private :: addDistributedVariable_long3d_dims
            procedure, private :: addDistributedVariable_real3d_dims
            procedure, private :: addDistributedVariable_dble3d_dims

            procedure, private :: addDistributedVariable_logical4d_dims
            procedure, private :: addDistributedVariable_byte4d_dims
            procedure, private :: addDistributedVariable_short4d_dims
            procedure, private :: addDistributedVariable_int4d_dims
            procedure, private :: addDistributedVariable_long4d_dims
            procedure, private :: addDistributedVariable_real4d_dims
            procedure, private :: addDistributedVariable_dble4d_dims

            procedure, private :: addDistributedVariable_logical5d_dims
            procedure, private :: addDistributedVariable_byte5d_dims
            procedure, private :: addDistributedVariable_short5d_dims
            procedure, private :: addDistributedVariable_int5d_dims
            procedure, private :: addDistributedVariable_long5d_dims
            procedure, private :: addDistributedVariable_real5d_dims
            procedure, private :: addDistributedVariable_dble5d_dims

            procedure, private :: addDistributedVariable_logical6d_dims
            procedure, private :: addDistributedVariable_byte6d_dims
            procedure, private :: addDistributedVariable_short6d_dims
            procedure, private :: addDistributedVariable_int6d_dims
            procedure, private :: addDistributedVariable_long6d_dims
            procedure, private :: addDistributedVariable_real6d_dims
            procedure, private :: addDistributedVariable_dble6d_dims

            procedure, private :: addDistributedVariable_logical7d_dims
            procedure, private :: addDistributedVariable_byte7d_dims
            procedure, private :: addDistributedVariable_short7d_dims
            procedure, private :: addDistributedVariable_int7d_dims
            procedure, private :: addDistributedVariable_long7d_dims
            procedure, private :: addDistributedVariable_real7d_dims
            procedure, private :: addDistributedVariable_dble7d_dims
            procedure :: synchronize

            procedure :: dataGroupConstructor
            final     :: dataGroupDestructor

            procedure :: zeroAll
            procedure :: cloneGroup
    end type

    contains

    subroutine dataGroupConstructor(this,name)
        implicit none

        class(DataGroup)                      :: this

        character(len=*), intent(in)          :: name

        call this%setName(name)

        ! give an approximate number of values for performance - this can grow at run-time, don't worry
        this%subgroups  => Dictionary(10)
        this%variables  => Dictionary(100)
        this%attributes => Dictionary(100)
        this%dims       => Dictionary(10)
        this%enums      => Dictionary(10)
    end subroutine

    subroutine dataGroupDestructor(this)
        implicit none

        type(DataGroup)    :: this

        if (associated(this%subgroups)) then
            deallocate(this%subgroups) ! deallocates all objects in the dictionary as well
        end if

        if (associated(this%variables)) then
            deallocate(this%variables) ! deallocates all objects in the dictionary as well
        end if

        if (associated(this%attributes)) then
            deallocate(this%attributes) ! deallocates all objects in the dictionary as well
        end if

        if (associated(this%dims)) then
            deallocate(this%dims) ! deallocates all objects in the linked list as well
        end if

        if (associated(this%enums)) then
            deallocate(this%enums) ! deallocates all objects in the linked list as well
        end if
    end subroutine

    function getName(this) result(name)
        implicit none

        class(DataGroup)          :: this

        character(:), allocatable :: name

        name = this%name
    end function

    subroutine setName(this,name)
        implicit none

        class(DataGroup)             :: this
        character(len=*), intent(in) :: name

        this%name = name
    end subroutine

    function trimSlashes(this,str,nstr) result(trimmedStr)
        implicit none

        class(DataGroup)                 :: this

        character(len=*),    intent(in)  :: str
        integer,             intent(out) :: nstr

        character(:), allocatable :: trimmedStr

        integer :: i

        trimmedStr = trim(str)

        nstr = len(trim(trimmedStr))

        ! remove all initial '/' characters
        do while (trimmedStr(1:1) == '/')
            trimmedStr = trimmedStr(2:nstr)
            nstr = nstr - 1
        end do

        ! Remove any trailing '/' as well. Make sure we don't go into negative indexes.
        do i=0,nstr-1
            if (trimmedStr(nstr-i:nstr-i) /= '/') then
                exit
            end if
        end do

        ! if there were any trailing '/'
        if (i > 0) then
            ! adjust the string
            trimmedStr = trimmedStr(1:nstr-i)
        end if

        nstr = len(trim(trimmedStr))
    end function

    recursive function getGroup(this,locationString,ierr) result(group)
        implicit none

        class(DataGroup), target          :: this

        character(len=*),    intent(in)   :: locationString
        integer,             intent(out)  :: ierr

        class(DataGroup), pointer :: group

        character(:), allocatable :: substr1, substr2, substr3

        integer :: nstr, i

        class(*), pointer :: optr

        class(DataGroup), pointer :: tmpgroup => null()

        substr1 = this%trimSlashes(locationString,nstr)

        if (nstr == 0) then
            ! found the requested group! it's us.
            group => this
            ierr = 0
        else
            ! now let's see if we have the group being requested

            ! the subgroup name might have a '/' after it, or it might not.
            ! in the case that it doesn't, the subgroup is just the remaining substring
            substr2 = substr1(1:nstr)
            substr3 = ''

            do i=1,nstr
                ! in the case there is a '/', break up the group name by the first '/' encountered
                if (substr1(i:i) == '/') then
                    substr2 = substr1(1:i-1)
                    substr3 = substr1(i:nstr)
                    exit ! the loop
                end if
            end do

            ierr = 2 ! group is not found (by default, changed below in getGroup)

            if (this%subgroups%hasKey(substr2)) then
                optr => this%subgroups%get(substr2)

                select type(optr)
                    class is (DataGroup)
                        ! cast down
                        tmpgroup => optr
                    class default
                        call error('Unknown class in subgroups dictionary')
                end select

                ! go further down the rabbit hole. We'll be fine (probably).
                group => tmpgroup%getGroup(substr3,ierr)
            end if
        end if
    end function

    function getGroupByName(this,varName) result(group)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: varName

        class(DataGroup),    pointer      :: group

        class(*),            pointer      :: optr      => null()

        if (this%variables%hasKey(varName)) then
            optr => this%subgroups%get(varName)

            select type(optr)
                class is (DataGroup)
                    ! cast down
                    group => optr
                class default
                    call error('Unknown class in variable dictionary')
            end select
        else
            group => null()
        end if
    end function

    function getOwnerGroup(this,locationString,groupFound,childString) result(ownerGroup)
        implicit none

        class(DataGroup),          target         :: this

        character(len=*),          intent(in)   :: locationString
        logical,                   intent(out)  :: groupFound
        character(:), allocatable, intent(out)  :: childString

        class(DataGroup), pointer :: ownerGroup

        character(:), allocatable :: substr1, substr2, substr3

        integer :: nstr, i, ierr2

        class(*), pointer :: optr

        logical :: subgroupFound

        substr1 = this%trimSlashes(locationString,nstr)

        subgroupFound  = .false.

        ! now we're going to get the substring before the last '/', which is the owner group
        do i=nstr-1,1,-1
            ! in the case there is a '/', break up the group name by the last '/' encountered
            if (substr1(i:i) == '/') then
                substr2       = substr1(1:i-1)
                childString   = substr1(i+1:nstr)
                subgroupFound = .true.
                exit ! the loop
            end if
        end do

        if (subgroupFound) then
            ! if there is a subgroup, we'll get it
            ownerGroup => this%getGroup(substr2,ierr2)
            if (.not. associated(ownerGroup)) then
                childString = ''
                groupFound = .false.
            else
                groupFound = .true.
            end if
        else
            ! there is no subgroup, so this group is the owner
            ownerGroup  => this
            groupFound  = .true.
            childString =  substr1
        end if
    end function

    function getVariable(this,locationString,ierr) result(var)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: locationString
        integer,             intent(out)  :: ierr

        class(DataVariable), pointer      :: var

        class(DataVariable), pointer      :: tmpvar    => null()
        class(DataGroup),    pointer      :: subgroup  => null()
        class(*),            pointer      :: optr      => null()

        logical             :: groupFound
        character(:), allocatable :: childString
        integer             :: nstr

        character(:), allocatable :: substr1

        subgroup => this%getOwnerGroup(locationString,groupFound,childString)

        if (.not. groupFound) then
            ierr = 2
            var => null()
        else
            substr1 = this%trimSlashes(childString,nstr)

            if (nstr == 0) then
                var => null()
                ierr = 2 ! couldn't find the requested variable. Sorry.
            else
                ! now let's see if the group has the variable being requested
                var => subgroup%getVariableByName(substr1)
                if (associated(var)) then
                    ierr = 0
                else
                    ierr = 2
                end if
            end if
        end if
    end function

    function getVariableByName(this,varName) result(var)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: varName

        class(DataVariable), pointer      :: var

        class(*),            pointer      :: optr      => null()

        if (this%variables%hasKey(varName)) then
            optr => this%variables%get(varName)

            select type(optr)
                class is (DataVariable)
                    ! cast down
                    var => optr
                class default
                    call error('Unknown class in variable dictionary')
            end select
        else
            var => null()
        end if
    end function

    subroutine getVariableNames(this,keyNames)
        implicit none

        class(DataGroup)                 :: this

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        call this%variables%keys(keyNames)
    end subroutine


    function getDataArray(this,locationString,ierr) result(dArray)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: locationString
        integer,             intent(out)  :: ierr

        class(DataArray),    pointer      :: dArray

        class(DataVariable), pointer      :: var

        var => this%getVariable(locationString,ierr)
        dArray => var%getDataArray()
    end function

    function getDataArrayByName(this,varName) result(dArray)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: varName
        class(DataArray),    pointer      :: dArray

        class(DataVariable), pointer      :: var

        var => this%getVariableByName(varName)
        dArray => var%getDataArray()
    end function

    function getMirroredArray(this,locationString,ierr) result(mArray)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),     intent(in)  :: locationString
        integer,              intent(out) :: ierr

        class(DataArray),     pointer     :: dArray
        class(MirroredArray), pointer     :: mArray

        dArray => this%getDataArray(locationString,ierr)

        select type(dArray)
            class is (MirroredArray)
                ! cast down
                mArray => dArray
            class default
                call error('Requested a mirrored array for a non-mirrored variable: ' // &
                    &locationString)
        end select
    end function

    function getMirroredArrayByName(this,varName) result(dArray)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),     intent(in)  :: varName

        class(DataArray),     pointer     :: dArray
        class(MirroredArray), pointer     :: mArray

        dArray => this%getDataArrayByName(varName)

        select type(dArray)
            class is (MirroredArray)
                ! cast down
                mArray => dArray
            class default
                call error('Requested a mirrored array for a non-mirrored variable: ' // &
                    &varName)
        end select
    end function

    function getAttribute(this,locationString,isGroupAttribute,ierr) result(attr)
        implicit none

        class(DataGroup)                  :: this
        character(len=*),    intent(in)   :: locationString
        logical,             intent(in)   :: isGroupAttribute
        integer,             intent(out)  :: ierr

        class(DataAttribute), pointer     :: attr

        class(DataAttribute), pointer     :: tmpattr  => null()

        class(DataVariable),  pointer     :: var      => null()
        class(DataGroup),     pointer     :: subgroup => null()
        class(*),             pointer     :: optr     => null()

        logical             :: groupFound
        character(:), allocatable :: childString
        character(:), allocatable :: substr1, substr2, substr3

        integer :: i, nstr
        integer :: attrInd

        logical :: hasAttribute

        ! attribute strings are a little bit special. The attribute name has to start with an '@'.
        ! if there is anything other than exactly one '@' symbol in the string, an error will be generated.

        substr1 = this%trimSlashes(locationString,nstr)

        hasAttribute = .false.

        ! assume no error for now
        ierr = 0

        do i=1,nstr
            if (substr1(i:i) == '@') then
                if (hasAttribute) then
                    ! already had an attribute! Error.
                    ierr = 3
                else
                    attrInd = i
                    hasAttribute = .true.
                end if
            end if
        end do

        if (ierr == 0 .and. hasAttribute) then
            ! the location of the attribute
            if (attrInd > 1) then
                substr2 = substr1(1:attrInd-1)
            else
                ! attribute will be on the group
                substr2 = ''
            end if

            if (attrInd+1 <= nstr) then
                ! get the attribute name
                substr3 = substr1(attrInd+1:nstr)

                ! make sure there are no slashes in this substring
                do i=attrInd+1,nstr
                    if (substr1(i:i) == '/') then
                        ierr = 3
                        exit ! the loop
                    end if
                end do
            else
                ! empty attribute name
                ierr = 3
            end if

            if (ierr == 0) then
                ! we now have a valid attribute name. The attribute could be on a group or on a variable.

                ierr = 2         ! assume not found until changed
                attr => null()

                if (isGroupAttribute) then

                    ! on the group then we will look through the subgroup's list of attributes
                    subGroup => this%getGroup(locationString,ierr)
                    if (ierr .eq. 0 .and. associated(subGroup)) then
                        attr => subGroup%getAttributeByName(substr3)

                        if (associated(attr)) then
                            ierr = 0
                        else
                            ierr = 2
                        end if
                    end if
                else
                    var => this%getVariable(locationString,ierr)

                    if (ierr .eq. 0 .and. associated(var)) then
                        ! it's up to the variable now
                        attr => var%getAttribute(substr3,ierr)
                    end if
                end if
            end if
        else
            ! no attribute in the string. Error.
            attr => null()
            ierr = 3
        end if
    end function

    function getAttributeByName(this,attrName) result(attr)
        implicit none

        class(DataGroup)                 :: this

        character(len=*),     intent(in) :: attrName

        class(DataAttribute), pointer    :: attr

        class(*), pointer :: optr => null()

        if (this%attributes%hasKey(attrName)) then
            optr => this%attributes%get(attrName)

            select type(optr)
                class is (DataAttribute)
                    ! cast down
                    attr => optr
                class default
                    call error('Unknown class in variable dictionary')
            end select
        else
            attr => null()
        end if
    end function

    function getDimension(this,locationString,ierr) result(dimv)
        implicit none

        class(DataGroup)                         :: this

        character(len=*),           intent(in)   :: locationString
        integer,                    intent(out)  :: ierr

        class(DataDimension),       pointer      :: dimv

        class(DataDimension),       pointer :: tmpdim   => null()
        class(DataGroup),           pointer :: subgroup => null()
        class(*),                   pointer :: optr     => null()

        logical             :: groupFound
        character(:), allocatable :: childString

        character(:), allocatable :: substr1, substr2, substr3
        integer :: i, nstr

        subgroup => this%getOwnerGroup(locationString,groupFound,childString)

        if (.not. groupFound) then
            ierr = 2
            dimv => null()
        else
            substr1 = this%trimSlashes(childString,nstr)

            if (nstr == 0) then
                ierr = 2 ! couldn't find the requested dimension. Sorry.
            else
                ! now let's see if the group has the dimension being requested

                dimv => subgroup%getDimensionByName(substr1)

                if (associated(dimv)) then
                    ierr = 0
                else
                    ierr = 2 ! dimension is not found
                end if
            end if
        end if
    end function

    function getDimensionByName(this,dimName) result(ddim)
        implicit none

        class(DataGroup)                 :: this

        character(len=*),     intent(in) :: dimName

        class(DataDimension),       pointer    :: ddim

        class(*), pointer :: optr => null()

        if (this%dims%hasKey(dimName)) then
            optr => this%dims%get(dimName)

            select type(optr)
                class is (DataDimension)
                    ! cast down
                    ddim => optr
                class default
                    call error('Unknown class in variable dictionary')
            end select
        else
            ddim => null()
        end if
    end function

    subroutine getDimensionNames(this,keyNames)
        implicit none

        class(DataGroup)                 :: this

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        call this%dims%keys(keyNames)
    end subroutine

    function getGlobalDimCount(this,dimName) result(ndim)
        implicit none

        class(DataGroup)                 :: this
        character(len=*),     intent(in) :: dimName

        class(DataDimension),       pointer :: ddim

        integer :: ndim, ierr

        ddim => this%getDimension(dimName,ierr)

        if (associated(ddim) .and. ierr == 0) then
            ndim = ddim%getGlobalCount()
        else
            ndim = 0
        end if
    end function

    function getNDimensions(this) result(ndim)
        implicit none

        class(DataGroup)                  :: this
        integer                           :: ndim

        ndim = this%dims%numkeys()
    end function

    function getEnum(this,locationString,ierr) result(enumv)
        implicit none

        class(DataGroup)                  :: this

        character(len=*),    intent(in)   :: locationString
        integer,             intent(out)  :: ierr

        class(DataEnum),     pointer      :: enumv

        class(DataVariable), pointer      :: var       => null()

        class(DataVariable), pointer      :: tmpvar    => null()
        class(DataGroup),    pointer      :: subgroup  => null()
        class(*),            pointer      :: optr      => null()

        logical                   :: groupFound
        character(:), allocatable :: childString
        integer                   :: nstr

        character(:), allocatable :: substr1

        subgroup => this%getOwnerGroup(locationString,groupFound,childString)

        enumv => null() ! assume the enum isn't found unless set below
        ierr  = 2

        if (groupFound) then
            substr1 = this%trimSlashes(childString,nstr)

            if (nstr .ne. 0) then
                ! now let's see if the group has the variable being requested

                if (subgroup%enums%hasKey(substr1)) then
                    optr => subgroup%enums%get(substr1)

                    select type(optr)
                        class is (DataEnum)
                            ! cast down
                            enumv => optr
                        class default
                            call error('Unknown class in enum dictionary')
                    end select
                    ierr = 0
                end if
            end if
        end if
    end function

    subroutine addGroup(this,subgroup)
        implicit none

        class(DataGroup)          :: this
        class(DataGroup), pointer :: subgroup

        class(*), pointer :: optr

        if (this%subgroups%haskey(subgroup%getName())) then
            call error('Cannot add group. The group ' // this%getName() // &
                & ' already has a subgroup of name ' // trim(subgroup%getName()))
        end if

        optr => subgroup

        call this%subgroups%add(subgroup%getName(),optr)
    end subroutine

    function addGroupByName(this,groupName) result(subgroup)
        implicit none

        class(DataGroup)             :: this
        character(len=*), intent(in) :: groupName

        class(DataGroup), pointer :: subgroup

        allocate(subgroup)

        call subgroup%dataGroupConstructor(groupName)

        call this%addGroup(subgroup)
    end function

    subroutine deleteVariable(this,var)
        implicit none

        class(DataGroup)             :: this
        class(DataVariable), pointer :: var

        logical :: deleted

        deleted = this%variables%delete(var%getName())

        deallocate(var)
    end subroutine

    subroutine addVariablePointer(this,var)
        implicit none

        class(DataGroup)             :: this
        class(DataVariable), pointer :: var

        class(DataShape),     pointer :: dshape
        class(DataDimension), pointer :: ddim

        class(*), pointer :: optr

        integer :: i, ndim

        if (this%variables%haskey(var%getName())) then
            call error('Cannot add variable. The data group ' // trim(this%getName()) // &
                & ' already has a variable of name ' // trim(var%getName()))
        end if

        ! now loop through the dimensions and see if any need to be added
        dshape => var%getDataShape()

        ndim = dshape%getNDimensions()

        do i=1,ndim
            ddim => dshape%getDimensionNumber(i)
            if (.not. this%dims%haskey(ddim%getName())) then
                call this%addDimension(ddim%clone())
            end if
        end do

        optr => var
        call this%variables%add(var%getName(),optr)
    end subroutine

    subroutine addAttribute(this,attr)
        implicit none

        class(DataGroup)              :: this
        class(DataAttribute), pointer :: attr

        class(*), pointer :: optr

        if (this%attributes%haskey(attr%getName())) then
            call error('Cannot add attribute. The data group ' // trim(this%getName()) // &
                & ' already has a attribute of name ' // trim(attr%getName()))
        end if

        optr => attr

        call this%attributes%add(attr%getName(),optr)
    end subroutine

    subroutine addDimension(this,dimv)
        implicit none

        class(DataGroup)                    :: this
        class(DataDimension),       pointer :: dimv

        class(*), pointer :: optr

        if (this%dims%haskey(dimv%getName())) then
            call error('Cannot add dimension. The group ' // this%getName() // &
                & ' already has a dimension of name ' // trim(dimv%getName()))
        end if

        optr => dimv
        call this%dims%add(dimv%getName(),optr) !,shouldDeallocate=.not. dimv%isShared())
    end subroutine

    function addDimensionByName(this,name,globalSize,globalCount,globalStart,&
        localCount,localStart,stagger,compareToDim) result(dimv)

        implicit none

        class(DataGroup)                       :: this

        character(len=*),           intent(in) :: name
        integer,                    intent(in) :: globalSize
        integer,          optional, intent(in) :: globalCount
        integer,          optional, intent(in) :: globalStart
        integer,          optional, intent(in) :: localCount
        integer,          optional, intent(in) :: localStart
        integer,          optional, intent(in) :: stagger
        character(len=*), optional, intent(in) :: compareToDim

        class(DataDimension),       pointer   :: dimv

        allocate(dimv)

        call dimv%dataDimensionConstructor(name,globalSize,globalCount,globalStart,&
            &stagger=stagger,compareToDim=compareToDim)

        call this%addDimension(dimv)
    end function

    subroutine addEnum(this,enumPtr)
        implicit none

        class(DataGroup)         :: this
        class(DataEnum), pointer :: enumPtr

        class(*), pointer :: optr

        if (this%enums%haskey(enumPtr%getName())) then
            call error('Cannot add enum. The group ' // this%getName() // &
                & ' already has an enum of name ' // trim(enumPtr%getName()))
        end if

        optr => enumPtr
        call this%enums%add(enumPtr%getName(),optr)
    end subroutine

    function cloneGroup(this,copyData) result(dgptr)
        implicit none

        class(DataGroup), target :: this
        logical, intent(in) :: copyData
        class(DataGroup), pointer :: dgptr, sdgptr
        class(*), pointer :: optr

        allocate(dgptr)

        call dgptr%dataGroupConstructor(this%name)

        sdgptr => this

        call dgptr%copy(sdgptr,copyData)
    end function

    recursive subroutine copy(this,copyFrom,copyData)

        class(DataGroup)             :: this

        class(DataGroup), pointer    :: copyFrom
        logical,          intent(in) :: copyData

        class(*), pointer :: optr

        class(DataGroup),           pointer :: sdgptr
        class(DataVariable),        pointer :: dvptr
        class(DataAttribute),       pointer :: daptr
        class(DataDimension),       pointer :: ddptr
        class(DataEnum),            pointer :: deptr

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        integer :: i

        ! now copy all the subgroups
        call copyFrom%subgroups%keys(keyNames)

        do i=1,size(keyNames)
            optr => copyFrom%subgroups%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataGroup)
                        ! cast down
                        sdgPtr => optr
                    class default
                        call error('A non DataGroup class was found in the subgroups dict.')
                end select

                call this%addGroup(sdgptr%cloneGroup(copyData))
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)

        ! now copy all the variables
        call copyFrom%variables%keys(keyNames)

        do i=1,size(keyNames)
            optr => copyFrom%variables%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataVariable)
                        ! cast down
                        dvPtr => optr
                    class default
                        call error('A non DataVariable class was found in the variables dict.')
                end select

                call this%addVariablePointer(dvptr%clone(copyData))
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)

        ! now copy all the attributes
        call copyFrom%attributes%keys(keyNames)

        do i=1,size(keyNames)
            optr => copyFrom%attributes%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataAttribute)
                        ! cast down
                        daPtr => optr
                    class default
                        call error('A non DataAttribute class was found in the attributes dict.')
                end select

                call this%addAttribute(daPtr%clone())
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)

        ! now copy all the dimensions
        call copyFrom%dims%keys(keynames)

        do i=1,size(keyNames)
            optr => copyFrom%dims%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataDimension)
                        ! cast down
                        ddptr => optr
                    class default
                        call error('Unknown class in dimension list')
                end select
            else
                call error('Dimension list is in an inconsistent state')
            end if

            call this%addDimension(ddptr%clone())
        end do

        ! now copy all the enums
        call copyFrom%enums%keys(keyNames)

        do i=1,size(keyNames)
            optr => copyFrom%enums%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataEnum)
                        ! cast down
                        dePtr => optr
                    class default
                        call error('A non DataEnum class was found in the enums dict.')
                end select

                call this%addEnum(dePtr%clone())
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)
    end subroutine

    recursive subroutine zeroAll(this)
        implicit none

        class(DataGroup) :: this

        class(DataGroup),    pointer :: dgptr
        class(DataVariable), pointer :: dvptr
        class(*),            pointer :: optr

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        integer :: i

        ! now zero all the subgroups
        call this%subgroups%keys(keyNames)

        do i=1,size(keyNames)
            optr => this%subgroups%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataGroup)
                        ! cast down
                        dgPtr => optr
                    class default
                        call error('A non DataGroup class was found in the subgroups dict.')
                end select

                call dgPtr%zeroAll()
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)

        ! now zero all the variables
        call this%variables%keys(keyNames)

        do i=1,size(keyNames)
            optr => this%variables%get(keyNames(i))

            if (associated(optr)) then
                select type(optr)
                    class is (DataVariable)
                        ! cast down
                        dvPtr => optr
                    class default
                        call error('A non DataVariable class was found in the variables dict.')
                end select

                call dvPtr%zeroAll()
            else
                write(msgstr,*) 'Could not find key ',trim(keyNames(i)),'in dictionary.'
                call error(msgstr)
            end if
        end do

        deallocate(keyNames)

    end subroutine

    function addVariable_logical1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function


    function addVariable_byte4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_byte7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_short7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_int7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_long7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_real7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_dble7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dshp.incl'
    end function

    function addVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function


    function addVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_byte7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_short7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_int7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_long7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_real7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_dble7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_dims.incl'
    end function

    function addVariable_logical1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble1d_extent(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble2d_extent(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble3d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function


    function addVariable_byte4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble4d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble5d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble6d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_logical7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_byte7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                       pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_short7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                        pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_int7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                        pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_long7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                        pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_real7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addVariable_dble7d_extent(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataExtent),                      pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addVariable_extent.incl'
    end function

    function addLocalVariable_logical1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble1d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble2d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble3d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function


    function addLocalVariable_byte4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble4d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble5d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble6d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_byte7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),           optional, pointer     :: pinfo

        class(DataVariable),                     pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_short7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo
        class(DataVariable),                      pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_int7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo
        class(DataVariable),                      pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_long7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_real7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_dble7d(this,variableName,dataptr,dShape, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dshp.incl'
    end function

    function addLocalVariable_logical1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble1d_dims(this,variableName,dataptr,dim1, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble2d_dims(this,variableName,dataptr,dim1,dim2, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo
        class(DataVariable),                    pointer     :: var
        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble3d_dims(this,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function


    function addLocalVariable_byte4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble4d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble5d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble6d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_logical7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_byte7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),           optional, pointer     :: pinfo

        class(DataVariable),                     pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_short7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo

        class(DataVariable),                      pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_int7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_long7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),            optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_real7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addLocalVariable_dble7d_dims(this,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData,pinfo) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),          optional, pointer     :: pinfo

        class(DataVariable),                    pointer     :: var

        include 'dataGroup_addLocalVariable_dims.incl'
    end function

    function addMirroredVariable_logical1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function


    function addMirroredVariable_byte4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_byte7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_short7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_int7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_long7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo

        class(MirroredVariable),                  pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_real7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_dble7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dshp.incl'
    end function

    function addMirroredVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function


    function addMirroredVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:), pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_logical7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_byte7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_short7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_int7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_long7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_real7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    function addMirroredVariable_dble7d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6,dim7, &
            & initVal,copyData) result(mvar)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo

        class(MirroredVariable),                pointer     :: mvar

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addMirroredVariable_dims.incl'
    end function

    ! add a variable by name with the requested data type and shape
    function addDistributedVariable_dtype(this,name,dtypeNum,dShape,pinfo) result(var)
        implicit none

        class(DataGroup)                         :: this

        character(len=*),             intent(in) :: name
        integer,                      intent(in) :: dtypeNum
        class(DataShape),             pointer    :: dShape
        class(ParallelInfo),          pointer    :: pinfo

        class(DataVariable), pointer :: var

        class(DataShape),    pointer :: dShapeNew

        allocate(var)

        call var%dataVariableConstructor(name,dtypeNum,dShape,.true.,.true.)

        call this%addVariablePointer(var)
    end function

    function addDistributedVariable_logical1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble1d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble2d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble3d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function


    function addDistributedVariable_byte4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble4d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble5d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:),        pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble6d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_byte7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                        pointer     :: dShape
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
        class(ParallelInfo),                     pointer     :: pinfo
        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_short7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_int7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_long7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                         pointer     :: dShape
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_real7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_dble7d(this,pinfo,variableName,dataptr,dShape, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataShape),                       pointer     :: dShape
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dshp.incl'
    end function

    function addDistributedVariable_logical1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:),                  pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble1d_dims(this,pinfo,variableName,dataptr,dim1, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:),             pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:),                pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble2d_dims(this,pinfo,variableName,dataptr,dim1,dim2, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:),           pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:),              pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:),        pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble3d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:),         pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:),            pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function


    function addDistributedVariable_byte4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble4d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:),       pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:),          pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:),    pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble5d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:),     pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:), pointer            :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:),  pointer      :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),               optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble6d_dims(this,pinfo,variableName,dataptr,dim1,dim2,dim3,dim4,dim5,dim6, &
            & initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:),   pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_logical7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        logical, dimension(:,:,:,:,:,:,:),      pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        logical,                      optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        logical, dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_byte7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                     :: this
        character(len=*),                        intent(in)  :: variableName

        integer(int8), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                    pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int8),                 optional, intent(in)  :: initVal
        logical,                       optional, intent(in)  :: copyData
         class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                     pointer     :: var

        integer(int8), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_short7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int16), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int16),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int16), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_int7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int32),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_long7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                      :: this
        character(len=*),                         intent(in)  :: variableName

        integer(int64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                     pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        integer(int64),                 optional, intent(in)  :: initVal
        logical,                        optional, intent(in)  :: copyData
        class(ParallelInfo),                      pointer     :: pinfo
        class(DataVariable),                      pointer     :: var

        integer(int64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_real7d_dims(this,pinfo,variableName,dataptr,&
            &dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real32), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real32),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real32), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    function addDistributedVariable_dble7d_dims(this,pinfo,variableName,dataptr,&
            & dim1,dim2,dim3,dim4,dim5,dim6,dim7,initVal,copyData) result(var)
        implicit none

        class(DataGroup)                                    :: this
        character(len=*),                       intent(in)  :: variableName

        real(real64), dimension(:,:,:,:,:,:,:), pointer     :: dataptr
        class(DataDimension),                   pointer     :: dim1,dim2,dim3,dim4,dim5,dim6,dim7
        real(real64),                 optional, intent(in)  :: initVal
        logical,                      optional, intent(in)  :: copyData
        class(ParallelInfo),                    pointer     :: pinfo
        class(DataVariable),                    pointer     :: var

        real(real64), dimension(:), pointer :: dptr1d

        include 'dataGroup_addDistributedVariable_dims.incl'
    end function

    subroutine synchronize(this,pinfo)
        implicit none

        class(DataGroup)             :: this

        class(ParallelInfo), pointer :: pinfo

        integer :: i, j

        character(len=DICT_KEY_LENGTH), dimension(:), allocatable :: keyNames

        class(DataVariable), pointer :: var
        class(DataGroup),    pointer :: grp
        class(*),            pointer :: optr

        call this%variables%keys(keyNames)

        ! loop through the list of variables
        do i=1,size(keyNames)
            optr => this%variables%get(trim(keyNames(i)))

            if (associated(optr)) then
                select type(optr)
                    class is (DataVariable)
                        ! cast down
                        var => optr
                    class default
                        call error('Unknown class in variable dictionary')
                end select
            else
                var => null()
            end if

            if (.not. associated(var)) then
                call error('Could not find the variable ' // trim(keyNames(i)))
            end if

            call var%synchronize(pinfo)
        end do

        deallocate(keyNames)

        call this%subgroups%keys(keyNames)

        ! loop through the list of groups
        do i=1,size(keyNames)
            grp => this%getGroupByName(keyNames(i))

            call grp%synchronize(pinfo)
        end do
    end subroutine
end module
