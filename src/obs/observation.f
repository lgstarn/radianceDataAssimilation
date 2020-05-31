module observation_mod

    use parallelInfo_mod

    use iso_fortran_env

    use platformInfo_mod

    use dataSet_mod
    use dataType_mod
    use dataArray_mod
    use dataExtent_mod
    use dataGroup_mod
    use dataShape_mod
    use dataVariable_mod
    use dataDimension_mod
    use dataArrayReader_mod
    use dataArrayWriter_mod

    use obsQCCodes_mod

    use mpiUtils_mod
    use asciiUtils_mod

    implicit none

    private

    character(*), public, parameter :: OBS_ERR_VAR_NAME  = 'obsErr'
    character(*), public, parameter :: OBS_DATA_VAR_NAME = 'obsData'
    character(*), public, parameter :: OBS_LOCI_VAR_NAME = 'obsLoci'
    character(*), public, parameter :: AUX_DATA_VAR_NAME = 'auxData'
    character(*), public, parameter :: QC_CODES_VAR_NAME = 'qcCodes'
    character(*), public, parameter :: OWNER_VAR_NAME    = 'owner'
    character(*), public, parameter :: CONTRIB_VAR_NAME  = 'contrib'

    character(*), public, parameter :: MOBS_DIM_NAME     = 'obs_per_loc'
    character(*), public, parameter :: NOBS_DIM_NAME     = 'obs_locs'
    character(*), public, parameter :: NLOCI_DIM_NAME    = 'loci_dims'
    character(*), public, parameter :: NAUX_DIM_NAME     = 'aux_vars'

    type, extends(DataSet), public :: Observation
        !private
            class(PlatformInfo),  pointer, public :: platform     => null() ! information regarding the observing platform; required

            !class(DataExtent),    pointer :: mobsExtent  => null()
            !class(DataExtent),    pointer :: nobsExtent  => null()
            !class(DataExtent),    pointer :: nlociExtent => null()
            !class(DataExtent),    pointer :: nauxExtent  => null()

            !class(DataVariable),  pointer :: obsDataVar => null()   ! double precision, mobs x nobs
            !class(DataVariable),  pointer :: obsLociVar => null()   ! double precision, ndim x nobs
            !class(DataVariable),  pointer :: auxDataVar => null()   ! double precision, naux x nobs
            !class(DataVariable),  pointer :: qcCodesVar => null()   ! integer, nobs x 1
            !class(DataVariable),  pointer :: ownerVar   => null()   ! integer, nobs x 1
            !class(DataVariable),  pointer :: contribVar => null()   ! logical, nobs x 1

            real(real64),         pointer :: obsErr(:)

        contains
            procedure :: observationConstructor

            procedure :: getNObsDim
            procedure :: getMObsDim
            procedure :: getNLociDim
            procedure :: getNauxDim

            procedure :: getNObsExtent
            procedure :: getMObsExtent
            procedure :: getNLociExtent
            procedure :: getNauxExtent

            procedure :: getPlatform
            procedure :: passesQC
            procedure :: getNPassesQC
            procedure :: getQCCode
            procedure :: setQCCode
            procedure :: getSensorName

            procedure :: getObsData
            procedure :: getObsLoci
            procedure :: getQcCodes
            procedure :: getAuxData
            procedure :: getObsOwners
            procedure :: getContributes

            procedure :: getObsDataVar
            procedure :: getObsLociVar
            procedure :: getQcCodesVar
            procedure :: getAuxDataVar
            procedure :: getObsOwnersVar
            procedure :: getContributesVar

            !procedure :: getObservation
            !procedure :: getObsLocus
            !procedure :: getAuxDatum
            !procedure :: getObsQcCode
            !procedure :: getObsOwner

            procedure :: setObsOwner
            procedure :: setContributes
            procedure :: doesContribute
            procedure :: setContributionRange
            procedure :: setObsOwnershipRange

            procedure :: getObservationError

            procedure :: loadObservation
            procedure :: clone
            procedure :: cloneObs
            procedure :: copySubset
            procedure :: cloneObsSubset
            procedure :: cloneSubset
            procedure :: writeObsToFile
            procedure :: loadObsFromFile

            final :: observationDestructor ! clean up all allocated variables
    end type

    contains

    subroutine observationConstructor(this,platform,obserr,reader)
        implicit none

        class(Observation) :: this

        class(PlatformInfo),               pointer    :: platform
        real(real64),           optional,  intent(in) :: obserr(:)
        class(DataArrayReader), optional,  pointer    :: reader

        class(DataVariable), pointer :: obsErrVar

        call this%dataSetConstructor(reader)

        this%platform => platform
        if (present(obserr)) then
            allocate(this%obsErr(size(obsErr)))
            this%obsErr(:) = obsErr(:)
        else
            this%obsErr => null()
        end if
    end subroutine

    subroutine observationDestructor(this)
        implicit none

        type(Observation)  :: this

        if (associated(this%obsErr)) then
            deallocate(this%obserr)
        end if
    end subroutine

    subroutine loadObservation(this,pinfo,obsDataVar,obsLociVar,auxDataVar,qcCodesVar,&
        ownerVar,contribVar)

        implicit none

        class(Observation)                      :: this

        class(ParallelInfo),            pointer :: pinfo
        class(DataVariable),            pointer :: obsDataVar
        class(DataVariable),            pointer :: obsLociVar
        class(DataVariable),  optional, pointer :: auxDataVar
        class(DataVariable),  optional, pointer :: qcCodesVar
        class(DataVariable),  optional, pointer :: ownerVar
        class(DataVariable),  optional, pointer :: contribVar

        integer, pointer :: qcCodes(:,:)
        integer, pointer :: owner(:)
        integer, pointer :: contrib(:)

        class(DataVariable),  pointer :: qcCodesVar_new
        class(DataVariable),  pointer :: ownerVar_new
        class(DataVariable),  pointer :: contribVar_new

        class(DataExtent),  pointer :: mobsExtent
        class(DataExtent),  pointer :: nobsExtent

        call this%addVariablePointer(obsDataVar,checkName=OBS_DATA_VAR_NAME,&
            checkType=DOUBLE_TYPE_NUM,checkDim=2)
        call this%addVariablePointer(obsLociVar,checkName=OBS_LOCI_VAR_NAME,&
            checkType=DOUBLE_TYPE_NUM,checkDim=2)

        if (present(auxDataVar)) then
            if (associated(auxDataVar)) then
                call this%addVariablePointer(auxDataVar,checkName=AUX_DATA_VAR_NAME,&
                    checkType=DOUBLE_TYPE_NUM,checkDim=2)
            end if
        end if

        if (present(qcCodesVar)) then
            call this%addVariablePointer(qcCodesVar,checkName=QC_CODES_VAR_NAME,&
                checkType=INT_TYPE_NUM,checkDim=2)
        else
            mobsExtent => obsDataVar%getExtentNumber(1)
            nobsExtent => obsDataVar%getExtentNumber(2)

            qcCodesVar_new => this%addVariable(pinfo, QC_CODES_VAR_NAME, qcCodes, &
                & mobsExtent, nobsExtent, initVal=QC_NOERR)
        end if

        if (present(ownerVar)) then
            call this%addVariablePointer(ownerVar, checkName=OWNER_VAR_NAME, &
                checkType=INT_TYPE_NUM,checkDim=1)
        else
            nobsExtent => obsDataVar%getExtentNumber(2)

            ownerVar_new => this%addVariable(pinfo, OWNER_VAR_NAME, owner,   &
                & nobsExtent, initVal=-1)
        end if

        if (present(contribVar)) then
            call this%addVariablePointer(contribVar, checkName=CONTRIB_VAR_NAME, &
                checkType=INT_TYPE_NUM,checkDim=1)
        else
            nobsExtent => obsDataVar%getExtentNumber(2)

            contribVar_new => this%addVariable(pinfo, CONTRIB_VAR_NAME, contrib,  &
                & nobsExtent, initVal=-1)
        end if
    end subroutine

!    function getNObs(this) result(nobs)
!        implicit none
!
!        class(Observation)  :: this
!        integer             :: nobs
!
!        nobs = this%nobsDim%getGlobalCount()
!    end function
!
!    function getMObs(this) result(mobs)
!        implicit none
!
!        class(Observation)  :: this
!        integer             :: mobs
!
!        mobs = this%mobsDim%getGlobalCount()
!    end function
!
!    function getNDim(this) result(ndim)
!        implicit none
!
!        class(Observation)  :: this
!        integer             :: ndim
!
!        ndim = this%nlociDim%getGlobalCount()
!    end function
!
!    function getNAux(this) result(naux)
!        implicit none
!
!        class(Observation)  :: this
!        integer             :: naux
!
!        naux = this%nauxDim%getGlobalCount()
!    end function

    function getNObsDim(this) result(nobsDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: nobsDim

        nobsDim => this%getDimensionByName(NOBS_DIM_NAME)
    end function

    function getMObsDim(this) result(mobsDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: mobsDim

        mobsDim => this%getDimensionByName(MOBS_DIM_NAME)
    end function

    function getNLociDim(this) result(nlociDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: nlociDim

        nlociDim => this%getDimensionByName(NLOCI_DIM_NAME)
    end function

    function getNAuxDim(this) result(nauxDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: nauxDim

        if (this%hasDimension(NAUX_DIM_NAME)) then
            nauxDim => this%getDimensionByName(NAUX_DIM_NAME)
        else
            nauxDim => null()
        end if
    end function

    function getNObsExtent(this) result(nobsExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nobsExtent

        class(DataVariable), pointer :: obsDataVar

        obsDataVar => this%getObsDataVar()

        nobsExtent => obsDataVar%getExtentNumber(2)
    end function

    function getMObsExtent(this) result(mobsExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: mobsExtent

        class(DataVariable), pointer :: obsDataVar

        obsDataVar => this%getObsDataVar()

        mobsExtent => obsDataVar%getExtentNumber(1)
    end function

    function getNLociExtent(this) result(nlociExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nlociExtent

        class(DataVariable), pointer :: obsLociVar

        obsLociVar => this%getObsLociVar()

        nlociExtent => obsLociVar%getExtentNumber(1)
    end function

    function getNAuxExtent(this) result(nauxExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nauxExtent

        class(DataVariable), pointer :: auxDataVar

        auxDataVar => this%getAuxDataVar()

        if (associated(auxDataVar)) then
            nauxExtent => auxDataVar%getExtentNumber(1)
        else
            nauxExtent => nulL()
        end if
    end function

    function getPlatform(this) result(platform)
        implicit none

        class(Observation)  :: this
        class(PlatformInfo), pointer :: platform

        platform => this%platform
    end function

    function passesQC(this, m, n, mend) result(pass)
        implicit none

        class(Observation)            :: this
        integer, intent(in)           :: m
        integer, intent(in)           :: n
        integer, intent(in), optional :: mend

        logical :: pass

        integer, pointer :: qcCodes(:,:)

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getQcCodesVar()

        call qcCodesVar%getArray(qcCodes)

        if (m < 1 .or. m > size(qcCodes,1) .or. &
            n < 1 .or. n > size(qcCodes,2)) then

            write (msgstr,*) 'Invalid call to passesQC: (',m,',',n,&
                & ') requested but QC size is (',size(qcCodes,1),'/', size(qcCodes,2),')'
        end if

        if (present(mend)) then
            pass = all(qcCodes(m:mend,n) <= QC_OBEVAL)
        else
            pass =     qcCodes(m,n)      <= QC_OBEVAL
        end if
    end function

    function getNPassesQC(this,mstart,mend) result(npass)
        implicit none

        class(Observation)            :: this
        integer, intent(in), optional :: mstart
        integer, intent(in), optional :: mend
        integer                       :: npass

        integer :: n, ms

        integer, pointer :: qcCodes(:,:)

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getQcCodesVar()

        call qcCodesVar%getArray(qcCodes)

        if (size(qcCodes,1) == 0 .or. size(qcCodes,2) == 0) then
            npass = 0
            return
        end if

        if (present(mstart)) then
            ms = mstart
        else
            ms = 1
        end if

        npass = 0

        do n=1,size(qcCodes,2)
            ! mend is optional here and in passesQC, so just pass it along
            if (this%passesQC(ms,n,mend)) then
                npass = npass + 1
            end if
        end do
    end function

    subroutine setQCCode(this, m, n, qcCode, mend)
        implicit none

        class(Observation)            :: this
        integer, intent(in)           :: m
        integer, intent(in)           :: n
        integer, intent(in)           :: qcCode
        integer, intent(in), optional :: mend

        integer :: i, me
        integer :: nch, ndim

        integer :: ierr

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getQcCodesVar()

        if (present(mend)) then
            me = mend
        else
            me = m
        end if

        dArray => qcCodesVar%getDataArray()

        call dArray%addChangeRange(reshape([[m,me],[n,n]],[2,2],order=[2,1]),&
            qcCode,2)
    end subroutine

    function getQCCode(this, m, n) result(qcCode)
        implicit none

        class(Observation)   :: this
        integer, intent(in)  :: m
        integer, intent(in)  :: n
        integer              :: qcCode

        integer, pointer     :: qcCodes(:,:)

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getQcCodesVar()

        call qcCodesVar%getArray(qcCodes)

        qcCode = qcCodes(m,n)
    end function

    function getSensorName(this) result(name)
        implicit none

        class(Observation)  :: this
        character (len=256) :: name

        name = this%platform%sensorName
    end function

    function getObsData(this) result(obsData)
        implicit none

        class(Observation)  :: this

        real(real64), dimension(:,:), pointer :: obsData

        class(DataArray),    pointer :: dArray

        class(DataVariable), pointer :: obsDataVar

        obsDataVar => this%getObsDataVar()

        dArray => obsDataVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(obsData)
        else
            obsData => null()
        end if
    end function

    function getObsLoci(this) result(obsLoci)
        implicit none

        class(Observation)  :: this

        real(real64), dimension(:,:), pointer :: obsLoci

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: obsLociVar

        obsLociVar => this%getObsLociVar()

        dArray => obsLociVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(obsLoci)
        else
            obsLoci => null()
        end if
    end function

    function getQcCodes(this) result(qcCodes)
        implicit none

        class(Observation)  :: this

        integer, dimension(:,:), pointer :: qcCodes

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getQcCodesVar()

        dArray => qcCodesVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(qcCodes)
        else
            qcCodes => null()
        end if
    end function

    function getAuxData(this) result(auxData)
        implicit none

        class(Observation)  :: this

        real(real64), dimension(:,:), pointer :: auxData

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: auxDataVar

        auxDataVar => this%getAuxDataVar()

        dArray => auxDataVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(auxData)
        else
            auxData => null()
        end if
    end function

    function getObservationError(this) result(obsErr)

        implicit none

        class(Observation)  :: this

        real(real64), pointer :: obsErr(:)

        obsErr => this%obsErr
    end function

    function getObsOwners(this) result(obsOwners)
        implicit none

        class(Observation)  :: this

        integer, dimension(:), pointer :: obsOwners

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: ownersVar

        ownersVar => this%getObsOwnersVar()

        dArray => ownersVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(obsOwners)
        else
            obsOwners => null()
        end if
    end function

    function getContributes(this) result(contribs)
        implicit none

        class(Observation)  :: this

        logical, dimension(:), pointer :: contribs

        class(DataArray), pointer :: dArray

        class(DataVariable), pointer :: contribVar

        contribVar => this%getContributesVar()

        dArray => contribVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(contribs)
        else
            contribs => null()
        end if
    end function

    function getObsDataVar(this) result(obsDataVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsDataVar

        obsDataVar => this%getVariableByName(OBS_DATA_VAR_NAME)
    end function

    function getObsLociVar(this) result(obsLociVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsLociVar

        obsLociVar => this%getVariableByName(OBS_LOCI_VAR_NAME)
    end function

    function getQcCodesVar(this) result(qcCodesVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%getVariableByName(QC_CODES_VAR_NAME)
    end function

    function getAuxDataVar(this) result(auxDataVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: auxDataVar

        if (this%hasVariable(AUX_DATA_VAR_NAME)) then
            auxDataVar => this%getVariableByName(AUX_DATA_VAR_NAME)
        else
            auxDataVar => null()
        end if
    end function

    function getObsOwnersVar(this) result(obsOwnersVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsOwnersVar

        obsOwnersVar => this%getVariableByName(OWNER_VAR_NAME)
    end function

    function getContributesVar(this) result(contribVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: contribVar

        contribVar => this%getVariableByName(CONTRIB_VAR_NAME)
    end function

!    function getObservation(this,n) result(obsptr)
!        implicit none
!
!        class(Observation)  :: this
!        integer, intent(in) :: n
!        real(real64), pointer    :: obsptr(:)
!
!        class(DataArray),     pointer :: dArray
!
!        class(DataVariable),  pointer :: obsDataVar
!
!        real(real64),         pointer :: obsData(:,:)
!
!        obsDataVar => this%getObsDataVar()
!
!        call obsDataVar%getArray(obsData)
!
!        obsptr => obsData(:,n)
!    end function
!
!    function getAuxDatum(this,n) result(auxptr)
!        implicit none
!
!        class(Observation)  :: this
!
!        integer, intent(in) :: n
!
!        real(real64), pointer    :: auxptr(:)
!
!        real(real64), pointer    :: auxData(:,:)
!
!        class(DataVariable), pointer :: auxDataVar
!
!        auxDataVar => this%getAuxDataVar()
!
!        if (associated(auxDataVar)) then
!            call auxDataVar%getArray(auxData)
!
!            auxptr => auxData(:,n)
!        else
!            auxptr => null()
!        end if
!    end function
!
!    function getObsLocus(this,n) result(obsLociPtr)
!        implicit none
!
!        class(Observation)             :: this
!        integer, intent(in)            :: n
!
!        real(real64), dimension(:), pointer   :: obsLociPtr
!
!        real(real64), pointer    :: obsLoci(:,:)
!
!        class(DataVariable), pointer :: obsLociVar
!
!        obsLociVar => this%getObsLociVar()
!
!        call obsLociVar%getArray(obsLoci)
!
!        obsLociPtr => obsLoci(:,n)
!    end function
!
!    function getObsQcCode(this,n) result(obsQcCodesPtr)
!        implicit none
!
!        class(Observation)             :: this
!        integer, intent(in)            :: n
!
!        integer, dimension(:),   pointer :: obsQcCodesPtr
!
!        integer, dimension(:,:), pointer :: obsQcCodes
!
!        call this%obsLociVar%getArray(obsQcCodes)
!
!        obsQcCodesPtr => obsQcCodes(:,n)
!    end function

    subroutine setObsOwner(this,n,owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n
        integer, intent(in)            :: owner

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray(OWNER_VAR_NAME)

        call dArray%addChangeRange(reshape([n,n],[1,2]),owner,1)
    end subroutine

    function getObsOwner(this,n) result(owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n

        integer                        :: owner

        integer, dimension(:), pointer :: owners

        owners => this%getObsOwners()

        owner = owners(n)
    end function

    subroutine setContributes(this,n,contrib)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n

        logical, intent(in), optional  :: contrib

        integer, dimension(:,:), allocatable :: ind

        logical :: cval

        class(DataArray), pointer :: dArray

        if (present(contrib)) then
            cval = contrib
        else
            cval = .true.
        end if

        dArray => this%getDataArray(CONTRIB_VAR_NAME)

        call dArray%addChangeRange(reshape([n,n],[1,2]),cval,1)
    end subroutine

    function doesContribute(this,n) result(contrib)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n

        logical                        :: contrib

        logical, dimension(:), pointer :: contribs

        contribs => this%getContributes()

        contrib = contribs(n)
    end function

    subroutine setContributionRange(this,startInd,endInd,contrib)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: startInd
        integer, intent(in)            :: endInd

        logical, intent(in), optional  :: contrib

        logical :: cval

        class(DataArray), pointer :: dArray

        if (present(contrib)) then
            cval = contrib
        else
            cval = .true.
        end if

        dArray => this%getDataArray(CONTRIB_VAR_NAME)

        call dArray%addChangeRange(reshape([startInd,endInd],[1,2]),cval,1)
    end subroutine

    subroutine setObsOwnershipRange(this,startInd,endInd,owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: startInd
        integer, intent(in)            :: endInd

        integer, intent(in)            :: owner

        class(DataArray), pointer :: dArray

        dArray => this%getDataArray(OWNER_VAR_NAME)

        call dArray%addChangeRange(reshape([startInd,endInd],[1,2]),owner,1)
    end subroutine

    subroutine writeObsToFile(this,pinfo,writer)

        class(Observation)        :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayWriter), pointer    :: writer

        call writer%writeDimension(pinfo, this%getNObsDim())
        call writer%writeDimension(pinfo, this%getMObsDim())
        call writer%writeDimension(pinfo, this%getNLociDim())

        if (associated(this%getNAuxDim())) then
            call writer%writeDimension(pinfo, this%getNAuxDim())
        end if

        call writer%writeVariable(pinfo, this%getObsDataVar())
        call writer%writeVariable(pinfo, this%getObsLociVar())

        if (associated(this%getAuxDataVar())) then
            call writer%writeVariable(pinfo, this%getAuxDataVar())
        end if

        call writer%writeVariable(pinfo, this%getQcCodesVar())

        !call writer%writeVariable(pinfo, this%getObsOwnersVar())
        !call writer%writeVariable(pinfo, this%getContributesVar())
    end subroutine

    subroutine loadObsFromFile(this,pinfo)

        class(Observation)            :: this

        class(ParallelInfo),  pointer :: pinfo

        class(DataDimension), pointer :: nobsDim
        class(DataDimension), pointer :: mobsDim
        class(DataDimension), pointer :: nLociDim
        class(DataDimension), pointer :: nauxDim

        class(DataVariable), pointer :: obsDataVar
        class(DataVariable), pointer :: obsLociVar
        class(DataVariable), pointer :: auxDataVar
        class(DataVariable), pointer :: qcCodesVar
        !class(DataVariable), pointer :: ownerVar
        !class(DataVariable), pointer :: contribVar

        mobsDim  => this%loadDimensionFromVariable(pinfo,MOBS_DIM_NAME, 1,&
            OBS_DATA_VAR_NAME)
        nobsDim  => this%loadDimensionFromVariable(pinfo,NOBS_DIM_NAME, 2,&
            OBS_DATA_VAR_NAME)
        nlociDim => this%loadDimensionFromVariable(pinfo,NLOCI_DIM_NAME,1,&
            OBS_LOCI_VAR_NAME)
        nauxDim  => this%loadDimensionFromVariable(pinfo,NAUX_DIM_NAME, 1,&
            AUX_DATA_VAR_NAME,required=.false.)

        obsDataVar => this%loadVariable(pinfo,DOUBLE_TYPE_NUM,OBS_DATA_VAR_NAME,&
            mobsDim,nobsDim)
        obsLociVar => this%loadVariable(pinfo,DOUBLE_TYPE_NUM,OBS_LOCI_VAR_NAME,&
            nlociDim,nobsDim)

        if (associated(nauxDim)) then
            auxDataVar => this%loadVariable(pinfo,DOUBLE_TYPE_NUM,AUX_DATA_VAR_NAME,&
                nauxDim,nobsDim)
        end if

        qcCodesVar => this%loadVariable(pinfo,INT_TYPE_NUM,QC_CODES_VAR_NAME,mobsDim,nobsDim)
        !ownerVar   => this%loadVariable(pinfo,INT_TYPE_NUM,OWNER_VAR_NAME,nobsDim)
        !contribVar => this%loadVariable(pinfo,LOGICAL_TYPE_NUM,CONTRIB_VAR_NAME,nobsDim)
    end subroutine

    function cloneObs(this,shallow,copyData) result(obsPtr)
        implicit none

        class(Observation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),          pointer :: dsPtr
        class(DataGroup),        pointer :: dgPtr
        class(Observation),      pointer :: obsPtr

        logical :: isShallow
        logical :: doCopy

        allocate(obsPtr)
        call obsPtr%observationConstructor(this%platform,this%obserr,&
            this%getDataArrayReader())

        ! FIXME: should call load here to make sure nothing gets missed
        ! call obsPtr%loadObservation(...)

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
            dsPtr => obsPtr
            dgPtr => this
            call dsPtr%copy(dgPtr,doCopy)
        end if
    end function

    function clone(this,shallow,copyData) result(dsPtr)
        implicit none

        class(Observation), target  :: this

        logical, optional, intent(in)     :: shallow
        logical, optional, intent(in)     :: copyData

        class(DataSet),          pointer :: dsPtr
        class(Observation),      pointer :: obsPtr

        obsPtr => this%cloneObs(shallow,copyData)
        dsPtr => obsPtr
    end function

    subroutine copySubset(this,copyTo,localInds)
        implicit none

        class(Observation)              :: this
        class(Observation), pointer     :: copyTo
        integer,            intent(in) :: localInds(:)

        real(real64), pointer :: obsData(:,:)
        real(real64), pointer :: thisObsData(:,:)
        real(real64), pointer :: obsLoci(:,:)
        real(real64), pointer :: thisObsLoci(:,:)
        real(real64), pointer :: auxData(:,:)
        real(real64), pointer :: thisAuxData(:,:)

        class(DataVariable), pointer :: var1
        class(DataVariable), pointer :: var2

        integer :: i

        if (.not. associated(copyTo)) then
            call error('The subset to copyTo was not associated')
        end if

        var1 => copyTo%getObsDataVar()

        call var1%getArray(obsData)

        if (size(obsData,2) /= size(localInds)) then
            call error('The sizes in cloneSubset did not match: ' // int2str(size(obsData,2)) // &
                & ' vs. ' // int2str(size(localInds)))
        end if

        var2 => this%getObsDataVar()

        call var2%getArray(thisObsData)

        do i=1,size(localInds)
            obsData(:,i) = thisObsData(:,localInds(i))
        end do

        var1 => copyTo%getObsLociVar()
        var2 => this%getObsLociVar()

        if (associated(var1) .and. associated(var2)) then
            call var1%getArray(obsLoci)
            call var2%getArray(thisObsLoci)

            do i=1,size(localInds)
                obsLoci(:,i) = thisObsLoci(:,localInds(i))
            end do
        end if

        var1 => copyTo%getAuxDataVar()
        var2 => this%getAuxDataVar()

        if (associated(var1) .and. associated(var2)) then
            call var1%getArray(auxData)
            call var2%getArray(thisAuxData)

            do i=1,size(localInds)
                auxData(:,i) = thisAuxData(:,localInds(i))
            end do
        end if
    end subroutine

    function cloneObsSubset(this,pinfo,nobsNewExtent,localInds) result(optr)
        implicit none

        class(Observation)              :: this

        class(ParallelInfo), pointer    :: pinfo
        class(DataExtent),   pointer    :: nobsNewExtent
        integer,             intent(in) :: localInds(:)

        class(DataSet),      pointer    :: dsPtr
        class(Observation),  pointer    :: optr

        class(DataVariable), pointer    :: obsDataVar
        class(DataVariable), pointer    :: obsLociVar
        class(DataVariable), pointer    :: auxDataVar
        class(DataVariable), pointer    :: qcCodesVar
        class(DataVariable), pointer    :: ownerVar
        class(DataVariable), pointer    :: contribVar

        real(real64), pointer :: obsData(:,:)
        real(real64), pointer :: obsLoci(:,:)
        real(real64), pointer :: auxData(:,:)
        integer,      pointer :: qcCodes(:,:)
        integer,      pointer :: owner(:)
        integer,      pointer :: contrib(:)

        class(DataExtent),   pointer    :: mobsNewExtent
        class(DataExtent),   pointer    :: nLociNewExtent

        dsPtr => this%clone(shallow=.true.,copyData=.false.)

        select type (dsPtr)
            class is (Observation)
                optr => dsPtr
            class default
                call error('Unknown observation type')
        end select

        mobsNewExtent  => this%getMObsExtent()
        nLociNewExtent => this%getNLociExtent()

        call optr%addDimension(mobsNewExtent%getDimension())
        call optr%addDimension(nobsNewExtent%getDimension())
        call optr%addDimension(nLociNewExtent%getDimension())

        if (associated(this%getNAuxDim())) then
            call optr%addDimension(this%getNAuxDim())
        end if

        obsDataVar => optr%addVariable(pinfo, OBS_DATA_VAR_NAME, obsData, &
            & this%getMobsExtent(), nobsNewExtent)

        obsLociVar =>  optr%addVariable(pinfo,OBS_LOCI_VAR_NAME, obsLoci, &
            & this%getNlociExtent(), nobsNewExtent)

        if (associated(this%getAuxDataVar())) then
            auxDataVar => optr%addVariable(pinfo,AUX_DATA_VAR_NAME, auxData, &
                & this%getMobsExtent(), nobsNewExtent)
        else
            auxDataVar => null()
        end if

        if (associated(this%getQcCodesVar())) then
            qcCodesVar => optr%addVariable(pinfo,QC_CODES_VAR_NAME, qcCodes, &
                & this%getMobsExtent(), nobsNewExtent)
        else
            qcCodesVar => null()
        end if

        ownerVar   => optr%addVariable(pinfo,OWNER_VAR_NAME, owner, &
            & nobsNewExtent)

        contribVar => optr%addVariable(pinfo,CONTRIB_VAR_NAME, contrib, &
            & nobsNewExtent)

        call optr%loadObservation(pinfo,obsDataVar,obsLociVar,auxDataVar,qcCodesVar,&
            ownerVar,contribVar)

        call optr%loadObservation(pinfo,optr%getObsDataVar(),optr%getObsLociVar(),&
            optr%getAuxDataVar(),optr%getQcCodesVar(),optr%getObsOwnersVar(),&
            optr%getContributesVar())

        call this%copySubset(optr,localInds)
    end function

    function cloneSubset(this,pinfo,nobsNewExtent,localInds) result(optr)
        implicit none

        class(Observation)              :: this

        class(ParallelInfo), pointer    :: pinfo
        class(DataExtent),   pointer    :: nobsNewExtent
        integer,             intent(in) :: localInds(:)

        class(Observation),  pointer    :: optr

        optr => this%cloneObsSubset(pinfo,nobsNewExtent,localInds)
    end function
end module
