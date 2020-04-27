module observation_mod

    use parallelInfo_mod

    use iso_fortran_env

    use platformInfo_mod

    use dataSet_mod
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
        private
            class(PlatformInfo), pointer, public :: platform     => null() ! information regarding the observing platform; required

            class(DataExtent),    pointer :: mobsExtent  => null()
            class(DataExtent),    pointer :: nobsExtent  => null()
            class(DataExtent),    pointer :: nlociExtent => null()
            class(DataExtent),    pointer :: nauxExtent  => null()

            class(DataVariable),  pointer :: obsDataVar => null()   ! double precision, mobs x nobs
            class(DataVariable),  pointer :: obsLociVar => null()   ! double precision, ndim x nobs
            class(DataVariable),  pointer :: auxDataVar => null()   ! double precision, naux x nobs
            class(DataVariable),  pointer :: qcCodesVar => null()   ! integer, nobs x 1
            class(DataVariable),  pointer :: ownerVar   => null()   ! integer, nobs x 1
            class(DataVariable),  pointer :: contribVar => null()   ! logical, nobs x 1

        contains
            procedure :: observationConstructor

            !procedure :: getNObs
            !procedure :: getMObs
            !procedure :: getNDim
            !procedure :: getNAux
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

            procedure :: getObsDataVar
            procedure :: getObsLociVar
            procedure :: getQcCodesVar
            procedure :: getAuxDataVar
            procedure :: getObsOwnersVar

            procedure :: getObservation
            procedure :: getObsLocus
            procedure :: getAuxDatum
            procedure :: getObsQcCode
            procedure :: getObsOwner

            procedure :: setObsOwner
            procedure :: setContributes
            procedure :: doesContribute
            procedure :: setContributionRange
            procedure :: setObsOwnershipRange

            procedure :: loadObservation
            procedure :: clone
            procedure :: cloneObs
            procedure :: copySubset
            procedure :: cloneSubset
            procedure :: writeObsToFile

            final :: observationDestructor ! clean up all allocated variables
    end type

    contains

    subroutine observationConstructor(this,platform,reader)
        implicit none

        class(Observation) :: this

        class(PlatformInfo),              pointer    :: platform
        class(DataArrayReader), optional, pointer    :: reader

        call this%dataSetConstructor(reader)

        this%platform => platform
    end subroutine

    subroutine observationDestructor(this)
        implicit none

        type(Observation)  :: this

        ! all of the variables will be deleted by the dataset / datagroup deconstructors
        ! also don't bother deleting the platform as it might be shared
        if (associated(this%mobsExtent)) then
            deallocate(this%mobsExtent)
        end if

        if (associated(this%nobsExtent)) then
            deallocate(this%nobsExtent)
        end if

        if (associated(this%nlociExtent)) then
            deallocate(this%nlociExtent)
        end if

        if (associated(this%nauxExtent)) then
            deallocate(this%nauxExtent)
        end if
    end subroutine

    subroutine loadObservation(this,pinfo,mobsExtent,nobsExtent,nlociExtent,nauxExtent,&
        & obsDataVar,obsLociVar,auxDataVar)

        implicit none

        class(Observation)            :: this

        class(ParallelInfo),  pointer :: pinfo
        class(DataExtent),    pointer :: mobsExtent
        class(DataExtent),    pointer :: nobsExtent
        class(DataExtent),    pointer :: nlociExtent
        class(DataExtent),    pointer :: nauxExtent

        class(DataVariable),  pointer :: obsDataVar
        class(DataVariable),  pointer :: obsLociVar
        class(DataVariable),  pointer :: auxDataVar

        integer, pointer :: qcCodes(:,:)
        integer, pointer :: owner(:)
        logical, pointer :: contrib(:)

        this%mobsExtent  => mobsExtent
        this%nobsExtent  => nobsExtent
        this%nlociExtent => nlociExtent
        this%nauxExtent  => nauxExtent

        this%obsDataVar => obsDataVar
        this%obsLociVar => obsLociVar
        this%auxDataVar => auxDataVar

        this%qcCodesVar => this%addVariable(pinfo, QC_CODES_VAR_NAME, qcCodes, &
            & mobsExtent, nobsExtent, initVal=QC_NOERR)

        this%ownerVar   => this%addVariable(pinfo, OWNER_VAR_NAME,    owner,   &
            & nobsExtent, initVal=-1)

        this%contribVar => this%addVariable(pinfo, CONTRIB_VAR_NAME, contrib,  &
            & nobsExtent, initVal=.false.)
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

        if (associated(this%nobsExtent)) then
            nobsDim => this%nobsExtent%getDimension()
        else
            nobsDim => null()
        end if
    end function

    function getMObsDim(this) result(mobsDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: mobsDim

        if (associated(this%mobsExtent)) then
            mobsDim => this%mobsExtent%getDimension()
        else
            mobsDim => null()
        end if
    end function

    function getNLociDim(this) result(nlociDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: nlociDim

        if (associated(this%nlociExtent)) then
            nlociDim => this%nlociExtent%getDimension()
        else
            nlociDim => null()
        end if
    end function

    function getNAuxDim(this) result(nauxDim)
        implicit none

        class(Observation)  :: this
        class(DataDimension), pointer :: nauxDim

        if (associated(this%nauxExtent)) then
            nauxDim => this%nauxExtent%getDimension()
        else
            nauxDim => null()
        end if
    end function

    function getNObsExtent(this) result(nobsExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nobsExtent

        nobsExtent => this%nobsExtent
    end function

    function getMObsExtent(this) result(mobsExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: mobsExtent

        mobsExtent => this%mobsExtent
    end function

    function getNLociExtent(this) result(nlociExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nlociExtent

        nlociExtent => this%nlociExtent
    end function

    function getNAuxExtent(this) result(nauxExtent)
        implicit none

        class(Observation)  :: this
        class(DataExtent), pointer :: nauxExtent

        nauxExtent => this%nauxExtent
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

        call this%qcCodesVar%getArray(qcCodes)

        if (present(mend)) then
            pass = all(qcCodes(m:mend,n) <= QC_OBEVAL)
        else
            pass =     qcCodes(m,n)      <= QC_OBEVAL
        end if
    end function

    function getNPassesQC(this,mstart,mend) result(npass)
        implicit none

        class(Observation)  :: this
        integer, intent(in), optional :: mstart
        integer, intent(in), optional :: mend
        integer             :: npass

        integer :: n, ms, me

        integer, pointer :: qcCodes(:,:)

        call this%qcCodesVar%getArray(qcCodes)

        if (present(mstart)) then
            ms = mstart
        else
            ms = 1
        end if

        npass = 0

        do n=1,size(qcCodes,2)
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

        if (present(mend)) then
            me = mend
        else
            me = m
        end if

        dArray => this%qcCodesVar%getDataArray()

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

        call this%qcCodesVar%getArray(qcCodes)

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

        dArray => this%obsDataVar%getDataArray()

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

        dArray => this%obsLociVar%getDataArray()

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

        dArray => this%qcCodesVar%getDataArray()

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

        dArray => this%auxDataVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(auxData)
        else
            auxData => null()
        end if
    end function


    function getObsOwners(this) result(obsOwners)
        implicit none

        class(Observation)  :: this

        integer, dimension(:), pointer :: obsOwners

        class(DataArray), pointer :: dArray

        dArray => this%ownerVar%getDataArray()

        if (associated(dArray)) then
            call dArray%getArray(obsOwners)
        else
            obsOwners => null()
        end if
    end function

    function getObsDataVar(this) result(obsDataVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsDataVar

        obsDataVar => this%obsDataVar
    end function

    function getObsLociVar(this) result(obsLociVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsLociVar

        obsLociVar => this%obsLociVar
    end function

    function getQcCodesVar(this) result(qcCodesVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: qcCodesVar

        qcCodesVar => this%qcCodesVar
    end function

    function getAuxDataVar(this) result(auxDataVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: auxDataVar

        auxDataVar => this%auxDataVar
    end function

    function getObsOwnersVar(this) result(obsOwnersVar)
        implicit none

        class(Observation)  :: this

        class(DataVariable), pointer :: obsOwnersVar

        obsOwnersVar => this%ownerVar
    end function

    function getObservation(this,n) result(obsptr)
        implicit none

        class(Observation)  :: this
        integer, intent(in) :: n
        real(real64), pointer    :: obsptr(:)

        class(DataVariable),  pointer :: var
        class(DataArray),     pointer :: dArray

        real(real64), pointer    :: obsData(:,:)

        call this%obsDataVar%getArray(obsData)

        obsptr => obsData(:,n)
    end function

    function getAuxDatum(this,n) result(auxptr)
        implicit none

        class(Observation)  :: this

        integer, intent(in) :: n

        real(real64), pointer    :: auxptr(:)

        real(real64), pointer    :: auxData(:,:)

        call this%auxDataVar%getArray(auxData)

        auxptr => auxData(:,n)
    end function

    function getObsLocus(this,n) result(obsLociPtr)
        implicit none

        class(Observation)             :: this
        integer, intent(in)            :: n

        real(real64), dimension(:), pointer   :: obsLociPtr

        real(real64), pointer    :: obsLoci(:,:)

        call this%obsLociVar%getArray(obsLoci)

        obsLociPtr => obsLoci(:,n)
    end function

    function getObsQcCode(this,n) result(obsQcCodesPtr)
        implicit none

        class(Observation)             :: this
        integer, intent(in)            :: n

        integer, dimension(:),   pointer :: obsQcCodesPtr

        integer, dimension(:,:), pointer :: obsQcCodes

        call this%obsLociVar%getArray(obsQcCodes)

        obsQcCodesPtr => obsQcCodes(:,n)
    end function

    subroutine setObsOwner(this,n,owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n
        integer, intent(in)            :: owner

        class(DataArray), pointer :: dArray
        integer :: ierr

        dArray => this%getDataArray(OWNER_VAR_NAME,ierr)

        call dArray%addChangeRange(reshape([n,n],[1,2]),owner,1)
    end subroutine

    function getObsOwner(this,n) result(owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n

        integer                        :: owner

        integer, dimension(:), pointer :: owners

        call this%ownerVar%getArray(owners)

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
        integer :: ierr

        if (present(contrib)) then
            cval = contrib
        else
            cval = .true.
        end if

        dArray => this%getDataArray(CONTRIB_VAR_NAME,ierr)

        call dArray%addChangeRange(reshape([n,n],[1,2]),cval,1)
    end subroutine

    function doesContribute(this,n) result(contrib)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: n

        logical                        :: contrib

        logical, dimension(:), pointer :: contribs

        call this%contribVar%getArray(contribs)

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
        integer :: ierr

        if (present(contrib)) then
            cval = contrib
        else
            cval = .true.
        end if

        dArray => this%getDataArray(CONTRIB_VAR_NAME,ierr)

        call dArray%addChangeRange(reshape([startInd,endInd],[1,2]),cval,1)
    end subroutine

    subroutine setObsOwnershipRange(this,startInd,endInd,owner)
        implicit none

        class(Observation)             :: this

        integer, intent(in)            :: startInd
        integer, intent(in)            :: endInd

        integer, intent(in)            :: owner

        class(DataArray), pointer :: dArray
        integer :: ierr

        dArray => this%getDataArray(OWNER_VAR_NAME,ierr)

        call dArray%addChangeRange(reshape([startInd,endInd],[1,2]),owner,1)
    end subroutine

    subroutine writeObsToFile(this,pinfo,writer)

        class(Observation)        :: this

        class(ParallelInfo),    pointer    :: pinfo
        class(DataArrayWriter), pointer    :: writer

        if (.not. associated(this%getNObsDim())) then
            call error('The NObs dimension must be associated to write the observation.')
        else
            call writer%writeDimension(pinfo, this%getNObsDim())
        end if

        if (.not. associated(this%getMObsDim())) then
            call error('The MObs dimension must be associated to write the observation.')
        else
            call writer%writeDimension(pinfo, this%getMObsDim())
        end if

        if (associated(this%getNLociDim())) then
            call writer%writeDimension(pinfo, this%getNLociDim())
        end if

        if (associated(this%getNAuxDim())) then
            call writer%writeDimension(pinfo, this%getNAuxDim())
        end if

        call writer%writeVariable(pinfo, this%getObsDataVar())

        if (associated(this%getObsLociVar())) then
            call writer%writeVariable(pinfo, this%getObsLociVar())
        end if

        if (associated(this%getAuxDataVar())) then
            call writer%writeVariable(pinfo, this%getAuxDataVar())
        end if
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
        call obsPtr%observationConstructor(this%platform,this%getDataArrayReader())

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

        integer :: i

        call copyTo%obsDataVar%getArray(obsData)

        if (size(obsData,2) /= size(localInds)) then
            call error('The sizes in cloneSubset did not match: ' // int2str(size(obsData,2)) // &
                & ' vs. ' // int2str(size(localInds)))
        end if

        call this%obsDataVar%getArray(thisObsData)

        do i=1,size(localInds)
            obsData(:,i) = thisObsData(:,localInds(i))
        end do

        if (associated(this%obsLociVar)) then
            call copyTo%obsLociVar%getArray(obsLoci)
            call this%obsLociVar%getArray(thisObsLoci)

            do i=1,size(localInds)
                obsLoci(:,i) = thisObsLoci(:,localInds(i))
            end do
        end if

        if (associated(this%auxDataVar)) then
            call copyTo%auxDataVar%getArray(auxData)
            call this%auxDataVar%getArray(thisAuxData)

            do i=1,size(localInds)
                auxData(:,i) = thisAuxData(:,localInds(i))
            end do
        end if
    end subroutine

    function cloneSubset(this,pinfo,nobsNewExtent,localInds) result(optr)
        implicit none

        class(Observation)              :: this

        class(ParallelInfo), pointer    :: pinfo
        class(DataExtent),   pointer    :: nobsNewExtent
        integer,             intent(in) :: localInds(:)

        class(DataSet),     pointer     :: dsPtr
        class(Observation), pointer     :: tptr
        class(Observation), pointer     :: optr

        class(DataVariable), pointer    :: obsDataVar
        class(DataVariable), pointer    :: obsLociVar
        class(DataVariable), pointer    :: auxDataVar

        real(real64), pointer :: obsData(:,:)
        real(real64), pointer :: obsLoci(:,:)
        real(real64), pointer :: auxData(:,:)

        dsptr => this%clone(shallow=.true.,copyData=.false.)

        select type (dsPtr)
            class is (Observation)
                optr => dsptr
            class default
                call error('Unknown satellite observation type')
        end select

        if (.not. associated(this%getMObsDim())) then
            call error('Cannot clone observation as mobs is not associated')
        end if

        call optr%addDimension(this%getMObsDim())
        call optr%addDimension(nobsNewExtent%getDimension())

        if (associated(this%getNLociDim())) then
            call optr%addDimension(this%getNLociDim())
        end if

        if (associated(this%getNAuxDim())) then
            call optr%addDimension(this%getNAuxDim())
        end if

        if (.not. associated(this%obsDataVar)) then
            call error('Cannot clone obsDataVar as it is not associated')
        end if

        obsDataVar => optr%addVariable(pinfo, OBS_DATA_VAR_NAME, obsData, &
            & this%mobsExtent, nobsNewExtent)

        if (size(obsData,2) /= size(localInds)) then
            call error('The sizes in cloneSubset did not match: ' // int2str(size(obsData,2)) // &
                & ' vs. ' // int2str(size(localInds)))
        end if

        if (associated(this%obsLociVar)) then
            obsLociVar =>  optr%addVariable(pinfo,OBS_LOCI_VAR_NAME, obsLoci, &
                & this%nlociExtent, nobsNewExtent)
        end if

        if (associated(this%auxDataVar)) then
            auxDataVar => optr%addVariable(pinfo,AUX_DATA_VAR_NAME, obsLoci, &
                & this%mobsExtent, nobsNewExtent)
        end if

        call optr%loadObservation(pinfo,this%mobsExtent,nobsNewExtent,&
            & this%nlociExtent,this%nauxExtent,obsDataVar,obsLociVar,auxDataVar)

        call this%copySubset(optr,localInds)
    end function
end module
