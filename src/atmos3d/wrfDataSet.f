module wrfDataSet_mod

    use parallelInfo_mod

    use iso_fortran_env

    use atmos3DDataSet_mod

    use dataGrid_mod
    use dataVariable_mod
    use dataAttribute_mod
    use dataArrayReader_mod

    use netcdfDataArrayReader_mod

    implicit none

    private

    ! the east/west dimension
    character(*), parameter, public :: TIME_DIM_NAME   = 'Time'

    type, abstract, extends(Atmos3DDataSet), public :: WrfDataSet
        contains
            !procedure :: writeToFile

            procedure :: loadWrfDataSet

            generic   :: wrfDataSetConstructor => &
                wrfDataSetConstructor_file, &
                wrfDataSetConstructor_class

            procedure, private :: wrfDataSetConstructor_file
            procedure, private :: wrfDataSetConstructor_class

            procedure :: wrfDataSetDestructor
    end type

    ! commented out because this is an abstract class... a real OO language would allow this though...
    !interface WrfDataSet
    !    procedure wrfDataSetConstructor ! allow generic instantiation
    !end interface

    contains

    subroutine wrfDataSetConstructor_file(this,inputFile)

        implicit none

        class(WrfDataSet)           :: this

        character(*),    intent(in) :: inputFile

        class(DataArrayReader),       pointer :: reader
        class(NetcdfDataArrayReader), pointer :: ncReader

        allocate(ncReader)
        call ncReader%netcdfDataArrayReaderConstructor(inputFile)
        reader => ncReader

        call this%wrfDataSetConstructor(reader)
    end subroutine

    subroutine wrfDataSetConstructor_class(this,reader)
        implicit none

        class(WrfDataSet)                  :: this

        class(DataArrayReader), pointer :: reader

        call this%atmos3dDataSetConstructor(reader)
    end subroutine

    subroutine loadWrfDataSet(this,pinfo,grid,&!westEastDim,southNorthDim,bottomTopDim,    &
        & pLevelVar,tVar,qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,    &
        & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar, &
        & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)

        implicit none

        class(WrfDataSet)      :: this

        class(ParallelInfo),    pointer     :: pinfo

        class(DataGrid),        pointer     :: grid

        class(DataVariable),    pointer     :: pLevelVar
        class(DataVariable),    pointer     :: tVar
        class(DataVariable),    pointer     :: qvaporVar
        class(DataVariable),    pointer     :: uVar
        class(DataVariable),    pointer     :: vVar
        class(DataVariable),    pointer     :: wVar
        class(DataVariable),    pointer     :: cldfraVar
        class(DataVariable),    pointer     :: cwmVar
        class(DataVariable),    pointer     :: qcloudVar
        class(DataVariable),    pointer     :: qiceVar
        class(DataVariable),    pointer     :: qrainVar
        class(DataVariable),    pointer     :: qsnowVar
        class(DataVariable),    pointer     :: qgraupVar
        class(DataVariable),    pointer     :: qhailVar
        class(DataVariable),    pointer     :: dzVar
        class(DataVariable),    pointer     :: u10Var
        class(DataVariable),    pointer     :: v10Var
        class(DataVariable),    pointer     :: tSurfVar
        class(DataVariable),    pointer     :: soilTypeVar
        class(DataVariable),    pointer     :: vegTypeVar
        class(DataVariable),    pointer     :: luIndexVar
        class(DataVariable),    pointer     :: sfcZVar
        class(DataVariable),    pointer     :: latVar
        class(DataVariable),    pointer     :: lonVar
        class(DataVariable),    pointer     :: pSfcVar
        class(DataVariable),    pointer     :: t2Var
        class(DataVariable),    pointer     :: q2Var

        call this%loadAtmos3dDataSet(pinfo,grid,&!westEastDim,southNorthDim,bottomTopDim,   &
            & pLevelVar,tVar, qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,  &
            & qrainVar,qsnowVar,qgraupVar,qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,&
            & vegTypeVar,luIndexVar,sfcZVar,latVar,lonVar,psfcVar,t2Var,q2Var)
    end subroutine

!    subroutine loadWrfDataSet_all(this,pinfo,westEastDim,southNorthDim,bottomTopDim,pLevelVar,tVar, &
!        & qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,qrainVar,qsnowVar,qgraupVar,  &
!        & qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,vegTypeVar,luIndexVar,sfcZVar,latVar,   &
!        & lonVar,psfcVar,t2Var,q2Var,dx,dy,map_proj,truelat1,truelat2,stdlon,pole_lat,pole_lon,     &
!        & latinc,loninc,cen_lat,cen_lon,num_land_cat)
!
!        class(WrfDataSet)      :: this
!
!        class(ParallelInfo),    pointer     :: pinfo
!
!        class(DataVariable),    pointer     :: pLevelVar
!        class(DataVariable),    pointer     :: tVar
!        class(DataVariable),    pointer     :: qvaporVar
!        class(DataVariable),    pointer     :: uVar
!        class(DataVariable),    pointer     :: vVar
!        class(DataVariable),    pointer     :: wVar
!        class(DataVariable),    pointer     :: cldfraVar
!        class(DataVariable),    pointer     :: cwmVar
!        class(DataVariable),    pointer     :: qcloudVar
!        class(DataVariable),    pointer     :: qiceVar
!        class(DataVariable),    pointer     :: qrainVar
!        class(DataVariable),    pointer     :: qsnowVar
!        class(DataVariable),    pointer     :: qgraupVar
!        class(DataVariable),    pointer     :: qhailVar
!        class(DataVariable),    pointer     :: dzVar
!        class(DataVariable),    pointer     :: u10Var
!        class(DataVariable),    pointer     :: v10Var
!        class(DataVariable),    pointer     :: tSurfVar
!        class(DataVariable),    pointer     :: soilTypeVar
!        class(DataVariable),    pointer     :: vegTypeVar
!        class(DataVariable),    pointer     :: luIndexVar
!        class(DataVariable),    pointer     :: sfcZVar
!        class(DataVariable),    pointer     :: latVar
!        class(DataVariable),    pointer     :: lonVar
!        class(DataVariable),    pointer     :: pSfcVar
!        class(DataVariable),    pointer     :: t2Var
!        class(DataVariable),    pointer     :: q2Var
!
!        real(real64),           intent(in)  :: dx
!        real(real64),           intent(in)  :: dy
!        integer,                intent(in)  :: map_proj
!        real(real64),           intent(in)  :: truelat1
!        real(real64),           intent(in)  :: truelat2
!        real(real64),           intent(in)  :: stdlon
!        real(real64),           intent(in)  :: pole_lat
!        real(real64),           intent(in)  :: pole_lon
!        real(real64),           intent(in)  :: latinc
!        real(real64),           intent(in)  :: loninc
!        real(real64),           intent(in)  :: cen_lat
!        real(real64),           intent(in)  :: cen_lon
!        integer,                intent(in)  :: num_land_cat
!
!        call this%loadAtmos3dDataSet(pinfo,westEastDim,southNorthDim,bottomTopDim,pLevelVar,tVar,      &
!            & qvaporVar,uVar,vVar,wVar,cldfraVar,cwmVar,qcloudVar,qiceVar,qrainVar,qsnowVar,qgraupVar, &
!            & qhailVar,dzVar,u10Var,v10Var,tSurfVar,soilTypeVar,vegTypeVar,luIndexVar,sfcZVar,latVar,  &
!            & lonVar,psfcVar,t2Var,q2Var,dx,dy,map_proj,truelat1,truelat2,stdlon,pole_lat,pole_lon,    &
!            & latinc,loninc,cen_lat,cen_lon,num_land_cat)
!
!    end subroutine

    subroutine wrfDataSetDestructor(this)
        implicit none

        class(WrfDataSet) :: this

        call this%atmos3dDataSetDestructor()

        ! all of the dimensions and variables will be destroyed in the data set / group
    end subroutine

!    subroutine writeToFile(this, fileName)
!        implicit none
!
!        class(WrfDataSet) :: this
!        character(len=*), intent(in) :: fileName
!
!        integer :: i, j, k, si, ei, nx, ny, nz, ncid, varid, dimId
!
!        integer,dimension(nf90_max_var_dims)::dimIDs
!
!        logical :: fileExists
!
!        real(8), dimension(:,:,:), allocatable :: data3D
!
!        inquire(file=fileName,exist=fileExists)
!
!        nx = this%getNX()
!        ny = this%getNY()
!
!        allocate(data3D(nx,ny,maxval(this%fieldEndInds - this%fieldStartInds)+1))
!
!        if (fileExists) then
!            call ncCheck( nf90_open(fileName, NF90_WRITE, ncid) )
!        else
!            call ncCheck( nf90_create(fileName, NF90_NOCLOBBER, ncid) )
!
!            call ncCheck( nf90_def_dim(ncid, 'NX', nx, dimIDs(1)) )
!            call ncCheck( nf90_def_dim(ncid, 'NY', ny, dimIDs(2)) )
!            call ncCheck( nf90_def_dim(ncid, 'NZ', this%nz, dimIDs(3)) )
!            call ncCheck( nf90_def_dim(ncid, 'NZS', this%nz+1, dimIDs(4)) )
!
!            do i=1,size(this%fieldNames)
!                si = this%fieldStartInds(i)
!                ei = this%fieldEndInds(i)
!                nz = ei - si + 1
!
!                if (nz .eq. 1) then
!                    call ncCheck( nf90_def_var(ncid, this%fieldNames(i), NF90_DOUBLE, dimIDs(1:2), varid) )
!                else if (nz .eq. this%nz) then
!                    call ncCheck( nf90_def_var(ncid, this%fieldNames(i), NF90_DOUBLE, dimIDs(1:3), varid) )
!                else if (nz .eq. this%nz+1) then
!                    call ncCheck( nf90_def_var(ncid, this%fieldNames(i), NF90_DOUBLE, dimIDs((/1,2,4/)), varid) )
!                else
!                    write(msgstr,'(A,A,A,I3)') 'Could not create the variable ',trim(this%fieldNames(i)), ' with size ',nz
!                    call error(msgstr)
!                end if
!            enddo
!        end if
!
!        do i=1,size(this%fieldNames)
!            si = this%fieldStartInds(i)
!            ei = this%fieldEndInds(i)
!            nz = ei - si + 1
!
!            call ncCheck( nf90_inq_varid(ncid, this%fieldNames(i), varid) )
!
!            do j=1,nx
!                do k=1,ny
!                    data3D(j,k,1:nz) = this%data(si:ei,j,k)
!                end do
!            end do
!
!            if (nz .ne. 1) then
!                call ncCheck(nf90_put_var(ncid, varid, data3D(:,:,1:nz), (/ 1, 1, 1/), (/nx, ny, nz/)))
!            else
!                call ncCheck(nf90_put_var(ncid, varid, data3D(:,:,1), (/ 1, 1/), (/nx, ny/)))
!            endif
!        enddo
!
!        call ncCheck( nf90_close(ncid) )
!
!        deallocate(data3D)
!    end subroutine
end module
