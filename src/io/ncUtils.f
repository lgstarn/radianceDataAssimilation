module ncUtils_mod
    use netcdf
    use iso_c_binding
    use mpiUtils_mod

    implicit none

    contains

    subroutine ncReadVariableDimensions4D(fileName,fieldName,nx,ny,nz,nt)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        integer, intent(out) :: nx,ny,nz,nt
        integer,dimension(nf90_max_var_dims) :: dimIDs,dimLengths
        integer :: j,ncid,varid,ndims

        call ncCheck( nf90_open(fileName, NF90_NOWRITE, ncid) )

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        dimIDs = 0

        call ncCheck(nf90_inquire_variable(ncid, varid, dimids=dimIDs))

        do j=1,nf90_max_var_dims
            if( DimIDs(j) .eq. 0) then
                exit
            end if

            ndims=j

            call ncCheck(nf90_inquire_dimension(ncid,dimIDs(j),len=dimLengths(j)))
        end do

        if (ndims .eq. 4) then
            nx=dimLengths(1)
            ny=dimLengths(2)
            nz=dimLengths(3)
            nt=dimLengths(4)
        elseif (ndims .eq. 3) then
            nx=dimLengths(1)
            ny=dimLengths(2)
            nz=1
            nt=dimLengths(3)
        elseif (ndims .eq. 2) then
            nx=dimLengths(1)
            ny=1
            nz=1
            nt=dimLengths(2)
        elseif (ndims .eq. 1) then
            nx=1
            ny=1
            nz=1
            nt=dimLengths(1)
        else
            write(msgstr,*) 'Unknown dimension length',ndims,dimLengths(1:ndims)
            call error(msgstr)
        endif

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariableDimensions3D(fileName,fieldName,nx,ny,nz)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        integer, intent(out) :: nx,ny,nz
        integer,dimension(nf90_max_var_dims) :: dimIDs,dimLengths
        integer :: j,ncid,varid,ndims

        call ncCheck( nf90_open(fileName, NF90_NOWRITE, ncid) )

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        dimIDs = 0

        call ncCheck(nf90_inquire_variable(ncid, varid, dimids=dimIDs))

        do j=1,nf90_max_var_dims
            if( DimIDs(j) .eq. 0) then
                exit
            end if

            ndims=j

            call ncCheck(nf90_inquire_dimension(ncid,dimIDs(j),len=dimLengths(j)))
        end do

        if (ndims .eq. 3) then
            nx=dimLengths(1)
            ny=dimLengths(2)
            nz=dimLengths(3)
        else
            write(msgstr,*) 'Unknown dimension length',ndims,dimLengths(1:ndims)
            call error(msgstr)
        endif

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariableDimensions2D(fileName,fieldName,nx,ny)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        integer, intent(out) :: nx,ny
        integer,dimension(nf90_max_var_dims) :: dimIDs,dimLengths
        integer :: j,ncid,varid,ndims

        call ncCheck( nf90_open(fileName, NF90_NOWRITE, ncid) )

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        dimIDs = 0

        call ncCheck(nf90_inquire_variable(ncid, varid, dimids=dimIDs))

        do j=1,nf90_max_var_dims
            if( DimIDs(j) .eq. 0) then
                exit
            end if

            ndims=j

            call ncCheck(nf90_inquire_dimension(ncid,dimIDs(j),len=dimLengths(j)))
        end do

        if (ndims .eq. 2) then
            nx=dimLengths(1)
            ny=dimLengths(2)
        else
            write(msgstr,*) 'Unknown dimension length',ndims,dimLengths(1:ndims)
            call error(msgstr)
        endif

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable4DAtTime(fileName,fieldName,data3D,nx,ny,nz,t,xstart,xend,ystart,yend,zstart,zend)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: data3D(:,:,:)
        integer, intent(in) :: nx,ny,nz,t
        integer, intent(in), optional :: xstart, xend, ystart, yend, zstart, zend
        integer :: ncid,varid,xs,xe,ys,ye,zs,ze,xl,yl,zl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        if (present(ystart)) then
            ys = ystart
        else
            ys = 1
        end if

        if (present(yend)) then
            ye = yend
        else
            ye = ny
        end if

        if (present(zstart)) then
            zs = zstart
        else
            zs = 1
        end if

        if (present(zend)) then
            ze = zend
        else
            ze = nz
        end if

        write(msgstr, '(A,A,A,A16,A,I6,A,I6,A,I6,A,I6)') 'Reading ',trim(fileName),' field ',&
            &trim(fieldName),' (',nx,',',ny,',',nz,') at time',t
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        xl = xe-xs+1
        yl = ye-ys+1
        zl = ze-zs+1

        if (nz .ne. 1) then
            call ncCheck(nf90_get_var(ncid, varid, data3D(1:xl,1:yl,1:zl), (/ xs, ys, zs, t/), &
                (/xl, yl, zl, 1/)))
        else
            call ncCheck(nf90_get_var(ncid, varid, data3D(1:xl,1:yl,1), (/ xs, ys, t/), &
                (/xl, yl, 1/)))
        endif

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable3DAtTime(fileName,fieldName,data2D,nx,ny,t,xstart,xend,ystart,yend)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: data2D(:,:)
        integer, intent(in) :: nx,ny,t
        integer, intent(in), optional :: xstart, xend, ystart, yend
        integer :: ncid,varid,xs,xe,ys,ye,xl,yl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        if (present(ystart)) then
            ys = ystart
        else
            ys = 1
        end if

        if (present(yend)) then
            ye = yend
        else
            ye = ny
        end if

        xl = xe-xs+1
        yl = ye-ys+1

        write (msgstr,'(A,A,A,A16,A,I6,A,I6,A,I6)') 'Reading ',trim(fileName),' field ',&
            &trim(fieldName),' (',nx,',',ny,') at time',t
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        call ncCheck(nf90_get_var(ncid, varid, data2D(1:xl,1:yl), (/ xs, ys, t/), &
            (/xl, yl, 1/)))

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable2DAtTime(fileName,fieldName,data1D,nx,t,xstart,xend)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: data1D(:)
        integer, intent(in) :: nx,t
        integer, intent(in), optional :: xstart, xend
        integer :: ncid,varid,xs,xe,xl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        xl = xe-xs+1

        write(msgstr,'(A,A,A,A16,A,I6,A,I6)') 'Reading ',trim(fileName),' field ',&
            &trim(fieldName),' (',nx,') at time',t
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        call ncCheck(nf90_get_var(ncid, varid, data1D(1:xl), (/ xs, t/), (/xl, 1/)))

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable1DAtTime(fileName,fieldName,dataValue,t)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: dataValue
        integer, intent(in) :: t
        integer :: ncid,varid

        real(8) :: dataValues(1)

        write(msgstr,'(A,A,A,A16,A,I6)') 'Reading ',trim(fileName),' field ',&
            &trim(fieldName),' at time',t
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        call ncCheck(nf90_get_var(ncid, varid, dataValues(1:1), (/ t /), (/ 1/)))

        dataValue = dataValues(1)

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable3D(fileName,fieldName,data3D,nx,ny,nz,&
        xstart,xend,ystart,yend,zstart,zend)

        implicit none

        character(len=*),intent(in)  :: fileName
        character(len=*),intent(in)  :: fieldName
        real(8),         intent(out) :: data3D(:,:,:)
        integer,         intent(in)  :: nx,ny,nz
        integer,         intent(in), optional :: xstart, xend, ystart, yend, zstart, zend
        integer :: ncid,varid,xs,xe,ys,ye,zs,ze,xl,yl,zl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        if (present(ystart)) then
            ys = ystart
        else
            ys = 1
        end if

        if (present(yend)) then
            ye = yend
        else
            ye = ny
        end if

        if (present(zstart)) then
            zs = zstart
        else
            zs = 1
        end if

        if (present(zend)) then
            ze = zend
        else
            ze = nz
        end if

        write(msgstr,'(A,A,A,A16,A,I6,A,I6,A,I6,A)') 'Reading ',trim(fileName),' field ',&
            &trim(fieldName),' (',nx,',',ny,',',nz,')'
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        xl = xe-xs+1
        yl = ye-ys+1
        zl = ze-zs+1

        if (nz .ne. 1) then
            call ncCheck(nf90_get_var(ncid, varid, data3D(1:xl,1:yl,1:zl), (/ xs, ys, zs/), &
                (/xl, yl, zl/)))
        else
            call ncCheck(nf90_get_var(ncid, varid, data3D(1:xl,1:yl,1), (/ xs, ys/), &
                (/xl, yl/)))
        endif

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable2D(fileName,fieldName,data2D,nx,ny,xstart,xend,ystart,yend)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: data2D(:,:)
        integer, intent(in) :: nx,ny
        integer, intent(in), optional :: xstart, xend, ystart, yend
        integer :: ncid,varid,xs,xe,ys,ye,xl,yl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        if (present(ystart)) then
            ys = ystart
        else
            ys = 1
        end if

        if (present(yend)) then
            ye = yend
        else
            ye = ny
        end if

        xl = xe-xs+1
        yl = ye-ys+1

        write(msgstr,'(A,A,A,A16)') 'Reading ',trim(fileName),' field ',trim(fieldName)
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        call ncCheck(nf90_get_var(ncid, varid, data2D(1:xl,1:yl), (/xs, ys/), &
            (/xl, yl/)))

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadVariable1D(fileName,fieldName,data1D,nx,xstart,xend)
        implicit none

        character(len=*),intent(in) :: fileName
        character(len=*),intent(in) :: fieldName
        real(8), intent(out) :: data1D(:)
        integer, intent(in) :: nx
        integer, intent(in), optional :: xstart, xend
        integer :: ncid,varid,xs,xe,xl

        if (present(xstart)) then
            xs = xstart
        else
            xs = 1
        end if

        if (present(xend)) then
            xe = xend
        else
            xe = nx
        end if

        xl = xe-xs+1

        write (msgstr,'(A,A,A,A16)') 'Reading ',trim(fileName),' field ',trim(fieldName)
        call print(msgstr)

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        call ncCheck(nf90_inq_varid(ncid, fieldName, varid))

        call ncCheck(nf90_get_var(ncid, varid, data1D(1:xl), (/ xs /), (/xl/)))

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadAttributeChar(fileName,attrName,attrValue,attrDefault,varName)
        implicit none

        character(len=*), intent(in) :: fileName
        character(len=*), intent(in) :: attrName
        character(len=1024), intent(out) :: attrValue
        character(len=*), intent(in), optional :: attrDefault
        character(len=*), intent(in), optional :: varName

        integer :: ncid, varId
        integer :: attrLength, attrType
        integer :: attrPresent

        if (present(varName)) then
            call ncCheck(nf90_inq_varid(ncid, varName, varid))
        else
            varId = NF90_GLOBAL
        end if

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        attrPresent = nf90_inquire_attribute(ncid, varId, attrName, len = attrLength, xtype = attrType)

        if (attrPresent .ne. NF90_NOERR) then
            if (present(attrDefault)) then
                attrValue = attrDefault
            else
                write(msgstr,*) 'Could not find attribute',trim(attrName),'and no default given'
                call error(msgstr)
            end if
        else
            if (attrLength .gt. 1024) then
                write(msgstr,*) 'Length of attribute',trim(attrName),'(',attrLength,') is greater than currently allowed.'
                call error(msgstr)
            end if

            if (attrType .ne. NF90_CHAR) then
                write(msgstr,*) 'Type of attribute',trim(attrName),'(',attrType,') is not NF90_CHAR (',NF90_CHAR,')'
                call error(msgstr)
            end if

            call ncCheck(nf90_get_att(ncid, varId, attrName, attrValue))
        end if

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadAttributeInteger(fileName,attrName,attrValue,attrDefault,varName)
        implicit none

        character(len=*), intent(in) :: fileName
        character(len=*), intent(in) :: attrName
        integer, intent(out) :: attrValue
        integer, intent(in), optional :: attrDefault
        character(len=*), intent(in), optional :: varName

        integer :: ncid, varId
        integer :: attrLength, attrType
        integer :: attrPresent

        if (present(varName)) then
            call ncCheck(nf90_inq_varid(ncid, varName, varid))
        else
            varId = NF90_GLOBAL
        end if

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        attrPresent = nf90_inquire_attribute(ncid, varId, attrName, len = attrLength, xtype = attrType)

        if (attrPresent .ne. NF90_NOERR) then
            if (present(attrDefault)) then
                attrValue = attrDefault
            else
                write(msgstr,*) 'Could not find attribute',trim(attrName),'and no default given'
                call error(msgstr)
            end if
        else
            if (attrLength .gt. 1) then
                write(msgstr,*) 'Cannot read integer attribute of length > 1 (',attrLength,')'
                call error(msgstr)
            end if

            if (attrType .eq. NF90_CHAR) then
                write(msgstr,*) 'Type of attribute',trim(attrName),'(',attrType,') is NF90_CHAR (',NF90_CHAR,')'
                call error(msgstr)
            end if

            call ncCheck(nf90_get_att(ncid, varId, attrName, attrValue))
        end if

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncReadAttributeDouble(fileName,attrName,attrValue,attrDefault,varName)
        implicit none

        character(len=*), intent(in) :: fileName
        character(len=*), intent(in) :: attrName
        real(8), intent(out) :: attrValue
        real(8), intent(in), optional :: attrDefault
        character(len=*), intent(in), optional :: varName

        integer :: ncid, varId
        integer :: attrLength, attrType
        integer :: attrPresent

        if (present(varName)) then
            call ncCheck(nf90_inq_varid(ncid, varName, varid))
        else
            varId = NF90_GLOBAL
        end if

        call ncCheck(nf90_open(fileName, NF90_NOWRITE, ncid))

        attrPresent = nf90_inquire_attribute(ncid, varId, attrName, len = attrLength, xtype = attrType)

        if (attrPresent .ne. NF90_NOERR) then
            if (present(attrDefault)) then
                attrValue = attrDefault
            else
                write(msgstr,*) 'Could not find attribute',trim(attrName),'and no default given'
                call error(msgstr)
            end if
        else
            if (attrLength .gt. 1) then
                write(msgstr,*) 'Cannot read double attribute of length > 1 (',attrLength,')'
                call error(msgstr)
            end if

            if (attrType .eq. NF90_CHAR) then
                write(msgstr,*) 'Type of attribute',trim(attrName),'(',attrType,') is NF90_CHAR (',NF90_CHAR,')'
                call error(msgstr)
            end if

            call ncCheck(nf90_get_att(ncid, varId, attrName, attrValue))
        end if

        call ncCheck(nf90_close(ncid))
    end subroutine

    subroutine ncCheck(status, message)
        implicit none

        integer,                    intent(in) :: status
        character(len=*), optional, intent(in) :: message

        if(status /= nf90_noerr) then
            if (present(message)) then
                write(msgstr,*) trim(message),', the NetCDF check failed - ' &
                    & // trim(nf90_strerror(status))
            else
                write(msgstr,*) 'NetCDF check failed - ' &
                    & // trim(nf90_strerror(status))
            end if

            call error(msgstr)
        end if
    end subroutine
end module
