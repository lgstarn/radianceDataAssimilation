program runKMatrix

    use dataSet_mod
    use atmos3dDataSet_mod
    use atmos3dDataSetFactory_mod
    use observationOperator_mod
    use observation_mod
    use satelliteObservationOperator_mod
    use satelliteObservation_mod
    use satellitePlatformInfo_mod
    use rtmUtils_mod
    use rtmOptions_mod
    use radianceAssimilationFactory_mod
    use mpiUtils_mod

    implicit none

    character(len=1024) :: inputFile
    character(len=1024) :: outputFile
    character(len=256)  :: platformNumberName
    character(len=256)  :: modelName
    character(len=256)  :: obsOpName
    character(len=256)  :: xsStr
    character(len=256)  :: xeStr
    character(len=256)  :: ysStr
    character(len=256)  :: yeStr

    class(Atmos3DDataSet),               pointer :: state, stateAdj
    class(SatelliteObservationOperator), pointer :: obsOp
    class(SatelliteObservation),         pointer :: obsData

    integer :: time = 1
    integer :: platformNumber

    class(RtmOptions), pointer :: rtmOpts

    integer, parameter :: nfields=0
    ! choose some default fields
    character(len=256), dimension(nfields) :: requestedfields

    integer, dimension(nfields) :: fieldNz

    integer :: narg, nchans, nz, nz_all, x, y, nx, ny, xl, yl
    real(8), dimension(:,:), pointer :: dptr2d

    integer :: ncid, kmvarid, lonvarid, latvarid, chsubsetid, xindvarid, yindvarid
    integer,dimension(4) :: dimIDs

    character(len=1024) :: cmdlineArg

    class(SatellitePlatformInfo), pointer :: platform

    real(8), dimension(:,:),     allocatable :: k_matrix_local
    real(8), dimension(:,:,:,:), allocatable :: k_matrix
    integer, dimension(:,:),     pointer     :: xinds, yinds

    integer :: xs, xe, ys, ye

    narg = command_argument_count()

    if (narg .ne. 9) then
        write(msgstr,*) 'Usage:'
        call print(msgstr)
        call get_command_argument(0, cmdlineArg)
        write(msgstr,*) '    ',adjustl(trim(cmdlineArg)),' [inputFile] [outputFile] [model string] [platform #] ' // &
            '[obs op string] [xs] [xe] [ys] [ye]'
        call print(msgstr)
        write(msgstr,*) ''
        call print(msgstr)
        write(msgstr,*) 'where'
        call print(msgstr)
        write(msgstr,*) ''
        call print(msgstr)
        write(msgstr,*) '    [inputFile] is the model file to read (must be of type [model string])'
        call print(msgstr)
        write(msgstr,*) '    [outputFile] is the location to place the output from the observation operator'
        call print(msgstr)
        write(msgstr,*) '    [model string] is the code for the state, e.g. "',trim(WRF_ARW_MODEL),&
            '" for the WRF ARW model (see radianceAssimilationFactory.f03)'
        call print(msgstr)
        write(msgstr,'(A,I4,A)') '     [platform #] is the number of the platform, e.g. "',PLATFORM_TRMM_TMI,&
            '" for TRMM/TMI'
        call print(msgstr)
        write(msgstr,*) '    [obs op string] is the code for the observation operator, e.g. "',&
            trim(CRTM_OBS_OP),'" for the Community Radiative Transfer Model (see radianceAssimilationFactory.f03)'
        call print(msgstr)
        write(msgstr,*) '    [xs] the x index in the inputFile to start from'
        call print(msgstr)
        write(msgstr,*) '    [xe] the x index in the inputFile to end on'
        call print(msgstr)
        write(msgstr,*) '    [ys] the y index in the inputFile to start from'
        call print(msgstr)
        write(msgstr,*) '    [xe] the y index in the inputFile to end on'
        call print(msgstr)

        call abortParallel()
    end if

    call get_command_argument(1, inputFile)
    call get_command_argument(2, outputFile)
    call get_command_argument(3, modelName)
    call get_command_argument(4, platformNumberName)
    call get_command_argument(5, obsOpName)
    call get_command_argument(6, xsStr)
    call get_command_argument(7, xeStr)
    call get_command_argument(8, ysStr)
    call get_command_argument(9, yeStr)

    read( platformNumberName, '(i10)' ) platformNumber

    read( xsStr, '(i10)' ) xs
    read( xeStr, '(i10)' ) xe
    read( ysStr, '(i10)' ) ys
    read( yeStr, '(i10)' ) ye

    write(msgstr,*) 'Running obs op ',trim(obsOpName), ' and platform number ',&
        trim(platformNumberName),' for model type ',trim(modelName)
    call print(msgstr)
    write(msgstr,*) '    on input file ',trim(inputFile)
    call print(msgstr)
    write(msgstr,*) '    and outputting the results to ',trim(outputFile)
    call print(msgstr)

    state    => getAtmos3dDataSet(pinfo,modelName,inputFile,time,.true.,null(),null(),&
        requestedFields,fieldNz)

    stateAdj => getAtmos3dDataSet(pinfo,modelName,inputFile,time,.false.,null(),null(),&
        requestedFields,fieldNz)

    rtmOpts => getRtmOptions()
    platform => getSatellitePlatform(platformNumber,obsOpName)
    obsOp => getSatelliteObservationOperator(obsOpName,rtmOpts,platform)

    if (platformNumber .eq. PLATFORM_HAMSR) then
      allocate(platform%channelSubset(6))
      platform%channelSubset(:) = (/9,10,11,12,13,14/)
    else if (platformNumber .eq. PLATFORM_AMSUA_AQUA) then
      allocate(platform%channelSubset(8))
      platform%channelSubset(:) = (/3,4,5,6,7,8,9,15/)
    end if

    allocate(obsData)
    call obsData%satelliteObservationConstructor(platform)

    nz = state%getNZ()
    nz_all = 9*nz+3
    if (associated(platform%channelSubset)) then
      nchans = size(platform%channelSubset)
    else
      nchans = platform%mobs
    end if

    nx = xe-xs+1
    ny = ye-ys+1
    allocate(k_matrix(nz_all,nchans,nx,ny))
    allocate(k_matrix_local(nz_all,nchans))

    allocate(xinds(nx,ny))
    allocate(yinds(nx,ny))

    k_matrix = 0.d0

    do x=xs,xe
        do y=ys,ye
            xl = x-xs+1
            yl = y-ys+1
            xinds(xl,yl) = x
            yinds(xl,yl) = y
            call doRtmAdjointMatrix(state, stateAdj, obsOp, x, y, 1, '', platform, &
                k_matrix_local)
            if (associated(platform%channelSubset)) then
               k_matrix(:,1:size(platform%channelSubset),xl,yl) = k_matrix_local(:,:)
            else
               k_matrix(:,:,xl,yl) = k_matrix_local(:,:)
            end if
        end do
    end do
    
    call ncCheck( nf90_create(outputFile, NF90_CLOBBER, ncid) )
    call ncCheck( nf90_def_dim(ncid, 'k_matrix_all',nz_all, dimIDs(1)) )
    call ncCheck( nf90_def_dim(ncid, 'nchannels',   nchans, dimIDs(2)) )
    call ncCheck( nf90_def_dim(ncid, 'west_east',   nx, dimIDs(3)) )
    call ncCheck( nf90_def_dim(ncid, 'south_north', ny, dimIDs(4)) )

    call ncCheck( nf90_def_var(ncid, 'K_MATRIX', NF90_DOUBLE, dimIDs(1:4), kmvarid) )
    call ncCheck( nf90_def_var(ncid, 'LON', NF90_DOUBLE, dimIDs(3:4), lonvarid) )
    call ncCheck( nf90_def_var(ncid, 'LAT', NF90_DOUBLE, dimIDs(3:4), latvarid) )
    call ncCheck( nf90_def_var(ncid, 'XIND',  NF90_INT, dimIDs(3:4), xindvarid) )
    call ncCheck( nf90_def_var(ncid, 'YIND',  NF90_INT, dimIDs(3:4), yindvarid) )
    if (associated(platform%channelSubset)) then
        call ncCheck( nf90_def_var(ncid, 'CHANNEL_SUBSET', NF90_DOUBLE, dimIDs(2), chsubsetid) )
    end if

    call ncCheck( nf90_enddef(ncid) )

    !call ncCheck( nf90_inq_varid(ncid, 'K_MATRIX', kmvarid) )
    !call ncCheck( nf90_inq_varid(ncid, 'LON', lonvarid) )
    !call ncCheck( nf90_inq_varid(ncid, 'LAT', latvarid) )
    !if (associated(platform%channelSubset)) then
    !    call ncCheck( nf90_inq_varid(ncid, 'CHANNEL_SUBSET', chsubsetid) )
    !end if

    write(msgstr,*) 'k_mat min/max:',minval(k_matrix),maxval(k_matrix)
    call print(msgstr)

    call ncCheck(nf90_put_var(ncid, kmvarid, k_matrix, (/ 1, 1, 1, 1 /), (/nz_all, nchans, nx, ny/)))
    dptr2d => state%getVariable2D(LON_NAME)
    call ncCheck(nf90_put_var(ncid, lonvarid, dptr2d(xs:xe,ys:ye), (/ 1, 1 /), (/nx, ny/)))
    dptr2d => state%getVariable2D(LAT_NAME)
    call ncCheck(nf90_put_var(ncid, latvarid, dptr2d(xs:xe,ys:ye), (/ 1, 1 /), (/nx, ny/)))
    call ncCheck(nf90_put_var(ncid, xindvarid, xinds(1:nx,1:ny), (/ 1, 1 /), (/nx, ny/)))
    call ncCheck(nf90_put_var(ncid, yindvarid, yinds(1:nx,1:ny), (/ 1, 1 /), (/nx, ny/)))
    if (associated(platform%channelSubset)) then
        call ncCheck(nf90_put_var(ncid, chsubsetid, platform%channelSubset, (/ 1 /), (/nchans/)))
    end if

    call ncCheck( nf90_close(ncid) )

    deallocate(state)
    deallocate(rtmOpts)
    deallocate(obsOp)
    if (associated(platform%channelSubset)) then
        deallocate(platform%channelSubset)
    end if
    deallocate(platform)
    deallocate(obsData)
    deallocate(k_matrix)
    deallocate(k_matrix_local)
    deallocate(xinds)
    deallocate(yinds)
end program
