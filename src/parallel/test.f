program test
    use parallelInfo_mod
    use parallelConstants_mod
    use parallelDecomposition_mod

    use, intrinsic :: iso_c_binding
    use iso_fortran_env

    use dictionary_mod

    use dataShape_mod
    use dataExtent_mod
    use dataVariable_mod

    use dataGrid_mod
    use tripack_mod
    use triangularTiling_mod

    use geodesic_mod

    use satelliteObservation_mod
    use scannedObservationBundle_mod
    use radianceAssimilationFactory_mod

    use sortUtils_mod
    use asciiUtils_mod
    use mpiUtils_mod

    implicit none

    class(ParallelInfo),              pointer :: pinfo_state  => null()
    class(ParallelInfo),              pointer :: pinfo_obs    => null()
    class(ParallelDecomposition),     pointer :: decomp_state => null()
    class(ParallelDecomposition),     pointer :: decomp_obs   => null()
    class(ScannedObservation),        pointer :: scannedObs
    class(DataGrid),                  pointer :: inputGrid

    character(len=:), allocatable :: orbitFile
    character(len=:), allocatable :: platformNumberString
    character(len=:), allocatable :: postingNumberString
    character(len=:), allocatable :: outputPrefix

    class(DataVariable),  pointer :: latLocal
    class(DataVariable),  pointer :: latGlobal
    class(DataVariable),  pointer :: lonLocal
    class(DataVariable),  pointer :: lonGlobal
    class(DataShape),     pointer :: latShape
    class(TriangularTiling), pointer :: gridTri

    class(Dictionary),    pointer :: triDict
    class(Dictionary),    pointer :: nbrDict

    integer :: platformNumber
    integer :: postingNumber
    integer :: xls, xle, nlx
    integer :: yls, yle, nly
    integer :: xbs, xbe, nbx
    integer :: ybs, ybe, nby
    integer :: xgs, xge, ngx
    integer :: ygs, yge, ngy
    integer :: xgc, ygc
    integer :: xis, xie, nix
    integer :: yis, yie, niy
    integer :: i, j, ind
    integer, parameter :: buffer  = 20
    integer, parameter :: buffer2 = 5
    integer :: nodenum
    integer :: numtot
    integer :: ierr

    integer :: localBounds(6)

    integer,      dimension(:,:), pointer :: node_ij   => NULL()
    real(real64), dimension(:,:), pointer :: node_xy   => NULL()
    real(real64), dimension(:,:), pointer :: node_xpyp => NULL()

    real(real32), pointer :: latptr(:,:)
    real(real32), pointer :: lonptr(:,:)

    integer, pointer :: triangles(:,:)
    integer, pointer :: neighbors(:,:)
    integer, pointer :: triv(:,:)
    integer, pointer :: nbrv(:,:)
    integer, pointer :: allTriangles(:,:)
    integer, pointer :: allNeighbors(:,:)
    integer, pointer :: ltri(:,:)

    integer, dimension(:,:), pointer :: tri_i_node_ij

    character(DICT_KEY_LENGTH) :: triKey, nbrKey

    integer, parameter :: HASH_SIZE = 2895134 ! a prime number about 20% larger than largest estimated data set

    real(real64), parameter :: d2r = dacos(0.d0)/90.d0

    real(real64) :: latc, lonc, lat, lon, k, x, y

    integer :: allTriInd, i1, j1, gind, l, m, loop, nbr, nodeInd, triInd

    logical :: allIn

    type triData
        integer :: triInd
    end type

    type(triData), pointer :: tdat

    class(*), pointer :: optr

    integer, pointer :: lptr(:)
    integer, pointer :: lend(:)
    integer, pointer :: list(:)
    integer :: lnew
    integer :: ier
    integer :: nit, nlist
    integer :: nlist_all

    integer, pointer :: lptrv(:)
    integer, pointer :: lendv(:)
    integer, pointer :: listv(:)
    integer, pointer :: nodeiv(:)
    integer, pointer :: nodejv(:)
    real(real64), pointer :: nodexv(:)
    real(real64), pointer :: nodeyv(:)
    integer :: xs, xe
    integer :: ys, ye
    integer :: proc, indAll, lnewAll, loldAll

    logical :: nbrb, nodeb
    integer :: listFac

    integer, allocatable :: allLptr(:)
    integer, allocatable :: allLend(:)
    integer, allocatable :: allList(:)
    integer, allocatable :: allNodei(:)
    integer, allocatable :: allNodej(:)
    real(real64), allocatable :: allNodex(:)
    real(real64), allocatable :: allNodey(:)
    integer :: allLnew

    integer :: node, lpl, lp, n1, na, nb, nt

    integer :: iv(3), jv(3)

    integer, allocatable :: nodes(:)

    integer :: a, b, c

    integer, allocatable :: lcc(:)
    integer, allocatable :: lct(:)
    integer :: lwk
    integer, allocatable :: iwk(:)

    real(real64) :: armax

    real(real64),   allocatable :: dist(:)
    integer(int32), allocatable :: near(:)
    integer(int32), allocatable :: next(:)

    integer      :: mink
    real(real64) :: mind
    real(real64) :: azi1, azi2
    real(real64) :: s12
    real(real64) :: px
    real(real64) :: py
    real(real64), pointer :: allXCenter(:)
    real(real64), pointer :: allYCenter(:)

    call initParallel()

    if (command_argument_count() /= 5) then
        call print('Usage: deconvolverObsRes.exe [Orbit file] [Platform #] [Posting #] [Output prefix] ' // &
            &'[Split #] [NSplit] [BHalf file]')
        call print('')
        call print('where')
        call print('')
        call print('    [Orbit file] is the orbit file, must be of file type corresponding [platform number]')
        write(msgstr,'(A,I4,A)') '    [platform #] is the number of the platform, e.g. "',PLATFORM_GPM_GMI,&
            '" for GPM/GMI (see radianceAssimilationConstants.f03).'
        call print(msgstr)
        call print('    [Posting #] is the posting to interpolate at; e.g. for GPM/GMI "2" denotes the HR channels.')
        call print('    [Output prefix] is the prefix to add to the split output files.')
        !call print('    [Split #] is number of the split between 1 and NSplit, inclusive, to compute and output.')
        !call print('    [NSplit] is total # of split files, of which this program will compute a single file.')
        call print('    [BHalf file] is the square-root (SVD) of the single column first guess error covariance matrix.')
        call endParallel()
        return
    end if

    call getArg(1, orbitFile)
    call getArg(2, platformNumberString)
    call getArg(3, postingNumberString)
    call getArg(4, outputPrefix)
    ! call getArg(5, bhalfFile)

    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    call print('Running observation-resolution deconvolution.')
    call print('   Orbit file: '      // trim(orbitFile))
    call print('   Platform number: ' // trim(platformNumberString))
    call print('   Posting number: '  // trim(postingNumberString))
    call print('   Output prefix: '   // trim(outputPrefix))
    ! call print('   BHalf file: '      // trim(bhalfFile))

    allocate(decomp_state)
    call decomp_state%parallelDecompositionConstructor(dim1Name=SCANS_DIM_NAME)

    allocate(pinfo_state)
    call pinfo_state%parallelInfoConstructor(DISTRIBUTED_PARALLEL_TYPE,decomp=decomp_state)

    write(msgstr,'(I6)') pinfo_state%getCommSize()
    call print('   Number of processors: ' // trim(adjustl(msgstr)))

    read( platformNumberString, '(i10)' ) platformNumber
    read( postingNumberString,  '(i10)' ) postingNumber

    !write(procNumString,'(i5)') pinfo_state%getRank() + 1 + 10000 ! make a zero padded string

    !splitFile       = 'tbDeconv_'   // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    !columnNormsFile = 'colNorms_'   // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    !firstGuessFile  = 'firstguess_' // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'
    !pcStarFile      = 'pcStar_'     // trim(outputPrefix) // '_SPLIT' // procNumString(2:5) // '.nc'

    call print('----------------------------------------------------')
    call print('')
    call print('')
    call print('')
    call print('----------------------------------------------------')
    !call print('Output deconvolution file: ' // trim(splitFile))
    !call print('Output column norms file: '  // trim(columnNormsFile))
    !call print('Output first guess file: '   // trim(firstGuessFile))
    !call print('Output PC solutions file: '  // trim(pcStarFile))
    !call print('')

!    nchans = 0
!
!    inquire(file=trim(BHalfFile),exist=fileExists)
!
!    if (.not. fileExists) then
!        call error('Could not load the ' // trim(BHalfFile) // ' file!')
!    end if
!
!    call print('Now reading BHalf file' // trim(BHalfFile))
!    call load2DRealFile(BHalfFile,npc,nchans,veofs)
!    write(msgstr,*) 'Read ',nchans,'chans /',npc,'PCs from the BHalf file ',trim(BHalfFile)
!    call print(msgstr)
!    call print('')

!    if (nchans <= 0 .or. npc <= 0) then
!        write(msgstr,*) 'Invalid BHalfFile ',trim(BHalfFile),'. Dims:',nchans,npc
!        call error(msgstr)
!    end if

    write(msgstr,*) 'Now reading orbit file ',trim(orbitFile), ' for deconvolution at posting:', &
        postingNumber
    call print(msgstr)

    ! load the distributed data
    scannedObs => getScannedObservation(pinfo_state,orbitFile,platformNumber,postingNumber)

    call scannedObs%findGrid(pinfo_state)
    inputGrid => scannedObs%getGrid()

    !print *,'tr list is:',ier

    !print *,'a,b,c:',a,b,c,allNodex(10),allNodey(10),minval(allNodex),maxval(allNodex),minval(allNodey),maxval(allNodey)

!    dxa = node_xy(1,a) - node_xy(1,c)
!    dya = node_xy(2,a) - node_xy(2,c)
!
!    dxb = node_xy(1,b) - node_xy(1,c)
!    dyb = node_xy(2,b) - node_xy(2,c)
!
!    dxp = p(1)         - node_xy(1,c)
!    dyp = p(2)         - node_xy(2,c)
!
!    det = dxa * dyb - dya * dxb
    !
    !  Compute the barycentric coordinates of the point P with respect
    !  to this triangle.
    !
!    alpha = ( dxp * dyb - dyp * dxb ) / det
!    beta =  ( dxa * dyp - dya * dxp ) / det
!    gamma = 1.0D+00 - alpha - beta

!    call trmtst ( ngx*ngy, allNodex, allNodey, allList, allLptr, allLend, &
!        lnewAll, 1d0, armax, ier )
!
!    print *,'ier:',pinfo_state%getRank(),ier,armax

!    allocate(lcc(1))
!    lwk = 2*nodenum
!    allocate(iwk(lwk))

!    lcc(1) = nbx*nby + 1

!    call addcst ( 1, lcc, nodenum, node_xy(1,:), node_xy(2,:), lwk, iwk, list, lptr, lend, ier )

!    if (pinfo_state%getRank() == 0) then
!        open(unit = 9, file='test.eps')
!        call trplot( 9, 8.5d0, minval(node_xy(1,:)), maxval(node_xy(1,:)), &
!            & minval(node_xy(2,:)), maxval(node_xy(2,:)), 1, lcc, &
!            & nodenum, node_xy(1,:), node_xy(2,:), list, lptr, lend, &
!            & '(Triangulation created by TRIPACK_PRB)', .false., ier )
!
!        close(unit=9)
!    end if

    call barrier()

    stop

    print *,'found ',pinfo_state%getRank(),xgs,xge,ygs,yge

    !call MPI_AllReduce(gridTri%getNumTriangles(),numtot,1,MPI_INTEGER,MPI_SUM,&
    !    & pinfo_state%getCommunicator(),ierr)

    allocate(allTriangles(3,numtot))
    allocate(allNeighbors(3,numtot))

    print *,'found ntot triangles:',pinfo_state%getRank(),numtot

    triDict => Dictionary(HASH_SIZE)
    nbrDict => Dictionary(HASH_SIZE)

    ! loop through this process twice - once for the triangles, then for the neighbors
    ! this is because the neighbors need to have all of the triangles ready before proceeding
    do loop=1,2
        ! counter for the global triangles
        allTriInd = 0

        ! we will send the triangles from each process
        do i=0,pinfo_state%getCommSize()-1
            ! if this is our turn, we'll set the necessary variables
            if (i == pinfo_state%getRank()) then
                !triangles => gridTri%getTriangleIndices()
                !triangles => triangles(:,1:gridTri%getNumTriangles())
                tri_i_node_ij => node_ij

                if (loop == 2) then
                    !neighbors => gridTri%getElementNeighbors()
                    !neighbors => neighbors(:,1:gridTri%getNumTriangles())
                end if

                ! we'll share the local bounds as well so the boundary can be handled correctly
                localBounds(1) = xls
                localBounds(2) = xle
                localBounds(3) = nlx
                localBounds(4) = yls
                localBounds(5) = yle
                localBounds(6) = nly
            end if

            ! broadcast the local bounds
            call bcast1d(localBounds,6,i,pinfo_state%getCommunicator(),'bcasting the local bounds')

            ! and pull the data out
            xis = localBounds(1)
            xie = localBounds(2)
            nix = localBounds(3)
            yis = localBounds(4)
            yie = localBounds(5)
            niy = localBounds(6)

            ! now set the second (smaller) buffer region
            ! any triangles inside here are considered good enough to add
            ! the triangles outside will be discarded for now (but added later)
            xbs = max(xgs,xis-buffer2)
            xbe = min(xge,xie+buffer2)
            ybs = max(ygs,yis-buffer2)
            ybe = min(yge,yie+buffer2)

            ! bcast all of the triangles from process i
            call bcastnd_varlen(triangles,i,pinfo_state%getCommunicator(),'bcasting the triangles from rank ' // &
                & int2str(i))

            ! bcast all of the nodes from process i
            call bcastnd_varlen(tri_i_node_ij,i,pinfo_state%getCommunicator(),'bcasting the node indexes from rank ' // &
                & int2str(i))

            ! if setting the neighbors
            if (loop == 2) then
                ! bcast all of the local neighbors from process i
                call bcastnd_varlen(neighbors,i,pinfo_state%getCommunicator(),'bcasting the neighbors from rank ' // &
                    & int2str(i))
            end if

            print *,'rank ',pinfo_state%getRank(),'now looping through ',size(triangles,2),'triangles',xbs,xbe,ybs,ybe

            ! now loop through all of the triangles for process i
            do j=1,size(triangles,2)
                allIn = .true.

                ! make sure the triangle has all points inside the buffer region
                do l=1,3
                    ind = triangles(l,j)

                    i1 = tri_i_node_ij(1,ind)
                    j1 = tri_i_node_ij(2,ind)

                    if (i1 < xbs .or. i1 > xbe .and. &
                        j1 < ybs .or. j1 > ybe) then
                        allIn = .false.
                    end if
                end do

                if (.not. allIn) then
                    ! not all of the indexes are in the buffer region so the triangle is suspect
                    cycle ! the j loop
                end if

                ! get the comma-separated key of all of the nodes
                triKey = getTriangleKey(j,triangles,tri_i_node_ij,ngx)

                ! check if this key has already been added to the triDict or nbrDict
                if (loop == 1) then
                    if (triDict%hasKey(triKey)) then
                        cycle
                    end if
                else
                    if (nbrDict%hasKey(triKey)) then
                        cycle
                    end if
                end if

                ! we now have a good triangle to add to the global list (tri or nbr)
                allTriInd = allTriInd + 1

                if (loop == 1) then
                    ! prepare to add this triangle to allTriangles
                    do l=1,3
                        ! move the local node indexes to global node indexes
                        ind = triangles(l,j)

                        i1 = tri_i_node_ij(1,ind)
                        j1 = tri_i_node_ij(2,ind)

                        gind = i1 + (j1-1)*ngx

                        allTriangles(l,allTriInd) = gind
                    end do

                    allocate(tdat)
                    tdat%triInd = allTriInd
                    optr => tdat
                    call triDict%add(triKey,optr)
                else
                    ! need to find all the neighbors of the triangle

                    do l=1,3
                        nbr = neighbors(l,j)

                        if (nbr > 0) then
                            ! if positive, this is a (local) triangle index
                            ! need to convert it to the global triangle index
                            triInd = nbr
                        else
                            ! if negative, this is a point on the convex hull
                            ! in this case, LINK = -(3*I + J-1) where I, J = triangle, edge index
                            ! need to convert I here to the global index
                            nodeInd = mod(-nbr,3)+1
                            triInd  = (-nbr-j+1)/3
                        end if

                        ! first get the triangle key
                        nbrKey = getTriangleKey(triInd,triangles,tri_i_node_ij,ngx)

                        ! check if we have the key in the triDict, otherwise error
                        if (.not. triDict%hasKey(nbrKey)) then
                            call print('Could not find the triangle corresponding to the key ' // trim(nbrKey) // &
                                & ' in the dictionary of triangles: ' // int2str(nbr))

                            call print('triangle:')

                            nbrKey = getTriangleKey(j,triangles,tri_i_node_ij,ngx,doDebug=.true.)

                            call print('neighbor:')

                            nbrKey = getTriangleKey(triInd,triangles,tri_i_node_ij,ngx,doDebug=.true.)

                            cycle
                        end if

                        ! now look-up the key to get the triData wrapper
                        optr => triDict%get(nbrKey)

                        ! cast down to the correct type
                        select type(optr)
                            type is (TriData)
                                tdat => optr
                            class default
                                call error('Unknown type in triDict.')
                        end select

                        if (nbr > 0) then
                            ! now set the global triangle index from the triData
                            allNeighbors(l,allTriInd) = tdat%triInd
                        else
                            ! now set the (negative) global link index from the triData
                            allNeighbors(l,allTriInd) = -(3*tdat%triInd + nodeInd - 1)
                        end if
                    end do

                    allocate(tdat)
                    tdat%triInd = allTriInd
                    optr => tdat
                    call nbrDict%add(triKey,optr)
                end if
            end do ! the "j = 1,numtri" loop

            print *,'rank ',pinfo_state%getRank(),'has now added ',allTriInd,'indexes'

            ! now clean up the allocated var len arrays
            if (i /= pinfo_state%getRank()) then
                deallocate(triangles)
                deallocate(tri_i_node_ij)

                if (loop == 2) then
                    deallocate(neighbors)
                end if
            end if
        end do ! the "i = 0,comm_size-1" loop
    end do ! the "loop = 1,2" loop

    contains

    function getTriangleKey(triInd,triangles,node_ij,nx,doDebug,forceOutput) result(triKey)
        implicit none

        integer, intent(in) :: triInd
        integer, intent(in) :: triangles(:,:)
        integer, intent(in) :: node_ij(:,:)
        integer, intent(in) :: nx
        logical, optional, intent(in) :: doDebug
        logical, optional, intent(in) :: forceOutput

        character(DICT_KEY_LENGTH) :: triKey
        integer :: l
        integer :: i, j, ind, gind

        logical :: doPrint

        integer :: tri(3)

        if (present(doDebug)) then
            doPrint = doDebug
        else
            doPrint = .false.
        end if

        triKey = ''

        tri(:) = triangles(:,triInd)

        ! sort the indexes so that all orders are considered the same
        call quicksort(tri,1,3)

        do l=1,3
            ind = tri(l)

            i = node_ij(1,ind)
            j = node_ij(2,ind)

            gind = i + (j-1)*nx

            if (doPrint) then
                write(msgstr,*) 'index',l,'(',ind,'/',size(node_ij,2),'):',i,j,'/',gind
                call print(msgstr,forceOutput=forceOutput)
            end if

            if (l > 1) then
                triKey = trim(triKey) // ','
            end if

            triKey = trim(triKey) // int2str(gind)
        end do
    end function
end program
