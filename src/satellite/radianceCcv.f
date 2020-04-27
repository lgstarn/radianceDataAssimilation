module radianceCcv_mod

    use mpiUtils_mod

    implicit none

    private

    public :: radianceCcvManager
    public :: radCcvManager

    type radianceCcvManager
        !private
            type(platform), dimension(:), pointer, private :: platforms
            logical, public :: initialized
        contains
            procedure :: initialize => radianceCcvManagerInit
            procedure :: forward
            procedure :: tangentLinear
            procedure :: adjoint
            procedure :: getObsCcv
            procedure :: getRegionNumber
            procedure :: getRSquared
            procedure :: getNumberOfRegions
    end type

    type(radianceCcvManager) :: radCcvManager

    type radianceCcv
         character(1024) :: ccvName, ccvType
         double precision, dimension(:), pointer :: coefficients
         double precision :: correlationCoefficient
    end type radianceCcv

    type ccvRegion
         type(radianceCcv), dimension(:), pointer :: obsCcvs
         type(radianceCcv), dimension(:), pointer :: modelCcvs
         type(ccvSpline), dimension(:), pointer :: ccvSplines
         character(1024), dimension(:), pointer :: modelVars
         double precision, dimension(:), pointer :: modelMean
         double precision, dimension(:), pointer :: obsMean
         double precision, dimension(:), pointer :: modelStddev
         double precision, dimension(:), pointer :: obsStddev
         double precision, dimension(:), pointer :: generator
         logical :: isGlobal
    end type ccvRegion

    type platform
         type(ccvRegion), pointer :: globalRegion
         type(ccvRegion), dimension(:), pointer :: regions
         character(1024) :: platformName
         integer :: nchannels
    end type platform

    type ccvSpline
         double precision, dimension(:), pointer :: xi
         double precision, dimension(:), pointer :: yi
         double precision, dimension(:), pointer :: b
         double precision, dimension(:), pointer :: c
         double precision, dimension(:), pointer :: d
    end type ccvSpline

contains

    ! compute the model CCV value from the given column NB: includes the regression to the obs ccv
    subroutine forward(this,platformNum,regionNum,ccvNums,modelColumn,hType,hxs)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum
        integer, intent(in) :: regionNum
        integer, dimension(:), intent(in) :: ccvNums
        double precision, dimension(:), intent(in) :: modelColumn
        integer, intent(in) :: hType
        double precision, dimension(:), intent(out) :: hxs

        ! local variables
        type(platform) :: pltfrm
        type(ccvRegion) :: region
        double precision, dimension(size(modelColumn)) :: work
        double precision :: hval
        integer :: i

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        if (regionNum .lt. 1) then
            region = pltfrm%globalRegion
        else
            region = pltfrm%regions(regionNum)
        end if

        work(:) = modelColumn(:)

        do i=1,size(work)
            if (region%modelStddev(i) > 1e-6) then
                work(i) = (work(i) - region%modelMean(i))/region%modelStddev(i)
            else
                work(i) = 0
            endif
        end do

        do i=1,size(ccvNums)
            if (hType == 1) then
                hval = dot_product(region%modelCcvs(ccvNums(i))%coefficients,work)
                hval = hval*region%modelCcvs(ccvNums(i))%correlationCoefficient
            elseif (hType == 2 .and. size(region%ccvSplines) .ge. ccvNums(i)) then
                hval = dot_product(region%modelCcvs(ccvNums(i))%coefficients,work)
                hval = ccvNlSpline(region%ccvSplines(ccvNums(i)),hval)
            else
                hval = 0.
                hval = hval/hval
            endif

            hxs(i) = hval
        end do
    end subroutine

    ! compute the model CCV value from the given column NB: includes the regression to the obs ccv
    subroutine tangentLinear(this,platformNum,regionNum,ccvNums,modelColumn,modelDelta,hType,hxs,hxprimes)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum
        integer, intent(in) :: regionNum
        integer, dimension(:), intent(in) :: ccvNums
        double precision, dimension(:), intent(in) :: modelColumn
        double precision, dimension(:), intent(in) :: modelDelta
        integer, intent(in) :: hType
        double precision, dimension(:), intent(out) :: hxs
        double precision, dimension(:), intent(out) :: hxprimes

        ! local variables
        type(platform) :: pltfrm
        type(ccvRegion) :: region
        double precision, dimension(size(modelColumn)) :: workDelta
        double precision :: hval, hvalprime
        integer :: i

        call this%forward(platformNum,regionNum,ccvNums,modelColumn,hType,hxs)

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        if (regionNum .lt. 1) then
            region = pltfrm%globalRegion
        else
            region = pltfrm%regions(regionNum)
        end if

        workDelta(:) = modelDelta(:)

        do i=1,size(workDelta)
            if (region%modelStddev(i) > 1d-6) then
                workDelta(i) = workDelta(i)/region%modelStddev(i)
            else
                workDelta(i) = 0.0d0
            endif
        end do

        do i=1,size(ccvNums)
            if (hType == 1) then
                hvalprime = dot_product(region%modelCcvs(ccvNums(i))%coefficients,workDelta)
                hvalprime = hvalprime*region%modelCcvs(ccvNums(i))%correlationCoefficient
            elseif (hType == 2 .and. size(region%ccvSplines) .ge. ccvNums(i)) then
                write(msgstr,*) 'Derivatives unavailable for type=2'
                call error(msgstr)
                hval = dot_product(region%modelCcvs(ccvNums(i))%coefficients,workDelta)
                hval = ccvNlSpline(region%ccvSplines(ccvNums(i)),hval)
            else
                hval = 0.
                hval = hval/hval
            endif

            hxprimes(i) = hvalprime
        end do
    end subroutine

    ! compute the model CCV value from the given column NB: includes the regression to the obs ccv
    subroutine adjoint(this,platformNum,regionNum,ccvNums,modelColumn,obsDelta,hType,hxs,adjvars)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum
        integer, intent(in) :: regionNum
        integer, dimension(:), intent(in) :: ccvNums
        double precision, dimension(:), intent(in) :: modelColumn
        double precision, dimension(:), intent(in) :: obsDelta
        integer, intent(in) :: hType
        double precision, dimension(:), intent(out) :: hxs
        double precision, dimension(:), intent(out) :: adjvars

        ! local variables
        type(platform) :: pltfrm
        type(ccvRegion) :: region
        double precision, dimension(size(obsDelta)) :: workDelta, workDelta2
        double precision :: htval
        integer :: i, j

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        call this%forward(platformNum,regionNum,ccvNums,modelColumn,hType,hxs)

        pltfrm = this%platforms(platformNum)

        if (regionNum .lt. 1) then
            region = pltfrm%globalRegion
        else
            region = pltfrm%regions(regionNum)
        end if

        workDelta = obsDelta

        do i=1,size(ccvNums)
            if (hType == 1) then
                workDelta(i) = workDelta(i)*region%modelCcvs(ccvNums(i))%correlationCoefficient
            end if
        end do

        do i=1,size(modelColumn)
            if (hType == 1) then
                do j=1,size(ccvNums)
                    workDelta2(j) = region%modelCcvs(ccvNums(j))%coefficients(i)
                end do
                htval = dot_product(workDelta2,workDelta)
            elseif (hType == 2) then
                call error('Derivatives unavailable for type=2')
            else
                htval = 0.
                htval = htval/htval
            endif

            adjvars(i) = htval
        end do

        do i=1,size(adjvars)
            if (region%modelStddev(i) > 1e-6) then
                adjvars(i) = adjvars(i)/region%modelStddev(i)
            else
                adjvars(i) = 0
            endif
        end do
    end subroutine

    ! compute the obs CCV value from the given column
    subroutine getObsCcv(this,platformNum,regionNum,ccvNums,obsChannels,ccvVals)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum
        integer, intent(in) :: regionNum
        integer, dimension(:), intent(in) :: ccvNums
        double precision, dimension(:), intent(in) :: obsChannels
        double precision, dimension(:), intent(out) :: ccvVals

        ! local variables
        type(platform) :: pltfrm
        type(ccvRegion) :: region
        double precision, dimension(size(obsChannels)) :: work
        double precision :: hval
        integer :: i

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        if (regionNum .lt. 1) then
            region = pltfrm%globalRegion
        else
            region = pltfrm%regions(regionNum)
        end if

        work = obsChannels

        do i=1,size(work)
            if (region%modelStddev(i) > 1e-6) then
                work(i) = (work(i) - region%obsMean(i))/region%obsStddev(i)
            else
                work(i) = 0
            endif
        end do

        do i=1,size(ccvNums)
            hval = dot_product(region%obsCcvs(ccvNums(i))%coefficients,work)

            ccvVals(i) = hval
        end do
    end subroutine

    ! compute the obs CCV value from the given column
    function getRegionNumber(this,platformNum,obsChannels) result(regionNum)
        implicit none

        class(RadianceCcvManager) :: this
        integer, intent(in) :: platformNum
        double precision, dimension(:), intent(in) :: obsChannels
        integer :: regionNum

        ! local variables
        type(platform) :: pltfrm
        integer :: k, min_k
        real(8) :: min_d, dist

        if (.not. this%initialized) then
            write(msgstr,*) 'Error: the radianceCcvManager object was not initialized.'
            call error(msgstr)
        end if

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        min_d = 1d10
        min_k = -1

        do k=1,size(pltfrm%regions)
            dist = dot_product(obsChannels-pltfrm%regions(k)%generator,&
                obsChannels-pltfrm%regions(k)%generator)

            if (dist .lt. min_d) then
                min_d = dist
                min_k = k
            end if
        end do

        write(msgstr,*) 'minimum k:',min_k,min_d
        call print(msgstr)

        regionNum = min_k
    end function

    !
    subroutine getRSquared(this,platformNum,regionNum,ccvNums,r2s)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum
        integer, intent(in) :: regionNum
        integer, dimension(:), intent(in) :: ccvNums
        double precision, dimension(:), intent(out) :: r2s

        ! local variables
        type(platform) :: pltfrm
        type(ccvRegion) :: region
        integer :: i

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        if (regionNum .lt. 1) then
            region = pltfrm%globalRegion
        else
            region = pltfrm%regions(regionNum)
        end if

        do i=1,size(ccvNums)
            r2s(i) = region%modelCcvs(ccvNums(i))%correlationCoefficient**2
        end do
    end subroutine

    ! initialize radianceCcv object
    subroutine radianceCcvInit(this,ccvCoeffFilename,ccvCorrelationCoeffFilename,ccvType)
        implicit none

        type(radianceCcv), intent(out) :: this
        character(1024), intent(in) :: ccvCoeffFilename, ccvCorrelationCoeffFilename, ccvType

        ! local variables
        integer :: i,ierr,coeffSize,ios

        this%ccvName = ccvCoeffFilename
        this%ccvType = ccvType

        call countLines(ccvCoeffFilename,coeffSize)

        allocate(this%coefficients(coeffSize),stat=ierr)

        if (ierr .ne. 0) then
            call error('Could not allocate the radianceCcv coefficients')
        endif

        ! read in the coefficients
        open(42,file=trim(ccvCoeffFilename))
        read(42,*) this%coefficients
        close(42)

        ! read in the correlation coefficient
        open(unit=42,file=trim(ccvCorrelationCoeffFilename))
        read(42,*) this%correlationCoefficient
        close(42)

        write(msgstr,*) trim(ccvCoeffFilename),' has',coeffSize,' coefficients and an R of',this%correlationCoefficient
        call print(msgstr)
    end subroutine radianceCcvInit

    subroutine countLines(fileName, lines)
        implicit none

        character(1024), intent(in) :: fileName
        integer, intent(out) :: lines

        integer :: ios
        character(1) :: junk

        open(unit=42,file=trim(fileName))

        lines = 0

        do while (.TRUE.)
            read(42,*,iostat=ios) junk
            if (ios /= 0) exit
            lines = lines + 1
        enddo

        close(42)

    end subroutine

    ! initialize ccvRegion object
    subroutine ccvRegionInit(this,configPath,coeffsPath,platformNum,regionNumber,isGlobal,&
        printDetail,msgoutUnit)
        implicit none

        ! input/output variables
        character(len=*), intent(in) :: configPath, coeffsPath
        type(ccvRegion), intent(out) :: this
        character(2), intent(in) :: platformNum, regionNumber
        logical, intent(in) :: isGlobal
        integer, intent(in) :: printDetail,msgoutUnit

        ! namelist variables
        integer :: nccvs, nvars
        character(1024), dimension(20) :: modelVars
        logical :: hasNlSpline
        namelist /ccvRegion_config/ nccvs, modelVars, hasNlSpline

        ! parameters
        character(1024), parameter :: OBS_TYPE = 'OBS'
        character(1024), parameter :: MODEL_TYPE = 'MODEL'

        ! local variables
        integer :: i, lines
        integer, dimension(10) ::  ierr
        character(2) :: ccvNumber
        character(1024) :: ccvCoeffFile, ccvCorrelationCoeffFile, regionPrefix
        character(1024) :: ccvSplineXFile, ccvSplineYFile, generatorFile
        character(1024) :: modelMeanFile, modelStddevFile, obsMeanFile, obsStddevFile

        ierr = 0

        ! clear modelVars (will be read from namelist below)
        do i=1,20
            modelVars(i) = ''
        enddo

        this%isGlobal = isGlobal

        if (isGlobal) then
            open(10,file=trim(configPath) // 'plat' // platformNum // '_global.cfg')
        else
            open(10,file=trim(configPath) // 'plat' // platformNum // '_rgn' // regionNumber // '.cfg')
        endif
        read(10,nml=ccvRegion_config)
        close(10)

        if (isGlobal) then
            if (printDetail > 0) then
                write(msgoutUnit,'(A,I4,A)') 'Allocating global region with ',nccvs,' CCVs'
            end if
        else
            if (printDetail > 0) then
                write(msgoutUnit,'(A,A,A,I4,A)') 'Allocating region ',trim(regionNumber),' with ',nccvs,' CCVs'
            end if
        endif

        do i=1,20
            if (len_trim(modelVars(i)) > 0) then
                if (printDetail > 0) then
                    write(msgoutUnit,'(A,A)') 'Will use model variable ',trim(modelVars(i))
                end if
            else
                nvars = i - 1
                if (printDetail > 0) then
                    write(msgoutUnit,'(A,I5,A)') 'Using a total of ',nvars,' model variables'
                    exit
                end if
            endif
        enddo

        allocate(this%obsCcvs(nccvs),stat=ierr(1))
        allocate(this%modelCcvs(nccvs),stat=ierr(2))
        allocate(this%modelVars(nvars),stat=ierr(3))

        if (hasNlSpline) then
            allocate(this%ccvSplines(nccvs),stat=ierr(4))
        endif

        if (any(ierr .ne. 0)) then
            call error('Could not allocate the ccvRegion ccvs')
        endif

        do i=1,nvars
            this%modelVars(i) = modelVars(i)
        enddo

        if (isGlobal) then
            regionPrefix = trim(coeffsPath) // 'plat' // platformNum // '_global'
        else
            regionPrefix = trim(coeffsPath) // 'plat' // platformNum // '_rgn' // regionNumber
        endif

        ! initialize the CCVs in this region
        do i=1,nccvs
            write(ccvNumber,'(i2.2)') i

            ! init obs file
            ccvCoeffFile = trim(regionPrefix) // '_ccv' // ccvNumber // '_obs.txt'
            ccvCorrelationCoeffFile = trim(regionPrefix) // '_ccv' // ccvNumber // '_r.txt'

            call radianceCcvInit(this%obsCcvs(i),ccvCoeffFile,ccvCorrelationCoeffFile,OBS_TYPE)

            ! init model file
            ccvCoeffFile = trim(regionPrefix) // '_ccv' // ccvNumber // '_model.txt'
            call radianceCcvInit(this%modelCcvs(i),ccvCoeffFile,ccvCorrelationCoeffFile,MODEL_TYPE)

            if (hasNlSpline) then
                ccvSplineXFile = trim(regionPrefix) // '_ccv' // ccvNumber // '_nl_x.txt'
                ccvSplineYFile = trim(regionPrefix) // '_ccv' // ccvNumber // '_nl_y.txt'

                call ccvSplineInit(this%ccvSplines(i),ccvSplineXFile,ccvSplineYFile)
            endif
        enddo

        modelMeanFile = trim(regionPrefix) // '_mean_model.txt'
        modelStddevFile = trim(regionPrefix) // '_stddev_model.txt'
        obsMeanFile = trim(regionPrefix) // '_mean_obs.txt'
        obsStddevFile = trim(regionPrefix) // '_stddev_obs.txt'
        generatorFile = trim(regionPrefix) // '_generator.txt'

        call countLines(modelMeanFile, lines)
        allocate(this%modelMean(lines),stat=ierr(1))

        call countLines(modelStddevFile, lines)
        allocate(this%modelStddev(lines),stat=ierr(2))

        call countLines(obsMeanFile, lines)
        allocate(this%obsMean(lines),stat=ierr(3))

        call countLines(obsStddevFile, lines)
        allocate(this%obsStddev(lines),stat=ierr(4))

        if (.not. isGlobal) then
            call countLines(generatorFile, lines)
            allocate(this%generator(lines),stat=ierr(5))
        end if

        open(42,file=trim(modelMeanFile))
        read(42,*) this%modelMean
        close(42)
        open(42,file=trim(modelStddevFile))
        read(42,*) this%modelStddev
        close(42)
        open(42,file=trim(obsMeanFile))
        read(42,*) this%obsMean
        close(42)
        open(42,file=trim(obsStddevFile))
        read(42,*) this%obsStddev
        close(42)
        if (.not. isGlobal) then
            open(42,file=trim(generatorFile))
            read(42,*) this%generator
            close(42)
        end if

        if (any(ierr .ne. 0)) then
            call error('Could not allocate the ccvRegion mean/stddevs')
        endif

    end subroutine ccvRegionInit

    ! initialize platform object
    subroutine platformInit(this,configPath,coeffsPath,platformNum,printDetail,msgoutUnit)
        implicit none

        ! input/output variables
        type(platform), intent(out) :: this
        character(len=*) configPath, coeffsPath
        character(2), intent(in) :: platformNum
        integer, intent(in) :: printDetail,msgoutUnit

        ! namelist variables
        integer :: nregions, nchannels
        logical :: hasGlobal
        character(1024) :: platformName
        namelist /platform_config/ nregions, platformName, nchannels, hasGlobal

        ! local variables
        integer :: i,ierr
        character(2) :: regionNumber

        ! read from namelist
        open(10,file=trim(configPath) // 'plat' // platformNum // '.cfg')
        read(10,nml=platform_config)
        close(10)

        this%platformName = platformName
        this%nchannels = nchannels

        if (printDetail > 0) then
            write(msgoutUnit,'(A,A)') 'Allocating platform ',trim(this%platformName)
        end if

        if (hasGlobal) then
            allocate(this%globalRegion,stat=ierr)

            if (ierr .ne. 0) then
                call error('Could not allocate the platform global region')
            endif

            call ccvRegionInit(this%globalRegion,configPath,coeffsPath,platformNum,'-1',&
                .True.,printDetail,msgoutUnit)
        endif

        allocate(this%regions(nregions),stat=ierr)

        if (ierr .ne. 0) then
            call error('Could not allocate the platform regions')
        endif

        do i=1,nregions
            write(regionNumber,'(i2.2)') i
            call ccvRegionInit(this%regions(i),configPath,coeffsPath,platformNum,regionNumber,&
                .False.,printDetail,msgoutUnit)
        enddo

    end subroutine platformInit

    ! initialize all platform objects
    subroutine radianceCcvManagerInit(this,nplat,configPath,coeffsPath,printDetail,msgoutUnit)
        implicit none

        class(radianceCcvManager) :: this
        integer, intent(in) :: nplat
        character(len=*), intent(in) :: configPath, coeffsPath
        integer, intent(in) :: printDetail, msgoutUnit

        ! local variables
        integer :: ierr, i
        character(2) :: platformNum

        if (printDetail > 0) then
            write(msgoutUnit,'(A,I5,A,A)') 'Allocating ',nplat,' platforms from ',adjustl(trim(configPath))
        end if

        allocate(this%platforms(nplat),stat=ierr)

        if (ierr .ne. 0) then
            call error('Could not allocate the platform regions')
        endif

        do i=1,nplat
            write(platformNum,'(i2.2)') i
            call platformInit(this%platforms(i),configPath,coeffsPath,platformNum,printDetail,msgoutUnit)
        enddo

        this%initialized = .true.

    end subroutine

    function getNumberOfRegions(this,platformNum) result(nregions)

        class(radianceCcvManager) :: this
        integer, intent(in) :: platformNum

        integer :: nregions

        ! local variables
        type(platform) :: pltfrm

        if (platformNum .lt. 1 .or. platformNum .gt. size(this%platforms,1)) then
            write(msgstr,*) 'The platform number',platformNum,'was either less than 1 ',&
                'or greater than the length of the platforms array',size(this%platforms,1)
            call error(msgstr)
        end if

        pltfrm = this%platforms(platformNum)

        nregions = size(pltfrm%regions)
    end function

    subroutine ccvSplineInit(this,ccvSplineXFilename,ccvSplineYFilename)
!======================================================================
!  Calculate the coefficients b(i), c(i), and d(i), i=1,2,...,n
!  for cubic spline interpolation
!  s(x) = y(i) + b(i)*(x-x(i)) + c(i)*(x-x(i))**2 + d(i)*(x-x(i))**3
!  for  x(i) <= x <= x(i+1)
!  Alex G: January 2010
!----------------------------------------------------------------------
!  input..
!  x = the arrays of data abscissas (in strictly increasing order)
!  y = the arrays of data ordinates
!  n = size of the arrays xi() and yi() (n>=2)
!  output..
!  b, c, d  = arrays of spline coefficients
! adapted from
! http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch01/spline.f90
!======================================================================
        implicit none
        type(ccvSpline), intent(out) :: this
        character(1024), intent(in) :: ccvSplineXFilename, ccvSplineYFilename
        integer, dimension(10) ::  ierr

        integer n
        integer i, j, gap
        double precision h

        call countLines(ccvSplineXFilename,n)

        if (n .eq. 0) then
            write(msgstr,*) 'The file ' // trim(ccvSplineXFilename) // ' was empty.'
            call error(msgstr)
        endif

        ierr = 0

        allocate(this%xi(n),stat=ierr(1))
        allocate(this%yi(n),stat=ierr(2))
        allocate(this%b(n),stat=ierr(3))
        allocate(this%c(n),stat=ierr(4))
        allocate(this%d(n),stat=ierr(5))

        ! read in the x values
        open(42,file=trim(ccvSplineXFilename))
        read(42,*) this%xi
        close(42)

        ! read in the y values
        open(42,file=trim(ccvSplineYFilename))
        read(42,*) this%yi
        close(42)

        if (any(ierr .ne. 0)) then
            write(msgstr,*) 'Could not allocate the ccvSpline arrays'
            call error(msgstr)
        endif

        gap = n-1
        ! check input
        if ( n < 2 ) return
        if ( n < 3 ) then
          ! linear interpolation
          this%b(1) = (this%yi(2)-this%yi(1))/(this%xi(2)-this%xi(1))
          this%c(1) = 0.
          this%d(1) = 0.
          this%b(2) = this%b(1)
          this%c(2) = 0.
          this%d(2) = 0.
          return
        end if
        !
        ! step 1: preparation
        !
        this%d(1) = this%xi(2) - this%xi(1)
        this%c(2) = (this%yi(2) - this%yi(1))/this%d(1)
        do i = 2, gap
          this%d(i) = this%xi(i+1) - this%xi(i)
          this%b(i) = 2.0*(this%d(i-1) + this%d(i))
          this%c(i+1) = (this%yi(i+1) - this%yi(i))/this%d(i)
          this%c(i) = this%c(i+1) - this%c(i)
        end do
        !
        ! step 2: end conditions
        !
        this%b(1) = -this%d(1)
        this%b(n) = -this%d(n-1)
        this%c(1) = 0.0
        this%c(n) = 0.0
        if(n /= 3) then
          this%c(1) = this%c(3)/(this%xi(4)-this%xi(2)) - this%c(2)/(this%xi(3)-this%xi(1))
          this%c(n) = this%c(n-1)/(this%xi(n)-this%xi(n-2)) - this%c(n-2)/(this%xi(n-1)-this%xi(n-3))
          this%c(1) = this%c(1)*this%d(1)**2/(this%xi(4)-this%xi(1))
          this%c(n) = -this%c(n)*this%d(n-1)**2/(this%xi(n)-this%xi(n-3))
        end if
        !
        ! step 3: forward elimination
        !
        do i = 2, n
          h = this%d(i-1)/this%b(i-1)
          this%b(i) = this%b(i) - h*this%d(i-1)
          this%c(i) = this%c(i) - h*this%c(i-1)
        end do
        !
        ! step 4: back substitution
        !
        this%c(n) = this%c(n)/this%b(n)
        do j = 1, gap
          i = n-j
          this%c(i) = (this%c(i) - this%d(i)*this%c(i+1))/this%b(i)
        end do
        !
        ! step 5: compute spline coefficients
        !
        this%b(n) = (this%yi(n) - this%yi(gap))/this%d(gap) + this%d(gap)*(this%c(gap) + 2.0*this%c(n))
        do i = 1, gap
          this%b(i) = (this%yi(i+1) - this%yi(i))/this%d(i) - this%d(i)*(this%c(i+1) + 2.0*this%c(i))
          this%d(i) = (this%c(i+1) - this%c(i))/this%d(i)
          this%c(i) = 3.*this%c(i)
        end do
        this%c(n) = 3.0*this%c(n)
        this%d(n) = this%d(n-1)
    end subroutine

    ! compute the non-linear operator
    function ccvNlSpline(this,u)
!======================================================================
! function ispline evaluates the cubic spline interpolation at point z
! ispline = y(i)+b(i)*(u-x(i))+c(i)*(u-x(i))**2+d(i)*(u-x(i))**3
! where  x(i) <= u <= x(i+1)
!----------------------------------------------------------------------
! input..
! u       = the abscissa at which the spline is to be evaluated
! b, c, d = arrays of spline coefficients computed by spline
! n       = the number of data points
! output:
! ispline = interpolated value at point u
! taken from
! http://ww2.odu.edu/~agodunov/computing/programs/book2/Ch01/spline.f90
!=======================================================================
        implicit none
        type(ccvSpline), intent(in) :: this
        double precision ccvNlSpline
        double precision u
        integer i, j, k, n
        double precision dx

        n = size(this%xi)

        ! if u is ouside the x() interval take a boundary value (left or right)
        if(u <= this%xi(1)) then
           ccvNlSpline = this%yi(1)
          return
        end if
        if(u >= this%xi(n)) then
          ccvNlSpline = this%yi(n)
          return
        end if

        !*
        !  binary search for for i, such that x(i) <= u <= x(i+1)
        !*
        i = 1
        j = n+1
        do while (j > i+1)
          k = (i+j)/2
          if(u < this%xi(k)) then
            j=k
            else
            i=k
           end if
        end do
        !*
        !  evaluate spline interpolation
        !*
        dx = u - this%xi(i)
        ccvNlSpline = this%yi(i) + dx*(this%b(i) + dx*(this%c(i) + dx*this%d(i)))

    end function

    ! deallocate radianceCcv object
    subroutine radianceCcvEnd(this)
        implicit none

        type(radianceCcv), intent(inout) :: this

        deallocate(this%coefficients)
        nullify(this%coefficients)
    end subroutine

    ! deallocate ccvSpline object
    subroutine ccvSplineEnd(this)
        implicit none

        type(ccvSpline), intent(inout) :: this

        deallocate(this%xi)
        deallocate(this%yi)
        deallocate(this%b)
        deallocate(this%c)
        deallocate(this%d)

        nullify(this%xi)
        nullify(this%yi)
        nullify(this%b)
        nullify(this%c)
        nullify(this%d)
    end subroutine

    ! deallocate ccvRegion object
    subroutine ccvRegionEnd(this)
        implicit none

        type(ccvRegion), intent(inout) :: this

        integer :: i

        do i=1,size(this%obsCcvs)
            call radianceCcvEnd(this%obsCcvs(i))
        enddo

        do i=1,size(this%modelCcvs)
            call radianceCcvEnd(this%modelCcvs(i))
        enddo

        do i=1,size(this%ccvSplines)
            call ccvSplineEnd(this%ccvSplines(i))
        enddo

        deallocate(this%obsCcvs)
        deallocate(this%modelCcvs)
        deallocate(this%modelVars)
        deallocate(this%ccvSplines)

        deallocate(this%modelMean)
        deallocate(this%modelStddev)
        deallocate(this%obsMean)
        deallocate(this%obsStddev)
        deallocate(this%generator)

        nullify(this%obsCcvs)
        nullify(this%modelCcvs)
        nullify(this%modelVars)
        nullify(this%ccvSplines)

        nullify(this%modelMean)
        nullify(this%modelStddev)
        nullify(this%obsMean)
        nullify(this%obsStddev)
        nullify(this%generator)

    end subroutine ccvRegionEnd

    ! deallocate platform object
    subroutine platformEnd(this)
        implicit none

        type(platform), intent(inout) :: this

        integer :: i

        do i=1,size(this%regions)
            call ccvRegionEnd(this%regions(i))
        enddo

        deallocate(this%regions)
        nullify(this%regions)
    end subroutine platformEnd
end module
