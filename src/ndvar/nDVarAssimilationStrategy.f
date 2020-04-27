module nDVarAssimilationStrategy_mod
    use assimilationStrategy_mod
    use assimilationProblem_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use dataSet_mod
    use datasetVectorConverter_mod
    use optimizer_mod
    use observer_mod
    use penalizer_mod
    use numericalUtils_mod
    use mpiUtils_mod

    implicit none

    private
        class(AssimilationProblem), pointer :: theProblem => null()
        real(8) :: cost0

    type, extends(AssimilationStrategy), public :: NDVarAssimilationStrategy
        private

        contains
            procedure :: assimilate => assimilateNDVar
            procedure :: NDVarAssimilationStrategyConstructor
            final :: NDVarAssimilationStrategyDestructor ! clean up all allocated variables
    end type

!    interface NDVarAssimilationStrategy
!        procedure NDVarAssimilationStrategyConstructor ! allow generic instantiation
!    end interface

    contains

    subroutine NDVarAssimilationStrategyConstructor(this)
        implicit none

        class(NDVarAssimilationStrategy) :: this

        !allocate(this)
    end subroutine

    subroutine NDVarAssimilationStrategyDestructor(this)
        implicit none

        type(NDVarAssimilationStrategy)  :: this
    end subroutine

    subroutine funder_ndvar(nctrl,z,costval,grad,iflag)

        implicit none

        integer, intent(in)                    :: nctrl
        real(8), intent(in),  dimension(nctrl) :: z
        real(8), intent(out)                   :: costval
        real(8), intent(out), dimension(nctrl) :: grad
        integer, intent(out)                   :: iflag

        class(DatasetVectorConverter), pointer :: converter => NULL()
        class(Observer),               pointer :: observer  => NULL()
        class(Penalizer),              pointer :: penmgr    => NULL()
        class(AbstractVector),         pointer :: zVector   => NULL()
        class(AbstractVector),         pointer :: xVector   => NULL()
        class(AbstractVector),         pointer :: xbVector  => NULL()
        class(AbstractVector),         pointer :: yDiff     => NULL()
        class(DataSet),                pointer :: baseState => NULL()
        class(DataSet),                pointer :: deltaX    => NULL()
        class(AbstractVectorOperator), pointer :: bHalf     => NULL()
        class(AbstractVectorOperator), pointer :: rInvHalf  => NULL()
        real(8), dimension(:),         pointer :: yptr      => NULL()
        real(8), dimension(:),         pointer :: zArray    => NULL()

        integer :: i

        real(8) :: dpx
        real(8) :: dpy
        real(8) :: dzcost
        real(8) :: dycost
        real(8) :: pencost

        real(8), parameter :: bfactor = 1.0d0

        ! get the pointers from the options class
        converter => theProblem%getDatasetVectorConverter()
        baseState => theProblem%getBaseDataSet()
        deltaX    => theProblem%getDeltaXDataSet()
        observer  => theProblem%getObserver()
        zVector   => theProblem%getControlVector()
        xVector   => theProblem%getStateVector()
        xbVector  => theProblem%getBackgroundVector()
        yDiff     => theProblem%getObsDiffVector()
        bHalf     => theProblem%getBHalfOperator()
        penmgr    => theProblem%getPenalizer()

        costval = 0.0d0

        ! set the z matrix to the passed in value
        call zVector%set1DArray(z)

        ! apply B^{1/2} as part of the transform B^{1/2}z = x - x_b
        call bHalf%applyOperator(zVector,xVector)

        ! set x = B^{1/2}z + x_b
        call xVector%add(xbVector, xVector)

        ! move x into the state
        call converter%convertToState(xVector, baseState)

        ! compute the obs - observation operator, y - H(x)
        call observer%getYDiff(baseState, yDiff)

        ! apply R^{-1/2} once for the cost function
        call observer%applyRInvHalf(ydiff, ydiff)

        ! get the 1D representation of R^{-1/2}(y - H(x)) for the cost function
        yptr => ydiff%get1DArrayPtr()

        !open(unit=993,file='ydiff_rhalf.txt')

        !do i=1,size(yptr,1)
        !    write(993,'(F12.6,F12.6,I8,I8,I8)') yptr(i)
        !end do

        !close(993)

        !stop

        ! we now have everything we need for the cost function
        ! J(z) = 0.5 z^T z + 0.5 (y - H(x(z)))^T R^{-1} (y - H(x(z))) + P(z)
        !costval = 0.5d0*(bfactor*dot_product(z,z) + dot_product(yptr,yptr) + penval)/cost0

        ! compute the z^T z dot product in high precision
        call qdotdd( z,z,nctrl,dpx,dpy)

        dzcost = 0.5d0*bfactor*(dpx+dpy)/cost0

        ! compute the \delta y^T \delta y dot product in high precision
        call qdotdd(yptr,yptr,size(yptr),dpx,dpy)

        dycost = 0.5d0*(dpx+dpy)/cost0

        if (associated(penmgr)) then
            call penmgr%getPenalty(zVector, xVector, bHalf, pencost, grad)
            pencost = 0.5*pencost/cost0
            grad = 0.5*grad/cost0
        else
            grad = 0.d0
            pencost = 0.d0
        end if

        costval = dzcost + dycost + pencost

        write(msgstr,*) 'cost val:',costval,dzcost,dycost,pencost
        call print(msgstr)

        ! apply R^{-1/2} again for the adjoint to compute R^{-1} (y - H(x(z)))
        call observer%applyRInvHalf(ydiff,ydiff)

        ! call the observation adjoint
        call observer%applyObsAdjoint(ydiff,baseState,deltaX)

        ! move the deltaX state into the x vector
        call converter%convertFromState(deltaX,xVector)

        ! hit deltaX with B^{T/2} to move to the control variable z
        call bHalf%applyTranspose(xVector,zVector)

        ! 4D-Var would also apply a model adjoint here; would require a loop over timesteps as well
        ! call modelM%applyTranspose(xVector, ...)

        ! get the one-dimensional pointer into the z array
        zArray => zVector%get1DArrayPtr()

        ! we now have our gradient
        ! \nabla J(z) = z - B^{T/2} \mathbf{H}^T R^{-1} (y - H(x)) + \nabla P
        grad = (bfactor*z - zArray + grad)/cost0
    end subroutine

    ! another nested subroutine
    subroutine alphaTest(startAlpha,bestx,nctr)

        implicit none

        real(8), intent(in)    :: startAlpha
        integer, intent(in)    :: nctr
        real(8), intent(inout) :: bestx(nctr)

        class(Penalizer),              pointer :: penmgr
        class(AbstractVectorOperator), pointer :: bHalf

        logical :: redoTest = .false.
        integer :: npt
        integer, parameter :: m = 5

        real(8), pointer, dimension(:) :: x, y, z, hdir, grad, gradp
        real(8), pointer, dimension(:,:) :: sk, yk
        real(8), pointer, dimension(:) :: alphai

        real(8) :: costval, costvalp, alpha, bestAlpha, testval, prod, bestCostval
        real(8) :: calpha, talpha, t, rhok, h0k, betak, gnorm, dpx, dpy

        integer :: i, k, kcur, kmax, iflag, ierr, iter, riter, niter, nfe, istart, nrst

        bHalf     => theProblem%getBHalfOperator()
        penmgr    => theProblem%getPenalizer()

        ! allocate local variables
        allocate (x(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for x"

        allocate (y(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for y"

        allocate (z(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for z"

        allocate (hdir(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for hdir"

        allocate (grad(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for grad"

        allocate (gradp(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for gradp"

        allocate (sk(nctr,m), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for sk"

        allocate (yk(nctr,m), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for yk"

        allocate (alphai(m), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for alphai"

        ! execute alpha test
        x = bestx

        if (redoTest) then
            npt = 1
            niter = 1
        else
            npt = 14
            niter = 1
        end if

        riter = 0
        nrst = 0

        t = 0.d0

        do iter=1,niter
            riter = riter + 1
            if (iter == 1 .or. bestCostval < costvalp .or. t .lt. 0.d0) then
                if (iter > 1) then
                    write(msgstr,*) 'FUNCTION VALUE DID NOT DECREASE, RESTARTING'
                    call print(msgstr)
                    nrst = nrst + 1

                    if (nrst > 2) then
                        write(msgstr,*) 'TOO MANY RESTARTS, STOPPING.'
                        call print(msgstr)
                        return
                    end if
                else
                    nfe = 1
                end if
                call funder_ndvar(nctr,bestx,costval,grad,iflag)

                call qdotdd(grad,grad,nctr,dpx,dpy)

                hdir = -grad / sqrt(dpx+dpy)

                write(msgstr,*) 'i=',0,'alpha = ',0.d0,', alpha test:',1.d0, &
                'O(alpha): n/a, cost:',costval,' grad norm:',sqrt(dpx+dpy)
                call print(msgstr)
                bestAlpha = 0
                bestCostval = costval
                kcur = 1
                riter = 1
            else
                call qdotdd(grad,grad,nctr,dpx,dpy)

!                    if (gnorm/sqrt(dpx+dpy) > 1.02d0) then
!                        print *,'GNORM DID NOT DECREASE, RETURNING'
!                        return
!                    end if

                sk(:,kcur) = alpha*hdir
                yk(:,kcur) = gradp - grad
                costval = costvalp
                x = y
                grad = gradp
                kmax = min(riter-1,m)
                do i=1,kmax
                    k = modulo(riter-i-1,m)+1
                    call qdotdd(sk(:,k),yk(:,k),nctr,dpx,dpy)

                    rhok = dpx+dpy
                    if (i == 1) then
                        call qdotdd(yk(:,k),yk(:,k),nctr,dpx,dpy)
                        h0k = rhok/(dpx+dpy)
                        kcur = modulo(k,m)+1
                    end if
                    call qdotdd(sk(:,k),gradp(:),nctr,dpx,dpy)
                    alphai(k) = (dpx+dpy)/rhok
                    gradp = gradp - alphai(k)*y(k)
                end do

                z= h0k*gradp

                do i=kmax,1,-1
                    k = modulo(riter-i-1,m)+1
                    call qdotdd(yk(:,k),z(:),nctr,dpx,dpy)
                    betak = dpx + dpy
                    call qdotdd(sk(:,k),yk(:,k),nctr,dpx,dpy)
                    betak = betak/(dpx+dpy)

                    z = z + (alphai(k)-betak)*sk(:,k)
                end do

                call qdotdd(z(:),z(:),nctr,dpx,dpy)

                hdir = -z / sqrt(dpx+dpy)
            end if

            !if (riter == 1 .or. talpha .lt. 1d-3) then
            alpha = startAlpha
            !else
            !    alpha = talpha
            !end if

            ! h^T grad
            call qdotdd(grad(:),hdir(:),nctr,dpx,dpy)
            prod = dpx + dpy

            do i = 1,npt+1
                y = x + alpha*hdir
                gradp = 0.d0

                call funder_ndvar(nctr,y,costvalp,gradp,iflag)
                nfe = nfe + 1

                if (costvalp < bestCostval) then
                    bestCostval = costvalp
                    bestAlpha = alpha
                    bestX = y
                end if

                ! \frac{J(x + \alpha h) - J(x)}{\alpha h^T grad}
                testval = (costvalp - costval) / (alpha*prod)

                calpha  = (alpha*prod  + costval - costvalp)/(alpha*alpha)

                call qdotdd(gradp(:),gradp(:),nctr,dpx,dpy)

                gnorm = sqrt(dpx+dpy)

                write(msgstr,*) 'iter=',iter,'nfe=',nfe,'alpha=',alpha,', alpha test:',testval, &
                  'O(alpha):',(1-testval)/alpha,' cost:',costvalp,' grad norm:',gnorm
                call print(msgstr)

                if (i == 1) then
                    talpha = prod/(2.d0*calpha)
                end if

                t = prod/(2.d0*calpha)

                ! print *,'Optimal alpha:',t,'predicted value:',costval + t*prod - t*t*calpha

!                    if (i .eq. 1. .and. riter > 1 .and. costvalp .lt. costval &
!                            &.and. costvalp - (costval - costvalp) .lt. costval + t*prod - t*t*calpha) then
!                        print *,'NOT WORTH IT ', costvalp,costval,costvalp - (costval - costvalp),costval + t*prod - t*t*calpha
!                        exit
!                    else if (i .eq. 1) then
!                        print *,'WORTH IT ', costvalp,costval,costvalp - (costval - costvalp),costval + t*prod - t*t*calpha
!                    end if

                if (i == npt) then
                    alpha = talpha
                else
                    alpha = alpha/10.d0
                end if
            end do
        end do

        ! deallocate local variables
        deallocate (x)
        deallocate (y)
        deallocate (hdir)
        deallocate (grad)
        deallocate (gradp)

        return
    end subroutine

    subroutine assimilateNDVar(this,problem)
        implicit none

        class(NDVarAssimilationStrategy)    :: this
        class(AssimilationProblem), pointer :: problem
        class(Optimizer), pointer           :: optimizer

        real(8), pointer :: z0(:)
        real(8), pointer :: gradFinal(:)
        real(8) :: cost
        integer :: nctrl
        integer :: iflag
        real(8) :: dpx
        real(8) :: dpy

        integer :: i

        procedure (funder_func), pointer :: funder_ptr

        theProblem => problem

        funder_ptr => null()

        nctrl = theProblem%getNControl()

        z0 => theProblem%getInitialGuess()

        cost0 = 1.d0

        if (theProblem%shouldDoAlphaTest()) then
            call alphaTest(10.0d0,z0,nctrl)
        end if

        write(msgstr,*) 'Finished alpha test'
        call print(msgstr)

        optimizer => theProblem%getOptimizer()

        if (.not. associated(optimizer)) then
            call error('Error: NDVar requires an optimizer')
        end if

        funder_ptr => funder_ndvar

        call theProblem%allocFinalGrad()

        gradFinal => theProblem%getFinalGrad()
        ! set the problem at the module level so it can be read by the funder_ndvar method.
        ! it would be preferable to have this be a contained subroutine in this subroutine,
        ! but gfortran seems to crash when using a contained subroutine function pointer
        ! (see https://gcc.gnu.org/bugzilla/show_bug.cgi?id=87838)

        call funder_ptr(nctrl,z0,cost,gradFinal,iflag)

        cost0 = cost

        call optimizer%optimize(funder_ptr,nctrl,z0,cost,gradFinal,iflag)

        call funder_ptr(nctrl,z0,cost,gradFinal,iflag)

        call qdotdd( gradFinal,gradFinal,nctrl,dpx,dpy)

        write(msgstr,*) 'Final cost is: ',cost*cost0,'Final gnorm is:',cost0*sqrt(dpx+dpy)
        call print(msgstr)

        deallocate(gradFinal)

        theProblem => null()
    end subroutine
end module
