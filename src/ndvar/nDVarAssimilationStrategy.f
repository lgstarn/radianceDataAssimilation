module nDVarAssimilationStrategy_mod
    use iso_fortran_env
    use, intrinsic :: iso_c_binding

    use mpiUtils_mod
    use parallelInfo_mod

    use assimilationStrategy_mod
    use assimilationProblem_mod
    use abstractVector_mod
    use abstractVectorOperator_mod
    use dataSet_mod
    use datasetVectorConverter_mod
!    use optimizer_mod
    use observer_mod
    use penalizer_mod
    use numericalUtils_mod

#include "petsc/finclude/petsctao.h"
    use petscdmda
    use petsctao

    implicit none

#if defined(PETSC_USING_F90) && !defined(PETSC_USE_FORTRANKIND)
    external PETSC_NULL_FUNCTION
#endif

    private
        class(AssimilationProblem), pointer :: theProblem => null()
        real(real64) :: cost0

!        Vec              localX
!        DM               dm
!        PetscReal        param
!        PetscInt         mx, my

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

    subroutine pqdotdd(pinfo,p1,p2,n,dpx,dpy)

        class(ParallelInfo), pointer     :: pinfo
        real(real64),        intent(in)  :: p1(n)
        real(real64),        intent(in)  :: p2(n)
        integer,             intent(in)  :: n
        real(real64),        intent(out) :: dpx
        real(real64),        intent(out) :: dpy

        real(real64) :: allsum(1), dval(1)

        integer :: ierr

        call qdotdd(p1,p2,n,dpx,dpy)

        dval(1) = dpx
        call MPI_AllReduce(dval,allsum,1,MPI_DOUBLE_PRECISION,MPI_SUM,&
            pinfo%getCommunicator(),ierr)
        dpx = allsum(1)

        dval(1) = dpy
        call MPI_AllReduce(dval,allsum,1,MPI_DOUBLE_PRECISION,MPI_SUM,&
            pinfo%getCommunicator(),ierr)
        dpy = allsum(1)
    end subroutine

    subroutine NDVarAssimilationStrategyConstructor(this)
        implicit none

        class(NDVarAssimilationStrategy) :: this

        !allocate(this)
    end subroutine

    subroutine NDVarAssimilationStrategyDestructor(this)
        implicit none

        type(NDVarAssimilationStrategy)  :: this
    end subroutine

    !subroutine funder_ndvar(nctrl,z,costval,grad,iflag)

    subroutine funder_ndvar(tao,z,costval,grad,dummy,ierr)

        implicit none

!        integer, intent(in)                    :: nctrl
!        real(real64), intent(in),  dimension(nctrl) :: z
!        real(real64), intent(out)                   :: costval
!        real(real64), intent(out), dimension(nctrl) :: grad
!        integer, intent(out)                   :: iflag
        Tao              tao
        Vec              z, grad
        PetscReal        costval
        PetscErrorCode   ierr
        PetscInt         dummy

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
        real(real64), dimension(:),    pointer :: yptr      => NULL()
        real(real64), dimension(:),    pointer :: zInPtr    => NULL()
        real(real64), dimension(:),    pointer :: zTmpPtr   => NULL()
        real(real64), dimension(:),    pointer :: gradPtr   => NULL()
        class(ParallelInfo),           pointer :: pinfo     => NULL()

        integer :: i

        real(real64) :: dpx
        real(real64) :: dpy
        real(real64) :: dzcost
        real(real64) :: dycost
        real(real64) :: pencost
        real(real64) :: gradnorm

        real(real64), parameter :: bfactor = 2.0d0

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
        pinfo     => theProblem%getParallelInfo()

        costval = 0.0d0

        call VecGetArrayReadF90(z,zInPtr,ierr)
        call VecGetArrayF90(grad,gradPtr,ierr)

        ! copy the values (NOT the pointer) of the input z vector to the zVector
        call zVector%set1DArray(zInPtr)

        ! get the one-dimensional pointer into the z array
        zTmpPtr => zVector%get1DArrayPtr()

        ! apply B^{1/2} as part of the transform B^{1/2}z = x - x_b
        call bHalf%applyOperator(zVector,xVector)

        ! set x = B^{1/2}z + x_b
        call xVector%add(xbVector, xVector)

        ! move x into the state
        call converter%convertToState(xVector, baseState)

        ! compute the obs - observation operator, y - H(x)
        call observer%getYDiff(baseState, yDiff)

        ! get the 1D representation of R^{-1/2}(y - H(x)) for the cost function
        yptr => ydiff%get1DArrayPtr()

        ! apply R^{-1/2} once for the cost function
        call observer%applyRInvHalf(ydiff, ydiff)

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
        call pqdotdd(pinfo,zInPtr,zInPtr,size(zInPtr),dpx,dpy)

        dzcost = 0.5d0*bfactor*(dpx+dpy)/cost0

        ! compute the \delta y^T \delta y dot product in high precision
        call pqdotdd(pinfo,yptr,yptr,size(yptr),dpx,dpy)

        dycost = 0.5d0*(dpx+dpy)/cost0

        if (associated(penmgr)) then
            call penmgr%getPenalty(zVector, xVector, bHalf, pencost, gradPtr)
            pencost = 0.5*pencost/cost0
            gradPtr = 0.5*gradPtr/cost0
        else
            gradPtr = 0.d0
            pencost = 0.d0
        end if

        costval = dzcost + dycost + pencost

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
        zTmpPtr => zVector%get1DArrayPtr()

        ! we now have our gradient
        ! \nabla J(z) = z - B^{T/2} \mathbf{H}^T R^{-1} (y - H(x)) + \nabla P
        gradPtr = (bfactor*zInPtr - zTmpPtr + gradPtr)/cost0

        call pqdotdd(pinfo,gradPtr,gradPtr,size(gradPtr),dpx,dpy)

        gradnorm = dpx+dpy

        write(msgstr,*) 'cost val:',costval,dzcost,dycost,pencost,gradnorm
        call print(msgstr)

        call VecRestoreArrayReadF90(z,zInPtr,ierr)
        call VecRestoreArrayF90(grad,gradPtr,ierr)
    end subroutine

    ! another nested subroutine
    subroutine alphaTest(pinfo,tao,startAlpha,bestxVec,yVec,gradVec,gradPVec)

        implicit none

        class(ParallelInfo), pointer :: pinfo

        Tao,          intent(in)    :: tao
        real(real64), intent(in)    :: startAlpha
        Vec,          intent(inout) :: bestxVec
        Vec,          intent(inout) :: yVec
        Vec,          intent(inout) :: gradVec
        Vec,          intent(inout) :: gradPVec

        integer :: nctr

        class(Penalizer),              pointer :: penmgr
        class(AbstractVectorOperator), pointer :: bHalf

        logical :: redoTest = .false.
        integer :: npt
        integer, parameter :: m = 5

        real(real64), pointer, dimension(:) :: x, y, z, hdir, grad, gradp
        real(real64), pointer, dimension(:,:) :: sk, yk
        real(real64), pointer, dimension(:) :: alphai

        real(real64), pointer :: bestx(:)

        real(real64) :: costval, costvalp, alpha, bestAlpha, testval, prod, bestCostval
        real(real64) :: calpha, talpha, t, rhok, h0k, betak, gnorm, dpx, dpy

        integer :: i, k, kcur, kmax, iflag, ierr, iter, riter, niter, nfe, istart, nrst

        call VecGetArrayF90(bestxVec,bestx,ierr)
        call VecGetArrayF90(yVec,    y,    ierr)
        call VecGetArrayF90(gradVec, grad, ierr)
        call VecGetArrayF90(gradPVec,gradp,ierr)

        nctr = size(bestx)

        bHalf     => theProblem%getBHalfOperator()
        penmgr    => theProblem%getPenalizer()

        ! allocate local variables
        allocate (x(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for x"

        allocate (z(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for z"

        allocate (hdir(nctr), STAT=ierr)
        if(ierr /= 0) write(*,*) "allocation error for hdir"

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
                !call funder_ndvar(nctr,bestx,costval,grad,iflag)
                call funder_ndvar(tao,bestxVec,costval,gradVec,0,iflag)

                call pqdotdd(pinfo,grad,grad,nctr,dpx,dpy)

                hdir = -grad / sqrt(dpx+dpy)

                write(msgstr,*) 'i=',0,'alpha = ',0.d0,', alpha test:',1.d0, &
                'O(alpha): n/a, cost:',costval,' grad norm:',sqrt(dpx+dpy)
                call print(msgstr)
                bestAlpha = 0
                bestCostval = costval
                kcur = 1
                riter = 1
            else
                call pqdotdd(pinfo,grad,grad,nctr,dpx,dpy)

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
                    call pqdotdd(pinfo,sk(:,k),yk(:,k),nctr,dpx,dpy)

                    rhok = dpx+dpy
                    if (i == 1) then
                        call pqdotdd(pinfo,yk(:,k),yk(:,k),nctr,dpx,dpy)
                        h0k = rhok/(dpx+dpy)
                        kcur = modulo(k,m)+1
                    end if
                    call pqdotdd(pinfo,sk(:,k),gradp(:),nctr,dpx,dpy)
                    alphai(k) = (dpx+dpy)/rhok
                    gradp = gradp - alphai(k)*y(k)
                end do

                z= h0k*gradp

                do i=kmax,1,-1
                    k = modulo(riter-i-1,m)+1
                    call pqdotdd(pinfo,yk(:,k),z(:),nctr,dpx,dpy)
                    betak = dpx + dpy
                    call pqdotdd(pinfo,sk(:,k),yk(:,k),nctr,dpx,dpy)
                    betak = betak/(dpx+dpy)

                    z = z + (alphai(k)-betak)*sk(:,k)
                end do

                call pqdotdd(pinfo,z(:),z(:),nctr,dpx,dpy)

                hdir = -z / sqrt(dpx+dpy)
            end if

            !if (riter == 1 .or. talpha .lt. 1d-3) then
            alpha = startAlpha
            !else
            !    alpha = talpha
            !end if

            ! h^T grad
            call pqdotdd(pinfo,grad,hdir,nctr,dpx,dpy)
            prod = dpx + dpy

            do i = 1,npt+1
                y = x + alpha*hdir
                gradp = 0.d0

                !call funder_ndvar(nctr,y,costvalp,gradp,iflag)
                call funder_ndvar(tao,yVec,costvalp,gradpVec,0,iflag)

                nfe = nfe + 1

                if (costvalp < bestCostval) then
                    bestCostval = costvalp
                    bestAlpha = alpha
                    bestX = y
                end if

                ! \frac{J(x + \alpha h) - J(x)}{\alpha h^T grad}
                testval = (costvalp - costval) / (alpha*prod)

                calpha  = (alpha*prod  + costval - costvalp)/(alpha*alpha)

                call pqdotdd(pinfo,gradp,gradp,nctr,dpx,dpy)

                gnorm = sqrt(dpx+dpy)

                write(msgstr,*) 'iter=',iter,'nfe=',nfe,'alpha=',alpha,', alpha test:',testval, &
                  'O(alpha):',(1-testval)/alpha,' cost:',costvalp,' grad norm:',gnorm
                call print(msgstr)

                if (i == 1) then
                    talpha = prod/(2.d0*calpha)
                end if

                t = prod/(2.d0*calpha)

                write(msgstr,*) 'Optimal alpha:',t,'predicted value:',costval + t*prod - t*t*calpha
                call print(msgstr)

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
        deallocate (z)
        deallocate (sk)
        deallocate (yk)
        deallocate (alphai)
        deallocate (hdir)

        call VecRestoreArrayF90(bestxVec,bestx,ierr)
        call VecRestoreArrayF90(yVec,    y,    ierr)
        call VecRestoreArrayF90(gradVec, grad, ierr)
        call VecRestoreArrayF90(gradpVec,gradp,ierr)
    end subroutine

    subroutine assimilateNDVar(this,pinfo,problem)
        implicit none

        class(NDVarAssimilationStrategy)    :: this
        class(ParallelInfo),        pointer :: pinfo
        class(AssimilationProblem), pointer :: problem
!        class(Optimizer), pointer           :: optimizer

        real(real64), pointer :: z0(:)
        real(real64), pointer :: gradFinal(:)
        real(real64) :: cost
        integer :: nctrl
        integer :: iflag
        real(real64) :: dpx
        real(real64) :: dpy

        Vec :: z0Vec
        Vec :: yVec
        Vec :: gradVec
        Vec :: gradpVec

        integer :: i, ierr

        Tao        tao            ! Tao solver context

        logical :: flg

        !procedure (funder_func), pointer :: funder_ptr

        !external funder_ndvar

        call PetscInitialize(PETSC_NULL_CHARACTER,ierr)

        if (ierr .ne. 0) then
            print *,'Unable to initialize PETSc'
            stop
        endif

        theProblem => problem

        !funder_ptr => null()

        nctrl = theProblem%getNControl()

        z0 => theProblem%getInitialGuess()
        gradFinal => theProblem%getFinalGrad()

        ! Create TAO solver
        call TaoCreate(PETSC_COMM_WORLD,tao,ierr)
        call TaoSetType(tao,TAOLMVM,ierr)

        call TaoSetObjectiveAndGradientRoutine(tao,funder_ndvar,0,ierr)
        call TaoSetMaximumFunctionEvaluations(tao,100,ierr)

        call VecCreateMPIWithArray(PETSC_COMM_WORLD,1,size(z0),PETSC_DECIDE,z0,z0Vec,ierr)
        call VecCreateMPIWithArray(PETSC_COMM_WORLD,1,size(gradFinal),PETSC_DECIDE,gradFinal,gradVec,ierr)

        cost0 = 1.d0

        if (theProblem%shouldDoAlphaTest()) then
            call VecCreateMPI(PETSC_COMM_WORLD,size(z0),PETSC_DECIDE,gradpVec,ierr)
            call VecCreateMPI(PETSC_COMM_WORLD,size(z0),PETSC_DECIDE,yVec,    ierr)
            call alphaTest(pinfo,tao,1000.d0,z0Vec,yVec,gradVec,gradpVec)

            write(msgstr,*) 'Finished alpha test'
            call print(msgstr)

            call VecDestroy(gradPVec,ierr)
            call VecDestroy(yVec,ierr)
        end if

        !call funder_ndvar(nctrl,z0,cost,gradFinal,iflag)
        call funder_ndvar(tao,z0Vec,cost,gradVec,0,iflag)

        cost0 = cost

        call TaoSetInitialVector(tao,z0Vec,ierr)

        !call PetscOptionsHasName(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,   &
        !                       & '-testmonitor',flg,ierr)
        !if (flg) then
            ! call TaoSetMonitor(tao,Monitor,dummy,PETSC_NULL_FUNCTION,ierr)
        ! endif

        !call PetscOptionsHasName(PETSC_NULL_OPTIONS,PETSC_NULL_CHARACTER,  &
        !                        & '-testconvergence',flg, ierr)
        ! if (flg) then
            ! call TaoSetConvergenceTest(tao,ConvergenceTest,dummy, ierr)
        ! endif

        ! Check for any TAO command line options
        call TaoSetFromOptions(tao,ierr)

        ! SOLVE THE APPLICATION
        call TaoSolve(tao,ierr)

        ! call TaoView(tao,PETSC_COMM_WORLD,ierr)

        call funder_ndvar(tao,z0Vec,cost,gradVec,0,iflag)

        call VecGetArrayF90(gradVec,gradFinal,ierr)
        call pqdotdd(pinfo,gradFinal,gradFinal,nctrl,dpx,dpy)
        call VecRestoreArrayF90(gradVec,gradFinal,ierr)

        write(msgstr,*) 'Final cost is: ',cost*cost0,'Final gnorm is:',cost0*sqrt(dpx+dpy)
        call print(msgstr)

        theProblem => null()

        ! Free TAO data structures
        call TaoDestroy(tao,ierr)
        call VecDestroy(z0Vec,ierr)
        call VecDestroy(gradVec,ierr)
    end subroutine
end module
