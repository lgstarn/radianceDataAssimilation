module optimizerFactory_mod

    use optimizer_mod
    use lmbmOptimizer_mod
    use lbfgsOptimizer_mod
    use mpiUtils_mod

    implicit none

    ! Optimization strategies
    character(len=256), parameter, public :: LMBM_OPTIMIZER  = 'LMBM'
    character(len=256), parameter, public :: LBFGS_OPTIMIZER = 'LBFGS'

    contains

    function getOptimizer(optimizerName,nctrl,maxiter) result(opt)
        implicit none

        character(len=256), intent(in)     :: optimizerName
        integer, intent(in)                :: nctrl, maxiter

        class(Optimizer), pointer          :: opt
        class(LmbmOptimizer), pointer      :: optLmbm
        class(LbfgsOptimizer), pointer     :: optLbfgs

        select case (optimizerName)
            case (LMBM_OPTIMIZER)
                allocate(optLmbm)
                call optLmbm%lmbmOptimizerConstructor(nctrl,maxiter)
                opt => optLmbm
            case (LBFGS_OPTIMIZER)
                allocate(optLbfgs)
                call optLbfgs%lbfgsOptimizerConstructor(nctrl,maxiter)
                opt => optLbfgs
            case default
                write(msgstr,*) 'Unknown optimizer ',trim(optimizerName)
                call error(msgstr)
        end select
    end function
end module
