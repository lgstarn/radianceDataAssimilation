module noiseUtils_mod

    use iso_fortran_env

    implicit none

    real(real64), parameter :: TAU = 8*atan(1.0d0)

    interface addStandardGaussianNoise
        module procedure addStandardGaussianNoise_dble, &
                         addStandardGaussianNoise_real
    end interface

    contains

    subroutine addStandardGaussianNoise_dble(n,x,sd,mean)
        implicit none

        integer,      intent(in)                   :: n
        real(real64), intent(inout), dimension(n)  :: x
        real(real64), intent(in),    optional      :: sd
        real(real64), intent(in),    optional      :: mean

        real(real64) :: sd1, mean1
        real(real64), dimension(2,1) :: r
        integer :: ij, ind2
        real(real64) :: u, v, f1, f2, x1, x2

        include 'noiseUtils_addStandardGaussianNoise.incl'
    end subroutine

    subroutine addStandardGaussianNoise_real(n,x,sd,mean)
        implicit none

        integer,      intent(in)                   :: n
        real(real32), intent(inout), dimension(n)  :: x
        real(real32), intent(in),    optional      :: sd
        real(real32), intent(in),    optional      :: mean

        real(real32) :: sd1, mean1
        real(real32), dimension(2,1) :: r
        integer :: ij, ind2
        real(real32) :: u, v, f1, f2, x1, x2

        include 'noiseUtils_addStandardGaussianNoise.incl'
    end subroutine
end module
