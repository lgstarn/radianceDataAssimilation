module noiseUtils_mod

    implicit none

    real(8), parameter :: TAU = 8*atan(1.0d0)

    contains

    subroutine addGaussianNoise(n,x,sd,mean)
        implicit none

        integer, intent(in)                :: n
        real(8), intent(out), dimension(n) :: x
        real(8), intent(in), optional      :: sd
        real(8), intent(in), optional      :: mean

        real(8) :: sd1, mean1
        real(8), dimension(2,1) :: r
        integer :: ij, ind2
        real(8) :: u, v, f1, f2, x1, x2

        integer :: neven

        if (present(sd)) then
            sd1 = sd
        else
            sd1 = 1.d0
        end if

        if (present(mean)) then
            mean1 = mean
        else
            mean1 = 0.d0
        end if

        do ij = 1,size(x),2
            call random_number(r)

            u = r(1,1)   !! uniform random number in [0,1)
            v = r(2,1)   !! same as above (another sample)

            f1 = sqrt(-2.0*log(max(u,1d-16)))
            f2 = TAU*v

            ind2 = min(ij+1,size(x))

            x1 = x(ij)
            x2 = x(ind2)

            x(ij)   = x1 + sd1*f1*cos(f2) + mean1
            x(ind2) = x2 + sd1*f1*sin(f2) + mean1
        end do
    end subroutine
end module
