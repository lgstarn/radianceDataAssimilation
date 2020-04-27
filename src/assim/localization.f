module localization_mod
    implicit none

    private

    public :: covfac_gaspari_cohn

    contains

    function covfac_gaspari_cohn( z, c ) result(covfac)
        ! covfac_gaspari_cohn: Compute covariance factor from Gaspari and Cohn (QJRMS, 1999)
        !
        !  z: distance
        !  c: cut-off distance

        implicit none

        real(8), intent(in) :: z, c
        real(8) :: covfac

        real(8) :: ch, r

        if ((z .eq. 0) .or. (c .eq. 0)) then
           covfac = 1.0d0
        else
            ch = 0.5d0*c
            r = z/ch

            if (z .gt. c) then
               covfac = 0.0d0
            else if (z .gt. ch .and. z .lt. c) then
               covfac = ((((r/12.d0-0.5d0)*r+0.625d0)*r+5.d0/3.d0)*r-5.d0)*r+4.d0-2.d0/(3.d0*r)
            else
               covfac = (((-0.25d0*r+0.5d0)*r+0.625d0)*r-5.d0/3.d0)*r**2+1.d0
            end if
        end if
    end function
end module
