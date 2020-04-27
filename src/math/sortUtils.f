module sortUtils_mod
    use iso_fortran_env

    implicit none

    interface quicksort
        module procedure quicksort_byte
        module procedure quicksort_short
        module procedure quicksort_int
        module procedure quicksort_long
        module procedure quicksort_real
        module procedure quicksort_dble
    end interface

    contains

    recursive subroutine quicksort_byte(a, first, last)

        implicit none

        integer(int8) :: a(*), x, t

        include 'qsort.incl'
    end subroutine

    recursive subroutine quicksort_short(a, first, last)

        implicit none

        integer(int16) :: a(*), x, t

        include 'qsort.incl'
    end subroutine

    recursive subroutine quicksort_int(a, first, last)

        implicit none

        integer(int32) :: a(*), x, t

        include 'qsort.incl'
    end subroutine

    recursive subroutine quicksort_long(a, first, last)

        implicit none

        integer(int64) :: a(*), x, t

        include 'qsort.incl'
    end subroutine

    recursive subroutine quicksort_real(a, first, last)

        implicit none

        real(real32) :: a(*), x, t

        include 'qsort.incl'
    end subroutine

    recursive subroutine quicksort_dble(a, first, last)

        implicit none

        real(real64) :: a(*), x, t

        include 'qsort.incl'
    end subroutine


end module
