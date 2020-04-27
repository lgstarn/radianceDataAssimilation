!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! Fortran 90 routines for computing a dotproduct q = v'*w,
! where v and w are double precision n-vectors
! and q is a quadruple precision scalar.
! In the past this would be called dotproduct accumulation.
!
! With declarations
!       integer  :: n
!       real(8)  :: v(1:n), w(1:n), x, y
!       real(16) :: q
! the code
!       call qdotdd( v,w,n,x,y )
!       q = real(x,qp) + real(y,qp)
! gives the dotproduct q = v'*w in quad precision using only real(8) floating-point.
!
! qdotddModule uses functions developed by Stef Graillat and Valerie Menissier-Morain,
! "Accurate summation, dot product and polynomial evaluation in complex
! floating point arithmetic", Information and Computation 216 (2012) 57--71.
!
! Ding Ma and Michael Saunders, MS&E, Stanford University.
! dingma@stanford.edu, saunders@stanford.edu
!
! 21 Sep 2014: First version of qdotddModule.f90.
!              The subroutines in qdotddModule were developed for this presentation:
!              Ding Ma and Michael Saunders,
!              Experiments with quad precision for iterative solvers,
!              SIAM Conference on Optimization, San Diego, CA, May 19-22, 2014.
!              http://stanford.edu/group/SOL/multiscale/talks/14sioptQuadLSMR.pdf
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
module numericalUtils_mod
    
    implicit none

    private
    public             :: qdotdd
    private            :: twosum, split, twoproduct, sum2, sum2i, qsum2i, dot2

    integer, parameter :: ip = 4, dp = 8, qp = 16

    contains

    pure subroutine twosum( a,b,x,y )
        implicit none

        real(dp), intent(in)    :: a,b
        real(dp), intent(out)   :: x,y
        
        ! twosum evaluates x and y such that a+b = x+y.
        ! If x+y is evaluated as qsum = real(x,qp) + real(y,qp),
        ! qsum will be accurate to quad precision.
        
        real(dp)                :: z
        
        x = a+b
        z = x-a
        y = (a - (x-z)) + (b-z)
    end subroutine twosum

    pure subroutine split( a,x,y )
        implicit none

        real(dp), intent(in)    :: a
        real(dp), intent(out)   :: x,y

        ! split computes x and y such that a = x+y.

        real(dp)                :: c
        real(dp), parameter     :: factor = real(2**27,dp) + 1_dp

        ! write(*,*)         'factor', factor
        ! write(*,'(a,z40)') 'factor', factor
        c = factor*a
        x = c - (c-a)
        y = a - x
    end subroutine split

    pure subroutine twoproduct( a,b,x,y )
        implicit none

        real(dp), intent(in)    :: a,b
        real(dp), intent(out)   :: x,y

        ! twoproduct computes x and y such that a*b = x+y.
        ! If x+y is evaluated as qsum2 = real(x,qp) + real(y,qp),
        ! qsum2 will be accurate to quad precision.

        real(dp)                :: a1, a2, b1, b2

        x = a*b
        call split( a,a1,a2 )
        call split( b,b1,b2 )
        y = a2*b2 - (((x - a1*b1) - a2*b1) - a1*b2)
    end subroutine twoproduct

    pure subroutine sum2( v,n,x,y )
        implicit none

        integer(ip), intent(in)    :: n
        real(dp),    intent(in)    :: v(n)
        real(dp),    intent(out)   :: x, y

        ! sum2 sums the elements v(1:n).
        ! The sum is dsum = x+y.
        ! If x+y is evaluated as qsum2 = real(x,qp) + real(y,qp),
        ! qsum2 will be accurate to quad precision.

        integer(ip)                :: i
        real(dp)                   :: a, y1

        x = v(1)
        y = 0_dp
        a = x

        do i = 2, n
            call twosum( a,v(i),x,y1 )
            a = x
            y = y + y1
        end do
    end subroutine sum2

    pure subroutine sum2i( v,n,x,y )

        implicit none

        integer(ip), intent(in)    :: n
        real(dp),    intent(in)    :: v(n)
        real(dp),    intent(out)   :: x, y

        ! sum2i is the inline version of sum2.
        ! It sums the elements v(1:n).
        ! The sum is dsum = x+y.
        ! If x+y is evaluated as qsum2 = real(x,qp) + real(y,qp),
        ! qsum2 will be accurate to quad precision.

        integer(ip)  :: i
        real(dp)     :: a, b, y1, z

        x = v(1)
        y = 0_dp
        a = x

        do i = 2, n
            ! call twosum( a,v(i),x,y1 )
            b = v(i)
            x = a+b
            z = x-a
            y1= (a - (x-z)) + (b-z)

            ! continue sum2
            a = x
            y = y + y1
        end do
    end subroutine sum2i

    pure subroutine qsum2i( v,n,x,y )
        implicit none

        integer(ip), intent(in)    :: n
        real(dp),    intent(in)    :: v(n)
        real(qp),    intent(out)   :: x, y

        ! qsum2i is a quad version of sum2i.
        ! It sums the elements v(1:n).
        ! The sum is qsum = x+y.
        ! This is just for checking sum2i.

        integer(ip)  :: i
        real(qp)     :: a, b, y1, z

        x = v(1)
        y = 0
        a = x

        do i = 2, n
            ! call twosum( a,v(i),x,y1 )
            b = v(i)
            x = a+b
            z = x-a
            y1= (a - (x-z)) + (b-z)

            ! continue sum2
            a = x
            y = y + y1
        end do
    end subroutine qsum2i

    pure subroutine dot2( v,w,n,x,y )

        implicit none

        integer(ip), intent(in)    :: n
        real(dp),    intent(in)    :: v(n), w(n)
        real(dp),    intent(out)   :: x, y

        ! dot2 computes the dotproduct v(1:n)'*w(1:n).
        ! The product is dot2 = x+y.
        ! If x+y is evaluated as qdot2 = real(x,qp) + real(y,qp),
        ! qdot2 will be accurate to quad precision.

        integer(ip)  :: i
        real(dp)     :: x1, x2, y1, y2

        call twoproduct( v(1),w(1),x,y )

        do i = 2, n
            call twoproduct( v(i),w(i),x1,y1 )
            call twosum( x,x1,x2,y2 )
            x = x2
            y = y + (y1+y2)
        end do
    end subroutine dot2

    pure subroutine qdotdd( v,w,n,x,y )
        implicit none

        integer(ip), intent(in)    :: n
        real(dp),    intent(in)    :: v(n), w(n)
        real(dp),    intent(out)   :: x, y

        ! qdotdd computes the dotproduct v(1:n)'*w(1:n).
        ! It is an inline version of dot2.
        ! The dotproduct is x+y.
        ! If x+y is evaluated as qdot = real(x,qp) + real(y,qp),
        ! qdot will be accurate to quad precision.

        integer(ip)  :: i
        real(dp)     :: a, a1, a2, b, b1, b2, c, x1, x2, y1, y2, z
        real(dp), parameter     :: factor = real(2**27,dp) + 1_dp

        ! call twoproduct( v(1),w(1),x,y )

        a  = v(1)
        b  = w(1)
        x  = a*b

        c  = factor*a
        a1 = c - (c-a)
        a2 = a - a1

        c  = factor*b
        b1 = c - (c-b)
        b2 = b - b1

        y  = a2*b2 - (((x - a1*b1) - a2*b1) - a1*b2)

        do i = 2, n
            ! call twoproduct( v(i),w(i),x1,y1 )
            a  = v(i)
            b  = w(i)
            x1 = a*b

            c  = factor*a
            a1 = c - (c-a)
            a2 = a - a1

            c  = factor*b
            b1 = c - (c-b)
            b2 = b - b1
            y1 = a2*b2 - (((x1 - a1*b1) - a2*b1) - a1*b2)

            ! call twosum( x,x1,x2,y2 )
            x2 = x + x1
            z  = x2 - x
            y2 = (x - (x2-z)) + (x1-z)

            x = x2
            y = y + (y1+y2)
        end do
    end subroutine qdotdd
end module
