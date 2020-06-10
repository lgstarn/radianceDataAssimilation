module asciiUtils_mod

    use iso_fortran_env

    implicit none

    private

    public :: load2DRealFile
    public :: int2str

    contains

    subroutine load2DRealFile(filename,nx,ny,data2d)
        implicit none

        character(len=*), intent(in)  :: filename
        integer,          intent(out) :: nx
        integer,          intent(out) :: ny
        real(real64),     pointer     :: data2d(:,:)

        character(1024) :: line
        integer         :: io

        integer         :: i, j, tmp

        logical :: fileExists

        nx = 0
        ny = 0

        inquire(file=filename,exist=fileExists)

        if (.not. fileExists) then
            print *,'Error: could not read the file ',trim(filename)
            stop
        end if

        open(unit=92, file=filename)
        do
            read(92,'(A)',iostat=io) line
            if (io/=0) exit
            ny = ny + 1
            tmp = ntokens(line)
            if (ny > 1) then
                if (tmp .ne. nx) then
                    print *, 'Error: line ',ny,' had more columns than the previous lines:',tmp,nx
                    stop
                end if
            end if
            nx = tmp
        end do
        close(92)

        allocate(data2d(ny,nx))
        open(unit=92, file=filename)
        do i = 1,ny
            read(92,*) data2d(i,1:nx)
        end do
        close(92)
    end subroutine


    integer function ntokens(line)
        implicit none

        character,intent(in):: line*(*)
        integer i, n, toks

        i = 1
        n = len_trim(line)
        toks = 0
        ntokens = 0
        do while(i <= n)
           do while(line(i:i) == ',')
             i = i + 1
             if (n < i) return
           enddo
           toks = toks + 1
           ntokens = toks
           do
             i = i + 1
             if (n < i) return
             if (line(i:i) == ',') exit
           enddo
        enddo
    end function ntokens

     function int2str(n) result(str)
         implicit none

         integer, intent(in) :: n

         character(:), allocatable :: str

         integer :: intLen

         if (n == 0) then
             intLen = 1
         else
             intLen = floor(log10(abs(dble(n)))) + 1
         end if

         ! add an extra space for the negative sign
         if (n < 0) then
             intLen = intLen + 1
         end if

         allocate(character(len=intLen) :: str)

         write(str,'(I0)') n
     end function
end module
