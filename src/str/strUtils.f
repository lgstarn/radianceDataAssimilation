module strUtils_mod
    implicit none

    contains

    subroutine strsplit(line,dlim,array,icount)

        ! split the line into tokens. The array will be allocated
        ! so it MUST be deallocated by the caller, especially before calling
        ! this method again.

        implicit none

        character(*),           intent(in)  :: line
        character(*),           intent(in)  :: dlim
        character(*), pointer,  intent(out) :: array(:)
        integer,                intent(out) :: icount

        integer :: alen, linelen, dlimlen
        integer :: iarray, idlim, icol, iend, istart, ifound

        icount=0
        linelen = len_trim(line)
        dlimlen = len_trim(dlim)

        istart = 1
        icount = 1

        if (dlimlen > 0) then
            ! do a first pass to find how many delimiters are in the line
            do while (istart <= linelen)
                ifound = index(line(istart:linelen),dlim(1:dlimlen))

                if (ifound == 0) then
                    exit ! the loop
                else
                    ! increment the start and token count
                    istart = istart + ifound + dlimlen - 1
                    icount = icount + 1
                end if
            end do
        end if

        ! didn't find any, so just return the line
        if (icount == 1) then
            allocate(array(1))
            alen = min(len(array(1)),len(line))
            array(1)(1:alen) = line(1:alen)
            return
        end if

        allocate(array(icount))

        istart = 1
        iarray = 1

        ! do a first pass to find how many delimiters are in the line
        do while (istart <= linelen)
            ifound = index(line(istart:linelen),dlim(1:dlimlen))

            if (ifound == 0) then
                array(iarray) = ''
                iend = linelen
                alen = min(len(array(iarray)),iend-istart+1)
                array(iarray)(1:alen) = line(istart:istart+alen-1)
                exit ! the loop
            else
                ! increment the start and token count
                array(iarray) = ''
                iend = istart + ifound
                alen = min(len(array(iarray)),iend-istart-1)
                array(iarray)(1:alen) = line(istart:istart+alen-1)
                istart = istart + ifound + dlimlen - 1
                iarray = iarray + 1
            end if
        end do
    end subroutine

    function str2double(strVal) result(dblVal)
        implicit none

        character(len=*) :: strVal
        real(8)          :: dblVal

        read(strVal,*) dblVal
    end function

end module
