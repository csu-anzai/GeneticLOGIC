c
c -----------------------------------------------------------------------
c       rnd
c -----------------------------------------------------------------------
c
        integer function rnd( low, high )
        integer low, high
        integer i
        real    d, random
        if ( low .ge. high ) then
                i=low
        else
                i=int(random(d) * (high-low)+low)
                if ( i .gt. high ) i=high
        endif
        rnd=i
        return
        end
