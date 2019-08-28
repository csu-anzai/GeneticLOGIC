c
c -----------------------------------------------------------------------
c       flip
c -----------------------------------------------------------------------
c
        integer function flip ( prob )
        real            prob, rr, random,d
        common /ranco/  d
        flip    = 0
        if ( prob .eq. 1.0) then
                flip    = 1
        else
                rr=random(d)
                if ( rr .lt. prob) flip = 1
        endif
        return
        end
