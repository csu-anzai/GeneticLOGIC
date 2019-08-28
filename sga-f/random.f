c
c -----------------------------------------------------------------------
c       random
c -----------------------------------------------------------------------
c
        real function random(d)
        real d
        real ran2,ranx
        integer irg
        common /ranco/ irg
c
1       ranx=ran2(irg)
        if (ranx .le. 0.0 .or. ranx .ge. 1.0 ) goto 1
        random = ranx
        return
        end
