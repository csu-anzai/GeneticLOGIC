c
c -----------------------------------------------------------------------
c       decodeparm
c -----------------------------------------------------------------------
c
        subroutine decodeparm ( parms, iparms, nparms, nprow,
     &          chromosome, lchrom )
c
c               calculate fitness from chromosom
c
        integer         iparms  (nparms, nprow )
        real            parms   (nparms, nprow )
        byte            chromosome ( lchrom )
        integer         nparms, nprow,lchrom
c
        integer         j,jposition,lparm,min0
        real            x,gdecode
c
        j               = 1
        jposition       = 1
1       lparm           = iparms( j, 2 )
        if ( lparm .gt. 0) then
                x       = gdecode( chromosome ( jposition ),
     &                          min0(lparm,lchrom-jposition+1) )
                jposition       = jposition + lparm
                parms ( j, 1 )  = parms ( j,3 ) +
     &                  (parms( j,4 ) - parms( j,3 )) * x
     &                          / (2.0**float(lparm)-1.0)
        else
                parms ( j,1 )   = 0.0
        endif
        j       = j + 1
        if ( j .le. nparms ) goto 1
        return
        end
