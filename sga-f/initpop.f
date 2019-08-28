c
c -----------------------------------------------------------------------
c       initpop
c -----------------------------------------------------------------------
c
        subroutine initpop ( chromosomes, maxchrom, maxpop, npop,
     &                       parameters,  npp3, parents, npar,
     &                       lchrom, popsize, bias,
     &                       parms, iparms, np, nprow, obj )
c
        integer         maxchrom, maxpop, npop, npp3, npar
        integer         lchrom, popsize, np, nprow
        real            bias, obj
        byte            chromosomes ( maxchrom, maxpop, npop )
        real            parameters  ( npp3,     maxpop, npop )
        integer         parents     ( npar,     maxpop, npop )
        integer         iparms(np,nprow)
        real            parms (np,nprow)
        REAL FF
c
        integer         j, j1, flip

c
        do j=1,popsize
                do j1=1,lchrom
                        if ( flip( bias ) .eq. 1 ) then
                                chromosomes(j1,j,1) = 1
                        else
                                chromosomes(j1,j,1) = 0
                        endif
                enddo
                call decodeparm ( parms, iparms, np, nprow,
     &                  chromosomes(1,j,1), lchrom )
                ff      = obj( NP, parms )
                call storegen ( 1, j, parms, np, ff, 0.0, 0.0, 0, 0, 0,
     &                          parameters, npp3, maxpop, npop,
     &                          parents , npar )
        enddo

        end
