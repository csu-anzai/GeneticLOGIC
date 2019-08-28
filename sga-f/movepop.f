c
c -----------------------------------------------------------------------
c       movepop
c -----------------------------------------------------------------------
c
        subroutine movepop( from, to,
     &                      chromosomes, maxchrom, maxpop, npop,
     &                      parameters,  npp3, parents, npar )
c
        integer         from, to, maxchrom, maxpop, npop, npp3, npar
        byte            chromosomes ( maxchrom, maxpop, npop )
        real            parameters  ( npp3,     maxpop, npop )
        integer         parents     ( npar,     maxpop, npop )
c
        integer         i, j
c
        do j=1,maxpop
                do i=1,maxchrom
                        chromosomes( i, j, to ) =
     &                           chromosomes( i, j, from )
                enddo
                do i=1,npp3
                        parameters( i, j, to ) =
     &                          parameters( i, j, from )
                enddo
                do i=1,npar
                        parents( i, j, to ) =
     &                          parents( i, j, from )
                enddo
        end do

        end
