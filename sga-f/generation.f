c
c -----------------------------------------------------------------------
c       generation
c -----------------------------------------------------------------------
c
        subroutine generation ( chromosomes, maxchrom, maxpop, npop,
     &                          parameters,  npp3, parents, npar,
     &                          popsize, lchrom, sumfitness, ncross,
     &                          nmutation, pcross, pmutation,
     &                          parms, iparms, np, nprow, obj )
c
        integer         maxchrom, maxpop, npop, npp3, npar
        integer         lchrom, popsize, np, nprow
        integer         ncross, nmutation
        real            pcross, pmutation, sumfitness, obj
        byte            chromosomes ( maxchrom, maxpop, npop )
        real            parameters  ( npp3,     maxpop, npop )
        integer         parents     ( npar,     maxpop, npop )
        integer         iparms(np,nprow)
        real            parms (np,nprow)
c
        integer j, mate1, mate2, jcross, select
	real    ff
c
c
        j       = 1
c
c               get the two parent pointer
c
1       mate1   = select ( parameters,  npp3, maxpop, npop,
     &                     popsize, sumfitness, 1 )
        mate2   = select ( parameters,  npp3, maxpop, npop,
     &                     popsize, sumfitness, 1 )
c
c               do crossover including mutation
c
        call crossover ( chromosomes(1,mate1,1), chromosomes(1,mate2,1),
     &                   chromosomes(1,j,2), chromosomes(1,j+1,2),
     &                   lchrom, ncross, nmutation, jcross,
     &                   pcross, pmutation )
c
c               decode and store parameters
c
        call decodeparm ( parms , iparms, np, nprow,
     &                    chromosomes(1,j,2), lchrom )
	ff	= obj ( NP, parms )
        call storegen ( 2, j, parms, np, ff, 0.0, 0.0,
     &                  mate1, mate2, jcross,
     &                  parameters, npp3, maxpop, npop,
     &                  parents, npar )
        call decodeparm ( parms , iparms, np, nprow,
     &                    chromosomes(1,j+1,2), lchrom )
	ff	= obj ( NP, parms )
        call storegen ( 2, j+1, parms, np, ff, 0.0, 0.0,
     &                  mate1, mate2, jcross,
     &                  parameters, npp3, maxpop, npop,
     &                  parents, npar )
c
c               initiate next pair
c
        j       = j + 2
        if ( j .le. popsize ) goto 1
        return
        end
