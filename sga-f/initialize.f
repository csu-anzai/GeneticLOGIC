c
c -----------------------------------------------------------------------
c       initialize
c -----------------------------------------------------------------------
c
        subroutine initialize ( chromosomes, maxchrom, maxpop, npop,
     &                          parameters,  npp3, parents, npar,
     &                          lchrom, popsize, maxgen,
     &                          pcross, pmutation, bias,
     &                          parms, iparms, np, nprow,
     &                          fmin, fmax, avg, sumfitness,
     &                          nmutation, ncross, obj,
     &                          iot, nout, iscl )
c
c               Initialization Coordinator for Genetic Algorithm
c
        integer         maxchrom, maxpop, npop, npp3, npar
        integer         lchrom, popsize, maxgen, np, nprow
        integer         nmutation, ncross
        integer         iot, nout, iscl
        real            fmin, fmax, avg, sumfitness
        real            pcross, pmutation, bias, obj
        byte            chromosomes ( maxchrom, maxpop, npop )
        real            parameters  ( npp3,     maxpop, npop )
        integer         parents     ( npar,     maxpop, npop )
        integer         iparms(np,nprow)
        real    	parms (np,nprow)

c
c               initdata ( nur noch eingeschraenkt benoetigt )
c
        nmutation       = 0
        ncross          = 0
c
        call initpop ( chromosomes, maxchrom, maxpop, npop,
     &                 parameters,  npp3, parents, npar,
     &                 lchrom, popsize, bias,
     &                 parms, iparms, np, nprow, obj )

        call statistics ( parameters, npp3, maxpop, npop,
     &                    popsize, fmin, fmax, avg, sumfitness,
     &			  1, iscl )

	call initreport ( popsize, lchrom, maxgen, pcross,
     &                    pmutation, fmin, fmax, avg, sumfitness,
     &                    iot, nout )
        return
        end
