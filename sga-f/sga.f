c
c -----------------------------------------------------------------------
c       $Id: sga.f,v 1.1 1993/11/24 17:57:23 joke Exp $
c -----------------------------------------------------------------------
c
        subroutine sga ( chromosomes, maxchrom, maxpop, npop,
     &                   parameters,  npp3, parents, npar,
     &                   lchrom, popsize, maxgen,
     &                   pcross, pmutation, bias,
     &                   parms, iparms, np, nprow,
     &                   fbest, xbest, nge, tuse,
     &                   iot, nout, init, tm, gtim, obj, it ,
     &                   iscl )
c
c               A simple genetic Algorithm - SGA - v1.0/f
c
c               multiparameter version ( byte addressing )
c
c
c               chromosomes     ...     chromosomes-workspace (byte)
c               (maxchrom,maxpop,2)     lchrom is multiple of 4 !!
c
c                                 +--------------------+
c                  (,,2)  newpop /                    /I
c                               +--------------------+ I
c                (,,1)  oldpop /                    /I I
c                             +--------------------+ I I
c                (,1,)     1  I cromosome(lchrom)  I I I
c                (,2,)     2  I  |  |  |  |  |  |  I I I
c                          .  I  |  |  |  |  |  |  I I I
c                          .  I  |  |  |  |  |  |  I I +
c                          .  I  |  |  |  |  |  |  I I/
c                          .  I  |  |  |  |  |  |  I +
c                (,j,) maxpop I  |  |  |  |  |  |  I/
c                             +--------------------+
c                chromosome = (1,,) .... (lchrom,,)
c
c               maxchrom        ...     maximal chromosom-total-length
c                                       multible of 4
c                                               >= lchrom
c
c               maxpop          ...     maximal population size
c                                               >= popsize
c
c               npop            ...     number of populations
c                                       =2
c
c               parameters      ...     parameter + fitness matrix
c               (np+1,maxpop,2)         (real)
c
c                           +-------+-------+-..-+-------+-----+-----+-----+
c            (,,2) newpop  /       /       /    /       /     /     /     /I
c                         +-------+-------+-..-+-------+-----+-----+-----+ I
c          (,,1) oldpop  /       /       /    /       /     /     /     /I I
c                       +-------+-------+-..-+-------+-----+-----+-----+ I I
c          (,1,)     1  I x( 1) I x( 2) I    I x(np) I  f  I  s  I  s  I I I
c          (,2,)     2  I       I       I    I       I  i  I  c  I  u  I I I
c                    .  I       I       I    I       I  t  I  a  I  m  I I I
c                    .  I       I       I    I       I  n  I  l  I  f  I I +
c                    .  I       I       I    I       I  e  I  e  I  i  I I/
c                    .  I       I       I    I       I  s  I  d  I  t  I +
c          (,j,) maxpop I       I       I    I       I  s  I     I     I/
c                       +-------+-------+-..-+-------+-----+-----+-----+
c          x(1..np) =   (1,,)      ...         (np,,)
c          fitness  =                                 (np+1)
c	   scaled   =                                      (np+2)
c	   sumfit   =                                            (np+3)
c
c               npp3            ...     number of parameters + 2
c                                        ( + fitness + scaled)
c
c               parents         ...     parent matrix
c               (3,maxpop,2)            (integer)
c
c                                 +-----+-----+-----+
c                  (,,2) newpop  /     /     /     /I
c                               +-----+-----+-----+ I
c                (,,1) oldpop  /     /     /     /I I
c                             +-----+-----+-----+ I I
c                (,1,)     1  I  p  I  p  I     I I I
c                (,2,)     2  I  a  I  a  I  x  I I I
c                          .  I  r  I  r  I  s  I I I
c                          .  I  e  I  e  I  i  I I +
c                          .  I  n  I  n  I  t  I I/
c                          .  I  t  I  t  I  e  I +
c                (,j,) maxpop I  1  I  2  I     I/
c                             +-----+-----+-----+
c                parent1 =     (1,,)
c                parent2 =           (2,,)
c                xsite   =                 (3,,)
c
c               npar            ...     number of parent-fields
c                                       =3
c
c               lchrom          ...     actual  chromosom-length
c
c               popsize         ...     actual  population size
c                                               = problem depending
c
c               maxgen          ...     number of generations
c
c               pcross          ...     crossover probability
c                                               =(0..1) [0.6]
c
c               pmutation       ...     mutation probability
c                                               =(0..1) [0.0333]
c
c               bias            ...     bias for starting random string
c                                               =(0..1) [0.5]
c
c               parms           ...     parameter table (real)
c
c                 1                  2     ..    np
c               ---------------------------------------------------
c             1 I parm        ( 1) I ... I ... I parm        (np) I
c             2 I lparm       ( 1) I ... I ... I lparm       (np) I
c             3 I lower_bound ( 1) I ... I ... I lower_bound (np) I
c             4 I upper_bound ( 1) I ... I ... I upper_bound (np) I
c               ---------------------------------------------------
c
c               iparms          ...     parameter table (integer)
c                                       same as parms (equivalenced)
c
c               np              ...     number of parameters
c
c               nprow           ...     number of rows in parameter
c                                       table          = 4
c
c               fbest           ...     best value of objective
c                                       function
c
c               xbest           ...     vector of lenght np with
c                                       appropriate parameter values
c
c               nge             ...     number of generations used
c
c               tuse            ...     time usage
c
c               iot             ...     i/o-trigger
c                                       = 0     ... no output
c                                       = 1     ... statistic output
c                                       = 2     ... full output
c
c               nfout           ...     output unit number
c                                       =  6    ... terminal
c                                       other>6 ... to file
c
c               init            ...     initialization
c                                       = 0     ... no init
c                                       !=0     ... init
c
c               tm              ...     time range
c                                       (seconds)
c
c               gtim            ...     external timer routine
c
c               it              ...     return trigger
c                                       = 0     ... no error
c                                       = 9     ... workspace size error
c
c		iscl		...	scaling trigger
c					= 0	... no scaling
c					= 1	... scaling
c
        integer         maxchrom, maxpop, npop, npp3, npar
        integer         lchrom, popsize, maxgen, np, nprow
        integer         nge, iot, nout,init, it, iscl
        real            pcross, pmutation, bias, fbest
        real            tuse, tm, gtim, obj
        byte            chromosomes     ( maxchrom, maxpop, npop )
        real            parameters      ( npp3,     maxpop, npop )
        integer         parents         ( npar,     maxpop, npop )
        integer         iparms          ( np, nprow )
        real            parms           ( np, nprow )
        real            xbest           ( np )
c
        integer gen, nge
        integer nmutation, ncross, nout
        real    fmin, fmax, avg, sumfitness, tn2, tn, d

c               start execution
c
c
        it      = 0
        gen     = 0
c
c               get timer
c
        tn      = gtim ( d ) + tm
c
c               initialize population
c
        if ( init .ne. 0 ) then
                call initialize ( chromosomes, maxchrom, maxpop, npop,
     &                            parameters,  npp3, parents, npar,
     &                            lchrom, popsize, maxgen,
     &                            pcross, pmutation, bias,
     &                            parms, iparms, np, nprow,
     &                            fmin, fmax, avg, sumfitness,
     &                            nmutation, ncross,obj,
     &                            iot, nout, iscl )
                call report ( parameters,  npp3, maxpop, npop,
     &                        gen , 1, popsize, fmin, fmax, avg,
     &                        sumfitness, nmutation, ncross,
     &                        fbest, xbest, np, iot, nout )
        endif
c
c               loop
c
1       gen     = gen + 1
        call       generation ( chromosomes, maxchrom, maxpop, npop,
     &                          parameters,  npp3, parents, npar,
     &                          popsize, lchrom, sumfitness, ncross,
     &                          nmutation, pcross, pmutation,
     &                          parms, iparms, np, nprow, obj )

        call statistics ( parameters, npp3, maxpop, npop,
     &                    popsize, fmin, fmax, avg, sumfitness,
     &			  2, iscl )

        call report ( parameters,  npp3, maxpop, npop,
     &                gen , 2, popsize, fmin, fmax, avg,
     &                sumfitness, nmutation, ncross,
     &                fbest, xbest, np, iot, nout )
        call movepop ( 2, 1, chromosomes, maxchrom, maxpop, npop,
     &                 parameters,  npp3, parents, npar )

        tn2     =gtim(d)
        if ( ((tn2 -tn) .lt. 0.0) .and. (gen .lt. maxgen) ) goto 1
        nge     = gen
        tuse    = tn2 - (tn - tm)
c
c               end loop
c
        return
        end
