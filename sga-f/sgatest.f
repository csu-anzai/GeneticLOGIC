c
c ---------------------------------------------------------------------
c       sgatest
c ---------------------------------------------------------------------
c
        program sgatest    
        real bf
        integer ibf
        common // bf( 360000 )
        integer*4 ibf( 360000 )
        equivalence (bf(1),ibf(1))
c
        integer nfin, nfout, np, nprow, npop, npar, npp3, npA
        integer ip1, ip2, ip3, ip4, ip5, ip6, i, lparm, iot, idim
        integer maxchrom, lchrom, maxpop, popsize, iscale
        integer maxgen, i	seed, it, ip, init, nge
        real pcross, pmutation, bias, fbest, runti, tm, tuse
c
        external runti, objfunc
c                          
        nfin		= 5                                  
        nfout		= 6
c
	npop		= 2                    
	npar		= 3
        nprow   	= 4
c
        print *," popsize       ... size of population"
        print *," maxgen        ... maximal number of generations"
        print *," lparm         ... lenght of the substrings"
        print *," np            ... number of parameters"
        print *," iscale        ... scale fitness[0|1]"
        print *," "
        print *," Enter popsize maxgen lparm np iscale"
        read(5,*) popsize, maxgen, lparm, np, iscale
        print *," "

	npA		= np
	npp3		= npA + 3
	lchrom		= npA * lparm
	idim		= (lchrom + 3 ) / 4
	maxchrom	= idim * 4
        maxpop  	= popsize

c	popsize		=popsize + mod(popsize,2)

        print *," pcross        ... crossover probatility"
        print *," pmutation     ... mutation probability"
        print *," bias          ... bias for start bitstring"
        print *," tm            ... maximal execution time"
        print *," "
        print *," Enter pcross pmutation bias tm"
        read(5,*)pcross,pmutation,bias,tm
        print *," "

c
c               define pointers
c
        ip1     = 1
        ip2	= ip1 + idim*maxpop*npop
        ip3	= ip2 + npp3*maxpop*npop
        ip4	= ip3 + npar*maxpop*npop
	ip5	= ip4 + npA*nprow
	ip6	= ip5 + npA
	write(6,*) " Number of bytes used = ", ip6*4
c
c               define parameter table
c               ---------------------------------------------------
c               I parm        ( 1) I ... I ... I parm        (np) I
c               I lparm       ( 1) I ... I ... I lparm       (np) I
c               I lower_bound ( 1) I ... I ... I lower_bound (np) I
c               I upper_bound ( 1) I ... I ... I upper_bound (np) I
c               ---------------------------------------------------
c
        do i=1,npA
                ip=ip4+i-1
                bf (ip     )    =   0.0
                ibf(ip+  npA)    = lparm   
                bf (ip+2*npA)    = -10.0
                bf (ip+3*npA)    =  10.0
        enddo
c
c
c
        write(6,*)' Enter iot[0,1], output-unit[6,>6] '
        read(nfin,*)iot,nfout
c
        iseed = -8455723
        call randomize(iseed)
        fbest=-1.0e+37
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
c               (npA+1,maxpop,2)         (real)
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
c          scaled   =                                      (np+2)
c          sumfit   =                                            (np+3)
c
c               npp3            ...     number of parameters + 3
c                                        ( +fitness )
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
c               npA             ...     number of parameters
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
        init    = 1
        call       sga ( ibf(ip1)  , maxchrom, maxpop, npop,
     &                   bf(ip2)   ,  npp3, ibf(ip3) , npar,
     &                   lchrom, popsize, maxgen,
     &                   pcross, pmutation, bias,
     &                   bf(ip4), ibf(ip4), npA, nprow,
     &                   fbest, bf(ip5), nge, tuse,
     &                   iot, nfout, init, tm, runti, objfunc, it,
     &			 iscale )

        write(6,'(" Fbest=",g15.7,"  after ",i4," generations ,",
     &          f10.5," sec"/" Xbest="/(5g13.5))')  fbest, nge, tuse,
     &                  (bf(ip5+i-1),i=1,np)
c
        init    = 0
        call       sga ( ibf(ip1)  , maxchrom, maxpop, npop,
     &                   bf(ip2)   ,  npp3, ibf(ip3) , npar,
     &                   lchrom, popsize, maxgen,
     &                   pcross, pmutation, bias,
     &                   bf(ip4), ibf(ip4), np, nprow,
     &                   fbest, bf(ip5), nge, tuse,
     &                   iot, nfout, init, tm, runti, objfunc, it,
     &			 iscale )

        write(6,'(" Fbest=",g15.7,"  after ",i4," generations ,",
     &          f10.5," sec"/" Xbest="/(5g13.5))')  fbest, nge, tuse,
     &                  (bf(ip5+i-1),i=1,np)

        end 
