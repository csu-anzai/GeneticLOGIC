
c
c -----------------------------------------------------------------------
c       initreport
c -----------------------------------------------------------------------
c
        subroutine initreport ( popsize, lchrom, maxgen, pcross,
     &                          pmutation, fmin, fmax, avg, sumfitness,
     &                          iot, nout )
        integer popsize, lchrom, maxgen, iot, nout
        real    pcross, pmutation, fmin, fmax, avg, sumfitness
c
c               SGA Parameters
c
        if ( iot .ge. 2 ) then
        write(nout,*)
     &          ' Population size ( popsize)         = ',popsize
        write(nout,*)
     &          ' Chromosome length (lchrom)         = ',lchrom
        write(nout,*)
     &          ' Maximum # of generations (maxgen)  = ',maxgen
        write(nout,*)
     &          ' Crossover probability (pcross)     = ',pcross
        write(nout,*)
     &          ' Mutation probability (pmutation)   = ',pmutation
        write(nout,*)' '
        write(nout,*)
     &          ' Initial population maximum fitness = ',fmax
        write(nout,*)
     &          ' Initial population average fitness = ',avg
        write(nout,*)
     &          ' Initial population minimum fitness = ',fmin
        write(nout,*)
     &          ' Initial population sum of  fitness = ',sumfitness
        endif

        end
