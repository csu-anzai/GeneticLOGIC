c
c -----------------------------------------------------------------------
c       select
c -----------------------------------------------------------------------
c
        integer function select ( parameters, npp3, maxpop, npop,
     &                            popsize, sumfitness, popnr)
        integer npp3, maxpop, npop, popsize, popnr
        real    sumfitness
        real    parameters ( npp3, maxpop, npop )
c
        real rand, partsum,random,d
        integer j
c
        partsum = 0.0
        j       = 0
        rand    =  random(d) * sumfitness
1       j       = j + 1
c        partsum = partsum + parameters(npp3, j, popnr)
        partsum = parameters(npp3, j, popnr)
        if (    .not. ( ( partsum .ge. rand    )
     &          .or.    ( j       .eq. popsize ) )      ) goto 1
        select  =j
        return
        end
