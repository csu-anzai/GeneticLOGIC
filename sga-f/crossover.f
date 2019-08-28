c
c -----------------------------------------------------------------------
c       crossover
c -----------------------------------------------------------------------
c
        subroutine crossover( parent1, parent2, child1, child2,
     &          lchrom, ncross, nmutation, jcross, pcross,
     &          pmutation )
c
        byte            parent1 (lchrom), parent2 (lchrom)
        byte            child1  (lchrom), child2  (lchrom)
        integer         lchrom, ncross, nmutation, jcross
        real            pcross, pmutation
c
        integer         j, rnd, flip
c
        if ( flip( pcross ) .eq. 1) then
                jcross  = rnd( 1, lchrom-1 )
                ncross  = ncross + 1
        else
                jcross  = lchrom
        endif
c
c               1st exchange, 1 to 1 and 2 to 2
c
        do j = 1, jcross
                call mutation ( parent1(j), child1(j),
     &                          pmutation, nmutation )
                call mutation ( parent2(j), child2(j),
     &                          pmutation, nmutation )
        enddo
c
c               2nd exchange, 1 to 2 and 2 to 1
c
        do j = jcross+1, lchrom
                call mutation ( parent2(j), child1(j),
     &                          pmutation, nmutation )
                call mutation ( parent1(j), child2(j),
     &                          pmutation, nmutation )
        enddo
        return
        end
