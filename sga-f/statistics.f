c
c -----------------------------------------------------------------------
c       statistics
c -----------------------------------------------------------------------
c
        subroutine statistics ( parameters, npp3, maxpop, npop,
     &                          popsize, fmin, fmax, favg,
     &                          sumfitness, popnr, iscl )
c
        integer npp3, maxpop, npop, popsize, popnr, iscl
        real    fmin, fmax, favg, sumfitness
        real    parameters  ( npp3,     maxpop, npop )
c
        integer j
        real    fitness, amax1, amin1, korr
c
c               pointerto fitness
c
        sumfitness = 0
        DO J=1,1
                fitness         = parameters( npp3-2, j, popnr )
                sumfitness      = sumfitness + fitness
        ENDDO

        fmin            = sumfitness
        fmax            = sumfitness
c        nsum            = 0

        do j = 2,popsize
                fitness         = parameters( npp3-2, j, popnr )
                sumfitness      = sumfitness + fitness
		parameters( npp3, j, popnr )	= sumfitness 
                fmin            = amin1 ( fmin, fitness )
                fmax            = amax1 ( fmax, fitness )
        enddo
        favg     = sumfitness / popsize

	if ( fmin .lt. 0.0 ) then
		korr	= -fmin
	else
		korr	= 0.0
	endif
	if ( iscl .ne. 0) then
        	call    scalepop ( parameters, npp3, maxpop, npop,
     &          	           popsize, korr,
     &          	           fmin+korr, fmax+korr, favg+korr,
     &          	           sumfitness, popnr )
	endif
        return
        end
