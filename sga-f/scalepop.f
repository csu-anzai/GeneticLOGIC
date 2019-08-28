c
c -----------------------------------------------------------------------
c       scalepop
c -----------------------------------------------------------------------
c
        subroutine scalepop ( parameters, npp3, maxpop, npop,
     &                        popsize, korr, umin, umax, uavg,
     &                        sumfitness, popnr )
c
	integer		npp3, maxpop, npop, popsize, popnr
	real		umin, umax, uavg, sumfitness, korr
	real		parameters	( npp3, maxpop, npop )
c
	real objective, fitness, a, b
	integer j
c
	call prescale ( umin, umax, uavg, a, b )

	sumfitness	= 0.0

	do j = 1, popsize

		objective	= parameters (npp3-2, j, popnr )
		fitness		= a*(objective+korr) + b

		sumfitness	= sumfitness + fitness

		parameters ( npp3-1, j, popnr )	= fitness
		parameters ( npp3  , j, popnr )	= sumfitness

	enddo

	end
