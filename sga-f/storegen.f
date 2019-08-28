c
c -----------------------------------------------------------------------
c       storegen
c -----------------------------------------------------------------------
c
        subroutine storegen( popnr, j, x, np, objective, fitness,sumfit,
     &                       parent1, parent2, xsite,
     &                       parameters, npp3, maxpop, npop,
     &                       parents, npar )
c
        integer popnr, j, np, parent1, parent2, xsite
        integer npp3, maxpop, npop, npar
        real    objective, fitness, sumfit
        real    parameters  ( npp3,     maxpop, npop )
        integer parents     ( npar,     maxpop, npop )
        real    x  ( np )
c
        integer i
c
c               store x-vector
c
        do i=1,np
                parameters(i,j,popnr)   = x(i)
        enddo
c
c               pointer to fitness
c
        parameters (np+1,j,popnr)       = objective
        parameters (np+2,j,popnr)       = fitness
        parameters (np+3,j,popnr)       = sumfit
        parents (1,j,popnr)             = parent1
        parents (2,j,popnr)             = parent2
        parents (3,j,popnr)             = xsite

        end
