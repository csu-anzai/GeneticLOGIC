c
c -----------------------------------------------------------------------
c       report
c -----------------------------------------------------------------------
c
      subroutine report ( parameters,  npp3, maxpop, npop,
     &                      gen, popnr, popsize, fmin, fmax, avg,
     &                      sumfitness, nmutation, ncross,
     &                      fbest, xbest, np, iot, nout )
c
        integer npp3, maxpop, npop, gen, popnr, popsize
        integer nmutation, ncross, np, iot, nout
        real    fmin, fmax, avg, sumfitness, fbest
        real    parameters  ( npp3,     maxpop, npop )
        real    xbest(np)
c
        integer k, j
        real    ff
c
        do j=1,popsize
		ff	= parameters(npp3-2, j, popnr)
                if ( ff.gt. fbest ) then
                        fbest	= ff
                        do k=1,np
                                xbest(k)=parameters(k, j, popnr)
                        enddo
                endif
                if (iot .ge. 2 )
     &                  write(nout,1000)
     &                           j,parameters(npp3, j, popnr),
     &                          (parameters(k, j, popnr),k=1,np)
1000    format(i3,')  f(x)=',g15.7/1x,'x(i)=',10g12.5)
        enddo
        if ( iot .ge. 1 ) then
                write(nout,'(1x,i3,a,3g12.5,a,g12.5,a,i6,a,i6)')
     &                  gen,
     &                  ' mi,ma,av=',fmin,fmax,avg,
     &                  ' su=',sumfitness,' nm=',
     &                  nmutation,' nc=',ncross
        endif
        if ( iot .ge. 2 )write(nout,'(132(''-''))')

        end
