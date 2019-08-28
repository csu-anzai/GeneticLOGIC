c
c -----------------------------------------------------------------------
c	objfunc
c -----------------------------------------------------------------------
c
	real function objfunc ( np, x )
c
c	objective function
c
	real x(np)
	integer np,i
c
	real y
c
	y=0
	do i=1,np
		y=y+x(i)**2
	enddo
	objfunc	= y
	return
	end
