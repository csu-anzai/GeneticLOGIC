c
c -----------------------------------------------------------------------
c       prescale
c -----------------------------------------------------------------------
c
	subroutine prescale ( umin, umax, uavg, a, b )
c
	real		umin, umax, uavg, a, b
c
c		calculate scaling coefficients for linear scaling
c
	real	fmultiple	/ 2.0 /
	real	delta
c
	if ( umin .gt. (fmultiple*uavg-umax )/(fmultiple -1.0) ) then
c
c			Normal scaling
c
		delta	= umax - uavg
		a	= (fmultiple -1.0)*uavg / delta
		b	= uavg * (umax - fmultiple*uavg) / delta
	else
c
c			scale as much as posible
c
		delta	= uavg - umin
		a	= uavg / delta
		b	= -umin * uavg / delta
	endif

	end
