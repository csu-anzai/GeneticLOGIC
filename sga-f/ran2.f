C $Log: ran2.f,v $
c Revision 1.1  1993/07/07  15:04:18  ziegenhi
c Initial revision
c
c
C
      FUNCTION RAN2(IDUM)
c
c	he, 30.3.93
c
	integer m,ia,ic,idum,iff,mod,j,ir,iy
	real ran2,rm
c
c	end he, 30.3.93
c
      PARAMETER (M=714025,IA=1366,IC=150889,RM=1.4005112E-6)
      DIMENSION IR(97)
      DATA IFF /0/
      IF(IDUM.LT.0.OR.IFF.EQ.0)THEN
        IFF=1
        IDUM=MOD(IC-IDUM,M)
        DO 11 J=1,97
          IDUM=MOD(IA*IDUM+IC,M)
          IR(J)=IDUM
11      CONTINUE
        IDUM=MOD(IA*IDUM+IC,M)
        IY=IDUM
      ENDIF
      J=1+(97*IY)/M
      IF(J.GT.97.OR.J.LT.1)PAUSE
      IY=IR(J)
      RAN2=IY*RM
      IDUM=MOD(IA*IDUM+IC,M)
      IR(J)=IDUM
      RETURN
      END
