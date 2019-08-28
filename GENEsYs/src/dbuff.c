/****************************************************************/
/*                                                           	*/
/*  Copyright (c) 1990-1992                                     */
/*  Thomas Baeck                                             	*/
/*  Computer Science Department, LSXI                        	*/
/*  University of Dortmund                                    	*/
/*  Baroper Str. 301						*/
/*  D-4600 Dortmund 50						*/
/*                                                           	*/
/*  e-mail: baeck@ls11.informatik.uni-dortmund.de		*/
/*								*/
/*  Permission is hereby granted to copy all or any part of  	*/
/*  this program for free distribution.   The author's name  	*/
/*  and this copyright notice must be included in any copy.  	*/
/*                                                           	*/
/****************************************************************/

/*
 *
 *	$Id$
 *	$Log$
 *
 *
 *      file:  	dbuff.c
 *
 *    author:  	Thomas Baeck
 *
 *   created:   23 May '91
 *
 *  purpose:    Provide a general data buffer mechanism to fulfill 
 *		the following purposes:
 *
 *		- Intermediate buffering of data in private buffers, which
 *		  are flushed to a file iff the buffer is full or it is
 *		  explicitly forced.
 *		- collect data which is closely related in the same buffer.
 *
 */

#include "extern.h"


BUFFER *
CrtBuf(Fil, Elt)
char		       *Fil;
int		 	Elt;

{
	extern void	Error();

	char	       *malloc(),
		       *calloc(),
		       *strcpy();

	register int	i;

	BUFFER	       *Buf;

	if ((Buf = (BUFFER *) calloc ((unsigned) 1, sizeof(BUFFER))) == NULL
	||  (Buf->BufFmt = (char **) calloc((unsigned) (Elt + 1), 
					            sizeof(char *))) == NULL) {
		Error("CrtBuf: No space");
	}
	Buf->BufSiz = 0;
	Buf->BufCol = Elt;
	strcpy(Buf->BufFil, Fil);
	for (i = 0; i <= Elt; i++) {
		if ((Buf->BufFmt[i] = (char *) malloc((unsigned) NSZ)) 
								== NULL) {
			Error("CrtBuf: No space");
		}
		strcpy(Buf->BufFmt[i], "% 1.5e ");
	}
	for (i = 0; i < ROWS; i++) {
		if ((Buf->BufVal[i] = (double *) calloc((unsigned) (Elt + 1),
					sizeof(double))) == NULL) {
			Error("CrtBuf: No space");
		}
	}
	return(Buf);

} /* end CrtBuf */




int
DelBuf(Buf)
BUFFER		       *Buf;

{
	register int	i;

	for (i = 0; i < ROWS; i++) {
		free((char *) (Buf->BufVal[i]));
	}
	for (i = 0; i <= Buf->BufCol; i++ ) {
		free((char *) (Buf->BufFmt[i]));
	}
	return(free((char *) (Buf->BufFmt)));

} /* end DelBuf */




void
ChgBuf(Buf, Fil)
BUFFER		       *Buf;
char		       *Fil;

{
	char	       *strcpy();

	strcpy(Buf->BufFil, Fil);

} /* end ChgBuf */




void
ChgFmt(Buf, Idx, Fmt)
BUFFER		       *Buf;
register int		Idx;
char		       *Fmt;

{

	char	       *strcpy();

	strcpy(Buf->BufFmt[Idx], Fmt);

} /* end ChgFmt */




void 
EntBuf(Val, Buf, n, Mod)
double			Val[];
BUFFER		       *Buf;
register int 		n;
register int 		Mod;

{
	void		StoBuf(),
			FshBuf();
	int		FulBuf();

	StoBuf(Val, Buf, n);		/* store a line of length n in Buf */
	
	if (FulBuf(Buf)) {
		FshBuf(Buf, n, Mod);		/* if full, flush buffer */
	}

} /* end EntBuf */




int
FulBuf(Buf)
BUFFER			*Buf;

{

	return(Buf->BufSiz >= (ROWS - 1));

} /* end FulBuf */




void
StoBuf(Val, Buf, n)
double			Val[];
BUFFER		       *Buf;
register int 		n;

{
	register int		i,
				Idx;

	for (Idx = Buf->BufSiz, i = 1; i <= n; i++) {
		Buf->BufVal[Idx][i] = Val[i - 1];
	}
	Buf->BufVal[Idx][0] = Gen;
	Buf->BufSiz++;

} /* end StoBuf */




void
FshBuf(Buf, n, Mod)
BUFFER			*Buf;		/* Buffer */
register int		 n;		/* number of columns */
register int		 Mod;		/* modus of data extraction */

{
	extern void		Error();

	register int		i,
				j;

	char			Msg[NSZ];

	FILE		       *fp;

	if ((fp = fopen(Buf->BufFil, "a")) != NULL) {
		if (Mod == _SGLLIN) {		/* single line modus */
			for (i = 0; i < Buf->BufSiz; i++) {
				for (j = 0; j <= n; j++) {
					fprintf(fp, Buf->BufFmt[j], 
						    Buf->BufVal[i][j]);
				}
				fprintf(fp, "\n");
			}
		}
		else {				/* multiple line modus */
			for (i = 0; i < Buf->BufSiz; i++) {
				for (j = 1; j <= n; j++) {
					fprintf(fp, Buf->BufFmt[0], 
						    Buf->BufVal[i][0]);
					fprintf(fp, Buf->BufFmt[j], 
						    Buf->BufVal[i][j]);
					fprintf(fp, "\n");
				}
			}
		}
		Buf->BufSiz = 0;
		fclose(fp);
	}
	else {
		sprintf(Msg, "FshBuf: can't open %s", Buf->BufFil);
		Error(Msg);
	}

} /* end FshBuf */



void
WrtPfmBuf(i, Mod)
int		       *i;
register int		Mod;

{
	
	double		Hlp[DTACOL];

	extern void	ChgFmt();

	*i = 0;
	ChgFmt(PfmBuf, *i, "%6.0f ");
	Hlp[(*i)++] = Trials;
	ChgFmt(PfmBuf, *i, "%6.0f ");
	Hlp[(*i)++] = Lost;
	ChgFmt(PfmBuf, *i, "%6.0f ");
	Hlp[(*i)++] = Conv;
	ChgFmt(PfmBuf, *i, "%6.0f ");
	Hlp[(*i)++] = (Proportionflag) ? Best_prop : Bias;
	ChgFmt(PfmBuf, *i, "%6.4f ");
	Hlp[(*i)++] = Online;
	ChgFmt(PfmBuf, *i, "%e ");
	Hlp[(*i)++] = Offline;
	ChgFmt(PfmBuf, *i, "%e ");
	Hlp[(*i)++] = Best_current_perf;
	ChgFmt(PfmBuf, *i, "%e ");
	Hlp[(*i)++] = Ave_current_perf;
	ChgFmt(PfmBuf, *i, "%e ");
	Hlp[(*i)++] = Worst_current_perf;
	ChgFmt(PfmBuf, *i, "%e ");
	if (NbrMttRts > 0) {
		Hlp[(*i)++] = MinRat;
		ChgFmt(PfmBuf, *i, "%e ");
		Hlp[(*i)++] = MaxRat;
		ChgFmt(PfmBuf, *i, "%e ");
		Hlp[(*i)++] = AvgRat;
		ChgFmt(PfmBuf, *i, "%e ");
	}
	EntBuf(Hlp, PfmBuf, *i, Mod);

} /* end WrtPfmBuf */


/*** end of file ***/
