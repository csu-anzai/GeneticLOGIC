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
 *	file:	dmpind.c
 *
 *    author: 	Thomas Baeck
 *
 *   purpose:	dump the phenotype information of an population to the 
 *		Popfile.
 */

#include "extern.h"

extern FUNCTION  	 f_tab[];
extern FILE 	        *fOpen();
extern int		 xcoord[],
			 ycoord[];

int
DmpPop(Pop, Gen, PopSiz, PopFil)
STRUCTURE	Pop[];		/* population */
register int	Gen;		/* generation count */
register int 	PopSiz;		/* population size */
char	       *PopFil;		/* population dump file */

{

  	FILE		       *fp;

  	register int		i;

	char			msg[NSZ];

	STRUCTURE	       *Ind;

	void			DmpPht(),
				DmpPmt(),
				DmpGnt();

  	if ((fp = fOpen(PopFil,"a")) != NULL) {
  
		for (i = 0; i < PopSiz; i++) {

			Ind = &Pop[i];
			switch (f_tab[F_nbr].MrkFct) {

				case PERM:
					DmpPmt(fp, Gen, Ind, 
					       FctDim, NbrMttRts, F_nbr);
					break;

				case BINY:
					DmpGnt(fp, Gen, Ind, Length, MttLen);
					break;

				case REAL:
					DmpPht(fp, Gen, Ind, FctDim, NbrMttRts);
					break;

				default:
					break;
		
			} 

		}
		fclose(fp);
	}
	else {
		sprintf(msg, "DmpPop: can't open %s", PopFil);
		Error(msg);
	}
	return(0);

} /* end DmpPop() */


void
DmpPmt(fp, Gen, Ind, Dim1, Dim2, Fno)
FILE	       	       *fp;
register int		Gen,
			Dim1,
			Dim2,
			Fno;
STRUCTURE              *Ind;

{

	register int	j,
			k;

	fprintf(fp,"%4d ", Gen);	
	for (j = 1; j <= Dim1; j++) {

		k = Ind->PerMut[j-1];
		fprintf(fp, "% 3d ", k);
		
		switch (Fno) {

			case 9: 
				fprintf(fp, "(%4d,%4d) ", 
					xcoord[k], ycoord[k]);
				break;

			default:
				break;
		}
		if (j % 4 == 0)
			fprintf(fp, "\n     ");
	}
	fprintf(fp,"% 12.10e\n",Ind->Perf);	
	
	if (Dim2 > 0) {
		fprintf(fp, "     ");
		for (j = 1; j <= Dim2; j++) {
			fprintf(fp, "% e ", Ind->MttRts[j-1]);
			if (j % 5 == 0)
				fprintf(fp, "\n     ");
		}
	}
	fprintf(fp, "\n");


} /* end DmpPmt */




void
DmpPht(fp, Gen, Ind, Dim1, Dim2)
FILE	       	       *fp;
register int		Gen,
			Dim1,
			Dim2;
STRUCTURE              *Ind;

{
	register int	j;

	fprintf(fp,"%4d ", Gen);	
	for (j = 1; j <= Dim1; j++) {
		fprintf(fp, "% e ", Ind->ObjVar[j-1]);
		if (j % 5 == 0)
			fprintf(fp, "\n     ");
	}
	fprintf(fp,"% e\n",Ind->Perf);	
	
	if (Dim2 > 0) {
		fprintf(fp, "     ");
		for (j = 1; j <= Dim2; j++) {
			fprintf(fp, "% e ", Ind->MttRts[j-1]);
			if (j % 5 == 0)
				fprintf(fp, "\n     ");
		}
	}
	fprintf(fp, "\n");

} /* end DmpPht() */


void
DmpGnt(fp, Gen, Ind, Len1, Len2)
FILE	       	       *fp;
register int		Gen,
			Len1,
			Len2;
STRUCTURE              *Ind;

{
	register int	j;

	fprintf(fp,"%4d ", Gen);	
	for (j = 1; j <= Len1; j++) {
		fprintf(fp, "%d", Ind->Gene[j-1]);
		if (j % 40 == 0)
			fprintf(fp, "\n     ");
	}
	fprintf(fp,"% e\n",Ind->Perf);	

	if (Len2 > 0) {
		fprintf(fp, "     ");
		for (j = 1; j <= Len2; j++) {
			fprintf(fp, "%d", Ind->MttGen[j-1]);
			if (j % 40 == 0)
				fprintf(fp, "\n     ");
		}
	}
	fprintf(fp, "\n");

} /* end DmpGnt() */



int
CrtPgm()		

	/* 
	 *	creates the bitmap file with respect to the following
	 *	bitmap file format 'pgm':
	 *
	 *	P2	# magic number
	 *	lgt no	# length of each entry, number of entries
	 *	1	# number of gray scales
	 *	
	 *	bitmap info
	 */

{
	FILE 	       *fp;
    	char 		msg[NSZ];

	Trace("CrtPgm() entered");

	if (PgmFrq > 0) {		/* bitmap file creation */
		if ((fp = fopen(Pgmfile, "w")) == NULL) {
			sprintf(msg, "Input: can't open %s", Pgmfile);
			Error(msg);
		}
		else {
			fprintf(fp, "P2\n");
			fprintf(fp, "#\tBitmap file for %s\n", GENESIS);
			fprintf(fp, "%d %d\n", Length, 
				(Totaltrials / Popsize) / PgmFrq);
			fprintf(fp, "1");
			fclose(fp);
		}
	}
	Trace("CrtPgm() completed");

	return (0);

} /* end CrtPgm() */


int
BitMap(Idx)
register int		Idx;		/* individual's index */

{
	/*
 	 * 	write an individual to the bitmapfile
	 */

	register int	 i;
	FILE		*fp;

	Trace("BitMap() entered");

	if ((fp = fopen(Pgmfile, "a")) == NULL) {
		perror("BitMap(): Writing Pgmfile");
	}
	else {
		fprintf(fp, "\n");
		for (i = 0; i < Length; i++) {
			fprintf(fp, "%1d ", New[Idx].Gene[i] == 0 ? 1 : 0);
			if ((i > 0) && (i % 30 == 0))
				fprintf(fp, "\n");
		}
		fclose(fp);
	}
	Trace("BitMap() completed");

	return (0);

} /* end BitMap() */
 
 
/*** end of file ***/
