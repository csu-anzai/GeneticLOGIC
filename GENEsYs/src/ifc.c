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
 *	$Id$
 *	$Log$
 *
 *	file:	ifc.c
 *
 *    author: 	Thomas Baeck
 *
 *   created:	11 May '91
 *
 *   purpose: 	Interface procedures for reading and writing the complete
 *		GA data from and to a file or stdin/stdout.
 *		The procedures are interacting as a finite state machine.
 *
 */

#include <stdio.h>
#include "extern.h"

#define fopen(a,b)	fOpen((a),(b))

#define RDDATA		1
#define WTDATA		0

extern FUNCTION		f_tab[];
extern FILE 	       *fOpen(); 

char			FilNam[NSZ];




int
WrtDta(fp)
FILE		*fp;

{

	int 		_sFctNbr();

	return( _sFctNbr(fp, WTDATA));

} /* end WrtDta */



int
GetDta(fp)
FILE		*fp;

{
	extern void	InzVal();
	int 		_sFctNbr();

	if (fp == stdin) { 
		InzVal();
	} 
	return( _sFctNbr(fp, RDDATA));

} /* end GetDta */



#define	_SFCTNBR	"Objective Function Number              : %d\n"
#define	_DFCTNBR	"Objective Function Number              [       %2d] : "

int
_sFctNbr(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sFctPar();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DFCTNBR, FctNbr);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &FctNbr);
		}
		else {
			fscanf(fp, _SFCTNBR, &FctNbr);
		}
		F_nbr = FctNbr - 1;
	}
	else {
		fprintf(fp, _SFCTNBR, FctNbr);
	}
	_sFctPar(fp, Flg);

	return(0);

} /* end _sFctNbr */




#define	_SFCTPAR	"Objective Function Parameter (cont.)   : %lf\n"
#define	_DFCTPAR	"[%5.3e] : "

int
_sFctPar(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 i,
			 j,
			 k,
			 _sDscPar(),
			 strlen();

	char		 Str[NSZ];
	char		*gets();

	for (	k = 0; 
		k < sizeof(f_tab[F_nbr].CstVal) / sizeof(double)
	     && f_tab[F_nbr].CstDsc[k] != (char *) NULL;
		k++) {
		if (Flg) {
			if (fp == stdin) {
				printf("%s", f_tab[F_nbr].CstDsc[k]);
				j = LOGLEN - strlen(f_tab[F_nbr].CstDsc[k]) - 1;
				for (i = 0; i < j; i++) 	printf(" ");
				printf(_DFCTPAR, f_tab[F_nbr].CstVal[k]);
				gets(Str);
				if (strlen(Str))
					sscanf(	Str, "%lf", 
						&(f_tab[F_nbr].CstVal[k]));
			}
			else {
				fscanf(fp, _SFCTPAR, &(f_tab[F_nbr].CstVal[k]));
			}
		}
		else {
			fprintf(fp, _SFCTPAR, f_tab[F_nbr].CstVal[k]);
		}
	}
	_sDscPar(fp, Flg);

	return(0);

} /* end _sFctPar */




#define	_SDSCPAR	"Objective Function Parameter (disc.)   : %d\n"
#define	_DDSCPAR	"[        %1d] : "

int
_sDscPar(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 i,
			 j,
			 k,
			 _sFctDim(),
			 _sChrLen(),
			 strlen();

	char		 Str[NSZ];
	char		*gets();

	for (	k = 0; 
		k < MOPT && f_tab[F_nbr].FctOpt[k].OTtl != (char *) NULL;
		k++) {
		if (Flg) {
			if (fp == stdin) {
				printf("%s", f_tab[F_nbr].FctOpt[k].OTtl);
				j = LOGLEN - 1 
				    - strlen(f_tab[F_nbr].FctOpt[k].OTtl);
				for (i = 0; i < j; i++) 	printf(" ");
				printf(_DDSCPAR, f_tab[F_nbr].FctOpt[k].OVal);
				gets(Str);
				if (strlen(Str))
					sscanf(	Str, "%d", 
					&(f_tab[F_nbr].FctOpt[k].OVal));
			}
			else {
				fscanf(fp, _SDSCPAR, 
					&(f_tab[F_nbr].FctOpt[k].OVal));
			}
		}
		else {
			fprintf(fp, _SDSCPAR, f_tab[F_nbr].FctOpt[k].OVal);
		}
	}
	if (!DimChk(F_nbr)) {
		_sFctDim(fp, Flg);
	}
	else {
		FctDim = f_tab[F_nbr].dim;
		_sChrLen(fp, Flg);
	}

	return(0);

} /* end _sDscPar */




#define _SFCTDIM	"Objective Function Dimension           : %d\n"
#define	_DFCTDIM	"Objective Function Dimension           [      %3d] : "

int
_sFctDim(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sChrLen();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			FctDim = f_tab[F_nbr].dim;	
			printf(_DFCTDIM, FctDim);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &FctDim);
		}
		else {
			fscanf(fp, _SFCTDIM, &FctDim);
		}
	}
	else {
		fprintf(fp, _SFCTDIM, FctDim);
	}
	_sChrLen(fp, Flg);

	return(0);

} /* end _sFctDim */




#define _SCHRLEN	"Length per Object Variable             : %d\n"
#define	_DCHRLEN	"Length per Object Variable             [       %2d] : "

int
_sChrLen(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sPopSiz();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			if (FctDim == 0) {
				ChrLen = -1;
			}
			printf(_DCHRLEN, ChrLen);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &ChrLen);
		}
		else {
			fscanf(fp, _SCHRLEN, &ChrLen);
		}
	}
	else {
		fprintf(fp, _SCHRLEN, ChrLen);
	}
	_sPopSiz(fp, Flg);

	return(0);

} /* end _sChrLen */




#define _SPOPSIZ	"Population Size                        : %d\n"
#define	_DPOPSIZ	"Population Size                        [      %3d] : "

int
_sPopSiz(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRepScm();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DPOPSIZ, Popsize);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Popsize);
		}
		else {
			fscanf(fp, _SPOPSIZ, &Popsize);
		}
	}
	else {
		fprintf(fp, _SPOPSIZ, Popsize);
	}
	_sRepScm(fp, Flg);

	return(0);

} /* end _sPopSiz */




#define _SREPSCM	"Reproduction Mechanism                 : %c\n"
#define	_DREPSCM	"Reproduction Mechanism                 [        %c] : "

int
_sRepScm(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sTmpCtl(),
			 _sRhoVal(),
			 _sWhiCst(),
			 _sEtaMax();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DREPSCM, SltScm);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%c", &SltScm);
		}
		else {
			fscanf(fp, _SREPSCM, &SltScm);
		}
	}
	else {
		fprintf(fp, _SREPSCM, SltScm);
	}
	switch (SltScm) {

		case PRP_SLT: 	_sRhoVal(fp, Flg);
				break;

		case BZM_SLT:	_sTmpCtl(fp, Flg);
				break;

		case MLR_SLT:
		case MLC_SLT: 	_sRhoVal(fp, Flg);
				break;

		case BRK_SLT:
		case IRK_SLT:	_sEtaMax(fp, Flg);
				break;

		case WRK_SLT:	
				_sWhiCst(fp, Flg);
				break;

		default:	_sRhoVal(fp, Flg);
				break;
	}

	return(0);

} /* end _sRepScm */




#define _SETAMAX	"Maximum Expected Value for Ranking     : %lf\n"
#define	_DETAMAX	"Maximum Expected Value for Ranking     [    %5.3f] : "

int
_sEtaMax(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sRhoVal();
	char		 Str[NSZ];
	char		*gets();
				
	if (Flg) {
		if (fp == stdin) {
			printf(_DETAMAX, Eta_max);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &Eta_max);
		}
		else {
			fscanf(fp, _SETAMAX, &Eta_max);
		}
	}
	else {
		fprintf(fp, _SETAMAX, Eta_max);
	}
	_sRhoVal(fp, Flg);

	return(0);

} /* end _sEta_max */




#define _SWHICST	"Value of Whitley's Constant 'a'        : %lf\n"
#define	_DWHICST	"Value of Whitley's Constant 'a'        [    %5.3f] : "

int
_sWhiCst(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sMttScm();
	char		 Str[NSZ];
	char		*gets();
				
	if (Flg) {
		if (fp == stdin) {
			printf(_DWHICST, Eta_max);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &Eta_max);
		}
		else {
			fscanf(fp, _SWHICST, &Eta_max);
		}
	}
	else {
		fprintf(fp, _SWHICST, Eta_max);
	}
	_sMttScm(fp, Flg);

	return(0);

} /* end WhiCst */




#define _STMPCTL	"Temperature Control Parameter          : %lf\n"
#define	_DTMPCTL	"Temperature Control Parameter          [     %4.2f] : "

int
_sTmpCtl(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRhoVal();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DTMPCTL, CtlPar);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &CtlPar);
		}
		else {
			fscanf(fp, _STMPCTL, &CtlPar);
		}
	}
	else {
		fprintf(fp, _STMPCTL, CtlPar);
	}
	_sChnLgt(fp, Flg);

	return(0);

} /* end _sTmpCtl */




#define _SCHNLGT	"Constant Temp. Cooling Interval (Gens) : %d\n"
#define	_DCHNLGT	"Constant Temp. Cooling Interval (Gens) [        %1d] : "

int
_sChnLgt(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRhoVal();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DCHNLGT, ChnLgt);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &ChnLgt);
		}
		else {
			fscanf(fp, _SCHNLGT, &ChnLgt);
		}
	}
	else {
		fprintf(fp, _SCHNLGT, ChnLgt);
	}
	_sRhoVal(fp, Flg);

	return(0);

} /* end _sChnLgt */




#define _SMUVAL		"Upper Selection Index                  : %d\n"
#define	_DMUVAL		"Upper Selection Index                  [      %3d] : "

int
_sMuVal(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sMttScm();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		Mu = Popsize;
		if (fp == stdin) {
			printf(_DMUVAL, Mu);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Mu);
		}
		else {
			fscanf(fp, _SMUVAL, &Mu);
		}
	}
	else {
		fprintf(fp, _SMUVAL, Mu);
	}
	_sMttScm(fp, Flg);

	return(0);

} /* end _sMuVal */



		
#define _SRHOVAL	"Lower Selection Index                  : %d\n"
#define	_DRHOVAL	"Lower Selection Index                  [      %3d] : "

int
_sRhoVal(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sMuVal();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		Rho = 1;
		if (fp == stdin) {
			printf(_DRHOVAL, Rho);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Rho);
		}
		else {
			fscanf(fp, _SRHOVAL, &Rho);
		}
	}
	else {
		fprintf(fp, _SRHOVAL, Rho);
	}
	_sMuVal(fp, Flg);

	return(0);

} /* end _sRhoVal */




#define _SMTTSCM	"Mutation Mechanism                     : %c\n"
#define	_DMTTSCM	"Mutation Mechanism                     [        %c] : "

int
_sMttScm(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sMttRat(),
			 _sNbrRts();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DMTTSCM, MttScm);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%c", &MttScm);
		}
		else {
			fscanf(fp, _SMTTSCM, &MttScm);
		}
	}
	else {
		fprintf(fp, _SMTTSCM, MttScm);
	}
	switch (MttScm) {

		case STD_MTT:	_sMttRat(fp, Flg);
				break;

		case ADX_MTT:	
		case ADT_MTT:	_sNbrRts(fp, Flg);
				break;
	
		default:	_sMttRat(fp, Flg);
				break;
	}

	return(0);

} /* end _sMttScm */




#define _SNBRRTS	"Number of Mutation Rates               : %d\n"
#define	_DNBRRTS	"Number of Mutation Rates               [      %3d] : "

int
_sNbrRts(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sMttBts();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		NbrMttRts = (FctDim) ? FctDim : 1;
		if (fp == stdin) {
			printf(_DNBRRTS, NbrMttRts);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &NbrMttRts);
		}
		else {
			fscanf(fp, _SNBRRTS, &NbrMttRts);
		}
	}
	else {
		fprintf(fp, _SNBRRTS, NbrMttRts);
	}
	_sMttBts(fp, Flg);

	return(0);

} /* end _sNbrRts */




#define _SMTTBTS        "Number of Bits for each Mutation Rate  : %d\n"
#define	_DMTTBTS	"Number of Bits for each Mutation Rate  [       %2d] : "

int
_sMttBts(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sMttRat(),
			 _sRecScm();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			M_bits = 20;
			printf(_DMTTBTS, M_bits);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &M_bits);
		}
		else {
			fscanf(fp, _SMTTBTS, &M_bits);
		}
	}
	else {
		fprintf(fp, _SMTTBTS, M_bits);
	}
	if (MttScm == ADX_MTT) {
		_sMttRat(fp, Flg);
	}
	else {
		_sRecScm(fp, Flg);
	}

	return(0);

} /* end _sMttBts */




#define _SMTTRAT        "Mutation Probability                   : %lf\n"
#define	_DMTTRAT	"Mutation Probability                   [    %5.3f] : "

int
_sMttRat(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRecScm();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DMTTRAT, M_rate);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &M_rate);
		}
		else {
			fscanf(fp, _SMTTRAT, &M_rate);
		}
	}
	else {
		fprintf(fp, _SMTTRAT, M_rate);
	}

	_sRecScm(fp, Flg);

	return(0);

} /* end _sMttRat */




#define _SRECSCM	"Recombination Mechanism                : %s\n"
#define	_DRECSCM	"Recombination Mechanism                [       %2s] : "

int
_sRecScm(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sXvrPts(),
			 _sXvrRat();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DRECSCM, RecScm);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%s", RecScm);
		}
		else {
			fscanf(fp, _SRECSCM, RecScm);
		}
	}
	else {
		fprintf(fp, _SRECSCM, RecScm);
	}
	if (RecScm[0] == STD_REC || RecScm[1] == STD_REC) {
		_sXvrPts(fp, Flg);
	}
	else {
		_sXvrRat(fp, Flg);
	}

	return(0);

} /* end _sRecScm */




#define _SXVRPTS	"Number of Crossover Points             : %d\n"
#define	_DXVRPTS	"Number of Crossover Points             [        %1d] : "

int
_sXvrPts(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sXvrRat();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DXVRPTS, C_points);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &C_points);
		}
		else {
			fscanf(fp, _SXVRPTS, &C_points);
		}
	}
	else {
		fprintf(fp, _SXVRPTS, C_points);
	}
	_sXvrRat(fp, Flg);

	return(0);

} /* end _sXvrPts */




#define _SXVRRAT	"Crossover Application Rate             : %lf\n"
#define	_DXVRRAT	"Crossover Application Rate             [      %3.1f] : "

int
_sXvrRat(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sGenGap();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DXVRRAT, C_rate);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &C_rate);
		}
		else {
			fscanf(fp, _SXVRRAT, &C_rate);
		}
	}
	else {
		fprintf(fp, _SXVRRAT, C_rate);
	}
	_sGenGap(fp, Flg);

	return(0);

} /* end _sXvrRat */




#define _SGENGAP	"Generation Gap                         : %lf\n"
#define	_DGENGAP	"Generation Gap                         [     %4.2f] : "

int
_sGenGap(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sWdwSiz();
	char		 Str[NSZ];
	char		*gets();
	
	if (Flg) {
		if (fp == stdin) {
			printf(_DGENGAP, Gapsize);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%lf", &Gapsize);
		}
		else {
			fscanf(fp, _SGENGAP, &Gapsize);
		}
	}
	else {
		fprintf(fp, _SGENGAP, Gapsize);
	}
	_sWdwSiz(fp, Flg);

	return(0);

} /* end _sGenGap */




#define _SWDWSIZ	"Scaling Window Size                    : %d\n"
#define	_DWDWSIZ	"Scaling Window Size                    [        %1d] : "

int
_sWdwSiz(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sNbrExp();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DWDWSIZ, Windowsize);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Windowsize);
		}
		else {
			fscanf(fp, _SWDWSIZ, &Windowsize);
		}
	}
	else {
		fprintf(fp, _SWDWSIZ, Windowsize);
	}
	_sNbrExp(fp, Flg);

	return(0);

} /* end _sWdwSiz */




#define _SNBREXP	"Number of Experiments to Perform       : %d\n"
#define	_DNBREXP	"Number of Experiments to Perform       [        %1d] : "

int
_sNbrExp(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sTotTrl();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DNBREXP, Totalexperiments);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Totalexperiments);
		}
		else {
			fscanf(fp, _SNBREXP, &Totalexperiments);
		}
	}
	else {
		fprintf(fp, _SNBREXP, Totalexperiments);
	}
	_sTotTrl(fp, Flg);

	return(0);

} /* end _sNbrExp */




#define _STOTTRL	"Number of Trials per Experiment        : %d\n"
#define	_DTOTTRL	"Number of Trials per Experiment        [     %4d] : "

int
_sTotTrl(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRptIvl();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DTOTTRL, Totaltrials);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Totaltrials);
		}
		else {
			fscanf(fp, _STOTTRL, &Totaltrials);
		}
	}
	else {
		fprintf(fp, _STOTTRL, Totaltrials);
	}
	_sRptIvl(fp, Flg);

	return(0);

} /* end _sWdwSiz */




#define _SRPTIVL	"Report Interval, Evaluations           : %d\n"
#define	_DRPTIVL	"Report Interval, Evaluations           [      %3d] : "

int
_sRptIvl(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sSavSiz();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DRPTIVL, Interval);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Interval);
		}
		else {
			fscanf(fp, _SRPTIVL, &Interval);
		}
	}
	else {
		fprintf(fp, _SRPTIVL, Interval);
	}
	_sSavSiz(fp, Flg);

	return(0);

} /* end _sRptIvl */




#define _SSAVSIZ	"Number of Structures to Save           : %d\n"
#define	_DSAVSIZ	"Number of Structures to Save           [       %2d] : "
 
int
_sSavSiz(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int 		 _sMaxSpn();
	char		 Str[NSZ];
	char		*gets();
	
	if (Flg) {
		if (fp == stdin) {
			printf(_DSAVSIZ, Savesize);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Savesize);
		}
		else {
			fscanf(fp, _SSAVSIZ, &Savesize);
		}
	}
	else {
		fprintf(fp, _SSAVSIZ, Savesize);
	}
	_sMaxSpn(fp, Flg);

	return(0);

} /* end _sSavSiz */




#define _SMAXSPN	"Maximum No. of Gens. w/o Evaluation    : %d\n"
#define	_DMAXSPN	"Maximum No. of Gens. w/o Evaluation    [        %1d] : "

int
_sMaxSpn(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sBmpIvl();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DMAXSPN, Maxspin);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Maxspin);
		}
		else {
			fscanf(fp, _SMAXSPN, &Maxspin);
		}
	}
	else {
		fprintf(fp, _SMAXSPN, Maxspin);
	}
	_sBmpIvl(fp, Flg);

	return(0);

} /* end _sMaxSpn */




#define _SBMPIVL	"Interval for Bitmap Dumps (Gens)       : %d\n"
#define	_DBMPIVL	"Interval for Bitmap Dumps (Gens)       [        %1d] : "

int
_sBmpIvl(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	int		 _sDmpIvl();
	char		 Str[NSZ];
	char		*gets();
	
	if (Flg) {
		if (fp == stdin) {
			printf(_DBMPIVL, PgmFrq);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &PgmFrq);
		}
		else {
			fscanf(fp, _SBMPIVL, &PgmFrq);
		}
	}
	else {
		fprintf(fp, _SBMPIVL, PgmFrq);
	}
	_sDmpIvl(fp, Flg);

	return(0);

} /* end _sBmpIvl */




#define _SDMPIVL	"Interval for Population Dumps (Gens)   : %d\n"
#define	_DDMPIVL	"Interval for Population Dumps (Gens)   [        %1d] : "

int
_sDmpIvl(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sOptStr();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DDMPIVL, Dump_freq);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%d", &Dump_freq);
		}
		else {
			fscanf(fp, _SDMPIVL, &Dump_freq);
		}
	}
	else {
		fprintf(fp, _SDMPIVL, Dump_freq);
	}
	_sOptStr(fp, Flg);

	return(0);

} /* end _sDmpIvl */




#define _SOPTSTR	"Internal GA-Options                    : %s\n"
#define	_DOPTSTR	"Internal GA-Options                    [      %3s] : "

int
_sOptStr(fp, Flg)
FILE			*fp;
register int		 Flg;

{	
	int		 _sRngSed();
	char		 Str[NSZ];
	char		*gets();

	if (Flg) {
		if (fp == stdin) {
			printf(_DOPTSTR, Options);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%s", Options);
		}
		else {
			fscanf(fp, _SOPTSTR, Options);
		}
	}
	else {
		fprintf(fp, _SOPTSTR, Options);
	}
	_sRngSed(fp, Flg);

	return(0);

} /* end _sOptStr */




#define _SRNGSED	"Seed for Random Number Generator       : %u\n"
#define	_DRNGSED	"Seed for Random Number Generator       [%u] : "

int
_sRngSed(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	
	char		 Str[NSZ];
	char		*gets();
	int		 _sFilNam();

	if (Flg) {
		if (fp == stdin) {
			printf(_DRNGSED, OrigSeed);
			gets(Str);
			if (strlen(Str))
				sscanf(Str, "%u", &OrigSeed);
		}
		else {
			fscanf(fp, _SRNGSED, &OrigSeed);
		}
	}
	else {
		fprintf(fp, _SRNGSED, OrigSeed);
	}
	_sFilNam(fp, Flg);

	return(0);

} /* end _sRngSed */




#define	_DFILNAM	"Suffix of GA-Data Infile               [%s] : "

int
_sFilNam(fp, Flg)
FILE			*fp;
register int		 Flg;

{
	
	char		 Str[NSZ];
	char		*gets(),
			*CrtFilNam();

	if (Flg && (fp == stdin)) {
		strcpy(Sfx, CrtFilNam());
		printf(_DFILNAM, Sfx);
		gets(Str);
		if (strlen(Str))
			sscanf(Str, "%s", Sfx);
	}

	return(0);

} /* end _sFilNam */




char *
CrtFilNam()

{

	char		 FilNam[NSZ],
			 Str[NSZ];

	char	        *strcat();

	sprintf(FilNam, "%d.", FctNbr);

	if (FctDim != 0) {
		sprintf(Str, "%d.%d.", FctDim, ChrLen);
	}
	else {
		sprintf(Str, "%d.", ChrLen);
	}
	strcat(FilNam, Str);

	if (Eliteflag) {
		sprintf(Str, "%c", 'E');
		strcat(FilNam, Str);
	}

	sprintf(Str, "%c", SltScm);
	strcat(FilNam, Str);

	switch (SltScm) {

		case PRP_SLT: 	
		case MLR_SLT:
		case MLC_SLT: 	
			break;

		case BZM_SLT:	
			sprintf(Str, "%g:%d", CtlPar, ChnLgt);
			strcat(FilNam, Str);
			break;

		case BRK_SLT:
		case IRK_SLT:
		case WRK_SLT:	
			sprintf(Str, "%g.", Eta_max);
			strcat(FilNam, Str);
			break;

		default:	
			break;
	}

	if (Rho != 1) {
		sprintf(Str, "%d:", Rho);
		strcat(FilNam, Str);
	}

	if (Mu != Popsize) {
		sprintf(Str, "%d:", Mu);
		strcat(FilNam, Str);
	}
	
	sprintf(Str, "%d", Popsize);
	strcat(FilNam, Str);

	sprintf(Str, "%c", MttScm);
	strcat(FilNam, Str);

	switch (MttScm) {

		case STD_MTT:	
			sprintf(Str, "%g", M_rate);
			strcat(FilNam, Str);
			break;

		case ADX_MTT:	
			sprintf(Str, "%d:%d:%g", NbrMttRts, M_bits, M_rate);
			strcat(FilNam, Str);
			break;

		case ADT_MTT:	
			sprintf(Str, "%d:%d", NbrMttRts, M_bits);
			strcat(FilNam, Str);
			break;
	
		default:	
			break;
	}
	strcat(FilNam, RecScm);

	if ((RecScm[0] == STD_REC) || (RecScm[1] == STD_REC)) {
		sprintf(Str, "%d.", C_points);
		strcat(FilNam, Str);
	}
	
	sprintf(Str, "%g", C_rate);
	strcat(FilNam, Str);

	return(FilNam);
	
} /* end CrtFilNam */

/*** end of file ***/
