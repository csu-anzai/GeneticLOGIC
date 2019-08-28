/* $Id: satsp.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : satsp.h                                                       */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_SATSP_H
#define TSP_SATSP_H

#include "define.h"


#define SATSP_ABORT_CHAR	'q'

#define SATSP_USAGE	"ParaTSP 1.0\t\t\t(c) 1994 by Holger Totzke\n\n\
Usage: satsp [Options]\n\
Options:\n\
\t-A tspfi\t\t# TSP input file\n\
\t-F tmpfa\t\t# temperature control factor\n\
\t-J jmax \t\t# number of steps per temperature levels\n\
\t-N nmax \t\t# number of temperature levels\n\
\t-S A,T  \t\t# algorithm\n\
\t   A    \t\t# simulated annealing\n\
\t   T    \t\t# threshold accepting\n\
\t-T tmpst\t\t# start temperature\n\
\t-d      \t\t# show results on display\n\
\t-e noexp\t\t# total number of experiments\n\
\t-h      \t\t# this help information\n\
\t-?      \t\t#\n\
\t-i comin\t\t# communication interval in temperature steps\n\
\t-m minqu\t\t# minimal tour length (quality) to abort\n\
\t-p nproc\t\t# number of processors\n\
\t-s seed \t\t# seed for random number generator\n\
\t-t r,p,g,o,h,t\t\t# type of communication topology\n\
\t   r    \t\t# ring\n\
\t   p    \t\t# pipe\n\
\t   g    \t\t# 2d-grid\n\
\t   o    \t\t# 2d-torus\n\
\t   h    \t\t# hypercube\n\
\t   t    \t\t# tree\n\
\n"

#define	SATSP_OPTSTR	"A:F:J:N:S:T:de:hi:m:p:s:t:?"

#define ALG_SAN		'A'
#define ALG_TAC		'T'


#ifdef USE_PROTO
extern void sa_init_values(void);
extern void sa_input(int, char **);
extern double sa_totallength(TOUR *);
extern double sa_evaluate(void);
extern BOOLEAN sa_accept(double, double);
extern void sa_reconnect(TOUR, TOUR);
extern void sa_control(void);
extern void sa_initialize(void);
extern BOOLEAN sa_temp_done(int, int);
extern BOOLEAN sa_done(int);
extern void sa_comm_neighbour(void);
extern void sa_algorithm(void);
extern int main(int, char **);
#else
extern void sa_init_values();
extern void sa_input();
extern double sa_totallength();
extern double sa_evaluate();
extern BOOLEAN sa_accept();
extern void sa_reconnect();
extern void sa_control();
extern void sa_initialize();
extern BOOLEAN sa_temp_done();
extern BOOLEAN sa_done();
extern void sa_comm_neighbour();
extern void sa_algorithm();
extern int main();
#endif


#endif


/*** end of file ***/
