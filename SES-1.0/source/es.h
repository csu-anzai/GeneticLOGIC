/*
 *      SES - Simple Evolution Strategy
 *
 *      Copyright (C) 1994 Joachim Sprave
 *
 *      This program is free software; you can redistribute it and/or modify
 *      it under the terms of the GNU General Public License as published by
 *      the Free Software Foundation; either version 2 of the License, or
 *      (at your option) any later version.
 *
 *      This program is distributed in the hope that it will be useful,
 *      but WITHOUT ANY WARRANTY; without even the implied warranty of
 *      MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *      GNU General Public License for more details.
 *
 *      You should have received a copy of the GNU General Public License
 *      along with this program; if not, write to the Free Software
 *      Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 */


/*
 *      See README for details of ES.
 *      Send bugs (better: bug descriptions) and comments to
 *
 * /---------------------------------------------------------------------\
 * | Joachim Sprave                  joe@ls11.informatik.uni-dortmund.de |
 * |                      //////\\                                       |
 * | Univ. Dortmund      /        \        44221 Dortmund                |
 * | Dept. CS           _|  _   _ |_       Tel.: +49-231-755 4678        |
 * | Systems Analysis  |.|-(.)-(.)+.|      Fax : +49-231-755 2450        |
 * \------------------  \|    J   |/  -----------------------------------/
 *                       \   ---  /
 *                        \      /
 *                         "####"
 */
#include <stdio.h>
#include <limits.h>

#if defined(DEBUG)
#define Trace(s) fprintf(stderr,"Trace: %s\n",s)
#else
#define Trace(s)
#endif

#if !defined(EXIT_SUCCESS)
#	define EXIT_SUCCESS 0
#	define EXIT_FAILURE 1
#endif

#if !defined(RAND_MAX)
#	define RAND_MAX INT_MAX
#endif


typedef enum {
        none, discrete, inter_a, inter_g
} reco_t;


typedef enum {
        plus, comma
} select_t;


typedef struct {
	int	valid;
	double	value;
} Fitness;

typedef struct {

	double	*x;
	double	*sigma;
	Fitness	fit;

} Individual;


typedef Individual *Population;

typedef struct {

  char		name[256];	/* Name of this optimization		*/
  char		run[256];	/* Name of this experiment		*/
  int		mu;		/* No. of parents			*/
  int		lambda;		/* No. of offspring			*/
  int		select;		/* Type of selection			*/
  int		vars;		/* No. of variables			*/
  int		sigmas;		/* =1 => n sigma			*/
  int		reco_x;		/* recombination type			*/
  int		reco_s;		/* recombination type			*/
  double	tau0;		/* variance of alpha			*/
  double	taui;		/* variance of sigma_i			*/
  int		maxgen;		/* Max. no. of generations		*/
  int		maxcalls;	/* Max. no. of function calls		*/
  double	min_init_x;	/* minimum initial x[i]			*/
  double	max_init_x;	/* maximum initail x[i]			*/
  double	min_init_s;	/* minimum initial sigma[i]		*/
  double	max_init_s;	/* maximum initial sigma[i]		*/
  double	smin;		/* minimum sigma[i]			*/
  double	smax;		/* maximum sigma[i]			*/
  int		dmpint;		/* Dump interval			*/
  int		seed;		/* Random seed				*/
  char		logfile[256];
  int		flushlog;
  int		pixmon;
  int		showxs;
  int		showy;
  int		genno;		/* Generation number			*/
  long		calls;		/* No of function calls			*/
  Population	pop;

} Field;


void crossover (Field *f, Individual *child);

void mutate(Field *f, Individual *current);

Population newpop        (int size, int n_sigma, int nvars);
void       initpop       (Field *f);

void       cycle         (Field *f);
void       generate      (Field *f);

int        measure       (Field *f);
void       dump          (Field *f);

double     randreal      (void);
double	   normrand      (double m, double sigma);
int        randint       (int lo,int hi);

int       better         (Individual *a, Individual *b);

extern Fitness	eval	(double *x, int n);

extern FILE *logfile;
extern FILE *bstfile;




#define MAX(a,b) ((a)>(b)?(a):(b))
#define MIN(a,b) ((a)<(b)?(a):(b))
