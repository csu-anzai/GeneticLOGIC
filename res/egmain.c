#include <math.h>
#include <stdio.h>
#include "gaga.h"
#include "globs.h"


/*
 * Following is application specific code - the problem to be solved.
 * It will contain three components.
 *
 *   1) COSTFUNC - the function to be optimised - the crux of the problem.
 *
 *   2) APPLICATIONINFO - This will be called by GENETICALG after each
 *                        generation to offer information on the progress of
 *                        the search.
 *
 *   3) The main program wich will set up the control parameter record
 *      and call gaga
 *
 *
 * The following example application is totally trivial.
 */

/* An Example of an applications program to use the Genetic Algorith */


 /*
  * A simple cost function.  Clearly this is where the bulk of the
  * applications code will normally reside.
  * Here we simply implement:
 */

#define b 0.1           /* > 1 => only one local min */
#define a 0.4           /* Granularity of search, in radians */
#define Nterms 5        /* Number of summed terms */

double
WaveyFunc (x)
double x;
{
       return (fabs(a*b*x) - cos(fabs(a*x)) + 1.0);
}


double
AppCostFunc (organism)
tporganism organism;
{
rgorganism      i;
double           t = 0.0;
	for (i=1; i<=Nterms; i++)
		t = t + WaveyFunc ((double)
				(organism [i] - organism [Nterms + i]));
	return t;
}

WriteOrganism (org)
tporganism org;
{
	rgorganism i;
	(void)printf("(");
	for(i=1; i<=norggenes; i++)
		(void)printf("%4d ", org[i]);
	(void)printf(")");
}


/*
 *Called from 'gaga' after each generation
 */
void
AppInfo(aorganism, acost, ageneration, stopflag)
tporganism      aorganism;
double          acost;
int             ageneration;
int            *stopflag;
{
	(void)printf("Gen. %1d:",ageneration);
	WriteOrganism (aorganism);
	(void)printf(" Cost = %6.3lf\n", acost);
	if (acost < 0.000001)
		*stopflag = true;
}


ReadParams (params)
tpGAparams *params;
{
	(void)printf("Enter any 3 integers: ");
	(void)scanf("%d %d %d", &params->r1, &params->r2, &params->r3);
	(void)printf("Population size [2..%1d].............: ",
		maxpopsize);
	(void)scanf("%d", &(params->popsize));
	(void)printf("Mutation rate [0.0..1.0, eg 0.03]...........: ");
	(void)scanf("%lf", &params->mutrate);
	(void)printf("Inversion rate: [0.0..1.0, eg 0.02].........: ");
	(void)scanf("%lf", &params->invrate);
	(void)printf("Proportion to reproduce [0.0..1.0, eg 0.9]..: ");
	(void)scanf("%lf", &params->prsave);
	(void)printf("Fittness window [0..%1d eg 1]..............: ", MaxWindow);
	(void)scanf("%d", &params->Window);
	(void)printf("Max. cost executions [eg 10000]..............: ");
	(void)scanf("%d", &params->costexmax);
	params->debug = (1|8); /* Display 'Elite' chrom, and version */
}



main()        /* Example */
{
	tporganism      AppBestOrganism;
	double          AppLowestCost;
	tpGAparams      AppParams;

	(void)printf("\n");
	ReadParams (&AppParams);
	(void)printf("\n");

	AppBestOrganism = (tporganism)calloc(chromlength+1, sizeof(tpgene));
	any(AppBestOrganism, tporganism);
		
	gaga (AppCostFunc, AppInfo, AppBestOrganism,
			10, &AppLowestCost, AppParams);

	(void)printf("\n");
	(void)printf("Best Solution Found\n");
	WriteOrganism (AppBestOrganism);
	(void)printf(" = %4.2lf\n", AppLowestCost);
}
