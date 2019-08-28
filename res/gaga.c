#include <stdio.h>
#include <math.h>
#include "gaga.h"
#include "globs.h"
#include "gagaglobs.h"


char *version   = "GALN version 9(b)  Thu Aug 25 16:21:06 GMT-0:00 1988 \n\n";

/*
 * This file contains the GAGA procedure along with additional declarations
 *
 * Code to serve as an example is in main.c.
 * The example optimisation problem involves 10
 * parameters, is multi-modal, and involes pairwise parameter interactions;
 * see AppCostfunc (near bottom of file main.c)
 *
 * To run this program on a Unix machine
 * % cc -o gaga main.c gaga.c -lm
 * % gaga
 *
 *      gaga(CostFunc, ApplicationInfo, BestOrganism,
 *                                              NoGenes, LowestCost, Param)
 *      double          (*CostFunc)();
 *      void            (*ApplicationInfo)();
 *      tporganism      BestOrganism;
 *      int             NoGenes;
 *      double           *LowestCost;
 *      tpGAparams      Param;
 *
 * where
 *
 *      (double)CostFunc(organism)
 *      tporganism organism;
 *
 * and
 *      (void)ApplicationInfo(aorganism, acost, ageneration, stopflag)
 *      tporgansmism    aorganism;
 *      double           acost;
 *      int             ageneration;
 *      int            *stopflag;
 *
 *
 *                      GAGA
 *
 *
 *      A Genetic Algorithm for General Application
 *
 *      ============================================
 *
 * Origional by Hilary Adams, University of York, 1987
 *
 * Modifications by Ian Poole, University College London, 1987-88.
 *
 *       mail addr.: ipoole@uk.ac.ucl.cs
 *
 * C-ised by Jon Crowcroft, UCL-CS, 1988
 *
 *       mail addr.: jon@uk.ac.ucl.cs
 *
 *  The algorithm comes from various sources, probably the main influence
 *  being:
 *
 *  GREFENSTETTE, J.J, "Optimisation of control parameters for genetic
 *  algorithms".  IEEE Trans on Systems, Man and Cybernetics, SMC-16(1)
 *  122-128
 *
 *
 *  This is the procedure that will be called from an applications program.
 * The parameters have the following function:
 *
 * COSTFUNC      - The function to be minimised.  The parameter space to
 *                 this function is an array of integers - 'tporganism';  it
 *                 is up to the user to map this structure onto their own
 *                  task specific solution space.
 *
 * APPLICATIONINFO-This is called after each generation. It may be used to
 *                 display progress of the search, and to terminate it by
 *                 setting STOPFLAG to true.
 * BESTORGANISM  - is the best solution found by the algorithm.
 *
 * LOWESTCOST    - is the cost of the best organism.
 *
 * PARAM         - is a bunch of control parameters which effect the working
 *              of the GA.  See the TYPE definition of TPGAPARAMS for
 *                 details.
 *
 */

/*
 * Note disgusting use of globals:
 * Population
 * Popcosts
 * Organism
 */
gaga(CostFunc, ApplicationInfo, BestOrganism, NoGenes, LowestCost, Param)
double           (*CostFunc)();
void            (*ApplicationInfo)();
tporganism      BestOrganism;
int             NoGenes;
double           *LowestCost;
tpGAparams      Param;
{
	tppop           Population;
	tppopcosts      Popcosts;
	tporganism      Organism;
	tpchromosome    BestChromSoFar;
	pinteger        generation, stopflag = false;

	Initialisation(&Param, &BestOrganism,
			&Population, &Popcosts, &Organism, &BestChromSoFar,
			NoGenes);
	setuppop (Population);
	assign(Population[1], BestChromSoFar, sizeof(tpgene)*(chromlength+1));
	for(generation=1; generation <= genmax && !stopflag; generation++) {
		DevelopAndEvaluateCosts (Population, Popcosts, CostFunc);
		SelectAndEvaluateFittness (Population, Popcosts,
						BestChromSoFar, &Param);
		if (Param.debug & 2)
			printcostinfo(Population, Popcosts, &Param, Popcosts[1].individual);
		Develop (Population[Popcosts[1].individual], Organism);
		(*ApplicationInfo) (Organism, Popcosts[1].cost,
			generation, &stopflag);
		mate (Population, Popcosts);
		InvertPopulation (Population);
		MutatePopulation (Population);
		setupcosts (Popcosts);
	}
	Develop (BestChromSoFar, BestOrganism);
	*LowestCost = BestCostSoFar;
}


/*
 * Should be called once before random.
 */
static int prime       = 3137;
setrandom (v1, v2, v3)
int *v1, *v2, *v3;
{
	*v1 = abs(*v1) % prime;
	*v2 = abs(*v2) % prime;
	*v3 = abs(*v3) % prime;
	if (*v1 + *v2 + *v3 == 0) {
		*v1 = 62;
		*v2 = 1903;
		*v3 = 205;
	}
}


/*
 * Returns an integer between 1 and 3137.
 */
static int v1, v2, v3;
grandom()
{
	int v4;
	v4 = v1 + v2;
	if (v4 >= prime)
		v4 -= prime;
	v1 = v2 + v3;
	if (v1 >= prime)
		v1 -= prime;
	v2 = v3 + v4;
	if (v2 >= prime)
		v2 -= prime;
	v3 = v4 + v1;
	if (v3 >= prime)
		v3 -= prime;
	return v3 + 1;
}


/*
 * Generate a random real number in the range 0.0 .. 1.0
 */
double
RandomReal()
{
	return ( ((double)grandom() - 1.0) / (prime - 1.0) );
}


/*
 * Write out a chromosome - IDs and gene values
 */
writechrom (chrom)
tpchromosome chrom;
{
	rglocus i;
	(void)printf("GID: ");
	for (i = 1; i <= chromlength; i++)
		(void)printf("%4d", chrom[i].geneid);
	(void)printf("\n");
	(void)printf("VAL: ");
	for(i = 1; i<= chromlength; i++)
		(void)printf("%4d", chrom[i].value);
	(void)printf("\n");
}


/*
 * Usefull for debug
 */
printcostinfo(population, popcosts, param, individual)
tppop           population;
tppopcosts      popcosts;
tpGAparams     *param;
int             individual;
{
int i;
	(void)printf("Cmax = %6.2le\n", Cmax);
	for(i=1; i<=popsize; i++) {
		if (param->debug & 4)
			writechrom (population[individual]);
		(void)printf("%3d: ", popcosts[i].individual);
		(void)printf("%6.4lf ", popcosts[i].cost);
		(void)printf("F=%6.4lf\n", popcosts[i].fittness);
	}
	(void)printf("\n");
}


swop (i1,i2)
int *i1, *i2;
{
	int tmp = *i1;
	*i1 = *i2;
	*i2 = tmp;
}


/*
 * Returns a chromosome number chosen with even prob. disp.
 */
poprange
PickChromEven()
{
	return (poprange) ((grandom() % popsize) + 1);
}


/*
 * Returns a locus number chosen with even prob. disp.
 */
rglocus
PickLocusEven()
{
	return (rglocus)(grandom() % chromlength) + 1;
}


/*
 * Returns a random bit number, 0..Nbits-1
 */
bitrange
PickBitEven()
{
	return (grandom() % nbits) + 1;
}


/*
 * Perform an inversion on the given chromosome
 */
Invert (structure)
tpchromosome structure;
{
	int break1, break2;
	pinteger i, j;
	tpchromosome tempchrom;

	tempchrom = (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(tempchrom, tpchromosome);

	break1 = PickLocusEven();
	do {
		break2 = PickLocusEven();
	} while (break2 == break1);
	if (break2 < break1)
		swop (&break1, &break2);
	assign(structure, tempchrom, sizeof(tpgene)*(chromlength+1));
	for (i = break1, j=break2; i<= break2; i++, j--)
		structure[i] = tempchrom[j];

	lose(tempchrom);
}



/*
 * setupchrom
 * Arrange genes on the chromosome.  Meta gens (-ve id) come fist, ie
 * starting at 1, followed by the +ve organism (expressed) genes.
 * note there is no gene zero.
 */
setupchrom (chromosome)
tpchromosome chromosome;
{
	pinteger j ;

	if (nmetagenes != 0)
		for (j=1; j<=nmetagenes; j++) {
			chromosome[j].value = (grandom() % (maxgeneval + 1));
			chromosome[j].geneid = -j;
		}
	for (j = nmetagenes+1; j<=chromlength; j++) {
		chromosome[j].value = (grandom() % (maxgeneval + 1));
		chromosome[j].geneid = j - nmetagenes;
	}
 }

/*
 * initialises the values of the genes randomly for each chromosome.
 */
#define permutfact 0.7

setuppop (population)
tppop   population;
{
	poprange i;
	pinteger n;

	for (i=1; i<=popsize; i++) {
		setupchrom (population[i]);

/* Easy (sub-optimal) way of randomising order */
		for (n = 1; n <= (int)(chromlength * permutfact); n++)
			Invert (population[i]);
	}
}

/*
 * For each chromosome initialises the individual field of each record.
 */
setupcosts (popcosts)
tppopcosts popcosts;
{
	poprange i;
	for (i = 1; i<=popsize; i++)
		popcosts[i].individual = i;
}

int
twopower (power)
genevalrange power;
{
	int forret, i;
	for (i = 1, forret=1; i<= power; i++)
		forret = forret * 2;
	return forret;
}

/*
 * Return the sum of the fittnesses of the population
 */
double
CalcTotalFittness(popcosts)
tppopcosts      popcosts;
{
	poprange i;
	double tot;
	for(i = 1, tot = 0.0; i <= popsize; i++)
		tot = tot + popcosts[i].fittness;
	return tot;
}


 /*
  * Select a chromosome from a probability distribution in proportion
  * to its fittness
 */

poprange
PickChromProp(popcosts, totalfittness)
tppopcosts popcosts;
double totalfittness;
{
	pinteger cnum, dum;
	double point, totup;

	point = RandomReal() * totalfittness;
	for(cnum=1, totup = 0.0; cnum <= popsize && totup <= point; cnum++) {
		totup += popcosts[cnum].fittness;
		dum = cnum;
	}
	return popcosts[dum].individual;
 }


/*
 * Orders the genes in structure2 to match those of structure1 and
 * places the result in structure2.
 */
rearrange (structure1, structure2)
tpchromosome structure1;
tpchromosome structure2;
{
	int i;
	tpchromosome glist;
	glist = (tpchromosome)calloc(nmetagenes+norggenes+1, sizeof(tpgene));
	any(glist, tpchromosome);

	for(i = 1;i<=chromlength;i++)
		glist[structure2[i].geneid + nmetagenes] = structure2[i];
	for(i = 1;i<=chromlength;i++)
		structure2[i] = glist[structure1[i].geneid+nmetagenes];

	lose(glist);
}




swopparts (g1, g2, bit)
genevalrange *g1, *g2;
bitrange bit;
{
	genevalrange l1, u1, l2, u2;
	int        tpb;

	tpb = twopower (bit);
	u1 = ((int) *g1 / (int)tpb) * tpb;
	u2 = ((int) *g2 / (int)tpb) * tpb;
	l1 = *g1 - u1;
	l2 = *g2 - u2;
	*g1 = u2 + l1;
	*g2 = u1 + l2;
}

/*
 * Perform crossover of the two parents to produce two offspring
 */
crossover (p1, p2, o1, o2)
tpchromosome p1, p2, o1, o2;
{
	int             break1, break2, k;
	bitrange        breakbit1, breakbit2;
	tpgene          o1b1, o2b1, o1b2, o2b2;
/*
 * Randomly select the two crossover points
 */
	break1 = PickLocusEven();
	do {
		break2 = PickLocusEven();
	} while (break1 == break2);

	if (break1 > break2 )
		swop (&break1, &break2);

	for (k = 1; k<=break1; k++) {
		o1[k] = p1[k] ;
		o2[k] = p2[k] ;
	}
	for(k = (break1+1);k<=break2; k++) {
		o1[k] = p2[k];
		o2[k] = p1[k];
	}
	for(k = (break2+1); k<=chromlength; k++) {
		o1[k] = p1[k];
		o2[k] = p2[k];
	}

	breakbit1 = PickBitEven();
	breakbit2 = PickBitEven();
	o1b1 = o1[break1];
	o2b1 = o2[break1];
	o1b2 = o1[break2];
	o2b2 = o2[break2];
	swopparts (&o1b1.value, &o2b1.value, breakbit1);
	swopparts (&o1b2.value, &o2b2.value, breakbit2);
	o1[break1] = o1b1;
	o2[break1] = o2b1;
	o1[break2] = o1b2;
	o2[break2] = o2b2;
 }


CopyPop (fromPop, toPop)
tppop fromPop, toPop;
{
	poprange i;
	for (i = 1; i<=popsize; i++)
		assign(fromPop [i], toPop[i], sizeof(tpgene)*(chromlength+1));
}



mate(population, popcosts)
tppop           population;
tppopcosts      popcosts;
{
	tppop   temppop;                /* We have to built a seperate copy */
	int     ploc, i;                /* Where to put the next offspring */
	double   totalfittness;          /* Total fittness of population */
	poprange pn1, pn2;
	tpchromosome partner1, partner2, offspring1, offspring2;

	temppop = (tppop)calloc(popsize+1, sizeof(tppop));
	any(temppop, tppop);

	for(i=0; i<=popsize; i++) {
		temppop[i] = (tpchromosome)calloc(chromlength+1,sizeof(tpgene));
		any(temppop[i], tpchromosome);
	}
	partner1 = (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(partner1, tpchromosome);
	partner2 = (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(partner2, tpchromosome);
	offspring1= (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(offspring1, tpchromosome);
	offspring2= (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(offspring2, tpchromosome);

/* Elitist strategy */
	assign(population[popcosts[1].individual], temppop[1], 
				sizeof(tpgene)*(chromlength + 1));
	for(ploc=2; ploc <= popsize; ploc++) {
		/* Needed by PickChromProp */
		totalfittness = CalcTotalFittness(popcosts);
		pn1 = PickChromProp(popcosts, totalfittness);
		do {
			pn2 =  PickChromProp(popcosts, totalfittness);
		} while (pn2 == pn1);

		assign(population [pn1], partner1, sizeof(tpgene)*(chromlength + 1));
		assign(population [pn2], partner2, sizeof(tpgene)*(chromlength + 1));

		rearrange (partner1, partner2);
		crossover (partner1, partner2, offspring1, offspring2);

		/* But we'll only use one of the offspring */
		assign(offspring1, temppop[ploc], sizeof(tpgene)*(chromlength + 1));
	}
/*
 * New generation supersedes the old structure[foo].
 */
	CopyPop (temppop, population);

	for(i=0; i<= popsize; i++)
		lose(temppop[i]);
	lose(temppop);
	lose(partner1);
	lose(partner2);
	lose(offspring1);
	lose(offspring2);
}

/*
 * Simulate a single bit mutation
 */
mutate(structure)
tpchromosome structure;
{
	int     howmuch;
	rglocus foo;

	howmuch = twopower (PickBitEven());

	foo = PickLocusEven();
	if (odd((int)structure[foo].value / (int)howmuch))
		structure[foo].value -= howmuch;
	else
		structure[foo].value += howmuch;

	switch (grandom() % 2) {
	case 0:
		break;
	case 1:
		if (structure[foo].value > 0)
			structure[foo].value--;
		break;
	case 2:
		if (structure[foo].value < maxgeneval)
			structure[foo].value++;
		break;
	}
	if (structure[foo].value > maxgeneval)
		structure[foo].value = maxgeneval;
}
/*
 * Apply 'mutnum' single bit mutatuins to the popuation
 * Also randomly increment or decrement.
 */
MutatePopulation (population)
tppop   population;
{
	pinteger i;
	for(i = 1; i <= mutnum; i++)
		mutate (population[PickChromEven()]);
}



/*
 * Carry out 'invertnum' inversions on the population
 */
InvertPopulation (population)
tppop population;
{
	int i;
	for(i = 1; i <= invertnum; i++)
		Invert (population[PickChromEven()]);
}


/*
 * Mapping from chromosome to organism.  We ignore metagenes.
 */
Develop(chromosome, organism)
tpchromosome chromosome;
tporganism organism;
{
	rglocus l;
	for (l = 1; l <= chromlength; l++)
		if (chromosome[l].geneid > 0)
			organism [chromosome[l].geneid] =
				chromosome[l].value;
}


/*
 * Develop each chromosome into an organism
 * and then cost the organsim with the provided 'costfunc'.  Save
 * all the costs in 'info'.                                                  
 */
DevelopAndEvaluateCosts (pop, popcosts, costfunc)
tppop           pop;
tppopcosts      popcosts;
double           (*costfunc)();
{
	poprange i;
	tporganism organism;

	organism = (tporganism)calloc(chromlength+1, sizeof(tporganism));
	any(organism, tporganism);

	for(i = 1; i <= popsize; i++) {
		Develop (pop[i], organism);
		popcosts[i].cost = (*costfunc) (organism);
	}

	lose(organism);
}

/*
 * Return the highest cost this generation, out of the 'prsave' we allow to
 * reproduce.
 */
double
FindMaxCost(popcosts)
tppopcosts popcosts;
{
	poprange i;
	double highest;

	for(i = 1, highest = -1.0E10; i<= savenum; i++) /* Very low! */
		if(popcosts [i].cost > highest)
			highest = popcosts [i].cost;
	return highest;
 }



/*
 * Return the maximum cost over the recorded generations
 */
double
FindMaxMax(param)
tpGAparams     *param;
{
	int i;
	double highest;
	for (i = 1, highest = -1.0E10; i <= param->Window; i++)
		if (MaxCostHistory[i] > highest)
			highest = MaxCostHistory[i];
	return highest;
}

cmppop(p1, p2)
info *p1, *p2;
{
	if (p1->cost == p2->cost)
		return 0;
	if (p1->cost > p2->cost)
		return 1;
	return -1;
}

/*
 * Fill in the 'fittness' component of each info record.  Fittness is
 * possatively related to goodness.  Fittness = Cmax - cost, where Cmax =MAX
 * cost over the last W generations  [Grefenstette].  We set W as a constant.
 */
SelectAndEvaluateFittness (pop, popcosts, best, param)
tppop           pop;
tppopcosts      popcosts;
tpchromosome    best;
tpGAparams     *param;
{
	poprange i;
	int     g;
	double Bestcostthisgen;      /* The cost of the best chromosome. */

/*
 * Hutch up the history of maximum cost, loosing that of the oldest gen.
 */
	for(g =  param->Window-1; g>=1; g--)
		MaxCostHistory[g+1] = MaxCostHistory[g];
/*
 * Sort by COST, lowest (best) to entry [0]
 */
	qsort((char *)&(popcosts[1]), popsize, sizeof(*popcosts), cmppop);

	MaxCostHistory [1] = FindMaxCost(popcosts); /* Fill in for this gen */
/*
 * Fudge ! - makes fittnesses non-zero
 */
	Cmax = FindMaxMax(param) + 1.0E-6;

	for(i = 1; i <= savenum; i++)
		popcosts[i].fittness = Cmax - popcosts[i].cost;
	for(i = savenum + 1;i<=popsize; i++)
		popcosts[i].fittness = 0.0;

	Bestcostthisgen = popcosts[1].cost;

	if (param->debug & 4)
		writechrom (pop[popcosts[1].individual]);

	if (Bestcostthisgen < BestCostSoFar) {
		BestCostSoFar = Bestcostthisgen;
		assign(pop[popcosts[1].individual], best, sizeof(tpgene)*(chromlength + 1));
		if (param->debug & 1)
			writechrom (best);
	}
}

/*
 * Miscellaneous, called at start of main body
 */
Initialisation(param, BestOrganism, Population, Popcosts, 
		Organism, BestChromSoFar, NoGenes)
tpGAparams     *param;
tporganism      *BestOrganism;
tppop           *Population;
tppopcosts      *Popcosts;
tporganism      *Organism;
tpchromosome    *BestChromSoFar;
int             NoGenes;
{
	int             i;
	tpchromosome *tp;

	if (param->debug & 8)
		(void)printf(version);

	popsize    = param->popsize;
	genmax     = param->costexmax / popsize;
	mutnum     = rint(param->mutrate * popsize * chromlength);
	invertnum  = rint(param->invrate * popsize);
	savenum    = rint(param->prsave * popsize);
	BestCostSoFar = 1.0E10;               /* Big number */

/* Allocate store for population of organisms with No of Genes */
	norggenes = NoGenes;
	chromlength = norggenes + nmetagenes;

	*Population= (tppop)calloc(popsize+1, sizeof(tppop)); 
	any(*Population, tppop);

	for(i=0, tp = *Population; i<= popsize; i++, tp++) {
		*tp = (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
		any(*tp, tpchromosome);
	}

	*BestChromSoFar = (tpchromosome)calloc(chromlength+1, sizeof(tpgene));
	any(*BestChromSoFar, tpchromosome);

	*Organism = (tporganism)calloc(chromlength+1, sizeof(tpgene));
	any(*Organism, tporganism);

	*Popcosts = (tppopcosts)calloc(popsize+1, sizeof(info));
	any(*Popcosts, tppopcosts);

/* So that other fields are initialised */
	assign(*BestOrganism, *Organism, sizeof(tpgene)*(chromlength + 1));

	setupcosts (*Popcosts);
	for(i=1; i<=MaxWindow; i++)
		MaxCostHistory [i] = 0.0; /* Problem !! */

	v1 = param->r1;
	v2 = param->r2;
	v3 = param->r3;

	setrandom (&v1, &v2, &v3); /* Seed the random number generator */
}

#ifndef sun


/*
 * algorithm for rint(x) in pseudo-pascal form ...
 *
 * real rint(x): real x;
 *      ... delivers integer nearest x in direction of prevailing rounding
 *      ... mode
 * const        L = (last consecutive integer)/2
 *        = 2**55; for VAX D
 *        = 2**52; for IEEE 754 Double
 * real s,t;
 * begin
 *      if x != x then return x;                ... NaN
 *      if |x| >= L then return x;              ... already an integer
 *      s := copysign(L,x);
 *      t := x + s;                             ... = (x+s) rounded to integer
 *      return t - s
 * end;
 *
 * Note: Inexact will be signaled if x is not an integer, as is
 *      customary for IEEE 754.  No other signal can be emitted.
 */
#ifdef VAX
static long Lx[] = {0x5c00,0x0};                /* 2**55 */
#define L *(double *) Lx
#else   /* IEEE double */
static double L = 4503599627370496.0E0;         /* 2**52 */
#endif
double
rint(x)
double x;
{
	double s,t,one = 1.0,copysign();
#ifndef VAX
	if (x != x)                             /* NaN */
		return (x);
#endif
	if (copysign(x,one) >= L)               /* already an integer */
	    return (x);
	s = copysign(L,x);
	t = x + s;                              /* x+s rounded to integer */
	return (t - s);
}
#endif sun
