/*
   sa2.c   A complete, though simple interactive Genetic Algorithm (GA) system.

   This is the source for the program I described in an article in
   the July 1992 Scientific American's Amateur Scientist column,
   ``Survival of the Fittest Bits.''

   The basic functions of this program are:
   - process some user commands, to set parameters, display things,
   generate a random population of binary strings (genomes), and so on.
   - allow the user to specify schemas to track in the current population,
   both to count intstances, get an average schema fitness in the population,
   and so on.
   - allow the user to run the genetic algorithm (GA), displaying the population
   periodicically, setting parameters to control the GA, and so on.
   - Collect statistics on average fitness, best fitness, and optionally, the
   instance counts and average fitnesses for user-specified schemas.

   For the GA, this uses:
   - a simple 'tournament' selection scheme, with optional retention of
   N top fitness individuals (an 'elitist' selection).
   - a simple 1-point crossover.
   - a simple mutation mechanism.

   Try h command after starting it up for some help on commands, etc.
   (Or see Usage() subroutine at the end of this file).

   Note that there is a fair number of GA software packages available for a number
   of platforms and compilers.  Some of the software must be purchased, but much of
   it is shareware or freeware.  An update-to-date list of software is in the file
   pub/GAucsd/GAsoft.txt available by anonymous ftp from cs.ucsd.edu, which is
   maintained by Nici Schraudolph at UCSD.

   This was developed using Ultrix 4.2, but it should be easy to compile
   under any ANSI compliant C compiler. The Makefile lines I used:

   CFLAGS = -c -O2
   sa2: sa2.c
   cc $(CFLAGS) -o sa2.o sa2.c
   cc -o sa2 sa2.o -lm

   NOTE ==>

   1. Some compilers (e.g., on Suns) can't accept the ANSI function prototypes
   I use here, both for forward declartions and for function definitions.
   So you may have to change the form of the parameter lists in those places.
   Or you could use gcc, with CFLAGS -c -tradition (I am told that works).

   2. I use 4-space tabs, but sometimes I forget and use 4 spaces.
   So if you use 8 space tabs, things main not line up too well.

   3. I can be reached at riolo@um.cc.umich.edu or rlr@home.merit.edu
   if you have comments, bug reports, etc.


   Copyright 1992, Rick L. Riolo. Some mods by -joke 11/4/93

 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <strings.h>
#include <ctype.h>

#include "sa2.h"

unsigned int GenomeLen = 16;	/* current Genome Length */

struct PopStr *CurPop, *NewPop;	/* pointers to current and new population */
struct PopStr *BestIndiv, *WorstIndiv;	/* pointers to highest and lowest fitness individuals */
unsigned int PopSize = 50;	/* current population size */
unsigned int NextId = 1;	/* next id to assign to new individual */
unsigned int NewIndivs = 0;	/* total number of new individuals */

unsigned int CurGen;		/* Generation count, used when GA is running. */

 /* variables for controlling the genetic algorithm */

double MuRate = 0.005;		/* this is prob *per gene* */
unsigned int MuMax = GenomeLEN;	/* max mutations per individual */
unsigned int MuCnt = 0;
unsigned int MuIndCnt = 0;
double XOverProb = 0.75;	/* this is prob per pair */
unsigned int XOverCnt = 0;
double TSProb = 0.75;		/* prob higher fitness individual wins tournament selection */
unsigned int Elitist = 1;	/* Currently, number of times to copy best */


 /* variables for schema counting, fitness calculations, and so on.
    the schemas to count are stored in the Schema array, from 0...SchemaNm-1.
    The fitnesses, counts, etc., are stored in parallel arrays.
    Some arrays have a Generations dimension, in anticipation of the
    more complete version of this that will count and store values over
    multiple generations.
  */

char Schema[SchemaMX][GenomeLEN + 1];
double SchemaFitness[SchemaMX];
unsigned int SchemaCount[SchemaMX][GenerationMX];
int SchemaCountPred[SchemaMX];
double TotalSchemaPred, TotalSchemaPredRight;
unsigned int SchemaNm = 0;

 /* definitions related to the fitness functions */

double scalefactor;		/* to get binary x into range of interest */
double Maxf, Maxfx;		/* max of function found by Restart() brute-force search */
int GenFoundMaxf = -1;		/* Set to generation first find Maxf */
double AveFitness[GenerationMX], BestFitness[GenerationMX];

unsigned int HiF = (double) 0.0;	/* coeffients for the fitness function in sf2() */
unsigned int LoF = (double) 32.0;
unsigned int Xwt = (double) 1.0;

 /* variables and routines to generate random numbers */

char *RandState[RandStateSZ];	/* store state used by random() */
unsigned int RandSeed = 103;	/* user settable seed to get different sequences */

 /* control pop. display format and frequency, and debugging messages */

unsigned int DisplayPopInt = 999;	/* if not 0, display pop every n generations */
unsigned int DisplayPopFmt = 0;	/* default format for pop display */
unsigned int TestFlag = 0;	/* set 1 to see any test/debugging messages compiled */


/**************************************
 *
 * main:  main entry point.
 *
 * First process command line options, which are of the form  ParNameValue
 * where ParName is the name of some parameter that can be set,
 * and Value is the value to set it to (no blank or = sign between them).
 * See Usage() for ParName's that are allowed, and what parameters they set.
 *
 * Next do some initialization, then call Restart to do more initialization.
 *
 * Then enter the command processing loop, and stay there until the user
 * enters exit.  Prompt for and read in commands, and do as user asks.
 * See Usage() for a brief description of the commands allowed.
 *
 **************************************/

void
main (int argc, char **argv)
{
  char inline[256], cmd[16], par1[64], par2[64], *cp;
  unsigned int p, fmt, step, steps;
  struct PopStr *ip;

  for (p = 1; p < argc; ++p)
    {				/* process command-line parameters */
      if (strcmp (argv[p], "h") == 0)
	{
	  Usage ();
	  return;
	}
      else
	{
	  for (cp = argv[p]; isalpha (*cp); ++cp);
	  SetPar (argv[p], cp);
	}
    }

  for (SchemaNm = 0; SchemaNm < SchemaMX; ++SchemaNm)	/* initilization */
    Schema[SchemaNm][0] = '\0';
  SchemaNm = 0;

  CurPop = P1;
  NewPop = P2;
  initstate (RandSeed, RandState, RandStateSZ);		/* initialize system random number generator */

  Restart ("", "");		/* call to initialize ga system, and print some messages */

  for (;;)
    {
      fprintf (stderr, "Cmd? ");
      cp = readline (inline, sizeof (inline), stdin);	/* read command */
      cp = striptoken (cp, cmd, sizeof (cmd), " ");	/* get command into cmd, any par starts at *cp */

      if (strncmp (cmd, "stop", 4) == 0 || strncmp (cmd, "exit", 4) == 0)
	{
	  break;
	}
      else if (strncmp (cmd, "di", 2) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), " ,");	/* split into two paramters */
	  cp = striptoken (cp, par2, sizeof (par2), " ");
	  if (strcmp (par1, "pop") == 0)
	    {
	      if (*par2 == '\0')
		fmt = DisplayPopFmt;
	      else
		fmt = atoi (par2);
	      PrintPop (CurPop, fmt);
	    }
	  else
	    {
	      Display (par1);
	    }
	}
      else if (strncmp (cmd, "set", 3) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), "= ");
	  cp = striptoken (cp, par2, sizeof (par2), " ");
	  SetPar (par1, par2);
	}
      else if (strncmp (cmd, "res", 3) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), ", ");
	  cp = striptoken (cp, par2, sizeof (par2), " ");
	  Restart (par1, par2);
	}
      else if (strncmp (cmd, "ep", 2) == 0)
	{
	  EvaluatePop (CurPop);
	}
      else if (strncmp (cmd, "es", 2) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), " ,");
	  cp = striptoken (cp, par2, sizeof (par2), " ,");
	  EvaluateSchemaOverFunction (par1, par2);
	}
      else if (strncmp (cmd, "d2b", 3) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), " ,");
	  DisplayD2B (par1);
	}
      else if (strncmp (cmd, "b2d", 3) == 0)
	{
	  cp = striptoken (cp, par1, sizeof (par1), " ,");
	  DisplayB2D (par1);
	}
      else if (cmd[0] == 'r' || strcmp (cmd, "st") == 0)
	{
	  if (strcmp (cmd, "st") == 0 || *cp == '\0')
	    steps = 1;
	  else
	    sscanf (cp, "%u", &steps);
	  if (steps < 0 || steps > 500)
	    steps = 1;
	  fprintf (stderr, "  run for %d steps...\n", steps);
	  for (step = 0; step < steps; ++step)
	    {			/* the main loop */
	      ++CurGen;
	      TournamentSelection (CurPop, NewPop);
	      ModifyPopulation (NewPop);
	      ip = CurPop;
	      CurPop = NewPop;
	      NewPop = ip;
	      EvaluatePop (CurPop);
	      if (CurGen % DisplayPopInt == 0)
		PrintPop (CurPop, DisplayPopFmt);
	      if (GenFoundMaxf < 0 && Maxf == BestIndiv->Fitness)
		{
		  GenFoundMaxf = CurGen;
		  fprintf (stderr, "\n => Found max at %d.\n", CurGen);
		}
	    }
	}
      else if (cmd[0] == '?' || cmd[0] == 'h')
	Usage ();
      else if (cmd[0] == '\0');	/* noop */
      else
	{
	  fprintf (stderr, "\nIllegal command. Try di | r | exit | set\n");
	}
    }

  fprintf (stderr, "\ndone.\n");

}				/* end main */


/*******************************************************
 *
 * TournamentSelection  Select parents by running a series of 'tournaments'.
 *
 * The basic idea is to pick pairs from the population, and for each pair,
 * pick the highest fitness to be reproduced with probability TSProb (eg 0.75),
 * otherwise use the lower fitness member of the pair.
 * Continue this until the new population is the desired size.
 *
 * Pick the pairs (uniform) randomly, with replacement.
 *
 * Its typical to automatically reproduce the highest fitness individual,
 * and in this system, its possible to reproduce the top N (where N is
 * the Elitist parameter).
 *
 * For more information about this selection technique,
 * cf. Goldberg and Deb, ...
 *
 *******************************************************/

void
TournamentSelection (struct PopStr CurP[], struct PopStr NewP[])
{
  register unsigned int g;	/* for genes */
  register struct PopStr *higher, *lower, *best;
  unsigned int old1, old2;	/* the candidates in old pop */
  unsigned int np;		/* new pop size */
  unsigned int oldid;		/* used for testing */

  if (Elitist == 1)
    {
      CopyIndiv (BestIndiv, &NewP[0]);	/* just copy the best */
      NewP[0].Id = BestIndiv->Id;	/* keep same Id, GenCreated for this first one */
      NewP[0].GenCreated = BestIndiv->GenCreated;
      np = 1;
    }
  else if (Elitist > 1)
    {				/* copy the best N=Elitist to new pop */
      ReSortPopulation (CurP, PopSize);		/* resort to take more that just 1 best */
      for (np = 0, best = BestIndiv; np < Elitist; ++np, best = best->NextLowerFit)
	{
	  CopyIndiv (best, &NewP[np]);
	  oldid = NewP[np].Id;
	  NewP[np].Id = NextId++;
	  NewP[np].GenCreated = CurGen;
	}
    }
  for (; np < PopSize; np++)
    {				/* run the tournament till be have a full new pop */
      old1 = random () % PopSize;	/* get two different competitors */
      while (1)
	{
	  old2 = random () % PopSize;
	  if (old1 != old2)
	    break;
	}

      if (CurP[old1].Fitness >= CurP[old2].Fitness)
	{			/* figure out which has greater fitness */
	  higher = &CurP[old1];
	  lower = &CurP[old2];
	}
      else
	{
	  higher = &CurP[old2];
	  lower = &CurP[old1];
	}

      if (URand01 < TSProb)
	{			/* the best wins! */
	  CopyIndiv (higher, &NewP[np]);
	  oldid = higher->Id;
	}
      else
	{			/* the weaker wins! */
	  CopyIndiv (lower, &NewP[np]);
	  oldid = lower->Id;
	}

      NewP[np].GenCreated = CurGen;
      NewP[np].Id = NextId++;
      if (TestFlag)
	fprintf (stderr, "\nTS: copy %d to %d.", oldid, NewP[np].Id);
    }

}				/* end TournamentSelection */

/****************************************************
 *
 * CopyIndiv            Copy an individual's Genome and Fitness from one structure to another.
 *
 ****************************************************/

void
CopyIndiv (struct PopStr *From, struct PopStr *To)
{
  register unsigned int g;

  for (g = 0; g < GenomeLen; ++g)
    {				/* NOT copy ID */
      To->Genome[g] = From->Genome[g];
      To->Fitness = From->Fitness;
    }

}				/* end CopyIndiv */

/****************************************************
 *
 * ModifyPopulation     Apply Crossover and Mutation to modifiy a population.
 *
 * Just work through the population by pairs, since they were put in place at random.
 * NOTE: if Elitist > 0, Skip the first one (the best from the last generation).
 *
 * For each pair, perform a single point crossover with probability XOverProb.
 * For each gene of each individual, perform a mutation with probability MuRate.
 * However, only do MuMax mutations per individual.
 *
 * Count the number of crossovers, mutations, individuals changed, and so on.
 *
 ****************************************************/

void
ModifyPopulation (struct PopStr *P)
{
  register unsigned int i, g, xpt, tchar;
  register struct PopStr *ip, *ip1;
  unsigned int maxi = PopSize - 1;
  unsigned int mu1, mu2, newindiv;

  if (Elitist == 0)
    {				/* start with first in new pop */
      i = 0;
      ip = P;
      ip1 = P + 1;		/* the first pair to (perhaps) cross and/or mutate */
    }
  else
    {				/* start with second in new pop */
      i = 1;
      ip = P + 1;
      ip1 = P + 2;
    }

  for (; i < maxi; i += 2, ip += 2, ip1 += 2)
    {				/* note we go two-by-two */
      mu1 = mu2 = newindiv = 0;	/* init counters for this individual */
      if (URand01 < XOverProb)
	{			/* lets cross this pair! */
	  ++XOverCnt;
	  xpt = random () % GenomeLen;	/* pick cross point at random */
	  for (g = 0; g < xpt; ++g)
	    {			/* if xpt==0,don't do it--we lose 1/GenomeLen crosses */
	      tchar = ip->Genome[g];
	      ip->Genome[g] = ip1->Genome[g];
	      ip1->Genome[g] = tchar;
	      newindiv = 2;
	    }
	  if (TestFlag)
	    fprintf (stderr, "\nXOver %d,%d at locus %d.", ip->Id, ip1->Id, xpt);
	}
      for (g = 0; g < GenomeLen; ++g)
	{			/* perhaps mutate the first new individual */
	  if (URand01 < MuRate)
	    {			/* a locus to mutation! */
	      ++mu1;
	      ++MuCnt;
	      if (ip->Genome[g] == '0')
		ip->Genome[g] = '1';
	      else
		ip->Genome[g] = '0';
	      if (TestFlag)
		fprintf (stderr, "\n   Mu %d at locus %d.", ip->Id, g);
	    }
	  if (mu1 > MuMax)	/* that's all we want in one individual */
	    break;
	}

      for (g = 0; g < GenomeLen; ++g)
	{			/* repeat for the other new individual */
	  if (URand01 < MuRate)
	    {
	      ++mu2;
	      ++MuCnt;
	      if (ip1->Genome[g] == '0')
		ip1->Genome[g] = '1';
	      else
		ip1->Genome[g] = '0';
	      if (TestFlag)
		fprintf (stderr, "\n   Mu %d at locus %d.", ip->Id, g);
	    }
	  if (mu2 > MuMax)
	    break;
	}

      if (mu1 > 0)
	{			/* increment some counters */
	  ++MuIndCnt;
	  if (newindiv < 2)
	    ++newindiv;
	}
      if (mu2 > 0)
	{
	  ++MuIndCnt;
	  if (newindiv < 2)
	    ++newindiv;
	}
      NewIndivs += newindiv;
    }

}				/* end ModifyPopulation */


/*************************************************
 *
 * Restart       Restart the system, perhaps with a new seed and genome length.
 *
 * Seed    if present set RandSeed to it and use it to initialize random number generator.
 *         if not, just reinitialize with same RandSeed.
 * L              if present, set GenomeLen to it, else use old value.
 *
 * Set counters (eg CurGen, NewIndivs, etc) all as if starting from scratch.
 *
 *************************************************/

void
Restart (char Seed[], char L[])
{
  int newl = atoi (L);
  int news = atoi (Seed);
  unsigned int i, maxix, maxi;
  double f;
  char buff[GenomeLEN + 1];

  if (*L != '\0')
    {
      if (1 < newl && newl < GenomeLEN)
	{
	  GenomeLen = newl;
	  fprintf (stderr, "\nNew GenomeLen %d...", GenomeLen);
	}
    }
  if (*Seed != '\0')
    {
      if (1 < news)
	{
	  RandSeed = news;
	  initstate (RandSeed, RandState, RandStateSZ);
	  fprintf (stderr, "\nNew RandSeed %d to initstate RandState...", RandSeed);
	}
    }
  else
    {
      initstate (RandSeed, RandState, RandStateSZ);
      fprintf (stderr, "\nUse same RandSeed %d to initstate RandState...", RandSeed);
    }

  /* set the scaling factor to get the encoded genome values into the range of the fitness function,
     in this case between 0 and Pi
   */
  scalefactor = M_PI / pow ((double) 2.0, (double) GenomeLen);

  NewIndivs = 0;
  NextId = 1;
  CurGen = 0;
  GenerateRandomPop (CurPop);
  EvaluatePop (CurPop);		/* not strictly needed, but nice if display the initial pop */

  MuMax = min (MuMax, GenomeLen);
  MuCnt = MuIndCnt = XOverCnt = 0;

  for (i = 0; i < SchemaMX; ++i)	/* indicate no prediction for 0th generation */
    SchemaCountPred[i] = -999;
  TotalSchemaPred = TotalSchemaPredRight = 0;

  fprintf (stderr, "\nL=%d, P=%d, Seed=%d.  Xwt %d, LoF %d, HiF %d.",
	   GenomeLen, PopSize, RandSeed, Xwt, LoF, HiF);
  fprintf (stderr, "\nTSProb %.3lf, MuRate=%.4lf, MuMax %d, XOverProb=%.3lf, Elitist=%d.\n",
	   TSProb, MuRate, MuMax, XOverProb, Elitist);

  if (GenomeLen > 16)		/* Longer than this takes forever to search brute force for max... */
    return;

  /* for GenomeLen <= 16, do brute force search to find the max fitness. */

  GenFoundMaxf = -1;
  Maxf = -1;
  maxi = pow ((double) 2.0, (double) GenomeLen);
  for (i = maxix = 0; i < maxi; ++i)
    {
      IntToBinaryChar (i, buff, GenomeLen);
      f = FitnessOfString (buff);
      if (f > Maxf)
	{
	  Maxf = f;
	  maxix = i;
	}
    }
  IntToBinaryChar (maxix, buff, GenomeLen);
  buff[GenomeLen] = '\0';
  Maxfx = BinaryCharToDoubleX (buff, GenomeLen);
  fprintf (stderr, "  Maxf at %s (int x %d, x %.8lf) => %.8lf.\n", buff, maxix, Maxfx, Maxf);
  if (Maxf == BestIndiv->Fitness)
    {
      GenFoundMaxf = CurGen;
      fprintf (stderr, "\n  Maxf found in initial population!\n");
    }
}				/* end Restart */


/**************************************************
 *
 * PrintPop   Print the population.
 *
 * Pop          Pointer to population to print (usually CurPop).
 *
 * Fmt          If 0, print whole population. Else, print most fit Fmt-PopSize individuals only.
 *
 * Print Id of individual, fitness, binary string representation, decimal representation
 * of that string, and in some cases a greater precision version of the fitness.
 *
 * If schema are being counted, print schemas to which each individual belongs.
 * Also call routine to count instances of the schemas and get the average sample fitnesses,
 * and print those for each schema.
 *
 * Store various summary statistics in arrays indexed by generation, so they can
 * be printed out later in a run (eg to use in another stat or plotting program).
 *
 * Note that the population is sorted by fitness, just to make it easier to
 * see what is going on in the population over time.
 *
 * NOTE WELL: This assumes the population has been evaluated and fitnesses assigned to them.
 *
 **************************************************/

void
PrintPop (struct PopStr Pop[], int Fmt)
{
  register int i, maxi, s;
  char tbuff[GenomeLEN + 64];
  double x;
  struct PopStr *p;

  ReSortPopulation (Pop, PopSize);

  if (Fmt == 0)
    maxi = PopSize;
  else
    maxi = Fmt - PopSize;
  maxi = min (maxi, PopSize);

  fprintf (stderr, "\nSorted Pop at Gen %d:", CurGen);

  for (i = 0, p = BestIndiv; i < maxi && i < PopSize; ++i, p = p->NextLowerFit)
    {
      strncpy (tbuff, p->Genome, GenomeLen);
      tbuff[GenomeLen] = '\0';
      x = BinaryCharToDoubleX (p->Genome, GenomeLen);
      fprintf (stderr, "\n%5d: %8.5lf '%s' (f(%.8lf)=%.8lf)",
	       p->Id, p->Fitness, tbuff, x, p->Fitness);

      for (s = 0; s < SchemaMX; ++s)
	{
	  if (Schema[s][0] != '\0')
	    {
	      if (IsMemberSchema (p, Schema[s]))
		fprintf (stderr, "   <= %s", Schema[s]);
	    }
	}

    }

  fprintf (stderr, "\nPop AveFitness %.4lf.", AveFitness[CurGen]);
  fprintf (stderr, "\n");

  for (s = 0; s < SchemaMX; ++s)
    {
      if (Schema[s][0] != '\0')
	EvaluateSchema (Schema[s], s, Pop, PopSize);
    }
  fprintf (stderr, "\n");

}				/* end PrintPop */


/************************************************
 *
 * GenerateRandomPop    Generate a population of with random binary strings as their 'genomes'.
 *
 * P is pointer to array of PopStr's in which to store the random genomes.
 *
 * Just generate and store 50% 1's and 50% 0's (more or less).
 * Assign Id's in sequence, incrementing NextId as we go.
 * Also store generation these were created (just for analysis).
 *
 ************************************************/

void
GenerateRandomPop (struct PopStr *P)
{
  register unsigned int i, j;
  register struct PopStr *ip;

  for (i = 0, ip = P; i < PopSize; ++i, ++ip, ++NextId)
    {
      ip->Id = NextId;
      ip->GenCreated = CurGen;
      for (j = 0; j < GenomeLen; ++j)
	if (URand01 <= 0.5)
	  ip->Genome[j] = '0';
	else
	  ip->Genome[j] = '1';
    }

  NewIndivs += i;		/* increment this global counter */

}				/* end GenerateRandomPop */


/*************************************************
 *
 * EvaluatePop    Evaluate and store fitness of the population members.
 *
 * Just call routine to return fitness of each genome.
 * Calculate and store population average fitness, and best fitness.
 *
 *************************************************/

void
EvaluatePop (struct PopStr *P)
{
  register unsigned int i;
  register struct PopStr *ip;
  register double avef;

  BestIndiv = P;
  BestIndiv->Fitness = 0;

  for (i = 0, ip = P, avef = 0; i < PopSize; ++i, ++ip)
    {
      ip->Fitness = FitnessOfString (ip->Genome);
      avef += ip->Fitness;
      if (ip->Fitness > BestIndiv->Fitness)
	BestIndiv = ip;
    }
  if (CurGen < GenerationMX)
    {
      AveFitness[CurGen] = avef / PopSize;
      BestFitness[CurGen] = BestIndiv->Fitness;
    }
}				/* end EvaluatePop */


/***************************************************
 *
 * FitnessOfString   return fitness associated with string.
 * sf1               a simple fitness function.
 *
 * Just use this as the single interface to the subroutine which calculates
 * the fitness function you are interested in now; that is, to change fitness
 * functions (other than by parameter setting), just change the call here.
 *
 * sf1 is the current fitness function here.  it just returns a sum of
 * a term linear in x, and some absolute values of sin terms with variable frequencies.
 * The binary strings are mapped into the domain 0...Pi-e (where e depends on the GenomeLen).
 * Thus 000...00 = 0, 000...01 = Pi/2**GenomeLen, and so on, up to 111...11 = Pi - Pi/2**GenomeLen.
 * sf2 then returns a function of that real value, as specified by
 * its form and parameters. The parameters to sf2 are:
 *    Xwt        weight on linear term
 *    HiF, LoF   use for low and high frequency terms, respectively,
 *               by setting to values > 0.
 *
 *******/

double
FitnessOfString (char BString[])
{

  return (sf1 (BString));

}				/* end FitnessOfString */


double
sf1 (char BString[])
{
  double v, db;

  db = BinaryCharToDoubleX (BString, GenomeLen);

  if (HiF != (double) 0.0)
    {
      v = (db * Xwt) + fabs (sin (LoF * db)) + fabs (sin (HiF * db));
    }
  else
    {
      v = (db * Xwt) + fabs (sin (LoF * db));
    }

  return (v);

}				/* end sf1 */


/***************************************************
 *
 * BinaryCharToDouble  convert ascii binary string to its integer equivalent, returned as a double.
 * BinaryCharToDoubleX convert ascii binary string to double in domain accespted by current fitness function.
 * IntToBinaryChar     convert binary value to ascii binary string.
 *
 * Buff is the ascii string to convert and return as a double.
 * L specifies the length of the string to convert.
 * B is the binary string to convert to ascii.
 *
 * BinaryCharToDouble maps an ascii binary representation into a double (real),
 * where 000...00 maps to 0, 000...01 to 1, ...etc., up to 111...11 to 2**L-1.
 * IntToBinaryChar does the inverse.
 *
 * BinaryChar2DoubleX just calls BinaryCharToDouble, and then scales the return
 * value so its in the domain of the current fitness function.
 *
 * Borrowed from the Genesis GA system, more or less.
 * Note: I am storing ascii binary strings as the characters '1' and '0' right
 * now, to make i/o easier.  It would be more elegant to store them as x1 and x0 values
 * in the char arrays, so that for example, the processing of BinaryCharToDouble could be
 *    r << 1;
 *    r += *Buff;
 *
 ***************************************************/

double
BinaryCharToDouble (register char *Buff, register int L)
{
  register int i, r;

  for (i = r = 0; i < L; ++i, ++Buff)
    {
      r <<= 1;
      if (*Buff == '1')
	++r;
    }

  return ((double) r);

}				/* end BinaryCharToDouble */

double
BinaryCharToDoubleX (char *Buff, int L)
{
  double x = BinaryCharToDouble (Buff, L);

  return (x * scalefactor);

}				/* end BinaryCharToDoubleX */

int
IntToBinaryChar (int B, char *Buff, int L)
{
  register int i;

  for (i = L - 1; i >= 0; --i)
    {
      if (B & 1)
	Buff[i] = '1';
      else
	Buff[i] = '0';
      B >>= 1;
    }

}				/* end IntToBinaryChar */


/******************** utilities of various kinds **********
 *
 * These are routines not necessary to implement a GA, but nice to have.
 * Included here are:
 * - rotuines to count schema instances, calculate average schema fitnesses, and so on
 * - routines to sort the population by fitness
 * - routines to read lines and help parsing them.
 * - routines to set and display various parameters
 * - routines to allow the user to see the ascii binary string representations
 *   of specified decimal values, and to see the fitness assigned by the current fitness function.
 *   (and to do the inverse, i.e., display the decimal version of an ascii string).
 *
 ********************************************************/

/****************************************************
 *
 * EvaluateSchema     Determine schema's average fitness over the current population
 *    S         Ascii form of schema (will pad to GenomeLen with *'s).
 *    SNm    Index for this schema in the arrays to store values for it.
 *    Pop    Population to evaluate over.
 *    N      Size of population.
 *
 * This also stores the average fitness and instance count in the summary arrays,
 * and it stores a prediction about whether the schema count should go up or down,
 * depending on whether the schema average fitenss is above or below the population average.
 *
 ***************************************************/

int
EvaluateSchema (char *S, unsigned int SNm, struct PopStr *Pop, unsigned int N)
{
  register int i;

  if (S[0] == '\0')
    {
      fprintf (stderr, "\nNo schema to evaluate.\n");
      return (0);
    }
  if (CurGen >= GenerationMX)
    {
      fprintf (stderr, "\nCurGen %d > GenerationMX %d, so can't store Schema evaluations.\n", CurGen, GenerationMX);
      return (0);
    }
  SchemaFitness[SNm] = SchemaCount[SNm][CurGen] = 0;	/* initialize */

  for (i = 0; i < N; ++i, ++Pop)
    {				/* do the counting */
      if (IsMemberSchema (Pop, S))
	{
	  SchemaFitness[SNm] += Pop->Fitness;
	  ++SchemaCount[SNm][CurGen];
	}
    }

  if (SchemaCount[SNm][CurGen] == 0)
    SchemaFitness[SNm] = 0;
  else
    SchemaFitness[SNm] /= SchemaCount[SNm][CurGen];
  fprintf (stderr, "\n%3d members of schema %s, SchemaFitness %lf.", SchemaCount[SNm][CurGen], S, SchemaFitness[SNm]);

  /* Now lets see how the prediction went: */

  if (SchemaCountPred[SNm] == -999 || CurGen == 0)
    {
      ;				/* no prediction */
    }
  else if (SchemaCountPred[SNm] > 0)
    {
      fprintf (stderr, "  Predicted Increase: %d -> %d  ", SchemaCount[SNm][CurGen - 1], SchemaCount[SNm][CurGen]);
      if (SchemaCount[SNm][CurGen - 1] < SchemaCount[SNm][CurGen])
	{
	  fprintf (stderr, "**Right!");
	  TotalSchemaPredRight += 1;
	}
      else
	fprintf (stderr, "--wrong.");
      TotalSchemaPred += 1;
    }
  else if (SchemaCountPred[SNm] < 0)
    {
      fprintf (stderr, "  Predicted Decrease: %d -> %d  ", SchemaCount[SNm][CurGen - 1], SchemaCount[SNm][CurGen]);
      if (SchemaCount[SNm][CurGen - 1] > SchemaCount[SNm][CurGen])
	{
	  fprintf (stderr, "**Right!");
	  TotalSchemaPredRight += 1;
	}
      else
	fprintf (stderr, "--wrong.");
      TotalSchemaPred += 1;
    }
  else
    {
      fprintf (stderr, "  Predicted nochange: %d -> %d  ", SchemaCount[SNm][CurGen - 1], SchemaCount[SNm][CurGen]);
      if (SchemaCount[SNm][CurGen - 1] == SchemaCount[SNm][CurGen])
	{
	  fprintf (stderr, "**Right!");
	  TotalSchemaPredRight += 1;
	}
      else
	fprintf (stderr, "--wrong.");
      TotalSchemaPred += 1;
    }

  /* now set up the next prediction... */

  if (SchemaCount[SNm][CurGen] == 0)
    SchemaCountPred[SNm] = -999;
  else if (SchemaFitness[SNm] > AveFitness[CurGen])
    SchemaCountPred[SNm] = 1;
  else if (SchemaFitness[SNm] < AveFitness[CurGen])
    SchemaCountPred[SNm] = -1;
  else
    SchemaCountPred[SNm] = 0;

}				/* end EvaluateSchema */

/********************************************
 *
 * IsMemberSchema    return 1 if inidividual P is a member of schema S, else return 0.
 *
 ********************************************/

int
IsMemberSchema (struct PopStr *P, char *S)
{
  register int g;

  for (g = 0; g < GenomeLen; ++g)
    if (S[g] != '*' && S[g] != P->Genome[g])
      return (0);

  return (1);

}				/* end IsMemberSchema */

/****************************************************
 *
 * EvaluateSchemaOverFunction  Determine schema's fitness over the fitness function
 *                              (ie independent of the current population estimate).
 *
 *   SchemaPar   ascii form of schema to evaluate (will pad with *'s to GenomeLen).
 *   SamplePar   ascii form of number of uniform random samples to take
 *                to get an estimate of the schema's average fitness.
 *
 * This also computes the standard deviation for the fitness, and for the domain (x).
 *
 ***************************************************/


int
EvaluateSchemaOverFunction (char SchemaPar[], char SamplesPar[])
{
  register int s, samples, g;
  double d, sa, sxa;
  char fulls[GenomeLEN + 1], tests[GenomeLEN + 1];
  double vsd, xsd;
  double samplex[10000], samplev[10000];

  /* get schema from par1 (pad out if needed).
     get sample size from par2
     the make samples of the schema by filling *'s with 0/1 at random
     and then accumulated the values from those.
   */

  if (*SamplesPar == '\0')
    samples = 100;
  else
    samples = atoi (SamplesPar);
  if (samples < 1 || samples > 10000)
    {
      fprintf (stderr, "\nSamples '%s' out of 1...1000. Using 100.", SamplesPar);
      samples = 100;
    }
  strncpy (fulls, SchemaPar, GenomeLen);
  for (g = strlen (fulls); g < GenomeLen; ++g)	/* pad out... */
    fulls[g] = '*';
  fulls[g] = '\0';

  for (g = 0; g < GenomeLen; ++g)
    if (fulls[g] != '0' && fulls[g] != '1' && fulls[g] != '*')
      {
	fprintf (stderr, "\nIllegal char '%c' in fulls '%s',\n", fulls[g], fulls);
	return;
      }
  fprintf (stderr, "Get %d samples fitness values from schema %s =>", samples, fulls);

  for (s = 0, sa = sxa = 0; s < samples; ++s)
    {
      for (g = 0; g < GenomeLen; ++g)
	{
	  if (fulls[g] == '*')
	    {
	      if (URand01 < 0.5)
		tests[g] = '0';
	      else
		tests[g] = '1';
	    }
	  else
	    {
	      tests[g] = fulls[g];
	    }
	}
      d = FitnessOfString (tests);
      sa += d;
      samplev[s] = d;

      d = BinaryCharToDoubleX (tests, GenomeLen);
      sxa += d;
      samplex[s] = d;
    }

  sa /= samples;
  sxa /= samples;

  for (s = 0, xsd = vsd = 0; s < samples; ++s)
    {				/* sum squared differences */
      d = sa - samplev[s];
      vsd += d * d;
      d = sxa - samplex[s];
      xsd += d * d;
    }

  if (samples > 1)
    {				/* divide by n-1, and take sqrt to get std dev */
      vsd /= samples - 1;
      vsd = sqrt (vsd);
      xsd /= samples - 1;
      xsd = sqrt (xsd);
    }
  else
    vsd = xsd = -1;

  fprintf (stderr, "\n  => schema average is %.8lf (sd=%lf), over x average %lf (sd=%lf).\n", sa, vsd, sxa, xsd);

}				/* end EvaluateSchemaOverFunction */


/***************************************
 *
 * ReSortPopulation
 * This just uses InsertSortIndiv() to re-insert sort individuals, since the list is typically almost
 * sorted anyway (i.e., most individuals don't change relative fitness if we turnover
 * only a small fraction of the population each step).
 * For efficiency, start with the high-fitness end, since InsertSortIndiv() inserts at low end.
 * NOTE:
 * On loading individuals (either vi LoadPop or GenRandPop), HighFitIndiv and LowFitIndiv
 * list heads are NULLed, and then InsertSortIndiv() is call to set up the list.
 *
 * InsertSortIndiv
 * The Individual nodes are linked to form a two-way linked list sorted on fitness.
 * HighFitIndiv points to the highest, and NextLowerFit is link to decreasing fitness individuals.
 * LowFitIndiv points to lowest fit, and NextHigherFit is link to increasing fitness.
 *
 *****************************************/

int
ReSortPopulation (struct PopStr Pop[], unsigned int Size)
{
  unsigned int i;

  BestIndiv = WorstIndiv = NULL;

  for (i = 0; i < Size; ++i)
    {
      InsertSortIndiv (&Pop[i]);
    }

}				/* end ReSortPopulation */


int
InsertSortIndiv (struct PopStr *IPtr)
{
  double f;
  struct PopStr *prev;

  f = IPtr->Fitness;		/* this is fitness of one to add */

  if (WorstIndiv == NULL)
    {				/* List is empty - so make new one first and only */
      WorstIndiv = BestIndiv = IPtr;
      IPtr->NextLowerFit = IPtr->NextHigherFit = NULL;
    }
  else if (WorstIndiv->Fitness >= f)
    {				/* new one goes first (it is new low) */
      IPtr->NextHigherFit = WorstIndiv;
      IPtr->NextLowerFit = NULL;
      WorstIndiv->NextLowerFit = IPtr;
      WorstIndiv = IPtr;
    }
  else
    {
      /* Move prev until it points to node that should be before (lower than) new one */
      prev = WorstIndiv;
      while (prev->NextHigherFit != NULL && f > prev->NextHigherFit->Fitness)
	prev = prev->NextHigherFit;

      /* link new node to node after it (if there is one) -- both ways */

      IPtr->NextHigherFit = prev->NextHigherFit;
      if (prev->NextHigherFit == NULL)
	BestIndiv = IPtr;	/* prev was last, so new one is now */
      else
	prev->NextHigherFit->NextLowerFit = IPtr;

      /* link new to prev -- both ways */

      IPtr->NextLowerFit = prev;
      prev->NextHigherFit = IPtr;
    }

}				/* InsertSortIndiv */


/*********************************************************
 *
 * readline  my own gets()
 * I like fgets because it has an explicit max length,
 * but I don't like it returning a newline char!
 *
 * striptoken  my own stptok()
 * I don't like strtok() because I can't easily see what is left in the string.
 * note this returns pointer *after all* the break chars after the token.
 *
 **********************************************************/

char *
readline (char *S, unsigned int L, FILE * F)
{
  char *cp;

  fgets (S, L, F);

  for (cp = S; *cp != '\0'; ++cp)
    if (*cp == '\n')
      {
	*cp = '\0';
	break;
      }
  for (cp = S; *cp == ' ' && *cp != '\0'; ++cp);	/* span blanks */

  return (cp);

}				/* end readline */

char *
striptoken (char *S, char *Token, unsigned int L, char *Breaks)
{
  unsigned int n;

  for (; *S == ' '; ++S);	/* skip leading blanks */

  for (n = 0, --L; n < L && !tokenend (S, Breaks); ++S, ++n, ++Token)
    *Token = *S;
  *Token = '\0';

  for (; *S != '\0' && tokenend (S, Breaks); ++S);

  return (S);

}				/* end striptoken */

int
tokenend (char *S, char *Breaks)
{

  if (*S == '\0')
    return (1);

  for (; *Breaks != '\0'; ++Breaks)
    if (*S == *Breaks)
      return (1);

  return (0);

}				/* end tokenend */

/**************************************************************
 *
 * SetPar     Set a specified parameter Par to a value, Value.
 * SetUI                Set an unsigned int parameter.
 * SetD         Set a double parameter.*
 *
 *
 **************************************************************/

int
SetPar (char *Par, char *Value)
{
  unsigned int ui;
  double d;

  if (strncmp (Par, "dpi", 3) == 0)
    {
      DisplayPopInt = atoi (Value);
      if (DisplayPopInt < 1)
	DisplayPopInt = 0;
    }
  else if (strncmp (Par, "dpf", 3) == 0)
    {
      DisplayPopFmt = atoi (Value);
      if (DisplayPopFmt < 1)
	DisplayPopFmt = 0;
    }
  else if (strncmp (Par, "sch", 3) == 0)
    SetSchema ();			/* was: SetSchema (Value)  -joke */
  else if (strncmp (Par, "p", 1) == 0)
    SetUI (&PopSize, "PopSize", Value, 1, PopSZ);
  else if (strncmp (Par, "l", 1) == 0)
    SetUI (&GenomeLen, "GenomeLen", Value, 1, 31);
  else if (strncmp (Par, "rs", 2) == 0)
    SetUI (&RandSeed, "RandSeed", Value, 0, UINT_MAX);
  else if (strncmp (Par, "hif", 3) == 0)
    SetUI (&HiF, "HiF", Value, 0, 1024);
  else if (strncmp (Par, "lof", 3) == 0)
    SetUI (&LoF, "LoF", Value, 0, 1024);
  else if (strncmp (Par, "xwt", 3) == 0)
    SetUI (&Xwt, "Xwt", Value, 0, 1024);
  else if (strncmp (Par, "test", 4) == 0)
    SetUI (&TestFlag, "TestFlag", Value, 0, 10);

  else if (strncmp (Par, "tsp", 3) == 0)
    SetD (&TSProb, "TSProb", Value, 0.0, 1.0);
  else if (strncmp (Par, "mr", 2) == 0)
    SetD (&MuRate, "MuRate", Value, 0.0, 1.0);
  else if (strncmp (Par, "mm", 2) == 0)
    SetUI (&MuMax, "MuMax", Value, 0, GenomeLen);
  else if (strncmp (Par, "xp", 2) == 0)
    SetD (&XOverProb, "XOverProb", Value, 0.0, 1.0);
  else if (strncmp (Par, "e", 1) == 0)
    SetUI (&Elitist, "Elitist", Value, 0, PopSize);

  else
    {
      fprintf (stderr, "\nCan't set '%s' yet (Value '%s').\n", Par, Value);
    }

}				/* end SetPar */

int
SetUI (unsigned int *Var, char *VarName, char *Val, unsigned int LB, unsigned int UB)
{
  unsigned int ui;

  ui = atoi (Val);
  if (ui < LB || ui > UB)
    fprintf (stderr, "\nIllegal %s '%s' (LB %d, UB %d: ui=%d).\n", VarName, Val, LB, UB, ui);
  else
    *Var = ui;

}				/* end SetUI */

int
SetD (double *Var, char *VarName, char *Val, double LB, double UB)
{
  double d;

  d = atof (Val);
  if (d < LB || d > UB)
    fprintf (stderr, "\nIllegal %s '%s' (LB %lf, UB %lf: d=%lf).\n", VarName, Val, LB, UB, d);
  else
    *Var = d;

}				/* end SetD */


/************************************************************************
 SetSchema

 Loop over array of schema.
 It user enters q, all done.
 Otherwise print what is there, and ask for a new schema.

 ******/

int
SetSchema ()
{
  int s, i;
  char tb[2 * GenomeLEN];

  for (s = 0; s < SchemaMX; ++s)
    {
      if (Schema[s][0] == '\0')
	{
	  fprintf (stderr, "\nSchema Template: ");
	  for (i = 0; i < GenomeLen; ++i)
	    {
	      if (i % 5 == 4)
		fprintf (stderr, "+");
	      else
		fprintf (stderr, ".");
	    }
	}
      else
	fprintf (stderr, "\nSchema      %2d:  %s.", s, Schema[s]);

      fprintf (stderr, "\nNew Schema?      ");

      readline (tb, sizeof (tb), stdin);

      if (*tb == 'e' || *tb == 'q')
	break;
      else if (*tb == '\0')
	continue;

      if (strlen (tb) > GenomeLen)
	{
	  fprintf (stderr, "\nSchema too long! Truncated at %d.", GenomeLen);
	  tb[GenomeLen] = '\0';
	}
      for (i = strlen (tb); i < GenomeLen; ++i)		/* pad out... */
	tb[i] = '*';
      tb[i] = '\0';

      for (i = 0; i < GenomeLen; ++i)
	{
	  if (tb[i] != '1' && tb[i] != '0' && tb[i] != '*')
	    {
	      fprintf (stderr, "\nIllegal char '%c' at pos %d. Schema not set.\n", tb[i], i);
	      return;
	    }
	}
      strcpy (Schema[s], tb);
      fprintf (stderr, "Schema now:      %s\n", Schema[s]);
    }

}				/* end SetSchema */


/****************************************************
 *
 * Display              Display papameters settings, or summary values, schema counts, etc.
 *
 ****************************************************/

int
Display (char *P1)
{
  register int s, g;

  if (strncmp (P1, "sch", 3) == 0)
    {
      for (s = 0; s < SchemaMX; ++s)
	{
	  if (Schema[s][0] != '\0')
	    EvaluateSchema (Schema[s], s, CurPop, PopSize);
	}
    }
  else if (strncmp (P1, "vars", 4) == 0)
    {
      fprintf (stderr, "\nCurGen %d.  PopSize %d.  AveFitness %lf, BestFitness %lf.",
	       CurGen, PopSize, AveFitness[CurGen], BestFitness[CurGen]);
      fprintf (stderr, "\n   TSProb %.3lf, MuRate %.3lf (MuMax %d), XOverProb %.3lf, Elitist %d.",
	       TSProb, MuRate, MuMax, XOverProb, Elitist);
      fprintf (stderr, "\n   NewIndivs %d; MuIndCnt %d (MuCnt %d); XOverCnt %d.",
	       NewIndivs, MuIndCnt, MuCnt, XOverCnt);
      fprintf (stderr, "\n   Xwt is %d, LoF is %d, HiF is %d.", Xwt, LoF, HiF);
    }
  else if (strncmp (P1, "sum", 3) == 0)
    {
      fprintf (stderr, "\nAveFitness:=[%d,%lf", 0, AveFitness[0]);
      for (g = 1; g <= CurGen; ++g)
	if (g % 12 == 11)
	  fprintf (stderr, "\n ,%d,%lf", g, AveFitness[g]);
	else
	  fprintf (stderr, " ,%d,%lf", g, AveFitness[g]);
      fprintf (stderr, "]\n");

      fprintf (stderr, "\nBestFitness:=[%d,%lf", 0, BestFitness[0]);
      for (g = 1; g <= CurGen; ++g)
	if (g % 12 == 11)
	  fprintf (stderr, "\n ,%d,%lf", g, BestFitness[g]);
	else
	  fprintf (stderr, " ,%d,%lf", g, BestFitness[g]);
      fprintf (stderr, "]\n");

      fprintf (stderr, "\nSummary of SchemaCounts: ");
      for (s = 0; Schema[s][0] != '\0'; ++s)
	{
	  fprintf (stderr, "\n  Schema %s, s%d:=[%d,%d", Schema[s], s, 0, SchemaCount[s][0]);
	  for (g = 1; g <= CurGen; ++g)
	    if (g % 12 == 11)
	      fprintf (stderr, "\n ,%d,%d", g, SchemaCount[s][g]);
	    else
	      fprintf (stderr, " ,%d,%d", g, SchemaCount[s][g]);
	  fprintf (stderr, "]");
	}
      fprintf (stderr, "\n");
      fprintf (stderr, "\n TotalSchemaPredRight %.0lf / %.0lf TotalSchemaPred = %.3lf.",
	       TotalSchemaPredRight, TotalSchemaPred, TotalSchemaPredRight / TotalSchemaPred);
      if (GenFoundMaxf >= 0)
	fprintf (stderr, "\n Maxf (%.8lf) found at generation %d.", Maxf, GenFoundMaxf);
      else
	fprintf (stderr, "\n Maxf (%.8lf) NOT found.  Best found is %.8lf.", Maxf, BestIndiv->Fitness);
      fprintf (stderr, "\n");
    }
  else
    {
      fprintf (stderr, "\nCan't display '%s' yet.", P1);
    }
  fprintf (stderr, "\n");

}				/* end Display */


/***************************************************************
 *
 * DisplayB2D  Display binary (ascii) string converted to decimal, and its fitness.
 * DisplayD2B  Display decimal value converted to an ascii string of GenomeLen, and its fitness.
 *
 * This encode/decode decimal/ascii binary values, in the domain accepted
 * by the fitness function, and display the converted value (and its fitness).
 * For B2D, the string is padded on the right with 0's to GenomeLen.
 *
 * NOTE: These may have to be changed if the fitness function is changed.
 *
 ****************************************************************/

int
DisplayD2B (char Par[])
{
  double x, sx, fx;
  int ix, i;
  char tb[GenomeLEN + 1];

  x = atof (Par);
  if (x < 0 || x > M_PI)
    {
      fprintf (stderr, "\nOut of range 0...%lf? Par = '%s', x %lf.\n", M_PI, Par, x);
      return;
    }
  sx = x / scalefactor;
  ix = sx;
  IntToBinaryChar (ix, tb, GenomeLen);
  tb[GenomeLen] = '\0';

  fx = FitnessOfString (tb);

  fprintf (stderr, "x %.12lf => scaled int %d;  f(x) => %.12lf.\n  Binary string: %s\n                 ", x, ix, fx, tb);

  for (i = 0; i < GenomeLen; ++i)
    {
      if (i % 5 == 4)
	fprintf (stderr, "+");
      else
	fprintf (stderr, ".");
    }
  fprintf (stderr, "\n");

}				/* end DisplayD2B */

int
DisplayB2D (char Par[])
{
  double x, sx, fx;
  int i;

  if (strlen (Par) > GenomeLen)
    {
      fprintf (stderr, "\nToo long (len %d). GenomeLen is %d.\n", strlen (Par), GenomeLen);
      return;
    }
  for (i = strlen (Par); i < GenomeLen; ++i)
    Par[i] = '0';
  Par[i] = '\0';

  x = BinaryCharToDouble (Par, GenomeLen);
  sx = x * scalefactor;
  fx = FitnessOfString (Par);

  fprintf (stderr, " int x %.0lf => scaled x %.12lf;  f(x) = %.12lf\n  Binary string: %s\n                 ", x, sx, fx, Par);
  for (i = 0; i < GenomeLen; ++i)
    {
      if (i % 5 == 4)
	fprintf (stderr, "+");
      else
	fprintf (stderr, ".");
    }
  fprintf (stderr, "\n");

}				/* end DisplayB2D */


int
Usage ()
{					/****************************************************************************/

  fprintf (stderr, "\nUsage:");
  fprintf (stderr, "\n   sa2  [<par><value>]");
  fprintf (stderr, "\nwhere all the pars are optional.");
  fprintf (stderr, "\nNote that new values for GenomeLen, PopSize and RandSeed will");
  fprintf (stderr, "\nonly take effect at startup (or after a restart).");
  fprintf (stderr, "\nThe pars are:");

  fprintf (stderr, "\nl	 GenomeLen (%d).", GenomeLen);
  fprintf (stderr, "\np    PopSize (%d).", PopSize);
  fprintf (stderr, "\nrs   RandSeed (%u).", RandSeed);
  fprintf (stderr, "\ndpi  DisplayPopInt (%d).", DisplayPopInt);
  fprintf (stderr, "\ndpf  DisplayPopFmt (%d).", DisplayPopFmt);
  fprintf (stderr, "\nlof  LoF component of fitness function (%.2lf).", LoF);
  fprintf (stderr, "\nhif  HiF component of fitness function (%.2lf).", HiF);
  fprintf (stderr, "\nxwt  weight on x component of fitness function (%.2lf).", Xwt);
  fprintf (stderr, "\ntest TestFlag off=0 or on=1 (%d).", TestFlag);

  fprintf (stderr, "\ntsp  TSProb (%.3lf).", TSProb);
  fprintf (stderr, "\nmr   MuRate (%.3lf).", MuRate);
  fprintf (stderr, "\nmm   MuMax (%d).", MuMax);
  fprintf (stderr, "\nxp   XOverProb (%.3lf).", XOverProb);
  fprintf (stderr, "\ne    Elitist (%d).", Elitist);

  fprintf (stderr, "\n\nThe run time commands:");

  fprintf (stderr, "\nr <n>        run for <n> generations");
  fprintf (stderr, "\nexit         Exit program.");
  fprintf (stderr, "\nres <r>,<l>  Restart (optinal RandSeed, GenomeLen).");
  fprintf (stderr, "\nset <par>=<value>  set parameter to new value");
  fprintf (stderr, "\ndi <what>,<fmt>  display pop, sch, var, in specified format.");
  fprintf (stderr, "\nep           evaluate the CurPop. ");
  fprintf (stderr, "\nes <sch> <s> evaluate schema sch, s samples of function. ");
  fprintf (stderr, "\nd2b <d>      convert decimal number <d> to binary (and display).");
  fprintf (stderr, "\nb2d <b>      convert binary <b> to decimal (and display).");
  fprintf (stderr, "\n");

}				/* end Usage */
