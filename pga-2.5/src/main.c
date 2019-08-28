/************************************************************************
 *                                                                      *
 *               PGA - Parallel Genetic Algorithm Testbed               *
 *                                                                      *
 *           "A Parallel Implementation of a genetic algorithm"         *
 *                                                                      *
 *                By Peter Ross (peter@ed.ac.uk),                       *
 *                   Geoffrey H. Ballinger (geoff@ed.ac.uk)             *
 *                                                                      *
 ************************************************************************/

/* 
   Copyright (C) 1993, Peter Ross and Geoffrey H. Ballinger.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. 
*/

#include <stdio.h>            /* Include the IO and maths header files. */
#include <math.h>
#include <curses.h>

#include "version.h"          /* title string and version number */
#include "pga.h"              /* defines genodata and chromosome structs */
#include "screen.h"           /* screen layout configuration */

#ifdef hpux
#define random() lrand48()
#define srandom(seed) srand48((long) seed)
#endif hpux



void handle();                /* Prototypes; defined in this file. */
void report();
void free_pops();
int compchromo();
CHROMOSOME *init_pops();
void insert();
void dump_chromos();
double drandom();

/* Defined in eval.c */

extern double eval_max();
extern double eval_dj1();
extern double eval_dj2();
extern double eval_dj3();
extern double eval_dj4();
extern double eval_dj5();
extern double eval_bf6();
extern double eval_knap();
extern int read_weights_file();
extern double eval_rr();
extern void read_rrdata_file();
extern void long_decode();
extern void double_decode();

/* Defined in select.c */

extern CHROMOSOME rank_select();
extern CHROMOSOME fitprop_select();
extern CHROMOSOME tm_select();
extern CHROMOSOME tn_select();

/* Defined in reprod.c */

extern void one_reproduction();
extern void gen_reproduction();
extern void ss_gen_reproduction();
extern void ss_one_reproduction();

/* Derfined in cross.c */

extern CHROMOPAIR one_pt_cross();
extern CHROMOPAIR two_pt_cross();
extern CHROMOPAIR uniform_cross();

/* Defined in mutate.c */

extern CHROMOSOME mutate();
extern double adaptive();
extern double nonadaptive();

/* Defined in help.c */

extern void pga_help();



/* Standard C lib stuff */

extern FILE *fopen();
extern int fscanf();
extern char *sprintf();
extern char *malloc();
extern char *calloc();
extern char *strdup();
extern long random();

/* Globals used in various files, but declared here  */

int numpop = 5,            /* Number of populations */
    numchromo = 50,        /* Number of chromos in a population */
    geno_size = 32,        /* Length of a chromosome */
    gens_stage = 100,      /* How many times to cycle before asking user */
    repinterval = 10,      /* Interval between screen/file reports */
    isadaptive = FALSE,    /* Using adaptive mutation? */
    last_line = 24,        /* Size of screen; actually found at run time */
    rr_blocks,             /* RR number of low-level blocks (no default) */
    rr_blocksize = 8,      /* RR default block size */
    rr_gap = 7,            /* RR default gap */
    rr_mstar = 4,          /* RR default no of rewardable bits, low-level */
    migint = 10,           /* Interval between migrations */
    tournament_size = 10,  /* Default size in tournament selection */
    chromo_id = 1;         /* Chromosome IDs start from this value */
long *sticks;              /* Used in knapsack prob: array of sizes */
long target;               /* Used in knapsack prob: target */
FILE *output = (FILE *)NULL;       /* Output file if any */
FILE *chromofile = (FILE *)NULL;   /* Chromo file, if any output file */
int chromoutput = FALSE;           /* Doing any output? */
int twins = FALSE;         /* Crossover produces pairs, or just one child? */
char outfilename[256], chromofilename[256];
char *geno_size_string;    /* String naming size of a chromo (hack) */
char *rr_blockarray;       /* Used in RR for higher-level block scoring */
double mute_rate = 0.02,   /* Default mutation rate */
       cross_rate = 0.6,   /* Default crossover rate if applicable */
       bias = 1.5,         /* Default bias for rank selection */
       rr_ustar = 1.0,     /* RR default reward for first block */
       rr_u = 0.3,         /* RR default reward for later blocks */
       rr_v = 0.02;        /* RR default bonus/penalty for low-level bit */
char *eval_name,           /* String giving name of problem */
     *maxfitness,          /* String giving max fitness (if known) */
     *cross_name,          /* String naming crossover type */
     *select_name,         /* String naming selection type */
     *repro_name;          /* String naming reproduction type */
double *total_abs_fitness; /* Used in roulette selection: size of wheel */
int evals;                 /* How often eval has been called */
int ss_xmax,ss_ymax,ss_n;  /* Size of grid and length of walk in ssN */



/* Default bindings for function pointers */

CHROMOSOME (*select)() = rank_select;
CHROMOPAIR (*crossover)() = two_pt_cross;
void (*reproduction)() = one_reproduction;
void (*decode)() = long_decode;
double (*mrate)() = nonadaptive;
double (*evaluate)() = eval_max;



/************************************************************************/
/* The main program.                                                    */
/************************************************************************/

main(argc,argv)
int argc;
char *argv[];
{
  int command,chromo,pop,gen,mignext,action,gen_total,fileoutput;
  double average;
  CHROMOSOME *pops,migchromo;


  srandom((unsigned)time(0));    /* Initialise the random number generator. */

  mignext=0;
  geno_size_string = (char *)malloc(20);
  sprintf(geno_size_string,"%d",geno_size);
  maxfitness = geno_size_string;
  eval_name = "max";
  cross_name = "two-point";
  select_name = "rank";
  repro_name = "one";  

  for (command=1; command<argc; command+=1) {
    handle(argv[command]);                    /* Handle any arguaments. */
  }

  if(geno_size > CRUCIAL_GENO_SIZE)
    decode = double_decode;
  if(evaluate == eval_knap) {
    sticks = (int *)calloc(geno_size,sizeof(long));
    if(!read_weights_file()) {
      fprintf(stderr,"No weights file accessible. It should hold\n");
      fprintf(stderr,"the target value (an integer) and up to\n");
      fprintf(stderr,"%d further integers. The aim is to find a\n", geno_size);
      fprintf(stderr,"subset with sum as close to the target as possible.\n");
      exit(1);
    }
  } else if(evaluate == eval_rr) {
      int n;
      double max;
      read_rrdata_file();
      rr_blockarray = malloc(rr_blocks);
      maxfitness = (char *)malloc(20);
      n = rr_blocks;
      max = 0.0;
      while(n>0) {
        max += rr_ustar + (n-1)*rr_u;
        n /= 2;
      }
      sprintf(maxfitness,"%.6f", max);
  }

  if((reproduction == ss_gen_reproduction)
     || (reproduction == ss_one_reproduction)) {
        ss_xmax = (int) sqrt((double)numchromo);
        ss_ymax = numchromo/ss_xmax;
        numchromo = ss_xmax*ss_ymax;
        migint = 0;
  }


  stdscr = initscr();
  last_line = tgetnum("li");
  if(last_line < 0) last_line = 24;
  

  fileoutput = (output == (FILE *)NULL)? FALSE : TRUE;

  mvprintw(POP_ROW,POP_COL,      "          Populations: %-6d",numpop);
  if((reproduction == ss_gen_reproduction)
     || (reproduction == ss_one_reproduction))
    mvprintw(CPP_ROW,CPP_COL,      "  Chromosomes per pop: %-4d (%dx%d)",
                                   numchromo,ss_xmax,ss_ymax);
  else
    mvprintw(CPP_ROW,CPP_COL,      "  Chromosomes per pop: %-4d",numchromo);
  mvprintw(EVLIM_ROW,EVLIM_COL,  "Generations per stage: %-6d",gens_stage);
  mvprintw(REPINT_ROW,REPINT_COL,"   Reporting interval: %-6d",repinterval);
  mvprintw(MIG_ROW,MIG_COL,      "   Migration interval: %-6d",migint);
  mvprintw(CTYPE_ROW,CTYPE_COL,  "   Crossover type: %s, %s", cross_name,
                                          (twins)? "twins" : "one child");
  if(!strcmp(repro_name,"one"))
    mvprintw(CROSS_ROW,CROSS_COL,"   Crossover rate: n/a");
  else
    mvprintw(CROSS_ROW,CROSS_COL,"   Crossover rate: %3.2f",cross_rate);
  if(!strcmp(select_name,"rank"))
    mvprintw(BIAS_ROW,BIAS_COL,  "   Selection bias: %3.2f",bias);
  else
    mvprintw(BIAS_ROW,BIAS_COL,  "   Selection bias: n/a");
  if(isadaptive)
    mvprintw(MUT_ROW,MUT_COL,    "    Mutation rate: adaptive %.4f",mute_rate);
  else
    mvprintw(MUT_ROW,MUT_COL,    "    Mutation rate: %.6f",mute_rate);
  mvprintw(CRL_ROW,CRL_COL,      "Chromosome length: %d",geno_size);
  if(fileoutput)
    mvprintw(FNAME_ROW,FNAME_COL,"    Log file name: %s",outfilename);
  mvprintw(EVAL_ROW,EVAL_COL,    "Eval function: %s",eval_name);
  mvprintw(SELECT_ROW,SELECT_COL,"    Selection: %s", select_name);
  mvprintw(REPRO_ROW,REPRO_COL,  " Reproduction: %s",repro_name);
  mvprintw(POPTABLE_ROW,POPTABLE_COL,
               "Pop.......Average..........Best.(max = %s)",maxfitness);

  total_abs_fitness = (double *)malloc(numpop*sizeof(double));
  pops = init_pops(numpop,numchromo);
  gen_total = 1;
  evals=0;
  action = RESTART;

  if(fileoutput) {
    fprintf(output,   "\n\n===================================\n");
    fprintf(output,   "               eval = %s\n",eval_name);
    fprintf(output,   "        max-fitness = %s\n",maxfitness);
    fprintf(output,   "       reproduction = %s\n",repro_name);
    fprintf(output,   "             select = %s\n",select_name);
    if(!strcmp(select_name,"fitprop"))
      fprintf(output, "     selection-bias = n/a\n");
    else
      fprintf(output, "     selection-bias = %.6f\n",bias);
    fprintf(output,   "  chromosome-length = %d\n",geno_size);
    fprintf(output,   "               pops = %d\n",numpop);
    if((reproduction == ss_gen_reproduction)
       || (reproduction == ss_one_reproduction))
      fprintf(output,   "chromosomes-per-pop = %d (%dx%d)\n",
                        numchromo,ss_xmax,ss_ymax);
    else
      fprintf(output,   "chromosomes-per-pop = %d\n",numchromo);
    fprintf(output,   " migration-interval = %d\n",migint);
    fprintf(output,   "     crossover-type = %s\n",cross_name);
    if(!strcmp(repro_name,"one"))
      fprintf(output, "     crossover-rate = n/a\n");
    else
      fprintf(output, "     crossover-rate = %.6f\n",cross_rate);
    if(isadaptive)
      fprintf(output, "      mutation-rate = adaptive\n");
    else
      fprintf(output, "      mutation-rate = %.6f\n", mute_rate);
    fprintf(output,   " reporting-interval = %d\n", repinterval);
    if(twins)
      fprintf(output, "              twins = yes\n");
    else
      fprintf(output, "              twins = no\n");
    fprintf(output,   "\n");
  }

  mvprintw(GEN_ROW,GEN_COL,"Generation: ");
  clrtoeol();
  mvprintw(EVCOUNT_ROW,EVCOUNT_COL,"Evaluations so far: ");
  clrtoeol();
  refresh();
  report(0,fileoutput,pops);
  cbreak();
  noecho();
  while(action != QUIT) {
      if(fileoutput)
        mvprintw(0,1,"(A)gain, (C)ontinue, (S)ave+continue, (Q)uit: ");
      else
        mvprintw(0,1,"(A)gain, (C)ontinue, (Q)uit: ");

      clrtoeol();
      refresh();
      do {
              switch(getch()) {
                    case 'a': case 'A':
                            action = RESTART;
                            free_pops(pops,numpop*numchromo);
                            gen_total = 1;
                            pops = init_pops(numpop,numchromo);
                            evals = 0;
                            move(GEN_ROW,GEN_COL+12);
                            clrtoeol();
                            move(EVCOUNT_ROW,EVCOUNT_COL+20);
                            clrtoeol();
                            refresh();
                            report(0,fileoutput,pops);
                            if(fileoutput)
                              fprintf(output,
                                      "\n\n------ re-starting ------\n\n");
                            break;
                    case 'c': case 'C':
                            action = CONTINUE;
                            break;
                    case 'q': case 'Q':
                            action = QUIT;
                            break;
		    case 's': case 'S':
                            if(fileoutput) {
                               action = CONTINUE;
                               dump_chromos(pops,gen_total-1);
                               break;
			    }
                    default:
                            action = ASK_AGAIN;
                            move(0,30);
                            clrtoeol();
                            refresh();
                            break;
              }
      } while(action == ASK_AGAIN);
      if(action == CONTINUE) {
        for(gen=1; gen <= gens_stage; gen++, gen_total++) {
          for(pop=0; pop<numpop; pop+=1) {            /* For each generation */
            reproduction(pops,pop*numchromo,numchromo);        /* Reproduce. */
          }
          if (numpop>1 && migint > 0 && gen%migint==0) {
            migchromo=select(pops,mignext*numchromo,numchromo);/* Migrate if */
            for(pop=0; pop<numpop; pop+=1) {                   /* necessary. */
              if(pop != mignext)               /* Don't insert to its source */
                insert(pops,migchromo,pop*numchromo,numchromo);
            }
            mignext=(mignext+1)%numpop;
          }
          if (gen_total%repinterval==0) {                      /* Report if  */
            report(gen_total,fileoutput,pops);                 /* necessary  */
  	  }
        }
      }      
  }
  echo();
  nocbreak();
  if(fileoutput)
     fclose(output);  
  if(chromoutput)
     fclose(chromofile);
  free_pops(pops,numpop*numchromo);
  move(last_line-1,0);
  refresh();
  endwin();
  exit(0);
}


/************************************************************************/
/* This updates the screen and, if necessary, the output file           */
/************************************************************************/

void report(gen_total,fileoutput,pops)
int gen_total, fileoutput;
CHROMOSOME *pops;
{
  int pop, chromo;
  double average, best, tmp;

  mvprintw(GEN_ROW,GEN_COL+12,"%d",gen_total);         /* necessary */
  mvprintw(EVCOUNT_ROW,EVCOUNT_COL+20,"%d", evals);
  if(fileoutput) {
       fprintf(output, "\nGenerations=%d  Evaluations-so-far=%d\n",
                       gen_total,evals);
   }
   for(pop=0; pop<numpop; pop+=1) {               
      average=0.0;
      best = -1000000.0;
      for(chromo=0; chromo<numchromo; chromo+=1) {
          /* Note that best is usually pops[pop*numchromo] since */
          /* the population is sorted - but not sorted with -rssN */
          tmp = pops[pop*numchromo+chromo].fitness;
          average+=tmp;
          if(best < tmp) best = tmp;
      }
      average/=(double)numchromo;
      mvprintw(POPTABLE_ROW+1+pop,POPTABLE_COL,
               "%-4d%c    %12.7f     %12.7f",
               pop, (fabs(average-best)
                          < 1e-9)? '=' : ' ',
                          average,
                                     best);
      if(fileoutput) {
          fprintf(output, "  %-4d    %12.7f     %12.7f\n",
                             pop,    average,   best);
      }
      refresh();
  }
}


/************************************************************************/
/* This frees whole population                                          */
/************************************************************************/

void free_pops(whole_pop,num)
CHROMOSOME *whole_pop;
{
  int i;

  for(i=0; i<num; i++) {
    /* Avoid freeing anything twice; matters for some operating systems */
    if(whole_pop[i].gdp->refcount == 1) {
      free(whole_pop[i].gdp->genotype);
      free(whole_pop[i].gdp);
    } else
      whole_pop[i].gdp->refcount--;
  }
  free(whole_pop);
}

/************************************************************************/
/* This initialises a chromosome                                        */
/************************************************************************/

void init_genotype(gdp)
GENODATA *gdp;
{
  int i;

  gdp->refcount = 1;
  gdp->id = chromo_id++;
  gdp->parent1 = gdp->parent2 = 0;
  gdp->genotype = (char *)malloc(geno_size+1);
  gdp->genotype[geno_size] = '\0';  /* for printing */
  for(i=0;i<geno_size;i++)
     gdp->genotype[i] = '0'+(random() & 1);
}

/************************************************************************/
/* This creates and randomly initialises the chromosome populations     */
/************************************************************************/

CHROMOSOME *init_pops(numpops,numchromo)
{
  int chromo,pop;
  CHROMOSOME *pops;

  pops=(CHROMOSOME *)malloc(numpop*numchromo*sizeof(CHROMOSOME));
  if(pops == (CHROMOSOME *)NULL) {
        mvprintw(POPTABLE_ROW+2,POPTABLE_COL,
                    "Cannot get enough memory - goodbye");
        move(last_line-1,0);
        refresh();
        exit(1);
  }

  for(pop=0;pop<numpops;pop++) total_abs_fitness[pop] = 0.0;

  for(chromo=0; chromo<numpops*numchromo; chromo+=1) {  /* Initialise the */
    pops[chromo].gdp = (GENODATA *)malloc(sizeof(GENODATA)); /* pops.     */
    init_genotype(pops[chromo].gdp);             
    pops[chromo].fitness=evaluate(pops[chromo].gdp->genotype);
    /* total_abs_fitness[n] is total fitness of population n */
    total_abs_fitness[chromo/numchromo] += fabs(pops[chromo].fitness);
  }

  for(pop=0; pop<numpops; pop+=1) {
    qsort((char *)((int)pops+(pop*numchromo*sizeof(CHROMOSOME))),
          numchromo, sizeof(CHROMOSOME), compchromo);
  }
  return(pops);
}


/************************************************************************/
/* This resets the defaults according to the argument 'arg'.            */
/************************************************************************/

void handle(arg)
char *arg;
{
  int newint;
  double newdouble;

  if (*arg=='-') {               /* Either a switch ...        */
    switch (*(arg+1)) {
    case 'P' :
      sscanf(arg,"-P%d",&newint);
      if (newint<1) {
        fprintf(stderr,"PGA: You must have at least 1 population.\n");
        exit(1);
      }
      else {
        numpop=newint;
      }
      break;
    case 'p' :
      sscanf(arg,"-p%d",&newint);
      if (newint<2) {
        fprintf(stderr,"PGA: You must have at least 2 chromosomes per population.\n");
        exit(1);
      }
      else {
        numchromo=newint;
      }
      break;
    case 'n' :
      sscanf(arg,"-n%d",&newint);
      if (newint<12) {
        fprintf(stderr,"PGA: Chromosome length must be at least 12.\n");
        exit(1);
      }
      else {
        geno_size=newint;
        sprintf(geno_size_string,"%d",geno_size);
      }
      break;
    case 'l' :
      sscanf(arg,"-l%d",&newint);
      if (newint<1) {
        fprintf(stderr,"PGA: Less than one generation per stage isn't sensible!\n");
        exit(1);
      }
      else {
        gens_stage=newint;
      }
      break;
    case 'i' :
      sscanf(arg,"-i%d",&newint);
      if (newint<1) {
        fprintf(stderr,"PGA: A report every %d generations isn't sensible!\n",newint);
        exit(1);
      }
      else {
        repinterval=newint;
      }
      break;
    case 'M' :
      if((reproduction == ss_gen_reproduction)
         || (reproduction == ss_one_reproduction)) {
        fprintf(stderr, "PGA: cannot use -M or -s with -rssoneN or -rssgenN\n");
        exit(1);
      }
      sscanf(arg,"-M%d",&newint);
      if (newint<0) {
        fprintf(stderr,"PGA: A migration interval of %d doesn't make sense.\n",newint);
        exit(1);
      }
      else {
        migint=newint;
      }
      break;
    case 'S' :
      sscanf(arg,"-S%d",&newint);
      if(newint<0) {
        fprintf(stderr,"PGA: random seed must be non-negative, set to 1.\n");
        newint = 1;
      }
      srandom(newint);
      break;
    case 'a' :
      isadaptive=!isadaptive;
      if (isadaptive) mrate=adaptive;
      else mrate=nonadaptive;
      break;
    case 't' :
      twins = !twins;
      break;
    case 'm' :
      sscanf(arg,"-m%lf",&newdouble);
      if (newdouble<0 || newdouble>1) {
        fprintf(stderr,"PGA: The mutation rate must be between 0 and 1.\n");
        exit(1);
      }
      else {
        mute_rate=newdouble;
      }
      break;
    case 'c' :
      sscanf(arg,"-c%lf",&newdouble);
      if (newdouble<0 || newdouble>1) {
        fprintf(stderr,"PGA: The crossover rate must be between 0 and 1.\n");
        exit(1);
      }
      else {
        cross_rate=newdouble;
      }
      break;
    case 'b' :
      sscanf(arg,"-b%lf",&newdouble);
      if (newdouble<1.0 || newdouble>2.0) {
        fprintf(stderr,"PGA: The bias must be strictly between 1 and 2.\n");
        exit(1);
      }
      else {
        bias=newdouble;
      }
      break;
    case 's' :
      if((reproduction == ss_gen_reproduction)
         || (reproduction == ss_one_reproduction)) {
        fprintf(stderr, "PGA: cannot use -s or -M with -rssN\n");
        exit(1);
      }
      if (!strcmp(arg,"-srank")) {
        select = rank_select;
        select_name = "rank";
      }
      else if (!strcmp(arg,"-sfitprop")) {
        select = fitprop_select;
        select_name = "fitprop";
      }
      else if (!strncmp(arg,"-stm",4)) {
        select = tm_select;
        select_name = strdup(arg+2);
        tournament_size = atoi(arg+4);
        if (tournament_size < 1) {
          fprintf(stderr,"PGA: tm selection requires arg > 1; set to 10\n");
          tournament_size = 10;
        }
      }
      else if (!strncmp(arg,"-stn",4)) {
        select = tn_select;
        select_name = strdup(arg+2);
        tournament_size = atoi(arg+4);
        if (tournament_size < 1) {
          fprintf(stderr,"PGA: tn selection requires arg > 1; set to 10\n");
          tournament_size = 10;
        }
      }
      else {
        fprintf(stderr,"PGA: I don't know about %s selection.\n",arg+2);
        exit(1);
      }
      break;
    case 'r' :
      if (!strcmp(arg,"-rone")) {
        reproduction = one_reproduction;
        repro_name = "one";
      }
      else if (!strcmp(arg,"-rgen")) {
        reproduction = gen_reproduction;
        repro_name = "gen";
      }
      else if (!strncmp(arg,"-rssgen",7)) {
        reproduction = ss_gen_reproduction;
        repro_name = strdup(arg+2);
        select_name = "*spatial*";
        ss_n = atoi(arg+7);
        if(ss_n < 1) {
          fprintf(stderr, "PGA: ss random walk must be of length > 0, set to 5\n");
          ss_n = 5;
	}
      }
      else if (!strncmp(arg,"-rssone",7)) {
        reproduction = ss_one_reproduction;
        repro_name = strdup(arg+2);
        select_name = "*spatial*";
        ss_n = atoi(arg+7);
        if(ss_n < 1) {
          fprintf(stderr, "PGA: ss random walk must be of length > 0, set to 5\n");
          ss_n = 5;
	}
      }
      else {
        fprintf(stderr,"PGA: I don't know about %s reproduction.\n",arg+2);
        exit(1);
      }
      break;
    case 'C':
      if (!strcmp(arg,"-Cone")) {
        cross_name = "one-point";
        crossover = one_pt_cross;
      } else if (!strcmp(arg,"-Ctwo")) {
        cross_name = "two-point";
        crossover = two_pt_cross;
      } else if (!strcmp(arg,"-Cuniform")) {
        cross_name = "uniform";
        crossover = uniform_cross;
      }
      break;
    case 'e' :
      if (!strcmp(arg,"-emax")) {
        evaluate = eval_max;
        eval_name = "max";
        sprintf(geno_size_string,"%d",geno_size);
        maxfitness = geno_size_string;
      }
      else if (!strcmp(arg,"-edj1")) {
        evaluate = eval_dj1;
        eval_name = "dj1";
        maxfitness = "100";
      }
      else if (!strcmp(arg,"-edj2")) {
        evaluate = eval_dj2;
        eval_name = "dj2";
        maxfitness = "1000";
      }
      else if (!strcmp(arg,"-edj3")) {
        evaluate = eval_dj3;
        eval_name = "dj3";
        maxfitness = "55";
      }
      else if (!strcmp(arg,"-edj5")) {
        evaluate = eval_dj5;
        eval_name = "dj5";
        maxfitness = "approx 499.001997";
      }
      else if (!strcmp(arg,"-ebf6")) {
        evaluate = eval_bf6;
        eval_name = "bf6";
        maxfitness = "approx 0.994007";
      }
      else if (!strcmp(arg,"-eknap")) {
        evaluate = eval_knap;
        eval_name = "knap";
        maxfitness = "1.0";
      }
      else if (!strcmp(arg,"-err")) {
        evaluate = eval_rr;
        eval_name = "rr";
        /* NB, maxfitness set after arg processing, once params known */
      }
      else {
        fprintf(stderr,"PGA: unknown evaluation function: %s\n",
                        arg+2);
        exit(1);
      }
      break;
    default :
      if (*(arg+1)!='h') fprintf(stderr,"Unknown argument %s.\n\n",arg);
      pga_help();
      exit(0);
    }
  }
  else {                         /* ... or an output filename. */
    output=fopen(arg,"a");
    if (output==NULL) {
      fprintf(stderr,"PGA: I can't open %s for writing.\n",arg);
      exit(1);
    }
    strcpy(outfilename,arg);
  }
}

/************************************************************************/
/* Dumps chromosomes to a file. Filename is outfilename.gen, eg foo.500 */
/************************************************************************/

void dump_chromos(pops,g)
CHROMOSOME *pops;
int g;
{
  int i,j;

  if(!chromoutput) {
     sprintf(chromofilename,"%s.chr",outfilename);
     chromofile = fopen(chromofilename,"a");
     chromoutput = (chromofile != (FILE *)NULL);
  }

  if(chromoutput) {
    for(i=0;i<numpop;i++) {
      for(j=0;j<numchromo;j++) {
        fprintf(chromofile,
                "%-2d %-4d %-5d %12.7f  %s  %5d %5d %5d\n",
                 i,   j,   g,    pops[i*numchromo+j].fitness,
                                 pops[i*numchromo+j].gdp->genotype,
                                 pops[i*numchromo+j].gdp->id,
                                 pops[i*numchromo+j].gdp->parent1,
                                 pops[i*numchromo+j].gdp->parent2);
      }
      fprintf(chromofile,"\n\n");
    }
  }
}    


/************************************************************************/
/* This function inserts 'chromo' into the correct place (sorted by     */
/* fitness) within the subpopulation defined by 'base' and 'range' of   */
/* the populations array 'pops'.                                        */
/************************************************************************/

void insert(pops,chromo,base,range)
CHROMOSOME *pops,chromo;
int base,range;
{
  int current, pop;

  pop = base/numchromo;         /* which pop we're hitting */

                                /* Can it be inserted at all? */
  if (chromo.fitness>pops[base+range-1].fitness) {
    chromo.gdp->refcount++;
    if(pops[base+range-1].gdp->refcount == 1) {
      total_abs_fitness[pop] -= fabs(pops[base+range-1].fitness);
      free(pops[base+range-1].gdp->genotype);
      free(pops[base+range-1].gdp);
    }
    current=base+range-2;       /* Start at the second last place. */
    do {                        /* If 'chromo' is fitter ... */
      if (chromo.fitness>pops[current].fitness) {
        pops[current+1]=pops[current]; /* ... then push it down one. */
      }
      else {
        total_abs_fitness[pop] += fabs(chromo.fitness) 
                                   - fabs(pops[current+1].fitness);        
        pops[current+1]=chromo;        /* ... else insert 'chromo' below */
        current=base;                  /* and finish.                    */
      }
      current-=1;               /* Work up the array until ... */
    } while (current>=base);    /* we get past the 'base'.     */
    if (chromo.fitness>pops[base].fitness) {
      total_abs_fitness[pop] += fabs(chromo.fitness) 
                                 - fabs(pops[base].fitness);
      pops[base]=chromo;        /* If chromo is the best put it in. */
    }
  } else {     /* It cannot be inserted, it is terrible */
      if(chromo.gdp->refcount==0) {
        free(chromo.gdp->genotype);
        free(chromo.gdp);
      }
  }
}


/************************************************************************/
/* Returns >0 if chromo2 is fitter than chromo1, <0 if chromo 1 is      */
/* fitter than chromo2, and 0 if they are equally fit. This is passed   */
/* to qsort as its comparison function.                                */
/************************************************************************/

int compchromo(chromo1,chromo2)
CHROMOSOME *chromo1,*chromo2;
{
  if((chromo2->fitness) > (chromo1->fitness))
     return(1);
  else if((chromo2->fitness) == (chromo1->fitness))
     return(0);
  else
     return(-1);
}

/***********************************************************************/
/* Returns a random double in range 0.0 to 1.0 (never 1.0 exactly)     */
/***********************************************************************/
double drandom()
{
  return((double)random()/2147483647.0);
}

/* end of pga.c */
