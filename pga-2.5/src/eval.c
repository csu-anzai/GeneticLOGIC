/* eval.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Defines the problems: each eval_whatever takes a genotype pointer
 * as argument, returns the fitness as a double.
 */

#include <stdio.h>
#include "pga.h"

#define TRUE 1
#define FALSE 0

extern int evals;
extern int geno_size;
extern void (*decode)();
extern long *sticks;
extern long target;
extern char *rr_blockarray;
extern int rr_blocks;
extern int rr_blocksize;
extern int rr_gap;
extern int rr_mstar;
extern double rr_ustar;
extern double rr_u;
extern double rr_v;

extern double pow();
extern double floor();
extern double sin();
extern double sqrt();
extern double fabs();

/************************************************************************/
/* This returns the number of bits set to one in 'genotype'. It is used */
/* to evaluate a chromosomes fitness.                                   */
/************************************************************************/

double eval_max(genotype)
char *genotype;
{
  int value, loop;

  evals+=1;

  value=0;
  for(loop=0; loop<geno_size; loop++) {       /* Check each bit in turn. */
    if (genotype[loop] == '1') value+=1;
  }

  return((double)value);
}


/************************************************************************/
/* De Jong's F1. Max fitness = 100                                      */
/************************************************************************/

double eval_dj1(genotype)
char *genotype;
{
  double x[3];

  evals+=1;

  decode(genotype,3,-5.12,5.12,x);

  return 100.0 - (x[0]*x[0]+x[1]*x[1]+x[2]*x[2]);
}


/************************************************************************/
/* De Jong's F2. Max fitness = 1000                                     */
/************************************************************************/

double eval_dj2(genotype)
char *genotype;
{
  double x[2];

  evals+=1;

  decode(genotype,2,-2.048,2.048,x);

  return 1000.0 - (100.0*(x[0]*x[0]-x[1])*(x[0]*x[0]-x[1]) + (1-x[0])*(1-x[0]));
}


/************************************************************************/
/* De Jong's F3. Max fitness = 55                                       */
/************************************************************************/

double eval_dj3(genotype)
char *genotype;
{
  double x[5];

  evals+=1;

  decode(genotype,5,-5.12,5.12,x);

  return 25.0 - (floor(x[0])+floor(x[1])+floor(x[2])+floor(x[3])+floor(x[4]));
}


/************************************************************************/
/* De Jong's F5. Max fitness = 500                                      */
/************************************************************************/

static int a[2][25] ={
        {
                -32, -16, 0, 16, 32, -32, -16, 0, 16, 32, -32, -16, 0, 16, 32,
                -32, -16, 0, 16, 32, -32, -16, 0, 16, 32        },
        {
                -32, -32, -32, -32, -32, -16, -16, -16, -16, -16,
                16, 16, 16, 16, 16, 32, 32, 32, 32, 32  }
};

double eval_dj5(genotype)
char *genotype;
{
  double x[2],total,lowtot,prod;
  int i,j,power;

  evals+=1;

  decode(genotype,2,-65.536,65.536,x);

  total=0.002;
  for(j=0; j<25; j+=1) {
    lowtot=1.0 + (double)j;
    for(i=0; i<2; i+=1) {
      prod=1.0;
      for(power=0; power<6; power+=1) {
        prod*=x[i]-a[i][j];
      }
      lowtot+=prod;
    }
    total+=1.0/lowtot;
  }

  return 500.0 - (1.0/total);
}

double eval_bf6(genotype)
char *genotype;
{
  double x[2], temp, temp2;

  evals+=1;

  decode(genotype,2,-100.0,100.0,x);

  temp = (x[1]*x[1] + x[0]*x[0]);
  temp2 = sin(sqrt(temp));
  
  return ( (temp2*temp2)/(1.0+0.001*temp*temp) );
}

double eval_knap(genotype)
char *genotype;
{
  int i;
  long sum = 0;
  double fit;

  evals += 1;

  for(i=0; i < geno_size; i++) {
    if(genotype[i]=='1') sum += sticks[i];
  }
  fit = 1.0/(1.0 + fabs((double)target - (double)sum));
  return(fit);
}

int getint(f,valaddr)
FILE *f;
int *valaddr;
{
  int r;
  while(1) {
    r = fscanf(f,"%d",valaddr);   /* try to read it */
    if(r == EOF) return(0);       /* EOF? give up */
    else if(r == 1) return(1);    /* got one? return happy */
    else fscanf(f,"%*s");         /* else try to skip */
  }
}

int getlong(f,valaddr)
FILE *f;
long *valaddr;
{
  int r;
  while(1) {
    r = fscanf(f,"%ld",valaddr);  /* try to read it */
    if(r == EOF) return(0);       /* EOF? give up */
    else if(r == 1) return(1);    /* got one? return happy */
    else fscanf(f,"%*s");         /* else try to skip */
  }
}

int getdouble(f,valaddr)
FILE *f;
double *valaddr;
{
  int r;
  while(1) {
    r = fscanf(f,"%lf",valaddr);   /* try to read it */
    if(r == EOF) return(0);       /* EOF? give up */
    else if(r == 1) return(1);    /* got one? return happy */
    else fscanf(f,"%*s");         /* else try to skip */
  }
}

/* For knapsack problem, must read integers from file `weights' */
int read_weights_file()
{
FILE *f;
    int i;
    long t;
    
    /* read target and sizes from file "weights" if it exists readable */
    if((f=fopen(WEIGHTSFILE,"r")) != (FILE *)NULL) {
          if(!getlong(f,&target)) {
             fclose(f);
             return(0);
	  }
          i = 0;
          while(i < geno_size && (getlong(f,&t))) {
             sticks[i++] = t;
          }
          fclose(f);
          return(1);
    }
    else return(0);
}

double eval_rr(genotype)
char *genotype;
{
    double score = 0.0;
    int total, i, j, index, n, proceed;

    evals+=1;

    n = 0;
    /* Do lowest-level blocks */
    for(i=0; i<rr_blocks; i++) { /* run through each low-level block */
      total = 0;
      for(j=i*(rr_blocksize+rr_gap);
          j<i*(rr_blocksize+rr_gap)+rr_blocksize;j++)
        if(genotype[j] == '1') total++;  /* count bits in block */
      if(total > rr_mstar && total < rr_blocksize)
        score -= (total-rr_mstar)*rr_v;
      else if(total <= rr_mstar)
        score += total * rr_v;
      if(total == rr_blocksize) {
        rr_blockarray[i] = '1';
        n++;
      } else
        rr_blockarray[i] = '0';
    }

    /* Bonus for filled low-level blocks */
    if(n>0)
      score += rr_ustar + (n-1)*rr_u;

    /* Do higher-level  blocks */
    n = rr_blocks;  /* n counts no. of lower-level blocks */
    proceed = TRUE; /* worth looking at next higher level? */
    while ((n > 1) && proceed) {
      proceed = FALSE;
      total = 0;
      /* there are n valid blocks in the blockarray each time */
      /* round, so n=2 is the last.                           */
      for(i=0,index=0;i<(n/2)*2;i+=2,index++) {
        if(rr_blockarray[i] == '1' && rr_blockarray[i+1] == '1') {
          total++;
          proceed = TRUE;
          rr_blockarray[index] = '1';
	} else
          rr_blockarray[index] = '0';
      }
      if(total > 0) score += rr_ustar + (total-1)*rr_u;
      n /= 2;
    }
    return(score);
}

void read_rrdata_file()
{
    FILE *f;
    
    /* read RR data from file if it exists; else use defaults */
    if((f=fopen(RRDATAFILE,"r")) != (FILE *)NULL) {
       if(getint(f,&rr_blocksize)
          && getint(f,&rr_gap)
          && getint(f,&rr_mstar)
          && getdouble(f,&rr_ustar)
          && getdouble(f,&rr_u)
          && getdouble(f,&rr_v));
    }
    rr_blocks = geno_size/(rr_blocksize+rr_gap);
}

  
/************************************************************************/
/* This decodes the requested number of real parameters from the given  */
/* chromosome.                                                          */
/************************************************************************/

void long_decode(genotype,number,lower,upper,array)
char *genotype;
int number;
double lower,upper,*array;
{
  int loop,count,bits;
  long value;

  bits=(geno_size/number);
  
  for(loop=0; loop<number; loop+=1) {
    value=0;
    for(count=0; count<bits; count++) {
      value = 2*value + (int)(genotype[count+loop*bits] - '0');
    }
    array[loop]=((upper-lower)*((double)value)/pow(2.0,(double)bits))+lower;
  }
}  

void double_decode(genotype,number,lower,upper,array)
char *genotype;
int number;
double lower,upper,*array;
{
  int loop,count,bits;
  double value;

  bits=(geno_size/number);
  
  for(loop=0; loop<number; loop+=1) {
    value=0.0;
    for(count=0; count<bits; count++) {
      value = 2*value + (double)(genotype[count+loop*bits] - '0');
    }
    array[loop]=((upper-lower)*(value)/pow(2.0,(double)bits))+lower;
  }
}  


