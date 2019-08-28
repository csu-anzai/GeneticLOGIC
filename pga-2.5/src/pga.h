/************************************************************************
 *                                                                      *
 *               PGA - Parallel Genetic Algorithm Testbed               *
 *                                                                      *
 *           "A Parallel Implementation of a genetic algorithm"         *
 *                                                                      *
 *                By Geoffrey H. Ballinger (geoff@ed.ac.uk),            *
 *                   Peter Ross (peter@ed.ac.uk)                        *
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

typedef struct genodata {     /* genotype string plus reference count    */
  char *genotype;
  int refcount;
  int id;
  int parent1, parent2;
} GENODATA;

typedef struct chromosome {   /* A chromosome consists of a genotype and */
  GENODATA *gdp;               /* a fitness.                             */
  double fitness;
} CHROMOSOME;

typedef struct chromopair {   /* Returned by crossover, contains one or */
  CHROMOSOME child1;          /* two children depending on -t flag      */
  CHROMOSOME child2;
} CHROMOPAIR;

/* If a genotype is longer than this (set by -n) then use
   a double in the decode routine, because a long will overflow.
   A long is 32 bits on most systems, so a value of 60 assumes
   that every call of decode() will be trying to split the
   genotype into at least two numbers, so won't be collecting
   up more than 30 bits at one go. */

#define CRUCIAL_GENO_SIZE 60

/* User actions possible */

#define QUIT      1
#define CONTINUE  2
#define RESTART   3
#define ASK_AGAIN 4

/* data input files */

#define WEIGHTSFILE  "weights"  /* name of file for knapsack data */
#define RRDATAFILE   "rrdata"   /* name of file for Royal Road data */
