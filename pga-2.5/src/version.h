/************************************************************************
 *                                                                      *
 *               PGA - Parallel Genetic Algorithm Testbed               *
 *                                                                      *
 *           "A Parallel Implementation of a genetic algorithm"         *
 *                                                                      *
 *                By Peter Ross (peter@ed.ac.uk)                        *
 *                   Geoffrey H. Ballinger (geoff@ed.ac.uk),            *
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

#define TITLE    "PGA: parallel genetic algorithm testbed"
#define VERSION  "2.5"

/*
 * Revision 2.5  93/9/17 pmr
 * Split into several C files for convenience.
 * Chromo dump also numbers chromos within population, to make 3-d
 *   plots easier to construct from a dump. Also makes it easier
 *   to summarise a complete population using a (g)awk script.
 * Marriage tournament selection (-stmN): choose one at random, have
 *   up to N tries to find one fitter, stop on first fitter. If none,
 *   return that initial choice. Based on `marriage' problem, classic
 *   of the dynamic programing literature.
 * Original tournament selection (-stnN): choose N at random, return
 *   fittest.
 * Allow seeding of random number generator (-SN).
 * Proper Royal Road function now (-err); parameter data is read from a file
 *   called rrdata, or else uses Holland's defaults.
 * Changed number reading: now when reading weights or rrdata file, all
 *   non-numeric text is just ignored. And no keyboard echo now.
 * Can choose whether crossover produces one child or two (-t)
 * Spatially organised reproduction (-rssN) using random walk to find
 *   parents, modelled on Wright's shifting balance model of evolution.
 *   Requires specific selection routine, and migration disallowed.
 * Bug fix thanks to Willem Vermin: free_pops no longer frees anything
 *   twice, as some OSs object fatally to this.
 * Bug fix thanks to Willem Vermin: insert should also deduct fitness
 *   of least fit member from total_abs_fitness when that member gets
 *   removed
 * Blunder pointed out by Dave Corne: default crossover was 1-pt, but
 *   shown as 2-pt.
 *
 * Revision 2.4  93/6/29 pmr
 * Added `Royal road' problem, user-selectable block size (-errK, K int)
 * Fixed minor problem in fitprop_select(), copes with tiny/negative
 *   fitnesses (negative..??)
 * Fitprop_select() doesn't recalculate total fitness every time now;
 *   just updated on insertion and in gen reproduction.
 * If saving to file, can save chromosomes too (in "file.chr")
 * Track chromosome parentage.
 *
 * Revision 2.3  93/6/18 pmr
 * Added one-point and uniform crossovers, (-C option)
 * First public (non-Edinburgh) release.
 *
 * Revision 2.2  93/6/2  pmr
 * Added 1-d knapsack problem, info from file `weights'
 * Modified screen layout a little
 *
 * Revision 2.1  93/3/18 pmr
 * Switch to char array for chromosomes, user-selectable length
 * Some code tidying
 *
 * Revision 2.0  92/2/27 pmr
 * Basic curses output
 * Added problem akin to binary F6 of Schaffer et al
 * Fixed problem with gen reproduction
 * Output to file too (append) if named
 * Fixed problem with sorting of chromosomes
 *
 * Revision 1.9  92/2/27 pmr
 * Some code and output tidying
 * Fixed bias problems
 *
 * Revision 1.8  90/11/23  10:24:33  ghb
 * Fitness is now a double.
 * Added De Jong's functions 1,2,3 and 5.
 * No longer display the best genotype.
 * Limit by evaluations, not generations.
 * Fixed bugs in generation based reproduction.
 * Adaptive mutation not allowed in generation based reproduction.
 * Fixed F5 using Genesis source as guide.
 * 
 * Revision 1.7  90/11/18  00:04:00  ghb
 * Deal in generations between migrations, not migrations per generation.
 * Fixed output bug.
 * Revision number in help screen.
 * 
 * Revision 1.6  90/11/14  11:51:49  ghb
 * Wrote replacement for drand48.
 * Adaptive mutation flag now works!
 * Added fitprop selection and gen reproduction.
 * Added user defined migration size.
 * 
 * Revision 1.5  90/11/13  10:16:32  ghb
 * Re-worked command line handleing.
 * Allow definable output file, operators, etc.
 * Removed "PGA: " from normal outputs.
 * Added help option.
 * 
 * Revision 1.4  90/11/12  08:56:48  ghb
 * Comments added.
 * 
 * Revision 1.3  90/11/09  13:30:42  ghb
 * Fixed Insertion to insert new best member.
 * It Works!
 * 
 * Revision 1.2  90/11/09  10:13:09  ghb
 * Shortened some variable names to make things more readable.
 * Limit number of generations, not number of evaluations.
 * Insertion, Selection, Reproduction, Crossover and Mutation!
 * 
 * Revision 1.1  90/11/07  21:27:02  ghb
 * Initial revision
 * 
*/
