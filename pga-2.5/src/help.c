/* help.c
 * Copyright (C) 1993 Peter Ross and Geoff Ballinger.
 * This is free software; you can redistribute it and/or modify it under the
 * terms of the GNU General Public License, see the file COPYING.
 *
 * Help invoked by -h lag or by command-line foulup.
 */

#include <stdio.h>
#include "version.h"

/***********************************************************************/
/* Help for the user                                                   */
/***********************************************************************/
void pga_help()
{
fprintf(stderr,"%s, version %s\n",TITLE,VERSION);
fprintf(stderr,"   -P<n>    Set number of populations. (5)\n");
fprintf(stderr,"   -p<n>    Set number of chromosomes per population. (50)\n");
fprintf(stderr,"   -n<n>    Set chromosome length. (32)\n");
fprintf(stderr,"   -l<n>    Set # of generations per stage. (100)\n");
fprintf(stderr,"   -i<n>    Set reporting interval in generations. (10)\n");
fprintf(stderr,"   -M<n>    Interval between migrations. (10)\n");
fprintf(stderr,"   -m<n>    Set bit mutation rate. (0.02)\n");
fprintf(stderr,"   -c<n>    Set crossover rate (only for `gen'). (0.6)\n");
fprintf(stderr,"   -b<n>    Set selection bias. (1.5)\n");
fprintf(stderr,"   -a       Adaptive mutation flag (FALSE)\n");
fprintf(stderr,"   -t       Twins: crossover produces pairs (FALSE)\n");
fprintf(stderr,"   -C<op>   Set crossover operator. (two)\n");
fprintf(stderr,"   -s<op>   Set selection operator. (rank)\n");
fprintf(stderr,"   -r<op>   Set reproduction operator. (one)\n");
fprintf(stderr,"   -e<fn>   Set evaluation function. (max)\n");
fprintf(stderr,"   -S<n>    Seed the random number generator. (from clock)\n");
fprintf(stderr,"   -h       Display this information.\n");
fprintf(stderr,"   <file>   Also log output in <file>. (none)\n\n");
fprintf(stderr,"   Crossover operators ... one, two, uniform.\n");
fprintf(stderr,"   Selection operators ... rank, fitprop,\n");
fprintf(stderr,"                           tnK (K=integer > 1),\n");
fprintf(stderr,"                           tmK (K=integer > 1).\n");
fprintf(stderr,"   Reproduction operators ... one, gen,\n");
fprintf(stderr,"                              ssoneN (N=integer > 0),\n");
fprintf(stderr,"                              ssgenN (N=integer > 0).\n");
fprintf(stderr,"   Evaluation functions ... max, dj1, dj2, dj3,\n");
fprintf(stderr,"                            dj5, bf6, knap,\n");
fprintf(stderr,"                            rr.\n");
}

