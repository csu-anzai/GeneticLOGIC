/**********************************************************************
* Source: statistic.c         Version 1.0
*
* Copyright (C) 1990  Jason J. Spofford
*
* This program is free software; you can redistribute it and/or modify
* it under the terms of the GNU General Public License as published by
* the Free Software Foundation; either version 1, or (at your option)
* any later version.
*
* This program is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU General Public License for more details.
*
* You should have received a copy of the GNU General Public License
* along with this program; if not, write to the Free Software
* Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*
*  Purpose:
*      This source code contains functions to start, record and stop
* statistical information from each generation. The stats.extension
* file is intended to be a text file importable into spreadsheets for
* graphing and plotting.  
*
* For further information contact:
*       Professor Kenneth J. Hintz
*       Department of Electrical and Computer Engineering
*       George Mason University
*       4400 University Drive
*       Fairfax, VA 22030
*
*       email:   khintz@gmuvax2.gmu.edu
*       Tel:     (703)993-1592
*
* Written by: Captain Jason J. Spofford, USAF
*             ECE Graduate Student, George Mason University
* Written on: 12 April  1990
* Updated on: 3  August 1990
*********************************************************************/
/* The following include files are required */
#include <stdio.h>
#include "comdef.h"

/* External variables required */
extern FILE *history;
extern struct net_info *par_info;
extern struct configuration conf;
extern int sum_out_neuron[MAX_NEURON];

/* The following functions are defined */
int  start_stats(char *, char *);
void stop_stats ();
void take_stats (int, int);

/* The following variables are defined */
FILE *stats;

/****************************************************************
* start_stats
*
*	This procedure creates or continues the statistic file. 
*
* Input : extension - the particular run's extension.
*         mode      - CREATE or APPEND the stats file.
* Output: Returns status (SUCCESS or ERROR)
*
* Global: stats - sets the stats file pointer.
****************************************************************/

int start_stats(char *extension, char *mode)

{
char stats_name[80];
int  error_code = SUCCESS;

/* Set up the file name */
strcpy(stats_name,"stats.");
strcat(stats_name,extension);

/* Create or append to the stats file */
stats=fopen(stats_name,mode);

if (stats!=NULL) 
  {
  if (!strcmp(mode,CREATE)) /* if mode equals CREATE */
    {
    fprintf(stats,"%s\n",extension);
    fprintf(stats,"gen ave_f bst_f wst_f av_nn lg_nn sm_nn pop ");
    fprintf(stats,"/ out num / fit bred\n");
    fputs("Stats file created.\n",history);
    }
  else fputs("Stats file opened.\n",history);
  }
else error_code = ERROR;

return(error_code);
} /* END OF start_stats */

/**************************************************************
* stop_stats
*
*	This procedure closes the statistics file.
*
* Input : none.
* Output: none.
*
* Global: stats - uses the stats file pointer to close the file.
***************************************************************/

void stop_stats()

{

if (!fclose(stats)) 
  fputs("Stats file closed.\n",history);

} /* END OF stop_history */

/*********************************************************************
* take_stats
*
*    This function gathers statistics on a generation of parents
* and stores those statistics in the stats.extension file.  Each
* generation is written out in one long line. The file should be
* importable into a spreadsheet for graphing.
*
* Input : replacement - how many parents replaced their children
*         gen         - the current generation number
* Output: none.
*
* Global: conf.stats     - the frequency of taking statistics
*         conf.pop_size  - the population size
*         par_info       - the parent information record array
*         sum_out_neuron - the statistics on output neurons
*         stats          - the statistics file pointer
*********************************************************************/

void take_stats(int replacement, int gen)

{
float low_fit,     high_fit,    ave_fit;
int   lg_neuron,   neuron_tot,  sm_neuron;
int   lg_diff,     sm_diff,     lg_sim;
int   sm_sim,      dead_count,  cnt;
float ave_cyc_fit, low_cyc_fit, high_cyc_fit;
float ave_nn_fit,  low_nn_fit,  high_nn_fit;
float ave_io_fit,  low_io_fit,  high_io_fit;
float death_rate,  ave_neuron,  fit_sum;
float ave_diff,    ave_sim;

/* Take statistics only if required */
if ( ((float) gen / (float) conf.stats) == (float) (gen / conf.stats))
  {
  /* Initialize variables */
  low_fit      = 2.0;
  high_fit     = -1.0;
  neuron_tot   = 0;
  lg_neuron    = 0;
  sm_neuron    = MAX_NEURON + 1;
  dead_count   = 0;
  lg_diff      = 0;
  sm_diff      = conf.io_sets * conf.io_sets;
  lg_sim       = 0;
  sm_sim       = conf.io_sets * conf.io_sets * conf.e_outputs;
  ave_cyc_fit  = 0.0;
  ave_io_fit   = 0.0;
  ave_nn_fit   = 0.0;
  ave_diff     = 0.0;
  ave_sim      = 0.0;
  low_cyc_fit  = 2.0;
  low_io_fit   = 2.0;
  low_nn_fit   = 2.0;
  high_cyc_fit = -1.0;
  high_nn_fit  = -1.0;
  high_io_fit  = -1.0;
  fit_sum      = 0.0;
  
  /* Gather stats from par_info */
  for ( cnt = 0; cnt < conf.pop_size; cnt++)
    {
    ave_cyc_fit += par_info[cnt].cyc_fit;
    ave_io_fit  += par_info[cnt].io_fit;
    ave_nn_fit  += par_info[cnt].nn_fit;
    ave_diff    += par_info[cnt].different;
    ave_sim     += par_info[cnt].similar;
    fit_sum     += par_info[cnt].fitness;
  
    if (par_info[cnt].bred == 0) dead_count++;

    if (low_cyc_fit > par_info[cnt].cyc_fit) 
      low_cyc_fit = par_info[cnt].cyc_fit;

    if (low_io_fit > par_info[cnt].io_fit) 
      low_io_fit = par_info[cnt].io_fit;

    if (low_nn_fit > par_info[cnt].nn_fit) 
      low_nn_fit = par_info[cnt].nn_fit;

    if (low_fit > par_info[cnt].fitness) 
      low_fit = par_info[cnt].fitness;

    if (high_cyc_fit < par_info[cnt].cyc_fit)
      high_cyc_fit = par_info[cnt].cyc_fit;

    if (high_io_fit < par_info[cnt].io_fit)
      high_io_fit = par_info[cnt].io_fit;

    if (high_nn_fit < par_info[cnt].nn_fit)
      high_nn_fit = par_info[cnt].nn_fit;

    if (high_fit < par_info[cnt].fitness)
      high_fit=par_info[cnt].fitness;

    if (lg_neuron < par_info[cnt].num_neurons)
      lg_neuron = par_info[cnt].num_neurons;

    if (sm_neuron > par_info[cnt].num_neurons)
      sm_neuron = par_info[cnt].num_neurons;

    if (lg_diff < par_info[cnt].different)
      lg_diff = par_info[cnt].different;

    if (sm_diff > par_info[cnt].different)
      sm_diff = par_info[cnt].different;

    if (lg_sim < par_info[cnt].similar)
      lg_sim = par_info[cnt].similar;

    if (sm_sim > par_info[cnt].similar)
      sm_sim = par_info[cnt].similar;

    neuron_tot += par_info[cnt].num_neurons;
    }

  /* Calculate averages */
  ave_neuron   = (float) neuron_tot / (float) conf.pop_size; 
  ave_cyc_fit /= (float) conf.pop_size;
  ave_io_fit  /= (float) conf.pop_size;
  ave_nn_fit  /= (float) conf.pop_size;
  ave_diff    /= (float) conf.pop_size;
  ave_sim     /= (float) conf.pop_size;
  death_rate   = (float) dead_count / (float) conf.pop_size;
  ave_fit      = fit_sum / (float) conf.pop_size;

  /* Print results to the stats file */
  fprintf(stats,"%d %d %f %f %f %f %f %f %f %f %f %f %f %f %f %d %f %d ",
    gen, replacement, death_rate, high_fit, ave_fit, low_fit, high_io_fit,
    ave_io_fit, low_io_fit, high_cyc_fit, ave_cyc_fit, low_cyc_fit,
    high_nn_fit, ave_nn_fit, low_nn_fit, lg_neuron, ave_neuron, sm_neuron);

  fprintf(stats,"%d %f %d %d %f %d : ", lg_diff, ave_diff, sm_diff,
    lg_sim, ave_sim, sm_sim);

  /* Print output neuron information to the stats file */
  for ( cnt = 0; cnt < lg_neuron; cnt++)
    fprintf(stats,"%d %d ",cnt,sum_out_neuron[cnt]);

  fprintf(stats,"\n");
  } /* stop <if gen, conf.stats> */
} /* END OF take_stats */

