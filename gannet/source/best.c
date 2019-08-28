/**********************************************************************
* Source: best.c         Version 1.0
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
* Purpose:
*
*    This source code contains functions that start and stop the
* best.extension file.  The best file contains the best phenotype
* of a generation taken at intervals of conf.best generations.
* Another function actually saves the best structure to the file.
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
* Written on: 27 March 1990
* Updated on: 26 June  1990
*
**********************************************************************/
/* The follwing include files are required */
#include <stdio.h>
#include "comdef.h"

/* external variables required */
extern FILE *history;
extern FILE *parents;
extern struct net_info *par_info;
extern struct configuration conf;
extern struct genetic_code parent[2];
extern char pheno[MAX_NEURON][N_INPUTS * 2 + 4];
extern char out_neuron[NUM_OUTPUTS];
extern struct genetic_code *all_parents;

/* external functions required */
/* Source: gen_code.c */
extern int read_gen_code (int, int, struct genetic_code [1], FILE *);
extern int read_gen_quick(int, struct genetic_code *,
                          struct genetic_code [1], int);

/* Source: evolve.c */
extern void geno_to_pheno(struct genetic_code [1], int);

/* The following functions are defined */
int  start_best(char *, char *);
void stop_best ();
void save_best (int, int);

/* The following variables are defined */
FILE *best;

/****************************************************************
* start_best
*
*	This procedure creates or continues the best.extension file. 
* This file contains the best phenotype of a particular generation.
*
* Input : extension - the particular run's extension.
*         mode      - CREATE or APPEND the best file.
* Output: Returns status (SUCCESS or ERROR)
*
* Global: best - sets the best file pointer.
****************************************************************/

int start_best(char *extension, char *mode)

{
char best_name[80];
int  error_code = SUCCESS;

/* Set up the file name */
strcpy(best_name, "best.");
strcat(best_name, extension);

/* Create or append to the best file */
best = fopen(best_name, mode);

if (best != NULL) 
  {
  if ( !strcmp(mode, CREATE)) /* if mode equals CREATE */
    {
    fprintf(best, "BEST NETS! exper. : %s\n", extension);
    fputs("Best file created.\n", history);
    }
  else fputs("Best file opened.\n", history);

  } /* stop < if best != NULL> */
else error_code = ERROR;

return(error_code);
} /* END OF start_best */

/**************************************************************
* stop_best
*
*	This procedure closes the best file.
*
* Input : none.
* Output: none.
*
* Global: best - uses the stats file pointer to close the file.
***************************************************************/

void stop_best()

{

if (!fclose(best)) fputs("Best file closed.\n", history);

} /* END OF stop_best */

/**********************************************************************
* save_best
*
*    This function stores as text the best pheno_type of a particular
* generation.
*
* Input : gen         - the current generation number.
*         best_parent - the best parent number.
* Output: none.
* Global: par_info         - parent information array.
*         out_neuron       - the output neurons.
*         conf.best        - the frequency to record the best net.
*         conf.save_memory - whether or not we need to access the disk.
*         conf.e_outputs   - the number of environmental outputs.
*         pheno            - the parent phenotype.
*         parent           - the temporary genetic code storage struct.
*         parents          - file pointer to par_code.extension.
*         all_parents      - the genetic code stored in main memory.
**********************************************************************/

void save_best(int gen, int best_parent)

{
int cnt1, cnt2;

if ( (float) (gen/ conf.best) == ((float) gen / (float) conf.best))
  {

  if (conf.save_memory == TRUE)
    read_gen_code(par_info[best_parent].num_neurons,
      par_info[best_parent].seek_pos, parent, parents);
  else
    read_gen_quick(best_parent, all_parents, parent,
      par_info[best_parent].num_neurons);

  geno_to_pheno(parent, par_info[best_parent].num_neurons);

  fprintf(best, "Best Generation   : %d\n", gen);
  fprintf(best, "Age               = %d\n", par_info[best_parent].age);
  fprintf(best, "Bred              = %d\n", par_info[best_parent].bred);
  fprintf(best, "Number of Neurons = %d\n", par_info[best_parent].num_neurons);
  fprintf(best, "Number of Inputs  = %d\n", conf.e_inputs);
  fprintf(best, "Number of Outputs = %d\n", conf.e_outputs);
  fprintf(best, "Neuron Model      = %d\n", conf.n_model);
  fprintf(best, "Fitness           = %f\n", par_info[best_parent].fitness);
  fprintf(best, "IO Fitness        = %f\n", par_info[best_parent].io_fit);
  fprintf(best, "NN Fitness        = %f\n", par_info[best_parent].nn_fit);
  fprintf(best, "Cycle Fitness     = %f\n", par_info[best_parent].cyc_fit);

  for (cnt1 = 0; cnt1 < par_info[best_parent].num_neurons; cnt1++)
    {
    fprintf(best,"%3d ", cnt1);
    if (conf.n_model == 4)
      {
      for(cnt2 = 2; cnt2 < 7; cnt2++)
        fprintf(best,"%3d ", pheno[cnt1][cnt2]);

      for(cnt2 = 7; cnt2 < 11; cnt2++)
        fprintf(best,"%3d ",((unsigned char) pheno[cnt1][cnt2]));
      }

    if (conf.n_model == 2)
      {
      for(cnt2 = 2; cnt2 < 7; cnt2++)
        fprintf(best,"%3d ",pheno[cnt1][cnt2]);

      for(cnt2 = 7; cnt2 < 11; cnt2++)
        fprintf(best,"%3d ",((unsigned char) pheno[cnt1][cnt2]));

      fprintf(best,"%3d ", pheno[cnt1][11]);
      }
    fprintf(best,"\n");
    }
  for(cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
    fprintf(best, "%3d %3d\n",cnt2, out_neuron[cnt2]);
  } /* stop <if float > */
} /* END OF  save_best */
