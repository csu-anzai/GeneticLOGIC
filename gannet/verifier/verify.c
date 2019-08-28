/**********************************************************************
* Program: verify.c         Version .1
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
* Verify is a simple tool to verify a best NN actual works, and
* shows how it works.
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
* Written on:  7 Sept 1990
* Updated on:  7 Sept 1990
*
**********************************************************************/
/* The following include files are required */
#include <math.h>
#include <stdio.h>
#include "comdef.h"

/* The following external functions are required */

/* Source: config.c  */
extern int  load_configuration (char *);
extern void set_default_configuration();

/* Source: io_load.c */
extern int load_data(char *);
extern int load_inputs(char *);
extern float get_conf_flt(FILE *);
extern int get_conf_int(FILE *);

/* The following external global variables are required  */
extern struct configuration conf; /* Source: config.c    */

/* The following functions are defined */
void   evolve_loop(char *);
void   input_noise(char *);
int    initialize(char *);
int    close_up_shop(char *);
float  random();
double drand();
float  evaluate(short int, float *, float *, float *, short int *,
                short int *);
double drand48();
FILE *history=NULL;

/* Global variables required */
struct net_info netinfo;

short unsigned int combo4index[COMBO4]; /* encoded output behavior values */
char combo4[COMBO4][INPUT_THRES4];      /* weights and threshold values   */
char combo2[COMBO2][INPUT_THRES2];      /* weights and thresholds values  */
char out_neuron[NUM_OUTPUTS];           /* output neuron numbers          */
char pheno[MAX_NEURON][N_INPUTS * 2 + 4]; /* evaluation phenotype         */
char inputs[IO_SET][NUM_INPUTS];        /* input data array               */
char outputs[IO_SET][NUM_OUTPUTS];      /* output data array              */

/**********************************************************************
* main
*
*     The main module controls the execution of the entire program.
* The "-s" option for the save memory option is recognized here. 
* 
* Input : argc - the number of arguments passed from the operating system.
*         argv - string representation of each argument.
*
* Global: conf.save_memory
* 
**********************************************************************/

main(int argc, char *argv[])

{
int error_code = SUCCESS;

/* get the experiment extension name  */
if (argc != 2)
  {
  printf("Use one argument, the experiment name\n");
  exit(0);
  }

/* Initialize Files and Variables Prior to Execution */
if (!initialize(argv[1]) && error_code == SUCCESS)
  {
  error_code == ERROR;
  printf("Error Initializing\n");
  }
else evolve_loop(argv[1]);

close_up_shop(argv[1]);
} /* END OF main */

/*******************************************************************
* evolve_loop
*
*     This function evolves the neural networks. When the evolving
* process is considered complete, the function returns control back
* to the main function.
*
* Input : extension - the experiment name.
* Output: none.
*
* Global: sum_out_neuron - stats on what neurons are outputs.
*         chd_info       - child information array.
*         par_info       - parent information array.
*         out_neuron     - actual output neuron numbers.
*         conf.pop_size,   conf.age
*         conf.top_heavy,  conf.save_memory
*         conf.cur_gen,    conf.max_gen
*         conf.min_fit,    conf.e_outputs
*         child - two children set of genetic code.
********************************************************************/

void evolve_loop(char *extension)

{
int   debug = 0;

/* Set the random seed for evolution */
srand48(conf.rand_seed2);

netinfo.fitness = evaluate(netinfo.num_neurons,
   &netinfo.io_fit, &netinfo.cyc_fit,
   &netinfo.nn_fit, &netinfo.similar,
   &netinfo.different);

printf("Number of Neurons    : %d\n",netinfo.num_neurons);
printf("Overall fitness      : %f\n",netinfo.fitness);
printf("Input/ Output fitness: %f\n",netinfo.io_fit);
printf("Neuron fitness       : %f\n",netinfo.nn_fit);
printf("Cycle fitness        : %f\n",netinfo.cyc_fit);
printf("Similarity           : %d\n",netinfo.similar);
printf("Different            : %d\n",netinfo.different);


} /* END OF evolve_loop */

/*************************************************************************
input_noise

     This function adds noise to the input data for each generation. Each
phenotype is tested against the same noisey data.

Input : extension - The experiment name.
Output: none.

Global: conf.inp_noise - The %chance of noise.
        inputs         - The actual input information.
        conf.e_inputs  - The number of environmental inputs.
        conf.io_sets   - The number of input / output sets.
***********************************************************************/

void input_noise(char *extension)

{
static int    noise_pt = -1;
static double base;
static char   input_copy[IO_SET][NUM_INPUTS];
int    cnt1,  cnt2;
double rand_value;

if (conf.inp_noise > 0.0)
  {

  if (noise_pt == -1)
    {
    base = log(1.0 - (double) conf.inp_noise);

    /* Ensure we don't get a zero for a random value */
    while ((rand_value = drand()) == 0);
    noise_pt += (int) ((log(rand_value) / base) + 1.0);
    
    for (cnt1 = 0; cnt1 < conf.io_sets; cnt1++)
      for (cnt2 = 0; cnt2 < conf.e_inputs; cnt2++)
        input_copy[cnt1][cnt2] = inputs[cnt1][cnt2];  
    }

  for (cnt1 = 0; cnt1 < conf.io_sets; cnt1++)
    for (cnt2 = 0; cnt2 < conf.e_inputs; cnt2++)
      {
      noise_pt--;
      if (noise_pt == 0)
        {
        if (input_copy[cnt1][cnt2] == 1) inputs[cnt1][cnt2] = -1;
        else inputs[cnt1][cnt2] = 1;
        while ((rand_value = drand()) == 0);
        noise_pt += (int) ((log(rand_value) / base) + 1.0);
        }
      else inputs[cnt1][cnt2] = input_copy[cnt1][cnt2];
      } /* stop < for cnt2 > */
  } /* stop < if inp_noise > */

} /* END OF input_noise */

/***********************************************************************
* Initialize
* 
*     This procedure prepares files and program variables prior to the
* execution of the evolutionary processes.
*
* Input : extension - the experiment name.
* Output: returns an error code of SUCCESS or ERROR.
*
* Global: parinfo - The parent information array.
*         conf.pop_size - The population size.       
*         conf.save_memory 
**********************************************************************/

int initialize(char *extension)

{
int  cnt1, cnt2;
int  found_conf;
char mode[4];

/* Set the default configuration */
set_default_configuration();

found_conf = load_configuration(extension);
if (!found_conf)
  {
  printf("Unable to find configuration file - exiting\n");
  return(ERROR);
  }

printf("Loading input/output/combo2 and combo4 data\n");
if (!load_data(extension)) return(ERROR);
printf("Loading sample data\n");
if (!load_best(extension)) return(ERROR);

return(SUCCESS);
} /* END OF initialize */

/**********************************************************************
* load_best
***********************************************************************/

load_best(char *extension)
{
FILE *sample;
char sample_name[80];
int error_code= SUCCESS;
int generation, inputs, outputs, model;
int cnt1,cnt2;
int value;

strcpy(sample_name,"./sample.");
strcat(sample_name, extension);

sample = fopen(sample_name,"r");

if (sample != NULL)
  {
  generation          = get_conf_int(sample);
  netinfo.age         = get_conf_int(sample);
  netinfo.bred        = get_conf_int(sample);
  netinfo.num_neurons = get_conf_int(sample);
  inputs              = get_conf_int(sample);
  outputs             = get_conf_int(sample);
  model               = get_conf_int(sample);
  netinfo.fitness     = get_conf_flt(sample);
  netinfo.io_fit      = get_conf_flt(sample);
  netinfo.nn_fit      = get_conf_flt(sample);
  netinfo.cyc_fit     = get_conf_flt(sample);
  
  for (cnt1 = 0; cnt1 < netinfo.num_neurons; cnt1++)
    {
    for (cnt2 = 0; cnt2 < 11; cnt2++)
      {
      pheno[cnt1][cnt2]=0;
      if (cnt2 == 1) fscanf(sample,"%d",&value);
      else if (cnt2 > 1)
        {
        fscanf(sample,"%d",&value);
        pheno[cnt1][cnt2] = value;
        }
    }
  }

  for (cnt1 = 0; cnt1 < conf.e_outputs; cnt1++)
    {    
    fscanf(sample," %d",&value);
    fscanf(sample," %d",&value);
    out_neuron[cnt1]=value;
  }
}
else return(ERROR);


printf("Best Analysis of Generation : %d\n",generation);
printf("Number of Neurons    : %d\n",netinfo.num_neurons);
printf("Overall fitness      : %f\n",netinfo.fitness);
printf("Input/ Output fitness: %f\n",netinfo.io_fit);
printf("Neuron fitness       : %f\n",netinfo.nn_fit);
printf("Cycle fitness        : %f\n",netinfo.io_fit);
printf("Phenotype :\n");
for (cnt1 = 0; cnt1 < netinfo.num_neurons; cnt1++)
  {
  printf("%4d",cnt1);
  for (cnt2 = 2; cnt2 < 11; cnt2++)
   printf("%4d",pheno[cnt1][cnt2]);
  printf("\n");
  } 

  for(cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
    printf("Out %d -> %d \n",cnt2,out_neuron[cnt2]);


return(SUCCESS);
}

/**********************************************************************
* close_up_shop
*
*     This procedure closes all files previously opened.  It also
* updates the configuration file with the last generation # run.
*
* Input:  extension - the experiment name.
* Output: none.
*
* Global: conf.save_memory 
*         conf.cur_gen
*         history - the history file pointer.
**********************************************************************/

close_up_shop(char *extension)

{
} /* END OF close_up_shop */

/************************************************************************
* random
*
*     Returns a random floating point value between 0 and 1.
*
* Input : none.
* Output: passes back a floating point random value.
*
* Global: none.
*************************************************************************/

float random()
{
float rand_var;

rand_var = (float) drand48();

return(rand_var);
} /* END OF random */

/***********************************************************************
* drand
*
*    Returns a random double floating point value between 0 and 1.
*
* Input : none.
* Output: passes back double floating point value.
*
* Global: none.
***********************************************************************/

double drand()
{
return(drand48());
}

/*******************************************************************
* evaluate
*
*     This function evaluates a neural network using a synchronous
* neural network evaluator.  Inputs are presented to a neural network
* and output performance, cycle performance and neural network size
* performance are all evaluated. 
*
* Input : num_neurons - the number of neurons in the net.
*         io_fitness  - input/ output fitness.
*         cyc_fitness - cycle fitness (how fast the neural net is)
*         n_fitness   - how large the neural net is.
* Output: similar_score - how many inputs are mapped to the same output.
*         difference_score - how many inputs map to different outputs.
*         passes back overall fitness.
*
* Global: conf.conv_per    - convergence period.
*         conf.stable_per  - stability test period.
*         conf.io_sets     - input / output data sets.
*         conf.n_model     - the neuron model (2 or 4 input).
*         conf.e_outputs   - the number of environmental outputs.
*         conf.e_inputs    - the number of environmental inputs.
*         conf.free_rep    - evaluate based on free output representation.
*         conf.clear_state - do we reset the net to a dead state or not.
*         conf.sim_dif     - how important similarity is over difference.
*         conf.io_fit      - how important input / output fitness is.
*         conf.neuron_fit  - how important neuron fitness is.
*         conf.cycle_fit   - how important cycle fitness is.
*         conf.max_neuron  - maximum number of neurons in a net.
*         outputs          - the output data set array.
*         inputs           - the input data set array.
*         pheno            - the phenotype array.
*         out_neuron       - the output neurons.
***********************************************************************/

float evaluate(short int num_neurons, float *io_fitness, float *cyc_fitness,
               float *n_fitness, short int *similar_score,
               short int *different_score)

{
char   results[IO_SET][NUM_OUTPUTS];
float  fitness, match;
int    cycles,  eval, cnt1,       cnt2, cnt3, sum;
int    flip,    first_cycle,      second_cycle;
int    conn,    unstable_flag,    stable_out;
int    dataset, max_similar,      similar;
int    max_different, different,  same, difference;
int    stable_out_total;
int    unstable[NUM_OUTPUTS];
int    debug1 = 1, debug2 = 1;

first_cycle = (int) ( (float) num_neurons * conf.conv_per);
second_cycle= (int) ( (float) num_neurons * conf.stable_per) + first_cycle;  

stable_out_total = 0;
match            = 0.0;

/* Test every input data set */
for (cnt1 = 0; cnt1 < conf.io_sets; cnt1++)
  {
  if (debug2)  printf("\nData Set # %d\n", cnt1); 
  cycles     = 0;     /* Number of cycles per data set required */
  eval       = TRUE;  /* Keep evaluating until FALSE            */
  stable_out = 0; 

  /* Assume every output position is stable */
  for (cnt3 = 0; cnt3 < conf.e_outputs; cnt3++)
    unstable[cnt3] = FALSE;

  /* Reset the network to the dead state */
  if (conf.clear_state == TRUE)
    for (cnt3 = 0; cnt3 < num_neurons; cnt3++)
      pheno[cnt3][0] = pheno[cnt3][1] = 0;

  /* Keep evaluating this data set until certain conditions are met */
  while (eval)
    {
    /* Reset the number of flips in each cycle */
    flip = 0; 

    /* Complete one cycle */
    for (cnt2 = 0; cnt2 < num_neurons; cnt2++)
      {
      /* Set the threshold */
      sum = -pheno[cnt2][2];

      if (conf.n_model == 2) sum = 0;

      for (cnt3 = 3; cnt3 < N_LIMIT; cnt3++) 
        {
        conn = (int) ((unsigned char) pheno[cnt2][cnt3 + 4]);
        if (conn < conf.e_inputs) /* Connected to an environment input */
          {
          sum += pheno[cnt2][cnt3] * inputs[cnt1][conn];
          }
        else /* Connected to another neuron */
	  {
          conn -= conf.e_inputs;
          sum  += pheno[cnt2][cnt3] * pheno[conn][0];
	  }
        } /* stop <for cnt3> */

      if (sum > 0) pheno[cnt2][1] = 1; /* assign the next output value  */
      else pheno[cnt2][1] = -1;

      if (conf.n_model == 2) 
        {
        if (sum > pheno[cnt2][2] && sum < pheno[cnt2][11])
          pheno[cnt2][1] = 1;
        else pheno[cnt2][1] = -1;
        }
      } /* stop <for cnt2> */

    /* Completed one cycle */
    cycles++;

   if (debug2)
     {
     printf(" N#  PS  NS    Cycle %d", cycles);
     for (cnt3 = 0; cnt3 < num_neurons; cnt3++)
       {
       printf("\n%4d",cnt3);
       for(cnt2 = 0; cnt2 < 2; cnt2++)
         printf("%4d",pheno[cnt3][cnt2]);
       }
      printf("\n");
      }

  /* Move next output into current output and record flips */
  unstable_flag = FALSE;

  for (cnt2 = 0; cnt2 < num_neurons; cnt2++)
    {
    if (pheno[cnt2][1] != pheno[cnt2][0])
      {
      flip++;
      pheno[cnt2][0] = pheno[cnt2][1]; /* current output equals next output */

      /* check to see if it is an output neuron */
      for (cnt3 = 0; cnt3 < conf.e_outputs; cnt3++)
        {
        if (out_neuron[cnt3]==cnt2 && cycles<=first_cycle)
          {
          unstable_flag = TRUE;
          stable_out    = 0;
          printf("Output unstable and under firstcycle\n");
          }
        else if (out_neuron[cnt3]==cnt2)
          {
          printf("Output unstable and over firstcycle\n");
          unstable[cnt3] = TRUE;
          stable_out     = 0;
          }
        } /* stop <for cnt3> */
      } /* stop <if pheno> */
    } /* stop <for cnt2> */

  if (!unstable_flag && (cycles <= first_cycle && cycles > 1))
    {
    printf("Stable out incremented \n");
    stable_out++;
  }

  if (flip == 0)
    {
    eval = FALSE; /* If we have no flips - solution stable */
    if (cycles <= first_cycle && cycles > 1)
      {
      stable_out = first_cycle - cycles;
      printf("Stable out set to first_cycle - cycles\n");
    }
    }

  if (cycles >= second_cycle) eval = FALSE;

    } /* stop <while eval> */
    /* Done evaluating a data set */

  /* Record output performance for latter analysis */
  for (cnt3 = 0; cnt3 < conf.e_outputs; cnt3++)
    {
    results[cnt1][cnt3] = pheno[out_neuron[cnt3]][0] * (!unstable[cnt3]);
    } 
  stable_out_total += stable_out;
  printf("Stable out %d & total %d\n",stable_out,stable_out_total);
  } /* stop <for cnt1> */

/* Perform output performance analysis */

if (conf.free_rep == FALSE)
  {
  /* Calculate how close the output is to the data set */
  for (dataset = 0; dataset < conf.io_sets; dataset++)
    for (cnt3 = 0; cnt3 < conf.e_outputs; cnt3++)
    {
    if (outputs[dataset][cnt3] - results[dataset][cnt3] == 0) match++;
    if (debug1) printf("dataset %d, result %d, output required %d \n",
       dataset, results[dataset][cnt3], outputs[dataset][cnt3]);
    }
  *io_fitness = match / (float) (conf.io_sets * conf.e_outputs);
  }
else
  {
  max_similar   = similar = 0;
  max_different = different = 0;

  for (dataset = 0; dataset < conf.io_sets - 1; dataset++)
    {
    for (cnt1 = dataset + 1; cnt1 < conf.io_sets; cnt1++)
      {
      /* Determine if they are similar or different */
      same = TRUE;
      for (cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
        if (outputs[dataset][cnt2] != outputs[cnt1][cnt2])
          {
          same = FALSE;
          break;
          }

        /* Record the best possible values for rating */
        difference = FALSE;
        if (same)  max_similar   += conf.e_outputs;
        if (!same) max_different += 1;
     
        for (cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
          if (results[dataset][cnt2] != 0 && results[cnt1][cnt2] != 0)
            if (results[dataset][cnt2] == results[cnt1][cnt2])
              {
             /* Award a point for each degree of similarity */
              if (same) similar++;
              }
            else if (!same) difference = TRUE;
    
        /* Award a point if different and it is supposed to be */
        if (difference) different++;
      } /* stop  < for cnt1 > */
    }  /* stop < for dataset > */

  /* Record points awarded */
  *different_score = (short int) different;
  *similar_score   = (short int) similar;

  if (debug1)
    printf("diff %d, sim %d, m_diff %d, m_sim %d\n",different,similar,
      max_different, max_similar);

  if (max_similar != 0)
    {
    match = (float) different + conf.sim_dif *
      ((float) similar * (float) max_different /  (float) max_similar);
    *io_fitness = ( match /  ((float) max_different
       + conf.sim_dif * (float) max_different ));
    }
  else
    {
    match = (float) different;
    *io_fitness = (match / (float) max_different);
    }
  } /* stop < else conf.free_rep > */

if (debug1)
    for (dataset = 0; dataset < conf.io_sets; dataset++)
      {
      printf("Data set %d,  ",dataset);
      for (cnt1 = 0; cnt1 < conf.e_outputs; cnt1++)
        printf("%2d ",results[dataset][cnt1]);
      printf("\n");
      } 


*cyc_fitness= 1.0 - (float) ((first_cycle - 1) * conf.io_sets 
              - stable_out_total) / (float) ((first_cycle-1) * conf.io_sets);
printf("First_cycle %d, conf.io_sets %d, stable_out_total %d\n",
        first_cycle, conf.io_sets, stable_out_total);

*n_fitness  = (float) (conf.max_neuron - num_neurons)
            / (float) conf.max_neuron;

fitness = conf.io_fit * (*io_fitness) + conf.neuron_fit * (*n_fitness) 
          + conf.cycle_fit * (*cyc_fitness);
if (debug2) printf("fit= %f, n_fit= %f, io_fit= %f, cyc_fit %f\n",
         fitness,*n_fitness,*io_fitness,*cyc_fitness);

return(fitness);
} /* END OF evaluate */

