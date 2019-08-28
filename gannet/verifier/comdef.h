/******************************************************************
* Source: comdef.h         Version 1.0
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
*    This header file contains the common definitions used throughout
* many of the source code programs. 
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
*
* Written on: 18 March 1990
* Updated on: 10 Aug   1990
********************************************************************/

/* The following definitions are required */
#define APPEND          "a"
#define BITS_PER_NEURON 36
#define BYTES_PER_NEURON 8 
#define CHD_NAME        "chd_code."
#define CREATE          "w"
#define PAR_NAME        "par_code."
#define SUCCESS         1
#define ERROR           0
#define TRUE            1
#define FALSE           0
#define MAX_NEURON      250   /* The maximum neural networks allowed   */
#define NUM_INPUTS      100   /* The maximum number of inputs allowed  */
#define IO_SET          50    /* The number of input to output sets    */
#define NUM_OUTPUTS     100   /* The maximum number of outputs allowed */
#define MAX_POP         500   /* The maximum allowable population size */
#define N_INPUTS        4     /* The number of inputs each neuron has  */
#define NOT_BRED        0     /* The value of not breding              */
#define INPUT_THRES4    5     /* The # of weights & threshold values   */
#define INPUT_THRES2    4     /* The # of weights & threshold values   */
#define COMBO4          1882  /* The # of unique combinations- 4 input */
#define COMBO2          16    /* The # of unique combinations- 2 input */
#define MAX_AGE         100   /* The longest time a net can live       */
#define DEBUG           0     /* Turns on all the screen printing      */
#define N_LIMIT         7     /* N_INPUTS + 3 - pheno type array size  */

/* Platform dependant definitions */
#ifdef i386 /* Sun 386i unix 4.0.2 */
#define WR_BINARY    "wb+"
#define READ_BINARY  "rb"
#define WRITE_BINARY "wb"
#else  /* Assume we are on the VAX using ultrix */ 
#define WR_BINARY    "w+"
#define READ_BINARY  "r"
#define WRITE_BINARY "w"
#endif

/* The following structures are defined */

struct configuration
  {
  int pop_size;             /* Population Size                          */
  int max_gen;              /* Maximum Generations, 0 if no limit       */
  int cur_gen;              /* The current generation number            */
  int rand_seed1;           /* The random generator seed for 1st gen.   */
  int rand_seed2;           /* The random generator seed for evolution  */
  int max_neuron;           /* Maximum # of neurons ever allowed        */
  int mean_neuron;          /* Mean number of neurons in the init popul.*/
  int deviation;            /* The deviation of net size in init popul. */
  int e_outputs;            /* The number of outputs to the Environment */
  int e_inputs;             /* The number of inputs from the Environment*/
  int io_sets;              /* The number of output sets in test data   */
  int stats;                /* The statistic sample interval            */
  int best;                 /* The best neural net sample interval      */
  int age;                  /* The maximum age a neural network can live*/
  int save_memory;          /* Use a "RAM" disk or use the actual disk  */
  int top_heavy;            /* Whether the top perf. get more children  */
  int n_model;              /* Neuron model  should be 2 or 4.          */
  int biased;               /* Biased model or not (for 4 input only)   */
  int clear_state;          /* Clear the neural net state each time     */
  int free_rep;             /* Allow net to use free data representation*/
  int norm_cross;           /* Normal Crossover or Genetic crossover    */
  int mutate_bit;           /* Use "word" mutations or bit mutations    */
  int out_fixed;            /* Use fixed outputs or not                 */
  float scale;              /* Amplifies fitness differences            */
  float stable_per;         /* How long to check for stability of eval  */
  float conv_per;           /* How long to allow the eval to take place */
  float io_fit;             /* Importance of input to output fitness    */
  float neuron_fit;         /* Importance of reducing number of neurons */
  float cycle_fit;          /* Importance of solution convergence time  */
  float min_fit;            /* Don't stop evolving until reached        */
  float cross;              /* The prob. of crossover for behavior      */
  float mutate_b;           /* The probability of mutation per bit      */
  float mutate_o;           /* The prob. of output mutation             */
  float mutate_c;           /* The prob. of connection mutation         */
  float mutate_s;           /* The prob. of a crossover matrix mutation */
  float sim_dif;            /* The relative importance of simil. & diff.*/
  float resize;             /* The probability of resizing a net        */
  float inp_noise;          /* Input Noise                              */
  };

struct net_info
{
float fitness;              /* overall fitness       */
float io_fit;               /* input/output fitness  */
float nn_fit;               /* neuron number fitness */
float cyc_fit;              /* cycle fitness         */
short int age;              /* age of individual     */
short int num_neurons;      /* number of neurons     */
short int bred;             /* number of times bred  */
short int select;           /* times selected to bred in a generation */
short int different;        /* actual difference award */
short int similar;          /* actual similarity award */
int seek_pos;               /* where the genetic code is located      */
};

struct genetic_code
  {
  unsigned short int bid[MAX_NEURON];       /* output neuron bid          */
  unsigned short int combo[MAX_NEURON];     /* neuron behavior            */
  unsigned char conn[MAX_NEURON][N_INPUTS]; /* where the inputs come from */
  unsigned char cross[MAX_NEURON][BYTES_PER_NEURON];
};






















