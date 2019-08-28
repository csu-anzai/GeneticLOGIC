/**********************************************************************
* Program: GANNET.c         Version .91
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
*      GANNET is a program designed to evolve neural networks to solve
* pattern recognition problems. The program can be easily adapted to
* solve other types of problems. Neural networks are evolved using a
* genetic algorithm. A more complete description of this program can be
* found with this program in "GANNET.doc"
*
* for further information contact:
*             Professor Kenneth J. Hintz
*             Department of Electrical and Computer Engineering
*             George Mason University
*             4400 University Drive
*             Fairfax, VA 22030
*  
*             email:  khintz@gmuvax2.gmu.edu
*             tel:    (703)993-1592
*
* Written by: Captain Jason J. Spofford, USAF
*             ECE Graduate Student, George Mason University
* Written on:  9 March 1990
* Updated on:  9 Oct   1990
*
**********************************************************************/
/* The following include files are required */
#include <math.h>
#include <stdio.h>
#include "comdef.h"

/* The following external functions are required */

/* Source: best.c    */
extern int  start_best(char *, char *);
extern void stop_best ();
extern void save_best (int, int);

/* Source: config.c  */
extern int  load_configuration (char *);
extern void input_configuration(char *);
extern void set_default_configuration();
extern int  store_configuration(char *);

/* Source: gen_code.c */
extern int store_gen_code (int, int, struct genetic_code [1], FILE *);
extern int store_gen_quick(int, struct genetic_code *,
			   struct genetic_code [1], int);
extern int read_gen_code  (int, int, struct genetic_code [1], FILE *);
extern int read_gen_quick (int, struct genetic_code *,
			   struct genetic_code [1], int);
extern int create_gen_code(char *, char *, FILE **);
extern int open_gen_code  (char *, char *, FILE **);
extern int close_gen_code (FILE *);

/* Source: graycode.c */
extern int graycode  (register int);
extern int degraycode(register int);

/* Source: history.c */
extern int start_history(char *, char *);
extern int stop_history ();

/* Source: io_load.c */
extern int load_data(char *);
extern int load_inputs(char *);

/* Source: parinfo.c */
extern int get_par_info  (char *);
extern int store_par_info(char *);

/* Source: statistic.c */
extern int  start_stats(char *, char *);
extern void stop_stats ();
extern void take_stats (int, int);

/* The following external global variables are required  */
extern struct configuration conf; /* Source: config.c    */
extern FILE   *history;           /* Source: history.c   */
extern FILE   *stats;             /* Source: statistic.c */
extern FILE   *best;              /* Source: best.c      */

/* The following functions are defined */
void   evolve_loop(char *);
int    resize(int, int);
int    sort_parents();
void   children_to_parents(char *);
int    load_all_code(char *);
void   input_noise(char *);
int    dump_all_code(char *);
void   reproduce(int, int *);
void   mutate(int, int);
int    initialize(char *);
int    grab_memory();
int    close_up_shop(char *);
int    create_parents(char *);
int    deviation();
float  random();
double drand();
float  evaluate(short int, float *, float *, float *, short int *,
		short int *);
void   geno_to_pheno(struct genetic_code [1], int);
double drand48();

/* Global file pointers required */
FILE *parents;
FILE *children;

/* Global variables required */
struct genetic_code parent[2], child[2];
struct genetic_code *all_parents, *all_children, *temp_hold;
struct net_info *par_info, *chd_info;

short unsigned int combo4index[COMBO4]; /* encoded output behavior values */
char combo4[COMBO4][INPUT_THRES4];      /* weights and threshold values   */
char combo2[COMBO2][INPUT_THRES2];      /* weights and thresholds values  */
char out_neuron[NUM_OUTPUTS];           /* output neuron numbers          */
char pheno[MAX_NEURON][N_INPUTS * 2 + 4]; /* evaluation phenotype         */
char inputs[IO_SET][NUM_INPUTS];        /* input data array               */
char outputs[IO_SET][NUM_OUTPUTS];      /* output data array              */
int  sum_out_neuron[MAX_NEURON];        /* stats on output neurons        */

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

conf.save_memory = FALSE;

/* get the experiment extension name  */
if (argc < 2 || argc > 3)
  {
  printf("Use one or two arguments, the experiment name\n");
  printf("and the memory option -s for saving RAM.\n");
  exit(0);
  }

/* check to see if the -s option was given */
if (argc == 3)
  {
  if ( !strcmp(argv[2],"-s") )
    {
    conf.save_memory = TRUE;
    printf("Save memory mode enabled.\n");
    }
  else
    {
    printf("Only the -s option is supported!\n");
    exit(0);
    }
  } /* stop <if argc> */

/* Initialize Files and Variables Prior to Execution */
if (!initialize(argv[1]) && error_code == SUCCESS)
  {
  printf("Fatal Error Initializing! - check history file.\n\n");
  error_code == ERROR;
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
float fit_sum;
float high_fit, next_high_fit = 1.0;
int   cnt, cnt2, cnt3;
int   seek, gen, evolving = TRUE;
int   partner[2], best_parent;
int   replacement, dude;
int   debug = 0;

/* Set the random seed for evolution */
gen = conf.cur_gen;
srand48(conf.rand_seed2);

fit_sum = 0;
for (cnt = 0; cnt < conf.pop_size; cnt++)
  fit_sum += par_info[cnt].fitness;

fprintf(history, "Starting at GENERATION %d\n", gen);
fflush(history);

/* Start the evolution process */
while (evolving)
  {
  seek = 0;
  if (debug) printf("Evolve loop - Generation %d\n", gen);  

  /* Reset stats on which neuron are outputs */
  for (cnt = 0; cnt < MAX_NEURON; cnt++)
    sum_out_neuron[cnt] = 0;

  best_parent   = sort_parents();
  high_fit      = next_high_fit;
  next_high_fit = par_info[best_parent].fitness;

  /* Reset stats on how many parents replace their children */
  replacement = 0;

  /* Add noise to the input, same for all individuals per generation */
  input_noise(extension);

  /* Generate one child for each select parent */
  for (cnt = 0; cnt < conf.pop_size; cnt++)
    {
    reproduce(cnt, partner);
    
    /* Two children are generated at a time */
    for (cnt3 = 0; cnt3 < 2; cnt3 ++)
      {
      mutate(cnt3, chd_info[cnt + cnt3].num_neurons);
      resize(cnt3, cnt + cnt3);

      geno_to_pheno(child + cnt3, chd_info[cnt + cnt3].num_neurons);

      chd_info[cnt + cnt3].fitness =evaluate(chd_info[cnt + cnt3].num_neurons,
	&chd_info[cnt + cnt3].io_fit, &chd_info[cnt + cnt3].cyc_fit,
	&chd_info[cnt + cnt3].nn_fit, &chd_info[cnt + cnt3].similar,
	&chd_info[cnt + cnt3].different);

      chd_info[cnt + cnt3].seek_pos = seek;

      if (debug) printf("Child %d, n%d is %f fit, Parent %d, n%d is %f fit.\n",
	cnt + cnt3, chd_info[cnt + cnt3].num_neurons,
	chd_info[cnt + cnt3].fitness, partner[cnt3], 
	par_info[partner[cnt3]].num_neurons, par_info[partner[cnt3]].fitness);
 
      /* Implement top heavy or parent / child replacement */
      if ( ((par_info[partner[0]].fitness > high_fit ||
	 par_info[partner[1]].fitness > high_fit) && (conf.top_heavy) &&
	 (par_info[partner[0]].fitness > chd_info[cnt+cnt3].fitness ||
	 par_info[partner[1]].fitness > chd_info[cnt+cnt3].fitness))  ||
	 (par_info[partner[cnt3]].fitness > chd_info[cnt + cnt3].fitness &&
	 (par_info[partner[cnt3]].age < conf.age || conf.age == 0)))
	{
	if (par_info[partner[0]].fitness > high_fit && conf.top_heavy)
	  dude = partner[0];
	else if (par_info[partner[1]].fitness > high_fit && conf.top_heavy)
	  dude = partner[1];
	else dude = partner[cnt3];

	if (debug) printf("Replacing child %d with Parent %d\n",
		     cnt + cnt3, dude);
	replacement++;
	chd_info[cnt + cnt3].fitness     = par_info[dude].fitness;
	chd_info[cnt + cnt3].io_fit      = par_info[dude].io_fit;
	chd_info[cnt + cnt3].cyc_fit     = par_info[dude].cyc_fit;
	chd_info[cnt + cnt3].nn_fit      = par_info[dude].nn_fit;
	chd_info[cnt + cnt3].age         = par_info[dude].age;
	chd_info[cnt + cnt3].bred        = par_info[dude].bred;
	chd_info[cnt + cnt3].num_neurons = par_info[dude].num_neurons;
	chd_info[cnt + cnt3].different   = par_info[dude].different;
	chd_info[cnt + cnt3].similar     = par_info[dude].similar;
	
	if (conf.save_memory == TRUE)
	  read_gen_code(par_info[dude].num_neurons,
	  par_info[dude].seek_pos, child + cnt3, parents);
	else
	  read_gen_quick(dude, all_parents, child + cnt3, 
	    par_info[dude].num_neurons);
	geno_to_pheno(child + cnt3, chd_info[cnt + cnt3].num_neurons);
	}     
      
      /* Store children to the hard disk drive if optioned */
      if (conf.save_memory == TRUE)
	store_gen_code(chd_info[cnt + cnt3].num_neurons,
	  chd_info[cnt + cnt3].seek_pos, child + cnt3, children);
      else 
	store_gen_quick((cnt + cnt3), all_children, child + cnt3,
	  chd_info[cnt + cnt3].num_neurons);

      seek += BYTES_PER_NEURON * 2 * chd_info[cnt + cnt3].num_neurons;
      
      /* Collect stats on which neurons are outputs */
      for (cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
	sum_out_neuron[out_neuron[cnt2]]++;

      if (cnt3 == 1) cnt++;
      } /* stop <for cnt3> */
    } /* stop <for cnt> */

  save_best(gen, best_parent);
  take_stats(replacement, gen);

  /* Should we stop evolving? (have we reached performance?) */
  if (conf.max_gen == 0)
    {
    fit_sum = 0;
    for (cnt = 0; cnt < conf.pop_size; cnt++)
      fit_sum += par_info[cnt].fitness;
    if ( (fit_sum / (float) conf.pop_size) > conf.min_fit)
    evolving=FALSE;
    }

  children_to_parents(extension);

  gen++;
  /* Should we stop evolving? (have we reached required # of generations?)*/
  if (gen >= (conf.cur_gen + conf.max_gen) && conf.max_gen != 0)
    evolving=FALSE;

  } /* stop while evolving */

/* Record the last generation we completed */
conf.cur_gen=gen;
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


/*************************************************************************
* resize
*
*     This function subjects each child to a certain chance of mutation.
* This mutation operator either inserts or deletes a neuron from an
* individual.  The lower limit on deletions is 2 or the number of
* outputs, which ever is greater.  The upper limit on insertions is
* conf.max_neuron. Insertion simply duplicates a neuron genetic code.
*
* Inputs : chd_num - Which of the two brand new children are we looking at.
*          individual - That childs placement amoung all the children.
* Outputs: none.
*
* Global : chd_info - The child information array.
*          child    - The child's genetic code.
*          conf.resize,       conf.e_outputs
*          conf.max_neuron,   conf.e_inputs
*          conf.n_model
************************************************************************/

int resize(int chd_num, int individual)

{
unsigned char curr_conn;
int cnt1, cnt2;
int split, num_neurons;
int position, deletion;
int debug = 0;

if (random() < conf.resize)
  {
  num_neurons = chd_info[individual].num_neurons;
  deletion = 0;

  if ( random() < .5 && num_neurons > 2 && num_neurons > conf.e_outputs)
    deletion = -1;
  else if ( num_neurons < conf.max_neuron)
	 deletion = 1;

  if (deletion != 0)
    {
    /* Normalize all connections to preserve existing structure */
    position = (int) (random() * (float) num_neurons);
    for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
      for (cnt2 = 0; cnt2 < conf.n_model; cnt2++)
	{
	curr_conn = (unsigned char) 
	  degraycode( (int) child[chd_num].conn[cnt1][cnt2]);
	
	curr_conn = (unsigned char) ((float) curr_conn / (256.0 / (float)
	(num_neurons + conf.e_inputs)));

	if (((int) curr_conn - conf.e_inputs) > position)
	  curr_conn += deletion;

	child[chd_num].conn[cnt1][cnt2] = (char) graycode((int) ((float)
	  curr_conn * (256.0 / (float) (conf.e_inputs + num_neurons +
	  deletion)) + (128.0 / (float) (conf.e_inputs + num_neurons +
	  deletion))));
	}

    if (deletion == -1)
      {
      if (debug) printf("Resize: Neuron Deletion for %d\n",individual);
      /* Delete a neuron */
      for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
	if (position < cnt1)
	  {
	  child[chd_num].bid[cnt1 - 1] = child[chd_num].bid[cnt1];        
	  child[chd_num].combo[cnt1 - 1] = child[chd_num].combo[cnt1];
	  for (cnt2 = 0; cnt2 < conf.n_model; cnt2++)
	    child[chd_num].conn[cnt1 - 1][cnt2] =
	      child[chd_num].conn[cnt1][cnt2];
	  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
	    child[chd_num].cross[cnt1 - 1][cnt2] =
	      child[chd_num].cross[cnt1][cnt2];
	  }

      chd_info[individual].num_neurons--;
      } /* stop < if < .5>*/
    else
      {
      if (debug) printf("Resize: Neuron Insertion for %d\n",individual);
      /* Insert a duplicate neuron */
      for (cnt1 = num_neurons + 1; cnt1 > position; cnt1--)
	if (position < cnt1)
	  {
	  child[chd_num].bid[cnt1]   = child[chd_num].bid[cnt1 - 1];
	  child[chd_num].combo[cnt1] = child[chd_num].combo[cnt1 - 1];
	  for (cnt2 = 0; cnt2 < conf.n_model; cnt2++)
	    child[chd_num].conn[cnt1][cnt2] = 
	      child[chd_num].conn[cnt1 - 1][cnt2];
	  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
	    child[chd_num].cross[cnt1][cnt2] = 
	      child[chd_num].cross[cnt1 - 1][cnt2];
	  }
      chd_info[individual].num_neurons++;
      } /* stop < else >*/
    } /* stop < if deletion > */
  } /* stop < if random() < conf.resize > */

} /* END OF resize */

/*********************************************************************
* sort_parents
*
*     This function logically sorts the parents information array
* in order of fitness.  Then it selects which parents will bred 
* and how many mates they will have. Finally it returns a reference
* for the best performing parent. (In ties, only one will be chosen
* simply based on which the sort algorithm places at the top) The
* alogirithm for selection is from James Edward Baker's paper entitled,
* "Reducing Bias and Inefficiency in the Selection Algorithm," from the
* Second International Conference on Genetic Algorithms, July 1987.
* Formally, it is called "Stochastic Universal Sampling."
* 
* Inputs : none.
* Output : passes back the best performing parent reference.
*
* Global : par_info      - The parent information record array.
*          conf.scale    - The fitness pressure.
*          conf.pop_size - The population size for the experiment.
*
*********************************************************************/

int sort_parents()

{
int   cnt1, cnt2, loner;
int   min,  temp_hold;
short int   sort[MAX_POP];
float ptr,  sum, slope;
float min_select;
int   debug = 0;

/* Set all the pointers to par_info in numerical order */
for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
  sort[cnt1] = cnt1;

/* Now sort parents in increasing order of fitness */
for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
  {
  min = cnt1;
  for (cnt2 = cnt1 + 1; cnt2 < conf.pop_size; cnt2++)
    if (par_info[sort[cnt2]].fitness < par_info[sort[min]].fitness)
      min = cnt2;

  temp_hold  = sort[min];
  sort[min]  = sort[cnt1];
  sort[cnt1] = temp_hold;
  } /* stop <for cnt1> */

/* Now assign expected reproduction values */
ptr   = random();
sum   = 0.0;
slope = 2.0 * conf.scale / (float) (conf.pop_size - 1);
min_select = 1.0 - conf.scale;

for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
  for (sum += slope * (float) cnt1 + min_select; sum > ptr; ptr++)
    par_info[sort[cnt1]].select++;

if (debug)
  {
  printf("Sorted Population:\n");
  for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
    printf("fit = %f, %2d neurons, # %2d, rep %d\n",
      par_info[sort[cnt1]].fitness, par_info[sort[cnt1]].num_neurons,
      sort[cnt1], par_info[sort[cnt1]].select);
  }

/* Return the best performer */
return(sort[conf.pop_size-1]);

} /* END OF sort_parents */

/*********************************************************************
* children_to_parents
*
*     This function makes the formal transition of children to parents.
*  Any parents that are to continue to survive should of already been
*  placed into the children array before calling this function.
*
*  Input : extension - the experiment name.
*  Output: none.
*
*  Global: conf.save_memory
*          parents        - the parent file pointer.
*          children       - the children file pointer.
*          all_parents    - the parent array pointer.
*          all_children   - the children array pointer.
*          temp_hold      - a temporary array pointer.
*          par_info       - the parent information array.
*          chd_info       - the children information array.
*********************************************************************/

void children_to_parents(char *extension)

{
int  cnt;
char par_name[80];
char chd_name[80];
char rm_par_name[80];

/* close genetic code files */
if (conf.save_memory == TRUE)
  {
  close_gen_code(parents);
  close_gen_code(children);

  strcpy(par_name, "par_code.");
  strcpy(chd_name, "chd_code.");
  strcat(par_name, extension);
  strcat(chd_name, extension);
  strcpy(rm_par_name, "rm ");
  strcat(rm_par_name, par_name);

  system(rm_par_name);
  rename(chd_name, par_name);

  create_gen_code(extension, CHD_NAME, &children);
  open_gen_code(extension, PAR_NAME, &parents);
  }
else
  {
  temp_hold = all_parents;
  all_parents = all_children;
  all_children = temp_hold;
  }

for (cnt=0; cnt < conf.pop_size; cnt++)
  {
  par_info[cnt].bred        = 0;
  par_info[cnt].age         = chd_info[cnt].age + 1;
  par_info[cnt].select      = 0;
  par_info[cnt].num_neurons = chd_info[cnt].num_neurons;
  par_info[cnt].seek_pos    = chd_info[cnt].seek_pos;
  par_info[cnt].fitness     = chd_info[cnt].fitness;
  par_info[cnt].io_fit      = chd_info[cnt].io_fit;
  par_info[cnt].cyc_fit     = chd_info[cnt].cyc_fit;
  par_info[cnt].nn_fit      = chd_info[cnt].nn_fit;
  par_info[cnt].similar     = chd_info[cnt].similar;
  par_info[cnt].different   = chd_info[cnt].different;
  }

} /* END OF children_to_parents */

/**********************************************************************
* load_all_code
*
*     This function loads all the genetic code from the par_code file
* into a large array in RAM.
* 
* Input : extension - the experiment name.
* Output: passes back SUCCESS or ERROR
*
* Global: parents     - the parents file pointer.
*         all_parents - the parents array pointer.
*         conf.pop_size
*         par_info    - the parents information array pointer.
**********************************************************************/

int load_all_code(char *extension)

{
int bytes, value, cnt, cnt1, cnt2;
int error_code;

error_code = open_gen_code(extension, PAR_NAME, &parents);

if (error_code == SUCCESS)
  {
  for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
    {
    bytes = par_info[cnt1].num_neurons * BYTES_PER_NEURON * 2;
    fseek(parents, par_info[cnt1].seek_pos, 0);
    value = 0;
    for (cnt=0; cnt < par_info[cnt1].num_neurons; cnt++)
      {
      value = value + fread(&all_parents[cnt1].combo[cnt],1,2,parents);
      value = value + fread(&all_parents[cnt1].bid[cnt],1,2,parents);
      for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
	value = value + fread(&all_parents[cnt1].conn[cnt][cnt2],
	  1, 1, parents);
      for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
	value = value + fread(&all_parents[cnt1].cross[cnt][cnt2],
	  1, 1, parents);
      }

      if (value != bytes)
	{
	error_code = ERROR;
	break;
	}
    } /* stop <for cnt1> */
  close_gen_code(parents);
  } /* stop <if error_code> */
  
if (error_code == ERROR) fputs("Error loading all genetic code!\n", history);

return(error_code);

} /* END OF load_all_code */

/**********************************************************************
* dump_all_code
*
*     This function dumps all the genetic code to the par_code file
* from a large array in RAM.
* 
* Input : extension - the experiment name.
* Output: passes back SUCCESS or ERROR
*
* Global: parents     - the parents file pointer.
*         all_parents - the parents array pointer.
*         conf.pop_size
*         par_info    - the parents information array pointer.
**********************************************************************/

int dump_all_code(char *extension)

{
int bytes, value, cnt, cnt1, cnt2;
int error_code;

error_code = create_gen_code(extension, PAR_NAME, &parents);

if (error_code == SUCCESS)
  {
  for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
    {
    bytes = par_info[cnt1].num_neurons * BYTES_PER_NEURON * 2;
    fseek(parents, par_info[cnt1].seek_pos, 0);
    value = 0;
    for (cnt=0; cnt < par_info[cnt1].num_neurons; cnt++)
      {
      value = value + fwrite(&all_parents[cnt1].combo[cnt],1,2,parents);
      value = value + fwrite(&all_parents[cnt1].bid[cnt],1, 2,parents);
      for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
	value = value + fwrite(&all_parents[cnt1].conn[cnt][cnt2],
	  1, 1, parents);
      for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
	value = value + fwrite(&all_parents[cnt1].cross[cnt][cnt2],
	  1, 1, parents);
      }

      if (value != bytes)
	{
	error_code = ERROR;
	break;
	}
    } /* stop <for cnt1> */
  close_gen_code(parents);
  } /* stop <if error_code> */
  
if (error_code == ERROR) fputs("Error dumping all genetic code!\n", history);

return(error_code);

} /* END OF dump_all_code */

/**********************************************************************
* reproduce
*
*     This function picks two parents and via crossover, creates two
* chldren.
*
* Input : chd_num - The child number to start storing the new children.
* Output: partner - Returns a pointer to the two parents involved.
*
* Global: conf.pop_size
*         par_info       - the parent information array pointer.
*         conf.save_memory
*         parent         - two sets of parent genetic code.
*         all_parents    - the parent genetic code array.
*         parents        - the parent genetic code file pointer.
*         chd_info       - the children information array pointer.
*         child          - two sets of children genetic code.
**********************************************************************/

void reproduce(int chd_num, int *partner)

{
static double base;
static int cross_pt = -1;
double rand_value;
int cnt1, cnt2, cnt3;
int choice, direction;
int n_count[2], n_length, cross_zone;
unsigned short int combo_bitmask, bid_bitmask;
unsigned char bitmask, parent_bitmask, cross_cuts, start_neuron;
unsigned char start_output, start;
int low_range, high_range, loops;
int keep_looping, long_parent;
int debug = 0;

if (cross_pt == -1 && conf.norm_cross == TRUE)
  {
  base = log(1.0 - (double) conf.cross);
  while ((rand_value = drand()) == 0);
  cross_pt += (int) ((log(rand_value) / base) + 1.0); 
  }

/* Pick first parent randomly based on fitness */
choice = (int) (random() * (float) conf.pop_size);

if (par_info[choice].select == 0)
  {
  direction = 1;
  if (random() < 0.5) direction = -1;

  while (par_info[choice].select == 0)
    {
    choice += direction;
    if (choice < 0) choice = conf.pop_size - 1;
    if (choice >= conf.pop_size) choice = 0;
    }

  } /* stop < if select == 0 > */

par_info[choice].select--;
par_info[choice].bred++;
partner[0] = choice;

/* Pick second parent of the same net size or close */
choice     = (int) (random() * (float) conf.pop_size);
low_range  = par_info[partner[0]].num_neurons;
high_range = low_range;

if (par_info[choice].select == 0 ||
  low_range  > par_info[choice].num_neurons ||
  high_range < par_info[choice].num_neurons)
  {
  direction = 1;
  if (random() < 0.5) direction = -1;

  keep_looping = TRUE;    

  while (keep_looping)
    {
    loops = 0;

    while ((par_info[choice].select == 0 ||
      low_range > par_info[choice].num_neurons ||
      high_range < par_info[choice].num_neurons) && loops <= conf.pop_size) 
      {
      loops++;
      choice += direction;
      if (choice < 0) choice = conf.pop_size - 1;
      if (choice >= conf.pop_size) choice = 0;
      }

    if (!(par_info[choice].select == 0 ||
      low_range  > par_info[choice].num_neurons ||
      high_range < par_info[choice].num_neurons)) keep_looping = FALSE;

    low_range--;
    high_range++;
    if (low_range < 1) low_range = 1;
    if (high_range > (conf.max_neuron + 1))
      high_range = conf.max_neuron + 1;
 
    } /* stop < while keep_looping > */
  } /* stop < if select == 0 > */

par_info[choice].select--;
par_info[choice].bred++;
partner[1] = choice;

for (cnt1 = 0; cnt1 < 2; cnt1++)
  {
  choice = partner[cnt1];
  if (conf.save_memory == TRUE)
    read_gen_code(par_info[choice].num_neurons, par_info[choice].seek_pos,
      parent+cnt1, parents);
  else
    read_gen_quick(choice, all_parents, parent+cnt1,
      par_info[choice].num_neurons);

  if (debug)
    {
    printf("pick_parent %d at %f, %d neurons\n",
      choice,par_info[choice].fitness, par_info[choice].num_neurons); 
    geno_to_pheno(parent + cnt1, par_info[choice].num_neurons);  
    }
  } /* stop <for cnt1> */

n_count[0] = par_info[partner[0]].num_neurons;
n_count[1] = par_info[partner[1]].num_neurons;

  if (n_count[0] >= n_count[1])
    {
    long_parent = 0;
    n_length    = n_count[0];
    cross_zone  = n_count[1];
    }
  else
    {
    long_parent = 1;
    n_length    = n_count[1];
    cross_zone  = n_count[0];
    }

start_neuron = 0;
start_output = 0;

for (cnt2 = 0; cnt2 < n_length; cnt2++)
  {
  if (cnt2 < cross_zone)
    {
    for (cnt1 = 0; cnt1 < BYTES_PER_NEURON; cnt1++)
      {
      /* Determine all the crossover points */
      if (conf.norm_cross == FALSE)
	cross_cuts = parent[0].cross[cnt2][cnt1] | parent[1].cross[cnt2][cnt1];
      else
	{
	cross_cuts = 0;
	while (cross_pt < 8)
	  {
	  bitmask = 1;
	  bitmask <<= (7 - cross_pt);
	  cross_cuts |= bitmask;

	  while ((rand_value = drand()) == 0);
	  cross_pt += (int) ((log(rand_value) / base) + 1.0);        
	  }
	cross_pt -= 8;
	}

      if (cnt1 < (BYTES_PER_NEURON - 2)) start = start_neuron;
      else start = start_output;

      /* Now Construct bitmask */
      parent_bitmask = 0;

      if (cross_cuts == 0 && start == 1)
	parent_bitmask = ~parent_bitmask;

      if (cross_cuts != 0)
	for (cnt3 = 0; cnt3 < 8; cnt3++)
	  {
	  bitmask = (1 << (7 - cnt3));
	  bitmask &= cross_cuts;
	  parent_bitmask = (parent_bitmask << 1) | start;
	  if (bitmask > 0) start = (start + 1) & 1;
	  }

      if (cnt1 < (BYTES_PER_NEURON - 2)) start_neuron = start;
      else start_output = start;

      /* Conduct Crossover for Behavior combination */
      if (cnt1 < 2)
	if (cnt1 == 1)
	  {
	  combo_bitmask = 0;
	  combo_bitmask = ((combo_bitmask | (short) parent_bitmask) << 8);
	  }
	else
	  {
	  combo_bitmask |= (short) parent_bitmask;
	  child[0].combo[cnt2] = (parent[1].combo[cnt2] & combo_bitmask) | 
	    (parent[0].combo[cnt2] & (~combo_bitmask));

	  child[1].combo[cnt2] = (parent[0].combo[cnt2] & combo_bitmask) |
	    (parent[1].combo[cnt2] & (~combo_bitmask));
	  }
	 
      /* Cross Over connections */
      if (cnt1 >= 2 && cnt1 < 6)
	{
	child[0].conn[cnt2][cnt1 - 2] = (parent[1].conn[cnt2][cnt1 - 2] &
	  parent_bitmask) | (parent[0].conn[cnt2][cnt1 - 2] & 
	  (~parent_bitmask));

	child[1].conn[cnt2][cnt1 - 2] = (parent[0].conn[cnt2][cnt1 - 2] &
	  parent_bitmask) | (parent[1].conn[cnt2][cnt1 - 2] & 
	  (~parent_bitmask));
	}

      /* Cross Over Outputs */
      if (cnt1 >= BYTES_PER_NEURON - 2)
	if (cnt1 == BYTES_PER_NEURON -2)
	  {
	  bid_bitmask = 0;
	  bid_bitmask = ((bid_bitmask | (short) parent_bitmask) << 8);
	  }
	else
	  {
	  bid_bitmask |= (short) parent_bitmask;

	  child[0].bid[cnt2] = (parent[1].bid[cnt2] & bid_bitmask) | 
	    (parent[0].bid[cnt2] & (~bid_bitmask));

	  child[1].bid[cnt2] = (parent[0].bid[cnt2] & bid_bitmask) |
	    (parent[1].bid[cnt2] & (~bid_bitmask));
	  }

      /* Cross over cross over! */
      child[0].cross[cnt2][cnt1] = (parent[1].cross[cnt2][cnt1] &
	parent_bitmask) | (parent[0].cross[cnt2][cnt1] & (~parent_bitmask));

      child[1].cross[cnt2][cnt1] = (parent[0].cross[cnt2][cnt1] &
	parent_bitmask) | (parent[1].cross[cnt2][cnt1] & (~parent_bitmask));

      }  /* stop <for cnt1 > */

    } /* stop < if cross_zone > */
  else 
    {
    if (cnt2 == cross_zone)
      start_neuron = (start_neuron + 1) & 1;
  
    child[start_neuron].combo[cnt2] = parent[long_parent].combo[cnt2];
    child[start_neuron].bid[cnt2]   = parent[long_parent].bid[cnt2];

    for (cnt3 = 0; cnt3 < N_INPUTS; cnt3++)
      child[start_neuron].conn[cnt2][cnt3] = 
	parent[long_parent].conn[cnt2][cnt3];

    for (cnt3 = 0; cnt3 < BYTES_PER_NEURON; cnt3++)
      child[start_neuron].cross[cnt2][cnt3] = 
	parent[long_parent].cross[cnt2][cnt3];

    }  /* stop <else> */

  } /* stop < for cnt2 >*/

chd_info[chd_num + start_neuron].num_neurons  = n_count[long_parent];

start_neuron = (start_neuron + 1) & 1;
long_parent  = (long_parent + 1)  & 1;
chd_info[chd_num + start_neuron].num_neurons  = n_count[long_parent];

if (debug)
  printf("%d, %d Children got %d, %d neurons\n", chd_num, chd_num + 1,
    chd_info[chd_num].num_neurons, chd_info[chd_num + 1].num_neurons);

chd_info[chd_num].bred = chd_info[chd_num + 1].bred = 0;
chd_info[chd_num].age  = chd_info[chd_num + 1].age  = 0;

} /* END OF reproduce */

/**********************************************************************
* mutate
*
*     This function mutates a child given the probability of mutation.
* Every bit is subjected to a uniform random chance on whether or not it
* will be mutated. The mutation rate is currently fixed over the length
* of the experiment.
*
* Input : child_num   - the number of the child to mutate (0 or 1).
*         num_neurons - the number of neurons that child has.
* Output: none.
*
* Global: conf.mutate_b   - the behavior mutation rate.
*         conf.mutate_o   - the output mutation rate.
*         conf.mutate_c   - the connection mutation rate.
*         conf.mutate_s   - the crossover mutation rate.
*         conf.n_model    - the neuron model.
*         child           - the child's genetic code.
**********************************************************************/

void mutate(int child_num, int num_neurons)

{
static   int    mutate_pt_b = -1;
static   int    mutate_pt_o;
static   int    mutate_pt_c;
static   int    mutate_pt_s;
static   double base_b, base_o, base_c, base_s;
unsigned short  int bitmask1;
unsigned char   bitmask3;
int    gene, cnt3, bits_neuron;
int    bit_position, total_bits;
double rand_value;
int debug = 0;

/* Set the initial mutation point if calling the first time */
if (mutate_pt_b == -1)
  {
  base_b = log(1.0 - (double) conf.mutate_b);
  base_o = log(1.0 - (double) conf.mutate_o);
  base_c = log(1.0 - (double) conf.mutate_c);
  base_s = log(1.0 - (double) conf.mutate_s);

  /* Ensure we don't get a zero for a random value */
  while ((rand_value = drand()) == 0);
  mutate_pt_b += (int) ((log(rand_value) / base_b) + 1.0);

  while ((rand_value = drand()) == 0);
  mutate_pt_o += (int) ((log(rand_value) / base_o) + 1.0);

  while ((rand_value = drand()) == 0);
  mutate_pt_c += (int) ((log(rand_value) / base_c) + 1.0);

  while ((rand_value = drand()) == 0);
  mutate_pt_s += (int) ((log(rand_value) / base_s) + 1.0);
  }

if (conf.n_model == 4) bits_neuron = 16;
if (conf.n_model == 2) bits_neuron = 4;

total_bits = num_neurons * bits_neuron;

/* Cause a behavior mutation one at a time */
while ( mutate_pt_b < total_bits )
  {
  /* Point to the right neuron */
  gene = mutate_pt_b / bits_neuron;
  bit_position = gene * bits_neuron;

  /* Cause the mutation using XOR */
  if (debug) printf("Beh. mutation was %d, ",child[child_num].combo[gene]);  
  if (conf.mutate_bit == TRUE)
    {
    bitmask1 = 1;
    bitmask1 <<= (mutate_pt_b - bit_position);
    child[child_num].combo[gene] ^= bitmask1;
    }
  else
    child[child_num].combo[gene] = (short int) (random() * 65536.0);

  if (debug) printf("now %d\n",child[child_num].combo[gene]);

  /* calculate the next mutation point */
  while ((rand_value = drand()) == 0);
  mutate_pt_b += (int) ((log(rand_value) / base_b) + 1.0);

  } /* stop <while mutate_pt_b> */

/* Get mutate_pt_b set for the next child */
mutate_pt_b -= total_bits;

/* The total number of output bits of code in the child */
total_bits =  16 * num_neurons;

/* Cause an output mutation one at a time */
while ( mutate_pt_o < total_bits )
  {
  /* Point to the right neuron */
  gene = mutate_pt_o / 16;
  bit_position = gene * 16;

  /* Cause the mutation using XOR */
  if (debug) printf("Output mutation was %d, ",child[child_num].bid[gene]);  
  if (conf.mutate_bit == TRUE)
    {
    bitmask1 = 1;
    bitmask1 <<= (mutate_pt_o - bit_position);
    child[child_num].bid[gene] ^= bitmask1;
    }
  else
    child[child_num].bid[gene] = (short int) (65536.0 * random());

  if (debug) printf("now %d\n",child[child_num].bid[gene]);  

  /* calculate the next mutation point */
  while ((rand_value = drand()) == 0);
  mutate_pt_o += (int) ((log(rand_value) / base_o) + 1.0);

  } /* stop <while mutate_pt_o> */

/* Get mutate_pt_o set for the next child */
mutate_pt_o -= total_bits;

/* The total number of connection bits of code in the child */
total_bits =  8 * conf.n_model * num_neurons;

/* Cause each mutation one at a time */
while ( mutate_pt_c < total_bits )
  {
  /* Point to the right neuron */
  gene = mutate_pt_c / (8 * conf.n_model);
  bit_position = gene * 8 * conf.n_model;

  /* Cause the mutation using XOR */
  for (cnt3 = 0; cnt3 < conf.n_model; cnt3++)
    {
    if ((bit_position <= mutate_pt_c) && (mutate_pt_c < (bit_position + 8)))
      {
      if (debug) printf("Conn mutation was %d, ",
	child[child_num].conn[gene][cnt3]);  
      if (conf.mutate_bit == TRUE)
	{
	bitmask3 = 1;
	bitmask3 <<= (mutate_pt_c - bit_position);
	child[child_num].conn[gene][cnt3] ^= bitmask3;
	}
      else
	child[child_num].conn[gene][cnt3] = (char) (random() * 256.0);        
  
      if (debug)
	printf("now %d\n", child[child_num].conn[gene][cnt3]);
      }
    bit_position += 8;
    }    
  
  /* calculate the next mutation point */
  while ((rand_value = drand()) == 0);
  mutate_pt_c += (int) ((log(rand_value) / base_c) + 1.0);

  } /* stop <while mutate_pt_c> */

/* Get mutate_pt_c set for the next child */
mutate_pt_c -= total_bits;

/* The total number of connection bits of code in the child */
total_bits =  8 * BYTES_PER_NEURON * num_neurons;

/* Cause each mutation one at a time */
while ( mutate_pt_s < total_bits )
  {
  /* Point to the right neuron */
  gene = mutate_pt_s / (8 * BYTES_PER_NEURON);
  bit_position = gene * 8 * BYTES_PER_NEURON;

  /* Cause the mutation using XOR */
  for (cnt3 = 0; cnt3 < BYTES_PER_NEURON; cnt3++)
    {
    if ((bit_position <= mutate_pt_s) && (mutate_pt_s < (bit_position + 8)))
      {
      if (debug) printf("Cross mutation was %d, ",
	child[child_num].cross[gene][cnt3]);  
      bitmask3 = 1;
      bitmask3 <<= (mutate_pt_s - bit_position);
      child[child_num].cross[gene][cnt3] ^= bitmask3;

      if (debug)
	printf("now %d\n",child[child_num].cross[gene][cnt3]);
      }
    bit_position += 8;
    }    
  
  /* calculate the next mutation point */
  while ((rand_value = drand()) == 0);
  mutate_pt_s += (int) ((log(rand_value) / base_s) + 1.0);

  } /* stop <while mutate_pt_s> */

/* Get mutate_pt_s set for the next child */
mutate_pt_s -= total_bits;

} /* END of mutate */
 
/**********************************************************************
* Initialize
* 
*     This procedure prepares files and program variables prior to the
* execution of the evolutionary processes.
*
* Input : extension - the experiment name.
* Output: returns an error code of SUCCESS or ERROR.
*
* Global: parinfo - The parent information array.
*         parents - The genetic code array for all parents.
*         parent  - Holds the genetic code of one parent.
*         history - The history file pointer.
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
if (found_conf) strcpy(mode, APPEND);
else strcpy(mode, CREATE);

if (!start_history(extension, mode)) return(ERROR);

if (found_conf) fputs("Configuration information loaded.\n",history);
else
  {
  input_configuration(extension);
  if (!store_configuration(extension)) return(ERROR);
  }

if (!grab_memory()) return(ERROR);

if (!load_data(extension)) return(ERROR);

if (!start_stats(extension, mode)) return(ERROR);

if (!start_best(extension, mode)) return(ERROR);

fflush(history);
if (get_par_info(extension))
  {
  if (!open_gen_code(extension, PAR_NAME, &parents)) return(ERROR);
  }
else
  {
  if (!create_parents(extension)) return(ERROR);

  if (!open_gen_code(extension, PAR_NAME, &parents)) return(ERROR);
 
  for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
    {
    read_gen_code(par_info[cnt1].num_neurons,
      par_info[cnt1].seek_pos, parent, parents);
    geno_to_pheno(parent, par_info[cnt1].num_neurons);

    for (cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
      sum_out_neuron[out_neuron[cnt2]]++;

    par_info[cnt1].fitness =
      evaluate(par_info[cnt1].num_neurons,
	&par_info[cnt1].io_fit, &par_info[cnt1].cyc_fit,
	&par_info[cnt1].nn_fit, &par_info[cnt1].similar,
	&par_info[cnt1].different);
    } /* stop <for cnt1> */

  if (!store_par_info(extension)) return(ERROR);
  } /* stop <else found_par_info> */

if (!close_gen_code(parents)) return(ERROR);

if (conf.save_memory == TRUE)
  {
  if (!create_gen_code(extension, CHD_NAME, &children)) return(ERROR);
  if (!open_gen_code  (extension, PAR_NAME, &parents))  return(ERROR);
  }
else if (!load_all_code(extension)) return(ERROR);

return(SUCCESS);
} /* END OF initialize */

/**********************************************************************
* grab_memory
*
*      This function grabs memory for the storage of large arrays.
* The largest array is all_parents and all_children.  If the option
* "-s" for slow mode is used, memory is not allocated to all_parents
* and all_children. Instead, that information is read/ written to 
* the hard drive.  On other arrays, no option is given and ERROR is
* returned if the memory can not be allocated.
*
* Inputs : none.
* Outputs: returns SUCCESS or ERROR.
*
* Global : conf.save_memory - If FALSE, allocate memory.
*          genetic_code     - structure reference only.
*          all_parents      - pointer to the parent genetic code array.
*          all_children     - pointer to the children genetic code array.
*          history          - pointer to the history file.
*          chd_info         - the child information array pointer.
*          par_info         - the parent information array pointer.
***********************************************************************/

int grab_memory()

{
int error_code = SUCCESS;

chd_info = (struct net_info *) calloc(conf.pop_size,
  sizeof(struct net_info));

par_info = (struct net_info *) calloc(conf.pop_size,
  sizeof(struct net_info));

if (chd_info == NULL || par_info == NULL) 
  {
  error_code = ERROR;
  fputs("Unable to allocate memory for par_info and chd_info!\n",
	history);
  }

/* Attempt to grab memory for all_parents and all_children */
if (conf.save_memory == FALSE)
  {
  all_parents = (struct genetic_code *) calloc(conf.pop_size,
    sizeof(struct genetic_code));

  all_children = (struct genetic_code *) calloc(conf.pop_size,
    sizeof(struct genetic_code));

  /* Free up memory if unable to grab it all */
  if (all_parents == NULL || all_children == NULL)
    {
    if (all_parents  == NULL && all_children != NULL) free(all_children);
    if (all_children == NULL && all_parents  != NULL) free(all_parents);

    /* Enable the slow mode so the experiment can continue */
    conf.save_memory  = TRUE;
    fputs("Unable to allocate memory required!\n", history);
    } /* stop < if all_parents > */
  else
    fputs("High speed mode - memory allocated.\n", history);

  } /* stop < if save_memory > */
else
 fputs("Slow speed mode\n", history);

/* return error_code */
return(error_code);

} /* END OF grab_memory */

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
if (conf.save_memory == TRUE)
  {
  close_gen_code(parents);
  close_gen_code(children);
  }
else dump_all_code(extension);

fprintf(history, "Stopping at GENERATION %d\n", conf.cur_gen);

stop_stats();
store_par_info(extension);
store_configuration(extension);
stop_best();
stop_history();
} /* END OF close_up_shop */

/**********************************************************************
* Create_parents
*
*     This procedure creates a par_info array and creates a par_code
* file. Parents are randomly created for the first generation.
*
* Input : extension - the experiment name.
* Output: passes back SUCCESS or ERROR.
*
* Global: par_info        - the parent information array.
*         conf.rand_seed1 - the random seed for creating parents.
*         conf.cross      - cross over rate
*         conf.pop_size
*         parent          - two sets of genetic code for parents.
*         parents         - the parents genetic code file pointer.
*         history         - the history file pointer.
***********************************************************************/

int create_parents(char *extension)

{
unsigned char bitmask;
int      cross_pt = -1;
int      cnt1, cnt2, cnt3, seek;
int      error_code = SUCCESS;
double   base, rand_value;

base = log(1.0 - (double) conf.cross);

/* Seed the random generator */
srand48( conf.rand_seed1 );

while ((rand_value = drand()) == 0);
cross_pt += (int) ((log(rand_value) / base) + 1.0);        

seek = 0;
if ( create_gen_code(extension, PAR_NAME, &parents) )
  {
  /* Ensure all fitness categories cleared */
  for (cnt1 = 0; cnt1 < conf.pop_size; cnt1++)
    {
    par_info[cnt1].io_fit      = 0.0;
    par_info[cnt1].cyc_fit     = 0.0;
    par_info[cnt1].nn_fit      = 0.0;
    par_info[cnt1].fitness     = 0.0;
    par_info[cnt1].bred        = NOT_BRED;
    par_info[cnt1].age         = 1;
    par_info[cnt1].select      = 0;
    par_info[cnt1].num_neurons = deviation();
    par_info[cnt1].seek_pos    = seek;

    for (cnt2 = 0; cnt2 < par_info[cnt1].num_neurons; cnt2++)
      {
      parent[0].combo[cnt2] = random() * 65536; 
      parent[0].bid[cnt2]   = random() * 65536; 

      for (cnt3 = 0; cnt3 < N_INPUTS; cnt3++)
	parent[0].conn[cnt2][cnt3] = random() * 256;

      for (cnt3 = 0; cnt3 < BYTES_PER_NEURON; cnt3++)
	{
	parent[0].cross[cnt2][cnt3] = 0;

	while (cross_pt < 8)
	  {
	  bitmask = 1;
	  bitmask <<= (7 - cross_pt);
	  parent[0].cross[cnt2][cnt3] |= bitmask;

	  while ((rand_value = drand()) == 0);
	  cross_pt += (int) ((log(rand_value) / base) + 1.0);        
	  }
	cross_pt -= 8;
	} /* stop <for cnt3> */

      } /* stop <for cnt2> */

    store_gen_code(par_info[cnt1].num_neurons, par_info[cnt1].seek_pos,
      parent,parents);
    seek = seek + BYTES_PER_NEURON * 2 * par_info[cnt1].num_neurons;

    } /* stop <for cnt1> */
  fputs("Created random parents\n", history);
  error_code = close_gen_code(parents);
  } /* stop <if create ...> */

else error_code=ERROR;

return(error_code);
} /* END OF create_parents */

/************************************************************************
* Deviation
*
*    This procedure creates deviations from the expected mean genetic
* code length.
*
* Input : none.
* Output: passes back the neural network size.
*
* Global: conf.deviation   - the +/- deviation from the mean.
*         conf.mean_neuron - the mean neuron size.
*************************************************************************/

int deviation()
{
int net_size;

net_size = (int) (( (float) conf.deviation * 2.0 + 1.0) * random());
net_size -= conf.deviation;
net_size += conf.mean_neuron;

return(net_size);
} /* END OF deviation */

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
int    stable_out_total;
int    cycles,  eval, cnt1,       cnt2, cnt3, sum;
int    flip,    first_cycle,      second_cycle;
int    conn,    unstable_flag,    stable_out;
int    dataset, max_similar,      similar;
int    max_different, different,  same, difference;
int    unstable[NUM_OUTPUTS];
int    debug1 = 0, debug2 = 0;

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
	  }
	else if (out_neuron[cnt3]==cnt2)
	  {
	  unstable[cnt3] = TRUE;
	  stable_out     = 0;
	  }
	} /* stop <for cnt3> */
      } /* stop <if pheno> */
    } /* stop <for cnt2> */

  if (!unstable_flag && cycles <= first_cycle && cycles > 1) stable_out++;

  if (flip == 0)
    {
    eval = FALSE; /* If we have no flips - solution stable */
    if (cycles <= first_cycle && cycles > 1)
      stable_out = first_cycle - cycles;
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

*n_fitness  = (float) (conf.max_neuron - num_neurons)
	    / (float) conf.max_neuron;

fitness = conf.io_fit * (*io_fitness) + conf.neuron_fit * (*n_fitness) 
	  + conf.cycle_fit * (*cyc_fitness);

if (debug2) printf("fit= %f, n_fit= %f, io_fit= %f, cyc_fit %f\n",
	 fitness,*n_fitness,*io_fitness,*cyc_fitness);

return(fitness);
} /* END OF evaluate */

/**********************************************************************
* geno_to_pheno
*
*      This function converts the genetic code to the phenotype
* representation.  The translation from genotype to phenotype has a
* strong affect on the performance of the GA.
*
* Inputs : gen_ptr     - pointer to a genetic code representation.
*          num_neurons - the number of neurons that representation has.
* Outputs: none. 
*
* Global : pheno          - the phenotype representation array.
*          conf.e_outputs - the environmental outputs
*          conf.e_inputs  - the environmental inputs
*          conf.biased    - which behavior genetic code model.
*          conf.n_model   - the neuron model (2 or 4 inputs)
**********************************************************************/

void geno_to_pheno(struct genetic_code gen_ptr[1], int num_neurons)

{
unsigned short int points[NUM_OUTPUTS], output_pt;
unsigned short int scores[NUM_OUTPUTS], score;
unsigned int bitmask1;
short unsigned int behav;
unsigned char conn;
int not_found, gap;
int cnt1, cnt2, index;
int debug = 0;

if (debug)
  {
  printf("Genetic code\n");
  printf(" N#   BID    BEH   C1  C2  C3  C4  CRS: BEH C1  C2  C3  C4  BID\n");
  for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
    {
    printf("%2d--> %5d %5d ",cnt1,gen_ptr[0].bid[cnt1],gen_ptr[0].combo[cnt1]);
    for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
      printf("%3d ",gen_ptr[0].conn[cnt1][cnt2]);
    for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
      printf("%3d ",gen_ptr[0].cross[cnt1][cnt2]);
    printf("\n");
    } 
  } /* stop < if debug > */

/* Initialize variables */
for (cnt1 = 0; cnt1 < conf.e_outputs; cnt1++)
  {
  out_neuron[cnt1] = cnt1;
  points[cnt1] = (unsigned short) (cnt1 * ( 65536 / conf.e_outputs));
  scores[cnt1] = 65535; 
  }

for(cnt1 = 0; cnt1 < num_neurons; cnt1++)
  {

  if (conf.out_fixed == FALSE)
    {
    output_pt = (short int) degraycode( (int) gen_ptr[0].bid[cnt1]);
    for(cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
      {
      if (points[cnt2] > output_pt) score = points[cnt2] - output_pt;
      else score = output_pt - points[cnt2];

      if (score > 32767)
	score = (65535 - score) + 1;

      if (score < scores[cnt2])
	{
	out_neuron[cnt2] = cnt1;
	scores[cnt2] = score;
	}
      } /* stop <for cnt2> */
    } /* stop < if conf.out_fixed > */

  if (conf.n_model == 4)
    {
    if (conf.biased == TRUE)
      {
      gap       = COMBO4 / 2;
      index     = 0;
      not_found = TRUE;
      behav     = (short int) graycode( (int) gen_ptr[0].combo[cnt1]);
      
      while (not_found)
	{
	if (combo4index[index] > behav) index -= gap;
	if (combo4index[index] < behav) index += gap;
	gap /= 2;
	if (gap < 1) gap = 1;

	if (combo4index[index] == behav)
	  not_found = FALSE;
	
	else if (combo4index[index] > behav && combo4index[index - 1] < behav)
	  {
	  if ((combo4index[index] - behav) > 
	     (behav - combo4index[index - 1]))
	    index--;
	  not_found = FALSE;
	  }
	
	else if (combo4index[index + 1] > behav && combo4index[index] < behav)
	  {
	  if ((combo4index[index + 1] - behav) < 
	     (behav - combo4index[index]))
	    index++;
	  not_found = FALSE;
	  }
	} /* stop < while not_found > */
      } /* stop < if biased > */
    else
      {
      index = (int) ((float) (
       degraycode( 
	((unsigned int) gen_ptr[0].combo[cnt1]))) / 34.822529);
      }
    } /* stop <if 4 input> */

  if (conf.n_model == 2)
    {
    bitmask1 = ~0;
    bitmask1 <<= 4;
    bitmask1 = ~bitmask1;
    index = bitmask1 & (int) gen_ptr[0].combo[cnt1];
    }
  
  for (cnt2 = 3; cnt2 < N_LIMIT; cnt2++)
    {  
    if (conf.n_model == 4) pheno[cnt1][cnt2] = combo4[index][cnt2 - 3];
    else if (conf.n_model == 2)
      {
      if ( (cnt2 - 3) < 2) pheno[cnt1][cnt2] = combo2[index][cnt2 - 3];
      else pheno[cnt1][cnt2] = 0;
      }

    conn = (unsigned char) degraycode((int) gen_ptr[0].conn[cnt1][cnt2 - 3]); 

    pheno[cnt1][cnt2 + 4] = (char) ((float) conn / ( 256.0 / 
      (float) (num_neurons + conf.e_inputs)));

    } /* stop <for cnt2> */

  pheno[cnt1][0] = 0;
  pheno[cnt1][1] = 0;

  /* Set the threshold levels */
  if (conf.n_model == 4) pheno[cnt1][2] = combo4[index][4];
  else if (conf.n_model == 2)
    {
    pheno[cnt1][2]  = combo2[index][2];
    pheno[cnt1][11] = combo2[index][3];
    }
  } /* stop <for cnt1> */

if (debug)
  {
  printf("Phenotype\n");
  printf(" Th   W1  W2  W3  W4  C1  C2  C3  C4  T2");
  for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
    {
    printf("\n");
    for(cnt2 = 2; cnt2 < N_INPUTS * 2 + 4; cnt2++)
      printf(" %2d,",pheno[cnt1][cnt2]);
    }
  for(cnt2 = 0; cnt2 < conf.e_outputs; cnt2++)
    printf("\nOut %d -> %d", cnt2, out_neuron[cnt2]);
  printf("\n");
  }

} /* END OF geno_to_pheno */

