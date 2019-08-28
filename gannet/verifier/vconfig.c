
/********************************************************************
* Source: config.c         Version 1.0
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
*    This source file contains all the functions required to enter,
* store, change and set the default value for all configuration
* information.
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
* Written on: 25 March 1990
* Updated on: 10 Aug   1990
*
********************************************************************/
/* The following include files are required */
#include <stdio.h>
#include "comdef.h"

/* external variables required */

/* The following global variables are defined */
struct configuration conf;

/* The following functions are defined */
int   load_configuration(char *);
int   get_conf_int(FILE *);
float get_conf_flt(FILE *);
void  input_configuration(char *);
int   input_int(int, int, int, char *);
float input_flt(float, float, float, char *);
void  set_default_configuration();
int   store_configuration(char *);

/***************************************************************
* load_configuration
*
* Purpose:
*	This procedure loads in the configuration information from
* conf.extension.  The configuration file contains an experiment's
* parameters. The configuration file is a text file.
*
* Input : extension - the experiment name.
* Output: An error code (SUCCESS or ERROR) is returned.
*
* Global: conf - structure containing all configuration data.
****************************************************************/ 

load_configuration(char *extension)

{
FILE *conf_file;           /* file pointer                   */
char conf_name[80];        /* the configuration file name    */
char conf_string[28];      /* used to skip text information  */
int  error_code = SUCCESS; /* set to either SUCCESS or ERROR */

/* Set up file name */
strcpy(conf_name, "./conf.");
strcat(conf_name, extension);

/* Open configuration file for read only */
conf_file = fopen(conf_name, "r");

if (conf_file != NULL)
  {
  /* File was opened successfully. */
  fgets(conf_string, 21, conf_file);    /* skip text info          */
  fscanf(conf_file, "%s", conf_string); /* ignore experiment name  */
  fgets(conf_string, 2, conf_file);     /* skip return             */
  /* Read in all the configuration information */
  conf.pop_size   = get_conf_int(conf_file);
  conf.max_gen    = get_conf_int(conf_file);
  conf.cur_gen    = get_conf_int(conf_file);
  conf.rand_seed1 = get_conf_int(conf_file);
  conf.rand_seed2 = get_conf_int(conf_file);
  conf.max_neuron = get_conf_int(conf_file);
  conf.mean_neuron= get_conf_int(conf_file);
  conf.deviation  = get_conf_int(conf_file);
  conf.e_outputs  = get_conf_int(conf_file);
  conf.e_inputs   = get_conf_int(conf_file);
  conf.io_sets    = get_conf_int(conf_file);
  conf.stats      = get_conf_int(conf_file);
  conf.best       = get_conf_int(conf_file);
  conf.age        = get_conf_int(conf_file);
  conf.top_heavy  = get_conf_int(conf_file);
  conf.n_model    = get_conf_int(conf_file);
  conf.biased     = get_conf_int(conf_file);
  conf.clear_state= get_conf_int(conf_file);
  conf.free_rep   = get_conf_int(conf_file);
  conf.norm_cross = get_conf_int(conf_file);
  conf.mutate_bit = get_conf_int(conf_file);
  conf.out_fixed  = get_conf_int(conf_file);
  conf.scale      = get_conf_flt(conf_file);
  conf.stable_per = get_conf_flt(conf_file);
  conf.conv_per   = get_conf_flt(conf_file);
  conf.io_fit     = get_conf_flt(conf_file);
  conf.neuron_fit = get_conf_flt(conf_file);
  conf.cycle_fit  = get_conf_flt(conf_file);
  conf.min_fit    = get_conf_flt(conf_file);
  conf.cross      = get_conf_flt(conf_file);
  conf.mutate_b   = get_conf_flt(conf_file);
  conf.mutate_o   = get_conf_flt(conf_file);
  conf.mutate_c   = get_conf_flt(conf_file);
  conf.mutate_s   = get_conf_flt(conf_file);
  conf.sim_dif    = get_conf_flt(conf_file);
  conf.resize     = get_conf_flt(conf_file);
  conf.inp_noise  = get_conf_flt(conf_file);
   
  fclose(conf_file);  
  }
else error_code = ERROR;

return(error_code);
} /* END OF load_configuration */

/***************************************************************
* get_conf_int
*
*     This procedure gets an integer from the configuration file.
* It skips over the text portion and extracts the value.
*
* inputs : file_ptr - The conf_file pointer.
* outputs: The integer value is returned.
*
* Global: none.
*****************************************************************/

get_conf_int(FILE *file_ptr)

{
char conf_string[28]; /* used to skip text information */
int  valueint;        /* the integer value extracted   */

fgets(conf_string, 21, file_ptr);  /* skip text             */
fscanf(file_ptr,"%d", &valueint);  /* extract integer value */
fgets(conf_string, 2, file_ptr);   /* skip return           */

return(valueint); /* pass back integer value */
} /* END OF get_conf_int */

/***************************************************************
* get_conf_flt
*
*     This procedure gets a floating point value from the
* configuration file. It skips over the text portion and extracts 
* the value.
*
* inputs : file_ptr - The conf_file pointer.
* outputs: The floating point value is returned.
*
* Global: none.
****************************************************************/

float get_conf_flt(FILE *file_ptr)

{
char  conf_string[28]; /* used to skip text information      */
float valueflt = 0.0;   /* the floating point value extracted */

fgets (conf_string, 21,   file_ptr);  /* skip text               */
fscanf(file_ptr,    "%f", &valueflt); /* extract float value     */
fgets (conf_string, 2,    file_ptr);  /* skip return             */

return(valueflt);
} /* END OF get_conf_flt */

/***************************************************************
* Input_configuration
*
*	This procedure prompts the user to provide configuration
* information. Pressing return results in default values.
*
* Input : extension - the experiment name.
* Output: none.
*
* Global: conf - structure containing all configuration data.
****************************************************************/

void input_configuration(char *extension)

{
int   invalid = TRUE;
float sum;
printf("**** EVOLVE ****\n\n");
printf("Set Configuration File for : %s\n\n",extension);

while( invalid )
  {
  conf.pop_size = input_int(2,MAX_POP,conf.pop_size,"Population Size");
  if ( ((float) conf.pop_size / 2.0) == (float) (conf.pop_size / 2))
    invalid = FALSE;
  else
    printf("*** Even population sizes only ***\n");
  } /* stop < while invalid > */

conf.max_gen    =input_int(0,-1,conf.max_gen,"Max Generations");
conf.cur_gen    =input_int(1,0,conf.cur_gen,"Curr Generation");
conf.rand_seed1 =input_int(0,0,conf.rand_seed1,"Random Seed 1");
conf.rand_seed2 =input_int(0,0,conf.rand_seed2,"Random Seed 2");
conf.max_neuron =input_int(1,MAX_NEURON,conf.max_neuron,"Max # of Neurons");
conf.mean_neuron=input_int(2,MAX_NEURON,conf.mean_neuron,"Mean # of Neurons");
conf.deviation  =input_int(0,conf.mean_neuron-2,conf.deviation,
                   "+/- Deviation");
conf.e_outputs  =input_int(1,conf.mean_neuron,conf.e_outputs,
                   "Environ. Outputs");
conf.e_inputs   =input_int(1,NUM_INPUTS,conf.e_inputs,"Environ.  Inputs");
conf.io_sets    =input_int(1,IO_SET,conf.io_sets,"# of I/O Sets");
conf.stats      =input_int(1,0,conf.stats,"Stats Interval");
conf.best       =input_int(1,0,conf.best,"Best Net Interval");
conf.age        =input_int(0,MAX_AGE,conf.age,"Maximum Age");
conf.top_heavy  =input_int(0,1,conf.top_heavy,"Top Heavy");
conf.n_model    =input_int(2,4,conf.n_model,"Neuron Model");
conf.biased     =input_int(0,1,conf.biased,"Biased Model");
conf.clear_state=input_int(0,1,conf.clear_state,"Clear Net State");
conf.free_rep   =input_int(0,1,conf.free_rep,"Net Free Repr.");
conf.norm_cross =input_int(0,1,conf.norm_cross,"Normal Crossover");
conf.mutate_bit =input_int(0,1,conf.mutate_bit,"Mutate Bit");
conf.out_fixed  =input_int(0,1,conf.out_fixed,"Fixed Outputs");
conf.scale      =input_flt(0.0,1.0,conf.scale,"Fitness Scale");
conf.stable_per =input_flt(0.0,1000.0,conf.stable_per,"Stable Test Time");
conf.conv_per   =input_flt(0.0,1000.0,conf.conv_per,"Conver. Test Time");

invalid = TRUE;
while( invalid )
  {
  conf.io_fit     =input_flt(0.0,1.0,conf.io_fit,"Input / Output %");
  conf.neuron_fit =input_flt(0.0,1.0,conf.neuron_fit,"Reduce Neuron %");
  conf.cycle_fit  =input_flt(0.0,1.0,conf.cycle_fit,"Reduce Cycle %");
  sum = conf.io_fit + conf.neuron_fit + conf.cycle_fit;
  if ( sum == 1.0 )
    invalid = FALSE;
  else
    printf("*** The last three values must sum to one ***\n");
  } /* stop < while invalid > */

conf.min_fit    =input_flt(0.0,1.0,conf.min_fit,"Minimum Fitness");
conf.cross      =input_flt(0.000001,0.5,conf.cross,"Crossover Rate");
conf.mutate_b   =input_flt(0.000001,0.5,conf.mutate_b,"Behavior Mutation");
conf.mutate_o   =input_flt(0.000001,0.5,conf.mutate_o,"Output Mutation");
conf.mutate_c   =input_flt(0.000001,0.5,conf.mutate_c,"Connect Mutation");
conf.mutate_s   =input_flt(0.000001,0.5,conf.mutate_s,"Cross Mutation");
conf.sim_dif    =input_flt(0.0,10000.0,conf.sim_dif,"Sim/Dif Strength");
conf.resize     =input_flt(0.0,1.0,conf.resize,"Prob of Resize");
conf.inp_noise  =input_flt(0.0,1.0,conf.inp_noise,"Input Noise");

/* Record the fact that configuration info was entered as history */

} /* END OF input_configuration */

/*******************************************************************
* input_int
*
*     This procedure asks for the user to input a configuration
* integer value.  If the user presses return, he will get the default
* value in the []'s.
*
* Input : lower - the lower allowable value.
*         upper - the upper allowable value.
*         default_value - the default value.
*         prompt - the text prompt.
*
* Output: returns the new integer value.
*
* Global: none.
******************************************************************/

input_int(int lower, int upper, int default_value, char *prompt)

{
int  invalid = TRUE;  /* Is the users input invalid?   */
int  input_value;     /* The actual input value        */
char string[20];      /* Users actually input a string */

while (invalid)    /* Keep asking the user for a valid answer */
  {

  /* Set the input value equal to the default value */
  input_value = default_value;
  printf("%17s = [%3d] : ", prompt, input_value);
  fflush(stdout);
  sscanf(gets(string),"%d",&input_value);

  /* If the user provided an out of range value .. complain */
  if (lower > upper)  /* If this is true, there is no upper bound */
    if (input_value >= lower) invalid = FALSE;
    else printf("*** must be greater than %d ***\n",lower);
  else
    if (lower == upper) invalid = FALSE; /* There is no range restrictions */
    else
      /* There are upper and lower range restrictions */
      if (input_value >= lower && input_value <= upper) invalid = FALSE;
      else printf("*** %d to %d only ***\n", lower, upper);

  } /* stop <while invalid> */

return(input_value);
} /* END OF input_int */

/*******************************************************************
* Input_flt
*
*     This procedure asks for the user to input a configuration
* float value.  If the user presses return, he will get the defualt
* value in the []'s.
*
* input : lower - the lower allowable value.
*         upper - the upper allowable value.
*         default_value - the default value.
* output: returns the new value.
*
* Global: none.
*******************************************************************/

float input_flt(float lower, float upper, float default_value, char *prompt)

{
int invalid = TRUE;  /* Is the user's input invalid? */
float input_value;   /* The actual input value       */
char string[20];     /* Used to capture the input    */

input_value = default_value;

while (invalid)
  {
  printf("%17s = [%f] : ", prompt, default_value);
  fflush(stdout);
  sscanf(gets(string),"%f", &input_value);
  if (lower > upper)
    if (input_value >= lower) invalid = FALSE;
    else printf("*** must be greater than %f ***\n", lower);
  else
    if (lower == upper) invalid = FALSE;
    else
      if (input_value >= lower && input_value <= upper) invalid = FALSE;
      else printf("*** %f to %f only ***\n", lower, upper);
  } /* stop <while invalid> */

return(input_value);
} /* END OF input_flt */

/****************************************************************
* Set_default_configuration
*
* Purpose:
*	This routine sets all the default configuration values.
*
* Input : none.
* Output: none.
*
* Global: conf - contains all the configuration information.
*
*****************************************************************/

void set_default_configuration()

{

conf.pop_size    = 50;
conf.max_gen     = 0;
conf.cur_gen     = 1;
conf.rand_seed1  = 123456789;
conf.rand_seed2  = 987654321;
conf.max_neuron  = 50;
conf.mean_neuron = 4;
conf.deviation   = 0;
conf.e_outputs   = 1;
conf.e_inputs    = 2;
conf.io_sets     = 4;
conf.stats       = 1;
conf.best        = 5;
conf.age         = 1;
conf.top_heavy   = 0;
conf.n_model     = 4;
conf.biased      = 0;
conf.clear_state = 1;
conf.free_rep    = 1;
conf.norm_cross  = 0;
conf.mutate_bit  = 1;
conf.out_fixed   = 0;
conf.scale       = 1.0;
conf.stable_per  = 1.0;
conf.conv_per    = 2.0;
conf.io_fit      = 1.0;
conf.neuron_fit  = 0.0;
conf.cycle_fit   = 0.0;
conf.min_fit     = 0.80;
conf.cross       = 0.01;
conf.mutate_b    = 0.008;
conf.mutate_o    = 0.002;
conf.mutate_c    = 0.002;
conf.mutate_s    = 0.002;
conf.sim_dif     = 1.00;
conf.resize      = 0.05;
conf.inp_noise   = 0.00;
} /* END OF set_default_configuration */

/****************************************************************
* Store_configuration
*
* Purpose:
*	This routine stores the configuration information in
* "conf.<extension>". 
*
* Input : extension - the experiment name.
* Output: An error code (SUCCESS or ERROR) is returned.
*
* Global: conf - structure containing all configuration data.
*****************************************************************/

store_configuration(char *extension)

{
FILE *conf_file;
char conf_name[80];

/* Set up file name */
strcpy(conf_name, "./conf.");
strcat(conf_name, extension);

/* Open configuration file for write only */
conf_file = fopen(conf_name, "w");
if (conf_file)
  {
  fprintf(conf_file,"       Experiment : %s\n",extension);
  fprintf(conf_file,"  Population Size = %d\n",conf.pop_size);
  fprintf(conf_file,"  Max Generations = %d\n",conf.max_gen);
  fprintf(conf_file,"  Curr Generation = %d\n",conf.cur_gen);
  fprintf(conf_file,"    Random Seed 1 = %d\n",conf.rand_seed1);
  fprintf(conf_file,"    Random Seed 2 = %d\n",conf.rand_seed2);
  fprintf(conf_file," Max # of Neurons = %d\n",conf.max_neuron);
  fprintf(conf_file,"Mean # of Neurons = %d\n",conf.mean_neuron);
  fprintf(conf_file,"    +/- Deviation = %d\n",conf.deviation);
  fprintf(conf_file," Environ. Outputs = %d\n",conf.e_outputs);
  fprintf(conf_file," Environ.  Inputs = %d\n",conf.e_inputs);
  fprintf(conf_file,"    # of I/O Sets = %d\n",conf.io_sets);
  fprintf(conf_file,"   Stats Interval = %d\n",conf.stats);
  fprintf(conf_file,"Best Net Interval = %d\n",conf.best);
  fprintf(conf_file,"      Maximum Age = %d\n",conf.age);
  fprintf(conf_file,"        Top Heavy = %d\n",conf.top_heavy);
  fprintf(conf_file,"     Neuron Model = %d\n",conf.n_model);
  fprintf(conf_file,"     Biased Model = %d\n",conf.biased);
  fprintf(conf_file,"  Clear Net State = %d\n",conf.clear_state);
  fprintf(conf_file,"   Net Free Repr. = %d\n",conf.free_rep);
  fprintf(conf_file," Normal Crossover = %d\n",conf.norm_cross);
  fprintf(conf_file,"       Mutate Bit = %d\n",conf.mutate_bit);
  fprintf(conf_file,"    Fixed Outputs = %d\n",conf.out_fixed);
  fprintf(conf_file,"    Fitness Scale = %f\n",conf.scale);
  fprintf(conf_file," Stable Test Time = %f\n",conf.stable_per);
  fprintf(conf_file,"Conver. Test Time = %f\n",conf.conv_per);
  fprintf(conf_file,"  Input / Output  = %f\n",conf.io_fit);
  fprintf(conf_file,"   Reduce Neuron  = %f\n",conf.neuron_fit);
  fprintf(conf_file,"   Reduce Cycles  = %f\n",conf.cycle_fit);
  fprintf(conf_file,"  Minimum Fitness = %f\n",conf.min_fit);
  fprintf(conf_file,"   Crossover Rate = %f\n",conf.cross);
  fprintf(conf_file,"Behavior Mutation = %f\n",conf.mutate_b);
  fprintf(conf_file,"  Output Mutation = %f\n",conf.mutate_o);
  fprintf(conf_file," Connect Mutation = %f\n",conf.mutate_c);
  fprintf(conf_file,"   Cross Mutation = %f\n",conf.mutate_s);
  fprintf(conf_file," Sim/Dif Strength = %f\n",conf.sim_dif);
  fprintf(conf_file,"   Prob of Resize = %f\n",conf.resize);
  fprintf(conf_file,"      Input Noise = %f\n",conf.inp_noise);

  fclose(conf_file);
  return(SUCCESS);
  }
else return(ERROR);

} /* END OF store_configuration */




