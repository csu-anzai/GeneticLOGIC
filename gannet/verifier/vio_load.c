/**********************************************************************
* Source: io_load.c         Version 1.0
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
*     This source code provides four functions to load input and output
* data.  The input/output .extension files are not rigidly structured.
* Every 1 or 0 encountered is interpreted as input or output data.
* The two functions, load_inputs and load_outputs, access files
* output.extension and input.extension. The other two functions load
* in the unique output combinations for a two and four input neuron.
* A fifth function, load_data, just calls the other four and indicates
* any ERROR to the caller.
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
* Written on: 31 March 1990
* Updated on: 23 June  1990 
*
***********************************************************************/
/* The following include files are required */
#include <stdio.h>
#include "comdef.h"

/* The following external variables are required */
extern struct configuration conf;
extern char inputs[IO_SET][NUM_INPUTS];
extern char outputs[IO_SET][NUM_OUTPUTS];
extern char combo4[COMBO4][INPUT_THRES4];
extern char combo2[COMBO2][INPUT_THRES2];
extern short unsigned int combo4index[COMBO4];

/* The following functions are defined */
int load_data(char *);
int load_outputs(char *);
int load_inputs(char *);
int load_four_combo();
int load_two_combo();

/*********************************************************************
* load_data
*
*     This function loads in the input and output patterns and the
* unique output combinations for a two and four input neuron.
*
* Input : extension - the experiment name.
* Output: returns SUCCESS or ERROR.
*
* Global: none.
*********************************************************************/

int load_data(char *extension)

{
int error_code = 0;

error_code += load_inputs(extension);
error_code += load_outputs(extension);
error_code += load_four_combo();
error_code += load_two_combo();

if (error_code != 4) error_code = ERROR;
else error_code = SUCCESS;

return(error_code);
} /* END OF load_data */

/**********************************************************************
* load_outputs
*
*     This procedure loads the input.extension file into a global 
* array. The file is created using a text editor prior to running 
* evolveNET.
*
* Input : extension - the experiment name.
* Output: returns SUCCESS or ERROR.
*
* Global: conf.e_outputs      - the number of outputs from the environment.
*         conf.io_sets          - the number of output data sets.
*         outputs[set][output#] - the actual output test data.
**********************************************************************/

load_outputs(char *extension)

{
FILE *output_file;
int  chr, error_code = SUCCESS;
int  set, output_num;
char output_name[80];

/* Set up the file name */
strcpy(output_name, "./output.");
strcat(output_name, extension);

/* Open output.extension file for read only */
output_file = fopen(output_name, "r");

/* Read in the data if the file exists */
if (output_file)
  {
  /* Try to read enough 1's and 0's for each set of outputs */
  set = 0;
  output_num = 0;
  while (set < conf.io_sets && error_code == SUCCESS)
    {

    while (output_num < conf.e_outputs && error_code == SUCCESS)
      {

      chr = fgetc(output_file);
      if (chr == EOF)
        {
        error_code = ERROR;
        }

      if (chr == '0')
        {
        outputs[set][output_num] = -1;
        output_num++;
        }

      if (chr == '1') 
        {
        outputs[set][output_num] = 1;
        output_num++;
        }

      } /* stop <while input_num> */
    output_num = 0;
    set++;

    } /* stop <while set> */

  fclose(output_file);
  } /* stop <if output_file> */
else
  {
  error_code = ERROR;
  }

return(error_code);
} /* END OF load_outputs */

/**********************************************************************
* load_inputs
*
*     This procedure loads the input.extension file into a global 
* array. This file is created using a text editor prior to running 
* evolveNET.
*
* Input : extension - the experiment name.
* Output: returns SUCCESS or ERROR.
*
* Global: conf.e_inputs	    - the number of inputs from the environment.
*         conf.io_sets        - the number of input data sets.
*         inputs[set][input#] - the actual input test data.
**********************************************************************/

load_inputs(char *extension)

{
FILE *input_file;
int  chr, error_code = SUCCESS;
int  set, input_num;
char input_name[80];

/* Set up file name */
strcpy(input_name, "./input.");
strcat(input_name, extension);

/* Open input.extension file for read only */
input_file = fopen(input_name, "r");

/* Read in the data if the file exists */
if (input_file)
  {
  /* Try to read enough 1's and 0's for each set of inputs */
  set = 0;
  input_num = 0;

  while (set < conf.io_sets && error_code == SUCCESS)
    {

    while (input_num < conf.e_inputs && error_code == SUCCESS)
      {
      chr = fgetc(input_file);

      if (chr == EOF)
        {
        error_code = ERROR;
        }

      if (chr == '0')
        {
        inputs[set][input_num] = -1;
        input_num++;
        }

      if (chr == '1') 
        {
        inputs[set][input_num] = 1;
        input_num++;
        }
      } /* stop <while input_num> */

    input_num = 0;
    set++;
    } /* stop <while set> */

  fclose(input_file);
  } /* stop <if input_file> */
else
  {
  error_code = ERROR;
  }

return(error_code);
} /* END OF load_inputs */

/*************************************************************************
* load_four_combo
*
*     This function loads the global array combo4 with all the unique
* weight and threshold combinations.  These values were calculated by
* a separate program and saved to a file called combo4.data.
*
* Input : none.
* Output: returns SUCCESS or ERROR.
* 
* Global: combo4[unique#][weight or thres #] - the unique combination
*                         positions 0 through 3 are weights 1 through 4.
*                         position  4 is the threshold level.
*************************************************************************/

load_four_combo()
{
int  cnt, unique_num;
int  error_code = SUCCESS;
FILE *combo_file;
int value0, value1, value2, value3, value4;
int debug = 0;

/* Open the combination 4 file for read only */
combo_file = fopen("./combo4.data", "r");

/* if it is open, load in the data */
if (combo_file != NULL)
  {
  for(cnt = 0; cnt < COMBO4; cnt++)
    {
    if (fscanf(combo_file,"%d %d %d %d %d %d ", &value0, &value1,
      &value2, &value3, &value4, &unique_num) == EOF)
      {
      error_code = ERROR;
      break;
      }
    else
      {
      combo4[cnt][0]   = (char) value0;
      combo4[cnt][1]   = (char) value1;
      combo4[cnt][2]   = (char) value2;
      combo4[cnt][3]   = (char) value3;
      combo4[cnt][4]   = (char) value4;
      combo4index[cnt] = (short) unique_num;
      }

    if (debug)
      printf("%d %d %d %d %d %d\n",combo4[cnt][0], combo4[cnt][1],
        combo4[cnt][2], combo4[cnt][3], combo4[cnt][4], unique_num);

    } /* Stop <for cnt> */
  } /* stop <if combo...> */
else
  {
  error_code = ERROR;
  }


fclose(combo_file);
return(error_code);
} /* END OF load_four_combo */

/*************************************************************************
* load_two_combo
*
*     This function loads the global array combo2 with all the unique
* weight and threshold combinations.  These values were calculated by
* a separate program and saved to a file called combo2.data.
* 
* Input : none.
* Output: returns SUCCESS or ERROR.
*
* Global: combo2[unique#][weight or thres #] - the unique combination
*                         positions 0 and 1 are weights 1 and 2.
*                         position  2 is the threshold level.
*************************************************************************/

load_two_combo()
{
int cnt, unique_num;
int error_code = SUCCESS;
FILE *combo_file;
int value0, value1, value2, value3, value4;
int debug = 0;

/* Open the combo2.data file for read only */
combo_file = fopen("./combo2.data.full", "r");

/* If able to open, read in the data */
if (combo_file != NULL)
  {
  for(cnt = 0; cnt < COMBO2; cnt++)
    {
    if (fscanf(combo_file,"%d %d %d %d %d ",&value0, &value1,
       &value2, &value3, &unique_num) == EOF)
      {
      error_code = ERROR;
      break;
      }
    else
      {
      combo2[cnt][0] = (char) value0;
      combo2[cnt][1] = (char) value1;
      combo2[cnt][2] = (char) value2;
      combo2[cnt][3] = (char) value3;
      }
    if (debug)
      printf("%d %d %d %d %d\n",combo2[cnt][0],combo2[cnt][1],combo2[cnt][2],
        combo2[cnt][3], unique_num);
    } /* Stop <for cnt> */
  } /* stop <if combo...> */
else
  {
  error_code = ERROR;

  }


fclose(combo_file);
return(error_code);
} /* END OF load_two_combo */
  
