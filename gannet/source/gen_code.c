/***********************************************************************
* Source: gen_code.c         Version 1.0
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
*      This source code module contains the routines necessary to create,
* open, write to and from and close the genetic code file.  The genetic code
* file contains all the genetic code of a particular generation.  
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
*  Written by: Captain Jason J. Spofford, USAF
*              ECE Graduate Student, George Mason University
* Written on : 15 April 1990
* Updated on : 26 June  1990
*
************************************************************************/
/* The following include files are required */
#include <stdio.h>
#include "comdef.h"

/* External variables required */
extern struct genetic_code *all_parents, *all_children;
extern FILE *history;

/* The following functions are defined */
int store_gen_code (int, int, struct genetic_code [1], FILE *);
int store_gen_quick(int, struct genetic_code *, struct genetic_code [1], int);
int read_gen_code  (int, int, struct genetic_code [1], FILE *);
int read_gen_quick (int, struct genetic_code *, struct genetic_code [1], int);
int create_gen_code(char *, char *, FILE **);
int open_gen_code  (char *, char *, FILE **);
int close_gen_code (FILE *);

/************************************************************************
* store_gen_code
*
*       This routine is only used if the "-s" save memory option is 
* given.  This option will hopefully allow this program to be run on
* personal computers with relatively small main memories.
*
* Inputs : num_neurons - The number of neurons represented by genetic code.
*          seek_pos    - The pointer into the file the code should be stored.
*          gen_code    - The genetic code in memory.
*          file_ptr    - The file the code should be stored into.
* Outputs: SUCCESS or ERROR is returned.
*
* Global : none.
************************************************************************/

store_gen_code(int num_neurons, int seek_pos,
               struct genetic_code gen_code[1],
               FILE *file_ptr)
{
int bytes, value, cnt1, cnt2;
int error_code = SUCCESS;


bytes = num_neurons * 2 * BYTES_PER_NEURON;
fseek(file_ptr, seek_pos, 0);

value = 0;
for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
  {
  value = value + fwrite(&gen_code[0].combo[cnt1], 2, 1, file_ptr);
  value = value + fwrite(&gen_code[0].bid[cnt1],   2, 1, file_ptr);

  for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
    value = value + fwrite(&gen_code[0].conn[cnt1][cnt2], 1, 1, file_ptr);

  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
    value = value + fwrite(&gen_code[0].cross[cnt1][cnt2], 1, 1, file_ptr);

  }

if (bytes != value) error_code = ERROR;

return(error_code);
} /* END OF store_gen_code */

/************************************************************************
* store_gen_quick
*
*     This function stores genetic code into a large array in main memory
* and avoids delays involving secondary storage devices.  This routine is
* used by default. Genetic code is always stored to all_children.
*
* Inputs : chd_num     - The record number of the child.
*          all_ptr     - Not used but the routine does not work without (why?)
*          gen_code    - The genetic code to store.
*          num_neurons - The number of neurons for this individual.
* Outputs: none.
*
* Global : all_children - receives the new genetic code.
************************************************************************/

store_gen_quick(int chd_num, struct genetic_code *all_ptr,
                struct genetic_code gen_code[1], int num_neurons)
{
int cnt1, cnt2;

for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
  {
  all_children[chd_num].combo[cnt1] = gen_code[0].combo[cnt1];
  all_children[chd_num].bid[cnt1]   = gen_code[0].bid[cnt1];
  for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
    all_children[chd_num].conn[cnt1][cnt2] = gen_code[0].conn[cnt1][cnt2];
  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
    all_children[chd_num].cross[cnt1][cnt2] = gen_code[0].cross[cnt1][cnt2];
  }

} /* END OF store_gen_quick */

/************************************************************************
* read_gen_code
*
*     This function reads in the genetic code from a genetic code file. 
* The information passed back to the calling routine.
*
* Inputs : num_neurons - The number of neurons in the individual.
*          seek_pos    - The location of the individual in the file.
*          file_ptr    - The particular file we are reading.
* Outputs: returns SUCCESS or ERROR.
*          gen_code    - Where the genetic code read will be placed.
*
* Global : none.
************************************************************************/

read_gen_code(int num_neurons, int seek_pos, 
              struct genetic_code gen_code[1], FILE *file_ptr)
{
int bytes, value, cnt1, cnt2;
int error_code = SUCCESS;

bytes = num_neurons * 2 * BYTES_PER_NEURON;
fseek(file_ptr, seek_pos, 0);

value = 0;
for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
  {
  value = value + fread(&gen_code[0].combo[cnt1], 1, 2, file_ptr);
  value = value + fread(&gen_code[0].bid[cnt1],   1, 2, file_ptr);

  for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
    value = value + fread(&gen_code[0].conn[cnt1][cnt2], 1, 1, file_ptr);

  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
    value = value + fread(&gen_code[0].cross[cnt1][cnt2], 1, 1, file_ptr);
  }

if (bytes != value) error_code = ERROR;

return(error_code);

} /* END OF read_gen_code */

/************************************************************************
* read_gen_quick
*
*     This function reads genetic code from a large array in main memory
* and avoids delays involving secondary storage devices.  This routine is
* used by default. Genetic code is always read from all_parents.
*
* Inputs : par_num     - The record number of the child.
*          all_ptr     - Not used but the routine does not work without (why?)
*          num_neurons - The number of neurons for this individual.
* Outputs: gen_code    - The genetic code is stored here.
*
* Global : all_parents - The array the genetic code is read from.
************************************************************************/

read_gen_quick(int par_num, struct genetic_code *all_ptr,
               struct genetic_code gen_code[1], int num_neurons)
{
int cnt1, cnt2;

for (cnt1 = 0; cnt1 < num_neurons; cnt1++)
  {
  gen_code[0].combo[cnt1] = all_parents[par_num].combo[cnt1]; 
  gen_code[0].bid[cnt1]   = all_parents[par_num].bid[cnt1];

  for (cnt2 = 0; cnt2 < N_INPUTS; cnt2++)
    gen_code[0].conn[cnt1][cnt2] = all_parents[par_num].conn[cnt1][cnt2];

  for (cnt2 = 0; cnt2 < BYTES_PER_NEURON; cnt2++)
    gen_code[0].cross[cnt1][cnt2] = all_parents[par_num].cross[cnt1][cnt2];
  }
} /* END OF read_gen_quick */

/************************************************************************
* create_gen_code
*
*      This function creates a genetic code file.  It is not meant to open
* an existing file.
*
* Inputs : extension - the experiment name.
*          name      - the file name.
* Outputs: file_ptr  - the file pointer is returned.
*
* Global : none.
************************************************************************/

create_gen_code(char *extension, char *name, FILE **file_ptr)

{
char file_name[80];
int  error_code = SUCCESS;

strcpy(file_name, name);
strcat(file_name, extension);

*file_ptr = fopen(file_name, WR_BINARY);

if (*file_ptr == NULL)
  {
  error_code = ERROR;
  fprintf(history, "Could not create %s file.\n", file_name);
  }

return(error_code);

} /* END OF create_gen_code */

/************************************************************************
* open_gen_code
*
*     This function opens a existing genetic code file.
*
* Inputs : extension - The experiment name.
*          name      - The file name (minus the extension)
* Outputs: file_ptr  - The file pointer.
*          returns (SUCCESS or ERROR)
*
* Global : none.
************************************************************************/

open_gen_code(char *extension, char *name, FILE **file_ptr)

{
char file_name[80];
int  error_code = SUCCESS;

strcpy(file_name, name);
strcat(file_name, extension);

*file_ptr = fopen(file_name, READ_BINARY);

if (*file_ptr == NULL)
  {
  error_code = ERROR;
  fprintf(history, "Can not find %s file!\n", file_name);
  }

return(error_code);
} /* END OF open_gen_code */

/***********************************************************************
* close_gen_code
*
*     This function simply closes a genetic code file (or any file for
* that matter).
*
* Inputs : file_ptr - The file we wish to close.
* Outputs: returns (SUCCESS or ERROR)
*
* Global : none.
************************************************************************/

close_gen_code(FILE *file_ptr)

{
int error_code = SUCCESS;

if (fclose(file_ptr))
  {
  error_code = ERROR;
  fputs("Could not close gen_code file!\n", history);
  }

return(error_code);
} /* END OF close_gen_code */

