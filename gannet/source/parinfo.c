/*******************************************************************
* Source: parinfo.c         Version 1.0
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
*    This source code module contains two functions that load and
* store the par_info.extension file. The par_info file contains
* information on a parents fitness, age, etc...
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
* Updated on: 26 June  1990
*
**********************************************************************/
/* The following include files are required */
#include <stdio.h>
#include "comdef.h"

/* external variables required */
extern FILE   *history;
extern struct net_info *par_info;
extern struct configuration conf;

/* The following functions are defined */
int get_par_info(char *);
int store_par_info(char *);

/**********************************************************************
* get_par_info
*
*   This procedure loads in the parent information from the file
* par_info.extension into the par_info global array.
*
* Input  : extension - the experiment name.
* Output : returns SUCCESS or ERROR.
*
* global : par_info      - the parent information array.
*          conf.pop_size - the population size of the experiment.
**********************************************************************/

get_par_info(char *extension)

{
char par_name[80];
FILE *par_info_ptr;
int  cnt, count, error_code = SUCCESS;

/* Set up the unix file name */
strcpy(par_name, "par_info.");
strcat(par_name, extension);

/* Attempt to open the file */
par_info_ptr = fopen(par_name, READ_BINARY);

if (par_info_ptr != NULL)
  {
  /* If it is opened, read in the information */
  for (cnt = 0; cnt < conf.pop_size; cnt++)
    {
    count = fread(&par_info[cnt], 1, 32, par_info_ptr);
    if (count != 32)
      {
      error_code = ERROR;
      fputs("Par_info information file too short!\n", history);
      break;
      }
    } /* stop <for cnt> */
  fclose(par_info_ptr);
  } /* stop <if par_info_ptr> */
else
  {
  error_code = ERROR;
  fputs("Parents information file does not exist!\n", history);
  }

if (error_code == SUCCESS)
  fputs("Parents information file read.\n", history);

return(error_code);
} /* END OF get_par_info */

/**********************************************************************
* store_par_info
*
*   This procedure stores the parent information array into a file called
* par_info.extension.
*
* Input  : extension - the experiment name.
* Output : returns SUCCESS or ERROR.
*
* Global : par_info      - The parent information array.
*          conf.pop_size - The experiment population size.
*
**********************************************************************/

store_par_info(char *extension)

{
char par_name[80];
FILE *par_info_ptr;
int  cnt, count, error_code = SUCCESS;

/* Set up the UNIX file name */
strcpy(par_name, "par_info.");
strcat(par_name, extension);

/* Create the file */
par_info_ptr = fopen(par_name, WRITE_BINARY);

if (par_info_ptr != NULL)
  {
  /* If able to create, write out the par_info array */
  for (cnt = 0; cnt < conf.pop_size; cnt++)
    {
    count = fwrite(&par_info[cnt], 1, 32, par_info_ptr);
    if (count != 32)
      {
      error_code = ERROR;
      fputs("Unsuccessful writing par_info file!\n", history);
      break;
      }
    } /* stop <for cnt> */
  fclose(par_info_ptr);
  }
else
  {
  error_code = ERROR;
  fputs("File par_info can not be created!\n", history);
  }
if (error_code == SUCCESS)
  fputs("File par_info created and closed.\n", history);

return(error_code);
} /* END OF store_par_info */



