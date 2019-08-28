/****************************************************************
* Source: history.c         Version 1.0
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
*   This source file contains two functions, start_history and
* stop_history.  These functions open and close a text file called
* history.extension.  The history file records any important messages
* during the evolutionary process and is useful for debugging.
* The global variable history must be declared external in the
* main source file.
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
* Written on:  10 March 1990.
* Updated on:  21 June  1990.
*
****************************************************************/

/* The following include files are required */
#include <time.h>
#include <stdio.h>
#include "comdef.h" 

/* The following global variable is defined */
FILE *history;

/* The following functions are defined here */
int start_history(char *, char *);
int stop_history();

/****************************************************************
* start_history
*
*	This procedure creates or continues the history file and
* records the time and date the history file was opened.
*
* Input : extension - the experiment name.
*         mode      - CREATE or APPEND the history file.
* Output: Returns status (SUCCESS or ERROR)
*
* Global:
* history - sets history file pointer.
*****************************************************************/

int start_history(char *extension, char *mode)

{
char history_name[80];     /* file name                     */
char start_time[80];       /* constructed start time string */
char *datetime;            /* time string                   */
int  systime;              /* integer form of time          */
int  error_code = SUCCESS; /* assigned SUCCESS or ERROR     */

/* Set up the file name */
strcpy(history_name, "history.");
strcat(history_name, extension);

/* Get the time and construct start_time */
strcpy(start_time, "\n********** Start Time --> ");
systime  = time(NULL);
datetime = ctime(&systime);
strcat(start_time, datetime);

/* Create or Append to a history file */
history = fopen(history_name, mode);

if (history != NULL) /* if the history file can be created */
  {
  /* Write start time to history */
  fputs("\n", history);
  fputs(extension, history);
  /* if it is append mode, record that it is a continuing experiment */
  if (!strcmp(APPEND, mode)) fputs("\nContinuing Experiment", history);
  fputs(start_time, history);
    }
else error_code = ERROR; /* Couldn't open the file */

return(error_code);
} /* END OF start_history */

/****************************************************************
* stop_history
*
*	This procedure terminates the history record and places
* the time and date of termination at the end of the history file.
*
* Input : none.
* Output: returns status (SUCCESS or ERROR)
*
* Global:
* history - uses the history file pointer to close the file.
****************************************************************/

int stop_history()

{
char stop_time[80];         /* constructed stop time string  */
char *datetime;             /* time string                   */
int  systime;               /* integer form of time          */
int  error_code = ERROR;    /* assigned SUCCESS or ERROR     */ 

/* Get the time and construct stop_time */
strcpy(stop_time, "********** Stop Time  --> ");
systime  = time(NULL);
datetime = ctime(&systime);
strcat(stop_time, datetime);

/* Only try to close the file if it was opened previously */
if (history != NULL) 
  {
  /* Write stop time to history */
  fputs(stop_time, history);
  /* fclose returns zero if successful */
  error_code =! fclose(history);
  }

return(error_code);
} /* END OF stop_history */






