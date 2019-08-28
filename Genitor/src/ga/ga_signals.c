
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1990                                       */
/*  Darrell L. Whitley                                       */
/*  Computer Science Department                              */
/*  Colorado State University                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
#include <stdio.h>
#include <signal.h>
#include "gene.h"
#include "ga_status.h"
#include "ga_global_extern.h" /* must use globals because dump routine has
								 predefined args...cannot pass as params */

/***************************************************************************
 * FUNCTION: ga_dump_interrupt
 *
 * DESCRIBE: This function designed to be called as an exception handler
 *           for operating system generated signals.  Its input arguments
 *           are as prescribed by the signal() man page.
 *
 *           This routine prints the current GA status to the screen
 *           and then prints all pertinent info about the GA to a file.
 *           This file can later be used to initialize a new GA process.
 *
 *           After printing out all necessary information, the GA will
 *           continue execution at the point where it was halted.
 *
 * RETURN VALUE:  none
 *
 * CALLS: show_progress
 *        dump_status
 *        warning
 ****************************************************************************/
void
ga_dump_interrupt(sig, code, scp, addr)
int     sig, code;
struct  sigcontext *scp;
char    *addr;
{
 FILE   *fp;

 fprintf (stderr, "\n User Signal (%d) Received. Dumping Status...\n", sig);
 
 /* Print current GA state to screen */
 show_progress (stdout, Pool, CurrentGeneration); 
 
 /* Dump all GA info to file */
 dump_status(Pool, DumpBase);

 fprintf (stderr, "\n...Continuing Execution.\n");
}


/***************************************************************************
 * FUNCTION: setup_signal
 *
 * DESCRIBE: Registers signal handling routines and prints messages to the
 *           user about how to send signals.
 *
 * INPUT PARAMETERS: none 
 *
 * RETURN VALUE:  none
 ****************************************************************************/
void
setup_signal ()
{
 signal  (SIGTSTP, ga_dump_interrupt);
 /* fprintf (stderr, "\nHit '<Ctrl> Z' to dump status.\n"); */
}
