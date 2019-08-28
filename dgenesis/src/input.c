
/*
 *  GENESIS  Copyright (c) 1986, 1990 by John J. Grefenstette
 *  This program may be freely copied for educational
 *  and research purposes.  All other rights reserved.
 *
 *  file:	input.c
 *
 *  purpose:	Set up filenames and read the input files, and
 *		initialize variables for this run.
 *
 *		See init.c for the initialization of variables for each
 *		experiment.
 *
 *  modified:	26 jun 86
 *		15 sep 90: read template file for floating point representation
 *		01 jul 93: handle multiple processes, allocate memory for
 *			   migration structures (ECP)
 */

/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 */


#include <time.h>
#include "extern.h"

void Setflag();

Input (argc, argv)
int     argc;
char   *argv[];
{
	FILE * fp;
	int     i;		/* loop control       */
	char    msg[40];	/* used when printing error message	 */
	time_t  clock;		/* current date       */
	char    expname[30];	/* experiment name    */
	char    mainaddr[20];	/* IP addr of main process	 */
	int     mainport;	/* UDP port of main process      */


	if (argc < 3)
		Error ("Fatal: Required parameters missing");

	/* process command line inputs */
	strcpy (expname, argv[1]);
	My_id = atoi (argv[2]);
	if (My_id) {
		if (argc < 5)
			Error ("Fatal: Required parameters missing");
		strcpy (mainaddr, argv[3]);
		mainport = atoi (argv[4]);
	}


	if (My_id == 0) {
		/* set up file names */
		sprintf (Infile, "in.%s", expname);
		sprintf (Initfile, "init.%s", expname);
		sprintf (Schemafile, "schema.%s", expname);
		sprintf (Templatefile, "template.%s", expname);
		sprintf (Procfile, "proc.%s", expname);
		sprintf (Linkfile, "link.%s", expname);
		sprintf (Regfile, "reg.%s", expname);
		sprintf (Endfile, "main-end.%s", expname);
		sprintf (Logfile, "log.%s", expname);
		sprintf (Infofile, "main-info.%s", expname);

		/* read in the parameters from the infile */

		if ((fp = fopen (Infile, "r")) == NULL) {
			sprintf (msg, "Input: can't open %s", Infile);
			Error (msg);
		}
		fscanf (fp, IN_FORMAT, IN_VARS);
		Seed = OrigSeed;
		Popsize = Totalpop;
		fclose (fp);

                if (Processes > 1){
                	/* Initialize socket interface */
	                InitSocket();

			/* Receive address and port info */
			MakeProcessTable ();

			/* send the parameters to the other processors */
			SendParameters ();

			/* make and send neighbors & migration info to each process */
			MakeLinks ();
			SendLinks ();
                }
	}
	else {
               	/* Initialize socket interface */
                InitSocket();

		/* send address and port info to main */
		My_id = Register (mainaddr, mainport);

		/* wait for initial message with parameters */
		ReceiveParameters ();

                /* Each process has a different Seed */
		Seed = OrigSeed * (My_id + 1);

		/* receive neighbors, migration info */
		ReceiveLinks ();
	}

	/* set up file names */
	sprintf (Outfile, "%dout.%s", My_id, expname);
	sprintf (Ckptfile, "%dckpt.%s", My_id, expname);
	sprintf (Minfile, "%dmin.%s", My_id, expname);
	strcpy (Bestfile, Minfile);
	sprintf (Logfile, "%dlog.%s", My_id, expname);

	/* activate the Options */
	for (i = 0; Options[i] != '\0'; i++)
		Setflag (Options[i]);
	if (Displayflag)
		Traceflag = 0;

	/* Bytes is the size of each packed chromosome */
	Bytes = Length / CHARSIZE;
	if (Length % CHARSIZE)
		Bytes++;

	/* read template file if used */
	if (Floatflag) {
		if (My_id)
			ReceiveTemplate ();
		else
			SendTemplate ();
	}

	/* echo Input params */
	if (Traceflag)
		printf (OUT_FORMAT, OUT_VARS);

	/* allocate storage for variable sized structures */

	/* used for floating representation of chromosomes */
	Vector = (double *) calloc ((unsigned) Genes, sizeof (double));
	if (Vector == NULL)
		Error ("Input: Memory allocation failed for Vector");

	/* used for string representation of chromosomes */
	Bitstring = malloc ((unsigned) (Length + 1));
	if (Bitstring == NULL)
		Error ("Input: Memory allocation failed for Bitstring");

	Bitstring[Length] = '\0';


	/* population arrays */

	Old = (STRUCTURE *) calloc ((unsigned) (Popsize + Migrecv), sizeof (STRUCTURE));
	if (Old == NULL)
		Error ("Input: Memory allocation failed for Old");
	for (i = 0; i < Popsize + Migrecv; i++)
		if ((Old[i].Gene = malloc ((unsigned) Bytes)) == NULL)
			Error ("Input: memory allocation failed for Old.Gene");

	New = (STRUCTURE *) calloc ((unsigned) (Popsize + Migrecv), sizeof (STRUCTURE));
	if (New == NULL)
		Error ("Input: Memory allocation failed for New");
	for (i = 0; i < Popsize + Migrecv; i++)
		if ((New[i].Gene = malloc ((unsigned) Bytes)) == NULL)
			Error ("Input: memory allocation failed for New.Gene");

	/* to receive migrants */
	Migrants = (STRUCTURE *) calloc ((unsigned) Migrecv, sizeof (STRUCTURE));
	if (Migrants == NULL)
		Error ("can't allocate memory for Migrants");

	for (i = 0; i < Migrecv; i++) {
		if ((Migrants[i].Gene = malloc ((unsigned) Bytes)) == NULL)
			Error ("Input: memory allocation failed for Migrants.Gene");
	}

        /* to send migrants */
        Migset = (STRUCTURE *) calloc ((unsigned) Migsend, sizeof(STRUCTURE));
        if (Migset == NULL)
		Error ("Input: memory allocation failed for Migset");

        for (i = 0; i < Migsend; i++)
		if ((Migset[i].Gene = malloc ((unsigned) Bytes)) == NULL)
			Error ("Input: memory allocation failed for Migset.Gene");

	/* used to compute moving value for Worst */
	if (Windowsize) {
		Window = (double *) calloc ((unsigned) Windowsize, sizeof (double));
		if (Window == NULL)
			Error ("Input: memory allocation failed for Window");
	}

	/* used to save best structures */
	if (Savesize) {
		Bestset = (BESTSTRUCT *) calloc ((unsigned) Savesize, sizeof (BESTSTRUCT));
		if (Bestset == NULL)
			Error ("Input: memory allocation failed for Bestset");

		for (i = 0; i < Savesize; i++)
			if ((Bestset[i].Gene = malloc ((unsigned) Bytes)) == NULL)
				Error ("Input: memory allocation failed for Bestset");
	}


	/* scratch the output file (unless this is a restart) */
	if (!Restartflag) {
		if ((fp = fopen (Outfile, "w")) == NULL) {
			sprintf (msg, "Input: can't open %s", Outfile);
			Error (msg);
		}
		fclose (fp);
	}

	if (Logflag && My_id == 0) LogStart();

	Trace ("Input completed");
} /* Input() */




void Setflag (c)
char c;
{
	switch (c) {
		case 'a':
			Allflag = 1;
			break;
		case 'b':
			Bestflag = 1;
			break;
		case 'c':
			Collectflag = 1;
			Convflag = 1;
			break;
		case 'C':
			Collectflag = 1;
			break;
		case 'd':
			Dumpflag = 1;
			break;
		case 'D':
			Displayflag = 1;
			break;
		case 'e':
			Eliteflag = 1;
			break;
		case 'f':
			Floatflag = 1;
			break;
		case 'g':
			Grayflag = 1;
			break;
		case 'i':
			Initflag = 1;
			break;
		case 'I':
			Interflag = 1;
			Displayflag = 1;
			break;
		case 'l': 
			Logflag = 1;
			break;
		case 'L': 
			Lastflag = 1;
			break;
		case 'M': 
			Maxflag = 1;
			break;
		case 'o': 
			Onlnflag = 1;
			break;
		case 'O': 
			Offlnflag = 1;
			break;
		case 'p': 
			Propiniflag = 1;
			break;
		case 'r':
			Restartflag = 1;
			break;
		case 'R':
			Rankflag = 1;
			break;
		case 's': 
			Schemflag = 1;
			break;
		case 'S': 
			Syncflag = 1;
			break;
		case 't': 
			Traceflag = 1;
			break;
	}
}


/** end of file **/
