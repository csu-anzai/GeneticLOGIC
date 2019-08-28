
/*
 *  file:	input.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	Set up filenames and read the input files, and
 *		initialize variables for this run.
 *
 *		See init.c for the initialization of variables for each
 *		experiment.
 *
 *  modified:	26 jun 86
 *		15 sep 90: read template file for floating point representation
 */

#include "Pextern.h"


Input(argc,argv)
     int argc;
     char *argv[];
{
  FILE *fopen(), *fp;
  
  int i;			/* loop control				*/
  char msg[40];		/* used when printing error message	*/
  long clock;		/* current date				*/
  int file_genes;
  
  long time();
  char *ctime();
  int ilog2();
  
  /* set up file names */
  
  if (argc < 2) 
    {
      strcpy(Infile,"in");   
      strcpy(Outfile,"out"); 
      strcpy(Ckptfile,"ckpt"); 
      strcpy(Minfile,"min"); 
      strcpy(Logfile, "log");
      strcpy(Initfile, "init");
      strcpy(Schemafile, "schema");
      strcpy(Templatefile, "template");
    }
  else 
    {
      sprintf(Infile, "in.%s", argv[1]);
      sprintf(Outfile, "out.%s", argv[1]);
      sprintf(Ckptfile, "ckpt.%s", argv[1]);
      sprintf(Minfile, "min.%s", argv[1]);
      sprintf(Logfile, "log.%s", argv[1]);
      sprintf(Initfile, "init.%s", argv[1]);
      sprintf(Schemafile, "schema.%s", argv[1]);
      sprintf(Templatefile, "template.%s", argv[1]);
    }
  
  strcpy(Bestfile, Minfile);
  
  /* read in the parameters from the infile */
  
  if ((fp = fopen(Infile, "r")) == NULL)
    {
      sprintf(msg, "Input: can't open %s", Infile);
      Error(msg);
    }
  fscanf(fp,IN_FORMAT,IN_VARS);
  Seed = OrigSeed;
  with(physical) {
    Index = pcoord(0);
    PSeed = OrigSeed + Index;
    for(i=0;i<20;i++)
      PRand();
  }
  fclose(fp);
  
  /* activate the Options */
  for (i=0; Options[i] != '\0'; i++)
    Setflag(Options[i]);
  if (Displayflag)
    Traceflag = 0;
  
  /* Bytes is the size of each packed chromosome */
  Bytes = Length / CHARSIZE;
  if (Length % CHARSIZE) Bytes++;
  
  /* read template file if used */
  if (Floatflag)
    {
      if ((fp = fopen(Templatefile, "r")) == NULL)
	{
	  sprintf(msg, "Input: can't open %s", Templatefile);
	  Error(msg);
	}
      fscanf(fp, "genes: %d ", &file_genes);
      if (Genes != file_genes) {
	sprintf(msg, 
		"Input: file specification of Genes inconsistent");
	Error(msg);
      }
      
      for (i=0; i<Genes; i++)
	{
	  fscanf(fp, " gene %*d");
	  fscanf(fp, " min: %lf", &Gene[i].min);
	  fscanf(fp, " max: %lf", &Gene[i].max);
	  fscanf(fp, " values: %lu", &Gene[i].values);
	  fscanf(fp, " format: %s", Gene[i].format);
	  Gene[i].bitlength = ilog2(Gene[i].values);
	  Gene[i].incr = (Gene[i].max - Gene[i].min) / 
	    (Gene[i].values - 1);
	}
      fclose(fp);
    }
  /* allocate storage for variable sized structures */
  
  Bitstring[Length] = '\0';
    
  /* echo Input params */
  if (Traceflag) printf(OUT_FORMAT, OUT_VARS);
  
  /* scratch the output file (unless this is a restart) */
  if (!Restartflag)
    {
      if ((fp = fopen(Outfile, "w")) == NULL)
	{
	  sprintf(msg, "Input: can't open %s", Outfile);
	  Error(msg);
	}
      fclose(fp);
    }
  
  /* log this activation */
  if (Restartflag) 
    {
      if (Logflag)
	{
	  if ((fp = fopen(Logfile, "a")) == NULL)
	    {
	      sprintf(msg,"Input: can't open %s", Logfile);
	      Error(msg);
	    }
	  fprintf(fp, "%s Restarted ", argv[0]);
	  time(&clock);
	  fprintf(fp, "%s", ctime(&clock));
	  fclose(fp);
	}
    }
  else
    {
      if (Logflag)
	{
	  if ((fp = fopen(Logfile, "a")) == NULL)
	    {
	      sprintf(msg,"Input: can't open %s", Logfile);
	      Error(msg);
	    }
	  fprintf(fp, "%s started   ", argv[0]);
	  time(&clock);
	  fprintf(fp, "%s", ctime(&clock));
	  fclose(fp);
	}
    }
}


int ilog2(n)
     unsigned long n;
{
  register int i;
  
  if (n <= 0)
    {
      printf("Help! values is %d, must be positive!\n", n);
      abort();
    }
  
  i = 0;
  while ((int) (n & 1) == 0)
    {
      n >>= 1;
      i++;
    }
  return(i);
}


Setflag(c)
     char c;
{
  switch (c) {
  case 'a' :
    Allflag = 1;
    break;
  case 'b' : 
    Bestflag = 1; 
    break;
  case 'c' : 
    Collectflag = 1; 
    Convflag = 1;
    break;
  case 'C' : 
    Collectflag = 1; 
    break;
  case 'd' : 
    Dumpflag = 1; 
    break;
  case 'D' : 
    Displayflag = 1; 
    break;
  case 'e' :
    Eliteflag = 1;
    break;
  case 'f' : 
    Floatflag = 1; 
    break;
  case 'g' :
    Grayflag = 1;
    break;
  case 'i' :
    Initflag = 1;
    break;
  case 'I' :
    Interflag = 1;
    Displayflag = 1;
    break;
  case 'l' :
    Logflag = 1;
    break;
  case 'L' :
    Lastflag = 1;
    break;
  case 'M' :
    Maxflag = 1;
    break;
  case 'n' :
    Localflag = 1;
    break;
  case 'o' : 
    Onlnflag = 1; 
    break;
  case 'O' :
    Offlnflag = 1;
    break;
  case 'p' :
    Probflag = 1;
    break;
  case 'r' : 
    Restartflag = 0; 
    break;
  case 'R' : 
    Rankflag = 1; 
    break;
  case 's' : 
    Schemflag = 1; 
    break;
  case 't' : 
    Traceflag = 1; 
    break;
  case 'T' :
    Timeflag = 1;
    break;
  case 'u' :
    Uniflag = 1;
    break;
  }
}


/** end of file **/
