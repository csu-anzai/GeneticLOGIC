
/*
 *  file:	setup.c
 *
 *  		Copyright (c) 1986, 1990 by John J. Grefenstette
 *
 *  purpose:	create an input file for GENESIS.
 *		Default values for input parameters are provided
 *		when the user gives a null response to a prompt.
 *
 *  modified:	feb. 1986
 *		10 sep 90: handle floating point option
 */

#include "define.h"

main()
{
  FILE *fp, *fopen();
  int i, j;
  char s[40];
  char ga[40];
  char infile[40];
  char templatefile[40];
  char format[20];
  char cmd[80];
  int bitlength;
  int interpret;
  int status;
  int genes;
  unsigned long values;
  unsigned long verify;
  double min;
  double max;
  int count;
  int ok;
  int recompile,temp;
  int parameter_length,parameter_genes,parameter_popsize;
  int parameter_window,parameter_save;
  char temp1[8],temp2[11];
  
  if ((fp = fopen("parameters.h","r")) == NULL) {
    recompile = 1;
    parameter_popsize = 0;
    parameter_genes = 0;
    parameter_length = 0;
    parameter_window = 0;
    parameter_save = 0;
  }
  else {
    recompile = 0;
    fscanf(fp,"%s %s %d\n",temp1,temp2,&parameter_popsize);
    fscanf(fp,"%s %s %d\n",temp1,temp2,&parameter_genes);
    fscanf(fp,"%s %s %d\n",temp1,temp2,&parameter_length);
    fscanf(fp,"%s %s %d\n",temp1,temp2,&parameter_window);
    fscanf(fp,"%s %s %d\n",temp1,temp2,&parameter_save);
  }
  fclose(fp);

  printf("File suffix []: ");
  getstring(s);
  if (strlen(s) == 0) {
    sprintf(infile, "in");
    sprintf(templatefile, "template");
  }
  else {
    sprintf(infile, "in.%s", s);
    sprintf(templatefile, "template.%s",s);
  }		
  
  printf("Floating point representation [y]: ");
  getstring(s);
  if (strlen(s) == 0 || strcmp(s, "y") == 0)
    interpret = 1;
  else
    interpret = 0;
  
  if (interpret)
    {
      bitlength = 0;
      
      /* get string interpretation */
      printf("number of genes: ");
      scanf("%d", &genes);
      if ((recompile) || (genes != parameter_genes)) {
	recompile = 1;
	parameter_genes = genes;
      }

      fp = fopen(templatefile, "w");
      fprintf(fp, "genes: %d\n\n", genes);
      printf("\n");
      
      for (i=0; i<genes; )
	{
	  printf("gene %d\n", i);
	  printf("min: ");
	  scanf("%lf", &min);
	  printf("max: ");
	  scanf("%lf", &max);
	  ok = 0;
	  while (!ok)
	    {
	      printf("values (must be a power of 2): ");
	      scanf("%lu", &values);
	      verify = 1L << ilog2(values);
	      ok = verify == values;
	      if (!ok)
		printf("bad choice for values\n");
	    }
	  printf("format string: ");
	  scanf("%s", format);
	  printf("count: ");
	  scanf("%d", &count);
	  printf("\n");
	  
	  for (j=0; j < count && i < genes; j++, i++)
	    {
	      fprintf(fp, "gene %d\n", i);
	      fprintf(fp, "min: %g\n", min);
	      fprintf(fp, "max: %g\n", max);
	      fprintf(fp, "values: %lu\n", values);
	      fprintf(fp, "format: %s\n", format);
	      fprintf(fp, "\n");
	      bitlength += ilog2(values);
	    }
	}
      fclose(fp);
    }
  
  if ((fp = fopen(infile, "w")) == NULL)
    {
      printf("can't open %s\n", infile);
      printf("Setup aborted.\n");
      exit(1);
    }
  
  /* kluge to get rid of left over LF */
  getchar();
  
  setpar(fp, "Experiments", "1");
  setpar(fp, "Total Trials", "1000");

  printf("Population Size [50]: ");
  getstring(s);
  if (strlen(s) == 0)
    strcpy(s,"50");
  fprintf(fp, "%18s = %s\n", "Population Size", s);
  temp = atoi(s);
  if ((recompile) || (temp != parameter_popsize)) {
    recompile = 1;
    parameter_popsize = temp;
  }
  
  if (interpret) {
    fprintf(fp, "%18s = %d\n", "Structure Length", bitlength);
    if ((recompile) || (bitlength != parameter_length)) {
      recompile = 1;
      parameter_length = bitlength;
    }
  }
  else {
    printf("Structure Length [30]: ");
    getstring(s);
    if (strlen(s) == 0)
      strcpy(s,"30");
    fprintf(fp, "%18s = %s\n", "Structure Length", s);
    temp = atoi(s);
    if ((recompile) || (temp != parameter_length)) {
      recompile = 1;
      parameter_length = temp;
    }
  }

  setpar(fp, "Crossover Rate", "0.6");
  setpar(fp, "Mutation Rate", "0.001");
  setpar(fp, "Generation Gap", "1.0");

  printf("Scaling Window [5]: ");
  getstring(s);
  if (strlen(s) == 0)
    strcpy(s,"5");
  fprintf(fp, "%18s = %s\n", "Scaling Window", s);
  temp = atoi(s);
  if ((recompile) || (temp != parameter_window)) {
    recompile = 1;
    parameter_window = temp;
  }
  
  setpar(fp, "Report Interval", "100");

  printf("Structures Saved [10]: ");
  getstring(s);
  if (strlen(s) == 0)
    strcpy(s,"10");
  fprintf(fp, "%18s = %s\n", "Structures Saved", s);
  temp = atoi(s);
  if ((recompile) || (temp != parameter_save)) {
    recompile = 1;
    parameter_save = temp;
  }
  
  setpar(fp, "Max Gens w/o Eval", "2");
  setpar(fp, "Dump Interval", "0");
  setpar(fp, "Dumps Saved", "0");
  if (interpret)
    setpar(fp, "Options", "acefgl");
  else
    setpar(fp, "Options", "acegl");
  setpar(fp, "Random Seed", "123456789");
  setpar(fp, "Rank Min", "0.75");
  fclose(fp);
  
#if TURBOC
  sprintf(cmd, "type %s", infile);
#else
  sprintf(cmd, "cat %s", infile);
#endif
  system(cmd);
  
  if (recompile) {
    if ((fp = fopen("parameters.h","w")) == NULL) {
      printf("Can't open parameters.h\n");
      printf("Setup aborted\n");
    }
    else {
      fprintf(fp,"#define Popsize %d\n",parameter_popsize); 
      if (parameter_genes == 0) parameter_genes = 1;
      fprintf(fp,"#define Genes %d\n",parameter_genes); 
      fprintf(fp,"#define Length %d\n",parameter_length); 
      fprintf(fp,"#define Windowsize %d\n",parameter_window); 
      fprintf(fp,"#define Savesize %d\n",parameter_save); 
    }
    fclose(fp);
    printf("parameters.h has changed\n");
    printf("Please redo your make command\n");
  }
  printf("Setup Done\n");
}

int ilog2(n)
     unsigned long n;
{
  int i;
  
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


setpar(fp, prompt, defaultstring)
     FILE *fp;
     char *prompt;
     char *defaultstring;
{
  char s[80];
  printf("%s [%s]: ", prompt, defaultstring);
  getstring(s);
  if (strlen(s) == 0)
    strcpy(s, defaultstring);
  fprintf(fp, "%18s = %s\n", prompt, s);
}

#if TURBOC

getstring(s)
     char s[];
{
  register int c;
  
  c = getchar();
  
  /* discard left over CR */
  if (c == '\r')
    c = getchar();
  
  /* read until next LF */
  while (c != '\n')
    {
      *s++ = c;
      c = getchar();
    }
  *s = '\0';
}

#else

getstring(s)
     char s[];
{
  gets(s);
}

#endif

/*** end of file ***/
