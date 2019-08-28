
/* SCS-C -- Goldberg's Simple Classifier System in C (Goldberg 1989) */

/* scs.c -- main program

   Copyright (C) 1993 Joerg Heitkoetter

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 2, or (at your option)
   any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA. */

#ifndef lint
static char *rcsid = "$Id: scs.c,v 1.1 1993/10/04 12:00:09 joke Exp $";
#endif

#include "scs.h"
#include "getopt.h"
#include "version.h"

/* application specific vars */
class_p population;		/* classifier population */
class_l matchlist;		/* matching list */
message_t envmessage;		/* environmental message */

FILE *rep;			/* log file */
char lfilename[MAXFILENAME]
= "scs.log";			/* log file name */

/* hook to panic */
char *program_name;		/* pointer to argv[0] */

/* hook to resource */
static char *rscfile_name = RSC_FILE;	/* pointer to resource */
static char *rscpath_name = RSC_PATH;	/* where to look for resource? */

/* global modes */
static int batchflag = 0;	/* run in batch mode? */

/* for apporttionment of credit: aoc.c */
int noiseflag = 1;		/* noisy auction? */
int speciflag = 1;		/* specificity in auction? */

/* program options */
static struct option opts[] =
{
  {
    "no-noisy-auction", 0, 0, 'N'
  },
  {
    "no-specificity-in-auction", 0, 0, 'S'
  },
  {
    "time-steps", 1, 0, 'T'
  },
  {
    "batch", 0, 0, 'b'
  },
  {
    "classifier-data", 1, 0, 'c'
  },
  {
    "detector-data", 1, 0, 'd'
  },
  {
    "environmental-data", 1, 0, 'e'
  },
  {
    "genetics-data", 1, 0, 'g'
  },
  {
    "help", 0, 0, 'h'
  },
  {
    "log-file", 1, 0, 'l'
  },
  {
    "plot-file", 1, 0, 'p'
  },
  {
    "reinforcement-data", 1, 0, 'r'
  },
  {
    "seed-for-random", 1, 0, 's'
  },
  {
    "timekeeper-data", 1, 0, 't'
  },
  {
    "version", 0, 0, 'V'
  },
  {
    "warranty", 0, 0, 'W'
  },
  {
    0, 0, 0, 0
  }
};

void
main (argc, argv)
     int argc;
     char **argv;
{
  int c, index;
  int maxtimes = 1;


  /* whoami? */
  program_name = *argv;

  /* parse args */
  while ((c = getopt_long (argc, argv, "NST:bc:d:e:g:hl:p:r:s:t:VW", opts, &index)) != EOF)
    {

      /* long option? */
      if (c == 0)
	{
	  c = opts[index].val;
	}
      /* setup program parameters */
      switch (c)
	{

	  /* Noisy auction? */
	case 'N':
	  noiseflag = FALSE;
	  break;

	  /* Specifity in auction? */
	case 'S':
	  speciflag = FALSE;
	  break;

	  /* Time steps */
	case 'T':
	  maxtimes = atoi (optarg);
	  break;

	  /* batch mode? */
	case 'b':
	  batchflag = TRUE;
	  break;

	  /* classifiers */
	case 'c':
	  strcpy ((char *) cfilename, optarg);
	  break;

	  /* detectors */
	case 'd':
	  strcpy ((char *) dfilename, optarg);
	  break;

	  /* environmental data */
	case 'e':
	  strcpy ((char *) efilename, optarg);
	  break;

	  /* genetic algorithm data */
	case 'g':
	  strcpy ((char *) gfilename, optarg);
	  break;

	  /* print usage info */
	case 'h':
	  usage (1);
	  break;

	  /* log file */
	case 'l':
	  strcpy ((char *) lfilename, optarg);
	  break;

	  /* plot file */
	case 'p':
	  strcpy ((char *) pfilename, optarg);
	  break;

	  /* reinforcement data */
	case 'r':
	  strcpy ((char *) rfilename, optarg);
	  break;

	  /* random number generator seed */
	case 's':
	  sscanf (optarg, "%lf", &randomseedvalue);
	  break;

	  /* timekeeping data */
	case 't':
	  strcpy ((char *) tfilename, optarg);
	  break;

	  /* print version info */
	case 'V':
	  version (0);
	  break;

	  /* print version info */
	case 'W':
	  warranty (0);
	  break;

	  /* print usage info */
	default:
	  usage (1);
	}
    }

  initialization (batchflag);
  detectors (&environrec, &detecrec, envmessage);
  report (rep);

  do
    {

      /* actions */
      timekeeper (&timekeeprec);

      environment (&environrec);
      detectors (&environrec, &detecrec, envmessage);

      matchclassifiers (&population, envmessage, &matchlist);
      aoc (&population, &matchlist, &clearingrec);

      effector (&population, &clearingrec, &environrec);
      reinforcement (&reinforcementrec, &population, &clearingrec, &environrec);

      /* log actions */
      if (timekeeprec.reportflag)
	report (rep);

      if (timekeeprec.consolereportflag)
	consolereport (&reinforcementrec);

      if (timekeeprec.plotreportflag)
	plotreport (pfile, &reinforcementrec);

      advance (&clearingrec);

      /* evaluate new rules */
      if (timekeeprec.gaflag)
	{
	  ga (&garec, &population);
	  if (timekeeprec.reportflag)
	    reportga (rep, &garec, &population);
	}
    }
  while (!halt () && --maxtimes > 0);

  /* final report */
  report (rep);

  /* shut down gracefully */
  deinitialization ();

  exit (0);
}

/*
 *    usage -- print usage information
 */
void
usage (code)
     int code;
{
  fprintf (stderr, "usage: %s [options]\n\
	[-N, --no-noise-in-auction]\n\
	[-S, --no-specificity-in-auction]\n\
	[-b, --batch]\n\
	[-h, --help]\n\
	[-V, --version]\n\
	[-W, --warranty]\n\n\
	[-c, --classifier-data <classifier data file name>]\n\
	[-d, --detector-data <detector data file name>]\n\
	[-e, --environmental-data <environmental data file name>]\n\
	[-g, --genetics-data <GA data file name>]\n\
	[-r, --reinforcement-data <reinforcement data file name>]\n\
	[-t, --timekeeper-data <timekeeper data file name>]\n\n\
	[-l, --log-file <log file name>]\n\
	[-p, --plot-file <plot file name>]\n\n\
	[-s, --seed-for-random <seed value>]\n\
	[-T, --time-steps <number>]\n\
	\n", program_name);

  exit (code);
}

/*
 *    version -- print version information
 */
void
version (code)
     int code;
{
  fprintf (stderr, "This is %s %s version %d.%d%c (%003d)\n",
	   V_NAME, V_OSTYPE, V_MAJOR, V_MINOR, V_MAGIC, V_MODF);

  fprintf (stderr, "Currently using %s random number generator.\n", randomversion);
  fprintf (stderr, "Copyright (C) 1993 by Joerg Heitkoetter. Type `%s -W' for WARRANTY.\n", program_name);

#ifndef LOCAL_MAINTAINER
  fprintf (stderr, "Send bugs, comments, etc., to %s.\n", V_EMAIL);
#else
  fprintf (stderr, "Last modification by %s, on %s\n", V_MAINTAINER, V_DATE);
  fprintf (stderr, "Send bugs, comments, etc., to %s\n", V_EMAIL);
#endif

  exit (code);
}

/*
 *    warranty -- print warranty information
 */
void
warranty (code)
     int code;
{
  fprintf (stderr, "\
    SCS-C  --  Copyright (C) 1993 by Joerg Heitkoetter\n\
\n\
    A Simple Classifier System in C translated from Pascal SCS\n\
    taken from Appendix D of David E. Goldberg's book\n\
    `Genetic Algorithms in Search, Optimization, and Machine Learning'\n\
    Addison-Wesley, Reading MA, 1989.\n\
\n\
    This program is free software; you can redistribute it and/or modify\n\
    it under the terms of the GNU General Public License as published by\n\
    the Free Software Foundation; either version 2 of the License, or\n\
    (at your option) any later version.\n\
\n\
    This program is distributed in the hope that it will be useful,\n\
    but WITHOUT ANY WARRANTY; without even the implied warranty of\n\
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n\
    GNU General Public License for more details.\n\
\n\
    You should have received a copy of the GNU General Public License\n\
    along with this program; if not, write to the Free Software\n\
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.\n");

  exit (code);
}
