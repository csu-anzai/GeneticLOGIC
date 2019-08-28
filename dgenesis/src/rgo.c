
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
 *
 *
 *   file:       rgo.c
 *
 *   purpose:    execute participants in a distributed experiment
 *
 */


#include <stdlib.h>
#include <stdio.h>
#include <string.h>

typedef struct {
	char host[80];
	int processes;
}      GAHOSTSTRUCT;


/***************************************************************************/
/*                            Function Prototypes                          */
/***************************************************************************/
#ifdef __STDC__
void Usage(void);
void Input(int argc, char *argv[]);
void DoMain(void);
void DoRemote(int);
void CopyOutFiles(void);
void Compress(void);
void Error(char *);
void IOError(char *);
char *ReadLine(FILE *, char *, int);
int CountLines(char *);
#endif



/***************************************************************************/
/*                            Global Variables                             */
/***************************************************************************/
char GAhosts[80];		/* pathname for 'GAhosts'	 */
char Infofile[80];		/* pathname for 'main-info'	 */
char Regfile[80];		/* file with registered processes */
char Endfile[80];		/* if this exists, main has finished */
char mainaddr[20];
int mainport;
int lines;			/* lines in GAhosts		 */
char exp[40];			/* name of experiment (from command line) */
char executable[40];		/* name of program (from cmd line) */
char hostname[64];		/* local hostname              */
GAHOSTSTRUCT *HostTable;
char Maxflag;
int Verboseflag;		/* print messages */
int Compressflag;		/* compress files at the end of experiment */
int Popsize;


/*
 * print program usage message
 */
void Usage()
{
	printf("\n\tDGENESIS Remote Execution Program\n\n");
	printf("Usage:\n");
	printf("\trgo executable-name experiment-name\n\n");
}



/*
 * process command line parameters, set filenames
 */
void Input(argc, argv)
int argc;
char *argv[];
{
	int i, j;
	char *arg;

	if (argc < 3) {
		Usage();
		exit(1);	/* requiered parameters missing	 */
	}
	j = 0;
	Verboseflag = 0;
	Compressflag = 0;

	for (i = 1; i < argc; i++) {
		arg = argv[i];
		if (arg[0] == '-') {
			switch (arg[1]) {
			case 'v':
			case 'V':
				Verboseflag = 1;
				break;
			case 'c':
				Compressflag = 1;
				break;
			default:
				printf("error: unknown option\n");
			}
		} else if (j == 0) {
			j = 1;
			strcpy(executable, arg);
		} else
			strcpy(exp, arg);
	}

	/* set file names */
	sprintf(GAhosts, "%s/usr/GAhosts", getenv("DGENESIS"));
	sprintf(Infofile, "%s/usr/%s/main-info.%s", getenv("DGENESIS"), executable, exp);
	sprintf(Regfile, "%s/usr/%s/reg.%s", getenv("DGENESIS"), executable, exp);
	sprintf(Endfile, "%s/usr/%s/main-end.%s", getenv("DGENESIS"), executable, exp);
}



/*
 * execute main process, reads the main's process IP address and port
 * to global variables 'mainaddr' and 'mainport'
 */
void DoMain()
{
	char ErrMsg[80];	/* to display error messages    */
	char cmd[150];		/* command to be executed       */
	FILE *fp;
	register int i;

	unlink(Infofile);
	unlink(Regfile);
	unlink(Endfile);
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; %s %s 0) &", executable, executable, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	if (system(cmd) < 0) {
		sprintf(ErrMsg, "rgo can't execute command:\n\t%s\n", cmd);
		IOError(ErrMsg);
	}

        /* waits for main process to execute and create Infofile */
	fp = fopen(Infofile, "r");
	while (fp == NULL)
		fp = fopen(Infofile, "r");

	/* reads mainaddr and mainport from Infofile */
	sleep(1);
	fscanf(fp, " %s %d", mainaddr, &mainport);
	fclose(fp);

	/* local host executes one less process */
	for (i = 0; i < lines; i++)
		if (strcmp(hostname, HostTable[i].host) == 0)
			HostTable[i].processes--;
}


/*
 * Execute remote processes using HostTable entries
 */
void DoRemote(totalproc)
int totalproc;
{
	register int i, cont;
	char cmd[150];		/* command to be executed       */
	char ErrMsg[80];	/* to display error messages    */
	FILE *fp;


	cont = 1;
	for (i = 1; cont < totalproc; i++) {

		if (HostTable[i % lines].processes == 0)
			continue;

		/* execute a rsh with the requiered parameters */
		if (strcmp(hostname, HostTable[i % lines].host) != 0)
			sprintf(cmd, "(rsh %s -n cd \\$DGENESIS/usr/%s \";\" %s %s %d %s %d) &",
				HostTable[i % lines].host, executable, executable, exp, cont, mainaddr, mainport);
		else
			sprintf(cmd, "(cd $DGENESIS/usr/%s ; %s %s %d %s %d) &",
				executable, executable, exp, cont, mainaddr, mainport);

		if (Verboseflag)
			printf("Executing: %s\n", cmd);

		if (system(cmd) < 0) {
			sprintf(ErrMsg, "rgo can't execute command:\n\t%s\n", cmd);
			IOError(ErrMsg);
		}

                /* wait a second */
		sleep(1);

                /* the ith host has room for one less process */
		HostTable[i % lines].processes--;

                /* one more executed process */
		cont++;

                /* if all needed processes have registered, stop launching processes */
		fp = fopen(Regfile, "r");
		if (fp != NULL){
			fclose(fp);
			cont = totalproc;
		}
	}
}


/*
 * Copies output files from remote processes after they finish
 */
void CopyOutFiles()
{
	int i;
	char *status;
	char cmd[150];		/* command to be executed       */
	char ErrMsg[80];	/* to display error messages    */
	char line[100];		/* read buffer from GAosts	 */
	char mach[64];		/* participating machine (from * regfile) */
	FILE *fp;

	if ((fp = fopen(Regfile, "r")) == NULL)
		Error("rgo: can't open Regfile");

	status = ReadLine(fp, line, 100);
	while (status != NULL) {
		sscanf(line, " %d %s", &i, mach);
		if (strcmp(mach, hostname)) {
			sprintf(cmd, "rcp %s:\\$DGENESIS/usr/%s/%dout.%s $DGENESIS/usr/%s",
				mach, executable, i, exp, executable);

			if (Verboseflag)
				printf("Executing: %s\n", cmd);

			if (system(cmd) < 0) {
				sprintf(ErrMsg, "rgo can't execute command:\n\t%s\n", cmd);
				IOError(ErrMsg);
			}
		}
		status = ReadLine(fp, line, 100);
	}
	fclose(fp);
}



/*
 * tars & compresses experiment files, keeps rep, avg and var files unchanged
 */
void Compress()
{
	char cmd[100];
        char Reptemp[L_tmpnam], Avgtemp[L_tmpnam], Vartemp[L_tmpnam];

	/* rename report, average and variance files */
        tmpnam(Reptemp);
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv rep.%s %s)", executable, exp, Reptemp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

        tmpnam(Avgtemp);
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv avg.%s %s)", executable, exp, Avgtemp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);
        tmpnam(Vartemp);

	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv var.%s %s)", executable, exp, Vartemp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);


	/* tar & compress experiment files */
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; tar -cf temp.tar *%s*; compress temp.tar)",
		executable, exp, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	/* remove experiment files */
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; rm *%s*)", executable, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	/* rename compress tar file */
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv temp.tar.Z %s.tar.Z)",
		executable, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	/* restore rep, avg and var files */
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv %s rep.%s)", executable, Reptemp, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv %s avg.%s)", executable, Avgtemp, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	sprintf(cmd, "(cd $DGENESIS/usr/%s ; mv %s var.%s)", executable, Vartemp, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);
}




/****************************************************************************/
/*                            The main program                              */
/****************************************************************************/
main(argc, argv)
int argc;
char *argv[];
{
	FILE *fp;
	char line[100];		/* read buffer from GAosts	 */
	char cmd[120];
	int i = 1;
	int totalproc;		/* total processes allowed in GAhosts */



	/* process command line parameters */
	Input(argc, argv);

	if (Verboseflag)
		printf("\n\tDGENESIS Remote Execution Program\n\n");

	if (gethostname(hostname, 64) < 0)
		Error("Can't get my own name\n");

	/* get number of lines in GAhosts */
	lines = CountLines(GAhosts);

	/* allocate memory for HostTable */
	HostTable = (GAHOSTSTRUCT *) calloc(lines, sizeof(GAHOSTSTRUCT));
	if (HostTable == NULL)
		Error("rgo: can't allocate memory for HostTable");

	/* read data from GAhosts into HostTable */
	if ((fp = fopen(GAhosts, "r")) == NULL)
		Error("rgo: can't open GAhosts");

	totalproc = 0;
	for (i = 0; i < lines; i++) {
		ReadLine(fp, line, 100);
		sscanf(line, " %s %d", HostTable[i].host, &HostTable[i].processes);
		totalproc += HostTable[i].processes;
	}

	fclose(fp);


	/* execute main process */
	DoMain();

	/* execute other processes */
	DoRemote(totalproc);
	if (Verboseflag)
		printf("\nAll processes executed\n");


	/* wait for main process to exit */
	while ((fp = fopen(Endfile, "r")) == NULL)
		sleep(1);
	fclose(fp);
	unlink(Endfile);

	if (Verboseflag)
		printf("\nExperiment %s Finished\n", exp);

	/* copy outfiles from remote processors */
	CopyOutFiles();

	if (Verboseflag)
		printf("Making report\n");
	sprintf(cmd, "(cd $DGENESIS/usr/%s ; report -ct %s > rep.%s)", executable, exp, exp);
	if (Verboseflag)
		printf("Executing: %s\n", cmd);
	system(cmd);

	if (Compressflag)
        	Compress();

	return 0;
}

/*** end of file ***/
