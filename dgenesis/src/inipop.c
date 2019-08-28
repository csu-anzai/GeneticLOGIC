
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
 *  file:	inipop.c
 *
 *  purpose:	Communication of initial structures
 *
 */

#include "extern.h"
#define ENDPOP "end_of_initial_pop"

extern int Sockets[];			/* endpoint for communications	 */
extern PROCESS *ProcTable;		/* process table, managed by main process */



/*
 * read next structure from 'fp'
 */
char *ReadStructure(fp, bitstring)
FILE *fp;
char *bitstring;
{
        char *status;
        char line[200];
	int j;

	if (Floatflag){
		for (j = 0; j < Genes && status != NULL; j++) {
			status = ReadLine(fp, line, 200);
			sscanf(line, "%lf", &Vector[j]);
		}
                StringRep(Vector, bitstring, Genes);
	} else {
		status = ReadLine(fp, line, 200);
		sscanf(line, "%s", bitstring);
	}

        return status;
}



/*
 * Send initial structures (read from Initfile)
 * returns number of main's initial structs
 */
int SendPop() 
{
	register int    i, j;           /* loop control	 */
        int             nStructures;    /* # of structs in Initfile     */
        int             pop;            /* # of structs for each process*/
	int 		returnvalue;
	FILE           *fp;             /* pointer to Initfile          */
	char msg[40];                   /* error message */

	Trace("SendPop entered");

        nStructures = CountLines(Initfile);

        /* open Initfile */
	if ((fp = fopen(Initfile, "r")) == NULL) {
		sprintf(msg, "SendPop: can't open %s", Initfile);
		Error(msg);
	}

        /* main process */
        if (Propiniflag)
                pop = nStructures / ProcTable[0].pop;
        else
                pop = nStructures / Processes;

         for (j = 0; j < pop; j++){
                ReadStructure(fp, Bitstring);
                Pack(Bitstring, New[i].Gene, Length);
                New[i].Needs_evaluation = 1;
         }
         returnvalue = pop;

         /* send initial structures to other processes */
         for (i = 1; i < Processes; i++){
                if (Propiniflag)
                        pop = nStructures / ProcTable[i].pop;
                else
                        pop = nStructures / Processes;

                for (j = 0; j < pop; j++){
                        ReadStructure(fp, Bitstring);
        	        if (Send(Sockets[0], Bitstring, Length, &ProcTable[i%Processes].addr) < 0)
				IOError("SendPop can't send");
                }
         }

	/* send end of population message */
	for (j = 1; j < Processes; j++)
		if (Send(Sockets[0], ENDPOP, strlen(ENDPOP), &ProcTable[j].addr) < 0)
			IOError("SendPop can't send end-of-pop");

	fclose(fp);

	Trace("SendPop completed");
	return returnvalue;

}



/*
 * Receives initial structures
 * returns number of structs received
 */
int ReceivePop()
{
	register int    i;
	char            buffer[MAXMSG];   /* send/receive buffer        */
	int             status;	          /* indicates last structure   */

	Trace("ReceivePop entered");

	bzero(buffer, MAXMSG);
	recvfrom(Sockets[0], buffer, MAXMSG, 0, (struct sockaddr *) NULL, 0);

	i = 0;
	status = strcmp(buffer, ENDPOP);
	while (status != 0) {
		Pack(buffer, New[i].Gene, Length);
		New[i].Needs_evaluation = 1;
		i++;
		if (Receive(Sockets[0], buffer, MAXMSG, (struct sockaddr_in *) NULL) < 0)
			IOError("ReceivePop Can't receive");
		status = strcmp(buffer, ENDPOP);
	}

	Trace("ReceivePop completed");
	return i;
}

/*** end of file ***/

