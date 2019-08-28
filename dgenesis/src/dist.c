
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
 * file:        dist.c
 *
 * purpose:	Communication of parameters & link info
 *		Maintenance of Process and Link tables
 *
 */


#include <errno.h>
#include <sys/time.h>
#include "extern.h"


/**************************** global variables ****************************/

static struct sockaddr_in main_addr;	/* main process address	*/
static int timeout = 0;		/* timeout flag		*/
PROCESS *ProcTable;		/* process table, managed by main process */
extern int Sockets[];		/* sockets for receiving requests and migrants */






/*
 * fired when the timeout set in MakeProcessTable expires
 */
void timeout_handler()
{
	timeout = 1;
}



/*
 * waits for answers from remote processors and registers their addresses
 * and ports in ProcTable
 */
void MakeProcessTable()
{
	register int    i;	/* loop control */
	int             answers;/* from remote processes		 */
	int             opt;	/* set/clear non-blocking socket	 */
	int             status;	/* > 0 if Receive is succesful		 */
	char            msg[80];
	char 		hostaddr[20];
        char 		buffer[MAXMSG];		/* send/receive buffer	*/
	int		port;
	struct in_addr  addr;
	struct hostent  he, *pointer;
	FILE           *fp;



	/* allocate memory for Process table */
	ProcTable = (PROCESS *) calloc((unsigned)Processes, sizeof(PROCESS));
	if (ProcTable == NULL)
		Error("MakeProcessTable: Memory allocation failed for ProcTable");

	/* remote processes have 60 seconds to answer */
	timeout = 0;
	signal(SIGALRM, timeout_handler);
	alarm(60);


        /* set non-blocking io */
	opt = 1;
	if (ioctl(Sockets[0], FIONBIO, &opt) < 0)
		IOError("Can't set options");

	/* receive answers into ProcTable */
	answers = 1;

	while (!timeout && (answers < Processes)) {
		bzero(buffer, MAXMSG);
		status = Receive(Sockets[0], buffer, MAXMSG, (struct sockaddr_in *)NULL);
		if (status > 0){
		sscanf(buffer, "%s %d", hostaddr, &port);
		if (inet_addr(hostaddr) > 0) {
			ProcTable[answers].addr.sin_addr.s_addr = inet_addr(hostaddr);
			ProcTable[answers].addr.sin_port = htons(port);
			ProcTable[answers].addr.sin_family = AF_INET;
			answers++;

		}
		} else
			if (errno != EWOULDBLOCK)
				IOError("MakeProcessTable: Receive doesn't work");
	}

	/* clear non-blocking */
	opt = 0;
	if (ioctl(Sockets[0], FIONBIO, &opt) < 0)
		IOError("Can't set options");


	if (timeout && (answers < Processes)) {
		sprintf(msg, "MakeProcessTable: timeout reached with %d answers", answers);
		Error(msg);
	}


	/* the first process is main */

	ProcTable[0].addr.sin_addr.s_addr = inet_addr(gethostaddr());
	ProcTable[0].addr.sin_port = htons(FindPorts());
	ProcTable[0].addr.sin_family = AF_INET;

	/* write registered processes to a file */
	if ((fp = fopen(Regfile, "w")) == NULL)
		IOError("MakeProcessTable: can't open Regfile");

	fprintf(fp, "#\n# file: %s\n#\n", Regfile);

	for (i = 0; i < Processes; i++){
		addr.s_addr = ProcTable[i].addr.sin_addr.s_addr;
		pointer = gethostbyaddr((char *)&addr, sizeof(addr), AF_INET);
		if (pointer) bcopy((char *)pointer, (char *)&he, sizeof(he));
		fprintf(fp, "%d %s\n", i, he.h_name);
	}
	fclose(fp);

	/* send ACK */
	for (i = 1; i < Processes; i++){
		bzero(buffer, MAXMSG);
		sprintf(buffer, "%s %d", ACK, i);
		if (Send(Sockets[0], buffer, strlen(buffer), &ProcTable[i].addr) < 0)
			IOError("MakeProcessTable: can't send ACK");
	}

} /* MakeProcessTable */



/*
 * Put link data in ProcTable
 */
void MakeLinks()
{
	register int    i;	/* loop control	*/
	int		*PortUsage;
	int             id1, id2;	/* process IDs in Linkfile	 */
	int             migint;	/* Migration interval 	 */
	float		migrate;/* Migration rate 	 */
	char		*status;	/* indicates EOF in Linkfile	 */
	FILE           *fpProc;	/* fp to the Procfile	 */
	FILE           *fpLink;	/* fp to the Linkfile	 */
	char            msg[80];
	char 		line[100];
	LINKSTRUCT     *aux, *new;



	/* read the Procfile, fill in the pop field of the process struct */

	if ((fpProc = fopen(Procfile, "r")) == NULL) {
		sprintf(msg, "MakeLinks: can't open %s", Procfile);
		Error(msg);
	}

	i = 0;
	status = ReadLine(fpProc, line, 100);
	while (status != NULL && i < Processes){
		sscanf(line, " %*d %d", &ProcTable[i].pop);
		status = ReadLine(fpProc, line, 100);
		i++;
	}

	fclose(fpProc);


	/* allocate memory for port usage table */
	PortUsage = (int *) calloc((unsigned)Processes, sizeof(int));
	if (PortUsage == NULL)
		Error("MakeProcessTable: Memory allocation failed for PortUsage");


	/* read the Linkfile, create and fill process links */
	if ((fpLink = fopen(Linkfile, "r")) == NULL) {
		sprintf(msg, "MakeLinks: can't open %s", Linkfile);
		Error(msg);
	}

	status = ReadLine(fpLink, line, 100);
	sscanf(line, " %d %d %d %f", &id1, &id2, &migint, &migrate);
	while (status != NULL) {

		if (id1 > Processes || id2 > Processes || id1 < 0 || id2 < 0)
			Error("MakeLinks: Bad process id in linkfile");

		/* adds the link (send) */
		if (ProcTable[id1].links == NULL) {
			ProcTable[id1].links = (LINKSTRUCT *) malloc(sizeof(LINKSTRUCT));
			ProcTable[id1].links->next_link = NULL;
		} else {
			aux = ProcTable[id1].links;
			ProcTable[id1].links = (LINKSTRUCT *) malloc(sizeof(LINKSTRUCT));
			ProcTable[id1].links->next_link = aux;
		}


		/* put link data into the LINKSTRUCT */
		bcopy((char *) &ProcTable[id2].addr, (char *) &ProcTable[id1].links->addr, ADDRLEN);
		ProcTable[id1].links->addr.sin_port = htons( ntohs(ProcTable[id2].addr.sin_port) + (++PortUsage[id2]));
		ProcTable[id1].links->migration_int = migint;
		ProcTable[id1].links->migration_rate = migrate;
		ProcTable[id1].sendlinks++;
		ProcTable[id2].recvlinks++;

		/* migrants to send	*/
		ProcTable[id1].migsend += (int)(migrate * ProcTable[id1].pop);

		/* migrants to receive	 */
		ProcTable[id2].migrecv += (int)(migrate * ProcTable[id1].pop);

		/* adds linkrecv */
		if (ProcTable[id2].linkr == NULL){
			new  = ProcTable[id2].linkr = (LINKSTRUCT *) malloc(sizeof(LINKSTRUCT));
			new->next_link = NULL;
		} else {
			aux = ProcTable[id2].linkr;
			while (aux->next_link != NULL) aux = aux->next_link;
			new = (LINKSTRUCT *) malloc(sizeof(LINKSTRUCT));
			new->next_link = NULL;
			aux->next_link = new;
		}
		bcopy((char *) &ProcTable[id1].addr, (char *) &(new->addr), ADDRLEN);
		new->addr.sin_port = ProcTable[id1].addr.sin_port;
		new->migration_int = migint;
		new->migration_rate = migrate;

		status = ReadLine(fpLink, line, 100);
		sscanf(line, " %d %d %d %f", &id1, &id2, &migint, &migrate);
	}
	fclose(fpLink);


	/* main process data */

	Popsize = ProcTable[0].pop;
	Links = ProcTable[0].sendlinks;
	Linkrecv = ProcTable[0].recvlinks;
	Migrecv = ProcTable[0].migrecv;
	Migsend = ProcTable[0].migsend;

	Linktable = (LINKSTRUCT *) calloc((unsigned) Links, sizeof(LINKSTRUCT));
	if (Linktable == NULL)
		Error("MakeLinks: memory allocation failed for LinkTable");

	LinkRecTable = (LINKSTRUCT *) calloc((unsigned) Linkrecv, sizeof(LINKSTRUCT));
	if (Linktable == NULL)
		Error("MakeLinks: memory allocation failed for LinkTable");

	/* set main's links */
	aux = ProcTable[0].links;
	for (i = 0; i < Links; i++) {
		Linktable[i].in_use = 1;
		Linktable[i].addr.sin_family = AF_INET;
		Linktable[i].addr.sin_port = aux->addr.sin_port;
		Linktable[i].addr.sin_addr = aux->addr.sin_addr;
		Linktable[i].migration_int = aux->migration_int;
		Linktable[i].migration_rate = aux->migration_rate;

		aux = aux->next_link;
	}

	aux = ProcTable[0].linkr;
	for (i = 0; i < Linkrecv; i++) {
		LinkRecTable[i].in_use = 3;
		LinkRecTable[i].addr.sin_family = AF_INET;
		LinkRecTable[i].addr.sin_port = aux->addr.sin_port;
		LinkRecTable[i].addr.sin_addr = aux->addr.sin_addr;
		LinkRecTable[i].migration_int = aux->migration_int;
		LinkRecTable[i].migration_rate = aux->migration_rate;

		aux = aux->next_link;
	}


	free((char *)PortUsage);
} /* MakeLinks() */



/*
 * Prints Proctable contents
 */
void PrintLinks()
{
	int i, j;
        LINKSTRUCT     *aux;


	Trace("PrintLinks entered");

	if (Processes > 1){
	for (i = 0; i < Processes; i++) {
		printf("\n\n----- Process %d -----\n", i);
		printf("Population %d\n", ProcTable[i].pop);
		printf("address %s\n", inet_ntoa(ProcTable[i].addr.sin_addr));
		printf("port %d\n", ntohs(ProcTable[i].addr.sin_port));
		printf("Num of sendlinks %d\n", ProcTable[i].sendlinks);
		printf("Num of recvlinks %d\n", ProcTable[i].recvlinks);
		printf("Must receive %d individuals\n", ProcTable[i].migrecv);
		printf("Will send %d individuls\n", ProcTable[i].migsend);
		aux = ProcTable[i].links;
		printf("Send Links \n");
		for (j = 0; j < ProcTable[i].sendlinks; j++) {
			printf(LINK_FORMAT,
			       ntohs(aux->addr.sin_port), inet_ntoa(aux->addr.sin_addr),
			       aux->migration_int, aux->migration_rate);
			printf("\n");
			aux = aux->next_link;
		}
		aux = ProcTable[i].linkr;
		printf("\nReceive links\n");
		for (j = 0; j < ProcTable[i].recvlinks; j++) {
			printf(LINK_FORMAT,
			       ntohs(aux->addr.sin_port), inet_ntoa(aux->addr.sin_addr),
			       aux->migration_int, aux->migration_rate);
			printf("\n");
			aux = aux->next_link;
		}

	}
	}

	Trace("PrintLinks completed");
}



/*
 * Sends parameters to registered processes
 */
void SendParameters()
{
	register int i;
        char buffer[MAXMSG];		/* send/receive buffer	*/


	bzero(buffer, MAXMSG);
	sprintf(buffer, OUT_FORMAT, OUT_VARS);


	for (i = 1; i < Processes; i++)
		if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
			IOError("SendParameters: Send failed");

}



/*
 * Sends Popsize, # of links, # of migrants and neighbors' info
 */
void SendLinks()
{
	LINKSTRUCT     *aux;	/* pointer to next link	 */
	register int    i, j;	/* loop control	 */
	char buffer[MAXMSG];		/* send/receive buffer	*/


	for (i = 1; i < Processes; i++) {

		bzero(buffer, MAXMSG);

		/* population size, # of links, # of migrants to receive, # of migs to send */
		sprintf(buffer, "%d %d %d %d %d", ProcTable[i].pop, ProcTable[i].sendlinks, ProcTable[i].recvlinks,
			ProcTable[i].migrecv, ProcTable[i].migsend);
		if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
			IOError("SendLinks: can't send parameters");

		/* send sendlink info */
		bzero(buffer, MAXMSG);
		aux = ProcTable[i].links;
		for (j = 0; j < ProcTable[i].sendlinks; j++) {
			sprintf(&buffer[strlen(buffer)], LINK_FORMAT,
				ntohs(aux->addr.sin_port), inet_ntoa(aux->addr.sin_addr),
				aux->migration_int, aux->migration_rate);
			aux = aux->next_link;
		}
		if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
			IOError("SendLinks: can't send sendlinks");

		/* send recvlinks */
		bzero(buffer, MAXMSG);
		aux = ProcTable[i].linkr;
		for (j = 0; j < ProcTable[i].recvlinks; j++) {
			sprintf(&buffer[strlen(buffer)], LINK_FORMAT,
				ntohs(aux->addr.sin_port), inet_ntoa(aux->addr.sin_addr),
				aux->migration_int, aux->migration_rate);
			aux = aux->next_link;
		}
		if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
			IOError("SendLinks: can't send recvlinks");
	}

} /* SendLinks */




/*
 * Send template for floating point representation
 */
void SendTemplate()
{

	FILE           *fp;
	register int    i, j;	/* loop control */
	char            msg[80];/* error message */
        char 		buffer[MAXMSG];		/* send/receive buffer	*/
	unsigned long len = 0;

	Trace("SendTemplate entered\n");

	/* read template file */
	if ((fp = fopen(Templatefile, "r")) == NULL) {
		sprintf(msg, "SendTemplate: can't open %s", Templatefile);
		Error(msg);
	}
	fscanf(fp, "genes: %d ", &Genes);

	Gene = (GENESTRUCT *) calloc((unsigned) Genes, sizeof(GENESTRUCT));
	if (Gene == NULL)
		Error("SendTemplate: memory allocation failed for Gene");

	/* read in template file */
	for (i = 0; i < Genes; i++) {
		fscanf(fp, " gene %*d");
		fscanf(fp, " min: %lf", &Gene[i].min);
		fscanf(fp, " max: %lf", &Gene[i].max);
		fscanf(fp, " values: %lu", &Gene[i].values);
		fscanf(fp, " format: %s", Gene[i].format);
		Gene[i].bitlength = ilog2(Gene[i].values);
		Gene[i].incr = (Gene[i].max - Gene[i].min) /
			(Gene[i].values - 1);
		len += Gene[i].bitlength;
	}
	fclose(fp);

	if (len != Length) Error("Wrong genome length in template file");


	for (i = 1; i < Processes; i++){
		bzero(buffer, MAXMSG);
		sprintf(buffer, "%d ", Genes);
		if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
			IOError("SendTemplate: can't send");
		
		for (j=0; j < Genes; j++){
			bzero(buffer, MAXMSG);
			sprintf(buffer, "%lf %lf %lu %s %lf %d ", Gene[j].min,
			  Gene[j].max, Gene[j].values, Gene[j].format, Gene[j].incr,
			  Gene[j].bitlength);
		
			if (Send(Sockets[0], buffer, strlen(buffer), & ProcTable[i].addr) < 0)
				IOError("SendTemplate: can't send");
		}
	}


	Trace("SendTemplate completed\n");
}



/*
 * Register to main processor as a participant. Sends IP address and free port
 * found to main process.
 */
int Register(mainaddr, mainport)
char *mainaddr;
int mainport;
{
	char buffer[MAXMSG];		/* send/receive buffer	*/
	struct timeval tv;
	int id;


	main_addr.sin_family = AF_INET;
	main_addr.sin_addr.s_addr = inet_addr(mainaddr);
	main_addr.sin_port = htons(mainport);

	bzero(buffer, MAXMSG);

	sprintf(buffer, "%s %d\n", gethostaddr(), FindPorts());

	if (Send(Sockets[0], buffer, strlen(buffer), &main_addr) < 0)
		IOError("Register: can't send");

	/* receive ack within 60 seconds or die */
	tv.tv_sec = 60;
	tv.tv_usec = 0;
	if (RecvTO(Sockets[0], buffer, MAXMSG, &main_addr, &tv) < 0)
		exit(0); /* I'm not needed */

	sscanf(buffer, "%*s %d", &id);
	return id;
}



/*
 * blocks until parameters are received
 */
void ReceiveParameters()
{

	char buffer[MAXMSG];		/* send/receive buffer	*/

	/* wait for parameters, get main's address */
	bzero(buffer, MAXMSG);
	if (Receive(Sockets[0], buffer, MAXMSG, &main_addr) < 0)
		IOError("ReceiveParameters: can't recv");
	sscanf(buffer, IN_FORMAT, IN_VARS);

}



/*
 * print a migration table, every process has two (Linktable, LinkRecTable)
 */
PrintTable(table, size)
LINKSTRUCT *table;
int size;
{
	int i;

	for (i=0; i<size; i++){
		printf(LINK_FORMAT, ntohs(table->addr.sin_port), inet_ntoa(table->addr.sin_addr),
		table->migration_int, table->migration_rate);
		printf("\n");

		table ++;
	}
}



/*
 * Receives Popsize, # of links (Links), # of migrants (Migrecv) and link
 * info (Linktable)
 */
void ReceiveLinks()
{
	register int    i, j;	/* loop control 	 */
	char           *aux;
	int             port;
	char            hostaddr[16];
	int             migint;
	float           migrate;
        char 		buffer[MAXMSG];		/* send/receive buffer	*/


	/* receive parameters */
	bzero(buffer, MAXMSG);
	if (Receive(Sockets[0], buffer, MAXMSG, &main_addr) < 0)
		IOError("ReceiveLinks: can't receive parameters");

	sscanf(buffer, " %d %d %d %d %d", &Popsize, &Links, &Linkrecv, &Migrecv, &Migsend);

	Linktable = (LINKSTRUCT *) calloc((unsigned) Links, sizeof(LINKSTRUCT));
	if (Linktable == NULL)
		Error("ReceiveLinks: memory allocation failed for LinkTable");

	LinkRecTable = (LINKSTRUCT *) calloc((unsigned) Linkrecv, sizeof(LINKSTRUCT));
	if (LinkRecTable == NULL)
		Error("ReceiveLinks: memory allocation failed for LinkRecTable");

	/* receive sendlink data */
	bzero(buffer, MAXMSG);
	if(Receive(Sockets[0], buffer, MAXMSG, &main_addr) < 0)
		IOError("ReceiveLinks: can't receive links");

	aux = buffer;
	for (i = 0; i < Links; i++) {
		sscanf(aux, LINK_FORMAT, &port, hostaddr, &migint, &migrate);
		Linktable[i].in_use = 3;
		Linktable[i].addr.sin_family = AF_INET;
		Linktable[i].addr.sin_port = htons(port);
		Linktable[i].addr.sin_addr.s_addr = inet_addr(hostaddr);
		Linktable[i].migration_int = migint;
		Linktable[i].migration_rate = migrate;

		/* scan string for next link */
		for (j = 0; j < 4; j++) {
			aux++;
			aux = strchr(aux, ' ');
		}
	}

	/* receive recvlink data */
	bzero(buffer, MAXMSG);
	if(Receive(Sockets[0], buffer, MAXMSG, & main_addr) < 0)
		IOError("ReceiveLinks: can't receive links");

	aux = buffer;
	for (i = 0; i < Linkrecv; i++) {
		sscanf(aux, LINK_FORMAT, &port, hostaddr, &migint, &migrate);
		LinkRecTable[i].in_use = 3;
		LinkRecTable[i].addr.sin_family = AF_INET;
		LinkRecTable[i].addr.sin_port = htons(port);
		LinkRecTable[i].addr.sin_addr.s_addr = inet_addr(hostaddr);
		LinkRecTable[i].migration_int = migint;
		LinkRecTable[i].migration_rate = migrate;

		/* scan string for next link */
		for (j = 0; j < 4; j++) {
			aux++;
			aux = strchr(aux, ' ');
		}
	}
}





/*
 * Receives template for floating point representation
 */
void ReceiveTemplate()
{
	register int    i;	/* loop control */
        char 		buffer[MAXMSG];		/* send/receive buffer	*/

	Trace("ReceiveTemplate entered\n");

	/* receive template */
	bzero(buffer, MAXMSG);
	if (Receive(Sockets[0], buffer, MAXMSG, &main_addr) < 0)
		IOError("Can't recv template");
	sscanf(buffer, "%d", &Genes);

	Gene = (GENESTRUCT *) calloc((unsigned) Genes, sizeof(GENESTRUCT));
	if (Gene == NULL)
		Error("SendTemplate: memory allocation failed for Gene");

	for (i = 0; i < Genes; i++) {
		bzero(buffer, MAXMSG);
		if (Receive(Sockets[0], buffer, MAXMSG, &main_addr) < 0)
			IOError("Can't recv template");

		sscanf(buffer, " %lf %lf %lu %s %lf %d", &Gene[i].min, &Gene[i].max,
		       &Gene[i].values, Gene[i].format, &Gene[i].incr,
		       &Gene[i].bitlength);
		if (Traceflag)
			printf("Gene rep: %lf %lf %lu %s %lf %d\n", Gene[i].min, Gene[i].max,
			       Gene[i].values, Gene[i].format, Gene[i].incr,
			       Gene[i].bitlength);

	}

	Trace("ReceiveTemplate completed\n");
}


/*
 * main sends a message at the beginning of each experiment
 */
void SendStart()
{
	register int i;

	Trace("SendStart entered");

	for(i=1; i < Processes; i++)
		if (Send(Sockets[0], STARTEXP, strlen(STARTEXP), &ProcTable[i].addr) < 0)
			IOError("SendStart: can't send");

	Trace("SendStart completed");
}


/*
 * everyone waits for a message from main at the beginning of each experiment
 */
void RecvStart()
{
	struct sockaddr_in aux_addr;
	char buffer[MAXMSG];		/* send/receive buffer	*/

	Trace("RecvStart entered");

	bzero(buffer, MAXMSG);

	if (Receive(Sockets[0], buffer, MAXMSG, &aux_addr) < 0)
		IOError("RecvStart: Can't start experiment");

	if (strcmp(buffer, STARTEXP)) {
        	/* something very strange is happening */
		printf("I am not needed, bye!\n");
		exit(0);
	}

	Trace("RecvStart completed");
}


/*
 * everyone send a message to main at the end of each experiment
 */
void SendEnd()
{
	Trace("SendEnd entered");

	if (Send(Sockets[0], ENDEXP, strlen(ENDEXP), &main_addr) < 0)
		IOError("SendEnd: Send doesn't work");

	Trace("SendEnd completed");
}


/*
 * main receives a message from everyone else at the end of each experiment
 */
int RecvEnd()
{
	struct sockaddr_in aux_addr;	/* sender of the message */
	int answers;			/* counts received answers */
	int status;			/* return value from select */
	struct timeval tv;		/* timeout value	*/
	char buffer[MAXMSG];		/* send/receive buffer	*/

	Trace("RecvEnd entered");

	/* receive answers */
	answers = 1;

	while (answers < Processes) {
		bzero(buffer, MAXMSG);
		status = Receive(Sockets[0], buffer, MAXMSG, &aux_addr);
		if (status < 0) answers++; /* if timeout assume it has finished !! */
		if (strcmp(buffer, ENDEXP)==0){
			answers++;
		}
	}

	Trace("RecvEnd completed");
}


/*
 * everyone (including main) sends a message at the experiment to the
 * processes in LinkTable, so that they don't wait for any more migrants.
 */
void SendByeLinks()
{
	register int i;

	for (i=0; i<Links; i++)
		if (Send(Sockets[0], ENDEXP, strlen(ENDEXP), &Linktable[i].addr) < 0)
			IOError("SendByeLinks can't send");

}


/*** end of file ***/
