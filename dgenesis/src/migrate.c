
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
 *  file:       migrate.c
 *
 *  purpose:	migration of individuals between processes
 *
 */


#include <sys/time.h>
#include "extern.h"

extern int FirstMigrant, LastMigrant;
extern int Sockets[];



/*
 * copy the best individuals from the population to Migset
 */
void MakeMigset()
{
	SortPopulation(New);
	bcopy((char *) New, (char *) Migset, (int) (sizeof(STRUCTURE) * Migsend));
}


/*
 * Sends a migration header with number of migrants, then sends migrants
 */
void SendMigrants(link)
int link;
{
	register int i;		/* loop control	 */
	int migrants;		/* number of individuals to migrate	 */
	char buffer[MAXMSG];	/* send / receive buffer */

	Trace("SendMigrants entered");

	if (!Linktable[link].in_use)
		return;

	migrants = (int) (Linktable[link].migration_rate * Popsize);

	/* sends migration header */
	bzero(buffer, MAXMSG);
	sprintf(buffer, "%s %d %d", MIGHEAD, migrants, Gen);
	if (Send(Sockets[0], buffer, strlen(buffer), &Linktable[link].addr) < 0)
		IOError("SendBest: can't send request");


	/* sends migrants */
	for (i = 0; i < migrants; i++) {
		bzero(buffer, MAXMSG);
		Unpack(Migset[i].Gene, Bitstring, Length);
		sprintf(buffer, "%s %lf", Bitstring, Migset[i].Perf);
		if (Send(Sockets[0], buffer, strlen(buffer), &Linktable[link].addr) < 0)
			IOError("SendBest: can't send migrants");

	}

	Trace("SendMigrants completed");
}




/*
 * Receives migrants from any process ** asynchronously **
 * Return value: number of received migrants.
 */
int ReceiveMigrantsAsync()
{
	register int i;		/* loop control	 */
	fd_set rfds;		/* candidate links */
	int nfds;		/* number of file descriptors (for select) */
	struct timeval tv;	/* timeout for select */
	int status;		/* return value from select, recv */
	int nNewMigrants = 0;	/* migrants received */

	Trace("ReceiveMigrantsAsync entered");

	nfds = getdtablesize();
	FD_ZERO(&rfds);
	for (i = 1; i < Linkrecv; i++)
		FD_SET(Sockets[i], &rfds);

	tv.tv_sec = 0;
	tv.tv_usec = 0;
	status = select(nfds, &rfds, (fd_set *) 0, (fd_set *) 0, &tv);
	if (status > 0)
		for (i = 1; i < Linkrecv; i++)
			if (FD_ISSET(Sockets[i], &rfds))
				nNewMigrants += ReceiveIndividuals(i - 1);


	Trace("ReceiveMigrantsAsync completed");

	return nNewMigrants;
}



/*
 * Receives migrants from any process ** SYNCHRONOUSLY **
 * Return value: number of received migrants.
 */
int ReceiveMigrantsSync()
{
	int nNewMigrants;	/* migrants received */
	int status, retry, cont, total, sum, j;
	fd_set migset;          /* set of active links */
	struct timeval tv;	/* better than polling */
	char flag[MAXSOCKETS];	/* true if something was received from each socket */

	Trace("ReceiveMigrantsSync entered");

	nNewMigrants = 0;
	tv.tv_sec = 0;
	tv.tv_usec = 500000;
	sum = 0;
	retry = 0;
	cont = total = 0;
	for (j = 0; j < MAXSOCKETS; j++)
		flag[j] = 0;

        /* total = number of active links */
	for (j = 0; j < Linkrecv; j++)
		if ((Gen % LinkRecTable[j].migration_int == 0) && LinkRecTable[j].in_use)
			total++;

	do {
        	/* create the set of candidate links */
		FD_ZERO(&migset);
		for (j = 0; j < Linkrecv; j++)
			if ((Gen % LinkRecTable[j].migration_int == 0) &&
			    !flag[j] && LinkRecTable[j].in_use) {
				FD_SET(Sockets[j + 1], &migset);
			}

		/* wait to receive something */
		status = select(getdtablesize(), &migset, NULL, NULL, &tv);
		if (status < 0)
			perror("select ");
		else {
                	/* receive the migrants */
			for (j = 0; j < Linkrecv; j++)
				if (FD_ISSET(Sockets[j + 1], &migset)) {
					flag[j] = 1;
					nNewMigrants += ReceiveIndividuals(j);
				}
		}

                /* if retry == 1, we have to wait for more migrants */
		cont = 0;
		for (j = 0; j < Linkrecv; j++)
			if (flag[j])
				cont++;
		if (cont == total)
			retry = 0;
		else
			retry = 1;

	} while (retry);

	Trace("ReceiveMigrantsSync completed");

	return nNewMigrants;
}

/*
 * Receives individuals from 'active' process into 'Migrants' queue
 * Return value : the number of received migrants.
 */
int ReceiveIndividuals(active)
int active;			/* active link from LinkRecTable */
{
	register int i;		/* loop control */
	struct sockaddr_in aux_addr;
	int status;		/* return value from select */
	int remgen;		/* remote generation */
	int nMigrantsRecv = 0;	/* migrants received */
	int nMigrants = 0;	/* migrants sent by remote process (from mig header) */
	char buffer[MAXMSG];
	struct timeval tv;	/*** now innecesary!! ***/

	Trace("ReceiveIndividuals entered");

	if (active < 0 || active > Linkrecv - 1)
		Error("ReceiveIndividuals: bad link number");

	/* is the link still active? */
	if (!LinkRecTable[active].in_use)
		return 0;

	/* receive a message: header or ENDEXP */
	bzero(buffer, MAXMSG);
	status = RecvTO(Sockets[active + 1], buffer, MAXMSG, &aux_addr, NULL);
	if (status < 0) {
		perror("timeout: ");
		printf("(%d %d)TIMEOUT!! esperando a %s %d\n", My_id, FindPorts(), inet_ntoa(LinkRecTable[active].addr.sin_addr),
		       ntohs(LinkRecTable[active].addr.sin_port));
		return 0;
	}

        /* did I receive the message from the right process? */
	if (strcmp(inet_ntoa(aux_addr.sin_addr), inet_ntoa(LinkRecTable[active].addr.sin_addr)) ||
	    aux_addr.sin_port != LinkRecTable[active].addr.sin_port) {
		printf("Link %d wrong %s %d vs. %s %d\n", active,
		       inet_ntoa(aux_addr.sin_addr), ntohs(aux_addr.sin_port),
		       inet_ntoa(LinkRecTable[active].addr.sin_addr), ntohs(LinkRecTable[active].addr.sin_port));
	}

        /* did I receive ENDEXP?, deactivate the link if true */
	if (strstr(buffer, ENDEXP)) {
		LinkRecTable[active].in_use = 0;
		return 0;
	}

        /* received a header, get the number of migrants and their gen */
	if (strstr(buffer, MIGHEAD)) {
		sscanf(buffer, "%*s %d %d", &nMigrants, &remgen);
	} else {
		printf("Should received MIGHEAD: \n%s\n", buffer);
		return 0;
	}

        /* migrants from the right generation? */
	if (Syncflag && Gen != remgen) {
		printf("Should receive migrants from gen %d, received %d\n", Gen, remgen);
		printf("%s\n", buffer);
		return 0;
	}
	tv.tv_sec = 100;
	tv.tv_usec = 0;

	/* copy arriving migrants to Migrants array */
	for (i = 0; i < nMigrants; i++) {
        	if (LastMigrant == Migrecv)
			printf("ReceiveIndividuals: migrants buffer full\n");

		bzero(buffer, MAXMSG);
		if (RecvTO(Sockets[active + 1], buffer, MAXMSG, &aux_addr, NULL) < 0)
			continue;
		sscanf(buffer, "%s %lf", Bitstring, &Migrants[LastMigrant].Perf);
		Pack(Bitstring, Migrants[LastMigrant].Gene, Length);
		Migrants[LastMigrant].Needs_evaluation = 0;
		nMigrantsRecv++;
		LastMigrant++;
	}


	Trace("ReceiveIndividuals completed");

	return nMigrantsRecv;
}



/*
 * Appends migrants to 'New' population, called by generate() after each
 * generation is new migrants have arrived
 */
void AppendMigrants(nNewMigrants)
int nNewMigrants;
{
	register int i;

        if (nNewMigrants != LastMigrant)
        	printf("AppendMigrants: something strange is going on\n");

	/* append the migrants to New population */
	for (i = 0; i < nNewMigrants; i++) {
               	bcopy((char *)&Migrants[i].Gene, (char *)&New[Popsize+i].Gene, Length);
               	New[Popsize+i].Perf = Migrants[i].Perf;
                New[Popsize+i].Needs_evaluation = 0;
	}

        LastMigrant = 0;
}


/*** end of file ***/
