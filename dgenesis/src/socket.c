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
 *  file:       socket.c
 *
 *  purpose:    socket interface functions
 *
 */

#include <errno.h>
#include "extern.h"


int Sockets[MAXSOCKETS];	/* sockets for receiving requests and migrants */



/*
 * If succesful, returns the IP address of the host in dotted decimal
 * notation. Otherwise returns NULL.
 */
char *gethostaddr()
{
	char myname[33];
	static char myaddr[20];
	struct hostent he, *pointer;
	int okflag = 0;

	if (gethostname(myname, 32) < 0)
		IOError("gethostaddr: Can't get my own name");
	else {
		pointer = gethostbyname(myname);
		if (pointer) {
			bcopy((char *) pointer, (char *) &he, sizeof(he));
			strcpy(myaddr, inet_ntoa(*(struct in_addr *) * he.h_addr_list));
			okflag = 1;
		} else
			IOError("gethostaddr: can't do gethostbyname");
	}

	if (okflag)
		return myaddr;
	else
		return NULL;
}



/*
 * Waits for data from a socket with a specified timeout.
 * Return values:
 *			-1 : timeout expired with no data available
 *			>0 : number of bytes received
 */
int RecvTO(sock, buffer, length, from, tv)
int sock;			/* recv's usual parameters */
char *buffer;
int length;
struct sockaddr_in *from;
struct timeval *tv;		/* desired timeout */
{
	fd_set rfds;		/* set of read file descriptors to check */
	int nfds;		/* # of fd's this process is allowed to have	 */
	int status;		/* return value from recv, status */

	nfds = getdtablesize();
	FD_ZERO(&rfds);
	FD_SET(sock, &rfds);

	do {
		status = select(nfds, &rfds, (fd_set *) 0, (fd_set *) 0, tv);
	} while (status == -1 && errno == EINTR);

	if (status > 0) {
		status = Receive(sock, buffer, length, from);
		if (status < 0)
			IOError("RecvTO: Receive doesn't work");
		return status;	/* ok */
	} else
		return -1;	/* timeout reached, no data available */
}



/*
 * receive data from a socket
 */
int Receive(sockfd, buffer, length, from)
int sockfd;			/* recv's usual parameters */
char *buffer;
int length;
struct sockaddr_in *from;
{
	int status;
	int addrlen;

	addrlen = ADDRLEN;

	bzero(buffer, length);
	do {
		status = recvfrom(sockfd, buffer, length, 0, (struct sockaddr *) from, &addrlen);
	} while (status < 0 && errno == EINTR);

	return status;
}



/*
 * send data
 */
int Send(sockfd, buffer, length, to)
int sockfd;
char *buffer;
int length;
struct sockaddr_in *to;
{
	int status;

	status = sendto(sockfd, buffer, length, 0, (struct sockaddr *) to, ADDRLEN);
	return status;
}



/*
 * clear the buffer asociated with 'sockfd'
 */
void ClearSocket(sockfd)
int sockfd;
{
	int opt, status;
	char buffer[MAXMSG];

	/* set non-blocking io */
	opt = 1;
	if (ioctl(sockfd, FIONBIO, (char *) &opt) < 0)
		IOError("ClearSocket: can't set options");

	do{
		status = Receive(sockfd, buffer, MAXMSG, (struct sockaddr_in *) NULL);
	} while (status > 0);


	/* clear non-blocking io */
	opt = 0;
	if (ioctl(sockfd, FIONBIO, (char *) &opt) < 0)
		IOError("ClearSocket: can't set options");
}



/*
 * Opens a socket with a receive buffer of size 'bufsiz'
 */
int OpenSocket(bufsiz)
int bufsiz;
{
	int sockfd;
	int opt;
	int size;
	char msg[80];

	if ((sockfd = socket(AF_INET, SOCK_DGRAM, 0)) < 0) {
		sprintf(msg, "Process %d: can't open socket", My_id);
		IOError(msg);
	}
	opt = 1;
	if (setsockopt(sockfd, SOL_SOCKET, SO_REUSEADDR, (char *) &opt, sizeof(int)))
		IOError("OpenSocket: can't set options");

	size = bufsiz;
	if (setsockopt(sockfd, SOL_SOCKET, SO_RCVBUF, (char *) &size, sizeof(int)))
		IOError("OpenSocket: can't set RCVBUF\n");

	return sockfd;
}




/*
 * Initialize socket interface for DGENESIS (Sockets[])
 */
void InitSocket()
{
	register int i;
	FILE *fp;

	/* initialize address structures */
	for (i = 0; i < MAXSOCKETS; i++)
		Sockets[i] = OpenSocket(32768);

	if (!My_id) {
		fp = fopen(Infofile, "w");
		if (fp == NULL)
			Error("InitSocket: can't open Infofile");

		fprintf(fp, "%s %d\n", gethostaddr(), FindPorts());
		fclose(fp);
	}
}



/*
 * finds MAXSOCKETS free contigous ports, and binds them to Sockets[]
 */
int FindPorts()
{
	int i, j, iniport, trial;
	char flagbind;		/* succesful binding of all ports */
	struct sockaddr_in proc_addr;
	static char firstflag = 1;
	static int port;

	trial = 0;

	if (firstflag) {
		firstflag = 0;
		flagbind = 0;
		while (!flagbind) {
			iniport = 5000 + trial;
			flagbind = 1;
			for (i = 0; i < MAXSOCKETS && flagbind; i++) {
				bzero((char *) &proc_addr, ADDRLEN);
				proc_addr.sin_family = AF_INET;
				proc_addr.sin_addr.s_addr = htonl(INADDR_ANY);
				proc_addr.sin_port = htons(iniport + i);
				if (bind(Sockets[i], (struct sockaddr *) & proc_addr, ADDRLEN) < 0) {
					/* if bind fails, close existing sockets,  */
					/* create new sockets, and clear flagbind    */
					for (j = 0; j <= i; j++) {
						close(Sockets[j]);
						Sockets[j] = OpenSocket(32768);
					}
					flagbind = 0;
				}
				trial++;
			}
		}
		port = iniport;
	}
	return port;
}


/*** end of file ***/
