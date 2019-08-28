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
 * file:        inet.h
 *
 * purpose:	include files for socket interface, constant definitions
 *
 */

#ifndef inet_h
#define inet_h

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#define MAXMSG	1000
#define MAXSOCKETS 10

/* size of Internet address structure used in socket functions */
#define ADDRLEN	(sizeof(struct sockaddr_in))

#define MIGHEAD  "migration_header"
#define ACK      "you_are_selected_to_participate"
#define STARTEXP "start_experiment"
#define ENDEXP   "end_of_experiment"

#endif

