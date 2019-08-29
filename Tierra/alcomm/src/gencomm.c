/*
* gencomm.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/


#include "alcommp.h"

#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#ifdef ultrix
char * strdup(char *str_in);
#endif


/*
* ALGetHostAddress()
*
* params
*   sHostname	string containing hostname to resolve
*   pAddr	ptr to u_long into which resolved address should go
*
* returns
*   ALsOK	success
*   ALsLOSE	couldn't resolve name into address
*
*   Resolves a string assumed to contain an ascii hostname into a 4-byte
* internet address in network byte order.
*
*/

ALtStatus ALGetHostAddress( sHostname, pAddr )
  char *	sHostname;
  u_long *	pAddr;
{
  char *		sInNISDomain;	/* NIS domain; used with NIS stuff */
  char *		sAsciiAddr;	/* ascii internet (dot) address */
  int			iAddrLen;	/* length of addr from yp_match */
  char *		pC;
  register int		iStat = ALsLOSE;

  	/* get default NIS domain to pass to yp_match */
  if ( yp_get_default_domain( &sInNISDomain ) == 0 ) {

      	/*   try using yp_match to fetch the address of hostname in    */
	/* Internet dot format.  If it doesn't work, try gethostbyname */
    if ( yp_match( sInNISDomain, "hosts.byname", sHostname, strlen(sHostname),
		   &sAsciiAddr, &iAddrLen ) == 0 ) {

	/* isolate actual address */
      for ( pC = sAsciiAddr; !isspace( *pC ); pC++ )
	/* NULL */ ;
      *pC = '\0';
      *pAddr = inet_addr( sAsciiAddr );
      ALFree( sAsciiAddr );
      iStat = ALsOK;
    }
  }

  	/*   If NIS didn't work, try gethostbyname().  In order for  */
	/* resolver routines to work properly, /etc/resolv.conf must */
	/* be set up correctly.  See resolv.conf(5).		     */

  if ( iStat != ALsOK ) {
    struct hostent *	pH;
    
    if (( pH = gethostbyname( sHostname )) != NULL ) {
      *pAddr = (u_long)*((u_long *)pH->h_addr_list[0]);
      iStat = ALsOK;
    }
  }

  return iStat;

} /* end ALGetHostAddress() */


/*
* ALGetHostnameFromAddress()
*
* params
*   ulAddr	u_long address to resolve into a hostname
*   psHostname	ptr to unallocated char *
*
* returns
*   ALsOK	success
*   ALsLOSE	couldn't resolve address into name
*   ALsNOMEM	hostname buffer alloc failed
*
*   Resolves a 4-byte internet address in network byte order into an ascii
* hostname.  psHostname is a char **; on successful return, psHostname will
* point to a newly allocated string holding the ascii hostname.
*
*/

ALtStatus ALGetHostnameFromAddress( ulAddr, psHostname )
  u_long	ulAddr;
  char **	psHostname;
{
  char *		sInNISDomain;	/* NIS domain; used with NIS stuff */
  char *		sAsciiAddr;	/* ascii internet (dot) address */
  char *		sNISReturn;	/* return string */
  int			iRetLen;	/* length of return from yp_match */
  char *		pC;
  register int		iStat = ALsLOSE;
  struct in_addr 	t_in_addr;
#ifdef __sgi
  /* BROKEN t_in_addr = (struct in_addr *)&ulAddr;
  sAsciiAddr = inet_ntoa( t_in_addr ); */
  exit(-666);
#else
  sAsciiAddr = inet_ntoa( (struct in_addr *)&ulAddr );
#endif

  	/* get default NIS domain to pass to yp_match */
  if ( yp_get_default_domain( &sInNISDomain ) == 0 ) {

    	/* try using yp_match to fetch the name assoc'd with the given addr */
    if ( yp_match( sInNISDomain, "hosts.byaddr", sAsciiAddr,
		   strlen(sAsciiAddr), &sNISReturn, &iRetLen ) == 0 ) {

      pC = strtok( sNISReturn, " \t" );
      if (( *psHostname = strdup( strtok( NULL, " \t" ) )) == NULL )
	iStat = ALsNOMEM;
      else {
	ALFree( sNISReturn );
	iStat = ALsOK;
      }
    }
  }

	/*   If NIS didn't work, try gethostbyaddr().  In order for  */
	/* resolver routines to work properly, /etc/resolv.conf must */
  	/* be set up correctly.  See resolv.conf(5).		     */

  if ( iStat != ALsOK ) {
    struct hostent *	pH;

    if (( pH = gethostbyaddr( (char *)&ulAddr, sizeof(u_long), AF_INET )
	) != NULL ) {
      if (( *psHostname = strdup( pH->h_name )) == NULL )
	iStat = ALsNOMEM;
      else
	iStat = ALsOK;
    }
  }
   
  return iStat;

} /* end ALGetHostnameFromAddress() */


/*
* ALInterpretHostAddress()
*
* params
*   sGiven	string to resolve
*   psHostname	ptr to unallocated char * to receive addr of hostname string
*   pAddr	ptr to u_long to receive host address
*
* returns
*   ALsOK	success
*   ALsLOSE	can't resolve sGiven as a hostname or an inet (dot) address
*   ALsNOMEM	hostname buffer alloc failed
*
*   Resolves the string pGiven into a hostname and a 4-byte internet address
* in network byte order.  pGiven may be either an actual hostname or an ascii
* numerical (dot) address.  On success, psHostname points to a newly allocated
* string.
*
*/

ALtStatus ALInterpretHostAddress( sGiven, psHostname, pAddr )
  char *	sGiven;
  char **	psHostname;
  u_long *	pAddr;
{
  register int		iRet;
  register int		iStat = ALsLOSE;

	/* See if we've been handed a real hostname */

  if (( iRet = ALGetHostAddress( sGiven, pAddr )) == ALsOK ) {

    if (( *psHostname = strdup( sGiven )) == NULL )
      iStat = ALsNOMEM;
    else
      iStat = ALsOK;

  } else if (( iRet = ALGetHostnameFromAddress( inet_addr(sGiven), psHostname )
	     ) == ALsOK ) {

    *pAddr = inet_addr( sGiven );
    iStat = ALsOK;
    
  } else if ( iRet == ALsNOMEM )

    iStat = ALsNOMEM;

  return iStat;

} /* end ALInterpretHostAddress() */


/*
* ALTCPConnectToPort()
*
* params
*
* returns
*
*/

ALtStatus ALTCPConnectToPort( pSoc, ulAddr, iPort )
  int *		pSoc;
  u_long	ulAddr;
  int		iPort;
{
  register int		iS;
  register int		iRet;
  register int		iCount;
  struct sockaddr_in	INAddr;
  register int		iStat = ALsLOSE;

  INAddr.sin_family = AF_INET;
  INAddr.sin_addr.s_addr = htonl( ulAddr );
  INAddr.sin_port = htons( iPort );

  for ( iCount = 0; iCount < AL_MAX_CONNATTEMPTS; iCount++ ) {

    if (( iS = socket( AF_INET, SOCK_STREAM, 0 )) <= 0 )
      break;

    iRet = connect( iS, (struct sockaddr *)&INAddr, sizeof( INAddr ));
    if ( iRet < 0 )
      ALGripeErr( "ALTCPConnectToPort, connect" );
    else {
      *pSoc = iS;
      iStat = ALsOK;
      break;
    }

    close( iS );
    sleep( 2 );		/* likely not needed at all */
  }

  return iStat;

} /* end ALTCPConnectToPort() */


/*
* ALSetupSocket()
*
* params
*
* returns
*
*/

ALtStatus ALSetupSocket( pSoc, iSockType, pPort )
  int *		pSoc;
  int		iSockType;
  int *		pPort;
{
  register int		iS;
  struct sockaddr_in	INAddr;
  int			iAddrLen;
  int			iOn = 1;
  int			iOnLen = sizeof( int );
  struct linger		SockLinger;
  register int		iStat = ALsLOSE;

  if (( iS = socket( AF_INET, iSockType, 0 )) >= 0 ) {
#ifdef OUT_FOR_NOW
#ifdef __sgi
    (void) setsockopt( iS, SOL_SOCKET, SO_REUSEADDR, &iOn, iOnLen );
#else
    (void) setsockopt( iS, SOL_SOCKET, SO_REUSEADDR, &iOn, &iOnLen );
#endif
#endif
    (void) setsockopt( iS, SOL_SOCKET, SO_REUSEADDR, &iOn, iOnLen );

    SockLinger.l_onoff = SockLinger.l_linger = 0;
    iOnLen = sizeof( SockLinger );
#ifdef OUT_FOR_NOW
#ifdef __sgi
    (void) setsockopt( iS, SOL_SOCKET, SO_LINGER, &SockLinger, iOnLen );
#else
    (void) setsockopt( iS, SOL_SOCKET, SO_LINGER, &SockLinger, &iOnLen );
#endif
#endif
    (void) setsockopt( iS, SOL_SOCKET, SO_LINGER, &SockLinger, iOnLen );

    INAddr.sin_family = AF_INET;
    INAddr.sin_addr.s_addr = htonl( INADDR_ANY );
    INAddr.sin_port = htons( *pPort );

    if ( bind( iS, (struct sockaddr *)&INAddr, sizeof( INAddr )) < 0 )
      close( iS );
    else {
      iStat = ALsOK;
      *pSoc = iS;
      if ( *pPort == 0 ) {
	iAddrLen = sizeof( INAddr );
	(void) getsockname( iS, (struct sockaddr *)&INAddr, &iAddrLen );
	*pPort = ntohs( INAddr.sin_port );
      }
    }
  }

  return iStat;

} /* end ALSetupSocket */

/* ----------------------------------------------------------------------------- */
#ifdef ultrix
char * strdup(str_in)
char * str_in;
{
char * str_out = NULL;
if ((str_in == NULL)||
    ((str_out = malloc(strlen(str_in)+1)) == NULL)) return NULL;
return (strcpy(str_out,str_in));

}
#endif /* ultrix */
/* ----------------------------------------------------------------------------- */

/* end gencomm.c */
