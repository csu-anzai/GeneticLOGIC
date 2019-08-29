/*
* try2.c
*
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/


#include <stdio.h>
#include <sys/types.h>

#include <arpa/inet.h>

#include "alcomm.h"
#include "alcommp.h"
#include "debug.h"



int main( argc, argv )
  int		argc;
  char *	argv[];
{
  u_long		ulAddr;
  int			iRet;
  char *		sHostname;
  int			iPort;
  hALtCLink		hLink;
  int			iMode;
  pALtRequest		pReq;
  char			sText[100];
  int			iTLen;
  int			iLoop;

  _debug_state = 0xffff;

  if ( argc != 3 ) {
    puts( "Please specify no args *or* <hostname> <port>." );
    exit( -1 );
  }
  printf( "trying %s : %s\n", argv[1], argv[2] );

  puts( "initialising alcomm" );
  if (( iRet = ALCommInitialise( AL_Default, AL_Default )) < 0 )
    ALGripe( "ALCommInitialise", "error %d\n", iRet ), exit( -1 );

  puts( "resolving host" );
  iPort = atoi( argv[2] );
  if (( iRet = ALInterpretHostAddress( argv[1], &sHostname, &ulAddr )
      ) != ALsOK )
    ALGripe( "AL call", "ALInterpretHostAddress returned %d\n", iRet ),
      exit( -1 );

  puts( "trying connect" );
  if (( iRet = ALOpenTCPPathway( ulAddr, iPort, AL_Default, &hLink )) != ALsOK)
    ALGripe( "AL open call", "open tcp pathway returned %d\n", iRet ),
      exit( -1 );

  printf( "operating with clink %d\n", hLink );

  iLoop = 0;
  while ( 1 ) {

    printf( "==> " );
    fflush( stdout );
    gets( sText );
    iTLen = strlen( sText ) + 1;
    if (( iRet = ALTCPGenRequest( hLink, (u_char)iLoop, (u_char)255,
				  (pALtOpaque)sText, iTLen )) < 0 )
      ALGripe( "ALTCPGenRequest", "error %d\n", iRet ), exit( -1 );
    iLoop++;
  }

}



/* end try.c */
