/*
* try.c
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
#include "debug.h"



int main( argc, argv )
  int		argc;
  char *	argv[];
{
  int			iRet;
  hALtCLink		hPortal;
  pALtRequest		pReq;

  _debug_state = 0x0000;

  puts( "simulating a listener" );

  puts( "initialising alcomm" );
  if (( iRet = ALCommInitialise( AL_Default, AL_Default )) < 0 )
    ALGripe( "ALCommInitialise", "error %d\n", iRet ), exit( -1 );

  puts( "opening portal 7001" );
  if (( iRet = ALOpenTCPPortal( 7001, AL_Asynch, AL_Asynch, &hPortal )) < 0 )
    ALGripe( "ALOpenTCPPortal", "error %d\n", iRet ), exit( -1 );

/*
  puts( "making portal asynchronous" );
  if (( iRet = ALChangeCLinkMode( hPortal, AL_Asynch )) < 0 )
    ALGripe( "ALChangeCLinkMode", "error %d\n", iRet ), exit( -1 );
*/

  while ( 1 ) {
    if (( iRet = ALServiceCLinks( AL_Synch, AL_Wait )) != ALsOK )
      ALGripe( "ALServiceCLinks", "error %d\n", iRet ), exit( -1 );

    puts( "..." );

    pReq = (pALtRequest)NULL;
    iRet = ALDequeueRequest( &pReq );

    while ( pReq != NULL ) {
      (void) ALDumpRequest( pReq );
      (void) ALDestroyRequest( pReq );
      pReq = (pALtRequest)NULL;
      iRet = ALDequeueRequest( &pReq );
    }

  }

}



/* end try.c */
