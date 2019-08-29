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

#include "mlayer.h"
#include "trequest.h"
#include "debug.h"


void	DummyMessageRoutine P_(( ));

void	TGenStatusQuery P_(( ));

void	TProcOrganismLifeEvent P_(( ));

void	TInitOrgLifeData P_(( ));


static MtDefaultRoutines	_message_fns[] = {
  { 1,	DummyMessageRoutine }
};

static MtDefaultRoutines	_query_fns[] = {
  { TrtGeneralStats,	TGenStatusQuery }
};

static MtDefaultRoutines	_dfinit_fns[] = {
  { TrtOrgLifeEvent,	TInitOrgLifeData }
};



int main( argc, argv )
  int		argc;
  char *	argv[];
{
  MtStatus		iRet;


  puts( "simulating a listener" );

  puts( "initialising mlayer" );
  if (( iRet = MInitialise( 0, 0, _message_fns, 1, _query_fns, 1, M_NoFns, 0,
			    _dfinit_fns, 1 )) != MsOK )
    ALGripe( "MInitialise", "error %d\n", iRet ), exit( -1 );

  puts( "opening portal 7001" );
  if (( iRet = MOpenPublicPort( 7001 )) != MsOK )
    ALGripe( "MOpenPublicPort", "error %d\n", iRet ), exit( -1 );

  while ( 1 ) {
    if (( iRet = MServiceRequests( M_NoWait )) != MsOK )
      ALGripe( "MServiceRequests", "error %d\n", iRet ), exit( -1 );
    putchar( '.' );
    fflush( stdout );
    if ( MIsDFEnabled( TrtOrgLifeEvent ) ) {
      ReportLifeEvent( 'b', 111, 222 );
      puts( "...[sent a BIRTH]" );
    }
    sleep( 1 );
  } /* end while(1) */

} /* end main() */


int ReportLifeEvent( cWhat, iWhere, iLen )
{
  TtOrgEvent	OE;		/* <== changed from TtOrgID 05-17-92 cygnus */
  MtStatus	iRet;

  OE.event = cWhat;
  OE.start = iWhere;
  OE.length = iLen;
  if (( iRet = MDistributeDataflow( TrtOrgLifeEvent, (pMtOpaque)&OE,
				    sizeof( OE ) )) != MsOK )
    ALGripe( "ReportLifeEvent", "MDistDF. failed (%d)\n", iRet );

} /* end ReportLifeEvent() */


void DummyMessageRoutine( )
{

  printf( "***-msg-*** got a message\n" );

} /* end DummyMessageRoutine() */


void TInitOrgLifeData( ucOp, hLI )
  u_char	ucOp;
  hMtLinkInfo	hLI;
{
  TtOrgEvent		orgev;	/* <== changed from TtOrgID 05-17-92 cygnus */
  int			i;
  int			iRet;

  printf( "*** Initialising for dataflow (%d)\n", ucOp );

  for ( i = 1; i < 15; i += i ) {
    orgev.event = 'b';
    orgev.start = i * 100;
    orgev.length = 69;

    if (( iRet = MGenRequest( 0, MrDataflowEvent, TrtOrgLifeEvent,
			      (pMtOpaque)&orgev, sizeof( orgev ) )
	) != MsOK )
      ALGripe( "MGenRequest", "error %d\n", iRet );
  }

} /* end TInitOrgLifeData() */


void TGenStatusQuery( ucQ, pData, pPrivate )
  u_char	ucQ;
  pMtOpaque	pData;
  pMtOpaque	pPrivate;
{
  TtGenStats		GS;	/* <== changed from TtGeneralStatus */
  int			iRet;	/*   05-17-92 cygnus */

  GS.memsize = 4321;
  if (( iRet = MGenReply( pPrivate, 0, &GS, sizeof( GS ) )) != MsOK )
    ALGripe( "TGenStatusQuery", "MGenReply failed (%d)\n", iRet );

} /* end TGenStatusQuery() */


/* end try.c */
