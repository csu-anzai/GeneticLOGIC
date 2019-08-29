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

#include "mlayer.h"
#include "trequest.h"
#include "debug.h"


void	TProcOrganismLifeEvent P_(( ));
void	TProcIPEvent P_(( ));


static MtDefaultRoutines	_dataflow_fns[] = {
  { TrtOrgLifeEvent,	TProcOrganismLifeEvent },
  { TrtIPEvent,		TProcIPEvent }
};





int main( argc, argv )
  int		argc;
  char *	argv[];
{
  int			iRet;
  int			iPort;
  char			sText[100];
  int			iTLen;
  int			iLoop;
  int			hLink;

  /* _debug_state = DBGAll; */

  if ( argc != 3 ) {
    puts( "Please specify: <hostname> <port>" );
    exit( -1 );
  }
  printf( "trying %s : %s\n", argv[1], argv[2] );

  puts( "initialising mlayer" );
  if (( iRet = MInitialise( 0, 0, M_NoFns, 0, M_NoFns, 0, _dataflow_fns, 2,
			    M_NoFns, 0 )) != MsOK )
    ALGripe( "MInitialise", "error %d\n", iRet ), exit( -1 );

  iPort = atoi( argv[2] );
  puts( "trying connect" );
  if (( iRet = MConnectTo( argv[1], iPort, &hLink )) != MsOK )
    ALGripe( "MConnectTo", "error %d\n", iRet ), exit( -1 );

  printf( "operating with LIT handle %d\n", hLink );

  puts( "turning on flowcontrol of 5:2" );
  (void) MSetFlowcontrol( hLink, 5, 2 );

  iLoop = 0;
  while ( 1 ) {

    printf( "==> " );
    fflush( stdout );
    gets( sText );

    if ( strcmp( sText, "enable" ) == 0 ) {
      int	iWhich;

      iWhich = M_Enable;
      if (( iRet = MGenRequest( hLink, MrModifyDataflow, TrtOrgLifeEvent,
			        (pMtOpaque)&iWhich, sizeof( int ) )
	  ) != MsOK )
	ALGripe( "MGenRequest", "error %d\n", iRet );

    } else if ( strcmp( sText, "disable" ) == 0 ) {
      int	iWhich;

      iWhich = M_Disable;
      if (( iRet = MGenRequest( hLink, MrModifyDataflow, TrtOrgLifeEvent,
			        (pMtOpaque)&iWhich, sizeof( int ) )
	  ) != MsOK )
	ALGripe( "MGenRequest", "error %d\n", iRet );

    } else if ( strcmp( sText, "Eip" ) == 0 ) {
      int	iWhich;

      iWhich = M_Enable;
      if (( iRet = MGenRequest( hLink, MrModifyDataflow, TrtIPEvent,
			        (pMtOpaque)&iWhich, sizeof( int ) )
	  ) != MsOK )
	ALGripe( "MGenRequest", "error %d\n", iRet );
    } else if ( strcmp( sText, "Dip" ) == 0 ) {
      int	iWhich;

      iWhich = M_Disable;
      if (( iRet = MGenRequest( hLink, MrModifyDataflow, TrtIPEvent,
			        (pMtOpaque)&iWhich, sizeof( int ) )
	  ) != MsOK )
	ALGripe( "MGenRequest", "error %d\n", iRet );

    } else if ( strcmp( sText, "query" ) == 0 ) {
      pTtGenStats		pGS;
      int			iGSLen;

      if (( iRet = MGenRequestWithReply( hLink, MrQuery, TrtGeneralStats,
					 M_NoData, 0, (pMtOpaque)&pGS,
					 &iGSLen )) != MsOK )
	ALGripe( "MGenRequestWithReply", "unhappy: %d\n", iRet );
      else {
	
	printf( "*** Sent a status request; got pGS" );
	if ( pGS == NULL )
	  printf( " = <null>\n" );
	else {
	  printf( "->memsize = (%d); length = %d\n", pGS->memsize, iGSLen );
	  (void) ALFree( pGS );
	}

      } /* end req w/ reply happy */
      
    }

    if (( iRet = MServiceRequests( M_NoWait )) != MsOK )
      ALGripe( "MServiceRequests", "error %d\n", iRet );

  }

}


void TProcOrganismLifeEvent( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtOrgEvent	pOEV;

  pOEV = (pTtOrgEvent)pData;

  printf( "[%c @ %d x %d]", pOEV->event, pOEV->start, pOEV->length );

} /* end TProcOrganismEvent() */

void TProcIPEvent( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtIPEvent	pIEV;

  pIEV = (pTtIPEvent)pData;

  printf( "[%c @ %d x %d]", pIEV->event, pIEV->start, pIEV->position );

} /* end TProcOrganismEvent() */






/* end try.c */
