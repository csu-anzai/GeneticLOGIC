/*
* overview.c
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <string.h>

#include <fcntl.h>
#include <sys/param.h>
#include <sys/stat.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include "portlayr.h"

#include <mlayer.h>

#include "overview.h"
#include "trequest.h"

#define	_DBGMainModule
#include "debug.h"
#undef	_DBGMainModule


void			local_initialise P_(( void ));

void			_handle_requests P_(( XtPointer dummy1, int * dummy2,
			  XtInputId * id ));

void			_ov_birthdeath P_(( ));
void			_ov_ipmove P_(( ));
void			_ov_plan P_(( ));
void			_ov_dmove P_(( ));
void			move_ip P_((u_long start, ulong ip_location ));
void			move_d P_((u_long start, ulong _from_, ulong _to_ ));
void			do_plan P_(( void ));

#define							_DFnCount	4
static MtDefaultRoutines	_dataflow_fns[] = {
  { TrtOrgLifeEvent,	_ov_birthdeath },
  { TrtIPEvent,         _ov_ipmove },
  { 1,         _ov_dmove },
  { TrtPlanEvent,         _ov_plan }
};


GtGlobalSet		GS;




/*** main body *********************************************************/


/*
* main()
*
*
*
*/

int
main( argc, argv )
  int		argc;
  char		*argv[];
{
  XEvent		event;
  char			fooer[10];
  int			lastarg;
  MtStatus		iRet;
  int			iPort;
  int			iWhich;
  int			fdLink;

  if ( argc < 3 ) {
    fprintf( stderr, "Usage:  %s <host> <port>\n", argv[0] );
    exit( 1 );
  }

  if (( iRet = MInitialise( 0, 0, M_NoFns, 0, M_NoFns, 0,
			    _dataflow_fns, _DFnCount, M_NoFns, 0 )) != MsOK ) {
    ALGripe( "main", "Minitialise error (%d)\n", iRet );
    exit( -1 );
  }

  printf( "Overview initialising... [ host %s:%s ]\n", argv[1], argv[2] );
  strncpy( GS.hostname, argv[1], MAXHOSTNAMELEN );
  GS.simport = atoi( argv[2] );

  if (( iRet = MConnectTo( GS.hostname, GS.simport, &GS.hLink )) != MsOK ) {
    ALGripe( "main", "MConnectTo error (%d)\n", iRet );
    exit( -1 );
  }
  if (( iRet = MGetLinkFD( GS.hLink, &fdLink )) != MsOK ) {
    ALGripe( "main", "MGetLinkFD error (%d)\n", iRet );
    exit( -1 );
  }

  sleep( 1 );	/* don't know why... we'll fix this. */

  if (argc >3)
     {
     printf( "... turning on flowcontrol \n" );
     (void) MSetFlowcontrol( GS.hLink, atoi(argv[3]), 
			    (argc>=5)?atoi(argv[4]): 2);
     }

  local_initialise();

  initialise_xinterface( argc, argv );

  	/* since overview's main function is organism life event */
	/*   monitoring, we'll start that up right from the beginning. */

  iWhich = M_Enable;
  if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtOrgLifeEvent,
			    (pMtOpaque)&iWhich, sizeof( iWhich ) )
      ) != MsOK ) {
    ALGripe( "main", "MGenRequest error (%d)\n", iRet );
    exit( 1 );
  }

  (void) XtAppAddInput( GS.X.app_con, fdLink, XtInputReadMask,
		        _handle_requests, (XtPointer)NULL );

/***
  while ( 1 ) {
/***
    XtAppPeekEvent( GS.X.app_con, &event );
/***    
    if ( XtAppPending( GS.X.app_con ) ) {
      XtAppNextEvent( GS.X.app_con, &event );
      XtDispatchEvent( &event );
    }
/***
    gets( fooer );
  }
***/

  XtAppMainLoop( GS.X.app_con );
}


void _handle_requests( dummy1, dummy2, id )
  XtPointer	dummy1;
  int *		dummy2;
  XtInputId *	id;
{
  int			iRet;

  if (( iRet = MServiceRequests( M_NoWait )) != MsOK )
    ALGripe( "_handle_requests", "MServiceRequests error (%d)\n", iRet );

} /* end _handle_requests() */


void
local_initialise()
{
  pTtGenStats		pGS;
  int			iGSLen;
  int			iWhich;
  MtStatus		iRet;

  if (( iRet = MGenRequestWithReply( GS.hLink, MrQuery, TrtGeneralStats,
				     M_NoData, 0, (pMtOpaque)&pGS,
				     &iGSLen )) != MsOK ) {
    ALGripe( "local_initialise", "MGenReqWRep. error (%d)\n", iRet );
    exit( -1 );
  }

  GS.memsize = pGS->memsize;
  printf( "soup size: %lu\n", GS.memsize );

  (void) ALFree( (char *)pGS );

  iWhich = M_Enable;
  if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtPlanEvent,
                            (pMtOpaque)&iWhich, sizeof( iWhich ) )
      ) != MsOK ) {
    ALGripe( "main", "MGenRequest error (%d)\n", iRet );
    exit( 1 );
  }

}


void _ov_ipmove( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtIPEvent   pIEV;

  pIEV = (pTtIPEvent)pData;
  move_ip(ntohl(pIEV->start),ntohl(pIEV->position));

}
void _ov_dmove( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtMVEvent   pMEV;

  pMEV = (pTtMVEvent)pData;
  move_d(ntohl(pMEV->start),ntohl(pMEV->from),ntohl(pMEV->to)); 

}
void _ov_plan( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtPlanEvent   pPEV;

  pPEV = (pTtPlanEvent)pData;
 
  GS.InstExe_m 		= ntohl(pPEV->m_i_exec);
  GS.NumCells  		= ntohl(pPEV->num_cells);
  GS.NumGenotypes  	= ntohl(pPEV->num_gen);
  GS.NumSizes  		= ntohl(pPEV->num_sizes);
  do_plan();

}
void _ov_birthdeath( ucOp, pData, hLink )
  u_char	ucOp;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  register pTtOrgEvent	pOEV;

  pOEV = (pTtOrgEvent)pData;

  switch ( ntohl(pOEV->event) ) {
    case 'b': record_organism_birth( 
		 ntohl(pOEV->start), ntohl(pOEV->length) );	
	    break;
    case 'd': record_organism_death( 
		 ntohl(pOEV->start), ntohl(pOEV->length) );	
	    break;
    case 'I': printf( "start init...\n" );	break;
    case 'i': printf( "...end init\n" );	break;
  }

}

