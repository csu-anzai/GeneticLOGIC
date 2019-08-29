/* tmonitor.c   9-9-92 monitor/reporting routines for the Tierra Simulator */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char	tmonitor_sccsid[] = "@(#)tmonitor.c	1.5	7/21/92";
#endif

#include <sys/types.h>
#include "license.h"
#include "tierra.h"
#include "extern.h"
#include "tmonitor.h"
#include "trequest.h"

#include <mlayer.h>


/*** These two (TRepBirth, TRepDeath) could really be made one function. ***/


void TRepBirth( ulStart, ulLength )
  u_long	ulStart;
  u_long	ulLength;
{
  TtOrgEvent		OEV;
  MtStatus		iRet;

  OEV.event = htonl('b');
  OEV.start = htonl(ulStart);
  OEV.length = htonl(ulLength);

  /* DAN ALGripe( "birth", "%lu x %lu\n", ulStart, ulLength ); */

  if (( iRet = MDistributeDataflow( TrtOrgLifeEvent, (pMtOpaque)&OEV,
				    sizeof( OEV ) )) != MsOK )
    /* ALGripe( "TRepBirth", "MDistDF. error (%d)\n", iRet ); */
    { FEError(-1100,NOEXIT,NOWRITE,
        "Tierra TRepBirth() MDistDF. error (%d)\n", iRet );}

} /* end TRepBirth() */


void TRepDeath( ulStart, ulLength )
  u_long	ulStart;
  u_long	ulLength;
{
  TtOrgEvent		OEV;
  MtStatus		iRet;

  OEV.event = htonl('d');
  OEV.start = htonl(ulStart);
  OEV.length = htonl(ulLength);

  /* DAN ALGripe( "death", "%lu x %lu\n", ulStart, ulLength ); */

  if (( iRet = MDistributeDataflow( TrtOrgLifeEvent, (pMtOpaque)&OEV,
				    sizeof( OEV ) )) != MsOK )
    { FEError(-1101,NOEXIT,NOWRITE,
          "Tierra TRepDeath() MDistDF. error (%d)\n", iRet );}

} /* end TRepDeath() */

void TMoveIP( ulStart, ulPosition)
  u_long	ulStart;
  u_long	ulPosition;
{
  TtIPEvent		IEV;
  MtStatus		iRet;

  IEV.event = htonl('m');
  IEV.start = htonl(ulStart);
  IEV.position = htonl(ulPosition);
#ifdef _DBG_ 
  if(Log) {fprintf(tfp_log,"MOVEIP: critter @ %ld ip @ %ld\n",ulStart,
		  ulPosition);
	    fflush(tfp_log);
	}
#endif
  if (( iRet = MDistributeDataflow( TrtIPEvent, (pMtOpaque)&IEV,
				    sizeof( IEV ) )) != MsOK )
    { FEError(-1102,NOEXIT,NOWRITE,
          "Tierra TMoveIP() MDistDF. error (%d)\n", iRet );}

} /* end TMoveIP() */

void TMoveD( ulStart, ulFrom, ulTo)
  u_long	ulStart;
  u_long	ulFrom;
  u_long	ulTo;
{
  TtMVEvent		MEV;
  MtStatus		iRet;

  MEV.event = htonl('M');
  MEV.start = htonl(ulStart);
  MEV.from = htonl(ulFrom);
  MEV.to = htonl(ulTo);
#ifdef _BDG_  
  if(Log) {fprintf(tfp_log,"MOVED: critter @ %ld from %ld to %ld\n",
		   ulStart, ulFrom, ulTo);
	    fflush(tfp_log);
	}
#endif 
  if (( iRet = MDistributeDataflow( TrtMVEvent, (pMtOpaque)&MEV,
				    sizeof( MEV ) )) != MsOK )
    { FEError(-1103,NOEXIT,NOWRITE,
          "Tierra TMoveD() MDistDF. error (%d)\n", iRet );}

} /* end TMoveD() */


void TPlan()
{
  TtPlanEvent		PEV;
  MtStatus		iRet;

  PEV.m_i_exec = htonl(InstExe.m);
  PEV.num_cells = htonl(NumCells);
  PEV.num_gen = htonl(NumGenotypes);
  PEV.num_sizes = htonl(NumSizes);
#ifdef _BDG_  
  if(Log) {fprintf(tfp_log,"TPLAN: iem %ld nc %ld ng %ld ns %ld\n",
           PEV.m_i_exec , PEV.num_cells , PEV.num_gen , PEV.num_sizes );
	   fflush(tfp_log);
	}
#endif

  if (( iRet = MDistributeDataflow( TrtPlanEvent, (pMtOpaque)&PEV,
				    sizeof( PEV ) )) != MsOK )
    { FEError(-1103,NOEXIT,NOWRITE,
          "Tierra TPlan() MDistDF. error (%d)\n", iRet );}

} /* end TPlan() */


void TInitOrgLifeEvents( ucOp, hLink )
  u_char	ucOp;
  hMtLinkInfo	hLink;
{
  TtOrgEvent		OEV;
  MtStatus		iRet;
  I32s			ar,ci;
  Pcells		ce;

  OEV.event = 'I';
  if (( iRet = MGenRequest( hLink, MrDataflowEvent, TrtOrgLifeEvent,
			    (pMtOpaque)&OEV, sizeof( OEV )) ) != MsOK )
    { sprintf(mes[0], 
	     "TInitOrgLifeEvents MGenReq. init begin error (%d)\n", iRet );
    FEMessage(1,mes); }

 for (ar = 0; ar < NumCelAr; ar++) for (ci = 0; ci < CelArSiz; ci++)
    {   if (ar == 0 && ci < 2)
            continue;
        ce = &cells[ar][ci];

    if ( ce->ld ) {
      if ( ce->mm.s ) {
	OEV.event = htonl('b');
	OEV.start = htonl(ce->mm.p);
	OEV.length = htonl(ce->mm.s);
	/* ALGripe( "birth", "%lu x %lu\n", OEV.start, OEV.length ); */
	if (( iRet = MGenRequest( hLink, MrDataflowEvent, TrtOrgLifeEvent,
				  (pMtOpaque)&OEV, sizeof( OEV ) )) != MsOK )
    { sprintf(mes[0], 
	      "TInitOrgLifeEvents MGenReq. df error (%d)\n", iRet );
    FEMessage(1,mes); }
      }
      if ( ce->md.s ) {
	OEV.event = htonl('b');
	OEV.start = htonl(ce->md.p);
	OEV.length = htonl(ce->md.s);
	/* DAN ALGripe( "birth", "%lu x %lu\n", OEV.start, OEV.length );*/
	if (( iRet = MGenRequest( hLink, MrDataflowEvent, TrtOrgLifeEvent,
				  (pMtOpaque)&OEV, sizeof( OEV ) )) != MsOK )
    { sprintf(mes[0], 
	  "TInitOrgLifeEvents MGenReq. df error (%d)\n", iRet );
    FEMessage(1,mes); }
      }
    }
  }
  
  OEV.event = htonl('i');
  if (( iRet = MGenRequest( hLink, MrDataflowEvent, TrtOrgLifeEvent,
			    (pMtOpaque)&OEV, sizeof( OEV )) ) != MsOK )
    { sprintf(mes[0], 
              "TInitOrgLifeEvents MGenReq. init end error (%d)\n", iRet );
    FEMessage(1,mes); }

} /* end TInitOrgLifeEvents() */


void TQueryGeneralStats( ucQ, pData, hLink )
  u_char	ucQ;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{
  TtGenStats		GS;
  MtStatus		iRet;

  GS.memsize = htonl(SoupSize);

  if (( iRet = MGenReply( hLink, ucQ, (pMtOpaque)&GS, sizeof( GS ) )
      ) != MsOK )
    { sprintf(mes[0], 
              "TQueryGeneralStats MGenReply error (%d)\n", iRet );
    FEMessage(1,mes); }

} /* end TGenStatusQuery() */


void TQueryOrganism( ucQ, pData, hLink )
  u_char	ucQ;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{

} /* end TQueryOrganism() */


void TSimRuncontrol( ucM, pData, hLink )
  u_char	ucM;
  pMtOpaque	pData;
  hMtLinkInfo	hLink;
{

if ( ucM == TrtPauseSim )
    AL_run_flag = 0;
  else if ( ucM == TrtResumeSim )
    AL_run_flag = 1;
  else {
    FEError(-1104,NOEXIT,NOWRITE,
        "Tierra TSimRuncontrol() unknown operation (%d)", ucM );
  }
  TPlan();

} /* end TSimRuncontrol() */


/* ---------------------------------------------------------------------- */
/* ---------------------------------------------------------------------- */
