/*
* trequest.h
*
*
* Copyright (c) 1992 by Marc W. Cygnus and
* Virtual Life
* All Rights Reserved.
* 92-3-19
*/

#ifndef __TREQUEST_H
#define __TREQUEST_H


/*** Message types ***********************************************************/

#define	TrtPauseSim		0x00
#define	TrtResumeSim		0x01

/*** Query types *************************************************************/

#define	TrtGeneralStats		0x00
#define	TrtQueryOrg		0x01


/*** TrtGeneralStats: reply ***/

typedef struct _t_genstats {

  u_long	memsize;

} TtGenStats, * pTtGenStats;

/*** TrtQueryOrg: query ***/

typedef struct _t_orgid {

  u_long	start;
  u_long	length;

} TtOrgID, * pTtOrgID;

/*** reply ***/

typedef struct _t_orginfo {

  u_long	exists;

} TtOrgInfo, * pTtOrgInfo;


/*** Dataflow types **********************************************************/

#define	TrtOrgLifeEvent		0x00
#define	TrtIPEvent		0x10
#define	TrtMVEvent		0x01
#define	TrtPlanEvent		0x02


/*** TrtOrgLifeEvent: ***/

typedef struct _t_orgevent {

  u_long	event;
  u_long	start;
  u_long	length;

} TtOrgEvent, * pTtOrgEvent;

/*** TrtIPEvent: ***/

typedef struct _t_ipevent {

  u_long	event;
  u_long	start;
  u_long	position;

} TtIPEvent, * pTtIPEvent;

/*** TrtMVEvent: ***/

typedef struct _t_mvevent {

  u_long	event;
  u_long	start;
  u_long	from;
  u_long	to;

} TtMVEvent, * pTtMVEvent;

/*** TrtPlanEvent: ***/

typedef struct _t_planevent {

  long		m_i_exec;
  long		num_cells;
  long		num_gen;
  long		num_sizes;

} TtPlanEvent, * pTtPlanEvent;


#endif  /* ifndef __TREQUEST_H; Add nothing past this point */
