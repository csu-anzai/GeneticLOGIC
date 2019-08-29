/*
* overview.h
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#ifndef	__OVERVIEW_H
#define	__OVERVIEW_H

#include "portlayr.h"

#include <almemory.h>
#include <mlayer.h>


#define	MicroRez	10
#define	ZoomRez		3
#define	HiRez		0
#define	MedRez		1
#define	LoRez		2

#define	HiRezCutoff	60000L
#define	LoRezCutoff	100000L

#define OV_DISABLED     0
#define OV_ENABLED      1
#define IP_DISABLED     2
#define IP_ENABLED      3
#define MV_DISABLED     4
#define MV_ENABLED      5

#define	NUM_SPECCOLOURS		9

/*** constants used with statbox ***/

#define         ST_UBorder      6
#define         ST_KeyWidth     10
#define         ST_KeyLength    30
#define         ST_Skip0        0.5
#define         ST_Skip1        0.4
#define         ST_Skip2        0.4
#define         ST_LBorder      6
#define		ST_UpdateRate	14

/*** ***/


#define	MEM_SCROLL		1


#define		OV_VERSION		"(beta.2)"


typedef struct	_size_map {	/* to keep track of size class info */

  u_long                len;
  u_long                pop;
  u_char                colour;
  struct _size_map *	next;

} Sizemap;


typedef struct	_org_map {	/* list of actual organisms */

  u_long		start;
  u_long		len;
  u_long		ip_location;
  u_long		from;
  u_long		to;
  u_long		colour;
  struct _org_map	*next;
  struct _org_map	*prev;

} OrgMap;


typedef struct	_mem_map {	/* to provide redraw and query abilities */

  u_short		start;
  u_short		len;
  struct _org_map	*org;
  struct _mem_map	*next;

} MemMap;


typedef struct {
  XtAppContext		app_con;
  Display		*d;

  Widget		top;
  Widget		gfxframe;
  Widget		memframe;
  Widget		etcbox;
  Widget		statbox;
  Widget		memscroll;

  Widget		l_simulation;
  Widget		l_stats;
  char			s_simulation[ 128 ];	/* HARDCODED CONSTANT */

  GC			memgc;

  Colormap		cmap;

  String		geometry;
  String		iconGeometry;
  Pixel			foreground;
  Pixel			background;
  Pixel			cmemory;
  Pixel			cspectrum[NUM_SPECCOLOURS];
  Pixel			cother;

  XFontStruct		*defaultfont;

  String		ifname;
  XFontStruct		*infofont;
  String		mlfname;
  XFontStruct		*memlabelfont;
  String		stfname;
  XFontStruct		*statfont;

  int			memscrollwidth;

} GtXStuff, *GtXStuffPtr;


typedef struct {
  GtXStuff	X;

  int		simstate;
  int		ipstate;
  int		mvstate;

  Sizemap	*sizemaproot;
  u_long	colours[NUM_SPECCOLOURS];
  int		numcolours;
  u_long	key_list[ NUM_SPECCOLOURS+1 ][ 4 ];

  char		hostname[ 65 ];		/* MAXHOSTNAMELEN + 1 */
  u_short	simport;

  MemMap	**memmap;
  int		memrows;

  OrgMap	*orgmap;

  long		InstExe_m;
  long		NumCells;
  long		NumGenotypes;
  long		NumSizes;

  u_long	memsize;
  int		rez;
  int		hashlen;
  int		memrwidth;
  int		memrspace;
  int		memrlen;
  int		vbd;
  int		lbd;
  int		rbd;
  int		ebwidth;
  int		memlabelskip;
  int		mfwidth;
  int		mfheight;
  int		realmfheight;
  int		statheight;
  int		winwidth;
  float		sbprop;

  int		memviewport;

  hMtLinkInfo	hLink;		/* link handle for sim being monitored */

} GtGlobalSet;



extern GtGlobalSet		GS;



#endif	/* add nothing past this line */
