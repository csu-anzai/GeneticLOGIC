/*
* ovxiface.c
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <errno.h>

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <X11/Xaw/Cardinals.h>
#include <X11/Xaw/Form.h>
#include <X11/Xaw/Box.h>
#include <X11/Xaw/Label.h>
#include <X11/Xaw/Command.h>
#include <X11/Xaw/Scrollbar.h>

#include "portlayr.h"

#include "debug.h"

#include <mlayer.h>
#include "overview.h"
#include "trequest.h"



/*** prototypes ********************************************************/

/*** local ***/

XtCallbackProc		quit_ov_proc P_(( Widget w, XtPointer client_data,
			  XtPointer call_data ));
XtCallbackProc		toggle_ov_active P_(( Widget w, XtPointer client_data,
			  XtPointer call_data ));
XtCallbackProc		toggle_mv_active P_(( Widget w, XtPointer client_data,
			  XtPointer call_data ));
XtCallbackProc		toggle_ip_active P_(( Widget w, XtPointer client_data,
			  XtPointer call_data ));
XtCallbackProc		update_scroll P_(( Widget w, XtPointer client_data,
			  XtPointer call_data ));

XtActionProc		set_resolution P_(( Widget w, XEvent *event,
			  String *params, Cardinal *pcount ));
XtActionProc		query_organism P_(( Widget w, XEvent *event,
			  String *params, Cardinal *pcount ));
XtActionProc		redraw_mem P_(( Widget w, XEvent *event,
			  String *params, Cardinal *pcount ));

OrgMap *		find_org_from_orgmap P_(( u_long where ));


void			please_setup_infobox P_(( Widget ib ));
void			do_plan P_(( void ));
void			init_visual_parameters P_(( void ));
void			setup_visual_constants P_(( void ));
void			initialise_colours P_(( void ));
void			initialise_fonts P_(( void ));


/*** ovexpose.c ***/

extern XtEventHandler	expose_memframe P_(( Widget w, XtPointer client_data,
			  XEvent *event, Boolean *ctd ));
extern XtEventHandler	expose_etcbox P_(( Widget w, XtPointer client_data,
			  XEvent *event, Boolean *ctd ));
extern XtEventHandler	expose_statbox P_(( Widget w, XtPointer client_data,
			  XEvent *event, Boolean *ctd ));

/*** ovmemmap.c ***/

extern void		generate_memmap P_(( void ));
extern void		free_old_memmap P_(( void ));
extern void		add_org_to_memmap P_(( OrgMap *om, int dodraw ));
extern void		del_org_from_memmap P_(( OrgMap *om ));
		



/*** application specific **********************************************/

static char	*SpecColorDefaultNames[] = {
  "#ff4f4f", "#ffba6b", "#ebff1c", "#1bff18", "#7effd4",
  "#00d0ff", "#4848ff", "#cc00ff", "#ff00ea"
};


/* THE STRINGS THAT FOLLOW SHOULD REALLY BE STATIC! */

#define XtNiconGeometry         "iconGeometry"
#define XtCIconGeometry         "IconGeometry"
#define	XtNmemColor		"memColor"
#define	XtCMemColor		"MemColor"
#define	XtNspecColor		"specColor"
#define	XtCSpecColor		"SpecColor"
#define	XtNotherColor		"otherColor"
#define	XtCOtherColor		"OtherColor"
#define	XtNinfoFont		"infoFont"
#define	XtCInfoFont		"InfoFont"
#define	XtNmemLabelFont		"memLabelFont"
#define	XtCMemLabelFont		"MemLabelFont"
#define	XtNstatFont		"statFont"
#define	XtCStatFont		"StatFont"

#define FGDEF                   "#00ff84"
#define BGDEF                   "#626482"
#define	CMEMDEF			"#757575"
#define	COTHERDEF		"#e5e5e5"

#define	IFONTDEF		"-*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*"
#define	MLFONTDEF		"-*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*"
#define	STFONTDEF		"-*-helvetica-medium-r-*-*-14-*-*-*-*-*-*-*"

#define	DEFAULTFONT		"-*-helvetica-bold-r-*-*-14-*-*-*-*-*-*-*"

#define         CMD_BUTTONWIDTH         140
#define         CMD_BUTTONHEIGHT        18





/*** other ***********************************************************/



String	fallback_resources[] = {
  "*Command*borderWidth:	2",
  "*Command*shapeStyle:		roundedRectangle",
  "*Command*highlightThickness:	2",
  "*Command*background:		#0092be",
  "*Scrollbar*thickness:	10",
  "*foreground:			#00ff84",
  "*background:			#626482",
  "*borderColor:		#00799e",
  "*font:			-*-helvetica-bold-r-*--14-*-*-*-*-*-*-*",
  NULL
};



static XtActionsRec	ov_actions[] = {
  { "SetRez",		(XtActionProc) set_resolution },
  { "OrgQuery",		(XtActionProc) query_organism },
  { "RedrawMem",	(XtActionProc) redraw_mem },
  NULL
};



/***********************************************************************/



/* DONE
* initialise_xinterface()
*
*/

void
initialise_xinterface( argc, argv )
  int		argc;
  char		*argv[];
{
  static char		_gs_app_name[] = "Overview";
  Arg			xargs[20];	/* HARDCODED CONSTANT */
  int			i;
  Widget		bigframe;
  Widget		infobox;
  Widget		cmdframe;
  Widget		quit_ov, onoff_ov,ip_ov,mv_ov;
  XGCValues		gcv;
  XtResource		ovresources[ 100 ];	/* HARDCODED CONSTANT */
  int			ovrsccount;


  ovrsccount = init_resource_spec_list( ovresources, 100 );

	/* create topmost shell widget */

  GS.X.top = XtAppInitialize( &GS.X.app_con, _gs_app_name, NULL, ZERO,
			      &argc, argv, fallback_resources, NULL, ZERO );
  GS.X.d = XtDisplay( GS.X.top );

	/* meander thru the resource database a bit */

  XtGetApplicationResources( GS.X.top, (XtPointer)&GS.X, ovresources,
			     ovrsccount, NULL, ZERO );

  initialise_colours();
  initialise_fonts();
  init_visual_parameters();

  XtAppAddActions( GS.X.app_con, ov_actions, XtNumber( ov_actions ) );

  bigframe = XtCreateManagedWidget( "bigframe", formWidgetClass, GS.X.top,
				    NULL, ZERO );

	/* create the gfxframe */

  i = 0;
  XtSetArg( xargs[i], XtNdefaultDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNtop, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNbottom, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNleft, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNright, XtChainLeft );		i++;
  GS.X.gfxframe = XtCreateManagedWidget( "gfxframe", formWidgetClass,
		    bigframe, xargs, i );

	/* create upper infobox */

  i = 0;
  XtSetArg( xargs[i], XtNwidth, GS.winwidth );		i++;
  XtSetArg( xargs[i], XtNborderWidth, 0 );		i++;
  XtSetArg( xargs[i], XtNhorizDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNvertDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNbackground,
	    BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++;
  infobox = XtCreateManagedWidget( "infobox", formWidgetClass, GS.X.gfxframe,
				   xargs, i );

  please_setup_infobox( infobox );

	/* create left etcbox */

  i = 0;
  XtSetArg( xargs[i], XtNwidth, GS.ebwidth );		i++;
  XtSetArg( xargs[i], XtNheight, GS.mfheight );		i++;
  XtSetArg( xargs[i], XtNfromVert, infobox );		i++;
  XtSetArg( xargs[i], XtNhorizDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNvertDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNbackground,
	    BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++;
  GS.X.etcbox = XtCreateManagedWidget( "etcbox", formWidgetClass,
		  GS.X.gfxframe, xargs, i );

  XtAddEventHandler( GS.X.etcbox, ExposureMask, False, expose_etcbox, NULL );

	/* create the actual overview frame (for graphics) */

  i = 0;
  XtSetArg( xargs[i], XtNwidth, GS.mfwidth );		i++;
  XtSetArg( xargs[i], XtNheight, GS.mfheight );		i++;
  XtSetArg( xargs[i], XtNfromVert, infobox );		i++;
  XtSetArg( xargs[i], XtNfromHoriz, GS.X.etcbox );	i++;
  XtSetArg( xargs[i], XtNtop, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNbottom, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNleft, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNright, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNdefaultDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNhorizDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNvertDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNbackground,
	    BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++;
  GS.X.memframe = XtCreateManagedWidget( "memframe", formWidgetClass,
		    GS.X.gfxframe, xargs, i );

  XtAddEventHandler( GS.X.memframe, ExposureMask, True, expose_memframe,
		     NULL );

	/* create the memscroll bar */

  i = 0;
  XtSetArg( xargs[i], XtNwidth, GS.X.memscrollwidth );	i++;
  XtSetArg( xargs[i], XtNheight, GS.mfheight );		i++;
  XtSetArg( xargs[i], XtNfromVert, infobox );		i++;
  XtSetArg( xargs[i], XtNfromHoriz, GS.X.memframe );	i++;
  XtSetArg( xargs[i], XtNtop, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNbottom, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNleft, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNright, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNdefaultDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNhorizDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNvertDistance, 0 );		i++;
  if ( sizeof(float) > sizeof(XtArgVal) ) {
    XtSetArg( xargs[i], XtNshown, GS.sbprop );		i++;
  } else {
    XtArgVal *l_sbprop = (XtArgVal *)&GS.sbprop;
    XtSetArg( xargs[i], XtNshown, *l_sbprop );		i++;
  }
  GS.X.memscroll = XtCreateManagedWidget( "memscroll", scrollbarWidgetClass,
		GS.X.gfxframe, xargs, i );

  XtAddCallback( GS.X.memscroll, XtNjumpProc, update_scroll, (XtPointer)NULL );

	/* create the lower statbox */

  i = 0;
  XtSetArg( xargs[i], XtNwidth, GS.winwidth );		i++;
  XtSetArg( xargs[i], XtNheight, GS.statheight );	i++;
  XtSetArg( xargs[i], XtNfromVert, GS.X.etcbox );	i++;
  XtSetArg( xargs[i], XtNborderWidth, 0 );		i++;
  XtSetArg( xargs[i], XtNhorizDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNvertDistance, 0 );		i++;
  XtSetArg( xargs[i], XtNbackground,
	    BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++;
  GS.X.statbox = XtCreateManagedWidget( "statbox", formWidgetClass,
		   GS.X.gfxframe, xargs, i );

  XtAddEventHandler( GS.X.statbox, ExposureMask, False, expose_statbox, NULL );

	/* create the lower command frame */

  i = 0;
  XtSetArg( xargs[i], XtNfromVert, GS.X.gfxframe );	i++;
  XtSetArg( xargs[i], XtNtop, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNbottom, XtChainTop );		i++;
  XtSetArg( xargs[i], XtNleft, XtChainLeft );		i++;
  XtSetArg( xargs[i], XtNright, XtChainLeft );		i++;
  cmdframe = XtCreateManagedWidget( "cmdframe", formWidgetClass, bigframe,
	       xargs, i );

	/* quit button */

  i = 0;
  XtSetArg( xargs[i], XtNfromHoriz, NULL );		i++;
  XtSetArg( xargs[i], XtNfromVert, NULL );		i++;
			/* HARDCODED CONSTANTS below */
  XtSetArg( xargs[i], XtNwidth, 60 );			i++;
  XtSetArg( xargs[i], XtNheight, 18 );			i++;
  quit_ov = XtCreateManagedWidget( "quit", commandWidgetClass, cmdframe,
	      xargs, i );
  XtAddCallback( quit_ov, XtNcallback, quit_ov_proc, (XtPointer)NULL );

	/* enable/disable button */

  i = 0;
  XtSetArg( xargs[i], XtNfromHoriz, quit_ov );		i++;
  XtSetArg( xargs[i], XtNfromVert, NULL );		i++;
			/* HARDCODED CONSTANTS below */
  XtSetArg( xargs[i], XtNwidth, 80 );			i++;
  XtSetArg( xargs[i], XtNheight, 18 );			i++;
  onoff_ov = XtCreateManagedWidget( "disable", commandWidgetClass, cmdframe,
	       xargs, i );
  XtAddCallback( onoff_ov, XtNcallback, toggle_ov_active, (XtPointer)NULL );

	/* enable/disable IP button */

  i = 0;
  XtSetArg( xargs[i], XtNfromHoriz, onoff_ov );		i++;
  XtSetArg( xargs[i], XtNfromVert, NULL );		i++;
			/* HARDCODED CONSTANTS below */
  XtSetArg( xargs[i], XtNwidth, 80 );			i++;
  XtSetArg( xargs[i], XtNheight, 18 );			i++;
  ip_ov = XtCreateManagedWidget( "enable IP", commandWidgetClass, cmdframe,
	       xargs, i );
  XtAddCallback( ip_ov, XtNcallback, toggle_ip_active, (XtPointer)NULL );

	/* enable/disable MV button */

  i = 0;
  XtSetArg( xargs[i], XtNfromHoriz, ip_ov );		i++;
  XtSetArg( xargs[i], XtNfromVert, NULL );		i++;
			/* HARDCODED CONSTANTS below */
  XtSetArg( xargs[i], XtNwidth, 88 );			i++;
  XtSetArg( xargs[i], XtNheight, 18 );			i++;
  ip_ov = XtCreateManagedWidget( "enable MV", commandWidgetClass, cmdframe,
	       xargs, i );
  XtAddCallback( ip_ov, XtNcallback, toggle_mv_active, (XtPointer)NULL );

	/* initialise organism list */
  initialise_orglist();

	/* generate a memory screen representation from orglist */
  GS.memmap = (MemMap **)NULL;
  generate_memmap();

	/* realise the top level overview widget */
  XtRealizeWidget( GS.X.top );

  gcv.foreground = GS.X.foreground;
  gcv.graphics_exposures = True;
  GS.X.memgc = XCreateGC( GS.X.d, XtWindow( GS.X.top ),
		 GCForeground | GCGraphicsExposures, &gcv);

	/* initialise local statistics */
  initialise_ov_statistics();

  GS.simstate = OV_ENABLED;
  GS.ipstate = IP_DISABLED;
  GS.mvstate = MV_DISABLED;

}


/* INCOMPLETE
* please_setup_infobox()
*
*/

void
please_setup_infobox( ib )
  Widget	ib;
{
  Arg			xargs[20];	/* HARDCODED CONSTANT */
  int			i;
  char bbb[120];

  i = 0;
  sprintf( GS.X.s_simulation, "Simulation [ %s:%d ]", GS.hostname,
	   GS.simport );
  XtSetArg( xargs[i], XtNlabel, GS.X.s_simulation );		i++;
  XtSetArg( xargs[i], XtNfont, GS.X.infofont );			i++;
  XtSetArg( xargs[i], XtNbackground,  
     BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++; 
  GS.X.l_simulation = XtCreateManagedWidget( "simlabel", labelWidgetClass, 
			ib, xargs, i );

  i = 0;
  sprintf(bbb, 
 "Millions Exec= %8.8ld Cells = %8.8ld  Genotypes = %8.8ld  Sizes =%8.8ld ",
   GS.InstExe_m,GS.NumCells,GS.NumGenotypes, GS.NumSizes); 

  XtSetArg( xargs[i], XtNfromHoriz, GS.X.l_simulation );	i++;
  XtSetArg( xargs[i], XtNlabel, bbb );				i++;
  XtSetArg( xargs[i], XtNfont, GS.X.infofont );			i++;
  XtSetArg( xargs[i], XtNbackground,  
     BlackPixelOfScreen( DefaultScreenOfDisplay( GS.X.d ) ) );	i++; 
  GS.X.l_stats = XtCreateManagedWidget( "label2", 
                 labelWidgetClass, ib, xargs, i );

}

void
do_plan( )
{
  Arg			xargs[20];	/* HARDCODED CONSTANT */
  char bbb[120];
  int i=0;

  sprintf(bbb, 
  "Millions Exec= %ld Cells = %4ld  Genotypes = %4ld  Sizes =%4ld ",
   GS.InstExe_m,GS.NumCells,GS.NumGenotypes, GS.NumSizes); 

  XtSetArg( xargs[i], XtNlabel, bbb );				i++;

  /* XtSetValues( GS.X.l_stats, xargs, XtNumber( xargs ) );*/

  XtSetValues( GS.X.l_stats, xargs, i);


}

/* DONE
* init_visual_parameters()
*
*/

void
init_visual_parameters()
{
  static char		_sizerule[] = "0009000000";

  GS.rez = ( GS.memsize <= HiRezCutoff ) ? HiRez :
	     ( GS.memsize > LoRezCutoff ) ? LoRez : MedRez;

  setup_visual_constants();

  GS.X.memscrollwidth = 10;
  GS.memrlen = 1000;
  GS.rbd = 8;
  GS.lbd = 6;

  GS.ebwidth = XTextWidth( GS.X.memlabelfont, _sizerule, strlen( _sizerule ) );
  GS.mfwidth = GS.lbd + GS.rbd + GS.memrlen;	/* memframe width */
	/* the next line has hardcoded constants which make the memory
	   window a consistent size over different magnifications */
  GS.mfheight = (60000 / GS.memrlen) * 12 - 7 + 2 * (GS.vbd + GS.hashlen);
  GS.realmfheight = ((GS.memsize - 1) / GS.memrlen + 1) *
		     (GS.memrwidth + GS.memrspace) -
		     GS.memrspace + 2 * (GS.vbd + GS.hashlen);
  GS.statheight = ST_UBorder + ST_KeyWidth + ST_LBorder +
    (int)(ST_Skip0 * (float)GS.X.memlabelfont->max_bounds.ascent) +
    GS.X.memlabelfont->max_bounds.ascent +
    (int)(ST_Skip1 * (float)GS.X.memlabelfont->max_bounds.ascent) +
    2 * GS.X.statfont->max_bounds.ascent +
    (int)(ST_Skip2 * (float)GS.X.statfont->max_bounds.ascent);

  GS.winwidth = GS.ebwidth + 2 + GS.mfwidth + 2 + GS.X.memscrollwidth + 2;

  GS.sbprop = (float)GS.mfheight / (float)GS.realmfheight;
  if ( GS.sbprop > 1.0 )
    GS.sbprop = 1.0;

  GS.memviewport = 0;

DEBUG(DBGD)
  fprintf( stderr, "soup %lu; resolution %lu; mfwin (%dx%d)\n",
	   GS.memsize, GS.rez, GS.mfwidth, GS.mfheight );
ENDDB()

}


void
setup_visual_constants()
{

  switch ( GS.rez ) {
    case MicroRez:
      GS.hashlen = 4;
      GS.memrwidth = 100;
      GS.memrspace = 30;
     /* GS.memrlen = 500; */
      GS.vbd = 7;
      GS.memlabelskip = 5;
      break;

    case ZoomRez:
      GS.hashlen = 2;
      GS.memrwidth = 12;
      GS.memrspace = 14;
      GS.vbd = 7;
      GS.memlabelskip = 5;
      break;

    case HiRez:
      GS.hashlen = 2;
      GS.memrwidth = 5;
      GS.memrspace = 7;
      GS.vbd = 7;
      GS.memlabelskip = 5;
      break;

    case MedRez:
      GS.hashlen = 1;
      GS.memrwidth = 3;
      GS.memrspace = 4;
      GS.vbd = 5;
      GS.memlabelskip = 5;
      break;

    case LoRez:
      GS.hashlen = 0;
      GS.memrwidth = 1;
      GS.memrspace = 0;
      GS.vbd = 7;
      GS.memlabelskip = 50;
      break;
  }

}


#define	PackXResource( rl, name, class, type, size, rstruc, relem, deftype, defaddr )									\
	{								\
	  rl.resource_name = name;					\
	  rl.resource_class = class;					\
	  rl.resource_type = type;					\
	  rl.resource_size = (size);					\
	  rl.resource_offset = XtOffset( rstruc, relem );		\
	  rl.default_type = deftype;					\
	  rl.default_addr = defaddr;					\
	}


/* DONE
* init_resource_spec_list()
*
*/

int
init_resource_spec_list( rsc, rscmax )
  XtResource	*rsc;
  int		rscmax;
{
  int			rc;
  int			i;			/* HARDCODED CONSTANT below */
  static char		colournames[NUM_SPECCOLOURS][ 15 ];

  rc = 0;
  PackXResource( rsc[rc], XtNgeometry, XtCGeometry, XtRString, sizeof(char *),
		 GtXStuffPtr, geometry, XtRString, (caddr_t)NULL );
  rc++;

  PackXResource( rsc[rc], XtNiconGeometry, XtCIconGeometry, XtRString,
		 sizeof(char *), GtXStuffPtr, iconGeometry, XtRString,
		 (caddr_t)NULL );
  rc++;

  PackXResource( rsc[rc], XtNforeground, XtCForeground, XtRPixel,
		 sizeof(Pixel), GtXStuffPtr, foreground, XtRString, FGDEF );
  rc++;

  PackXResource( rsc[rc], XtNbackground, XtCBackground, XtRPixel,
		 sizeof(Pixel), GtXStuffPtr, background, XtRString, BGDEF );
  rc++;

  PackXResource( rsc[rc], XtNmemColor, XtCMemColor, XtRPixel,
		 sizeof(Pixel), GtXStuffPtr, cmemory, XtRString, CMEMDEF );
  rc++;

  PackXResource( rsc[rc], XtNotherColor, XtCOtherColor, XtRPixel,
		 sizeof(Pixel), GtXStuffPtr, cother, XtRString, COTHERDEF );
  rc++;

  PackXResource( rsc[rc], XtNinfoFont, XtCInfoFont, XtRString,
		 sizeof(String), GtXStuffPtr, ifname, XtRString, IFONTDEF );
  rc++;

  PackXResource( rsc[rc], XtNmemLabelFont, XtCMemLabelFont, XtRString,
		 sizeof(String), GtXStuffPtr, mlfname, XtRString, MLFONTDEF );
  rc++;

  PackXResource( rsc[rc], XtNstatFont, XtCStatFont, XtRString,
		 sizeof(String), GtXStuffPtr, stfname, XtRString, STFONTDEF );
  rc++;

  for ( i = 0; i < NUM_SPECCOLOURS; i++ ) {
    sprintf( colournames[i], "specColour%d", i );
    PackXResource( rsc[rc], colournames[i], XtCSpecColor, XtRPixel,
		   sizeof(Pixel), GtXStuffPtr, cspectrum[i], XtRString,
		   SpecColorDefaultNames[i] );
    rc++;
  }

  return rc;
}


/* DONE
* initialise_colours()
*
*/

void
initialise_colours()
{
  int		numpix;
  Colormap	cmap;
  XColor	*xc;

  		int           i;
  		XColor        c;
  		XColor        ctmp;

	/* sum of:	number of spectrum colours */
	/*		one "dead" memory colour */

  GS.numcolours = 0;
  cmap = DefaultColormapOfScreen( XtScreen( GS.X.top ) );

  numpix = NUM_SPECCOLOURS + 4;		/* see GtXStuff for colour counts */

/***
  if ( ! XAllocColorCells( GS.X.d, cmap, False, NULL, ZERO, xc, numpix ) ) {
    fprintf( stderr, "Whoops; out of colourmap cells in init_colours()!!\n" );
    exit( 1 );
  }

  XStoreColor( GS.X.d, cmap, &GS.X.foreground );
  XStoreColor( GS.X.d, cmap, &GS.X.background );
  XStoreColor( GS.X.d, cmap, &GS.X.cmemory );
  XStoreColor( GS.X.d, cmap, &GS.X.cother );
  XStoreColors( GS.X.d, cmap, GS.X.cspectrum, NUM_SPECCOLOURS );
***/
  
}


/* DONE
* initialise_fonts()
*
*/

void
initialise_fonts()
{
  static char		_fonterrmsg[] =
			  "bummer: can't load font %s; using default...\n";

  if (( GS.X.defaultfont = XLoadQueryFont( GS.X.d, DEFAULTFONT )
      ) == (XFontStruct *)NULL ) {
    fprintf( stderr, "Lackey, fix your X11 setup! I can't even load %s!\n",
	     DEFAULTFONT );
    exit( 1 );
  }

  if (( GS.X.infofont = XLoadQueryFont( GS.X.d, GS.X.ifname )
      ) == (XFontStruct *)NULL ) {
    fprintf( stderr, _fonterrmsg, GS.X.ifname );
    GS.X.infofont = GS.X.defaultfont;
  }

  if (( GS.X.memlabelfont = XLoadQueryFont( GS.X.d, GS.X.mlfname )
      ) == (XFontStruct *)NULL ) {
    fprintf( stderr, _fonterrmsg, GS.X.mlfname );
    GS.X.memlabelfont = GS.X.defaultfont;
  }

  if (( GS.X.statfont = XLoadQueryFont( GS.X.d, GS.X.stfname )
      ) == (XFontStruct *)NULL ) {
    fprintf( stderr, _fonterrmsg, GS.X.stfname );
    GS.X.statfont = GS.X.defaultfont;
  }

}


/* DONE
* quit_ov_proc()
*
* widget callback
*
*/

static XtCallbackProc
quit_ov_proc( w, client_data, call_data )
  Widget	w;
  XtPointer	client_data;
  XtPointer	call_data;
{
  int		i;
  MemMap	*mm, *mt;

  XFreeGC( GS.X.d, GS.X.memgc );	/* free allocated GC */

	/* free memory redraw structure */

  free_old_memmap();

  XtDestroyWidget( GS.X.top );

  exit( 0 );
}


/* DONE
* toggle_ip_active()
*
* widget callback
*
*/

static XtCallbackProc
toggle_ip_active( w, client_data, call_data )
  Widget	w;
  XtPointer	client_data;
  XtPointer	call_data;
{
  static Arg	labelArgs[] = {
    { XtNlabel, (XtArgVal)NULL },		/* ...set later */
  };
  static char	str_en[] = "enable IP";
  static char	str_dis[] = "disable IP";
  MtStatus	iRet;
  int iWhich;

  switch ( GS.ipstate ) {
    case IP_DISABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_dis );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.ipstate = IP_ENABLED;
      iWhich = M_Enable;
      if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtIPEvent,
                                (pMtOpaque)&iWhich, sizeof( int ) )
          ) != MsOK )
        ALGripe( "MGenRequest", "error %d\n", iRet );
      draw_stat_keys( 0, GS.winwidth, 0, GS.statheight, False );
      break;

    case IP_ENABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_en );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.ipstate = IP_DISABLED;
      iWhich = M_Disable;
      if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtIPEvent,
                                (pMtOpaque)&iWhich, sizeof( int ) )
          ) != MsOK )
        ALGripe( "MGenRequest", "error %d\n", iRet );
      break;
  }
}

/* DONE
* toggle_mv_active()
*
* widget callback
*
*/

static XtCallbackProc
toggle_mv_active( w, client_data, call_data )
  Widget	w;
  XtPointer	client_data;
  XtPointer	call_data;
{
  static Arg	labelArgs[] = {
    { XtNlabel, (XtArgVal)NULL },		/* ...set later */
  };
  static char	str_en[] = "enable MV";
  static char	str_dis[] = "disable MV";
  MtStatus	iRet;
  int iWhich;

  switch ( GS.mvstate ) {
    case MV_DISABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_dis );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.mvstate = MV_ENABLED;
      iWhich = M_Enable;
      if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtMVEvent,
                                (pMtOpaque)&iWhich, sizeof( int ) )
          ) != MsOK )
        ALGripe( "MGenRequest", "error %d\n", iRet );
      draw_stat_keys( 0, GS.winwidth, 0, GS.statheight, False );
      break;

    case MV_ENABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_en );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.mvstate = MV_DISABLED;
      iWhich = M_Disable;
      if (( iRet = MGenRequest( GS.hLink, MrModifyDataflow, TrtMVEvent,
                                (pMtOpaque)&iWhich, sizeof( int ) )
          ) != MsOK )
        ALGripe( "MGenRequest", "error %d\n", iRet );
      break;
  }
}

/* DONE
* toggle_ov_active()
*
* widget callback
*
*/

static XtCallbackProc
toggle_ov_active( w, client_data, call_data )
  Widget	w;
  XtPointer	client_data;
  XtPointer	call_data;
{
  static Arg	labelArgs[] = {
    { XtNlabel, (XtArgVal)NULL },		/* ...set later */
  };
  static char	str_en[] = "enable";
  static char	str_dis[] = "disable";
  MtStatus	iRet;

  switch ( GS.simstate ) {
    case OV_DISABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_dis );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.simstate = OV_ENABLED;
      if (( iRet = MGenRequest( GS.hLink, MrMessage, TrtResumeSim, M_NoData,
			        0 )) != MsOK )
	ALGripe( "MGenRequest", "failed resuming simulation (%d)\n", iRet );
      break;

    case OV_ENABLED:
      XtSetArg( labelArgs[0], XtNlabel, str_en );
      XtSetValues( w, labelArgs, XtNumber( labelArgs ) );
      GS.simstate = OV_DISABLED;
      if (( iRet = MGenRequest( GS.hLink, MrMessage, TrtPauseSim, M_NoData,
			        0 )) != MsOK )
	ALGripe( "MGenRequest", "failed pausing simulation (%d)\n", iRet );
      draw_stat_keys( 0, GS.winwidth, 0, GS.statheight, False );
      do_plan();
      break;
  }
}


XtActionProc
set_resolution( w, event, params, pcount )
  Widget	w;
  XEvent	*event;
  String	*params;
  Cardinal	*pcount;
{
  float		scrollwant;

  if ( *pcount < 1 )
    return;

  if ( strcmp( "hi", params[0] ) == 0 ) {
    if ( GS.rez == HiRez )
      return;
    GS.rez = HiRez;
  } else if ( strcmp( "med", params[0] ) == 0 ) {
    if ( GS.rez == MedRez )
      return;
    GS.rez = MedRez;
  } else if ( strcmp( "lo", params[0] ) == 0 ) {
    if ( GS.rez == LoRez )
      return;
    GS.rez = LoRez;
  } else if ( strcmp( "zoom", params[0] ) == 0 ) {
    if ( GS.rez == ZoomRez )
      return;
    GS.rez = ZoomRez;
  } else if ( strcmp( "micro", params[0] ) == 0 ) {
    if ( GS.rez == MicroRez )
      return;
    GS.rez = MicroRez;
  } else
    return;

  setup_visual_constants();

  free_old_memmap();

  scrollwant = (float)GS.memviewport / (float)GS.realmfheight;
  GS.realmfheight = ((GS.memsize - 1) / GS.memrlen + 1) *
		     (GS.memrwidth + GS.memrspace) -
		     GS.memrspace + 2 * (GS.vbd + GS.hashlen);

  generate_memmap();

  GS.sbprop = (float)GS.mfheight / (float)GS.realmfheight;
  if ( GS.sbprop > 1.0 )
    GS.sbprop = 1.0;
  if ( scrollwant > 1.0 - GS.sbprop )
    scrollwant = 1.0 - GS.sbprop;

  XawScrollbarSetThumb( GS.X.memscroll, scrollwant, GS.sbprop );
  GS.memviewport = scrollwant * (float)GS.realmfheight;

  XClearWindow( GS.X.d, XtWindow( GS.X.memframe ) );
  XClearWindow( GS.X.d, XtWindow( GS.X.etcbox ) );
  expose_memory( 0, GS.mfwidth - 1, 0, GS.mfheight - 1 );
  expose_memaxis( 0, GS.ebwidth - 1, 0, GS.mfheight - 1, GS.memviewport,
		  WhitePixel( GS.X.d, 0 ) );

}


XtCallbackProc
update_scroll( w, client_data, call_data )
  Widget	w;
  XtPointer	client_data;
  XtPointer	call_data;
{
  static float		where;
  XClientMessageEvent	ev;

  where = *(float *)call_data;
  ev.type = ClientMessage;
  ev.display = GS.X.d;
  ev.window = XtWindow( GS.X.memframe );
  ev.message_type = (Atom)MEM_SCROLL;
  ev.format = 32;
  ev.data.l[0] = (long)&where;

  XSendEvent( GS.X.d, XtWindow( GS.X.memframe ), True, ClientMessage, &ev );

}


void
actually_scroll_screen( where )
  float		where;
{
  int		newy, dely, oldvpt;
  Pixel		xorcolour;

  if ( where > 1.0 - GS.sbprop ) {
    XawScrollbarSetThumb( GS.X.memscroll, 1.0 - GS.sbprop, -1.0 );
    where = 1.0 - GS.sbprop;
  }

  newy = where * (float)GS.realmfheight;
  dely = GS.memviewport - newy;

  oldvpt = GS.memviewport;
  GS.memviewport = newy;

  if ( dely == 0 )
    return;

  xorcolour = WhitePixel( GS.X.d, 0 ) ^ BlackPixel( GS.X.d, 0 );

  if ( dely < 0 ) {

    XCopyArea( GS.X.d, XtWindow( GS.X.memframe ), XtWindow( GS.X.memframe ),
	       GS.X.memgc, 0, -dely, GS.mfwidth, GS.mfheight + dely, 0, 0 );
    XClearArea( GS.X.d, XtWindow( GS.X.memframe ), 0, GS.mfheight + dely,
		GS.mfwidth, - dely, False );
    expose_memory( 0, GS.mfwidth - 1, GS.mfheight + dely, GS.mfheight - 1 );

  } else {

    XCopyArea( GS.X.d, XtWindow( GS.X.memframe ), XtWindow( GS.X.memframe ),
	       GS.X.memgc, 0, 0, GS.mfwidth, GS.mfheight - dely, 0, dely );
    XClearArea( GS.X.d, XtWindow( GS.X.memframe ), 0, 0, GS.mfwidth, dely,
		False );
    expose_memory( 0, GS.mfwidth - 1, 0, dely - 1 );

  }

  XSetFunction( GS.X.d, GS.X.memgc, GXxor );
  expose_memaxis( 0, GS.ebwidth - 1, 0, GS.mfheight - 1, newy, xorcolour );
  expose_memaxis( 0, GS.ebwidth - 1, 0, GS.mfheight - 1, oldvpt, xorcolour );
  XSetFunction( GS.X.d, GS.X.memgc, GXcopy );

  XFlush( GS.X.d );

}


XtActionProc
query_organism( w, event, params, pcount )
  Widget	w;
  XEvent	*event;
  String	*params;
  Cardinal	*pcount;
{
  XButtonEvent		*ev;
  register int		yref, rowsep, row;
  register u_long	xmem;
  OrgMap		*om;
  int 			tsz;
  TtOrgID		OID;
  pTtOrgInfo		pOI;
  int			iOILen;
  MtStatus		iRet;

  if ( event->type != ButtonPress ) {
    fprintf( stderr, "OrgQuery() only valid when bound to a ButtonPress event\n" );
    return;
  } else if ( GS.rez == LoRez ) {
    fprintf( stderr, "OrgQuery() not valid in LoRez mode\n" );
    return;
  }
  ev = (XButtonEvent *)event;

  rowsep = GS.memrwidth + GS.memrspace;
  yref = ( ev->y + GS.memviewport - GS.vbd + 1 );
  if ( yref % rowsep >= 2 * GS.hashlen + GS.memrwidth )
    return;

  row = yref / rowsep;

  xmem = (u_long)row * (u_long)GS.memrlen + (u_long)(ev->x - GS.lbd);

  if (( om = find_org_from_orgmap( xmem ) ) == (OrgMap *)NULL )
    return;

  fprintf( stderr, "Organism: [%u:%u]\n", om->start, om->len );

  OID.start = om->start;
  OID.length = om->len;
  if (( iRet = MGenRequestWithReply( GS.hLink, MrQuery, TrtQueryOrg,
				     (pMtOpaque)&OID, sizeof( OID ),
				     (pMtOpaque)&pOI, &iOILen )) != MsOK )
    ALGripe( "MGenReqWRep.", "error (%d)\n", iRet );

  if ( pOI != NULL ) {

/***
    schlepp_off_process( oi ,tsz);
***/
    ALGripe( "info", "reply gotten ok to org query\n" );

    (void) ALFree( (char *)pOI );

  } else
    fprintf( stderr, "Oops!  Sorry, it's dead; you juuuuuust missed it!\n" );

}


int
schlepp_off_process( oi , ois)
  char *	oi;
  int ois;
{
FILE *dan_fd;
int tt;

  if ( fork() == 0 ) {
     signal(SIGIO,SIG_IGN);
     signal(SIGPIPE,SIG_IGN);
    oi[ois] = 0;
    /* printf("\n %d \n",ois); */
    dan_fd = fopen ("/tmp/schmoo","w");
    if ( dan_fd == NULL ) {perror("moo-open"); exit(-66);}
    tt = fwrite(oi,ois,ois,dan_fd);
    /* printf("%d\n",tt); */
    if ( tt < 0 ) {perror("moo-write"); exit(-66);}
    fclose(dan_fd);
    system( "xterm -e less /tmp/schmoo " ); 
    system( "rm -f /tmp/schmoo " ); 
     exit(-1); 
  }
}


/* DONE
* find_org_from_orgmap()
*
* NOTE: virtually identical to most of del_org_from_orgmap().
*
*/

OrgMap *
find_org_from_orgmap( where )
  u_long	where;
{
  OrgMap	*orgmap;

  orgmap = GS.orgmap;

  if ( (long)(where / 2) < GS.memsize / 2 ) {

    while ( orgmap->next != (OrgMap *)NULL && orgmap->next->start <= where )
      orgmap = orgmap->next;

  } else {

    while ( orgmap->prev != (OrgMap *)NULL && orgmap->prev->start >= where )
      orgmap = orgmap->prev;

  }

  if ( orgmap != (OrgMap *)NULL )
    if ( orgmap->start > where || orgmap->start + orgmap->len <= where )
      orgmap = (OrgMap *)NULL;

  return orgmap;
}


XtActionProc
redraw_mem( w, event, params, pcount )
  Widget	w;
  XEvent	*event;
  String	*params;
  Cardinal	*pcount;
{

  expose_memory( 0, GS.mfwidth - 1, 0, GS.mfheight - 1 );

}



