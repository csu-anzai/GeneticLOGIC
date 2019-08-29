/*
* ovexpose.c
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <X11/Intrinsic.h>

#include "portlayr.h"

#include "debug.h"

#include "overview.h"



extern void		actually_scroll_screen P_(( float where ));


void		expose_memory P_(( int xlo, int xhi, int ylo, int yhi ));
void		expose_memory_row P_(( int xlo, int xhi, int row ));
void		expose_memaxis_row P_(( int xlo, int xhi, int row,
		  int viewport ));
void		handle_client_message P_(( Widget w, XtPointer client_data,
		  XClientMessageEvent *event, Boolean *ctd ));
void		expose_stats P_(( int xlo, int xhi, int ylo, int yhi ));
void		draw_stat_keys P_(( int xlo, int xhi, int ylo, int yhi,
		  int force ));





/* DONE
* expose_memframe()
*
* event handler
*
*/

XtEventHandler
expose_memframe( w, client_data, event, ctd )
  Widget	w;
  XtPointer	client_data;
  XEvent	*event;
  Boolean	*ctd;
{
  XExposeEvent		*ev;
  static int		state = 0;
  static int		xhi, xlo, yhi, ylo;
  register int		valtmp;

  if ( event->type == NoExpose )
    return;

  if ( event->type == ClientMessage ) {
    handle_client_message( w, client_data, (XClientMessageEvent *)event, ctd );
    return;
  }

  if ( event->type != Expose && event->type != GraphicsExpose ) {
    fprintf( stderr, "expose_ovframe() got event %d... ???\n", event->type );
    return;
  }
  ev = (XExposeEvent *)event;

  if ( state == 0 ) {

    state = 1;
    xlo = ev->x;
    xhi = xlo + ev->width - 1;
    ylo = ev->y;
    yhi = ylo + ev->height - 1;

  } else {

    if ( ev->x < xlo )
      xlo = ev->x;
    if ( (valtmp = ev->x + ev->width - 1) > xhi )
      xhi = valtmp;

    if ( ev->y < ylo )
      ylo = ev->y;
    if ( (valtmp = ev->y + ev->height - 1) > yhi )
      yhi = valtmp;

  }
  if ( ev->count == 0 ) {

    state = 0;
    expose_memory( xlo, xhi, ylo, yhi );

  }
}


/* DONE
* expose_memory()
*
*/

void
expose_memory( xlo, xhi, ylo, yhi )
  int		xlo;
  int		xhi;
  int		ylo;
  int		yhi;
{
  register int		row, startrow, endrow, rowsep;

  rowsep = GS.memrwidth + GS.memrspace;

  startrow = ( ylo + GS.memviewport - GS.vbd ) / rowsep;
  if ( startrow >= GS.memrows )
    return;

  endrow = ( yhi + GS.memviewport - GS.vbd ) / rowsep;
  if ( endrow < 0 )
    return;

  if ( startrow < 0 )		startrow = 0;
  if ( endrow >= GS.memrows )	endrow = GS.memrows - 1;

  for ( row = startrow; row <= endrow; row++ )
    expose_memory_row( xlo, xhi, row );

  XFlush( GS.X.d );

}


/* DONE
* expose_memory_row()
*
*/

void
expose_memory_row( xlo, xhi, row )
  int		xlo;
  int		xhi;
  int		row;
{
  register MemMap	*mm;
  register int		hhash;
  register int		introw;
  register int		oldmmend;
  register int		x1, x2, y1;
  register int		starthash, endhash;

  if ( GS.hashlen == 0 )
    XSetLineAttributes( GS.X.d, GS.X.memgc, GS.memrwidth, LineSolid,
			CapButt, JoinMiter );

  hhash = GS.hashlen + ( GS.memrwidth / 2 );
  introw = GS.memrwidth + GS.memrspace;

  oldmmend = 0;		/* actually end of an organism + 1 */
  y1 = GS.vbd + hhash + introw * row - GS.memviewport;

  XSetLineAttributes( GS.X.d, GS.X.memgc, GS.memrwidth , LineSolid, CapButt,
		      JoinMiter );

  for ( mm = GS.memmap[row]; mm != (MemMap *)NULL; mm = mm->next ) {

    starthash = (long)mm->org->start / GS.memrlen;
    endhash = ((long)mm->org->start + (long)mm->org->len - 1) / GS.memrlen;

    if ( mm->start - oldmmend > 0 ) {	/* there is blank mem to draw */

      x1 = GS.lbd + oldmmend;
      x2 = GS.lbd + mm->start;

      if ( ! (xlo > x2 || xhi < x1) ) {
        XSetForeground( GS.X.d, GS.X.memgc, GS.X.cmemory );
	XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   x1, y1, x2, y1 );
      }
    }

    x1 = GS.lbd + mm->start;
    x2 = x1 + mm->len;

    if ( ! (xlo > x2 || xhi < x1 ) ) {
      if ( GS.hashlen > 0 ) {
        XSetForeground( GS.X.d, GS.X.memgc, WhitePixel( GS.X.d, 0 ) );
	XSetLineAttributes( GS.X.d, GS.X.memgc, 1, LineSolid, CapButt,
			    JoinMiter );
	if ( row == starthash )
	  XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		     x1, y1 - hhash, x1, y1 + hhash + 1 );
	if ( row == endhash )
	  XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		     x2-1, y1 - hhash, x2-1, y1 + hhash + 1 );
	XSetLineAttributes( GS.X.d, GS.X.memgc, GS.memrwidth, LineSolid,
			    CapButt, JoinMiter );
      }
      XSetForeground( GS.X.d, GS.X.memgc, mm->org->colour );
      XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		 x1, y1, x2, y1 );
    }

    oldmmend = mm->start + mm->len;

  }

  if ( GS.memrlen - oldmmend > 0 ) {	/* there is blank mem to draw */

    x1 = GS.lbd + oldmmend;
    x2 = (row == GS.memrows - 1) ? GS.lbd + (GS.memsize - 1) % GS.memrlen + 2
				 : GS.lbd + GS.memrlen;

    if ( ! (xlo > x2 || xhi < x1) ) {
      XSetForeground( GS.X.d, GS.X.memgc, GS.X.cmemory );
      XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		 x1, y1, x2, y1 );
    }
  }

}


/* NOTE:  look into the possibility of moving the functionality of
*  expose_etcbox() and expose_memaxis() into expose_memframe() and
*  expose_memory(); expose_etcbox() is identical to expose_memframe(),
*  and expose_memaxis() is 90% identical to expose_memory().
*/


/* DONE
* expose_etcbox()
*
*/

XtEventHandler
expose_etcbox( w, client_data, event, ctd )
  Widget	w;
  XtPointer	client_data;
  XEvent	*event;
  Boolean	*ctd;
{
  XExposeEvent		*ev;
  static int		state = 0;
  static int		xhi, xlo, yhi, ylo;
  register int		valtmp;

  if ( event->type == NoExpose )
    return;

  if ( event->type != Expose && event->type != GraphicsExpose ) {
    fprintf( stderr, "expose_etcbox() got event %d... ???\n", event->type );
    return;
  }
  ev = (XExposeEvent *)event;

  if ( state == 0 ) {

    state = 1;
    xlo = ev->x;
    xhi = xlo + ev->width - 1;
    ylo = ev->y;
    yhi = ylo + ev->height - 1;

  } else {

    if ( ev->x < xlo )
      xlo = ev->x;
    if ( (valtmp = ev->x + ev->width - 1) > xhi )
      xhi = valtmp;

    if ( ev->y < ylo )
      ylo = ev->y;
    if ( (valtmp = ev->y + ev->height - 1) > yhi )
      yhi = valtmp;

  }
  if ( ev->count == 0 ) {
    
    state = 0;
    expose_memaxis( xlo, xhi, ylo, yhi, GS.memviewport,
		    WhitePixel( GS.X.d, 0 ) );
  }
}


/* DONE
* expose_memaxis
*
*/

expose_memaxis( xlo, xhi, ylo, yhi, viewport, colour )
  int		xlo;
  int		xhi;
  int		ylo;
  int		yhi;
  int		viewport;
  Pixel		colour;
{
  register int		row, startrow, endrow, rowsep;

  rowsep = GS.memrwidth + GS.memrspace;

  startrow = ( ylo + viewport - GS.vbd ) / rowsep;
  if ( startrow >= GS.memrows )
    return;

  endrow = ( yhi + viewport - GS.vbd ) / rowsep;
  if ( endrow < 0 )
    return;

  if ( startrow < 0 )		startrow = 0;
  startrow -= startrow % GS.memlabelskip;

  endrow++;
  if ( endrow >= GS.memrows )	endrow = GS.memrows - 1;

  XSetFont( GS.X.d, GS.X.memgc, GS.X.memlabelfont->fid );
  XSetForeground( GS.X.d, GS.X.memgc, colour );

  for ( row = startrow; row <= endrow; row += GS.memlabelskip )
    expose_memaxis_row( xlo, xhi, row, viewport );

  XFlush( GS.X.d );

}


/* DONE
* expose_memaxis_row()
*
* NOTE that xhi and xlo are unused so far.
*
*/

void
expose_memaxis_row( xlo, xhi, row, viewport )
  int		xlo;
  int		xhi;
  int		row;
  int		viewport;
{
  char		labelbuf[10];	/* big enough to store "9000000".  i doubt */
				/* we'll be running 9 Meg soups! */
  register int	x, y, lx1, lx2, ly;

  sprintf( labelbuf, "%d", row * GS.memrlen );

  lx1 = GS.ebwidth - GS.X.memlabelfont->max_bounds.rbearing +
	GS.X.memlabelfont->min_bounds.lbearing;
  x = lx1 - XTextWidth( GS.X.memlabelfont, labelbuf, strlen( labelbuf ) );
  lx1 += 2;
  lx2 = GS.ebwidth - 1;

  ly = GS.vbd + row * ( GS.memrwidth + GS.memrspace ) + ( GS.memrwidth / 2 ) -
	viewport;
  y = ly + ( GS.X.memlabelfont->max_bounds.ascent / 2 );
  ly += GS.hashlen;

  XDrawString( GS.X.d, XtWindow( GS.X.etcbox ), GS.X.memgc, x, y, labelbuf,
	       strlen( labelbuf ) );
  XDrawLine( GS.X.d, XtWindow( GS.X.etcbox ), GS.X.memgc, lx1, ly, lx2, ly );
  
}


void
handle_client_message( w, client_data, event, ctd )
  Widget		w;
  XtPointer		client_data;
  XClientMessageEvent	*event;
  Boolean		*ctd;
{

  if ( event->message_type != MEM_SCROLL ) {
    fprintf( stderr, "handle_client_message: message_type %d unknown\n",
	     event->message_type );
    return;
  }

  actually_scroll_screen( *(float *)event->data.l[0] );

}


/* DONE
* expose_statbox()
*
* event handler
*
*/

XtEventHandler
expose_statbox( w, client_data, event, ctd )
  Widget	w;
  XtPointer	client_data;
  XEvent	*event;
  Boolean	*ctd;
{
  XExposeEvent		*ev;
  static int		state = 0;
  static int		si_xhi = 0;
  static int		si_xlo = 0;
  static int		si_yhi = 0;
  static int		si_ylo = 0;
  register int		valtmp;

  if ( event->type != Expose ) {
    fprintf( stderr, "expose_ovframe() got event %d... ???\n", event->type );
    return;
  }
  ev = (XExposeEvent *)event;

  if ( state == 0 ) {

    state = 1;
    si_xlo = ev->x;
    si_xhi = si_xlo + ev->width - 1;
    si_ylo = ev->y;
    si_yhi = si_ylo + ev->height - 1;

  } else {

    if ( ev->x < si_xlo )
      si_xlo = ev->x;
    if ( (valtmp = ev->x + ev->width - 1) > si_xhi )
      si_xhi = valtmp;

    if ( ev->y < si_ylo )
      si_ylo = ev->y;
    if ( (valtmp = ev->y + ev->height - 1) > si_yhi )
      si_yhi = valtmp;

  }
  if ( ev->count == 0 ) {

    state = 0;
    expose_stats( si_xlo, si_xhi, si_ylo, si_yhi );

  }
}


void
expose_stats( xlo, xhi, ylo, yhi )
  int		xlo;
  int		xhi;
  int		ylo;
  int		yhi;
{

  draw_stat_keys( xlo, xhi, ylo, yhi, True );

}


void
draw_stat_keys( xlo, xhi, ylo, yhi, force )
  int		xlo;
  int		xhi;
  int		ylo;
  int		yhi;
  int		force;
{
  register int		i, x, y, dx;
  register Sizemap	*sm;
  char			textbuf[32];	/* big enough; HARDCODED CONSTANT */
  register int		notused;
  register int		skipspace0, skipspace1, skipspace2;
  static char		_stat_not_applicable[] = "   < n/a >   ";
  static char		_stat_none[] = "      ---      ";
  u_long		popsum;
  float			wpopsum;

  XSetLineAttributes( GS.X.d, GS.X.memgc, ST_KeyWidth, LineSolid,
		      CapButt, JoinMiter );
  dx = GS.mfwidth / (NUM_SPECCOLOURS + 1);
  x = GS.ebwidth + 3 - dx / 2;
  skipspace0 = (int)(ST_Skip0 * (float)GS.X.memlabelfont->max_bounds.ascent);
  skipspace1 = (int)(ST_Skip1 * (float)GS.X.memlabelfont->max_bounds.ascent);
  skipspace2 = (int)(ST_Skip2 * (float)GS.X.statfont->max_bounds.ascent);

  sm = GS.sizemaproot;

  for ( i = 0; i <= NUM_SPECCOLOURS; i++ ) {

    if ( ! force ) {
      if ( sm == (Sizemap *)NULL ) {
	int		j;

	for ( j = 0; j < 4; j++ )
	  GS.key_list[i][j] = 0L;
      } else {
	if ( i < NUM_SPECCOLOURS ) {
	  GS.key_list[i][0] = sm->len;
	  GS.key_list[i][2] = sm->pop;
	  GS.key_list[i][3] = (u_long)sm->colour;
			   			/* HARDCODED CONSTANT */
	  GS.key_list[i][1] = (u_long)( ( (float)sm->pop * (float)sm->len *
					  10000.0 ) / (float)GS.memsize );
	  sm = sm->next;
	} else {
	  GS.key_list[i][0] = 1L;	/* any nonzero */
	  GS.key_list[i][3] = GS.X.cother;

	  popsum = 0L;
	  wpopsum = 0.0;
	  while ( sm != (Sizemap *)NULL ) {
	    popsum += sm->pop;
	    wpopsum += ( (float)sm->pop * (float)sm->len ) / (float)GS.memsize;
	    sm = sm->next;
	  }

	  GS.key_list[i][2] = popsum;
	  GS.key_list[i][1] = (u_long)( wpopsum * 10000.0 );
	}
      }
    }

    notused = ( GS.key_list[i][0] == 0 );
    x += dx;
    y = ST_UBorder + (ST_KeyWidth / 2);

    if ( notused ) {		/* no size here yet */
      XSetForeground( GS.X.d, GS.X.memgc, GS.X.cmemory );
      XDrawLine( GS.X.d, XtWindow( GS.X.statbox ), GS.X.memgc,
	         x - (ST_KeyLength / 2), y, x + (ST_KeyLength / 2), y );
      XSetForeground( GS.X.d, GS.X.memgc, GS.X.cother );
    } else {
      XSetForeground( GS.X.d, GS.X.memgc,
	( i == NUM_SPECCOLOURS ) ? GS.X.cother : GS.key_list[i][3] );
      XDrawLine( GS.X.d, XtWindow( GS.X.statbox ), GS.X.memgc,
	         x - (ST_KeyLength / 2), y, x + (ST_KeyLength / 2), y );
      XSetForeground( GS.X.d, GS.X.memgc, WhitePixel( GS.X.d, 0 ) );
    }

    y = ST_UBorder + ST_KeyWidth + skipspace0 +
	GS.X.memlabelfont->max_bounds.ascent;

    if ( notused )
      strcpy( textbuf, _stat_not_applicable );
    else {
      if ( i == NUM_SPECCOLOURS )
	strcpy( textbuf, "   others   " );
      else
        sprintf( textbuf, "     %lu     ", GS.key_list[i][0] );
    }

    XSetForeground( GS.X.d, GS.X.memgc, WhitePixel( GS.X.d, 0 ) );
    XSetBackground( GS.X.d, GS.X.memgc, BlackPixel( GS.X.d, 0 ) );
    XSetFont( GS.X.d, GS.X.memgc, GS.X.memlabelfont->fid );
    XDrawImageString( GS.X.d, XtWindow( GS.X.statbox ), GS.X.memgc,
      x - XTextWidth( GS.X.memlabelfont, textbuf, strlen( textbuf ) ) / 2, y,
      textbuf, strlen( textbuf ) );

    y += skipspace1 + GS.X.statfont->max_bounds.ascent;

    if ( notused )
      strcpy( textbuf, _stat_none );
    else
      sprintf( textbuf, "  %.4f  ", (float)GS.key_list[i][1] / 10000.0 );

    XSetFont( GS.X.d, GS.X.memgc, GS.X.statfont->fid );
    XDrawImageString( GS.X.d, XtWindow( GS.X.statbox ), GS.X.memgc,
      x - XTextWidth( GS.X.statfont, textbuf, strlen( textbuf ) ) / 2, y,
      textbuf, strlen( textbuf ) );

    y += skipspace2 + GS.X.statfont->max_bounds.ascent;

    if ( notused )
      strcpy( textbuf, _stat_none );
    else
      sprintf( textbuf, "     %lu     ", GS.key_list[i][2] );

    XDrawImageString( GS.X.d, XtWindow( GS.X.statbox ), GS.X.memgc,
      x - XTextWidth( GS.X.statfont, textbuf, strlen( textbuf ) ) / 2, y,
      textbuf, strlen( textbuf ) );

  }

  XFlush( GS.X.d );
}


