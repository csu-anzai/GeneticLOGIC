/*
* ovmemmap.c
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

#include <X11/Intrinsic.h>

#include "debug.h"

#include "overview.h"



void		free_old_memmap P_(( void ));
void		free_memmap_rowlist P_(( MemMap *mm ));
void 		move_ip P_(( u_long start; u_long ip_location));
void 		move_d P_(( u_long start; u_long _from_, u_long _to_));
void		add_org_to_memrow P_(( int row, int starthash, int endhash,
		  int startinrow, int sizeinrow, OrgMap *om, int dodraw,
		  int addordel ));
void		add_org_to_memmap P_(( OrgMap *om, int dodraw ));
OrgMap *	find_org_from_orgmap P_(( u_long where ));

#define		PleaseAdd		0
#define		PleaseDelete		1




/* DONE
* generate_memmap()
*
*/

void
generate_memmap()
{
  register int		i, rows;
  OrgMap		*om;
  
  if ( GS.memmap != (MemMap **)NULL )
    free_old_memmap();

  GS.memrows = ( GS.memsize - 1 ) / GS.memrlen + 1;

  if (( GS.memmap = (MemMap **)ALMalloc( GS.memrows * sizeof(MemMap *) )
      ) == (MemMap **)NULL ) {
    fprintf( stderr, "generate_memmap: error allocating memmap row ptrs\n" );
    exit( 1 );
  }

  bzero( (char *)GS.memmap, GS.memrows * sizeof(MemMap *) );

  for ( om = GS.orgmap->next; om != (OrgMap *)NULL; om = om->next )
    add_org_to_memmap( om, False );
  
}


/* DONE
* free_old_memmap()
*
*/

void
free_old_memmap()
{
  register int		i;

  for ( i = 0; i < GS.memrows; i++ )
    free_memmap_rowlist( GS.memmap[i] );

  (void) ALFree( (char *)GS.memmap );
  GS.memmap = (MemMap **)NULL;
}


/* DONE
* free_memmap_rowlist()
*
*/

void
free_memmap_rowlist( mm )
  MemMap	*mm;
{
  MemMap	*mt;

  while ( mm != (MemMap *)NULL ) {
    mt = mm;
    mm = mm->next;
    (void) ALFree( (char *)mt );
  }

}


/* DONE
* addordel_org_to_memmap()
*
*/

void
addordel_org_to_memmap( om, dodraw, addordel )
  OrgMap	*om;
  int		dodraw;
  int		addordel;
{
  register int		row;
  register int		endrow;
  register int		orgrowstart;
  register int		orgfit, morefit;
  register int		starthash, endhash;

	/* figure out in which row the organism belongs */

DEBUG(DBGD)
  fprintf( stderr, "addordel: %s org %lu:%lu\n",
	   (addordel == PleaseAdd) ? "adding" : "deleting",
	   om->start, om->len );
ENDDB()

  row = (long)om->start / GS.memrlen;
  endrow = (long)(om->start + om->len - 1) / GS.memrlen;

DEBUG(DBGD)
  fprintf( stderr, "addordel: ...goes from row %d to %d\n", row, endrow );
ENDDB()

  starthash = (long)om->start / GS.memrlen;
  endhash = ((long)om->start + (long)om->len - 1) / GS.memrlen;
  orgrowstart = (long)om->start % GS.memrlen;
  orgfit = (orgrowstart + (long)om->len > GS.memrlen)
	   ? GS.memrlen - orgrowstart : om->len;
  add_org_to_memrow( row, starthash, endhash, orgrowstart, orgfit, om, dodraw,
		     addordel );

  while ( orgfit < (long)om->len ) {

    morefit = (long)om->len - orgfit;
    if ( morefit > GS.memrlen )
      morefit = GS.memrlen;
    add_org_to_memrow( ++row, starthash, endhash, 0, morefit, om, dodraw,
		       addordel );
    orgfit += morefit;

  }
}


/* DONE
* add_org_to_memmap()
*
*/

void
add_org_to_memmap( om, dodraw )
  OrgMap	*om;
  int		dodraw;
{

  addordel_org_to_memmap( om, dodraw, PleaseAdd );

}


/* DONE
* del_org_from_memmap()
*
*/

void
del_org_from_memmap( om )
  OrgMap	*om;
{

  addordel_org_to_memmap( om, True, PleaseDelete );

}


/* DONE
* add_org_to_memrow()
*
*/

void
add_org_to_memrow( row, starthash, endhash, startinrow, sizeinrow, om,
		   dodraw, addordel )
  int		row;
  int		starthash;
  int		endhash;
  int		startinrow;
  int		sizeinrow;
  OrgMap	*om;
  int		dodraw;
  int		addordel;
{
  int		x1, x2, y1, hhash,ip_color;
  MemMap	**mm, *mnew;
  OrgMap	*om_ip;


  mm = &GS.memmap[row];

  while ( *mm != (MemMap *)NULL && (*mm)->start < startinrow )
    mm = &(*mm)->next;

  if ( *mm != (MemMap *)NULL ) {

    if ( (*mm)->start == startinrow ) {

      if ( addordel == PleaseAdd ) {
        fprintf( stderr, "add_org_to_memrow: row %d, mem %d, already taken!\n",
	         row, startinrow );
        exit( 1 );
      }

DEBUG(DBGD)
      fprintf( stderr, "del: successfully removed memmap node\n" );
ENDDB()

      mnew = *mm;
      *mm = (*mm)->next;

    } else {

      if ( addordel == PleaseDelete ) {
	fprintf( stderr, "add_org_to_memrow, deleting: can't find org at %d in row %d\n", startinrow, row );
	exit( 1 );
      }
    }

    if ( addordel == PleaseAdd && (*mm)->start < startinrow + sizeinrow ) {
      fprintf( stderr, "add_org_to_memrow: row %d, mem %d + size %d, runs into next org at %d!\n", row, startinrow, sizeinrow, (*mm)->start );
      exit( 1 );
    }

  } else {

    if ( addordel == PleaseDelete ) {
      fprintf( stderr, "add_org_to_memrow, deleteing2: can't find org at %d in row %d\n", startinrow, row );
      exit( 1 );
    }

  }

  if ( addordel == PleaseAdd ) {

    if (( mnew = (MemMap *)ALMalloc( sizeof( MemMap ) )
        ) == (MemMap *)NULL ) {
      fprintf( stderr, "add_org_to_memrow: can't malloc new memnode!\n" );
      exit( 1 );
    }

    mnew->start = startinrow;
    mnew->len = sizeinrow;
    om->ip_location =om->start; 
    om->from =om->start; 
    om->to =om->start; 
    mnew->org = om;

    mnew->next = *mm;
    *mm = mnew;
  }

  if ( dodraw ) {
    x1 = GS.lbd + startinrow;
    x2 = x1 + sizeinrow;
    hhash = GS.hashlen + ( GS.memrwidth / 2 );
    y1 = GS.vbd + hhash + ( GS.memrwidth + GS.memrspace ) * row
         - GS.memviewport;

    if ( GS.hashlen > 0 ) {
      XSetForeground( GS.X.d, GS.X.memgc,
                      ( addordel == PleaseAdd ) ? WhitePixel( GS.X.d, 0 )
                                                : BlackPixel( GS.X.d, 0 ) );
      XSetLineAttributes( GS.X.d, GS.X.memgc, 1, LineSolid, CapButt,
			  JoinMiter );
      if ( row == starthash )
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
                   x1, y1 - hhash, x1, y1 + hhash + 1 );
      if ( row == endhash )
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
                   x2-1, y1 - hhash, x2-1, y1 + hhash + 1 );
    }
    XSetLineAttributes( GS.X.d, GS.X.memgc, GS.memrwidth, LineSolid,
                        CapButt, JoinMiter );
    XSetForeground( GS.X.d, GS.X.memgc,
                    ( addordel == PleaseAdd ) ? om->colour : GS.X.cmemory );
    XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
               x1, y1, x2, y1 );
    XFlush( GS.X.d );
  }

  if ( addordel == PleaseDelete ) {
    (void) ALFree( (char *)mnew );
     if ( !dodraw || GS.ipstate == IP_DISABLED) return;
	/* blank out old critters ip */
	/* if location we are at is Free grey out old ip */

     if((om->ip_location >= om->start) && 
        (om->ip_location < (om->start+om->len)))
        ip_color = GS.X.cmemory;
     else
        {
	   /* else color with color of owning critter */
	if((om_ip = find_org_from_orgmap(om->ip_location)) == NULL)
	     {
             ip_color = GS.X.cmemory;
	     }
	else ip_color = om_ip->colour;

	}

        row = (int)((long)om->ip_location % GS.memrlen);
        x1 = GS.lbd + row -1;
        y1 = GS.vbd + 2 +(( GS.memrwidth + GS.memrspace ) * 
	     ((int) om->ip_location / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,ip_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 3, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   x1, y1 , x1, y1+GS.memrwidth-1 );
    XFlush( GS.X.d );
    }

}



/* DONE	(by daniel pirone 4/5/92)
* move_ip()
*
*/
void
move_ip ( start,  ip_location)
  u_long start;
  u_long ip_location;

{
  char ddd[84];
  int		x1, x2, y1, row ;
  u_long 	ip_color;
  OrgMap *om, *om_ip;

	/* make sure we have a critter starting at start */
  if ((start < 0) || (start > GS.memsize) || 
     (ip_location < 0) || (ip_location > GS.memsize)) 
     {
     fprintf(stderr,"\nMOVE_IP: passed params out of bounds\n");
     return;
     }
   if((om = find_org_from_orgmap(start)) == NULL)
     {
     fprintf(stderr,"\nMOVE_IP: passed not in orgmap!\n");
     return;
     }

	/* if location we are at is Free grey out old ip */

     if((om->ip_location >= om->start) && 
        (om->ip_location < (om->start+om->len)))
        ip_color = om->colour;
     else
        {
	   /* else color with color of owning critter */
	if((om_ip = find_org_from_orgmap(ip_location)) == NULL)
	     {
             ip_color = GS.X.cmemory;
	     }
	else ip_color = om_ip->colour;

	}

        row = (int)((long)om->ip_location % GS.memrlen);
        x1 = GS.lbd + row -1;
        y1 = GS.vbd + 2 +(( GS.memrwidth + GS.memrspace ) * 
	     ((int) om->ip_location / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,ip_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 3, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   x1, y1 , x1, y1+GS.memrwidth-1 );

	/* update its ip_location */
	om->ip_location = ip_location;


	/* if location we are at is Free DRAW ip in our color */
     if((om->ip_location >= om->start) && 
        (om->ip_location < (om->start+om->len)))
	     ip_color = BlackPixel(GS.X.d, 0 );
     else
        {
	/* else color with our color */
	ip_color = om->colour;
	}

        row = om->ip_location % GS.memrlen;
        x1 = GS.lbd + row -1;
        y1 = GS.vbd + 2 + (( GS.memrwidth + GS.memrspace ) * 
	     ((u_long) om->ip_location / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,ip_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 3, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   x1, y1 , x1, y1+GS.memrwidth-1 );
    XFlush( GS.X.d );

}

/* DONE	(by daniel pirone 4/14/92)
* move_d()
*
*/
void
move_d ( start,  _from_, _to_)
  u_long start;
  u_long _from_;
  u_long _to_;

{
  char ddd[84];
  int		t_x1, t_x2, t_y1, t_row ;
  u_long 	from_color, to_color;
  OrgMap *om, *om_f, *om_t;

	/* make sure we have a critter starting at start */
  if ((start < 0) || (start > GS.memsize) || 
     (_from_ < 0) || (_from_ > GS.memsize)||
     (_to_ < 0) || (_to_ > GS.memsize)) 
     {
     fprintf(stderr,"\nMOVE_D: passed params out of bounds\n");
     return;
     }
   if((om = find_org_from_orgmap(start)) == NULL)
     {
     fprintf(stderr,"\nMOVE_D: passed not in orgmap!\n");
     return;
     }

	/* if location we are at is Free grey out old from */

     if((om->from >= om->start) && 
        (om->from < (om->start+om->len)))
        from_color = om->colour;
     else
        {
	   /* else color with color of owning critter */
	if((om_f = find_org_from_orgmap(om->from)) == NULL)
	     {
             from_color = GS.X.cmemory;
	     }
	else from_color = om_f->colour;

	}

	/* if location we are at is Free grey out old to */

     if((om->to >= om->start) && 
        (om->to < (om->start+om->len)))
        to_color = om->colour;
     else
        {
	   /* else color with color of owning critter */
	if((om_t = find_org_from_orgmap(om->to)) == NULL)
	     {
             to_color = GS.X.cmemory;
	     }
	else to_color = om_t->colour;

	}

        t_row = (int)((long)om->to % GS.memrlen);
        t_x1 = GS.lbd + t_row -1;
        t_y1 = GS.vbd + 2 +(( GS.memrwidth + GS.memrspace ) * 
	     ((int) om->to / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,to_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 4, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1, t_y1 , t_x1, t_y1+GS.memrwidth-1 );

        t_row = (int)((long)om->from % GS.memrlen);
        t_x1 = GS.lbd + t_row -1;
        t_y1 = GS.vbd + 2 +(( GS.memrwidth + GS.memrspace ) * 
	     ((int) om->from / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,from_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 4, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1, t_y1 , t_x1, t_y1+GS.memrwidth-1 );

	/* update its to and from locations */

	om->to = _to_;
	om->from = _from_;


	/* if location we are at is Free DRAW  in our color */
     if((om->to >= om->start) && 
        (om->to < (om->start+om->len)))
	     to_color = BlackPixel(GS.X.d, 0 );
     else
        {
	/* else color with our color */
	to_color = om->colour;
	}

     if((om->from >= om->start) && 
        (om->from < (om->start+om->len)))
	     from_color = BlackPixel(GS.X.d, 0 );
     else
        {
	/* else color with our color */
	from_color = om->colour;
	}

        t_row = om->to % GS.memrlen;
        t_x1 = GS.lbd + t_row -1;
        t_y1 = GS.vbd + 2 + (( GS.memrwidth + GS.memrspace ) * 
	     ((u_long) om->to / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,BlackPixel(GS.X.d, 0 )); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 2, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1, t_y1 , t_x1, t_y1+GS.memrwidth-1 );
        XSetForeground( GS.X.d, GS.X.memgc,to_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 2, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1+2, t_y1 , t_x1+2, t_y1+GS.memrwidth-1 );

        t_row = om->from % GS.memrlen;
        t_x1 = GS.lbd + t_row -1;
        t_y1 = GS.vbd + 2 + (( GS.memrwidth + GS.memrspace ) * 
	     ((u_long) om->from / GS.memrlen)) - GS.memviewport;

        XSetForeground( GS.X.d, GS.X.memgc,WhitePixel(GS.X.d, 0 )); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 2, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1, t_y1 , t_x1, t_y1+GS.memrwidth-1 );
        XSetForeground( GS.X.d, GS.X.memgc,from_color); 
        XSetLineAttributes( GS.X.d, GS.X.memgc, 2, LineSolid, CapButt,
			  JoinMiter );
        XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   t_x1+2, t_y1 , t_x1+2, t_y1+GS.memrwidth-1 );

    XFlush( GS.X.d );

}


