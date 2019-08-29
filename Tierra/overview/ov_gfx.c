/*
* ov_gfx.c
*
* Copyright (c) 1991, 1992 by Marc W. Cygnus and Virtual Life
* All Rights Reserved.
*
*/

#include <stdio.h>

#include <sys/param.h>		/* stuff like hostname length */

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>

#include "portlayr.h"

#include "overview.h"

#include "debug.h"


/*** prototypes ********************************************************/

void		re_add_to_sizemap();
Pixel		pop_colour_assignment P_(( void ));
void		push_colour_assignment P_(( Pixel colour ));
void		depict_colour_swap P_(( u_long len1, u_long c1, u_long len2,
		  u_long c2 ));
u_long		add_org_to_sizemap P_(( OrgMap *om ));
void		del_org_from_sizemap P_(( OrgMap *om ));

extern OrgMap	*del_org_from_orgmap();


/*** application specific **********************************************/





/* DONE
* initialise_ov_statistics()
*
*/

void
initialise_ov_statistics()
{
  int		i;

  GS.sizemaproot = (Sizemap *)NULL;
  for ( i = 0; i < NUM_SPECCOLOURS; i++ )
    GS.colours[i] = GS.X.cspectrum[ NUM_SPECCOLOURS - 1 - i ];
  GS.numcolours = NUM_SPECCOLOURS;

}


/* DONE
* record_organism_birth()
*
*/

void
record_organism_birth( start, length )
  u_long	start;
  u_long	length;
{
  OrgMap	*om;
  static int	addcounter = 0;

DEBUG(DBGD)
	fprintf( stderr, "B: %lu:%lu\n", start, length ); 
ENDDB()

  if (( om = (OrgMap *)ALMalloc( sizeof( OrgMap ) )) == (OrgMap *)NULL ) {
    fprintf( stderr, "record_organism_birth: can't malloc orgmap\n" );
    exit( 1 );
  }
  om->start = start;
  om->len = length;
  om->colour = add_org_to_sizemap( om );

  add_org_to_orgmap( om );
  add_org_to_memmap( om, True );

  if ( ++addcounter == ST_UpdateRate ) {
    addcounter = 0;
    draw_stat_keys( 0, GS.winwidth, 0, GS.statheight, False );
  }

}


/* DONE
* add_org_to_sizemap()
*
*/

u_long
add_org_to_sizemap( om )
  OrgMap	*om;
{
  Sizemap	**node, *sptr, *sprev, *movenode;

  node = &(GS.sizemaproot);
  sprev = (Sizemap *)NULL;

  while ( *node != (Sizemap *)NULL && (*node)->len != om->len ) {
    sprev = *node;
    node = &( (*node)->next );
  }

  if ( *node == (Sizemap *)NULL ) {

    if (( sptr = (Sizemap *)ALMalloc( sizeof( Sizemap ) )
	) == (Sizemap *)NULL ) {
      fprintf( stderr, "error malloc-ing Sizemap node!\n" );
      exit( 1 );
    }
    sptr->len = om->len;
    sptr->pop = 1;
    sptr->colour = pop_colour_assignment();
    sptr->next = (Sizemap *)NULL;
    *node = sptr;

  } else {

    (*node)->pop++;
    if ( sprev != (Sizemap *)NULL && (*node)->pop > sprev->pop ) {
      movenode = *node;
      *node = (*node)->next;

      re_add_to_sizemap( &(GS.sizemaproot), movenode );

      return movenode->colour;
    }
  }

  return (*node)->colour;
}


/* DONE
* re_add_to_sizemap()
*
*/

void
re_add_to_sizemap( node, newnode )
  Sizemap	**node;
  Sizemap	*newnode;
{
  Sizemap	*sptr;
  int		count;

  count = 0;
  while ( *node != (Sizemap *)NULL && (*node)->pop > newnode->pop ) {
    node = &((*node)->next);
    count++;
  }
  sptr = *node;
  *node = newnode;
  newnode->next = sptr;

  if ( newnode->colour != GS.X.cother )
    return;

  if ( count >= NUM_SPECCOLOURS )
    newnode->colour = GS.X.cother;
  else {
    while ( *node != (Sizemap *)NULL && count < NUM_SPECCOLOURS ) {
      node = &((*node)->next);
      count++;
    }
    newnode->colour = (*node)->colour;
    (*node)->colour = GS.X.cother;
    depict_colour_swap( newnode->len, newnode->colour, (*node)->len,
		        (*node)->colour );
  }
}


/* DONE
* record_organism_death()
*
*/

void
record_organism_death( start, length )
  u_long	start;
  u_long	length;
{
  OrgMap	*om;
  static int	delcounter;

DEBUG(DBGD)
  fprintf( stderr, "     D: %lu:%lu\n", start, length );
ENDDB()

  om = del_org_from_orgmap( start );
  del_org_from_memmap( om );
  del_org_from_sizemap( om );

  if ( ++delcounter == ST_UpdateRate ) {
    delcounter = 0;
    draw_stat_keys( 0, GS.winwidth, 0, GS.statheight, False );
  }

  ALFree( (char *)om );
}


/*
* DONE
* del_org_from_sizemap()
*
*/

void
del_org_from_sizemap( om )
  OrgMap	*om;
{
  Sizemap	**node, *moveblock, *stmp;
  int		count;

  node = &GS.sizemaproot;

  count = 0;
  while ( *node != (Sizemap *)NULL && (*node)->len != om->len ) {
    node = &((*node)->next);
    count++;
  }

  if ( *node == (Sizemap *)NULL ) {
    fprintf( stderr, "Bogus death: size = %d!\n", om->len );
    return;
  }

  (*node)->pop--;

  if ( (*node)->pop == 0 ) {

    stmp = *node;
    *node = (*node)->next;

    if ( count < NUM_SPECCOLOURS ) {
      while ( *node != (Sizemap *)NULL && count < (NUM_SPECCOLOURS - 1) ) {
	node = &((*node)->next);
	count++;
      }
      if ( *node != (Sizemap *)NULL ) {
	(*node)->colour = stmp->colour;
	depict_colour_swap( stmp->len, GS.X.cmemory, (*node)->len,
			    (*node)->colour );
      } else
        push_colour_assignment( stmp->colour );
    } 
    ALFree( (char *)stmp );

  } else if ( (*node)->next != (Sizemap *)NULL &&
	      (*node)->next->pop > (*node)->pop ) {

    moveblock = *node;
    *node = (*node)->next;
    if ( count < NUM_SPECCOLOURS ) {
      while ( *node != (Sizemap *)NULL && count < (NUM_SPECCOLOURS - 1) ) {
	node = &((*node)->next);
	count++;
      }
      if ( *node != (Sizemap *)NULL ) {
	(*node)->colour = moveblock->colour;
	moveblock->colour = GS.X.cother;
	depict_colour_swap( moveblock->len, moveblock->colour, (*node)->len,
			    (*node)->colour );
      }
    }

    re_add_to_sizemap( &GS.sizemaproot, moveblock );

  }

}


/*** FOR DEBUGGING PURPOSES ONLY ***/

/* DONE
* depict_sizemaplist()
*
*/

void
depict_sizemaplist()
{
  Sizemap	*s;

  printf( "Sizemap: " );
  s = GS.sizemaproot;
  while ( s != (Sizemap *)NULL ) {
    printf( "[%d;%d=%d]->", s->len, s->pop, s->colour );
    s = s->next;
  }
  printf( "[NULL]\n" );
}

/*** END DEBUG CODE ***/


/* DONE
* pop_colour_assignment()
*
*/

Pixel
pop_colour_assignment()
{
  if ( GS.numcolours == 0 )
    return GS.X.cother;

  return GS.colours[ --GS.numcolours ];
}


/* DONE
* push_colour_assignment()
*
*/

void
push_colour_assignment( colour )
  Pixel		colour;
{
  if ( GS.numcolours == NUM_SPECCOLOURS ) {
    fprintf( stderr, "push_colour_assignment(): stack overflow!\n" );
    return;
  }
  if ( colour != GS.X.cother )
    GS.colours[ GS.numcolours++ ] = colour;
}


/* DONE
* depict_colour_swap()
*
*/

void
depict_colour_swap( len1, c1, len2, c2 )
  u_long	len1;
  u_long	c1;
  u_long	len2;
  u_long	c2;
{
  MemMap	*mm;
  int		i;
  int		x1, x2, y1, hhash, introw;
  int		redraw;

  redraw = False;

  hhash = GS.hashlen + ( GS.memrwidth / 2 );
  introw = ( GS.memrwidth + GS.memrspace );
  y1 = GS.vbd + hhash - introw - GS.memviewport;

  XSetLineAttributes( GS.X.d, GS.X.memgc, GS.memrwidth, LineSolid,
		      CapButt, JoinMiter );

  for ( i = 0; i < GS.memrows; i++ ) {

    y1 += introw;
    mm = GS.memmap[i];

    for ( mm = GS.memmap[i]; mm != (MemMap *)NULL; mm = mm->next ) {

      if ( mm->org->len == len1 ) {
	mm->org->colour = c1;
	redraw = True;
      } else if ( mm->org->len == len2 ) {
	mm->org->colour = c2;
	redraw = True;
      }

      if ( redraw == True && y1 - hhash >= 0 && y1 + hhash < GS.mfheight ) {
	redraw = False;
	x1 = GS.lbd + mm->start;
	x2 = x1 + mm->len;
	XSetForeground( GS.X.d, GS.X.memgc, mm->org->colour );
	XDrawLine( GS.X.d, XtWindow( GS.X.memframe ), GS.X.memgc,
		   x1, y1, x2, y1);
      }

    }

  }
}

