/*
* ovstats.c
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



/* DONE
* initialise_orglist()
*
*/

void
initialise_orglist()
{
  OrgMap		*om;
  register int		i, j;

  if (( om = (OrgMap *)ALMalloc( sizeof( OrgMap ) )) == (OrgMap *)NULL ) {
    fprintf( stderr, "init_memoryspace: can't malloc orgmap\n" );
    exit( 1 );
  }

  GS.orgmap = om;

  om->start = 0;
  om->len = 0;
  om->colour = 0;
  om->next = (OrgMap *)NULL;
  om->prev = (OrgMap *)NULL;

  for ( i = 0; i < (NUM_SPECCOLOURS+1); i++ )	/* HARDCODED CONSTANT */
    for ( j = 0; j < 4; j++ )			/* HARDCODED CONSTANT */
      GS.key_list[i][j] = 0;
}


/* DONE
* add_org_to_orgmap()
*
*/

void
add_org_to_orgmap( om )
  OrgMap	*om;
{
  OrgMap	*orgmap;

  orgmap = GS.orgmap;
  orgmap->start++;	/* header node; start is population, so increment */

  if ( (long)(om->start / 2) < GS.memsize / 2 ) {

    while ( orgmap->next != (OrgMap *)NULL && orgmap->next->start < om->start )
      orgmap = orgmap->next;

  } else {

    while ( orgmap->prev != (OrgMap *)NULL && orgmap->prev->start > om->start )
      orgmap = orgmap->prev;
  }

  om->next = orgmap->next;
  orgmap->next = om;
  om->prev = orgmap;
  if ( om->next == (OrgMap *)NULL )
    GS.orgmap->prev = om;
  else
    om->next->prev = om;
  
}


/* DONE
* del_org_from_orgmap()
*
*/

OrgMap *
del_org_from_orgmap( start )
  u_long	start;
{
  OrgMap	*orgmap;

  orgmap = GS.orgmap;
  orgmap->start--;

  if ( (long)(start / 2) < GS.memsize / 2 ) {

    while ( orgmap->next != (OrgMap *)NULL && orgmap->next->start <= start )
      orgmap = orgmap->next;

  } else {

    while ( orgmap->prev != (OrgMap *)NULL && orgmap->prev->start >= start )
      orgmap = orgmap->prev;

  }

  if ( orgmap == (OrgMap *)NULL ) {
    fprintf( stderr, "del_org_from_orgmap: bogus death! start = %d\n", start );
    GS.orgmap->start++;
  }

  orgmap->prev->next = orgmap->next;
  if ( orgmap->next == (OrgMap *)NULL )
    GS.orgmap->prev = orgmap->next;
  else
    orgmap->next->prev = orgmap->prev;

  return orgmap;
}


