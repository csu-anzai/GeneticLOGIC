
/*************************************************************/
/*                                                           */
/*  Copyright (c) 1990                                       */
/*  Darrell L. Whitley                                       */
/*  Computer Science Department                              */
/*  Colorado State University                                */
/*                                                           */
/*  Permission is hereby granted to copy all or any part of  */
/*  this program for free distribution.   The author's name  */
/*  and this copyright notice must be included in any copy.  */
/*                                                           */
/*************************************************************/
typedef struct citydata
{  int mom_position;		/* mom & dad position and select list are */
   int dad_position;		/* not used by the position operator */
   int used;
   int select_list;
} CITY_DATA;

extern CITY_DATA * get_city_table (/* int length */);

extern void position( /* GENE_DATA mom[], pop[], kid[];
			int length; CITY_DATA *city_table */ );


extern void maketour( /* GENE_DATA gene[]; int num_points */ );
