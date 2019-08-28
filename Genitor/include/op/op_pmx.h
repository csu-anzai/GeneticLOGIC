

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
{  int failed;
   int from;
   int fail_indx;
   int check_list;
} CITY_DATA;


extern int * get_array ( /* int length */);

extern void maketour(/* GENE_DATA gene[], num_points */);

extern void pmx ( /* GENE_DATA mom[], dad[], kid[]; int length;
		int *failed, *from, *indx, *checklist; */);

