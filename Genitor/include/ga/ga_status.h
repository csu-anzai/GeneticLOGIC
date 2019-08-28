
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
extern float
avg_pool (/* POOLPTR pool */);

extern void
print_pool (/* FILE *fp, 
			   POOLPTR pool
			   int start_pt
			   int count
		   */);

extern void
show_progress(/* FILE * fp, 
				 POOLPTR pool
			 */);

extern void
show_progress_brief(/* FILE * fp, 
				 POOLPTR pool
			 */);

extern void
final_pool (/* char filename[],
			   POOLPTR pool 
		   */);

extern void
dump_status(/* POOLPTR pool,
               char dump_base[];
		   */);

extern void
fatal_error(/* char *mssg */);

extern void
warning(/* char mssg[] */);

extern void
pause_it();
