

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
extern void    free_pool 
			   (/* GENEPTR pool */);
			   
extern GENEPTR get_gene
			   (/* int string_length */);

extern POOLPTR get_pool 
			   (/* int     pool_size,
				   int     string_length
			   */);

extern int     init_pool
			   (/* char   *seed_file,
				   GENEPTR pool,
                   int     strt, stop,
				   float   (*eval_fun)()
				*/);

extern void    insert_gene 
			   (/* GENE    newgene, 
			       GENEPTR pool, 
		       */);

extern void    insert_unique_gene 
			   (/* GENE    newgene, 
			       GENEPTR pool, 
			       int     length,
			       int     sequence_flag
		       */);

extern int     random_init_pool 
			   (/* GENEPTR pool, 
				   int     strt, stop,
				   float   (*eval_fun)() 
                */);

extern int     seed_pool 
			   (/* FILE    *fp,
				   GENEPTR pool, 
				   int     strt, stop, 
				   float   (*eval_fun)()
			   */);

extern void    sort_pool 
			   (/* GENEPTR pool */);
