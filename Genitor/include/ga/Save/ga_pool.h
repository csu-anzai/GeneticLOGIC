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
