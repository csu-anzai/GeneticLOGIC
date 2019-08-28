#include <stdio.h>
#include <malloc.h>
#include "gene.h"
#include "ga_status.h"
#include "ga_random.h"
#include "ga_pool.h"
#include "ga_copy.h"

/****************************************************************************
 * FUNCTION: free_pool
 *
 * DESCRIBE: Deallocates memory for the data structures of a genetic pool
 *           which was created by get_pool().
 *           NOTE: if start_pt != 0 (i.e. we will not be freeing the
 *                 whole pool, just reducing the pool_size) then the
 *                 pool ptr will not be freed
 *                 
 *
 * INPUT PARAMETERS: pointer to a pool of GENE structures
 *                   the location at which to begin freeing genes                   
 *
 * RETURN VALUE: none
 ****************************************************************************/

void
free_pool (pool, start_pt)
POOLPTR    pool;
int        start_pt;
{
 int i;  /* loop counter */

 for (i=start_pt; i<pool->size; i++)
	 {
	 if (pool->data[i].string)
		 free (pool->data[i].string);
     if (pool->data[i].tags)
		 free (pool->data[i].tags);
     }

 /************************************
  The pool structure should only be
  freed when all genes have been freed
  ************************************/
 if (!start_pt)
	{
    free (pool->data);
	free (pool);
	}

 /******************
  Update pool's size 
  ******************/
 else
	{
	pool->size = start_pt;
	}
}

/***************************************************************************
 * FUNCTION: get_gene
 *
 * DESCRIBE: allocates a gene and string space
 *
 * INPUT PARAMETERS: length of string
 *
 * RETURN VALUE: geneptr
 ****************************************************************************/
GENEPTR
get_gene (string_length)
int       string_length;
{
 GENEPTR gene;

 if (!(gene = (GENEPTR) malloc (sizeof(GENE)))
	 ||
     !(gene->string = 
	  (GENE_DATAPTR) malloc ((string_length+1)*sizeof(GENE_DATA)))
     ||
     !(gene->tags = 
	  (int *) malloc ((string_length+1)*sizeof(int)))
    )
	fatal_error ("get_gene(): malloc failure\n");

 
 return (gene);
}


/***************************************************************************
 * FUNCTION: get_pool_data
 *
 * DESCRIBE: Allocates memory for the gene data structures of a genetic pool
 *
 * INPUT PARAMETERS: pool_size, length of string
 *
 * RETURN VALUE: GENEPTR
 ****************************************************************************/
GENEPTR
get_pool_data (pool_size, string_length)
int            pool_size;
int            string_length;
{
 int i;
 GENEPTR pool_data;

 
 if ( !( pool_data = (GENEPTR) malloc (pool_size * sizeof(GENE))) )
	 goto POOLDATA_ALLOC_ERROR;              

 for (i=0; i<pool_size; i++)
	 {
	 if ( !(pool_data[i].string = 
			(GENE_DATAPTR) malloc ((string_length+1)*sizeof(GENE_DATA))) 
          ||
		  !(pool_data[i].tags = 
			(int *) malloc ((string_length+1)*sizeof(int))) 
        ) 
	     goto POOLDATA_ALLOC_ERROR;              
     }

 return (pool_data);

 POOLDATA_ALLOC_ERROR:
  fatal_error("get_pool_data(): Malloc failure.\n");
}


/****************************************************************************
 * FUNCTION: get_pool
 *
 * DESCRIBE: Allocates memory for the pool data structure and 
 *           the data structures of its genetic pool
 *
 * INPUT PARAMETERS: number of genes in pool;
 *                   length of a gene;
 *
 * RETURN VALUE: NULL if sufficient memory was not available,
 *               a pointer to a new pool if was available.
 ****************************************************************************/

POOLPTR
get_pool (pool_size, string_length)
int        pool_size;
int        string_length;
{
 POOLPTR new_pool;
 int     i;

 if ( !(new_pool = (POOLPTR) malloc (sizeof(POOL))) )
	 goto POOL_ALLOC_ERROR;

 new_pool->size = pool_size;
 new_pool->string_length = string_length;
 
 if ( !(new_pool->data = get_pool_data (pool_size, string_length)) )
	 goto POOL_ALLOC_ERROR;              

 return (new_pool);

 POOL_ALLOC_ERROR:
  fatal_error ("get_pool(): Malloc failure.\n");
}


/***************************************************************************
 * FUNCTION: init_pool
 *
 * DESCRIBE: gives initial values to genetic pool
 *
 * INPUT PARAMETERS: seed file name; 
 *                   ptr to pool of genes;
 *                   start and stop position in gene pool;
 *                   evaluation function
 *
 * RETURN VALUE: number of genes initialized
 *
 * RESULT: changes values of genes strings & worths in input pool
 *
 * CALLS: random_init_pool
 *        seed_pool
 ****************************************************************************/
int
init_pool (seed_file, pool, strt, stp, eval)
char      *seed_file;
POOLPTR    pool;
int        strt, stp;
float      (*eval)();
{
 int num_init;

 if (strlen (seed_file) == 0)
    num_init =  random_init_pool (pool, strt, stp, eval);

 else
	{
	 FILE *fp;
	 char mssg[80];

	 if (!(fp = fopen (seed_file, "r")))
		{ sprintf (mssg, 
				   "Cannot open pool initialization file '%s'\n", 
				   seed_file);
		  fatal_error (mssg);
        }
		

     num_init = seed_pool (fp, pool, strt, stp, eval);
   
     if (num_init < 0)
		fatal_error ("init_pool: bad initialization indices");

     /*********************************************
	  If file did not contain enough initialization
	  values, finish off with random initialization
	  *********************************************/
     if (num_init != stp-strt)
		{
		char mssg[80];
		int  rand_num_init;

		sprintf (mssg, 
	"\n%d genes read from file '%s';\n%d genes will be randomly generated.\n",
				num_init, seed_file, stp-num_init);
		warning(mssg);

        rand_num_init = random_init_pool (pool, num_init-1, stp, eval);
		num_init = num_init + rand_num_init;
		}

     fclose (fp);
	}
 return (num_init);
}


/****************************************************************************
 * FUNCTION: insert_gene
 *
 * DESCRIBE: inserts a new gene into pool, displacing worst gene in pool
 *
 *           NOTE: assumes best->worst = smallest->largest
 *                 (see sort_pool() for further details)
 *
 * INPUT PARAMETERS: gene structure;
 *                   pool of genes;
 *
 * RETURN VALUE: none
 ****************************************************************************/

void
insert_gene (newgene, pool)
GENEPTR      newgene;
POOLPTR      pool;
{
 int top, mid, bot;
 int rank;

 /* new gene is so bad we can't use it */
 if (newgene->worth > pool->data[pool->size-1].worth)
	return;


 /* do a binary search to find the rank of the new gene */

 top = 0;
 mid = pool->size/2;
 bot = pool->size-1;
 rank = -1;      

 while (rank == -1)
	{
	 /* these 4 cases find a new location */

     if (newgene->worth + newgene->repro_worth <= 
		 pool->data[top].worth + pool->data[top].repro_worth)
	    rank = top;
     else
     if (newgene->worth + newgene->repro_worth == 
		 pool->data[mid].worth + pool->data[mid].repro_worth)
    	rank = mid;
     else
     if (newgene->worth + newgene->repro_worth ==
	     pool->data[bot].worth + pool->data[bot].repro_worth)
	    rank = bot;
     else
     if (bot-top <=1)
	    rank = bot;


     /* These 2 cases move the search indices since
		a new location has not yet been found. */

     else if (newgene->worth + newgene->repro_worth < 
			  pool->data[mid].worth + pool->data[mid].repro_worth)
	    {
   	     bot = mid;
	     mid = top+((bot-top)/2);
        }
     else /* (newgene->worth + newgene->repro_worth > 
			  pool->data[mid].worth+ pool->data[mid].repro_worth) */
	    {
	     top = mid;
	     mid = top+((bot-top)/2);
	    }
	}

 /*
  * Move every gene from rank on down
  * one position to make room for newgene.
  */

 {
  GENE travel_down, temp;
  int  i;

  /* copy new gene into pool storage;
   * always replace worst gene in pool 
   */
  gene_copy (&pool->data[pool->size-1], newgene, pool->string_length);

  gene_copy_ptr (&travel_down, &pool->data[pool->size-1]);

  for (i=rank; i<pool->size; i++)
	 {
      gene_copy_ptr (&temp, &pool->data[i]);
      gene_copy_ptr (&pool->data[i], &travel_down);
	  gene_copy_ptr (&travel_down, &temp);
	 }
 }

}

/****************************************************************************
 * FUNCTION: random_init_pool
 *
 * DESCRIBE: Randomly initializes the string data structures of a genetic pool;
 *           registers the worth of each randomly created string.
 *
 * INPUT PARAMETERS: pointer to genetic pool;
 *                   start & stop position in pool;
 *                   pointer to evaluation function;  
 *
 * RETURN VALUE: number of genes initialized
 ****************************************************************************/
int
random_init_pool (pool, strt, stp, eval_fun)
POOLPTR    pool;
int        strt, stp;
float      (*eval_fun)();
{
 int i; /* loop counter */

 for (i=strt; i<stp; i++)
	 {
	  INIT(pool->data[i].string, pool->string_length);
      pool->data[i].worth = 
	   (*eval_fun)(pool->data[i].string, pool->string_length);

      rand_sequence (pool->data[i].tags, pool->string_length);
	  /**********************************************************
	   * initially, reproductive worth is equal to string worth *
	   **********************************************************/
      pool->data[i].repro_worth = pool->data[i].worth;
	  pool->data[i].repro_count = 1;
     }
 return (stp-strt);
}

/***************************************************************************
 * FUNCTION: seed_pool
 *
 * DESCRIBE: Sets ga pool structures with intial values and worths.
 *           File format as follows:  string worth
 *                                       :     :
 *
 * INPUT PARAMETERS: (FILE *) ptr to read from, 
 *                   (GENEPTR) pool to write to,
 *                   (int) place in pool to begin writing;
 *                         place in pool to quit writing
 *                   ptr to eval function (if null, use seeded worth)
 *
 * RETURN VALUE: number of genes initialized
 *
 * CALLS: copy_string
 ****************************************************************************/
int
seed_pool (fp, pool, istart, istop, eval_fun)
FILE     *fp;
POOLPTR   pool;
int       istart, istop;
float      (*eval_fun)();
{
 int  i,j;
 float val;
 GENE_DATA temp;

 for (i=istart; i<istop; i++)
	 {
      for (j=0; j<pool->string_length; j++)
		  {
		  /*
		  if (fscanf(fp, GENE_DATA_IN_FORMAT, &temp) != 1)
	         return (i);
          pool->data[i].string[j] = GENE_DATA_IN_TRANS(temp);
		  */
		  if (fscanf(fp, GENE_DATA_IN_FORMAT, &pool->data[i].string[j]) != 1)
	         return (i);
          }

      if (fscanf(fp, "%f", &val) != 1)
		 return(i);

	  if (eval_fun)
          pool->data[i].worth = 
		   (*eval_fun)(pool->data[i].string, pool->string_length);
      else	
	      pool->data[i].worth = val;
	 }
 return (istop-istart);
}

/****************************************************************************
 * FUNCTION: sort_pool
 *
 * DESCRIBE: Sorts input pool according to worth, from smallest to largest
 *           
 *           NOTE: To affect the opposite sort (from largest to smallest)
 *                 the recommended method is to make the evaluation
 *                 function return a negative value.
 *
 *                 ( If you change sort_pool() itself (NOT recommended)
 *                   don't forget to change insert_gene() )
 *                 
 * INPUT PARAMETERS: number of genes in pool;
 *                   length of a gene;
 *
 * RETURN VALUE: none
 ****************************************************************************/

void
sort_pool(pool)
POOLPTR   pool;
{ 
 int  ndx, next, min;
 GENE temp;
	    
 for (ndx=0; ndx<pool->size-1; ndx++)
    { 
     min = ndx;

	 for (next=(ndx+1); next<pool->size; next++)
	     if (pool->data[next].worth + pool->data[next].repro_worth 
			 < 
			 pool->data[min].worth + pool->data[min].repro_worth) 
			 min = next;
	       
     /* swap */
     if (min != ndx)
        {
		 gene_copy_ptr (&temp, &pool->data[ndx]); 
		 gene_copy_ptr (&pool->data[ndx], &pool->data[min]);
		 gene_copy_ptr (&pool->data[min], &temp);
        }
    }
}
