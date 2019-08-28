
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
/****************
 *  BINARY DATA *
 ****************/

#ifdef BIN
typedef
char GENE_DATA, *GENE_DATAPTR;


#define INIT(string, len) {int z; \
						 for (z=0; z<len; z++) \
							 /* Generates a '1' or a '0' */ \
						     string[z]=bitgenc(); \
					    }	

#define MUTATE(x)  ((x=='1') ? '0' : '1')

#define XDR_DATA  xdr_char

#define GENE_DATA_IN_FORMAT     "%c"
#define GENE_DATA_OUT_FORMAT(x) "%c",x

#else


/***********************
 * FLOATING POINT DATA *
 ***********************/

#ifdef FLOAT
typedef
float GENE_DATA, *GENE_DATAPTR;


#define INIT(string, len) {int z; \
					     for (z=0; z<len; z++) \
                              /* Generates number between -1 and +1 */ \
						      string[z]=(fracrand()-.5)*2; \
					    }	
#define MUTATE

#define XDR_DATA  xdr_float

#define GENE_DATA_IN_FORMAT     "%f"
#define GENE_DATA_OUT_FORMAT(x) "%f ",x

#else


/****************
 * INTEGER DATA *
 ****************/
#ifdef INT
typedef
int GENE_DATA, *GENE_DATAPTR;


#define INIT(string, len)  maketour(string, len);
#define MUTATE

#define XDR_DATA  xdr_int

#define GENE_DATA_IN_FORMAT     "%d"
#define GENE_DATA_OUT_FORMAT(x) "%d ",x

#endif
#endif
#endif



/****************************
 * independent of data type *
 ****************************/

typedef 
struct gene
	   {
        GENE_DATAPTR string;
        float worth;
       }GENE, *GENEPTR;

typedef
struct pool
	   {
		GENEPTR  data;
		int      size;
		int      string_length;
	   }POOL, *POOLPTR;
