
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
int
xdr_GENE(/* XDR     *xdrs;
            GENEPTR  gene;
         */);
int
process_xdr(/* XDR         *xdrs;
               GENEPTR      pool;
               unsigned int pool_size,
			                string_len;
            */);
int
print_xdr(/* FILE        *fp; 
             GENEPTR      pool;
          */);
int
read_xdr(/* FILE        *fp;
            GENEPTR      pool;
         */);
