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
