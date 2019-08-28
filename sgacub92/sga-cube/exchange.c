#ifdef NCUBE
/*----------------------------------------------------------------------------*/
/* exchange.c - exchange exchngnum individuals between nodes every exchnggen  */
/*              generations                                                   */
/*----------------------------------------------------------------------------*/

#include "external.h"

exchange(pop)
struct individual *pop;
{
    unsigned int nbytes;
    struct   individual *replace;
    char     *malloc();
    void     nbrdcst();
    int      othernode;
    int      i, flag;
    int      msgexchng = 0xfd01;;

    /* malloc enough space to hold best replacements from another node */
    nbytes = exchngnum * sizeof(*pop);
    if((replace = (struct individual *) malloc(nbytes)) == NULL)
    {
        nlocal();
        fprintf(outfp,"exchange: malloc failed on node %d!\n",mynode);
        exit(-1);
    }

    /* have node 0 randomly pick a direction in n-dimensional space */
    /* all nodes exchange individuals with nearest neighbor in this */
    /* direction, so broadcast the same direction to all nodes...   */
    if(mynode == 0) exchngdir = rnd(0,dimcube-1);
    nbrdcst(&exchngdir,sizeof(exchngdir));
    othernode = neighbors[exchngdir];

    /* write out exchgnum best individuals to next node */
    for(i=0; i<exchngnum; i++)
    {
        if((nwrite(pop+i,sizeof(*pop),othernode,msgexchng,&flag)) != 0)
        {
            nlocal();
            fprintf(outfp,"nwrite, node %d->%d failed: ",mynode,othernode);
            nperror("");
            nglobal();
        }
    }

    /* read in replacement individuals for this node */
    for(i=0; i<exchngnum; i++)
    {
        if((nread(replace+i,sizeof(*pop),&othernode,&msgexchng,&flag)) < 0)
        {
            nlocal();
            fprintf(outfp,"nread, node %d<-%d failed: ",mynode,othernode);
            nperror("");
            nglobal();
        }
    }

    /* replace worst individuals on this node with best just read in */
    for(i=0; i<exchngnum; i++)
    {
        *(pop+popsize-i-1) = *(replace+i);
    }

    /* increment exchange counter */
    nexchange += exchngnum;

    /* free up the space for the replacements */
    free(replace);
}


nearest(thisnode,cubesize,nodes)
/* finds array of nearest neighbor nodes in n-dimensional hypercube */
/* enough memory must be allocated for nodes array, nodes[cubesize] */
int thisnode, cubesize;
int *nodes;
{
    int mask=1;
    int i;

    for (i=0; i<cubesize; i++)
    {
        nodes[i] = thisnode^mask;
        mask <<=1;
    }
}
#endif
