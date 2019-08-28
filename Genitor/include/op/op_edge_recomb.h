
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
typedef struct edg_nde
{ GENE_DATA edge_list[4];
  int       total_edges;
  int       unused_edges;
} EDGE_NODE;


extern 
 EDGE_NODE *
 get_edge_table( /* int            num_points;
				 */
               );
extern
 void
 free_edge_table( /* EDGE_NODE       edge_table[];
				  */
                );
extern
 void
 maketour( /* GENE_DATA gene[];
              int      num_points;
           */
         );
extern
 float
 build_edge_table ( /* GENE_DATA         gene1[],
                                         gene2[];
                       int               num_points;
                       EDGE_NODE         edge_table[];
                    */
				  );
extern
 int
 add_edge ( /* GENE_DATA   point1,
		                   point2;
               EDGE_NODE   edge_table[];
            */
          );
extern
 int
 build_tour ( /* EDGE_NODE   edge_table[];
                 GENE_DATA   new_gene[];
                 int         num_points;
              */
            );
extern
 int
 select_point ( /* EDGE_NODE     edge_node;
                   EDGE_NODE     edge_table[];
                */
              );
extern
 void
 remove_point ( /* GENE_DATA     point;
                   EDGE_NODE     edge_node;
                   EDGE_NODE     edge_table[];
                */
              );
extern
 GENE_DATA
 handle_failure ( /* GENE_DATA   stranded;
					 EDGE_NODE   edge_table[];
                  */
                );
extern
 int
 number_failures (/**/);
