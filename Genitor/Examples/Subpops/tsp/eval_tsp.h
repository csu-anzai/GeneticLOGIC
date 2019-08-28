extern float
tsp_eval (/* GENE_DATA tour[];
             int       num_points;
         */);

extern int **
get_coord_array(/* int num_coords;
                   int num_points;
               */);

extern void
free_coord_array(/* int *coords[];
                    int  num_points;
                */);

extern int **
get_2D_dist_array(/* int num_points */);

extern void
free_2D_dist_array(/* int *dists[];
                      int num_points;
                  */);
extern int
read_coords(/* FILE  *fp;
               int   *xy[];
               int   num_points;
           */);
extern void
calc_distances(/* int  *xy[];
                  int   num_points;
                  int  *distances[];
              */);
extern void
make_dist_array(/* char coord_file[];
                   int  num_points;
               */);
