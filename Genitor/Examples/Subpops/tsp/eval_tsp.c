#include <stdio.h>
#include <math.h>
#include "gene.h"
#include "op_edge_recomb.h"

double **DistArray;
int **CoordArray;

float
tsp_eval (tour, num_points)
GENE_DATA tour[];
int       num_points;
{
 int    i;
 float sum;

 sum = 0.0;

 for (i=0; i<num_points; i++)
     sum = sum + (float) DistArray[tour[i]][tour[((i+1)%num_points)]]; 

 return (sum); 
}


#define ABS(x)       (x<0 ? 0-x : x)

/***************************************************************************
 * FUNCTION: get_coord_array
 *
 * DESCRIBE: Allocates space for integer coordinate array.
 *
 * INPUT PARAMETERS: number of coordinates per point
 *                   number of points
 *
 * RETURN VALUE: pointer to coordinate array
 ****************************************************************************/
 int **
 get_coord_array(num_coords, num_points)
 int             num_coords;
 int             num_points;
 {
  int **coords;
  int i;

  /******************************************************
   malloc one extra point location so that nodes numbered
   1 - N can be indexed directly; 0 will not be used
   ******************************************************/

  if (!(coords = (int **) malloc ((num_points+1)*sizeof(int *))))
     fatal_error ("get_coord_array: Malloc failure (1).\n");

  for (i=0; i<=num_points; i++)
       if (!(coords[i] = (int *) malloc (num_coords*sizeof(int))))
          fatal_error ("get_coord_array: Malloc failure (2).\n"); 
  
  return (coords);
 }

/***************************************************************************
 * FUNCTION: free_coord_array
 *
 * DESCRIBE: deallocates space for integer coordinate array
 *
 *           NOTE: use this function only when the integer coordinate
 *                 array was Allocated using get_coord_array
 *
 * INPUT PARAMETERS: coordinate array to free,
 *                   number of points
 *
 * RETURN VALUE: none 
 ****************************************************************************/
 void
 free_coord_array(coords, num_points)
 int            *coords[];
 int             num_points;
 {
  int i;

  for (i=0; i<=num_points; i++)
      free (coords[i]);

  free (coords);
 }

/***************************************************************************
 * FUNCTION: get_2D_dist_array
 *
 * DESCRIBE: Allocates space for 2D double-float distance array
 *
 * INPUT PARAMETERS: number of points
 *
 * RETURN VALUE: pointer to distance array
 ****************************************************************************/
 double **
 get_2D_dist_array(num_points)
 {
  double **dists;
  int i;

  /*************************************************
   malloc one extra location so that nodes numbered
   1 - N can be indexed directly; 0 will not be used
   *************************************************/

  if (!(dists = (double **) malloc ((num_points+1)*sizeof(double *))))
     fatal_error ("get_2D_dist_array: Malloc failure (1).\n");


  for (i=0; i<=num_points; i++)
       if (!(dists[i] = (double *) malloc ((num_points+1)*sizeof(double))))
          fatal_error ("get_2D_dist_array: Malloc failure (2).\n"); 
  
  return (dists);
 }

/***************************************************************************
 * FUNCTION: free_2D_dist_array
 *
 * DESCRIBE: deallocates space for integer distance
 *
 *           NOTE: use this function only when the integer distance
 *                 array was Allocated using get_2D_dist_array
 *
 * INPUT PARAMETERS: distance array to free,
 *                   number of points
 *
 * RETURN VALUE: none 
 ****************************************************************************/
 void
 free_2D_dist_array(dists, num_points)
 int            *dists[];
 int             num_points;
 {
  int i;

  for (i=0; i<=num_points; i++)
      free (dists[i]);

  free (dists);
 }

/***************************************************************************
 * FUNCTION: read_coords
 *
 * DESCRIBE: Read x y coordinates (describing points in the tour) from input
 *           stream. Coordinate values must be separated by white space. 
 *           The coordinates will be saved in the input xy array, with x
 *           in the 0th element and y in the 1rst.
 *
 *           Points will be given an identification number which corresponds
 *           to their file position.  Thus, the first x y coordinates
 *           describe point 1, the second point 2, and so on.
 *
 *           NOTE: This function assumes that points are described by 
 *                 integer coordinates in a 2D area.  
 *
 * INPUT PARAMETERS: input stream
 *                   xy array pointer
 *                   number of points
 *
 * RETURN VALUE: 1 if successful, 0 upon failure
 ****************************************************************************/
 int
 read_coords(fp, xy, num_points)
 FILE      *fp;
 int       *xy[];
 int        num_points;
 {
  int i;

  for (i=1; i<=num_points; i++)
      if (fscanf(fp, "%d %d", &xy[i][0], &xy[i][1]) != 2)
         {
          warning ("read_coords: Error reading input coordinate file.\n");
          return (0);
         }

  return (1);
 }

/***************************************************************************
 * FUNCTION: calc_distances
 *
 * DESCRIBE: Uses the coordinates saved in the input xy array (x
 *           in the 0th element and y in the 1rst) to derive the
 *           distances between the points represented by the coordinates.
 *           The calculated distances are placed in the input distance 
 *           array.
 *
 *           NOTE: This function assumes that points are described by 
 *                 integer coordinates in a 2D area.  
 *
 *           NOTE: distances are rounded up to integers (13.3 = 14)
 *
 * INPUT PARAMETERS: xy array pointer
 *                   number of points
 *                   distance array pointer
 *
 * RETURN VALUE: none
 ****************************************************************************/
 void
 calc_distances(xy, num_points, distances)
 int       *xy[];
 int        num_points;
 double    *distances[];
 {
  int i,j;
  int point1, point2;

#define DIFFSQ(x,y)  ((x-y)*(x-y))

  for (i=1; i<=num_points; i++)
      for (j=1; j<i; j++)
          {
           distances[i][j] = distances[j][i] = 
              ( (sqrt ( (DIFFSQ((double)(xy[i][0]),(double)(xy[j][0])))	
             + (DIFFSQ((double)(xy[i][1]), (double)(xy[j][1]))))));

          }
 }


/***************************************************************************
 * FUNCTION: make_dist_array
 *
 * DESCRIBE: makes a 2 dimensional array of distances between points
 *
 * INPUT PARAMETERS: filename of list of coordinates of points
 *                   number of points
 *                   
 *
 * RETURN VALUE: none
 *
 * CALLS: get_coord_array
 *        read_coords
 *        get_2D_dist_array
 *        calc_distances
 ****************************************************************************/
void
make_dist_array(coord_file, num_points)
char            coord_file[];
int             num_points;
{
 FILE *fp;

 if (fp = fopen(coord_file, "r"))
    {
     CoordArray = get_coord_array(2, num_points);
     read_coords (fp, CoordArray, num_points);
     DistArray = get_2D_dist_array (num_points);
     calc_distances(CoordArray, num_points, DistArray);
     fclose (fp);
    }
 else
    fatal_error ("Cannot read input city file");
}
