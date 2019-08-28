//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      class TourInfo is used to store the distances of the cities
//      the input file is divided into three parts :
//
//      the first line stores the total number of cities
//      the second line stores the coordinate system used :
//              { EUD, ATT, GEO }
//      the last part of the file stores the coordinates of the cities,
//      one line for one city, the expected format is :
//
//      CITY_NUMBER XCORD YCORD
//
//      the TSPLIB found in INTERNET contains many test problems
//
#include <stdio.h>
#include <stdlib.h>
#include <fstream.h>
#include <string.h>
#include <math.h>
#include "tour.h"

#ifndef PI
#define	PI	((float) 3.141592)
#endif

float	aint(float val)
{
	return floor(val);
}

float	eud2d(float x1, float y1, float x2, float y2)
{
	double xd = x1 - x2;
	double yd = y1 - y2;
	return aint( sqrt( xd * xd + yd * yd) + 0.5 );
}


float	att2d(float x1, float y1, float x2, float y2)
{
	float x, y, r, t;

	x = x1 - x2;
	y = y1 - y2;
	r = sqrt( (x * x + y * y) / 10.0 );
	t = aint(r);
	if (t < r)
           return ( t + 1);
	else
           return ( t );
}

float	geo2d(float x1, float y1, float x2, float y2) 
{
	float lat1, lat2, long1, long2, deg, min;
	float q1, q2, q3;

	deg = aint(x1);
	min = x1 - deg;
	lat1 = (PI / 180.0)  * (deg + (5.0 * min) / 3.0);

	deg = aint(y1);
	min = y1 - deg;
	long1 = (PI / 180.0)  * (deg + (5.0 * min) / 3.0);

	deg = aint(x2);
	min = x2 - deg;
	lat2 = (PI / 180.0)  * (deg + (5.0 * min) / 3.0);

	deg = aint(y2);
	min = y2 - deg;
	long2 = (PI / 180.0)  * (deg + (5.0 * min) / 3.0);
	
	q1 = cos(long1 - long2);
	q2 = cos(lat1 - lat2);
	q3 = cos(lat1 + lat2);

	return(floor(6378.388 * acos( 0.5 * ( ( 1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0));
}

TourInfo::TourInfo(char *szFile)
{
	ifstream costfile(szFile);
	char weighttype[80];
	int	i, j;
	float   x, y, tmp;

	if (!costfile) {
	   cout << "cost file read error" << endl;
	   exit(-1);
	}

	costfile >> dimension;
	costfile >> weighttype;
	printf("parameter dimension = %d, type = %s\n", dimension, weighttype);
	
        costmatrix = (COSTDATA **) calloc(dimension + 1, sizeof(COSTDATA *));
 
        /* costmatrix[0] and costmatrix[1] is not used */
        for (i = 2; i <= dimension; i++)
             if ((costmatrix[i] = (COSTDATA *) calloc(i, sizeof(COSTDATA))) == NULL) {
                printf("cannot allocate cost matrix\n");
                exit(-1);
             }   

	locations = (double **) calloc(dimension + 1, sizeof(double *));
	for (i = 1; i <= dimension; i++) 
	     if ((locations[i]  = (double *) calloc(2, sizeof(double))) == NULL) {
		printf("cannot allocate locations\n");
		exit(-1);
	}

	for (i = 1; i <= dimension; i++) {
		if (! costfile) {
		    printf("costfile unexpected EOF\n");
		    exit(-1);
		}
		costfile >> tmp >> x >> y;
		locations[i][0] = x;
		locations[i][1] = y;
	}



      if (strcmp("EUC_2D", weighttype) == 0) {
        for (i = 2; i <= dimension; i++)
             for (j=1; j < i; j++)
                  costmatrix[i][j] = eud2d(locations[i][0],
                                           locations[i][1],
                                           locations[j][0],
                                           locations[j][1]);
       }
        else if (strcmp("ATT", weighttype) == 0) {
        for (i = 2; i <= dimension; i++)
             for (j=1; j < i; j++)
                  costmatrix[i][j] = att2d(locations[i][0],
                                           locations[i][1],
                                           locations[j][0],
                                           locations[j][1]);
        }
        else if (strcmp("GEO", weighttype) == 0) {
        for (i = 2; i <= dimension; i++)
             for (j=1; j < i; j++)
                  costmatrix[i][j] = geo2d(locations[i][0],
                                           locations[i][1],
                                           locations[j][0],
                                           locations[j][1]);
 
        }
}

void	TourInfo::printLocations(ostream & out, ShortVec& cities) 
{
	for (int i = 0; i < cities.capacity(); i++) {
	     out << locations[(int) cities[i]][0] << ' ';
	     out << locations[(int) cities[i]][1] << endl;
	}
	out << locations[(int) cities[0]][0] << ' ';
	out << locations[(int) cities[0]][1] << endl;
}

TourInfo::~TourInfo()
{
        for (int i = 2; i <= dimension; i++) 
             free((char *) costmatrix[i]);
        
        free((char *) costmatrix);

        for (i = 1; i <= dimension; i++) 
             free((char *) locations[i]);
        
        free((char *) locations);

}

double	TourInfo::cost(const ShortVec& tour)
{
    double summ = 0; 
    int i, j, m, n;

    for (i = 0, j = dimension - 1; i < j; i++) {
	m = tour[i];
	n = tour[i+1];
        summ += (n > m ? costmatrix[n][m] : costmatrix[m][n]);
    }

    summ += (tour[i] > tour[0] ? costmatrix[tour[i]][tour[0]] : costmatrix[tour[0]][tour[i]]);

    return summ;
}

void	TourInfo::canonical()
{ 
	ShortVec ints(dimension);

	for (int i=0; i < ints.capacity(); i++)
	    ints[i] = i+1;

	cout << "cost of canonical tour = " << cost(ints) << endl;
}

