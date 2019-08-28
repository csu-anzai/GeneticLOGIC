//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#if !defined ( __TOUR_H )
#define  __TOUR_H

#include "SIntVec.h"
#include <iostream.h>

#define  COSTDATA float

class	TourInfo 
{
public:
	TourInfo(char *sz);
	~TourInfo();
	double cost(const ShortVec&);
	void   canonical();
	int    cities() 
	{
		return dimension;
	}

	void	printLocations(ostream &, ShortVec&);
	double  edgeCost(int i, int j) const
        {
          	return (i > j ? costmatrix[i][j] : costmatrix[j][i]);
        } 

protected:

	COSTDATA** costmatrix;
	double** locations;
	int	 dimension;
};

#endif
