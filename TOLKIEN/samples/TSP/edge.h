//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//
//  the edge recombination algorithm is adopted from Genitor :
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
//
#if !defined ( __EDGE_H )
#define  __EDGE_H

#include "toarray.h"
#include "SIntVec.h"

_CLASSDEF(EdgeMap) 
_CLASSDEF(EdgeNode) 

struct EdgeNode
{
public:
	EdgeNode() : total(0), unused(0) {}
	~EdgeNode() {}

	void printOn(ostream&) const;
	short	edgeList[4];
	short	total;
	short	unused;
};

typedef	EdgeNode *EdgeNodePtr;

class EdgeMap 
{
public :
        EdgeMap();
	EdgeMap(int);
	~EdgeMap();

	float	buildEdgeTable(const ShortVec&, const ShortVec&);
	int	addEdge(int,int);
        void    removePoint(int point, REdgeNode iNode);
        int 	selectPoint(int point, REdgeNode iNode);
	int	buildTour(ShortVec&);
	int	handleFailure(ShortVec&, int);
	void	resize(int);
	int	dimension() const { return size; }
	friend ostream & operator << (ostream &, RCEdgeMap);

protected:

	EdgeNodePtr	*edgeLists;
        int             size;
	
};
	
inline EdgeMap::EdgeMap() : size(0), edgeLists(NULL)
{
}

extern EdgeMap edgeMap;

#endif

