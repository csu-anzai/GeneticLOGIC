//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992.
//      Department of Computer Science
//      Chinese University of Hong Kong
//

// the edge recombination algorithm is adopted from Genitor :
//
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

#include "edge.h"
#include "tour.h"

extern TourInfo *pTourInfo;

EdgeMap::EdgeMap(int nNodes)
{
	// nodes[0] is not used
	edgeLists = (EdgeNodePtr *) new EdgeNodePtr[nNodes + 1];
	for (int i = 0; i <= nNodes; i++)
	     edgeLists[i] = new EdgeNode();
	size = nNodes;
}

EdgeMap::~EdgeMap()
{
	if (edgeLists != NULL) {
	    for (int i = 0; i <= size; i++)
		delete edgeLists[i];
	    delete [] edgeLists;
	}
}

void	EdgeMap::resize(int newSize)
{
	int i;
	int oldSize = size;

	if (oldSize != 0) {
	    for (int i = 0; i <= size; i++)
		delete edgeLists[i];
	    delete [] edgeLists;
	}
           
	edgeLists = (EdgeNodePtr *) new EdgeNodePtr[newSize + 1];
	for (i = 0; i <= newSize; i++)
	     edgeLists[i] = new EdgeNode();

	size = newSize;
}

float	EdgeMap::buildEdgeTable(const ShortVec& mother, const ShortVec& father)
//
// assumes circular tours and bidirectional edges
//
{
	// failures = 0;

	if (mother.capacity() != father.capacity()) {
	    lib_error_handler("EdgeMap", "unequal parent lengths");
	    exit(-1);
	}

	if (mother.capacity() > size) {
	    lib_error_handler("EdgeMap", "parent length too long");
	    exit(-1);
	}

	int i, j,  edgeTotal = 0;

	for (i = 1; i <= mother.capacity(); i++)
	     edgeLists[i]->total = edgeLists[i]->unused = 0;

	for (i = 0, j = mother.capacity() - 1; i < j; i++) {
		edgeTotal += addEdge(mother[i], mother[i+1]);
		             addEdge(mother[i+1], mother[i]);
		edgeTotal += addEdge(father[i], father[i+1]);
		             addEdge(father[i+1], father[i]);
	}
	// maps last city to first city
	edgeTotal += addEdge(mother[0], mother[j]);
	             addEdge(mother[j], mother[0]);
	edgeTotal += addEdge(father[0], father[j]);
	             addEdge(father[j], father[0]);

	return (edgeTotal * 2) / (float) mother.capacity();
}

int	EdgeMap::addEdge(int node1, int node2)
{
	int i;
	int edgesSoFar;

	edgesSoFar = edgeLists[node1]->total;
	for (i=0; i<edgesSoFar; i++) {
	    if (abs(edgeLists[node1]->edgeList[i] == node2)) {
		// mark shared edges as negative
		edgeLists[node1]->edgeList[i] = 0 - node2;
		return 0;
	    }
	}

	// add node1 -> node2

	edgeLists[node1]->edgeList[edgesSoFar] = node2;
	edgeLists[node1]->total++;
	edgeLists[node1]->unused++;

	return 1;
}

int	EdgeMap::buildTour(ShortVec& child)
{
	int i, j;

	child.resize(size);
	// randomly choose a start node
	child[0] = tRand(1, child.capacity());
	for (i=1; i < child.capacity(); i++) {
	     j = i - 1;
	     removePoint(child[j], *edgeLists[child[j]]);
	     //
             // find a destination for the newly entered point 
	     //  
	     if (edgeLists[child[j]]->unused > 0) 
		child[i] = selectPoint(child[j], *edgeLists[child[j]]);
	     else 
	        child[i] = handleFailure(child, j);
		
	     // mark this node as consumed
	     edgeLists[child[j]]->unused = -1;
	}
	return 1;
}

void	EdgeMap::removePoint(int point, REdgeNode iNode)
{
	int i, j;
	int hasEdge;
	int ptsRemaining;

	//
	// for every node known to have an edge to the input node
	//
	for (i=0; i < iNode.unused; i++) {
		hasEdge = abs(iNode.edgeList[i]);
		ptsRemaining = edgeLists[hasEdge]->unused;

		// find the input node in all edge lists and delete it
		for (j=0; j < ptsRemaining; j++) {
		    if (abs(edgeLists[hasEdge]->edgeList[j]) == point) {
			edgeLists[hasEdge]->unused--;
			edgeLists[hasEdge]->edgeList[j] = 
			     edgeLists[hasEdge]->edgeList[ptsRemaining-1];
			break;
		    }
		}
	}
}

int     EdgeMap::selectPoint(int point, REdgeNode iNode)
{
//
//      the parameter point is not used in this version of selectPoint
//
	int minEdges = 5;
	int candidatePt;
	int i, j;
	int minCnt;

	for (i=0; i < iNode.unused; i++) {
	     candidatePt = iNode.edgeList[i];
             //
	     // share edges are negative and receive priority
	     //
	     if (candidatePt < 0)
		return abs(candidatePt);

	     // 
	     // give priority to candidates with fewest remaining unused edges
             //

	     if (edgeLists[candidatePt]->unused < minEdges) {
		minEdges = edgeLists[candidatePt]->unused;
                minCnt = 1;
	     }
             else 
		if (edgeLists[candidatePt]->unused == minEdges)
		    minCnt++;
	}
	
	j = tRand(0, minCnt - 1);

	for (i = 0; i < iNode.unused; i++) {
	     candidatePt = iNode.edgeList[i];
	     //
	     // return the chosen candidate point
	     //
	     if (edgeLists[candidatePt]->unused == minEdges) {
		minCnt--;
                if (j == minCnt)
                   return candidatePt;
	     }
	}

	lib_error_handler("EdgeMap", "unexplained failure");
	exit(-1);
}

ostream & operator << (ostream & out, RCEdgeMap map)
{
	int i; 
	for (i=1; i <= map.dimension(); i++) {
		out << i << ':';
		((REdgeMap) map).edgeLists[i]->printOn(out);
	}
	return out;
}

void	EdgeNode::printOn(ostream & out) const
{
	int i, j, k;
        if (total > 0) {
	    out << edgeList[0];
            for (k = 1; k < total; k++)
                out << ',' << edgeList[k];
        }
	else
	    out << "NULL";
	out << " - " << unused << endl;
}

int     EdgeMap::handleFailure(ShortVec& child, int lastPoint)
{
	int	i, j;
	int	stranded;
	int	edgesRemain = 0;
	int	totalFour = 0;
	int	randPick;
        
	stranded = child[lastPoint];

	//
	// find out how many edges remain.  In particular keep track of
	// how many points with four total (initial) edges remain
	//

	for (i=1; i <= size; i++) {
	    if (edgeLists[i]->unused != -1 && i != stranded) {
		edgesRemain++;
		if (edgeLists[i]->total == 4)
		    totalFour++;
	    }
	}

	// randomly choose one of the points with edges remaining
	
	if (totalFour) {
	    randPick = tRand(0, totalFour - 1);
	    for (i=1; i <= size; i++) {
		if (i != stranded &&
		    edgeLists[i]->unused != -1 &&
		    edgeLists[i]->total == 4) {
		    totalFour--;
		    if (randPick == totalFour)
                       return i;
		}
	    }
	    lib_error_handler("EdgeMap", "(1) Cannot find an edge");
	}
	else if (edgesRemain) {
	//
	// randomly choose one of the points with edges remaining
	//
	     randPick = tRand(0, edgesRemain - 1);
	     for (i=1; i <= size; i++) {
		if (i != stranded &&
		    edgeLists[i]->unused != -1 ) {
		    edgesRemain--;
		    if (randPick == edgesRemain)
                        return i;
                }
	     }
	     lib_error_handler("EdgeMap", "(2) Cannot find an edge");
        }
	// the edge table appears empty
	else {
	     for (i=1; i <= size; i++) 
		if (edgeLists[i]->unused >= 0)
		   return i;
	     lib_error_handler("EdgeMap", "(3) Cannot find an edge");
	}
	
	lib_error_handler("EdgeMap", "handle_failure : unexplained failure");
}
