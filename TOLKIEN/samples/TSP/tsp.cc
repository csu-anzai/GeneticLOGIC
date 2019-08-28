//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
#include <iostream.h>
#include "tsp.h"
#include "tour.h"
#include "tdefs.h"
#include "SmplStat.h"

EdgeMap	edgeMap;
extern	TourInfo	*pTourInfo;

long	TSPind::crossTotal = 0;
double	TSPind::fraction = 0;

long	TSPind::orOptTotal = 0;
long	TSPind::twoOptTotal = 0;

BOOL	TSPind::printFlag = FALSE;

static  PCTypeInfo tspBases[] = { &TGAObj::infoObj, 0 };
const TypeInfo  TSPind::infoObj("TSPind", tspBases);

short*	mybsearch(ShortVec& ints, int val)
{
	int lower, upper, mid;

	if (ints.capacity() == 0)
	    return 0;

	lower = 0;
	upper = ints.capacity() - 1;

	if ((val < ints[0]) || (val > ints[upper]))
	    return 0;

	while (lower <= upper) {
	        mid = (upper + lower) / 2.0;
		if (val == ints[mid])
		    return & ints[mid];
		else
		    if (val < ints[mid])
                        upper = mid - 1;
		    else
			lower = mid + 1;
	}

	return 0;
}

TSPind::TSPind(unsigned cities, BOOL uFlag) : ints(cities)
{
	 if (uFlag)
	     randomize();
	 else
		for (int i=0; i<ints.capacity(); i++)
		    ints[i] = i + 1;
}

BOOL     TSPind::isEqual(RCTObject obj) const
{
	if (obj.isA() == TSPind::isA()) {



	    // a cheaper but not always work way to
	    // check for equality
	    //
	    // tour_length(x1) == tour_length(x2) => x1 == x2
	    //
	    return ((RCTGAObj) obj).objValue() == objValue();

	    //
	    //	the following code checks for genotyping equality
	    //
	    RCTSPind tour = (RCTSPind) obj;
	    int i, j;

	    if (ints.capacity() != tour.ints.capacity())
		return FALSE;
	    for (j = 0; j < ints.capacity(); j++)
		if (tour.ints[j] == ints[0])
		    break;

	    if (j < ints.capacity()) { // first element aligned
		for (i = 1, j = j + 1; i < ints.capacity(); i++, j++) {
		     j %= ints.capacity();
		     if (tour.ints[j] != ints[i])
			 return FALSE;
		}
		return TRUE;
	    }
	    else
		return FALSE;
	}
	else
	    return FALSE;
}

void TSPind::randomize()
{
	int i, j, k, tmp;

	for (i = 0; i < ints.capacity(); i++)
	     ints.elem(i) = i + 1;

	for (i = 0, k = ints.capacity() - 1; i < ints.capacity(); i++) {
	    j = tRand(0, k);
	    tmp = ints.elem(i);
	    ints.elem(i) = ints.elem(j);
	    ints.elem(j) = tmp;
	}
}

void     TSPind::printOn( ostream  & out) const
{
	if (ints.capacity() > 0)
	    out << ints[0];

	for (register int nI = 1; nI < ints.capacity(); nI++)
	     out << ',' << ints[nI];

	out << " : " << objValue() << ", " << fitness();
}

int      TSPind::compare(RCTObject obj) const
{
	if (! obj.isKindOf(isA()))
	    return -1;
	else
            return objValue() - ((RCTGAObj) obj).objValue();
}

RTGAObj  TSPind::operator=(RCTGAObj source)
{
	if (!(this->isEqual(source))) {
	    PRECONDITION(source.isKindOf(isA()));
	    TGAObj::operator=(source);
	    ints = ((RCTSPind) source).ints;
	}
	return *this;
}

PTGAObj  TSPind::oddPtCrossover(RCTGAObj mate, int i) const
{
	PTSPind pChild = (PTSPind) new TSPind();
	edge(mate, *pChild);
	return pChild;
}

void     TSPind::oddPtCrossover(RCTGAObj father, int i, RTGAObj child) const
{
	edge(father, child);
}

PTGAObj  TSPind::evenPtCrossover(RCTGAObj mate, int i) const
{
	return oddPtCrossover(mate, i);
}

void     TSPind::evenPtCrossover(RCTGAObj father, int i, RTGAObj child) const
{
	oddPtCrossover(father, i, child);
}

PTGAObj  TSPind::uniformCrossover(RCTGAObj mate, float f) const
{
	return oddPtCrossover(mate, 1);
}

void     TSPind::uniformCrossover(RCTGAObj father, float f, RTGAObj child) const
{
	oddPtCrossover(father, 1, child);
}

void     TSPind::edge(RCTGAObj mate, RTGAObj offspring) const
{
	RCTSPind mother = (RCTSPind) *this;
	RCTSPind father = (RCTSPind) mate;
	RTSPind child = (RTSPind) offspring;

	if (mother.crossTotal++ % 50 == 0)
	    mother.printFlag = TRUE;

	edgeMap.buildEdgeTable(mother, father);
        edgeMap.buildTour(child.ints);
}

BOOL	TSPind::legalTour()
{
	ShortVec sorted = ints;

	sorted.sort(shortCompare);
	for (int i=1, j = 0; j < ints.capacity(); i++, j++)
	    if (i != sorted[j]) {
		cout << sorted << endl;
		return FALSE;
	    }

	return TRUE;
}

void	TSPind::twoOpt()
//
// given two segments that are not connected : (i,j) and (m,n)
// swap if d(i,m) + d(j,n) < d(i,j) + d(m,n)
//
// each segment is formed by two adjacent cities
//
{
	int i,j,k, endCheck = ints.capacity() - 2;
	int head = 1, tail = 2;
	int iprev, inext;
	int jprev, jnext;

	double curCost, newCost, deltaCost = MAXFLOAT;
	double savedCost;

	for (i=0; i < endCheck; i++) {
	    inext = i + 1;
	    for (j = i + 2; j < ints.capacity(); j++) {
		jnext = (j + 1) % ints.capacity();
		curCost = pTourInfo->edgeCost(ints.elem(i),
					     ints.elem(inext)) +
			 pTourInfo->edgeCost(ints.elem(j),
					     ints.elem(jnext));
		newCost = pTourInfo->edgeCost(ints.elem(i),
					     ints.elem(j)) +
			 pTourInfo->edgeCost(ints.elem(inext),
					     ints.elem(jnext));
		savedCost = newCost - curCost;
		if (savedCost <= deltaCost) {
		   if (savedCost == deltaCost) {
		       if (tRand.flip(0.5)) {
			  head = inext;
			  tail = j;
		       }
		   }
                   else {
                       deltaCost = savedCost;
                       head = inext;
                       tail = j;
                   }
		}
	    }
	}


	if (deltaCost < 0) {
	   reverse(head,tail);
	   curCost = objValue();
	   newCost = curCost + deltaCost;
/*
	   if ((objValue() = pTourInfo->cost(ints)) != newCost) {
	       cout << "twoOpt " << objValue() << "(" << newCost << ")" << endl;
	       exit(-1);
	   }
*/
	   objValue() = newCost;
	   twoOptTotal++;
	}

	return;
}

void	TSPind::mutate(float mrate)
{
	if (! tRand.flip(mrate))
	    return;

	int tmp, i, j, k, left, right;

	i = ints.capacity() * 0.4;
	left = tRand(0, ints.capacity() - 1);
	right = left + i;
	if (right >= ints.capacity()) {
	    tmp = left;
	    left -= i;
            right = tmp;
        }

	for (k = i / 4.0; k >= 0; k--) {
	    i = tRand(left,right);
	    do {
	       j = tRand(left,right);
	    } while (i == j);
	    tmp = ints.elem(i);
            ints.elem(i) = ints.elem(j);	
            ints.elem(j) = tmp;
        }
}

void	TSPind::optimizeLocal(double fraction)
{
        twoOpt();
}

void    TSPind::orOpt()
{
	for (int j = 3; j > 0; j--)
	     for (int i = 0; i < ints.capacity(); i++)
		  while (_orOpt(i,j));
	swapOpt();
}

int	TSPind::_orOpt(int head, int segLen)
{
	int tail;
	int cur, next, first, last;
	int insertAfter;
	double oldCost, fixedCost, savedCost, newCost1, newCost2;
	int i, j;
	int len;
	double newLinkCost, bestDelta, deltaCost;
	double tourCost = objValue();
	BOOL retFlag = 0;
	BOOL revFlag = 0;

	tail = (head + segLen - 1) % ints.capacity();
	first = (tail + 1) % ints.capacity();
	last = head - 1;

	if (last < 0)
	    last += ints.capacity();

	newLinkCost = pTourInfo->edgeCost(ints[last],ints[first]);

	fixedCost = pTourInfo->edgeCost(ints[last],ints[head]) +
		    pTourInfo->edgeCost(ints[tail],ints[first]);

	// try first edge
	cur = first;
	next = (cur + 1) % ints.capacity();
	oldCost = fixedCost + pTourInfo->edgeCost(ints[cur],ints[next]);
	newCost1 = pTourInfo->edgeCost(ints[cur],ints[head]) +
		   pTourInfo->edgeCost(ints[tail],ints[next]);
	newCost2 = pTourInfo->edgeCost(ints[cur],ints[tail]) +
		   pTourInfo->edgeCost(ints[head],ints[next]);
	
	if (newCost1 <= newCost2)
            bestDelta = deltaCost = newCost1 + newLinkCost - oldCost;
	else {
            bestDelta = deltaCost = newCost2 + newLinkCost - oldCost;
	    revFlag = 1;	// the segment will be reversed
        }

        insertAfter = cur; 

        // test other edges and find the best place 
	for (cur += 1; cur != last; cur++) {
	     if (cur >= ints.capacity()) {
		 cur = 0;
                 if (last == 0)
                     break;
             }
             next = (cur + 1) % ints.capacity();
             oldCost = fixedCost + pTourInfo->edgeCost(ints.elem(cur),
                                                       ints.elem(next));
	     newCost1 = pTourInfo->edgeCost(ints.elem(cur),ints.elem(head)) +
	                pTourInfo->edgeCost(ints.elem(tail),ints.elem(next));
	     newCost2 = pTourInfo->edgeCost(ints.elem(cur),ints.elem(tail)) +
	                pTourInfo->edgeCost(ints.elem(head),ints.elem(next));

	     if (newCost1 <= newCost2) {
	         if ((deltaCost = newCost1 + newLinkCost - oldCost) < bestDelta) {
                    insertAfter = cur; 
                    bestDelta = deltaCost;
		    revFlag = 0;
                 }
             }
             else
	         if ((deltaCost = newCost2 + newLinkCost - oldCost) < bestDelta) {
                    insertAfter = cur; 
                    bestDelta = deltaCost;
		    revFlag = 1;
                 }
	}

        if (bestDelta < 0) {  // the best change reduces tour length
	   ShortVec seg(segLen), newTour(ints.capacity());
	   for (i = head, j = 0; j < segLen; i++, j++) {
                if (i >= ints.capacity())
                    i = 0;
                seg.elem(j) = ints.elem(i);
           }

	   if (revFlag) 
	       seg.reverse();

	   cur = 0;
           if (first > last) {
               if (first <= insertAfter) {
                  cur += newTour.copySegment(cur,ints,first,insertAfter-first+1);
                  cur += newTour.copySegment(cur,seg,0,segLen);
                  if (insertAfter < (ints.capacity() - 1))
                      cur += newTour.copySegment(cur,ints,insertAfter+1,
                                                 ints.capacity()-insertAfter-1);
                  if (head > 0) 
                      newTour.copySegment(cur,ints,0,head);
               }
               else { // first > insertAfter
		  cur += newTour.copySegment(cur,ints,first,ints.capacity() - first);
                  cur += newTour.copySegment(cur,ints,0,insertAfter+1);
                  cur += newTour.copySegment(cur,seg,0,segLen);
                  newTour.copySegment(cur,ints,insertAfter+1,last-insertAfter);
               }
           }
           else { // last > first
               cur += newTour.copySegment(cur,ints,first,insertAfter-first+1);
               cur += newTour.copySegment(cur,seg,0,segLen);
               newTour.copySegment(cur,ints,insertAfter+1,last-insertAfter);
           }

           ints = newTour;  // set the current tour to new tour

	   objValue() = tourCost + bestDelta;
           retFlag = 1;
           orOptTotal++;
        }

	return retFlag;
}

void	TSPind::swapOpt()
{
	int i, j, iprev, inext, jprev, jnext, swapCity, tmp;
	double fixedCost, newCost, tourCost = objValue(), savedCost;

	i = 0;
        // calculate the cost of the edges connecting city i
        if ((iprev = i - 1) < 0)
	    iprev += ints.capacity();
        inext = (i + 1) % ints.capacity();

	// do not check adjacent city
        jprev = inext;
        j = (inext + 1) % ints.capacity();
        jnext = (j + 1) % ints.capacity();

	while (i < ints.capacity()) {
             fixedCost = pTourInfo->edgeCost(ints.elem(iprev), ints.elem(i)) + 
                         pTourInfo->edgeCost(ints.elem(i), ints.elem(inext));

             // calculate the cost changed if city i and city (i + 1) exchange
	     savedCost = pTourInfo->edgeCost(ints.elem(iprev), ints.elem(j)) + 
                         pTourInfo->edgeCost(ints.elem(j), ints.elem(inext)) + 
	                 pTourInfo->edgeCost(ints.elem(jprev), ints.elem(i)) + 
	                 pTourInfo->edgeCost(ints.elem(i), ints.elem(jnext)) - 
                         fixedCost -
                         pTourInfo->edgeCost(ints.elem(jprev), ints.elem(j)) - 
                         pTourInfo->edgeCost(ints.elem(j), ints.elem(jnext));
	     swapCity = j;

             jprev = j;
             j = jnext;
             jnext = (j + 1) % ints.capacity();
             do {
               // calculate the cost if i and j are swapped
	       newCost = pTourInfo->edgeCost(ints.elem(iprev), ints.elem(j)) +
			 pTourInfo->edgeCost(ints.elem(j), ints.elem(inext)) +
	                 pTourInfo->edgeCost(ints.elem(jprev), ints.elem(i)) + 
			 pTourInfo->edgeCost(ints.elem(i), ints.elem(jnext)) -
                         fixedCost -
                         pTourInfo->edgeCost(ints.elem(jprev), ints.elem(j)) - 
                         pTourInfo->edgeCost(ints.elem(j), ints.elem(jnext));
               if (savedCost > newCost) {
                   savedCost = newCost;
                   swapCity = j;
               }
               jprev = j;
               j = jnext;
               jnext = (j + 1) % ints.capacity();
             } while (jnext != i); 

             if (savedCost < 0) { // exchange city i and city 'swapCity'
                tmp = ints.elem(i);
                ints.elem(i) = ints.elem(swapCity);
                ints.elem(swapCity) = tmp;

                newCost = tourCost + savedCost;
/*
                tourCost = newCost;
                objValue() = pTourInfo->cost(ints);
                if (newCost != objValue()) {
                  cout << "swap opt : ";  
                  cout << ints;
                  cout << ' ' << objValue() << "(" << tourCost << '-' << savedCost << ")" << endl;
                  exit(-1);
		}
		else
		    tourCost = newCost;
*/
		objValue() = tourCost = newCost;
	     }
	     else
		i++;

	     // calculate the cost of the edges connecting city i
	     if ((iprev = i - 1) < 0)
		 iprev += ints.capacity();
	     inext = (i + 1) % ints.capacity();

	     // do not check adjacent city
	     jprev = inext;
	     j = (inext + 1) % ints.capacity();
	     jnext = (j + 1) % ints.capacity();
	}
}

void    TSPind::reverse(int head, int tail)
{
	if (head < tail) {
	   ShortVec seg = ints.at(head,tail-head+1);
	   seg.reverse();
	   ints.copySegment(head,seg,0,seg.capacity());
	}
	else {
	   int partLen = ints.capacity() - head;
	   ShortVec seg = ints.at(head, partLen);
	   seg += ints.at(0, tail + 1);
	   seg.reverse();
	   ints.copySegment(head,seg,0,partLen);
	   ints.copySegment(0,seg,partLen,seg.capacity() - partLen);
	}
}

void TSPind::debug(const char *string)
{
	double value;

		if ( ! legalTour() ) {
		   cout << *string << " illegal tour : " << ints << endl;
		   ints.sort(shortCompare);
		   cout << ints << endl;
		   exit(-1);
		}
}

