//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "ranksb.h"

RandIntSubSet    ranksb;

void RandIntSubSet::ordered(IntVec& ints, int m, int n)
//
// use algorithm S and algorithm P of section 3.4.2 of
// Knuth's Seminumerical Algorithms to generate M
// random integers from 0, 1, .., N - 1
//
{
        int select, remaining, i, j;

	if (m < n) {
	   ints.resize(m);
	   for (i=1, j = 0, remaining = n, select = m;
		i <= n; i++, remaining--)
		if (tRand.asFloat() < ((float) select)/remaining) {
		    ints[j++] = i - 1;
		    select--;
		}
	}
	else { // generate a random permutation
	   ints.resize(n);
	   for (i=0; i < n; i++)
		ints[i] = i;
	   for (i=n - 1; i > 0; i--) {
		j = tRand(0,i);
		select = ints[j];
		ints[j] = ints[i];
		ints[i] = select;
	   }

	}
}

void RandIntSubSet::random(IntVec& ints, int k, int n)
//
// generate K random integers from 0, 1, .., N - 1
// the elements of the subset are not sorted
//
{
        int r, i, m, j;

        ints.resize(k);

        if (k == n) {
            // shuffle the set [0 ... N-1]
            for (i = 0; i < n; i++)
                 ints.elem(i) = i;
            for (i = 0, r = n - 1; i < n; i++) {
                 j = tRand(i, r);  // swap ints[i] and ints[j]
                 m = ints.elem(i);
                 ints.elem(i) = ints.elem(j);
                 ints.elem(j) = m;
            }
        }
        else {
            IntVec link(k,0);
            ints.fill(-1);  // ints[k] == -1 means element k is available

            for (r = k; r >= 0;) {
                 i = (m = tRand.asFloat() * n) % k;

                 if (ints.elem(i) < 0) { // slot available
                     ints.elem(i) = m;
                     link.elem(i) = 0;
                 }
                 else
                     if (m != ints.elem(i)) {
                         while (link.elem(i) > 0) {
                                if (ints.elem(i) == m)
                                    break;
                                i = link.elem(i);
                         }

                         if (ints.elem(i) != m) {
                            // find home for m
                            while (--r > 0)
                                if (ints.elem(r) < 0)
                                    break;
                            if (r >= 0) {
                                link.elem(i) = r;
                                i = r;
                                ints.elem(i) = m;
                                link.elem(i) = 0;
                             }
                         }
                 }
            }
        }
}

