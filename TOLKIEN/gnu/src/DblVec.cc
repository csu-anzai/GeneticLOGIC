// This may look like C code, but it is really -*- C++ -*-
/* 
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of the GNU C++ Library.  This library is free
software; you can redistribute it and/or modify it under the terms of
the GNU Library General Public License as published by the Free
Software Foundation; either version 2 of the License, or (at your
option) any later version.  This library is distributed in the hope
that it will be useful, but WITHOUT ANY WARRANTY; without even the
implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
PURPOSE.  See the GNU Library General Public License for more details.
You should have received a copy of the GNU Library General Public
License along with this library; if not, write to the Free Software
Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
*/

#ifdef __GNUG__
#pragma implementation
#endif

#include <iostream.h>
#include <builtin.h>
#include "DblVec.h"

// error handling


void default_DblVec_error_handler(const char* msg)
{
  cerr << "Fatal DblVec error. " << msg << "\n";
  exit(1);
}

one_arg_error_handler_t DblVec_error_handler = default_DblVec_error_handler;

one_arg_error_handler_t set_DblVec_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = DblVec_error_handler;
  DblVec_error_handler = f;
  return old;
}

void DblVec::error(const char* msg)
{
  (*DblVec_error_handler)(msg);
}

void DblVec::range_error() const
{
  (*DblVec_error_handler)("Index out of range.");
}

DblVec::DblVec(DblVec& v)
{
  s = new double [len = v.len];
  double* top = &(s[len]);
  double* t = s;
  double* u = v.s;
  while (t < top) *t++ = *u++;
}

DblVec::DblVec(int l, double  fill_value)
{
  s = new double [len = l];
  double* top = &(s[len]);
  double* t = s;
  while (t < top) *t++ = fill_value;
}


DblVec& DblVec::operator = (DblVec& v)
{
  if (this != &v)
  {
    delete [] s;
    s = new double [len = v.len];
    double* top = &(s[len]);
    double* t = s;
    double* u = v.s;
    while (t < top) *t++ = *u++;
  }
  return *this;
}

void DblVec::apply(doubleProcedure f)
{
  double* top = &(s[len]);
  double* t = s;
  while (t < top) (*f)(*t++);
}

// can't just realloc since there may be need for constructors/destructors
void DblVec::resize(int newl)
{
  double* news = new double [newl];
  double* p = news;
  int minl = (len < newl)? len : newl;
  double* top = &(s[minl]);
  double* t = s;
  while (t < top) *p++ = *t++;
  delete [] s;
  s = news;
  len = newl;
}

DblVec concat(DblVec & a, DblVec & b)
{
  int newl = a.len + b.len;
  double* news = new double [newl];
  double* p = news;
  double* top = &(a.s[a.len]);
  double* t = a.s;
  while (t < top) *p++ = *t++;
  top = &(b.s[b.len]);
  t = b.s;
  while (t < top) *p++ = *t++;
  return DblVec(newl, news);
}


DblVec combine(doubleCombiner f, DblVec& a, DblVec& b)
{
  int newl = (a.len < b.len)? a.len : b.len;
  double* news = new double [newl];
  double* p = news;
  double* top = &(a.s[newl]);
  double* t = a.s;
  double* u = b.s;
  while (t < top) *p++ = (*f)(*t++, *u++);
  return DblVec(newl, news);
}

double DblVec::reduce(doubleCombiner f, double  base)
{
  double r = base;
  double* top = &(s[len]);
  double* t = s;
  while (t < top) r = (*f)(r, *t++);
  return r;
}

DblVec reverse(DblVec& a)
{
  double* news = new double [a.len];
  if (a.len != 0)
  {
    double* lo = news;
    double* hi = &(news[a.len - 1]);
    while (lo < hi)
    {
      double tmp = *lo;
      *lo++ = *hi;
      *hi-- = tmp;
    }
  }
  return DblVec(a.len, news);
}

void DblVec::reverse()
{
  if (len != 0)
  {
    double* lo = s;
    double* hi = &(s[len - 1]);
    while (lo < hi)
    {
      double tmp = *lo;
      *lo++ = *hi;
      *hi-- = tmp;
    }
  }
}

int DblVec::index(double  targ)
{
  for (int i = 0; i < len; ++i) if (targ == s[i]) return i;
  return -1;
}

DblVec map(doubleMapper f, DblVec& a)
{
  double* news = new double [a.len];
  double* p = news;
  double* top = &(a.s[a.len]);
  double* t = a.s;
  while(t < top) *p++ = (*f)(*t++);
  return DblVec(a.len, news);
}

int operator == (DblVec& a, DblVec& b)
{
  if (a.len != b.len)
    return 0;
  double* top = &(a.s[a.len]);
  double* t = a.s;
  double* u = b.s;
  while (t < top) if (*t++ != *u++) return 0;
  return 1;
}

void DblVec::fill(double  val, int from, int n)
{
  int to;
  if (n < 0)
    to = len - 1;
  else
    to = from + n - 1;
  if ((unsigned)from > (unsigned)to)
    range_error();
  double* t = &(s[from]);
  double* top = &(s[to]);
  while (t <= top) *t++ = val;
}

DblVec DblVec::at(int from, int n)
{
  int to;
  if (n < 0)
  {
    n = len - from;
    to = len - 1;
  }
  else
    to = from + n - 1;
  if ((unsigned)from > (unsigned)to)
    range_error();
  double* news = new double [n];
  double* p = news;
  double* t = &(s[from]);
  double* top = &(s[to]);
  while (t <= top) *p++ = *t++;
  return DblVec(n, news);
}

DblVec merge(DblVec & a, DblVec & b, doubleComparator f)
{
  int newl = a.len + b.len;
  double* news = new double [newl];
  double* p = news;
  double* topa = &(a.s[a.len]);
  double* as = a.s;
  double* topb = &(b.s[b.len]);
  double* bs = b.s;

  for (;;)
  {
    if (as >= topa)
    {
      while (bs < topb) *p++ = *bs++;
      break;
    }
    else if (bs >= topb)
    {
      while (as < topa) *p++ = *as++;
      break;
    }
    else if ((*f)(*as, *bs) <= 0)
      *p++ = *as++;
    else
      *p++ = *bs++;
  }
  return DblVec(newl, news);
}

static int gsort(double*, int, doubleComparator);
 
void DblVec::sort (doubleComparator compar)
{
  gsort(s, len, compar);
}


// An adaptation of Schmidt's new quicksort

static inline void SWAP(double* A, double* B)
{
  double tmp = *A; *A = *B; *B = tmp;
}

/* This should be replaced by a standard ANSI macro. */
#define BYTES_PER_WORD 8
#define BYTES_PER_LONG 4

/* The next 4 #defines implement a very fast in-line stack abstraction. */

#define STACK_SIZE (BYTES_PER_WORD * BYTES_PER_LONG)
#define PUSH(LOW,HIGH) do {top->lo = LOW;top++->hi = HIGH;} while (0)
#define POP(LOW,HIGH)  do {LOW = (--top)->lo;HIGH = top->hi;} while (0)
#define STACK_NOT_EMPTY (stack < top)                

/* Discontinue quicksort algorithm when partition gets below this size.
   This particular magic number was chosen to work best on a Sun 4/260. */
#define MAX_THRESH 4


/* Order size using quicksort.  This implementation incorporates
   four optimizations discussed in Sedgewick:
   
   1. Non-recursive, using an explicit stack of pointer that
      store the next array partition to sort.  To save time, this
      maximum amount of space required to store an array of
      MAX_INT is allocated on the stack.  Assuming a 32-bit integer,
      this needs only 32 * sizeof (stack_node) == 136 bits.  Pretty
      cheap, actually.

   2. Chose the pivot element using a median-of-three decision tree.
      This reduces the probability of selecting a bad pivot value and 
      eliminates certain extraneous comparisons.

   3. Only quicksorts TOTAL_ELEMS / MAX_THRESH partitions, leaving
      insertion sort to order the MAX_THRESH items within each partition.  
      This is a big win, since insertion sort is faster for small, mostly
      sorted array segements.
   
   4. The larger of the two sub-partitions is always pushed onto the
      stack first, with the algorithm then concentrating on the
      smaller partition.  This *guarantees* no more than log (n)
      stack size is needed! */
      
static int gsort (double *base_ptr, int total_elems, doubleComparator cmp)
{
/* Stack node declarations used to store unfulfilled partition obligations. */
  struct stack_node {  double *lo;  double *hi; };
  double   pivot_buffer;
  int   max_thresh   = MAX_THRESH;

  if (total_elems > MAX_THRESH)
    {
      double       *lo = base_ptr;
      double       *hi = lo + (total_elems - 1);
      double       *left_ptr;
      double       *right_ptr;
      stack_node stack[STACK_SIZE]; /* Largest size needed for 32-bit int!!! */
      stack_node *top = stack + 1;

      while (STACK_NOT_EMPTY)
        {
          {
            double *pivot = &pivot_buffer;
            {
              /* Select median value from among LO, MID, and HI. Rearrange
                 LO and HI so the three values are sorted. This lowers the 
                 probability of picking a pathological pivot value and 
                 skips a comparison for both the LEFT_PTR and RIGHT_PTR. */

              double *mid = lo + ((hi - lo) >> 1);

              if ((*cmp) (*mid, *lo) < 0)
                SWAP (mid, lo);
              if ((*cmp) (*hi, *mid) < 0)
              {
                SWAP (mid, hi);
                if ((*cmp) (*mid, *lo) < 0)
                  SWAP (mid, lo);
              }
              *pivot = *mid;
              pivot = &pivot_buffer;
            }
            left_ptr  = lo + 1;
            right_ptr = hi - 1; 

            /* Here's the famous ``collapse the walls'' section of quicksort.  
               Gotta like those tight inner loops!  They are the main reason 
               that this algorithm runs much faster than others. */
            do 
              {
                while ((*cmp) (*left_ptr, *pivot) < 0)
                  left_ptr += 1;

                while ((*cmp) (*pivot, *right_ptr) < 0)
                  right_ptr -= 1;

		if (left_ptr < right_ptr)
                  {
                    SWAP (left_ptr, right_ptr);
                    left_ptr += 1;
                    right_ptr -= 1;
                  }
                else if (left_ptr == right_ptr) 
                  {
                    left_ptr += 1;
                    right_ptr -= 1;
                    break;
                  }
              } 
            while (left_ptr <= right_ptr);

          }

          /* Set up pointers for next iteration.  First determine whether
             left and right partitions are below the threshold size. If so, 
             ignore one or both.  Otherwise, push the larger partition's
             bounds on the stack and continue sorting the smaller one. */

	  if ((right_ptr - lo) <= max_thresh)
            {
              if ((hi - left_ptr) <= max_thresh) /* Ignore both small partitions. */
                POP (lo, hi); 
              else              /* Ignore small left partition. */  
                lo = left_ptr;
            }
          else if ((hi - left_ptr) <= max_thresh) /* Ignore small right partition. */
            hi = right_ptr;
          else if ((right_ptr - lo) > (hi - left_ptr)) /* Push larger left partition indices. */
            {                   
              PUSH (lo, right_ptr);
              lo = left_ptr;
            }
          else                  /* Push larger right partition indices. */
            {                   
              PUSH (left_ptr, hi);
              hi = right_ptr;
            }
        }
    }

  /* Once the BASE_PTR array is partially sorted by quicksort the rest
     is completely sorted using insertion sort, since this is efficient 
     for partitions below MAX_THRESH size. BASE_PTR points to the beginning 
     of the array to sort, and END_PTR points at the very last element in
     the array (*not* one beyond it!). */


  {
    double *end_ptr = base_ptr + 1 * (total_elems - 1);
    double *run_ptr;
    double *tmp_ptr = base_ptr;
    double *thresh  = (end_ptr < (base_ptr + max_thresh))? 
      end_ptr : (base_ptr + max_thresh);

    /* Find smallest element in first threshold and place it at the
       array's beginning.  This is the smallest array element,
       and the operation speeds up insertion sort's inner loop. */

    for (run_ptr = tmp_ptr + 1; run_ptr <= thresh; run_ptr += 1)
      if ((*cmp) (*run_ptr, *tmp_ptr) < 0)
        tmp_ptr = run_ptr;

    if (tmp_ptr != base_ptr)
      SWAP (tmp_ptr, base_ptr);

    /* Insertion sort, running from left-hand-side up to `right-hand-side.' 
       Pretty much straight out of the original GNU qsort routine. */

    for (run_ptr = base_ptr + 1; (tmp_ptr = run_ptr += 1) <= end_ptr; )
      {

        while ((*cmp) (*run_ptr, *(tmp_ptr -= 1)) < 0)
          ;

        if ((tmp_ptr += 1) != run_ptr)
          {
            double *trav;

            for (trav = run_ptr + 1; --trav >= run_ptr;)
              {
                double c = *trav;
                double *hi, *lo;

                for (hi = lo = trav; (lo -= 1) >= tmp_ptr; hi = lo)
		  *hi = *lo;
                *hi = c;
	      }
	  }

      }
  }
  return 1;
}

//
// added by Anthony Tang (CUHK) for use by TOLKIEN
//

#include "ranksb.h"

void    DblVec::evenPtCrossover(const DblVec& mother,
                                const DblVec& father, int nXpts)
//
//      no checking on the length of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
//      it is assumed that nXpts is even
//
//      so if length of father = length of mother = 1
//      a runtime error will occur
//
{
        DblVec *pShorter, *pLonger;
        int i, j, k, m, n, len;
        BOOL   flag;

        if (mother.capacity() >= father.capacity()) {
            pLonger = (DblVec*) &mother;
            pShorter = (DblVec*) &father;
	}
	else {
            pLonger =(DblVec*) &father;
            pShorter = (DblVec*) &mother;
	}

	len = pShorter->capacity() - 1;
	*this = *pLonger;

        IntVec  xpts;
        ranksb.ordered(xpts, nXpts, pShorter->capacity());

        if (xpts[0] == 0) {
            i = xpts.capacity() - 1;
            if (xpts[i] == len)
	       xpts[i]--;
        }

        for (i = 0; i < nXpts; i+= 2) {
                j = xpts[i];
		k = xpts[i+1] - j;
                for (m = 0, n = j; m < k; m++, n++)
                     elem(n) = pShorter->elem(n);
        }
}

void    DblVec::oddPtCrossover(const DblVec& mother,
                               const DblVec& father, int nXpt)
//
//      no checking on the capacity of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
//      so if capacity of father = capacity of mother = 1
//      a runtime error will occur
//
{
        DblVec *pCurMate, *pNextMate, *pTemp;
        int cur, i, select, remaining;
        BOOL flag;
	IntVec xpts;

	if (mother.capacity() >= father.capacity()) {
	    pNextMate = (DblVec*) &mother;
	    pCurMate = (DblVec*) &father;
	}
	else {
	    pNextMate = (DblVec*) &father;
	    pCurMate = (DblVec*) &mother;
	}

	resize(0);  // empty child first


	ranksb.ordered(xpts, nXpt, (int) pCurMate->capacity());

	for (i=0, cur=0; i < nXpt; i++) {
	       *this = concat(*this, pCurMate->at(cur, xpts[i] - cur + 1));
	       // swap parents
	       pTemp = pCurMate;
	       pCurMate = pNextMate;
	       pNextMate = pTemp;
	       cur = xpts[i] + 1;
	}

        if (cur < pCurMate->capacity()) {
            // copy last segment
            if ((i = pCurMate->capacity() - cur) > 0)
                *this = concat(*this, pCurMate->at(cur, i) );
        }
}

void    DblVec::uniformCrossover(const DblVec& mother,
				    const DblVec& father,
				    float flXRate)
{
	this->resize(mother.capacity());

	for (register int nI = 0 ; nI < mother.capacity(); nI++)
		if (tRand.flip(flXRate))
		    elem(nI) = mother[nI];
		else
		    elem(nI) = father[nI];

}

ostream & operator << (ostream & out, DblVec & dbls)
{
	if (dbls.capacity() == 0)
	    return out;

	out << dbls.elem(0);
	for (register int i = 1, j = dbls.capacity(); i < j; i++)
	    out << ',' << dbls.elem(i);

	return out;
}
