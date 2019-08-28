// This may look like C code, but it is really -*- C++ -*-
/*
Copyright (C) 1988 Free Software Foundation
    written by Doug Lea (dl@rocky.oswego.edu)

This file is part of GNU CC.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY.  No author or distributor
accepts responsibility to anyone for the consequences of using it
or for whether it serves any particular purpose or works at all,
unless he says so in writing.  Refer to the GNU CC General Public
License for full details.

Everyone is granted permission to copy, modify and redistribute
GNU CC, but only under the conditions described in the
GNU CC General Public License.   A copy of this license is
supposed to have been given to you along with GNU CC so you
can know your rights and responsibilities.  It should be in a
file named COPYING.  Among other things, the copyright notice
and this notice must be preserved on all copies.
*/

#include <iostream.h>
#include <stdlib.h>
#include "SIntVec.h"
#include "ranksb.h"

// error handling

void default_ShortVec_error_handler(const char* msg)
{
  cerr << "Fatal ShortVec error. " << msg << "\n";
  exit(1);
}

one_arg_error_handler_t ShortVec_error_handler = default_ShortVec_error_handler;

one_arg_error_handler_t set_ShortVec_error_handler(one_arg_error_handler_t f)
{
  one_arg_error_handler_t old = ShortVec_error_handler;
  ShortVec_error_handler = f;
  return old;
}

void ShortVec::error(const char* msg) const
{
  (*ShortVec_error_handler)(msg);
}

void ShortVec::range_error() const
{
  (*ShortVec_error_handler)("Index out of range.");
}

ShortVec::ShortVec(ShortVec& v)
{
  s = new short [len = v.len];
  short* top = &(s[len]);
  short* t = s;
  short* u = v.s;
  while (t < top) *t++ = *u++;
}

ShortVec::ShortVec(int l, short  fill_value)
{
  s = new short [len = l];
  short* top = &(s[len]);
  short* t = s;
  while (t < top) *t++ = fill_value;
}


ShortVec& ShortVec::operator = (const ShortVec& v)
{
  if (this != &v)
  {
    delete[len] s;
    s = new short [len = v.len];
    short* top = &(s[len]);
    short* t = s;
    short* u = v.s;
    while (t < top) *t++ = *u++;
  }
  return *this;
}

void ShortVec::apply(shortProcedure f)
{
  short* top = &(s[len]);
  short* t = s;
  while (t < top) (*f)(*t++);
}

// can't just realloc since there may be need for constructors/destructors
void ShortVec::resize(int newl)
{
  short* news = new short [newl];
  short* p = news;
  int minl = (len < newl)? len : newl;
  short* top = &(s[minl]);
  short* t = s;
  while (t < top) *p++ = *t++;
  delete [len] s;
  s = news;
  len = newl;
}

ShortVec concat(ShortVec & a, ShortVec & b)
{
  int newl = a.len + b.len;
  short* news = new short [newl];
  short* p = news;
  short* top = &(a.s[a.len]);
  short* t = a.s;
  while (t < top) *p++ = *t++;
  top = &(b.s[b.len]);
  t = b.s;
  while (t < top) *p++ = *t++;
  return ShortVec(newl, news);
}


ShortVec combine(shortCombiner f, ShortVec& a, ShortVec& b)
{
  int newl = (a.len < b.len)? a.len : b.len;
  short* news = new short [newl];
  short* p = news;
  short* top = &(a.s[newl]);
  short* t = a.s;
  short* u = b.s;
  while (t < top) *p++ = (*f)(*t++, *u++);
  return ShortVec(newl, news);
}

short ShortVec::reduce(shortCombiner f, short  base)
{
  short r = base;
  short* top = &(s[len]);
  short* t = s;
  while (t < top) r = (*f)(r, *t++);
  return r;
}

ShortVec reverse(ShortVec& a)
{
  short* news = new short [a.len];
  if (a.len != 0)
  {
    short* lo = news;
    short* hi = &(news[a.len - 1]);
    while (lo < hi)
    {
      short tmp = *lo;
      *lo++ = *hi;
      *hi-- = tmp;
    }
  }
  return ShortVec(a.len, news);
}

void ShortVec::reverse()
{
  if (len != 0)
  {
    short* lo = s;
    short* hi = &(s[len - 1]);
    while (lo < hi)
    {
      short tmp = *lo;
      *lo++ = *hi;
      *hi-- = tmp;
    }
  }
}

int ShortVec::index(short  targ)
{
  for (int i = 0; i < len; ++i) if (targ == s[i]) return i;
  return -1;
}

ShortVec map(shortMapper f, ShortVec& a)
{
  short* news = new short [a.len];
  short* p = news;
  short* top = &(a.s[a.len]);
  short* t = a.s;
  while(t < top) *p++ = (*f)(*t++);
  return ShortVec(a.len, news);
}

int operator == (ShortVec& a, ShortVec& b)
{
  if (a.len != b.len)
    return 0;
  short* top = &(a.s[a.len]);
  short* t = a.s;
  short* u = b.s;
  while (t < top) if (*t++ != *u++) return 0;
  return 1;
}

void ShortVec::fill(short  val, int from, int n)
{
  int to;
  if (n < 0)
    to = len - 1;
  else
    to = from + n - 1;
  if ((unsigned)from > to)
    range_error();
  short* t = &(s[from]);
  short* top = &(s[to]);
  while (t <= top) *t++ = val;
}

ShortVec ShortVec::at(int from, int n)
{
  int to;
  if (n < 0)
  {
    n = len - from;
    to = len - 1;
  }
  else
    to = from + n - 1;
  if ((unsigned)from > to)
    range_error();
  short* news = new short [n];
  short* p = news;
  short* t = &(s[from]);
  short* top = &(s[to]);
  while (t <= top) *p++ = *t++;
  return ShortVec(n, news);
}

ShortVec merge(ShortVec & a, ShortVec & b, shortComparator f)
{
  int newl = a.len + b.len;
  short* news = new short [newl];
  short* p = news;
  short* topa = &(a.s[a.len]);
  short* as = a.s;
  short* topb = &(b.s[b.len]);
  short* bs = b.s;

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
  return ShortVec(newl, news);
}

static int gsort(short*, int, shortComparator);

void ShortVec::sort (shortComparator compar)
{
  gsort(s, len, compar);
}


// An adaptation og Schmidt's new quicksort

static inline void SWAP(short* A, short* B)
{
  short tmp = *A; *A = *B; *B = tmp;
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

static int gsort (short *base_ptr, int total_elems, shortComparator cmp)
{
/* Stack node declarations used to store unfulfilled partition obligations. */
  struct stack_node {  short *lo;  short *hi; };
  short   pivot_buffer;
  int   max_thresh   = MAX_THRESH;

  if (total_elems > MAX_THRESH)
    {
      short       *lo = base_ptr;
      short       *hi = lo + (total_elems - 1);
      short       *left_ptr;
      short       *right_ptr;
      stack_node stack[STACK_SIZE]; /* Largest size needed for 32-bit int!!! */
      stack_node *top = stack + 1;

      while (STACK_NOT_EMPTY)
        {
          {
            short *pivot = &pivot_buffer;
            {
              /* Select median value from among LO, MID, and HI. Rearrange
                 LO and HI so the three values are sorted. This lowers the
                 probability of picking a pathological pivot value and
                 skips a comparison for both the LEFT_PTR and RIGHT_PTR. */

              short *mid = lo + ((hi - lo) >> 1);

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
    short *end_ptr = base_ptr + 1 * (total_elems - 1);
    short *run_ptr;
    short *tmp_ptr = base_ptr;
    short *thresh  = (end_ptr < (base_ptr + max_thresh))?
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
            short *trav;

            for (trav = run_ptr + 1; --trav >= run_ptr;)
              {
                short c = *trav;
                short *hi, *lo;

                for (hi = lo = trav; (lo -= 1) >= tmp_ptr; hi = lo)
                  *hi = *lo;
                *hi = c;
              }
          }

      }
  }
  return 1;
}

void    ShortVec::evenPtCrossover(const ShortVec& mother,
                                const ShortVec& father, int nSegment)
//
//      no checking on the length of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
//      so if length of father = length of mother = 1
//      a runtime error will occur
//
{
        ShortVec *pShorter, *pLonger;
        int i, j, k, m, n, len, nXpt;
        BOOL   flag;

        if (mother.capacity() >= father.capacity()) {
            pLonger = (ShortVec*) &mother;
            pShorter = (ShortVec*) &father;
        }
        else {
            pLonger =(ShortVec*) &father;
            pShorter = (ShortVec*) &mother;
        }

        len = pShorter->capacity() - 1;
        *this = *pLonger;
        nXpt =  nSegment * 2;

        IntVec  xpts;
        ranksb.ordered(xpts, nXpt, pShorter->capacity());

        if (xpts[0] == 0) {
            i = xpts.capacity() - 1;
            if (xpts[i] == len)
               xpts[i]--;
        }

        for (i = 0, nXpt = nSegment * 2; i < nXpt; i+= 2) {
                j = xpts[i];
                k = xpts[i+1] - j;
                for (m = 0, n = j; m < k; m++, n++)
                     elem(n) = pShorter->elem(n);
        }
}

void    ShortVec::oddPtCrossover(const ShortVec& mother,
                                  const ShortVec& father, int nXpt)
//
//      no checking on the capacity of the strings is performed
//      because this is assumed to be the task of the class Crossover
//
//      so if capacity of father = capacity of mother = 1
//      a runtime error will occur
//
{
        ShortVec *pCurMate, *pNextMate, *pTemp;
        int cur, i, j, k, select, remaining;
        BOOL flag;

        if (mother.capacity() >= father.capacity()) {
            pNextMate = (ShortVec*) &mother;
            pCurMate = (ShortVec*) &father;
        }
        else {
            pNextMate = (ShortVec*) &father;
            pCurMate = (ShortVec*) &mother;
        }

        resize(0);  // empty child first

        IntVec  xpts;
	ranksb.ordered(xpts, nXpt, (int) pCurMate->capacity());

        for (i=0, cur=0; i < xpts.capacity(); i++) {
               *this = concat(*this, pCurMate->at(cur, xpts[i] - cur + 1));
               // swap parents
               pTemp = pCurMate;
               pCurMate = pNextMate;
               pNextMate = pTemp;
               cur = xpts[i] + 1;
        }

        if (cur <= pCurMate->capacity())
            // copy last segment
            *this = concat(*this, pCurMate->at(cur, pCurMate->capacity() - cur));
}

void    ShortVec::uniformCrossover(const ShortVec& mother,
                                    const ShortVec& father,
                                    float flXRate)
{
        this->resize(mother.capacity());

        for (register int nI = 0 ; nI < mother.capacity(); nI++)
                if (tRand.flip(flXRate))
                    elem(nI) = mother[nI];
                else
                    elem(nI) = father[nI];

}

ostream & operator << (ostream & out, const ShortVec & ints)
{
        if (ints.capacity() == 0)
            return out;

        out << ints[0];
        for (int i = 1, j = ints.capacity(); i < j; i++)
            out << ',' << ints[i];

        return out;
}

const ShortVec& ShortVec::operator+=(ShortVec & a)
{
  int newl = len + a.len;
  short* news = new short[newl];
  short* p = news;
  short* top = &(s[len]);
  short* t = s;
  while (t < top) *p++ = *t++;
//  memcpy(p,t,len * sizeof(short));
  top = &(a.s[a.len]);
  t = a.s;
  while (t < top) *p++ = *t++;
/*
  p+=len;
  memcpy(p, a.s, a.len * sizeof(short));
*/
  delete [len] s;
  s = news;
  len = newl;
  return *this;
}

int ShortVec::copySegment(int pos, ShortVec & a, int start, int segLen)
{
  if ((segLen <= 0) || (pos >= len))
      return 0;

  if (start > a.len)
      start = 0;

  if (segLen > (len - pos))
      segLen = len - pos;

  if (start + segLen > a.len)
      segLen = a.len - start;

  short* top = &(a.s[start + segLen]);
  short* t = &(a.s[start]);
  short* p = &(s[pos]);
  // while (t < top) *p++ = *t++;
  memcpy(p,t,segLen * sizeof(short));

  return segLen;
}

