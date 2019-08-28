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


#ifndef _IntVec_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _IntVec_h 1

#include  <builtin.h>

#ifndef _int_typedefs
#define _int_typedefs 1
typedef void (*intProcedure)(int );
typedef int  (*intMapper)(int );
typedef int  (*intCombiner)(int , int );
typedef int  (*intPredicate)(int );
typedef int  (*intComparator)(int , int );
#endif

inline  int intCompare(int elem1, int elem2)
{
        return elem1 - elem2;
}

inline	int  numeric(void *p1, void *p2)
{
	return (*((int *) p1) - *((int *) p2));
}

class IntVec
{
protected:      
  int                   len;
  int                   *s;

                        IntVec(int l, int* d);
public:
                        IntVec ();
                        IntVec (int l);
                        IntVec (int l, int  fill_value);
                        IntVec (IntVec&);
                virtual ~IntVec ();

  virtual IntVec &              operator = (const IntVec & a);
  virtual IntVec                at(int from = 0, int n = -1);

  virtual   int                   capacity() const;
  virtual   void                  resize(int newlen);

  virtual   int&                 operator [] (int n);
  virtual   int                  operator [] (int n) const;

  virtual   int&                  elem(int n);

  friend IntVec         concat(IntVec & a, IntVec & b);
  friend IntVec         map(intMapper f, IntVec & a);
  friend IntVec         merge(IntVec & a, IntVec & b, intComparator f);
  friend IntVec         combine(intCombiner f, IntVec & a, IntVec & b);
  friend IntVec         reverse(IntVec & a);

  virtual   void                  reverse();
  virtual   void                  sort(intComparator f);
  virtual   void                  fill(int  val, int from = 0, int n = -1);

  virtual   void                  apply(intProcedure f);
  virtual   int                   reduce(intCombiner f, int  base);
  virtual   int                   index(int  targ);

  friend int            operator == (IntVec& a, IntVec& b);
  friend int            operator != (IntVec& a, IntVec& b);

  virtual   void                  error(const char* msg) const;
  virtual   void                  range_error() const;

// added by Anthony Tang (CUHK) for use by TOLKIEN

    // crossover operators
  virtual  void oddPtCrossover(const IntVec&, const IntVec&, int);
  virtual  void evenPtCrossover(const IntVec&, const IntVec&, int);
  virtual  void uniformCrossover(const IntVec&, const IntVec&, float);

  friend ostream & operator << (ostream &, IntVec &);

};

extern void default_IntVec_error_handler(const char*);
extern one_arg_error_handler_t IntVec_error_handler;

extern one_arg_error_handler_t 
        set_IntVec_error_handler(one_arg_error_handler_t f);


inline IntVec::IntVec()
{
  len = 0; s = 0;
}

inline IntVec::IntVec(int l)
{
  s = new int [len = l];
}


inline IntVec::IntVec(int l, int* d) :len(l), s(d) {}


inline IntVec::~IntVec()
{
  delete[len] s;
}


inline int& IntVec::operator [] (int n)
{
  if ((unsigned)n >= len)
    range_error();
  return s[n];
}

inline int IntVec::operator [] (int n) const
{
  if ((unsigned)n >= len)
    range_error();
  return s[n];
}

inline int& IntVec::elem(int n)
{
  return s[n];
}


inline int IntVec::capacity() const
{
  return len;
}

inline int operator != (IntVec& a, IntVec& b)
{
  return !(a == b);
}

#endif

