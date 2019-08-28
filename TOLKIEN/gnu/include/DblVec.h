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


#ifndef _DblVec_h
#ifdef __GNUG__
#pragma interface
#endif
#define _DblVec_h 1

#include <builtin.h>

#ifndef _double_typedefs
#define _double_typedefs 1
typedef void (*doubleProcedure)(double );
typedef double  (*doubleMapper)(double );
typedef double  (*doubleCombiner)(double , double );
typedef int  (*doublePredicate)(double );
typedef int  (*doubleComparator)(double , double );
#endif


class DblVec 
{
protected:      
  int                   len;
  double                   *s;                  

                        DblVec(int l, double* d);
public:
                        DblVec ();
                        DblVec (int l);
                        DblVec (int l, double  fill_value);
                        DblVec (DblVec&);
                        ~DblVec ();

  DblVec &              operator = (DblVec & a);
  DblVec                at(int from = 0, int n = -1);

  int                   capacity() const;
  void                  resize(int newlen);                        

  double&                  operator [] (int n);
  double                   operator [] (int n) const;

  double&                  elem(int n);

  friend DblVec         concat(DblVec & a, DblVec & b);
  friend DblVec         map(doubleMapper f, DblVec & a);
  friend DblVec         merge(DblVec & a, DblVec & b, doubleComparator f);
  friend DblVec         combine(doubleCombiner f, DblVec & a, DblVec & b);
  friend DblVec         reverse(DblVec & a);

  void                  reverse();
  void                  sort(doubleComparator f);
  void                  fill(double  val, int from = 0, int n = -1);

  void                  apply(doubleProcedure f);
  double                   reduce(doubleCombiner f, double  base);
  int                   index(double  targ);

  friend int            operator == (DblVec& a, DblVec& b);
  friend int            operator != (DblVec& a, DblVec& b);

  void                  error(const char* msg);
  void                  range_error() const;

// added by Anthony Tang (CUHK) for use by TOLKIEN

    // crossover operators
  virtual  void oddPtCrossover(const DblVec&, const DblVec&, int);
  virtual  void evenPtCrossover(const DblVec&, const DblVec&, int);
  virtual  void uniformCrossover(const DblVec&, const DblVec&, float);

  friend ostream & operator << (ostream &, DblVec &);
};

extern void default_DblVec_error_handler(const char*);
extern one_arg_error_handler_t DblVec_error_handler;

extern one_arg_error_handler_t 
        set_DblVec_error_handler(one_arg_error_handler_t f);


inline DblVec::DblVec()
{
  len = 0; s = 0;
}

inline DblVec::DblVec(int l)
{
  s = new double [len = l];
}


inline DblVec::DblVec(int l, double* d) :len(l), s(d) {}


inline DblVec::~DblVec()
{
  delete [] s;
}


inline double& DblVec::operator [] (int n)
{
  if ((unsigned)n >= (unsigned)len)
    range_error();
  return s[n];
}

inline double DblVec::operator [] (int n) const
{
  if ((unsigned)n >= (unsigned)len)
    range_error();
  return s[n];
}

inline double& DblVec::elem(int n)
{
  return s[n];
}


inline int DblVec::capacity() const
{
  return len;
}

inline int operator != (DblVec& a, DblVec& b)
{
  return !(a == b);
}

#endif

