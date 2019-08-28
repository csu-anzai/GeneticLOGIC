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


#ifndef _ShortVec_h
#ifdef __GNUG__
#pragma once
#pragma interface
#endif
#define _ShortVec_h 1

#include <builtin.h>

#ifndef _short_typedefs
#define _short_typedefs 1
typedef void (*shortProcedure)(short );
typedef short  (*shortMapper)(short );
typedef short  (*shortCombiner)(short , short );
typedef int  (*shortPredicate)(short );
typedef int  (*shortComparator)(short , short );
#endif


inline  int shortCompare(short elem1, short elem2)
{
        return elem1 - elem2;
}

class ShortVec
{
protected:
  int                   len;
  short                   *s;

                        ShortVec(int l, short* d);
public:
                        ShortVec ();
                        ShortVec (int l);
                        ShortVec (int l, short  fill_value);
                        ShortVec (ShortVec&);
                        ~ShortVec ();

  virtual ShortVec &              operator = (const ShortVec & a);
  virtual ShortVec                at(int from = 0, int n = -1);

  virtual   int                   capacity() const;
  virtual   void                  resize(int newlen);

  virtual   short&                 operator [] (int n) const;
  virtual   short&                  elem(int n);

  friend ShortVec         concat(ShortVec & a, ShortVec & b);
  friend ShortVec         map(shortMapper f, ShortVec & a);
  friend ShortVec         merge(ShortVec & a, ShortVec & b, shortComparator f);
  friend ShortVec         combine(shortCombiner f, ShortVec & a, ShortVec & b);
  friend ShortVec         reverse(ShortVec & a);

  virtual   void                  reverse();
  virtual   void                  sort(shortComparator f);
  virtual   void                  fill(short  val, int from = 0, int n = -1);

  virtual   void                  apply(shortProcedure f);
  virtual   short                   reduce(shortCombiner f, short  base);
  virtual   int                   index(short  targ);

  friend int            operator == (ShortVec& a, ShortVec& b);
  friend int            operator != (ShortVec& a, ShortVec& b);

  virtual   void                  error(const char* msg) const;
  virtual   void                  range_error() const;

   // added by Anthony Tang (CUHK) for use by TOLKIEN

   // crossover operators
  virtual  void oddPtCrossover(const ShortVec&, const ShortVec&, int);
  virtual  void evenPtCrossover(const ShortVec&, const ShortVec&, int);
  virtual  void uniformCrossover(const ShortVec&, const ShortVec&, float);

  friend ostream & operator << (ostream &, const ShortVec &);
  const ShortVec&  operator +=(ShortVec & a);

  int   copySegment(int, ShortVec & a, int, int);



};

extern void default_ShortVec_error_handler(const char*);
extern one_arg_error_handler_t ShortVec_error_handler;

extern one_arg_error_handler_t
        set_ShortVec_error_handler(one_arg_error_handler_t f);


inline ShortVec::ShortVec()
{
  len = 0; s = 0;
}

inline ShortVec::ShortVec(int l)
{
  s = new short [len = l];
}


inline ShortVec::ShortVec(int l, short* d) :len(l), s(d) {}


inline ShortVec::~ShortVec()
{
  delete[len] s;
}


inline short& ShortVec::operator [] (int n) const
{
  if ((unsigned)n >= len)
    range_error();
  return s[n];
}

inline short& ShortVec::elem(int n)
{
  return s[n];
}


inline int ShortVec::capacity() const
{
  return len;
}



inline int operator != (ShortVec& a, ShortVec& b)
{
  return !(a == b);
}

#endif

