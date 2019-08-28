//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TCHECKS_H )

#define __TCHECKS_H

#include <stdio.h>
#include <assert.h>

#if !defined( __DEBUG )
#define __DEBUG 2
#endif

#undef PRECONDITION

#if (__DEBUG < 1)
#define PRECONDITION(p)   ((void)0)
#else
#define PRECONDITION(p)   assert(p)
#endif

#undef CHECK

#if (__DEBUG < 2)
#define CHECK(p)    ((void)0)
#else
#define CHECK(p)    assert(p)
#endif

#endif  // __TCHECKS_H

