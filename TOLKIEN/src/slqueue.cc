//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "slqueue.h"

static  PCTypeInfo slqueueBases[] = { &Collection::infoObj, 0 };
const TypeInfo  SLQueue::infoObj("SLQueue", slqueueBases);
