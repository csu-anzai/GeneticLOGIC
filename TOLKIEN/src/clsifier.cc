//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "clsifier.h"

static  PCTypeInfo classifierBases[] = { &TGAObj::infoObj, 0 };
const TypeInfo  Classifier::infoObj("Classifier", classifierBases);
