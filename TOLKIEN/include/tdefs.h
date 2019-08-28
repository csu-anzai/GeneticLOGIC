//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __TDEFS_H )
#define __TDEFS_H

#include <stdlib.h>
#include "tvalues.h"
#include "tchecks.h"

typedef unsigned char  BYTE;   // 8-bit unsigned entity
typedef unsigned short WORD;   // 16-bit unsigned number
typedef unsigned int   UINT;   // machine sized unsigned number (preferred)
typedef long           LONG;   // 32-bit signed number
typedef unsigned long DWORD;   // 32-bit unsigned number
typedef int            BOOL;   // BOOLean (0 or !=0)
typedef unsigned int    classType;
typedef unsigned int    hashValueType;
typedef long            sizeType;


#if !defined( PASCAL )
#define PASCAL pascal
#endif

#if defined(__BORLANDC__)
#if defined(__LARGE__) || defined(__HUGE__)
#define T_FAR __far
#else
#define T_FAR
#endif
#else
#define T_FAR
#endif

#if defined(__BORLANDC__)
#define __postfix_inc__
#endif

#if !defined(_CLASSDEF)
#if defined(_MSC_VER) || defined(__BORLANDC__) || defined(__GNUG__)
#define _PTRDEF(name) typedef name T_FAR * P##name;
#define _REFDEF(name) typedef name T_FAR & R##name;
#define _REFPTRDEF(name) typedef name T_FAR * T_FAR & RP##name;
#define _PTRCONSTDEF(name) typedef const name T_FAR * PC##name;
#define _REFCONSTDEF(name) typedef const name T_FAR & RC##name;
#else
#define _PTRDEF(name) typedef name T_FAR * P/**/name;
#define _REFDEF(name) typedef name T_FAR & R/**/name;
#define _REFPTRDEF(name) typedef name T_FAR * T_FAR & RP/**/name;
#define _PTRCONSTDEF(name) typedef const name T_FAR * PC/**/name;
#define _REFCONSTDEF(name) typedef const name T_FAR & RC/**/name;
#endif

#define _CLASSDEF(name) class name; \
	_PTRDEF(name) \
	_REFDEF(name) \
	_REFPTRDEF(name) \
	_PTRCONSTDEF(name) \
	_REFCONSTDEF(name)
#endif

#if defined(_MSC_VER) || defined(__BORLANDC__) || defined(__GNUG__)
#define _isA(name)       PCTypeInfo isA() const { return &infoObj; }
#define _nameOf(class)    char  *nameOf() const { return #class; }
#define _error() \
	void error(const char  *msg) const \
		{ (*lib_error_handler)(nameOf(),msg); }
#else
#define _isA(name)       PCTypeInfo isA() const { return &infoObj; }
#define _nameOf(class)    char  *nameOf() const { return "class"; }
#define _error() \
	void error(const char  *msg) const \
		{ (*lib_error_handler)(nameOf(),msg); }
#endif

#define _shallowCopy(name) \
    PTObject shallowCopy() const { return new name(*this); }

_CLASSDEF(Assoc)
_CLASSDEF(Atom)
_CLASSDEF(AtomBase)
_CLASSDEF(BinDiploid)
_CLASSDEF(BinHaploid)
_CLASSDEF(BinIndividual)
_CLASSDEF(BitString)
_CLASSDEF(Chromosome)
_CLASSDEF(Classifier)
_CLASSDEF(ClassifierSystem)
_CLASSDEF(Collection)
_CLASSDEF(CollectionIterator)
_CLASSDEF(Crossover)
_CLASSDEF(CrowdingPopulation)
_CLASSDEF(Dictionary)
_CLASSDEF(Diploid)
_CLASSDEF(ElitePopulation)
_CLASSDEF(EvenPtCrossover)
_CLASSDEF(GeneticAlgorithm)
_CLASSDEF(GrayCode)
_CLASSDEF(Haploid)
_CLASSDEF(HashSet)
_CLASSDEF(HashTable)
_CLASSDEF(HashTableIterator)
_CLASSDEF(IdentDict)
_CLASSDEF(IdentHashTable)
_CLASSDEF(Individual)
_CLASSDEF(Iterator)
_CLASSDEF(LinearScaling)
_CLASSDEF(LinearRanking)
_CLASSDEF(LookupKey)
_CLASSDEF(MatchList)
_CLASSDEF(Message)
_CLASSDEF(Min2MaxScaling)
_CLASSDEF(MsgList)
_CLASSDEF(MultiPtCrossover)
_CLASSDEF(NonNegScaling)
_CLASSDEF(OddPtCrossover)
_CLASSDEF(Population)
_CLASSDEF(PtCrossover)
_CLASSDEF(RandSequence)
_CLASSDEF(RW_Select)
_CLASSDEF(RWwoR_Select)
_CLASSDEF(ScalingScheme)
_CLASSDEF(SchemataStatistics)
_CLASSDEF(SelectionScheme)
_CLASSDEF(Set)
_CLASSDEF(SigmaScaling)
_CLASSDEF(SimplePopulation)
_CLASSDEF(SLQueue)
_CLASSDEF(SLQueueIterator)
_CLASSDEF(SortedCltn)
_CLASSDEF(SubArray)
_CLASSDEF(SUS_Select)
_CLASSDEF(TChar)
_CLASSDEF(TClassifier)
_CLASSDEF(TDouble)
_CLASSDEF(TGAObj)
_CLASSDEF(TInteger)
_CLASSDEF(TLong)
_CLASSDEF(TMatchList)
_CLASSDEF(TOArray)
_CLASSDEF(TOArrayIterator)
_CLASSDEF(TObject)
_CLASSDEF(TObjDLList)
_CLASSDEF(TObjDLListIterator)
_CLASSDEF(TObjSLList)
_CLASSDEF(TObjSLListIterator)
_CLASSDEF(TournSelect)
_CLASSDEF(TRandom)
_CLASSDEF(TritString)
_CLASSDEF(TTClassifier)
_CLASSDEF(TTMatchList)
_CLASSDEF(TypeInfo)
_CLASSDEF(UniformCrossover)

#define ASSERT(f)          assert(f)

#ifndef RAND_MAX
#define RAND_MAX	(2.0E+31 - 1)
#endif

//
// HASH_MAX is dependent on the word size of the computer system
// on IBMPC it is assumed that far address is being used
//
#if defined(_MSC_VER) || defined(__BORLANDC__)
#define HASH_MAX        MAXLONG
#else
#define HASH_MAX        MAXINT
#endif

#define HAPLOID                 0
#define DIPLOID                 1

#define DEFAULT_NUMBITS         8

#define MAX_POPSIZE             MAXINT
#define MAX_CHROMLEN            MAXINT
#define MAX_XOVERATE            1.0
#define MAX_MUTATRATE           1.0
#define MAX_GENERATIONS         MAXINT
#define MAX_INDLEN              MAXINT

#define DEFAULT_COLLECTION_SIZE 16
#define DEFAULT_GROW_SIZE       16
#define DEFAULT_CHROMLEN	32
#define DEFAULT_INDLEN		DEFAULT_CHROMLEN
#define DEFAULT_XOVERATE        0.6
#define DEFAULT_MUTATRATE       0.0003

#define RANDOMIZE               1
#define DEFAULT_MUTDC           (1 / 3.0)

#define CSF_STRENGTH_MAX        100000
#define CSF_STRENGTH_MIN        0
#define PRD_TAX_MAX             100

#ifndef TRUE
#define	TRUE (1)
#endif

#ifndef FALSE
#define	FALSE (0)
#endif

#endif

