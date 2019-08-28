//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __SCHEMATA_H )
#define     __SCHEMATA_H

#include "toarray.h"
#include "IntVec.h"
#include "tritstr.h"

class   SchemataStatistics
{
public:
        SchemataStatistics();

        virtual                 ~SchemataStatistics();
        virtual void            reset();
        virtual void            operator()(RCCollection);
        virtual BOOL            addSchema(const char *);
	virtual BOOL            addSchema(RCTritString);
        virtual RCTritString    getSchema(unsigned index) const;
	virtual int             getCount(unsigned index) const;
				operator RCTOArray() const;
                int             numSchemata() const;

        friend ostream & operator << ( ostream &, RCSchemataStatistics);

protected :
        TOArray schemata;
        IntVec  counts;
};

inline  SchemataStatistics::SchemataStatistics()
{
	schemata.ownsElements(owner);
}

inline SchemataStatistics::~SchemataStatistics()
{
}

inline void SchemataStatistics::reset()
{
        schemata.removeAll();
        counts.resize(0);
}

inline RCTritString SchemataStatistics::getSchema(unsigned index) const
{
	return * (PCTritString) schemata.at(index);
}

inline int SchemataStatistics::getCount(unsigned index) const
{
        return counts[index];
}

inline SchemataStatistics::operator RCTOArray() const
{
	return schemata;
}

inline  int  SchemataStatistics::numSchemata() const
{
        return schemata.size();
}

#endif

