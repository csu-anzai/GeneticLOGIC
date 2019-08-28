//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include        "schemata.h"
#include	"hpldbin.h"
#include	"dpldbin.h"
#include	"tritstr.h"

void    SchemataStatistics::operator()(RCCollection pop)
//
//	This function assumes that the individuals of pop
//	are of the same species
//
{
	CollectionIterator iter(schemata), popiter(pop);
	PTritString    pSchema;
	int index = 0;

	while ( iter ) {
		pSchema = (PTritString) iter();
		popiter.restart();
		counts[index] = 0;
		while ( popiter ) {
                        if (pSchema->matches(((PCBinIndividual) popiter())->operator RCBitString()))
			       counts[index] += 1;
			popiter++;
		}
		index++;
		iter++;
	}

	for (; index < counts.capacity(); index++)
	     counts[index] = 0;
}

BOOL    SchemataStatistics::addSchema(const char *szString)
{
	if ( schemata.add(new TritString(szString))) {
	     counts.resize(counts.capacity() + 1);
	     return TRUE;
	}
	else
	     return FALSE;
}

BOOL    SchemataStatistics::addSchema(RCTritString tstr)
{
        if ( schemata.add(new TritString(tstr))) {
	     counts.resize(counts.capacity() + 1);
	     return TRUE;
	}
	else
	     return FALSE;
}

ostream & operator << ( ostream & out, RCSchemataStatistics st)
{
        for (register i = 0, j = st.numSchemata(); i < j; i++)
	     out << st.getSchema(i) << " : " << st.getCount(i) << endl;

	return out;
}
