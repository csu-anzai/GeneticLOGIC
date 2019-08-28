//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined ( __CFGFILE_H )
#define        __CFGFILE_H

#include "atom.h"
#include <iostream.h>
#include <fstream.h>

class 	ConfigFile
{
public:
			ConfigFile( istream& );

        virtual BOOL    read(char*,TDouble&);
        virtual BOOL    read(char*,TInteger&);
        virtual BOOL    read(char*,TLong&);
	virtual BOOL	read(char*,char*);

protected:
	istream& 	filein;
	char  		buffer[200];

	char*		seek(char*);

};

inline	ConfigFile::ConfigFile(istream& in) : filein(in)
{
}

#endif

