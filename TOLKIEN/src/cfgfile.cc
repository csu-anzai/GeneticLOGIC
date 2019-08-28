//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "cfgfile.h"

char*	ConfigFile::seek(char* szName)
{
        char* p;

	filein.seekg(0);
        while (! filein.eof()) {
            filein.getline(buffer, 200,'\n');
            if (buffer[0] == '[')
                if ((p = strstr(buffer+1,szName)) != NULL)
                    return p;
        }
        return NULL;
}


BOOL	ConfigFile::read(char* szName, TDouble& dbl)
{
	float val;
	if (seek(szName)) {
	    filein.getline(buffer, 200,'\n');
	    if (buffer[0] == ';') // this is a comment line
	       do {
		    filein.getline(buffer, 200,'\n');
	       } while (buffer[0] == ';' && !filein.eof());
	    if (buffer[0] != ';') {
		sscanf(buffer,"%f", &val);
		dbl = val;
		return TRUE;
	    }
	}
	return FALSE;
}

BOOL	ConfigFile::read(char* szName,TInteger& integer)
{
	int val;
	if (seek(szName)) {
	    filein.getline(buffer, 200,'\n');
	    if (buffer[0] == ';') // this is a comment line
	       do {
		    filein.getline(buffer, 200,'\n');
	       } while (buffer[0] == ';' && !filein.eof());
	    if (buffer[0] != ';') {
		sscanf(buffer,"%d", &val);
		integer = val;
		return TRUE;
	    }
	}
	return FALSE;
}

BOOL	ConfigFile::read(char* szName,TLong& tlong)
{
	long val;
	if (seek(szName)) {
	    filein.getline(buffer, 200,'\n');
	    if (buffer[0] == ';') // this is a comment line
	       do {
		    filein.getline(buffer, 200,'\n');
	       } while (buffer[0] == ';' && !filein.eof());
	    if (buffer[0] != ';') {
		sscanf(buffer,"%I", &val);
		tlong = val;
		return TRUE;
	    }
	}
	return FALSE;
}

BOOL	ConfigFile::read(char* szName, char* szString)
{
	if (seek(szName)) {
	    filein.getline(buffer, 200,'\n');
	    if (buffer[0] == ';') // this is a comment line
	       do {
		    filein.getline(buffer, 200,'\n');
	       } while (buffer[0] == ';' && !filein.eof());
	    if (buffer[0] != ';') {
		strcpy(szString,buffer);
		return TRUE;
	    }
	}
	return FALSE;
}
