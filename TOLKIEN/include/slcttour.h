//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined(__SLCTTOUR_H)
#define __SLCTTOUR_H

#include "select.h"
#include "set.h"
#include "IntVec.h"

class  TournSelect : public SelectionScheme
{
public:
				DECLARE_RTTI()
				TournSelect(int = 2);
				TournSelect(RCCollection, int = 2);
	virtual                 ~TournSelect();

				_shallowCopy(TournSelect)
	virtual void            printOn( ostream  & out) const;

	virtual PCTGAObj        select();
	virtual void            setTournSize(unsigned int );
        virtual void            reset(RCCollection);

protected:
        IntVec  players;
};

inline  TournSelect::TournSelect(int size) :
        SelectionScheme(), players(size)
{
}

inline  TournSelect::TournSelect(RCCollection src, int size) :
        SelectionScheme(src),
        players((sizeType) rangeCheck(size,2,src.size()/2.0,2))
{
}

inline  TournSelect::~TournSelect()
{
}

inline void    TournSelect::printOn( ostream  & out) const
{
	out << nameOf() << " : " << players.capacity() << endl << *pPop;
}

inline  void TournSelect::setTournSize(unsigned int nNewSize)
{
	if (pPop)
	    players.resize((sizeType) rangeCheck(nNewSize,2,pPop->size()/2.0,2));
	else
	    players.resize(2);
}

inline  void TournSelect::reset(RCCollection src)
{
	SelectionScheme::reset(src);
	players.resize((sizeType) rangeCheck(players.capacity(),2,src.size()/2.0,2));
}

#endif


