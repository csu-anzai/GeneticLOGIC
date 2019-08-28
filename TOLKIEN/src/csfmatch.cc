//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "csfmatch.h"
#include "tclsf.h"
#include "ttclsf.h"
#include "msglist.h"
#include "identhsh.h"

static  PCTypeInfo matchlistBases[] = { &TGAObj::infoObj, 0 };
const TypeInfo  MatchList::infoObj("MatchList", matchlistBases);

static  PCTypeInfo tmatchlistBases[] = { &MatchList::infoObj, 0 };
const TypeInfo  TMatchList::infoObj("TMatchList", tmatchlistBases);

static  PCTypeInfo ttmatchlistBases[] = { &MatchList::infoObj, 0 };
const TypeInfo  TTMatchList::infoObj("TTMatchList", ttmatchlistBases);

float TTMatchList::sumIntensities() const
//
//      a message's intensity will be added only once
//
{
	float	flBids = 0;

	CollectionIterator	iter(msgs1);
	IdentHashTable	htable;

	while ( iter ) {
               if ( htable.includes(*iter()) == NOOBJECT ) {
		    flBids += ((PCMessage) iter())->intensity();
		    htable.add(iter());
		}
                ++iter;
	}

        iter.restart(msgs2);
	while ( iter ) {
               if ( iter() != NOOBJECT ) // may be a negated condition
                   if ( htable.includes(*iter()) == NOOBJECT ) {
			flBids += ((PCMessage) iter())->intensity();
			htable.add(iter());
		   }
               ++iter;
	}

	return flBids;
}

void TMatchList::printOn(ostream & out) const
{
        MatchList::printOn(out);
        out << endl;
        CollectionIterator iter(msgs);
        while (iter) {
            if (iter()) // cater for negated conditions
                iter()->printOn(out);
            iter++;
        }
}

BOOL  TMatchList::isEqual( RCTObject obj ) const
{
        if ( isA() == obj.isA() ) {
             RTMatchList mlist = (RTMatchList) obj;
             if (pCls->isEqual(*mlist.pCls)) {
                    CollectionIterator p(msgs), q(mlist.msgs);
                    while (p && q) {
                        if (p() != NOOBJECT && q() != NOOBJECT ) {
                            if ( ! p()->isEqual(*q()) )
                                return FALSE;
                        }
                        else
                            return FALSE;
                        p++;
                        q++;
                    }
                    if ( ! (p || q) )  // both lists exhausted
			return TRUE;
                 }
	}

	return FALSE;
}

BOOL  TTMatchList::isEqual( RCTObject obj ) const
{
        if ( isA() == obj.isA() ) {
             RTTMatchList mlist = (RTTMatchList) obj;
             if (pCls->isEqual(*mlist.pCls))
		 if ( msgs1.isEqual(mlist.msgs1) ) {
		    CollectionIterator p(msgs2), q(mlist.msgs2);
		    while (p && q) {
                        if (p() != NOOBJECT && q() != NOOBJECT ) {
			    if ( ! p()->isEqual(*q()) )
				return FALSE;
			}
			else
			    return FALSE;
			p++;
			q++;
		    }

		    if ( ! (p || q) )  // both lists exhausted
			return TRUE;
		 }
	}

	return FALSE;
}

#if !defined ( __postfix_inc__ )
int     TMatchList::post(RMsgList msgList) const
{
        int       nCount = 0;

	CollectionIterator	iter(msgs);

	pCls->reset();    // reset counter

	while ( iter ) {
	      if (pCls->post(msgList, (PCMessage) iter(), NULL)) {
		  nCount++;
                  ++iter;
	      }
	      else
		  break;
	}
	return nCount;
}

int	TTMatchList::post(RMsgList msgList) const
{
	int       nCount = 0;
	CollectionIterator	iter1(msgs1);
	CollectionIterator	iter2(msgs2);
	pCls->reset();    // reset counter

	while (iter1 && iter2) {
	     if ( pCls->post(msgList,
			     (PCMessage) iter1(),
			     (PCMessage) iter2()) ) {
		  nCount++;
                  ++iter1;
                  ++iter2;
	      }
	      else
		  break;
	}
	return nCount;
}

float TMatchList::sumIntensities() const
{
	CollectionIterator 	iter(msgs);
	float	flBids = 0;

	while (iter) {
		flBids += ((PCMessage) iter())->intensity();
                ++iter;
	}

	return flBids;
}

#else
int     TMatchList::post(RMsgList msgList) const
{
	int       nCount = 0;

	CollectionIterator	iter(msgs);

	pCls->reset();    // reset counter

	while ( iter ) {
              if ( pCls->post(msgList, (PCMessage) iter++, NULL) )
		  nCount++;
	      else
		  break;
	}
	return nCount;
}

int	TTMatchList::post(RMsgList msgList) const
{
	int       nCount = 0;
	CollectionIterator	iter1(msgs1);
	CollectionIterator	iter2(msgs2);
	pCls->reset();    // reset counter

	while (iter1 && iter2) {
	     if ( pCls->post(msgList,
                             (PCMessage) iter1++,
                             (PCMessage) iter2++) )
		  nCount++;
	      else
		  break;
	}
	return nCount;
}

float TMatchList::sumIntensities() const
{
	CollectionIterator 	iter(msgs);
	float	flBids = 0;

        while (iter)
                flBids += ((PCMessage) iter++)->intensity();

	return flBids;
}

#endif
