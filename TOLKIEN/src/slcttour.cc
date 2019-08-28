//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include "slcttour.h"
#include "trand.h"
#include "ranksb.h"

static  PCTypeInfo tournselectBases[] = { &SelectionScheme::infoObj, 0 };
const TypeInfo  TournSelect::infoObj("TournSelect", tournselectBases);

PCTGAObj TournSelect::select()
{
	PCTGAObj        pWinner, pObj;
	ranksb.random(players, players.capacity(), pPop->size());
	pWinner = (PCTGAObj) pPop->at(players[0]);
	for (register i = 1; i < players.capacity(); i++) {
	     pObj = (PCTGAObj) pPop->at(players[i]);
             if (pWinner->fitness() < pObj->fitness())
                 pWinner = pObj;
        }
        return pWinner;
}
