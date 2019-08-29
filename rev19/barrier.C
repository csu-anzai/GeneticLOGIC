#ifndef BARRIER_C
#define BARRIER_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// barrier.C - implementation of barrier classes


#include "basicincludes.h"
#include "graphics.h"
//#include "externglobals.h"
#include "barrier.h"


extern float size2energy;


implement(gdlink,pbarrier)
implement(gdlist,pbarrier)


void bxsortedlist::add(pbarrier a)
{
    Boolean inserted = FALSE;
    pbarrier o;
    this->reset();
    while (this->next(o))
    {
        if (a->xmin() < o->xmin())
        {
            this->inserthere(a);
            inserted = TRUE;
            break;
        }
    }
    if (!inserted) this->append(a);
}


void bxsortedlist::sort()
{
// This technique assumes that the list is almost entirely sorted at the start
// Hopefully, with some reasonable frame-to-frame coherency, this will be true!
// Actually, barriers are expected to be static.
    gdlink(pbarrier) *savecurr;
    pbarrier o = NULL;
    pbarrier p = NULL;
    pbarrier b = NULL;
    this->reset();
    this->next(p);
    savecurr = curr;
    while (this->next(o))
    {
        if (o->xmin() < p->xmin())
        {
            gdlink(pbarrier)* link = this->unlink();  // at o, just unlink it
            curr = savecurr;  // back up to previous one directly
            while (this->prev(b)) // then use iterator to move back again
                if (b->xmin() < o->xmin())
                    break; // until we have one that starts before o
            if (curr)  // we did find one, and didn't run off beginning of list
                this->appendhere(link);
            else  // we have a new head of the list
                this->insert(link);
            curr = savecurr;
            o = p;
        }
        p = o;
        savecurr = curr;
    }
}


// end of barrier.C

#endif BARRIER_C
