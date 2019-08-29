#ifndef FOOD_C
#define FOOD_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// food.C - implementation of food classes


#include "basicincludes.h"
#include "graphics.h"
//#include "externglobals.h"
#include "food.h"


extern float size2energy;

void food::dump(ostream& out)
{
    out << myenergy nl;
    out << pos[0] sp pos[1] sp pos[2] nl;
}


void food::load(istream& in)
{
    in >> myenergy;
    in >> pos[0] >> pos[1] >> pos[2];

    initlen();
}


implement(gdlink,pfood)
implement(gdlist,pfood)


void fxsortedlist::add(pfood a)
{
    Boolean inserted = FALSE;
    pfood o;
    this->reset();
    while (this->next(o))
    {
        if ((a->x()-a->radius()) < (o->x()-o->radius()))
        {
            this->inserthere(a);
            inserted = TRUE;
            break;
        }
    }
    if (!inserted) this->append(a);
}


void fxsortedlist::sort()
{
// This technique assumes that the list is almost entirely sorted at the start
// Hopefully, with some reasonable frame-to-frame coherency, this will be true!
    gdlink(pfood) *savecurr;
    pfood o = NULL;
    pfood p = NULL;
    pfood b = NULL;
    this->reset();
    this->next(p);
    savecurr = curr;
    while (this->next(o))
    {
        if ((o->x()-o->radius()) < (p->x()-p->radius()))
        {
            gdlink(pfood)* link = this->unlink();  // at o, just unlink it
            curr = savecurr;  // back up to previous one directly
            while (this->prev(b)) // then use iterator to move back again
                if ((b->x()-b->radius()) < (o->x()-o->radius()))
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



// end of food.C

#endif FOOD_C
