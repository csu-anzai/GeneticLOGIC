#ifndef GSTAGE_C
#define GSTAGE_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gstage.C: implementation of gstage classes


#include "basicincludes.h"
#include "graphics.h"


void gstage::setcast(gcast* pc)
{
    if (madecast) { pcast->clear(); delete pcast; madecast = FALSE; }
    pcast = pc;
}


void gstage::setlights(glights* pl)
{
    if (madelights) { plights->clear(); delete plights; madelights = FALSE; }
    if (pl)
    {
        if (pl->count() >= MAXLIGHTS)
        {
            char msg[256]; char num[16];
            strcpy(msg,"Can only use MAXLIGHTS of the ");
            sprintf(num,"%d\0",(pl->count())+1);
            strcat(msg,num);
            strcat(msg," lights in this list");
            error(0,msg);
        }
    }
    plights = pl;
}


void gstage::clear()
{
    if (plights) plights->clear();
    if (pset)    pset->clear();
    if (pprops)  pprops->clear();
    if (pcast)   pcast->clear();
}


void gstage::draw()
{
    if (plightmodel) plightmodel->use();
    if (plights)
    {
        if (drawlights)
            plights->draw();  // does position() and draw() for each
        plights->use();       // does position() and bind() for each
    }
    if (pset)    pset->draw();
    if (pprops)  pprops->draw();
    if (pcast)   pcast->draw();
}


void gstage::draw(const frustumXZ* fxz)
{
    if (plightmodel) plightmodel->use();
    if (plights)
    {
        if (drawlights)
            plights->draw(fxz);  // does position() and draw() for each
        plights->use();       // does position() and bind() for each
    }
    if (pset)    pset->draw(); // Kludge to let ground plane always be drawn
                               // However, some group immune to the frustum
                               // is a good idea.
    if (pprops)  pprops->draw(fxz);
    if (pcast)   pcast->draw(fxz);
}


void gstage::print()
{
    cout << "For the stage named \"" << name << "\"...\n";
    if (pcast)
    {
        cout << "* The cast at " << pcast << ":" nl;
        pcast->print();
    }
    else
        cout << "* There is no cast list" nl;
    if (pset)
    {
        cout << "* The set at " << pset << ":" nl;
        pset->print();
    }
    else
        cout << "* There is no set list" nl;
    if (pprops)
    {
        cout << "* The props at " << pprops << ":" nl;
        pprops->print();
    }
    else
        cout << "* There is no props list" nl;
    if (plights)
    {
        cout << "* The lights at " << plights << ":" nl;
        plights->print();
    }
    else
        cout << "* There is no lights list" nl;
    if (plightmodel)
    {
        cout << "* The lightmodel at " << plightmodel << ":" nl;
        plightmodel->print();
    }
    else
        cout << "* There is no lighting model" nl;
}


void gstage::addobject(const pgobject po)
{
    if (pcast)
        pcast->add(po);
    else
    {
        pcast = new gcast(po);
        if (pcast == 0) error(2,
            "Unable to allocate memory for a new cast; program terminating");
        madecast = TRUE;
    }
}


void gstage::addlight(const pglight pl)
{
    if (plights)
    {
        if (plights->count() >= MAXLIGHTS)
        {
            char msg[256]; char num[16];
            strcpy(msg,"Can only use MAXLIGHTS of the ");
            sprintf(num,"%d\0",(plights->count())+1);
            strcat(msg,num);
            strcat(msg," lights in this list");
            error(0,msg);
        }
        plights->add(pl);
    }
    else
    {
        plights = new glights(pl);
        if (plights == 0)
           errorwait(2,
           "Unable to allocate memory for new lights; program terminating");
        madelights = TRUE;
    }
}


// end of gstage.C

#endif GSTAGE_C
