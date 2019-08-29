#ifndef GSCENE_C
#define GSCENE_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gscene.C: implementation of gscene classes


#include "basicincludes.h"
#include "graphics.h"

#ifdef TIMING
    extern void timebeg(short);
    extern void timeend(short);
#endif TIMING

void gscene::makecamera()
{
#ifdef SAFETY2
    if (pcamera) // this really shouldn't happen
    {
        error(1,"gscene::makecamera called when camera already exists");
        return;
    }
#endif SAFETY2
    pcamera = new gcamera();  // just take defaults
    if (pcamera == 0) error(2,
        "Unable to allocate memory for a new camera; program terminating");
    madecamera = TRUE;
}


void gscene::fixcamera(cBoolean cf)
{
    if (cf)
    {
        if (pcamera)
            pcamera->use();
        else
            error(1,"can't fix a scene's camera when one has not been set");
    }
    camerafixed = cf;
}


void gscene::setcamera(gcamera* pc)
{
    if (madecamera) { delete pcamera; madecamera = FALSE; }
    pcamera = pc;
}


void gscene::clear()
{
    if (pstage) pstage->clear();
}


void gscene::draw()
{
    if (!pcamera) makecamera();
    pushmatrix();
#ifdef TIMING
    timebeg(21);
#endif TIMING
      if (!camerafixed) pcamera->use();
#ifdef TIMING
    timeend(21);
#endif TIMING
      if (pstage)
      {
#ifdef TIMING
    timebeg(22);
#endif TIMING
          pstage->setcurrentcamera(pcamera);
#ifdef TIMING
    timeend(22);
    timebeg(23);
#endif TIMING
          pstage->setdrawlights(drawlights);
#ifdef TIMING
    timeend(23);
    timebeg(24);
#endif TIMING
          pstage->draw();
#ifdef TIMING
    timeend(24);
#endif TIMING
      }
    popmatrix();
}


void gscene::draw(const frustumXZ* fxz)
{
    if (!pcamera) makecamera();
    pushmatrix();
#ifdef TIMING
    timebeg(21);
#endif TIMING
      if (!camerafixed) pcamera->use();
#ifdef TIMING
    timeend(21);
#endif TIMING
      if (pstage)
      {
#ifdef TIMING
    timebeg(22);
#endif TIMING
          pstage->setcurrentcamera(pcamera);
#ifdef TIMING
    timeend(22);
    timebeg(23);
#endif TIMING
          pstage->setdrawlights(drawlights);
#ifdef TIMING
    timeend(23);
    timebeg(24);
#endif TIMING
          pstage->draw(fxz);
#ifdef TIMING
    timeend(24);
#endif TIMING
      }
    popmatrix();
}


void gscene::print()
{
    cout << "For the scene named \"" << name << "\"...\n";
    if (pcamera)
    {
        cout << "**The camera at " << pcamera << ":" nl;
        pcamera->print();
        cout << "**The camera is ";
        if (!camerafixed) cout << "not ";
        cout << "fixed" nl;
    }
    else
        cout << "**There is no camera" nl;
    if (pstage)
    {
        cout << "**The stage at " << pstage << ":" nl;
        pstage->print();
    }
    else
        cout << "**There is no stage" nl;
    cout << "**The lights in this scene will ";
    if (!drawlights) cout << "not ";
    cout << "be drawn" nl;
}


// end of gscene.C

#endif GSCENE_C
