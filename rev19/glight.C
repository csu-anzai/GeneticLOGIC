#ifndef GLIGHT_C
#define GLIGHT_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// glight.C: implementation of graphic light classes


#include "basicincludes.h"
#include "graphics.h"


float glprops[6]; // for constructing the properties array to pass to lmdef

indexlist* glight::pindices;
indexlist* glightmodel::pindices;


glight::glight()
{
    if (pindices == NULL) pindices = new indexlist(1,65535);
    index = short((*pindices).getindex());
    if (index < 1)
        error(1,"Attempt to define more than 65535 light types");
    else
    {
#ifdef ATT_IRIX3.2
        lmdef(DEFLIGHT,index,0,NULL);
#else
        lmdef(DEFLIGHT,index,0,(const float[])NULL);
#endif
        name = NULL;
        dims = 3;
        ambient[0] = ambient[1] = ambient[2] = 0.0;
        col[0] = col[1] = col[2] = 1.0; col[3] = 0.4; // translucent for draw
        pos[0] = pos[1] = pos[3] = 0.0; pos[2] = 1.0;
        pattobj = NULL;
    }
}


void glight::settranslation(cfloat x, cfloat y, cfloat z, cfloat w)
{
    glprops[0] = POSITION;
    glprops[1] = x; glprops[2] = y; glprops[3] = z; glprops[4] = w;
    glprops[5] = LMNULL;
    lmdef(DEFLIGHT,index,6,glprops);
    pos[0] = x; pos[1] = y; pos[2] = z; pos[3] = w;
}


void glight::setcol(cfloat r, cfloat g, cfloat b)
{
    glprops[0] = LCOLOR;
    glprops[1] = r; glprops[2] = g; glprops[3] = b;
    glprops[4] = LMNULL;
    lmdef(DEFLIGHT,index,5,glprops);
    col[0] = r; col[1] = g; col[2] = b;
}


void glight::setdims(const int d)
{
    if ((d<1) || (d>3))
    {
        char errmsg[256],cdims[16];
        strcpy(errmsg,"dimensionality must 1, 2, or 3 (not ");
        sprintf(cdims,"%d\0",dims);
        strcat(errmsg,cdims);
        strcat(errmsg,")");
        error(1,errmsg);
    }
    else
        dims = d;
}


void glight::bind(cshort lightnum)
{
    if ((lightnum < 0) || (lightnum > MAXLIGHTS))
    {
        char errmsg[256],lnum[16];
        strcpy(errmsg, "Light number must be >= 0 and < MAXLIGHTS (not ");
        sprintf(lnum,"%d\0",lightnum);
        strcat(errmsg,lnum);
        strcat(errmsg,")");
        error(1,errmsg);
    }
    else
    {
        lmbind(LIGHT0+lightnum,index);  // 110# == LIGHT#
    }
}


void glight::draw()
{
    c4f(col);
    pushmatrix();
//    loadmatrix(identmat);
      position();
      drawunitcube(); // make this a sphere some day
    popmatrix();
}


void glight::use(cshort lightnum)
{
    c4f(col);
    pushmatrix();
//    loadmatrix(identmat);
      position();
      bind(lightnum);
    popmatrix();
}


void glight::print()
{
   cout << "For light name = " << name nl;
   cout << "  light dimensionality = " << dims nl;
   switch (dims)
   {
   case 1:
      cout << "  light position = " << pos[0] nl;
      break;
   case 2:
      cout << "  light position = " << pos[0] cms
                                       pos[1] nl;
      break;
   case 3:
      cout << "  light position = " << pos[0] cms
                                       pos[1] cms
                                       pos[2] nl;
      break;
   }
}


implement(gdlink,pglight)
implement(gdlist,pglight)


glightmodel::glightmodel()
{
    if (pindices == NULL) pindices = new indexlist(1,65535);
    index = short((*pindices).getindex());
    if (index < 1)
        error(1,"Attempt to define more than 65535 lighting models");
    else
    {
        ambient[0] = ambient[1] = ambient[2] = 0.2;
        localviewer = 0.0;
        attenuation[0] = 1.0; attenuation[1] = 0.0;
#ifdef ATT_IRIX3.2
        lmdef(DEFLMODEL,index,0,NULL);
#else
        lmdef(DEFLMODEL,index,0,(const float[])NULL);
#endif
    }
}


void glightmodel::setambient(const float r, const float g, const float b)
{
    glprops[0] = AMBIENT;
    glprops[1] = r; glprops[2] = g; glprops[3] = b;
    glprops[4] = LMNULL;
    lmdef(DEFLMODEL,index,5,glprops);
    ambient[0] = r; ambient[1] = g; ambient[2] = b;
}


void glightmodel::setlocalviewer(const Boolean lv)
{
    glprops[0] = LOCALVIEWER;
    if (lv) glprops[1] = 1.0; else glprops[1] = 0.0;
    glprops[2] = LMNULL;
    lmdef(DEFLMODEL,index,3,glprops);
    localviewer = glprops[1];
}


void glightmodel::setattenuation(const float fixed, const float variable)
{
    glprops[0] = ATTENUATION;
    glprops[1] = fixed;
    glprops[2] = variable;
    glprops[3] = LMNULL;
    lmdef(DEFLMODEL,index,4,glprops);
    attenuation[0] = fixed;
    attenuation[1] = variable;
}


void glightmodel::print()
{
    cout << "For lighting model number " << index << "...\n";
    cout << "  ambient color (r,g,b) = " << ambient[0] cms
                                            ambient[1] cms
                                            ambient[2] nl;
    cout << "  attenuation (fixed,variable) = " << attenuation[0] cms
                                                   attenuation[1] nl;
    cout << "  localviewer = " << (localviewer == 1.0) nl;
}


// end of glight.C

#endif GLIGHT_C
