#ifndef GMISC_C
#define GMISC_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gmisc.C: implementation of miscellaneous graphics classes


#include "basicincludes.h"
#include "graphics.h"


float ucube[8][3] = { {-0.5, -0.5, -0.5},
                      {-0.5, -0.5,  0.5},
                      {-0.5,  0.5, -0.5},
                      {-0.5,  0.5,  0.5},
                      { 0.5, -0.5, -0.5},
                      { 0.5, -0.5,  0.5},
                      { 0.5,  0.5, -0.5},
                      { 0.5,  0.5,  0.5} };


float identmat[4][4] = { {1.0, 0.0, 0.0, 0.0},
                         {0.0, 1.0, 0.0, 0.0},
                         {0.0, 0.0, 1.0, 0.0},
                         {0.0, 0.0, 0.0, 1.0} };


void golist::draw()
{
    gobject* pobj;
    this->reset();
    while (this->next(pobj))
        if (pobj != (gobject*)pcurrentcamera) pobj->draw();
}


void golist::draw(const frustumXZ* fxz)
{
    gobject* pobj;
//  cout << "Drawing with frustum(x0,z0,angmin,angmax) = "
//       << fxz->x0 cm fxz->z0 cm fxz->angmin cm fxz->angmax pnlf;
    this->reset();
    while (this->next(pobj))
    {
        if (pobj != (gobject*)pcurrentcamera)
        {
//          float* p = pobj->getposptr();
//          cout << "  object at (x,z) = (" << p[0] cm p[2] << ") ";
            if (fxz->inside(pobj->getposptr()))//object position inside frustum?
            {
//              cout << "IS     ";
                pobj->draw();
            }
//          else
//              cout << "is NOT ";
//          cout << "inside" nlf;
        }
    }
}


void golist::print()
{
    gobject* pobj;
    this->reset();
    while (this->next(pobj))
        pobj->print();
}


void gllist::draw()
{
    glight* plight;
    this->reset();
    while (this->next(plight))
        plight->draw();
}


void gllist::draw(const frustumXZ* fxz)
{
    glight* plight;
    this->reset();
    while (this->next(plight))
    {
        if (fxz->inside(plight->getposptr())) // object position inside frustum?
        {
            plight->draw();
        }
    }
}


void gllist::print()
{
    glight* plight;
    this->reset();
    while (this->next(plight))
        plight->print();
}


void gllist::use()
{
    glight* plight;
    short count = 0;
    this->reset();
    while (this->next(plight))
    {
        if (++count > MAXLIGHTS)
        {
            error(1,"Attempted to use more than MAXLIGHTS lights");
            break;
        }
        else
            plight->use(count-1);  // subtract 1 due to 0-based light numbering
    }
    for (short i = count; i < MAXLIGHTS; i++)
        lmbind(LIGHT0+i,0);  // make sure no old lights are left bound
}


void drawunitcube()
{
    bgnpolygon();
      v3f(ucube[0]); v3f(ucube[1]); v3f(ucube[3]); v3f(ucube[2]);
    endpolygon();
    bgnpolygon();
      v3f(ucube[0]); v3f(ucube[4]); v3f(ucube[5]); v3f(ucube[1]);
    endpolygon();
    bgnpolygon();
      v3f(ucube[4]); v3f(ucube[6]); v3f(ucube[7]); v3f(ucube[5]);
    endpolygon();
    bgnpolygon();
      v3f(ucube[2]); v3f(ucube[3]); v3f(ucube[7]); v3f(ucube[6]);
    endpolygon();
    bgnpolygon();
      v3f(ucube[5]); v3f(ucube[7]); v3f(ucube[3]); v3f(ucube[1]);
    endpolygon();
    bgnpolygon();
      v3f(ucube[0]); v3f(ucube[2]); v3f(ucube[6]); v3f(ucube[4]);
    endpolygon();
}


void frameunitcube()
{
    bgnline();
      v3f(ucube[0]); v3f(ucube[1]); v3f(ucube[3]); v3f(ucube[2]);
      v3f(ucube[0]); v3f(ucube[4]); v3f(ucube[6]); v3f(ucube[2]);
      v3f(ucube[3]); v3f(ucube[7]); v3f(ucube[6]); v3f(ucube[4]);
      v3f(ucube[5]); v3f(ucube[1]); v3f(ucube[5]); v3f(ucube[7]);
    endline();
}


void frustumXZ::set(cfloat x, cfloat z, cfloat ang, cfloat fov)
{
//  cout << "frustum being set with (x,z,ang,fov) = ("
//       << x cm z cm ang cm fov pnlf;
    x0 = x;
    z0 = z;
    angmin = fmod((ang - 0.5*fov)*DEGTORAD,TWOPI);
    if (fabs(angmin) > PI) angmin -= (angmin > 0.0) ? TWOPI : (-TWOPI);
    angmax = fmod((ang + 0.5*fov)*DEGTORAD,TWOPI);
    if (fabs(angmax) > PI) angmax -= (angmin > 0.0) ? TWOPI : (-TWOPI);
//  cout << "resulting frustum is (x0,z0,angmin,angmax) = ("
//       << x0 cm z0 cm angmin cm angmax pnlf;
}


void frustumXZ::set(cfloat x, cfloat z, cfloat ang, cfloat fov, cfloat rad)
{
//  cout << "frustum being set with (x,z,ang,fov,rad) = ("
//       << x cm z cm ang cm fov cm rad pnlf;
    float x1 = x + rad * sin(ang*DEGTORAD) / sin(fov*0.5*DEGTORAD);
    float z1 = z + rad * cos(ang*DEGTORAD) / sin(fov*0.5*DEGTORAD);
    set(x1, z1, ang, fov);
}


static long infrustum = 0;
static long outfrustum = 0;
int frustumXZ::inside(cfloat* p)
{
    float ang = atan2(x0-p[0],z0-p[2]);
//  cout << "(ang = " << ang << ") ";
    if (angmin < angmax)
    {
        if (ang < angmin)
        {
            outfrustum++;
            return 0;
        }
        else if (ang > angmax)
        {
            outfrustum++;
            return 0;
        }
        else
        {
            infrustum++;
            return 1;
        }
    }
    else
    {
        if (ang > angmin)
        {
            infrustum++;
            return 1;
        }
        else if (ang < angmax)
        {
            infrustum++;
            return 1;
        }
        else
        {
            outfrustum++;
            return 0;
        }
    }
}


// end of gmisc.C

#endif GMISC_C
