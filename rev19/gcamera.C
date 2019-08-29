#ifndef GCAMERA_C
#define GCAMERA_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gcamera.C: implementation of graphic camera classes


#include "basicincludes.h"
#include "graphics.h"

#ifdef TIMING
    extern void timebeg(short);
    extern void timeend(short);
#endif TIMING

// Orientation, like with gobjects, is handled by yaw, pitch, and roll
// (rotate-y, rotate-x, rotate-z) applied in that order.
// Also like gobjects, gcameras are assumed to begin life facing down the
// negative z-axis.

// One of the most common useages in PolyWorld will be cameras attached
// to "critter" objects.  Accordingly, the camera need only be declared,
// cam.attachto(critter) invoked once, and cam.use() should be invoked by
// the scene to which the camera is attached.  Cam.setfovy and possibly
// cam.setnear and cam.setfar may be used if the defaults are not adequate.
// Cam.settranslation(x,y,z) and cam.setrotation(yaw,pitch,roll) can be used
// to set the camera's position and rotation with respect to the
// object to which it is attached (or to global origin if it is not
// attached to an object).

// The other methods, setperspective, setlookat, etc. are provided for
// closer compatability with the Iris gl functions, but will probably
// not be used in PolyWorld.


void gcamera::init()
{
    myfovy = DEFAULTFOVY;
    aspect = DEFAULTASPECT; // should probably be 0.
    near = DEFAULTNEAR;
    far = DEFAULTFAR;
    pos[0] = pos[1] = 0.; pos[2] = 0.;
    fix[0] = fix[1] = fix[2] = 0.;
    angle[0] = 0.; angle[1] = angle[2] = 0.;
    usinglookat = FALSE;
    pattobj = NULL;
    name = NULL;
    perspectivefixed = FALSE;
    perspectiveinuse = FALSE;
}


void gcamera::use()
{
//  resetaspect(); // NEED TO HANDLE THIS AT THE TIME THE ASPECT IS CHANGED
//  setaspect(); // only changes aspect if it = 0
    updateperspective();  // will do nothing if (perspectivefixed) is true
    if (usinglookat)
    {
        uselookat();
    }
    else
    {
        rot(-angle[2],'z');  // -roll
        rot(-angle[1],'x');  // -pitch
        rot(-angle[0],'y');  // -yaw
        ::translate(-pos[0],-pos[1],-pos[2]);
        if (pattobj) pattobj->inverseposition();
    }
}


void gcamera::print()
{
    cout << "For the camera named \"" << name << "\"...\n";
    cout << "  fovy = " << myfovy nl;
    cout << "  aspect = " << aspect nl;
    cout << "  near = " << near nl;
    cout << "  far = " << far nl;
    cout << "  position (x,y,z) = (" << pos[0] cm pos[1] cm pos[2] pnl;
    if (usinglookat)
        cout << "  using lookat, not heading, (x,y,z) = ("
             << fix[0] cm fix[1] cm fix[2] pnl;
    else
        cout << "  using heading, not lookat, (yaw,pitch,roll) = ("
             << angle[0] cm angle[1] cm angle[2] pnl;
    if (pattobj)
    {
        long address = long(pattobj);
        const char* objname = pattobj->getname();
        cout << "  attached to object at " << address
             << " named: \"" << objname qnl;
    }
    else
        cout << "  not attached to any object" nl;
}


// end of gcamera.C

#endif GCAMERA_C
