#ifndef GCAMERA_H
#define GCAMERA_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gcamera.h: declarations of graphic camera classes


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


class gcamera : public gboxf  // graphical camera
{
private:
    float myfovy; // short
    float aspect;
    Coord near, far;  // float
    float fix[3]; // fixation point; lookat point
    Boolean usinglookat;
    const gobject* pattobj;
    Boolean perspectivefixed;
    Boolean perspectiveinuse;
public:
    void init();
    gcamera() { init(); }
    gcamera(cchar* n) : (n) { init(); }
    ~gcamera() { }
    void setfovy(cfloat fov) { myfovy = fov; }
    void fovy(cfloat fov) { myfovy = fov; }
    float getfovy() { return myfovy; }
    float fovy() { return myfovy; }
    void setnear(cfloat n) { near = n; }
    void setfar(cfloat f) { far = f; }
    void resetaspect()
    {
        lpt size; getsize(&size.x,&size.y);  // size of current window
        aspect = float(size.x) / float(size.y);
    }
    void setaspect() { if (!aspect) resetaspect(); }
    void setaspect(cfloat a) { aspect = a; }
// you'd better know what you're doing if you use translate(), rotate()
// or position()
// they are intended for use between pushmatrix()/popmatrix() pairs as below
    void use();
    virtual void print();
    void attachto(const gobject& gobj) { pattobj = &gobj; }
    void attachto(const gobject* pgobj) { pattobj = pgobj; }
// The following methods, along with setaspect(cfloat a) above, are provided for
// closer compatability with the Iris gl functions, but may not be used
// in PolyWorld.
    void setperspective(cfloat fov, cfloat a, cfloat n, cfloat f)
        { myfovy = fov; aspect = a; near = n; far = f; }
    void settwist(cfloat t) { angle[2] = t; }
    void useperspective()
    {
            ::perspective(Angle(myfovy*10.),aspect,near,far);
            perspectiveinuse = TRUE;
    }
    void updateperspective()
    {
        if (!perspectivefixed)
            useperspective();
    }
    void perspective(cfloat fov, cfloat a, cfloat n, cfloat f)
        { setperspective(fov,a,n,f); useperspective(); }
    void fixperspective(Boolean pf)
        { setaspect(); if (pf) useperspective(); perspectivefixed = pf; }
    Boolean perspectiveset() { return perspectiveinuse; }
    void setlookat(cfloat vx, cfloat vy, cfloat vz,
                   cfloat px, cfloat py, cfloat pz, cfloat t)
    {
        pos[0] = vx; pos[1] = vy; pos[2] = vz;
        fix[0] = px; fix[1] = py; fix[2] = pz;
        angle[2] = t;  // 'z' (should this be negated?)
        usinglookat = TRUE;
    }
    void setfix(cfloat x, cfloat y, cfloat z)
        { fix[0] = x; fix[1] = y; fix[2] = z; usinglookat = TRUE; }
    void uselookat()
    {
        ::lookat(pos[0],pos[1],pos[2],fix[0],fix[1],fix[2],Angle(angle[2]*10.));
    }
    void lookat(cfloat vx, cfloat vy, cfloat vz,
                cfloat px, cfloat py, cfloat pz, cfloat t)
        { setlookat(vx,vy,vz,px,py,pz,t); uselookat(); }
};


// end of gcamera.h

#endif GCAMERA_H
