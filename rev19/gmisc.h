#ifndef GMISC_H
#define GMISC_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gmisc.h: declaration of miscellaneous graphics classes


extern float ucube[8][3];
extern float identmat[4][4];

extern void drawunitcube();
extern void frameunitcube();

extern class gcamera;


class frustumXZ
{
public:
    frustumXZ() { }
    frustumXZ(cfloat x, cfloat z, cfloat ang, cfloat fov)
        { set(x, z, ang, fov); }
    frustumXZ(cfloat x, cfloat z, cfloat ang, cfloat fov, cfloat rad)
        { set(x, z, ang, fov, rad); }
    ~frustumXZ() { }
    int inside(cfloat* p);
    void set(cfloat x, cfloat z, cfloat ang, cfloat fov);
    void set(cfloat x, cfloat z, cfloat ang, cfloat fov, cfloat rad);
public:
    float x0;
    float z0;
    float angmin;
    float angmax;
};


// graphic objects list (adds draw to regular doubly linked list)

class golist : public gdlist(pgobject)
{
protected:
    const gcamera* pcurrentcamera;
public:
    golist() { pcurrentcamera = NULL; }
    golist(pgobject pgo) : (pgo) { pcurrentcamera = NULL; }
    ~golist() { }
    void setcurrentcamera(const gcamera* pcam) { pcurrentcamera = pcam; }
    void setcurrentcamera(const gcamera&  cam) { pcurrentcamera = &cam; }
    virtual void draw();
    virtual void draw(const frustumXZ* fxz);
    void print();
};


// graphic lights list (adds draw to regular doubly linked list)
class gllist : public gdlist(pglight)
{
public:
    gllist() { }
    gllist(pglight pl) : (pl) { }
    ~gllist() { }
    virtual void draw();
    virtual void draw(const frustumXZ* fxz);
    void use();
    void print();
};


// graphics cast (assumed to be mostly dynamic objects)

class gcast : public golist
{
public:
    gcast() { }
    gcast(pgobject pgo) : (pgo) { }
    ~gcast() { }
};


// graphics set (assumed to be mostly (all?) static objects)

class gset : public golist
{
public:
    gset() { }
    gset(pgobject pgo) : (pgo) { }
    ~gset() { }
};


// graphics props (assumed to be a mix of static & dynamic objects)

class gprops : public golist
{
public:
    gprops() { }
    gprops(pgobject pgo) : (pgo) { }
    ~gprops() { }
};


// graphics lights (assumed to be a mix of static & dynamic objects)

class glights : public gllist
{
public:
    glights() { }
    glights(pglight pl) : (pl) { }
    ~glights() { }
};


// end of gmisc.h

#endif GMISC_H
