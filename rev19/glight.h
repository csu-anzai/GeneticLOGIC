#ifndef GLIGHT_H
#define GLIGHT_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// glight.h: declarations of graphic light classes


class glight // graphical light
{
private:
    static indexlist* pindices;
    short index; // index by which the iris will know this light definition
    float pos[4];
    float col[4]; // only 3 apply to lighting, 4th is for translucent draw
    float ambient[3]; // not really needed except for print
    gobject* pattobj;
    cchar* name;
    int dims;
public:
    glight();
    virtual ~glight() { (*pindices).freeindex(index); }
    void settranslation(cfloat x, cfloat y, cfloat z, cfloat w);
    void settranslation(cfloat x, cfloat y = 0.0, cfloat z = 0.0)
        { settranslation(x,y,z,pos[3]); }
    void settranslation(cfloat* pp) { settranslation(pp[0],pp[1],pp[2],pos[3]); }
    void setcol(cfloat r, cfloat g, cfloat b);
    void setcol(cfloat* pcol) { setcol(pcol[0],pcol[1],pcol[2]); }
    void setambient(cfloat r, cfloat g, cfloat b);
    void setname(cchar* pc) { name = pc; }
    void setdims(cint d);
    void bind(cshort lightnum); // 0 <= lightnum <= MAXLIGHTS
    void use(cshort lightnum);
    virtual void draw();
    virtual void print();
    void translate() { ::translate(pos[0],pos[1],pos[2]); }
    void position() { if (pattobj) pattobj->position(); translate(); }
    float* getposptr() { return &pos[0]; }
};


typedef glight* pglight;

declare(gdlink,pglight)
declare(gdlist,pglight)


class glightmodel // graphical lighting model
{
private:
    static indexlist* pindices;
    short index; // index by which the iris will know this lighting-model
    float ambient[3]; // not really needed except for print
    float localviewer; // not really needed except for print
    float attenuation[2]; // not really needed except for print
    cchar* name;
public:
    glightmodel();
    ~glightmodel() { (*pindices).freeindex(index); }
    void setambient(cfloat r, cfloat g, cfloat b);
    void setlocalviewer(cBoolean lv);
    void setattenuation(cfloat fixed, cfloat variable);
    void setname(cchar* pc) { name = pc; }
    void bind() { lmbind(LMODEL,index); }
    void use() { bind(); }  // synonym
    void print();
};


// end of glight.h

#endif GLIGHT_H
