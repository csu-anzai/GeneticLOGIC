#ifndef GSTAGE_H
#define GSTAGE_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gstage.h: declaration of gstage classes

class gstage
{
private:
    gset* pset;
    gprops* pprops;
    glights* plights;
    glightmodel* plightmodel;
    cchar* name;
    Boolean drawlights;
    Boolean madecast;    // TRUE if allocated room for cast list
    Boolean madelights;  // TRUE if allocated room for lights list
    gcast* pcast; // temporary during timing tests???
public:
    void init()
    {
        pcast = NULL;
        pset = NULL;
        pprops = NULL;
        plights = NULL;
        plightmodel = NULL;
        name = NULL;
        drawlights = madecast = madelights = FALSE;
    }
    gstage() { init(); }
    gstage(cchar* n) { init(); name = n; }
    ~gstage()
    {
        if (madecast)   { pcast->clear();   delete pcast; }
        if (madelights) { plights->clear(); delete plights; }
    }
    void setset(gset* ps) { pset = ps; }
    void setset(gset&  s) { pset = &s; }
    void setprops(gprops* pp) { pprops = pp; }
    void setprops(gprops&  p) { pprops = &p; }
    void setcast(gcast*);
    void setcast(gcast& c) { setcast(&c); }
    void setlights(glights*);
    void setlights(glights& l) { setlights(&l); }
    void setdrawlights(cBoolean dl) { drawlights = dl; }
    void setlightmodel(glightmodel* plm) { plightmodel = plm; }
    void setlightmodel(glightmodel&  lm) { plightmodel = &lm; }
    void setcurrentcamera(gcamera* pcam)
        { if (pcast) pcast->setcurrentcamera(pcam); }
    void setcurrentcamera(gcamera&  cam)
        { if (pcast) pcast->setcurrentcamera( cam); }
    void clear();  // issues ->clear() for all associated lists
    cchar* getname() { return name; }
    virtual void draw();
    virtual void draw(const frustumXZ* fxz);
    virtual void print();
// The following are added mostly for some quick & dirty testing.
// Use the cast, set, props, and lights list plus the camera pointer
// for real applications.
    void addobject(const gobject* po);
    void addobject(const gobject&  o) { addobject(&o); }
    void removeobject(const gobject* po)
        { if (pcast) pcast->remove(pgobject(po)); }
    void removeobject(const gobject&  o)
        { if (pcast) pcast->remove(pgobject(&o)); }
    void addlight(const glight* pl);
    void addlight(const glight&  l) { addlight(&l); }
    void removelight(const glight* pl)
        { if (plights) plights->remove(pglight(pl)); }
    void removelight(const glight&  l)
        { if (plights) plights->remove(pglight(&l)); }
    gcast* cast() { return pcast; }
};

typedef gstage* pgstage;

// end of gstage.h

#endif GSTAGE_H
