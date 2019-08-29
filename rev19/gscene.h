#ifndef GSCENE_H
#define GSCENE_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gscene.h: declaration of gscene classes

class gscene
{
private:
    gcamera* pcamera;
    gstage* pstage;
    cchar* name;
    Boolean drawlights;
    Boolean camerafixed;
    Boolean madecamera;  // TRUE if allocated room for default camera
    void makecamera();
public:
    void init()
    {
        pcamera = NULL;
        name = NULL;
        drawlights = camerafixed = madecamera = FALSE;
    }
    gscene() { init(); }
    gscene(cchar* n) { init(); name = n; }
    ~gscene()
    {
        if (madecamera) { delete pcamera; }
    }
    void setdrawlights(cBoolean dl) { drawlights = dl; }
    void fixcamera(cBoolean);
    void setcamera(gcamera*);
    void setcamera(gcamera& c) { setcamera(&c); }
    void setstage(gstage* s) { pstage = s; }
    void setstage(gstage& c) { setstage(&c); }
    void clear();  // issues ->clear() to pstage
    cchar* getname() { return name; }
    virtual void draw();
    virtual void draw(const frustumXZ* fxz);
    virtual void print();
    Boolean perspectiveset()
    {
        if (pcamera)
            return pcamera->perspectiveset();
        else
            return FALSE;
    }
    void useperspective() { if (pcamera) pcamera->useperspective(); }
};

typedef gscene* pgscene;

// end of gscene.h

#endif GSCENE_H
