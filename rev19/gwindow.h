#ifndef GWINDOW_H
#define GWINDOW_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gwindow.h: declaration of gwindow classes

// following "SAFETYn" flags can be undefined once code is 'fully' checked out
#define SAFETY1
//#define RESTOREWINDOW
//#define RESTORECOLOR


class gwindow // gwindow
{
// ?? friend eventloop;
protected:
    static long windowsever;
    static long windowsactive;
    static long windowseveropened;
    static long windowsopen;
    lpt minsiz;
    Boolean minsizeset;
    lpt maxsiz;
    Boolean maxsizeset;
    lpt aspect;
    Boolean keepaspectset;
    lpt prefsiz;
    Boolean prefsizeset;
    lpt prefpos1;
    lpt prefpos2;
    Boolean prefposset;
    Boolean border;
    short framewidth;  // set to 0 to turn off frame
    cchar* name;
    long id;
    Boolean zbuffered;
    char drawbuffer;  // 'f' = front, 'b' = back, anything else = default
    gscene* pwinscene;
    colf bg;  // background color
    colf frm;  // frame color
    long saveid;
    cols savecolor;
    Boolean visible;
    Boolean titlebar;  // (THIS DOESN'T WORK)
    void applyconstraints();
    long isopen(const char* pc)
    {
        if (!id)
            error(1,"attempt to ",pc," an unopened window (\"",name,"\")");
        return id;
    }
    void drawframe();
    void adjustscrmask();
    void adjustscrmask(long, long);
    void resetscrmask();
    Boolean perspectiveset()
    {
        if (pwinscene)
            return pwinscene->perspectiveset();
        else
            return FALSE;
    }
    inline void justdraw();
    inline void justdraw(const frustumXZ* fxz);
public:
    Boolean doublebuffered;
    void init();
    gwindow() { init(); }
    gwindow(cchar* n) { init(); name = n; }
    virtual ~gwindow() { close(); windowsactive--; }
    virtual void print();
    void open();
    long isopen() { return id; }
    virtual void close() { windowsopen--; if (id) winclose(id); id = 0; }
    void makecurrentwindow()
    {
#ifdef RESTOREWINDOW
        saveid = winget();
#endif RESTOREWINDOW
        winset(id);
    }
    void restorecurrentwindow()
    {
#ifdef RESTOREWINDOW
        winset(saveid);
#endif RESTOREWINDOW
    }
    void makecurrentcolor()
    {
#ifdef RESTORECOLOR
        gRGBcolor(&savecolor.r,&savecolor.g,&savecolor.b);
#endif RESTORECOLOR
        c3f(&bg.r);
    }
    void makecurrentcolor(cfloat* c)
    {
#ifdef RESTORECOLOR
        gRGBcolor(&savecolor.r,&savecolor.g,&savecolor.b);
#endif RESTORECOLOR
        c3f(c);
    }
    void makecurrentcolor(cfloat r, cfloat g, cfloat b)
    {
#ifdef RESTORECOLOR
        gRGBcolor(&savecolor.r,&savecolor.g,&savecolor.b);
#endif RESTORECOLOR
        RGBcolor(scol(r),scol(g),scol(b));
    }
    void restorecurrentcolor()
    {
#ifdef RESTORECOLOR
        RGBcolor(savecolor.r,savecolor.g,savecolor.b);
#endif RESTORECOLOR
    }
    void setminsize(clong x, clong y);
    void setmaxsize(clong x, clong y);
    void setkeepaspect(clong x, clong y);
    void setprefsize(clong x, clong y);
    void setprefposition(clong x1, clong x2,
                         clong y1, clong y2);
    void setborder(cBoolean);
    void setframewidth(cshort);
    void setframecolor(cfloat*);
    void setframecolor(cfloat, cfloat, cfloat);
    void settitle(cchar* pwintitle);
    cchar* gettitle() { return name; }
    cchar* getname() { return name; }
    void setposition(clong x1, clong x2,
                     clong y1, clong y2);
    void move(clong x, clong y);
    void setorigin(clong x, clong y) { move(x,y); }
    long getid() { return id; }
    void getsize(long* width, long* height) { ::getsize(width,height); }
    void setscene(gscene* ps) { pwinscene = ps; }
    void setscene(gscene&  s) { pwinscene = &s; }
    void setzbuffer(cBoolean);
    void setdoublebuffer(cBoolean);
    void setdrawbuffer(cchar);  // 'f' = front, 'b' = back, else = default
    void setcolor(cfloat r, cfloat g, cfloat b)
        { bg.r = r; bg.g = g; bg.b = b; }
    virtual void clearc();
    void clearcs();
    virtual void doubleclearc();
    void doubleclearcz();
    virtual void draw();
    virtual void draw(const frustumXZ* fxz);
    virtual void drawnoswap();
    virtual void drawnoswap(const frustumXZ* fxz);
    virtual void pop();
    void push();
    void setvisibility(cBoolean);
    void settitlebar(cBoolean); // (THIS DOESN'T WORK)
    virtual void dumbtext(cchar*);
    void swapbuffers();
    void justswapbuffers()
    {
//      cout << "About to swap buffers" nlf;
//      sleep(1);
        ::swapbuffers();
//      cout << "Buffer swapped" nlf;
//      sleep(5);
    }
    virtual void refresh();
// only call justviewport & useperspective when the intended window
// is already current
    void justviewport(cshort xleft, cshort xright, cshort ybottom, cshort ytop)
        { ::viewport(xleft,xright,ybottom,ytop); }
    void useperspective() { if (pwinscene) pwinscene->useperspective(); }
};


typedef gwindow* pgwindow;

declare(gdlink,pgwindow)
declare(gdlist,pgwindow)


class gscreen : public gwindow
{
private:
    gdlist(pgwindow) winlist;
    void init0();
    void init();
    void init(cint width, cint height);
public:
    gscreen() { init(); }
    gscreen(cchar* n) : (n) { init(); }
    gscreen(cint width, cint height) { init(width,height); }
    gscreen(cchar* n, cint width, cint height) : (n) { init(width,height); }
    virtual ~gscreen() { winlist.clear(); }
    void clear() { winlist.clear(); }
    virtual void addwindow(const gwindow* pw)
        { winlist.append(pgwindow(pw)); }
    virtual void addwindow(const gwindow&  w)
        { winlist.append(pgwindow(&w)); }
    virtual void removewindow(const gwindow* pw)
        { winlist.remove(pgwindow(pw)); }
    virtual void removewindow(const gwindow&  w)
        { winlist.remove(pgwindow(&w)); }
    virtual void draw()
    {
        if (isopen())
        {
//          gwindow::draw();
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
            {
                pwin->draw();
            }
        }
    }
    virtual void drawnoswap()
    {
        if (isopen())
        {
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
                pwin->drawnoswap();
        }
    }
    virtual void print()
    {
        gwindow* pwin;
        winlist.reset();
        while (winlist.next(pwin))
            pwin->print();
    }
    virtual void clearc()
    {
        if (isopen())
        {
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
                pwin->clearc();
        }
    }
    virtual void doubleclearc()
    {
        if (isopen())
        {
            gwindow::doubleclearc();
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
                pwin->doubleclearc();
        }
    }
    virtual void pop()
    {
        if (isopen())
        {
            gwindow::pop();
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
                pwin->pop();
        }
    }
    virtual void refresh()
    {
        if (isopen())
        {
            makecurrentwindow();
            gwindow::pop();
            gwindow::doubleclearc();
            gwindow* pwin;
            winlist.reset();
            while (winlist.next(pwin))
                pwin->refresh();
        }
    }
};


class debugwindow : public gwindow
{
protected:
    char type;  // type of variable being displayed
    void* p;  // pointer to variable to be displayed
    char c[256];  // character array to hold converted variable for display
    void init()
    {
        c[0]='\0';
        setzbuffer(FALSE);
        setdoublebuffer(FALSE);
        setframewidth(0);
        setborder(TRUE);
        setprefsize(130,36);
    }
public:
    debugwindow() { init(); }
    debugwindow(cchar* n) : (n) { init(); }
    ~debugwindow() { }
    void set(cchar* v) { type = 'c'; p = (void*)  v; }
    void set(cchar& v) { type = 'c'; p = (void*) &v; }
    void set(cfloat* v) { type = 'f'; p = (void*)  v; }
    void set(cfloat& v) { type = 'f'; p = (void*) &v; }
    void set(clong* v) { type = 'l'; p = (void*)  v; }
    void set(clong& v) { type = 'l'; p = (void*) &v; }
    void set(cint* v) { type = 'i'; p = (void*)  v; }
    void set(cint& v) { type = 'i'; p = (void*) &v; }
    void set(cshort* v) { type = 's'; p = (void*)  v; }
    void set(cshort& v) { type = 's'; p = (void*) &v; }
    void setp(clong* v) { type = 'p'; p = (void*)  v; } // pointer
    void setp(clong& v) { type = 'p'; p = (void*) &v; } // pointer
    virtual void draw()
    {
        gwindow::draw();
        cchar* pc = c;  // by default display our protected character array
        switch (type)
        {
        case 'c':
            pc = (cchar*)p;  // it's a character array, so just display it
            break;
        case 'f':
            sprintf(c,"%g\0",*((float*)p));
            break;
        case 'l':
            sprintf(c,"%d\0",*((long*)p));
            break;
        case 'i':
            sprintf(c,"%d\0",*((int*)p));
            break;
        case 's':
            sprintf(c,"%d\0",*((short*)p));
            break;
        case 'p':
            sprintf(c,"%x\0",*((long*)p));
            break;
        }
//      cout << "in debugwindow " << name << " pc = " << pc nlf;
        dumbtext(pc);
    }
    void printval()
    {
        switch (type)
        {
        case 'c':
            cout << "character array " << name << " = " << (cchar*)p nlf;
            break;
        case 'f':
            cout << "float " << name << " = " << *((float*)p) nlf;
            break;
        case 'l':
            cout << "long " << name << " = " << *((long*)p) nlf;
            break;
        case 'i':
            cout << "int " << name << " = " << *((int*)p) nlf;
            break;
        case 's':
            cout << "short " << name << " = " << *((short*)p) nlf;
            break;
        case 'p':
            cout << "pointer " << name << " = " << p nlf;
            break;
        }
    }
    void print() { gwindow::print(); printval(); }
};


class nodrawwindow : public gwindow
{
public:
    nodrawwindow() { }
    nodrawwindow(cchar* n) : (n) { }
    ~nodrawwindow() { }
    virtual void draw() { }
    virtual void draw(const frustumXZ* fxz) { }
};


class chartwindow : public nodrawwindow
{
protected:
    long *y;  // dynamically allocated memory for storing plotted values
    long *numpoints;
    long maxpoints;
    short numcurves;
    float *vlo;
    float *vhi;
    long ylo;
    long yhi;
    long xlo;
    long xhi;
    long ytitle;
    float *dydv;
    colf *color;
    short decimation;
    void init(short ncurves);
    void terminate();
    void drawaxes();
    virtual void plotpoints();
    void plotpoint(cshort ic, clong x, clong y);
    void plotpoint(clong x, clong y) { plotpoint(0,x,y); }
public:
    chartwindow() { init(1); }
    chartwindow(cchar* n) : (n) { init(1); }
    chartwindow(short ncurves) { init(ncurves); }
    chartwindow(cchar* n, short ncurves) : (n) { init(ncurves); }
    ~chartwindow() { terminate(); }
    void open();
    void setrange(cshort ic, cfloat valmin, cfloat valmax);
    void setrange(cfloat valmin, cfloat valmax) { setrange(0,valmin,valmax); }
    void setcolor(cshort ic, const colf col);
    void setcolor(const colf col) { setcolor(0,col); }
    void setcolor(cshort ic, cfloat r, cfloat g, cfloat b);
    void setcolor(cfloat r, cfloat g, cfloat b) { setcolor(0,r,g,b); }
    void addpoint(cshort ic, cfloat val);
    void addpoint(cfloat val) { addpoint(0,val); }
    void setprefsize(clong x, clong y);
    void setprefposition(clong x1, clong x2,
                         clong y1, clong y2);
    void redraw();
    virtual void refresh();
    virtual void dump(ostream& out);
    virtual void load(istream& in);
};


class binchartwindow : public chartwindow
{
protected:
    long numbins;
    float* fy;
    float myexp;
    void init();
    virtual void plotpoints();
    void plotpoint(clong x, cfloat* y);
public:
    binchartwindow() { init(); }
    binchartwindow(cchar* n) : (n) { init(); }
    ~binchartwindow() { }
    void exponent(float e) { myexp = e; }
    float exponent() { return myexp; }
    void addpoint(cfloat* val, clong numval);
    void setprefsize(clong x, clong y);
    void setprefposition(clong x1, clong x2,
                         clong y1, clong y2);
    virtual void dump(ostream& out);
    virtual void load(istream& in);
};


// end of gwindow.h

#endif GWINDOW_H
