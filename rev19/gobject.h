#ifndef GOBJECT_H
#define GOBJECT_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gobject.h: declarations of graphic object classes


/*
enum gobjtype { POINT, LINE, RECT, RECTF, BOX, BOXF, POLY, POLYOBJ };

const char gobjtypename[8][16] =
    { "Point", "Line", "Rect", "RectF", "Box", "BoxF", "Poly", "PolyObj" };
*/


// Orientation, like with gcameras, is handled by yaw, pitch, and roll
// (rotate-y, rotate-x, rotate-z) applied in that order.
// Also like gcameras, gobjects are assumed to begin life facing down
// the positive x-axis.


class gobject // graphical object
{
private:
    Boolean rotated;
protected:
    float pos[3];
    float angle[3];
    float scale;
    float col[4];  // default color (r,g,b,t) if no material is bound
    char* name;
    float rad;  // for sphere of influence
    void init()
    {
        pos[0] = pos[1] = pos[2] = 0.; scale = 1.;
        angle[0] = angle[1] = angle[2] = 0.;
        col[0] = rand()/32767.; col[1] = rand()/32767.;
        col[2] = rand()/32767.; col[3] = 0.;
        name = NULL; rotated = FALSE;
        rad = 0.0;
    }
    gobject() { init(); }
    gobject(cchar* n) { init(); setname(n); }
    virtual ~gobject() { if (name) delete name; }
    void dump(ostream& out);
    void load(istream& in);
public:
    virtual void print();
    virtual void draw() { }
    void settranslation(cfloat* p)
        { pos[0] = p[0]; pos[1] = p[1]; pos[2] = p[2]; }
    void settranslation(cfloat p0, cfloat p1 = 0., cfloat p2 = 0.)
        { pos[0] = p0;   pos[1] = p1;   pos[2] = p2;   }
    void addtranslation(cfloat p0, cfloat p1 = 0., cfloat p2 = 0.)
        { pos[0] += p0;  pos[1] += p1;  pos[2] += p2;   }
    void setx(cfloat x) { pos[0] = x; }
    void sety(cfloat y) { pos[1] = y; }
    void setz(cfloat z) { pos[2] = z; }
    void addx(cfloat x) { pos[0] += x; }
    void addy(cfloat y) { pos[1] += y; }
    void addz(cfloat z) { pos[2] += z; }
    void setpos(cint k, cfloat p) { pos[k] = p; }
    void setrotation(cfloat yaw, cfloat pitch, cfloat roll)
        { angle[0] = yaw; angle[1] = pitch; angle[2] = roll; rotated = TRUE; }
    void setyaw(cfloat yaw) { angle[0] = yaw; rotated = TRUE; }
    void setpitch(cfloat pitch) { angle[1] = pitch; rotated = TRUE; }
    void setroll(cfloat roll) { angle[2] = roll; rotated = TRUE; }
    void addyaw(cfloat yaw) { angle[0] += yaw; rotated = TRUE; }
    void addpitch(cfloat pitch) { angle[1] += pitch; rotated = TRUE; }
    void addroll(cfloat roll) { angle[2] += roll; rotated = TRUE; }
    float getyaw() { return angle[0]; }
    float getpitch() { return angle[1]; }
    float getroll() { return angle[2]; }
    float yaw() { return angle[0]; }
    float pitch() { return angle[1]; }
    float roll() { return angle[2]; }
    void setscale(cfloat s) { scale = s; }
    void setradius(cfloat r) { rad = r; }
    void setcol3(cfloat* c)
        { col[0] = c[0]; col[1] = c[1]; col[2] = c[2]; }
    void setcol4(cfloat* c)
        { col[0] = c[0]; col[1] = c[1]; col[2] = c[2]; col[3] = c[3]; }
    void setcolor(cfloat r, cfloat g, cfloat b)
        { col[0] = r;    col[1] = g;    col[2] = b;    }
    void setcolor(cfloat r, cfloat g, cfloat b, cfloat t)
        { col[0] = r;    col[1] = g;    col[2] = b;    col[3] = t;    }
    void setcolor(const colf c)
        { col[0] = c.r;  col[1] = c.g;  col[2] = c.b;  col[3] = c.t;  }
    void settransparency(cfloat t) { col[3] = t; }
    void setr(cfloat r) { col[0] = r; }
    void setg(cfloat g) { col[1] = g; }
    void setb(cfloat b) { col[2] = b; }
    float getr() { return col[0]; }
    float getg() { return col[1]; }
    float getb() { return col[2]; }
    void setname(cchar* pc) // { name = pc; }
    {
        name = new char[strlen(pc)+1];
        if (name == 0)
            error(1,"Insufficient memory to store object name");
        else
            strcpy(name,pc);
    }
    cchar* getname() { return name; }
    float* getposptr() { return &pos[0]; }
    void getpos(float* p) { p[0] = pos[0]; p[1] = pos[1]; p[2] = pos[2]; }
    void getpos(float& px, float& py, float& pz)
        { px = pos[0]; py = pos[1]; pz = pos[2]; }
    void getpos(float* px, float* py, float* pz)
        { *px = pos[0]; *py = pos[1]; *pz = pos[2]; }
    float getpos(cint i) { return pos[i]; }
    float getx() { return pos[0]; }
    float gety() { return pos[1]; }
    float getz() { return pos[2]; }
    float x() { return pos[0]; }
    float y() { return pos[1]; }
    float z() { return pos[2]; }
    float radius() { return rad; }
// you'd better know what you're doing if you invoke these...
// (they are intended for use between pushmatrix()/popmatrix() pairs)
    cvoid translate() { ::translate(pos[0],pos[1],pos[2]); }
    cvoid rotate()
    {
        if (rotated)
            { rot(angle[0],'y'); rot(angle[1],'x'); rot(angle[2],'z'); }
    }
    void position() { translate(); rotate(); }
    cvoid inversetranslate() { ::translate(-pos[0],-pos[1],-pos[2]); }
    cvoid inverserotate()
    {
        if (rotated)
            { rot(-angle[2],'z'); rot(-angle[1],'x'); rot(-angle[0],'y'); }
    }
    cvoid inverseposition() { inverserotate(); inversetranslate(); }
};


typedef gobject* pgobject;

declare(gdlink,pgobject)
declare(gdlist,pgobject)


class gxsortedlist : public gdlist(pgobject)
{
public:
    gxsortedlist() { }
    ~gxsortedlist() { }
    void add(pgobject a);
    void sort();
};


class gpoint : public gobject
{
public:
    gpoint() { }
    gpoint(cfloat* p) { settranslation(p); }
    gpoint(cfloat px, cfloat py = 0., cfloat pz = 0.)
        { settranslation(px,py,pz); }
    gpoint(cchar* n) : (n) { }
    gpoint(cchar* n, cfloat* p) : (n) { settranslation(p); }
    gpoint(cchar* n, cfloat px, cfloat py, cfloat pz) : (n)
        { settranslation(px,py,pz); }
    ~gpoint() { }
    virtual void draw();
};


class gline : public gobject
{
private:
    float end[3];  // beginning is considered to be at gobject::pos[]
public:
    void init() { end[0] = end[1] = end[2] = 0.; }
    gline() { init(); }
    gline(cchar* n) : (n) { init(); }
    gline(cfloat* e) { setend(e); }
    gline(cfloat* b, cfloat* e) { settranslation(b); setend(e); }
    gline(cfloat b0, cfloat b1, cfloat b2, cfloat* e)
        { settranslation(b0,b1,b2); setend(e); }
    gline(cfloat b0, cfloat b1, cfloat b2, cfloat e0, cfloat e1, cfloat e2)
        { settranslation(b0,b1,b2); setend(e0,e1,e2); }
    ~gline() { }
    void setend(cfloat* e)
        { end[0] = e[0]; end[1] = e[1]; end[2] = e[2]; }
    void setend(cfloat e0, cfloat e1, cfloat e2)
        { end[0] = e0;   end[1] = e1;   end[2] = e2;   }
    virtual void draw();
    virtual void print();
};


/*
class gvector : public gline
{
public:
    gvector() : (1.,0.,0.) { }
    gvector(cfloat l, cAngle* a) : (sin(a[0]*DEGTORAD/10)*l,...) { }
    gvector(cfloat l, cAngle ax = 0, cAngle ay = 0, cAngle az = 0) : (sin...) { }
    gvector(cfloat l, cfloat* a) : (sin(a[0]*DEGTORAD)*l,...) { }
    gvector(cfloat l, cfloat ax = 0., cfloat ay = 0., cfloat az = 0.) : (...) { }
    ~gvector() { }
    void setvector(cfloat l, cAngle* a) { as above; }
    ... plus all the Angle & float versions ... { so below; }
    void setangle(cAngle* a) { etc.; } ...
    void setlength(cfloat l) { ditto; } ...
};
*/


class grect : public gobject
{
protected:
    float lenx;
    float leny;
    Boolean filled;
    Boolean radiusfixed;
    float radscale;
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(lenx*lenx + leny*leny)
                * radscale * scale * 0.5;
    }
public:
    void init() { lenx = leny = 1.; radiusfixed = FALSE; radscale = 1.0; }
    grect() { init(); }
    grect(cchar* n) : (n) { init(); }
    grect(cfloat lx, cfloat ly) { setrect(lx,ly); }
    grect(cchar* n, cfloat lx, cfloat ly) : (n) { setrect(lx,ly); }
    grect(cfloat xa, cfloat ya, cfloat xb, cfloat yb) { setrect(xa,ya,xb,yb); }
    grect(cchar* n, cfloat xa, cfloat ya, cfloat xb, cfloat yb) : (n)
        { setrect(xa,ya,xb,yb); }
    ~grect() { }
    void setrect(cfloat lx, cfloat ly) { lenx = lx; leny = ly; setradius(); }
    void setrect(cfloat xa, cfloat ya, cfloat xb, cfloat yb)
    {
        float xp;
        float yp;
        if (xa < xb)
            { xp = xa; lenx = xb - xa; }
        else
            { xp = xb; lenx = xa - xb; }
        if (ya < yb)
            { yp = ya; leny = yb - ya; }
        else
            { yp = yb; leny = ya - yb; }
        settranslation(xp,yp);
        setradius();
    }
    virtual void setfilled(cBoolean f) { filled = f; }
    void setradius(cfloat r) { radiusfixed = TRUE; gobject::setradius(r); }
    void setradiusscale(cfloat s)
        { radiusfixed = FALSE; radscale = s; setradius(); }
    void setscale(cfloat s) { scale = s; setradius(); }
    float getlenx()   { return lenx; }
    float getleny()   { return leny; }
    float getwidth()  { return lenx; }
    float getheight() { return leny; }
    float lx()   { return lenx; }
    float ly()   { return leny; }
    float radiusscale() { return radscale; }
    virtual void draw();
    virtual void print();
};


class grectf : public grect
{
public:
    grectf() { filled = TRUE; }
    grectf(cchar* n) : (n) { filled = TRUE; }
    grectf(cfloat lx, cfloat ly) : (lx,ly) { filled = TRUE; }
    grectf(cchar* n, cfloat lx, cfloat ly) : (n,lx,ly) { filled = TRUE; }
    grectf(cfloat xa, cfloat ya, cfloat xb, cfloat yb) : (xa,ya,xb,yb)
        { filled = TRUE; }
    grectf(cchar* n, cfloat xa, cfloat ya, cfloat xb, cfloat yb)
        : (n,xa,ya,xb,yb) { filled = TRUE; }
    ~grectf() { }
    void setfilled(cBoolean f)
        // the "if (f)..." statement is purely to eliminate a compiler warning
        // about f being unused.
        { if (f) filled = TRUE; error(0,"setfilled() called for filled rect"); }
};


class gsquare : public gobject
{
protected:
    float lenx;
    float leny;
    Boolean filled;
    Boolean radiusfixed;
    float radscale;
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(lenx*lenx + leny*leny)
                * radscale * scale * 0.5;
    }
public:
    void init() { lenx = leny = 1.; radiusfixed = FALSE; radscale = 1.0; }
    gsquare() { init(); }
    gsquare(cchar* n) : (n) { init(); }
    gsquare(cfloat lx, cfloat ly) { setsquare(lx,ly); }
    gsquare(cchar* n, cfloat lx, cfloat ly) : (n) { setsquare(lx,ly); }
    gsquare(cfloat xa, cfloat ya, cfloat xb, cfloat yb)
        { setsquare(xa,ya,xb,yb); }
    gsquare(cchar* n, cfloat xa, cfloat ya, cfloat xb, cfloat yb) : (n)
        { setsquare(xa,ya,xb,yb); }
    ~gsquare() { }
    void setsquare(cfloat lx, cfloat ly) { lenx = lx; leny = ly; setradius(); }
    void setsquare(cfloat lx, cfloat ly, cfloat x, cfloat y)
        { lenx = lx; leny = ly; pos[0] = x; pos[1] = y; setradius(); }
    void setrect(cfloat xa, cfloat ya, cfloat xb, cfloat yb)
    {
        float xp;
        float yp;
        xp = (xa + xb) * 0.5;
        yp = (ya + yb) * 0.5;
        lenx = fabs(xb - xa);
        leny = fabs(yb - ya);
        settranslation(xp,yp);
        setradius();
    }
    virtual void setfilled(cBoolean f) { filled = f; }
    void setradius(cfloat r) { radiusfixed = TRUE; gobject::setradius(r); }
    void setradiusscale(cfloat s)
        { radiusfixed = FALSE; radscale = s; setradius(); }
    void setscale(cfloat s) { scale = s; setradius(); }
    float getlenx()   { return lenx; }
    float getleny()   { return leny; }
    float getwidth()  { return lenx; }
    float getheight() { return leny; }
    float lx()   { return lenx; }
    float ly()   { return leny; }
    float radiusscale() { return radscale; }
    virtual void draw();
    virtual void print();
};


class gsquaref : public gsquare
{
public:
    gsquaref() { filled = TRUE; }
    gsquaref(cchar* n) : (n) { filled = TRUE; }
    gsquaref(cfloat lx, cfloat ly) : (lx,ly) { filled = TRUE; }
    gsquaref(cchar* n, cfloat lx, cfloat ly) : (n,lx,ly) { filled = TRUE; }
    gsquaref(cfloat xa, cfloat ya, cfloat xb, cfloat yb) : (xa,ya,xb,yb)
        { filled = TRUE; }
    gsquaref(cchar* n, cfloat xa, cfloat ya, cfloat xb, cfloat yb)
        : (n,xa,ya,xb,yb) { filled = TRUE; }
    ~gsquaref() { }
    void setfilled(cBoolean f)
        // the "if (f)..." statement is purely to eliminate a compiler warning
        // about f being unused.
        { if (f) filled = TRUE; error(0,"setfilled() called for filled rect"); }
};


class gbox : public gobject
{
protected:
    Boolean filled;
    float len[3];
    Boolean radiusfixed;
    float radscale;
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(len[0]*len[0] + len[1]*len[1] + len[2]*len[2])
                * radscale * scale * 0.5;
    }
public:
    void init()
    {
        radiusfixed = FALSE;
        radscale = 1.0;
    }
    gbox()
        { init(); setsize(1.,1.,1.); }
    gbox(cchar* n) : (n)
        { init(); setsize(1.,1.,1.); }
    gbox(cfloat lx, cfloat ly, cfloat lz)
        { init(); setsize(lx,ly,lz); }
    gbox(cchar* n, cfloat lx, cfloat ly, cfloat lz) : (n)
        { init(); setsize(lx,ly,lz); }
    ~gbox() { }
    void setsize(cfloat lx, cfloat ly, cfloat lz)
        { len[0] = lx; len[1] = ly; len[2] = lz; setradius(); }
    void setlen(cfloat lx, cfloat ly, cfloat lz)
        { len[0] = lx; len[1] = ly; len[2] = lz; setradius(); }
    void setlen(cint i, cfloat l) { len[i] = l; setradius(); }
    void setlenx(cfloat lx) { len[0] = lx; setradius(); }
    void setleny(cfloat ly) { len[1] = ly; setradius(); }
    void setlenz(cfloat lz) { len[2] = lz; setradius(); }
    void setradius(cfloat r) { radiusfixed = TRUE; gobject::setradius(r); }
    void setradiusscale(cfloat s)
        { radiusfixed = FALSE; radscale = s; setradius(); }
    void setscale(cfloat s) { scale = s; setradius(); }
    float getlen(cint i) { return len[i]; }
    float getlenx() { return len[0]; }
    float getleny() { return len[1]; }
    float getlenz() { return len[2]; }
    float lx()   { return len[0]; }
    float ly()   { return len[1]; }
    float lz()   { return len[2]; }
    float radiusscale() { return radscale; }
    virtual void draw();
    virtual void print();
};


class gboxf : public gbox
{
public:
    gboxf() { filled = TRUE; }
    gboxf(cchar* n) : (n) { filled = TRUE; }
    gboxf(cfloat lx, cfloat ly, cfloat lz) : (lx,ly,lz) { filled = TRUE; }
    gboxf(cchar* n, cfloat lx, cfloat ly, cfloat lz) : (n,lx,ly,lz)
        { filled = TRUE; }
    ~gboxf() { }
    void setfilled(cBoolean f)
        // the "if (f)..." statement is purely to eliminate a compiler warning
        // about f being unused.
        { if (f) filled = TRUE; error(0,"setfilled() called for filled box"); }
};


struct opoly // Object POLY's for gpolyobj objects (don't need gobject members)
{
    long numpts;
    float* vert;
};


class gpoly : public gobject
{
friend istream& operator>>(istream&,gpoly&);
protected:
    long numpts;
    float* vert;
    float len[3];
    float radscale;
    Boolean radiusfixed;
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(len[0]*len[0] + len[1]*len[1] + len[2]*len[2])
                * radscale * scale * 0.5;
    }
    void init()
    {
        radiusfixed = FALSE;
        radscale = 1.0;
    }
public:
    gpoly() { numpts = 0; vert = 0; init(); }
    gpoly(cchar* n) : (n) { numpts = 0; vert = 0; init(); }
    gpoly(clong np,float* v) { numpts = np; vert = v; init(); }
    gpoly(cchar* n, clong np,float* v) : (n) { numpts = np; vert = v; init(); }
    gpoly(const opoly& p) { numpts = p.numpts; vert = p.vert; init(); }
    gpoly(cchar* n, const opoly& p) : (n)
        { numpts = p.numpts; vert = p.vert; init(); }
    gpoly(const opoly* pp) { numpts = (*pp).numpts; vert = (*pp).vert; init(); }
    gpoly(cchar* n, const opoly* pp) : (n)
        { numpts = (*pp).numpts; vert = (*pp).vert; init(); }
    ~gpoly() { }
    void setradius(cfloat r) { radiusfixed = TRUE; gobject::setradius(r); }
    void setradiusscale(cfloat s)
        { radiusfixed = FALSE; radscale = s; setradius(); }
    void setscale(cfloat s) { scale = s; setradius(); }
    float lx()   { return len[0]; }
    float ly()   { return len[1]; }
    float lz()   { return len[2]; }
    float radiusscale() { return radscale; }
    virtual void draw();
    virtual void print();
};


class gpolyobj : public gobject
{
friend void operator>>(cchar*,gpolyobj&);
//protected:
public:
    long numpolys;
    opoly* poly;
    float len[3];
    float radscale;
    Boolean radiusfixed;
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(len[0]*len[0] + len[1]*len[1] + len[2]*len[2])
                * radscale * scale * 0.5;
    }
    void setlen(); // determine len[] (bounding box) and do a setradius()
    void init(clong np, opoly* p)
    {
        numpolys = np;
        poly = p;
        radiusfixed = FALSE;
        radscale = 1.0;
        if (np && p) setlen();
    }
public:
    gpolyobj() { init(0,0); }
    gpolyobj(cchar* n) : (n) { init(0,0); }
    gpolyobj(clong np,opoly* p) { init(np,p); }
    gpolyobj(cchar* n, clong np,opoly* p) : (n) { init(np,p); }
    ~gpolyobj()
    {
        if (poly)
        {
            for (long i = 0; i < numpolys; i++)
                if (poly[i].vert)
                    delete poly[i].vert;
            delete poly;
        }
    }
    void clonegeom(const gpolyobj& o);
    void setradius(cfloat r) { radiusfixed = TRUE; gobject::setradius(r); }
    void setradiusscale(cfloat s)
        { radiusfixed = FALSE; radscale = s; setradius(); }
    void setscale(cfloat s) { scale = s; setradius(); }
    float lx()   { return len[0]; }
    float ly()   { return len[1]; }
    float lz()   { return len[2]; }
    float radiusscale() { return radscale; }
    void drawcolpolyrange(clong i1, clong i2, cfloat* color)
    {
        c4f(color);
        for (long i = i1; i <= i2; i++)
        {
            bgnpolygon();
              for (long j = 0; j < poly[i].numpts; j++)
                  v3f(&poly[i].vert[j*3]);
              v3f(&poly[i].vert[0]);  // send the first point again to close it
            endpolygon();
        }
    }
    virtual void draw();
    virtual void print();
};


// end of gobject.h

#endif GOBJECT_H
