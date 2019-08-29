#ifndef GOBJECT_C
#define GOBJECT_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// gobject.C: implementation of graphic object classes


#include "basicincludes.h"
#include "graphics.h"


implement(gdlink,pgobject)
implement(gdlist,pgobject)


void gxsortedlist::add(pgobject a)
{
    Boolean inserted = FALSE;
    pgobject o;
    this->reset();
    while (this->next(o))
    {
        if ((a->x()-a->radius()) < (o->x()-o->radius()))
        {
            this->inserthere(a);
            inserted = TRUE;
            break;
        }
    }
    if (!inserted) this->append(a);
}


void gxsortedlist::sort()
{
// This technique assumes that the list is almost entirely sorted at the start
// Hopefully, with some reasonable frame-to-frame coherency, this will be true!
    gdlink(pgobject) *savecurr;
    pgobject o = NULL;
    pgobject p = NULL;
    pgobject b = NULL;
    this->reset();
    this->next(p);
    savecurr = curr;
    while (this->next(o))
    {
        if ((o->x()-o->radius()) < (p->x()-p->radius()))
        {
            gdlink(pgobject)* link = this->unlink();  // at o, just unlink it
            curr = savecurr;  // back up to previous one directly
            while (this->prev(b)) // then use iterator to move back again
                if ((b->x()-b->radius()) < (o->x()-o->radius()))
                    break; // until we have one that starts before o
            if (curr)  // we did find one, and didn't run off beginning of list
                this->appendhere(link);
            else  // we have a new head of the list
                this->insert(link);
            curr = savecurr;
            o = p;
        }
        p = o;
        savecurr = curr;
    }
}


void gobject::dump(ostream& out)
{
    if (name)
        out << name nl;
    else
        out << "NULL" nl;
    out << pos[0] sp pos[1] sp pos[2] nl;
    out << angle[0] sp angle[1] sp angle[2] nl;
    out << col[0] sp col[1] sp col[2] sp col[3] nl;
    out << scale nl;
    out << rad nl;
    out << rotated nl;
}


void gobject::load(istream& in)
{
    if (name)
        delete name;
    name = new char[256];
    in >> name;
    in >> pos[0] >> pos[1] >> pos[2];
    in >> angle[0] >> angle[1] >> angle[2];
    in >> col[0] >> col[1] >> col[2] >> col[3];
    in >> scale;
    in >> rad;
    in >> rotated;
}


void gobject::print()
{
    cout << "For object named = \"" << name << "\"...\n";
    cout << "  position = " << pos[0] << ", "
                            << pos[1] << ", "
                            << pos[2] nl;
    cout << "  scale = " << scale nl;
    cout << "  default color (r,g,b) = ("
         << col[0] cm col[1] cm col[2] pnl;
    cout << "  radius = " << rad nl;
    cout.flush();
//  cout << "done in gobject::print()" nlf;
}


void gpoint::draw()
{
    c4f(col);
    bgnpoint();
      v3f(pos);
    endpoint();
}


void gline::draw()
{
    c4f(col);
    bgnline();
      v3f(pos); v3f(end);
    endline();
}


void gline::print()
{
    gobject::print();
    cout << "  end-pt (x,y,z) = (" << end[0] cm end[1] cm end[2] pnl;
    cout.flush();
}


void grect::draw()
{
    c4f(col);
    pushmatrix();
      position();
      ::scale(scale,scale,scale);
      if (filled)
          rectf(0.,0.,lenx,leny);
      else
          rect(0.,0.,lenx,leny);
    popmatrix();
}


void grect::print()
{
    gobject::print();
    cout << "  lenx = " << lenx nl;
    cout << "  leny = " << leny nl;
    if (filled)
        cout << "  it is filled" nl;
    else
        cout << "  it is not filled" nl;
    cout.flush();
}


void gsquare::draw()
{
    c4f(col);
    pushmatrix();
      position();
      ::scale(scale,scale,scale);
      if (filled)
          rectf(-0.5*lenx,-0.5*leny,0.5*lenx,0.5*leny);
      else
          rect(-0.5*lenx,-0.5*leny,0.5*lenx,0.5*leny);
    popmatrix();
}


void gsquare::print()
{
    gobject::print();
    cout << "  lenx = " << lenx nl;
    cout << "  leny = " << leny nl;
    if (filled)
        cout << "  it is filled" nl;
    else
        cout << "  it is not filled" nl;
    cout.flush();
}


void gbox::draw()
{
    c3f(col);
    pushmatrix();
      position();
      ::scale(scale*len[0],scale*len[1],scale*len[2]);
      if (filled)
          drawunitcube();
      else
          frameunitcube();
    popmatrix();
}


void gbox::print()
{
    gobject::print();
    cout << "  lenx = " << len[0] nl;
    cout << "  leny = " << len[1] nl;
    cout << "  lenz = " << len[2] nl;
    if (filled)
        cout << "  it is filled" nl;
    else
        cout << "  it is not filled" nl;
    cout.flush();
}


void gpoly::draw()
{
    c4f(col);
    pushmatrix();
      position();
      ::scale(scale,scale,scale);
      bgnpolygon();
        for (int j = 0; j < numpts; j++)
            v3f(&vert[j*3]);
      endpolygon();
    popmatrix();
}


void gpoly::print()
{
    gobject::print();
    for (int j = 0; j < numpts; j++)
        cout << "  vertex# (x,y,z) = " << j << " ("
             << vert[j*3] cm vert[j*3+1] cm vert[j*3+2] pnl;
    cout.flush();
}


void gpolyobj::clonegeom(const gpolyobj& o)
{
// WARNING:  this routine assumes the object to be cloned has the precise
// same topology as the current object, unless the current object has yet
// to be defined (in which case appropriate memory will be allocated).
    if (poly == NULL)
    {
        poly = new opoly[o.numpolys];
        if (poly == NULL)
        {
            error(1,"Insufficient memory to clone gpolyobj geometry");
            return;
        }
        for (long i = 0; i < o.numpolys; i++)
        {
            poly[i].vert = new float[o.poly[i].numpts*3];
            if (poly[i].vert == NULL)
            {
                error(1,"Insufficient memory to clone gpolyobj geometry");
                return;
            }
        }
    }
    numpolys = o.numpolys;
    for (long i = 0; i < numpolys; i++)
    {
        poly[i].numpts = o.poly[i].numpts;
        for (long j = 0; j < poly[i].numpts*3; j++)
            poly[i].vert[j] = o.poly[i].vert[j];
    }
}


void gpolyobj::draw()
{
    pushmatrix();
      position();
      ::scale(scale,scale,scale);
      drawcolpolyrange(0,numpolys-1,col);
    popmatrix();
}


void gpolyobj::print()
{
    gobject::print();
    for (int i = 0; i < numpolys; i++)
    {
        for (int j = 0; j < poly[i].numpts; j++)
        {
            cout << "  poly# vertex#  (x,y,z) = "
                 << i << "  " << j << " ("
                 << poly[i].vert[j*3  ] cm
                    poly[i].vert[j*3+1] cm
                    poly[i].vert[j*3+2] pnl;
        }
    }
    cout.flush();
}


void gpolyobj::setlen() // determine len[] (bounding box) and do a setradius()
{
    float xmin = poly[0].vert[0];
    float xmax = poly[0].vert[0];
    float ymin = poly[0].vert[1];
    float ymax = poly[0].vert[1];
    float zmin = poly[0].vert[2];
    float zmax = poly[0].vert[2];
    for (int i = 0; i < numpolys; i++)
    {
        for (int j = 0; j < poly[i].numpts; j++)
        {
            xmin = xmin < poly[i].vert[j*3  ] ? xmin : poly[i].vert[j*3  ];
            xmax = xmax > poly[i].vert[j*3  ] ? xmax : poly[i].vert[j*3  ];
            ymin = ymin < poly[i].vert[j*3+1] ? ymin : poly[i].vert[j*3+1];
            ymax = ymax > poly[i].vert[j*3+1] ? ymax : poly[i].vert[j*3+1];
            zmin = zmin < poly[i].vert[j*3+2] ? zmin : poly[i].vert[j*3+2];
            zmax = zmax > poly[i].vert[j*3+2] ? zmax : poly[i].vert[j*3+2];
        }
    }
    len[0] = xmax - xmin;
    len[1] = ymax - ymin;
    len[2] = zmax - zmin;
    setradius();
}


istream& operator>>(istream& s, opoly& op)
{
    s >> op.numpts;
    if (op.numpts <= 0)
        error(1,"invalid number of points (",op.numpts,") in polyobj");
    else
    {
        op.vert = new float[op.numpts*3];
        if (op.vert)
        {
            for (int i = 0; i < op.numpts*3; i++)
            {
                if (!(s >> op.vert[i]))  // catch error or eof
                {
                    switch (s.rdstate())
                    {
                    case _eof:
                        if (i != op.numpts-1)
                            error(1,"premature end-of-file for polyobj");
                        break;
                    case _fail:
                        error(1,"_fail reading polyobj file;",
                                " probably a formatting error");
                        break;
                    case _bad:
                        error(1,"_bad error reading polyobj file;",
                                " characters probably lost");
                        break;
                    }
                    break;  // for loop
                }
            }
        }
        else
            error(1,"unable to allocate memory for polyobj's polys");
    }
    return s;
}


void operator>>(cchar* filename, gpolyobj& gpo)
{
    filebuf fb;
    if (fb.open(filename,input) == 0)
        error(1,"unable to open polyobj file \"",filename,"\"");
    else
    {
        Boolean noerror = TRUE;
        istream in(&fb);
        char version[16];
        in >> version;
        if (!strcmp(version,"pw1"))
        {
            in >> gpo.numpolys;
            if (gpo.numpolys <= 0)
                error(1,"invalid number of polys (",gpo.numpolys,
                        ") in \"",filename,"\"");
            else
            {
                gpo.poly = new opoly[gpo.numpolys];
                if (gpo.poly)
                {
                    for (long i = 0; i < gpo.numpolys; i++)
                    {
                        if (!(in >> gpo.poly[i]))  // catch error or eof
                        {
                            switch (in.rdstate())
                            {
                            case _eof:
                                if (i != gpo.numpolys-1)
                                    error(1,"premature eof for polyobj \"",
                                            filename,"\"");
                                break;
                            case _fail:
                                error(1,"_fail reading polyobj \"",
                                        filename,"\";",
                                        " probably a formatting error");
                                break;
                            case _bad:
                                error(1,"_bad error reading polyobj \"",
                                        filename,"\";",
                                        " characters probably lost");
                                break;
                            }
                            noerror = FALSE;
                            break;  // for loop
                        }
                    }
                    if (noerror)
                        gpo.setlen();  // determine lengths (bounding box)
                }
                else
                    error(1,"unable to allocate memory for polyobj \"",
                            filename,"\"");
            }
        }
        else
            error(1,"object \"",filename,
                    "\" is of unknown type (",version,")");
        fb.close();
    }
}


// end of gobject.C

#endif GOBJECT_C
