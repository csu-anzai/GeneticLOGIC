/* PPHIGSGenerator.c - methods for PPHIGS database generator
 *
 * Copyright (C) 1990, Jonathan P. Leech
 *
 * This software may be freely copied, modified, and redistributed,
 * provided that this copyright notice is preserved on all copies.
 *
 * There is no warranty or other guarantee of fitness for this software,
 * it is provided solely "as is". Bug reports or fixes may be sent
 * to the author, who may or may not act on them as he desires.
 *
 * You may not include this software in a program or other software product
 * without supplying the source, or without informing the end-user that the
 * source is available for no extra charge.
 *
 * If you modify this software, you should include a notice giving the
 * name of the person performing the modification, the date of modification,
 * and the reason for such modification.
 *
 * $Log:	PPHIGSGenerator.c,v $
 * Revision 1.2  91/03/20  10:38:29  leech
 * Better color support. Made some output functions into class methods.
 * 
 * Revision 1.1  91/03/18  16:29:39  leech
 * Initial revision
 *
 */
static char RCSid[] = "$Id: PPHIGSGenerator.c,v 1.2 91/03/20 10:38:29 leech Exp Locker: leech $";

#include <osfcn.h>
#include "global_headers.h"
#include "vector.h"
#include "Turtle.h"
#include "PPHIGSGenerator.h"

float max(float a, float b) { return (a > b) ? a : b; }

PPHIGSGenerator::PPHIGSGenerator(
    ofstream *o,
    Camera &v,
    int nf) : DBGenerator(o,v) {

    if (nf < 3) {
	cerr << "PPHIGSGenerator(): constructor requires nfacets >= 3\n";
	exit(1);
    }

    // Build the offset coefficient table for cylinders
    nfacets = nf;
    costab = new float[nfacets];
    sintab = new float[nfacets];
    for (int i = 0; i < nfacets; i++) {
	float theta = (2 * M_PI * i) / nfacets;
	costab[i] = cos(theta);
	sintab[i] = sin(theta);
    }
    n = new Vector[nfacets];
    p = new Vector[nfacets];
}

// Generate a color primitive
void PPHIGSGenerator::out_color(
    char *primitive,
    const Vector &f,
    const Vector &b)
{
    *out << "\tcolor " << primitive << " {\n";
    *out << "\t\tdiff "
	 << int(f(0)) << ' ' << int(f(1)) << ' ' << int(f(2)) << " 0 "
	 << int(b(0))  << ' ' << int(b(1))  << ' ' << int(b(2))  << " 0\n";
    *out << "\t\tspec 0 0 0 0 0 0 0 0\n";
    *out << "\t};\n";
}

// Generate a single point & associated normal
void PPHIGSGenerator::out_p(const Vector &p) {
    *out << "\t\t" << p(0) << ' ' << p(1) << ' ' << p(2) << ";\n";
}

// Generate a single point & associated normal
void PPHIGSGenerator::out_p_n(const Vector &p, const Vector &n) {
    *out << "\t\t" << p(0) << ' ' << p(1) << ' ' << p(2) << ' '
		<< n(0) << ' ' << n(1) << ' ' << n(2) << ";\n";
}

// Generate a rectangle
void PPHIGSGenerator::out_rect(
    const Vector &ul, const Vector &uln,
    const Vector &ur, const Vector &urn,
    const Vector &lr, const Vector &lrn,
    const Vector &ll, const Vector &lln)
{
    *out << "\tpolygon 4 {\n";
    out_p_n(ul, uln);
    out_p_n(ur, urn);
    out_p_n(lr, lrn);
    out_p_n(ll, lln);
    *out << "\t};\n";
}

// Generate a sphere
void PPHIGSGenerator::out_sphere(
    const Vector  &p,
    float    r)
{
    *out << "\tsphere "
	 << p(0) << ' ' << p(1) << ' ' << p(2) << ' ' << r << ";\n";
}

void PPHIGSGenerator::prelude(Turtle &t) {
    (void)t;

    *out << "structure " << object_name << " {\n";
    DBGenerator::prelude(t);
}

void PPHIGSGenerator::postscript(Turtle &t) {
    Vector vmin, vmax;
    BoundingBox b = t.bounds();

    vmin = b.min();
    vmax = b.max();

    // Figure out scaling to [-1000..1000] cube
    // Pick the largest box size to scale by in all dimensions,
    //	so the aspect ratio is retained.
    const float pagesize = 2000;

    float  scale;
    Vector size = vmax - vmin;

    scale = max(size[0], max(size[1], size[2]));
    if (scale != 0)
	scale = pagesize / scale;
    else
	scale = 1;

    //float tx = -vmin[0] * scale,
    //	    ty = -vmin[1] * scale;

    // Close 'plant' structure and start a new one for scaling
    //	to default viewing space
    *out << "};\n";

    *out << "structure disp_" << object_name << " posted {\n";
    *out << "\tmatrix Pre scale " << scale << ' ' << scale << ' '
	 << scale << ";\n";
    // *out << "\tmatrix Pre translate "%d %d %d ;\n", dx, dy, dz);
    *out << "\texecute " << object_name << ";\n";
    *out << "};\n";
    *out << "/* Camera: " << view << "\n*/\n";

    DBGenerator::postscript(t);
}

// Called when starting a new stream of graphics
void PPHIGSGenerator::start_graphics(Turtle &t) {
    (void)t;
}

// Called when ending a stream of graphics, to display it
void PPHIGSGenerator::flush_graphics(Turtle &t) {
    (void)t;
}

void PPHIGSGenerator::polygon(Turtle &t, Polygon &p) {
    (void)t;
    PolygonIterator pi(p);
    Vector *vec;
    static Vector *vbuf;
    static int vbuflen = 0;

    // Reallocate the vertex buffer if needed
    if (p.size() > vbuflen) {
	if (vbuflen)
	    delete vbuf;
	vbuf = new Vector[p.size()];
	vbuflen = p.size();
    }

    // Accumulate non-duplicate vertices
    int n;
    for (n = 0, vec = pi.first(); vec != NULL; vec = pi.next()) {
	if (n == 0 || (*vec != vbuf[n-1]))
	    vbuf[n++] = *vec;
    }

    // Don't generate a polygon with < 3 vertices
    if (n < 3)
	return;

    *out << "\tpolygon " << n << " {\n";
    for (int i = 0; i < n; i++) {
	out_p(vbuf[i]);
    }
    *out << "\t};\n";
}

void PPHIGSGenerator::lineto(Turtle &t) {
    Vector start = currentpos;
    Vector end = t.location();

    // Generate a polygonal approximation to a cylinder. The number of
    //	facets is set at creation of the generator and returned by
    //	nfacets. The frame vectors U and L provide a basis for
    //	the nfacets rectangles making up the approximation and
    //	the coefficients of U and L for each vertex offset are in
    //	members costab[] and sintab[].
    // For example, the offsets for nfacets = 6
    //	form a hexagon of width w/2:
    //		p1
    //	       /\	    ^ U
    //	      /  \	    |
    //	  p6 /	  \ p2	    |
    //	    |	   |	<---+
    //	    |	   |	 L
    //	    |	   |
    //	  p5 \	  / p3
    //	      \  /
    //	       \/p4
    Vector u = t.U(), l = t.L();
    float  w = t.linewidth() / 2;
    for (int i = 0; i < nfacets; i++) {
	n[i] = costab[i] * u + sintab[i] * l;	// Normal for vertex offset i
	p[i] = w * n[i];			// Vertex offset i
    }

    for (i = 0; i < nfacets; i++) {
	int nexti = (i+1) % nfacets;
	out_rect(start+p[i],	 n[i],
		 end+p[i],	 n[i],
		 end+p[nexti],	 n[nexti],
		 start+p[nexti], n[nexti]);
    }

    // Cap the twig with hemispheres
    out_sphere(start, w*2);
    out_sphere(end, w*2);

    DBGenerator::lineto(t);
}

void PPHIGSGenerator::flower(Turtle &t, float radius) {
    *out << "/* execute flower " << t.location() << " r = " << radius << "*/\n";
}

void PPHIGSGenerator::leaf(Turtle &t, float length) {
    *out << "/* execute leaf " << t.location() << " length = "
	 << length << "*/\n";
}

void PPHIGSGenerator::apex(Turtle &t, Vector &start, float length) {
    *out << "/* execute apex " << start << " to " << t.location()
	 << " length = " << length << "*/\n";
}

void PPHIGSGenerator::draw_object(Turtle &t, Module &m) {
    *out << "/* execute " << m << '\n'
	 << "\ttranslation = " << t.location() << '\n'
	 << "\trotation =\n" << t.orientation() << '\n' << "*/\n";
}

void PPHIGSGenerator::set_color(Turtle &t) {
    Vector v = t.linecolor().rgbcolor();

    // Scale fractional intensities to 0..255; assumes that any value > 1
    //	means no scaling need be done
    if (v(0) <= 1 && v(1) <= 1 && v(2) <= 1)
	v = 255 * v;

    out_color("polygon", v, v);
    out_color("sphere", v, v);
}

void PPHIGSGenerator::set_width(Turtle &t) {
    *out << "/* width " << t.linewidth() << " */\n";
}
