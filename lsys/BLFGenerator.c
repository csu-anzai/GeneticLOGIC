/* BLFGenerator.c - methods for BLF (Blinn-like Format) database generator
 *
 * Copyright (C) 1991, Jonathan P. Leech
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
 * $Log:	BLFGenerator.c,v $
 * Revision 1.1  91/03/20  10:45:19  leech
 * Initial revision
 * 
 *
 */
static char RCSid[] = "$Id: BLFGenerator.c,v 1.1 91/03/20 10:45:19 leech Exp Locker: leech $";

#include <osfcn.h>
#include "global_headers.h"
#include "vector.h"
#include "Turtle.h"
#include "BLFGenerator.h"

// Output a point with unique index and return the index
// BLF: POINT i Xi Yi Zi
int BLFGenerator::out_point(const Vector &p) {
    *out << "point " << pointnum << ' '
	 << p(0) << ' ' << p(1) << ' ' << p(2) << '\n';
    return pointnum++;
}

// BLF: MATRIX M11 M21 M31 M12 .. M32 ... M34
// Matrix is: [H L U T] where H,L,U are the orientation (frame)
//  and T is the translation.
void BLFGenerator::out_matrix(Turtle &t) {
    Vector h = t.H(), l = t.L(), u = t.U(), p = t.location();

    *out << "matrix ";
    *out << h(0) << ' ' << h(1) << ' ' << h(2) << ' '
	 << l(0) << ' ' << l(1) << ' ' << l(2) << ' '
	 << u(0) << ' ' << u(1) << ' ' << u(2) << ' '
	 << p(0) << ' ' << p(1) << ' ' << p(2) << '\n';
}

BLFGenerator::BLFGenerator(
    ofstream *o,
    Camera &v) : DBGenerator(o,v) {

    pointnum = 0;
}

// BLF: DEF object
void BLFGenerator::prelude(Turtle &t) {
    (void)t;

    *out << "def " << object_name << '\n';
    DBGenerator::prelude(t);
}

// BLF: ENDDEF
// BLF: BBOX object xmin ymin zmin xmax ymax zmax
void BLFGenerator::postscript(Turtle &t) {
    BoundingBox b = t.bounds();
    Vector vmin = b.min(), vmax = b.max();

    *out << "enddef\n";
    *out << "bbox " << object_name << ' '
	 << vmin(0) << ' ' << vmin(1) << ' ' << vmin(2) << ' '
	 << vmax(0) << ' ' << vmax(1) << ' ' << vmax(2) << '\n';

    // Could set up the viewing transformation here

    DBGenerator::postscript(t);
}

// Called when starting a new stream of graphics
void BLFGenerator::start_graphics(Turtle &) {
}

// Called when ending a stream of graphics, to display it
void BLFGenerator::flush_graphics(Turtle &) {
}

// BLF: POLY n v1 .. vn
void BLFGenerator::polygon(Turtle &t, Polygon &p) {
    (void)t;
    PolygonIterator pi(p);
    Vector *vec;
    static int	  *index;
    static Vector *vbuf;
    static int vbuflen = 0;

    // Reallocate the vertex buffer if needed
    if (p.size() > vbuflen) {
	if (vbuflen) {
	    delete vbuf;
	    delete index;
	}
	vbuf = new Vector[p.size()];
	index = new int[p.size()];
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

    // Spit out the points with unique indices
    for (int i = 0; i < n; i++)
	index[i] = out_point(vbuf[i]);

    // Spit out the polygon in terms of the point indices
    *out << "poly " << n;
    for (i = 0; i < n; i++)
	*out << ' ' << index[i];
    *out << '\n';
}

// BLF: twig vstart vend radius
void BLFGenerator::lineto(Turtle &t) {
    int    i, j;

    i = out_point(currentpos);
    j = out_point(t.location());

    *out << "twig "
	 << i << ' ' << j << ' ' << t.linewidth() / 2 << '\n';

    DBGenerator::lineto(t);
}

// BLF: FLOWER radius
void BLFGenerator::flower(Turtle &t, float radius) {
    out_matrix(t);
    *out << "flower " << radius << '\n';
}

// BLF: LEAF length
void BLFGenerator::leaf(Turtle &t, float length) {
    out_matrix(t);
    *out << "leaf " << length << '\n';
}

// BLF: APEX length X1 Y1 Z1 X2 Y2 Z2
void BLFGenerator::apex(Turtle &t, Vector &start, float length) {
    Vector p = t.location();

    out_matrix(t);
    *out << "apex " << length << ' '
	 << start(0) << ' ' << start(1) << ' ' << start(2)
	 << p(0) << ' ' << p(1) << ' ' << p(2) << '\n';
}

// BLF: DRAW object
void BLFGenerator::draw_object(Turtle &t, Module &m) {
    out_matrix(t);
    *out << "draw " << m << '\n';
}

// BLF: COLOR R G B
void BLFGenerator::set_color(Turtle &t) {
    Vector v = t.linecolor().rgbcolor();

    // Scale fractional intensities to 0..255; assumes that any value > 1
    //	means no scaling need be done
    if (v(0) <= 1 && v(1) <= 1 && v(2) <= 1)
	v = 255 * v;

    *out << "color "
	 << int(v(0)) << ' ' << int(v(1)) << ' ' << int(v(2)) << '\n';
}

void BLFGenerator::set_width(Turtle &) {
}
