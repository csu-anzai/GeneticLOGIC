/* PSGenerator.c - methods for PostScript database generator
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
 * $Log:	PSGenerator.c,v $
 * Revision 1.3  91/03/20  10:37:52  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:08  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: PSGenerator.c,v 1.3 91/03/20 10:37:52 leech Exp Locker: leech $";

#include "global_headers.h"
#include "vector.h"
#include "Turtle.h"
#include "PSGenerator.h"

// Maximum number of line segments to draw in a path
const int maxseg = 100;

// Spit out a point (ignore Z coordinate)
static void outpoint(ostream &o, const Vector &v) {
    o << v(0) << ' ' << v(1) << ' ';
}

void PSGenerator::prelude(Turtle &t) {
    (void)t;
    // Leave a buffer for the translate/scale information at start of file
    const streamoff offset = 96;
    out->seekp(offset, ios::beg);

    //if (off != offset)
    //	  cerr << "Prelude: seek failed, offset is " << off << endl;

    *out << "%!\n"
	 << "/lsys_saveobj save def\n"
	 << "1 setlinecap\n";

    // Abbreviations to shrink the output file size
    *out << "/m { moveto } bind def\n"
	 << "/l { lineto } bind def\n"
	 << "/w { setlinewidth } bind def\n"
	 << "/c { setgray } bind def\n"
	 << "/s { stroke } bind def\n";

    DBGenerator::prelude(t);
}

void PSGenerator::postscript(Turtle &t) {
    Vector vmin, vmax;

    flush_graphics(t);

    *out << "lsys_saveobj restore\n";
    *out << "showpage\n";

    BoundingBox b = t.bounds();
    *out << "% BoundingBox (object space) : " << b << '\n';

    if (autoscale) {
	b = b.transform(view.view_matrix());

	vmin = b.min();
	vmax = b.max();
	*out << "% BoundingBox (viewing space) : " << b << '\n';
    } else {
	// Canonical viewing space

	vmin = Vector(-1,-1,-1);
	vmax = Vector(1,1,1);
    }

    // Figure out scaling to page size.
    // Assume we use an 7.75 inch square region (558 points).
    // Pick the larger of x and y sizes to scale by so a square aspect
    //	ratio is retained.
    const float pagesize = 558;

    float  scale;
    Vector size = vmax - vmin;

    if (size[1] > size[0])
	scale = pagesize / size[1];
    else if (size[0] != 0)
	scale = pagesize / size[0];
    else
	scale = 1;

    float tx = -vmin[0] * scale,
	  ty = -vmin[1] * scale;

    // Offset by 1/4 inch (36 points) to avoid page edge.
    tx += 36;
    ty += 36;

    // Spit out the PostScript scaling commands at start of file
    out->seekp(0, ios::beg);
    //if (off != 0)
    //	  cerr << "Postscript: seek to beginning failed, offset is " << off << endl;

#ifndef __GNUG__
    long flags = out->flags();

    // Use a 12-character field width for numeric conversions
    out->setf(ios::fixed);
#endif /*__GNUG__*/

    // Scaling information totals to 96 bytes

    *out << "%!Device scaling parameters\n";	 // 28 chars including nl

#ifndef __GNUG__
    *out << setw(12) << tx << ' '
	 << setw(12) << ty << " translate\n";	 // 36 chars
    *out << setw(12) << scale << ' '
	 << setw(12) << scale << " scale\n";	 // 32 chars

    out->flags(flags);
#else
    // This may be a severe problem
    *out << form("%12.6f %12.6f translate\n", tx, ty);	    // 36 chars
    *out << form("%12.6f %12.6f scale\n", scale, scale);    // 32 chars
#endif /*__GNUG__*/

    DBGenerator::postscript(t);
}

// Called when starting a new stream of graphics
void PSGenerator::start_graphics(Turtle &t) {
    // Only need to generate a single 'newpath' PostScript command at
    //	start of the sequence, since 'stroke' implicitly does a newpath.
    if (newpath == false) {
	*out << "newpath\n";
	newpath = true;
    }

    moveto(t);
}

// Called when ending a stream of graphics, to display it
void PSGenerator::flush_graphics(Turtle &t) {
    (void)t;

    *out << "s\n";
    nseg = 0;
}

void PSGenerator::polygon(Turtle &t, Polygon &p) {
    PolygonIterator pi(p);
    Vector *vec;
    int     i;

    start_graphics(t);
    // Draw the polygon
    for (i = 0, vec = pi.first(); vec != NULL; vec = pi.next(),i++) {
	Vector p = view.transform(*vec);
	outpoint(*out, p);
	*out << (i == 0 ? " m\n" : " l\n");
    }
    *out << "fill\n";
}

void PSGenerator::lineto(Turtle &t) {
    // Start a series of lines at the last point moved to, if needed
    if (lastmove) {
	Vector p = view.transform(currentpos);
	outpoint(*out, p);
	*out << " m\n";
    }

    Vector here = view.transform(t.location());
    outpoint(*out, here);
    *out << " l\n";

    DBGenerator::lineto(t);

    // Don't spit out more than maxseg segments in any path.
    // This is to avoid PostScript limits.
    if (++nseg > maxseg) {
	flush_graphics(t);
	moveto(t);
    }
}

void PSGenerator::flower(Turtle &t, float radius) {
    start_graphics(t);

    outpoint(*out, view.transform(t.location()));

    // Should transform circle as well, much harder
    *out << radius << " 0 360 arc fill\n";

    // flush_graphics done implicitly by 'fill'
}

// Generate a leaf. There is an implicit assumption that motion takes
//  place in the plane of the paper, as the Z coordinate is ignored.
// Two Bezier curves bounding the leaf are generated, controlled by
//  the following parameters:
//	theta	- divergence angle from the branch
//	phi	- leaf tip angle
//	beta	- relative offset along branch
//	lambda	- base width
void PSGenerator::leaf(Turtle &t, float length)
{
    static float theta	= 45;
    static float phi	= 5;
    static float beta	= 0.5;
    static float lambda = 0.2;
    static float dudv	= tan(dtor(theta));
    static float tanphi = tan(dtor(phi));

    Vector center = t.location();
    Vector v = t.H();		    // Heading vector
    Vector u = -t.L();		    // Right vector
    float width = t.linewidth();

    // Control points for leaf top
    Vector c1 = center + (width/2) * u;
    Vector c2 = c1 + (length/3) * u + (length/3) * dudv * v;
    Vector c3 = c1 + (length*(2.0/3)) * u + beta * length * v;
    Vector c4 = c1 + length * u + beta * length * v;

    // Control points for leaf bottom
    Vector d1 = c1 - lambda * length * v;
    Vector d2 = c2 - lambda * length * v;
    Vector d3 = c4 - (length/3) * u - tanphi * v;
    //Vector d4 = c4;

    // Transform all the points from modelling to viewing space
    c1 = view.transform(c1);
    c2 = view.transform(c2);
    c3 = view.transform(c3);
    c4 = view.transform(c4);
    d1 = view.transform(d1);
    d2 = view.transform(d2);
    d3 = view.transform(d3);

    // Generate a closed region defined by two Bezier curves
    start_graphics(t);

    *out << c1(0) << ' ' << c1(1) << " m\n";
    *out << c2(0) << ' ' << c2(1) << ' '
	 << c3(0) << ' ' << c3(1) << ' '
	 << c4(0) << ' ' << c4(1) << " curveto\n";
    *out << d3(0) << ' ' << d3(1) << ' '
	 << d2(0) << ' ' << d2(1) << ' '
	 << d1(0) << ' ' << d1(1) << " curveto\n";
    *out << "fill\n";

    // flush_graphics done implicitly by 'fill'
}

// Generate a flowering apex. The starting point is 'start' and
//  the apex nominally extends to the current location. 'length'
//  is the distance between the two.
// The following parameters control the apex arrowhead:
//	beta	- length of arrowhead relative to apex length
//	theta	- angle of arrowhead tip
//	phi	- angle of arrowhead root
void PSGenerator::apex(Turtle &t, Vector &start, float length)
{
    static float beta  = 0.5;
    static float theta = 10;
    static float phi   = 30;
    static float k     = beta * tan(dtor(theta));
    static float m     = k * beta * tan(dtor(phi));

    start_graphics(t);
    outpoint(*out, view.transform(start));
    *out << " m\n";
    outpoint(*out, view.transform(t.location()));
    *out << " l\n";
    flush_graphics(t);

    // Now generate an arrowhead to indicate the apex
    Vector center = t.location();
    Vector v = t.H();		    // Heading vector
    Vector u = -t.L();		    // Right vector

    // Arrowhead vertices
    Vector tip	 = center + beta * length * v;
    Vector rside = center + k * length * u;
    Vector root  = center - m * length * v;
    Vector lside = center - k * length * u;

    // Transform vertices from modelling to viewing space
    tip   = view.transform(tip);
    rside = view.transform(rside);
    root  = view.transform(root);
    lside = view.transform(lside);

    // Generate a polygon defining the arrowhead
    start_graphics(t);

    outpoint(*out, tip);   *out << " m\n";
    outpoint(*out, rside); *out << " l\n";
    outpoint(*out, root);  *out << " l\n";
    outpoint(*out, lside); *out << " l\n";
    *out << " fill\n";

    // flush_graphics done implicitly by 'fill'
}

void PSGenerator::draw_object(Turtle &t, Module &m) {
    (void)t;
    // Does practically nothing at present
    *out << "% draw_object: " << m << '\n';
}

void PSGenerator::set_color(Turtle &t) {
    *out << t.linecolor().graylevel() << " c\n";
}

void PSGenerator::set_width(Turtle &t) {
    *out << t.linewidth() << " w\n";
}
