/* GenericGenerator.c - methods for generic database generator
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
 * $Log:	GenericGenerator.c,v $
 * Revision 1.3  91/03/20  10:38:00  leech
 * Better color support.
 * 
 * Revision 1.2  90/10/12  18:48:04  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: GenericGenerator.c,v 1.3 91/03/20 10:38:00 leech Exp Locker: leech $";

#include "global_headers.h"
#include "vector.h"
#include "Turtle.h"
#include "GenericGenerator.h"

void GenericGenerator::prelude(Turtle &t) {
    (void)t;

    DBGenerator::prelude(t);
}

void GenericGenerator::postscript(Turtle &t) {
    (void)t;

    *out << "Camera: " << view << '\n';

    DBGenerator::postscript(t);
}

// Called when starting a new stream of graphics
void GenericGenerator::start_graphics(Turtle &t) {
    (void)t;
}

// Called when ending a stream of graphics, to display it
void GenericGenerator::flush_graphics(Turtle &t) {
    (void)t;
}

void GenericGenerator::polygon(Turtle &t, Polygon &p) {
    PolygonIterator pi(p);
    Vector *vec;
    int     i;

    start_graphics(t);
    // Draw the polygon
    *out << "polygon " << p.size() << '\n';
    for (i = 0, vec = pi.first(); vec != NULL; vec = pi.next(),i++) {
	*out << "\tvertex " << i << ' ' << *vec << '\n';
    }
}

void GenericGenerator::lineto(Turtle &t) {
    Vector start = currentpos;
    Vector end = t.location();

    *out << "twig " << start << ' ' << end << '\n';

    DBGenerator::lineto(t);
}

void GenericGenerator::flower(Turtle &t, float radius) {
    *out << "flower " << t.location() << " r = " << radius << '\n';
}

void GenericGenerator::leaf(Turtle &t, float length) {
    *out << "leaf " << t.location() << " length = " << length << '\n';
}

void GenericGenerator::apex(Turtle &t, Vector &start, float length) {
    *out << "apex " << start << " to " << t.location() << " length = "
	 << length << '\n';
}

void GenericGenerator::draw_object(Turtle &t, Module &m) {
    *out << "draw obj " << m << '\n'
	 << "\ttranslation = " << t.location() << '\n'
	 << "\trotation =\n" << t.orientation() << '\n';
}

void GenericGenerator::set_color(Turtle &t) {
    *out << "color " << t.linecolor() << '\n';
}

void GenericGenerator::set_width(Turtle &t) {
    *out << "width " << t.linewidth() << '\n';
}
