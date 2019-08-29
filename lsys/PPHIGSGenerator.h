/* PPHIGSGenerator.h - class definition for PPHIGS database generator
 *
 * $Id: PPHIGSGenerator.h,v 1.2 91/03/20 10:41:53 leech Exp Locker: leech $
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
 * $Log:	PPHIGSGenerator.h,v $
 * Revision 1.2  91/03/20  10:41:53  leech
 * Better color support. Made some output functions into class methods.
 * 
 * Revision 1.1  91/03/18  16:30:11  leech
 * Initial revision
 *
 */

#ifndef PPHIGS_GENERATOR_H
#define PPHIGS_GENERATOR_H

#include "DBGenerator.h"

// A class to generate a database; uses virtual methods and should be
//  subclassed for a specific type of database, e.g. PostScript or
//  PPHIGS databases.
class PPHIGSGenerator : public DBGenerator {
protected:
    int      nfacets;
    float   *costab, *sintab;
    Vector  *n, *p;		// Points and normals of a twig

    void out_color(char *, const Vector &, const Vector &);
    void out_p(const Vector &);
    void out_p_n(const Vector &p, const Vector &n);
    void out_rect(
	const Vector &, const Vector &,
	const Vector &, const Vector &,
	const Vector &, const Vector &,
	const Vector &, const Vector &);
    void out_sphere(const Vector &, float);

public:
    PPHIGSGenerator(ofstream *o, Camera &v, int nfacets = 6);

    // Functions to provide bracketing information
    void prelude(Turtle &t);
    void postscript(Turtle &t);

    // Functions to start/end a stream of graphics
    void start_graphics(Turtle &t);
    void flush_graphics(Turtle &t);

    // Functions to draw objects in graphics mode
    void polygon(Turtle &t, Polygon &p);
    void lineto(Turtle &t);
    void flower(Turtle &t, float radius);
    void leaf(Turtle &t, float length);
    void apex(Turtle &t, Vector &start, float length);
    void draw_object(Turtle &t, Module &m);

    // Functions to change rendering parameters
    void set_color(Turtle &t);
    void set_width(Turtle &t);
};

#endif /*PPHIGS_GENERATOR_H*/

