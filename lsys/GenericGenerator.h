/* GenericGenerator.h - class definition for generic database generator
 *
 * $Id: GenericGenerator.h,v 1.2 90/10/12 18:48:04 leech Exp Locker: leech $
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
 * $Log:	GenericGenerator.h,v $
 * Revision 1.2  90/10/12  18:48:04  leech
 * First public release.
 * 
 */

#ifndef GENERIC_GENERATOR_H
#define GENERIC_GENERATOR_H

#include "DBGenerator.h"

// A class to generate a database; uses virtual methods and should be
//  subclassed for a specific type of database, e.g. PostScript or
//  PPHIGS databases.
class GenericGenerator : public DBGenerator {
public:
    GenericGenerator(ofstream *o, Camera &v) : DBGenerator(o,v) { }

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

#endif /*GENERIC_GENERATOR_H*/

