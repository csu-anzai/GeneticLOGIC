/* BLFGenerator.h - class definition for BLF database generator
 *
 * $Id: BLFGenerator.h,v 1.1 91/03/20 10:45:39 leech Exp Locker: leech $
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
 * $Log:	BLFGenerator.h,v $
 * Revision 1.1  91/03/20  10:45:39  leech
 * Initial revision
 * 
 *
 */

#ifndef BLF_GENERATOR_H
#define BLF_GENERATOR_H

#include "DBGenerator.h"

// A class to generate a database; uses virtual methods and should be
//  subclassed for a specific type of database, e.g. PostScript or
//  BLF databases.
class BLFGenerator : public DBGenerator {
protected:
    int     pointnum;	       // Next sequential point to generate
    int     out_point(const Vector &p);
    void    out_matrix(Turtle &t);

public:
    BLFGenerator(ofstream *o, Camera &v);

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

#endif /*BLF_GENERATOR_H*/

