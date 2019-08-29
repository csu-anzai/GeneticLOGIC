/* DBGenerator.h - class definition for abstract database generators
 *
 * $Id: DBGenerator.h,v 1.3 91/03/20 10:39:07 leech Exp Locker: leech $
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
 * $Log:	DBGenerator.h,v $
 * Revision 1.3  91/03/20  10:39:07  leech
 * Added set_name() method. Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:02  leech
 * First public release.
 *
 */

#ifndef DB_GENERATOR_H
#define DB_GENERATOR_H

#include "Module.h"
#include "Polygon.h"
#include "Turtle.h"

#ifdef __GNUG__
#define pure_virtual { cerr << "Fatal! pure virtual function called!\n"; }
#else
// AT&T 2.0 & compatible compilers
#define pure_virtual = 0
#endif /*__GNUG__*/

// A class to generate a database; uses virtual methods and should be
//  subclassed for a specific type of database, e.g. PostScript or
//  PPHIGS databases.
class DBGenerator {
protected:
    Camera    view;
    ofstream *out;
    Vector    currentpos;
    boolean   lastmove;     // Was last move/draw a move?
    char     *object_name;  // Name of generated object

public:
    DBGenerator(ofstream *o, Camera &v) : view(v) {
	object_name = "null_object";
	out = o;
	lastmove = true;
    }

    void set_name(char *name);

    virtual void output_failed();

    // Functions to provide bracketing information
    virtual void prelude(Turtle &t);
    virtual void postscript(Turtle &t);

    // Functions to start/end a stream of graphics
    virtual void start_graphics(Turtle &t) pure_virtual;
    virtual void flush_graphics(Turtle &t) pure_virtual;

    // Functions to draw objects in graphics mode
    virtual void polygon(Turtle &t, Polygon &p) pure_virtual;
    virtual void moveto(Turtle &t);
    virtual void lineto(Turtle &t);
    virtual void flower(Turtle &t, float radius) pure_virtual;
    virtual void leaf(Turtle &t, float length) pure_virtual;
    virtual void apex(Turtle &t, Vector &start, float length) pure_virtual;
    virtual void draw_object(Turtle &t, Module &m) pure_virtual;

    // Functions to change rendering parameters
    virtual void set_color(Turtle &t) pure_virtual;
    virtual void set_width(Turtle &t) pure_virtual;
};

#endif /*DB_GENERATOR_H*/
