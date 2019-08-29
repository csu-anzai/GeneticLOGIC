/* PSGenerator.h - class definition for PostScript database generator
 *
 * $Id: PSGenerator.h,v 1.2 90/10/12 18:48:09 leech Exp Locker: leech $
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
 * $Log:	PSGenerator.h,v $
 * Revision 1.2  90/10/12  18:48:09  leech
 * First public release.
 * 
 */

#ifndef PS_GENERATOR_H
#define PS_GENERATOR_H

#include "DBGenerator.h"

// A PostScript database generator
class PSGenerator : public DBGenerator {
protected:
    int nseg;		// # of segments in current path
    boolean newpath;	// Has a 'newpath' been done?
    boolean autoscale;	// Is autoscaling enabled?

public:
    PSGenerator(ofstream *o, Camera &v, boolean scale) :
	DBGenerator(o, v) {
	nseg = 0;
	newpath = false;
	autoscale = scale;
    }

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
    virtual void draw_object(Turtle &t, Module &m);

    // Functions to change rendering parameters
    void set_color(Turtle &t);
    void set_width(Turtle &t);
};
#endif /*PS_GENERATOR_H*/
