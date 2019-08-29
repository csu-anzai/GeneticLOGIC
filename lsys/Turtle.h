/* Turtle.h - class definition for a 3D turtle geometry engine.
 *
 * $Id: Turtle.h,v 1.3 91/03/20 10:40:26 leech Exp Locker: leech $
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
 * $Log:	Turtle.h,v $
 * Revision 1.3  91/03/20  10:40:26  leech
 * Better color support.
 * 
 * Revision 1.2  90/10/12  18:48:14  leech
 * First public release.
 *
 */

#ifndef TURTLE_H
#define TURTLE_H
#include "vector.h"

// This should be restricted to the scope of class Turtle
struct TropismInfo {
    Vector  t;	    // Tropism vector
    float   e;	    // Suspectibility parameter
    boolean flag;   // Whether to apply it
};

enum ColorType { COLOR_INDEX, COLOR_RGB };
struct Color {
    ColorType type;
    union {
	int	index;
	char	rgb[sizeof(Vector)];
    } c;

    Color() { }
    Color(int i) {
	type = COLOR_INDEX;
	c.index = i;
    }
    Color(const Vector &v) {
	type = COLOR_RGB;
	*(Vector *)c.rgb = v;
    }

    float graylevel();		// Force interpretation as gray scale [0..1]
    Vector rgbcolor();		// Force interpretation as RGB [0..1]^3
    int operator==(Color &b);
};

ostream &operator<<(ostream &o, Color &c);

class Turtle {
    Matrix  frame,   *f;    // Orientation frame of turtle
    Vector  pos,     *p;    // Position of turtle
    TropismInfo
	    tropism, *t;    // Tropism
    float   width,   *w;    // Line width
    Color   color,   *c;    // Color index

    int     stackptr;	    // Stack depth

    BoundingBox
	    bbox;	    // Bounding box of turtle path

    Vector  gravity;	    // Antigravity vector

    float
	    width_scale,    // Amount to scale linewidth by
	    default_turn;   // Default turn angle

public:
    enum direction { positive, negative };

    Turtle(float turn = 90, float width_scale = 1);

    void get_defaults(float &w_scale, float &delta);
    void set_defaults(float w_scale, float delta);
    BoundingBox bounds() { return bbox; }

    // Methods to modify turtle parameters
    Vector H();			    // Get heading
    void   set_H(const Vector &h);  // Set heading
    Vector L();			    // Get left
    void   set_L(const Vector &l);  // Set left
    Vector U();			    // Get up
    void   set_U(const Vector &u);  // Set up
    Matrix orientation() {	    // Get frame
	return frame;
    }

    void   set_frame(const Matrix &);
    void   set_gravity(const Vector &gvec);

    // Functions for enabling/disabling application of tropism
    //	after each segment is drawn
    void   set_tropism_vector(const Vector &t);
    void   set_tropism_param(float e);
    void   disable_tropism();
    void   enable_tropism();

    void   set_width(float w = 1);
    void   set_color(int c = 0);
    void   set_color(const Vector &c);
    void   increment_color();

    Vector location() { return pos; }	    // Position
    float  linewidth() { return width; }    // Line width
    Color  linecolor() { return color; }    // Line color

    // The turtle may be instructed to rotate a default amount
    //	in the positive or negative direction, or a specified
    //	number of degrees.
    void turn(direction d);
    void turn(float alpha);

    void pitch(direction d);
    void pitch(float alpha);

    void roll(direction d);
    void roll(float alpha);

    void reverse();		// Spin around 180 degrees
    void roll_horizontal();	// Align up vector with v

    void move(float distance = 1);

    void push();
    void pop();

    friend ostream &operator<<(ostream &o, Turtle &t);
};

ostream &operator<<(ostream &o, TropismInfo &t);
#endif /*TURTLE_H*/
