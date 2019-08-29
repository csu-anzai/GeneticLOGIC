/* Turtle.c - methods for a 3D turtle geometry engine.
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
 * $Log:	Turtle.c,v $
 * Revision 1.3  91/03/20  10:36:50  leech
 * Better color support. Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:13  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: Turtle.c,v 1.3 91/03/20 10:36:50 leech Exp Locker: leech $";

#include "streamio.h"
#include "Turtle.h"

// Default stack depth
const int stackdepth = 100;

// Line width relative to unit move
const float relative_line_width = 0.01;

Turtle::Turtle(float delta, float wscale) :
    pos(0,0,0), bbox(pos)
{
    // Allocate and initialize turtle state stack
    stackptr = 0;
    f = new Matrix[stackdepth];
    p = new Vector[stackdepth];
    t = new TropismInfo[stackdepth];
    w = new float[stackdepth];
    c = new Color[stackdepth];

    set_defaults(wscale, delta);

    // Set up initial frame and position, moving in +X with up in Z
    frame.identity();
    set_gravity(H());

    // Default tropism vector is towards ground, but tropism is disabled
    set_tropism_vector(-H());
    set_tropism_param(0.2);
    disable_tropism();

    set_width(1);
    set_color(0);
}

// Get the default drawing parameters
void Turtle::get_defaults(float &wscale, float &delta) {
    wscale = width_scale;
    delta = rtod(default_turn);
}

// Set the default drawing parameters
void Turtle::set_defaults(float wscale, float delta) {
    width_scale  = wscale;
    default_turn = dtor(delta);
}

// Heading is first column of frame
Vector Turtle::H() {
    return Vector(frame[0][0], frame[1][0], frame[2][0]);
}

// Set heading
void Turtle::set_H(const Vector &h) {
    frame[0][0] = h(0);
    frame[1][0] = h(1);
    frame[2][0] = h(2);
}

// Left is second column of frame
Vector Turtle::L() {
    return Vector(frame[0][1], frame[1][1], frame[2][1]);
}

// Set left
void Turtle::set_L(const Vector &l) {
    frame[0][1] = l(0);
    frame[1][1] = l(1);
    frame[2][1] = l(2);
}

// Up is third column of frame
Vector Turtle::U() {
    return Vector(frame[0][2], frame[1][2], frame[2][2]);
}

// Set up
void Turtle::set_U(const Vector &u) {
    frame[0][2] = u(0);
    frame[1][2] = u(1);
    frame[2][2] = u(2);
}

// Set the whole frame at once
void Turtle::set_frame(const Matrix &m) {
    frame = m;
}

// Set the antigravity vector
void Turtle::set_gravity(const Vector &gvec) {
    gravity = gvec;
}

void Turtle::set_tropism_vector(const Vector &t) {
    tropism.t = t;
}

void Turtle::set_tropism_param(float e) {
    tropism.e = e;
}

void Turtle::disable_tropism() {
    tropism.flag = false;
}

void Turtle::enable_tropism() {
    tropism.flag = true;
}

// Set line width
void Turtle::set_width(float w) {
    width = w * width_scale * relative_line_width;
}

// Set color index. This is interpreted by the output generator.
// It may index a color map or define a grayscale value.
void Turtle::set_color(int c) {
    color = Color(c);
}

// Set RGB color
void Turtle::set_color(const Vector &c) {
    color = Color(c);
}

// Increment the current color index
void Turtle::increment_color() {
    if (color.type == COLOR_INDEX)
	color.c.index++;
    else
	cerr << "Turtle::increment_color(): current color is RGB, not index!" << endl;
}

// Turn left or right (rotate around the up vector)
void Turtle::turn(direction d) {
    if (d == positive)
	frame.rotate(Matrix::z, default_turn);
    else
	frame.rotate(Matrix::z,-default_turn);
}

void Turtle::turn(float alpha) {
    frame.rotate(Matrix::z, alpha);
}

// Pitch up or down (rotate around the left vector)
void Turtle::pitch(direction d) {
    if (d == positive)
	frame.rotate(Matrix::y, default_turn);
    else
	frame.rotate(Matrix::y,-default_turn);
}

void Turtle::pitch(float alpha) {
    frame.rotate(Matrix::y, alpha);
}

// Roll left or right (rotate around the heading vector)
void Turtle::roll(direction d) {
    if (d == positive)
	frame.rotate(Matrix::x, default_turn);
    else
	frame.rotate(Matrix::x,-default_turn);
}

void Turtle::roll(float alpha) {
    frame.rotate(Matrix::x, alpha);
}

// Spin around 180 degrees
void Turtle::reverse() {
    frame.reverse();
}

// Roll the turtle so the left vector is perpendicular to the antigravity
//  vector (see pg. 57).
void Turtle::roll_horizontal() {
    const float tolerance = 1e-4;
    Vector
	h = H(),
	l = gravity ^ h,
	u;

    // Don't do anything if heading is too close to the antigravity vector
    float m = l.magnitude();
    if (m < tolerance)
	return;

    // new Left vector is normalized v ^ h
    m = 1 / m;
    l[0] *= m;
    l[1] *= m;
    l[2] *= m;

    // new Up vector is fixed by h and l
    u = h ^ l;

    // reset the L and U vectors
    set_L(l);
    set_U(u);
}

// Move along heading vector for distance d
// Default turtle movement is 1 (may be changed by movement scaling)
// Keep track of the motion bounds and apply a tropism correction
//  if that is enabled.
void Turtle::move(float d) {
    pos += d * H();
    bbox.expand(pos);

    // Apply tropism, if enabled
    // This consists of rotating by the vector e (H ^ T)
    if (tropism.flag && (tropism.e != 0)) {
	Vector a = H() ^ tropism.t;
	float m = a.magnitude();

	// This is bogus
	//if (m != 0)
	frame.rotate(a, tropism.e);
    }
}

// Save state
// Handle over/underflow gracefully
void Turtle::push() {
    if (stackptr < 0) {
	cerr << "Turtle::push(): can't push below bottom of stack!" << endl;
    } else if (stackptr < stackdepth - 1) {
	f[stackptr] = frame;
	t[stackptr] = tropism;
	p[stackptr] = pos;
	w[stackptr] = width;
	c[stackptr] = color;
    } else {
	cerr << "Turtle::push(): stack depth exceeded, lost new frame!" << endl;
    }
    stackptr++;
}

// Restore state
// Handle over/underflow gracefully
void Turtle::pop() {
    if (stackptr > stackdepth) {
	cerr << "Turtle::pop(): can't restore lost frame!" << endl;
    } else if (stackptr > 0) {
	frame	 = f[stackptr-1];
	tropism  = t[stackptr-1];
	pos	 = p[stackptr-1];
	width	 = w[stackptr-1];
	color	 = c[stackptr-1];
    } else {
	cerr << "Turtle::pop(): cannot pop frame below bottom of stack!" << endl;
    }
    stackptr--;
}

ostream &operator<<(ostream &o, TropismInfo &t) {
    if (t.flag)
	o << "[enabled";
    else
	o << "[disabled";

    return o << "; vector: " << t.t << " e: " << t.e << " ]";
}

ostream &operator<<(ostream &o, Turtle &t) {
    o << "Turtle:\n"
      << "\tpos =     " << t.location()  << '\n'
      << "\tH   =     " << t.H()	 << '\n'
      << "\tL   =     " << t.L()	 << '\n'
      << "\tU   =     " << t.U()	 << '\n'
      << "\tTropism = " << t.tropism	 << '\n'
      << "\twidth =   " << t.linewidth() << endl;
    return o;
}

// Interpret the color as an intensity; map RGB color to gray.
float Color::graylevel() {
    if (type == COLOR_INDEX)
	return c.index / 100.0;
    else {
	Vector *v = (Vector *)&c.rgb;
	// G = .3R + .6G + .1B
	return .3 * (*v)(0) + .6 * (*v)(1) + .1 * (*v)(2);
    }
}

// Interpret the color as RGB; map color index into gray scale
Vector Color::rgbcolor() {
    if (type == COLOR_INDEX) {
	float g = graylevel();
	return Vector(g,g,g);
    } else
	return *(Vector *)&c.rgb;
}

int Color::operator==(Color &b) {
    if (type != b.type)
	return 0;
    if (type == COLOR_INDEX)
	return c.index == b.c.index;
    else
	return rgbcolor() == b.rgbcolor();
}

ostream &operator<<(ostream &o, Color &c) {
    if (c.type == COLOR_INDEX)
	o << "index = " << c.c.index;
    else
	o << "RGB = " << c.rgbcolor();
    return o;
}

