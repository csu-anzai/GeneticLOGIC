/* vector.h - class definitions for vector and transformation Matrix classes.
 *
 * $Id: vector.h,v 1.3 91/03/20 10:41:13 leech Exp Locker: leech $
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
 * $Log:	vector.h,v $
 * Revision 1.3  91/03/20  10:41:13  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:26  leech
 * First public release.
 *
 */

#ifndef VECTOR_H
#define VECTOR_H

#include "streamio.h"
#include <math.h>
#include "boolean.h"

inline float sqr(float x) { return x * x; }

// Conversions between degrees and radians
inline float dtor(float deg) {
    return deg * M_PI / 180.0;
}

inline float rtod(float rad) {
    return rad * 180.0 / M_PI;
}

// This is a Cartesian vector class.
// The usual scalar and vector operators are defined.
// Cross product is denoted by a ^ b.
#define Point Vector
class Vector {
    float x[3];
public:
    float &operator[](int i) { return x[i]; }
    float operator()(int i) const { return x[i]; }

    Vector() {};
    Vector(const Vector &v) {
	x[0] = v(0);
	x[1] = v(1);
	x[2] = v(2);
    }
    Vector(float xval, float yval, float zval) {
	x[0] = xval;
	x[1] = yval;
	x[2] = zval;
    };

    operator float*() { return x; }

    float magnitude() {
	return sqrt(sqr(x[0]) + sqr(x[1]) + sqr(x[2]));
    }

    Vector &normalize() {
	float mag = this->magnitude();

	if (mag != 0.0) {
	    mag = 1 / mag;
	    x[0] *= mag;
	    x[1] *= mag;
	    x[2] *= mag;
	}

	return *this;
    }

    Vector &operator+=(const Vector &v) {
	x[0] += v(0);
	x[1] += v(1);
	x[2] += v(2);
	return *this;
    }

    Vector &operator-=(const Vector &v) {
	x[0] -= v(0);
	x[1] -= v(1);
	x[2] -= v(2);
	return *this;
    }

    Vector &operator*=(float s) {
	x[0] *= s;
	x[1] *= s;
	x[2] *= s;
	return *this;
    }

    Vector &operator/=(float s) {
	x[0] /= s;
	x[1] /= s;
	x[2] /= s;
	return *this;
    }

    friend Vector operator-(const Vector &);			// Negation
    friend Vector operator*(float, const Vector &);		// Scalar multiply
    friend Vector operator/(const Vector &, float);		// Scalar division
    friend Vector operator+(const Vector &, const Vector &);	// Vector addition
    friend Vector operator-(const Vector &, const Vector &);	// Vector subtraction
    friend Vector operator^(const Vector &, const Vector &);	// Vector cross product
    friend float  operator*(const Vector &, const Vector &);	// Vector inner product
};

inline Vector operator-(const Vector &v)
{
    return Vector(-v(0), -v(1), -v(2));
}

inline Vector operator*(float s, const Vector &v)
{
    return Vector(s * v(0), s * v(1), s * v(2));
}

inline Vector operator/(const Vector &v, float s)
{
    float recip = 1 / s;
    return Vector(v(0) * recip, v(1) * recip, v(2) * recip);
}

inline Vector operator+(const Vector &a, const Vector &b)
{
    return Vector(a(0) + b(0), a(1) + b(1), a(2) + b(2));
}

inline Vector operator-(const Vector &a, const Vector &b)
{
    return Vector(a(0) - b(0), a(1) - b(1), a(2) - b(2));
}

inline Vector operator^(const Vector &a, const Vector &b)
{
    return Vector(a(1) * b(2) - a(2) * b(1),
		  a(2) * b(0) - a(0) * b(2),
		  a(0) * b(1) - a(1) * b(0));
}

inline float operator*(const Vector &a, const Vector &b)
{
    return a(0) * b(0) + a(1) * b(1) + a(2) * b(2);
}

inline int operator==(const Vector &a, const Vector &b)
{
    return (a(0) == b(0)) && (a(1) == b(1)) && (a(2) == b(2));
}

inline int operator!=(const Vector &a, const Vector &b) {
    return !(a == b);
}

ostream &operator<<(ostream &s, const Vector &v);

// 3x4 transformation matrix (no perspective)
class Matrix {
    float   m[3][4];

public:
    enum initialize { columns, rows };
    enum axis { x, y, z };

    Matrix() {}
    Matrix(initialize flag, const Vector &u, const Vector &v, const Vector &w);

    float *operator[](int i) { return m[i]; }
    Matrix &zero();
    Matrix &identity();
    Matrix &rotate(axis a, float alpha);
    Matrix &rotate(const Vector &a, float alpha);
    Matrix &translate(const Vector &t);
    Matrix &reverse();
    Vector  operator*(const Vector &s);
    Matrix  operator*(Matrix &b);
};

Matrix view_matrix(const Point &eye, const Point &lookat, const Vector &vup);

ostream &operator<<(ostream &s, Matrix &m);

class BoundingBox {
    Vector  vmin;
    Vector  vmax;

public:
    BoundingBox() : vmin(0,0,0), vmax(0,0,0) { }
    BoundingBox(const Vector &p) { vmin = p; vmax = p; }
    void expand(const Vector &p);
    Vector min() { return vmin; }
    Vector max() { return vmax; }
    BoundingBox transform(Matrix &m);
};

ostream &operator<<(ostream &s, BoundingBox &m);

class Camera {
    Matrix  viewmat;	    // Viewing matrix
    boolean perspective;    // true if perspective is added
    float   fov;	    // field of view, in degrees
    float   depthscale;     // cot(fov/2) scaling factor
public:
    Camera(Matrix &view, float fov = 0);    // no fov -> orthographic projection
    Vector transform(const Vector &p);
    Matrix view_matrix() { return viewmat; }
    float field_of_view() { return fov; }

    friend ostream &operator<<(ostream &s, Camera &m);
};

#endif /*VECTOR_H*/
