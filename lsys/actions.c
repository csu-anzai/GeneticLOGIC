/* actions.c - actions taken in interpretation of an L-system to
 *  produce PostScript.
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
 * $Log:	actions.c,v $
 * Revision 1.3  91/03/20  10:36:16  leech
 * Better color support.
 * 
 * Revision 1.2  90/10/12  18:48:16  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: actions.c,v 1.3 91/03/20 10:36:16 leech Exp Locker: leech $";

#include "global_headers.h"
#include "Symtab.h"
#include "Value.h"
#include "Expression.h"
#include "Module.h"
#include "Turtle.h"
#include "Polygon.h"
#include "DBGenerator.h"
#include "actions.h"

// The stack used by { } commands to define polygons
// The stack can get quite deep in recursive L-system productions, thus
//  the depth of 100 (probably should use a dynamically allocated list).
static int polyptr = -1;
static const int max_polys = 100;
Polygon *polystack[max_polys];

static enum { START, DRAWING, POLYGON } State = START;

// Common move/draw routine
static void moveturtle(Turtle &t, int nargs, float args[maxargs]) {
    if (nargs == 0)
	t.move();
    else
	t.move(args[0]);
}

// Set line width only if changed too much
static void setlinewidth(Turtle &t, DBGenerator &db) {
    const float epsilon = 1e-6;
    static float lastlinewidth = -1;

    // Don't bother changing line width if 'small enough'.
    // This is an optimization to handle e.g. !(w)[!(w/2)F][!(w/2)F]
    //	sort of cases, which happen a lot with trees.
    if (fabs(t.linewidth() - lastlinewidth) < epsilon)
	return;

    if (State == DRAWING) {
	db.flush_graphics(t);
	State = START;
    }

    db.set_width(t);
    lastlinewidth = t.linewidth();
}

// Set color only if changed
static void setcolor(Turtle &t, DBGenerator &db) {
    static Color lastcolor(-1);

    // Don't change color if not needed, again an optimization
    if (t.linecolor() == lastcolor)
	return;

    if (State == DRAWING) {
	db.flush_graphics(t);
	State = START;
    }

    db.set_color(t);
    lastcolor = t.linecolor();
}

// Add an edge to the current polygon while moving
static void add_poly_edge(Turtle &t, int nargs, float args[maxargs])
{
    // Add an edge to the current polygon
    PolygonIterator pi(*polystack[polyptr]);
    Vector *v = pi.last();

    // See if the starting point needs to be added (only if
    //	it's different from the last point defined in
    //	the polygon, or there are no points defined in the polygon
    //	yet).
    Vector *p = new Vector(t.location());
    if (v == NULL || *v != *p) {
	PDEBUG(PD_INTERPRET, cerr << "add_poly_edge: adding first vertex " << *p << endl);
	polystack[polyptr]->append(p);
    }

    // Move and add the ending point to the polygon.
    moveturtle(t, nargs, args);
    PDEBUG(PD_INTERPRET, cerr << "add_poly_edge: adding last vertex  " << t.location() << endl);
    polystack[polyptr]->append(new Vector(t.location()));
}

// {	Start a new polygon
ACTION(StartPolygon) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "StartPolygon  " << endl);

    if (State == DRAWING)
	db.flush_graphics(t);

    State = POLYGON;
    polyptr++;
    if (polyptr >= max_polys) {
	cerr << "StartPolygon: polygon stack filled" << endl;
	return;
    }

    polystack[polyptr] = new Polygon;
}

// .	Add a vertex to the current polygon
ACTION(PolygonVertex) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "PolygonVertex " << endl);

    if (State != POLYGON) {
	cerr << "Illegal: Polygon vertex while not in polygon mode\n";
	return;
    }

    if (polyptr < 0) {
	cerr << "PolygonVertex: no polygon being defined" << endl;
	return;
    } else if (polyptr >= max_polys)
	return;     // Already got an error from StartPolygon

    Vector *v = new Vector(t.location());
    polystack[polyptr]->append(v);
}

// G	Move without creating a polygon edge
// This seems like it should be illegal outside a polygon, but
//  the rose leaf example in the text uses G in this context.
//  Until the behavior is specified, just move the turtle without
//  other effects.
ACTION(PolygonMove) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "PolygonMove   " << endl);

    //if (State != POLYGON) {
    //	  cerr << "Illegal: Polygon move-without-draw while not in polygon mode\n";
    //	  return;
    //}

    moveturtle(t, nargs, args);
}

// }	Close the current polygon
ACTION(EndPolygon) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "EndPolygon    " << endl);

    if (State != POLYGON || polyptr < 0) {
	cerr << "EndPolygon: no polygon being defined" << endl;
	return;
    }

    if (polyptr >= max_polys) {
	cerr << "EndPolygon: polygon stack too deep, polygon lost" << endl;
	polyptr--;
    } else {
	db.polygon(t, *polystack[polyptr]);
	delete polystack[polyptr];
	polyptr--;

	// Return to start state if no more polys on stack
	if (polyptr < 0)
	    State = START;
    }
}

// F(l) Move while drawing
// Fr(l), Fl(l) - Right and Left edges respectively
ACTION(Draw) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Draw          " << endl);

    if (State == START) {
	db.start_graphics(t);
	State = DRAWING;
    }

    if (State == DRAWING) {
	moveturtle(t, nargs, args);
	db.lineto(t);
    } else if (State == POLYGON) {
	add_poly_edge(t, nargs, args);
    }
}

// f(l) Move without drawing
ACTION(Move) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Move          " << endl);

    if (State == DRAWING || State == START) {
	moveturtle(t, nargs, args);
	db.moveto(t);
    } else if (State == POLYGON) {
	add_poly_edge(t, nargs, args);
    }
}

// A	Draw a flowering apex (alias for forward, right now)
ACTION(FloweringApex) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "FloweringApex " << endl);

    if (State == POLYGON) {
	cerr << "FloweringApex: undefined within polygon\n";
	return;
    } else if (State == DRAWING) {
	db.flush_graphics(t);
	State = START;
    }

    // Start of internode
    Vector start = t.location();

    // Move turtle to end of internode
    moveturtle(t, nargs, args);

    db.apex(t, start, (nargs > 0) ? args[0] : 1);
}

// I	Draw an internode (alias for forward, right now)
ACTION(Internode) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Internode     " << endl);

    // For now
    Draw(mi, t, db, nargs, args);
}

// K(r) Draw a 'flower' (actually, a filled circle of specified radius)
ACTION(Flower) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Flower        " << endl);

    if (State == POLYGON) {
	cerr << "Flower: undefined within polygon\n";
	return;
    } else if (State == DRAWING) {
	db.flush_graphics(t);
	State = START;
    }

    // Default flower scaling relative to unit movement
    const float scale = 0.15;
    float radius;

    // No longer in graphics state
    if (nargs == 0)
	radius = scale;
    else
	radius = args[0] * scale;

    db.flower(t, radius);
}

// L(l) Draw a 'leaf' (actually, a filled polygon of specified length)
ACTION(Leaf) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Leaf          " << endl);

    if (State == POLYGON) {
	cerr << "Leaf: undefined within polygon\n";
	return;
    } else if (State == DRAWING) {
	db.flush_graphics(t);
	State = START;
    }

    // Default leaf scaling relative to unit movement
    const float scale = 0.5;
    float length;

    // No longer in graphics state
    if (nargs == 0)
	length = scale;
    else
	length = args[0] * scale;

    // To be done
    db.leaf(t, length);
}

// -(t) Turn right: negative rotation about Z
ACTION(TurnRight) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "TurnRight     " << endl);

    if (nargs == 0)
	t.turn(Turtle::negative);
    else
	t.turn(-dtor(args[0]));
}

// +(t) Turn left; positive rotation about Z
ACTION(TurnLeft) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "TurnLeft      " << endl);

    if (nargs == 0)
	t.turn(Turtle::positive);
    else
	t.turn(dtor(args[0]));
}

// ^(t) Pitch up; negative rotation about Y
ACTION(PitchUp) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "PitchUp       " << endl);

    if (nargs == 0)
	t.pitch(Turtle::negative);
    else
	t.pitch(-dtor(args[0]));
}

// &(t) Pitch down; positive rotation about Y
ACTION(PitchDown) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "PitchDown     " << endl);

    if (nargs == 0)
	t.pitch(Turtle::positive);
    else
	t.pitch(dtor(args[0]));
}

// /(t) Roll right; positive rotation about X
ACTION(RollRight) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "RollRight     " << endl);

    if (nargs == 0)
	t.roll(Turtle::positive);
    else
	t.roll(dtor(args[0]));
}

// \(t) Roll left; negative rotation about X
ACTION(RollLeft) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "RollLeft      " << endl);

    if (nargs == 0)
	t.roll(Turtle::negative);
    else
	t.roll(-dtor(args[0]));
}

// |	Turn around
ACTION(Reverse) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Reverse       " << endl);

    t.reverse();
}

// [	Push turtle state
ACTION(Push) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Push          " << endl);

    t.push();
}

// ]	Pop turtle state
ACTION(Pop) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "Pop           " << endl);

    t.pop();

    // Look ahead; if the next module is also a pop, don't do anything.
    // Otherwise, reset the line width, color, and position
    // This is an optimization to handle deep nesting ([[...][...[...]]]),
    //	which happens a lot with trees.

    Module *obj = mi.next();
    if (obj) {
	if (strcmp(obj->name(), "]")) {
	    setlinewidth(t, db);
	    setcolor(t, db);
	    db.moveto(t);
	}

	// Back off one step so the next module is interpreted properly
	(void)mi.previous();
    }
}

// $	Roll to horizontal plane (pg. 57)
ACTION(RollHorizontal) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "RollHorizontal" << endl);

    t.roll_horizontal();
}

// !(d) Set line width
ACTION(LineWidth) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "LineWidth     " << endl);

    if (nargs == 0)
	t.set_width();
    else
	t.set_width(args[0]);

    setlinewidth(t, db);
}

// '	Increment color index
// '(n) Set color index
// '(r,g,b) Set RGB color
ACTION(ChangeColor) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "ChangeColor   " << endl);

    if (nargs >= 3)
	t.set_color(Vector(args[0], args[1], args[2]));
    else if (nargs > 0)
	t.set_color((int)args[0]);
    else
	t.increment_color();

    setcolor(t, db);
}

// ~	Draw the following object at the turtle's position and frame
// Needs a standardized object file format for this.
// Should leave graphics mode before drawing object.
ACTION(DrawObject) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "DrawObject    " << endl);
    Module *obj = mi.next();

    if (obj) {
	db.draw_object(t, *obj);
    }
}

// %	Truncate a branch
// E.g., ignore all modules up to the next ]
// Note that this code is cloned from right-context
//  matching in Production::matches()
ACTION(CutBranch) {
    ARGSUSED;
    PDEBUG(PD_INTERPRET, cerr << "CutBranch     " << endl);

    Module *obj;
    int brackets;

    // Must find a matching ]; skip anything else including
    //	bracketed substrings.
    for (brackets = 0, obj = mi.next(); obj; obj = mi.next()) {
	if (obj->name() == RBRACKET) {
	    if (brackets-- == 0)
		break;
	} else if (obj->name() == LBRACKET)
	    brackets++;
    }

    // Back off one step so the pop itself is handled by interpret()
    if (obj)
	(void)mi.previous();
}

// t	Enable/disable tropism corrections after each move
// t(x,y,z,e)	- enable tropism; tropism vector is T = (x,y,z),
//  e is `suspectibility parameter', preferably between 0 and 1.
// t(0)		- disable tropism
// t(1)		- reenable tropism with last (T,e) parameters

static void tropism_error() {
    cerr << "Tropism: expect arguments (x,y,z,e) or (e)." << endl;
}

ACTION(Tropism) {
    ARGSUSED;

    if (nargs == 0) {
	tropism_error();
	return;
    }

    if (nargs == 1) {
	// Only one parameter; disable tropism if it equals 0,
	//  otherwise enable tropism with the specified parameter.

	float e = args[0];
	if (e == 0)
	    t.disable_tropism();
	else if (e == 1) {
	    t.set_tropism_param(e);
	    t.enable_tropism();
	}
	return;
    }

    if (nargs < 4) {
	tropism_error();
	return;
    }

    // Construct the tropism vector
    t.set_tropism_vector(Vector(args[0], args[1], args[2]));
    t.set_tropism_param(args[3]);
    t.enable_tropism();
}
