/* DBGenerator.c - methods for abstract database generator (do nothing)
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
 * $Log:	DBGenerator.c,v $
 * Revision 1.3  91/03/20  10:37:32  leech
 * Add set_name() method.
 * 
 * Revision 1.2  90/10/12  18:47:21  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: DBGenerator.c,v 1.3 91/03/20 10:37:32 leech Exp Locker: leech $";

#include <osfcn.h>
#include "global_headers.h"
#include "vector.h"
#include "Turtle.h"
#include "DBGenerator.h"

void DBGenerator::output_failed() {
    cerr << "Fatal error in output generator, aborting\n";
    exit(1);
}

void DBGenerator::set_name(char *name) {
    object_name = new char[strlen(name)+1];
    strcpy(object_name, name);
}

void DBGenerator::prelude(Turtle &t) {
    set_color(t);
    set_width(t);
    if (out->bad())
	output_failed();
}

void DBGenerator::postscript(Turtle &t) {
    (void)t;
    if (out->bad())
	output_failed();
    out->close();
}

void DBGenerator::moveto(Turtle &t) {
    currentpos = t.location();
    lastmove = true;
}

void DBGenerator::lineto(Turtle &t) {
    currentpos = t.location();
    lastmove = false;
}
