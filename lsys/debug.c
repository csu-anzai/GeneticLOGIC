/* debug.c - methods for efficient small block allocation and memory
 *  tracing aids.
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
 * $Log:	debug.c,v $
 * Revision 1.3  91/03/20  10:34:21  leech
 * Support for G++.
 * 
 * Revision 1.2  90/10/12  18:48:17  leech
 * First public release.
 *
 */
static char RCSid[] = "$Id: debug.c,v 1.3 91/03/20 10:34:21 leech Exp Locker: leech $";

#ifndef __GNUG__
#include <libc.h>
#else
#include <std.h>
#endif /*__GNUG__*/

#include "debug.h"
#include "streamio.h"

static boolean inited = false;

// Enable memory tracing within global new() and delete() functions.
void memlog_enable(boolean on) {
    inited = on;
}

/* For trapping memory leaks, redefine the global new() and delete()
 *  functions for diagnostics.
 */
#include <assert.h>
#include <malloc.h>

#define DEBUG(code) PDEBUG(PD_MEMLEAK, \
    if (inited == true) {   \
	inited = false;     \
	{ code; }	    \
	inited = true;	    \
    });

// Tuning parameters:
//
// grain_size is the granularity of the small free block list.
//  This should be a multiple of 4 (usually 4).
//
// grain_shift is log2(grain_size), used because we don't trust
//  the compiler to do fast divides of grain_size.
//
// max_grains is the number of entires on the small free block list.
//  Requests of size <= (max_grains * grain_size) are allocated
//  from this list.
//
// chunks is the number of small blocks to allocate at once when
//  the appropriate free list is empty. By allocating a large number
//  of blocks at once, the malloc() header overhead is made
//  insignificant.
//
// Requests are rounded up to the grain size boundary and satisfied
//  from the small free block list if they are sufficiently small.
// It is critical that all pointers obtained in this fashion be freed
//  back to the small block list, or the memory arena will become
//  corrupted by free() of a pointer not allocated by malloc().
// To aid in this process, the small block allocator only comes into
//  play when the overloaded operator new(size_t sz, char *label) is
//  explicitly called; a call to new(size_t sz) just returns a pointer
//  allocated from the system.
// To free small block pointers, call delete_sz() instead of delete.
//
// Unfortunately, enforcement of these routines is not possible. What
//  works best is defining class new() and delete() operators which
//  call the global routines, and not making other use of these routines.
//
// This is all pretty nasty, but it's also far more space efficient than
//  the system malloc(). Similar behavior is available on some malloc()
//  implementations by using the mallopt() function, but this is not
//  available on all systems and still imposes overhead on each block.

const int grain_shift = 2;
const int grain_size = (1 << grain_shift);
const int max_grains = 16;
const int chunks = 25;

// Conversion between grains and bytes
inline int gtob(int g) { return g * grain_size; }
inline int btog(int b) { return (b + grain_size - 1) >> grain_shift; }

// The small free block lists
static int   free_init = 0;
static char *free_list[max_grains+1];

// Initialize the free lists; we don't trust the system to put
//  NULL pointers in each list.
static void init_free_list() {
    for (int i = 0; i <= max_grains; i++)
	free_list[i] = NULL;
    free_init = 1;
    DEBUG(cerr << "initialized free list\n");
}

/* Should exit when a breakpoint arrives? */
static int breakpoint_exit = 1;
static void breakpoint(int g, char *msg) {
    cerr << "breakpoint: g = " << g << ": " << msg << endl;
    if (breakpoint_exit)
	do_exit(2);
}

void *operator new(size_t sz) {
    return operator new(sz, NULL);
}

void *operator new(size_t sz, char *label) {
    char *p;

    if (label == NULL || sz >= max_grains * grain_size) {
	p = malloc(sz);
	DEBUG(cerr << "::new(" << sz << ") -> malloc -> " << (void *)p
	      << " [" <<  (label ? label : "NO LABEL?") << "]\n");
	return (void *)p;
    } else {
	// Take it off the free list, if available
	if (!free_init)
	    init_free_list();

	// Find index on the free block list
	int g = btog(sz);

	DEBUG(cerr << "::new(" << sz << "): grains = " << g
	      << " [" <<  (label ? label : "NO LABEL?") << "]\n");

	// If blocks are chained, remove the first one and return it
	// Otherwise, allocate some new blocks and thread them onto
	//  the free list.
	if (free_list[g]) {
	    // Return the next free block; adjust the free block
	    //	list pointer.
	    p = free_list[g];
	    free_list[g] = *(char **)free_list[g];
	    if (free_list[g] == p)
		breakpoint(g, "loop in free list within operator new()");
	} else {
	    // Allocate 'chunks' contiguous blocks of
	    //	the right size.
	    int blocksize = gtob(g);
	    p = malloc(chunks * blocksize);
	    DEBUG(cerr << "\tallocating " << chunks
		  << " new blocks of size " << blocksize
		  << " at " << (void *)p << '\n');
	    if (p != NULL) {
		// Thread the blocks so the first word in each
		//  block points to the next block. Return the
		//  first block and thread the last block to NULL.
		char *ptr = p + blocksize;
		free_list[g] = ptr;
		DEBUG(cerr << "\tthreading ");
		for (int i = 1; i < chunks - 1; i++) {
		    *(char **)ptr = ptr + blocksize;
		    DEBUG(cerr << ((i == 1) ? "" : " -> ") << (void *)ptr);
		    ptr = *(char **)ptr;
		}
		// Thread the last block to NULL
		DEBUG(cerr << " -> NULL\n");
		*(char **)ptr = NULL;
	    }
	}
	DEBUG(cerr << "\treturning " << (void *)p << " next free " << (void *)free_list[g] << '\n');
	return (void *)p;
    }
}

// Not knowing the size of the object being deleted, there's no choice
//  but to free() it to the general pool.
void operator delete(void *p) {
    DEBUG(cerr << "::delete(" << p << ") -> free()\n");
    if (p != NULL)
	free((char *)p);
}

// Delete a block of the specified size. If the size is small enough,
//  just thread the block onto the head of the free block list.
//  Otherwise, free it to the general pool.
void delete_sz(void *p, size_t sz, char *label) {
    DEBUG(cerr << "delete_sz(" << setw(8) << p << ", sz = "
	  << setw(2) << sz << ") [" << (label ? label : "NO LABEL?") << "]\n");
    if (p == NULL)
	return;

    int g = btog(sz);
    DEBUG(cerr << "\tgrains = " << g << '\n');

    if (g > max_grains || label == NULL) {
	DEBUG(cerr << "\t-> free(ptr)\n");
	free((char *)p);
    } else {
	DEBUG(cerr << "\tthread onto free block list\n");
	if (!free_init)
	    init_free_list();

	*(char **)p = free_list[g];
	if (free_list[g] == p)
	    breakpoint(g, "loop in free list within delete_sz()");
	free_list[g] = (char *)p;
	DEBUG(cerr << "\tp->next = " << (void *)*(char **)p
	      << " free_list[" << g << "] = " << (void *)free_list[g] << '\n');
    }
}

const unsigned long heapmax = 0x80000000L;

// Called to log a constructor
void memalloc(const char *label, void *p) {
    PDEBUG(PD_MEMLEAK, if ((unsigned long)p < heapmax) cerr << "\nmemalloc -> " << p << '\t' << label << endl);
}

// Called to log a destructor
void memfree(const char *label, void *p) {
    PDEBUG(PD_MEMLEAK, if ((unsigned long)p < heapmax) cerr << "\nmemfree  <- " << p << '\t' << label << endl);
}

// Turn off memory logging (to avoid cleanup problems) and exit()
#undef exit
extern "C" void do_exit(int status) {
    memlog_enable(false);
    exit(status);
}

