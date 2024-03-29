/*
 * alloc.h -- memory allocation defines for cluster
 *
 * $Header: alloc.h,v 1.6 91/07/10 21:26:57 stolcke Exp Locker: stolcke $
 * $Log:	alloc.h,v $
 * Revision 1.6  91/07/10  21:26:57  stolcke
 * saber-cleaned and bad bug in allocation macro fixed
 * 
 * Revision 1.5  91/04/20  16:17:57  stolcke
 * second release (beta)
 * 
 * Revision 1.4  91/04/18  18:28:20  stolcke
 * general cleanup and partial rewrite
 * 
 * Revision 1.3  90/11/10  22:53:13  stolcke
 * *** empty log message ***
 *
 * Revision 1.2  90/11/10  20:02:11  stolcke
 * added decl for new_string
 *
 */

extern char *malloc();
extern char *calloc();
extern free();

#ifndef FLOAT
#define FLOAT	float			/* float type used throughout */
#endif

#define FLOAT_FORMAT	"%.15lg"	/* printf format use for floats */
#define FLOAT2_FORMAT	"%.15lg %.15lg"	/* printf format use for floats */

#define new(type) \
	(type *)calloc(1, sizeof(type))

#define new_array_of(n, type) \
	(type *)calloc((unsigned)(n), sizeof(type))

#define change_array_size(array, n, type) \
	(type *)realloc(array, (unsigned)(n) * sizeof(type))

#define new_2d_array_of(n1, n2, type) \
	(type **)calloc_2d((unsigned)(n1), (unsigned)(n2), sizeof(type))

#define new_3d_array_of(n1, n2, n3, type) \
	(type ***)calloc_3d((unsigned)(n1), (unsigned)(n2), (unsigned)(n3), \
								sizeof(type))

#define change_2d_array_size(array, n1, n2, m1, m2, type) \
	(type **)realloc_2d(array, (unsigned)(n1),(unsigned)(n2), \
				(unsigned)(m1), (unsigned)(m2), sizeof(type))

extern char **calloc_2d();
extern char ***calloc_3d();
extern char **realloc_2d();
extern free_2d_array();
extern free_3d_array();
extern char *new_string();
