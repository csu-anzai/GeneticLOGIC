/* Function prototypes for Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 */

#ifdef PROTOTYPES
#define _(x) x
#else
#define _(x) ()
#endif
#ifdef	UNIX
#include <signal.h>
void			sigint_handler _((int, int, struct sigcontext *));
void			init_signals _((void));
#endif

void			main _((void));
void			init _((void));
void			init_random _((void));
void			rndseed _((unsigned long));
unsigned long		rnd _((unsigned long));
unsigned long		awcrnd _((void));
unsigned long		addc _((unsigned long, unsigned long, short *));
unsigned long		scale _((unsigned long, unsigned long));
void			init_soup _((void));
void			init_organisms _((void));
void			dump_soup _((long));
void			dump_organisms _((void));
void			dump_one_organism _((long));
void			dump_stats _((void));
void			mon _((void));
void			monhelp _((void));
void			do_soup _((void));
void			adjust_population _((void));
int				spawn _((long));
void			kill_organism _((long));
int				reap _((long));
void			mutate _((void));
void			execute _((void));
void			read_soup _((char *));
void			write_soup _((char *));
long			disassemble _((unsigned char *, long, short, short));
long			orgindex _((long));
void			banner _((void));
void			soup_exit _((void));
unsigned long		scale _((unsigned long, unsigned long));
#ifdef	GBASE
void			init_gbase _((void));
void			record_birth _((long));
void			record_death _((long));
void			nextgenomename _((char *));
#endif
