/* $Header: /tmp_mnt/vida/disks/disk5/Users/terry/r/echo/RCS/globals.h,v 1.6 1992/10/22 04:14:17 terry Exp $ */

extern void error();
extern long seed;
extern long old_seed;
extern BOOLEAN sysdie;
extern NRESOURCES nresources;
extern AGENT_POPULATION_SIZE nspecies;
extern char *myname;
extern char *chromosome_names[];
extern WORLD *the_world;
extern STRING echo_version;
extern BOOLEAN graphics;
extern BOOLEAN sex;
extern BOOLEAN self_replication;
extern STRING verbose;
extern HASH_TABLE *genome_hash;
extern HASH_TABLE *species_name_hash;
extern int genome_hash_size;

#ifdef NO_GRAPHICS
#define popup_message error
#endif

