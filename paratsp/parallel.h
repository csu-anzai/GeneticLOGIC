/* $Id: parallel.h,v 1.21 1994/05/01 10:38:48 holger Exp holger $ */

/* ========================================================================= */
/* (c) 1994 by Holger Totzke, Universitaet "Otto v. Guericke" Magdeburg      */
/* Diplomarbeit                                                              */
/* ------------------------------------------------------------------------- */
/* Dateiname : parallel.h                                                    */
/* Autor     : Holger Totzke                                                 */
/* Projekt   : PARATSP                                                       */
/* ========================================================================= */

#ifndef TSP_PARALLEL_H
#define TSP_PARALLEL_H

#include "define.h"


#ifdef USE_PROTO
extern void exit_parallel(int);
extern void make_ring(void);
extern void make_pipe(void);
extern void make_grid(void);
extern void make_torus(void);
extern void make_hypercube(void);
extern void make_tree(void);
extern void init_topology(void);
extern void free_topology(void);
extern void init_parvars(void);
extern void init_parallel(void);
extern void end_parallel(void);
extern void init_population(void);
extern int my_send(int, int, void *, int);
extern int my_recv(int, int, void *, int);
extern int my_sendnode(int, int, void *, int);
extern int my_recvnode(int, int, void *, int);
extern void send_allpop(CHROM *);
extern void recv_allpop(CHROM *);
extern void comm_wind(int *, int *);
extern void send_to_pop(int, CHROM *);
extern BOOLEAN recv_all_my_pop(int, int *, int *, CHROM **);
extern void sendnode_to_pop(int, int, CHROM *);
extern void recvnode_from_pop(int, int, CHROM *);
extern void synchronize(void);
extern BOOLEAN sync_done(BOOLEAN);
extern double get_global_optimum(void);
extern void sa_send_to_neighbour(int, TOUR *);
extern void sa_recv_from_neighbour(int, TOUR *);
extern BOOLEAN sa_sync_done(BOOLEAN, BOOLEAN);
extern double sa_get_global_optimum(double);
#else
extern void exit_parallel();
extern void make_ring();
extern void make_pipe();
extern void make_grid();
extern void make_torus();
extern void make_hypercube();
extern void make_tree();
extern void init_topology();
extern void free_topology();
extern void init_parvars();
extern void init_parallel();
extern void end_parallel();
extern void init_population();
extern int my_send();
extern int my_recv();
extern int my_sendnode();
extern int my_recvnode();
extern void send_allpop();
extern void recv_allpop();
extern void comm_wind();
extern void send_to_pop();
extern BOOLEAN recv_all_my_pop();
extern void sendnode_to_pop();
extern void recvnode_from_pop();
extern void synchronize();
extern BOOLEAN sync_done();
extern double get_global_optimum();
extern void sa_send_to_neighbour();
extern void sa_recv_from_neighbour();
extern BOOLEAN sa_sync_done();
extern double sa_get_global_optimum();
#endif


#endif


/*** end of file ***/
