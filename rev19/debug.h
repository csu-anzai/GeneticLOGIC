#ifndef DEBUG_H
#define DEBUG_H

/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/
#define DEBUGCHECK
#define DEBUGCALLS
#define PRINTBRAIN

#ifdef DEBUGCHECK
extern void  debugcheck(const char* s);
extern char  debugstring[256];
#endif DEBUGCHECK

#ifdef DEBUGCALLS
extern void  pushproc(const char* s);
extern void  popproc();
extern const char* topofprocstack();
extern void  listprocstack();
#endif DEBUGCALLS

#endif DEBUG_H
