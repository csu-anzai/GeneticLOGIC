
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/
#include "basicincludes.h"
#include "graphics.h"
#include "error.h"
#include "critter.h"
#include "barrier.h"
#include "pw.h"

#include "debug.h"

#include "externglobals.h"

char debugstring[256];

extern domainstruct domains[MAXDOMAINS];
extern short whichdomain(float x, float z, short d);
extern cxsortedlist xsortedcritters;
extern fxsortedlist xsortedfood;
extern bxsortedlist xsortedbarriers;
extern critter* curfittestcrit[5];
extern float curmaxfitness[5];
extern short numfit;
extern genome** fittest;
extern float* fitness;
extern Boolean crittersRfood;

static const char* slast = NULL;

#ifdef DEBUGCALLS
#define PROCSTACKDEPTH 1000
static const char* procstack[PROCSTACKDEPTH];
static procstackdepth = -1;
static Boolean procstackoverflow = FALSE;
static Boolean procstackunderflow = FALSE;
#endif DEBUGCALLS

void debugcheck(const char* s)
{
#ifdef DEBUGCALLS
    pushproc("debugcheck");
#endif DEBUGCALLS

    gcast* c = worldstage.cast();
    if (c)
    {
        if (c->kount)
        {
            c->checklast(s);
            if (!(c->last))
                printf("Bad l: %s\n",s);
            else if (!(c->last->next))
                printf("Bad l-n: %s\n",s);
            else if (!(c->last->next->next))
                printf("Bad l-n-n: %s\n",s);
            else if (!(c->last->next->next->next))
                printf("Bad l-n-n-n: %s\n",s);
            else if (!(c->last->next->next->next->next))
                printf("Bad l-n-n-n-n: %s\n",s);
        }
    }
    else
        printf("cast NULLed out: %s\n",s);

    if (maxneurons > 217)
        printf("Bad maxneurons(%d): %s\n",maxneurons,s);
    if (maxsynapses > 45584)
        printf("Bad maxsynapses(%d): %s\n",maxsynapses,s);
    if (!strcmp(s,"show last call"))
    {
        if (s)
            printf("debugcheck last called from: %s\n",slast);
        else
            printf("debugcheck: sorry, slast is NULL\n");
    }
    slast = s;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


#ifdef DEBUGCALLS
void pushproc(cchar* s)
{
    if (procstackdepth >= PROCSTACKDEPTH)
    {
        procstackoverflow = TRUE;
        return;
    }
    procstack[++procstackdepth] = s;
}


void popproc()
{
    if (procstackdepth < 0)
    {
        procstackunderflow = TRUE;
        return;
    }
    procstackdepth--;
}


void listprocstack()
{
    if (procstackdepth < 0)
        printf("  procstack is empty\n");
    else
        for (short i = procstackdepth; i >= 0; i--)
            printf("  %s\n",procstack[i]);
    if (procstackunderflow)
        printf("  procstack underflowed\n");
    if (procstackoverflow)
        printf("  procstack overflowed\n");
}


const char* topofprocstack()
{
    if (procstackdepth >= 0)
        return procstack[procstackdepth];
    else
        return "procstack is empty";
}
#endif DEBUGCALLS

