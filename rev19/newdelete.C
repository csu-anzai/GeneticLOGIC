/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/
#include <malloc.h>

#define MAXLOC 100000

extern long age;

// set the following flags to 1 (or 0) to see (or not) the various statistics
// xxxage0 means start displaying from age = 0 and continue displaying
// xxxageN means start displaying *after* age = 0
short shownewsage0 = 0;
short shownewsageN = 0;
short showdeletesage0 = 0;
short showdeletesageN = 0;
short showhimemage0 = 0;
short showhimemageN = 0;
short showmemovermembaseage0 = 0;
short showmemovermembaseageN = 0;
#ifdef DEBUGCALLS
short showprocstacknewage0 = 0;
short showprocstacknewageN = 0;
short showprocstackdeleteage0 = 0;
short showprocstackdeleteageN = 0;
extern void listprocstack();
extern const char* topofprocstack();
const char* memproc[MAXLOC];
#endif DEBUGCALLS

long maxbytes = 1000000;
long maxmem = 70000000;

void* memloc[MAXLOC];
long  memamt[MAXLOC];
long numloc = 0;
long himem = 0;
long membase = 0;
long memovermembase = 0;
long mynumlocbase = 0;
long mymembase = 0;
void* operator new(long l)
{
#ifdef DEBUGCALLS
    const char* proc;
#endif DEBUGCALLS
    short stackprinted = 0;
    if (numloc == 0) printf("Using my new & delete\n");
    if (l > maxbytes)
    {
        printf("this memory request too big (> %d). Exiting now.",maxbytes);
        debugcheck("show last call");
        if (!stackprinted)
        {
            listprocstack();
            stackprinted = 1;
        }
        exit(1);
    }
    if (numloc && ((l+(himem-membase)) > maxmem))
    {
        printf("total memory request too big (> %d). Exiting now.",maxmem);
        debugcheck("show last call");
#ifdef DEBUGCALLS
        if (!stackprinted)
        {
            listprocstack();
            stackprinted = 1;
        }
#endif DEBUGCALLS
        exit(1);
    }
    void* m = malloc(l);
#ifdef DEBUGCALLS
    proc = topofprocstack();
    if (shownewsage0 || (shownewsageN && (age > 0)))
        printf("age %5d: %6d bytes allocated at location %x by %s\n",
                age,l,m,proc);
#else  DEBUGCALLS
    if (shownewsage0 || (shownewsageN && (age > 0)))
        printf("age %5d: %6d bytes allocated at location %x\n",
                age,l,m);
#endif DEBUGCALLS
    if (l == 0)
    {
        printf("WARNING: REQUEST TO ALLOCATE 0 BYTES IN NEW\n");
#ifdef DEBUGCALLS
        if (!stackprinted)
        {
            listprocstack();
            stackprinted = 1;
        }
#endif DEBUGCALLS
    }
    if (numloc < MAXLOC)
    {
        memloc[numloc] = m;
        memamt[numloc] = l;
#ifdef DEBUGCALLS
        memproc[numloc] = proc;
#endif DEBUGCALLS
    }
    else if (numloc == MAXLOC)
        printf("age %5d: Cannot track any more memory requests\n",age);
    if (numloc == 0) membase = long(m);
    if ((long(m)+l-1) > himem)
    {
        himem = long(m)+l-1;
        if (showhimemage0 || (showhimemageN && (age > 0)))
            printf("NOTE: at age %d, new himem = %x to %x (%d bytes)\n",
                   age,m,himem,l);
        memovermembase = long(m)+l-1-membase;
        if (showmemovermembaseage0 || (showmemovermembaseageN && (age > 0)))
            printf("NOTE: at age %d, new memovermembase = %x (%d)\n",
                   age,memovermembase,memovermembase);
    }
#ifdef DEBUGCALLS
    if (showprocstacknewage0 || (showprocstacknewageN && (age > 0)))
    {
        if (!stackprinted)
            listprocstack();
        stackprinted = 1;
    }
#endif DEBUGCALLS
    numloc++;
    return m;
}
void operator delete(void* p)
{
    short stackprinted = 0;
    if (numloc < MAXLOC)
    {
        Boolean foundit = FALSE;
        for (long i = 0; i < numloc; i++)
            if (memloc[i] == p)
            {
                foundit = TRUE;
                break;
            }
        if (foundit)
        {
            if (showdeletesage0 || (showdeletesageN && (age > 0)))
                printf("age %5d: deleting %8d bytes of memory at location %x\n",
                age,memamt[i],p);
            for (long j = i; j < numloc-1; j++)
            {
                memloc[j] = memloc[j+1];
                memamt[j] = memamt[j+1];
#ifdef DEBUGCALLS
                memproc[j] = memproc[j+1];
#endif DEBUGCALLS
            }
            numloc--;
            free(p);
        }
        else
        {
            printf("AGE %5d:, TRYING TO DELETE MEMORY NEVER REQUESTED OR ALREADY DELETED at %x\n",age,p);
            if (!stackprinted)
            {
                listprocstack();
                stackprinted = 1;
            }
        }
    }
    else
    {
        free(p);
        if (showdeletesage0 || (showdeletesageN && (age > 0)))
            printf("age %5d: deleting        ? bytes of memory at location %x\n",age,p);
    }
#ifdef DEBUGCALLS
    if (showprocstackdeleteage0 || (showprocstackdeleteageN && (age > 0)))
    {
        if (!stackprinted)
            listprocstack();
        stackprinted = 1;
    }
#endif DEBUGCALLS
}


void startmybit()
{
    mynumlocbase = numloc;
    mymembase = membase;
}


void howbigwasi()
{
    long i, numbytes, mybadbytes;
    printf("\nMemory useage report:\n");
    printf("himem = %x (%d)\n",himem,himem);
    printf("memovermembase = %x (%d)\n\n",memovermembase,memovermembase);
    mybadbytes = 0;
    if (numloc == mynumlocbase)
        printf("everything pw allocated was deleted\n");
    else
    {
        printf("there were %d news without corresponding deletes\n",
            numloc-mynumlocbase);
        for (i = mynumlocbase; i < numloc; i++)
            mybadbytes += memamt[i];
        printf("totaling to %d bytes\n",mybadbytes);
        if ((numloc-mynumlocbase) < 100)
        {
            printf("following are the locations and amounts:\n");
            for (i = mynumlocbase; i < numloc; i++)
            {
#ifdef DEBUGCALLS
                printf("  %d bytes at %x (%d) by %s\n",
                    memamt[i],memloc[i],memloc[i],memproc[i]);
#else  DEBUGCALLS
                printf("  %d bytes at %x (%d)\n",
                    memamt[i],memloc[i],memloc[i]);
#endif DEBUGCALLS
            }
        }
        else
        {
            printf("following are the first 25 locations and amounts:\n");
            for (i = mynumlocbase; i < (mynumlocbase+25); i++)
            {
#ifdef DEBUGCALLS
                printf("  %d bytes at %x (%d) by %s\n",
                    memamt[i],memloc[i],memloc[i],memproc[i]);
#else  DEBUGCALLS
                printf("  %d bytes at %x (%d)\n",
                    memamt[i],memloc[i],memloc[i]);
#endif DEBUGCALLS
            }
            printf("following are the last 25 locations and amounts:\n");
            for (i = (numloc-25); i < numloc; i++)
            {
#ifdef DEBUGCALLS
                printf("  %d bytes at %x (%d) by %s\n",
                    memamt[i],memloc[i],memloc[i],memproc[i]);
#else  DEBUGCALLS
                printf("  %d bytes at %x (%d)\n",
                    memamt[i],memloc[i],memloc[i]);
#endif DEBUGCALLS
            }
            printf("following are a sampling of the remaining locations and amounts:\n");
            float prob = 50.0 / float(numloc-mynumlocbase-50);
            for (i = (mynumlocbase+25); i < numloc; i++)
            {
                if (drand48() < prob)
                {
#ifdef DEBUGCALLS
                    printf("  %d bytes at %x (%d) by %s\n",
                        memamt[i],memloc[i],memloc[i],memproc[i]);
#else  DEBUGCALLS
                    printf("  %d bytes at %x (%d)\n",
                        memamt[i],memloc[i],memloc[i]);
#endif DEBUGCALLS
                }
            }
        }
    }
    numbytes = 0;
    for (i = 0; i < numloc; i++)
        numbytes += memamt[i];
    printf("there are still %d bytes remaining to delete (by the system)\n",
        numbytes-mybadbytes);
}
