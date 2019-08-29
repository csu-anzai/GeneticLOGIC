#ifndef CRITTER_H
#define CRITTER_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// critter.h - declaration of critter classes

#include "misc.h"

#include "externglobals.h"

#include "food.h"

#include "debug.h"

// #define OF1


extern unsigned char binofgray[256];


// random, energy, red, green, blue
extern short numinputneurgroups;
extern short firstnoninputgroup;

// eat, mate, fight, speed, yaw, light, focus
extern short numoutneur;
extern short numoutneurgroups;


// note: activation levels are not maintained in the neuronstruct
// so that after the new activation levels are computed, the old
// and new blocks of memory can simply be repointered rather than
// copied.

struct neuronstruct {
    short group;
    float bias;
    long  startsynapses;
    long  endsynapses;
};


struct synapsestruct {
    float efficacy;   // > 0 for excitatory, < 0 for inhibitory
    short fromneuron; // > 0 for excitatory, < 0 for inhibitory
    short toneuron;   // > 0 for excitatory, < 0 for inhibitory
};


class genome
{
friend void genomeinit();
friend void genomedestruct();
public:
    genome() { init('n'); } // null or no initialization of bits
    genome(cchar c) { init(c); } // 'r'andom is all that is currently defined
    ~genome() { if (genes) delete genes; }
    void init(cchar c);
    void dump(ostream& out);
    void load(istream& in);
    void randomize()
        { randomize(minbitprob+drand48()*(maxbitprob-minbitprob)); }
    void randomize(cfloat bitonprob);
    void mutate(); // mutate this genome (mutation rate is one of the genes)
//  genome* crossover(const genome* g); // given second genome, g, return ptr to
                                        // new genome after performing crossover
    void crossover(const genome* g1, const genome* g2, cBoolean mutate);
    void copygenes(genome* sgenome)
    {
        unsigned char* sg = sgenome->genes;
        unsigned char* tg = genes;
        for (long i = 0; i < numbytes; i++)
            *(tg++) = *(sg++);
    }
    float sepcalc(genome* g);
    float mateprob(genome* g);
// the following are specific to the genetic code for these critters
    float mutationrate() // 8 bits between minmrate and maxmrate
        { return interp(geneval(mrategene),minmrate,maxmrate); }
    long numcpts() // 8 bits between minnumcpts and maxnumcpts
        { return interp(geneval(ncptsgene),minnumcpts,maxnumcpts); }
    long lifespan() // 8 bits between minlifespan and maxlifespan
        { return interp(geneval(lifespangene),minlifespan,maxlifespan); }
    float id() // 8 bits, mapped to green color
        { return geneval(idgene); }
    float strength() // 8 bits between minstrength and maxstrength
                     // relative to size, so 1.0 is normal for all sizes
                     // affects power (overall, non-size-relative strength)
                     // and scales all energy consumption
        { return interp(geneval(strengthgene),minstrength,maxstrength); }
    float size() // 8 bits between mincsize and maxcsize
                 // also affects power (overall, non-size-relative strength)
                 // so power is a combination of strength & size
        { return interp(geneval(sizegene),mincsize,maxcsize); }
    float maxspeed() // 8 bits between minmaxspeed and maxmaxspeed
        { return interp(geneval(maxspeedgene),minmaxspeed,maxmaxspeed); }
    float mateenergy() // 8 bits between minlrate and maxlrate
        { return interp(geneval(mateenergygene),minmateenergy,maxmateenergy); }
    short numneurgroups()
    {
        return nint(interp(geneval(numneurgroupsgene),
            mininternalneurgroups,maxinternalneurgroups))
          + numinputneurgroups + numoutneurgroups;
    }
    short numeneur(cshort i);
    short numineur(cshort i);
    short numneurons(cshort i);
    float bias(cshort i)
        { return interp(geneval(biasgene+i),minbias,maxbias); }
    float biaslearningrate(cshort i)
        { return interp(geneval(biaslrategene+i),minbiaslrate,maxbiaslrate); }
    long numeesynapses(cshort i, cshort j)
        { return nint(eecd(i,j)*numeneur(i)*(numeneur(j)-((i==j)?1:0))); }
    long numeisynapses(cshort i, cshort j)
    {
        if (i >= (numneurgroups()-numoutneurgroups))
            return 0;
        else
            return nint(eicd(i,j)*numineur(i)*numeneur(j));
    }
    long numiisynapses(cshort i, cshort j)
    {
        if (i >= (numneurgroups()-numoutneurgroups))
            return 0;
        else
            return nint(iicd(i,j)*numineur(i)*(numineur(j)-((i==j)?1:0)));
    }
    long numiesynapses(cshort i, cshort j)
    {
        return nint(iecd(i,j)*numeneur(i)*
         (numineur(j)-(((i==j)&&(i>=(numneurgroups()-numoutneurgroups)))?1:0)));
    }
    long numsynapses(cshort i, cshort j)
        { return numeesynapses(i,j)
                +numeisynapses(i,j)
                +numiisynapses(i,j)
                +numiesynapses(i,j); }
    float eecd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eecd called with input group as to-group (",i,")");
        return interp(geneval(eecdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minconnectiondensity,maxconnectiondensity);
    }
    float eicd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eicd called with input group as to-group (",i,")");
        return interp(geneval(eicdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minconnectiondensity,maxconnectiondensity);
    }
    float iicd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"iicd called with input group as to-group (",i,")");
        return interp(geneval(iicdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minconnectiondensity,maxconnectiondensity);
    }
    float iecd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"iecd called with input group as to-group (",i,")");
        return interp(geneval(iecdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minconnectiondensity,maxconnectiondensity);
    }
    float eetd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eetd called with input group as to-group (",i,")");
        return interp(geneval(eetdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      mintopologicaldistortion,maxtopologicaldistortion);
    }
    float eitd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eitd called with input group as to-group (",i,")");
        return interp(geneval(eitdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      mintopologicaldistortion,maxtopologicaldistortion);
    }
    float iitd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"iitd called with input group as to-group (",i,")");
        return interp(geneval(iitdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      mintopologicaldistortion,maxtopologicaldistortion);
    }
    float ietd(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"ietd called with input group as to-group (",i,")");
        return interp(geneval(ietdgene+(i-numinputneurgroups)*maxneurgroups+j),
                      mintopologicaldistortion,maxtopologicaldistortion);
    }
    float eelr(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eelr called with input group as to-group (",i,")");
        return interp(geneval(eelrgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minlrate,maxlrate);
    }
    float eilr(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"eilr called with input group as to-group (",i,")");
        return interp(geneval(eilrgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minlrate,maxlrate);
    }
    float iilr(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"iilr called with input group as to-group (",i,")");
        // note: learning rate at inhibitory synapses must be < 0.0 (not <= 0.0)
        return min(-1.e-10,
              -interp(geneval(iilrgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minlrate,maxlrate));
    }
    float ielr(cshort i, cshort j)
    {
        if (i < numinputneurgroups)
            error(2,"ielr called with input group as to-group (",i,")");
        // note: learning rate at inhibitory synapses must be < 0.0 (not <= 0.0)
        return min(-1.e-10,
              -interp(geneval(ielrgene+(i-numinputneurgroups)*maxneurgroups+j),
                      minlrate,maxlrate));
    }
    void print(clong lobit, clong hibit);
    void setone(clong lobit, clong hibit);
    void setone();
    void setzero(clong lobit, clong hibit);
    void setzero();
protected:
    unsigned char* genes;
    static Boolean classinited;
    static long numbytes;
    static long numphysbytes;
    static long mrategene;
    static long ncptsgene;
    static long lifespangene;
    static long idgene;
    static long strengthgene;
    static long sizegene;
    static long maxspeedgene;
    static long mateenergygene;
// definitions of the following brain-related genes are in the .C file
    static long numrneurgene;
    static long numgneurgene;
    static long numbneurgene;
    static long numneurgroupsgene;
    static long numeneurgene;
    static long numineurgene;
    static long biasgene;
    static long biaslrategene;
    static long eecdgene;
    static long eicdgene;
    static long iicdgene;
    static long iecdgene;
    static long eelrgene;
    static long eilrgene;
    static long iilrgene;
    static long ielrgene;
    static long eetdgene;
    static long eitdgene;
    static long iitdgene;
    static long ietdgene;
    static long* cpts;
// use of geneval(byte) permits simple binary encoding currently in
// use to be easily replaced by Gray coding at a later date.
    float geneval(clong byte)
    {
        if (graycoding)
            return float(binofgray[genes[byte]])/255.0;
        else
            return float(genes[byte])/255.0;
    }
};


class brain
{
friend void braininit();
friend void braindestruct();
extern class critter;
public:
    brain()
    {
#ifdef DEBUGCALLS
        pushproc("brain constructor");
#endif DEBUGCALLS
        if (!classinited)
            braininit();
        neuron = NULL;
        neuronactivation = NULL;
        newneuronactivation = NULL;
        synapse = NULL;
        groupblrate = NULL;
        grouplrate = NULL;
//      neuronsize = neuronactivationsize = synapsesize = 0;
        mygenes = NULL; // but don't delete them, because we don't new them
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    ~brain()
    {
#ifdef DEBUGCALLS
        pushproc("brain destructor");
#endif DEBUGCALLS
        if (neuron) delete neuron;
        if (neuronactivation) delete neuronactivation;
        if (newneuronactivation) delete newneuronactivation;
        if (synapse) delete synapse;
        if (groupblrate) delete groupblrate;
        if (grouplrate) delete grouplrate;
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void dump(ostream& out);
    void load(istream& in);
    void report();
    void grow(genome* g);
    void update(cfloat energyfraction);
    float random() { return neuronactivation[randomneuron]; }
    float eat() { return neuronactivation[eatneuron]; }
    float mate() { return neuronactivation[mateneuron]; }
    float fight() { return neuronactivation[fightneuron]; }
    float speed() { return neuronactivation[speedneuron]; }
    float yaw() { return neuronactivation[yawneuron]; }
    float light() { return neuronactivation[lightneuron]; }
    float focus() { return neuronactivation[focusneuron]; }
    float brainenergy() { return energyuse; }
    short numvispixels()
        { return max(max(numrneurons,numgneurons),numbneurons); }
    short numredneurons() { return numrneurons; }
    short numgreenneurons() { return numgneurons; }
    short numblueneurons() { return numbneurons; }
    void gdump(cshort patchwidth, cshort patchheight);
    void gdump2(cshort patchwidth, cshort patchheight);
// put these under protected: someday, but need them in
// critter::setupmonitor() for now
    short numinputneurons;
    short numnoninputneurons;
    short numneurons;
    long numsynapses;
protected:
    static Boolean classinited;
    static short* firsteneur; // [maxneurgroups]
    static short* firstineur; // [maxneurgroups]
    static float* eeremainder; // [maxneurgroups]
    static float* eiremainder; // [maxneurgroups]
    static float* iiremainder; // [maxneurgroups]
    static float* ieremainder; // [maxneurgroups]
    static Boolean* neurused; // [max(maxeneurpergroup,maxineurpergroup)]
    static short randomneuron;
    static short energyneuron;
    genome* mygenes;
    short redneuron;
    short greenneuron;
    short blueneuron;
    short eatneuron;
    short mateneuron;
    short fightneuron;
    short speedneuron;
    short yawneuron;
    short lightneuron;
    short focusneuron;
    float* groupblrate;
    float* grouplrate;
    neuronstruct* neuron;
    synapsestruct* synapse;
    float* neuronactivation;
    float* newneuronactivation;
    short numneurgroups;
    short numrneurons;
    short numgneurons;
    short numbneurons;
    short firstnoninputneuron;
    float xredwidth;
    float xgreenwidth;
    float xbluewidth;
    short xredintwidth;
    short xgreenintwidth;
    short xblueintwidth;
    float energyuse;
//  short neuronsize;
//  short neuronactivationsize;
//  long synapsesize;
    void allocatebrainmemory();
    short nearestfreeneuron(cshort iin, cBoolean* used, cshort num,
                            cshort exclude);
};


class critter : public gpolyobj
{
friend void critterinit();
friend void critterwindowrefresh();
friend void critterdestruct();
friend critter* getfreecritter();
friend void critterpovswap();
friend void critterdump(ostream& out);
friend void critterload(istream& in);
//friend void operator>>(cchar*,critter&);
public:
    critter() { init(); }
    critter(cchar* n) : (n) { init(); }
    critter(cchar c)
    {
#ifdef DEBUGCALLS
        pushproc("critter constructor");
#endif DEBUGCALLS
        init();
        if (c == '#')
            number2name();
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    ~critter()
    {
#ifdef DEBUGCALLS
        pushproc("critter destructor");
#endif DEBUGCALLS
        if (pg) delete pg;
        if (pb) delete pb;
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void dump(ostream& out);
    void load(istream& in);
    void update();
    void setvel(cfloat x, cfloat y, cfloat z)
        { v[0] = x; v[1] = y; v[2] = z; }
    void setvel(short k, float f) { v[k] = f; }
    void setvelx(cfloat f) { v[0] = f; }
    void setvely(cfloat f) { v[1] = f; }
    void setvelz(cfloat f) { v[2] = f; }
    void setmass(cfloat f) { m = f; }
    virtual void draw();
    void grow();
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(len[0]*len[0] + len[2]*len[2])
                * radscale * scale * 0.5;
    }
    void eat(food* f)
    {
#ifdef DEBUGCALLS
        pushproc("critter::eat");
#endif DEBUGCALLS
        if (pb->eat() > eatthreshold)
        {
            float trytoeat = pb->eat()*eat2consume;
            if ((myenergy+trytoeat) > mymaxenergy)
                trytoeat = mymaxenergy - myenergy;
            float actuallyeat = f->eat(trytoeat);
            myenergy += actuallyeat;
            myfoodenergy += actuallyeat;
#ifdef OF1
            mytotein += actuallyeat;
#endif
            if (myfoodenergy > mymaxenergy)
            {
                foodenergyout += myfoodenergy - mymaxenergy;
                myfoodenergy = mymaxenergy;
            }
//          myfitness += actuallyeat / (mymaxenergy * pg->lifespan());
//          myfitness += eatfitparam * actuallyeat / maxmaxenergy;
            myfitness += eatfitparam * actuallyeat
                         / (eat2consume * pg->lifespan());
        }
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void damage(cfloat e) { myenergy -= (e<myenergy) ? e : myenergy; }
    float mateprob(critter* c) { return pg->mateprob(c->genes()); }
    float mating()
    {
        mylastmate = myage;
        myfitness += matefitparam * matewait / pg->lifespan();
        float mymateenergy = pg->mateenergy() * myenergy;
        myenergy -= mymateenergy;
        myfoodenergy -= mymateenergy;
#ifdef OF1
        mytotebirth += mymateenergy;
        mynumbirths++;
#endif
        return mymateenergy;
    }
    void rewardmovement()
    {
        myfitness += movefitparam *
                     (fabs(pos[0]-oldpos[0])+fabs(pos[2]-oldpos[2])) /
                     (pg->maxspeed()*speed2dpos*pg->lifespan());
    }
    void lastrewards()
    {
        myfitness += energyfitparam * myenergy / mymaxenergy
                   + agefitparam * myage / pg->lifespan();
    }
    void die()
    {
#ifdef DEBUGCALLS
        pushproc("critter::die");
#endif DEBUGCALLS
        crittersliving--; 
        critterlist->freeindex(myindex);
        if (showvision && graphics)
        {
            povwindow->makecurrentwindow();
            povwindow->makecurrentcolor();
            povwindow->justviewport(xleft,xright,ybottom,ytop);
            ::frontbuffer(TRUE);
            ::clear();
            ::frontbuffer(FALSE);
        }
        endbrainmonitoring();
        myalive = FALSE;
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void xo(cfloat x) { oldpos[0] = x; }
    void yo(cfloat y) { oldpos[1] = y; }
    void zo(cfloat z) { oldpos[2] = z; }
    void setoldpos()
        { oldpos[0] = pos[0]; oldpos[1] = pos[1]; oldpos[2] = pos[2]; }
    void saveoldpos()
        { oldpos[0] = pos[0]; oldpos[1] = pos[1]; oldpos[2] = pos[2]; }
    float x() { return pos[0]; }
    float y() { return pos[1]; }
    float z() { return pos[2]; }
    float xo() { return oldpos[0]; }
    float yo() { return oldpos[1]; }
    float zo() { return oldpos[2]; }
    float vel(short i) { return v[i]; }
    float velx() { return v[0]; }
    float vely() { return v[1]; }
    float velz() { return v[2]; }
    float mass() { return m; }
    float sizeadvantage() { return mysizeadvantage; }
    float energy() { return myenergy; }
    void energy(cfloat e) { myenergy = e; }
    float foodenergy() { return myfoodenergy; }
    void foodenergy(cfloat e) { myfoodenergy = e; }
    float fight() { return pb->fight(); }
    float strength() { return pg->strength(); }
    float mate() { return pb->mate(); }
    float size() { return pg->size(); }
    long age() { return myage; }
    long maxage() { return pg->lifespan(); }
    float maxenergy() { return mymaxenergy; }
    long lastmate() { return mylastmate; }
    genome* genes() { return pg; }
    long number() { return mycritternumber; }
    float fitness() { return myfitness; }
    virtual void print();
    void beginbrainmonitoring(cshort pw, cshort ph);
    void endbrainmonitoring();
    float fov() { return pb->focus()*(maxfocus-minfocus)+minfocus; }
    short domain() { return mydomain; }
    void domain(short id) { mydomain = id; }
    Boolean alive() { return myalive; }
#ifdef OF1
    float f() { return (float)myt0/(float)myage; } // actual frac time food side
    long t0() { return myt0; } // actual time on food side
    long t1() { return myage - myt0; } // actual time on safe side
    long numbirths() { return mynumbirths; }
    // following is average energy to birthing process
    float ebirth() { return mynumbirths>0?mytotebirth/mynumbirths:0.0; }
    // following assumes median case until we know better
    long tbirth()
        { return mynumbirths>0?myage/mynumbirths:(maxage()+matewait)*0.5; }
    float dedtout() { return fixedenergydrain;} // all others zero for now
    // following assumes median case eating efficiency if we don't know better
    float dedtin() { return myt0>0?mytotein/myt0:eat2consume*0.5; }
    float e() { return dedtin()/eat2consume; } // eating efficiency
    // following is birthing efficiency
    float b() { return mynumbirths>0?(float)matewait/(float)tbirth():0.0; }
    float fopt() { return (ebirth()+dedtout()*tbirth())/(dedtin()*tbirth()); }
#endif
protected:
    static Boolean classinited;
    static long crittersever;
    static long crittersliving;
    static gpolyobj* critterobj;
    static indexlist* critterlist;
    static critter** pc;
    static gwindow* povwindow;
    static short povwidth;
    static short povheight;
    Boolean myalive;
    gwindow* monitorwindow;
    short patchwidth;
    short patchheight;
    void setupmonitor();
    long myindex;
    long mycritternumber;
    long myage;
    long mylastmate;
    float myenergy;
    float myfoodenergy;
    float mymaxenergy;
    float myspeed2energy;
    float myyaw2energy;
    float mysizeadvantage;
    float lenx;
    float lenz;
    float m; // mass (not used)
    float oldpos[3];
    float v[3];
    float nosecol[3];
    float myfitness;  // crude guess for keeping minimum population early on
    genome* pg;
    brain* pb;
    short xleft;
    short xright;
    short ybottom;
    short ytop;
    short ypix;
    gcamera mycamera;
//    gstage mystage;
    gscene myscene;
    frustumXZ myfrustum;
    short mydomain;
#ifdef OF1
    long myt0;  // actual time on food side
    long mynumbirths;
    float mytotebirth;  // total energy given to all offspring
    float mytotein;     // total energy ingested
#endif
    void reinit()
    {
        v[0] = v[1] = v[2] = myfitness = 0.;
#ifdef OF1
        mynumbirths = 0;
        mytotebirth = 0.0;
        mytotein = 0.0;
        myt0 = 0;
#endif
    }
    void init()
    {
#ifdef DEBUGCALLS
        pushproc("critter::init");
#endif DEBUGCALLS
        if (!classinited)
            critterinit();
        reinit();
        pb = NULL;
        myfitness = 0.;  // crude guess for keeping minimum population early on
        pg = new genome;
        if (pg == NULL)
        {
            error(1,"Insufficient memory for critter to grow new genome");
        }
        monitorwindow = NULL;
        myscene.setstage(worldstage);
        myscene.setcamera(mycamera);
        myalive = FALSE; // must grow() to be truly alive
        xleft = -1;  // to show it hasn't been initialized
        m = 0.0; // mass - not used
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void behave();
    void number2name()
    {
#ifdef DEBUGCALLS
        pushproc("critter::number2name");
#endif DEBUGCALLS
        sprintf(tempstring,"critter#%d\0",crittersever);
        this->setname(tempstring);
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
    }
    void setgeom();
    void setgraphics();
    void dobrainmonitor();
};


typedef critter* pcritter;

declare(gdlink,pcritter)
declare(gdlist,pcritter)


class cxsortedlist : public gdlist(pcritter)
{
public:
    cxsortedlist() { }
    ~cxsortedlist() { }
    void add(pcritter a);
    void sort();
    void list();
};


// end of critter.h

#endif CRITTER_H
