#if (!defined(EXTERNGLOBALS_H)) && (!defined(GLOBALS_H))
#define EXTERNGLOBALS_H
// externglobals.h - global variables for pw (PolyWorld)
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

extern const char progname[];

extern gstage worldstage;

extern Boolean graphics;

extern short numdomains;
//extern domainstruct domains[MAXDOMAINS];

//extern cxsortedlist xsortedcritters;
//extern fxsortedlist xsortedfood;
//extern bxsortedlist xsortedbarriers;

//extern gdlist(pcritter)** critterlistarray;
//extern gdlist(pfood)** foodlistarray;
//extern gdlist(pbarrier)** barrierlistarray;
//extern short numxcells;
//extern short numcells;
//extern float cellsize;

extern float maxradius;

//extern critter* curfittestcrit[5];
//extern float curmaxfitness[5];
//extern short numfit;
//genome** fittest;
//float* fitness;

extern float eatfitparam;
extern float matefitparam;
extern float movefitparam;
extern float energyfitparam;
extern float agefitparam;
extern uchar* retinabuf;
extern short retinawidth;
extern short retinaheight;
extern long* whitebuf;
extern float maxfitness;
extern float avgfitness;
extern float totalfitness;
extern float nexfitness;
extern long age;
extern long numdied;
extern long numdiedage;
extern long numdiedenergy;
extern long numdiedfight;
extern long numdiededge;
extern long numborn;
extern long numcreated;
extern long numcreatedrandom;
extern long numcreated1fit;
extern long numcreated2fit;
extern long numfights;
extern long miscnobirth;
extern long lastcreate;
extern long maxgapcreate;
extern long numbornsincecreated;
extern long monstride;

extern long screenleft;
extern long screenbottom;
extern long xscreensize;
extern long yscreensize;

extern Boolean edges;
extern Boolean wraparound;
extern Boolean vision;
extern Boolean showvision;
extern short minwin; // 21
extern float maxvel;
extern long minnumcritters;
extern long maxnumcritters;
extern long initnumcritters;
extern long misccritters;// number of critters born without intervening creation
                         // before miscegenation function kicks in

extern float positionseed;
extern float genomeseed;

extern long minfoodcount;
extern long maxfoodcount;
extern long maxfoodgrown;
extern long initfoodcount;
extern float foodrate;
extern Boolean crittersRfood;
extern long fit1freq;
extern long fit2freq;
extern float minfoodenergy;
extern float maxfoodenergy;
extern float minfoodenergyatdeath;
extern float size2energy; // (converts food/critter size to available energy)
extern float eat2consume; // (converts eat neuron value to energy consumed)

extern short minintneurons;
extern short maxintneurons;
extern short minvispixels;
extern short maxvispixels;
extern short minvisneurons;
extern short maxvisneurons;
extern short minneurons;
extern short maxneurons;
extern long maxsynapses;
extern long firstnonclamped;
extern float minmrate;
extern float maxmrate;
extern long minnumcpts;
extern long maxnumcpts;
extern long minlifespan;
extern long maxlifespan;
extern long matewait;
extern long initmatewait;
extern float minstrength;
extern float maxstrength;
extern float mincsize;
extern float maxcsize;
extern float minmaxenergy;
extern float maxmaxenergy;
extern float minmateenergy;
extern float maxmateenergy;
extern float minmatefrac;
extern float minmaxspeed;
extern float maxmaxspeed;
extern float speed2dpos;
extern float yaw2dyaw;
extern float minlrate;
extern float maxlrate;
extern float minfocus;
extern float maxfocus;
extern float critterfovy;
extern float maxsizeadvantage;
extern float power2energy; // controls amount of damage to other critter
extern float fixedenergydrain;
extern float decayrate;

extern float eat2energy;
extern float mate2energy;
extern float fight2energy;
extern float maxsizepenalty;
extern float speed2energy;

extern float yaw2energy;

extern float light2energy;
extern float focus2energy;
extern float maxsynapse2energy;

extern float eatthreshold;
extern float matethreshold;
extern float fightthreshold;
extern float miscbias;
extern float miscinvslope;

extern float logisticslope;
extern float maxweight;
extern float initmaxweight;
extern float minbitprob;
extern float maxbitprob;
extern float critterheight;
extern float foodheight;
extern colf foodcolor;
extern float barrierheight;
extern colf barriercolor;
extern colf groundcolor;
extern colf cameracolor;
extern float camradius;
extern float camheight;
extern float camrotationrate;
extern float camanglestart;
extern float camfov;
extern float camangle;
extern float worldsize;

extern Boolean genesepmon;
extern Boolean geneseprec;
extern float genesepmax;
extern float genesepmin;
extern float genesepavg;
extern FILE *genesepfile;
extern Boolean binchartgs;
extern binchartwindow* gswin;
extern float* gsvals;
extern long numgsvals;

extern float foodenergyin;
extern float foodenergyout;
extern float totfoodenergyin;
extern float totfoodenergyout;
extern float avgfoodenergyin;
extern float avgfoodenergyout;

extern short mininternalneurgroups;
extern short maxinternalneurgroups;
extern short mineneurpergroup;
extern short maxeneurpergroup;
extern short minineurpergroup;
extern short maxineurpergroup;
extern short maxneurgroups;
extern short maxnoninputneurgroups;
extern short maxinternalneurons;
extern short maxinputneurons;
extern short maxnoninputneurons;
extern float minbias;
extern float maxbias;
extern float minbiaslrate;
extern float maxbiaslrate;
extern float minconnectiondensity;
extern float maxconnectiondensity;
extern float mintopologicaldistortion;
extern float maxtopologicaldistortion;
extern float maxneuron2energy;
extern long numprebirthcycles;

extern Boolean graycoding;

#ifdef OF1
extern float deathprob;
#endif

extern short smite;
extern short minsmiteage;

extern short ifit;
extern short jfit;

#endif EXTERNGLOBALS_H
