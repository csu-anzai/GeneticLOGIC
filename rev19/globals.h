#ifndef GLOBALS_H
#define GLOBALS_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/
// globals.h - global variables for pw (PolyWorld)

const char progname[] = "PolyWorld";

gstage worldstage("worldstage");

Boolean graphics;

short numdomains;
domainstruct domains[MAXDOMAINS];

cxsortedlist xsortedcritters;
fxsortedlist xsortedfood;
bxsortedlist xsortedbarriers;

//gdlist(pcritter)** critterlistarray;
//gdlist(pfood)** foodlistarray;
//gdlist(pbarrier)** barrierlistarray;
//short numxcells;
//short numcells;
//float cellsize;

float maxradius;

critter* curfittestcrit[5];
float curmaxfitness[5];
short numfit;
genome** fittest;
float* fitness;
float eatfitparam;
float matefitparam;
float movefitparam;
float energyfitparam;
float agefitparam;
uchar* retinabuf;
short retinawidth;
short retinaheight;
long* whitebuf;
float maxfitness;
float avgfitness;
float totalfitness;
float nexfitness;
long age;
long numdied;
long numdiedage;
long numdiedenergy;
long numdiedfight;
long numdiededge;
long numborn;
long numcreated;
long numcreatedrandom;
long numcreated1fit;
long numcreated2fit;
long numfights;
long miscnobirth;
long lastcreate;
long maxgapcreate;
long numbornsincecreated;
long monstride;

long screenleft;
long screenbottom;
long xscreensize = XMAXSCREEN+1;
long yscreensize = YMAXSCREEN+1;

Boolean edges;
Boolean wraparound;
Boolean vision;
Boolean showvision;
short minwin;
float maxvel;
long minnumcritters;
long maxnumcritters;
long initnumcritters;
long misccritters; // number of critters born without intervening creation
                   // before miscegenation function kicks in

float positionseed;
float genomeseed;

long minfoodcount;
long maxfoodcount;
long maxfoodgrown;
long initfoodcount;
float foodrate;
Boolean crittersRfood;
long fit1freq;
long fit2freq;
float minfoodenergy;
float maxfoodenergy;
float minfoodenergyatdeath;
float size2energy; // (converts food/critter size to available energy)
float eat2consume; // (converts eat neuron value to energy consumed)

short minintneurons;
short maxintneurons;
short minvispixels;
short maxvispixels;
short minvisneurons;
short maxvisneurons;
short minneurons;
short maxneurons;
long maxsynapses;
long firstnonclamped;
float minmrate;
float maxmrate;
long minnumcpts;
long maxnumcpts;
long minlifespan;
long maxlifespan;
long matewait;
long initmatewait;
float minstrength;
float maxstrength;
float mincsize;
float maxcsize;
float minmaxenergy;
float maxmaxenergy;
float minmateenergy;
float maxmateenergy;
float minmatefrac;
float minmaxspeed;
float maxmaxspeed;
float speed2dpos;
float yaw2dyaw;
float minlrate;
float maxlrate;
float minfocus;
float maxfocus;
float critterfovy;
float maxsizeadvantage;
float power2energy; // controls amount of damage to other critter

float eat2energy;
float mate2energy;
float fight2energy;
float maxsizepenalty;
float speed2energy;
float yaw2energy;
float light2energy;
float focus2energy;
float maxsynapse2energy; // (amount if all synapses usable)
float fixedenergydrain;
float decayrate;

float eatthreshold;
float matethreshold;
float fightthreshold;
float miscbias;
float miscinvslope;

float logisticslope;
float maxweight;
float initmaxweight;
float minbitprob;
float maxbitprob;
float critterheight;
float foodheight;
colf foodcolor;
float barrierheight;
colf barriercolor;
colf groundcolor;
colf cameracolor;
float camradius;
float camheight;
float camrotationrate;
float camanglestart;
float camfov;
float camangle;
float worldsize;

Boolean genesepmon; // whether gene-separation will be monitored or not
Boolean geneseprec; // whether gene-separation will be recorded or not
float genesepmax;
float genesepmin;
float genesepavg;
FILE *genesepfile;
Boolean binchartgs;
binchartwindow* gswin;
float* gsvals;
long numgsvals;

float foodenergyin;
float foodenergyout;
float totfoodenergyin;
float totfoodenergyout;
float avgfoodenergyin;
float avgfoodenergyout;

short mininternalneurgroups;
short maxinternalneurgroups;
short mineneurpergroup;
short maxeneurpergroup;
short minineurpergroup;
short maxineurpergroup;
short maxneurpergroup;
short maxneurgroups;
short maxnoninputneurgroups;
short maxinternalneurons;
short maxinputneurons;
short maxnoninputneurons;
float minbias;
float maxbias;
float minbiaslrate;
float maxbiaslrate;
float minconnectiondensity;
float maxconnectiondensity;
float mintopologicaldistortion;
float maxtopologicaldistortion;
float maxneuron2energy;
long numprebirthcycles;

Boolean graycoding;

#ifdef OF1
float deathprob;
#endif

short smite;
short minsmiteage;

short ifit;
short jfit;

#endif GLOBALS_H
