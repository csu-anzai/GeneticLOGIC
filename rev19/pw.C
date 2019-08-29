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

#include "globals.h"

//#define NORMALRECORD
#define SYSTEMVIDEO

gscreen screen("screen");
gcast worldcast;
Boolean docpuwork;
long dumpfreq;
long statfreq;
short moncritrank;
short moncritrankold;
critter* moncrit;
Boolean tracking;
short monwidth;
short monheight;
Boolean alternatebraindisplay = FALSE;
float groundclearance;
short overheadrank;
short overheadrankold;
critter* overheadcrit;

Boolean chartborn;
Boolean chartfit;
Boolean chartfoodenergy;
Boolean chartpop;
chartwindow* bornwin;
chartwindow* fitwin;
chartwindow* fewin;
chartwindow* popwin;
chartwindow* ofwin;

Boolean announceclears = FALSE;

#ifdef PRINTBRAIN
Boolean printbrain = FALSE;
#endif PRINTBRAIN


#define MYNEWDELETE
#ifdef MYNEWDELETE
#include "newdelete.C"
#endif MYNEWDELETE


//#define CHECKMEM
#ifdef CHECKMEM
const long baseaddr = 0x00462850;
const long topaddr  = 0x00463528;
const uchar* memory = 0;
Boolean checked = FALSE;
long checksum = 0;
void checkmem(char* caller)
{
    printf("in checkmem (%s)\n",caller);
    long newchecksum = 0;
    for (long addr = baseaddr; addr <= topaddr; addr++)
        newchecksum += memory[addr];
    if (checked)
    {
        if (newchecksum != checksum)
        {
//          printf("when called by %s...\n",caller);
            printf("checksum changed from %d to %d in memory range (%x , %x)\n",
                    checksum,newchecksum,baseaddr,topaddr);
        }
        else
            printf("checksum = %d\n",newchecksum);
    }
    else
    {
        checked = TRUE;
        checksum = newchecksum;
        printf("checksum = %d in memory range (%x, %x)\n",
                checksum,baseaddr,topaddr);
    }
}
#endif CHECKMEM


#include <sys/times.h>
#define FHZ float(HZ)
tms timebuf;

#ifdef TIMING
const short timeevents = 25;
#else TIMING
const short timeevents = 1;
#endif TIMING

const short maxlabellength = 64;

char* timelabel[timeevents];
long timecalls[timeevents];
time_t usercpu[timeevents];
time_t syscpu[timeevents];
time_t wall[timeevents];
time_t usercpustart[timeevents];
time_t syscpustart[timeevents];
time_t wallstart[timeevents];

#ifdef TIMING
time_t walltotal;
time_t usercputotal;
time_t syscputotal;

const short timegroups = 1;
short timegrouplen[timegroups];
short timegroup[timegroups][timeevents];
#endif TIMING

void timeinit()
{
    for (short i = 0; i < timeevents; i++)
    {
        timelabel[i] = new char[maxlabellength];
        usercpu[i] = syscpu[i] = wall[i] = 0.;
    }

    strncpy(timelabel[0],"main loop",maxlabellength);
#ifdef TIMING
    strncpy(timelabel[1],"critter behavior",maxlabellength);
    strncpy(timelabel[2],"interaction",maxlabellength);
    strncpy(timelabel[3],"screen update",maxlabellength);
    strncpy(timelabel[4],"event queue handling",maxlabellength);
    strncpy(timelabel[5],"textstatus window",maxlabellength);
    strncpy(timelabel[6],"critter:graphics",maxlabellength);
    strncpy(timelabel[7],"critter:brain",maxlabellength);
    strncpy(timelabel[8],"critter:graphics:draw",maxlabellength);
    strncpy(timelabel[9],"critter:graphics:read",maxlabellength);
    strncpy(timelabel[10],"critter:graphics:swap",maxlabellength);
    strncpy(timelabel[11],"critter:brain:retina",maxlabellength);
    strncpy(timelabel[12],"critter:brain:neurons",maxlabellength);
    strncpy(timelabel[13],"critter:brain:synapses",maxlabellength);
    strncpy(timelabel[14],"critter:graphics:draw:aspect",maxlabellength);
    strncpy(timelabel[15],"critter:graphics:draw:current",maxlabellength);
    strncpy(timelabel[16],"critter:graphics:draw:justdraw",maxlabellength);
    strncpy(timelabel[17],"critter:graphics:draw:justdraw:reshapeviewport",maxlabellength);
    strncpy(timelabel[18],"critter:graphics:draw:justdraw:adjustscrmask",maxlabellength);
    strncpy(timelabel[19],"critter:graphics:draw:justdraw:czclear",maxlabellength);
    strncpy(timelabel[20],"critter:graphics:draw:justdraw:scene->draw",maxlabellength);
    strncpy(timelabel[21],"scene->draw:camera->use",maxlabellength);
    strncpy(timelabel[22],"scene->draw:stage->setcurrentcamera",maxlabellength);
    strncpy(timelabel[23],"scene->draw:stage->setdrawlights",maxlabellength);
    strncpy(timelabel[24],"scene->draw:stage->draw",maxlabellength);

    timegrouplen[0] = 3;
    timegroup[0][0] = 1; timegroup[0][1] = 2; timegroup[0][2] = 3;

    walltotal = times(&timebuf);
    usercputotal = timebuf.tms_utime;
    syscputotal = timebuf.tms_stime;
#endif TIMING
}

void timefinish()
{
#ifdef TIMING
    walltotal = times(&timebuf) - walltotal;
    usercputotal = timebuf.tms_utime - usercputotal;
    syscputotal = timebuf.tms_stime - syscputotal;

    for (short i = 0; i < timeevents; i++)
    {
        cout << "**" << timelabel[i] << "..." nl;
        cout << "   # of calls = " << timecalls[i] nl;

        cout << "   user_cpu seconds = " << usercpu[i]/FHZ nl;
        cout << "     % of total user_cpu         = "
             << 100.*usercpu[i]/usercputotal nl;
        cout << "     % of main loop user_cpu     = "
             << 100.*usercpu[i]/usercpu[0] nl;
        cout << "     % of total user+sys_cpu     = "
             << 100.*usercpu[i]/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of main loop user+sys_cpu = "
             << 100.*usercpu[i]/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of total wall = "
             << 100.*usercpu[i]/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*usercpu[i]/wall[0] nl;

        cout << "   sys_cpu seconds = " << syscpu[i]/FHZ nl;
        cout << "     % of total sys_cpu         = "
             << 100.*syscpu[i]/syscputotal nl;
        cout << "     % of main loop sys_cpu     = "
             << 100.*syscpu[i]/syscpu[0] nl;
        cout << "     % of total user+sys_cpu     = "
             << 100.*syscpu[i]/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of main loop user+sys_cpu = "
             << 100.*syscpu[i]/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of total wall = "
             << 100.*syscpu[i]/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*syscpu[i]/wall[0] nl;

        cout << "   wall seconds = " << wall[i]/FHZ nl;
        cout << "     % of total wall = "
             << 100.*wall[i]/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*wall[i]/wall[0] nl;

    }
    for (i = 0; i < timegroups; i++)
    {
        float groupusercpu = 0.;
        float groupsyscpu = 0.;
        float groupwall = 0.;
        cout << "@@";
        for (short j = 0; j < timegrouplen[i]; j++)
        {
            groupusercpu += usercpu[timegroup[i][j]];
            groupsyscpu += syscpu[timegroup[i][j]];
            groupwall += wall[timegroup[i][j]];
            cout << timelabel[timegroup[i][j]];
            if (j == (timegrouplen[i]-1))
                cout << "..." nl;
            else
                cout << " + ";
        }
        cout << "   user_cpu seconds = " << groupusercpu/FHZ nl;
        cout << "     % of total user_cpu         = "
             << 100.*groupusercpu/usercputotal nl;
        cout << "     % of main loop user_cpu     = "
             << 100.*groupusercpu/usercpu[0] nl;
        cout << "     % of total user+sys_cpu     = "
             << 100.*groupusercpu/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of main loop user+sys_cpu = "
             << 100.*groupusercpu/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of total wall = "
             << 100.*groupusercpu/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*groupusercpu/wall[0] nl;

        cout << "   sys_cpu seconds = " << groupsyscpu/FHZ nl;
        cout << "     % of total sys_cpu          = "
             << 100.*groupsyscpu/syscputotal nl;
        cout << "     % of main loop sys_cpu      = "
             << 100.*groupsyscpu/syscpu[0] nl;
        cout << "     % of total user+sys_cpu     = "
             << 100.*groupsyscpu/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of main loop user+sys_cpu = "
             << 100.*groupsyscpu/(usercpu[0]+syscpu[0]) nl;
        cout << "     % of total wall = "
             << 100.*groupsyscpu/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*groupsyscpu/wall[0] nl;

        cout << "   wall seconds = " << groupwall/FHZ nl;
        cout << "     % of total wall = "
             << 100.*groupwall/walltotal nl;
        cout << "     % of main loop wall = "
             << 100.*groupwall/wall[0] nl;
    }

    cout << "-----------------" nl;
    cout << "Summary of wall-time % of main loop wall values:" nl;
    for (i = 0; i < timeevents; i++)
    {
        cout << timelabel[i] << ":  " << 100.*wall[i]/wall[0] nl;
    }

    cout << "-----------------" nl;
    cout << "Summary of wall-time seconds & per-frame seconds:" nl;
    for (i = 0; i < timeevents; i++)
    {
        cout << timelabel[i] << ":  " << wall[i]/FHZ
             cms wall[i]/(FHZ*age) nl;
    }

    cout << "-----------------" nl;
#endif TIMING
    cout << "After running for " << age << " cycles:" nl;
    cout << "Average user-cpu/wall = " << float(usercpu[0])/wall[0] nl;
    cout << "Average  sys-cpu/wall = " << float( syscpu[0])/wall[0] nl;
    cout << "Average frame rate = "
         << FHZ*age/wall[0] << " frames per sec" nl;
    cout.flush();
}

void timebeg(short timeevent)
{
    if ( (timeevent >= 0) && (timeevent < timeevents) )
    {
        timecalls[timeevent]++;
        wallstart[timeevent] = times(&timebuf);
        usercpustart[timeevent] = timebuf.tms_utime;
        syscpustart[timeevent] = timebuf.tms_stime;
    }
}

void timeend(short timeevent)
{
    if ( (timeevent >= 0) && (timeevent < timeevents) )
    {
        wall[timeevent] += times(&timebuf) - wallstart[timeevent];
        usercpu[timeevent] += timebuf.tms_utime - usercpustart[timeevent];
        syscpu[timeevent] += timebuf.tms_stime - syscpustart[timeevent];
    }
}


void cleanup()
{
#ifdef DEBUGCALLS
    pushproc("cleanup");
#endif DEBUGCALLS
    long i;
//  setmonitor(HZ60);
//  if (video)
    {
#ifdef SYSTEMVIDEO
        system("60hz");
#else
        setvideo(DE_R1,DER1_60HZ | DER1_SYNCG | DER1_UNBLANK); // return to 60HZ
#endif
        blanktime(36000);
    }
    food* f = NULL;
    xsortedfood.reset();
    while (xsortedfood.next(f))
        delete f;
    xsortedfood.clear();
    critter* c = NULL;
    // all the critters are deleted in critterdestruct
    // rather than cycling through just those in xsortedcritters here
    xsortedcritters.clear();
    barrier* b = NULL;
    xsortedbarriers.reset();
    while (xsortedbarriers.next(b))
        delete b;
    xsortedbarriers.clear();
    worldstage.clear();
    screen.clear();
    for (short id = 0; id < numdomains; id++)
    {
        if (domains[id].fittest)
        {
            for (i = 0; i < numfit; i++)
                if (domains[id].fittest[i])
                    delete domains[id].fittest[i];
            delete domains[id].fittest;
        }
        if (domains[id].fitness) delete domains[id].fitness;
    }
    if (fittest)
    {
        for (i = 0; i < numfit; i++)
            if (fittest[i])
                delete fittest[i];
        delete fittest;
    }
    if (fitness)
        delete fitness;
    genomedestruct();
    braindestruct();
    critterdestruct();
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


/*
int onterm()
{
    cout << "Exiting normally after trapping the SIGTERM signal" nl;
    cleanup();
    if (TRUE) exit(0);
    return 0;
}
*/


void makemaxneurvals()
{
    maxnoninputneurgroups = maxinternalneurgroups + numoutneurgroups;
    maxneurgroups = maxnoninputneurgroups + numinputneurgroups;
    maxneurpergroup = maxeneurpergroup + maxineurpergroup;
    maxinternalneurons = maxneurpergroup * maxinternalneurgroups;
    maxinputneurons = maxvispixels*3 + 2;
    maxnoninputneurons = maxinternalneurons + numoutneur;
    maxneurons = maxinternalneurons + maxinputneurons + numoutneur;
    // the 2's are due to the input & output neurons
    //     doubling as e & i presynaptically
    // the 3's are due to the output neurons also acting as e-neurons
    //     postsynaptically, accepting internal connections
    // the -'s are due to the output & internal neurons not self-stimulating
    maxsynapses = maxinternalneurons*maxinternalneurons // internal
                + 2*numoutneur*numoutneur               // output
                + 3*maxinternalneurons*numoutneur       // internal/output
                + 2*maxinternalneurons*maxinputneurons  // internal/input
                + 2*maxinputneurons*numoutneur          // input/output
                - 2*numoutneur				// output/output
                - maxinternalneurons;                   // internal/internal
}


void readworldfile(cchar* filename)
{
#ifdef DEBUGCALLS
    pushproc("readworldfile");
#endif DEBUGCALLS
    filebuf fb;
    if (fb.open(filename,input) == 0)
    {
        error(0,"unable to open world file \"",filename,"\"");
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }
    istream in(&fb);
    short version;
    char label[64];

    in >> version; in >> label;

    if (version < 1)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> docpuwork; in >> label;
    cout << "docpuwork" ses docpuwork nl;
    in >> dumpfreq; in >> label;
    cout << "dumpfreq" ses dumpfreq nl;
    in >> statfreq; in >> label;
    cout << "statfreq" ses statfreq nl;
    in >> edges; in >> label;
    cout << "edges" ses edges nl;
    in >> wraparound; in >> label;
    cout << "wraparound" ses wraparound nl;
    in >> vision; in >> label;
    if (!graphics)
        vision = FALSE;
    cout << "vision" ses vision nl;
    in >> showvision; in >> label;
    if (!graphics)
        showvision = FALSE;
    cout << "showvision" ses showvision nl;
    in >> minwin; in >> label;
    cout << "minwin" ses minwin nl;
    in >> maxvel; in >> label;
    cout << "maxvel" ses maxvel nl;
    in >> minnumcritters; in >> label;
    cout << "minnumcritters" ses minnumcritters nl;
    in >> maxnumcritters; in >> label;
    cout << "maxnumcritters" ses maxnumcritters nl;
    in >> initnumcritters; in >> label;
    cout << "initnumcritters" ses initnumcritters nl;
    in >> misccritters; in >> label;
    cout << "misccritters" ses misccritters nl;
    in >> positionseed; in >> label;
    cout << "positionseed" ses positionseed nl;
    in >> genomeseed; in >> label;
    cout << "genomeseed" ses genomeseed nl;
    in >> minfoodcount; in >> label;
    cout << "minfoodcount" ses minfoodcount nl;
    in >> maxfoodcount; in >> label;
    cout << "maxfoodcount" ses maxfoodcount nl;
    in >> maxfoodgrown; in >> label;
    cout << "maxfoodgrown" ses maxfoodgrown nl;
    in >> initfoodcount; in >> label;
    cout << "initfoodcount" ses initfoodcount nl;
    in >> foodrate; in >> label;
    cout << "foodrate" ses foodrate nl;
    in >> crittersRfood; in >> label;
    cout << "crittersRfood" ses crittersRfood nl;
    in >> fit1freq; in >> label;
    cout << "fit1freq" ses fit1freq nl;
    in >> fit2freq; in >> label;
    cout << "fit2freq" ses fit2freq nl;
    in >> numfit; in >> label;
    cout << "numfit" ses numfit nl;
    in >> eatfitparam; in >> label;
    cout << "eatfitparam" ses eatfitparam nl;
    in >> matefitparam; in >> label;
    cout << "matefitparam" ses matefitparam nl;
    in >> movefitparam; in >> label;
    cout << "movefitparam" ses movefitparam nl;
    in >> energyfitparam; in >> label;
    cout << "energyfitparam" ses energyfitparam nl;
    in >> agefitparam; in >> label;
    cout << "agefitparam" ses agefitparam nl;
  totalfitness = eatfitparam + matefitparam + movefitparam + energyfitparam
               + agefitparam;
    in >> minfoodenergy; in >> label;
    cout << "minfoodenergy" ses minfoodenergy nl;
    in >> maxfoodenergy; in >> label;
    cout << "maxfoodenergy" ses maxfoodenergy nl;
    in >> size2energy; in >> label;
    cout << "size2energy" ses size2energy nl;
    in >> eat2consume; in >> label;
    cout << "eat2consume" ses eat2consume nl;
    in >> minintneurons; in >> label;
    cout << "minintneurons" ses minintneurons nl;
    in >> maxintneurons; in >> label;
    cout << "maxintneurons" ses maxintneurons nl;
    // note: minintneurons & maxintneurons are no longer used
    cout << "note: minintneurons & maxintneurons are no longer used" nl;
    in >> minvispixels; in >> label;
    cout << "minvispixels" ses minvispixels nl;
    in >> maxvispixels; in >> label;
    cout << "maxvispixels" ses maxvispixels nl;
    in >> minmrate; in >> label;
    cout << "minmrate" ses minmrate nl;
    in >> maxmrate; in >> label;
    cout << "maxmrate" ses maxmrate nl;
    in >> minnumcpts; in >> label;
    cout << "minnumcpts" ses minnumcpts nl;
    in >> maxnumcpts; in >> label;
    cout << "maxnumcpts" ses maxnumcpts nl;
    in >> minlifespan; in >> label;
    cout << "minlifespan" ses minlifespan nl;
    in >> maxlifespan; in >> label;
    cout << "maxlifespan" ses maxlifespan nl;
    in >> matewait; in >> label;
    cout << "matewait" ses matewait nl;
    in >> initmatewait; in >> label;
    cout << "initmatewait" ses initmatewait nl;
    in >> minstrength; in >> label;
    cout << "minstrength" ses minstrength nl;
    in >> maxstrength; in >> label;
    cout << "maxstrength" ses maxstrength nl;
    in >> mincsize; in >> label;
    cout << "mincsize" ses mincsize nl;
    in >> maxcsize; in >> label;
    cout << "maxcsize" ses maxcsize nl;
    in >> minmaxenergy; in >> label;
    cout << "minmaxenergy" ses minmaxenergy nl;
    in >> maxmaxenergy; in >> label;
    cout << "maxmaxenergy" ses maxmaxenergy nl;
    in >> minmateenergy; in >> label;
    cout << "minmateenergy" ses minmateenergy nl;
    in >> maxmateenergy; in >> label;
    cout << "maxmateenergy" ses maxmateenergy nl;
    in >> minmatefrac; in >> label;
    cout << "minmatefrac" ses minmatefrac nl;
    in >> minmaxspeed; in >> label;
    cout << "minmaxspeed" ses minmaxspeed nl;
    in >> maxmaxspeed; in >> label;
    cout << "maxmaxspeed" ses maxmaxspeed nl;
    in >> speed2dpos; in >> label;
    cout << "speed2dpos" ses speed2dpos nl;
    in >> yaw2dyaw; in >> label;
    cout << "yaw2dyaw" ses yaw2dyaw nl;
    in >> minlrate; in >> label;
    cout << "minlrate" ses minlrate nl;
    in >> maxlrate; in >> label;
    cout << "maxlrate" ses maxlrate nl;
    in >> minfocus; in >> label;
    cout << "minfocus" ses minfocus nl;
    in >> maxfocus; in >> label;
    cout << "maxfocus" ses maxfocus nl;
    in >> critterfovy; in >> label;
    cout << "critterfovy" ses critterfovy nl;
    in >> maxsizeadvantage; in >> label;
    cout << "maxsizeadvantage" ses maxsizeadvantage nl;
    in >> power2energy; in >> label;
    cout << "power2energy" ses power2energy nl;
    in >> eat2energy; in >> label;
    cout << "eat2energy" ses eat2energy nl;
    in >> mate2energy; in >> label;
    cout << "mate2energy" ses mate2energy nl;
    in >> fight2energy; in >> label;
    cout << "fight2energy" ses fight2energy nl;
    in >> maxsizepenalty; in >> label;
    cout << "maxsizepenalty" ses maxsizepenalty nl;
    in >> speed2energy; in >> label;
    cout << "speed2energy" ses speed2energy nl;
    in >> yaw2energy; in >> label;
    cout << "yaw2energy" ses yaw2energy nl;
    in >> light2energy; in >> label;
    cout << "light2energy" ses light2energy nl;
    in >> focus2energy; in >> label;
    cout << "focus2energy" ses focus2energy nl;
    in >> maxsynapse2energy; in >> label;
    cout << "maxsynapse2energy" ses maxsynapse2energy nl;
    in >> fixedenergydrain; in >> label;
    cout << "fixedenergydrain" ses fixedenergydrain nl;
    in >> decayrate; in >> label;
    cout << "decayrate" ses decayrate nl;
    in >> eatthreshold; in >> label;
    cout << "eatthreshold" ses eatthreshold nl;
    in >> matethreshold; in >> label;
    cout << "matethreshold" ses matethreshold nl;
    in >> fightthreshold; in >> label;
    cout << "fightthreshold" ses fightthreshold nl;
    in >> miscbias; in >> label;
    cout << "miscbias" ses miscbias nl;
    in >> miscinvslope; in >> label;
    cout << "miscinvslope" ses miscinvslope nl;
    in >> logisticslope; in >> label;
    cout << "logisticslope" ses logisticslope nl;
    in >> maxweight; in >> label;
    cout << "maxweight" ses maxweight nl;
    in >> initmaxweight; in >> label;
    cout << "initmaxweight" ses initmaxweight nl;
    in >> minbitprob; in >> label;
    cout << "minbitprob" ses minbitprob nl;
    in >> maxbitprob; in >> label;
    cout << "maxbitprob" ses maxbitprob nl;
    in >> critterheight; in >> label;
    cout << "critterheight" ses critterheight nl;
    in >> foodheight; in >> label;
    cout << "foodheight" ses foodheight nl;
    in >> foodcolor.r >> foodcolor.g >> foodcolor.b >> label;
    cout << "foodcolor = (" << foodcolor.r cms
                               foodcolor.g cms
                               foodcolor.b pnl;
    in >> barrierheight; in >> label;
    cout << "barrierheight" ses barrierheight nl;
    in >> barriercolor.r >> barriercolor.g >> barriercolor.b >> label;
    cout << "barriercolor = (" << barriercolor.r cms
                                  barriercolor.g cms
                                  barriercolor.b pnl;
    in >> groundcolor.r >> groundcolor.g >> groundcolor.b >> label;
    cout << "groundcolor = (" << groundcolor.r cms
                                 groundcolor.g cms
                                 groundcolor.b pnl;
    in >> groundclearance; in >> label;
    cout << "groundclearance" ses groundclearance nl;
    in >> cameracolor.r >> cameracolor.g >> cameracolor.b >> label;
    cout << "cameracolor = (" << cameracolor.r cms
                                 cameracolor.g cms
                                 cameracolor.b pnl;
    in >> camradius; in >> label;
    cout << "camradius" ses camradius nl;
    in >> camheight; in >> label;
    cout << "camheight" ses camheight nl;
    in >> camrotationrate; in >> label;
    cout << "camrotationrate" ses camrotationrate nl;
    in >> camanglestart; in >> label;
    cout << "camanglestart" ses camanglestart nl;
    in >> camfov; in >> label;
    cout << "camfov" ses camfov nl;
    in >> moncritrank; in >> label;
    if (!graphics)
        moncritrank = 0; // cannot monitor critter brain without graphics
    cout << "moncritrank" ses moncritrank nl;
    in >> monwidth; in >> label;
    cout << "monwidth" ses monwidth nl;
    in >> monheight; in >> label;
    cout << "monheight" ses monheight nl;
    in >> monstride; in >> label;
    cout << "monstride" ses monstride nl;
    in >> worldsize; in >> label;
    cout << "worldsize" ses worldsize nl;
    long numbarriers;
    barrier* b = NULL;
    float x1; float z1; float x2; float z2;
    in >> numbarriers; in >> label;
    cout << "numbarriers = " << numbarriers nl;
    for (long i = 0; i < numbarriers; i++)
    {
        in >> x1 >> z1 >> x2 >> z2 >> label;
        cout << "barrier #" << i << " position = ("
             << x1 cms z1 << ") to (" << x2 cms z2 pnl;
        b = new barrier(x1,z1,x2,z2);
        if (b)
            xsortedbarriers.add(b);
        else
            error(1,"Insufficient memory to define barriers");
    }
    cout nlf;

    if (version < 2)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> genesepmon; in >> label;
    cout << "genesepmon" ses genesepmon nl;
    in >> geneseprec; in >> label;
    cout << "geneseprec" ses geneseprec nl;
    cout nlf;

    if (version < 3)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> chartborn; in >> label;
    cout << "chartborn" ses chartborn nl;
    in >> chartfit; in >> label;
    cout << "chartfit" ses chartfit nl;
    in >> chartfoodenergy; in >> label;
    cout << "chartfoodenergy" ses chartfoodenergy nl;
    in >> chartpop; in >> label;
    cout << "chartpop" ses chartpop nl;
    cout nlf;

    if (version < 4)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> numdomains; in >> label;
    cout << "numdomains" ses numdomains nl;
    if (numdomains < 1)
    {
        sprintf(tempstring,"%s%d%s\0",
            "Number of fitness domains must always be > 0 (not ",
            numdomains,").\nIt will be reset to 1.");
        error(1,tempstring);
        numdomains = 1;
    }
    if (numdomains > MAXDOMAINS)
    {
        sprintf(tempstring,
            "Too many fitness domains requested (%d > %d)\0",
            numdomains,MAXDOMAINS);
        error(2,tempstring);
    }

    short id;
    domains[0].xleft = 0.0;
    for (id = 0; id < numdomains-1; id++)
    {
        in >> domains[id].xright; in >> label;
        cout << "barrier between domains " << id << " and " << (id+1)
             << " is at x = " << domains[id].xright nl;
        domains[id].xsize = domains[id].xright - domains[id].xleft;
        domains[id+1].xleft = domains[id].xright;
    }
    domains[id].xright = worldsize;
    domains[id].xsize = domains[id].xright - domains[id].xleft;

    if (numdomains > 1)
    {
        long totmaxnumcritters = 0;
        long totminnumcritters = 0;
        for (id = 0; id < numdomains; id++)
        {
            in >> domains[id].minnumcritters; in >> label;
            cout << "minnumcritters in domains[" << id << "]"
                 ses domains[id].minnumcritters nl;
            in >> domains[id].maxnumcritters; in >> label;
            cout << "maxnumcritters in domains[" << id << "]"
                 ses domains[id].maxnumcritters nl;
            in >> domains[id].initnumcritters; in >> label;
            cout << "initnumcritters in domains[" << id << "]"
                 ses domains[id].initnumcritters nl;
            in >> domains[id].minfoodcount; in >> label;
            cout << "minfoodcount in domains[" << id << "]"
                 ses domains[id].minfoodcount nl;
            in >> domains[id].maxfoodcount; in >> label;
            cout << "maxfoodcount in domains[" << id << "]"
                 ses domains[id].maxfoodcount nl;
            in >> domains[id].maxfoodgrown; in >> label;
            cout << "maxfoodgrown in domains[" << id << "]"
                 ses domains[id].maxfoodgrown nl;
            in >> domains[id].initfoodcount; in >> label;
            cout << "initfoodcount in domains[" << id << "]"
                 ses domains[id].initfoodcount nl;
            totmaxnumcritters += domains[id].maxnumcritters;
            totminnumcritters += domains[id].minnumcritters;
        }
        if (totmaxnumcritters > maxnumcritters)
        {
            sprintf(tempstring,"%s%d%s%d%s\0",
                "The maximum number of organisms in the world (",
                maxnumcritters,
                ") is < the maximum summed over domains (",
                totmaxnumcritters,
                "), so there may still be some indirect global influences.");
            errorflash(0,tempstring);
        }
        if (totminnumcritters < minnumcritters)
        {
            sprintf(tempstring,"%s%d%s%d%s\0",
                "The minimum number of organisms in the world (",
                minnumcritters,
                ") is > the minimum summed over domains (",
                totminnumcritters,
                "), so there may still be some indirect global influences.");
            errorflash(0,tempstring);
        }
    }
    else
    {
        domains[0].minnumcritters = minnumcritters;
        domains[0].maxnumcritters = maxnumcritters;
        domains[0].initnumcritters = initnumcritters;
        domains[0].minfoodcount = minfoodcount;
        domains[0].maxfoodcount = maxfoodcount;
        domains[0].maxfoodgrown = maxfoodgrown;
        domains[0].initfoodcount = initfoodcount;
    }

    for (id = 0; id < numdomains; id++)
    {
        domains[id].numcritters = 0;
        domains[id].numcreated = 0;
        domains[id].numborn = 0;
        domains[id].numbornsincecreated = 0;
        domains[id].numdied = 0;
        domains[id].lastcreate = 0;
        domains[id].maxgapcreate = 0;
        domains[id].foodcount = 0;
        domains[id].ifit = 0;
        domains[id].jfit = 1;
        domains[id].fittest = NULL;
        domains[id].fitness = NULL;
    }

    cout nlf;

    if (version < 5)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> binchartgs; in >> label;
    cout << "binchartgs" ses binchartgs nl;
    if (binchartgs)
        genesepmon = TRUE;

    cout nlf;

    if (version < 6)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> mininternalneurgroups; in >> label;
    cout << "mininternalneurgroups" ses mininternalneurgroups nl;
    in >> maxinternalneurgroups; in >> label;
    cout << "maxinternalneurgroups" ses maxinternalneurgroups nl;
    in >> mineneurpergroup; in >> label;
    cout << "mineneurpergroup" ses mineneurpergroup nl;
    in >> maxeneurpergroup; in >> label;
    cout << "maxeneurpergroup" ses maxeneurpergroup nl;
    in >> minineurpergroup; in >> label;
    cout << "minineurpergroup" ses minineurpergroup nl;
    in >> maxineurpergroup; in >> label;
    cout << "maxineurpergroup" ses maxineurpergroup nl;
    in >> minbias; in >> label;
    cout << "minbias" ses minbias nl;
    in >> maxbias; in >> label;
    cout << "maxbias" ses maxbias nl;
    in >> minbiaslrate; in >> label;
    cout << "minbiaslrate" ses minbiaslrate nl;
    in >> maxbiaslrate; in >> label;
    cout << "maxbiaslrate" ses maxbiaslrate nl;
    in >> minconnectiondensity; in >> label;
    cout << "minconnectiondensity" ses minconnectiondensity nl;
    in >> maxconnectiondensity; in >> label;
    cout << "maxconnectiondensity" ses maxconnectiondensity nl;
    in >> mintopologicaldistortion; in >> label;
    cout << "mintopologicaldistortion" ses mintopologicaldistortion nl;
    in >> maxtopologicaldistortion; in >> label;
    cout << "maxtopologicaldistortion" ses maxtopologicaldistortion nl;
    in >> maxneuron2energy; in >> label;
    cout << "maxneuron2energy" ses maxneuron2energy nl;
    in >> numprebirthcycles; in >> label;
    cout << "numprebirthcycles" ses numprebirthcycles nl;

    makemaxneurvals();

    cout << "maxneurgroups" ses maxneurgroups nl;
    cout << "maxneurons" ses maxneurons nl;
    cout << "maxsynapses" ses maxsynapses nl;

    in >> overheadrank; in >> label;
    cout << "overheadrank" ses overheadrank nl;
    in >> tracking; in >> label;
    cout << "tracking" ses tracking nl;
    in >> minfoodenergyatdeath; in >> label;
    cout << "minfoodenergyatdeath" ses minfoodenergyatdeath nl;

    in >> graycoding; in >> label;
    cout << "graycoding" ses graycoding nl;

#ifdef OF1
// Following items are unique to the optimal foraging version
    in >> deathprob; in >> label;
    cout << "deathprob" ses deathprob nl;
#endif

    if (version < 7)
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }

    in >> smite; in >> label;
    cout << "smite" ses smite nl;
    in >> minsmiteage; in >> label;
    cout << "minsmiteage" ses minsmiteage nl;

    cout nlf;

    fb.close();

#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


short whichdomain(float x, float z, short d)
{
    if ( ((x >= domains[d].xleft) && (x <= domains[d].xright)) ||
         ((d == 0) && (x <= domains[d].xright)) ||
         ((d == numdomains-1) && (x >= domains[d].xleft)) )
        return d;
    short nd = d;
    for (short i = 0; i < numdomains-1; i++)
    {
        nd++;
        if (nd > (numdomains-1))
            nd = 0;
        if ( ((x >= domains[nd].xleft) && (x <= domains[nd].xright)) ||
             ((nd == 0) && (x <= domains[nd].xright)) ||
             ((nd == numdomains-1) && (x >= domains[nd].xleft)) )
            return nd;
    }
    sprintf(tempstring,"%s(%g,%g)%s%d,%d\0",
        "whichdomain failed to find any domain for point at (x,z) = ",
        x,z," & d,nd = ",d,nd);
    error(2,tempstring);
    return -1; // not really returning, as error(2,...) will abort
}


void textstatus(gwindow& statwin, Boolean filestatus, cchar* filename)
{
#ifdef DEBUGCALLS
    pushproc("textstatus");
#endif DEBUGCALLS
    char t[256];
    char t2[256];
    short id;
    if (graphics) {
// Don't need following makecurrentwindow() so long as RESTORE is off in gwindow
//      statwin.makecurrentwindow();
        statwin.clearc(); // does makecurrentwindow & makecurrentcolor
        lpt size;
        long x,y;
        getsize(&size.x,&size.y);
        statwin.makecurrentcolor(1.,1.,1.);

        x = size.x/4 - strwidth((String)filename)/2;
        if (x < 5)
            x = 5;
        y = size.y - 15;
        cmov2i(x,y);
        charstr((String)filename);

        x = 5;

        sprintf(t,"age = %d",age);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"critters = %4d",xsortedcritters.count());
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].numcritters);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].numcritters);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"created  = %4d",numcreated);
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].numcreated);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].numcreated);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -random = %4d",numcreatedrandom);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -two    = %4d",numcreated2fit);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -one    = %4d",numcreated1fit);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"born     = %4d",numborn);
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].numborn);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].numborn);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"died     = %4d",numdied);
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].numdied);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].numdied);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -age    = %4d",numdiedage);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -energy = %4d",numdiedenergy);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -fight  = %4d",numdiedfight);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t," -edge   = %4d",numdiededge);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
/*
        sprintf(t,"fights = %d",numfights);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
*/
        sprintf(t,"food = %d",xsortedfood.count());
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].foodcount);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].foodcount);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"miscden = %d",miscnobirth);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"agecreat = %d",lastcreate);
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].lastcreate);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].lastcreate);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"maxgapcr = %d",maxgapcreate);
        if (numdomains > 1)
        {
            sprintf(t2," (%d\0",domains[0].maxgapcreate);
            strcat(t,t2);
            for (id = 1; id < numdomains; id++)
            {
                sprintf(t2,",%d\0",domains[id].maxgapcreate);
                strcat(t,t2);
            }
            strcat(t,")");
        }
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"born/total = %.2f",float(numborn)/float(numcreated+numborn));
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"maxfitN = %.2f",maxfitness/totalfitness);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"cmaxfitN = %.2f",curmaxfitness[0]/totalfitness);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"avgfitN = %.2f",avgfitness/totalfitness);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"maxfit = %g",maxfitness);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"cmaxfit = %g",curmaxfitness[0]);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"avgfit = %g",avgfitness);
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"avgfenergy = %.2f",(avgfoodenergyin-avgfoodenergyout)
                                     /(avgfoodenergyin+avgfoodenergyout));
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        sprintf(t,"totfenergy = %.2f",(totfoodenergyin-totfoodenergyout)
                                     /(totfoodenergyin+totfoodenergyout));
        y -= 15;
        cmov2i(x,y);
        charstr((String)t);
        if (genesepmon)
        {
            sprintf(t,"genesepmax = %g",genesepmax);
            y -= 15;
            cmov2i(x,y);
            charstr((String)t);
            sprintf(t,"genesepmin = %g",genesepmin);
            y -= 15;
            cmov2i(x,y);
            charstr((String)t);
            sprintf(t,"genesepavg = %g",genesepavg);
            y -= 15;
            cmov2i(x,y);
            charstr((String)t);
        }

        statwin.justswapbuffers();
        statwin.restorecurrentwindow();
        statwin.restorecurrentcolor();

    }  // if (graphics)

    if (filestatus) {
        FILE *statusfile = fopen("pw.stat","w");
        fprintf(statusfile,"%s\n",filename);
        fprintf(statusfile,"age = %d\n",age);
        fprintf(statusfile,"critters = %4d",xsortedcritters.count());
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].numcritters);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].numcritters);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"created  = %4d",numcreated);
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].numcreated);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].numcreated);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile," -random = %4d\n",numcreatedrandom);
        fprintf(statusfile," -two    = %4d\n",numcreated2fit);
        fprintf(statusfile," -one    = %4d\n",numcreated1fit);
        fprintf(statusfile,"born     = %4d",numborn);
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].numborn);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].numborn);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"died     = %4d",numdied);
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].numdied);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].numdied);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile," -age    = %4d\n",numdiedage);
        fprintf(statusfile," -energy = %4d\n",numdiedenergy);
        fprintf(statusfile," -fight  = %4d\n",numdiedfight);
        fprintf(statusfile," -edge   = %4d\n",numdiededge);
        fprintf(statusfile,"fights = %d\n",numfights);
        fprintf(statusfile,"maxfit = %g\n",maxfitness);
        fprintf(statusfile,"food = %d",xsortedfood.count());
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].foodcount);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].foodcount);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"miscden = %d\n",miscnobirth);
        fprintf(statusfile,"agecreat = %d",lastcreate);
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].lastcreate);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].lastcreate);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"maxgapcr = %d",maxgapcreate);
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%4d",domains[0].maxgapcreate);
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %4d",domains[id].maxgapcreate);
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"born/total = %.2f",
            float(numborn)/float(numcreated+numborn));
        if (numdomains > 1)
        {
            fprintf(statusfile,"  (%.2f",
                float(domains[0].numborn)/
                float(domains[0].numcreated+domains[0].numborn));
            for (id = 1; id < numdomains; id++)
                fprintf(statusfile,", %.2f",
                    float(domains[id].numborn)/
                    float(domains[id].numcreated+domains[id].numborn));
            fprintf(statusfile,")");
        }
        fprintf(statusfile,"\n");
        fprintf(statusfile,"maxfitnorm = %.2f\n",maxfitness/totalfitness);
        fprintf(statusfile,"curmaxfitnorm = %.2f\n",curmaxfitness[0]/totalfitness);
        fprintf(statusfile,"avgfitnorm = %.2f\n",avgfitness/totalfitness);
        fprintf(statusfile,"maxfit = %g\n",maxfitness);
        fprintf(statusfile,"curmaxfit = %g\n",curmaxfitness[0]);
        fprintf(statusfile,"avgfit = %g\n",avgfitness);
        fprintf(statusfile,"average foodenergy flux = %.2f\n",
            (avgfoodenergyin-avgfoodenergyout)
           /(avgfoodenergyin+avgfoodenergyout));
        fprintf(statusfile,"total foodenergy flux = %.2f\n",
            (totfoodenergyin-totfoodenergyout)
           /(totfoodenergyin+totfoodenergyout));
        if (genesepmon)
        {
            fprintf(statusfile,"genesepmax = %g\n",genesepmax);
            fprintf(statusfile,"genesepmin = %g\n",genesepmin);
            fprintf(statusfile,"genesepavg = %g\n",genesepavg);
        }

        fclose(statusfile);
    }  // if (filestatus)
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void initworld()
{
    docpuwork = TRUE;
    dumpfreq = 500;
    statfreq = 100;
    edges = TRUE;
    wraparound = FALSE;
    vision = TRUE;
    showvision = TRUE;
    minwin = 22;
    maxvel = 1.0;
    minnumcritters = 15;
    maxnumcritters = 50;
    initnumcritters = 15;
    misccritters = 150;
    positionseed = 42.;
    genomeseed = 42.;
    minfoodcount = 15;
    maxfoodcount = 50;
    maxfoodgrown = 25;
    initfoodcount = 25;
    foodrate = 0.1;
    crittersRfood = TRUE;
    fit1freq = 100;
    fit2freq = 2;
    numfit = 10;
    eatfitparam = 10.0;
    matefitparam = 10.0;
    movefitparam = 1.0;
    energyfitparam = 1.0;
    agefitparam = 1.0;
  totalfitness = eatfitparam + matefitparam + movefitparam + energyfitparam
               + agefitparam;
    minfoodenergy = 20.0;
    maxfoodenergy = 300.0;
    size2energy = 100.0;
    eat2consume = 20.0;
    minintneurons = 30;
    maxintneurons = 30;
    minvispixels = 1;
    maxvispixels = 8;
    minmrate = 0.01;
    maxmrate = 0.1;
    minnumcpts = 1;
    maxnumcpts = 5;
    minlifespan = 1000;
    maxlifespan = 2000;
    matewait = 25;
    initmatewait = 25;
    minstrength = 0.5;
    maxstrength = 2.0;
    mincsize = 1.0;
    maxcsize = 4.0;
    minmaxenergy = 500.0;
    maxmaxenergy = 1000.0;
    minmateenergy = 0.2;
    maxmateenergy = 0.8;
    minmatefrac = 0.05;
    minmaxspeed = 0.5;
    maxmaxspeed = 1.5;
    speed2dpos = 1.0;
    yaw2dyaw = 1.0;
    minlrate = 0.0;
    maxlrate = 0.1;
    minfocus = 20.0;
    maxfocus = 140.0;
    critterfovy = 10.0;
    maxsizeadvantage = 2.5;
    power2energy = 2.5;
    eat2energy = 0.01;
    mate2energy = 0.1;
    fight2energy = 1.0;
    maxsizepenalty = 10.0;
    speed2energy = 0.1;
    yaw2energy = 0.1;
    light2energy = 0.01;
    focus2energy = 0.001;
    maxsynapse2energy = 0.01;
    fixedenergydrain = 0.1;
    decayrate = 0.9;
    eatthreshold = 0.2;
    matethreshold = 0.5;
    fightthreshold = 0.2;
    miscbias = 1.0;
    miscinvslope = 2.0;
    logisticslope = 0.5;
    maxweight = 1.0;
    initmaxweight = 0.5;
    minbitprob = 0.1;
    maxbitprob = 0.9;
    critterheight = 0.2;
    foodheight = 0.6;
    foodcolor.r = 0.2;
    foodcolor.g = 0.6;
    foodcolor.b = 0.2;
    barrierheight = 5.0;
    barriercolor.r = 0.35;
    barriercolor.g = 0.25;
    barriercolor.b = 0.15;
    groundcolor.r = 0.1;
    groundcolor.g = 0.15;
    groundcolor.b = 0.05;
    groundclearance = 0.0;
    cameracolor.r = 1.0;
    cameracolor.g = 1.0;
    cameracolor.b = 1.0;
    camradius = 0.6;
    camheight = 0.35;
    camrotationrate = 0.09;
    camanglestart = 0.0;
    camfov = 90.;
    moncritrank = 0;
    moncritrankold = 0;
    moncrit = NULL;
    monwidth = 8;
    monheight = 8;
    monstride = 25;
    worldsize = 100.0;
    // numbarriers = 0; // barriers are dynamically allocated; too much hassle
                        // to deallocate them later, so implicitly default to 0

    genesepmon = FALSE;
    geneseprec = FALSE;

    numcreated = 0;
    numcreatedrandom = 0;
    numcreated2fit = 0;
    numcreated1fit = 0;
    numborn = 0;
    numdied = 0;
    numdiedage = 0;
    numdiedenergy = 0;
    numdiedfight = 0;
    numdiededge = 0;
    numfights = 0;
    maxfitness = 0.;
    avgfitness = 0.;
    miscnobirth = 0;
    lastcreate = 0;
    maxgapcreate = 0;
    genesepmin = 1.e+10;
    genesepmax = 0.0;
    genesepavg = 5.e+9;

    numbornsincecreated = 0;
    chartborn = TRUE;
    chartfit = TRUE;
    chartfoodenergy = TRUE;
    chartpop = TRUE;
    binchartgs = FALSE; // GeneSeparation (if TRUE, genesepmon must be TRUE)

    mininternalneurgroups = 1;
    maxinternalneurgroups = 5;
    mineneurpergroup = 1;
    maxeneurpergroup = 8;
    minineurpergroup = 1;
    maxineurpergroup = 8;
    minbias = -1.0;
    maxbias = 1.0;
    minbiaslrate = 0.0;
    maxbiaslrate = 0.2;
    minconnectiondensity = 0.0;
    maxconnectiondensity = 1.0;
    mintopologicaldistortion = 0.0;
    maxtopologicaldistortion = 1.0;
    maxneuron2energy = 0.1;
    numprebirthcycles = 25;
    makemaxneurvals();

    overheadrank = 0;
    overheadrankold = 0;
    overheadcrit = NULL;
    tracking = FALSE;

    minfoodenergyatdeath = minfoodenergy;
    graycoding = TRUE;

#ifdef OF1
    deathprob = 0.001;
#endif
    smite = 0;
    minsmiteage = (short)initmatewait + (short)matewait;

    ifit = 0;
    jfit = 1;
}


void tilecharts(long w,long h,short i,long *l,long *r,long *t,long *b)
{
    if (i & 1)  // odd index, so move to right
    {
        *l = *r + 6;
        *r = *l + w - 1;
    }
    else       // even index, so move down, and back to left
    {
        *t = *b - 5;
        *b = *t - h + 1;
        if (i)  // leave l & r alone on first call
        {
            *r = *l - 6;
            *l = *r - w + 1;
        }
    }
}


#ifdef OF1
void plotoptfor(chartwindow* ofwin)
{
    critter* c;
    float ebar = 0.0;
    float bbar = 0.0;
    float fbar = 0.0;
    float foptbar = 0.0;
    float foptofbar;
    float ebbar = 0.0;
    float tbbar = 0.0;
    float dedtinbar = 0.0;

    xsortedcritters.reset();
    while (xsortedcritters.next(c))
    {
        ebar += c->e();
        bbar += c->b();
        fbar += c->f();
        foptbar += c->fopt();
        ebbar += c->ebirth();
        tbbar += c->tbirth();
        dedtinbar += c->dedtin();
    }
    ebar /= xsortedcritters.count();
    bbar /= xsortedcritters.count();
    fbar /= xsortedcritters.count();
    foptbar /= xsortedcritters.count();
    ebbar /= xsortedcritters.count();
    tbbar /= xsortedcritters.count();
    dedtinbar /= xsortedcritters.count();

    foptofbar = (ebbar+fixedenergydrain*tbbar)/(dedtinbar*tbbar);

    ofwin->addpoint(0,ebar);
    ofwin->addpoint(1,bbar);
    ofwin->addpoint(2,foptbar);
    ofwin->addpoint(3,foptofbar);
    ofwin->addpoint(4,fbar);
}
#endif


main(int argc,char *argv[])
{
#ifdef DEBUGCALLS
    pushproc("main");
#endif DEBUGCALLS

    const int maxviews = 2;
    gscreen background("background");
    gwindow win[maxviews];
    gscene scene[maxviews];
    gcamera camera[maxviews];
    gpolyobj ground("ground");
    gset worldset;

#ifndef NORMALRECORD
    FILE* recfile;
#endif

    "ground.obj" >> ground;

#ifdef MYNEWDELETE
    startmybit();
#endif MYNEWDELETE

    extern void genesepcalcall();
    extern void geneseprecord();
    extern void interact();
    extern void dump();
    extern void load();

    Boolean loadstate = FALSE;
    graphics = TRUE;
    Boolean video = FALSE;
    Boolean preview = FALSE;
    long cgmode = 2;
    Boolean record = FALSE;
    long startrecord = 0;
    long black = 0;
    long maxloops = 1000000;
    unsigned int delay = 0;
    char filename[256];
    strcpy(filename,"worldfile");

    Boolean argerror = FALSE;

    if (argc > 1)
    {
        for (short i = 1; i < argc; i++)
        {
            switch (argv[i][0])
            {
            case '-':
                switch (argv[i][1])
                {
                case 'd':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%d",&delay);
                    else
                        delay = 1;
                    break;
                case 'f':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%s\0",filename);
                    else
                        sscanf("worldfile","%s\0",filename);
                    break;
                case 'l':
                    loadstate = TRUE;
                    break;
                case 'v':
                    video = TRUE;
                    if (argv[i][2] == 'p')
                        preview = TRUE;
                    else if (argv[i][2] == '3')
                        cgmode = 3;
                    break;
                case 'r':
                    record = TRUE;
                    break;
                case 'g':
                    graphics = FALSE;
                    break;
                case 'w':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%d,%d",&xscreensize,&yscreensize);
                    else
                        argerror = TRUE;
                    break;
                case 'p':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%d,%d",&screenleft,&screenbottom);
                    else
                        argerror = TRUE;
                    break;
                case 's':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%d",&startrecord);
                    else
                        argerror = TRUE;
                    break;
                case 'b':
                    if ((i+1) < argc)
                        sscanf(argv[++i],"%d",&black);
                    else
                        black = 30;
                    break;
                default:
                    cout << "unknown argument (-" << argv[i][1] pnl;
                    argerror = TRUE;
                }
                break;
            case '+':
                cout << "so what do i do with a '+'?" nl;
                argerror = TRUE;
                break;
            default:
                sscanf(argv[i],"%d",&maxloops);
            }
        }
    }

    if (argerror)
    {
        cerr << "Error in arguments.  Usage:" nl;
        cerr << "pw -l -g -d delay -f worldfilename" nl;
        cerr << "   -w width,height -p screenleft,screenbottom" nl;
        cerr << "   -v[p] -r -s startrecordframe -b blackframes" nlf;
        exit(0);
    }

    foreground();

    if (graphics)
    {

        if (video)
        {
            background.open(); // blank-out entire screen
            if (!preview)
            {
#ifdef SYSTEMVIDEO
                system("ntsc");
#else
//              setmonitor(NTSC);
                // set the de3 to RS 170, genlock
                // set the cg2 to mode 2, standalone master
                // reset the de3
                setvideo(DE_R1,(long) 0);
                // send 0xa8, genlocked RS170, no blanking
                setvideo(DE_R1,DER1_G_170 | DER1_UNBLANK);
                // set the mode to 2 (for cg2) or 3 (for cg3)
                setvideo(CG_MODE,cgmode);
#endif
                blanktime(0);
                if (record && black && (startrecord == 0))
                {
                    char msg[256];
                    sprintf(msg,"/usr/local/bin/record -s %d",black);
#ifdef NORMALRECORD
                    system(msg);
#else
                    while (recfile = fopen("recordfile","r"))
                    {
                        fclose(recfile);
                    }
                    recfile = fopen("recordfile","w");
                    fprintf(recfile,"%s\n",msg);
                    fclose(recfile);
#endif
                } // if (record ...
            } // if (!preview)
        } // if (video)
    } // if (graphics)
    else
    {
        cout << "NOTE: Running simulation without any graphics (or vision)" nlf;
    }


//  signal(SIGINT,SIG_IGN);
//  signal(SIGTERM,SIG_TYP(onterm));

    timeinit();

    long i;

    srand(1);

    screen.setprefposition(screenleft,screenleft+xscreensize-1,
                           screenbottom,screenbottom+yscreensize-1);
    nodrawwindow titlewin;
    titlewin.setborder(FALSE);
    titlewin.setframewidth(3);
    titlewin.setframecolor(0.,1.,1.);
    titlewin.setprefposition(screenleft+320,screenleft+959,
                             screenbottom+612,screenbottom+742);
    titlewin.setdoublebuffer(FALSE);
    titlewin.setvisibility(TRUE);
    if (graphics) {
        screen.open();
        titlewin.open();
        titlewin.dumbtext("PolyWorld:  An Artificial Life Ecological Simulator                   by Larry Yaeger                     Copyright 1990,1991,1992 Apple Computer, Inc.  ");
    }

    initworld();

    readworldfile(filename);

    if (loadstate && geneseprec)
        error(0,
            "continuing from dump file, but genesepfile will be overwritten");

    genomeinit();
    braininit();
    critterinit();

/*
// Following is part of one way to speed up the graphics
// Note:  this code must agree with the critter sizing in critter.C/grow()
// and the food sizing in food.h/initlen().
*/

    float maxcritterlenx = maxcsize / sqrt(minmaxspeed);
    float maxcritterlenz = maxcsize * sqrt(maxmaxspeed);
    float maxcritterradius = 0.5 * sqrt(maxcritterlenx*maxcritterlenx +
                                        maxcritterlenz*maxcritterlenz);
    float maxfoodlen = 0.75 * maxfoodenergy / size2energy;
    float maxfoodradius = 0.5 * sqrt(maxfoodlen * maxfoodlen * 2.0);
    maxradius = maxcritterradius > maxfoodradius ?
                maxcritterradius : maxfoodradius;
    
    int numviews = maxviews;

    critter* c = NULL;
    short id;
    if (numfit > 0)
    {
        for (id = 0; id < numdomains; id++)
        {
            domains[id].fittest = new genome*[numfit];
            domains[id].fitness = new float[numfit];
            for (i = 0; i < numfit; i++)
            {
                domains[id].fittest[i] = new genome;
                domains[id].fitness[i] = 0.;
            }
        }

        fittest = new genome*[numfit];
        fitness = new float[numfit];
        for (i = 0; i < numfit; i++)
        {
            fittest[i] = new genome;
            fitness[i] = 0.;
        }
    }

    worldstage.setcast(worldcast);

    foodenergyin = 0.0;
    foodenergyout = 0.0;

    srand48(genomeseed);
//  srand48(positionseed);
    if (!loadstate)
    {

// first handle the individual domains

        long curcount;
        for (id = 0; id < numdomains; id++)
        {
            curcount = xsortedcritters.count();
            for (i = 0; i < min((maxnumcritters-curcount)
                                ,domains[id].initnumcritters); i++)
            {
                c = getfreecritter();
                if (c == NULL)
                    error(1,"Insufficient memory to construct new critter");
                else
                {
                    numcreated++;
                    numcreatedrandom++;
                    domains[id].numcreated++;
                    c->genes()->randomize();
                    c->grow();
                    foodenergyin += c->foodenergy();
                    worldstage.addobject(c);
                    float x = drand48()*(domains[id].xsize-0.02)
			    + domains[id].xleft + 0.01;
                    float z = -0.01 - drand48()*(worldsize-0.02);
                    float y = 0.5 * critterheight;
                    c->settranslation(x,y,z);
                    float yaw =  360.0 * drand48();
                    c->setyaw(yaw);
                    xsortedcritters.add(c);
                    c->domain(id);
                    domains[id].numcritters++;
                }
            }

            curcount = xsortedfood.count();
            for (i = 0; i < min((maxfoodcount-curcount)
                                ,domains[id].initfoodcount); i++)
            {
                food* f = new food;
                if (f == NULL)
                    error(1,"Insufficient memory to construct new food");
                else
                {
                    foodenergyin += f->energy();
                    f->setx(drand48()*(domains[id].xsize-0.02)
			    + domains[id].xleft + 0.01);
                    f->domain(id);
                    worldstage.addobject(f);
                    xsortedfood.add(f);
                    domains[id].foodcount++;
                }
            }
        }

// then handle global initial creations, if necessary

        while (xsortedcritters.count() < initnumcritters)
        {
            c = getfreecritter();
            if (c == NULL)
                error(1,"Insufficient memory to construct new critter");
            else
            {
                numcreated++;
                numcreatedrandom++;
                c->genes()->randomize();
                c->grow();
                foodenergyin += c->foodenergy();
                worldstage.addobject(c);
                float x =  0.01 + drand48()*(worldsize-0.02);
                float z = -0.01 - drand48()*(worldsize-0.02);
                float y = 0.5 * critterheight;
                c->settranslation(x,y,z);
                float yaw =  360.0 * drand48();
                c->setyaw(yaw);
                xsortedcritters.add(c);
                id = whichdomain(x,z,0);
                c->domain(id);
                domains[id].numcritters++;
            }
        }

        while (xsortedfood.count() < initfoodcount)
        {
            food* f = new food;
            if (f == NULL)
                error(1,"Insufficient memory to construct new food");
            else
            {
                foodenergyin += f->energy();
                worldstage.addobject(f);
                xsortedfood.add(f);
                id = whichdomain(f->x(),f->z(),0);
                f->domain(id);
                domains[id].foodcount++;
            }
        }

    }

    totfoodenergyin = foodenergyin;
    totfoodenergyout = foodenergyout;
    avgfoodenergyin = 0.0;
    avgfoodenergyout = 0.0;

    ground.sety(-groundclearance);
    ground.setscale(worldsize);
    ground.setcolor(groundcolor);
    worldset.add(&ground);
    worldstage.setset(worldset);

    xsortedbarriers.reset();
    barrier* b = NULL;
    while (xsortedbarriers.next(b))
        worldset.add(b);

    for (i = 0; i < maxviews; i++)
    {
        scene[i].setstage(worldstage);
        scene[i].setcamera(camera[i]);
        camera[i].setnear(.01);
        camera[i].setfar(1.5*worldsize);
    }

    camangle = camanglestart;
    float camrad = camangle * DEGTORAD;
    camera[0].settranslation((0.5+camradius*sin(camrad))*worldsize,
                             camheight*worldsize,
                             (-.5+camradius*cos(camrad))*worldsize);
    camera[0].setfovy(camfov);
    if (camrotationrate != 0.0)
    {
        camera[0].setfix(0.5*worldsize,0.,-0.5*worldsize);
    }
    else
        camera[0].setrotation(0.,-camfov/3.,0.);

    camera[0].setcolor(cameracolor);
    worldstage.addobject(camera[0]);

    screen.addwindow(win[0]);
    win[0].settitle("environment");
    win[0].setprefposition(screenleft,screenleft+.75*xscreensize,
                           screenbottom,screenbottom+(5./6.)*yscreensize);
    win[0].setframecolor(.2,1.,.2);
    win[0].setscene(scene[0]);
    if (graphics)
    {
        win[0].open();
        camera[0].fixperspective(TRUE); // purely for speed considerations
    }

    screen.addwindow(win[1]);
    win[1].settitle("overhead");
    camera[1].settranslation(0.5*worldsize,.7*worldsize,-0.5*worldsize);
    camera[1].setrotation(0.,-90.,0.);
    camera[1].setfovy(90.);
    win[1].setprefposition(screenleft+.75*xscreensize+1,screenleft+xscreensize,
                           screenbottom,screenbottom+(5./12.)*yscreensize);
    win[1].setframecolor(.6,1.,.4);
    win[1].setscene(scene[1]);
    win[1].setvisibility(TRUE);
    if (graphics)
    {
        win[1].open();
        camera[1].fixperspective(TRUE); // purely for speed considerations
    }

    nodrawwindow statwin("status window");
    statwin.setborder(FALSE);
    statwin.setzbuffer(FALSE);
    statwin.setdoublebuffer(TRUE);
    statwin.setframewidth(3);
    statwin.setframecolor(.8,.8,.8);
    statwin.setprefposition(screenleft+.75*xscreensize+1,screenleft+xscreensize,
                            screenbottom+(5./12.)*yscreensize+1,screenbottom+yscreensize);
    statwin.setvisibility(TRUE);
    screen.addwindow(statwin);
    if (graphics)
        statwin.open();

    short numcharts = 0;
    if (chartborn) numcharts++;
    if (chartfit) numcharts++;
    if (chartfoodenergy) numcharts++;
    if (chartpop) numcharts++;
    if (binchartgs) numcharts++;
#ifdef OF1
    numcharts++;
#endif

    long chartwidth   = (.75*xscreensize - 16) / 2;
    long chartheight  = .3*yscreensize/((numcharts-1)/2+1) - 3;

    long chartleft = screenleft + 6;
    long chartright = chartleft + chartwidth - 1;
    long charttop = 0;  // will be reset by tilecharts(...)
    long chartbottom = screenbottom + (5./6.)*yscreensize; // also reset

    numcharts = 0;

    bornwin = new chartwindow;
    if (!bornwin)
	error(2,"unable to open bornwin");
    if (chartborn)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        bornwin->setframecolor(.9,.9,.9);
        bornwin->settitle("born/(born+created)");
        bornwin->setprefposition(chartleft,chartright,
                                 chartbottom,charttop);
        bornwin->setrange(0.,1.);
        screen.addwindow(bornwin);
        if (graphics)
            bornwin->open();
    }

    fitwin = new chartwindow(3);
    if (!fitwin)
	error(2,"unable to open fitwin");
    if (chartfit)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        fitwin->setframecolor(.9,.9,.9);
        fitwin->settitle("maxfit,curmaxfit,avgfit");
        fitwin->setprefposition(chartleft,chartright,
                                chartbottom,charttop);
        fitwin->setrange(0,0.,1.);
        fitwin->setrange(1,0.,1.);
        fitwin->setrange(2,0.,1.);
        fitwin->setcolor(0,1.,1.,1.);
        fitwin->setcolor(1,1.,.3,0.);
        fitwin->setcolor(2,0.,1.,1.);
        screen.addwindow(fitwin);
        if (graphics)
            fitwin->open();
    }

    fewin = new chartwindow(3);
    if (!fewin)
	error(2,"unable to open fewin");
    if (chartfoodenergy)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        fewin->setframecolor(.9,.9,.9);
        fewin->settitle("energy");
        fewin->setprefposition(chartleft,chartright,
                               chartbottom,charttop);
        fewin->setrange(0,-1.,1.);
        fewin->setrange(1,-1.,1.);
        fewin->setrange(2,-1.,1.);
        screen.addwindow(fewin);
        if (graphics)
            fewin->open();
    }

    short numpop = (numdomains < 2) ? 1 : (numdomains+1);
    popwin = new chartwindow(numpop);
    if (!popwin)
	error(2,"unable to open popwin");
    if (chartpop)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        popwin->setframecolor(.9,.9,.9);
        popwin->settitle("population size");
        popwin->setprefposition(chartleft,chartright,
                               chartbottom,charttop);
        for (i = 0; i < numpop; i++)
            popwin->setrange(short(i),0,maxnumcritters);
        popwin->setcolor(1,1.,0.,0.);
        popwin->setcolor(2,0.,1.,0.);
        popwin->setcolor(3,0.,0.,1.);
        screen.addwindow(popwin);
        if (graphics)
            popwin->open();
    }

    gswin = new binchartwindow; // declared in globals.h
    if (!gswin)
	error(2,"unable to open gswin");
    if (binchartgs)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        gswin->setframecolor(.9,.9,.9);
        gswin->settitle("genetic separation");
        gswin->setprefposition(chartleft,chartright,
                              chartbottom,charttop);
        gswin->setrange(0.,1.);
        gswin->exponent(0.5);
        screen.addwindow(gswin);
        if (graphics)
            gswin->open();
    }

#ifdef OF1
    ofwin = new chartwindow(5);
    if (!ofwin)
        error(2,"unable to open ofwin");
    if (1)
    {
        tilecharts(chartwidth,chartheight,numcharts,
                  &chartleft,&chartright,&charttop,&chartbottom);
        numcharts++;
        ofwin->setframecolor(.9,.9,.9);
        ofwin->settitle("foraging");
        ofwin->setprefposition(chartleft,chartright,
                               chartbottom,charttop);
        // default of 0.0 to 1.0 range will do for all of these
        ofwin->setcolor(0,foodcolor.r,foodcolor.g,foodcolor.b); // eating eff.
        ofwin->setcolor(1,.3,.3,1.); // birthing efficiency
        ofwin->setcolor(2,1.,.2,.2); // foptbar
        ofwin->setcolor(3,1.,.7,.1); // foptofbar
        ofwin->setcolor(4,1.,1.,1.); // fbar (actual)
        screen.addwindow(ofwin);
        if (graphics)
            ofwin->open();
    }
#endif

    long qdev;
    short qdata;
    long valx;
    long valy;
    Boolean leftmousedown = FALSE;
    Boolean middlemousedown = FALSE;
    long leftvalx0;
    long leftvaly0;
    long midvalx0;
    long midvaly0;
    float pitch0;
    float yaw0;
    float y0;
    float dposscale = 3.;
    float dyawscale = 8.;
    float yawscale = 1.;
    float pitchscale = 1.;
    float yscale = 15.;
    float dpos; float dx; float dy; float dz; float dpitch; float dyaw;
    Boolean pause = FALSE;

    age = 0;

    if (loadstate)
        load();

    camrad = camangle * DEGTORAD;
    camera[0].settranslation((0.5+camradius*sin(camrad))*worldsize,
                             camheight*worldsize,
                             (-.5+camradius*cos(camrad))*worldsize);

    if (genesepmon)
    {
        gsvals = new float[maxnumcritters*(maxnumcritters-1)/2];
        numgsvals = 0;
        genesepcalcall();
        if (geneseprec)
        {
            genesepfile = fopen("pw.genesep","w");
	    geneseprecord();
        }
        if (graphics && binchartgs)
            gswin->addpoint(gsvals,numgsvals);
    }

    if (graphics)
    {
        titlewin.close();
        screen.draw();
        textstatus(statwin,TRUE,filename);
        if (delay > 0) sleep(delay);

        qdevice(KEYBD);
        qdevice(LEFTMOUSE);
        qdevice(MIDDLEMOUSE);
        qreset();
    }

#ifdef DEBUGCHECK
    debugcheck("before main loop");
#endif DEBUGCHECK
    Boolean done = (maxloops == 0);
    long stepage = 0;
    timebeg(0);
    while (!done)
    {
        age++;
#ifdef DEBUGCHECK
        sprintf(debugstring,"in main loop at age %d\0",age);
        debugcheck(debugstring);
#endif DEBUGCHECK
        Boolean step = FALSE;
        if ( ((age-lastcreate) > maxgapcreate) && (lastcreate > 0) )
            maxgapcreate = age - lastcreate;
        if (numdomains > 1)
        {
            for (id = 0; id < numdomains; id++)
                if ( ((age-domains[id].lastcreate) > domains[id].maxgapcreate)
                  && (domains[id].lastcreate > 0) )
                    domains[id].maxgapcreate = age - domains[id].lastcreate;
        }
        if (age >= maxloops)
            done = TRUE;

        foodenergyin = 0.0;
        foodenergyout = 0.0;

        long numcritters = xsortedcritters.count();
        float critrefresh = numcritters > 15
                          ? numcritters / ((numcritters+4)/10)
                          : numcritters;
        short critsincedraw = 0;
        xsortedcritters.reset();
        long critnum = 0;
        while (xsortedcritters.next(c) && (!done))
        {
#ifdef DEBUGCHECK
            sprintf(debugstring,"in critter loop at age %d, critnum = %d\0",
                age,critnum);
            debugcheck(debugstring);
#endif DEBUGCHECK
#ifdef TIMING
            timebeg(4);
#endif TIMING
            critnum++;
            if (graphics)
            {
            do
            {
                if (qtest())
                {
                    qdev = qread(&qdata);
                    switch (qdev)
                    {
                    case KEYBD:
                        switch (qdata)
                        {
                        case (27): // ESCape
                            done = TRUE;
                            sleep(1);
                            break;
                        case (32): // space
                            pause = !pause;
                            break;
                        case (97): // a (alternate brain-monitor display)
                            alternatebraindisplay = !alternatebraindisplay;
                            break;
                        case (98): // b (brain-monitor)
                            if (moncritrank) // already on, so turn it off
                                moncritrank = 0;
                            else
                            {
                                if (overheadrank)
                                    moncritrank = overheadrank;
                                else
                                    moncritrank = 1;
                            }
                            break;
                        case (100): // d (delay)
                            if (delay)
                                delay = 0;
                            else
                                delay = 1;
                            break;
#ifdef PRINTBRAIN
                        case (112): // p (printbrain)
                            printbrain = !printbrain;
                            break;
#endif PRINTBRAIN
                        case (114): // r (refresh)
                            screen.refresh();
                            critterwindowrefresh();
                            break;
                        case (115): // s (step)
                            step = TRUE;
                            break;
                        case (116): // t (tracking)
                            tracking = !tracking;
                            if (tracking)
                            {
                                if (!overheadrank)
                                {
                                    if (moncritrank)
                                        overheadrank = moncritrank;
                                    else
                                        overheadrank = 1;
                                }
                            }
                            else
                            {
                                overheadrank = 0;
                                camera[1].setx( 0.5*worldsize);
                                camera[1].setz(-0.5*worldsize);
                                camera[1].setfovy(90.);
                            }
                            break;
                        case (48): // 0 (overhead window to pure overhead)
                            camera[1].setx( 0.5*worldsize);
                            camera[1].setz(-0.5*worldsize);
                            camera[1].setfovy(90.);
                            overheadrank = 0; // not following any critters
                            break;
                        case (49): // 1 (overhead window follows fittest)
                            overheadrank = 1; // following fittest critter
                            if (moncritrank)
                                moncritrank = overheadrank;
                            break;
                        case (50): // 2 (overhead window follows 2nd fittest)
                            overheadrank = 2; // following 2nd fittest critter
                            if (moncritrank)
                                moncritrank = overheadrank;
                            break;
                        case (51): // 3 (overhead window follows 3rd fittest)
                            overheadrank = 3; // following 3rd fittest critter
                            if (moncritrank)
                                moncritrank = overheadrank;
                            break;
                        case (52): // 4 (overhead window follows 4th fittest)
                            overheadrank = 4; // following 4th fittest critter
                            if (moncritrank)
                                moncritrank = overheadrank;
                            break;
                        case (53): // 5 (overhead window follows 5th fittest)
                            overheadrank = 5; // following 5th fittest critter
                            if (moncritrank)
                                moncritrank = overheadrank;
                            break;
                        case (43): // + (zoom in in overhead window)
                        case (61): // = ("lowercase +")
                            if ((camera[1].getfovy()/1.5) >= 2.)
                            {
                                camera[1].setfovy(camera[1].getfovy()/1.5);
                                win[1].makecurrentwindow();
                                win[1].useperspective();
                            }
                            break;
                        case (45): // - (zoom out in overhead window)
                        case (95): // _ ("uppercase -")
                            if ((camera[1].getfovy()*1.5) < 179.)
                            {
                                camera[1].setfovy(camera[1].getfovy()*1.5);
                                win[1].makecurrentwindow();
                                win[1].useperspective();
                            }
                            break;
                        default:
                            ringbell();  // don't know what they want!
                        }
                        break;
                    case LEFTMOUSE:
                        leftmousedown = TRUE;
                        leftvalx0 = getvaluator(MOUSEX);
                        leftvaly0 = getvaluator(MOUSEY);
                        dpos = 0.;
                        dyaw = 0.;
                        dx = 0.;
                        dy = 0.;
                        dz = 0.;
                        break;
                    case MIDDLEMOUSE:
                        middlemousedown = TRUE;
                        midvalx0 = getvaluator(MOUSEX);
                        midvaly0 = getvaluator(MOUSEY);
                        yaw0 = camera[0].getyaw();
                        pitch0 = camera[0].getpitch();
                        y0 = camera[0].y();
                        break;
                    }
                }
    
                valx = getvaluator(MOUSEX);
                valy = getvaluator(MOUSEY);
                if (middlemousedown)
                {
                    middlemousedown = getbutton(MIDDLEMOUSE);  // still down?
                    if (middlemousedown) // y => pitch & x => yaw, or y => y
                    {
                        if (leftmousedown) // both buttons down, so y => y
                        {
                            dy = yscale * (midvaly0 - valy) /512.;
                            float y = y0-dy;
                            if (y < 0.) y = 0.;
                            camera[0].sety(y);
                        }
                        else // x => yaw
                        {
                            dyaw = yawscale * (midvalx0 - valx) * 180./640.;
                            camera[0].setyaw(yaw0+dyaw);
                            dpitch = pitchscale * (midvaly0 - valy) * 90./512.;
                            camera[0].setpitch(pitch0+dpitch);
                        }
                    }
                }
                else if (leftmousedown)
                {
                    leftmousedown = getbutton(LEFTMOUSE);  // still down?
                    if (leftmousedown) // x => dyaw, y => dpos (speed)
                    {
                        dyaw = dyawscale * (leftvalx0 - valx) / 640.;
                        dpos = dposscale * (valy - leftvaly0) * maxvel / 512.;
                        dx = -dpos * sin(camera[0].getyaw()*DEGTORAD);
                        dz = -dpos * cos(camera[0].getyaw()*DEGTORAD);
                        float x = camera[0].x() + dx;
                        float z = camera[0].z() + dz;
                        if (x < -0.1*worldsize)
                        {
                            if (wraparound)
                                x += 1.2*worldsize;
                            else
                                x = -0.1*worldsize;
                        }
                        if (x > 1.1*worldsize)
                        {
                            if (wraparound)
                                x -= 1.2*worldsize;
                            else
                                x = 1.1*worldsize;
                        }
                        if (z > 0.1*worldsize)
                        {
                            if (wraparound)
                                z -= 1.2*worldsize;
                            else
                                z = 0.1*worldsize;
                        }
                        if (z < -1.1*worldsize)
                        {
                            if (wraparound)
                                z += 1.2*worldsize;
                            else
                                z = -1.1*worldsize;
                        }
                        camera[0].setx(x);
                        camera[0].setz(z);
                        camera[0].addyaw(dyaw);
                    }
                    else // just lifted button, so rezero...
                    {
                        dyaw = 0.;
                        dpos = 0.;
                        dx = 0.;
                        dy = 0.;
                        dz = 0.;
                    }
                }
                if (pause) screen.draw();
            } while (pause && (!done) && (!step));

            if ( (leftmousedown || middlemousedown) &&
                 (critsincedraw > critrefresh) )
            {
                screen.draw();
                critsincedraw = 0;
            }
            } // if (graphics)
#ifdef TIMING
            timeend(4);
#endif TIMING

#ifdef TIMING
    timebeg(1);
#endif TIMING
            if (docpuwork)
                c->update();
#ifdef TIMING
    timeend(1);
#endif TIMING

            critsincedraw++;

        }

        if (showvision && graphics)
            critterpovswap();

#ifdef TIMING
    timebeg(2);
#endif TIMING
        long oldnumborn = numborn;
        long oldnumcreated = numcreated;
        if (docpuwork)
            interact(); // handles collisions, matings, fights, deaths, births, etc.
#ifdef DEBUGCHECK
        sprintf(debugstring,"after interact at age %d\0",age);
        debugcheck(debugstring);
#endif DEBUGCHECK
#ifdef TIMING
    timeend(2);
#endif TIMING

//      win[1].print();

        totfoodenergyin += foodenergyin;
        totfoodenergyout += foodenergyout;
/*
        if (age < 101)
        {
*/
            avgfoodenergyin = (float(age-1)*avgfoodenergyin+foodenergyin)
                             / float(age);
            avgfoodenergyout = (float(age-1)*avgfoodenergyout+foodenergyout)
                             / float(age);
/*
        }
        else
        {
            avgfoodenergyin = 0.998*avgfoodenergyin + 0.002*foodenergyin;
            avgfoodenergyout = 0.998*avgfoodenergyout + 0.002*foodenergyout;
        }
*/

#ifdef TIMING
    timebeg(3);
#endif TIMING
        if (graphics)
        {
            if (overheadrank)
            {
                if (!tracking || (overheadrank != overheadrankold) ||
                    !overheadcrit || !(overheadcrit->alive()))
                {
                    overheadcrit = curfittestcrit[overheadrank-1];
                    overheadrankold = overheadrank;
                }
                if (overheadcrit)
                {
                    camera[1].setx(overheadcrit->x());
                    camera[1].setz(overheadcrit->z());
                }
            }
            screen.draw();
            if ( chartborn &&
                ((oldnumborn != numborn) || (oldnumcreated != numcreated)) )
                bornwin->addpoint(float(numborn)/float(numborn+numcreated));
            if (chartfit)
            {
                fitwin->addpoint(0,maxfitness/totalfitness);
                fitwin->addpoint(1,curmaxfitness[0]/totalfitness);
                fitwin->addpoint(2,avgfitness/totalfitness);
            }
            if (chartfoodenergy)
            {
                fewin->addpoint(0,(foodenergyin-foodenergyout)
                                /(foodenergyin+foodenergyout));
                fewin->addpoint(1,(totfoodenergyin-totfoodenergyout)
                                /(totfoodenergyin+totfoodenergyout));
                fewin->addpoint(2,(avgfoodenergyin-avgfoodenergyout)
                                /(avgfoodenergyin+avgfoodenergyout));
            }
            if (chartpop)
            {
                popwin->addpoint(0,float(xsortedcritters.count()));
                if (numdomains > 1)
                {
                    for (id = 0; id < numdomains; id++)
                    {
                        popwin->addpoint((id+1),float(domains[id].numcritters));
                    }
                }
            }
#ifdef OF1
            if (1)
            {
                plotoptfor(ofwin);
            }
#endif
            if ( (moncritrank != moncritrankold) ||
                 (moncritrank && !tracking &&
                  (curfittestcrit[moncritrank-1] != moncrit)) ||
                 (moncritrank && tracking && !(moncrit->alive())) )
            {
                if (moncrit)
                {
                    moncrit->endbrainmonitoring();
                    screen.refresh();
                    critterwindowrefresh();
                }
                if (moncritrank)
                {
                    moncrit = curfittestcrit[moncritrank-1];
                    moncrit->beginbrainmonitoring(monwidth,monheight);
                }
                else
                    moncrit = NULL;
                moncritrankold = moncritrank;
            }
        }
#ifdef DEBUGCHECK
        sprintf(debugstring,"after extra graphics at age %d\0",age);
        debugcheck(debugstring);
#endif DEBUGCHECK
#ifdef TIMING
    timeend(3);
#endif TIMING

#ifdef TIMING
    timebeg(5);
#endif TIMING
//      if ((age%20) == 0)
            textstatus(statwin,(age%statfreq)==0,filename);
#ifdef TIMING
    timeend(5);
#endif TIMING

        if (video && record && (age >= startrecord) && graphics)
        {
#ifdef NORMALRECORD
            system("/usr/local/bin/record");
#else
            while (recfile = fopen("recordfile","r"))
            {
                fclose(recfile);
            }
            recfile = fopen("recordfile","w");
            fprintf(recfile,"/usr/local/bin/record\n");
            fclose(recfile);
#endif
       }

        if (camrotationrate != 0.0)
        {
            camangle += camrotationrate;
            camrad = camangle * DEGTORAD;
            camera[0].settranslation((0.5+camradius*sin(camrad))*worldsize,
                                     camheight*worldsize,
                                     (-.5+camradius*cos(camrad))*worldsize);
        }

        if ( ((age%dumpfreq) == 0) && age )
        {
            dump();
            textstatus(statwin,TRUE,filename);
        }

        if (delay > 0) sleep(delay);

    }
    timeend(0);

    if ((maxloops < 2) && (maxloops >= 0))
        sleep(5);
//  else
//      sleep(2);

//  screen.print();

    timefinish();

    Boolean takedump = TRUE;

    if (graphics)
    {
        if (qtest())
        {
            qdev = qread(&qdata);
            if ( (qdev == KEYBD) && (qdata == 27) ) // ESCape
                takedump = FALSE;
        }
    }


    if (takedump)
    {
        dump();
        if (genesepmon && geneseprec)
            geneseprecord();
    }

    extern char* getfileroot(cchar* f);
    textstatus(statwin,TRUE,filename);
    char* fileroot = getfileroot(filename);
    sprintf(tempstring,"cp pw.stat %s.stat.%d\0",fileroot,age);
    system(tempstring);
    delete fileroot;

    if (genesepmon && geneseprec)
        fclose(genesepfile);

/*
    if (video && record && graphics)
    {
        background.makecurrentwindow();
        winpop();
        background.clearc();
#ifdef NORMALRECORD
        system("/usr/local/bin/record -s 60");
#else
        while (recfile = fopen("recordfile","r"))
        {
            fclose(recfile);
        }
        recfile = fopen("recordfile","w");
        fprintf(recfile,"/usr/local/bin/record -s 60\n");
        fclose(recfile);
        sleep(10);
        while (recfile = fopen("recordfile","r"))
        {
            fclose(recfile);
        }
#endif
    }
*/

    cleanup();

    howbigwasi();

#ifdef DEBUGCALLS
    popproc();
    listprocstack();
#endif DEBUGCALLS
}


char* getfileroot(cchar* filename)
{
#ifdef DEBUGCALLS
    pushproc("getfileroot");
#endif DEBUGCALLS
    char* fileroot = NULL;
    char* dot = strrchr(filename,'.');
    if (dot)
    {
        short len = strlen(filename) - strlen(dot);
        fileroot = new char[len+1];
        strncpy(fileroot,filename,len);
        fileroot[len] = '\0';
    }
    else
    {
        fileroot = new char[strlen(filename)+1];
        strcpy(fileroot,filename);
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
    return fileroot;
}
