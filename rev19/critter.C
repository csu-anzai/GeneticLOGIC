#ifndef CRITTER_C
#define CRITTER_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// critter.C - implementation of critter classes

#include "basicincludes.h"
#include "graphics.h"
#include "critter.h"
#include "barrier.h"
#include "pw.h"

#include "graybin.h"

//#define DEBUGBRAINGROW
#include "debug.h"

short numinputneurgroups = 5;
short firstnoninputgroup = numinputneurgroups;
short numoutneur = 7;
short numoutneurgroups = numoutneur;

Boolean genome::classinited;
long genome::numbytes;
long genome::numphysbytes;
long genome::mrategene;
long genome::ncptsgene;
long genome::lifespangene;
long genome::idgene;
long genome::strengthgene;
long genome::sizegene;
long genome::maxspeedgene;
long genome::mateenergygene;
// definitions of the following brain-related genes are in the .C file
long genome::numrneurgene;
long genome::numgneurgene;
long genome::numbneurgene;
long genome::numneurgroupsgene;
long genome::numeneurgene;
long genome::numineurgene;
long genome::biasgene;
long genome::biaslrategene;
long genome::eecdgene;
long genome::eicdgene;
long genome::iicdgene;
long genome::iecdgene;
long genome::eelrgene;
long genome::eilrgene;
long genome::iilrgene;
long genome::ielrgene;
long genome::eetdgene;
long genome::eitdgene;
long genome::iitdgene;
long genome::ietdgene;
long* genome::cpts;

Boolean brain::classinited;
short* brain::firsteneur; // [maxneurgroups]
short* brain::firstineur; // [maxneurgroups]
float* brain::eeremainder; // [maxneurgroups]
float* brain::eiremainder; // [maxneurgroups]
float* brain::iiremainder; // [maxneurgroups]
float* brain::ieremainder; // [maxneurgroups]
Boolean* brain::neurused; // [max(maxeneurpergroup,maxineurpergroup)]
short brain::randomneuron;
short brain::energyneuron;

Boolean critter::classinited;
long critter::crittersever;
long critter::crittersliving;
gpolyobj* critter::critterobj;
indexlist* critter::critterlist;
critter** critter::pc;
gwindow* critter::povwindow;
short critter::povwidth;
short critter::povheight;

extern domainstruct domains[MAXDOMAINS];

extern short whichdomain(float x, float z, short d);
extern float logistic(cfloat x, cfloat slope);

extern Boolean alternatebraindisplay;

extern cxsortedlist xsortedcritters;
extern bxsortedlist xsortedbarriers;

static float initminweight = 0.0; // could read this in

#ifdef TIMING
    extern void timebeg(short);
    extern void timeend(short);
#endif TIMING

#ifdef PRINTBRAIN
extern Boolean printbrain;
extern short overheadrank;
critter* currentcritter;
critter* monitorcritter;
Boolean brainprinted = FALSE;
extern critter* curfittestcrit[5];
#endif PRINTBRAIN


void genomeinit()
{
    if (genome::classinited)
        return;

#ifdef DEBUGCALLS
    pushproc("genomeinit");
#endif DEBUGCALLS

    genome::classinited = TRUE;
    genome::mrategene = 0;
    genome::ncptsgene = 1;
    genome::lifespangene = 2;
    genome::idgene = 3;
    genome::strengthgene = 4;
    genome::sizegene = 5;
    genome::maxspeedgene = 6;
    genome::mateenergygene = 7;
    genome::numphysbytes = genome::mateenergygene + 1;

    // number of neurons in red vision input group
    genome::numrneurgene = genome::numphysbytes + 1;
    // number of neurons in green vision input group
    genome::numgneurgene = genome::numrneurgene + 1;
    // number of neurons in blue vision input group
    genome::numbneurgene = genome::numbneurgene + 1;

    // Note the distinction between "non-input" neuronal groups and
    // the 5 input groups -- 3 groups of vision neurons (r,g,b), the
    // energy-neuron "group" (of one), and the random-neuron "group"
    // (again, of size one).  The input neurons do not themselves
    // accept inputs, though they obviously do generate outputs.
    // This is the reason for the odd sizing of the connection
    // density, learning rate, and topological distortion groupings
    // below (maxneurgroups*maxnoninputneurgroups).
    // Input neurons double as both excitatory and inhibitory
    // neurons (as do output/behavior neurons), but only when they
    // are the pre-synaptic source neuron.  As post-synaptic target
    // neurons, they are exclusively excitatory.  This is a bit of
    // a hack to permit them to be used as inhibitors, without
    // going through a separate, signal-inverting group, primarily
    // for the sake of computational efficiency.

    // Correspondingly, since the last non-input neuronal group is
    // designated to be the "output" (behavior) group, the number of
    // neurons (again doubling as both excitatory and inhibitory) in
    // that group is fixed; hence only maxneurgroups-1 specifications
    // are needed to specify all the non-input group sizes.  On the
    // other hand, the output group's neurons *are* allowed to feedback
    // their activation levels to other non-input groups, hence their
    // inclusion as just another non-input group (rather than
    // separating them as is done with the input groups).
    // Also, the output neurons need a bias and biaslearningrate, just
    // like the rest of the non-input neurons, hence the full number
    // of maxneurgroups values must be specified for them.

    // number of non-input neuronal groups
    genome::numneurgroupsgene = genome::numbneurgene + 1;
    // number of excitatory neurons (per group)
    genome::numeneurgene = genome::numneurgroupsgene + 1;
    // number of inhibitory neurons (per group)
    genome::numineurgene = genome::numeneurgene + maxinternalneurgroups;

    // bias level for all neurons in this group
    genome::biasgene = genome::numineurgene + maxinternalneurgroups;
    // bias learning rate for all neurons in this group
    genome::biaslrategene = genome::biasgene + maxnoninputneurgroups;

    // excitatory-to-excitatory connection density        (for pairs of groups)
    // excitatory-to-inhibitory connection density        (for pairs of groups)
    // inhibitory-to-inhibitory connection density        (for pairs of groups)
    // inhibitory-to-excitatory connection density        (for pairs of groups)
    genome::eecdgene = genome::biaslrategene + maxnoninputneurgroups;
    genome::eicdgene = genome::eecdgene + maxneurgroups*maxnoninputneurgroups;
    genome::iicdgene = genome::eicdgene + maxneurgroups*maxnoninputneurgroups;
    genome::iecdgene = genome::iicdgene + maxneurgroups*maxnoninputneurgroups;

    // excitatory-to-excitatory learning rate             (for pairs of groups)
    // excitatory-to-inhibitory learning rate             (for pairs of groups)
    // inhibitory-to-inhibitory learning rate             (for pairs of groups)
    // inhibitory-to-excitatory learning rate             (for pairs of groups)
    genome::eelrgene = genome::iecdgene + maxneurgroups*maxnoninputneurgroups;
    genome::eilrgene = genome::eelrgene + maxneurgroups*maxnoninputneurgroups;
    genome::iilrgene = genome::eilrgene + maxneurgroups*maxnoninputneurgroups;
    genome::ielrgene = genome::iilrgene + maxneurgroups*maxnoninputneurgroups;

    // excitatory-to-excitatory topological distortion    (for pairs of groups)
    // excitatory-to-inhibitory topological distortion    (for pairs of groups)
    // inhibitory-to-inhibitory topological distortion    (for pairs of groups)
    // inhibitory-to-excitatory topological distortion    (for pairs of groups)
    genome::eetdgene = genome::ielrgene + maxneurgroups*maxnoninputneurgroups;
    genome::eitdgene = genome::eetdgene + maxneurgroups*maxnoninputneurgroups;
    genome::iitdgene = genome::eitdgene + maxneurgroups*maxnoninputneurgroups;
    genome::ietdgene = genome::iitdgene + maxneurgroups*maxnoninputneurgroups;

    genome::numbytes   = genome::ietdgene + maxneurgroups*maxnoninputneurgroups;

    genome::cpts = new long[maxnumcpts+1];
    if (genome::cpts == NULL)
        error(2,"Insufficient memory for crossover array in genome class");

// may someday want to make the learning rate an evolvable schedule
// may also want to support overgrowth and dieback of neural connections,
// both here and in the brain class.

#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genomedestruct() { if (genome::cpts) delete genome::cpts; }


void genome::randomize(cfloat bitonprob)
{
    // do a random initialization of the bitstring
    for (long byte = 0; byte < numbytes; byte++)
        for (long bit = 0; bit < 8; bit++)
            if (drand48() < bitonprob)
                genes[byte] |= Byte(1 << (7-bit));
            else
                genes[byte] &= Byte(255 ^ (1 << (7-bit)));
}


void genome::init(cchar c)
{
#ifdef DEBUGCALLS
    pushproc("genome::init");
#endif DEBUGCALLS
    if (!classinited)
        genomeinit();
    genes = new uchar[numbytes];
    if (genes == NULL)
        error(2,"Insufficient memory to construct new gene set");
    else
    {
        switch (c)
        {
        case 'r':
            randomize();
            break;
        case 'n':
        default:
            // do no initialization of the bitstring
            break;
        }
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genome::dump(ostream& out)
{
#ifdef DEBUGCALLS
    pushproc("genome::dump");
#endif DEBUGCALLS
    for (register long i = 0; i < numbytes; i++)
        out << (int)(genes[i]) nl;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genome::load(istream& in)
{
#ifdef DEBUGCALLS
    pushproc("genome::load");
#endif DEBUGCALLS
    int num = 0;
    for (register long i = 0; i < numbytes; i++)
    {
        in >> num;
        genes[i] = (unsigned char)num;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genome::mutate()
{
#ifdef DEBUGCALLS
    pushproc("genome::mutate");
#endif DEBUGCALLS
    float rate = mutationrate();
    for (long byte = 0; byte < numbytes; byte++)
    {
        for (long bit = 0; bit < 8; bit++)
        {
            if (drand48() < rate)
                genes[byte] ^= Byte(1 << (7-bit));
        }
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genome::crossover(const genome* g1, const genome* g2, cBoolean mutate)
{
#ifdef DEBUGCALLS
    pushproc("genome::crossover");
#endif DEBUGCALLS
    if (genes == NULL)
    {
        error(1,"Attempted crossover into NULL genome");
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }
    long i;
    long j;
    long ncpts;
    if (drand48() < 0.5)
        ncpts = g1->numcpts();
    else
        ncpts = g2->numcpts();
    long newcpt;
    // guarantee crossover in "physiology" genes
    cpts[0] = long(drand48() * numphysbytes * 8 - 1);
    cpts[1] = numphysbytes * 8;
    for (i = 2; i <= ncpts+1; i++) // generate & order the crossover pts...
    {
        newcpt = long(drand48()*(numbytes-numphysbytes)*8 - 1) + cpts[1];
        Boolean equal;
        do
        {
            equal = FALSE;
            for (j = 1; j < i; j++)
            {
                if (newcpt == cpts[j])
                    equal = TRUE;
            }
            if (equal)
                newcpt = long(drand48()*(numbytes-numphysbytes)*8 - 1) +cpts[1];
        } while (equal);
        if (newcpt > cpts[i-1])
            cpts[i] = newcpt;  // happened to come out ordered
        else
        {
            for (j = 2; j < i; j++)
            {
                if (newcpt < cpts[j])
                {
                    for (long k = i; k > j; k--)
                        cpts[k] = cpts[k-1];
                    cpts[j] = newcpt;
                    break;
                }
            }
        }
    }
/*
    cout << "**The crossover bits(bytes) are:" nl << "  ";
    for (i = 0; i <= ncpts+1; i++)
    {
        long byte = cpts[i] >> 3;
        cout << cpts[i] << "(" << byte << ") ";
    }
    cout nlf;
*/
    float mrate;
    if (mutate)
    {
        if (drand48() < 0.5)
            mrate = g1->mutationrate();
        else
            mrate = g2->mutationrate();
    }
    long begbyte = 0;
    long endbyte = -1;
    long bit;
    Boolean first = (drand48() < 0.5);
    const genome* g;
    for (i = 0; i <= ncpts+2; i++)  // now do crossover using the ordered pts
    {
        if (i == ncpts+2)  // for copying the end of the genome
        {
            if (endbyte == numbytes - 1)  // already copied last byte (done)
                break;                   // so just get out of the loop
            endbyte = numbytes - 1;
        }
        else
            endbyte = cpts[i] >> 3;
        g = first ? g1 : g2;
/*
        cout << "**copying bytes " << begbyte << " to " << endbyte
             << " from the ";
        if (first)
            cout << "first genome" nl;
        else
            cout << "second genome" nl;
        cout.flush();
*/
        if (mutate)
        {
            for (j = begbyte; j < endbyte; j++)
            {
                genes[j] = g->genes[j];    // copy from the appropriate genome
                for (bit = 0; bit < 8; bit++)
                {
                    if (drand48() < mrate)
                        genes[j] ^= Byte(1 << (7-bit));
                }
            }
        }
        else
        {
            for (j = begbyte; j < endbyte; j++)
                genes[j] = g->genes[j];    // copy from the appropriate genome
        }
        if (i != (ncpts+2))  // except on the last stretch...
        {
            first = !first;
            bit = cpts[i] - (endbyte << 3);
            if (first)
                genes[endbyte] = Byte(
                     (g2->genes[endbyte] & (255 << (8-bit)))
                   | (g1->genes[endbyte] & (255 >> bit))       );
            else
                genes[endbyte] = Byte(
                     (g1->genes[endbyte] & (255 << (8-bit)))
                   | (g2->genes[endbyte] & (255 >> bit))       );
            begbyte = endbyte + 1;
        }
        else
            genes[endbyte] = g->genes[endbyte];
        if (mutate)
        {
            for (bit = 0; bit < 8; bit++)
            {
                if (drand48() < mrate)
                    genes[endbyte] ^= Byte(1 << (7-bit));
            } // for (bit...
        } // if (mutate)
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


float genome::sepcalc(genome* g)
{
    float sep = 0.0;
    unsigned char* gi = genes;
    unsigned char* gj = g->genes;
    if (graycoding)
    {
        for (long i = 0; i < numbytes; i++)
        {
            register short vi, vj;
            vi = binofgray[*(gi++)];
            vj = binofgray[*(gj++)];
            sep += float(abs(vi-vj));
        }
    }
    else
    {
        for (long i = 0; i < numbytes; i++)
        {
            register short vi, vj;
            vi = *(gi++);
            vj = *(gj++);
            sep += float(abs(vi-vj));
        }
    }
    sep /= 255.0*float(numbytes);
    return sep;
}


float genome::mateprob(genome* g)
{
    // returns probability that two critters will successfully mate
    // based on their degree of genetic similarity/difference
    if (miscbias == 0.0)
        return 1.;
    float a = sepcalc(g);
    float cosa = cos(pow(a,miscbias)*PI);
    float s = cosa > 0. ? .5 : -.5;
    float p = 0.5  +  s * pow(fabs(cosa),miscinvslope);
    return p;
}


short genome::numeneur(cshort i)
{
    if ((i >= numinputneurgroups) && (i < numneurgroups()))
    {
        if (i >= (numneurgroups()-numoutneurgroups))
            return 1; // each output (behavior) group has just one neuron
        else
            return nint(interp(geneval(numeneurgene+i-numinputneurgroups),
                               mineneurpergroup,maxeneurpergroup));
    }
    else if (i == 0)
        return 1; // single random neuron
    else if (i == 1)
        return 1; // single energy neuron
    else if (i == 2)
        return nint(interp(geneval(numrneurgene),minvispixels,maxvispixels));
    else if (i == 3)
        return nint(interp(geneval(numgneurgene),minvispixels,maxvispixels));
    else if (i == 4)
        return nint(interp(geneval(numbneurgene),minvispixels,maxvispixels));
    else
        error(2,"numeneur called for invalid group number (",i,")");
}


short genome::numineur(cshort i)
{
    if ((i >= numinputneurgroups) && (i < numneurgroups()))
    {
        if (i >= (numneurgroups()-numoutneurgroups))
            return 1; // each output (behavior) group has just one neuron
        else
            return nint(interp(geneval(numineurgene+i-numinputneurgroups),
                               minineurpergroup,maxineurpergroup));
    }
    else if (i == 0)
        return 1; // single random neuron
    else if (i == 1)
        return 1; // single energy neuron
    else if (i == 2)
        return nint(interp(geneval(numrneurgene),minvispixels,maxvispixels));
    else if (i == 3)
        return nint(interp(geneval(numgneurgene),minvispixels,maxvispixels));
    else if (i == 4)
        return nint(interp(geneval(numbneurgene),minvispixels,maxvispixels));
    else
        error(2,"numineur called for invalid group number (",i,")");
}


short genome::numneurons(cshort i)
{
    if ((i >= numinputneurgroups) && (i < numneurgroups()))
    {
        if (i >= (numneurgroups()-numoutneurgroups))
            return 1; // each output (behavior) group has just one neuron
        else
            return (numeneur(i)+numineur(i));
    }
    else if (i >= 0)
        return numeneur(i); // only excitatory
    else
        error(2,"numneurons called for invalid group number (",i,")");
}


void genome::print(clong lobit, clong hibit)
{
    cout << "genome bits " << lobit << " through " << hibit << " =" nl;
    for (long i = lobit; i <= hibit; i++)
    {
        long byte = i >> 3; // 0-based
        long bit = i % 8; // 0-based,from left
        cout << ((genes[byte] >> (7-bit)) & 1);
    }
    cout nlf;
}


void genome::setone(clong lobit, clong hibit)
{
    for (long i = lobit; i <= hibit; i++)
    {
        long byte = i >> 3; // 0-based
        long bit = i % 8; // 0-based,from left
        genes[byte] |= (uchar)(1 << (7-bit));
    }
}


void genome::setzero(clong lobit, clong hibit)
{
    for (long i = lobit; i <= hibit; i++)
    {
        long byte = i >> 3; // 0-based
        long bit = i % 8; // 0-based,from left
        genes[byte] &= (uchar)(~(1 << (7-bit)));
    }
}


void genome::setone()
{
    for (long i = 0; i < numbytes; i++)
        genes[i] = 0xff;
}


void genome::setzero()
{
    for (long i = 0; i < numbytes; i++)
        genes[i] = 0;
}


void braininit()
{
    if (brain::classinited)
        return;

#ifdef DEBUGCALLS
    pushproc("braininit");
#endif DEBUGCALLS

    brain::classinited = TRUE;

    brain::randomneuron = 0;
    brain::energyneuron = 1;

    // remaining neuron-indexes must be determined at "grow" time,
    // now that neural architectures are evolved.

    brain::firsteneur = new short[maxneurgroups];
    if (brain::firsteneur == NULL)
        error(2,"Insufficient memory to allocate firsteneur in braininit");

    brain::firstineur = new short[maxneurgroups];
    if (brain::firstineur == NULL)
        error(2,"Insufficient memory to allocate firstineur in braininit");

    brain::eeremainder = new float[maxneurgroups];
    if (brain::eeremainder == NULL)
        error(2,"Insufficient memory to allocate eeremainder in braininit");

    brain::eiremainder = new float[maxneurgroups];
    if (brain::eiremainder == NULL)
        error(2,"Insufficient memory to allocate eiremainder in braininit");

    brain::iiremainder = new float[maxneurgroups];
    if (brain::iiremainder == NULL)
        error(2,"Insufficient memory to allocate iiremainder in braininit");

    brain::ieremainder = new float[maxneurgroups];
    if (brain::ieremainder == NULL)
        error(2,"Insufficient memory to allocate ieremainder in braininit");

    brain::neurused = new Boolean[max(maxeneurpergroup,maxineurpergroup)];
    if (brain::neurused == NULL)
        error(2,"Insufficient memory to allocate neurused in braininit");
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void braindestruct()
{
#ifdef DEBUGCALLS
    pushproc("braindestruct");
#endif DEBUGCALLS
    if (brain::firsteneur)
        delete brain::firsteneur;
    if (brain::firstineur)
        delete brain::firstineur;
    if (brain::eeremainder)
        delete brain::eeremainder;
    if (brain::eiremainder)
        delete brain::eiremainder;
    if (brain::iiremainder)
        delete brain::iiremainder;
    if (brain::ieremainder)
        delete brain::ieremainder;
    if (brain::neurused)
        delete brain::neurused;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void brain::dump(ostream& out)
{
    register long i;

    out << numneurons sp numsynapses sp numinputneurons sp numnoninputneurons
        sp energyuse nl;
    out << redneuron sp greenneuron sp blueneuron
        sp eatneuron sp mateneuron sp fightneuron sp speedneuron
        sp yawneuron sp lightneuron sp focusneuron nl;
    out << numneurgroups sp numrneurons sp numgneurons sp numbneurons
        sp firstnoninputneuron nl;
    out << xredwidth sp xgreenwidth sp xbluewidth
        sp xredintwidth sp xgreenintwidth sp xblueintwidth nl;

    if (!neuron)
        error(1,"attempted to dump brain with no neurons");
    for (i = 0; i < numneurons; i++)
        out << neuron[i].group sp neuron[i].bias
            sp neuron[i].startsynapses sp neuron[i].endsynapses nl;

    if (!neuronactivation)
        error(1,"attempted to dump brain with no neuronactivations");
    for (i = 0; i < numneurons; i++)
        out << neuronactivation[i] nl;

    if (!synapse)
        error(1,"attempted to dump brain with no synapses");
    for (i = 0; i < numsynapses; i++)
        out << synapse[i].efficacy
            sp synapse[i].fromneuron sp synapse[i].toneuron nl;

    if (!groupblrate)
        error(1,"attempted to dump brain with no groupblrate");
    for (i = 0; i < numneurgroups; i++)
        out << groupblrate[i] nl;

    if (!grouplrate)
        error(1,"attempted to dump brain with no grouplrate");
    for (i = 0; i < (numneurgroups*numneurgroups*4); i++)
        out << grouplrate[i] nl;

}


void brain::report()
{
    cout << numneurons sp numsynapses sp numinputneurons sp numnoninputneurons
        sp energyuse nl;
    cout << redneuron sp greenneuron sp blueneuron
        sp eatneuron sp mateneuron sp fightneuron sp speedneuron
        sp yawneuron sp lightneuron sp focusneuron nl;
    cout << numneurgroups sp numrneurons sp numgneurons sp numbneurons
        sp firstnoninputneuron nl;
    cout << xredwidth sp xgreenwidth sp xbluewidth
        sp xredintwidth sp xgreenintwidth sp xblueintwidth nl;
    cout.flush();
}


void brain::load(istream& in)
{
    register long i;

    in >> numneurons >> numsynapses >> numinputneurons >> numnoninputneurons
       >> energyuse;
    in >> redneuron >> greenneuron >> blueneuron
       >> eatneuron >> mateneuron >> fightneuron >> speedneuron
       >> yawneuron >> lightneuron >> focusneuron;
    in >> numneurgroups >> numrneurons >> numgneurons >> numbneurons
       >> firstnoninputneuron;
    in >> xredwidth >> xgreenwidth >> xbluewidth
       >> xredintwidth >> xgreenintwidth >> xblueintwidth;

    allocatebrainmemory(); // if needed

    for (i = 0; i < numneurons; i++)
        in >> neuron[i].group >> neuron[i].bias
           >> neuron[i].startsynapses >> neuron[i].endsynapses;

    for (i = 0; i < numneurons; i++)
        in >> neuronactivation[i];

    for (i = 0; i < numsynapses; i++)
        in >> synapse[i].efficacy 
           >> synapse[i].fromneuron >> synapse[i].toneuron;

    for (i = 0; i < numneurgroups; i++)
        in >> groupblrate[i];

    for (i = 0; i < (numneurgroups*numneurgroups*4); i++)
        in >> grouplrate[i];

}


void brain::allocatebrainmemory()
{
#ifdef DEBUGCALLS
    pushproc("brain::allocatebrainmemory");
#endif DEBUGCALLS
/*
    // bites off the maximum amount first time only
    if (!neuron)
        neuron = new neuronstruct[maxneurons];
    if (!neuronactivation)
        neuronactivation = new float[maxneurons];
    if (!newneuronactivation)
        newneuronactivation = new float[maxneurons];
    if (!synapse)
        synapse = new synapsestruct[maxsynapses];
*/
/*
    // limits deletes and news by reusing if room is available
    if (neuron && (numneurons > neuronsize))
        delete neuron;
    if (neuronactivation && (numneurons > neuronactivationsize))
        delete neuronactivation;
    if (newneuronactivation && (numneurons > neuronactivationsize))
        delete newneuronactivation;
    if (synapse && (numsynapses > synapsesize))
        delete synapse;

    if (numneurons > neuronsize)
    {
        neuron = new neuronstruct[numneurons];
        if (neuron == NULL)
            error(2,"Insufficient memory to grow neurons");
        else
            neuronsize = numneurons;
    }
    if (numneurons > neuronactivationsize)
    {
        neuronactivation = new float[numneurons];
        if (neuronactivation == NULL)
            error(2,"Insufficient memory to grow neuronactivations");

        newneuronactivation = new float[numneurons];
        if (newneuronactivation == NULL)
            error(2,"Insufficient memory to grow new neuronactivations");
        else
            neuronactivationsize = numneurons;
    }
    if (numsynapses > synapsesize)
    {
        synapse = new synapsestruct[numsynapses];
        if (synapse == NULL)
            error(2,"Insufficient memory to grow synapses");
        else
            synapsesize = numsynapses;
    }
*/
    // sacrifices speed for minimum memory use
    if (neuron) delete neuron;
    if (neuronactivation) delete neuronactivation;
    if (newneuronactivation) delete newneuronactivation;
    if (synapse) delete synapse;
    if (groupblrate) delete groupblrate;
    if (grouplrate) delete grouplrate;

    neuron = new neuronstruct[numneurons];
    if (neuron == NULL)
        error(2,"Insufficient memory to grow neurons");

    neuronactivation = new float[numneurons];
    if (neuronactivation == NULL)
        error(2,"Insufficient memory to grow neuronactivations");

    newneuronactivation = new float[numneurons];
    if (newneuronactivation == NULL)
        error(2,"Insufficient memory to grow new neuronactivations");

    synapse = new synapsestruct[numsynapses];
    if (synapse == NULL)
        error(2,"Insufficient memory to grow synapses");

    groupblrate = new float[numneurgroups];
    if (groupblrate == NULL)
        error(2,"Insufficient memory to store group bias learning rates");
    for (register long i = 0; i < numneurgroups; i++)
        groupblrate[i] = 0.0;
        
    grouplrate = new float[numneurgroups*numneurgroups*4];
    if (grouplrate == NULL)
        error(2,"Insufficient memory to store group learning rates");
    for (i = 0; i < (numneurgroups*numneurgroups*4); i++)
        grouplrate[i] = 0.0;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


short brain::nearestfreeneuron(cshort iin, cBoolean* used, cshort num,
                               cshort exclude)
{
    short iout;
    Boolean tideishigh;
    short hitide = iin;
    short lotide = iin;

//  cout << "*****************************************************" nlf;
//  cout << "iin = " << iin << " , num = " << num << " , exclude = "
//       << exclude nlf;
//  for (short i = 0; i < num; i++)
//      cout << "used[" << i << "] = " << used[i] nl;
//  cout.flush();

    if (iin < (num-1))
    {
        iout = iin + 1;
        tideishigh = TRUE;
    }
    else
    {
        iout = iin - 1;
        tideishigh = FALSE;
    }

    while (used[iout] || (iout == exclude))
    {
//      cout << "iout = " << iout << " , lotide, hitide, tideishigh = "
//           << lotide cms hitide cms tideishigh nlf;
        if (tideishigh)
        {
            hitide = iout;
            if (lotide > 0)
            {
                iout = lotide - 1;
                tideishigh = FALSE;
            }
            else if (hitide < (num-1))
                iout++;
        }
        else
        {
            lotide = iout;
            if (hitide < (num-1))
            {
                iout = hitide + 1;
                tideishigh = TRUE;
            }
            else if (lotide > 0)
                iout--;
        }
//      cout << "new iout = " << iout nlf;
        if ((lotide == 0) && (hitide == (num-1)))
            error(2,"brain::nearestfreeneuron search failed");
    }
    return iout;
}


void brain::grow(genome* g)
{
#ifdef DEBUGCALLS
    pushproc("brain::grow");
#endif DEBUGCALLS
    short i,j,ii;

#ifdef DEBUGCHECK
    debugcheck("brain::grow entry");
#endif DEBUGCHECK

    mygenes = g;

    numneurgroups = g->numneurgroups();

#ifdef DEBUGBRAINGROW
    cout << "****************************************************" nlf;
    cout << "Starting a new brain with numneurgroups = " << numneurgroups nlf;
#endif DEBUGBRAINGROW

    numinputneurons = 0;
    for (i = 0; i < numinputneurgroups; i++)
    {
        firsteneur[i] = numinputneurons;
        firstineur[i] = numinputneurons; // input neurons double as e & i
        numinputneurons += g->numeneur(i);

#ifdef DEBUGBRAINGROW
        cout << "group " << i << " has " << g->numeneur(i) << " neurons" nlf;
#endif DEBUGBRAINGROW

    }
    firstnoninputneuron = numinputneurons;

#ifdef DEBUGBRAINGROW
    cout << "numinputneurons = " << numinputneurons nlf;
#endif DEBUGBRAINGROW

    // note, group 0 = randomneuron, group 1 = energyneuron
    // group 2 = redneuron(s)
    redneuron = energyneuron + 1;
    numrneurons = short(g->numeneur(2));
    // group 3 = greenneuron(s)
    greenneuron = redneuron + numrneurons;
    numgneurons = short(g->numeneur(3));
    // group 4 = blueneuron(s)
    blueneuron = greenneuron + numgneurons;
    numbneurons = short(g->numeneur(4));

    xredwidth = float(retinawidth) / float(numrneurons);
    xgreenwidth = float(retinawidth) / float(numgneurons);
    xbluewidth = float(retinawidth) / float(numbneurons);
    xredintwidth = retinawidth / numrneurons;
    if ((xredintwidth*numrneurons) != retinawidth)
        xredintwidth = 0;
    xgreenintwidth = retinawidth / numgneurons;
    if ((xgreenintwidth*numgneurons) != retinawidth)
        xgreenintwidth = 0;
    xblueintwidth = retinawidth / numbneurons;
    if ((xblueintwidth*numbneurons) != retinawidth)
        xblueintwidth = 0;

#ifdef DEBUGBRAINGROW
    cout << "numrneurons, numgneurons, numbneurons = "
         << numrneurons cms numgneurons cms numbneurons nlf;;
#endif DEBUGBRAINGROW

    numsynapses = 0;
    numnoninputneurons = 0;
    for (i = firstnoninputgroup, ii = 0; i < numneurgroups; i++, ii++)
    {
        firsteneur[i] = numinputneurons + numnoninputneurons;
        if (i < (numneurgroups-numoutneurgroups))//output neurons are both e & i
            numnoninputneurons += g->numeneur(i);

        firstineur[i] = numinputneurons + numnoninputneurons;
        numnoninputneurons += g->numineur(i);

#ifdef DEBUGBRAINGROW
        cout << "group " << i << " has " << g->numeneur(i) << " e-neurons" nlf;
        cout << "  and " << i << " has " << g->numineur(i) << " i-neurons" nlf;
#endif DEBUGBRAINGROW

        for (j = 0; j < numneurgroups; j++)
        {
            numsynapses += g->numsynapses(i,j);

#ifdef DEBUGBRAINGROW
            cout << "  from " << j << " to " << i << " there are "
                 << g->numeesynapses(i,j) << " e-e synapses" nlf;
            cout << "  from " << j << " to " << i << " there are "
                 << g->numiesynapses(i,j) << " i-e synapses" nlf;
            cout << "  from " << j << " to " << i << " there are "
                 << g->numeisynapses(i,j) << " e-i synapses" nlf;
            cout << "  from " << j << " to " << i << " there are "
                 << g->numiisynapses(i,j) << " i-i synapses" nlf;
            cout << "  from " << j << " to " << i << " there are "
                 << g->numsynapses(i,j) << " total synapses" nlf;
#endif DEBUGBRAINGROW

        }
    }
    numneurons = numnoninputneurons + numinputneurons;
    if (numneurons > maxneurons)
        error(2,"numneurons (",numneurons,") > maxneurons (",
                               maxneurons,") in brain::grow");
    if (numsynapses > maxsynapses)
        error(2,"numsynapses (",numsynapses,") > maxsynapses (",
                                maxsynapses,") in brain::grow");

    // set up the ouput/behavior neurons as the last numoutneur neurons

    focusneuron = numneurons - 1;
    lightneuron = focusneuron - 1;
    yawneuron = lightneuron - 1;
    speedneuron = yawneuron - 1;
    fightneuron = speedneuron - 1;
    mateneuron = fightneuron - 1;
    eatneuron = mateneuron - 1;

#ifdef DEBUGBRAINGROW
    cout << "numneurons = " << numneurons << "  (of " << maxneurons pnlf;
    cout << "numsynapses = " << numsynapses << "  (of " << maxsynapses pnlf;
#endif DEBUGBRAINGROW

#ifdef DEBUGCHECK
    debugcheck("brain::grow before allocating memory");
#endif DEBUGCHECK

    allocatebrainmemory(); // if needed

#ifdef DEBUGCHECK
    debugcheck("brain::grow after allocating memory");
#endif DEBUGCHECK

    short ini,ineur,jneur,nneuri,nneurj,joff,disneur;
    short isyn,newsyn;
    long  nsynij;
    float nsynijperneur;
    long numsyn = 0;
    short numneur = numinputneurons;
    float tdij;

    for (i = 0, ineur = 0; i < numinputneurgroups; i++)
    {
        for (j = 0; j < g->numeneur(i); j++, ineur++)
        {
            neuron[ineur].group = i;
            neuron[ineur].bias = 0.0;         // not used
            neuron[ineur].startsynapses = -1; // not used
            neuron[ineur].endsynapses = -1;   // not used
        }
    }

    for (i = firstnoninputgroup; i < numneurgroups; i++)
    {
#ifdef DEBUGBRAINGROW
        cout << "For group " << i << ":" nlf;
#endif DEBUGBRAINGROW

        float groupbias = g->bias(i);
        groupblrate[i] = g->biaslearningrate(i);

#ifdef DEBUGBRAINGROW
        cout << "  groupbias = " << groupbias nlf;
        cout << "  groupbiaslearningrate = " << groupblrate[i] nlf;
#endif DEBUGBRAINGROW

        for (j = 0; j < numneurgroups; j++)
        {
            eeremainder[j] = 0.0;
            eiremainder[j] = 0.0;
            iiremainder[j] = 0.0;
            ieremainder[j] = 0.0;

            grouplrate[index4(i,j,0,0,numneurgroups,2,2)] = g->eelr(i,j);
            grouplrate[index4(i,j,0,1,numneurgroups,2,2)] = g->ielr(i,j);
            grouplrate[index4(i,j,1,1,numneurgroups,2,2)] = g->iilr(i,j);
            grouplrate[index4(i,j,1,0,numneurgroups,2,2)] = g->eilr(i,j);
        }

        // setup all e-neurons for this group

        nneuri = g->numeneur(i);

#ifdef DEBUGBRAINGROW
        cout << "  Setting up " << nneuri << " e-neurons" nlf;
#endif DEBUGBRAINGROW

        for (ini = 0; ini < nneuri; ini++)
        {
            ineur = ini + firsteneur[i];

#ifdef DEBUGBRAINGROW
            cout << "  For ini, ineur = "
                 << ini cms ineur << ":" nlf;
#endif DEBUGBRAINGROW

            neuron[ineur].group = i;
            neuron[ineur].bias = groupbias;
            neuron[ineur].startsynapses = numsyn;

#ifdef DEBUGBRAINGROW
            cout << "    group = " << neuron[ineur].group nlf;
            cout << "    bias = " << neuron[ineur].bias nlf;
            cout << "    startsynapses = " << neuron[ineur].startsynapses nlf;
            cout << "    Setting up e-e connections:" nlf;
#endif DEBUGBRAINGROW

            // setup all e-e connections for this e-neuron

            for (j = 0; j < numneurgroups; j++)
            {
                nneurj = g->numeneur(j);

#ifdef DEBUGBRAINGROW
                cout << "      From group " << j nlf;
                cout << "      with nneurj, (old)eeremainder = "
                     << nneurj cms eeremainder[j] nlf;
#endif DEBUGBRAINGROW

                nsynij = g->numeesynapses(i,j);
                nsynijperneur = float(nsynij)/float(nneuri);
                newsyn = short(nsynijperneur + eeremainder[j] + 1.e-5);
                eeremainder[j] += nsynijperneur - newsyn;
                tdij = g->eetd(i,j);

                joff = short((float(ini)/float(nneuri)) * float(nneurj)
                     - float(newsyn) * 0.5);
                joff = max(0,min(nneurj-newsyn,joff));

#ifdef DEBUGBRAINGROW
                cout << "      and nsynij, nsynijperneur, newsyn = "
                     << nsynij cms nsynijperneur cms newsyn nlf;
                cout << "      and (new)eeremainder, tdij, joff = "
                     << eeremainder[j] cms tdij cms joff nlf;
#endif DEBUGBRAINGROW

                if ((joff+newsyn) > nneurj)
                {
                    error(2,"Illegal architecture generated: ",
                        "more e-e synapses from group ",j,
                        " to group ",i,
                        " than there are i-neurons in group ",j);
                }

                if (newsyn > 0)
                    for (ii = 0; ii < nneurj; ii++)
                        neurused[ii] = FALSE;

                for (isyn = 0; isyn < newsyn; isyn++)
                {
                    if (drand48() < tdij)
                    {
                        disneur = short(nint(rrand(-0.5,0.5)*tdij*nneurj));
                        jneur = isyn + joff + disneur;
                        if (jneur < 0)
                            jneur += nneurj;
                        else if (jneur >= nneurj)
                            jneur -= nneurj;
                    }
                    else
                        jneur = isyn + joff;

                    if ( ((jneur+firsteneur[j]) == ineur) // same neuron or
                        || neurused[jneur] ) // already connected to this one
                    {
                        if (i == j) // same group and neuron type
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,ini);
                        else
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,jneur);
                    }

                    neurused[jneur] = TRUE;

                    jneur += firsteneur[j];

                    synapse[numsyn].fromneuron =  jneur; // + denotes excitatory
                    synapse[numsyn].toneuron   =  ineur; // + denotes excitatory
                    if (ineur == jneur)
                        synapse[numsyn].efficacy = 0.0;
                    else
                        synapse[numsyn].efficacy =
                            rrand(initminweight,initmaxweight);

#ifdef DEBUGBRAINGROW
                    cout << "        synapse[" << numsyn
                         << "].toneur, fromneur, efficacy, lrate = "
                         << ineur cms jneur cms synapse[numsyn].efficacy nlf;
#endif DEBUGBRAINGROW

                    numsyn++;
                }
            }

            // setup all i-e connections for this e-neuron

#ifdef DEBUGBRAINGROW
            cout << "    Setting up i-e connections:" nlf;
#endif DEBUGBRAINGROW

            for (j = 0; j < numneurgroups; j++)
            {
                nneurj = g->numineur(j);

#ifdef DEBUGBRAINGROW
                cout << "      From group " << j nlf;
                cout << "      with nneurj, (old)ieremainder = "
                     << nneurj cms ieremainder[j] nlf;
#endif DEBUGBRAINGROW

                nsynij = g->numiesynapses(i,j);
                nsynijperneur = float(nsynij)/float(nneuri);
                newsyn = short(nsynijperneur + ieremainder[j] + 1.e-5);
                ieremainder[j] += nsynijperneur - newsyn;
                tdij = g->ietd(i,j);

                joff = short((float(ini)/float(nneuri)) * float(nneurj)
                     - float(newsyn) * 0.5);
                joff = max(0,min(nneurj-newsyn,joff));

#ifdef DEBUGBRAINGROW
                cout << "      and nsynij, nsynijperneur, newsyn = "
                     << nsynij cms nsynijperneur cms newsyn nlf;
                cout << "      and (new)ieremainder, tdij, joff = "
                     << ieremainder[j] cms tdij cms joff nlf;
#endif DEBUGBRAINGROW

                if ((joff+newsyn) > nneurj)
                {
                    error(2,"Illegal architecture generated: ",
                        "more i-e synapses from group ",j,
                        " to group ",i,
                        " than there are i-neurons in group ",j);
                }

                if (newsyn > 0)
                    for (ii = 0; ii < nneurj; ii++)
                        neurused[ii] = FALSE;

                for (isyn = 0; isyn < newsyn; isyn++)
                {
                    if (drand48() < tdij)
                    {
                        disneur = short(nint(rrand(-0.5,0.5)*tdij*nneurj));
                        jneur = isyn + joff + disneur;
                        if (jneur < 0)
                            jneur += nneurj;
                        else if (jneur >= nneurj)
                            jneur -= nneurj;
                    }
                    else
                        jneur = isyn + joff;

                    if ( ((jneur+firstineur[j]) == ineur) // same neuron or
                        || neurused[jneur] ) // already connected to this one
                    {
                        if ((i==j)&&(i==(numneurgroups-1)))//same & output group
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,ini);
                        else
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,jneur);
                    }

                    neurused[jneur] = TRUE;

                    jneur += firstineur[j];

                    synapse[numsyn].fromneuron = -jneur; // - denotes inhibitory
                    synapse[numsyn].toneuron   =  ineur; // + denotes excitatory
                    if (ineur == jneur) // can't happen anymore?
                        synapse[numsyn].efficacy = 0.0;
                    else
                        synapse[numsyn].efficacy =
                            min(-1.e-10,-rrand(initminweight,initmaxweight));

#ifdef DEBUGBRAINGROW
                    cout << "        synapse[" << numsyn
                         << "].toneur, fromneur, efficacy, lrate = "
                         << ineur cms jneur cms synapse[numsyn].efficacy nlf;
#endif DEBUGBRAINGROW

                    numsyn++;
                }
            }

            neuron[ineur].endsynapses = numsyn;
            numneur++;
        }

        // setup all i-neurons for this group

        if (i >= (numneurgroups-numoutneurgroups))
            nneuri = 0;  // output/behavior neurons are e-only postsynaptically
        else
            nneuri = g->numineur(i);

#ifdef DEBUGBRAINGROW
        cout << "  Setting up " << nneuri << " i-neurons" nlf;
#endif DEBUGBRAINGROW

        for (ini = 0; ini < nneuri; ini++)
        {
            ineur = ini + firstineur[i];

#ifdef DEBUGBRAINGROW
            cout << "  For ini, ineur = "
                 << ini cms ineur << ":" nlf;
#endif DEBUGBRAINGROW

            neuron[ineur].group = i;
            neuron[ineur].bias = groupbias;
            neuron[ineur].startsynapses = numsyn;

#ifdef DEBUGBRAINGROW
            cout << "    group = " << neuron[ineur].group nlf;
            cout << "    bias = " << neuron[ineur].bias nlf;
            cout << "    startsynapses = " << neuron[ineur].startsynapses nlf;
            cout << "    Setting up e-i connections:" nlf;
#endif DEBUGBRAINGROW

            // setup all e-i connections for this i-neuron

            for (j = 0; j < numneurgroups; j++)
            {
                nneurj = g->numeneur(j);

#ifdef DEBUGBRAINGROW
                cout << "      From group " << j nlf;
                cout << "      with nneurj, (old)eiremainder = "
                     << nneurj cms eiremainder[j] nlf;
#endif DEBUGBRAINGROW

                nsynij = g->numeisynapses(i,j);
                nsynijperneur = float(nsynij)/float(nneuri);
                newsyn = short(nsynijperneur + eiremainder[j] + 1.e-5);
                eiremainder[j] += nsynijperneur - newsyn;
                tdij = g->eitd(i,j);

                joff = short((float(ini)/float(nneuri)) * float(nneurj)
                     - float(newsyn) * 0.5);
                joff = max(0,min(nneurj-newsyn,joff));

#ifdef DEBUGBRAINGROW
                cout << "      and nsynij, nsynijperneur, newsyn = "
                     << nsynij cms nsynijperneur cms newsyn nlf;
                cout << "      and (new)eiremainder, tdij, joff = "
                     << eiremainder[j] cms tdij cms joff nlf;
#endif DEBUGBRAINGROW

                if ((joff+newsyn) > nneurj)
                {
                    error(2,"Illegal architecture generated: ",
                        "more e-i synapses from group ",j,
                        " to group ",i,
                        " than there are e-neurons in group ",j);
                }

                if (newsyn > 0)
                    for (ii = 0; ii < nneurj; ii++)
                        neurused[ii] = FALSE;

                for (isyn = 0; isyn < newsyn; isyn++)
                {
                    if (drand48() < tdij)
                    {
                        disneur = short(nint(rrand(-0.5,0.5)*tdij*nneurj));
                        jneur = isyn + joff + disneur;
                        if (jneur < 0)
                            jneur += nneurj;
                        else if (jneur >= nneurj)
                            jneur -= nneurj;
                    }
                    else
                        jneur = isyn + joff;

                    if ( ((jneur+firsteneur[j]) == ineur) // same neuron or
                        || neurused[jneur] ) // already connected to this one
                    {
                        if ((i==j)&&(i==(numneurgroups-1)))//same & output group
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,ini);
                        else
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,jneur);
                    }

                    neurused[jneur] = TRUE;

                    jneur += firsteneur[j];

                    synapse[numsyn].fromneuron =  jneur; // + denotes excitatory
                    synapse[numsyn].toneuron   = -ineur; // - denotes inhibitory
                    if (ineur == jneur) // can't happen anymore?
                        synapse[numsyn].efficacy = 0.0;
                    else
                        synapse[numsyn].efficacy =
                            rrand(initminweight,initmaxweight);

#ifdef DEBUGBRAINGROW
                    cout << "        synapse[" << numsyn
                         << "].toneur, fromneur, efficacy, lrate = "
                         << ineur cms jneur cms synapse[numsyn].efficacy nlf;
#endif DEBUGBRAINGROW

                    numsyn++;
                }
            }

            // setup all i-i connections for this i-neuron

            for (j = 0; j < numneurgroups; j++)
            {
                nneurj = g->numineur(j);

#ifdef DEBUGBRAINGROW
                cout << "      From group " << j nlf;
                cout << "      with nneurj, (old)iiremainder = "
                     << nneurj cms iiremainder[j] nlf;
#endif DEBUGBRAINGROW

                nsynij = g->numiisynapses(i,j);
                nsynijperneur = float(nsynij)/float(nneuri);
                newsyn = short(nsynijperneur + iiremainder[j] + 1.e-5);
                iiremainder[j] += nsynijperneur - newsyn;
                tdij = g->iitd(i,j);

                joff = short((float(ini)/float(nneuri)) * float(nneurj)
                     - float(newsyn) * 0.5);
                joff = max(0,min(nneurj-newsyn,joff));

#ifdef DEBUGBRAINGROW
                cout << "      and nsynij, nsynijperneur, newsyn = "
                     << nsynij cms nsynijperneur cms newsyn nlf;
                cout << "      and (new)iiremainder, tdij, joff = "
                     << iiremainder[j] cms tdij cms joff nlf;
#endif DEBUGBRAINGROW

                if ((joff+newsyn) > nneurj)
                {
                    error(2,"Illegal architecture generated: ",
                        "more i-i synapses from group ",j,
                        " to group ",i,
                        " than there are i-neurons in group ",j);
                }

                if (newsyn > 0)
                    for (ii = 0; ii < nneurj; ii++)
                        neurused[ii] = FALSE;

                for (isyn = 0; isyn < newsyn; isyn++)
                {
                    if (drand48() < tdij)
                    {
                        disneur = short(nint(rrand(-0.5,0.5)*tdij*nneurj));
                        jneur = isyn + joff + disneur;
                        if (jneur < 0)
                            jneur += nneurj;
                        else if (jneur >= nneurj)
                            jneur -= nneurj;
                    }
                    else
                        jneur = isyn + joff;

                    if ( ((jneur+firstineur[j]) == ineur) // same neuron or
                        || neurused[jneur] ) // already connected to this one
                    {
                        if (i == j) // same group and neuron type
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,ini);
                        else
                            jneur = nearestfreeneuron(jneur,&neurused[0],
                                                      nneurj,jneur);
                    }

                    neurused[jneur] = TRUE;

                    jneur += firstineur[j];

                    synapse[numsyn].fromneuron = -jneur; // - denotes inhibitory
                    synapse[numsyn].toneuron   = -ineur; // - denotes inhibitory
                    if (ineur == jneur) // can't happen anymore?
                        synapse[numsyn].efficacy = 0.0;
                    else
                        synapse[numsyn].efficacy =
                            min(-1.e-10,-rrand(initminweight,initmaxweight));

#ifdef DEBUGBRAINGROW
                    cout << "        synapse[" << numsyn
                         << "].toneur, fromneur, efficacy, lrate = "
                         << ineur cms jneur cms synapse[numsyn].efficacy nlf;
#endif DEBUGBRAINGROW

                    numsyn++;
                }
            }

            neuron[ineur].endsynapses = numsyn;
            numneur++;
        }
    }

    if (numneur != (numneurons))
        error(2,"Bad neural architecture, numneur (",numneur,
           ") not equal to numneurons (",numneurons,")");

    if (numsyn != (numsynapses))
        error(2,"Bad neural architecture, numsyn (",numsyn,
           ") not equal to numsynapses (",numsynapses,")");

    for (i = 0; i < numneurons; i++)
        neuronactivation[i] = 0.5;

    energyuse = maxneuron2energy *float(numneurons) /float(maxneurons)
              + maxsynapse2energy*float(numsynapses)/float(maxsynapses);

#ifdef DEBUGCHECK
    debugcheck("brain::grow after setting up architecture");
#endif DEBUGCHECK

    // now send some signals through the system
    // try pure noise for now...

    for (i = 0; i < numprebirthcycles; i++)
    {
        // load up the retinabuf with noise
        for (j = 0; j < (retinawidth*4); j++)
            retinabuf[j] = (unsigned char)(rrand(0.,255.));
        update(drand48());
    }

#ifdef DEBUGCHECK
    debugcheck("brain::grow after prebirth cycling");
#endif DEBUGCHECK

#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS

}


void brain::update(cfloat energyfraction)
{
#ifdef DEBUGCALLS
    pushproc("brain::update");
#endif DEBUGCALLS

#ifdef DEBUGCHECK
    debugcheck("brain::update entry");
#endif DEBUGCHECK

    short i,j,ii,jj;
    long k;
    if ((neuron == NULL) || (synapse == NULL) || (neuronactivation == NULL))
    {
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return;
    }
#ifdef TIMING
    timebeg(11);
#endif TIMING
#ifdef PRINTBRAIN
    if (printbrain && overheadrank && (!brainprinted) &&
        (currentcritter == monitorcritter))
    {
        brainprinted = TRUE;
        printf("neuron (toneuron)  fromneuron   synapse   efficacy\n");
        for (i = firstnoninputneuron; i < numneurons; i++)
        {
            for (k = neuron[i].startsynapses; k < neuron[i].endsynapses; k++)
            {
                printf(" %3d   %3d    %3d    %5d    %g\n",
                    i,synapse[k].toneuron,synapse[k].fromneuron,
                    k,synapse[k].efficacy);
            }
        }
        printf("numrneurons, numgneurons, numbneurons = %d, %d, %d\n",
                numrneurons, numgneurons, numbneurons);
        printf("xredwidth, xgreenwidth, xbluewidth = %g, %g, %g\n",
                xredwidth, xgreenwidth, xbluewidth);
        printf("xredintwidth, xgreenintwidth, xblueintwidth = %d, %d, %d\n",
                xredintwidth, xgreenintwidth, xblueintwidth);
    }
#endif PRINTBRAIN

    neuronactivation[randomneuron] = drand48();
    neuronactivation[energyneuron] = energyfraction;
    short pixel;
    float avgcolor;
    float endpixloc;
    if (xredintwidth)
    {
        pixel = 0;
        for (i = 0; i < numrneurons; i++)
        {
            avgcolor = 0.0;
            for (short ipix = 0; ipix < xredintwidth; ipix++)
                avgcolor += retinabuf[(pixel++)*4+3];
            neuronactivation[redneuron+i] = avgcolor/(xredwidth*255.0);
        }
    }
    else
    {
        pixel = 0;
        avgcolor = 0.0;
#ifdef PRINTBRAIN
        if (printbrain &&
            (currentcritter == monitorcritter))
        {
            printf("xredwidth = %g\n",i,xredwidth);
        }
#endif PRINTBRAIN
        for (i = 0; i < numrneurons; i++)
        {
            endpixloc = xredwidth * float(i+1);
#ifdef PRINTBRAIN
            if (printbrain &&
                (currentcritter == monitorcritter))
            {
                printf("  neuron %d, endpixloc = %g\n",i,endpixloc);
            }
#endif PRINTBRAIN
            while (float(pixel) < (endpixloc-1.))
            {
                avgcolor += retinabuf[(pixel++)*4+3];
#ifdef PRINTBRAIN
                if (printbrain &&
                    (currentcritter == monitorcritter))
                {
                    printf("    in loop with pixel %d, avgcolor = %g\n",
                        pixel,avgcolor);
                    if ((float(pixel) < (endpixloc-1.)) &&
                        (float(pixel) >= (endpixloc-1.-1.e-5)) )
                        printf("Got in-loop borderline case - red\n");
                }
#endif PRINTBRAIN
            }
            avgcolor += (endpixloc-float(pixel)) * retinabuf[pixel*4+3];
            neuronactivation[redneuron+i] = avgcolor/(xredwidth*255.0);
#ifdef PRINTBRAIN
            if (printbrain &&
                (currentcritter == monitorcritter))
            {
                printf("    after loop with pixel %d, avgcolor = %g, color = %g\n",
                    pixel,avgcolor,neuronactivation[redneuron+i]);
                if ((float(pixel) >= (endpixloc-1.)) &&
                    (float(pixel) < (endpixloc-1.+1.e-5)) )
                    printf("Got outside-loop borderline case - red\n");
            }
#endif PRINTBRAIN
            avgcolor = (1.-(endpixloc-float(pixel))) * retinabuf[pixel*4+3];
#ifdef PRINTBRAIN
            if (printbrain &&
                (currentcritter == monitorcritter))
            {
                printf("  before incrementing pixel = %d, avgcolor = %g\n",
                    pixel,avgcolor);
            }
#endif PRINTBRAIN
            pixel++;
        }
    }
    if (xgreenintwidth)
    {
        pixel = 0;
        for (i = 0; i < numgneurons; i++)
        {
            avgcolor = 0.0;
            for (short ipix = 0; ipix < xgreenintwidth; ipix++)
                avgcolor += retinabuf[(pixel++)*4+2];
            neuronactivation[greenneuron+i] = avgcolor/(xgreenwidth*255.0);
        }
    }
    else
    {
        pixel = 0;
        avgcolor = 0.0;
        for (i = 0; i < numgneurons; i++)
        {
            endpixloc = xgreenwidth * float(i+1);
            while (float(pixel) < (endpixloc-1.))
            {
                avgcolor += retinabuf[(pixel++)*4+2];
#ifdef PRINTBRAIN
                if (printbrain &&
                    (currentcritter == monitorcritter))
                {
                    if ((float(pixel) < (endpixloc-1.)) &&
                        (float(pixel) >= (endpixloc-1.-1.e-5)) )
                        printf("Got in-loop borderline case - green\n");
                }
#endif PRINTBRAIN
            }
#ifdef PRINTBRAIN
            if (printbrain &&
                (currentcritter == monitorcritter))
            {
                if ((float(pixel) >= (endpixloc-1.)) &&
                    (float(pixel) < (endpixloc-1.)) )
                    printf("Got outside-loop borderline case - green\n");
            }
#endif PRINTBRAIN
            avgcolor += (endpixloc-float(pixel)) * retinabuf[pixel*4+2];
            neuronactivation[greenneuron+i] = avgcolor/(xgreenwidth*255.0);
            avgcolor = (1.-(endpixloc-float(pixel))) * retinabuf[pixel*4+2];
            pixel++;
        }
    }
    if (xblueintwidth)
    {
        pixel = 0;
        for (i = 0; i < numbneurons; i++)
        {
            avgcolor = 0.0;
            for (short ipix = 0; ipix < xblueintwidth; ipix++)
                avgcolor += retinabuf[(pixel++)*4+1];
            neuronactivation[blueneuron+i] = avgcolor/(xbluewidth*255.0);
        }
    }
    else
    {
        pixel = 0;
        avgcolor = 0.0;
        for (i = 0; i < numbneurons; i++)
        {
            endpixloc = xbluewidth * float(i+1);
            while (float(pixel) < (endpixloc-1.+1.e-5))
            {
                avgcolor += retinabuf[(pixel++)*4+1];
#ifdef PRINTBRAIN
                if (printbrain &&
                    (currentcritter == monitorcritter))
                {
                    if ((float(pixel) < (endpixloc-1.)) &&
                        (float(pixel) >= (endpixloc-1.-1.e-5)) )
                        printf("Got in-loop borderline case - blue\n");
                }
#endif PRINTBRAIN
            }
#ifdef PRINTBRAIN
            if (printbrain &&
                (currentcritter == monitorcritter))
            {
                if ((float(pixel) >= (endpixloc-1.)) &&
                    (float(pixel) < (endpixloc-1.+1.e-5)) )
                    printf("Got outside-loop borderline case - blue\n");
            }
#endif PRINTBRAIN
            avgcolor += (endpixloc-float(pixel)) * retinabuf[pixel*4+1];
            neuronactivation[blueneuron+i] = avgcolor/(xbluewidth*255.0);
            avgcolor = (1.-(endpixloc-float(pixel))) * retinabuf[pixel*4+1];
            pixel++;
        }
    }

#ifdef DEBUGCHECK
    debugcheck("brain::update after updating vision");
#endif DEBUGCHECK

#ifdef PRINTBRAIN
    if (printbrain && overheadrank &&
        (currentcritter == monitorcritter))
    {
        printf("***** age = %d ****** overheadrank = %d ******\n",
            age,overheadrank);
        printf("retinabuf [0 - %d]\n",(retinawidth-1));
        printf("red:");
        for (i = 3; i < (retinawidth*4); i+=4)
            printf(" %3d",retinabuf[i]);
        printf("\ngreen:");
        for (i = 2; i < (retinawidth*4); i+=4)
            printf(" %3d",retinabuf[i]);
        printf("\nblue:");
        for (i = 1; i < (retinawidth*4); i+=4)
            printf(" %3d",retinabuf[i]);
        printf("\n");
    }
#endif PRINTBRAIN

#ifdef TIMING
    timeend(11);
    timebeg(12);
#endif TIMING
    for (i = firstnoninputneuron; i < numneurons; i++)
    {
        newneuronactivation[i] = neuron[i].bias;
        for (k = neuron[i].startsynapses; k < neuron[i].endsynapses; k++)
            newneuronactivation[i] += synapse[k].efficacy *
               neuronactivation[abs(synapse[k].fromneuron)];
        newneuronactivation[i] = logistic(newneuronactivation[i],logisticslope);
    }

#ifdef DEBUGCHECK
    debugcheck("brain::update after updating neurons");
#endif DEBUGCHECK

#ifdef PRINTBRAIN
    if (printbrain && overheadrank &&
        (currentcritter == monitorcritter))
    {
        printf("  i neuron[i].bias neuronactivation[i] newneuronactivation[i]\n");
        for (i = 0; i < numneurons; i++)
            printf("%3d  %1.4f  %1.4f  %1.4f\n",
                i,neuron[i].bias,neuronactivation[i],newneuronactivation[i]);
    }
#endif PRINTBRAIN

#ifdef TIMING
    timeend(12);
    timebeg(13);
#endif TIMING
    float learningrate;
    for (k = 0; k < numsynapses; k++)
    {
        if (synapse[k].toneuron >= 0) // 0 can't happen it's an input neuron
        {
            i = synapse[k].toneuron;
            ii = 0;
        }
        else
        {
            i = -synapse[k].toneuron;
            ii = 1;
        }
        if ( (synapse[k].fromneuron > 0) ||
            ((synapse[k].toneuron  == 0) && (synapse[k].efficacy >= 0.0)) )
        {
            j = synapse[k].fromneuron;
            jj = 0;
        }
        else
        {
            j = -synapse[k].fromneuron;
            jj = 1;
        }
        // Note: If .toneuron == 0, and .efficacy were to equal
        // 0.0 for an inhibitory synapse, we would choose the
        // wrong learningrate, but we prevent efficacy from going
        // to zero below & during initialization to prevent this.
        // Similarly, learningrate is guaranteed to be < 0.0 for
        // inhibitory synapses.
        short ix1 = index4(neuron[i].group,neuron[j].group,ii,jj,
                           numneurgroups,2,2);
        short gi = neuron[i].group;
        short gj = neuron[j].group;
        short ix2 = index4(gi,gj,ii,jj,numneurgroups,2,2);
        learningrate =
            grouplrate[index4(neuron[i].group,neuron[j].group,ii,jj,
                              numneurgroups,2,2)];
        synapse[k].efficacy += learningrate
                             * (newneuronactivation[i]-0.5)
                             * (   neuronactivation[j]-0.5);

        if (fabs(synapse[k].efficacy) > (0.5*maxweight))
        {
            synapse[k].efficacy *= 1.0 + (decayrate-1.0)*
                (fabs(synapse[k].efficacy)-0.5*maxweight)/(0.5*maxweight);
            if (synapse[k].efficacy > maxweight)
                synapse[k].efficacy = maxweight;
            else if (synapse[k].efficacy < -maxweight)
                synapse[k].efficacy = -maxweight;
        }
        else
        {
            // not strictly correct for this to be in an else clause,
            // but if lrate is reasonable, efficacy should never change
            // sign with a new magnitude greater than 0.5*maxweight
            if (learningrate > 0.0)  // excitatory
                synapse[k].efficacy = max(0.0,synapse[k].efficacy);
            if (learningrate < 0.0)  // inhibitory
                synapse[k].efficacy = min(-1.e-10,synapse[k].efficacy);
        }
    }

#ifdef DEBUGCHECK
    debugcheck("brain::update after updating synapses");
#endif DEBUGCHECK

    for (i = firstnoninputneuron; i < numneurons; i++)
    {
        neuron[i].bias += groupblrate[neuron[i].group]
                        * (newneuronactivation[i]-0.5)
                        * 0.5;
        if (fabs(neuron[i].bias) > (0.5*maxweight))
        {
            neuron[i].bias *= 1.0 + (decayrate-1.0)*
                (fabs(neuron[i].bias)-0.5*maxweight)/(0.5*maxweight);
            if (neuron[i].bias > maxweight)
                neuron[i].bias = maxweight;
            else if (neuron[i].bias < -maxweight)
                neuron[i].bias = -maxweight;
        }
    }

#ifdef DEBUGCHECK
    debugcheck("brain::update after updating biases");
#endif DEBUGCHECK

#ifdef TIMING
    timeend(13);
#endif TIMING
    float* saveneuronactivation = neuronactivation;
    neuronactivation = newneuronactivation;
    newneuronactivation = saveneuronactivation;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void brain::gdump(cshort patchwidth, cshort patchheight)
{
    if ((neuron == NULL) || (synapse == NULL))
        return;
#ifdef DEBUGCALLS
    pushproc("brain::gdump");
#endif DEBUGCALLS
    short i;
    short x1,y1,x2,y2;
    short xoff,yoff;
    long k;

    // the neuronactivation and newneuronactivation arrays have been
    // repointered by this time, so their contents are the reverse of
    // their names in this routine

    // this horizontal row of elements shows the new inputs and the states
    // at the previous time step used to calculate the new values

    y1 = patchheight;
    y2 = y1 + patchheight;
    for (i = 0, x1 = 2*patchheight; i < short(numneurons); i++, x1+=patchwidth)
    {
        // the following reference to "newneuron" really gets the old
        // values, except for the clamped input neuron values (which are new)
        register uchar mag = (uchar)(newneuronactivation[i] * 255.);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x1+patchwidth,y2);
    }

    // this vertical row of elements shows the biases (unfortunately the
    // new, updated values rather than those actually used to compute the
    // neuronal states which are adjacent) in all non-input neurons

    x1 = 0;
    x2 = patchheight;
    for (i = 0, y1 = 2*patchheight; i < numnoninputneurons;
         i++, y1 += patchwidth)
    {
        register uchar mag = (uchar)((maxweight+neuron[i].bias) * 127.5
                           / maxweight);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x2,y1+patchwidth);
    }

    // this vertical row of elements shows the new states in all the
    // unclamped neurons, including the output neurons

    x1 = x2;
    x2 = x1 + patchheight;
    for (i = short(firstnoninputneuron), y1 = 2*patchheight; i < numneurons;
         i++, y1 += patchwidth)
    {
        register uchar mag = (uchar)(neuronactivation[i] * 255.);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x2,y1+patchwidth);
    }

    // this array of synaptic strengths unfortunately shows the new, updated
    // values rather than those actually used to map the displayed horizontal
    // row of neuronal values onto the displayed vertical row of values

    xoff = 2*patchheight;
    yoff = 2*patchheight;
    cpack((127<<16) | (127<<8));
    sboxfi(xoff,
           yoff,
           xoff+short(numneurons)*patchwidth,
           yoff+short(numnoninputneurons)*patchwidth);

    for (k = 0; k < numsynapses; k++)
    {
        register uchar mag = (uchar)((maxweight+synapse[k].efficacy) * 127.5
                           / maxweight);
        cpack((mag<<16) | (mag<<8) | mag);
        x1 = xoff + abs(synapse[k].fromneuron)*patchwidth;
        y1 = yoff + (abs(synapse[k].toneuron)-firstnoninputneuron)*patchwidth;
        sboxfi(x1,y1,x1+patchwidth,y1+patchwidth);
    }

    // now highlight the input and output neurons for clarity

    y1 = patchheight;
    y2 = y1 + patchheight;

    x1 = short(redneuron)*patchwidth + xoff;
    x2 = x1 + numrneurons*patchwidth;
    cpack(255);
    sboxi(x1,y1,x2-1,y2);

    x1 = x2;
    x2 = x1 + numgneurons*patchwidth;
    cpack(255<<8);
    sboxi(x1,y1,x2-1,y2);

    x1 = x2;
    x2 = x1 + numbneurons*patchwidth;
    cpack(255<<16);
    sboxi(x1,y1,x2,y2);

/*
    x2 = short(numneurons)*patchwidth + xoff;
    x1 = x2 - short(numoutneur)*patchwidth;
    cpack(0);
    sboxi(x1,y1,x2,y2);

    x1 = patchheight;
    x2 = x1 + patchheight;
    y2 = short(numnoninputneurons)*patchwidth + yoff;
    y1 = y2 - short(numoutneur)*patchwidth;
    sboxi(x1,y1,x2,y2);
*/

    cpack(0);
    x2 = numinputneurons*patchwidth + xoff;
    for (i = firstnoninputgroup; i < numneurgroups; i++)
    {
        x1 = x2;
        x2 = x1 + (mygenes->numneurons(i))*patchwidth;
        sboxi(x1,y1,x2,y2);
    }

    x1 = patchheight;
    x2 = x1 + patchheight;
    y2 = yoff;
    for (i = firstnoninputgroup; i < numneurgroups; i++)
    {
        y1 = y2;
        y2 = y1 + (mygenes->numneurons(i))*patchwidth;
        sboxi(x1,y1,x2,y2);
    }
    
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void brain::gdump2(cshort patchwidth, cshort patchheight)
{
    if ((neuron == NULL) || (synapse == NULL))
        return;
#ifdef DEBUGCALLS
    pushproc("brain::gdump2");
#endif DEBUGCALLS
    short i;
    short x1,y1,x2,y2;
    short xoff,yoff;
    long k;

    // the neuronactivation and newneuronactivation arrays have been
    // repointered by this time, so their contents are the reverse of
    // their names in this routine

    // this horizontal row of elements shows the new inputs and the states
    // at the previous time step used to calculate the new values

    y1 = patchheight;
    y2 = y1 + patchheight;
    for (i = 0, x1 = 2*patchheight; i < short(numneurons); i++, x1+=patchwidth)
    {
        // the following reference to "newneuron" really gets the old
        // values, except for the clamped input neuron values (which are new)
        register uchar mag = (uchar)(newneuronactivation[i] * 255.);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x1+patchwidth,y2);
    }

    // this vertical row of elements shows the biases (unfortunately the
    // new, updated values rather than those actually used to compute the
    // neuronal states which are adjacent) in all non-input neurons

    x1 = 0;
    x2 = patchheight;
    for (i = 0, y1 = 2*patchheight; i < numnoninputneurons;
         i++, y1 += patchwidth)
    {
        register uchar mag = (uchar)((maxweight+neuron[i].bias) * 127.5
                           / maxweight);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x2,y1+patchwidth);
    }

    // this vertical row of elements shows the new states in all the
    // unclamped neurons, including the output neurons

    x1 = x2;
    x2 = x1 + patchheight;
    for (i = short(firstnoninputneuron), y1 = 2*patchheight; i < numneurons;
         i++, y1 += patchwidth)
    {
        register uchar mag = (uchar)(neuronactivation[i] * 255.);
        cpack((mag<<16) | (mag<<8) | mag);
        sboxfi(x1,y1,x2,y1+patchwidth);
    }

    // this array of synaptic strengths unfortunately shows the new, updated
    // values rather than those actually used to map the displayed horizontal
    // row of neuronal values onto the displayed vertical row of values

    xoff = 2*patchheight;
    yoff = 2*patchheight;
    cpack((127<<16) | (127<<8) | 127);
    sboxfi(xoff,
           yoff,
           xoff+short(numneurons)*patchwidth,
           yoff+short(numnoninputneurons)*patchwidth);

    for (k = 0; k < numsynapses; k++)
    {
        register uchar mag = (uchar)((maxweight+synapse[k].efficacy) * 127.5
                           / maxweight);
        cpack((mag<<16) | (mag<<8) | mag);
        x1 = xoff + abs(synapse[k].fromneuron)*patchwidth;
        y1 = yoff + (abs(synapse[k].toneuron)-firstnoninputneuron)*patchwidth;
        sboxfi(x1,y1,x1+patchwidth,y1+patchwidth);
        cpack(0);
        sboxi(x1,y1,x1+patchwidth,y1+patchwidth);
    }

    // now highlight the input and output neurons for clarity

    y1 = patchheight;
    y2 = y1 + patchheight;

    x1 = short(redneuron)*patchwidth + xoff;
    x2 = x1 + numrneurons*patchwidth;
    cpack(255);
    sboxi(x1,y1,x2-1,y2);

    x1 = x2;
    x2 = x1 + numgneurons*patchwidth;
    cpack(255<<8);
    sboxi(x1,y1,x2-1,y2);

    x1 = x2;
    x2 = x1 + numbneurons*patchwidth;
    cpack(255<<16);
    sboxi(x1,y1,x2,y2);

/*
    x2 = short(numneurons)*patchwidth + xoff;
    x1 = x2 - short(numoutneur)*patchwidth;
    cpack(0);
    sboxi(x1,y1,x2,y2);

    x1 = patchheight;
    x2 = x1 + patchheight;
    y2 = short(numnoninputneurons)*patchwidth + yoff;
    y1 = y2 - short(numoutneur)*patchwidth;
    sboxi(x1,y1,x2,y2);
*/

    cpack(0x00ffffff);
    x2 = numinputneurons*patchwidth + xoff;
    for (i = firstnoninputgroup; i < numneurgroups; i++)
    {
        x1 = x2;
        x2 = x1 + (mygenes->numneurons(i))*patchwidth;
        sboxi(x1,y1,x2,y2);
    }

    x1 = patchheight;
    x2 = x1 + patchheight;
    y2 = yoff;
    for (i = firstnoninputgroup; i < numneurgroups; i++)
    {
        y1 = y2;
        y2 = y1 + (mygenes->numneurons(i))*patchwidth;
        sboxi(x1,y1,x2,y2);
    }
    
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}

void critterinit()
{
    if (critter::classinited)
        return;
#ifdef DEBUGCALLS
    pushproc("critterinit");
#endif DEBUGCALLS
    long i;
    critter::classinited = TRUE;
    critter::critterobj = new gpolyobj;
    "critter.obj" >> (*(critter::critterobj));
//  critter::critterobj.setname("critterobj");
    critter::critterlist = new indexlist(0,maxnumcritters-1);
    critter::pc = new critter*[maxnumcritters];
    for (i = 0; i < maxnumcritters; i++)
        critter::pc[i] = NULL;
    critter::povwindow = new gwindow("critter povwindow");
    critter::povwidth = short(0.75*xscreensize);
    critter::povheight = short((1./6.)*yscreensize);
    critter::povwindow->
        setprefposition(screenleft,
                        screenleft+critter::povwidth-1,
                        screenbottom+yscreensize-critter::povheight,
                        screenbottom+yscreensize-1);
    critter::povwindow->setdrawbuffer('b');
    critter::povwindow->setframewidth(0);
    critter::povwindow->setvisibility(showvision);
    if (graphics)
        critter::povwindow->open();

    retinawidth = max(minwin,maxvispixels);
    if (retinawidth & 1)
        retinawidth++;  // keep it even for efficiency (so I'm told)
    retinaheight = minwin;
    if (retinaheight & 1)
        retinaheight++;
    retinabuf = new uchar[retinawidth*4];
    if (retinabuf == NULL)
        error(2,"Insufficient memory to allocate retina buffer");
    else
    {
        if (mpin((char*)retinabuf,(unsigned int)(retinawidth*4)))
            error(1,"Unable to mpin the retina-buffer memory");
        for (i = 0; i < retinawidth*4; i++)
            retinabuf[i] = 0;
    }
    whitebuf = new long[retinawidth];
    if (whitebuf == NULL)
        error(2,"Insufficient memory to allocate white buffer");
    else
    {
        if (mpin((char*)whitebuf,(unsigned int)(retinawidth*4)))
            error(1,"Unable to mpin the white-buffer memory");
        for (i = 0; i < retinawidth; i++)
            whitebuf[i] = 0x00ffffff;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critterwindowrefresh()
{
    if (!critter::povwindow)
        return;
    critter::povwindow->makecurrentwindow();
    critter::povwindow->justviewport(0,short(critter::povwidth-1),
                                     0,short(critter::povheight-1));
    critter::povwindow->pop();
    critter::povwindow->doubleclearc();
}


void critterdestruct()
{
#ifdef DEBUGCALLS
    pushproc("critterdestruct");
#endif DEBUGCALLS
    if (critter::critterobj)
        delete critter::critterobj;
    if (critter::pc)
    {
        for (long i = 0; i < maxnumcritters; i++)
            if (critter::pc[i])
                delete critter::pc[i];
            else
                break;
        delete critter::pc;
    }
    if (critter::critterlist)
        delete critter::critterlist;
    if (critter::povwindow)
        delete critter::povwindow;
    if (retinabuf)
    {
        munpin((char*)retinabuf,(unsigned int)(retinawidth*4));
        delete retinabuf;
    }
    if (whitebuf)
    {
        munpin((char*)whitebuf,(unsigned int)(retinawidth*4));
        delete whitebuf;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


critter* getfreecritter()
{
#ifdef DEBUGCALLS
    pushproc("getfreecritter");
#endif DEBUGCALLS
    long i = critter::critterlist->getindex();
    if (i < 0)
    {
        error(1,"Unable to satisfy request for free critter (too many)");
#ifdef DEBUGCALLS
        popproc();
#endif DEBUGCALLS
        return NULL;
    }
    if (!critter::pc[i])
    {
        critter::pc[i] = new critter;
        if (critter::pc[i] == NULL)
        {
            error(2,"Insufficient memory for new critter");
#ifdef DEBUGCALLS
            popproc();
#endif DEBUGCALLS
            return NULL;
        }
    }
    else
        critter::pc[i]->reinit();
    critter::crittersliving++;
    critter::pc[i]->mycritternumber = ++critter::crittersever;
    critter::pc[i]->myindex = i;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
    return critter::pc[i];
}


void critterpovswap()
{
    if (showvision)
    {
        critter::povwindow->makecurrentwindow();
        critter::povwindow->justviewport(0,short(critter::povwidth-1),
                                         0,short(critter::povheight-1));
        critter::povwindow->justswapbuffers();
#ifdef RESTOREWINDOWS
        critter::povwindow->restorecurrentwindow();
#endif RESTOREWINDOWS
    }
}


void critterdump(ostream& out)
{
#ifdef DEBUGCALLS
    pushproc("critterdump");
#endif DEBUGCALLS
    out << critter::crittersever nl;
    out << critter::crittersliving nl;

    critter::critterlist->dump(out);

    for (register long i = 0; i < maxnumcritters; i++)
        if (!critter::pc[i])
            break;
    long numcrittersallocated = i;
    out << numcrittersallocated nl;
    for (i = 0; i < numcrittersallocated; i++)
        if (critter::critterlist->isone(i))
            (critter::pc[i])->dump(out);
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critterload(istream& in)
{
#ifdef DEBUGCALLS
    pushproc("critterload");
#endif DEBUGCALLS
    in >> critter::crittersever;
    in >> critter::crittersliving;

    critter::critterlist->load(in);

    for (register long i = 0; i < maxnumcritters; i++)
        if (!critter::pc[i])
            break;
    if (i)
        error(2,"critter::pc[] array not empty during critterload");
    if (xsortedcritters.count())
        error(2,"xsortedcritters list not empty during critterload");
    long numcrittersallocated = 0;
    in >> numcrittersallocated;
    for (i = 0; i < numcrittersallocated; i++)
    {
        critter::pc[i] = new critter;
        if (critter::pc[i] == NULL)
            error(2,"Insufficient memory for new critter in critterload");
        if (critter::critterlist->isone(i))
        {
            (critter::pc[i])->load(in);
            xsortedcritters.add(critter::pc[i]);
            worldstage.addobject(critter::pc[i]);
            if ((critter::pc[i])->myindex != i)
            {
                char msg[256];
                sprintf(msg,
                    "pc[i]->myindex (%d) does not match actual index (%d)",
                    (critter::pc[i])->myindex,i);
                error(2,msg);
            }
        }
        else
        {
            (critter::pc[i])->myindex = i;
        }
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::dump(ostream& out)
{
#ifdef DEBUGCALLS
    pushproc("critter::dump");
#endif DEBUGCALLS
    out << mycritternumber nl;
    out << myindex nl;
    out << myage nl;
    out << mylastmate nl;
    out << myenergy nl;
    out << myfoodenergy nl;
    out << mymaxenergy nl;
    out << myspeed2energy nl;
    out << myyaw2energy nl;
    out << mysizeadvantage nl;
    out << m nl;
    out << oldpos[0] sp oldpos[1] sp oldpos[2] nl;
    out << v[0] sp v[1] sp v[2] nl;
    out << nosecol[0] sp nosecol[1] sp nosecol[2] nl;
    out << myfitness nl;

    gobject::dump(out);

    pg->dump(out);
    if (pb)
        pb->dump(out);
    else
        error(1,"Attempted to dump a critter with no brain");
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::load(istream& in)
{
#ifdef DEBUGCALLS
    pushproc("critter::load");
#endif DEBUGCALLS

    in >> mycritternumber;
    in >> myindex;
    in >> myage;
    in >> mylastmate;
    in >> myenergy;
    in >> myfoodenergy;
    in >> mymaxenergy;
    in >> myspeed2energy;
    in >> myyaw2energy;
    in >> mysizeadvantage;
    in >> m;
    in >> oldpos[0] >> oldpos[1] >> oldpos[2];
    in >> v[0] >> v[1] >> v[2];
    in >> nosecol[0] >> nosecol[1] >> nosecol[2];
    in >> myfitness;

    gobject::load(in);

    pg->load(in);
    if (!pb)
    {
        pb = new brain;
        if (!pb)
           error(2,"Insufficient memory to allocate brain during critter load");
    }
    pb->load(in);

    // done loading in raw information, now setup some derived quantities:

    setgeom();
    setgraphics();
    mydomain = whichdomain(pos[0],pos[2],0);
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::grow()
{
#ifdef DEBUGCALLS
    pushproc("critter::grow");
#endif DEBUGCALLS

#ifdef DEBUGCHECK
    debugcheck("critter::grow entry");
#endif DEBUGCHECK

    if (pb == NULL)
    {
        pb = new brain;
        if (pb == NULL)
        {
            error(1,"Insufficient memory for critter to grow new brain");
#ifdef DEBUGCALLS
            popproc();
#endif DEBUGCALLS
            return;
        }
    }
    pb->grow(pg);  // grow the brain from the genome's specifications

    // setup the critter's geometry
    setgeom();

    col[0] = col[2] = 0.0;  // initially set red & blue to 0
    col[1] = pg->id();  // set green color by the "id"
    nosecol[0] = nosecol[1] = nosecol[2] = 0.5; // start neutral gray
    myage = 0;
    mylastmate = -initmatewait;
    mymaxenergy = minmaxenergy + ( (pg->size() - mincsize) *
                  (maxmaxenergy - minmaxenergy) / (maxcsize - mincsize) );
    myenergy = mymaxenergy;
    myfoodenergy = mymaxenergy;
    myspeed2energy = speed2energy * pg->maxspeed()
              * (pg->size()-mincsize) * (maxsizepenalty-1.)/(maxcsize-mincsize);
    myyaw2energy = yaw2energy * pg->maxspeed()
              * (pg->size()-mincsize) * (maxsizepenalty-1.)/(maxcsize-mincsize);
    mysizeadvantage = 1.0 + ( (pg->size() - mincsize) *
                (maxsizeadvantage - 1.0) / (maxcsize - mincsize) );

    // now setup the camera & window for our critter to see the world in
    setgraphics();

    myalive = TRUE;

#ifdef DEBUGCHECK
    debugcheck("critter::grow exit");
#endif DEBUGCHECK
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS

}


void critter::setgeom()
{
#ifdef DEBUGCALLS
    pushproc("critter::setgeom");
#endif DEBUGCALLS
    clonegeom(*critterobj); // obtain a fresh copy of the basic critter geometry
    // then adjust the geometry to fit size, speed, & critterheight
    lenx = pg->size() / sqrt(pg->maxspeed());
    lenz = pg->size() * sqrt(pg->maxspeed());
    for (long i = 0; i < numpolys; i++)
    {
        for (long j = 0; j < poly[i].numpts; j++)
        {
            poly[i].vert[j*3  ] *= lenx;
            poly[i].vert[j*3+1] *= critterheight;
            poly[i].vert[j*3+2] *= lenz;
        }
    }
    setlen();
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::setgraphics()
{
#ifdef DEBUGCALLS
    pushproc("critter::setgraphics");
#endif DEBUGCALLS
    // setup the camera & window for our critter to see the world in
//  float fovx = fov();
//  mycamera.setaspect(fovx*retinaheight/(critterfovy*retinawidth));
    mycamera.settranslation(0.0,0.0,-0.5*lenz);
    if (xleft < 0)  // not initialized yet
    {
        short maxrow = (povheight+2) / (retinaheight+2);
        short maxcol = (povwidth+2) / (retinawidth+2);
        short irow = short(myindex / maxcol)  +  1;
        if (irow > maxrow)
            irow = maxrow;
        ybottom = short(povheight - irow*retinaheight - (irow-1)*2);
        short icol = short(myindex) - (irow-1)*maxcol + 1;
        if (icol > maxcol)
            icol = maxcol;
        xleft = short((icol-1) * (retinawidth+2));
        mycamera.setnear(.01);
        mycamera.setfar(1.5*worldsize);
        mycamera.setfovy(critterfovy);
        mycamera.attachto(this);
        xright = short(xleft + retinawidth - 1);
        ytop = short(ybottom + retinaheight - 1);
        ypix = ybottom + retinaheight / 2  +  1;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


/*
void critter::setstage()
{
    mystage.clear();
    // zcellmin
    if (cos(angle[2]) > 0.)
    {
        if (cos(
    }
}
*/


void critter::dobrainmonitor()
{
#ifdef DEBUGCALLS
    pushproc("critter::dobrainmonitor");
#endif DEBUGCALLS

    if (monitorwindow)
    {
        monitorwindow->makecurrentwindow();
        reshapeviewport();
//      winpop();
        monitorwindow->clearc();
        rectzoom(float(patchwidth),float(patchheight));
#ifdef ATT_IRIX3.2
        lrectwrite(2*patchheight,0,2*patchheight+xright-xleft,0,
            (long *)retinabuf);
#else
        lrectwrite(2*patchheight,0,2*patchheight+xright-xleft,0,
            (const unsigned long [])retinabuf);
#endif
        rectzoom(1.0,1.0);
        if (alternatebraindisplay)
            pb->gdump(patchwidth,patchheight);
        else
            pb->gdump2(patchwidth,patchheight);
        monitorwindow->justswapbuffers();
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::behave()
{
#ifdef DEBUGCALLS
    pushproc("critter::behave");
#endif DEBUGCALLS
#ifdef TIMING
    timebeg(6);
#endif TIMING
    if (vision)
    {
        // create retinal pixmap, based on values of focus & numvisneurons
#ifdef TIMING
        timebeg(8);
        timebeg(14);
#endif TIMING
        float fovx = pb->focus()*(maxfocus-minfocus)+minfocus;
        myfrustum.set(pos[0],pos[2],angle[0],fovx,maxradius);
        mycamera.setaspect(fovx*retinaheight/(critterfovy*retinawidth));
//      setstage();
        povwindow->setscene(myscene);
        povwindow->makecurrentwindow();
        povwindow->justviewport(xleft,xright,ybottom,ytop);
#ifdef TIMING
        timeend(14);
#endif TIMING
        povwindow->drawnoswap(&myfrustum);
#ifdef TIMING
        timeend(8);
#endif TIMING

#ifdef DEBUGCHECK
        debugcheck("critter::behave after drawing pov");
#endif DEBUGCHECK

        if (retinabuf)
        {
#ifdef TIMING
            timebeg(9);
#endif TIMING
#ifdef RESTOREWINDOW
            povwindow->makecurrentwindow();
#endif RESTOREWINDOW
#ifdef PRINTBRAIN
            if (printbrain && (currentcritter == monitorcritter))
                printf("lrectread(%d,%d,%d,%d,%x)\n",
                    xleft,ypix,xright,ypix,retinabuf);
#endif PRINTBRAIN
#ifdef ATT_IRIX3.2
            long err = lrectread(xleft,ypix,xright,ypix,(long *)retinabuf);
#else
            long err = lrectread(xleft,ypix,xright,ypix,
                (unsigned long [])retinabuf);
#endif
            if (err == 0)
                error(1,"Error trying to read retinal pixmap");
#ifdef TIMING
                timeend(9);
#endif TIMING
#ifdef TIMING
                timebeg(10);
#endif TIMING
#ifdef RESTOREWINDOW
            povwindow->restorecurrentwindow();
#endif RESTOREWINDOW
#ifdef TIMING
            timeend(10);
#endif TIMING
        } // if (retinabuf)
    } // if (vision)
#ifdef TIMING
    timeend(6);
#endif TIMING

#ifdef DEBUGCHECK
    debugcheck("critter::behave after reading retinabuf");
#endif DEBUGCHECK

#ifdef TIMING
    timebeg(7);
#endif TIMING
    // now update the brain
    pb->update(myenergy/mymaxenergy);
#ifdef TIMING
    timeend(7);
#endif TIMING

#ifdef DEBUGCHECK
    debugcheck("critter::behave after pb->update");
#endif DEBUGCHECK

    if (monitorwindow &&
        ((::age == 1) || (::age == monstride*(::age/monstride))))
        dobrainmonitor();

#ifdef DEBUGCHECK
    debugcheck("critter::behave after dobrainmonitor");
#endif DEBUGCHECK

#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS

}


void critter::update()
{
#ifdef DEBUGCALLS
    pushproc("critter::update");
#endif DEBUGCALLS

#ifdef DEBUGCHECK
    debugcheck("critter::update entry");
#endif DEBUGCHECK

#ifdef PRINTBRAIN
    currentcritter = this;
#endif PRINTBRAIN

    behave(); // updates vision and brain

#ifdef DEBUGCHECK
    debugcheck("critter::update after behave()");
#endif DEBUGCHECK

// just do x & z dimensions in this version
    saveoldpos();
    float dpos = pb->speed() * pg->maxspeed() * speed2dpos;
    if (dpos > maxvel)
        dpos = maxvel;
    float dx = -dpos * sin(gobject::yaw()*DEGTORAD);
    float dz = -dpos * cos(gobject::yaw()*DEGTORAD);
    float dyaw = (2.0*pb->yaw() - 1.0) * pg->maxspeed() * yaw2dyaw;
    addx(dx);
    addz(dz);
    addyaw(dyaw);
    float energyused = pb->eat()   * eat2energy
                     + pb->mate()  * mate2energy
                     + pb->fight() * fight2energy
                     + pb->speed() * myspeed2energy
                     + pb->yaw()   * myyaw2energy
                     + pb->light() * light2energy
                     + pb->brainenergy()
                     + fixedenergydrain;
    float denergy = energyused * pg->strength();
    myenergy -= denergy;
    myfoodenergy -= denergy;
    foodenergyout += denergy;
    setr(pb->fight());  // set red color according to desire to fight
    setb(pb->mate());  // set blue color according to desire to mate
    nosecol[0] = nosecol[1] = nosecol[2] = pb->light();
    myage++;

// Hardwire knowledge of world boundaries for now
// Need to do something special with the xsortedcritters list,
// when the critters do a wraparound (for the sake of efficiency
// and possibly correctness in the sort)
    if (edges)
    {
        if (pos[0] > worldsize)
        {
            if (wraparound)
                pos[0] -= worldsize;
            else
                pos[0] = worldsize;
        }
        else if (pos[0] < 0.0)
        {
            if (wraparound)
                pos[0] += worldsize;
            else
                pos[0] = 0.0;
        }
        if (pos[2] < -worldsize)
        {
            if (wraparound)
                pos[2] += worldsize;
            else
                pos[2] = -worldsize;
        }
        else if (pos[2] > 0.0)
        {
            if (wraparound)
                pos[2] -= worldsize;
            else
                pos[2] = 0.0;
        }
    }

// Keep track of the domain in which the critter resides

    short newdomain = whichdomain(pos[0],pos[2],mydomain);
    if (newdomain != mydomain)
    {
        domains[ mydomain].numcritters--;
        domains[newdomain].numcritters++;
        mydomain = newdomain;
    }

#ifdef OF1
    if (mydomain == 0)
        myt0++;
#endif

// Also do boundary overrun testing here...
// apply a multiplicative Fudge Factor to keep critters from mating
// *across* the barriers
#define FF 1.01
    xsortedbarriers.reset();
    barrier* b = NULL;
    while (xsortedbarriers.next(b))
    {
        if ( (b->xmax() > ( x()-FF*radius())) ||
             (b->xmax() > (xo()-FF*radius())) )
        {
            // end of barrier comes after beginning of critter
            // in either its new or old position
            if ( (b->xmin() > ( x()+FF*radius())) &&
                 (b->xmin() > (xo()+FF*radius())) )
            {
                // beginning of barrier comes after end of critter,
                // in both new and old positions,
                // so there is no overlap, and we can stop searching
                // for this critter's possible barrier overlaps
                break;  // get out of the sorted barriers while loop
            }
            else // we have an overlap in x
            {
                if ( ((b->zmin() < ( z()+FF*radius())) ||
                      (b->zmin() < (zo()+FF*radius()))) &&
                     ((b->zmax() > ( z()-FF*radius())) ||
                      (b->zmax() > (zo()-FF*radius()))) )
                {
                    // also overlap in z, so there may be an intersection

                    float dist  = b->dist( x(), z());
                    float disto = b->dist(xo(),zo());
                    float p;
                    if (fabs(dist) < FF*radius())
                    {
                        // they actually overlap/intersect
                        if ((disto*dist) < 0.0)
                        {   // sign change, so crossed the barrier already
                            p = fabs(dist) + FF*radius();
                            if (disto < 0.0) p = -p;
                        }
                        else
                        {
                            p = FF*radius() - fabs(dist);
                            if (dist < 0.) p = -p;
                        }

                        addz( p*b->sina());
                        addx(-p*b->cosa());

                    } // actual intersection
                    else if ((disto*dist) < 0.0)
                    {
                        // the critter completely passed through the barrier
                        p = fabs(dist) + FF*radius();
                        if (disto < 0.0) p = -p;
                        addz( p*b->sina());
                        addx(-p*b->cosa());
                    }
                } // overlap in z
            } // beginning of barrier comes after end of critter
        } // end of barrier comes after beginning of critter
    } // while(xsortedbarriers.next(b))
/*
    xsortedbarriers.reset();
    while (xsortedbarriers.next(b)) {
        float dist = b->dist(x(),z());
        float disto = b->dist(xo(),zo());
        if ((disto*dist) < 0.0) {
            cout << "****Got one! moving from (" << xo() cm zo()
                 << ") to (" << x() cm z() pnlf;
            cout << "rad = " << radius() nlf;
        }
    }
*/
    v[0] = x() - xo();
    v[2] = z() - zo();

    rewardmovement();

#ifdef DEBUGCHECK
    debugcheck("critter::update exit");
#endif DEBUGCHECK

#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS

}


void critter::draw()
{
    pushmatrix();
      position();
      ::scale(scale,scale,scale);
      gpolyobj::drawcolpolyrange(0,4,nosecol);
      gpolyobj::drawcolpolyrange(5,9,col);
    popmatrix();
}


void critter::print()
{
    cout << "Printing critter #" << mycritternumber nl;
    gobject::print();
    cout << "  myage = " << myage nl;
    cout << "  mylastmate = " << mylastmate nl;
    cout << "  myenergy = " << myenergy nl;
    cout << "  myfoodenergy = " << myfoodenergy nl;
    cout << "  mymaxenergy = " << mymaxenergy nl;
    cout << "  myspeed2energy = " << myspeed2energy nl;
    cout << "  myyaw2energy = " << myyaw2energy nl;
    cout << "  mysizeadvantage = " << mysizeadvantage nl;
    cout << "  lenx = " << lenx nl;
    cout << "  lenz = " << lenz nl;
    cout << "  v[0], v[2] = " << v[0] cms v[2] nl;
    cout << "  myfitness = " << myfitness nl;
    if (pg)
    {
        cout << "  pg->lifespan() = " << pg->lifespan() nl;
        cout << "  pg->mutationrate() = " << pg->mutationrate() nl;
        cout << "  pg->strength() = " << pg->strength() nl;
        cout << "  pg->size() = " << pg->size() nl;
        cout << "  pg->maxspeed() = " << pg->maxspeed() nl;
    }
    else
        cout << "  genome is not yet defined" nl;
    if (pb)
    {
        cout << "  pb->brainenergy() = " << pb->brainenergy() nl;
        cout << "  pb->eat() = " << pb->eat() nl;
        cout << "  pb->mate() = " << pb->mate() nl;
        cout << "  pb->fight() = " << pb->fight() nl;
        cout << "  pb->speed() = " << pb->speed() nl;
        cout << "  pb->yaw() = " << pb->yaw() nl;
        cout << "  pb->light() = " << pb->light() nl;
    }
    else
        cout << "  brain is not yet defined" nl;
    cout.flush();
}


void critter::setupmonitor()
{
#ifdef DEBUGCALLS
    pushproc("critter::setupmonitor");
#endif DEBUGCALLS
    if (!monitorwindow)
    {
        monitorwindow = new gwindow("brain internals");
        monitorwindow->setborder(TRUE);
        monitorwindow->setframewidth(0);
        monitorwindow->setzbuffer(FALSE);
        monitorwindow->setdoublebuffer(TRUE);
        monitorwindow->setvisibility(TRUE);
    }
    short width = pb->numneurons*patchwidth + 2*patchheight;
    short height = pb->numnoninputneurons*patchwidth + 2*patchheight;
    monitorwindow->setprefposition(screenleft+xscreensize-10-width,
                                   screenleft+xscreensize-10,
                                   screenbottom+yscreensize-65-height,
                                   screenbottom+yscreensize-65);
    if (!monitorwindow->isopen())
        monitorwindow->open();
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::beginbrainmonitoring(cshort pw, cshort ph)
{
#ifdef DEBUGCALLS
    pushproc("critter::beginbrainmonitoring");
#endif DEBUGCALLS
#ifdef PRINTBRAIN
    monitorcritter = this;
#endif PRINTBRAIN
    povwindow->makecurrentwindow();
    povwindow->justviewport(0,short(povwidth-1),
                            0,short(povheight-1));
    short savelwidth = short(getlwidth());
    linewidth(3);
    if (povwindow->doublebuffered) frontbuffer(TRUE);
    cpack(0x00ffffff);
    sboxi(xleft-3,ybottom-3,xright+3,ytop+3);
    if (povwindow->doublebuffered) frontbuffer(FALSE);
    linewidth(savelwidth);
    patchwidth = pw;
    patchheight = ph;
    setupmonitor();
//  monitorwindow->makecurrentwindow();
//  monitorwindow->pop();
//  monitorwindow->doubleclearc();
    dobrainmonitor();
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void critter::endbrainmonitoring()
{
#ifdef DEBUGCALLS
    pushproc("critter::endbrainmonitoring");
#endif DEBUGCALLS
    if (monitorwindow)
    {
//      monitorwindow->makecurrentwindow();
//      monitorwindow->pop();
//      monitorwindow->doubleclearc();
        short savelwidth = short(getlwidth());
        linewidth(3);
        if (povwindow->doublebuffered) frontbuffer(TRUE);
        cpack(0);
        sboxi(xleft-3,ybottom-3,xright+3,ytop+3);
        if (povwindow->doublebuffered) frontbuffer(FALSE);
        linewidth(savelwidth);
        delete monitorwindow;
        monitorwindow = NULL;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


implement(gdlink,pcritter)
implement(gdlist,pcritter)


void cxsortedlist::add(pcritter a)
{
#ifdef DEBUGCALLS
    pushproc("cxsortedlist::add");
#endif DEBUGCALLS
    Boolean inserted = FALSE;
    pcritter o;
    this->reset();
    while (this->next(o))
    {
        if ((a->x()-a->radius()) < (o->x()-o->radius()))
        {
            this->inserthere(a);
            inserted = TRUE;
            break;
        }
    }
    if (!inserted)
    {
        this->append(a);
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void cxsortedlist::sort()
{
#ifdef DEBUGCALLS
    pushproc("cxsortedlist::sort");
#endif DEBUGCALLS
// This technique assumes that the list is almost entirely sorted at the start
// Hopefully, with some reasonable frame-to-frame coherency, this will be true!
    gdlink(pcritter) *savecurr;
    pcritter o = NULL;
    pcritter p = NULL;
    pcritter b = NULL;
    this->reset();
    this->next(p);
    savecurr = curr;
    while (this->next(o))
    {
        if ((o->x()-o->radius()) < (p->x()-p->radius()))
        {
            gdlink(pcritter)* link = this->unlink();  // at o, just unlink
            curr = savecurr;  // back up to previous one directly
            while (this->prev(b)) // then use iterator to move back again
                if ((b->x()-b->radius()) < (o->x()-o->radius()))
                    break; // until we have one that starts before o
            if (curr)  // we did find one, and didn't run off beginning of list
                this->appendhere(link);
            else  // we have a new head of the list
                this->insert(link);
            curr = savecurr;
            o = p;
        }
        p = o;
        savecurr = curr;
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void cxsortedlist::list()
{
#ifdef DEBUGCALLS
    pushproc("cxsortedlist::list");
#endif DEBUGCALLS
    gdlink(pcritter) *savecurr;
    critter* pcrit;
    savecurr = curr;
    this->reset();
    while(this->next(pcrit))
        cout sp pcrit->number();
    cout nlf;
    curr = savecurr;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


// end of critter.C

#endif CRITTER_C
