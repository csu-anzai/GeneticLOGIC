// interact.C - this routine handles all critter interactions and world physics,
//              including collisions, matings, fights, deaths, and births
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

//#define TEXTTRACE

#include "basicincludes.h"
#include "graphics.h"
#include "error.h"
#include "critter.h"
#include "barrier.h"
#include "pw.h"

#include "debug.h"

extern domainstruct domains[MAXDOMAINS];

extern short whichdomain(float x, float z, short d);

//extern gdlist(pcritter)** critterlistarray;
//extern gdlist(pfood)** foodlistarray;
//extern gdlist(pbarrier)** barrierlistarray;
//extern short numxcells;
//extern short numcells;
//extern float cellsize;

//#include "externglobals.h" // done in critter.h

extern cxsortedlist xsortedcritters;
extern fxsortedlist xsortedfood;
extern bxsortedlist xsortedbarriers;

extern critter* curfittestcrit[5];
extern float curmaxfitness[5];
extern short numfit;
extern genome** fittest;
extern float* fitness;
extern Boolean crittersRfood;
#ifdef OF1
extern float deathprob;
#endif
extern short ifit;
extern short jfit;


long newdeaths;
long numglobalcreated = 0;


void death(critter* c,short id)
{
#ifdef DEBUGCALLS
    pushproc("death");
#endif DEBUGCALLS
#ifdef TEXTTRACE
    cout << "age " << age << ": critter #" << c->number() << " died" nlf;
#endif TEXTTRACE
    newdeaths++;
    numdied++;
    domains[id].numdied++;
    domains[id].numcritters--;
    c->lastrewards(); // if any
    if ( crittersRfood && (xsortedfood.count() < maxfoodcount) &&
            (domains[id].foodcount < domains[id].maxfoodcount) &&
         (edges || ((c->x() >= 0.0) && (c->x() <=  worldsize) &&
                    (c->z() <= 0.0) && (c->z() >= -worldsize))) )
    {
//      float foodenergy = c->size() * size2energy * c->energy()/c->maxenergy();
//      if (foodenergy < minfoodenergy) foodenergy = minfoodenergy;
        float foodenergy = c->foodenergy();
        if (foodenergy < minfoodenergyatdeath)
        {
            if (minfoodenergyatdeath >= minfoodenergy)
                foodenergyin += minfoodenergyatdeath - foodenergy;
            else
                foodenergyout += foodenergy;
            foodenergy = minfoodenergyatdeath;
        }
        if (foodenergy >= minfoodenergy)
        {
            food* f = new food(foodenergy,c->x(),c->z());
            if (!f)
                error(1,"Not enough memory to replace dead critter with food");
            else
            {
                xsortedfood.add(f);  // dead critter becomes food
                worldstage.addobject(f);  // put replacement food into the world
                domains[id].foodcount++;
                f->domain(id);
            }
        }
    }
    else
    {
        foodenergyout += c->foodenergy();
    }
    // following assumes (requires!) list to be currently pointing to c,
    // and will leave the list pointing to the previous critter
    xsortedcritters.remove(); // get critter out of the list
    worldstage.removeobject(c);  // get critter out of the world
    long newfit;
    genome* savegenome;
    if (c->fitness() > domains[id].fitness[numfit-1])
    {
        for (short i = 0; i < numfit; i++)
            if (c->fitness() > domains[id].fitness[i])
                { newfit = i; break; }

        savegenome = domains[id].fittest[numfit-1];
        for (i = numfit-1; i > newfit; i--)
        {
            domains[id].fitness[i] = domains[id].fitness[i-1];
            domains[id].fittest[i] = domains[id].fittest[i-1];
        }
        domains[id].fitness[newfit] = c->fitness();
        domains[id].fittest[newfit] = savegenome;
        domains[id].fittest[newfit]->copygenes(c->genes());
    }
    if (c->fitness() > fitness[numfit-1])
    {
        for (short i = 0; i < numfit; i++)
            if (c->fitness() > fitness[i])
                { newfit = i; break; }

        savegenome = fittest[numfit-1];
        for (i = numfit-1; i > newfit; i--)
        {
            fitness[i] = fitness[i-1];
            fittest[i] = fittest[i-1];
        }
        fitness[newfit] = c->fitness();
        fittest[newfit] = savegenome;
        fittest[newfit]->copygenes(c->genes());
    }
    if (c->fitness() > maxfitness)
        maxfitness = c->fitness();
    c->die();  // free up this critter slot
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void geneseprecord()
{
    fprintf(genesepfile,"%d %g %g %g\n",age,genesepmax,genesepmin,genesepavg);
}


void genesepcalc(const critter* ci)
{
#ifdef DEBUGCALLS
    pushproc("genesepcalc");
#endif DEBUGCALLS
    // NOTE: This version assumes ci is *not* currently in xsortedcritters.
    // It also marks the current position in the list on entry and returns
    // there before exit so it can be invoked during the insertion of the
    // newcritters list into the existing xsortedcritters list.
    xsortedcritters.mark();
    critter* cj = NULL;
    register float genesep;
    float genesepsum = 0.0;
    long numgsvalsold = numgsvals;
    xsortedcritters.reset();
    while(xsortedcritters.next(cj))
    {
        genesep = ci->genes()->sepcalc(cj->genes());
        genesepmax = fmax(genesep,genesepmax);
        genesepmin = fmin(genesep,genesepmin);
        genesepsum += genesep;
        gsvals[numgsvals++] = genesep;
    }
    register long n = xsortedcritters.count();
    if (numgsvalsold != (n*(n-1)/2))
    {
        sprintf(tempstring,"%s%s%d%s%d%s%d\0",
            "genesepcalc: numgsvalsold not equal to n*(n-1)/2.\n",
            "  numgsvals, n, n*(n-1)/2 = ",
            numgsvalsold,", ",n,", ",n*(n-1)/2);
        error(2,tempstring);
    }
    if (numgsvals != (n*(n+1)/2))
    {
        sprintf(tempstring,"%s%s%d%s%d%s%d\0",
            "genesepcalc: numgsvals not equal to n*(n+1)/2.\n",
            "  numgsvals, n, n*(n+1)/2 = ",
            numgsvals,", ",n,", ",n*(n+1)/2);
        error(2,tempstring);
    }
    genesepavg = (genesepsum + genesepavg*numgsvalsold) / numgsvals;
    xsortedcritters.tomark();
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void genesepcalcall()
{
#ifdef DEBUGCALLS
    pushproc("genesepcalcall");
#endif DEBUGCALLS
    critter* ci = NULL;
    critter* cj = NULL;
    register float genesep;
    float genesepsum = 0.0;
    genesepmin = 1.e+10;
    genesepmax = 0.0;
    numgsvals = 0;
    xsortedcritters.reset();
    while (xsortedcritters.next(ci))
    {
        xsortedcritters.mark();
        while (xsortedcritters.next(cj))
        {
            genesep = ci->genes()->sepcalc(cj->genes());
            genesepmax = max(genesep,genesepmax);
            genesepmin = min(genesep,genesepmin);
            genesepsum += genesep;
            gsvals[numgsvals++] = genesep;
        }
        xsortedcritters.tomark();
    }
    // n*(n-1)/2 is how many calculations were made
    register long n = xsortedcritters.count();
    if (numgsvals != (n*(n-1)/2))
    {
        sprintf(tempstring,"%s%s%d%s%d%s%d\0",
            "genesepcalcall: numgsvals not equal to n*(n-1)/2.\n",
            "  numgsvals, n, n*(n-1)/2 = ",
            numgsvals,", ",n,", ",n*(n-1)/2);
        error(2,tempstring);
    }
    genesepavg = genesepsum / numgsvals;
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}


void smiteone(short id, short smite)
{
/*
    if (id < 0)  // smite one anywhere in the world
    {
        xsortedcritters.reset();
        xsortedcritters.next(c);
        smitten = c;
        while (xsortedcritters.next(c))
        {
            if (c->age() > minsmiteage)
            {
                case (smite)
                1:  // fitness rate
            }
        }
    }
    else  // smite one in domain id
    {
    }
*/
}


void ijfitinc(short* i, short* j)
{
    (*j)++;
    if ((*j) == (*i))
        (*j)++;
    if ((*j) > numfit-1)
    {
        (*j) = 0;
        (*i)++;
        if ((*i) > numfit-1)
            (*i) = min(1,numfit-1);
    }
}


void interact()
{
#ifdef DEBUGCALLS
    pushproc("interact");
#endif DEBUGCALLS
    critter* c = NULL;
    critter* d = NULL;
    food* f = NULL;
//  barrier* b = NULL;
    float cpower;
    float dpower;
    Boolean cdied;
    Boolean foodmarked;
//  Boolean barriermarked;
    cxsortedlist newcritters;
    register long i;
    register long j;
    short id = 0;
    short jd;
    short kd;
    short fd;

    newdeaths = 0;

// first x-sort all the critters

    xsortedcritters.sort();

// food list should be x-sorted already
// and so should the barrier list

// now go through the list, and use the influence radius to determine
// all possible interactions

    xsortedcritters.reset();
    xsortedfood.reset();
//  xsortedbarriers.reset();
    for (i = 0; i < 5; i++)
    {
        curmaxfitness[i] = 0.0;
        curfittestcrit[i] = NULL;
    }
    avgfitness = 0.0;

    while (xsortedcritters.next(c))
    {
        // determine the domain in which the critter currently is located

        id = c->domain();

/* Do the barrier tests in critter.C now

// if anyone has tried to overrun a barrier, catch it and push them back

        xsortedbarriers.marklast(); // mark last in case no x-overlaps found
        barriermarked = FALSE;
        while (xsortedbarriers.next(b))
        {

            if ( b->xmax() > (c->x()-c->radius()) )
            {
                // end of barrier comes after beginning of critter
                if (!barriermarked) // first time only for each critter
                {
                    barriermarked = TRUE;
                    xsortedbarriers.markprev();  // mark previous item in list
                }
                if ( b->xmin() > (c->x()+c->radius()) )
                {
                    // beginning of barrier comes after end of critter,
                    // so there is no overlap, and we can stop searching
                    // for this critter's possible barrier overlaps

                    // first, set the sorted barrier list back to the marked
                    // item which immediately preceeds the first possible
                    // barrier for this, and therefore the next, critter
                    // actually, due to "tomark" at end of loop, don't need this
                    // xsortedbarriers.tomark();
                    break;  // then get out of the sorted barriers while loop
                }
                else // we have an overlap in x
                {
                    if ( (b->zmin() < (c->z()+c->radius())) &&
                         (b->zmax() > (c->z()-c->radius())) )
                    {
                        // also overlap in z, so there may be an intersection

                        float dist = b->dist(c->x(),c->z());
                        if (fabs(dist) < c->radius())
                        {
                            // they actually overlap/intersect
#ifdef TEXTTRACE
                            cout << "age " << age
                                 << ": critter #" << c->number()
                                 << " collided with a barrier" nlf;
#endif TEXTTRACE
                            float p;
                            float disto = b->dist(c->xo(),c->zo());
                            if ((disto*dist) < 0.0)
                            {   // sign change, so crossed the barrier already
                                p = fabs(dist) + c->radius();
                                if (disto < 0.0) p *= -1.;
                            }
                            else
                            {
                                p = c->radius() - fabs(dist);
                                if (dist < 0.) p *= -1.;
                            }

                            c->addz( p*b->sina());
                            c->addx(-p*b->cosa());

                            // unlike the food overlap test, we want to check
                            // for overlap with all possible barriers,
                            // so do NOT set the sorted barrier list back to
                            // the marked item, and do NOT break out of the
                            // while loop (these things are done only when
                            // a barrier is found that is entirely after
                            // the current critter (see above)
                        } // actual intersection
                    } // overlap in z
                } // beginning of barrier comes after end of critter
            } // end of barrier comes after beginning of critter
        } // while loop on barriers
        xsortedbarriers.tomark(); // either first x-overlap found, or last one

        c->rewardmovement();
*/

// now take care of deaths

        if ( (c->energy() <= 0.0) || (c->age() >= c->maxage()) ||
             ((!edges) && ((c->x() < 0.0) || (c->x() >  worldsize) ||
                           (c->z() > 0.0) || (c->z() < -worldsize))))
        {
            if (c->age() >= c->maxage())
                numdiedage++;
            else if (c->energy() <= 0.0)
                numdiedenergy++;
            else
                numdiededge++;
            death(c,id);
            continue; // nothing else to do for this poor schmo
        }

#ifdef OF1
        if ( (id == 0) && (drand48() < deathprob) )
        {
            death(c,id);
            continue;
        }
#endif

#ifdef DEBUGCHECK
        debugcheck("after a death in interact");
#endif DEBUGCHECK

// now see if there's an overlap with any other critters

        xsortedcritters.mark(); // so can point back to this critter later
        cdied = FALSE;
        jd = id;
        kd = id;

        while (xsortedcritters.next(d)) // to end of list or...
        {
            if ( (d->x()-d->radius()) >= (c->x()+c->radius()) )
                break;  // this guy (& everybody else in list) is too far away

            // so if we get here, then c & d are close enough in x to interact

            if ( fabs(d->z()-c->z()) < (d->radius()+c->radius()) )
            {
                // and if we get here then they are also close enough in z,
                // so must actually worry about their interaction

#ifdef TEXTTRACE
                cout << "age " << age
                     << ": critters #" << c->number() << " & #"
                                       << d->number() << " close" nlf;
#endif TEXTTRACE

                jd = d->domain();

// now take care of mating

#ifdef OF1
                if ( (c->mate() > matethreshold) &&
                     (d->mate() > matethreshold) &&          // it takes two!
                     ((c->age()-c->lastmate()) >= matewait) &&
                     ((d->age()-d->lastmate()) >= matewait) &&  // and some time
                     (c->energy() > minmatefrac*c->maxenergy()) &&
                     (d->energy() > minmatefrac*d->maxenergy()) && // and energy
                     (kd == 1) && (jd == 1) ) // in the safe domain
#else
                if ( (c->mate() > matethreshold) &&
                     (d->mate() > matethreshold) &&          // it takes two!
                     ((c->age()-c->lastmate()) >= matewait) &&
                     ((d->age()-d->lastmate()) >= matewait) &&  // and some time
                     (c->energy() > minmatefrac*c->maxenergy()) &&
                     (d->energy() > minmatefrac*d->maxenergy()) ) // and energy
#endif
                {
                    kd = whichdomain(0.5*(c->x()+d->x()),
                                     0.5*(c->z()+d->z()),
                                     kd);

                    if (smite)
                    {
                        if (domains[kd].numcritters ==
                            domains[kd].maxnumcritters)
                        {
                            smiteone(kd,smite);
                        }
                        else if ((xsortedcritters.count()+newcritters.count())
                                  >= maxnumcritters)
                        {
                            smiteone(-1,smite);
                        }
                    }

                    if ( (domains[kd].numcritters < domains[kd].maxnumcritters)
                        && ((xsortedcritters.count()+newcritters.count())
                            < maxnumcritters) )
                    {
                        if ( (domains[kd].numbornsincecreated < misccritters) ||
                             (drand48() < c->mateprob(d)) )
                        {
#ifdef TEXTTRACE
                            cout << "age " << age << ": critters #"
                                 << c->number() << " & #"
                                 << d->number() << " mating" nlf;
#endif TEXTTRACE
                            numbornsincecreated++;
                            domains[kd].numbornsincecreated++;
                            critter* e = getfreecritter();
                            e->genes()->crossover(c->genes(),d->genes(),TRUE);
                            e->grow();
                            float eenergy = c->mating() + d->mating();
                            e->energy(eenergy);
                            e->foodenergy(eenergy);
                            e->settranslation(0.5*(c->x()+d->x()),
                                              0.5*(c->y()+d->y()),
                                              0.5*(c->z()+d->z()));
                            e->setyaw(360.0*drand48());
                            e->domain(kd);
                            worldstage.addobject(e);
                            newcritters.add(e); // add it to the full list later
                            domains[kd].numcritters++;
                            numborn++;
                            domains[kd].numborn++;
#ifdef TEXTTRACE
                            cout << "age " << age
                                 << ": critter #" << e->number() << " born" nlf;
#endif TEXTTRACE
                        }
                        else
                            miscnobirth++;
                    }
                }

#ifdef DEBUGCHECK
                debugcheck("after a birth in interact");
#endif DEBUGCHECK

// now take care of fighting

                if (power2energy > 0.0)
                {
                    if ( (c->fight() > fightthreshold) )
                        cpower = c->strength() * c->sizeadvantage()
                               * c->fight() * (c->energy()/c->maxenergy());
                    else
                        cpower = 0.0;

                    if ( (d->fight() > fightthreshold) )
                        dpower = d->strength() * d->sizeadvantage()
                               * d->fight() * (d->energy()/d->maxenergy());
                    else
                        dpower = 0.0;

                    if ( (cpower > 0.0) || (dpower > 0.0) )
                    {
#ifdef TEXTTRACE
                        cout << "age " << age
                             << ": critters #" << c->number() << " & #"
                                              << d->number() << " fighting" nlf;
#endif TEXTTRACE
                        // somebody wants to fight
                        numfights++;
                        c->damage(dpower * power2energy);
                        d->damage(cpower * power2energy);
                        if (d->energy() <= 0.0)
                        {
                            death(d,jd);
                            numdiedfight++;
                        }
                        if (c->energy() <= 0.0)
                        {
                            xsortedcritters.tomark(); // point back to c
                            death(c,id);
                            numdiedfight++;
                            // note: this leaves list pointing to item before c
                            xsortedcritters.mark();
                            cdied = TRUE;
                            break;
                        }
                    }
                }

#ifdef DEBUGCHECK
                debugcheck("after fighting in interact");
#endif DEBUGCHECK

            }  // if close enough
        }  // while (xsortedcritters.next(d))

#ifdef DEBUGCHECK
        debugcheck("after all critter interactions in interact");
#endif DEBUGCHECK

        if (cdied) continue; // nothing else to do with c, it's gone!
        xsortedcritters.tomark(); // point critter list back to c

// they finally get to eat (couldn't earlier to keep from conferring
// a special advantage on critters early in the sorted list)

        fd = id;
        foodmarked = FALSE;
        while (xsortedfood.next(f))
        {
            if ( (f->x()+f->radius()) > (c->x()-c->radius()) )
            {
                // end of food comes after beginning of critter
                if (!foodmarked) // first time only
                {
                    foodmarked = TRUE;
                    xsortedfood.markprev();   // mark previous item in list
                }
                if ( (f->x()-f->radius()) > (c->x()+c->radius()) )
                {
                    // beginning of food comes after end of critter,
                    // so there is no overlap, and we can stop searching
                    // for this critter's possible foods

                    // first, set the sorted food list back to the marked
                    // item which immediately preceeds the first possible
                    // food item for this, and therefore the next, critter
                    xsortedfood.tomark();
                    break;  // then get out of the sorted food while loop
                }
                else // we actually have an overlap in x
                {
                    if ( fabs(f->z()-c->z()) < (f->radius()+c->radius()) )
                    {
                        // also overlap in z, so they really interact
#ifdef TEXTTRACE
                        cout << "age " << age
                             << ": critter #" << c->number() << " eating" nlf;
#endif TEXTTRACE
                        fd = f->domain();
                        c->eat(f);
                        if (f->energy() <= 0.0)  // all gone
                        {
                           domains[fd].foodcount--;
                           xsortedfood.remove();  // get it out of the list
                           worldstage.removeobject(f); //get it out of the world
                           delete f;  // get it out of memory
                        }
                        // whether this food item is completely used up or not,
                        // must set the sorted food list back to the marked
                        // item which immediately preceeded f, so either f,
                        // or the next item in the list, will be available
                        // to the next critter
                        xsortedfood.tomark();

                        // but this guy only gets to eat from one food source
                        break;  // so get out of the sorted food while loop
                    }
                }
            }
        } // while loop on food

#ifdef DEBUGCHECK
        debugcheck("after eating in interact");
#endif DEBUGCHECK

// now keep tabs of current and average fitness for surviving organisms

        avgfitness += c->fitness();
        if (c->fitness() > curmaxfitness[4])
        {
            for (i = 0; i < 5; i++)
            {
                if (c->fitness() > curmaxfitness[i])
                {
                    for (j = 4; j > i; j--)
                    {
                        curmaxfitness[j] = curmaxfitness[j-1];
                        curfittestcrit[j] = curfittestcrit[j-1];
                    }
                    curmaxfitness[i] = c->fitness();
                    curfittestcrit[i] = c;
                    break;
                }
            }
        }

    } // while loop on critters

    avgfitness /= xsortedcritters.count();

    if (genesepmon && (newdeaths > 0))
    {
        genesepcalcall();
    }

// now for a little spontaneous generation!

    if ((xsortedcritters.count()+newcritters.count()) < maxnumcritters)
    {
        // provided there are less than the maximum number of critters already

// first deal with the individual domains

        for (id = 0; id < numdomains; id++)
        {
            while (domains[id].numcritters < domains[id].minnumcritters)
            {
                numcreated++;
                domains[id].numcreated++;
                numbornsincecreated = 0;
                domains[id].numbornsincecreated = 0;
                lastcreate = age;
                domains[id].lastcreate = age;
                c = getfreecritter();
                if ( numfit && (domains[id].numdied >= numfit) )
                {
                    // the list exists and is full
                    if ( fit1freq &&
                        ((domains[id].numcreated/fit1freq)*fit1freq ==
                          domains[id].numcreated) )
                    {
                        // revive 1 fittest
                        c->genes()->copygenes(domains[id].fittest[0]);
                        numcreated1fit++;
                    }
                    else if ( fit2freq &&
                        ((domains[id].numcreated/fit2freq)*fit2freq ==
                          domains[id].numcreated) )
                    {
                        // mate 2 from array of fittest
                        c->genes()->crossover(
                            domains[id].fittest[domains[id].ifit],
                            domains[id].fittest[domains[id].jfit],
                            TRUE);
                        numcreated2fit++;
                        ijfitinc(&(domains[id].ifit),&(domains[id].jfit));
                    }
                    else
                    {
                        // otherwise, just generate a random, hopeful monster
                        c->genes()->randomize();
                        numcreatedrandom++;
                    }
                }
                else
                {
                    // otherwise, just generate a random, hopeful monster
                    c->genes()->randomize();
                    numcreatedrandom++;
                }

                c->grow();
                foodenergyin += c->foodenergy();
                c->settranslation(drand48()*domains[id].xsize+domains[id].xleft,
                                  0.5 * critterheight,
                                  drand48() * -worldsize);
                c->setyaw(drand48() * 360.0);
                c->domain(id);
                worldstage.addobject(c);
                domains[id].numcritters++;
                newcritters.add(c); // add it to the full list later

            } // while loop until we have enough critters in this domain
        } // for loop on numdomains

#ifdef DEBUGCHECK
        debugcheck("after domain creations in interact");
#endif DEBUGCHECK

// then deal with global creation if necessary

        while ( (xsortedcritters.count()+newcritters.count()) < minnumcritters )
        {
            numcreated++;
            numglobalcreated++;
            if ( (numglobalcreated == 1) && (numdomains > 1) )
                errorflash(0,
                  "Possible global influence on domains due to minnumcritters");
            numbornsincecreated = 0;
            lastcreate = age;

            c = getfreecritter();

            if ((numfit) && (numdied >= numfit))
            {
                // the list exists and is full
                if ( fit1freq &&
                    ((numglobalcreated/fit1freq)*fit1freq ==
                      numglobalcreated) )
                {
                    // revive 1 fittest
                    c->genes()->copygenes(fittest[0]);
                    numcreated1fit++;
                }
                else if ( fit2freq &&
                    ((numglobalcreated/fit2freq)*fit2freq == numglobalcreated) )
                {
                    // mate 2 from array of fittest
                    c->genes()->crossover(fittest[ifit],
                                          fittest[jfit], TRUE);
                    numcreated2fit++;
                    ijfitinc(&ifit,&jfit);
                }
                else
                {
                    // otherwise, just generate a random, hopeful monster
                    c->genes()->randomize();
                    numcreatedrandom++;
                }
            }
            else
            {
                // otherwise, just generate a random, hopeful monster
                c->genes()->randomize();
                numcreatedrandom++;
            }

            c->grow();
            foodenergyin += c->foodenergy();
            c->settranslation(drand48() * worldsize,
                              0.5 * critterheight,
                              drand48() * -worldsize);
            c->setyaw(drand48() * 360.0);
            id = whichdomain(c->x(),c->z(),0);
            c->domain(id);
            domains[id].numcreated++;
            domains[id].lastcreate = age;
            domains[id].numcritters++;
            worldstage.addobject(c);
            newcritters.add(c); // add it to the full list later
        } // while loop handling global creations

#ifdef DEBUGCHECK
        debugcheck("after global creations in interact");
#endif DEBUGCHECK

    } // if there are less than the maximum number of critters already

// now add the new critters to the existing list

    long newlifes = newcritters.count();
    if (newcritters.count() > 0)
    {
        Boolean foundinsertionpt;
        Boolean oldlistfinished = FALSE;
        xsortedcritters.reset();
        newcritters.reset();
        while (newcritters.next(c))
        {
            if (genesepmon)
                genesepcalc(c);

            if (oldlistfinished)
                xsortedcritters.append(c);
            else
            {
                foundinsertionpt = FALSE;
                while (xsortedcritters.next(d))
                {
                    if ( (c->x()-c->radius()) < (d->x()-d->radius()) )
                    {
                        xsortedcritters.inserthere(c);
                        foundinsertionpt = TRUE;
                        break;
                    }
                }
                if (!foundinsertionpt)
                {
                    oldlistfinished = TRUE;
                    xsortedcritters.append(c);
                }
            }
        }
        newcritters.clear();
    }

#ifdef DEBUGCHECK
    debugcheck("after newcritters added to xsortedcritters in interact");
#endif DEBUGCHECK

    if ((newlifes || newdeaths) && genesepmon)
    {
        if (geneseprec)
            geneseprecord();
        if (graphics && binchartgs)
            gswin->addpoint(gsvals,numgsvals);
    }

// finally, keep the world's food supply going...

// first deal with the individual domains

    if (xsortedfood.count() < maxfoodcount) // can't create if too many overall
    {
        for (fd = 0; fd < numdomains; fd++)
        {
            if (domains[fd].foodcount < domains[fd].maxfoodgrown)
            {
                float foodprob = (domains[fd].maxfoodgrown
                                - domains[fd].foodcount) * foodrate;
                if (drand48() < foodprob)
                {
                    f = new food;
                    foodenergyin += f->energy();
                    f->setx(drand48()*domains[fd].xsize + domains[fd].xleft);
                    f->domain(fd);
                    xsortedfood.add(f);
                    worldstage.addobject(f);
                    domains[fd].foodcount++;
                }
                long newfood = domains[fd].minfoodcount - domains[fd].foodcount;
                for (i = 0; i < newfood; i++)
                {
                    f = new food;
                    foodenergyin += f->energy();
                    f->setx(drand48()*domains[fd].xsize + domains[fd].xleft);
                    f->domain(fd);
                    xsortedfood.add(f);
                    worldstage.addobject(f);
                    domains[fd].foodcount++;
                }
            }
#ifdef DEBUGCHECK
            debugcheck("after domain food growth in interact");
#endif DEBUGCHECK
        }


// then deal with the global food supply if necessary

        if (xsortedfood.count() < maxfoodgrown) // can get higher due to deaths
        {
            float foodprob = (maxfoodgrown - xsortedfood.count()) * foodrate;
            if (drand48() < foodprob)
            {
                f = new food;
                foodenergyin += f->energy();
                fd = whichdomain(f->x(),f->z(),0);
                f->domain(fd);
                xsortedfood.add(f);
                worldstage.addobject(f);
                domains[fd].foodcount++;
            }
            long newfood = minfoodcount - xsortedfood.count();
            for (i = 0; i < newfood; i++)
            {
                f = new food;
                foodenergyin += f->energy();
                fd = whichdomain(f->x(),f->z(),0);
                f->domain(fd);
                xsortedfood.add(f);
                worldstage.addobject(f);
                domains[fd].foodcount++;
            }
#ifdef DEBUGCHECK
            debugcheck("after global food growth in interact");
#endif DEBUGCHECK
        }
    }
#ifdef DEBUGCALLS
    popproc();
#endif DEBUGCALLS
}
