#ifndef FOOD_H
#define FOOD_H
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// food.h - declaration of food classes


extern float maxfoodenergy;
extern float minfoodenergy;
extern float size2energy;
extern float foodheight;
extern colf foodcolor;
extern float worldsize;


class food : public gboxf
{
protected:
    float myenergy;
    float mydomain;
    void initfood()
    {
        myenergy = drand48() * (maxfoodenergy - minfoodenergy) + minfoodenergy;
        initlen();
        initpos();
        initrest();
    }
    void initfood(cfloat e) { myenergy = e; initlen(); initpos(); initrest(); }
    void initfood(cfloat e, cfloat x, cfloat z)
    {
        myenergy = e;
        initlen();
        pos[0] = x; pos[1] = 0.5*len[1]; pos[2] = z;
        initrest();
    }
    void initpos()
    {
        pos[0] = drand48() * worldsize;
        pos[1] = 0.5 * len[1];
        pos[2] = drand48() * -worldsize;
    }
    void initlen()
    {
        float lxz = 0.75 * myenergy / size2energy;
        float ly = foodheight;
        setlen(lxz,ly,lxz);
    }
    void initrest()
    {
        setcolor(foodcolor);
    }
    virtual void setradius()
    {
        if (!radiusfixed)  //  only set radius anew if not set manually
            rad = sqrt(len[0]*len[0] + len[2]*len[2])
                * radscale * scale * 0.5;
    }
public:
    food() { initfood(); }
    food(cfloat e) { initfood(e); }
    food(cfloat e, cfloat x, cfloat z) { initfood(e,x,z); }
    ~food() { }
    void dump(ostream& out);
    void load(istream& in);
    float eat(cfloat e)
    {
        float er = e < myenergy ? e : myenergy;
        myenergy -= er;
        initlen();
        return er;
    }
    float energy() { return myenergy; }
    short domain() { return mydomain; }
    void domain(short id) { mydomain = id; }
};


typedef food* pfood;

declare(gdlink,pfood)
declare(gdlist,pfood)


class fxsortedlist : public gdlist(pfood)
{
public:
    fxsortedlist() { }
    ~fxsortedlist() { }
    void add(pfood a);
    void sort();
};


// end of food.h

#endif FOOD_H
