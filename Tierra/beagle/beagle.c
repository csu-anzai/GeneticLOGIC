/* beagle.c  22-3-92  Exploratory program for the Tierra Simulator */
/*** Beagle Explorer: Version 3.1  Copyright (c) 1990, 1991, 1992  Tom Ray ***/
/* beagle uses Greenleaf Datawindows to handle the user interface */
/* compiled with Borland Turbo C V2.0 */

#include "beagle.h"
#include "declareb.h"

int    firstop = 1, resminmax;
float  ds = 1;
char   dv[4] = "scr";
char   directory[80] = "d:\\tiedat\\ex01", info_file[53];
/* char   directory[80] = "c:\\tierra\\td", info_file[53]; */
unsigned  _stklen = 60000;

struct last_out  lo;

TRANSACTION  *ptse;
HWND         whse;
MENUHDR      *mmain;
MENUITEM     *mbar, *mtrac, *mdiv, *mtemp, *mprob, *mprep, *quit, *select;

void helpfn(MENUITEM  *), writehelp(char  *);

void main(void)
{   static int   first = 1;
/*  char  path[46] = "e:/tc/bgi"; */
    char  path[46] = "c:/tc/bgi";

    if(first)
    {   first = resminmax = xywin.n = cad = hp = 0;
        scr = xywin.f = 1;
        wind_setup();
     /* if(scr) grwisetup(path); */
        if(scr) grinit(path);
    }
    if(resminmax) { xywin.f = 1; xywin.n = resminmax = 0; }
    mmain = MNUCreateHdr(POPUP);
    mmain->toprow = 5;
    mmain->topcol = 15;
/*
    mmain->uattr  = 8;
    mmain->dattr  = 16;
    mmain->lattr  = 13;
*/
    mbar = MNUAddItem("Bars",      "running size distribution      ",'B',0,
        NULL,mmain,bars);
    mtrac = MNUAddItem("Trace",    "x,y-trajectory of two sizes    ",'T',0,
        NULL,mmain,trace);
    mtrac = MNUAddItem("Diversity","diversity, age, time indices   ",'D',0,
        NULL,mmain,diverse);
    mtemp = MNUAddItem("Template", "find templates in creatures    ",'T',0,
        NULL,mmain,template);
    mprob = MNUAddItem("Probe",    "probe one genome with another  ",'P',0,
        NULL,mmain,probe);
    mprep = MNUAddItem("Prepare",  "prepare output for bar or trace",'P',0,
        NULL,mmain,NULLF);
    quit = MNUAddItem("Quit",      "exit from program              ",'Q',ALTQ,
        NULL,mmain,NULLF);
    MNUAddItem("Run Info","make run_info file  ", 'R',0,mprep,mmain,gtierunc);
    MNUAddItem("Fragment","make fragment of run", 'F',0,mprep,mmain,fragment);
    MNUSetGlobalHelp(mmain,helpfn);
    do  { select = MNUDoSelect(mmain,NULLF); }
        while(select != quit);
    MNUDeleteMenu(mmain);
    fclose(ouf);
    if(scr) { closegraph(); vexit(0); }
    vexit(0);
}

void helpfn(MENUITEM  *item)
{   char  *ret, lib[6];

    if(item == mbar)
        strcpy(lib,"Bars");
    else if(item == mtrac)
        strcpy(lib,"Trace");
    ret = ExtractFromHelpFile("beagle.hlp",lib);
    if(ret)
        writehelp(ret);
}

void writehelp(char  *string)
{   HWND  wihe;

    wihe = vcreat(6,30,HELP,YES);
    vlocate(wihe,15,25);
    wordwrap(wihe,ON);
    vframe(wihe,REVHELP,FRSINGLE);
    visible(wihe,YES,YES);
    vdispstr(wihe,string);
    vdispstr(wihe,"\nPress Any Key...");
    getkey();
    vdelete(wihe,NONE);
}
