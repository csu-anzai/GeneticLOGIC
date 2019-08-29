#ifndef ERROR_C
#define ERROR_C
/********************************************************************/
/* PolyWorld:  An Artificial Life Ecological Simulator              */
/* by Larry Yaeger                                                  */
/* Copyright Apple Computer 1990,1991,1992                          */
/********************************************************************/

// error.C: implementation of standard error reporting mechanism

#include "basicincludes.h"
#include "error.h"
#include "graphics.h"

#define LOOPSPERSECOND 5000

char errorstring[256];


extern Boolean graphics;


Boolean exitnow;

void mywait(cint errwait)
{
   long dev;
   short data;

   qdevice(LEFTMOUSE);
   qdevice(KEYBD);

   if (errwait < 0)
   {
      while (((dev = qread(&data)) != LEFTMOUSE) && (dev != KEYBD))
         { qenter(short(dev),data); }
      if ((dev == KEYBD) && (data == 27)) // ESCape
          exitnow = TRUE;
   }
   else
   {
      long waitlength = ((errwait == 0) ? 5 : errwait) * LOOPSPERSECOND;
      long waited = 0;
      Boolean done = FALSE;
      while ((!done) && (waited++ < waitlength))
      {
         if (qtest())
         {
            dev = qread(&data);
            if ((dev == LEFTMOUSE) || (dev == KEYBD))
            {
               done = TRUE;
               if ((dev == KEYBD) && (data == 27)) // ESCape
                   exitnow = TRUE;
            }
            else
                qenter(short(dev),data);
         }
      }
   }
}

void error(cint errlevel, cint errwait, cchar* errmsg)
{
// errlevel = 0 ==> warning only
//            1 ==> error, but no abort
//            2 ==> error, abort
// errwait >= 0 ==> sleep(errwait) [0 defaults to 5]
//          < 0 ==> wait for LEFTMOUSE or KEYBD
   extern const char progname[];
   exitnow = (errlevel > 1);
   char errmsg2[256];
   strcpy(errmsg2,progname);
   if (errlevel)
      strcat(errmsg2," ERROR: ");
   else
      strcat(errmsg2," WARNING: ");
   strcat(errmsg2,errmsg);
   strcat(errmsg2,"\n");
   cerr << errmsg2;
   if (graphics)
   {
       gwindow errwin;
       errwin.setprefposition(319,959,511,767);
       errwin.setborder(TRUE);
       errwin.setframewidth(0);
       errwin.setdoublebuffer(FALSE);
       errwin.setvisibility(TRUE);
       errwin.settitle("Error message");
       errwin.open();
       errwin.dumbtext(errmsg2);
       mywait(errwait);
       errwin.clearc();
       errwin.close();
   }
   cerr.flush();
   if (exitnow) exit(errlevel);
}


void error(     cint level, cchar* msg) { error(level,-1,msg); }


void errorwait( cint level, cchar* msg) { error(level,-1,msg); }


void errorflash(cint level, cchar* msg) { error(level, 5,msg); }


void error(     cint level, cchar* msg1, cchar* msg2)
{
    sprintf(errorstring,"%s%s\0",msg1,msg2);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3)
{
    sprintf(errorstring,"%s%s%s\0",msg1,msg2,msg3);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3, cchar* msg4)
{
    sprintf(errorstring,"%s%s%s%s\0",msg1,msg2,msg3,msg4);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3, cchar* msg4,
                            cchar* msg5)
{
    sprintf(errorstring,"%s%s%s%s%s\0",msg1,msg2,msg3,msg4,msg5);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3, cchar* msg4,
                            cchar* msg5, cchar* msg6)
{
    sprintf(errorstring,"%s%s%s%s%s%s\0",msg1,msg2,msg3,msg4,msg5,msg6);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, cchar* msg3, cchar* msg4,
                            cchar* msg5, cchar* msg6, cchar* msg7)
{
    sprintf(errorstring,"%s%s%s%s%s%s%s\0",msg1,msg2,msg3,msg4,msg5,msg6,msg7);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, clong num2, cchar* msg3)
{
    sprintf(errorstring,"%s%d%s\0",msg1,num2,msg3);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, clong num2, cchar* msg3, cchar* msg4,
                            cchar* msg5)
{
    sprintf(errorstring,"%s%d%s%s%s\0",msg1,num2,msg3,msg4,msg5);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, clong num2, cchar* msg3, clong  num4,
                            cchar* msg5)
{
    sprintf(errorstring,"%s%d%s%d%s\0",msg1,num2,msg3,num4,msg5);
    error(level,-1,errorstring);
}


void error(     cint level, cchar* msg1, cchar* msg2, clong  num3, cchar* msg4,
                            clong  num5, cchar* msg6, clong  num7)
{
    sprintf(errorstring,"%s%s%d%s%d%s%d\0",msg1,msg2,num3,msg4,num5,msg6,num7);
    error(level,-1,errorstring);
}


// end of error.C

#endif ERROR_C
