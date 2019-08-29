/* ======================================================================*/
/* tturbo.c 9-9-92 support for Turbo C frontend for
   Tierra Simulator V4.0: Copyright (c) 1992 Dan Pirone & Virtual Life
   written by Daniel pirone
   v 1.0
*/
/* ======================================================================*/
#include <conio.h>
#include <graphics.h>
/* ======================================================================*/

int  DispText = 11, DispBack = 2, InfoText = 5, InfoBack = 3;

void FEExit(ENO)
I32s ENO;
{   sleep(1); textmode(C80);
/*  textattr(15); */
    textbackground(0);
    textcolor(15);
    clrscr(); exit(ENO);
}

/* ======================================================================*/
#define FECeol() clreol()
/* ======================================================================*/

int FEGetc()
{
return getch();
}

void FEMemCheck(msg)
I8s  *msg;
{
    FEPrintf(1, 11, 0, "%s\n", msg);
    FEPrintf(1, 12, 0, "%ld,%ld\n", InstExe.m, InstExe.i);
        if (strncmp(msg,"Out of Memory", 13));
    {   FEPrintf(1, 13, 0, "Hit any key to continue...\n");
        FEGetc();
    }
}

/* ======================================================================*/
/* called immediately after the soup_in file is read */
/* an opportunity to interactively set soup_in vars */
void FEStartup()
{   FILE  *inf;
    I8s tt, first[12], second[12];
    I16s  gmode, color;

#if __TURBOC__ >= 0x200         /* deal with older compilers */
FE_DV_Mode = DETECT;
detectgraph(&FE_DV_Mode,&gmode );
#endif

    if(inf = fopen("tcolors.cfg","r"))
    {   fgets(Buff, 84, inf);
        do
        {   if (*Buff != '#' && strlen(Buff) > 3)
            {   sscanf(Buff, "%11s%11s%d", first, second, &color);
                if (!strcmp(first,"information"))
                {   if (!strcmp(second,"text:"))
                        InfoText = color;
                    else if (!strcmp(second,"background:"))
                        InfoBack = color;
                }
                else if (!strcmp(first,"display"))
                {   if (!strcmp(second,"text:"))
                        DispText = color;
                    else if (!strcmp(second,"background:"))
                        DispBack = color;
                }
            }
        } while (fgets(Buff, 84, inf) != NULL);
        fclose(inf);
    }

fe_width = 57;
IMode = PLN_STATS;
PLN_X = 1;
PLN_Y = 3;
MSG_X = 1;
MSG_Y = PLN_Y;
ERR_X = 1;
HLP_X = 1;

textmode(C80);   
/* textattr(43); */
    textbackground(DispBack);
    textcolor(DispText);
clrscr();

/* Title screen */
gotoxy(35,5);
cprintf("TIERRA");
gotoxy(34,7);
cprintf("July  92");
gotoxy(29,9);
cprintf("Artificial Life System");
gotoxy(37,11);
cprintf("By");
gotoxy(31,13);
cprintf("Dr. Thomas S. Ray");
gotoxy(20,15);
cprintf("Daniel Pirone, Tom Uffner & Marc Cygnus");

sleep(3);
FE_DosVideoToggle(0);
FEPrintf(HLP_X,HLP_Y,1,
"                     Tierra initializing, please wait ...                  "
);

}
/* ======================================================================*/
void FEPrintf(I32s scr_x, I32s scr_y, I32s scr_a,...)
{
    va_list ap;


    I8s *msg_str,
        buf[80];
    I8s CF = 0;
    va_start(ap,scr_a);

    if((scr_x < 1) || (scr_x > 80) || (scr_y < 1) || (scr_y > HLP_Y))
       {sound(100); return ; }
    if (scr_a)
    {   textbackground(InfoBack);
        textcolor(InfoText);
    /*  textattr(53); */
    }

    if (*(msg_str = va_arg(ap, I8s *)) != 0)
       {
       vsprintf(buf,msg_str,ap);
       if (buf[strlen(buf)-1] == '\n') {buf[strlen(buf)-1]='\0';CF=1;}
       gotoxy(scr_x,scr_y);
       cprintf(buf);
       if (CF) clreol();
       }
    va_end(ap);
    if (scr_a)
    {   textbackground(DispBack);
        textcolor(DispText);
    /*  textattr(43); */
    }

}

/* ======================================================================*/
void FEStats()
{ if (GeneBnker)
  {  FEPrintf(1,1,1,
     "InstExec  = %ld,%6.6ld  Cells = %4ld  Genotypes = %4ld  Sizes =%4ld ",
         InstExe.m,InstExe.i,NumCells, NumGenotypes, NumSizes);
     FEPrintf(1,2,1, 
       "Extracted = %-20.20s                                                ",
       ExtrG);
  }
  else /* (GeneBnker) */
  {  FEPrintf(1,1,1,
     "InstExec  = %ld,%6.6ld  Cells = %4ld ", InstExe.m,InstExe.i,NumCells);
  }
}
/* ======================================================================*/

/*-----------------------------------------------------------------------*/

void FEMenu()
{
   I8s  answer;
   I32s tsz;
   I8s  data[85];

#ifdef ALCOMM
   AL_run_flag = 1;
#endif
   while(1)
      {
      FEPrintf(HLP_X,HLP_Y,1,
"TIERRA |  i-info  v-var  s-save  q-save&quit  Q-quit  m-misc c-continue |->"
      );
      answer = getch();

      if (answer == 'c')
          {
          break;
          }
      if (answer == 'v')
      { IMode = 69;
         FE_DosVideoToggle(0);
         FEPrintf(MSG_X,MSG_Y,0,
                  "To alter any global variable from soup_in, type\n");
         FEPrintf(MSG_X,MSG_Y+1,0,
                  "the variable name (using proper case), a space,\n");
         FEPrintf(MSG_X,MSG_Y+2,0,
                  "an equal sign, a space, and the new value.\n");
         FEPrintf(MSG_X,MSG_Y+3,0,
                  "Use no space at start of line.  Some examples:\n");
         FEPrintf(MSG_X,MSG_Y+4,0, "alive = 0\n");
         FEPrintf(MSG_X,MSG_Y+5,0, "DistProp = .6\n");
         FEPrintf(MSG_X,MSG_Y+6,0,"GenebankPath = newpath/\n");
         FEClrmsg(MSG_Y+7);
         gotoxy(HLP_X,HLP_Y-2);
         if ((gets(data)) == NULL) break;
/* DAN get a string */
    if (!GetAVar(data))
        FEError(-1600,NOEXIT,NOWRITE,
            "Tierra FEMenu() Not a valid soup_in variable: %s", data);
        IMode = PLN_STATS;
        ToggleLog(0L);
       }

      if (answer == 'i')
         {
#ifdef ERROR
         FEPrintf(1,2,1,
                  "Coreleft  = %lu TotMemUse  = %6ld  FreeMemCurrent = %6ld"
                  ,(unsigned long) coreleft(), TotMemUse, FreeMemCurrent);
#else
         FEPrintf(1,2,1,
                  "Coreleft  = %lu FreeBlocks = %6ld  FreeMemCurrent = %6ld"
                  ,(unsigned long) coreleft(), FreeBlocks, FreeMemCurrent);
#endif
         FEPrintf(HLP_X,HLP_Y,1,
"INFO   | p-plan  s-size_histo g-gen_histo m-mem_histo z-size_query      |->" 
);
         answer = getch();
         if (answer == 'z')
            {
            IMode = 69;
            FE_DosVideoToggle(FE_DV_Mode);
            FEClrmsg(MSG_Y);
            sprintf(mes[0],
            "Enter a size class ( eg: 80 ) to examine -> ");
            FEMessage(1,mes);
            gotoxy(HLP_X,HLP_Y-2);
         if ((gets(data)) == NULL) break;
            sscanf(data,"%d", &tsz);
            FEClrmsg(MSG_Y);
            if (Hist != NULL)
            {  thfree(Hist);
               Hist = NULL;
            }
            query_size((I16u)tsz);
            FEPrintf(HLP_X,HLP_Y,1,
"INFO   | z-size_query       Press any key to continue ...               |->" 
);
            getch();
            IMode = PLN_STATS;
            FE_DosVideoToggle(0);
            }

         if (answer == 'p')
            {
            IMode = 69;
            FE_DosVideoToggle(0);
            FEClrmsg(MSG_Y);
            sprintf(mes[0],
             "Now in Plan Display mode, updated every million time steps \n");
            FEMessage(1,mes);
            IMode = PLN_STATS;
            if (Hist != NULL)
              {
	      thfree(Hist);
              Hist = NULL;
              }
            }
         if (answer == 's')
            {
            if(GeneBnker)
               {
               IMode = SIZ_HIST;
               FE_DosVideoToggle(FE_DV_Mode);
               query_species(fe_lines);
               }
            }
         if (answer == 'm')
            {
            if(GeneBnker)
               {
               IMode = SIZM_HIST;
               FE_DosVideoToggle(FE_DV_Mode);
               query_species(fe_lines);
               }
            }
         if (answer == 'g')
            {  
            if(GeneBnker) 
               {
               IMode = GEN_HIST;
               FE_DosVideoToggle(FE_DV_Mode);
               query_species(fe_lines);
               }
            }
         answer = ' ';
       } 
      if (answer == 's')
         {  
         FEError(-1601,NOEXIT,WRITE," ");
         }
      if (answer == 'q')
         {
         FEError(-1602,EXIT,WRITE," ");
         }
      if (answer == 'm')
          {
          IMode = 69;
          FE_DosVideoToggle(0);
          FEClrmsg(MSG_Y);
          FEPrintf(1,2,1,"VER=%1.2f INST=%d PLOIDY=%d  %s  %s  %s\n",
                   VER,INST,PLOIDY,
#ifdef ERROR
"ERROR",
#else
"",
#endif
#ifdef MEM_PROF
"MEM_PROF",
#else
"",
#endif
#ifdef MICRO
"MICRO"
#else
""
#endif
          );

          FEPrintf(HLP_X,HLP_Y,1,
"MISC |  H-Histo Logging  S-DOS Shell  I-Inject Gene  %s  |->\n",

#ifdef MICRO
          "M-Micro Toggle"
#else
          ""
#endif
         );
         gotoxy(HLP_X+79,HLP_Y);
         answer = getch();
         if (answer == 'H')
            {
            HistPrint = (Log && !HistPrint)? 1 : 0;
            sprintf(mes[0],
             "%s \n",(Log)? (HistPrint)? "Logging Histograms \n":
                                         "NOT Logging Histograms \n" :
                           "Log NOT on ! \n");
            FEMessage(1,mes);
            }
#ifdef MICRO
         if (answer == 'M')
             {
             if(MC_step == -1L) MC_step = 0L;
             else if(MC_step == 0L) MC_step = 1L;
             else {MC_step =-1L; if(tfp_log != NULL) Log = 1L;}
             sprintf(mes[0]," MICRO STEP Mode = %s\n",
             (MC_step == -1)?"off": (MC_step == 1)?"keypress":"delay");
             FEMessage(1,mes);
             }
#endif /* MICRO */

          if (answer == 'S')
             {
             FEPrintf(HLP_X,HLP_Y,1,
             "TIERRA: shell (%s), type exit to return ... ",SHELL);
             system(SHELL);
             FE_DosVideoToggle(0);
             }
        if (answer == 'I')
             {
             sprintf(mes[0],"INJECT GENE TO RUNNING SIMULATION\n");
             sprintf(mes[1],"Enter gene name ( eg 0080aaa) or \n");
             sprintf(mes[2],"Enter abort to cancel\n");
             FEMessage(3,mes);
             FEPrintf(HLP_X,HLP_Y-2,0," ");
             fgets(data,84,stdin);
             if ((data[0] >= '0') && (data[0] <= '9') )
		{
                InjectFromBank(data, -1, 0);
		}
	     else
  	        {
                FEError(-13667,NOEXIT,NOWRITE,"User abort of Injection!"); 
		}
             }
  
          answer = ' ';
          IMode = PLN_STATS;
          }     /* end of misc */

      if (answer == 'Q') FEExit(-1);
      }         /* end while loop */
FEPrintf(HLP_X,HLP_Y,1,
"                         Press Any Key for menu ...                        "
);
}

/*-----------------------------------------------------------------------*/

/* ======================================================================*/
void FEClrmsg(n)
I32s n;
{
I8s t;
if(n <0) return;
for(t= n;  t < fe_lines; t++)
    {
    gotoxy(MSG_X,t);
    clreol();
    }

}
/* ======================================================================*/
void FE_DosVideoToggle(mode)
I16s mode;
{
int tt;

#if __TURBOC__ >= 0x200         /* deal with older compilers */
if (mode == EGA)
   {
   textmode(C4350); /* use C4350 for ega/vga */
   fe_lines = 42;
   ERR_Y = 42;
   HLP_Y = 43;
   }
else
if (mode == VGA)
   {
   textmode(C4350);
   fe_lines = 49;
   ERR_Y = 49;
   HLP_Y = 50;
   }
else
#endif /* __TURBOC__ >= 0x200 */
   {
   textmode(C80);   
   fe_lines = 24;
   ERR_Y = 24;
   HLP_Y = 25;
   }
/*  textattr(43); */
    textbackground(DispBack);
    textcolor(DispText);
clrscr();

/*  textattr(53); */
    textbackground(InfoBack);
    textcolor(InfoText);
for(tt = 1; tt < 81; tt++)
   {
   gotoxy(tt,1);
   cprintf(" ");
   gotoxy(tt,2);
   cprintf(" ");
   gotoxy(tt,ERR_Y);
   cprintf(" ");
   gotoxy(tt,HLP_Y);
   cprintf(" ");
   }
/*  textattr(43); */
    textbackground(DispBack);
    textcolor(DispText);
FEStats();
}
/* ======================================================================*/
/* ======================================================================*/
