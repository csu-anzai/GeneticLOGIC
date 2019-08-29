
/* ======================================================================*/
/* tcurses.c 9-9-92 support for unix curses frontend for 
   Tierra simulation,
   Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life
   written by Daniel pirone
   v 1.0
*/
/* ======================================================================*/
void FEExit(ENO)
I32s ENO;
{sleep(1);endwin();exit(ENO);}
/* ======================================================================*/
void FECeol()
{
clrtoeol();
}
/* ======================================================================*/
I16s FEGetc()
{
return getch();
}
/* ======================================================================*/
void FEStartup()
{
/* called immediately after the soup_in file is read */
/* an opportunity to interactively set soup_in vars */
I8s tt;

initscr();

if ((LINES < 20) || (COLS < 80))
   {
   fprintf(stderr,"\nTIERRA: not enough space on screen ... \n");
   FEExit(-666);
   }

clear();
/*
 set up keyboard
 */
nonl(); cbreak(); noecho(); 

/* scrollok(); */

fe_width = COLS - 23;
fe_lines = LINES -2 ;
IMode = PLN_STATS;
PLN_X = 0;
PLN_Y = 2;
MSG_X = 0;
MSG_Y = PLN_Y;
ERR_X = 0;
ERR_Y = LINES -2;
HLP_X = 0;
HLP_Y = LINES -1;

/* Title screen */
printw("\n\n\t\t\t\t      TIERRA\n");
printw("\n\n\t\t\t\t     July  92\n");
printw("\n\t\t\t     Artificial Life System\n\t\t\t\t\tBy\n");
printw("\t\t\t\tDr. Thomas S. Ray\n\t\t\t\t\t&\n");
printw("\t\t    Daniel Pirone, Tom Uffner & Marc Cygnus\n\n");
refresh();
sleep(0); 
clear();
/* nice stats banner stuff */
standout();
for(tt = 0; tt < COLS-1; tt++)
   {
   mvaddch(0,tt,' ');
   mvaddch(1,tt,' ');
   mvaddch(ERR_Y,tt,' ');
   mvaddch(LINES-1,tt,' ');
   }
standend();
/* FEPrintf(HLP_X+15,HLP_Y,1,"Press Interupt Key for menu ..."); */
FEPrintf(HLP_X+15,HLP_Y,1,"Tierra initializing, please wait ...");
refresh();

}
/* ======================================================================*/
#ifdef __GNUC__
void FEPrintf(I32s scr_x, I32s scr_y, I32s scr_a, ... )
#else
void FEPrintf(scr_x, scr_y, scr_a, va_alist )
I32s scr_x, scr_y, scr_a;
va_dcl
#endif /*__GNUC__ */
{
    va_list ap;

    I8s *msg_str,
        buf[80];
    if((scr_x < 0) || (scr_x > COLS) || (scr_y < 0) || (scr_y > LINES))
       {fprintf(stderr,"\0x07"); return ; }
    if (scr_a) standout();

#ifdef __GNUC__
    va_start(ap,acr_a);
#else
    va_start(ap);
#endif /*__GNUC__*/
    if ((msg_str = va_arg(ap, I8s *)) != NULL)
       {
       vsprintf(buf,msg_str,ap);
       mvprintw(scr_y,scr_x,buf);
       }
    va_end(ap);
    if (scr_a) standend();
    refresh();

}
/* ======================================================================*/
void FEStats()
{ if (GeneBnker)
  { FEPrintf(0,0,1,
    "InstExec  = %ld,%6.6ld  Cells = %4ld  Genotypes = %4ld  Sizes =%4ld ",
        InstExe.m,InstExe.i,NumCells, NumGenotypes, NumSizes);
    FEPrintf(0,1,1, 
      "Extracted = %-20.20s                                                ",
      ExtrG);
  }
  else /* (GeneBnker) */
  { FEPrintf(0,0,1, "InstExec  = %ld,%6.6ld  Cells = %4ld ",
        InstExe.m,InstExe.i,NumCells);
  }
}
/* ======================================================================*/

/*-----------------------------------------------------------------------*/
void T_sig_int(sig,code,scp,addr)
    I32s  sig,code;
    /* struct sigcontext *scp; */
    I32s *scp; /* DO NOT USE !!!!! */
    I8s  *addr;
{
TC_Menu =1;
}
/*-----------------------------------------------------------------------*/

void FEMenu ()
{
   I8s  answer;
   I32s tsz;
   I8s  data[85];

   signal(2,T_sig_int);
   touchwin(stdscr);
   refresh();
   while(1)
      {
      FEPrintf(HLP_X,HLP_Y,1,
"TIERRA |  i-info  v-var  s-save  q-save&quit  Q-quit  m-misc c-continue |->"
      );

      answer = mvgetch(HLP_Y,HLP_X+31);

      if (answer == 'c') 
          {
          FEPrintf(ERR_X,ERR_Y,1,"\t\t\t\t\t\t\t\t\t\t");
          break;
          }

      if (answer == 'v')
         {  
	 IMode = 69;
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
         echo(); nl();nocbreak();
         FEPrintf(HLP_X,HLP_Y-1,0," ");
         clrtoeol();
         fgets(data,84,stdin);
         /* getstr(data); */
         cbreak();noecho(); nonl();
         if (!GetAVar(data))
             FEError(-900,NOEXIT,NOWRITE,
                 "Tierra T_sig_int() Not a valid soup_in variable: %s", data);
         IMode = PLN_STATS;
         ToggleLog(0L);
         }
   
      if (answer == 'i')
         {

         FEPrintf(0,1,1,
#ifdef ALCOMM
"Port = %5ld FreeBlocks = %6ld  FreeMemCurrent = %6ld seed = %ld   ",
          VPORT,FreeBlocks, FreeMemCurrent,seed);
#else
"FreeBlocks = %6ld  FreeMemCurrent = %6ld seed = %ld   ",
          FreeBlocks, FreeMemCurrent,seed);
#endif
         FEPrintf(HLP_X,HLP_Y,1,
"INFO   |  p-plan  s-size_histo g-gen_histo m-mem_histo z-size_query     |->"
	 );
         answer = mvgetch(HLP_Y,HLP_X+31);

         if (answer == 'z')
            {
            IMode = 69;
            FEClrmsg(MSG_Y);
            sprintf(mes[0],
            "Enter a size class ( eg: 80 ) to examine -> ");
            FEMessage(1,mes);
            FEPrintf(HLP_X,HLP_Y-2,0," ");
            echo(); nl();nocbreak();
            fgets(data,84,stdin);
            /* getstr(data); */
            cbreak();noecho(); nonl();
            sscanf(data,"%d", &tsz);
            if (Hist != NULL)
            {   thfree(Hist);
                Hist=NULL;
            }
            query_size((I16u)tsz);
            IMode = PLN_STATS;
            }
         if (answer == 'p')
            {  
            IMode = 69;
            sprintf(mes[0],
            "Now in Plan Display mode, updated every million time steps \n");
            FEMessage(1,mes);
            if (Hist != NULL)
            {   thfree(Hist);
                Hist=NULL;
            }
            IMode = PLN_STATS;
            }
         if (answer == 's')
            {  
            if(GeneBnker) 
               {
               IMode = SIZ_HIST;
               query_species(fe_lines);
               }
            }
         if (answer == 'm')
            {  
            if(GeneBnker) 
               {
               IMode = SIZM_HIST;
               query_species(fe_lines);
               }
            }
         if (answer == 'g')
            {  
            if(GeneBnker) 
               {
               IMode = GEN_HIST;
               query_species(fe_lines);
               }
            }
         answer = ' ';
       } 
      if (answer == 's')
         {  
         FEError(-901,NOEXIT,WRITE," ");
         }
      if (answer == 'q')
         {  
         FEError(-902,EXIT,WRITE," ");
         }
      if (answer == 'Q') FEExit(-1);
      if (answer == 'm') 
          {
	  IMode = 69;
          FEClrmsg(MSG_Y);
          FEPrintf(0,1,1,"VER=%1.2f INST=%d PLOIDY=%d  %s %s %s %s\n",
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
"MICRO",
#else
"",
#endif
#ifdef ALCOMM
"ALCOMM"
#else
""
#endif
	  );

          FEPrintf(HLP_X,HLP_Y,1,
"MISC |  H-Histo Logging  I-Inject Gene %s  %s  |->\n",

#ifdef MICRO
          "M-Micro Toggle",
#else
          "",
#endif
#ifdef ALCOMM
          "P-ALmonD Pause"
#else
          ""
#endif
         );
      answer = mvgetch(HLP_Y,HLP_X+31);
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
#ifdef ALCOMM
         if (answer == 'P') 
             {
             AL_run_flag = (AL_run_flag)?0:1; 
	     sprintf(mes[0]," ALmond Pause = %s\n",
		     (AL_run_flag)?"off": "on");
	     FEMessage(1,mes);
             }
#endif /* ALCOMM */
         if (answer == 'I') 
             {
	     sprintf(mes[0],"INJECT GENE TO RUNNING SIMULATION\n"); 
	     sprintf(mes[1],"Enter gene name ( eg 0080aaa) or \n"); 
	     sprintf(mes[2],"Enter ESC to cancel\n");
	     FEMessage(3,mes);
             echo(); nl();nocbreak();
             FEPrintf(HLP_X,HLP_Y-1,0," ");
	     clrtoeol();
             fgets(data,84,stdin);
             /* getstr(data); */
             cbreak();noecho(); nonl();
	     if (data[0] != 0x1b) InjectFromBank(data, -1, 0);
	     sprintf(mes[0]," Attempted Injection of %30s done\n", data);
	     FEMessage(1,mes);
	     FEStats();
             }
          answer = ' ';
	  IMode = PLN_STATS;
          }	/* end of misc */
      }         /* end while loop */

standend();
FEPrintf(HLP_X,HLP_Y,1,
"\t\t\tPress Interupt Key for menu ...\t\t\t");
refresh();
TC_Menu =0;

}	/* end of FEMenu */

/*-----------------------------------------------------------------------*/

/* ======================================================================*/
void FEClrmsg(n)
I32s n;
{
I8s t;
if(n <0) return;
mvprintw(n,0," ");
clrtobot();
FEPrintf(HLP_X,HLP_Y,1,
"                          Press Interupt Key for menu ...                    "
);

/*
for(t= n; ( t < fe_lines); t++)
    {
    FEPrintf(MSG_X,t,0," ");
    FECeol();
    }
*/
}
/* ======================================================================*/
/* ======================================================================*/

