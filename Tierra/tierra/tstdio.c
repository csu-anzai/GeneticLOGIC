/* ======================================================================*/
/* tstdio.c  9-9-92  support for stdio frontend for
   Tierra Simulator V4.0: Copyright (c) 1992 Dan Pirone & Virtual Life
   written by Daniel pirone
   v 1.1
*/
/* ======================================================================*/
#if FRONTEND == STDIO  /* STDIO == 0 */
/* ======================================================================*/
/* ======================================================================*/
void FEExit(ENO) 
I32s ENO;
{exit(ENO);}
/* ======================================================================*/
void FEClrmsg(n)
I32s n;
{
}
/* ======================================================================*/
void FECeol()
{
}
/* ======================================================================*/
I16s FEGetc()
{
#ifdef __TURBOC__
return getch();
#else 
return getchar();
#endif /* __TURBOC__ */
}
/* ======================================================================*/
void FEStartup()
{
/* called immediately after the soup_in file is read */
/* an opportunity to interactively set soup_in vars */

fe_width = 60;
MSG_X = 1;
MSG_Y = 1;
ERR_X = 1;
ERR_Y = 1;
PLN_X = 1;
PLN_Y = 1;

#ifdef __TURBOC__
clrscr();
#else
system("clear"); 
#endif
/* Title screen */
printf("\n\n\t\t\t\t      TIERRA\n");
printf("\n\n\t\t\t\t     July  92\n");
printf("\n\t\t\t     Artificial Life System\n\t\t\t\t\tBy\n");
printf("\t\t\t\tDr. Thomas S. Ray\n\t\t\t\t\t&\n");
printf("\t\t    Daniel Pirone, Tom Uffner & Marc Cygnus\n\n");
/* sleep(5); */

}
/* ======================================================================*/

#ifdef __TURBOC__
void FEPrintf(I32s scr_x, I32s scr_y, I32s scr_a, ... )
#else /* else of __TURBOC__ */
#ifdef __GNUC__
void FEPrintf(I32s scr_x, I32s scr_y, I32s scr_a, ... )
#else
void FEPrintf(scr_x, scr_y, scr_a, va_alist )
I32s scr_x, scr_y, scr_a;
va_dcl
#endif /* __GNUC__ */
#endif /* else of __TURBOC__ */
{
    va_list ap;

    I8s *msg_str;
#ifdef __TURBOC__
    va_start(ap,scr_a);
#else /* __TURBOC__ */
#ifdef __GNUC__
    va_start(ap,scr_a);
#else
    va_start(ap);
#endif /* else of __GNUC__ */
#endif /* __TURBOC__ */
    if ((msg_str = va_arg(ap, I8s *)) != NULL)
       {
       vfprintf(stdout,msg_str,ap);
       }

    va_end(ap);
    fflush(stdout);

}
/* ======================================================================*/

#ifdef __TURBOC__
int T_sig_int(void)
#endif

#ifdef unix /* unix with stdio ... */

void T_sig_int(sig,code,scp,addr)
    I32s  sig,code;
    /* struct sigcontext *scp; */
    I32s *scp; /* DO NOT USE !!!!! */
    I8s  *addr;

#endif 	/* unix with stdio ... */
{
TC_Menu = 1;
}
/* ======================================================================*/
void FEMenu()
{
   I8s  answer;
   I32s tsz;
   I8s  data[85];
#ifdef ALCOMM
   AL_run_flag = 1;
#endif
   FEError(-1400,NOEXIT,NOWRITE,
           "\n==========================================================");
   FEError(-1401,NOEXIT,NOWRITE,
   "TIERRA: Main Menu");
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
   FEError(-1402,NOEXIT,NOWRITE,
           "---------------------------------------------------------");
   FEError(-1403,NOEXIT,NOWRITE,
"InstExe.m    = %6ld  InstExec.i = %6ld  NumCells = %4ld ",
	   InstExe.m,InstExe.i,NumCells);
  if(GeneBnker)
     {
     FEError(-1404,NOEXIT,NOWRITE,
"NumGenotypes =   %4ld  NumSizes   =   %4ld", NumGenotypes, NumSizes);
     }
#ifdef __TURBOC__
     FEError(-1405,NOEXIT,NOWRITE,"CoreLeft     =  %ld",(I32s) coreleft());
#endif
#ifdef ALCOMM
     FEError(-1406,NOEXIT,NOWRITE,"VPORT        =    %hd", VPORT);
#endif

   FEError(-1407,NOEXIT,NOWRITE,
           "---------------------------------------------------------");
   FEError(-1408,NOEXIT,NOWRITE,
	"\tKey\tFunction\n");
   FEError(-1409,NOEXIT,NOWRITE,
	"\ti\tInformation on simulation");
   FEError(-1410,NOEXIT,NOWRITE,
	"\tv\tChange a soup_in variable");
   FEError(-1410,NOEXIT,NOWRITE,
	"\tm\tMisc. commands");
   FEError(-1411,NOEXIT,NOWRITE,
	"\tS\tExecute a system Shell");
   FEError(-1412,NOEXIT,NOWRITE,
	"\ts\tSave the soup");
   FEError(-1413,NOEXIT,NOWRITE,
	"\tq\tSave the soup & quit");
   FEError(-1414,NOEXIT,NOWRITE,
	"\tQ\tQuit/Abort simulation");
   FEError(-1415,NOEXIT,NOWRITE,
	"\tc\tTo Continue simulation");

   FEError(-1416,NOEXIT,NOWRITE,
           "---------------------------------------------------------");
   while(1)
      {
      FEError(-1417,NOEXIT,NOWRITE,
"TIERRA | i-info v-var m-misc s-save S-shell q-save&quit Q-quit c-cont |-> ");

      fgets(data,84,stdin);
      sscanf(data,"%c", &answer); 
      if (answer == 'c') break; 
      switch (answer)
	 {
      case 'v':
      {
      FEError(-1418,NOEXIT,NOWRITE,
           "---------------------------------------------------------");
      FEError(-1419,NOEXIT,NOWRITE,
         "To alter any global variable from soup_in, type");
      FEError(-1420,NOEXIT,NOWRITE,
         "the variable name (using proper case), a space,");
      FEError(-1421,NOEXIT,NOWRITE,
         "an equal sign, a space, and the new value.");
      FEError(-1422,NOEXIT,NOWRITE,
         "Use no space at start of line.  Some examples:");
      FEError(-1423,NOEXIT,NOWRITE,
         "alive = 0");
      FEError(-1424,NOEXIT,NOWRITE,
         "DistProp = .6");
      FEError(-1425,NOEXIT,NOWRITE,
         "GenebankPath = newpath/");
      FEError(-1426,NOEXIT,NOWRITE,
              "---------------------------------------------------------");
   
         fgets(data,84,stdin);
         if (!GetAVar(data))
            {   
	    FEError(-1427,NOEXIT,NOWRITE,
		   "Not a valid soup_in variable: %s", data);
            }
         ToggleLog(0L);
      break;
      }
   
      case 'i' :
         {
         FEError(-1428,NOEXIT,NOWRITE,
              "---------------------------------------------------------");
         FEError(-1429,NOEXIT,NOWRITE,
              "\ts\tSpectrum of all Size Classes");
         FEError(-1430,NOEXIT,NOWRITE,
              "\tm\tSpectrum of Size Classes, by memory use");
         FEError(-1431,NOEXIT,NOWRITE,
              "\tg\tSpectrum of Size Classes, by geneotype");
         FEError(-1432,NOEXIT,NOWRITE,
              "\tz\tBreak down of a specific Size Class");
         FEError(-1433,NOEXIT,NOWRITE,
	      "\t\tAny other key to main menu ...");
         FEError(-1434,NOEXIT,NOWRITE,
              "---------------------------------------------------------");
         fgets(data,84,stdin);
         sscanf(data,"%c", &answer); 
         switch(answer) 
	   {
         case 'z':
            {
            FEError(-1435,NOEXIT,NOWRITE,
            "Enter a size class ( eg: 80 ) to examine ");
            fgets(data,84,stdin);
            sscanf(data,"%d", &tsz);
	    if (Hist != NULL)
            {  thfree(Hist);
               Hist = NULL;
            }
            query_size((I16u)tsz); 
	    break;
            }
         case 'g':
            {  
	    IMode = GEN_HIST;
            query_species(20);
            if (Hist)
	    {  thfree(Hist);
	       Hist = NULL;
            }
	    break;
            }
         case 'm':
            {  
	    IMode = SIZM_HIST;
            query_species(20);
            if (Hist)
	    {  thfree(Hist);
	       Hist = NULL;
            }
	    break;
            }
         case 's':
            {  
	    IMode = SIZ_HIST;
            query_species(20);
            if (Hist)
	    {  thfree(Hist);
	       Hist = NULL;
            }
	    break;
            }
           }
         answer = ' ';break;
         }	/* end  i info case */
      case 'm':
         {  
         FEError(-1434,NOEXIT,NOWRITE,
              "---------------------------------------------------------");
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
         fgets(data,84,stdin);
         sscanf(data,"%c", &answer);

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
             fgets(data,84,stdin);
             if (data[0] != 0x1b) InjectFromBank(data, -1, 0);
             sprintf(mes[0]," Attempted Injection of %30s done\n", data);
             FEMessage(1,mes);
             }
          answer = ' ';
	  break;
         }     /* end of misc */
      case 's':
         {  
         FEError(-1436,NOEXIT,WRITE," ");
         }
      case 'q':
         {  
         FEError(-1437,EXIT,WRITE," ");
         }
      case 'S':
         {
         FEError(-1438,NOEXIT,NOWRITE,
         "TIERRA: shell (%s), type exit to return ... ",SHELL);
         system(SHELL);
         }
      case 'Q': exit(-666);
         } 	/* end switch */
      } 	/* end while loop */
   FEError(-1439,NOEXIT,NOWRITE,"\nTIERRA: Continuing from interupt...");
   FEError(-1440,NOEXIT,NOWRITE,
           "==========================================================");
TC_Menu = 0;
#ifdef __TURBOC__
   return(1);
#endif
}
/*-----------------------------------------------------------------------*/

/* ======================================================================*/
#endif 	/* stdio ... */
/* ======================================================================*/

