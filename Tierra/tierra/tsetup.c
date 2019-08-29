/* tsetup.c  9-9-92  Tierra Simulator  setup routines */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

#ifndef lint
static char sccsid[] = "@(#)tsetup.c	1.5 7/21/92";

#endif

#include "license.h"
#include "tierra.h"
#include "extern.h"
#include <errno.h>
#include <signal.h>
#ifdef unix
#include <unistd.h>
#endif /* unix */

#ifdef MEM_CHK
#include <memcheck.h>
#endif

I8s GetAVar(data)
    I8s data[85];
{
    switch (*data) {
    case 'a':
    if (!strncmp(data, "alive", 5))
        return sscanf(data, "alive = %ld", &alive), 1;
    case 'd':
    if (!strncmp(data, "debug", 5))
        return sscanf(data, "debug = %ld", &debug), 1;
    case 'h':
    if (!strncmp(data, "hangup", 6))
        return sscanf(data, "hangup = %ld", &hangup), 1;
    case 'n':
    if (!strncmp(data, "new_soup", 8))
        return sscanf(data, "new_soup = %ld", &new_soup), 1;
    case 's':
    if (!strncmp(data, "seed", 4))
        return sscanf(data, "seed = %ld", &seed), 1;
    case 'B':
    if (!strncmp(data, "BrkupSiz", 8))
        return sscanf(data, "BrkupSiz = %ld", &BrkupSiz), 1;
    case 'C':
    if (!strncmp(data, "CumGeneBnk", 10))
        return sscanf(data, "CumGeneBnk = %ld", &CumGeneBnk), 1;
    case 'D':
    if (!strncmp(data, "DiskOut", 7))
        return sscanf(data, "DiskOut = %ld", &DiskOut), 1;
    if (!strncmp(data, "DistFreq", 8))
        return sscanf(data, "DistFreq = %f", &DistFreq), 1;
    if (!strncmp(data, "DistProp", 8))
        return sscanf(data, "DistProp = %f", &DistProp), 1;
    if (!strncmp(data, "DivSameSiz", 10))
        return sscanf(data, "DivSameSiz = %ld", &DivSameSiz), 1;
    if (!strncmp(data, "DivSameGen", 10))
        return sscanf(data, "DivSameGen = %ld", &DivSameGen), 1;
    if (!strncmp(data, "DropDead", 8))
        return sscanf(data, "DropDead = %ld", &DropDead), 1;
    case 'G':
    if (!strncmp(data, "GeneBnker", 9))
        return sscanf(data, "GeneBnker = %ld", &GeneBnker), 1;
    if (!strncmp(data, "GenebankPath", 12))
        return sscanf(data, "GenebankPath = %s", GenebankPath), 1;
    if (!strncmp(data, "GenPerBkgMut", 12))
        return sscanf(data, "GenPerBkgMut = %f", &GenPerBkgMut), 1;
    if (!strncmp(data, "GenPerFlaw", 10))
        return sscanf(data, "GenPerFlaw = %f", &GenPerFlaw), 1;
    if (!strncmp(data, "GenPerMovMut", 12))
        return sscanf(data, "GenPerMovMut = %f", &GenPerMovMut), 1;
    case 'L':
        if (!strncmp(data, "Log", 3))
            return sscanf(data, "Log = %ld", &Log), 1;
    case 'I':
         if (!strncmp(data, "IMapFile", 8))
           return sscanf(data, "IMapFile = %s", IMapFile), 1;
    case 'M':
    if (!strncmp(data, "MalMode", 7))
        return sscanf(data, "MalMode = %ld", &MalMode), 1;
    if (!strncmp(data, "MalReapTol", 10))
        return sscanf(data, "MalReapTol = %ld", &MalReapTol), 1;
    if (!strncmp(data, "MalTol", 6))
        return sscanf(data, "MalTol = %ld", &MalTol), 1;
    if (!strncmp(data, "MateProb", 8))
        return sscanf(data, "MateProb = %f", &MateProb), 1;
    if (!strncmp(data, "MateSearchL", 11))
        return sscanf(data, "MateSearchL = %ld", &MateSearchL), 1;
    if (!strncmp(data, "MateSizeEp", 10))
        return sscanf(data, "MateSizeEp = %ld", &MateSizeEp), 1;
    if (!strncmp(data, "MateXoverProp", 13))
        return sscanf(data, "MateXoverProp = %f", &MateXoverProp), 1;
    if (!strncmp(data, "MaxFreeBlocks", 13))
        return sscanf(data, "MaxFreeBlocks = %ld", &MaxFreeBlocks), 1;
    if (!strncmp(data, "MemModeFree", 11))
        return sscanf(data, "MemModeFree = %ld", &MemModeFree), 1;
    if (!strncmp(data, "MemModeProt", 11))
        return sscanf(data, "MemModeProt = %ld", &MemModeProt), 1;
    if (!strncmp(data, "MaxMalMult", 10))
        return sscanf(data, "MaxMalMult = %f", &MaxMalMult), 1;
    if (!strncmp(data, "MinCellSize", 11))
        return sscanf(data, "MinCellSize = %ld", &MinCellSize), 1;
    if (!strncmp(data, "MinTemplSize ", 12))
        return sscanf(data, "MinTemplSize = %ld", &MinTemplSize), 1;
    if (!strncmp(data, "MovPropThrDiv", 13))
        return sscanf(data, "MovPropThrDiv = %f", &MovPropThrDiv), 1;
    case 'N':
    if (!strncmp(data, "NumCells", 8))
        return sscanf(data, "NumCells = %ld", &NumCells), 1;
    case 'O':
    if (!strncmp(data, "OutPath", 7))
        return sscanf(data, "OutPath = %s", OutPath), 1;
    case 'P':
    if (!strncmp(data, "PhotonPow", 9))
        return sscanf(data, "PhotonPow = %lf", &PhotonPow), 1;
    if (!strncmp(data, "PhotonWidth", 11))
        return sscanf(data, "PhotonWidth = %ld", &PhotonWidth), 1;
    if (!strncmp(data, "PhotonWord", 10))
        return sscanf(data, "PhotonWord = %s", PhotonWord), 1;
    if (!strncmp(data, "PutLimit", 8))
        return sscanf(data, "PutLimit = %f", &PutLimit), 1;
    case 'R':
    if (!strncmp(data, "RamBankSiz", 10))
        return sscanf(data, "RamBankSiz = %ld", &RamBankSiz), 1;
    if (!strncmp(data, "ReapRndProp", 11))
        return sscanf(data, "ReapRndProp = %g", &ReapRndProp), 1;
    case 'S':
    if (!strncmp(data, "SaveFreq", 8))
        return sscanf(data, "SaveFreq = %ld", &SaveFreq), 1;
    if (!strncmp(data, "SavMinNum", 9))
        return sscanf(data, "SavMinNum = %ld", &SavMinNum), 1;
    if (!strncmp(data, "SavThrMem", 9))
        return sscanf(data, "SavThrMem = %f", &SavThrMem), 1;
    if (!strncmp(data, "SavThrPop", 9))
        return sscanf(data, "SavThrPop = %f", &SavThrPop), 1;
    if (!strncmp(data, "SearchLimit", 11))
        return sscanf(data, "SearchLimit = %f", &SearchLimit), 1;
    if (!strncmp(data, "SizDepSlice", 11))
        return sscanf(data, "SizDepSlice = %ld", &SizDepSlice), 1;
    if (!strncmp(data, "SlicePow", 8))
        return sscanf(data, "SlicePow = %lf", &SlicePow), 1;
    if (!strncmp(data, "SliceSize", 9))
        return sscanf(data, "SliceSize = %ld", &SliceSize), 1;
    if (!strncmp(data, "SliceStyle", 10))
        return sscanf(data, "SliceStyle = %ld", &SliceStyle), 1;
    if (!strncmp(data, "SlicFixFrac", 11))
        return sscanf(data, "SlicFixFrac = %f", &SlicFixFrac), 1;
    if (!strncmp(data, "SlicRanFrac", 11))
        return sscanf(data, "SlicRanFrac = %f", &SlicRanFrac), 1;
    if (!strncmp(data, "SoupSize", 8))
        return sscanf(data, "SoupSize = %ld", &SoupSize), 1;
    case 'W':
    if (!strncmp(data, "WatchExe", 8))
        return sscanf(data, "WatchExe = %ld", &WatchExe), 1;
    if (!strncmp(data, "WatchMov", 8))
        return sscanf(data, "WatchMov = %ld", &WatchMov), 1;
    if (!strncmp(data, "WatchTem", 8))
        return sscanf(data, "WatchTem = %ld", &WatchTem), 1;
    default:
        return 0;
    }
}

void GetSoup(argc,argv)
I32s argc;
I8s *argv[];
{
    FILE *inf;
    I32s i;
    SList **tsl;

    sprintf(mes[0], "Using instruction set (INST) = %d", INST);
    FEMessage(1,mes);

#ifdef __TURBOC__
    timezone = (I32s) 5L *60L * 60L;
#endif /* __TURBOC__ */

    if (argc > 1)
    {   sprintf(soup_fn,"%s", argv[1]); }
    else
    {
#ifdef IBM3090
        sprintf(soup_fn,"soup_in%d.io.d", INST);
#else
        sprintf(soup_fn,"soup_in%d", INST);
#endif
    }

    inf = fopen(soup_fn, "r");
    if (inf == NULL)
    {   FEError(-1300,EXIT,NOWRITE,
            "Tierra GetSoup() file %s not opened, exiting\n", soup_fn);
    }
    fgets(Buff, 84, inf);
    while (1)
    {   if (*Buff != '#' && strlen(Buff) > 3 && !GetAVar(Buff))
            FEError(-1301,NOEXIT,NOWRITE,
                "Tierra GetSoup() bad soup_in line: %s\n", Buff);
        fgets(Buff, 84, inf);
        if (*Buff >= '0' && *Buff <= '9' || !strncmp("space",Buff,5))
            break;
    }
    ToggleLog(0L);
    if (!seed)
    {   seed = (I32s) time(NULL);
        tsrand(seed);
        seed = tlrand();
        for (i = tcrand(); i >=0; i--)
            seed = tlrand();
        if (!new_soup)
        {   tsrand(seed);
            tlrand();
        }
    }
    if (new_soup)
    {   tsrand(seed);
        tlrand();
        sprintf(mes[0], "seed = %ld", seed);
        FEMessage(1,mes);
        GenInList = (I8s **) tcalloc((I32u) NumCells, sizeof(I8s *));
        GenInBuf = (I8s *) tcalloc((I32u) NumCells * 30, sizeof(I8s));
        for (i = 0; i < NumCells; i++)
        {   GenInList[i] = GenInBuf + (i * 30);
            sscanf(Buff, "%[^\n]", GenInList[i]);
            fgets(Buff, 84, inf);
        }
    }

    /* FEStartup();       */

    /* allocate arrays */
    sprintf(mes[0], "sizeof(Instruction)   = %ld",
        (I32s) sizeof(Instruction));
    sprintf(mes[1], "sizeof(Cell)   = %ld",
        (I32s) sizeof(Cell));
    sprintf(mes[2], "sizeof(MemFr) = %ld",
        (I32s) sizeof(MemFr));
    FEMessage(3,mes);
#ifdef __TURBOC__
    sprintf(mes[0], "coreleft = %lu", (I32u) coreleft());
    FEMessage(1,mes);
#endif
    if (new_soup)
    {
        soup = (HpInst) thcalloc((I32u) SoupSize, sizeof(Instruction));
        sprintf(mes[0], "    %ld bytes allocated for soup",
            SoupSize * sizeof(Instruction));
        CelArSiz = CellsSize = (I32s) 16384 / (I32s) sizeof(Cell);
        NumCelAr = 1;
        if (NumCells > CellsSize - 2)
        {   NumCelAr = (NumCells + 2) / CelArSiz;
            if((NumCells + 2) % CelArSiz)
                NumCelAr++;
            CellsSize = NumCelAr * CelArSiz;
        }
        cells = (Pcells  Fp) tcalloc(NumCelAr, sizeof(Pcells));
        for (i = 0; i < NumCelAr; i++)
            cells[i] = (Pcells) tcalloc(CelArSiz, sizeof(Cell));
        sprintf(mes[1], "    %ld bytes allocated for cells",
            CellsSize * sizeof(Cell));
        TopDummy =    &cells[0][0];
        BottomDummy = &cells[0][1];
        FreeMem = (MemFr Fp) tcalloc(MaxFreeBlocks, sizeof(MemFr));
        sprintf(mes[2], "    %ld bytes allocated for MemFr",
            MaxFreeBlocks * sizeof(MemFr));
        FEMessage(3,mes);
#ifdef __TURBOC__
        sprintf(mes[0], "coreleft = %lu  tsetup (soup, cells, FreeMem)",
            coreleft());
        FEMessage(1,mes);
#endif
        if (!soup || !cells || !FreeMem)
            FEError(-1302,EXIT,NOWRITE,
                "Tierra GetSoup() setup malloc error, exiting\n");
        else
        {   sprintf(mes[0], "tsetup: arrays allocated without error");
            FEMessage(1,mes);
        }
    }
    TotFlaw = TotMovMut = TotMut = isolate = 0;
    extr.a = extr.i = 0;
    Disturb.m = Disturb.i = DistNext.m = DistNext.i = 0L;
    siz_sl = 1;
    if (DivSameGen)
        DivSameSiz = 1;
    if (GeneBnker)
    {   tsl = (SList **) tcalloc(siz_sl, sizeof(SList *));
        if (tsl == NULL)
           FEError(-1303,EXIT,NOWRITE,"Tierra GetSoup() tsl tcalloc error\n");
        else
            sl = tsl;
    }
    else
        WatchExe = WatchMov = WatchTem = 0;
    if (new_soup)
        GetNewSoup();
    else
        GetOldSoup(inf);
    sprintf(mes[0], "tsetup: soup gotten");
    FEMessage(1,mes);
    if (SliceStyle == 1)
    {   PhotonSize = (I32s) strlen(PhotonWord);
        PhotonTranslate(PhotonInst, PhotonWord);
        slicer = SlicerPhoton;
    }
    else if (SliceStyle == 0)
        slicer = SlicerQueue;
    else /* default SliceStyle == 2 */
        slicer = RanSlicerQueue;
    if (new_soup)
    {   if (GenInList)
        {   tfree(GenInList);
            GenInList = NULL;
        }
        if (GenInBuf)
        {   tfree(GenInBuf);
            GenInBuf = NULL;
        }
    }
    fclose(inf);
    if(strncmp(IMapFile,"-.map",5) ) /* not the default inst map */
    {   sprintf(Buff,"%s%s",GenebankPath,IMapFile);
        GetAMap(Buff);
    }
/* added to GetSoup from old tierra.c */
#ifdef __TURBOC__
#if FRONTEND == STDIO
    ctrlbrk( T_sig_int);
#endif
#if FRONTEND == BASIC
FEPrintf(HLP_X,HLP_Y,1,
"                         Press Any Key for menu ...                        "
);
#endif  /* BASIC */
#endif	/* __TURBOC__ */

#ifdef unix
    signal(SIGINT,  T_sig_int);
    signal(SIGUSR1, T_sig_read);
    signal(SIGUSR2, T_sig_info);
    signal(SIGHUP,  T_sig_write);
    SigBlockSet = sigmask(SIGINT);
#endif

}

void GetNewSoup()
{   I32s i, j, ar = 0, ci = 2, cs, sad = 0, tNumCells = NumCells;
    Pcells  ce;
    FILE *inf;

    NumCells = 0;
    sprintf(mes[0], "beginning of GetNewSoup");
    FEMessage(1,mes);
    reaped = InstExe.i = InstExe.m = ExtractCount = CountMovMut = 0;
    CountMutRate = CountFlaw = RateMovMut = RateMut = RateFlaw = 0;
    FreeBlocks = FirstOutDisk = 1L;
    Generations = 0.;
    FreeMemCurrent = SoupSize;
    sprintf(mes[0], "init of soup complete");
    FEMessage(1,mes);
    /* initialize FreeMem array */
    MemInit();
    /* initialize cells array */
    for (i = 0; i < NumCelAr; i++)
        for (j = 0; j < CelArSiz; j++)
            InitCell(i,j,&cells[i][j]);
    TopDummy->ld = BottomDummy->ld = 1;
    ce = ThisSlice = BottomReap = TopReap = &cells[0][2];

    /* read in the cell genotypes */
    for (i = 0; i < tNumCells; i++)
    {   if (!strncmp("space", GenInList[i], 5))
        {   sscanf(GenInList[i], "%*s%ld", &cs);
            sprintf(mes[0], "skipping %ld instructions", cs);
            FEMessage(1,mes);
        }
        else
        {   sscanf(GenInList[i], "%4ld", &cs);
            sprintf(mes[0], "GetNewSoup: loading %s into cell %ld,%ld",
                GenInList[i], ar, ci);
            FEMessage(1,mes);
            InjectFromBank(GenInList[i], sad, 0);
            ci++;
        }
        if (!(ci % CelArSiz))
        {   ci = 0;
            ar++;
        }
        sad += cs;
    }
    plan();
}

void GetOldSoup(inf)
    FILE *inf;
{   I32s i, gi, si, ar, ci, tNumGenotypes = 0, tNumSizes = 0;
    CellInd  brind, trind, tsind;
    Pcells  ce;
    FILE *inc;

    fgets(Buff, 84, inf);
    fgets(Buff, 84, inf);
    sscanf(Buff, "AverageSize = %ld", &AverageSize);
    fgets(Buff, 84, inf);
    sscanf(Buff, "BrkupCou = %ld", &BrkupCou);
    fgets(Buff, 84, inf);
    sscanf(Buff, "BrkupCum = %ld", &BrkupCum);
    fgets(Buff, 84, inf);
    sscanf(Buff, "CelArSiz = %ld", &CelArSiz);
    fgets(Buff, 84, inf);
    sscanf(Buff, "CellsSize = %ld", &CellsSize);
    fgets(Buff, 84, inf);
    sscanf(Buff, "CountFlaw = %ld", &CountFlaw);
    fgets(Buff, 84, inf);
    sscanf(Buff, "CountMovMut = %ld", &CountMovMut);
    fgets(Buff, 84, inf);
    sscanf(Buff, "CountMutRate = %ld", &CountMutRate);
    fgets(Buff, 84, inf);
    sscanf(Buff, "debug_switch = %ld", &debug_switch);
    fgets(Buff, 84, inf);
    sscanf(Buff, "DistNext.m = %ld", &DistNext.m);
    fgets(Buff, 84, inf);
    sscanf(Buff, "DistNext.i = %ld", &DistNext.i);
    fgets(Buff, 84, inf);
    sscanf(Buff, "Disturb.m = %ld", &Disturb.m);
    fgets(Buff, 84, inf);
    sscanf(Buff, "Disturb.i = %ld", &Disturb.i);
    fgets(Buff, 84, inf);
    sscanf(Buff, "ExtractCount = %ld", &ExtractCount);
    fgets(Buff, 84, inf);
    sscanf(Buff, "FirstOutDisk = %ld", &FirstOutDisk);
    fgets(Buff, 84, inf);
    sscanf(Buff, "FreeBlocks = %ld", &FreeBlocks);
    fgets(Buff, 84, inf);
    sscanf(Buff, "FreeMemCurrent = %ld", &FreeMemCurrent);
    fgets(Buff, 84, inf);
    sscanf(Buff, "Generations = %lf", &Generations);
    fgets(Buff, 84, inf);
    sscanf(Buff, "InstExe.i = %ld", &InstExe.i);
    fgets(Buff, 84, inf);
    sscanf(Buff, "InstExe.m = %ld", &InstExe.m);
    fgets(Buff, 84, inf);
    sscanf(Buff, "isolate = %ld", &isolate);
    fgets(Buff, 84, inf);
    sscanf(Buff, "LastDiv.i = %ld", &LastDiv.i);
    fgets(Buff, 84, inf);
    sscanf(Buff, "LastDiv.m = %ld", &LastDiv.m);
    fgets(Buff, 84, inf);
    sscanf(Buff, "MalLimit = %ld", &MalLimit);
    fgets(Buff, 84, inf);
    sscanf(Buff, "NumCelAr = %ld", &NumCelAr);
    fgets(Buff, 84, inf);
    sscanf(Buff, "NumGenotypes = %ld", &NumGenotypes);
    fgets(Buff, 84, inf);
    sscanf(Buff, "NumSizes = %ld", &NumSizes);
    fgets(Buff, 84, inf);
    sscanf(Buff, "Put_limit = %ld", &Put_limit);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RandIx1 = %ld", &RandIx1);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RandIx2 = %ld", &RandIx2);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RandIx3 = %ld", &RandIx3);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RateFlaw = %ld", &RateFlaw);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RateMovMut = %ld", &RateMovMut);
    fgets(Buff, 84, inf);
    sscanf(Buff, "RateMut = %ld", &RateMut);
    fgets(Buff, 84, inf);
    sscanf(Buff, "reaped = %ld", &reaped);
    fgets(Buff, 84, inf);
    sscanf(Buff, "Search_limit = %ld", &Search_limit);
    fgets(Buff, 84, inf);
    sscanf(Buff, "SoupBot = %ld", &SoupBot);
    fgets(Buff, 84, inf);
    sscanf(Buff, "SoupTop = %ld", &SoupTop);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TimeBirth = %ld", &TimeBirth);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TimeDeath = %ld", &TimeDeath);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TimePop = %lf", &TimePop);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TotFlaw = %ld", &TotFlaw);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TotMovMut = %ld", &TotMovMut);
    fgets(Buff, 84, inf);
    sscanf(Buff, "TotMut = %ld", &TotMut);
    fgets(Buff, 84, inf);
    sscanf(Buff, "filepos = %ld", &pos);
#ifdef IBM3090
    strcpy(Buff, "core_out.io.d");
#else
    strcpy(Buff, "core_out");
#endif
    ReadDynMem(Buff);
    ReadGeneBank();
    if (DiskOut)
    {
#ifdef IBM3090
        if (BrkupSiz)
            sprintf(Buff, "break.%ld.d", BrkupCou);
        else
            sprintf(Buff, "tierra.run");
#else
        if (BrkupSiz)
            sprintf(Buff, "%sbreak.%ld", OutPath, BrkupCou);
        else
            sprintf(Buff, "%stierra.run", OutPath);
#endif
        oufr = fopen(Buff, "r+");
        if (oufr == NULL)
        {   FEError(-1309,EXIT,NOWRITE,
                "Tierra GetOldSoup() 1 file %s not opened, exiting", Buff);
        }
        fseek(oufr, pos, SEEK_SET);
    }
}

void WriteSoup(close_disk)
    I32s close_disk;
{
    FILE    *ouf;
    I32s    i, j, pos;
    I8s     comd[120], path[99], tpath[99];
    GList   *tgl;
    FILE    *fp, *tf;
    head_t  head, thead;
    indx_t  *indx, *tindx, indxt, gindx;
    long int tp;

    if (DiskOut)
    {   pos = ftell(oufr);
        if (close_disk)
            fclose(oufr);
    }
    if (GeneBnker)
        SavGeneBank();
    new_soup = 0;
#ifdef IBM3090
    sprintf(comd, "soup_out.io.d");
#else
    sprintf(comd, "soup_out");
#endif
    tp = (I32s) time(NULL);
    ouf = fopen(comd, "w");
    if (ouf == NULL)
    {   FEError(-1310,EXIT,NOWRITE,
            "Tierra WriteSoup() 0 file %s not opened, exiting", comd);
    }
    fprintf(ouf, "# tierra core:  %s\n", ctime(&tp));
    fprintf(ouf, "# observational parameters:\n\n");

    fprintf(ouf, "BrkupSiz = %ld\n", BrkupSiz);
    fprintf(ouf, "CumGeneBnk = %ld\n", CumGeneBnk);
    fprintf(ouf, "debug = %ld\n", debug);
    fprintf(ouf, "DiskOut = %ld\n", DiskOut);
    fprintf(ouf, "GeneBnker = %ld\n", GeneBnker);
    fprintf(ouf, "GenebankPath = %s\n", GenebankPath);
    fprintf(ouf, "hangup = %ld\n", hangup);
    fprintf(ouf, "Log = %ld\n", Log);
    fprintf(ouf, "MaxFreeBlocks = %ld\n", MaxFreeBlocks);
    fprintf(ouf, "OutPath = %s\n", OutPath);
    fprintf(ouf, "RamBankSiz = %ld\n", RamBankSiz);
    fprintf(ouf, "SaveFreq = %ld\n", SaveFreq);
    fprintf(ouf, "SavMinNum = %ld\n", SavMinNum);
    fprintf(ouf, "SavThrMem = %g\n", SavThrMem);
    fprintf(ouf, "SavThrPop = %g\n", SavThrPop);
    fprintf(ouf, "WatchExe = %ld\n", WatchExe);
    fprintf(ouf, "WatchMov = %ld\n", WatchMov);
    fprintf(ouf, "WatchTem = %ld\n", WatchTem);

    fprintf(ouf, "\n# environmental variables:\n\n");

    fprintf(ouf, "alive = %ld\n", alive);
    fprintf(ouf, "DistFreq = %f\n", DistFreq);
    fprintf(ouf, "DistProp = %f\n", DistProp);
    fprintf(ouf, "DivSameSiz = %ld\n", DivSameSiz);
    fprintf(ouf, "DivSameGen = %ld\n", DivSameGen);
    fprintf(ouf, "DropDead = %ld\n", DropDead);
    fprintf(ouf, "GenPerBkgMut = %f\n", GenPerBkgMut);
    fprintf(ouf, "GenPerFlaw = %f\n", GenPerFlaw);
    fprintf(ouf, "GenPerMovMut = %f\n", GenPerMovMut);
    fprintf(ouf, "IMapFile = %s\n", IMapFile);
    fprintf(ouf, "MalMode = %ld\n", MalMode);
    fprintf(ouf, "MalReapTol = %ld\n", MalReapTol);
    fprintf(ouf, "MalTol = %ld\n", MalTol);
    fprintf(ouf, "MateProb = %f\n", MateProb);
    fprintf(ouf, "MateSearchL = %ld\n", MateSearchL);
    fprintf(ouf, "MateSizeEp = %ld\n", MateSizeEp);
    fprintf(ouf, "MateXoverProp = %f\n", MateXoverProp);
    fprintf(ouf, "MaxMalMult = %g\n", MaxMalMult);
    fprintf(ouf, "MinCellSize = %ld\n", MinCellSize);
    fprintf(ouf, "MinTemplSize = %ld\n", MinTemplSize);
    fprintf(ouf, "MovPropThrDiv = %g\n", MovPropThrDiv);
    fprintf(ouf, "MemModeFree = %ld\n", MemModeFree);
    fprintf(ouf, "MemModeProt = %ld\n", MemModeProt);
    fprintf(ouf, "new_soup = %ld\n", new_soup);
    fprintf(ouf, "NumCells = %ld\n", NumCells);
    fprintf(ouf, "PhotonPow = %g\n", PhotonPow);
    fprintf(ouf, "PhotonWidth = %ld\n", PhotonWidth);
    fprintf(ouf, "PhotonWord = %s\n", PhotonWord);
    fprintf(ouf, "PutLimit = %g\n", PutLimit);
    fprintf(ouf, "ReapRndProp = %g\n", ReapRndProp);
    fprintf(ouf, "SearchLimit = %g\n", SearchLimit);
    fprintf(ouf, "seed = %ld\n", seed);
    fprintf(ouf, "SizDepSlice = %ld\n", SizDepSlice);
    fprintf(ouf, "SlicePow = %g\n", SlicePow);
    fprintf(ouf, "SliceSize = %ld\n", SliceSize);
    fprintf(ouf, "SliceStyle = %ld\n", SliceStyle);
    fprintf(ouf, "SlicFixFrac = %g\n", SlicFixFrac);
    fprintf(ouf, "SlicRanFrac = %g\n", SlicRanFrac);
    fprintf(ouf, "SoupSize = %ld\n", SoupSize);

    fprintf(ouf, "\n0 to stop GetAVar\n\n");
    /* end soup_in variables */

    fprintf(ouf, "AverageSize = %ld\n", AverageSize);
    fprintf(ouf, "BrkupCou = %ld\n", BrkupCou);
    fprintf(ouf, "BrkupCum = %ld\n", BrkupCum);
    fprintf(ouf, "CelArSiz = %ld\n", CelArSiz);
    fprintf(ouf, "CellsSize = %ld\n", CellsSize);
    fprintf(ouf, "CountFlaw = %ld\n", CountFlaw);
    fprintf(ouf, "CountMovMut = %ld\n", CountMovMut);
    fprintf(ouf, "CountMutRate = %ld\n", CountMutRate);
    fprintf(ouf, "debug_switch = %ld\n", debug_switch);
    fprintf(ouf, "DistNext.m = %ld\n", DistNext.m);
    fprintf(ouf, "DistNext.i = %ld\n", DistNext.i);
    fprintf(ouf, "Disturb.m = %ld\n", Disturb.m);
    fprintf(ouf, "Disturb.i = %ld\n", Disturb.i);
    fprintf(ouf, "ExtractCount = %ld\n", ExtractCount);
    fprintf(ouf, "FirstOutDisk = %ld\n", FirstOutDisk);
    fprintf(ouf, "FreeBlocks = %ld\n", FreeBlocks);
    fprintf(ouf, "FreeMemCurrent = %ld\n", FreeMemCurrent);
    fprintf(ouf, "Generations = %lf\n", Generations);
    fprintf(ouf, "InstExe.i = %ld\n", InstExe.i);
    fprintf(ouf, "InstExe.m = %ld\n", InstExe.m);
    fprintf(ouf, "isolate = %ld\n", isolate);
    fprintf(ouf, "LastDiv.i = %ld\n", LastDiv.i);
    fprintf(ouf, "LastDiv.m = %ld\n", LastDiv.m);
    fprintf(ouf, "MalLimit = %ld\n", MalLimit);
    fprintf(ouf, "NumCelAr = %ld\n", NumCelAr);
    fprintf(ouf, "NumGenotypes = %ld\n", NumGenotypes);
    fprintf(ouf, "NumSizes = %ld\n", NumSizes);
    fprintf(ouf, "Put_limit = %ld\n", Put_limit);
    fprintf(ouf, "RandIx1 = %ld\n", RandIx1);
    fprintf(ouf, "RandIx2 = %ld\n", RandIx2);
    fprintf(ouf, "RandIx3 = %ld\n", RandIx3);
    fprintf(ouf, "RateFlaw = %ld\n", RateFlaw);
    fprintf(ouf, "RateMovMut = %ld\n", RateMovMut);
    fprintf(ouf, "RateMut = %ld\n", RateMut);
    fprintf(ouf, "reaped = %ld\n", reaped);
    fprintf(ouf, "Search_limit = %ld\n", Search_limit);
    fprintf(ouf, "SoupBot = %ld\n", SoupBot);
    fprintf(ouf, "SoupTop = %ld\n", SoupTop);
    fprintf(ouf, "TimeBirth = %ld\n", TimeBirth);
    fprintf(ouf, "TimeDeath = %ld\n", TimeDeath);
    fprintf(ouf, "TimePop = %lf\n", TimePop);
    fprintf(ouf, "TotFlaw = %ld\n", TotFlaw);
    fprintf(ouf, "TotMovMut = %ld\n", TotMovMut);
    fprintf(ouf, "TotMut = %ld\n", TotMut);
    fprintf(ouf, "filepos = %ld\n", pos);

    fclose(ouf);

#ifdef IBM3090
    strcpy(comd, "core_out.io.d");
#else
    strcpy(comd, "core_out");
#endif
    SavDynMem(comd);
}

void SavDynMem(filename)
I8s  filename[];
{   I32s    i, j;
    I8s     path[80], tpath[80];
    GList   *tgl;
    FILE    *fp, *tf, *ouf;
    head_t  head, thead;
    indx_t  *indx, *tindx, indxt, gindx;
    CellInd  cind[3];

    ouf = fopen(filename, "wb");
    if (ouf == NULL)
    {   FEError(-1311,EXIT,NOWRITE,
            "Tierra SavDynMem() file %s not opened, exiting", filename);
    }
    tfwrite((I8s Hp) soup, sizeof(Instruction), SoupSize, ouf);
    if (DiskOut)
        fwrite(&lo, sizeof(LastOut), 1, ouf);
    fwrite(&is, sizeof(PInst), 1, ouf);
    fwrite(TrandArray, sizeof(double), 98, ouf);
    cind[0].a = BottomReap->q.this.a;
    cind[0].i = BottomReap->q.this.i;
    cind[1].a = TopReap->q.this.a;
    cind[1].i = TopReap->q.this.i;
    cind[2].a = ThisSlice->q.this.a;
    cind[2].i = ThisSlice->q.this.i;
    fwrite(cind, sizeof(CellInd), 3, ouf);
    for (i = 0; i < NumCelAr; i++)
        fwrite((I8s Fp) cells[i], sizeof(Cell), CelArSiz, ouf);
    tfwrite((I8s Hp) FreeMem, sizeof(MemFr), MaxFreeBlocks, ouf);
    fclose(ouf);
}

void ReadDynMem(filename)
I8s  filename[];
{   I32s     i;
    CellInd  cind[3];
    FILE     *inf;

    inf = fopen(filename, "rb");
    if (inf == NULL)
    {   FEError(-1311,EXIT,NOWRITE,
            "Tierra ReadDynMem() file %s not opened, exiting", filename);
    }
    soup = (HpInst) thcalloc((I32u) SoupSize, sizeof(Instruction));
    tfread((I8s Hp) soup, sizeof(Instruction), SoupSize, inf);
    if (DiskOut)
        fread(&lo, sizeof(LastOut), 1, inf);
    fread(&is, sizeof(PInst), 1, inf);
    fread(TrandArray, sizeof(double), 98, inf);
    fread(cind, sizeof(CellInd), 3, inf);
    cells = (Pcells  *) tcalloc(NumCelAr, sizeof(Pcells));
    for (i = 0; i < NumCelAr; i++)
    {   cells[i] = (Pcells) tcalloc(CelArSiz, sizeof(Cell));
        fread((I8s Fp) cells[i], sizeof(Cell), CelArSiz, inf);
    }
    FreeMem = (MemFr Fp) tcalloc(MaxFreeBlocks, sizeof(MemFr));
    tfread((I8s Hp) FreeMem, sizeof(MemFr), MaxFreeBlocks, inf);
    fclose(inf);

    TopDummy    = &cells[0][0];
    BottomDummy = &cells[0][1];
    BottomReap  = &cells[cind[0].a][cind[0].i];
    TopReap     = &cells[cind[1].a][cind[1].i];
    ThisSlice   = &cells[cind[2].a][cind[2].i];
}

void ReadGeneBank()
{   I32s    gi, tgi, si, ar, ci, tNumGenotypes = 0, tNumSizes = 0;
    Pcells  ce;
    GlIndex  GiHash;

    for (ar = 0; ar < NumCelAr; ar++)
        for (ci = 0; ci < CelArSiz; ci++)
    {   ce = &cells[ar][ci];
        if (!ce->ld || (ar == 0 && ci < 2))
            continue;
        ce->d.genome = soup + ce->mm.p;
        GiHash = CheckGenotype(ce->d, 15); /* check .gen and .tmp files */
#ifdef ERROR                               /* and preserve bit 1 */
        if (ce->d.hash != GiHash.si)
            FEError(-1308,EXIT,NOWRITE,
                "ReadGeneBank(): (ce->d.hash = %ld) != (GiHash.si = %ld)\n",
                ce->d.hash, GiHash.si);
        if (GiHash.gi != (tgi = Lbl2Int(ce->d.gen.label)))
            FEError(-1308,EXIT,NOWRITE,
             "ReadGeneBank(): ce->d.gen.label = %s = %ld, GiHash.gi = %hd\n",
                ce->d.gen.label, tgi, GiHash.gi);
        if (GiHash.gi != ce->d.gi)
            FEError(-1308,EXIT,NOWRITE,
             "ReadGeneBank(): ce->d.gi = %hd, GiHash.gi = %hd\n",
                ce->d.gi, GiHash.gi);
#endif /* ERROR */
        gi = ce->d.gi = GiHash.gi;
        si = ce->d.gen.size;
        strcpy(ce->d.gen.label, Int2Lbl(GiHash.gi));
        if (!sl[si]->g[gi]->pop)
        {   tNumGenotypes++;
            sl[si]->num_g++;
        }
        sl[si]->g[gi]->pop++;
        if (!sl[si]->num_c)
            tNumSizes++;
        sl[si]->num_c++;
    }
#ifdef ERROR
    if(tNumGenotypes != NumGenotypes)
        FEError(-1308,NOEXIT,NOWRITE,
    "Tierra ReadGeneBank() error: tNumGenotoypes = %ld  NumGenotypes = %ld\n",
            tNumGenotypes, NumGenotypes);
    if(tNumSizes != NumSizes)
       FEError(-1308,NOEXIT,NOWRITE,
           "Tierra ReadGeneBank() error: tNumSizes = %ld  NumSizes = %ld\n",
           tNumSizes, NumSizes);
#endif /* ERROR */
}

void SavGeneBank()
{   I32s    i, j;
    I8s     path[80], tpath[80];
    GList   *tgl;
    FILE    *fp, *tf;
    head_t  head, thead;
    indx_t  *indx, *tindx, indxt, gindx;

    for (i = 0; i < siz_sl; i++)
    {   if (!sl[i] || !sl[i]->num_c)
            continue;
        sprintf(path, "%s%04ld.gen", GenebankPath, i);
        sprintf(tpath, "%s%04ld.tmp", GenebankPath, i);
        fp = open_ar(path, i, GFormat, -1);
        tf = open_ar(tpath, i, GFormat, 1);
        head = read_head(fp);
        thead = read_head(tf);

#ifdef __TURBOC__
        indx = &gindx;
        tindx = &indxt;
#else  /* __TURBOC__ */
        indx = read_indx(fp, &head);
        tindx = read_indx(tf, &thead);
#endif /* __TURBOC__ */

        /* for each genotype of this size */
        for (j = 0; j < sl[i]->a_num; j++)
        {   if ((I32u) (tgl = sl[i]->g[j]) > 4)
            {   if (IsBit(tgl->bits, 0)) /* if permanent genotype name */
                    add_gen(fp, &head, &indx, tgl);
                else if (tgl->pop)    /* or has a residual population */
                    add_gen(tf, &thead, &tindx, tgl);
            }
        }
        fclose(fp); fclose(tf);
        if (!head.n)
            unlink(path);
        if (!thead.n)
            unlink(tpath);
#ifndef __TURBOC__
        if (indx)
        {   thfree(indx);
            indx = NULL;
        }
        if (tindx)
        {   thfree(tindx);
            tindx = NULL;
        }
#endif /* __TURBOC__ */
    }
}

void InitCell(ar, ci, ce)
    I32s    ar, ci;
    Pcells  ce;
{   I16s  i;
#ifdef unix
    bzero(ce, sizeof(Cell));
#endif               /* unix */
#ifdef __TURBOC__
    memset(ce, 0, sizeof(Cell));
#endif               /* __TURBOC__ */
    strcpy(ce->d.gen.label, "---");
    strcpy(ce->d.parent.label, "---");
    ce->d.gi = (I32s) -1;    /* index to genome in bank */
    ce->d.ploidy = (I8s) 1;    /* how many tracks */
    ce->q.this.a = ar; ce->q.this.i = ci;
    ce->d.ne = ce->q.n_time = ce->q.p_time = ce->q.n_reap
        = ce->q.p_reap = ce->q.this;
    ce->c.sp = (I16s) STACK_SIZE - 1;
#if INST == 2
	for (i = 0; i < NUMREG; i++)
		ce->c.re[NUMREG + i] = i;
#endif /* INST == 2 */
}

#ifdef FUTURE

void FreeDynMem()
{   I32s  i, j;

    if (GeneBnker)
    {   for (i = 0; i < siz_sl; i++) /* for each size */
        {   if (!sl[i])
                continue;       /* for each genotype of this size */
            for (j = 0; j < sl[i]->a_num; j++)
            {   if ((I32u) (sl[i]->g[j]) > 4)
                {   tfree(sl[i]->g[j]);
                    sl[i]->g[j] = NULL;
                }
            }
            tfree(sl[i]->g);
            sl[i]->g = NULL;
        }
        tfree(sl);
        sl = NULL;
    }
    if (soup)
    {   thfree(soup);
        soup = NULL;
    }
    for (i = 0; i < NumCelAr; i++)
    {   if (cells[i])
        {   tfree(cells[i]);
            cells[i] = NULL;
        }
    }
    if (cells)
    {   tfree(cells);
        cells = NULL;
    }
    if (FreeMem)
    {   tfree(FreeMem);
        FreeMem = NULL;
    }
}

#endif /* FUTURE */
