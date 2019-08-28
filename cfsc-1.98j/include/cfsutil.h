#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* cfsutil.c */
VOID InSrtCf PRO((struct CfNode *CfPtr));
VOID ReSrtCfs PRO((void));
VOID FindHighLowStrCf PRO((void));
int IsMessage PRO((char String[]));
int IsCondAct PRO((char String[]));
int AMsgtoB PRO((char AsciiMsg[], unsigned BinMsg[]));
VOID BMsgtoA PRO((unsigned BinMsg[], char AsciiMsg[]));
int ACndtoB PRO((char AsciiCnd[], unsigned int CndBits[], unsigned int CndDCs[]));
VOID BCndtoA PRO((unsigned CndBits[], unsigned CndDCs[], char AsciiCnd[]));
int AActtoB PRO((char AsciiAct[], unsigned ActBits[], unsigned ActDCs[]));
VOID BActtoA PRO((unsigned ActBits[], unsigned ActDCs[], char AsciiAct[]));
int Loci2Int PRO((char Loci[], int Len));
VOID Int2Loci PRO((int Bin, char Loci[], int Len));
float CalcSpec PRO((char Cnd1[STRNGSZ ], char Cnd2[STRNGSZ ], unsigned int Cnd1Type, unsigned int Cnd2Type));
VOID Mod_Cf PRO((char CmdPars[]));
VOID Mod_Msg PRO((char CmdPars[]));

#undef PRO
