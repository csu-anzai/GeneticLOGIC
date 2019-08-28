#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* cfmsgio.c */
VOID LoadMsgs PRO((char *FDName, int How, int List));
VOID ReadMsgs PRO((FILE *FilePtr, int How, int List));
int StoreMsg PRO((char LineBuff[], int List));
VOID EmptyML PRO((int List));
struct MsgNode *GtNxtMNd PRO((int List));
int DsplMsgs PRO((char *FDName, int Format, int List));
VOID WrtMsgs PRO((FILE *Destin, int Format, int List));
VOID PutMNd PRO((struct MsgNode *MsgPtr, char Buff[], int Format));
VOID LoadCfs PRO((char *FDName, int How));
VOID ReadCfs PRO((FILE *Source, int How));
int StoreCf PRO((char LineBuff[]));
int AddCf PRO((char Cond1[], char Cond2[], char Action[], unsigned int Cond1Type, unsigned int Cond2Type, unsigned int ActCode, unsigned int Id, double Str, double BRatio, double ChangeS, unsigned int TNmBid, unsigned int TMtch, unsigned int TProd, unsigned int TPost, unsigned int TEMtch, unsigned int TEAct, unsigned int TPosRw, unsigned int TNegRw, unsigned int TNmOfs, unsigned int StpC, unsigned int StpLPr, unsigned int StpLPs, int NoBB, int NoRpl, int NoPrn));
int DsplCfs PRO((char *FDName, int Format));
VOID WrtCfs PRO((FILE *Destin, int Format));
VOID PutCNd PRO((struct CfNode *CP, char OutBuf[], int Format));

#undef PRO
