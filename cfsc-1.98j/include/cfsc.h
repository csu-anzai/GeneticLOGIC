#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* cfsc.c */
VOID main PRO((int argc, char *argv[]));
VOID DoCmd PRO((unsigned int CmdCode, char *CmdPars));
VOID ExecCmds PRO((char *FDName));
int Classify PRO((char StepsPar[]));
int StartStp PRO((void));
int EndStep PRO((void));
int Init_CFS PRO((char *InitPath, char *RunFName));
int InitVT PRO((char *InitPath));
VOID InitCfLs PRO((void));
VOID LoadSys PRO((char *FileName, FILE *FilePtr));
VOID ReadVars PRO((FILE *FilePtr));
VOID LinkCMs PRO((void));
struct MsgNode *GetMsgAd PRO((unsigned int Id, int List));
struct CfNode *GetCfAd PRO((unsigned int Id));
int SaveSys PRO((char *FDName));
VOID WrtCons PRO((FILE *FilePtr));
int ReadEffs PRO((FILE *FilePtr));
VOID WrtEffs PRO((FILE *FilePtr));
VOID InitDate PRO((void));

#undef PRO
