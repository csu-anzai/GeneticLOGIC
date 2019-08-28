#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* dscops.c */
VOID DscCDM PRO((void));
int PkPrntCD PRO((struct MsgNode *Msg, int How));
int CfMtchS PRO((struct MsgNode *Msg, struct CfNode *Cf, int *Cond));
int CndMtchS PRO((unsigned int Msg[], unsigned int CndBits[], unsigned int CndDCs[], unsigned int CndType));
VOID GnrlzCf PRO((struct MsgNode *Msg, struct NCfNode *NewCf, int WhichCnd));
VOID CDMGeo PRO((struct MsgNode *Msg, struct NCfNode *NewCf));
VOID DscBkgGA PRO((void));
VOID CrFullCf PRO((struct NCfNode *CfPtr1, struct NCfNode *CfPtr2));
VOID DCrFullCf PRO((struct NCfNode *CfPtr1, struct NCfNode *CfPtr2));
VOID CrCfStrg PRO((struct NCfNode *CfPtr1, struct NCfNode *CfPtr2));
VOID RCrossSt PRO((char String1[], char String2[]));
struct NCfNode *ReprodCf PRO((struct CfNode *ParentCf));
VOID Mutate PRO((struct NCfNode *CfPtr));
VOID MutatCA PRO((struct NCfNode *CfPtr, unsigned int What));

#undef PRO
