
/*								 LETSEQ1.H

This file, LETSEQ1.H, defines constants for the LETSEQ1 environment
(LETSEQ1 program and documentation, Copyright 1988 Rick L. Riolo).

This file should be #include-d in the LETSEQ1.DEF file.

*/

#define  LETMEMSZ	 4		/* short-term memory size, i.e., how much of the sequence it keeps */
#define  LETMEMMX	 (LETMEMSZ-1)	/* in the systems 'hardware', unlearned memory.  */

#define  STRLETSZ	 7		/* Number of right-most loci of message in which
										letter is encoded (at detector/effector interfaces).
										(This includes the 2 loci for 'type' and the rest for specific letter.)
									 */
#define  NMLETATT	 7
#define  BINLETMX	 127		/* Max binary integer of size STRLETSZ */

#define  SYMSUPSZ	 128
#define  SYMSUPMX	 (SYMSUPSZ-1)

#define  EFRHIBID	 0		/* Effector resolution: use High Bidder */
#define  EFRHISUM	 1		/* ditto: use highest sum of support (bids) */
#define  EFRPRSUM	 2		/* ditto: pick with probability proportional to sums */

#define  ENVLINSZ	 80		/* maximum length of input line of letters. */
#define  ENVLINMX	 (ENVLINSZ-1)

#define  TYPENONE	 0		/* illegal type */
#define  TYPEVOW	 1		/* Character is a vowel */
#define  TYPECON	 2		/*	"	  is a consonant */
#define  TYPEPUNC	 3		/*	"	  is punctuation mark */

#if defined(__STDC__) || defined(__cplusplus)
# define PRO(s) s
#else
# define PRO(s) ()
#endif


/* letseq1.c */
int LoadEnv PRO((char *FDName));
VOID ReadEnv PRO((FILE *FilePtr));
VOID InitEnv PRO((void));
VOID ReadLetS PRO((FILE *FilePtr));
struct EnvNode *EnvAlloc PRO((void));
int EnvFree PRO((struct EnvNode *enode));
int DsplEnv PRO((char *FDName, int Format));
int WrtEnv PRO((FILE *FilePtr, int Format));
VOID SaveEnv PRO((FILE *FilePtr));
int GetDMsgs PRO((char MStrings[], float MIntens[]));
int StoreSup PRO((struct MsgNode *MsgPtr));
int MkGuess PRO((int BehavStp));
int DftGuess PRO((int BehavStp));
int EQGuess PRO((char EMsg[]));
VOID DoBkgBeh PRO((void));
int GtSysRew PRO((void));
float ISysRew PRO((struct CfNode *CfPtr));
VOID MakeActS PRO((char NewAct[]));
int SameType PRO((int Let1, int Let2));
int GtLetTyp PRO((int Let));
int Strg2Let PRO((char String[STRNGSZ ]));
VOID Let2Strg PRO((int Letter, char String[]));
int DoEnvCmd PRO((char *ParStrng));
int SetEnv PRO((char *Par));
VOID DisCfIE PRO((struct CfNode *Cp, char Buff[], unsigned int Format));
VOID DsCfSTag PRO((char CfBuff[], char OutBuff[]));
VOID DsCfLet PRO((char CfBuff[], char OutBuff[]));
VOID DsCfWnd PRO((char CfBuff[], char OutBuff[]));
VOID DsCfAct PRO((struct CfNode *CPtr, char Buff[]));
int IsMemSym PRO((unsigned int Symbol, char Set[]));
VOID HelpEnv PRO((void));
VOID DsplDetM PRO((void));
int GtUsrMsg PRO((char *NxtMsg, char MStrings[], float MIntense[], int *NumMsgs));

#undef PRO
