#ifndef __PROTO_H__
#define __PROTO_H__

#if defined(__STDC__) || defined(__cplusplus)
# define PP(x) x
#else
# define PP(x) ()
#endif

#ifndef min
#define		min(a,b)	((a)<=(b)?(a):(b))
#define		max(a,b)	((a)>(b)?(a):(b))
#endif

/* some compiled-in limits for... */
#define		PopSZ		512	/* population size */
#define		GenomeLEN   	64	/* genome length */
#define		SchemaMX	16	/* number of schema we can track */
#define		GenerationMX    512	/* number of generations we can remember for later display, etc. */

struct PopStr
  {				/* structures for individuals */
    unsigned int Id;		/* Each individual has an Id for identification only */
    unsigned int GenCreated;	/* Just nice to know... */
    char Genome[GenomeLEN + 1];	/* Store the genome as leftmost char's 1 and 0 (GenomeLen of them) */
    double Fitness;		/* fitness of this individual */
    struct PopStr
	*NextHigherFit,
	*NextLowerFit;		/* For maintianing a sorted list---nice for display */
  }
P1[PopSZ], P2[PopSZ];		/* Room for a current and new population */

/* variables and routines to generate random numbers */
#define		URand01		((double)random()/(double)LONG_MAX)	/* return 0.0 to 1.0 */
#define		RandStateSZ	256	/* for calls to random() in Ultrix lib */


/* sa2.c */
void main PP((int argc, char **argv));
void TournamentSelection PP((struct PopStr CurP[], struct PopStr NewP[]));
void CopyIndiv PP((struct PopStr *From, struct PopStr *To));
void ModifyPopulation PP((struct PopStr *P));
void Restart PP((char Seed[], char L[]));
void PrintPop PP((struct PopStr Pop[], int Fmt));
void GenerateRandomPop PP((struct PopStr *P));
void EvaluatePop PP((struct PopStr *P));
double FitnessOfString PP((char BString[]));
double sf1 PP((char BString[]));
double BinaryCharToDouble PP((register char *Buff, register int L));
double BinaryCharToDoubleX PP((char *Buff, int L));
int IntToBinaryChar PP((int B, char *Buff, int L));
int EvaluateSchema PP((char *S, unsigned int SNm, struct PopStr *Pop, unsigned int N));
int IsMemberSchema PP((struct PopStr *P, char *S));
int EvaluateSchemaOverFunction PP((char SchemaPar[], char SamplesPar[]));
int ReSortPopulation PP((struct PopStr Pop[], unsigned int Size));
int InsertSortIndiv PP((struct PopStr *IPtr));
char *readline PP((char *S, unsigned int L, FILE *F));
char *striptoken PP((char *S, char *Token, unsigned int L, char *Breaks));
int tokenend PP((char *S, char *Breaks));
int SetPar PP((char *Par, char *Value));
int SetUI PP((unsigned int *Var, char *VarName, char *Val, unsigned int LB, unsigned int UB));
int SetD PP((double *Var, char *VarName, char *Val, double LB, double UB));
int SetSchema PP((void));
int Display PP((char *P1));
int DisplayD2B PP((char Par[]));
int DisplayB2D PP((char Par[]));
int Usage PP((void));

#undef PP
#endif	/* __PROTO_H__ */
