/* prototyp.h  9-9-92 */
/* function prototypes for Tierra Simulator, included at end of tierra.h */
/* Tierra Simulator V4.0: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * h_sccsid: @(#)prototyp.h	1.4	4/26/92
 */

#include "license.h"
#include "configur.h"

#ifndef PROTOTYP_H
#define PROTOTYP_H

extern void FreeGen P_((GList  *g));
extern GList * GetAGen P_((I8s  *crit));
extern I8s IsFree P_((I32s  x));
extern I32s MemAlloc P_((I32s  size, I32s  pref, I32s  tol));
extern void MemDealloc P_((I32s  x, I32s  y));
extern void MemInit P_((void));
extern void Inject P_((FpInst  g, I32s  size, I32s  sad, I32s  tol,
        I32s  disk, float  *rrpi));
extern void InjectFromBank P_((I8s  *crit, I32s  sad, I32s  tol));
extern Pcells GetFreeCell P_((void));
extern Pcells FindPutCell P_((I32s  adre));
extern I32s Hash P_((I32s  size, FpInst  v));
extern void FreeDynMem P_((void));
extern void GarbageCollectGB P_((void));
extern void VerifyGB P_((void));
extern void SavGeneBank P_((void));
extern void SavDynMem P_((I8s  filename[]));
extern void ReadGeneBank P_((void));
extern void ReadDynMem P_((I8s  filename[]));
extern void DivideBookeep P_((Pcells  ce, Pcells  ne));
extern Event DivGenBook P_((Pcells  ce, Pcells  nc, Event  InstExe,
        I32s  reaped, I32s  mom, I32s  same, I32s  disk));
extern void OutDisk P_((I32s bd, Pcells nc));
extern void ReapBookeep P_((Pcells  ce));
extern Event ReapGenBook P_((Pcells  ce));
extern void MutBookeep P_((I32s i));
extern void plan P_((void));
extern void extract P_((Pcells  ce));
extern void GetGenList P_((I32s  flags));
extern void InitGList P_((Pgl g, I32s si, I32s gi, I32s size));
extern GlIndex CheckGenotype P_((Dem  ce, I32s  flags));
extern int gc P_((void));
extern I8s IsNewSize P_((I32s  si));
extern void NewSize P_((Dem  *ce, I32s  flags));
extern I16s IsInGenQueue P_((Dem  *ce, I32s  hash));
extern I16s IsInGenBank P_((Dem  *ce, I32s  hash, I32s  flags));
extern I16s NewGenotype P_((Dem  *ce, I32s  hash));
extern I32u WhoIs P_((Pcells  Fp ce, I32s a));
extern I8s IsSameGen P_((I32s size, FpInst g1, FpInst g2));
extern void gq_add P_((GList  *p));
extern void gq_movtop P_((GList  *p));
extern I8s gq_swap P_((void));
extern void gq_rem P_((GList  *p));
extern GList *gq_read P_((int si, int gi));
extern void printq P_((void));
extern void IncrLbl P_((I8s *lbln, I8s *lblo));
extern void MaxLbl P_((I8s *lbl, I8s *s));
extern I8s *Int2Lbl P_((I32s i));
extern I16s Lbl2Int P_((I8s  *s));
extern void DelGenFile P_((Pgl tgl));
extern FILE *open_ar P_((I8s *file, I32s size, I32s format, I32s mode));
extern head_t read_head P_((FILE * fp));
extern void write_head P_((FILE * fp, head_t * head));
extern indx_t *read_indx P_((FILE * fp, head_t * head));
extern void write_indx P_((FILE * fp, head_t * head, indx_t * indx));
extern I32s find_gen P_((FILE  *fp, indx_t  *indx, I8s  *gen, I32s  n));
extern Pgl get_gen P_((FILE * fp, head_t * head, indx_t * indxn, I32s n));
extern I32s add_gen P_((FILE * fp, head_t * head, indx_t ** indx, Pgl gen));
extern I16s GetAscGen P_((Pgl g, I8s ifile[]));
extern I8s WritGenFile P_((Pgl  g, I8s  file[]));
extern I8s WritAscFile P_((Pgl  g, I8s  *file));
extern I16s GetGenFormat P_((Pgl g, I8s file[]));
extern Ascii2Ebcdic P_((I8s *s));
extern Ebcdic2Ascii P_((I8s *s));
extern void WritEcoB P_((I32u bits, I8s * buf));
extern void SetBit P_((I32u *seed, I32u bit, I32u value));
extern void nop P_((Pcells ce));
extern void not0 P_((Pcells ce));
extern void shl P_((Pcells ce));
extern void ifz P_((Pcells ce));
extern void math P_((Pcells ce));
extern void push P_((Pcells ce));
extern void pop P_((Pcells ce));
extern void tcall P_((Pcells ce));
extern void call P_((Pcells ce));
extern void mov P_((Pcells ce));
extern void movdd P_((Pcells ce));
extern void movdi P_((Pcells ce));
extern void movid P_((Pcells ce));
extern void movii P_((Pcells ce));
extern void adr P_((Pcells ce));
extern I32s mal P_((Pcells  ce, I32s  *sug_addr, I32s  sug_size, I32s  mode));
extern I8s  chmode P_((Pcells ce, I32s start, I32s size, I32s mode));
extern void malchm P_((Pcells ce));
extern void divide P_((Pcells ce));
extern void CheckCells P_((void));
extern I32s flaw P_((Pcells ce));
extern I32s template P_((I32s f, I32s b, I32s  *slim, I32s tz, I32s dir,
    I32s mode, Pcells ce));
extern I32s ctemplate P_((I32s  *f, I32s  *b, I32s  *slim, I8s  *mode,
    I32s  tz, I32s  dir, Pcells  ce));
extern I32s btemplate P_((I32s f, I32s b, I32s  *slim, I32s tz, I32s dir,
    I32s mode, Pcells ce));
extern I8s IsPriv P_((Pcells ce, I32s a));
extern I8s IsBitPriv P_((Pcells ce, I32s a, I32s mode, I32s track));
extern I8s IsInsideCell P_((Pcells ce, I32s a));
extern void WhichCell P_((I32s  a, Pcells Fp  ce, I8s  *md));
/* extern void SetFlag P_((Pcells ce)); */
extern void thfree P_((I8s Hp ptr));
extern void tfree P_((I8s Fp ptr));
extern I8s Hp threcalloc P_((I8s  Hp ptr, I32u  nsiz, I32u  osiz));
extern I8s Fp trecalloc P_((I8s  Fp ptr, I32u  nsiz, I32u  osiz));
extern I8s Hp thcalloc P_((I32u num, I32u siz));
extern I8s Fp tcalloc P_((I32u  num, I32u  siz));
extern I32u tfread P_((I8s Hp ptr, I32s size, I32s n, FILE * stream));
extern I32u tfwrite P_((I8s Hp ptr, I32s size, I32s n, FILE * stream));
extern void IncrSliceQueue P_((void));
extern void EntBotSlicer P_((Pcells  ce));
extern void UpReaper P_((Pcells  ce));
extern void DownReaper P_((Pcells  ce));
extern void UpRprIf P_((Pcells  ce));
extern void DownReperIf P_((Pcells  ce));
extern void EntBotReaper P_((Pcells  ce));
extern void RmvFrmReaper P_((Pcells  ce));
extern void RmvFrmSlicer P_((Pcells  ce));
extern void SlicerPhoton P_((void));
extern I32s PhotonFit P_((I32s a, I8s *PhotonInst, I32s PhotonSize));
extern I32s PhotonSlide P_((I32s a, I8s *PhotonInst, I32s PhotonSize,
			    I32s PhotonWidth));
extern void PhotonTranslate P_((I8s *PhotonInst, I8s *PhotonWord));
extern void SlicerQueue P_((void));
extern void RanSlicerQueue P_((void));
extern int main P_((int argc, char *argv[]));
extern void life P_((void));
extern void TimeSlice P_((Pcells  ce, I32s size_slice));
extern I16s FetchDecode P_((Pcells  ce));
extern void IncrementIp P_((Pcells  ce));
extern void SystemWork P_((Pcells  ce));
extern void mutate P_((void));
extern void mut_site P_((HpInst s, I32s t));
extern void ReapCheck P_((void));
extern void reaper P_((I32s  ex, I32s  sad));
extern I32s SubEvent P_((Event * event1, Event * event2, Event * result));
extern void tsrand P_((I32s seed));
extern double tdrand P_((void));
extern I8s GetAVar P_((I8s data[85]));
extern void GetAMap P_((I8s  file[85]));
extern void GetSoup P_((I32s argc, I8s *argv[]));
extern void StupGenLists P_((void));
extern void GetNewSoup P_((void));
extern void GetOldSoup P_((FILE * inf));
extern void WriteSoup P_((I32s close_disk));
extern void ToggleLog P_((I32s mode));
extern I16s glcmp P_((const void *gl1, const void *gl2));
extern I16s slcmp P_((const void *sl1, const void *sl2));
extern void InitCell P_((I32s  ar, I32s  ci, Pcells  ce));
extern void GenExExe P_((Pcells  ce, I32s adrt));
extern void GenExMov P_((Pcells  ce, I32s  to, I32s  from));
extern void GenExTemp P_((I32s adrt, Pcells  ce, I32s tsize));
extern void FEExit P_((I32s n));
extern void FEMemCheck P_((I8s  *msg));
extern void FEMemProf P_((I32s  SizSoup, I32s  SizCells, I32s  SizFreeMem,
    I32s  SizSl, I32s  SizSli, I32s  SizGl, I32s  SizGli, I32s  SizGen));
extern void FEMessage P_((I32s n, I8s ** pbuf));
extern void FEStats P_((void));
extern void FEClrmsg P_((I32s n));
extern void FECeol P_((void));
extern I16s FEGetc P_((void));
extern void FEStartup P_((void));
extern void FEPlan P_((I32s MaxPop, I32s MaxMem, Genotype * MaxGenPop,
		       Genotype * MaxGenMem));
extern void FEPrintf P_((I32s scr_x, I32s scr_y, I32s scr_a, ...));
/* turbo c insanity in line above */
extern void FEError P_((I32s err_no, I32s err_exit, I32s err_write, ...));

extern void query_size P_((I32s size_class));
extern void query_species P_((I32s num));
#if FRONTEND != STDIO
extern void query_spec_d P_((I32s size, I32s lbl));
extern void FEMenu P_((void));
#endif


#ifdef __TURBOC__
#if FRONTEND == STDIO
extern int T_sig_int   P_((void));
#endif
#if FRONTEND == BASIC
extern void FE_DosVideoToggle P_((I16s mode));
#endif
#endif

#ifdef unix
extern void T_sig_int  P_((I32s sig, I32s code, I32s *scp, I8s *addr));
extern void T_sig_read P_((I32s sig, I32s code, I32s * scp, I8s *addr));
extern void T_sig_write P_((I32s sig, I32s code, I32s * scp, I8s *addr));
extern void T_sig_info P_((I32s sig, I32s code, I32s * scp, I8s *addr));
#endif

#ifdef ALCOMM
extern void	_t_init_alcomm P_((void));
extern void	_t_life_bookeep P_((void));
extern void     _t_memory_stats P_(( unsigned char which, void *indata,
                    int inlen, void **outdata, int *outdatalen ));
extern void     _t_sim_runcontrol P_(( unsigned char which, void *data,
                    int datalen ));
extern void     _t_init_birthdeath P_(( ALtCLink *clink ));
extern void     _t_query_org P_(( I32s which, void * indata, int inlen, 
                    void **outdata, int *outdatalen ));
#endif

#ifdef MICRO
extern void Micro_Spy P_(( Pcells ce));
#endif

#ifdef HSEX
extern I8s  FindMate P_((Pcells ce));
extern I16s SetXover P_((Pcells ce));
extern I8s  DoMate   P_((void));
#endif

#if INST != 1

extern void regorder P_((Pcells  ce));
extern void pushst P_((Pcells  ce));
extern void not P_((Pcells  ce));
extern void put P_((Pcells  ce));
extern I8s ReadFPut P_((Pcells  ce, I32s  *value));
extern void get P_((Pcells  ce));
extern void Write2Get P_((Pcells  ce, I32s  value));
extern void Broad2Get P_((I32s  value));
extern void Emigrate P_((Pcells  ce));

#endif /* INST != 1 */

#if INST == 1

extern void pnop P_((Pcells  ce));
extern void pnot0 P_((Pcells  ce));
extern void pshl P_((Pcells  ce));
extern void pzero P_((Pcells  ce));
extern void pifz P_((Pcells  ce));
extern void psub_ab P_((Pcells  ce));
extern void psub_ac P_((Pcells  ce));
extern void pinc_a P_((Pcells  ce));
extern void pinc_b P_((Pcells  ce));
extern void pdec_c P_((Pcells  ce));
extern void pinc_c P_((Pcells  ce));
extern void ppushax P_((Pcells  ce));
extern void ppushbx P_((Pcells  ce));
extern void ppushcx P_((Pcells  ce));
extern void ppushdx P_((Pcells  ce));
extern void ppopax P_((Pcells  ce));
extern void ppopbx P_((Pcells  ce));
extern void ppopcx P_((Pcells  ce));
extern void ppopdx P_((Pcells  ce));
extern void ptjmp P_((Pcells  ce));
extern void ptjmpb P_((Pcells  ce));
extern void ptcall P_((Pcells  ce));
extern void pret P_((Pcells  ce));
extern void pmovdc P_((Pcells  ce));
extern void pmovba P_((Pcells  ce));
extern void pmovii P_((Pcells  ce));
extern void padr P_((Pcells  ce));
extern void padrb P_((Pcells  ce));
extern void padrf P_((Pcells  ce));
extern void pmal P_((Pcells  ce));
extern void pdivide P_((Pcells  ce));

#endif /* INST == 1 */

#if INST == 2

extern void pnop P_((Pcells  ce));
extern void pax P_((Pcells  ce));
extern void pbx P_((Pcells  ce));
extern void pcx P_((Pcells  ce));
extern void pdx P_((Pcells  ce));
extern void pmovdd P_((Pcells  ce));
extern void pmovdi P_((Pcells  ce));
extern void pmovid P_((Pcells  ce));
extern void pmovii P_((Pcells  ce));
extern void ppush P_((Pcells  ce));
extern void ppop P_((Pcells  ce));
extern void pput P_((Pcells  ce));
extern void pget P_((Pcells  ce));
extern void pinc P_((Pcells  ce));
extern void pdec P_((Pcells  ce));
extern void padd P_((Pcells  ce));
extern void psub P_((Pcells  ce));
extern void pzero P_((Pcells  ce));
extern void pnot0 P_((Pcells  ce));
extern void pshl P_((Pcells  ce));
extern void pnot P_((Pcells  ce));
extern void pifz P_((Pcells  ce));
extern void piffl P_((Pcells  ce));
extern void ptjmp P_((Pcells  ce));
extern void ptjmpb P_((Pcells  ce));
extern void ptcall P_((Pcells  ce));
extern void padr P_((Pcells  ce));
extern void padrb P_((Pcells  ce));
extern void padrf P_((Pcells  ce));
extern void pmal P_((Pcells  ce)); 
extern void pdivide P_((Pcells  ce)); 

#endif /* INST == 2 */

#if INST == 3

extern void pnop P_((Pcells  ce));
extern void pmovdi P_((Pcells  ce));
extern void pmovid P_((Pcells  ce));
extern void pmovii P_((Pcells  ce));
extern void ppush P_((Pcells  ce));
extern void ppop P_((Pcells  ce));
extern void pput P_((Pcells  ce));
extern void pget P_((Pcells  ce));
extern void pinc P_((Pcells  ce));
extern void pdec P_((Pcells  ce));
extern void padd P_((Pcells  ce));
extern void psub P_((Pcells  ce));
extern void pzero P_((Pcells  ce));
extern void pnot0 P_((Pcells  ce));
extern void pshl P_((Pcells  ce));
extern void pnot P_((Pcells  ce));
extern void prand P_((Pcells  ce));
extern void pifz P_((Pcells  ce));
extern void piffl P_((Pcells  ce));
extern void ptjmp P_((Pcells  ce));
extern void ptjmpb P_((Pcells  ce));
extern void ptcall P_((Pcells  ce));
extern void padr P_((Pcells  ce));
extern void padrb P_((Pcells  ce));
extern void padrf P_((Pcells  ce));
extern void pmal P_((Pcells  ce)); 
extern void pdivide P_((Pcells  ce)); 

extern void rollu P_((Pcells  ce));
extern void rolld P_((Pcells  ce));
extern void enter P_((Pcells  ce));
extern void exch P_((Pcells  ce));
extern void pop3 P_((Pcells  ce));
extern void math3 P_((Pcells  ce));
extern void movdd3 P_((Pcells  ce));
extern void adr3 P_((Pcells  ce));
extern void malchm3 P_((Pcells  ce));

#endif /* INST == 3 */

#if INST == 4

extern void pnop P_((Pcells  ce));
extern void pmovdi P_((Pcells  ce));
extern void pmovid P_((Pcells  ce));
extern void pmovii P_((Pcells  ce));
extern void ppushax P_((Pcells  ce));
extern void ppushbx P_((Pcells  ce));
extern void ppushcx P_((Pcells  ce));
extern void ppushdx P_((Pcells  ce));
extern void ppopax P_((Pcells  ce));
extern void ppopbx P_((Pcells  ce));
extern void ppopcx P_((Pcells  ce));
extern void ppopdx P_((Pcells  ce));
extern void pput P_((Pcells  ce));
extern void pget P_((Pcells  ce));
extern void pinc P_((Pcells  ce));
extern void pdec P_((Pcells  ce));
extern void padd P_((Pcells  ce));
extern void psub P_((Pcells  ce));
extern void pzero P_((Pcells  ce));
extern void pnot0 P_((Pcells  ce));
extern void pshl P_((Pcells  ce));
extern void pifz P_((Pcells  ce));
extern void piffl P_((Pcells  ce));
extern void ptjmp P_((Pcells  ce));
extern void ptjmpb P_((Pcells  ce));
extern void ptcall P_((Pcells  ce));
extern void padr P_((Pcells  ce));
extern void padrb P_((Pcells  ce));
extern void padrf P_((Pcells  ce));
extern void pmal P_((Pcells  ce));
extern void pdivide P_((Pcells  ce));

#endif /* INST == 4 */

#endif /* PROTOTYP_H */
