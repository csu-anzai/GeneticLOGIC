/* protob.h  14-8-92  prototype file for the Beagle Explorer */
/*** Beagle Explorer:  Copyright (c) 1992  Tom Ray ***/

#ifndef PROTOB_H
#define PROTOB_H

/* bars.c  30-8-90  bar display for the Beagle Explorer */

int bars(void);
int run_info_chk2(TRANSACTION  *tp);
void run_info2(TRANSACTION  *tp);
void species_array_setup(int  *tnum_sp);
void get_total(char  *data);
void bars_update(void);
void barst_setup(void);
int spcmp2(const void  *s1, const void  *s2);
int spcmp3(const void  *s1, const void  *s2);
void do_bupdate(void);
void time_update(void);
void bebar(void);

/* bdivers.c  28-11-91  diversity trace display for the Beagle Explorer */

int diverse(void);
int dFieldSet(TRANSACTION  *tp);
unsigned dKeySim(unsigned (*fp)(), TRANSACTION *tp);
int RunInfoChk(TRANSACTION  *tp);
void TracestSetup(void);
void divers(void);
void setranges(void);
int setxy(char  s[]);
double putout(int  s);

/* beagle.c  30-6-90  Exploratory program for the Tierra Simulator */

void main(void);
void helpfn(MENUITEM  *item);
void writehelp(char  *string);

/* bread.c  30-8-90  reads tierra.run data */

int t_read(char  data[], struct last_out  *lo, int  *first, int  *genotypes);

/* fragment.c  4-8-90  makes fragment of tierra.run */

int fragment(void);
int temp_info_chk_f(TRANSACTION  *tp);
void fragmen(char  *infile, char  *oufile, char  *directory, int  start,
    int  stop);
void CountTTree(struct tnode  *t, int  *num);
void OutFragTree(struct tnode  *t, FILE  *ouf);

/* genework.c  30-6-90  */

int  find_next_use_template(Instruction  *pgene, int  start, int  tail_size,
    int  *site, char  track);
int  is_use_template(Instruction  *locus, char  track);
int  get_template_size(Instruction  *locus, int  tail_size, char  track);
char * inst_name(int  inst, char  *name);
char inst_letter(int  inst);
void unlap(char  track);
void lineup(char  track);
void comp_ins_del(void);
int clear_conflicts(int  num);
int idcmp(const void  *i1, const void  *i2);
void judgement(int  num);
struct insdel ins_del(int  i1, int  i2);
struct insdel insert(int  p1, int  p2);
struct insdel delete(int  p1, int  p2);
int probefit(int  position, Instruction  *pprobe, int  psize,
    Instruction  *pgene, int  gsize, char  track);
void probeslide(Instruction  *pprobe, int  psize, Instruction  *pgene,
    int  gsize, char  track);
void instlist(void);
FpInst get_gentype(char  genepath[], char  genefile[], char  genotype[]);

/* geniob.c   14-8-92 genebank input/output routines for Beagle */

head_t read_head(FILE  *fp);
indx_t *read_indx(FILE  *fp, head_t  *head);
I32s find_gen(indx_t  indx[], I8s  *gen, I32s  n);
Pgl get_gen(FILE  *fp, head_t  *head, indx_t  *indxn, I32s  n);
void WritEcoB(I32u  bits, I8s  *buf);
void SetBit(I32u  *seed, I32u  bit, I32u  value);
void GetAMap(I8s  data[85]);
I16s aid_compare(ArgInstDef  *i, ArgInstDef  *j);

/* probe.c  30-6-90  genome probe for the Beagle Explorer */

int probe(void);
int prob_info_chk(TRANSACTION  *tp);
void prob(char  track);
void pmatch(char  track);
void plogical(int  size);
int orcmp(const void  *g1, const void  *g2);
int prcmp(const void  *g1, const void  *g2);
int rprcmp(const void  *g1, const void  *g2);

/* template.c  29-11-91  template locator for the Beagle Explorer */

int template(void);
int temp_info_chk(TRANSACTION  *tp);
void templat(int  track);
void logical(int  c);

/* tierun.c  4-8-90  makes run_info file for tierra.run, with genotypes */

int Lbl2Int(char  s[]);
void Int2Lbl(int  i, char  s[]);
int gtierunc(void);
int temp_info_chk_g(TRANSACTION  *tp);
void gtierun(void);
struct tnode * AddTree(struct tnode  *p, struct last_out  *lo);
struct snode * Tree2Tree(struct tnode  *t, struct snode  *s, int  index);
struct snode * AddMaxTree(struct tnode  *t, int  index, struct snode  *s);
void OutSearchTree(struct snode  *s, FILE  *ouf, int  *out_num, int  update);
void CountTree(struct snode  *s, int  *num);
void OutUpdate(struct snode  *s, FILE  *ouf, int  update);
void FreeSTree(struct snode  *p);
void FreeTTree(struct tnode  *p);
struct tnode * oldPruneTree(struct tnode  *p, struct tnode  *root);
void PruneTree(struct tnode  *p);
struct tnode * AddPrunedTree(struct tnode  *r, struct tnode *p);
struct tnode * DoUpdate(struct tnode  *p, int  update);
struct tnode * AddFragTree(struct tnode *p, long size, char lbl[], long end);
void ZeroTree(struct tnode  *p);
void StartFragment(char  data[], struct last_out  *lo,
    struct tnode  **uroot, struct tnode  **troot, int  c);

/* tools.c  21-3-92  tools for graphis and windows */

void grinit(char  path[]);
void initerror(int  val);
void grwisetup(char  path[]);
int gr_chk(TRANSACTION  *tp);
void resolutionn(void);
void resolutions(void);
void g_keyboard(void);
int getkeystroke(void);
void minmax(struct point  *p, struct xyrange  *r);
struct point coords(struct point  *p);
struct point rcoords(struct point  *p);
struct point * fxy(struct point  *p);
struct point rrotr(struct point  *xy);
struct point rotr(struct point  *xy);
void sline(struct point  *p);
void cline(struct point  *p);
void eline(struct point  *p);
void putpoint(struct point  *p, int color);
void charsiz(float xz, float yz);
void labchr(struct point  *p, float xz, float yz, float an, char lc);
void labstr(struct point  *p, float xz, float yz, float an, char lab[]);
void curon(struct point  *p, int siz);
void curof(struct point  *p, int siz);
struct rect ptr(struct polar  *rth);
struct polar rtp(struct rect  *xy);
float redangle(float th);
void ptro(float th, float r);
void rtpo(float x, float y);
float atanc(float y, float x);
void wind_setup(void);
HWND wigen(int rp, int cp, int rs, int cs, char frame[7], char title[80]);
HWND wigenlp(int  row_pos, int  col_pos, int  row_siz_log, int  col_siz_log,
    int  row_siz_phy, int  col_siz_phy, char  frame[7], char  title[80]);
void wipak(int rp, int cp);
void wiin(int x, int y);
HWND wiinv(int x, int y, char  mes[80]);
HWND wint(int x, int y, char  mes[80]);
void wipw(int x, int y);
void wiwait(int rp, int cp);
void wiwaitm(int rp, int cp, char  mes[80]);
void witest(int rp, int cp, int rs, int cs);
void wierror(int rp, int cp, char  mes[80]);
void nrerror(char  error_text[80]);
void scroll(int  page, HWND  wi);

/* trace.c  30-8-90  trace display for the Beagle Explorer */

int trace(void);
int FieldSet(TRANSACTION  *tp);
unsigned KeySim(unsigned (*fp)(), TRANSACTION *tp);
int run_info_chk(TRANSACTION  *tp);
void run_info(TRANSACTION  *tp);
void tracest_setup(void);
void trac(void);

#endif /* PROTOB_H */
