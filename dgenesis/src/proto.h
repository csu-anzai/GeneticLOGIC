/*
 * ==================================================
 *
 *    Distributed GENESIS
 *
 *    Erick Cantu-Paz
 *    ecantu@lamport.rhon.itam.mx
 *
 *    Instituto Tecnologico Autonomo de Mexico
 *    1993
 *
 * --------------------------------------------------
 *
 *
 *  file:       proto.h
 *
 *  purpose:    function definitions
 *
 */

#ifdef __STDC__
#ifndef PROTO_H
#define PROTO_H 1

/* from best.c */
void Savebest(int);
void Printbest(void);
void Readbest(void);

/* from checkpnt.c */
void Checkpoint(char *);

/* from convert.c */
unsigned long int Ctoi(char *instring, int length);
void Itoc(unsigned long int n, char * outstring, int length);
void Pack(char *instring, char *outstring, int length);
void Unpack(char *instring, char *outstring, int length);
void Gray(char *instring, char *outstring, int length);
void Degray(char *instring, char *outstring, int length);
void FloatRep(char instring[], double vect[], int length);
void StringRep(double *vect, char *outstring, int length);

/* from cross.c */
void Crossover(void);

/* from display.c */
void Dtrace(char *string);
void Interactive(void);
void die(void);
#if TURBOC
void move(int, int);
void clear(void);
void getstr(char *);
void initscr(void);
void endwin(void);
#endif

/* from dist.c */
void SendParameters(void);
void timeout_handler(void);
void MakeProcessTable(void);
void MakeLinks(void);
void SendLinks(void);
void SendTemplate(void);
int Register(char *mainaddr, int mainport);
void ReceiveParameters(void);
void ReceiveLinks(void);
void ReceiveTemplate(void);
void SendStart(void);
void RecvStart(void);
void SendEnd(void);
int RecvEnd(void);
void SendByeLinks(void);

/* from done.c */
char Done();

/* from elitist.c */
void Elitist(void);

/* from error.c */
void Error(char *msg);
void IOError(char *msg);

/* from evaluate.c */
void Evaluate(int nNewMigrants);

/* from generate.c */
void Generate(void);

/* from inipop.c */
char *ReadStructure(FILE *fp, char *bitstring);

/* from init.c */
void Initialize(void);

/* from input.c */
Input(int argc, char *argv[]);
void Setflag(char flag);

/* from main.c */
void FreeMem(void);

/* from measure.c */
void Measure(int nNewMigrants);
double New_worst(void);
void Converge(void);

/* from migrate.c */
void SendMigrants(int link);
int ReceiveMigrants(void);
int ReceiveIndividuals(int active);
void AppendMigrants(int nNewMigrants);
void MakeMigset(void);

/* from misc.c */
char *ReadLine(FILE *fp, char *line, int length);
int CountLines(char *file);
int ilog2(unsigned long n);
void SortPopulation(STRUCTURE *pop);
int compstruct(STRUCTURE *, STRUCTURE *);

/* from mutate.c */
void Mutate(void);

/* from restart.c */
void Restart(void);

/* from schema.c */
void Schema(void);

/* from select.c */
void Select(int nNewMigrants);

/* from socket.c */
char *gethostaddr(void);
int RecvTO(int sockfd, char *buffer, int length, struct sockaddr_in *from, struct timeval *tv);
int Receive(int sockfd, char *buffer, int length, struct sockaddr_in *from);
int Send(int sockfd, char *buffer, int length, struct sockaddr_in *to);
void ClearSocket(int sockfd);
int OpenSocket(int bufsiz);
void InitSocket(void);
int FindPorts(void);

#endif
#endif
