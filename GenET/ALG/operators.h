/* operators.h defines standard interface to current rep's operators         */

/* getOperName(operNum) returns name of operator operNum (of size NAMESIZ)   */
extern char *getOperName(int operNum);

/* getOperAddr(operNum) returns address of operator operNum for indirect     */
/*   invocationrs                                                            */
extern operAddrType getOperAddr(int operNum);

/* getNumOpers() returns the number of implemented operators                 */
extern int getNumOpers(void);       

/* setOperSpecifics(fp,operNum) sets any operator-specific information       */
/*   for operNum. Specific information includes any parameters other than    */
/*   probability of application to a chromosome                              */
extern void setOperSpecifics(FILE *fp, int operNum);  
