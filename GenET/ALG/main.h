 
#define NAMEMAX 50
#define NAMESIZ NAMEMAX+1
#define FAIL(str) fail(str,__FILE__,__LINE__)

typedef enum {failure, success} statusType;
typedef enum {false,true} boolean;

extern double getCurPrctRun(void);
