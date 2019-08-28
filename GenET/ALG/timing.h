/* timing.h contains routines to control iterations and operator */
/*   weights modifications */
/* Runs can be controlled by time limits or iteration limits */

/* updateTiming() sets counters for each new generation */
/* called at end of each generation */
extern void updateTiming(void);

/* setTiming must be called before first generation */
extern void setTiming(void);

extern void setRunLimits(FILE *fp);

/* prctRun() returns the percentage of the current run */
extern double prctRun(void);

/* isDspChrom() returns true and resets internal counter if time for display */
extern boolean isDspChrom(void);

/* isModOper() returns true and resets internal counter if time for oper mod */
extern boolean isModOper(void);


