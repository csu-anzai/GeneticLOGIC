/* storage.h contains functions for standard storage allocation */

extern boolean *makeBooleanVector(int length, boolean initial);

extern int *makeIntVector(int length, int initial);

extern double *makeDoubleVector(int length, double initial);

extern void *getStorage(int length, size_t bytes);
