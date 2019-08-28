//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#include <time.h>
#include "trand.h"
#include "MLCG.h"
#include "ACG.h"

MLCG            mlcg(0, time(NULL));
TRandom         tRand(&mlcg);

