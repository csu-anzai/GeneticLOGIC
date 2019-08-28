UNIT GATypes;

INTERFACE

	USES
		DialogUtils, GFiles, GCommonDec;
	CONST
		cMaxBitStr = 30;
		cMaxGenes = 10;	{Each gene represents a modifiable factor}
		cGeneSize = 31;		{Size of the gene i.e. LongInt}
		cPopulationMax = 100;
		cPopulationMin = 10;
		cMaxTarget = 10;	{Max number of targets}
		cMaxGenerations = 500;

		cDfltLower = -1.0;
		cDfltUpper = 1.0;
		cDfltRes = 8;
		cDfltGenerations = 50;
		cDfltPCross = 0.8;
		cDfltPMut = 0.1;

	TYPE
		TBitStr = ARRAY[1..cMaxGenes] OF LONGINT;
		TChromosome = RECORD		{WARNING!!! This is used in the GeneList LDEF}
				geneID: INTEGER;	{Unique ID #}
				parent1, parent2: INTEGER;	{Ancestry}
				fitness: DOUBLE;	{Fitness value for this gene}
				bits: TBitStr;	{bits array}
			END;
		TPopulation = ARRAY[1..cPopulationMax] OF TChromosome;

		TCodeMethod = (binaryC, otherC);
		TCrossSites = (oneSiteC, twoSiteC);
		TMateMethod = (crossM, otherM);
		TPhenoTypeParms = RECORD
				factID, factNo: INTEGER;	{factID is permament, factNo is reset seach time a search starts}
				lower, upper: DOUBLE;		{Value range}
				res: INTEGER;		{Bit resolution of range}
				codeMethod: TCodeMethod;
				crossSites: TCrossSites;
				mateMethod: TMateMethod;
			END;
		TPTParmArry = ARRAY[1..cMaxGenes] OF TPhenoTypeParms;

		TSearchStats = ARRAY[1..cMaxGenerations] OF DOUBLE;
		TSearchStatsPtr = ^TSearchStats;
		TSearchStatsHdl = ^TSearchStatsPtr;
		TSelectMethod = (rouletteS, randomS, fitFitS, fitWeakS);
		TConvergeMethod = (fitSumV, avgFitV, bestChromV, worstChromV);
		TReplaceMethod = (weakParentR, bothParentR, weakestChromR, randomR);
		TSearchStatus = (searching, stopped, paused, seeding);
		TPopulationRec = RECORD
				popMax: INTEGER;	{Population limit}
				nSeed: INTEGER;
				nGenes: INTEGER;	{Population count}
				nChrom: INTEGER;		{Number of genes in a chromosome}
				growths: TResHdl;		{Fixed growth values for simulation}
				nextID: INTEGER;
				theChroms: TPopulation;		{The chromosome records}
				theModel: TModelHdl;	{The underlying model}
				phenoParms: TPTParmArry;
{Reproduction Parameters}
				selectMethod: TSelectMethod;
				replaceMethod: TReplaceMethod;
				pCross, pMut: DOUBLE;
				nCross, nMut: INTEGER;
				lastParent: INTEGER;
				maxGenerations, genNum: INTEGER;
				convMethod: TConvergeMethod;
				convergeVal: DOUBLE;
{Statistics}
				fitMax, fitMin, fitSum: DOUBLE;
				fitMaxGene, fitMinGene: INTEGER;
				status: TSearchStatus;
				bestFitStats, worstFitStats, avgFitStats: TSearchStatsHdl;
{Support stuff}
				myWD: WDHandle;
			END;
		TPopnPtr = ^TPopulationRec;
		TPopnHdl = ^TPopnPtr;

		TPhenoType = ARRAY[1..cMaxGenes] OF DOUBLE;
		TPhenoTypePtr = ^TPhenoType;
		TPhenoTypeHdl = ^TPhenoTypePtr;

IMPLEMENTATION

END. {GATypes}