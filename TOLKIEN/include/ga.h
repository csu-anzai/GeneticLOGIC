//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      The Chinese University of Hong Kong.
//
#if !defined( __GA_H )
#define __GA_H

#include "populatn.h"
#include "xover.h"
#include "select.h"
#include "SmplStat.h"
#include "DblVec.h"
#include "float.h"

class   GeneticAlgorithm : public TObject
{
public:
	GeneticAlgorithm(PPopulation,
			 PSelectionScheme,
			 PCrossover,
			 float,
			 float,
			 PScalingScheme = NULL,
			 BOOL = FALSE);
	GeneticAlgorithm(PTGAObj,unsigned,PHENOTYPEFUNC);

	virtual ~GeneticAlgorithm();

	virtual void 			printOn( ostream  & out) const;
	virtual int 			compare( RCTObject ) const;

					_shallowCopy(GeneticAlgorithm)
	virtual void 			deepenShallowCopy();

	virtual void                    generation();
	virtual void                    statistics();

		void                    selectionScheme(PSelectionScheme
							pNewSch);
		void                    crossover(PCrossover pNewXOver);
		void                    scaleScheme(PScalingScheme pNewScale);
		PCSelectionScheme       selectionScheme() const;
		PCCrossover             crossover() const;
		PCScalingScheme         scaleScheme() const;
		int                     trials() const;
		int                     trialsMonitored() const;
		void                    resetMonitor();
		RCTGAObj                bestInd() const;
		RCTGAObj                worstInd() const;
		RCPopulation            pop() const;
                DblVec                  onLines(BOOL=FALSE);
                DblVec                  offLines(BOOL=FALSE);
		const SampleStatistic & objValueStat() const;
		const SampleStatistic & fitnessStat() const;

protected:

	PPopulation      pPop;

	int              nTrials;               // trials counter

	int              nOffSprings;           // number of offsprings
						// created in each generation

	float            flMRate;               // mutation rate

	BOOL             reEvaluate;            // whether to re-evaluate
						// old individuals

	PCrossover       pXover;

	PSelectionScheme pSSch;

	PScalingScheme	 pScale;

	SampleStatistic objStat;                // statistics of
						// objective values

	SampleStatistic fitStat;                // statistics of
						// fitness values

	DblVec        	onLineVec, offLineVec;
			// DeJong's online and offline performance

	DblVec        	objOnLineVec, objOffLineVec;
			// online and offline performance
			// for objective values

	int             nTrialsMonitored;       // number of on-line and
						// and off-line performaces
						// taken

	double		flBestSoFar;            // the best on-line so far

	double		flSumOnLines;           // sum of on-line performances

	double          flSumOffLines;          // sum of off-line performances

	double		flObjBestSoFar;
	double		flObjSumOnLines;
	double          flObjSumOffLines;


	PTGAObj         pBestInd, pWorstInd;

	GeneticAlgorithm();

};

inline GeneticAlgorithm::GeneticAlgorithm()
{
}

inline  void GeneticAlgorithm::selectionScheme(PSelectionScheme pNewSch)
{
	delete pSSch;
	pSSch = pNewSch;
}

inline  void GeneticAlgorithm::crossover(PCrossover pNewXOver)
{
	delete pXover;
	pXover = pNewXOver;
}

inline  PCSelectionScheme GeneticAlgorithm::selectionScheme() const
{
	return pSSch;
}

inline  PCCrossover GeneticAlgorithm::crossover() const
{
	return pXover;
}

inline  PCScalingScheme GeneticAlgorithm::scaleScheme() const
{
	return pScale;
}

inline  int  GeneticAlgorithm::trials() const
{
	return nTrials;
}

inline  DblVec GeneticAlgorithm::onLines(BOOL returnObjVec)
{
        if (returnObjVec && pScale != NULL)
	    return objOnLineVec.at(0,nTrialsMonitored);
	else
	    return onLineVec.at(0,nTrialsMonitored);
}

inline  DblVec GeneticAlgorithm::offLines(BOOL returnObjVec)
{
        if (returnObjVec && pScale != NULL)
	    return objOffLineVec.at(0,nTrialsMonitored);
	else
	    return offLineVec.at(0,nTrialsMonitored);
}

inline  int   GeneticAlgorithm::trialsMonitored() const
{
	return nTrialsMonitored;
}

inline   void  GeneticAlgorithm::resetMonitor()
{
	 flBestSoFar = FLT_MIN;
	 flObjBestSoFar = FLT_MIN;
	 onLineVec.resize(50);
	 offLineVec.resize(50);
	 objOnLineVec.resize(0);
	 objOffLineVec.resize(0);
	 nTrialsMonitored = 0;
	 flSumOnLines = 0;
         flSumOffLines = 0;
	 flObjSumOnLines = 0;
	 flObjSumOffLines = 0;
}

inline  RCTGAObj  GeneticAlgorithm::bestInd() const
{
	return    *pBestInd;
}

inline  RCTGAObj  GeneticAlgorithm::worstInd() const
{
	return    *pWorstInd;
}

inline  RCPopulation   GeneticAlgorithm::pop() const
{
	return *pPop;
}

inline const SampleStatistic & GeneticAlgorithm::objValueStat() const
{
	return objStat;
}

inline const SampleStatistic & GeneticAlgorithm::fitnessStat() const
{
	return fitStat;
}

#endif

