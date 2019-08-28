//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1992-93.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      A program developed using TOLKIEN to training a
//      classifier system for a simple multiplexer problem
//
//      The classifier used is the simplest one-taxon-one-action type :
//
//              (TAG)(INPUT) : (TAG)(OUTPUT)
//              1 bit  6 bits   1 bit  6 bits
//      more precisely,
//              0 d3 d2 d1 d0 a1 a0 : 1 output
//              where a1 a0 encodes which of the data line to be used :
//              a1 a0   data line used
//              ----------------------
//              0  0        d0
//              0  1        d1
//              1  0        d2
//              1  1        d3
//
//      The are two kinds of messages : detector and effector messages
//
//      detector message : tag = 0, the remaining 6 bits encode the
//                         six inputs
//
//      effector message : tag = 1, the bit following the tag bit
//                         is the output, the remaining five bits
//                         are not used
//
#include <time.h>
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "sortcltn.h"
#include "ga.h"
#include "populatn.h"
#include "hpldbin.h"
#include "message.h"
#include "tclsf.h"
#include "identdic.h"
#include "toarray.h"
#include "csfsys.h"
#include "dpldbin.h"
#include "slctrwwr.h"
#include "slctrw.h"
#include "slcttour.h"
#include "csfmatch.h"
#include "IntVec.h"

int     testSignal = 0;
int     correct = 0;
int     maxTrials = 1000;
int     inds = 100;
int     gapd = 200;
fstream msgout;
static BitString inputTag = atoBitString("0");
static BitString outputTag = atoBitString("1");

#define TAG_LEN         1
#define DATA_LEN        6
#define MPCLS_LEN       (TAG_LEN + DATA_LEN)

class   MyMsgList : public MsgList
{
public :
                 MyMsgList(unsigned mlen) : MsgList(mlen)
			   {
			   }
		virtual   BOOL  isEffectorMsg(RCMessage msg) const
                {
                        // the effector msg tag is 1
                        return msg.test(0);
                }

                virtual   BOOL  isDetectorMsg(RCMessage msg) const
                {
                        // the detector msg tag is 0
			return ! msg.test(0);
                }

                _shallowCopy(MyMsgList)
};

_CLASSDEF(MPcsf)
class   MPcsf : public TClassifier
{
public:
                                MPcsf();
                                MPcsf(TritString,TritString);
                                MPcsf(RCMPcsf);

				_shallowCopy(MPcsf)
	virtual void            printOn( ostream  & out) const;
};

MPcsf::MPcsf() : TClassifier()
{
        txn.resize(MPCLS_LEN);
        act.resize(MPCLS_LEN);
        txn.randomize();
        act.randomize();
        txn.setAt(0, inputTag[0]);
        act.setAt(0, outputTag[0]);
        fitness() = 10;
	findSpecificity();
	findLength();
}

MPcsf::MPcsf(RCMPcsf src) : TClassifier(src)
{
}

MPcsf::MPcsf(TritString t1,TritString t2) : TClassifier(t1,t2)
{
        fitness() = 10;
}

void    MPcsf::printOn( ostream  & out) const
//
//      Just print out the values of the six input lines and the
//      output line
//
{
/*
	TritString t = txn.at(1,6);
	TritString a = act.at(1,1);
        out << t << ':' << a << " [" << fitness() << ']';
*/
        TClassifier::printOn(out);
}

class   Multiplexer : public ClassifierSystem
{
public:
        Multiplexer(PMsgList,
		    PSelectionScheme,
                    PCrossover);
        void    detectors();
        void    effectors();
        void    run();
	void    finalResults();

protected :
	int nOutput;
	BOOL    gaflag;
        int     nextga;

	unsigned genOutput(RCMessage msg)
	{
           return msg.test(TAG_LEN);
	}

        unsigned decode(RCBitString msg)
        {
                int val = msg.test(5) * 2 + msg.test(6);
                switch (val) {
                        case 0 : return msg.test(4);
                        case 1 : return msg.test(3);
                        case 2 : return msg.test(2);
                        case 3 : return msg.test(1);
                }
        }
};

Multiplexer::Multiplexer(PMsgList         pMsgList,
			 PSelectionScheme pSch,
			 PCrossover       pCross) :
	ClassifierSystem(pMsgList, pSch, pCross),
        nextga(gapd)
{
	PClassifier     pCls;
	BitString       bits;
        Set     clset;

        runBB(TRUE);
	oneMsgPCsf = TRUE;
        bidSp(0.2);
        eBidSp(0.2);
        bidRatio(0.1);
        lifeTaxRate(0);
        bidTaxRate(0.01);
        useSupport(FALSE);

        for (int k = INT_MAX; clset.size() < inds && k >= 0; k--) {
	     pCls = (PClassifier) new MPcsf();
	     if ( ! clset.add(pCls) )
		 delete pCls;
	     else
		 cout << *pCls << endl;
	}

	csfs().addAll(clset);
	for (k = csfs().size(); k < inds; k++) {
             pCls = (PClassifier) new MPcsf();
	     csfs().add(pCls);
	     cout << *pCls << endl;
	}

        // parameters for De Jong's crowding replacement scheme
        crowdingFactor(3);
        crowdingSubPop(csfs().size() * 0.1);

/*      This commented code encodes a set of perfect rules
        according to Goldberg's book

        TritString t1, t2;

        t1 = "0###000";
        t2 = "1000000";
        csfs().add(new MPcsf(t1,t2));
        t1 = "0##0#01";
        t2 = "1000000";
        csfs().add(new MPcsf(t1,t2));
        t1 = "0#0##10";
        t2 = "1000000";
        csfs().add(new MPcsf(t1,t2));
        t1 = "00###11";
        t2 = "1000000";
        csfs().add(new MPcsf(t1,t2));
        t1 = "#######";
        t2 = "1100000";
        csfs().add(new MPcsf(t1,t2));
*/
}

void    Multiplexer::detectors()
{
        BitString       msg;

        testSignal = tRand(0,63);

        msgs().removeAll();
	msg = shorttoBitString(testSignal);
	msg = msg.at(0,6);
//        cout  << "Detector msg : " << msg << " [expected output = ";
	msg = inputTag + msg;
	PMessage pMsg = msgs().newMsg();
        *pMsg = msg;
        nOutput = decode(msg);
//        cout  << nOutput << "]" << endl;
	msgs().add(pMsg);
}

void 	Multiplexer::effectors()
{
	CollectionIterator iter(pNewMsgList->effectorMsgs());

	while (iter) {
		if (nOutput == genOutput(* ((PCMessage) iter()))) {
                    // pay reward to the classifier posting
                    // this message
                    ((PTGAObj) ((PMessage) iter())->producer())->fitness() += 1;
//                    cout <<  *iter() << endl;
		    correct++;
		    break;
		}
		iter++;
	}

        if (nTrials % 50 == 0)
            cout << nTrials << " : " << (100.0 * correct) / nTrials << " % correct" << endl;

        if (nextga == nTrials) {
            newClassifiers(0.2);
            nextga += gapd;
	}

	return;
}

void    Multiplexer::run()
{
	while (maxTrials-- > 0) {
             nTrials++;
             preProcessing();
	     detectors();
             produceMsgs();
             updateCsfStr();
	     effectors();
	     swapMsgList();
	}
}

void    Multiplexer::finalResults()
//
//      for all possible messages check the final classifier set
//      and print out the classifiers which match each message
//
{
        MsgList tmpMsgs(MPCLS_LEN, 10);
	CollectionIterator citer(csfList);
	CollectionIterator miter(matchSet);
	CollectionIterator msgIter(tmpMsgs);
	PMessage pMsg;
	BitString bits, msg;
	int     nMatch;

	csfList.sort(decFitness);

	while (citer) {
		((PClassifier) citer())->bid() =
		   ((PClassifier) citer())->ebid() = 0;
	      citer++;
	}

	for (int i = 0; i < 64; i++) {
	     msgs().removeAll();
             bits = shorttoBitString(i);
             bits = bits.at(0,6);
	     msg = inputTag + bits;
             nOutput = decode(msg);
	     pMsg = new Message(msg);
	     msgs().add(pMsg);
	     msgout << bits << " => " << nOutput << endl;
	     citer.restart();
	     nMatch = 0;
	     while (citer) {
		    tmpMsgs.removeAll();
		    matchSet.removeAll();
		    ((PClassifier) citer())->matchMsgs(msgs());
		    ((PCClassifier) citer())->matchList().post(tmpMsgs);
		    msgIter.restart();
		    while (msgIter) {
			   pMsg = (PMessage) msgIter();
			   if (nOutput == genOutput(*pMsg)) {
                                // this matching classifier
                                // produces the correct output
                                msgout << "    " << * pMsg->producer() << endl;
				nMatch++;
			   }
                           else
                                // this matching classifier
                                // doesn't produce the correct output
                                msgout << "x   " << * pMsg->producer() << endl;

			   msgIter++;
		    }
		    citer++;
	     }
	     if (nMatch == 0)
		 msgout << "    no matches" << endl;
	     else
		 correct++;
	}

	msgout << endl << csfs() << endl;
}

void    main(int argc, char *argv[])
{
	Dictionary testdict;

	float    flMRate = 0.05;
	float	 flXRate = 1.0;
	int      n = 1;

	msgout.open("result.out", ios::out);

	if (argc > 1) {
	    if (strstr(argv[1],"-h")) {
		cout << "Calling convention : " << argv[0];
		cout << " [number of trials | number of classifiers | ga_period | crossover rate | mutation rate ]" << endl;
		return;
	    }
	    else
		maxTrials = atoi(argv[1]);

	}
	if (argc > 2)
	    inds = atoi(argv[2]);
	if (argc > 3)
	    gapd = atoi(argv[3]);
	if (argc > 4)
	    flXRate = atof(argv[4]);
	if (argc > 5)
	    flMRate = atof(argv[5]);

	 noise.variance(0.075 * 0.075); // set std. dev of noise to 0.075

	 PMsgList        pMsgList = new MyMsgList(MPCLS_LEN);
	 Multiplexer     mtp(pMsgList,
			     (PSelectionScheme) new RW_Select(),
			     (PCrossover) new MultiPtCrossover(4, flXRate));
	mtp.mutRate(flMRate);

	mtp.run();
	cout << "Number of trials : " << mtp.trials() << endl;
        mtp.finalResults();
}
