//
//      TOLKIEN: Tool Kit for Genetics-based Applications
//
//      Anthony Yiu-Cheung Tang, 1993-94.
//      Department of Computer Science
//      Chinese University of Hong Kong
//
//      The classifiers used by MazeSolver are of following format :
//
//      (INPUT_TAG)(STATE) : (OUTPUT_TAG)(ACTION)
//
//      INPUT_TAG  (1 bit ) : 0
//      OUTPUT_TAG (1 bit ) : 1
//      STATE      (7 bits) : the current state of the animat (see below)
//      ACTION     (2 bits) :
//
//            There are four possible actions
//
//            00        UP
//            10        DOWN
//            01        LEFT
//            11        RIGHT
//
//      In this program a 5 by 6 maze is used, the maze looks like this:
//
//                      0000*G
//                      0*00*0
//                      S*00*0
//                      0*0000
//                      000000
//
//      S is the start state
//      G is the goal state
//      0 is an empty space
//      * is an obstacle
//
//
//      The animat starts from S, and tries to move to G.  It cannot move into
//      an obstacle or move out of the maze.
//
//      Each state is represented by an integer
//
//      For convience, the maze is represented by a 8 * 7 matrix
//
//      00 01 02 03 04 05 06 07
//      08 09 10 11 12 13 14 15
//      16 17 18 19 20 21 22 23
//      24 25 26 27 28 29 30 31
//      32 33 34 35 36 37 38 39
//      40 41 42 43 44 45 46 47
//      48 49 50 51 52 53 54 55
//
//      the upper left corner of the maze is state 09,
//      the lower right corner is 46
//
//      G corresponds to state 14
//      S corresponds to state 25
//
//      the reason for this representation is made clear in
//      the procedure for computing legal moves
//
#include <time.h>
#include <math.h>
#include <iostream.h>
#include <fstream.h>
#include "csfsys.h"
#include "tclsf.h"
#include "slctrw.h"
#include "message.h"

const   unsigned        M_ROWS      = 7;
const   unsigned        M_COLS      = 8;
const   unsigned        START_STATE = 25;
const   unsigned        GOAL_STATE  = 14;
const   unsigned        NUM_STATES  = M_ROWS * M_COLS;

BitString upBits, downBits, leftBits, rightBits;

const   unsigned short  MOVE_UP     = 0;
const   unsigned short  MOVE_LEFT   = 1;
const   unsigned short  MOVE_DOWN   = 2;
const   unsigned short  MOVE_RIGHT  = 3;

unsigned        uBaseStr = 10;
unsigned        uReward  = 50;

#define	STATE_BIT_LEN 	7
#define TAG_BIT_LEN     1
#define ACTION_BIT_LEN 	2
#define MSG_LEN         (STATE_BIT_LEN + TAG_BIT_LEN)

static  int mazeMap[NUM_STATES];
static  BitString stateBits[NUM_STATES];
fstream csfs_out, trails;

char    mazeBuf[NUM_STATES + 1];

#define EMPTYSPACE      '.'
#define OBSTACLE        '#'
#define WALL            '*'
#define ANIMAT          '@'

_CLASSDEF(TransRule)
class   TransRule : public TClassifier
{
public:
				TransRule();
                                TransRule(unsigned,unsigned,TritString,TritString);
                                TransRule(RCTransRule);

				_shallowCopy(TransRule)
                void            printOn( ostream  & out) const;
protected:
                unsigned        state, action; // for debugging purpose
};

class   MyMsgList : public MsgList
{
public :
		 MyMsgList(unsigned mlen,
			   BOOL dupMsgs = TRUE,
			   sizeType size = DEFAULT_COLLECTION_SIZE) :
			MsgList(mlen, dupMsgs, size)
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

                virtual BOOL add(PMessage);

};

BOOL MyMsgList::add(PMessage pMsg)
{
	if ( pMsg && (pMsgs->size() < maxMsgs) ) {
            if (! pMsgs->add((PTObject) pMsg)) {
                deleteMsg(pMsg);
                return FALSE;
            }
	    if (isEffectorMsg(*pMsg)) {
		if (emMax > 0)
		    if (eMsgs.size() >= emMax)
			return FALSE;
		eMsgs.add((PTObject) pMsg);
	    }
	    else
		if (isDetectorMsg(*pMsg)) {
		    if (dmMax > 0)
			if (dMsgs.size() >= dmMax)
			    return FALSE;
		    dMsgs.add((PTObject) pMsg);
		}
	    else {
		if (imMax > 0)
		    if (iMsgs.size() >= imMax)
                        return FALSE;
		iMsgs.add((PTObject) pMsg);
	    }
	    return TRUE;

	}
	else
            return FALSE;
}

TransRule::TransRule() : TClassifier()
{
	txn.resize(MSG_LEN);
        act.resize(MSG_LEN);
        txn.randomize();
	act.randomize();
	txn.setAt(0, 0);
	act.setAt(0, 1);
        fitness() = uBaseStr;
	findSpecificity();
	findLength();
}

TransRule::TransRule(RCTransRule src) : TClassifier(src),
                                        state(src.state),
					action(src.action)
{
}

TransRule::TransRule(unsigned uState,unsigned uAction,TritString t1,TritString t2) :
                TClassifier(t1,t2), state(uState), action(uAction)
{
        fitness() = uBaseStr;
}

void    TransRule::printOn(ostream& out) const
{
        BitString bits = txn;
	int nDir;

        bits = bits.at(TAG_BIT_LEN,STATE_BIT_LEN);
        out << bits.asLong() << " : ";
        bits = act;
        bits = bits.at(TAG_BIT_LEN,ACTION_BIT_LEN);
        nDir = bits.asLong();
        switch (nDir) {
           case MOVE_UP : out << "up";
                          break;
           case MOVE_LEFT : out << "left";
                          break;
           case MOVE_DOWN : out << "down";
			  break;
           case MOVE_RIGHT : out << "right";
                          break;
        }
        out << " [" << fitness() << ']';
}

class   MazeSolver : public ClassifierSystem
{
public:
	MazeSolver( PMsgList,
		    PSelectionScheme,
		    PCrossover);

	void    effectors();
	void    detectors();
        void    run();
        void    detectorCover();

private:

	virtual PTClassifier    auction(RCollection, RCMessage);

        unsigned verifyMove(int, int);
	unsigned moveDirection(RCMessage);
	unsigned msgState(RCMessage);
	void	 printMaze();
        void     normalize();
	unsigned        uCurState;
	unsigned        uPrevState;
	unsigned	uMoves;
        PTransRule      pPrevActiveRule;

        int             visitedStates[NUM_STATES];
        int             transRule[NUM_STATES];
        BitString       detectorMsg, effectorMsg; // template for messages
};

MazeSolver::MazeSolver(PMsgList         pMsgList,
		       PSelectionScheme pSch,
		       PCrossover       pCross) :
	    ClassifierSystem(pMsgList, pSch, pCross),
	    uCurState(START_STATE),
	    uPrevState(START_STATE),
            uMoves(0),
            pPrevActiveRule(NULL)
{
	oneMsgPCsf = TRUE;
        runBB(TRUE);
        bidRatio(0.1);
        bidTaxRate(0.05);
        lifeTaxRate(0);
        useSupport(FALSE);

        detectorMsg.resize(MSG_LEN);
        effectorMsg.resize(MSG_LEN);

        detectorMsg.clear(); // tag bit is zero
	effectorMsg.set();   // tag bit is one

        for (int nI = 0; nI < NUM_STATES; nI++)
             visitedStates[nI] = FALSE;
}

void    MazeSolver::effectors()
{
        int nNewState, nMsgState;
	char lineBuf[30];
	TOArray moves;
	PMessage pMsg = NULL;
	PClassifier pCsf;

	CollectionIterator iter(pNewMsgList->effectorMsgs());

	while (iter) {
	    // there should be only one message if oneMsgPCsf is TRUE
	    if ((nNewState =
                verifyMove(uCurState,
                           moveDirection(* (PCMessage) iter()))) != 0) {
		pMsg = (PMessage) iter();
            }
            iter++;
	}

	if (pMsg != NULL) {
            pPrevActiveRule = (PTransRule) pMsg->producer();
	    // a legal move is suggested
	    mazeBuf[uCurState] = EMPTYSPACE;
	    mazeBuf[nNewState] = ANIMAT;
	    uPrevState = uCurState;
	    if ((uCurState = nNewState) == GOAL_STATE)
                pPrevActiveRule->fitness() += uReward;
            detectorCover();
	    uMoves++;
	}
}

void    MazeSolver::detectors()
{
	msgs().removeAll();
	PMessage pMsg = msgs().newMsg();
        detectorMsg.at(TAG_BIT_LEN, STATE_BIT_LEN) = stateBits[uCurState];
        *pMsg = detectorMsg;
	pMsg->intensity(0);
        pMsg->producer((PCClassifier) pPrevActiveRule);
        msgs().add(pMsg);
}

unsigned MazeSolver::msgState(RCMessage msg)
{
	BitString bits =
	    ((RCBitString) msg).at(TAG_BIT_LEN, STATE_BIT_LEN);
        unsigned state = bits.asLong();
        return state;
}

unsigned MazeSolver::moveDirection(RCMessage msg)
//      it is assumed that msg is an effector message
{
#if defined (__BORLANDC__)
        BitString bits = ((RCBitString) msg).at(TAG_BIT_LEN, ACTION_BIT_LEN);
#else
        BitString bits = msg;
        bits = bits.at(TAG_BIT_LEN, ACTION_BIT_LEN);
#endif
        return bits.asLong();
}

unsigned MazeSolver::verifyMove(int curState, int moveDir)
{
        switch (moveDir) {
	     case MOVE_UP:
                  curState -= M_COLS;
		  break;
	     case MOVE_DOWN:
                  curState += M_COLS;
		  break;
	     case MOVE_LEFT:
                  curState -= 1;
		  break;
	     case MOVE_RIGHT:
                  curState += 1;
		  break;
	}

	if (mazeMap[curState] != 1)
            return curState;
        else
	    return 0;
}

PTClassifier MazeSolver::auction(RCollection availCsfs,
                                 RCMessage msg)
{
	CollectionIterator citer(availCsfs);
	PTClassifier  pCsf, pWinner = NULL;
	TOArray	      matched;
        BitString     dir;

	while (citer) {
               pCsf = (PTClassifier) citer();
               if (pCsf->taxon(0).matches(msg)) {
                   dir = pCsf->action();
                   dir = dir.at(TAG_BIT_LEN,ACTION_BIT_LEN);
                   if (uPrevState == verifyMove(uCurState, dir.asLong()))
                       pCsf->ebid() *= 0.1;
                   matched.add(pCsf);
               }
             ++citer;
	}

	if (matched.size() > 0) {
	   // there are classifiers that matched this message
	   // choose a winner
	   CollectionIterator titer(matched);
	   pWinner = (PTClassifier) titer();
           ++titer;
	   while (titer) {
		if (((PTClassifier) titer())->ebid() >
		    pWinner->ebid())
		    pWinner = (PTClassifier) titer();
                ++titer;
	   }
	}

	return pWinner;
}
void    MazeSolver::run()
{
	BOOL               printTrail;
	PClassifier        pCsf;
	PMessage           pMsg;
	CollectionIterator iter;
        double             flShare;
        unsigned           uNewLine = 0;

        if (nTrials++ % 5 == 0)
            printTrail = TRUE;
	else
            printTrail = FALSE;
	detectorCover();
	printMaze();
        if (printTrail)
            trails << nTrials << endl << "-------------" << endl;
	while (uCurState != GOAL_STATE) {
             if (printTrail) {
                 trails << uCurState << '.';
                 if (uNewLine++ > 9) {
                     trails << endl;
                     uNewLine = 0;
                 }
             }
             preProcessing();
	     detectors();
	     produceMsgs();
	     updateCsfStr();
	     effectors();
	     printMaze();
	     swapMsgList();
	}
        if (printTrail) {
            trails << GOAL_STATE << endl;
            trails << "Number of moves (" << uMoves << ')' << endl << endl;
        }

	mazeBuf[GOAL_STATE] = EMPTYSPACE;
	uCurState = START_STATE;        // return to start
	uMoves = 0;
	msgs().removeAll();
        pPrevActiveRule = NULL;
        if (nTrials % 2 == 0)
            normalize();
}

void    MazeSolver::normalize()
//
//      linearly normalize the strength of the classifiers such that
//      the classifier with the smallest strength has strength = uBaseStr
//
{
        double  flDiff;
	csfList.sort(decFitness);
        flDiff = ((PCTGAObj) csfList.at(csfList.size() - 1))->fitness() - uBaseStr;

        CollectionIterator iter(csfList);
        while (iter) {
                ((PTGAObj) iter())->fitness() -= flDiff;
                iter++;
        }
}

void    MazeSolver::detectorCover()
//
//      create at most four transitions rules correpsonds
//      to the four possible directions
//
{
        TritString  taxon(detectorMsg), action(effectorMsg);
	PClassifier pNewCls;

	if (visitedStates[uCurState] == TRUE) // this state has been visited
            return;
	else
            visitedStates[uCurState] = TRUE;

	if (verifyMove(uCurState,MOVE_UP) != 0) {
	    // add transition rule 0.CUR_STATE : 1.MOVE_UP
	    taxon.at(TAG_BIT_LEN, STATE_BIT_LEN) = stateBits[uCurState];
	    action.at(TAG_BIT_LEN, ACTION_BIT_LEN) = upBits;
	    pNewCls = new TransRule(uCurState, MOVE_UP,taxon, action);
	    csfs().add(pNewCls);
	}
	if (verifyMove(uCurState,MOVE_DOWN) != 0) {
	    // add transition rule 0.CUR_STATE : 1.MOVE_DOWN
	    taxon.at(TAG_BIT_LEN, STATE_BIT_LEN) = stateBits[uCurState];
	    action.at(TAG_BIT_LEN, ACTION_BIT_LEN) = downBits;
	    pNewCls = new TransRule(uCurState, MOVE_DOWN,taxon, action);
	    csfs().add(pNewCls);
	}
	if (verifyMove(uCurState,MOVE_LEFT) != 0) {
	    // add transition rule 0.CUR_STATE : 1.MOVE_LEFT
	    taxon.at(TAG_BIT_LEN, STATE_BIT_LEN) = stateBits[uCurState];
            action.at(TAG_BIT_LEN, ACTION_BIT_LEN) = leftBits;
            pNewCls = new TransRule(uCurState, MOVE_LEFT,taxon, action);
            csfs().add(pNewCls);
	}
	if (verifyMove(uCurState,MOVE_RIGHT) != 0) {
	    // add transition rule 0.CUR_STATE : 1.MOVE_RIGHT
	    taxon.at(TAG_BIT_LEN, STATE_BIT_LEN) = stateBits[uCurState];
	    action.at(TAG_BIT_LEN, ACTION_BIT_LEN) = rightBits;
	    pNewCls = new TransRule(uCurState, MOVE_RIGHT,taxon, action);
	    csfs().add(pNewCls);
	}
}

void    initMazeMap()
{
	int nI, nJ, nK, nL;

	// initialize obstacles
        mazeMap[18] = 1;
        mazeMap[26] = 1;
        mazeMap[34] = 1;

	mazeMap[13] = 1;
	mazeMap[21] = 1;
	mazeMap[29] = 1;

	mazeBuf[NUM_STATES] =  '\0';
        // make buffer null-terminated

        for (nI=0; nI < NUM_STATES; nI++)
                 if (mazeMap[nI] == 1)
                     mazeBuf[nI] = OBSTACLE;
		 else
                     mazeBuf[nI] = EMPTYSPACE;

	// the walls of the maze are also obstacles
        for (nI=0; nI < M_COLS; nI++) {
	     mazeMap[nI] = 1;
	     mazeBuf[nI] = WALL;
        }
	for (nI = (M_ROWS - 1) * M_COLS, nJ = M_COLS; nJ > 0; nI++, nJ--) {
	     mazeMap[nI] = 1;
	     mazeBuf[nI] = WALL;
	}
        for (nI = M_COLS, nJ = (M_ROWS - 1) * M_COLS; nI < nJ; nI+= M_COLS) {
	     mazeMap[nI] = 1;
	     mazeBuf[nI] = WALL;
        }
        for (nI = (M_COLS - 1); nI < NUM_STATES - 1; nI+= M_COLS) {
	     mazeMap[nI] = 1;
             mazeBuf[nI] = WALL;
	}
}

void	MazeSolver::printMaze()
{
}

void    initStateBits()
{
	int nI, nJ, nK, nL;
	for (nI=M_COLS + 1, nL = M_COLS * (M_ROWS - 1) + 1; nI < nL; nI+=M_COLS)
	     for (nJ=nI, nK=M_COLS - 2; nK > 0; nJ++, nK--) {
		  stateBits[nJ] = shorttoBitString(nJ);
		  stateBits[nJ].resize(STATE_BIT_LEN);
	     }
}

BOOL    isEmptySpace(int nState)
{
	if (nState < 0 || nState >= NUM_STATES)
	    return FALSE;
	else
            return (mazeMap[nState] != 1);
}

void	main(int argc, char *argv[])
{
        csfs_out.open("maze.csf",ios::out);
        trails.open("trails.out",ios::out);

	upBits    = shorttoBitString(MOVE_UP);
	downBits  = shorttoBitString(MOVE_DOWN);
	leftBits  = shorttoBitString(MOVE_LEFT);
	rightBits = shorttoBitString(MOVE_RIGHT);

	upBits.resize(2);
	downBits.resize(2);
	leftBits.resize(2);
	rightBits.resize(2);

	initMazeMap();
	initStateBits();

	mazeBuf[START_STATE] = ANIMAT;

	int      nTrials = 10;

	if (argc > 1)
            nTrials = atoi(argv[1]);
        if (argc > 2)
            uBaseStr = atoi(argv[2]);
        if (argc > 3)
            uReward = atoi(argv[3]);

        TritString      taxon(MSG_LEN,~RANDOMIZE), action(MSG_LEN,~RANDOMIZE);
        PClassifier     pCls;
        MazeSolver      mazeSolver(new MyMsgList(MSG_LEN, FALSE),
				   new RW_Select(),
				   new UniformCrossover(1));
	CollectionIterator iter;

	do {
	       mazeSolver.run();
               iter.restart(mazeSolver.csfs());
	       while (iter) {
                   csfs_out << *iter() << endl;
		   iter++;
	       }
               csfs_out << endl;
        } while (mazeSolver.trials() < nTrials);
}
