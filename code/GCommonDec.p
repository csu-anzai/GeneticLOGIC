UNIT GCommonDec;

INTERFACE

	USES
		DialogUtils, GFiles;

	CONST
		genieVersionNo = 1;		{For checking compatability of model files - it changes a bit}

		cGenieSign = 'Gnie';
		cModelFType = 'GMdl';
		cMaxFacts = 100;		{cMaxFacts MUST be equal to cMaxItns!!!!}
		cMaxItns = 100;
		cFactNameLen = 20;
		cWeightPrec = 4;
		cGraphWidth = 408;	{Divisible by 4 is best}
		cGraphHeight = 268;
		cGListCols = 4;
		cMaxGraphs = 50;
		cMaxGPoints = 12;

		cDefItns = 20;
		cDefStartVal = 1.0;


	TYPE

		TGrowthType = (add, mult, expon);
		TDblBoundRec = RECORD
				upper, lower: DOUBLE;
			END;
		TIntBoundRec = RECORD
				start, finish: INTEGER;
			END;
		TGPointRec = RECORD
				period: INTEGER;
				gVal: DOUBLE;
			END;
		TGPointArry = ARRAY[1..cMaxGPoints] OF TGPointRec;
		TScaledPtRec = RECORD
				pt: DOUBLE;
				val: DOUBLE;
			END;
		TScaledPtArry = ARRAY[1..cMaxGPoints] OF TScaledPtRec;
		TFactFlags = (canMod, gotLinks, crossDelay, delayImpact, graphIt, shocks, bounded, threshholds, variGrowth, targeted);
		TFlags = INTEGER;
		TFactName = STRING[cFactNameLen];
		TFactRec = RECORD
				factNum, factID: INTEGER;	{factID is a unique (in this model) value}
				name: TFactName;
				startVal, targetVal, altVal: DOUBLE;
				onLine: TIntBoundRec;
				gBounds, gTHolds: TDblBoundRec;	{Bounds and threshholds}
				gType: TGrowthType;		{Additive, multiplicative,exponential}
				factShocks: TGPointArry;	{Shock Points}
				vGrowths: TScaledPtArry;		{Variable growth points}
				flags: TFlags;
				yMax, yMin: DOUBLE;
				pad1, pad2, pad3, pad4: LONGINT;
			END;
		TFactArry = ARRAY[1..cMaxFacts] OF TFactRec;
		TFactsPtr = ^TFactArry;
		TFactsHdl = ^TFactsPtr;

		TCGArry = ARRAY[1..cMaxFacts, 1..cMaxFacts] OF DOUBLE; {Vector for 2D arrays}
		TCGPtr = ^TCGArry;
		TCGHdl = ^TCGPtr;

		TVector = ARRAY[0..cMaxFacts] OF DOUBLE;
		TVectPtr = ^TVector;
		TVectHdl = ^TVectPtr;

		TResArry = ARRAY[0..cMaxFacts] OF TVector;
		TResPtr = ^TResArry;
		TResHdl = ^TResPtr;

		TGraphRec = RECORD
				factID, grID: INTEGER;
				picHdl: PicHandle;
				name: TFactName;
				org, targPt: Point;
				grRect: Rect;
				xScale, yScale: DOUBLE;
				xMin, xMax: INTEGER;
				yMin, yMax: DOUBLE;
			END;
		TGraphArray = ARRAY[1..cMaxGraphs] OF TGraphRec;
		TGraphPtr = ^TGraphArray;
		TGraphHdl = ^TGraphPtr;

		TModelRec = RECORD
				nFacts: INTEGER;
				nextFactID: INTEGER;
				iteratns: INTEGER;
				dfltStartVal: DOUBLE;
				dfltGType: TGrowthType;
				enableFlags: TFlags;		{Enable functions like shocks, bounds, threshholds}
				numPrec: INTEGER;		{Numeric display precision}
				forceDelay, forceImmed: BOOLEAN;	{Cross growth processing}
				graphArray: TGraphHdl;		{Dont worry about this being here}
				crGrowths: TCGHdl;		{Growth table}
				lastResults: TResHdl;
				facts: TFactsHdl;
			END;
		TModelPtr = ^TModelRec;
		TModelHdl = ^TModelPtr;

		TWindType = (modelWind, graphWind, graphBrowse, waitWind, popnWind, noWInd);
		TSrchGrafType = (statsGraf, geneGraf);
		WDHandle = ^WDPtr;
		WDPtr = ^WindowData;
		WindowData = RECORD
				windType: TWindType;
				next, owner: WDHandle;
				wName: Str255;
				thisWindow: WindowPtr;
				hScroll, vScroll: ControlHandle;
				windDirty, revert: BOOLEAN;
				contRect: Rect;
				offsetPt: Point;
				CASE TWindType OF
					modelWind: (
							modelPic: PicHandle;	{Frames and stuff}
							factList, linkList: ListHandle;
							te1Rect, te2Rect: Rect;
							showValPopM: TPopMenuInfo;
							inNameTE: BOOLEAN;	{Which te is current}
							xStr: Str255;	{Non current text edit string}
							curFactor: INTEGER;	{Currently selected factor}
							teHdl: TEHandle;
							modelFile: TFile;
							theModel: TModelHdl;
							gBrowser: WDHandle;		{Associated graph browse windows}
					);
					graphWind: (
							pFrame: Rect;
							picHdl: PicHandle;
							grNum, grID: INTEGER;
					);
					graphBrowse: (
							gbList: ListHandle;
					);
					waitWind: (
							txtRect: Rect;
							ovRect: Rect;
							waitPic: PicHandle;
							prevWind: WindowPtr;
							initted: BOOLEAN
					);
					popnWind: (
							popnWindPic, rscPict: PicHandle;
							statRects: ARRAY[1..9] OF Rect;
							progRect, iconRect, pGraphRect, rescaleRect: Rect;
							showGraph: BOOLEAN;
							popnHdl: Handle;
							resID: INTEGER;
							pGraphInfo: TGraphRec;

					);

			END;

		WSPtr = ^WStateData;
		WSHdl = ^WSPtr;


{Factor list data}
		TListPics = RECORD
				modPic, linkPic, delayPic: PicHandle;
				targetPic, circPic, grafPic: PicHandle;
			END;
		TDisplayValue = (showImpacts, showAltVal, showStart, showActTarg, showAllTarg, none);
		TLDataRec = RECORD
				vScroll, hScroll: ControlHandle;	{To remember the scroll bar}
{lHeight,}
				lWidth: INTEGER;
				showFlags, indentFlags: TFlags;
				lPics: TListPics;
				whichVal: TDisplayValue;
			END;
		TLDataPtr = ^TLDataRec;
		TLDataHdl = ^TLDataPtr;

{Factor list cell data}
		TFactCellRec = RECORD
				factID, linkID, indent: INTEGER;
			END;

{Graph browser list cell data}
		TPicCell = RECORD
				graphNum: INTEGER;
			END;
		TPicCellPtr = ^TPicCell;
		TPicCellHdl = ^TPicCellPtr;

{Factor shock list cell data}
		TShockCell = RECORD
				pNum: INTEGER;
				shockVal: DOUBLE;
			END;


IMPLEMENTATION

END.