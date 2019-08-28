UNIT Genetic;
INTERFACE

	USES
		SANE, DialogUtils, GFiles, GCommonDec, GGlobals, DialogUtils, GDialogs, GATypes, Simulation, GADialogs;

	PROCEDURE ShowGenePool (popn: TPopnHdl);
	PROCEDURE DrawPGraphAxes (theWD: WDHandle;
									xMn, xMx, yMn, yMx: DOUBLE);
	PROCEDURE ZoomPopnGraph (theWD: WDHandle;
									r: Rect);
	PROCEDURE PlotPopnStats (theWD: WDHandle);

	PROCEDURE StartSearch (theModel: TModelHdl);
	FUNCTION NewPopn (VAR popn: TPopnHdl): BOOLEAN;

	PROCEDURE ResetPopn (resetChroms: BOOLEAN;
									popn: TPopnHdl);
	PROCEDURE InitPopn (popn: TPopnHdl);
	PROCEDURE GoSearch (popWD: WDHandle);
	PROCEDURE DecodeChrom (chrom: TChromosome;
									VAR phenoType: TPhenoType;
									popn: TPopnHdl);

IMPLEMENTATION
{$S Genetic}

	PROCEDURE RunGenie (quiet: BOOLEAN);
	EXTERNAL;

	FUNCTION ShowGeneFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;

		CONST
			okBtn = 1;
			cancelBtn = 2;
			geneList = 3;

		VAR
			listVars: ListVarsHand;
			tmpBool, dblClick: BOOLEAN;
			aPoint: Point;
			theCell: Cell;
			ch: CHAR;

	BEGIN
	listVars := ListVarsHand(GetWRefCon(theDialog));
	tmpBool := FALSE;
	IF DoListEvent(theEvent, FALSE, TRUE, dblClick, listVars^^.list1) THEN
		BEGIN
		itemHit := geneList;
		tmpBool := TRUE;
		END
	ELSE
		WITH theEvent DO
			BEGIN
			IF what = mouseDown THEN
				BEGIN
				aPoint := where;
				GlobalToLocal(aPoint);
				IF PtInRect(aPoint, listVars^^.rect2) THEN
					BEGIN		{Only for the scroll bar}
					IF aPoint.v >= (listVars^^.rect2.bottom - 15) THEN
						BEGIN
						IF LClick(aPoint, modifiers, listVars^^.list2) THEN
							;
						END
					ELSE
						SysBeep(5);
					END;
				END
			ELSE IF what = activateEvt THEN
				BEGIN
				IF BitAnd(modifiers, activeFlag) <> 0 THEN	{ if an activate }
					LActivate((BitAnd(modifiers, activeFlag) <> 0), listVars^^.list1);
				END
			ELSE IF what IN [keyDown, autokey] THEN
				BEGIN
				ch := CHR(BITAND(message, charCodeMask));
				IF ch IN [CHR($0D), CHR(3)] THEN
					BEGIN
					itemHit := okBtn;
					tmpBool := TRUE;
					END;
				END;
			END;
	ShowGeneFilter := tmpBool;
	END; {ShowGeneFilter}

	PROCEDURE ShowGenePool (popn: TPopnHdl);
{It's a dialog, but it needs access to the decoding procedure}
		CONST
			showGenesID = 1102;
			okBtn = 1;
			outlineItem = 2;

			geneList = 3;
			phenoList = 9;
			setBtn = 11;

		VAR
			theDialog: DialogPtr;
			stor: DialogRecord;
			savePort: GrafPtr;
			dummy, itemHit: INTEGER;
			itemHdl: Handle;
			itemBox: Rect;
			str: Str255;
			listVars: ListVarsHand;
			curSelLine: INTEGER;
			theCell: Cell;
			i, j, dLen: INTEGER;
			phenoArray: TPhenoType;

		PROCEDURE BuildLists;
			VAR
				theCell: Cell;
				cSize: Point;
				dBnds: Rect;
				i, factNo: INTEGER;
				str: Str255;
		BEGIN
		GetDItem(theDialog, geneList, dummy, itemHdl, itemBox);
		listVars^^.item1 := geneList;
		listVars^^.rect1 := itemBox;
		WITH listVars^^.rect1 DO
			right := right - 15;
		SetPt(cSize, 0, 0);
		SetRect(dBnds, 0, 0, 4, 0);
		listVars^^.list1 := LNew(listVars^^.rect1, dBnds, cSize, cRowListID, theDialog, FALSE, FALSE, FALSE, TRUE);
		listVars^^.list1^^.selFlags := lOnlyOne;
		listVars^^.rect1 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, geneList, dummy, Handle(@UpdateListItem), itemBox);

		SetPt(theCell, 0, 0);
		FOR i := 1 TO popn^^.nChrom DO
			BEGIN
			theCell.v := LAddRow(1, 999, listVars^^.list1);
			theCell.h := 0;
			NumToString(popn^^.theChroms[i].geneID, str);
			LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);

			theCell.h := 1;
			NumToString(popn^^.theChroms[i].parent1, str);
			LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);

			theCell.h := 2;
			NumToString(popn^^.theChroms[i].parent2, str);
			LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);

			theCell.h := 3;
			str := NumToDecStr(popn^^.theModel^^.numPrec, popn^^.theChroms[i].fitness);
			LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);
			END;
		LDoDraw(TRUE, listVars^^.list1);
		LUpdate(theDialog^.visRgn, listVars^^.list1);

{Phenotype list}
		GetDItem(theDialog, phenoList, dummy, itemHdl, itemBox);
		listVars^^.item2 := phenoList;
		listVars^^.rect2 := itemBox;
		WITH listVars^^.rect2 DO
			bottom := bottom - 15;
		SetPt(cSize, 90, 30);
		SetRect(dBnds, 0, 0, popn^^.nGenes, 2);
		listVars^^.list2 := LNew(listVars^^.rect2, dBnds, cSize, cRowListID, theDialog, FALSE, FALSE, TRUE, FALSE);
		listVars^^.list2^^.selFlags := lOnlyOne;
		listVars^^.rect2 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right;
		SetDItem(theDialog, phenoList, userItem, Handle(@UpdateListItem), itemBox);
		FOR i := 1 TO popn^^.nGenes DO
			IF FactIDIdx(popn^^.theModel, popn^^.phenoParms[i].factID, factNo) THEN
				BEGIN
				SetPt(theCell, (i - 1), 0);
				str := popn^^.theModel^^.facts^^[factNo].name;
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list2);
				END;
		LDoDraw(TRUE, listVars^^.list2);
		LUpdate(theDialog^.visRgn, listVars^^.list2);
		END; {BuildLists}

		PROCEDURE SetPhenoLine;
			VAR
				theCell: Cell;
				str: Str255;
				i: INTEGER;
		BEGIN
		LDoDraw(FALSE, listVars^^.list2);
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, theCell, listVars^^.list1) THEN
			BEGIN
			i := theCell.v;
			SetPt(theCell, 0, 0);
			WHILE LGetSelect(TRUE, theCell, listVars^^.list1) DO
				BEGIN
				LSetSelect(FALSE, theCell, listVars^^.list1);
				IF theCell.h <= (listVars^^.list2^^.dataBounds.right) THEN
					theCell.h := theCell.h + 1
				ELSE
					BEGIN
					theCell.h := 0;
					theCell.v := theCell.v + 1;
					END;
				END;
			SetPt(theCell, 0, i);
			LSetSelect(TRUE, theCell, listVars^^.list1);
			SetDCtlHilite(theDialog, setBtn, TRUE);
			DecodeChrom(popn^^.theChroms[i + 1], phenoArray, popn);
			FOR i := 1 TO popn^^.nGenes DO
				BEGIN
				SetPt(theCell, (i - 1), 1);
				str := NumToDecStr(popn^^.theModel^^.numPrec, phenoArray[i]);
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list2);
				END;
			END
		ELSE
			SetDCtlHilite(theDialog, setBtn, FALSE);
		LDoDraw(TRUE, listVars^^.list2);
		LUpdate(theDialog^.visRgn, listVars^^.list2);
		END; {SetPhenoLine}

		PROCEDURE SetModelVals;
			VAR
				i: INTEGER;
				factNo: INTEGER;
		BEGIN
		FOR i := 1 TO popn^^.nGenes DO
			IF FactIDIdx(popn^^.theModel, popn^^.phenoParms[i].factID, factNo) THEN
				popn^^.theModel^^.facts^^[factNo].altVal := phenoArray[i];
		END; {SetModelVals}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(showGenesID, @stor, windowPtr(-1));
	listVars := ListVarsHand(NewHandle(SizeOf(ListVarsRec)));
	SetWRefCon(theDialog, LONGINT(listVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, UserItem, HANDLE(@OutlineButton), itemBox);

	BuildLists;
	SetPt(theCell, 0, 0);
	LSetSelect(TRUE, theCell, listVars^^.list1);
	SetPhenoLine;

	ShowWindow(theDialog);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@ShowGeneFilter, itemHit);
		CASE itemHit OF
		geneList: 
			SetPhenoLine;
		SetBtn: 
			SetModelVals;
		END;
	UNTIL itemHit = okBtn;


	LDispose(listVars^^.list1);
	LDispose(listVars^^.list2);
	DisposHandle(Handle(listVars));
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {ShowGenePool}


	PROCEDURE DrawPGraphAxes (theWD: WDHandle;
									xMn, xMx, yMn, yMx: DOUBLE);

		VAR
			i: INTEGER;
			x, y: pVector;
			yMin, yMax, xMin, xMax, step: DOUBLE;
			pErr: PError;
			thePt, tr: Point;
			savePort: GrafPtr;
			savePicSave: Handle;
			r: Rect;
			gPort: GrafPort;
			popn: TPopnHdl;

	BEGIN
	IF theWD^^.windType = popnWind THEN
		BEGIN
		popn := TPopnHdl(theWD^^.popnHdl);
		GetPort(savePort);
		OpenPort(@gPort);	{Create off screen GrafPort}
		InitPort(@gPort);
		gPort.portBits.bounds := screenBits.bounds;
		WITH screenBits.bounds DO
			OffsetRect(gPort.portBits.bounds, (right - left + 10), (bottom - top + 10));
		PortSize((WDWidth(theWD) + 16), (WDHeight(theWD) + 16));
		SetPort(@gPort);
		ClipRect(gPort.portRect);
		TextFont(monaco);
		TextSize(9);

		PlotInit;
		WITH theWD^^.pGraphRect DO
			SetPlotRect(left, top, right, bottom);
		IF (TRUNC(xMx) = TRUNC(xMn)) THEN
			BEGIN
			xMax := popn^^.maxGenerations;
			xMin := 1.0;
			END
		ELSE
			BEGIN
			xMin := TRUNC(xMn);
			xMax := TRUNC(xMx);
			END;
		step := FindNiceStep(xMin, xMax, TRUE, 15, 999);
		SetXTicks(xMin, step, xMax);
		IF (popn^^.nChrom < cPopulationMin) OR (popn^^.fitMax = INF) THEN
			BEGIN

			END
		ELSE
			BEGIN
			IF (yMx = yMn) THEN
				BEGIN
				yMax := popn^^.fitMax;
				yMin := 0.0;
				END
			ELSE
				BEGIN
				yMax := yMx;
				yMin := yMn;
				END;
			step := FindNiceStep(yMin, yMax, FALSE, 5, 999);
			SetYTicks(yMin, step, yMax);
			END;
		NoXGrids;
		NoYGrids;
		DrawXYAxes(2);
		SetTickFont(monaco, monaco);
		SetTickSizes(9, 9);
		SetTickPrecision(0, 2);

		theWD^^.pGraphInfo.yMax := yMax;
		theWD^^.pGraphInfo.yMin := yMin;
		theWD^^.pGraphInfo.xMin := TRUNC(xMin);
		theWD^^.pGraphInfo.xMax := TRUNC(xMax);
		theWD^^.pGraphInfo.name := '';
		theWD^^.pGraphInfo.factID := -1;
		theWD^^.pGraphInfo.grID := -1;

		IF theWD^^.pGraphInfo.picHdl <> NIL THEN
			KillPicture(theWD^^.pGraphInfo.picHdl);
		theWD^^.pGraphInfo.picHdl := OpenPicture(theWD^^.pGraphRect);
		x[1] := xMin;
		y[1] := yMin;
		PlotIn(1, x, y, pErr);
		ClosePicture;

{Determine the actual plot rectangle}
{    MoveHHi(Handle(theWD));}
		HLock(Handle(theWD));
		WITH theWD^^.pGraphInfo, grRect DO
			BEGIN
			PlotReturn(xMin, yMin, org.h, org.v, pErr);
			PlotReturn(xMax, yMax, tr.h, tr.v, pErr);
			top := tr.v;
			left := org.h;
			bottom := org.v;
			right := tr.h;
			xScale := (right - left) / (xMax - xMin);
			yScale := (top - bottom) / (yMax - yMin);
			END;
		HUnlock(Handle(theWD));

		SetPort(savePort);
		ClosePort(@gPort);
		EraseRect(theWD^^.pGraphRect);
		InvalRect(theWD^^.pGraphRect);
		ClipRect(savePort^.portRect);
		END;
	END; {DrawPGraphAxes}

	PROCEDURE ZoomPopnGraph (theWD: WDHandle;
									r: Rect);
		VAR
			xMn, xMx, yMn, yMx: DOUBLE;
	BEGIN
	IF NOT EmptyRect(r) THEN
		BEGIN
		WITH r, theWD^^.pGraphInfo DO
			BEGIN
			xMn := (left - org.h) / xScale + xMin;
			xMx := (right - org.h) / xScale + xMin;
			yMn := (bottom - org.v) / yScale + yMin;
			yMx := (top - org.v) / yScale + yMin;
			END;
		DrawPGraphAxes(theWD, xMn, xMx, yMn, yMx);
		END;
	END; {RescalePopnGraph}


	PROCEDURE PlotPopnStats (theWD: WDHandle);
		VAR
			i, xPt, yPt, xxPt: INTEGER;
			popn: TPopnHdl;

	BEGIN
	IF theWD^^.windType = popnWind THEN
		IF theWD^^.showGraph AND (theWD^^.pGraphInfo.picHdl <> NIL) THEN
			BEGIN
			ClipRect(theWD^^.pGraphInfo.grRect);
			PenSize(2, 2);
			popn := TPopnHdl(theWD^^.popnHdl);
			IF popn^^.genNum > 1 THEN
				BEGIN
				IF popn^^.bestFitStats <> NIL THEN
					BEGIN
					PenPat(black);
					WITH theWD^^.pGraphInfo DO
						BEGIN
						IF 1 < xMin THEN
							xPt := org.h
						ELSE
							xPt := ROUND(org.h + ((1 - xMin) * xScale));
						IF popn^^.bestFitStats^^[1] < yMin THEN
							yPt := org.v
						ELSE IF popn^^.bestFitStats^^[1] > yMax THEN
							yPt := grRect.top
						ELSE
							yPt := ROUND(org.v + ((popn^^.bestFitStats^^[1] - yMin) * yScale));
						END;
					xxPt := xPt;
					MoveTo(xPt, yPt);
					FOR i := 2 TO popn^^.genNum DO
						BEGIN
						WITH theWD^^.pGraphInfo DO
							BEGIN
							IF i < xMin THEN
								xPt := org.h
							ELSE IF i > xMax THEN
								xPt := grRect.right
							ELSE
								xPt := ROUND(org.h + ((i - xMin) * xScale));
							IF popn^^.bestFitStats^^[i] < yMin THEN
								yPt := org.v
							ELSE IF popn^^.bestFitStats^^[i] > yMax THEN
								yPt := grRect.top
							ELSE
								yPt := ROUND(org.v + ((popn^^.bestFitStats^^[i] - yMin) * yScale));
							END;
						LineTo(xPt, yPt);
						END;
					END;

				IF popn^^.worstFitStats <> NIL THEN
					BEGIN
					PenPat(ltGray);
					WITH theWD^^.pGraphInfo DO
						BEGIN
						IF 1 < xMin THEN
							xPt := org.h
						ELSE
							xPt := ROUND(org.h + ((1 - xMin) * xScale));
						IF popn^^.worstFitStats^^[1] < yMin THEN
							yPt := org.v
						ELSE IF popn^^.worstFitStats^^[1] > yMax THEN
							yPt := grRect.top
						ELSE
							yPt := ROUND(org.v + ((popn^^.worstFitStats^^[1] - yMin) * yScale));
						END;
					MoveTo(xPt, yPt);
					FOR i := 2 TO popn^^.genNum DO
						BEGIN
						WITH theWD^^.pGraphInfo DO
							BEGIN
							IF i < xMin THEN
								xPt := org.h
							ELSE IF i > xMax THEN
								xPt := grRect.right
							ELSE
								xPt := ROUND(org.h + ((i - xMin) * xScale));
							IF popn^^.worstFitStats^^[i] < yMin THEN
								yPt := org.v
							ELSE IF popn^^.worstFitStats^^[i] > yMax THEN
								yPt := grRect.top
							ELSE
								yPt := ROUND(org.v + ((popn^^.worstFitStats^^[i] - yMin) * yScale));
							END;
						LineTo(xPt, yPt);
						END;
					END;

				IF popn^^.avgFitStats <> NIL THEN
					BEGIN
					PenPat(dkGray);
					WITH theWD^^.pGraphInfo DO
						BEGIN
						IF 1 < xMin THEN
							xPt := org.h
						ELSE
							xPt := ROUND(org.h + ((1 - xMin) * xScale));
						IF popn^^.avgFitStats^^[1] < yMin THEN
							yPt := org.v
						ELSE IF popn^^.avgFitStats^^[1] > yMax THEN
							yPt := grRect.top
						ELSE
							yPt := ROUND(org.v + ((popn^^.avgFitStats^^[1] - yMin) * yScale));
						END;
					MoveTo(xPt, yPt);
					FOR i := 2 TO popn^^.genNum DO
						BEGIN
						WITH theWD^^.pGraphInfo DO
							BEGIN
							IF i < xMin THEN
								xPt := org.h
							ELSE IF i > xMax THEN
								xPt := grRect.right
							ELSE
								xPt := ROUND(org.h + ((i - xMin) * xScale));
							IF popn^^.avgFitStats^^[i] < yMin THEN
								yPt := org.v
							ELSE IF popn^^.avgFitStats^^[i] > yMax THEN
								yPt := grRect.top
							ELSE
								yPt := ROUND(org.v + ((popn^^.avgFitStats^^[i] - yMin) * yScale));
							END;
						LineTo(xPt, yPt);
						END;
					END;
				END;
			ClipRect(theWD^^.thisWindow^.portRect);
			PenNormal;
			END;
	END; {PlotPopnStats}

	PROCEDURE UpdateStatus (popWD: WDHandle);
	BEGIN
	SelectWindow(popWD^^.thisWindow);
	SetPort(popWD^^.thisWindow);
	InvalRect(popWD^^.thisWindow^.portRect);
{InvalRect(popWD^^.statRects[1]);}
{InvalRect(popWD^^.statRects[2]);}
{InvalRect(popWD^^.statRects[3]);}
{InvalRect(popWD^^.statRects[4]);}
	RunGenie(TRUE);
	END; {UpdateStatus}

{************}
	FUNCTION Rand: DOUBLE;
	BEGIN	{PseudoRandom, uniform dist, 0..1}
	Rand := RandomX(rndSeed) / (Scalb(31, 1) - 1);
	END; {Rand}

	FUNCTION Flip (flipProb: DOUBLE): BOOLEAN;
	BEGIN
	IF rand <= flipProb THEN
		Flip := TRUE
	ELSE
		Flip := FALSE;
	END; {Flip}

	FUNCTION RndI (lower, upper: INTEGER): INTEGER;
	BEGIN	{PseudoRandom, uniform dist, lower..upper}
	RndI := Round(lower + ((upper - lower) * Rand));
	END; {RndI}

	FUNCTION RndX (lower, upper: DOUBLE): DOUBLE;
	BEGIN	{PseudoRandom, uniform dist, lower..upper}
	RndX := lower + ((upper - lower) * Rand);
	END; {RndX}


	FUNCTION NewGene (insert: INTEGER;
									VAR gID: INTEGER;
									popn: TPopnHdl): INTEGER;
		VAR
			i, gNum: INTEGER;
	BEGIN
	IF (popn^^.nChrom >= popn^^.popMax) OR (insert > (popn^^.nChrom + 1)) THEN
		NewGene := 0
	ELSE
		BEGIN
		IF insert <> 0 THEN
			BEGIN
			IF insert <= popn^^.nChrom THEN
				FOR i := (popn^^.nChrom + 1) DOWNTO insert DO
					popn^^.theChroms[i + 1] := popn^^.theChroms[i];
			gNum := insert;
			END
		ELSE
			gNum := popn^^.nChrom + 1;
		popn^^.nChrom := popn^^.nChrom + 1;
		popn^^.nextID := popn^^.nextID + 1;
		WITH popn^^.theChroms[gNum] DO
			BEGIN
			geneID := popn^^.nextID;
			gID := popn^^.nextID;
			fitness := INF;
			parent1 := 0;
			parent2 := 0;
			FOR i := 1 TO cMaxGenes DO
				bits[i] := 0;
			END;
		NewGene := gNum;
		END;
	END; {NewGene}

	PROCEDURE AddChrom (VAR chrom: TChromosome;
									popn: TPopnHdl);
		VAR
			gNum, i: INTEGER;
	BEGIN
	IF popn^^.selectMethod IN [rouletteS, randomS] THEN
		BEGIN
		gNum := NewGene(0, chrom.geneID, popn);
		popn^^.theChroms[gNum] := chrom;
		END
	ELSE IF popn^^.selectMethod IN [fitFitS, fitWeakS] THEN
		BEGIN
		i := 1;
		WHILE (chrom.fitness > popn^^.theChroms[i].fitness) AND (i <= popn^^.nChrom) DO
			i := i + 1;
		gNum := NewGene(i, chrom.geneID, popn);
		popn^^.theChroms[gNum] := chrom;
		END;
	IF chrom.fitness < popn^^.fitMin THEN
		BEGIN
		popn^^.fitMin := chrom.fitness;
		popn^^.fitMinGene := gNum;
		END;
	IF chrom.fitness > popn^^.fitMax THEN
		BEGIN
		popn^^.fitMax := chrom.fitness;
		popn^^.fitMaxGene := gNum;
		END;
	popn^^.fitSum := popn^^.fitSum + chrom.fitness;
	END; {AddChrom}


	PROCEDURE RecalcStats (popn: TPopnHdl);
		VAR
			i: INTEGER;
	BEGIN
	popn^^.fitMin := INF;
	popn^^.fitMax := -INF;
	popn^^.fitSum := 0.0;
	FOR i := 1 TO popn^^.nChrom DO
		WITH popn^^, theChroms[i] DO
			BEGIN
			IF fitness < fitMin THEN
				BEGIN
				fitMin := fitness;
				fitMinGene := i;
				END;
			IF fitness > fitMax THEN
				BEGIN
				fitMax := fitness;
				fitMaxGene := i;
				END;
			fitSum := fitSum + fitness;
			END;
	END; {RecalcStats}


	PROCEDURE DelGene (whichGene: INTEGER;
									popn: TPopnHdl);
		VAR
			i: INTEGER;

	BEGIN
	IF whichGene <= popn^^.nChrom THEN
		BEGIN
		popn^^.nChrom := popn^^.nChrom - 1;
		popn^^.fitSum := popn^^.fitSum - popn^^.theChroms[whichGene].fitness;
		IF whichGene <= popn^^.nChrom THEN
			FOR i := whichGene TO popn^^.nChrom DO
				popn^^.theChroms[i] := popn^^.theChroms[i + 1];
		IF (whichGene = popn^^.fitMinGene) OR (whichGene = popn^^.fitMaxGene) THEN
			RecalcStats(popn)		{Just zapped either the best or worst gene}
		ELSE
			BEGIN
			IF popn^^.fitMinGene > whichGene THEN
				popn^^.fitMinGene := popn^^.fitMinGene - 1;
			IF popn^^.fitMaxGene > whichGene THEN
				popn^^.fitMaxGene := popn^^.fitMaxGene - 1;
			END;
		END;
	END; {DelGene}


	FUNCTION Mutation (allele: BOOLEAN;
									popn: TPopnHdl): BOOLEAN;

	BEGIN
	IF Flip(popn^^.pMut) THEN
		BEGIN
		Mutation := NOT allele;
		popn^^.nMut := popn^^.nMut + 1;
		END
	ELSE
		Mutation := allele;
	END; {Mutation}

	PROCEDURE CrossOver (p1, p2: TChromosome;
									VAR c1, c2: TChromosome;
									geneNo: INTEGER;
									popn: TPopnHdl);
		VAR
			cross1, cross2, tmp, i, chromLen: INTEGER;

	BEGIN
	c1.bits[geneNo] := 0;
	c2.bits[geneNo] := 0;
	chromLen := popn^^.phenoParms[geneNo].res;
	IF Flip(popn^^.pCross) THEN
		BEGIN
		cross1 := RNDI(1, chromLen - 1);
		IF popn^^.phenoParms[geneNo].crossSites = twoSiteC THEN
			cross2 := RNDI(2, chromLen)
		ELSE
			cross2 := cross1;
		popn^^.nCross := popn^^.nCross + 1;
		IF cross1 > cross2 THEN
			BEGIN
			tmp := cross1;
			cross1 := cross2;
			cross2 := tmp;
			END;
		FOR i := 0 TO (cross1 - 1) DO
			BEGIN
			IF Mutation(BITTST(@p1.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c1.bits[geneNo], cGeneSize - i);
			IF Mutation(BITTST(@p2.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c2.bits[geneNo], cGeneSize - i);
			END;
		IF cross1 <> cross2 THEN
			FOR i := cross1 TO (cross2 - 1) DO
				BEGIN
				IF Mutation(BITTST(@p1.bits[geneNo], cGeneSize - i), popn) THEN
					BitSet(@c2.bits[geneNo], cGeneSize - i);
				IF Mutation(BITTST(@p2.bits[geneNo], cGeneSize - i), popn) THEN
					BitSet(@c1.bits[geneNo], cGeneSize - i);
				END;
		FOR i := cross2 TO (chromLen - 1) DO
			BEGIN
			IF Mutation(BITTST(@p1.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c1.bits[geneNo], cGeneSize - i);
			IF Mutation(BITTST(@p2.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c2.bits[geneNo], cGeneSize - i);
			END;
		END
	ELSE {Straight Copy}
		BEGIN
		FOR i := 0 TO (chromLen - 1) DO
			BEGIN
			IF Mutation(BITTST(@p1.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c1.bits[geneNo], cGeneSize - i);
			IF Mutation(BITTST(@p2.bits[geneNo], cGeneSize - i), popn) THEN
				BitSet(@c2.bits[geneNo], cGeneSize - i);
			END;
		END;
	END; {CrossOver}

	PROCEDURE MateChroms (p1, p2: TChromosome;
									VAR c1, c2: TChromosome;
									popn: TPopnHdl);
		VAR
			thisGene: INTEGER;
	BEGIN
	c1.parent1 := p1.geneID;
	c1.parent2 := p2.geneID;
	c2.parent1 := p1.geneID;
	c2.parent2 := p2.geneID;

	FOR thisGene := 1 TO popn^^.nGenes DO
		CrossOver(p1, p2, c1, c2, thisGene, popn);
	END; {MateChroms}

	PROCEDURE EncodeChrom (VAR chrom: TChromosome;
									phenoType: TPhenoType;
									popn: TPopnHdl);
		VAR
			i, j: INTEGER;
			step: DOUBLE;
	BEGIN
	FOR i := 1 TO cMaxGenes DO
		chrom.bits[i] := 0;
	FOR i := 1 TO popn^^.nGenes DO
		BEGIN
		CASE popn^^.phenoParms[i].codeMethod OF
		binaryC: 
			BEGIN
			WITH popn^^.phenoParms[i] DO
				BEGIN
				step := (upper - lower) / (XPwrI(2.0, res) - 1);
				chrom.bits[i] := ROUND((phenoType[i] - lower) / step);
				END;
			END;
		END;
		END;
	END; {EncodeChrom}

	PROCEDURE DecodeChrom (chrom: TChromosome;
									VAR phenoType: TPhenoType;
									popn: TPopnHdl);
		VAR
			i, j: INTEGER;
			step: DOUBLE;
	BEGIN
	FOR i := 1 TO popn^^.nGenes DO
		BEGIN
		phenoType[i] := 0.0;
		CASE popn^^.phenoParms[i].codeMethod OF
		binaryC: 
			BEGIN
			WITH popn^^.phenoParms[i] DO
				step := (upper - lower) / (XPwrI(2.0, res) - 1);
			phenoType[i] := popn^^.phenoParms[i].lower + (step * chrom.bits[i]);
			END;
		END;
		END;
	END; {DecodeChrom}

	PROCEDURE Fitness (VAR chrom: TChromosome;
									popn: TPopnHdl;
									seedRun: BOOLEAN);
		VAR
			factNo, thisGene, itn: INTEGER;
			sum: DOUBLE;
			theModel: TModelHdl;
			phenoType: TPhenoType;
	BEGIN
	theModel := popn^^.theModel;
	DecodeChrom(chrom, phenoType, popn);
	FOR thisGene := 1 TO popn^^.nGenes DO
		FOR itn := 1 TO popn^^.theModel^^.iteratns DO
			popn^^.growths^^[itn, popn^^.phenoParms[thisGene].factNo] := phenoType[thisGene];
	RunSimulation(theModel, popn^^.growths, (NOT seedRun), TRUE);
	sum := 0.0;
	FOR factNo := 1 TO theModel^^.nFacts DO
		WITH theModel^^ DO
			IF TstFactFlag(facts^^[factNo].flags, targeted) THEN
				sum := sum + ABS(facts^^[factNo].targetVal - lastResults^^[iteratns, factNo]);
	chrom.fitness := sum;
	END; {Fitness}

	FUNCTION SelectParent (exclude: INTEGER;
									popn: TPopnHdl): INTEGER;
		VAR
			sum, targetVal: DOUBLE;
			j, clocked: INTEGER;
	BEGIN
	CASE popn^^.selectMethod OF
	rouletteS: 
		BEGIN
		clocked := 0;
		sum := 0.0;
		j := popn^^.lastParent;
		targetVal := Rand * popn^^.fitSum;
		REPEAT
			j := j + 1;
			IF j > popn^^.nChrom THEN
				BEGIN
				clocked := clocked + 1;
				j := 1;
				END
			ELSE IF j <> exclude THEN
				sum := sum + ABS(popn^^.fitMax - popn^^.theChroms[j].fitness);
		UNTIL (sum >= targetVal) OR (clocked = 2);
		IF clocked = 2 THEN	{Too hard to find a gene}
			BEGIN
			REPEAT
				j := RndI(1, popn^^.popMax);
			UNTIL j <> exclude;
{OneBtnAlert('Clocked the selection process');}
			END;
		popn^^.lastParent := j;
		SelectParent := j;
		END;
	randomS: 
		BEGIN
		REPEAT
			j := RndI(1, popn^^.nGenes);
		UNTIL (j <> exclude);
		popn^^.lastParent := j;
		SelectParent := j;
		END;
	fitFitS: 
		BEGIN
		j := popn^^.lastParent;
		REPEAT
			j := j + 1;
			IF j = popn^^.nChrom THEN
				j := 1;
		UNTIL j <> exclude;
		SelectParent := j;
		popn^^.lastParent := j;
		END;
	fitWeakS: 
		BEGIN
		IF (exclude < (popn^^.nChrom DIV 2)) AND (exclude > 0) THEN
			BEGIN
			j := popn^^.nChrom - (popn^^.lastParent - 1);
			END
		ELSE
			BEGIN
			IF popn^^.lastParent >= (popn^^.nChrom DIV 2) THEN
				j := 1
			ELSE
				j := popn^^.lastparent + 1;
			popn^^.lastParent := j;
			END;
		SelectParent := j;
		END;
	END;
	END;{SelectParent}

	PROCEDURE Generation (popn: TPopnHdl);
		VAR
			p1, p2: INTEGER;
			c1, c2: TChromosome;
			endGenerate: BOOLEAN;
			p1Fit, p2Fit, c1Fit, c2Fit: DOUBLE;


	BEGIN
	p1 := SelectParent(0, popn);
	p2 := SelectParent(p1, popn);

	MateChroms(popn^^.theChroms[p1], popn^^.theChroms[p2], c1, c2, popn);

	Fitness(c1, popn, FALSE);
	c1Fit := c1.fitness;
	Fitness(c2, popn, FALSE);
	c2Fit := c2.fitness;
	p1fit := popn^^.theChroms[p1].fitness;
	p2Fit := popn^^.theChroms[p2].fitness;

	CASE popn^^.replaceMethod OF
	weakParentR: 
		BEGIN
		IF c1Fit < p1Fit THEN
			BEGIN
			delGene(p1, popn);
			AddChrom(c1, popn);
			c1Fit := INF;		{Only one replacement}
			END
		ELSE IF c2Fit < p1Fit THEN
			BEGIN
			delGene(p1, popn);
			AddChrom(c2, popn);
			c2Fit := INF;		{Only one replacement}
			END;

		IF c1Fit < p2Fit THEN
			BEGIN
			delGene(p2, popn);
			AddChrom(c1, popn);
			END
		ELSE IF c2Fit < p2Fit THEN
			BEGIN
			delGene(p2, popn);
			AddChrom(c2, popn);
			END;
		END;
	bothParentR: 
		BEGIN
		DelGene(p1, popn);
		AddChrom(c1, popn);
		DelGene(p2, popn);
		AddChrom(c2, popn);
		END;
	weakestChromR: 
		BEGIN
		IF popn^^.fitMax > c1Fit THEN
			BEGIN
			DelGene(popn^^.fitMaxGene, popn);
			AddChrom(c1, popn);
			END;
		IF popn^^.fitMax > c2Fit THEN
			BEGIN
			DelGene(popn^^.fitMaxGene, popn);
			AddChrom(c2, popn);
			END;
		END;
	RandomR: 
		BEGIN
		DelGene(RndI(1, popn^^.nChrom), popn);
		AddChrom(c1, popn);
		DelGene(RndI(1, (popn^^.nChrom - 1)), popn);	{Don't zap last gene, its a child}
		AddChrom(c2, popn);
		END;
	END;
	END; {Generation}


	FUNCTION NewPopn (VAR popn: TPopnHdl): BOOLEAN;
		VAR
			i: INTEGER;
	BEGIN
	popn := TPopnHdl(NewHandle(SizeOF(TPopulationRec)));
	CheckMemErr;
	IF gotMemErr THEN
		popn := NIL
	ELSE
		popn^^.theModel := NIL;
	NewPopn := popn <> NIL;
	END; {NewPopn}

	PROCEDURE SeedPopulation (nSeeds: INTEGER;
									popn: TPopnHdl);

		VAR
			i, j, gNum: INTEGER;
			phenoType: TPhenoType;
			chrom: TChromosome;
	BEGIN
	chrom.parent1 := 0;
	chrom.parent2 := 0;
	i := 0;
	REPEAT
		i := i + 1;
		FOR j := 1 TO popn^^.nGenes DO
			phenoType[j] := RndX(popn^^.phenoParms[j].lower, popn^^.phenoParms[j].upper);
		EncodeChrom(chrom, phenoType, popn);
		Fitness(chrom, popn, FALSE);
		AddChrom(chrom, popn);
		UpdateStatus(popn^^.myWD);
		IF userBreak THEN
			BEGIN
			IF NOT TwoBtnAlert('Abort Seeding?', 'OK', 'CANCEL') THEN
				userBreak := FALSE
			ELSE
				popn^^.status := stopped;
			END;
	UNTIL (i = nSeeds) OR userBreak OR gotMemErr;
	IF NOT gotMemErr THEN
		RecalcStats(popn);
	END; {SeedPopulation}



	PROCEDURE ResetPopn (resetChroms: BOOLEAN;
									popn: TPopnHdl);
		VAR
			thisFact, i, nc: INTEGER;
			nullChrom: TChromosome;
			initPheno: BOOLEAN;

	BEGIN
	WITH popn^^ DO
		BEGIN
		IF resetChroms THEN
			BEGIN
			nullChrom.geneID := 0;
			nullChrom.fitness := INF;
			nullChrom.parent1 := 0;
			nullChrom.parent2 := 0;
			nullChrom.fitness := INF;
			lastParent := 1;
			FOR i := 1 TO cMaxGenes DO
				nullChrom.bits[i] := 0;
			FOR i := 1 TO cPopulationMax DO
				theChroms[i] := nullChrom;
			nChrom := 0;
			END;
		nCross := 0;
		nMut := 0;
		fitMin := INF;
		fitMax := -INF;
		fitSum := 0;
		fitMaxGene := 0;
		fitMinGene := 0;
		genNum := 0;
		status := stopped;
		END;
	END; {ResetPopn}


	PROCEDURE InitPopn (popn: TPopnHdl);
		VAR
			thisFact, i, nc: INTEGER;
			nullChrom: TChromosome;
			initPheno: BOOLEAN;

	BEGIN
	WITH popn^^ DO
		BEGIN
		nullChrom.geneID := 0;
		nullChrom.fitness := INF;
		nullChrom.parent1 := 0;
		nullChrom.parent2 := 0;
		nullChrom.fitness := INF;
		lastParent := 1;
		FOR i := 1 TO cMaxGenes DO
			nullChrom.bits[i] := 0;
		FOR i := 1 TO cPopulationMax DO
			theChroms[i] := nullChrom;
		nCross := 0;
		nMut := 0;
		fitMin := INF;
		fitMax := -INF;
		fitSum := 0;
		fitMaxGene := 0;
		fitMinGene := 0;
		genNum := 0;
		status := stopped;
		nChrom := 0;

		maxGenerations := cDfltGenerations;
		thisFact := 0;
		nGenes := 0;
		REPEAT
			thisFact := thisFact + 1;
			IF TstFactFlag(theModel^^.facts^^[thisFact].flags, canMod) THEN
				BEGIN
				nGenes := nGenes + 1;
				WITH phenoParms[nGenes] DO
					BEGIN
					lower := cDfltLower;
					upper := cDfltUpper;
					res := cDfltRes;
					codeMethod := binaryC;
					crossSites := twoSiteC;
					mateMethod := crossM;
					factID := theModel^^.facts^^[thisFact].factID;
					END;
				END;
		UNTIL (thisFact = theModel^^.nFacts) OR (nGenes = cMaxGenes);
		IF (nGenes = cMaxGenes) AND (thisFact < theModel^^.nFacts) THEN
			OneBtnAlert('Potentially more target factors than allowed. Yeah, Im lazy ; I havent looked to see if there really are more , Im just guessing ');
		nextID := 0;
		popMax := cPopulationMax;
		nSeed := popMax;
		convergeVal := 0.0;
		convMethod := fitSumV;
		replaceMethod := weakestChromR;
		pCross := cDfltPCross;
		pMut := cDfltPMut;
		selectMethod := rouletteS;
		END;
	END; {InitPopn}

	FUNCTION Converged (popn: TPopnHdl): BOOLEAN;
	BEGIN
	WITH popn^^ DO
		CASE convMethod OF
		fitSumV: 
			Converged := fitSum <= convergeVal;
		avgFitV: 
			Converged := ((fitMin + fitMax) / nChrom) <= convergeVal;
		bestChromV: 
			Converged := fitMin <= convergeVal;
		END;
	END; {Converged}

	PROCEDURE UpdateStats (gen: INTEGER;
									popn: TPopnHdl);
	BEGIN
	IF popn^^.bestFitStats <> NIL THEN
		popn^^.bestFitStats^^[gen] := popn^^.fitMin;
	IF popn^^.worstFitStats <> NIL THEN
		popn^^.worstFitStats^^[gen] := popn^^.fitMax;
	IF popn^^.avgFitStats <> NIL THEN
		popn^^.avgFitStats^^[gen] := popn^^.fitSum / popn^^.nChrom;
	END; {UpdateStats}

	PROCEDURE GoSearch (popWD: WDHandle);
		VAR
			endSearch, reinit: BOOLEAN;
			thePop: TPopnHdl;

		FUNCTION AllocateStats: BOOLEAN;

			VAR
				mErr: OSErr;
				i: INTEGER;
		BEGIN
		IF thePop^^.bestFitStats <> NIL THEN
			DisposHandle(Handle(thePop^^.bestFitStats));
		thePop^^.bestFitStats := NIL;
		IF thePop^^.worstFitStats <> NIL THEN
			DisposHandle(Handle(thePop^^.worstFitStats));
		thePop^^.worstFitStats := NIL;
		IF thePop^^.avgFitStats <> NIL THEN
			DisposHandle(Handle(thePop^^.avgFitStats));

		thePop^^.bestFitStats := TSearchStatsHdl(NewHandle(SizeOf(TSearchStats)));
		CheckMemErr;
		IF NOT gotMemErr THEN
			BEGIN
			thePop^^.worstFitStats := TSearchStatsHdl(NewHandle(SizeOf(TSearchStats)));
			CheckMemErr;
			END;
		IF NOT gotMemErr THEN
			BEGIN
			thePop^^.avgFitStats := TSearchStatsHdl(NewHandle(SizeOf(TSearchStats)));
			CheckMemErr;
			END;
		IF NOT gotMemErr THEN
			BEGIN
			FOR i := 1 TO cMaxGenerations DO
				BEGIN
				thePop^^.worstFitStats^^[i] := 0.0;
				thePop^^.bestFitStats^^[i] := 0.0;
				thePop^^.avgFitStats^^[i] := 0.0
				END;
			END;
		AllocateStats := NOT gotMemErr;
		END; {AllocateStats}

		FUNCTION InitGrowths: BOOLEAN;
{Initialise the growths table and check if search parameters consistent with the model}
			VAR
				thisFact, nc: INTEGER;
				initPheno: BOOLEAN;
		BEGIN
{Check pheno type parameters - model might have changed}
		thisFact := 0;
		nc := 0;
		initPheno := FALSE;
		REPEAT
			thisFact := thisFact + 1;
			IF TstFactFlag(thePop^^.theModel^^.facts^^[thisFact].flags, canMod) THEN
				BEGIN
				nc := nc + 1;
				initPheno := (thePop^^.phenoParms[nc].factID <> thePop^^.theModel^^.facts^^[thisFact].factID) OR (nc > thePop^^.nGenes);
				thePop^^.phenoParms[nc].factNo := thisFact
				END;
		UNTIL (thisFact = thePop^^.theModel^^.nFacts) OR initPheno;
		initPheno := initPheno OR (nc <> thePop^^.nGenes);
		IF initPheno THEN
			BEGIN
			OneBtnAlert('Model Structure changed. Phenotype parameters reset, search aborted!');
			thisFact := 0;
			nc := 0;
			REPEAT
				thisFact := thisFact + 1;
				IF TstFactFlag(thePop^^.theModel^^.facts^^[thisFact].flags, canMod) THEN
					BEGIN
					nc := nc + 1;
					WITH thePop^^.phenoParms[nc] DO
						BEGIN
						lower := cDfltLower;
						upper := cDfltUpper;
						res := cDfltRes;
						codeMethod := binaryC;
						crossSites := twoSiteC;
						mateMethod := crossM;
						factID := thePop^^.theModel^^.facts^^[thisFact].factID;
						factNo := 0;
						END;
					END;
			UNTIL (thisFact = thePop^^.theModel^^.nFacts) OR (nc = cMaxGenes);
			thePop^^.nGenes := nc;
			IF (nc = cMaxGenes) AND (thisFact < thePop^^.theModel^^.nFacts) THEN
				OneBtnAlert('Potentially more target factors than allowed. Yeah, Im lazy ; I havent looked to see if there really are more , Im just guessing ');
			END;
		IF NOT initPheno THEN
			BEGIN
			thePop^^.growths := TResHdl(NewHandle(SizeOf(TResArry)));
			CheckMemErr;
			IF gotMemErr THEN
				OneBtnAlert('Insufficent memory, cant do search.')
			ELSE
				SetGrowthsTable(thePop^^.theModel, thePop^^.growths, TRUE);
			END;
		InitGrowths := NOT (gotMemErr OR initPheno);
		END; {InitGrowths}

	BEGIN
	endSearch := FALSE;
	reinit := FALSE;
	userBreak := FALSE;
	thePop := TPopnHdl(popWD^^.popnHdl);
	IF NOT AllocateStats THEN
		OneBtnAlert('Insufficient memory for search.')
	ELSE IF InitGrowths THEN
		BEGIN
		IF thePop^^.status <> paused THEN
			BEGIN
			ResetPopn(FALSE, thePop);
			RecalcStats(thePop);
			IF thePop^^.nChrom >= thePop^^.nSeed THEN
				reinit := TwoBtnAlert('Reinitialise population?', 'YES', 'NO')
			ELSE
				reinit := TRUE;
			END
		ELSE IF NOT TwoBtnAlert('Previous search suspended. Reinitialise?', 'NO', 'YES') THEN
			BEGIN
			ResetPopn(TRUE, thePop);
			reinit := TRUE;
			END;
		IF reinit THEN
			BEGIN
			ResetPopn(TRUE, thePop);
			thePop^^.status := seeding;
			SeedPopulation(thePop^^.nSeed, thePop);
			popWD^^.windDirty := TRUE;
			SysBeep(5);
			END
		ELSE
			RecalcStats(thePop);
		IF gotMemErr THEN
			BEGIN
			OneBtnAlert('Search aborted - insufficient memory.');
			thePop^^.status := stopped;
			END
		ELSE IF NOT userBreak THEN
			BEGIN
			thePop^^.status := searching;
			UpdateStatus(popWD);
			IF userBreak THEN
				BEGIN
				ResetPopn(TRUE, thePop);
				thePop^^.status := stopped;
				END
			ELSE
				BEGIN
				REPEAT
					BeachBallCursor;
					thePop^^.genNum := thePop^^.genNum + 1;
					Generation(thePop);
					UpdateStats(thePop^^.genNum, thePop);
					UpdateStatus(popWD);
					endSearch := Converged(thePop) OR (thePop^^.genNum > thePop^^.maxGenerations);
					IF userBreak THEN
						BEGIN	{Fine tune time}
						endSearch := endSearch OR TwoBtnAlert('Suspend search?', 'OK', 'CANCEL');
						END;
				UNTIL endSearch OR gotMemErr;
				popWD^^.windDirty := TRUE;
				END;
			IF userBreak THEN
				thePop^^.status := paused
			ELSE IF gotMemErr THEN
				BEGIN
				OneBtnAlert('Insufficient memory - search abandoned.');
				thePop^^.status := stopped;
				END
			ELSE
				BEGIN
				SysBeep(10);
				SysBeep(10);
				OneBtnAlert('Search Completed');
				thePop^^.status := stopped;
				END;
			userBreak := FALSE;
			SysBeep(10);
			END;
		DisposHandle(Handle(thePop^^.growths));
		END;
	END; {GoSearch}


END. {Genetic}