UNIT GControl;
INTERFACE

	USES
		SANE, DialogUtils, GFiles, GCommonDec, GGlobals, DialogUtils, GDialogs, GATypes, Simulation, Genetic;

	FUNCTION OwnerWindowData (theWindow: WindowPtr): WDHandle;
	PROCEDURE MoveScrollBars (theWD: WDHandle);
	PROCEDURE AdjustLists (theWD: WDHandle);
	PROCEDURE AdjustScrollBar (theWD: WDHandle);
	PROCEDURE NewGWindow (wTitle: Str255;
									wType: TWindType);

	PROCEDURE SetLinks (theWD: WDHandle);
	PROCEDURE FactListClick (theWD: WDHandle;
									dblClick: BOOLEAN);
	PROCEDURE LinkListClick (theWD: WDHandle;
									dblClick: BOOLEAN);
	PROCEDURE EnterText (theWD: WDHandle);
	PROCEDURE SwapTEdit (theWD: WDHandle);
	PROCEDURE NewFactTarget (theModel: TModelHdl;
									factNo: INTEGER);
	PROCEDURE GBrowseSelect (theWD: WDHandle);

{Menu Procs}
	PROCEDURE NewModel;
	PROCEDURE OpenModel;
	FUNCTION CloseWindow (theWind: WindowPtr;
									savePrompt: BOOLEAN): BOOLEAN;
	PROCEDURE SaveWindow (theWD: WDHandle);
	PROCEDURE SaveWindAs (theWD: WDHandle);
	PROCEDURE DoUndo;
	PROCEDURE DoCut (theWD: WDHandle);
	PROCEDURE DoCopy (theWD: WDHandle);
	PROCEDURE DoPaste (theWD: WDHandle);
	PROCEDURE DoClear (theWD: WDHandle);
	PROCEDURE DoSelectAll (theWD: WDHandle);
	PROCEDURE NewFactor (theWD: WDHandle);
	PROCEDURE RemoveFactor (theWD: WDHandle);
	PROCEDURE ResetFactor (theModel: TModelHdl;
									theFact: INTEGER);
	PROCEDURE FactorAttributes (theWD: WDHandle);
	PROCEDURE SetModelPrefs (theWD: WDHandle);
	PROCEDURE DoSimulation (theWD: WDHandle);
	PROCEDURE CreatePopulation (topWD: WDHandle);
	PROCEDURE GetPopulation (theWD: WDHandle;
									menuItem: INTEGER);
	PROCEDURE ClonePopulation (popWD: WDHandle);
	PROCEDURE RenamePopnWind (modelWD: WDHandle);
	PROCEDURE DelPopnWind (modelWD: WDHandle);
	PROCEDURE RescalePopnGraph (popWD: WDHandle;
									thePart: INTEGER);

	PROCEDURE RescaleGraphWind (theWD: WDHandle;
									auto: BOOLEAN);
	PROCEDURE ScaleAllGraphs;
	PROCEDURE SaveSearchStats (theWD: WDHandle);
	PROCEDURE ShowPopnGraph (theWD: WDHandle;
									show: BOOLEAN);
IMPLEMENTATION
{$S ControlSeg1}

	TYPE
		TGFHeaderRec = RECORD
				version: INTEGER;
				size1, size2, size3, size4, size5: LONGINT;
			END;
		TGFHeaderPtr = ^TGFHeaderRec;
		TGFHeaderHdl = ^TGFHeaderPtr;

	FUNCTION OwnerWindowData (theWindow: WindowPtr): WDHandle;
		VAR
			tWD: WDHandle;

	BEGIN
	tWD := wdList;
	IF theWindow <> NIL THEN
		BEGIN
		WHILE ((tWD <> NIL) AND (tWD <> WDHandle(GetWRefCon(theWindow)))) DO
			tWD := tWD^^.next;
		IF (tWD <> NIL) THEN
			IF GetWRefCon(theWindow) <> 0 THEN
				OwnerWindowData := WDHandle(GetWRefCon(theWindow))
			ELSE
				OwnerWindowData := NIL
		ELSE
			OwnerWindowData := NIL
		END
	ELSE
		OwnerWindowData := NIL;
	END; {OwnerWindowData}

	PROCEDURE MoveScrollBars (theWD: WDHandle);
		VAR
			tempVS, tempHS: ControlHandle;
			spareRect: Rect;

	BEGIN
	IF theWD^^.windType IN [modelWind, graphWind] THEN
		BEGIN
		tempVS := theWD^^.vScroll;
		tempHS := theWD^^.hScroll;
		spareRect := theWD^^.thisWindow^.portRect;
		WITH spareRect DO
			BEGIN
			MoveControl(tempVS, right - 15, top - 1);
			SizeControl(tempVS, 16, bottom - top - 13);
			InvalRect(tempVS^^.contrlRect);
			MoveControl(tempHS, left - 1, bottom - 15);
			SizeControl(tempHS, right - left - 13, 16);
			InvalRect(tempHS^^.contrlRect);
			END;
		END;
	END; {MoveScrollBars}


	PROCEDURE AdjustScrollBar (theWD: WDHandle);
		CONST
			active = 0;
			inactive = 255;
		VAR
			wHeight, wWidth: INTEGER;
			oldValue, oldMax, newMax: INTEGER;
			frame: Rect;
	BEGIN
	IF theWD^^.windType IN [modelWind, graphWind] THEN
		BEGIN
		CASE theWD^^.windType OF
		modelWind: 
			frame := theWD^^.contRect;
		graphWind: 
			frame := theWD^^.pFrame;
		END;
		IF theWD^^.windType = graphBrowse THEN
			EXIT(AdjustScrollBar);
		wHeight := frame.bottom - frame.top;
		oldValue := GetCtlvalue(theWD^^.vScroll);
		oldMax := GetCtlMax(theWD^^.vScroll);
		IF WDHeight(theWD) >= (wHeight + 16) THEN
			BEGIN
			SetCtlValue(theWD^^.vScroll, 0);
			HiliteControl(theWD^^.vScroll, inactive);
			END
		ELSE
			BEGIN
			HiliteControl(theWD^^.vScroll, active);
			newMax := (wHeight - WDHeight(theWD) + 16);
			SetCtlValue(theWD^^.vScroll, 0);
			SetCtlMax(theWD^^.vScroll, newMax);
			END;
		wWidth := frame.right - frame.left;
		oldValue := GetCtlvalue(theWD^^.hScroll);
		oldMax := GetCtlMax(theWD^^.hScroll);
		IF WDWidth(theWD) >= (wWidth + 16) THEN
			BEGIN
			SetCtlValue(theWD^^.hScroll, 0);
			HiliteControl(theWD^^.hScroll, inActive);
			END
		ELSE
			BEGIN
			HiliteControl(theWD^^.hScroll, active);
			newMax := (wWidth - WDWidth(theWD) + 16);
			SetCtlValue(theWD^^.hScroll, 0);
			SetCtlMax(theWD^^.hScroll, newMax);
			END;
		END;
	END; {AdjustScrollBars}

	PROCEDURE ZapTextRects (theWD: WDHandle);
	BEGIN
	EraseRect(theWD^^.te1Rect);
	InvalRect(theWD^^.te1Rect);
	EraseRect(theWD^^.te2Rect);
	InvalRect(theWD^^.te2Rect);
	END; {ZapTextRects}

	PROCEDURE AdjustLists (theWD: WDHandle);
		VAR
			lWidth, lHeight: INTEGER;
			pRect, r: Rect;
			ctlHdl: Controlhandle;
	BEGIN
	IF theWD^^.windType <> modelWind THEN
		EXIT(AdjustLists);
	pRect := theWD^^.thisWindow^.portRect;
	r := theWD^^.factList^^.rView;
	InsetRect(r, -2, -1);
	r.right := r.right + 15;
	InvalRect(r);
	WITH theWD^^, factList^^, rView DO
		BEGIN
		lWidth := right - left;
		lHeight := pRect.bottom - 30 - top;
{lHeight := Min(TLDataHdl(userHandle)^^.lHeight, (pRect.bottom - 16 - top));}
		END;
	ctlHdl := ControlHandle(TLDataHdl(theWD^^.factList^^.userHandle)^^.vScroll);
	theWD^^.factList^^.vScroll := ctlHdl;	{Restore it so it gets resized}
	LSize(lWidth, lHeight, theWD^^.factList);
	r := theWD^^.factList^^.rView;
	InsetRect(r, -2, -1);
	r.right := r.right + 15;
	InvalRect(r);
	IF (pRect.right - 16) < ctlHdl^^.contrlRect.right THEN
		BEGIN
		HideControl(ctlHdl);
		theWD^^.factList^^.vScroll := NIL;
		END
	ELSE
		ShowControl(ctlHdl);

	r := theWD^^.linkList^^.rView;
	InsetRect(r, -1, -1);
	r.right := r.right + 15;
	InvalRect(r);
	WITH theWD^^, linkList^^, rView DO
		BEGIN
		lWidth := right - left;
		lHeight := pRect.bottom - 30 - top;
{lHeight := Min(TLDataHdl(userHandle)^^.lHeight, (pRect.bottom - 16 - top));}
		END;
	ctlHdl := ControlHandle(TLDataHdl(theWD^^.linkList^^.userHandle)^^.vScroll);
	theWD^^.linkList^^.vScroll := ctlHdl;	{Restore it so it gets resized}
	LSize(lWidth, lHeight, theWD^^.linkList);
	r := theWD^^.linkList^^.rView;
	InsetRect(r, -1, -1);
	r.right := r.right + 15;
	InvalRect(r);
	IF (pRect.right - 16) < ctlHdl^^.contrlRect.right THEN
		BEGIN
		HideControl(ctlHdl);
		theWD^^.linkList^^.vScroll := NIL;
		END
	ELSE
		ShowControl(ctlHdl);
	END; {AdjustLists}



	PROCEDURE MakeScrollBars (theWD: WDHandle);

		VAR
			cRect, pRect: Rect;

	BEGIN
	pRect := theWD^^.thisWindow^.portRect;
	WITH pRect DO
		BEGIN
		OffsetRect(pRect, -left, -top); { Global rect to local }
		SetRect(cRect, right - 16, top, right, bottom - 16);
		END;
	theWD^^.vScroll := NewControl(theWD^^.thisWindow, cRect, '', False, 0, 0, 0, 16, LONGINT(theWD));
	WITH pRect DO
		SetRect(cRect, left, bottom - 16, right - 16, bottom);
	theWD^^.HScroll := NewControl(theWD^^.thisWindow, cRect, '', False, 0, 0, 0, 16, LONGINT(theWD));
	MoveScrollBars(theWD);
	AdjustScrollBar(theWD);
	END; {MakeScrollBars}



	PROCEDURE NewGWindow (wTitle: Str255;
									wType: TWindType);
		VAR
			thisWD: WDHandle;
			windID: INTEGER;

		PROCEDURE MakeModelWind;
			VAR
				dBnds, destRect, r: Rect;
				cSize: Point;
		BEGIN
		WITH thisWD^^ DO
			BEGIN
{Make a picture for all the frames and stuff}
			TextFont(helvetica);
			TextSize(12);
			SetRect(contRect, 7, 8, 472, 237);
			ClipRect(contRect);
			modelPic := OpenPicture(contRect);
			WITH showValPopM DO
				BEGIN
				mID := showValMenuID;
				mHdl := theMenus[showValM];
				SetRect(mRect, 130, 5, 200, 21);
				InsertMenu(mHdl, -1);
				mRect.right := mRect.left + mHdl^^.menuWidth;
				DeleteMenu(mID);
				SetRect(tRect, 0, 0, 0, 0);
				lastSel := 1;
				END;
			MoveTo(10, 18);
			DrawString('Factor Name');
			MoveTo(249, 71);
			DrawString('Linked To');
			MoveTo(376, 71);
			DrawString('Link Weight');
			SetRect(r, 9, 24, 140, 47);	{Fact name TE}
			FrameRect(r);
			SetRect(r, 151, 24, 208, 47);	{Fact weight TE}
			FrameRect(r);
{    PenSize(3, 3);}
{    SetRect(r, 235, 57, 470, 210);}
{    FrameRect(r);}
			PenSize(1, 1);
			ClosePicture;


			TextFont(monaco);	{For the lists}
			TextSize(9);
			SetRect(dBnds, 0, 0, 1, 0);
			SetPt(cSize, 191, 0);

			SetRect(r, 10, 50, 201, 236);
			factList := LNew(r, dBnds, cSize, cGListProcID, thisWindow, FALSE, FALSE, FALSE, TRUE);
			cSize := factList^^.cellSize;
			cSize.v := cSize.v + 3;
			LCellSize(cSize, factList);
			factList^^.refCon := 0;
			WITH TLDataHdl(factList^^.userHandle)^^ DO
				BEGIN
				lPics := listPics;
				whichVal := showImpacts;
				showFlags := 0;
				SetFactFlag(showFlags, canMod, TRUE);
				SetFactFlag(showFlags, gotLinks, TRUE);
				SetFactFlag(showFlags, crossDelay, TRUE);
				SetFactFlag(showFlags, targeted, TRUE);
				SetFactFlag(showFlags, graphIt, TRUE);
				indentFlags := 0;
				END;
			factList^^.selFlags := lOnlyOne;
			LDoDraw(TRUE, factList);

			SetRect(r, 229, 77, 450, 201);
			cSize.h := 221;
			linkList := LNew(r, dBnds, cSize, cGListProcID, thisWindow, FALSE, FALSE, FALSE, TRUE);
			linkList^^.selflags := lOnlyOne;
			linkList^^.refCon := 0;
			WITH TLDataHdl(linkList^^.userHandle)^^ DO
				BEGIN
				lPics := listPics;
				whichVal := showImpacts;
				showFlags := 0;
				SetFactFlag(showFlags, gotLinks, TRUE);
				indentFlags := 0;
				SetFactFlag(indentFlags, gotLinks, TRUE);
				END;
			LDoDraw(TRUE, linkList);

			SetRect(te1Rect, 12, 27, 137, 42);
			SetRect(te2Rect, 154, 27, 205, 42);
			destRect := te1Rect;
			InsetRect(destRect, 2, 2);
			teHdl := TENew(destRect, te1Rect);
			inNameTE := TRUE;
			curFactor := 0;
			xStr := '';
			theModel := NIL;
			gBrowser := NIL;
			END;
		MakeScrollBars(thisWD);
		AdjustScrollBar(thisWD);
		END; {MakeModelWind}

		PROCEDURE MakeGraphWind;
		BEGIN
		TextFont(monaco);
		TextSize(9);
		TextFace([]);
		MakeScrollBars(thisWD);
		AdjustScrollBar(thisWD);
		END; {MakeGraphWind}

		PROCEDURE MakeBrowser;
			VAR
				r, dBnds: Rect;
				cSize: Point;
		BEGIN
		WITH thisWD^^ DO
			BEGIN
			r := thisWindow^.portRect;
			WITH r DO
				BEGIN
				right := right - 15;
				bottom := bottom - 15;
				END;
			SetRect(dBnds, 0, 0, cGListCols, 0);
			SetPt(cSize, (cGraphWidth DIV 4) + 10, (cGraphHeight DIV 4) + 20);
			gbList := LNew(r, dBnds, cSize, cGraphList, thisWindow, FALSE, TRUE, TRUE, TRUE);
			END;
		END; {MakeBrowser}

		PROCEDURE MakeWait;
		BEGIN
		WITH thisWD^^ DO
			BEGIN
			SetRect(ovRect, 12, 26, 157, 108);
			SetRect(txtRect, 48, 119, 123, 135);
			SetPort(thisWindow);
			PenSize(2, 2);
			TextFont(monaco);
			TextSize(9);
			TextFace([]);
			ClipRect(thisWindow^.portRect);
			waitPic := OpenPicture(thisWindow^.portRect);
			EraseOval(ovRect);
			InsetRect(ovRect, -2, -2);
			FrameOval(ovRect);
			InsetRect(ovRect, 2, 2);
			ClosePicture;
			hScroll := NIL;
			vScroll := NIL;
			prevWind := NIL;
			initted := FALSE;
			END;
		END; {MakeWait}

		PROCEDURE MakePopn;
			CONST
				rescalePictID = 2000;
			VAR
				str: Str255;
				r, vRect: Rect;
		BEGIN
		WITH thisWD^^ DO
			BEGIN
			SetRect(r, 0, 0, 492, 290);
			ClipRect(r);
			TextFont(0);
			TextSize(12);
			TextFont(monaco);
			TextSize(9);
			popnWindPic := OpenPicture(r);
			str := 'Best Fit:';
			SetRect(r, 14, 12, 70, 24);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[1], 72, 12, 147, 24);
			FrameInsetRect(-2, -2, statRects[1]);

			str := 'Worst Fit:';
			SetRect(r, 9, 33, 70, 45);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[2], 72, 33, 147, 45);
			FrameInsetRect(-2, -2, statRects[2]);

			str := 'Avg. Fit';
			SetRect(r, 14, 54, 70, 66);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[3], 72, 54, 147, 66);
			FrameInsetRect(-2, -2, statRects[3]);

			str := 'Max Population:';
			SetRect(r, 331, 12, 423, 24);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[4], 428, 12, 482, 24);
			FrameInsetRect(-2, -2, statRects[4]);

			str := 'Current Population:';
			SetRect(r, 308, 33, 423, 45);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[5], 428, 33, 482, 45);
			FrameInsetRect(-2, -2, statRects[5]);

			str := 'Max Generations:';
			SetRect(r, 323, 54, 423, 66);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[6], 428, 54, 482, 66);
			FrameInsetRect(-2, -2, statRects[6]);

			str := 'Generation #:';
			SetRect(r, 344, 75, 423, 87);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[7], 428, 75, 482, 87);
			FrameInsetRect(-2, -2, statRects[7]);

			str := 'Status:';
			SetRect(r, 213, 87, 258, 99);
			TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);
			SetRect(statRects[8], 208, 100, 272, 112);
			FrameInsetRect(-2, -2, statRects[8]);

			str := 'Step:';
			SetRect(r, 266, 87, 301, 99);
{TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustLeft);}
			SetRect(statRects[9], 256, 102, 310, 114);
{FrameInsetRect(-2, -2, statRects[9]);}

			MoveTo(0, 129);
			LineTo(thisWindow^.portRect.right, 129);

			SetRect(iconRect, 447, 94, 479, 126);
			FrameInsetRect(-2, -2, iconRect);

			SetRect(progRect, 193, 6, 295, 84);
			FrameInsetRect(-2, -2, progRect);
			EraseOval(progRect);
			FrameOval(progRect);

			SetRect(pGraphRect, 2, 157, 487, 289);
{    FrameRect(pGraphRect);}


			ClosePicture;
			rscPict := GetPicture(rescalePictID);
			SetRect(rescaleRect, 27, 134, 74, 155);
			popnHdl := NIL;
			showGraph := FALSE;
			pGraphInfo.picHdl := NIL;
			END;
		END; {MakePopn}

		PROCEDURE InsertWindowName;
			VAR
				i, nItems: INTEGER;
				str: Str255;
				found: BOOLEAN;
		BEGIN
		i := 0;
		nItems := CountMItems(theMenus[windM]);
		REPEAT
			i := i + 1;
			GetItem(theMenus[windM], i, str);
			found := str = 'Graphs';
		UNTIL (i = nItems) OR (found);
		IF thisWD^^.windType IN [modelWind, popnWind] THEN
			BEGIN
			i := i - 1;
			InsMenuItem(theMenus[windM], thisWD^^.wName, i)
			END
		ELSE
			InsMenuItem(theMenus[windM], thisWD^^.wName, i);
		EnableItem(theMenus[windM], (i + 1));
		END; {InsertWindowName}

	BEGIN
	thisWD := WDHandle(NewHandle(SizeOf(WindowData)));
	MoveHHi(Handle(thisWD)); { move block to top of heap and }
	HLock(Handle(thisWD)); { lock it there. }
	WITH thisWD^^ DO
		BEGIN
		CASE wType OF
		modelWind: 
			windID := modelWID;
		graphWind: 
			windID := graphWID;
		graphBrowse: 
			windID := browseWID;
		waitWind: 
			windID := waitWID;
		popnWind: 
			windID := popnWID;
		END;
		thisWindow := GetNewWindow(windID, NIL, Pointer(-1));
		SetWRefCon(thisWindow, Longint(thisWD));
		SetPort(thisWindow); {set current grafPort to this window}
		TextFont(monaco);
		TextSize(9);
		ClipRect(thisWindow^.portRect);
		next := wdList;
		owner := NIL;
		windType := wType;
		windDirty := FALSE;
		wName := UniqueWindName(wType, wTitle);
		SetWTitle(thisWindow, wName);
		SetPt(offsetPt, 0, 0);
		InvalRect(thisWindow^.portRect);
		hScroll := NIL;
		vScroll := NIL;
		CASE wType OF		{*** Careful, these unlock theWD ***}
		modelWind: 
			MakeModelWind;
		graphWind: 
			MakeGraphWind;
		graphBrowse: 
			MakeBrowser;
		waitWind: 
			MakeWait;
		popnWind: 
			MakePopn;
		END;
		END; {of With}
	HUnLock(Handle(thisWD));	{Just in case}
	wdList := thisWD;
	IF wType <> waitWind THEN
		InsertWindowName;
	ShowWindow(thisWD^^.thisWindow);
	SelectWindow(thisWD^^.thisWindow);
	END; {NewGWindow}

{***** Graph Procs *****}
	PROCEDURE NewFactTarget (theModel: TModelHdl;
									factNo: INTEGER);
		VAR
			upper, lower: DOUBLE;
			oldGrID, grNum, factID: INTEGER;
			graphArray: TGraphHdl;
			theWD: WDHandle;
			oErr: OSErr;

{Find any existing graphs of factor factNo and adjust the target}
	BEGIN
	IF TstFactFlag(theModel^^.facts^^[factNo].flags, graphIt) THEN
		IF theModel^^.graphArray <> NIL THEN
			BEGIN
			graphArray := theModel^^.graphArray;
			factID := theModel^^.facts^^[factNo].factID;
			grNum := 1;
			WHILE (factID <> graphArray^^[grNum].factID) AND (grNum <= cMaxGraphs) DO
				grNum := grNum + 1;
			IF grNum <= cMaxGraphs THEN
				BEGIN
				oldGrID := graphArray^^[grNum].grID;
				upper := theModel^^.facts^^[factNo].yMax;
				lower := theModel^^.facts^^[factNo].yMin;
				KillPicture(graphArray^^[grNum].picHdl);
				GraphFact(theModel, grNum, factNo, TstFactFlag(theModel^^.facts^^[factNo].flags, targeted), lower, upper);
				theWD := wdList;
				WHILE (theWD <> NIL) DO
					BEGIN
					IF theWD^^.windType = graphBrowse THEN
						LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.gbList)
					ELSE IF (theWD^^.windType = graphWind) AND (theWD^^.grID = oldGrID) THEN
						BEGIN
						KillPicture(theWD^^.picHdl);
						theWD^^.picHdl := graphArray^^[grNum].picHdl;
						oErr := HandToHand(Handle(theWD^^.picHdl));
						theWD^^.grID := graphArray^^[grNum].grID;
						InvalRect(theWD^^.thisWindow^.portRect);
						END;
					theWD := theWD^^.next;
					END;
				TextFont(monaco);
				TextSize(9);

				END;
			END;
	END; {NewFactTarget}

	PROCEDURE GBrowseSelect (theWD: WDHandle);
		VAR
			dLen: INTEGER;
			pCell: TPicCell;
			theCell: Cell;
			theModel: TModelHdl;
			grWD: WDHandle;
			oErr: OSErr;
	BEGIN
	SetPt(theCell, 0, 0);
	WHILE LGetSelect(TRUE, theCell, theWD^^.gbList) DO
		BEGIN
		dLen := SizeOf(TPicCell);
		LGetCell(@pCell, dLen, theCell, theWD^^.gbList);
		IF dLen = SizeOf(TPicCell) THEN
			BEGIN
			theModel := theWD^^.owner^^.theModel;
			NewGWindow(Concat('Graph - ', theModel^^.graphArray^^[pCell.graphNum].name), graphWind);
			grWD := wdList;
			grWD^^.owner := theWD^^.owner;
			grWD^^.grNum := pCell.graphNum;
			grWD^^.grID := theModel^^.graphArray^^[pCell.graphNum].grID;
			SetRect(grWD^^.pFrame, 0, 0, cGraphWidth, cGraphHeight);
			grWD^^.picHdl := theModel^^.graphArray^^[pCell.graphNum].picHdl;
			oErr := HandToHand(Handle(grWD^^.picHdl));
			InvalRect(grWD^^.thisWindow^.portRect);
			LSetSelect(FALSE, theCell, theWD^^.gbList);
			END;
		IF NOT LNextCell(TRUE, TRUE, theCell, theWD^^.gbList) THEN
			theCell.v := 999;
		END;
	END; {GBrowseSelect}

{***** Model Procs *****}
	PROCEDURE InitModel (VAR theModel: TModelHdl);
	BEGIN
	theModel := TModelHdl(NewHandle(SizeOf(TModelRec)));
	gotMemErr := gotMemErr OR (MemError <> noErr);
	theModel^^.facts := TFactsHdl(NewHandle(SizeOf(TFactArry)));
	gotMemErr := gotMemErr OR (MemError <> noErr);
	theModel^^.crGrowths := TCGHdl(NewHandle(SizeOf(TCGArry)));
	gotMemErr := gotMemErr OR (MemError <> noErr);
	theModel^^.graphArray := NIL;
	IF gotMemErr THEN
		BEGIN
		OneBtnAlert('Cant allocate model');
		theModel := NIL;
		END
	ELSE
		WITH theModel^^ DO
			BEGIN
			nFacts := 0;
			iteratns := cDefItns;
			nextFactID := 1;
			graphArray := NIL;
			numPrec := 4;
			forceDelay := FALSE;
			forceImmed := FALSE;
			dfltStartVal := cDefStartVal;
			lastResults := NIL;
			enableFlags := 0;
			SetFactFlag(enableFlags, shocks, TRUE);
			SetFactFlag(enableFlags, bounded, TRUE);
			SetFactFlag(enableFlags, threshholds, TRUE);
			dfltGType := mult;
			END;
	END; {InitModel}


	PROCEDURE SetLinks (theWD: WDHandle);
		VAR
			theCell: Point;
	BEGIN
	IF SetFactLinks(theWD^^.theModel, theWD^^.curFactor) THEN
		BEGIN
		SetPt(theCell, 0, theWD^^.curfactor - 1);
		SetFactorCell(theWD^^.curFactor, 0, theCell, theWD^^.factList);
		InvalRect(theWD^^.factList^^.rView);
		LDraw(theCell, theWD^^.factList);
		END;
	theWD^^.windDirty := TRUE;
	END; {SetLinks}

	PROCEDURE DisplayFactLinks (theWD: WDHandle);
		VAR
			theCell: Cell;
			curfact, i: INTEGER;
			str: Str255;
			theModel: TModelHdl;
			num: DOUBLE;
	BEGIN
	theModel := theWD^^.theModel;
	curfact := theWD^^.curFactor;
	SetPt(theCell, 0, 0);
	LDoDraw(FALSE, theWD^^.linkList);
	LDelRow(0, 99, theWD^^.linkList);
	FOR i := 1 TO theModel^^.nFacts DO
		IF (i <> theWD^^.curFactor) AND (theModel^^.crGrowths^^[curFact, i] <> 0) THEN
			BEGIN
			theCell.v := LAddRow(1, 999, theWD^^.linkList);
			SetFactorCell(i, curfact, theCell, theWD^^.linkList);
			END;
	LDoDraw(TRUE, theWD^^.linkList);
	InvalRect(theWD^^.linkList^^.rView);
	IF theWD^^.inNameTE THEN
		SwapTEdit(theWD);
	CASE TLDataHdl(theWD^^.factList^^.userhandle)^^.whichVal OF
	showImpacts: 
		str := NumToDecStr(cWeightPrec, theModel^^.crGrowths^^[curFact, curFact]);
	showStart: 
		str := NumToDecStr(cWeightPrec, theModel^^.facts^^[curfact].startVal);
	showActTarg: 
		IF TstFactFlag(theModel^^.facts^^[theCell.v + 1].flags, targeted) THEN
			str := NumToDecStr(cWeightPrec, theModel^^.facts^^[curfact].targetVal)
		ELSE
			str := '';
	ShowAllTarg: 
		str := NumToDecStr(cWeightPrec, theModel^^.facts^^[curfact].targetVal);
	END;
	theWD^^.xStr := theModel^^.facts^^[curFact].name;
	TESetText(Ptr(LONGINT(@str) + 1), Length(str), theWD^^.teHdl);
	TESetSelect(0, 32000, theWD^^.teHdl);
	ZapTextRects(theWD);
	END; {DisplayFactLinks}

	PROCEDURE FactListClick (theWD: WDHandle;
									dblClick: BOOLEAN);
		VAR
			theCell: Cell;
			thisSel: INTEGER;
	BEGIN
	SetPt(theCell, 0, 0);
	IF LGetSelect(TRUE, theCell, theWD^^.factList) THEN
		BEGIN
		thisSel := theCell.v + 1;
		IF (thisSel <> theWD^^.curFactor) OR dblClick THEN
			BEGIN
			IF theWD^^.curFactor <> 0 THEN
				BEGIN
				IF TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal IN [showAllTarg, showActTarg] THEN
					NewFactTarget(theWD^^.theModel, theWD^^.curFactor);
				END;
			theWD^^.curFactor := thisSel;
			DisplayFactLinks(theWD);
			IF dblClick THEN
				SetLinks(theWD);
			DisplayFactLinks(theWD);
			END;
		END
	ELSE
		theWD^^.curFactor := 0;
	END; {FactListClick}

	PROCEDURE LinkListClick (theWD: WDHandle;
									dblClick: BOOLEAN);
		VAR
			theCell, toCell, fromCell: Cell;
			linkList: ListHandle;
			theModel: TModelHdl;
			val: DOUBLE;
			indent, nextIndent: INTEGER;
			fRec: TFactRec;
			nXFacts, i, baseFact, factNo: INTEGER;
			xFacts: ARRAY[1..cMaxFacts] OF INTEGER;
	BEGIN
	linkList := theWD^^.linkList;
	theModel := theWD^^.theModel;
	SetPt(theCell, 0, 0);
	IF dblClick THEN
		IF LGetSelect(TRUE, theCell, linkList) THEN
			BEGIN
			fRec := GetIFactorCell(val, indent, theCell, linkList);
			baseFact := fRec.factNum;
			IF NOT TstFactFlag(fRec.flags, gotLinks) OR (indent < 0) THEN
				SysBeep(5)	{No links or circular}
			ELSE
				BEGIN
				LDoDraw(FALSE, linkList);
				IF theCell.v < (linkList^^.dataBounds.bottom - 1) THEN
					BEGIN
					theCell.v := theCell.v + 1;
					fRec := GetIFactorCell(val, nextIndent, theCell, linkList)
					END
				ELSE
					BEGIN
					theCell.v := theCell.v + 1;
					nextIndent := 0;
					END;
				IF ABS(indent) < ABS(nextIndent) THEN
					BEGIN		{Remove sub list}
					indent := ABS(nextIndent);
					toCell := theCell;
					REPEAT
						toCell.v := toCell.v + 1;
						fRec := GetIFactorCell(val, nextIndent, toCell, linkList);
					UNTIL (indent > ABS(nextIndent)) OR (toCell.v >= (linkList^^.dataBounds.bottom - 1));
					IF (indent <= ABS(nextIndent)) THEN {(indent < ABS(nextIndent))}
						toCell.v := toCell.v + 1;
					LDelRow((toCell.v - theCell.v), theCell.v, linkList);
					END
				ELSE
					BEGIN	{Create sub list}
{Build table of existing factors in the list}
					SetPt(toCell, 0, 0);
					nXFacts := 0;
					WHILE toCell.v < (linkList^^.dataBounds.bottom) DO
						BEGIN
						fRec := GetFactorCell(val, toCell, linkList);
						nXFacts := nXFacts + 1;
						xFacts[nXFacts] := fRec.factID;
						toCell.v := toCell.v + 1;
						END;
					nextIndent := ABS(indent + 1);
					theCell.v := theCell.v - 1;
					FOR factNo := 1 TO theModel^^.nFacts DO
						IF factNo <> baseFact THEN
							BEGIN
							IF (theModel^^.crGrowths^^[baseFact, factNo] <> 0.0) THEN
								BEGIN
								theCell.v := LAddRow(1, (theCell.v + 1), linkList);
								i := 0;
								WHILE (i <= nXFacts) AND (theModel^^.facts^^[factNo].factID <> xFacts[i]) DO
									i := i + 1;
								IF i <= nXFacts THEN
									SetIFactorCell(factNo, 0, -nextIndent, theCell, linkList)
								ELSE
									BEGIN
									SetIFactorCell(factNo, 0, nextIndent, theCell, linkList);
									nXFacts := nXFacts + 1;
									xFacts[nXFacts] := theModel^^.facts^^[factNo].factID;
									END;
								END;
							END;
					END;
				LDoDraw(TRUE, theWD^^.linkList);
				InvalRect(theWD^^.linkList^^.rView);
				END;
			END;
	END; {LinkListClick}

	PROCEDURE EnterText (theWD: WDHandle);
		VAR
			s, fName: Str255;	{Not a real string}
			i, j, dLen: INTEGER;
			theCell: Cell;
			cHdl: CharsHandle;
			zCh: STRING[1];
			w: DOUBLE;
			dStr: DecStr;
	BEGIN
	zCh[0] := CHR(1);
	zCh[1] := CHR(0);
	SetPt(theCell, 0, 0);
	IF LGetSelect(TRUE, theCell, theWD^^.factList) THEN
		BEGIN
		cHdl := TEGetText(theWD^^.teHdl);
		FOR i := 0 TO (theWD^^.teHdl^^.teLength - 1) DO
			s[i + 1] := cHdl^^[i];
		s[0] := CHR(theWD^^.teHdl^^.teLength);
		IF theWD^^.inNameTE THEN
			BEGIN
			fName := s;
			dStr := theWD^^.xStr;
			w := Str2Num(dStr);
			END
		ELSE
			BEGIN
			fName := theWD^^.xStr;
			dStr := s;
			w := Str2Num(dStr);
			END;
		WITH theWD^^.theModel^^ DO
			BEGIN
			facts^^[theCell.v + 1].name := fName;
			CASE TLDataHdl(theWD^^.factList^^.userhandle)^^.whichVal OF
			showImpacts: 
				crGrowths^^[theCell.v + 1, theCell.v + 1] := w;
			showStart: 
				facts^^[theCell.v + 1].startVal := w;
			showActTarg: 
				IF TstFactFlag(facts^^[theCell.v + 1].flags, targeted) THEN
					facts^^[theCell.v + 1].targetVal := w;
			showAllTarg: 
				BEGIN
				facts^^[theCell.v + 1].targetVal := w;
				SetFactFlag(facts^^[theCell.v + 1].flags, targeted, TRUE);
				END;
			END;
			END;
		SetFactorCell((theCell.v + 1), 0, theCell, theWD^^.factList);
		END;
	END; {EnterText}

	PROCEDURE SwapTEdit (theWD: WDHandle);
		VAR
			s: Str255;
			cHdl: CharsHandle;
			r: Rect;
			i: INTEGER;
	BEGIN
	s := theWD^^.xStr;
	cHdl := TEGetText(theWD^^.teHdl);
	FOR i := 1 TO theWD^^.teHdl^^.teLength DO
		theWD^^.xStr[i] := cHdl^^[i - 1];
	theWD^^.xStr[0] := CHR(theWD^^.teHdl^^.teLength);
	TESetText(Ptr(LONGINT(@s) + 1), Length(s), theWD^^.teHdl);
	ZapTextRects(theWD);
	IF theWD^^.inNameTE THEN
		r := theWD^^.te2Rect
	ELSE
		r := theWD^^.te1Rect;
	WITH theWD^^.teHdl^^ DO
		BEGIN
		viewRect := r;
		destRect := r;
		InsetRect(destRect, 2, 2);
		END;
	TESetSelect(0, 32000, theWD^^.teHdl);
	theWD^^.inNameTE := NOT theWD^^.inNameTE;
	InvalRect(theWD^^.te1Rect);
	EraseRect(theWD^^.te1Rect);
	InvalRect(theWD^^.te2Rect);
	EraseRect(theWD^^.te2Rect);
	END; {SwapTEdit}

	PROCEDURE DisplayFactors (theWD: WDHandle);
		VAR
			theCell: Cell;
			i: INTEGER;
			str: Str255;
			theModel: TModelHdl;
	BEGIN
	theModel := theWD^^.theModel;
	SetPt(theCell, 0, 0);
	LDoDraw(FALSE, theWD^^.factList);
	LDelRow(0, 99, theWD^^.factList);
	FOR i := 1 TO theModel^^.nFacts DO
		BEGIN
		theCell.v := LAddRow(1, 999, theWD^^.factList);
		SetFactorCell(i, 0, theCell, theWD^^.factList);
		END;
	LDoDraw(TRUE, theWD^^.factList);
	InvalRect(theWD^^.factList^^.rView);
	theWD^^.curFactor := 1;
	SetPt(theCell, 0, 0);
	LSetSelect(TRUE, theCell, theWD^^.factList);
	DisplayFactLinks(theWD);
	END; {DisplayFactors}

	PROCEDURE LoadModel (theWD: WDHandle;
									theFile: TFile);
		VAR
			count: LONGINT;
			theModel: TModelHdl;
			fErr: OSErr;
			fHdr: TGFHeaderHdl;
			crHdl: TCGHdl;
			fHdl: TFactsHdl;
			i: INTEGER;
			str: Str255;

	BEGIN
	IF theFile.status = RW THEN
		BEGIN
		InitModel(theModel);
		crHdl := theModel^^.crGrowths;
		fHdl := theModel^^.facts;
		IF theModel = NIL THEN
			EXIT(LoadModel);
		theWD^^.theModel := theModel;
		theWD^^.factList^^.refcon := LONGINT(theModel);
		theWD^^.linkList^^.refcon := LONGINT(theModel);
		theWD^^.modelFile := theFile;
{Read the file header}
		fErr := SetFPos(theWD^^.modelFile.refNum, fsFromStart, 0);
		fHdr := TGFHeaderHdl(NewHandle(SizeOf(TGFHeaderRec)));
		count := SizeOf(TGFHeaderRec);
		HLock(Handle(fHdr));
		fErr := FSRead(theFile.refNum, count, Ptr(fHdr^));
		HUnlock(Handle(fHdr));
		IF fErr <> noErr THEN
			BEGIN
			NumToString(fErr, str);
			OneBtnAlert(Concat('Error reading model.  Err:', str));
			EXIT(LoadModel);
			END;
		IF fHdr^^.version <> genieVersionNo THEN
			BEGIN
			OneBtnAlert('Sorry. This model file no longer compatible.');
			EXIT(LoadModel);
			END;
		count := fHdr^^.size1;
{Read the model}
		HLock(Handle(theModel));
		fErr := FSRead(theFile.refNum, count, Ptr(theModel^));
		HUnlock(Handle(theModel));
		IF fErr <> noErr THEN
			BEGIN
			NumToString(fErr, str);
			OneBtnAlert(Concat('Error reading model.  Err:', str));
			EXIT(LoadModel);
			END;
{Read the factors array}
		count := fHdr^^.size2;
		HLock(Handle(fHdl));
		fErr := FSRead(theFile.refNum, count, Ptr(fHdl^));
		HUnlock(Handle(fHdl));
		theModel^^.facts := fHdl;
		IF fErr <> noErr THEN
			BEGIN
			NumToString(fErr, str);
			OneBtnAlert(Concat('Error reading model.  Err:', str));
			EXIT(LoadModel);
			END;
{Read the cross growths table}
		count := fHdr^^.size3;
		HLock(Handle(crHdl));
		fErr := FSRead(theFile.refNum, count, Ptr(crHdl^));
		HUnlock(Handle(crHdl));
		theModel^^.crGrowths := crHdl;
		IF fErr <> noErr THEN
			BEGIN
			NumToString(fErr, str);
			OneBtnAlert(Concat('Error reading model.  Err:', str));
			EXIT(LoadModel);
			END;
		DisplayFactors(theWD);
		theModel^^.lastResults := NIL;
		theModel^^.graphArray := NIL;
		END;
	END; {LoadModel}

	PROCEDURE ZapPopnMenu;
		VAR
			i, nItems: INTEGER;
	BEGIN
	nItems := CountMItems(theMenus[popnSelM]);
	FOR i := 5 TO nItems DO
		DelMenuItem(theMenus[popnSelM], 5);
	END; {ZapPopnMenu}

{***** Menu Procs *****}
	PROCEDURE NewModel;
		VAR
			theFile: TFile;
			theWD: WDHandle;
	BEGIN
	ZapPopnMenu;
	InitFileRec(theFile, 'New Data', cModelFType);
	IF NewFile(theFile, cGenieSign, TRUE, 'Create Model:') THEN
		BEGIN
		NewGWindow(theFile.fName, modelWind);
		theWD := wdList;
		InitModel(theWD^^.theModel);
		theWD^^.factList^^.refcon := LONGINT(theWD^^.theModel);
		theWD^^.linkList^^.refcon := LONGINT(theWD^^.theModel);
		theWD^^.modelFile := theFile;
		theWD^^.windDirty := TRUE;
		END;
	END; {NewModel}

	PROCEDURE OpenModel;
		VAR
			theFile: TFile;
			theWD: WDHandle;
			refNum, rErr: INTEGER;
	BEGIN
	ZapPopnMenu;
	InitFileRec(theFile, '', cModelFType);
	IF OpenFile(theFile, TRUE, NIL, 'Open Which Model?') THEN
		BEGIN
		NewGWindow(theFile.fName, modelWind);
		theWD := wdList;
		LoadModel(theWD, theFile);
		theWD^^.revert := TRUE;
		refNum := OpenResFork(theFile, rErr);
		IF refNum <> 0 THEN
			BEGIN
			AddResMenu(theMenus[popnSelM], popnWindResType);
			CloseResFile(refNum);
			END;
		END;
	END; {OpenModel}

	FUNCTION CloseWindow (theWind: WindowPtr;
									savePrompt: BOOLEAN): BOOLEAN;
{Returns TRUE if the window was closed. If user cancels operation, FALSE is returned}
		VAR
			theWD, twd: WDHandle;
			i, nItems: INTEGER;
			str: Str255;
			saveOpt: INTEGER;
			abort: BOOLEAN;
			popn: TPopnHdl;
	BEGIN
	abort := FALSE;
	theWD := OwnerWindowData(theWind);
	IF theWD <> NIL THEN
		BEGIN
		IF theWD^^.windDirty AND savePrompt AND (theWD^^.windType IN [modelWind, popnWind]) THEN
			BEGIN
			saveOpt := ThreeBtnAlert(Concat('Save window ', theWD^^.wName, ' before closing?'), 'YES', 'NO', 'CANCEL');
			IF saveOpt = 1 THEN
				SaveWindow(theWD)
			ELSE IF saveOpt = 3 THEN
				abort := TRUE;
			END;
		IF NOT abort THEN
			BEGIN
			ChangeWindMenuName(theWD^^.wName, '');

			CASE theWD^^.windType OF
			modelWind: 
				BEGIN
				IF theWD^^.gBrowser <> NIL THEN
					abort := CloseWindow(theWD^^.gBrowser^^.thisWindow, FALSE);
				twd := wdList;
				WHILE (twd <> NIL) AND NOT abort DO
					BEGIN
					WHILE NOT (twd^^.windType IN [graphWind, popnWind]) AND (twd <> NIL) DO
						twd := twd^^.next;
					IF twd^^.owner = theWD THEN
						BEGIN
						abort := NOT CloseWindow(twd^^.thisWindow, savePrompt);
						twd := wdList;	{Start looking again}
						END
					ELSE IF twd <> NIL THEN
						twd := twd^^.next;
					END;
				IF NOT abort THEN
					BEGIN
					LDispose(theWD^^.factList);
					LDispose(theWD^^.linkList);
					KillPicture(theWD^^.modelPic);
					IF theWD^^.theModel <> NIL THEN
						BEGIN
						CloseFile(theWD^^.modelFile);
						IF theWD^^.theModel^^.lastResults <> NIL THEN
							Disposhandle(Handle(theWD^^.theModel^^.lastResults));
						DisposHandle(Handle(theWD^^.theModel^^.crGrowths));
						DisposHandle(Handle(theWD^^.theModel));
						END;
					TEDispose(theWD^^.teHdl);
					END;
				END;
			graphBrowse: 
				BEGIN
				theWD^^.owner^^.gBrowser := NIL;
				LDispose(theWD^^.gbList);
				END;
			graphWind: 
				KillPicture(theWD^^.picHdl);
			popnWind: 
				BEGIN
				KillPicture(theWD^^.popnWindPic);
				IF theWD^^.popnHdl <> NIL THEN
					BEGIN
					popn := TPopnHdl(theWD^^.popnHdl);
					IF popn^^.bestFitStats <> NIL THEN
						DisposHandle(Handle(popn^^.bestFitStats));
					IF popn^^.worstFitStats <> NIL THEN
						DisposHandle(Handle(popn^^.worstFitStats));
					IF popn^^.avgFitStats <> NIL THEN
						DisposHandle(Handle(popn^^.avgFitStats));
					DisposHandle(theWD^^.popnHdl);
					END;
				END;
			END;
			IF NOT abort THEN
				BEGIN
				IF theWD = wdList THEN
					wdList := theWD^^.next
				ELSE
					BEGIN
{Rebuild window list}
					twd := wdList;
					WHILE (twd <> NIL) DO
						IF twd^^.next = theWD THEN
							BEGIN
							twd^^.next := theWD^^.next;
							twd := NIL;
							END
						ELSE
							twd := twd^^.next;
					END;
				DisposHandle(Handle(theWD));
				KillControls(theWind);	{A bit incongruous, given the prev IF, but...}
				END;
			END;
		IF NOT abort THEN
			DisposeWindow(theWind);
		END;
	CloseWindow := NOT abort;
	END; {CloseWindow}

	FUNCTION MakeBrowsePic (theWD: WDHandle): PicHandle;
		VAR
			gPort: GrafPort;
			savePort: GrafPtr;
			pH, pV: INTEGER;
			i, j: INTEGER;
			r, r1: Rect;
			grafNum, dLen: INTEGER;
			theModel: TModelHdl;
			picHdl: PicHandle;
			gbList: ListHandle;
			fName: Str255;
			cellRec: TPicCell;
			theCell: Cell;

	BEGIN
	BeachBallCursor;
	theModel := theWD^^.owner^^.theModel;
	gbList := theWD^^.gbList;
	GetPort(savePort);
	OpenPort(@gPort);
	InitPort(@gPort);
	gPort.portBits.bounds := screenBits.bounds;
	WITH screenBits.bounds DO
		OffsetRect(gPort.portBits.bounds, (right - left + 10), (bottom - top + 10));
	WITH theWD^^.gbList^^, dataBounds DO
		BEGIN
		pH := right * cellSize.h;
		pV := bottom * cellSize.v;
		END;
	PortSize(pH, pV);
	SetPort(@gPort);
	SetRect(gPort.portRect, 0, 0, pH, pV);
	ClipRect(gPort.portRect);
	TextFont(monaco);
	TextSize(9);
	grafNum := 0;
	picHdl := OpenPicture(gPort.portRect);
	FOR i := 0 TO (gbList^^.dataBounds.bottom - 1) DO
		BEGIN
		BeachBallCursor;
		r.top := i * gbList^^.cellSize.v;
		r.bottom := r.top + gbList^^.cellSize.v;
		FOR j := 0 TO (gbList^^.dataBounds.right - 1) DO
			BEGIN
			dLen := SizeOf(TPicCell);
			SetPt(theCell, j, i);
			LGetCell(@cellRec, dLen, theCell, gbList);
			IF dLen = SizeOf(TPicCell) THEN
				BEGIN
				grafNum := grafNum + 1;
				r.left := j * gbList^^.cellSize.h;
				r.right := r.left + gbList^^.cellSize.h;
				SetRect(r1, 0, 0, (cGraphWidth DIV 4), (cGraphHeight DIV 4));
				OffsetRect(r1, (r.left + 5), (r.top + 5));
				IF theModel^^.graphArray^^[grafNum].picHdl <> NIL THEN
					DrawPicture(theModel^^.graphArray^^[grafNum].picHdl, r1);
				r1 := r;
				r1.top := r1.bottom - 10;
				fName := theModel^^.graphArray^^[grafNum].name;
				TextBox(Ptr(LONGINT(@fName) + 1), INTEGER(fName[0]), r1, teJustCenter);
				END;
			END;
		END;
	ClosePicture;
	SetPort(savePort);
	ClosePort(@gPort);
	MakeBrowsePic := picHdl;
	END; {MakeBrowsePic}

	PROCEDURE SaveWindow (theWD: WDHandle);
		VAR
			fErr: OSErr;
			ok: BOOLEAN;

		PROCEDURE SaveModel (modelWD: WDHandle);
			VAR
				count, tmpLong: LONGINT;
				theModel: TModelHdl;
				fHdr: TGFHeaderHdl;
				str: Str255;

		BEGIN
		fHdr := TGFHeaderHdl(NewHandle(SizeOf(TGFHeaderRec)));
		theModel := modelWD^^.theModel;
		IF modelWD^^.modelFile.status = NO THEN
			ok := OpenFile(modelWD^^.modelFile, FALSE, NIL, '')
		ELSE
			ok := TRUE;

		IF ok THEN
			BEGIN
			fHdr^^.version := genieVersionNo;
			fHdr^^.size1 := SizeOf(TModelRec);
			fHdr^^.size2 := theModel^^.nFacts * SizeOf(TFactRec); {Size of factor array}
			tmpLong := (cMaxFacts * SizeOf(DOUBLE));	{Otherwise it goes negative for big models}
			fHdr^^.size3 := theModel^^.nFacts * tmpLong; {Size of cross growth table}
			fHdr^^.size4 := 0;
			fHdr^^.size5 := 0;
			fErr := SetFPos(modelWD^^.modelFile.refNum, fsFromStart, 0);
			fErr := SetEOF(modelWD^^.modelFile.refNum, 0);
			count := SizeOf(TGFHeaderRec);
{Write the header}
			HLock(Handle(fHdr));
			fErr := FSWrite(modelWD^^.modelFile.refNum, count, Ptr(fHdr^));
			HUnlock(Handle(fHdr));
			IF fErr <> noErr THEN
				BEGIN
				NumToString(fErr, str);
				OneBtnAlert(Concat('Error writing model.  Err:', str));
				EXIT(SaveModel);
				END;
{Write the model record}
			count := fHdr^^.size1;
			HLock(Handle(theModel));
			fErr := FSWrite(modelWD^^.modelFile.refNum, count, Ptr(theModel^));
			HUnlock(Handle(theModel));
			IF fErr <> noErr THEN
				BEGIN
				NumToString(fErr, str);
				OneBtnAlert(Concat('Error writing model.  Err:', str));
				EXIT(SaveModel);
				END;
{Write the factors}
			count := fHdr^^.size2;
			HLock(Handle(theModel^^.facts));
			fErr := FSWrite(modelWD^^.modelFile.refNum, count, Ptr(theModel^^.facts^));
			HUnlock(Handle(theModel^^.facts));
			IF fErr <> noErr THEN
				BEGIN
				NumToString(fErr, str);
				OneBtnAlert(Concat('Error writing model.  Err:', str));
				EXIT(SaveModel);
				END;
{Write the cross growths table}
			count := fHdr^^.size3;
			HLock(Handle(theModel^^.crGrowths));
			fErr := FSWrite(modelWD^^.modelFile.refNum, count, Ptr(theModel^^.crGrowths^));
			HUnlock(Handle(theModel^^.crGrowths));
			IF fErr <> noErr THEN
				BEGIN
				NumToString(fErr, str);
				OneBtnAlert(Concat('Error writing model.  Err:', str));
				EXIT(SaveModel);
				END;
			modelWD^^.windDirty := FALSE;
			fErr := FlushVol(NIL, modelWD^^.modelFile.vRefNum);
			modelWD^^.windDirty := FALSE;
			modelWD^^.revert := TRUE;
			END;
		END; {SaveModel}

		PROCEDURE SavePopn (srchWD: WDHandle);
			VAR
				refNum, rErr, nextID: INTEGER;
				oErr: OSErr;
				rHdl: Handle;
		BEGIN
		IF srchWD^^.owner^^.windDirty THEN
			SaveModel(srchWD^^.owner);
		refNum := OpenResFork(srchWD^^.owner^^.modelFile, rErr);
		IF refNum <> 0 THEN
			BEGIN
			IF srchWD^^.resID = 0 THEN
				nextID := UniqueID(popnWindResType)
			ELSE
				nextID := srchWD^^.resID;
			rHdl := GetResource(popnWindResType, nextID);
			IF rHdl <> NIL THEN
				BEGIN
				RmveResource(rHdl);
				DisposHandle(rHdl);
				END;
			rHdl := srchWD^^.popnHdl;
			oErr := HandToHand(rHdl);
			AddResource(rHdl, popnWindResType, nextID, srchWD^^.wName);
			WriteResource(rHdl);
			ReleaseResource(rHdl);
			srchWD^^.resID := nextID;
			CloseResFile(refNum);
			END;
		END; {SavePopn}

	BEGIN
	IF theWD^^.windDirty THEN
		BEGIN
		CASE theWD^^.windType OF
		modelWind: 
			SaveModel(theWD);
		popnWind: 
			SavePopn(theWD);
		END;
		theWD^^.windDirty := FALSE;
		END;
	END; {SaveWindow}

	PROCEDURE SaveResults (theModel: TModelHdl;
									factNo: INTEGER;
									theFile: TFile);
{crCh, tabCh in GGlobals}
		VAR
			str: Str255;
			dataHdl: Handle;
			dataLen: LONGINT;
			fErr: OSErr;
			itn, thefact: INTEGER;
	BEGIN
	dataHdl := NewHandle(cMinDataBlk);
	dataLen := 0;
	fErr := SetFPos(theFile.refNum, fsFromStart, 0);
	fErr := SetEOF(theFile.refNum, 0);
	IF factNo = 0 THEN
		FOR thefact := 1 TO theModel^^.nFacts DO
			BEGIN
			str := theModel^^.facts^^[thefact].name;
			IF theFact = theModel^^.nFacts THEN
				str := Concat(str, crCh)
			ELSE
				str := Concat(str, tabCh);
			AddStrToData(dataHdl, dataLen, str);
			END
	ELSE
		BEGIN
		str := Concat(theModel^^.facts^^[factNo].name, crCh);
		AddStrToData(dataHdl, dataLen, str);
		END;
	FOR itn := 1 TO theModel^^.iteratns DO
		BEGIN
		IF factNo = 0 THEN
			FOR thefact := 1 TO theModel^^.nFacts DO
				BEGIN
				str := NumToDecStr(8, theModel^^.lastResults^^[itn, thefact]);
				IF theFact = theModel^^.nFacts THEN
					str := Concat(str, crCh)
				ELSE
					str := Concat(str, tabCh);
				AddStrToData(dataHdl, dataLen, str);
				END
		ELSE
			BEGIN
			str := Concat(NumToDecStr(8, theModel^^.lastResults^^[itn, factNo]), crCh);
			AddStrToData(dataHdl, dataLen, str);
			END;
		END;
	fErr := FSWrite(theFile.refNum, dataLen, dataHdl^);
	IF fErr <> noErr THEN
		OneBtnAlert('File write failed');
	DisposHandle(dataHdl);
	fErr := fsClose(theFile.refNum);
	fErr := FlushVol(NIL, theFile.vRefNum);
	END; {SaveResults}

	PROCEDURE SaveWindAs (theWD: WDHandle);
		VAR
			theFile, curFile: TFile;
			BufPtr: Ptr;
			count: LONGINT;
			fErr: OSErr;
			pmHdl: MenuHandle;
			fType: INTEGER;


		PROCEDURE SaveModelAs;
			PROCEDURE DoModelSave;
			BEGIN
			fErr := SetFPos(curFile.refNum, fsFromStart, 0);
			IF fErr <> noErr THEN
				OneBtnAlert(Concat('Cant read file ', curFile.fName))
			ELSE
				BEGIN
				REPEAT
					count := 1024;
					fErr := FSRead(curFile.refNum, count, bufPtr);
					fErr := FSWrite(theFile.refNum, count, bufPtr);
				UNTIL TestEof(curFile) OR (count < 1024);
				CloseFile(curFile);
				theWD^^.modelFile := theFile;
				ChangeWindMenuName(theWD^^.wName, theFile.fName);
				theWD^^.wName := theFile.fName;
				SetWTitle(theWD^^.thisWindow, theFile.fName);
				END;
			END; {DoModelSave}

			PROCEDURE DoTextSave;
				VAR
					str: Str255;
					dataHdl: Handle;
					dataLen: LONGINT;
					theModel: TModelHdl;
					facts: TFactsHdl;
					i, factNo, prec: INTEGER;
					fErr: OSErr;

				FUNCTION OnOffStr (bool: BOOLEAN): Str255;
				BEGIN
				IF bool THEN
					OnOffStr := 'On'
				ELSE
					OnOffStr := 'Off';
				END; {OnOffStr}

			BEGIN
			IF ChangeFileType(theFile, cGenieSign, 'TEXT') THEN
				;
			dataHdl := NewHandle(cMinDataBlk);
			dataLen := 0;
			fErr := SetFPos(theFile.refNum, fsFromStart, 0);
			fErr := SetEOF(theFile.refNum, 0);
			theModel := theWD^^.theModel;
			facts := theModel^^.facts;
			prec := theModel^^.numPrec;
			str := Concat('Description of Model ', theWD^^.wName, crCh, crCh, crCh);
			AddStrToData(dataHdl, dataLen, str);
			AddStrToData(dataHdl, dataLen, Concat('Simulation Iterations: ', tabCh));
			AddStrToData(dataHdl, dataLen, Concat(NumToDecStr(0, theModel^^.iteratns), crCh));
			AddStrToData(dataHdl, dataLen, Concat('System Shocks: ', OnOffStr(TstFactFlag(theModel^^.enableFlags, shocks)), crCh));
			AddStrToData(dataHdl, dataLen, 'Cross Impacts Processing: ');
			IF theModel^^.forceDelay THEN
				AddStrToData(dataHdl, dataLen, 'All Delayed')
			ELSE IF theModel^^.forceImmed THEN
				AddStrToData(dataHdl, dataLen, 'All Immediate')
			ELSE
				AddStrToData(dataHdl, dataLen, 'As Defined');
			AddStrToData(dataHdl, dataLen, Concat(crCh, crCh));
			AddStrToData(dataHdl, dataLen, Concat('No. of Factors: ', tabCh, NumToDecStr(0, theModel^^.nFacts), crCh));
			FOR factNo := 1 TO theModel^^.nFacts DO
				BEGIN
				AddStrToData(dataHdl, dataLen, Concat(NumToDecStr(0, factNo), ' ', facts^^[factNo].name, crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Start Value', tabCh, 'Self Impact', tabCh, 'Target Value', crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].startVal)));
				IF TstFactFlag(facts^^[factNo].flags, variGrowth) THEN
					AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Vari'))
				ELSE
					AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, theModel^^.crGrowths^^[factNo, factNo])));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].targetVal), crCh));
				IF TstFactFlag(facts^^[factNo].flags, canMod) THEN
					AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Controllable'));
				IF TstFactFlag(facts^^[factNo].flags, crossDelay) THEN
					AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Delayed Cross Impacts'));
				IF TstFactFlag(facts^^[factNo].flags, shocks) THEN
					AddStrToData(dataHdl, dataLen, Concat(tabCh, 'System Shocks'));
				IF TstFactFlag(facts^^[factNo].flags, targeted) THEN
					AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Targeted'));
				AddStrToData(dataHdl, dataLen, crCh);
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Online'));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(0, facts^^[factNo].online.start)));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, ' TO '));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(0, facts^^[factNo].onLine.finish)));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, OnOffStr(TstFactFlag(facts^^[factNo].flags, delayImpact)), crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Growth Bounds'));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].gBounds.lower), tabCh, ' TO '));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].gBounds.upper)));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, OnOffStr(TstFactFlag(facts^^[factNo].flags, bounded)), crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Threshholds'));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].gTHolds.lower), tabCh, ' TO '));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, facts^^[factNo].gTHolds.upper)));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, OnOffStr(TstFactFlag(facts^^[factNo].flags, threshholds)), crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Growth Type: '));
				CASE facts^^[factNo].gType OF
				add: 
					AddStrToData(dataHdl, dataLen, 'Additive');
				mult: 
					AddStrToData(dataHdl, dataLen, 'Multiplicative');
				expon: 
					AddStrToData(dataHdl, dataLen, 'Exponential');
				END;
				AddStrToData(dataHdl, dataLen, Concat(crCh, crCh));
				AddStrToData(dataHdl, dataLen, Concat(tabCh, 'Cross Impacts', crCh));
				FOR i := 1 TO theModel^^.nfacts DO
					BEGIN
					IF theModel^^.crGrowths^^[i, factNo] <> 0 THEN
						BEGIN
						AddStrToData(dataHdl, dataLen, Concat(tabCh, facts^^[i].name));
						AddStrToData(dataHdl, dataLen, Concat(tabCh, NumToDecStr(prec, theModel^^.crGrowths^^[i, factNo]), crCh));
						END;
					END;
				AddStrToData(dataHdl, dataLen, Concat(crCh, crCh));
				END;
			fErr := FSWrite(theFile.refNum, dataLen, dataHdl^);
			IF fErr <> noErr THEN
				OneBtnAlert('File write failed');
			DisposHandle(dataHdl);
			fErr := fsClose(theFile.refNum);
			fErr := FlushVol(NIL, theFile.vRefNum);
			END; {DoTextSave}

		BEGIN
		IF theWD^^.theModel <> NIL THEN
			BEGIN
			pmHdl := GetMenu(modelFTypeMenu);
			InsertMenu(pmHdl, -1);
			bufPtr := NewPtr(1024);
			curFile := theWD^^.modelFile;
			theFile := curFile;
			IF PopMNewFile(theFile, fType, cGenieSign, modelFTypeMenu, 'Save file as:') THEN
				IF EqualString(theFile.fName, curFile.fName, FALSE, FALSE) AND (theFile.vRefNum = curFile.vRefNum) THEN
					OneBtnAlert('Please use a different name.')
				ELSE
					BEGIN
					CASE fType OF
					1: 
						DoModelSave;
					2: 
						DoTextSave;
					END;
					END;
			END;
		DisposPtr(bufPtr);
		DeleteMenu(modelFTypeMenu);
		DisposeMenu(pmHdl);
		END; {SaveModelAs}

		PROCEDURE SaveGraph (grafWD: WDHandle);
			VAR
				grafFile: TFile;
				factNo: INTEGER;
				theModel: TModelHdl;

		BEGIN
		InitFileRec(grafFile, grafWD^^.wName, 'PICT');
		pmHdl := GetMenu(graphFTypeMenu);
		InsertMenu(pmHdl, -1);
		IF NOT PopMNewFile(grafFile, fType, cGenieSign, graphFTypeMenu, 'Save Graph:') THEN
			OneBtnAlert('Cant create file')
		ELSE
			BEGIN
			CASE fType OF
			1: 
				IF NOT SavePicture(grafFile, grafWD^^.picHdl) THEN
					OneBtnAlert('Cant write file');
			2: 
				BEGIN
				IF ChangeFileType(grafFile, cGenieSign, 'TEXT') THEN
					;
				theModel := theWD^^.owner^^.theModel;
				IF (theModel^^.lastResults = NIL) OR NOT FactIDIdx(theModel, theModel^^.graphArray^^[theWD^^.grNum].factID, factNo) THEN
					BEGIN
					CloseFile(grafFile);
					OneBtnAlert('Results not available');
					END
				ELSE
					SaveResults(theModel, factNo, grafFile);
				END;
			END;
			END;
		DeleteMenu(modelFTypeMenu);
		DisposeMenu(pmHdl);
		END; {SaveGraph}

		PROCEDURE SaveGraphBrowse (browseWD: WDHandle);
			VAR
				grafFile: TFile;
				picHdl: PicHandle;
				theModel: TModelHdl;
		BEGIN
		InitFileRec(grafFile, browseWD^^.wName, 'PICT');
		pmHdl := GetMenu(graphFTypeMenu);
		InsertMenu(pmHdl, -1);
		IF NOT PopMNewFile(grafFile, fType, cGenieSign, graphFTypeMenu, 'Save Graph:') THEN
			OneBtnAlert('Cant create file')
		ELSE
			BEGIN
			CASE fType OF
			1: 
				BEGIN
				picHdl := MakeBrowsePic(browseWD);
				IF NOT SavePicture(grafFile, picHdl) THEN
					OneBtnAlert('Cant write file');
				END;
			2: 
				BEGIN
				IF ChangeFileType(grafFile, cGenieSign, 'TEXT') THEN
					;
				theModel := theWD^^.owner^^.theModel;
				IF (theModel^^.lastResults = NIL) THEN
					BEGIN
					CloseFile(grafFile);
					OneBtnAlert('Results not available');
					END
				ELSE
					SaveResults(theModel, 0, grafFile);
				END;
			END;
			END;
		DeleteMenu(modelFTypeMenu);
		DisposeMenu(pmHdl);
		END; {SaveGraphBrowse}

		PROCEDURE SavePopnAs (theWD: WDHandle);
			VAR
				sel: INTEGER;
				statsEnable: BOOLEAN;
				popn: TPopnHdl;

			PROCEDURE SavePopnAsText;
				VAR
					theFile: TFile;
					dataHdl: Handle;
					dLen: LONGINT;
					theModel: TModelHdl;
					factNo, i, gNum: INTEGER;
					pType: TPhenoType;
					str: Str255;
					fErr: OSErr;

			BEGIN
			InitFileRec(theFile, theWD^^.wName, 'TEXT');
			IF NewFile(theFile, cGenieSign, TRUE, 'Save Population (TEXT):') THEN
				BEGIN
				dataHdl := NewHandle(cMinDataBlk);
				dLen := 0;
				fErr := SetFPos(theFile.refNum, fsFromStart, 0);
				fErr := SetEOF(theFile.refNum, 0);
				theModel := popn^^.theModel;

				AddStrToData(dataHdl, dLen, Concat('Population ', theWD^^.wName, crCh, crCh));
				AddStrToData(dataHdl, dLen, Concat('Population Limit: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.popMax), crCh));
				AddStrToData(dataHdl, dLen, Concat('Seed Population: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat('Maximum Generations: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.maxGenerations), crCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.nSeed), crCh));
				AddStrToData(dataHdl, dLen, Concat('p Cross: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(4, popn^^.pCross), crCh));
				AddStrToData(dataHdl, dLen, Concat('p Mutation: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(4, popn^^.pMut), crCh));
				CASE popn^^.selectMethod OF
				rouletteS: 
					str := 'Roulette';
				randomS: 
					str := 'Random';
				fitFitS: 
					str := 'Fit/Fit Gene';
				fitWeakS: 
					str := 'Fit/Weak Gene';
				END;
				AddStrToData(dataHdl, dLen, Concat(str, ' Parent Selection', crCh));
				CASE popn^^.replaceMethod OF
				weakParentR: 
					str := 'Weak Parent';
				bothParentR: 
					str := 'Both Parent';
				weakestChromR: 
					str := 'Weakest Gene';
				randomR: 
					str := 'Random';
				END;
				AddStrToData(dataHdl, dLen, Concat(str, ' Replacement', crCh));
				CASE popn^^.convMethod OF
				fitSumV: 
					str := 'Fitness Sum';
				avgFitV: 
					str := 'Average Fitness';
				bestChromV: 
					str := 'Best Fitness';
				END;
				AddStrToData(dataHdl, dLen, Concat(str, ' Convergence', crCh));
				AddStrToData(dataHdl, dLen, Concat('Convergence Value: ', tabCh));
				AddStrToData(dataHdl, dLen, Concat(NumToDecStr(4, popn^^.convergeVal), crCh, crCh));
				AddStrToData(dataHdl, dLen, Concat('Gene Definitions', crCh));
				FOR i := 1 TO popn^^.nGenes DO
					BEGIN
					AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, i), ' '));
					IF FactIDIdx(theModel, popn^^.phenoParms[i].factID, factNo) THEN
						;
					AddStrToData(dataHdl, dLen, Concat(theModel^^.facts^^[factNo].name, crCh));
					AddStrToData(dataHdl, dLen, Concat(tabCh, 'Value Range: '));
					AddStrToData(dataHdl, dLen, Concat(tabCh, NumToDecStr(4, popn^^.phenoParms[i].lower)));
					AddStrToData(dataHdl, dLen, Concat(tabCh, ' TO '));
					AddStrToData(dataHdl, dLen, Concat(tabCh, NumToDecStr(4, popn^^.phenoParms[i].upper), crCh));
					AddStrToData(dataHdl, dLen, Concat(tabCh, 'Resolution: '));
					AddStrToData(dataHdl, dLen, Concat(tabCh, NumToDecStr(0, popn^^.phenoParms[i].res), crCh));
					CASE popn^^.phenoParms[i].codeMethod OF
					binaryC: 
						str := 'Binary';
					END;
					AddStrToData(dataHdl, dLen, Concat(tabCh, str, ' Coding', crCh));
					CASE popn^^.phenoParms[i].crossSites OF
					oneSiteC: 
						str := 'One Site Splice';
					twoSiteC: 
						str := 'Two Site Splice';
					END;
					AddStrToData(dataHdl, dLen, Concat(tabCh, str, ' Mating', crCh));
					END;
				AddStrToData(dataHdl, dLen, crCh);
				IF popn^^.nChrom = 0 THEN
					AddStrToData(dataHdl, dLen, Concat('Zero Population', crCh))
				ELSE
					BEGIN
					AddStrToData(dataHdl, dLen, Concat('Gene ID', tabCh, 'Parent 1', tabCh, 'Parent 2', tabCh, 'Fitness'));
					FOR i := 1 TO popn^^.nGenes DO
						BEGIN
						IF FactIDIdx(theModel, popn^^.phenoParms[i].factID, factNo) THEN
							;
						AddStrToData(dataHdl, dLen, Concat(tabCh, theModel^^.facts^^[factNo].name));
						END;
					AddStrToData(dataHdl, dLen, crCh);
					FOR gNum := 1 TO popn^^.nChrom DO
						BEGIN
						AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.theChroms[gNum].geneID), tabCh));
						AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.theChroms[gNum].parent1), tabCh));
						AddStrToData(dataHdl, dLen, Concat(NumToDecStr(0, popn^^.theChroms[gNum].parent2), tabCh));
						AddStrToData(dataHdl, dLen, Concat(NumToDecStr(4, popn^^.theChroms[gNum].fitness)));
						DecodeChrom(popn^^.theChroms[gNum], pType, popn);
						FOR i := 1 TO popn^^.nGenes DO
							AddStrToData(dataHdl, dLen, Concat(tabCh, NumToDecStr(4, pType[i])));
						AddStrToData(dataHdl, dLen, crCh);
						END;
					END;
				fErr := FSWrite(theFile.refNum, dLen, dataHdl^);
				IF fErr <> noErr THEN
					OneBtnAlert('File write failed');
				DisposHandle(dataHdl);
				fErr := fsClose(theFile.refNum);
				fErr := FlushVol(NIL, theFile.vRefNum);
				END;
			END;{SavePopnAsText}

		BEGIN
		popn := TPopnHdl(theWD^^.popnHdl);
		statsEnable := (popn^^.bestFitStats <> NIL) OR (popn^^.worstFitStats <> NIL) OR (popn^^.avgFitStats <> NIL);
		sel := 1;
		IF SavePopnAsDlog(sel, statsEnable) THEN
			BEGIN
			CASE sel OF
			1: {Clone Popn}
				BEGIN
				ClonePopulation(theWD);
				END;
			2: {Text Description}
				SavePopnAsText;
			3: {Save Stats}
				SaveSearchStats(theWD);
			END;
			END;
		END; {SavePopnAs}

	BEGIN
	IF theWD <> NIL THEN
		CASE theWD^^.windType OF
		modelWind: 
			SaveModelAs;
		graphWind: 
			SaveGraph(theWD);
		graphBrowse: 
			SaveGraphBrowse(theWD);
		popnWind: 
			SavePopnAs(theWD);
		END;
	END; {SaveWindAs}

	PROCEDURE DoUndo;
	BEGIN
	END; {DoUndo}

	PROCEDURE DoCut (theWD: WDHandle);
	BEGIN
	END; {DoCut}


	PROCEDURE DoCopy (theWD: WDHandle);
		VAR
			sErr: LONGINT;
			picHdl: PicHandle;
	BEGIN
	IF theWD <> NIL THEN
		CASE theWD^^.windType OF
		graphWind: 
			BEGIN
			MoveHHi(Handle(theWD^^.picHdl));
			HLock(Handle(theWD^^.picHdl));
			sErr := ZeroScrap;
			IF sErr = noErr THEN
				sErr := PutScrap(GetHandleSize(Handle(theWD^^.picHdl)), 'PICT', Ptr(theWD^^.picHdl^));
			HUnlock(Handle(theWD^^.picHdl));
			END;
		graphBrowse: 
			BEGIN
			picHdl := MakeBrowsePic(theWD);
			MoveHHi(Handle(picHdl));
			HLock(Handle(picHdl));
			sErr := ZeroScrap;
			IF sErr = noErr THEN
				sErr := PutScrap(GetHandleSize(Handle(picHdl)), 'PICT', Ptr(picHdl^));
			HUnlock(Handle(picHdl));
			KillPicture(picHdl);
			END;
		END;
	END; {DoCopy}

	PROCEDURE DoPaste (theWD: WDHandle);
	BEGIN
	END; {DoPaste}

	PROCEDURE DoClear (theWD: WDHandle);
	BEGIN
	END; {DoClear}

	PROCEDURE DoSelectAll (theWD: WDHandle);
		VAR
			theCell: Cell;
	BEGIN
	IF theWD^^.windType = graphBrowse THEN
		BEGIN
		LDoDraw(FALSE, theWD^^.gbList);
		SetPt(theCell, 0, 0);
		REPEAT
			LSetSelect(TRUE, theCell, theWD^^.gbList);
		UNTIL NOT LNextCell(TRUE, TRUE, theCell, theWD^^.gbList);
		LDoDraw(TRUE, theWD^^.gbList);
		LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.gbList);
		END
	ELSE
		SysBeep(5);
	END; {DoSelectAll}

{$S ControlSeg2}

	PROCEDURE NewFactor (theWD: WDHandle);
		VAR
			str: Str255;
			rNum: INTEGER;
			theCell: Cell;
			theModel: TModelHdl;
			cRect: Rect;

	BEGIN
	theModel := theWD^^.theModel;
	IF theWD^^.windType = modelWind THEN
		BEGIN
		IF theModel^^.nFacts = cMaxFacts THEN
			OneBtnAlert('Too many factors')
		ELSE
			BEGIN
			IF theModel^^.lastResults <> NIL THEN
				BEGIN
				DisposHandle(Handle(theModel^^.lastResults));
				theModel^^.lastResults := NIL;
				END;
			theModel^^.nFacts := theModel^^.nFacts + 1;
			theModel^^.facts^^[theModel^^.nFacts].factNum := theModel^^.nfacts;
			theModel^^.facts^^[theModel^^.nFacts].factID := theModel^^.nextFactID;
			theModel^^.nextFactID := theModel^^.nextFactID + 1;
			ResetFactor(theModel, theModel^^.nFacts);

			SetPt(theCell, 0, 0);
			IF LGetSelect(TRUE, theCell, theWD^^.factList) THEN
				LSetSelect(FALSE, theCell, theWD^^.factList);
			IF NOT theWD^^.inNameTE THEN
				SwapTEdit(theWD);
			theWD^^.xStr := '0.00';
			InvalRect(theWD^^.te2Rect);
			str := 'New Factor';
			TESetText(Ptr(LONGINT(@str) + 1), Length(str), theWD^^.teHdl);
			TEActivate(theWD^^.teHdl);
			ZapTextRects(theWD);
			rNum := cMaxFacts + 99;
			rNum := LAddRow(1, rNum, theWD^^.factList);
			SetPt(theCell, 0, rNum);
			LSetSelect(TRUE, theCell, theWD^^.factList);
			LRect(cRect, theCell, theWD^^.factList);
			CheckListScroll(theCell, theWD^^.factList);
			theWD^^.curFactor := theCell.v + 1;
			EnterText(theWD);
			TESetSelect(0, 32000, theWD^^.teHdl);
			theWD^^.windDirty := TRUE;
			END;
		END;
	END; {NewFactor}

	PROCEDURE RemoveFactor (theWD: WDHandle);
		VAR
			theCell: Cell;
			i, j: INTEGER;
			theModel: TModelHdl;
	BEGIN
	theModel := theWD^^.theModel;
	SetPt(theCell, 0, 0);
	IF LGetSelect(TRUE, theCell, theWD^^.factList) THEN
		BEGIN
		IF theModel^^.lastResults <> NIL THEN
			BEGIN
			DisposHandle(Handle(theModel^^.lastResults));
			theModel^^.lastResults := NIL;
			END;
		LSetSelect(FALSE, theCell, theWD^^.factList);
		ResetFactor(theWD^^.theModel, theCell.v + 1);
		LDelRow(1, theCell.v, theWD^^.factList);
		IF theCell.v < (theModel^^.nFacts - 1) THEN
			BEGIN
			FOR i := (theCell.v + 2) TO theModel^^.nFacts DO
				BEGIN
				theModel^^.facts^^[i - 1] := theModel^^.facts^^[i];
				FOR j := 1 TO theModel^^.nFacts DO
					BEGIN
					theModel^^.crGrowths^^[i - 1, j] := theModel^^.crGrowths^^[i, j];
					theModel^^.crGrowths^^[j, i - 1] := theModel^^.crGrowths^^[j, i];
					END;
				END;
			END;
		IF theWD^^.curFactor > 1 THEN
			SetPt(theCell, 0, theWD^^.curFactor - 1);
		LSetSelect(TRUE, theCell, theWD^^.factList);
		InvalRect(theWD^^.factList^^.rView);
		theModel^^.nFacts := theModel^^.nFacts - 1;
		theWD^^.windDirty := TRUE;
		theWD^^.curFactor := 0;
		FactListClick(theWD, FALSE);
		END;
	END; {RemoveFactor}

	PROCEDURE FactorAttributes (theWD: WDHandle);
		VAR
			newTarg: BOOLEAN;
	BEGIN
	IF SetFactAttributes(theWD^^.theModel, theWD^^.curFactor, newTarg) THEN
		BEGIN
		theWD^^.windDirty := TRUE;
		IF newtarg THEN
			NewFactTarget(theWD^^.theModel, theWD^^.curFactor);
		END;
	END; {FactorAttributes}

	PROCEDURE ResetFactor (theModel: TModelHdl;
									theFact: INTEGER);
		VAR
			i: INTEGER;
	BEGIN
	MoveHHi(Handle(theModel));
	HLock(Handle(theModel));
	MoveHHi(Handle(theModel^^.facts));
	HLock(Handle(theModel^^.facts));
	WITH theModel^^, facts^^[theFact] DO
		BEGIN
		flags := 0;
		SetFactFlag(flags, graphIt, TRUE);
		startVal := dfltStartVal;
		altVal := 0.0;
		targetVal := INF;
		gType := dfltGType;
		onLine.start := 1;
		onLine.finish := 9999;
		gBounds.lower := -INF;
		gBounds.upper := INF;
		gTHolds.lower := -INF;
		gTHolds.upper := INF;
		yMax := 0.0;
		yMin := 0.0;
		pad1 := 0;
		pad2 := 0;
		pad3 := 0;
		FOR i := 1 TO cMaxGPoints DO
			BEGIN
			WITH factShocks[i] DO
				BEGIN
				period := 0;
				gVal := 0.0;
				END;
			WITH vGrowths[i] DO
				BEGIN
				pt := 0.0;
				val := 0.0;
				END;
			END;
		END;
	FOR i := 1 TO theModel^^.nFacts DO
		BEGIN
		theModel^^.crGrowths^^[theFact, i] := 0.0;
		theModel^^.crGrowths^^[i, theFact] := 0.0;
		END;
	HUnlock(Handle(theModel));
	HUnlock(Handle(theModel^^.facts));
	END; {ResetFactor}

	PROCEDURE SetModelPrefs (theWD: WDHandle);
	BEGIN
	IF ModelPrefsDlog(theWD^^.theModel) THEN
		;
	END; {SetModelPrefs}

	PROCEDURE InitBrowser (theWD: WDHandle;
									nGraphs: INTEGER);
		VAR
			theCell: Cell;
			nRows, rNum, i: INTEGER;
			pCell: TPicCell;
			gbWind: WDHandle;
	BEGIN
	gbWind := theWD^^.gBrowser;
	LDoDraw(FALSE, gbWind^^.gbList);
	LDelRow(0, 999, gbWind^^.gbList);
	nRows := nGraphs DIV cGListCols;
	IF nGraphs MOD cGListCols > 0 THEN
		nRows := nRows + 1;
	rNum := LAddRow(nRows, 0, gbWind^^.gbList);
	SetPt(theCell, 0, 0);
	FOR i := 1 TO nGraphs DO
		BEGIN
		pCell.graphNum := i;
		LSetCell(@pCell, SizeOf(TPicCell), theCell, gbWind^^.gbList);
		IF theCell.h = (cGListCols - 1) THEN
			BEGIN
			theCell.h := 0;
			theCell.v := theCell.v + 1;
			END
		ELSE
			theCell.h := theCell.h + 1;
		END;
	LDoDraw(TRUE, gbWind^^.gbList);
	END; {InitBrowser}


	PROCEDURE InitGraphArray (theModel: TModelHdl);
		VAR
			i: INTEGER;
	BEGIN
	IF theModel^^.graphArray = NIL THEN
		BEGIN
		theModel^^.graphArray := TGraphHdl(NewHandle(SizeOf(TGraphArray)));
		CheckMemErr;
		IF gotMemErr THEN
			theModel^^.graphArray := NIL
		ELSE
			FOR i := 1 TO cMaxGraphs DO
				theModel^^.graphArray^^[i].picHdl := NIL;
		END
	ELSE
		FOR i := 1 TO cMaxGraphs DO
			BEGIN
			IF theModel^^.graphArray^^[i].picHdl <> NIL THEN
				KillPicture(theModel^^.graphArray^^[i].picHdl);
			theModel^^.graphArray^^[i].picHdl := NIL;
			END;
	END; {InitGraphArray}


	PROCEDURE DoSimulation (theWD: WDHandle);
		VAR
			nGraphs: INTEGER;
	BEGIN
	SetCursor(watchHdl^^);
	RunSimulation(theWD^^.theModel, NIL, FALSE, (theWD^^.showValPopM.lastSel = srchValCmd));
	IF theWD^^.theModel^^.lastResults <> NIL THEN
		BEGIN
		InitGraphArray(theWD^^.theModel);
		nGraphs := GraphRun(theWD^^.theModel);
		IF nGraphs > 0 THEN
			BEGIN
			IF theWD^^.gBrowser = NIL THEN
				BEGIN
				NewGWindow(Concat(theWD^^.wName, ' - Graphs'), graphBrowse);
				theWD^^.gBrowser := wdList;
				wdList^^.owner := theWD;
				wdList^^.gbList^^.userHandle := HANDLE(theWD^^.theModel);
				wdList^^.gbList^^.selFlags := lNoRect + lNoNilHilite;
				END
			ELSE
				SelectWindow(theWD^^.gBrowser^^.thisWindow);
			InitBrowser(theWD, nGraphs);
			END;
		END;
	END; {DoSimulation}


	PROCEDURE RescaleGraphWind (theWD: WDHandle;
									auto: BOOLEAN);
		VAR
			theModel: TModelHdl;
			grafArray: TGraphHdl;
			fIdx, grNum: INTEGER;
			upper, lower: DOUBLE;
			targ, ok: BOOLEAN;
			oErr: OSErr;
			theCell: Cell;
			dLen, resp, itn: INTEGER;
			cellRec: TPicCell;

		PROCEDURE RescaleGraph (grNum, fIdx: INTEGER);
		BEGIN
		theModel^^.facts^^[fIdx].yMax := upper;
		theModel^^.facts^^[fIdx].yMin := lower;
		KillPicture(grafArray^^[grNum].picHdl);
		targ := TstFactFlag(theModel^^.facts^^[fIdx].flags, targeted);
		GraphFact(theModel, grNum, fIdx, targ, lower, upper);
		IF theWD^^.windType = graphWind THEN
			BEGIN
			theWD^^.picHdl := grafArray^^[grNum].picHdl;
			oErr := HandToHand(Handle(theWD^^.picHdl));
			theWD^^.grID := grafArray^^[grNum].grID;
			InvalRect(theWD^^.thisWindow^.portRect);
			END;
		TextFont(monaco);
		TextSize(9);
		END; {RescaleGraph}

	BEGIN
	theModel := theWD^^.owner^^.theModel;
	grafArray := theModel^^.graphArray;
	IF grafArray <> NIL THEN
		CASE theWD^^.windType OF
		graphWind: 
			BEGIN
			grNum := theWD^^.grNum;
			IF grafArray^^[grNum].grID = theWD^^.grID THEN
				IF FactIDIdx(theModel, grafArray^^[grNum].factID, fIdx) THEN
					BEGIN
					upper := theModel^^.facts^^[fIdx].yMax;
					lower := theModel^^.facts^^[fIdx].yMin;
					IF auto THEN
						BEGIN
						upper := 0.0;
						lower := 0.0;
						ok := TRUE;
						END
					ELSE
						ok := SetVBounds(lower, upper, -INF, INF, FALSE);
					IF ok THEN
						RescaleGraph(grNum, fIdx);
					END;
			END;
		graphBrowse: 
			BEGIN
			ok := TRUE;
			upper := 0.0;
			lower := 0.0;
			resp := 1;
			IF auto THEN
				BEGIN
				SetPt(theCell, 0, 0);	{More than one cell selected?}
				IF LGetSelect(TRUE, theCell, theWD^^.gbList) THEN
					IF LNextCell(TRUE, TRUE, theCell, theWD^^.gbList) THEN
						IF LGetSelect(TRUE, theCell, theWD^^.gbList) THEN
							BEGIN	{Two or more graphs selected}
							resp := ThreeBtnAlert('Scale all graphs to the same scale?', 'YES', 'NO', 'CANCEL');
							IF resp = 1 THEN
								BEGIN
								IF theModel^^.lastResults <> NIL THEN
									BEGIN
									ok := TRUE;
									lower := INF;
									upper := -INF;
									SetPt(theCell, 0, 0);
									WHILE LGetSelect(TRUE, theCell, theWD^^.gbList) DO
										BEGIN
										dLen := SizeOf(TPicCell);
										LGetCell(@cellRec, dLen, theCell, theWD^^.gbList);
										IF dLen = SizeOf(TPicCell) THEN
											IF FactIDIdx(theModel, grafArray^^[cellRec.graphNum].factID, fIdx) THEN
												BEGIN
												FOR itn := 1 TO theModel^^.iteratns DO
													IF theModel^^.lastResults^^[itn, fIdx] < lower THEN
														lower := theModel^^.lastResults^^[itn, fIdx]
													ELSE IF theModel^^.lastResults^^[itn, fIdx] > upper THEN
														upper := theModel^^.lastResults^^[itn, fIdx];
												IF TstFactFlag(theModel^^.facts^^[fIdx].flags, targeted) THEN
													IF theModel^^.facts^^[fIdx].targetVal < lower THEN
														lower := theModel^^.facts^^[fIdx].targetVal
													ELSE IF theModel^^.facts^^[fIdx].targetVal > upper THEN
														upper := theModel^^.facts^^[fIdx].targetVal
												END;
										BeachBallCursor;
										IF NOT LNextCell(TRUE, TRUE, theCell, theWD^^.gbList) THEN
											theCell.v := 999;
										END;
									END;
								END;
							END;
				END
			ELSE
				ok := SetVBounds(lower, upper, -INF, INF, FALSE);
			IF ok AND (resp <> 3) THEN
				BEGIN
				SetPt(theCell, 0, 0);
				WHILE LGetSelect(TRUE, theCell, theWD^^.gbList) DO
					BEGIN
					BeachBallCursor;
					dLen := SizeOf(TPicCell);
					LGetCell(@cellRec, dLen, theCell, theWD^^.gbList);
					IF dLen = SizeOf(TPicCell) THEN
						IF FactIDIdx(theModel, grafArray^^[cellRec.graphNum].factID, fIdx) THEN
							BEGIN
							RescaleGraph(cellRec.graphNum, fIdx);
							END;
					IF NOT LNextCell(TRUE, TRUE, theCell, theWD^^.gbList) THEN
						theCell.v := 999;
					END;
				InvalRect(theWD^^.thisWindow^.portRect);
				END;
			END;
		END;
	END; {RescaleGraphWind}

	PROCEDURE RenamePopnWind (modelWD: WDHandle);
	BEGIN

	END; {RenamePopnWind}

	PROCEDURE DelPopnWind (modelWD: WDHandle);
	BEGIN

	END; {DelPopnWind}

	FUNCTION NewPopnWind (popnHdl: TPopnHdl;
									name: Str255;
									modelWD: WDHandle): WDHandle;
		VAR
			theWD: WDhandle;
			refNum, rErr: INTEGER;
	BEGIN
	refNum := OpenResFork(modelWD^^.modelFile, rErr);
	NewGWindow(name, popnWind);
	IF refNum <> 0 THEN
		CloseResFile(refNum);
	theWD := wdList;
	popnHdl^^.myWD := theWD;
	theWD^^.popnHdl := Handle(popnHdl);
	theWD^^.owner := modelWD;
	theWD^^.windDirty := TRUE;
	theWD^^.resID := 0;
	IF theWD^^.wName <> 'xxx' THEN
		AppendMenu(theMenus[popnSelM], theWD^^.wName);
	NewPopnWind := theWD;
	END; {NewPopnWind}

	PROCEDURE CreatePopulation (topWD: WDHandle);
		VAR
			popn: TPopnHdl;
			popWD, modelWD: WDhandle;
			pName, str: Str255;
			refNum, rErr: INTEGER;
			ok: BOOLEAN;

	BEGIN
	IF topWD^^.windType = modelWind THEN
		modelWD := topWD
	ELSE
		modelWD := topWD^^.owner;

	refNum := OpenResFork(modelWD^^.modelFile, rErr);
	IF refNum = 0 THEN
		OneBtnAlert('Cant open resource fork. Cant create population.')
	ELSE
		BEGIN
		pName := UniqueWindName(popnWind, 'Population');
		REPEAT
			ok := GetNameDlog('Population Name', pName);
			IF ok THEN
				BEGIN
				str := UniqueWindName(popnWind, pName);
				ok := EqualString(str, pName, FALSE, FALSE);
				IF NOT ok THEN
					OneBtnAlert('Population name is not unique. Please enter an unique name.');
				END;
		UNTIL ok;
		CloseResFile(refNum);

		IF ok THEN
			IF NewPopn(popn) THEN
				BEGIN
				popn^^.theModel := modelWD^^.theModel;
				popn^^.bestFitStats := NIL;
				popn^^.worstFitStats := NIL;
				popn^^.avgFitStats := NIL;
				popn^^.growths := NIL;
				InitPopn(popn);
				popWD := NewPopnWind(popn, pName, modelWD);
				END;
		END;
	END; {CreatePopulation}

	PROCEDURE GetPopulation (theWD: WDHandle;
									menuItem: INTEGER);
		VAR
			str, mStr: Str255;
			twd, modelWD, popWD: WDHandle;
			popHdl: Handle;
			popn: TPopnHdl;
			refNum, rErr, i, nMItems: INTEGER;
			rID: INTEGER;
			rType: ResType;
			rName: Str255;

	BEGIN
	IF theWD^^.windType = modelWind THEN
		modelWD := theWD
	ELSE
		modelWD := theWD^^.owner;

	GetItem(theMenus[popnSelM], menuItem, str);
	twd := wdList;
	WHILE (twd <> NIL) AND NOT EqualString(str, twd^^.wName, FALSE, FALSE) DO
		twd := twd^^.next;
	IF twd <> NIL THEN
		SelectWindow(twd^^.thisWindow)
	ELSE
		BEGIN
		refNum := OpenResFork(modelWD^^.modelFile, rErr);
		IF refNum <> 0 THEN
			BEGIN
			popHdl := GetNamedResource(popnWindResType, str);
			IF popHdl = NIL THEN
				OneBtnAlert('Cant load population')
			ELSE
				BEGIN
				GetResInfo(popHdl, rID, rType, rName);
				DetachResource(popHdl);
				popn := TPopnHdl(popHdl);
				popn^^.theModel := modelWD^^.theModel;
				popn^^.bestFitStats := NIL;
				popn^^.worstFitStats := NIL;
				popn^^.avgFitStats := NIL;
				popn^^.growths := NIL;
				popWD := NewPopnWind(popn, 'xxx', modelWD);
				popWD^^.resID := rID;
{Fix up the window names}
				i := 1;
				nMItems := CountMItems(theMenus[windM]);
				REPEAT
					i := i + 1;
					GetItem(theMenus[windM], i, mStr);
				UNTIL EqualString(popWD^^.wName, mStr, FALSE, FALSE) OR (i >= nMItems);
				IF i <= nMItems THEN
					SetItem(theMenus[windM], i, rName);
				popWD^^.wName := str;
				SetWTitle(popWD^^.thisWindow, rName);
				popWD^^.windDirty := FALSE;
				END;
			CloseResFile(refNum);
			END;
		END;
	END; {GetPopulation}

	PROCEDURE ClonePopulation (popWD: WDHandle);

		VAR
			popn: TPopnHdl;
			pName, str: Str255;
			refNum, rErr: INTEGER;
			ok: BOOLEAN;
			oErr: OSErr;
			newPopWD: WDHandle;

	BEGIN
	refNum := OpenResFork(popWD^^.owner^^.modelFile, rErr);
	IF refNum = 0 THEN
		OneBtnAlert('Cant open resource fork. Cant create population.')
	ELSE
		BEGIN
		pName := UniqueWindName(popnWind, popWD^^.wName);
		REPEAT
			ok := GetNameDlog('Population Name:', pName);
			IF ok THEN
				BEGIN
				str := UniqueWindName(popnWind, pName);
				ok := EqualString(str, pName, FALSE, FALSE);
				IF NOT ok THEN
					OneBtnAlert('Population name is not unique. Please enter an unique name.');
				END;
		UNTIL ok;
		CloseResFile(refNum);

		IF ok THEN
			BEGIN
			popn := TPopnHdl(popWD^^.popnHdl);
			oErr := HandToHand(Handle(popn));
			CheckMemErr;
			IF NOT gotMemErr THEN
				BEGIN
				popn^^.growths := NIL;
				IF popn^^.bestFitStats <> NIL THEN
					BEGIN
					oErr := HandToHand(Handle(popn^^.bestFitStats));
					CheckMemErr;
					END;
				END;
			IF NOT gotMemErr THEN
				IF popn^^.worstFitStats <> NIL THEN
					BEGIN
					oErr := HandToHand(Handle(popn^^.worstFitStats));
					CheckMemErr;
					END;
			IF NOT gotMemErr THEN
				IF popn^^.avgFitStats <> NIL THEN
					BEGIN
					oErr := HandToHand(Handle(popn^^.avgFitStats));
					CheckMemErr;
					END;
			IF NOT gotMemErr THEN
				BEGIN
				newPopWD := NewPopnWind(popn, pName, popWD^^.owner);
				END
			ELSE
				OneBtnAlert('Insufficient memory to clone population.');
			END;
		END;
	END; {ClonePopulation}


	PROCEDURE SaveSearchStats (theWD: WDHandle);
		VAR
			theFile: TFile;
			crCh, tabCh: STRING[1];
			str, str1: Str255;
			dataHdl: Handle;
			dataLen: LONGINT;
			fErr: OSErr;
			gen: INTEGER;
			popn: TPopnHdl;
	BEGIN
	IF theWD^^.windType = popnWind THEN
		BEGIN
		popn := TPopnHdl(theWD^^.popnHdl);
		IF (popn^^.bestFitStats = NIL) OR (popn^^.worstFitStats = NIL) OR (popn^^.avgFitStats = NIL) THEN
			SysBeep(5)
		ELSE
			BEGIN
			crCh[0] := CHR(1);
			crCh[1] := CHR($0D);
			tabCh[0] := CHR(1);
			tabCh[1] := CHR($09);
			InitFileRec(theFile, Concat(theWD^^.wName, ' - Stats'), 'TEXT');
			IF NewFile(theFile, cGenieSign, TRUE, 'Save Stats:') THEN
				BEGIN
				dataHdl := NewHandle(cMinDataBlk);
				dataLen := 0;
				fErr := SetFPos(theFile.refNum, fsFromStart, 0);
				fErr := SetEOF(theFile.refNum, 0);
				str := Concat('Best Fit', tabCh, 'Worst Fit', tabCh, 'Avg. Fit', crCh);
				AddStrToData(dataHdl, dataLen, str);
				FOR gen := 1 TO popn^^.genNum DO
					BEGIN
					str := NumToDecStr(8, popn^^.bestFitStats^^[gen]);
					str := Concat(str, tabCh);
					str1 := NumToDecStr(8, popn^^.worstFitStats^^[gen]);
					str := Concat(str, str1, tabCh);
					str1 := NumToDecStr(8, popn^^.avgFitStats^^[gen]);
					str := Concat(str, str1, crCh);
					AddStrToData(dataHdl, dataLen, str);
					END;
				fErr := FSWrite(theFile.refNum, dataLen, dataHdl^);
				IF fErr <> noErr THEN
					OneBtnAlert('File write failed');
				DisposHandle(dataHdl);
				fErr := fsClose(theFile.refNum);
				fErr := FlushVol(NIL, theFile.vRefNum);
				END;
			END;
		END;
	END; {SaveSearchStats}

	PROCEDURE RescalePopnGraph (popWD: WDHandle;
									thePart: INTEGER);
		VAR
			popn: TPopnHdl;
			min, max, low, up: DOUBLE;
			i: INTEGER;
	BEGIN
	popn := TPopnHdl(popWD^^.popnHdl);
	CASE thePart OF
	1: {Rescale to global max & min}
		BEGIN
		min := INF;
		max := -INF;
		FOR i := 1 TO popn^^.genNum DO
			BEGIN
			IF popn^^.bestFitStats <> NIL THEN
				BEGIN
				IF popn^^.bestFitStats^^[i] < min THEN
					min := popn^^.bestFitStats^^[i];
				IF popn^^.bestFitStats^^[i] > max THEN
					max := popn^^.bestFitStats^^[i];
				END;
			IF popn^^.worstFitStats <> NIL THEN
				BEGIN
				IF popn^^.worstFitStats^^[i] < min THEN
					min := popn^^.worstFitStats^^[i];
				IF popn^^.worstFitStats^^[i] > max THEN
					max := popn^^.worstFitStats^^[i];
				END;
			IF popn^^.avgFitStats <> NIL THEN
				BEGIN
				IF popn^^.avgFitStats^^[i] < min THEN
					min := popn^^.avgFitStats^^[i];
				IF popn^^.avgFitStats^^[i] > max THEN
					max := popn^^.avgFitStats^^[i];
				END;
			END;
		DrawPGraphAxes(popWD, 0, 0, min, max);
		END;
	2: {Rescale x axis}
		BEGIN
		min := 1;
		max := popn^^.maxGenerations;
		low := popWD^^.pGraphInfo.xMin;
		up := popWD^^.pGraphInfo.xMax;
		IF SetVBounds(low, up, min, max, TRUE) THEN
			DrawPGraphAxes(popWD, low, up, popWD^^.pGraphInfo.yMin, popWD^^.pGraphInfo.yMax);
		END;
	3: {Rescale y Axis}
		BEGIN
		min := 0;
		max := INF;
		low := popWD^^.pGraphInfo.yMin;
		up := popWD^^.pGraphInfo.yMax;
		IF SetVBounds(low, up, min, max, FALSE) THEN
			DrawPGraphAxes(popWD, popWD^^.pGraphInfo.xMin, popWD^^.pGraphInfo.xMax, low, up);
		END;
	END;
	END; {RescalePopnGraph}

	PROCEDURE ShowPopnGraph (theWD: WDHandle;
									show: BOOLEAN);

	BEGIN
	IF theWD^^.windType = popnWind THEN
		BEGIN
		theWD^^.showGraph := show;
		InvalRect(theWD^^.iconRect);
		InvalRect(theWD^^.rescaleRect);
		IF show THEN
			BEGIN
			SizeWindow(theWD^^.thisWindow, (WDWidth(theWD) + 16), cPopnFullHeight, TRUE);
			DrawPGraphAxes(theWD, 0, 0, 0, 0);
			END
		ELSE
			BEGIN
			SizeWindow(theWD^^.thisWindow, (WDWidth(theWD) + 16), cPopnHalfHeight, TRUE);
			END;
		END;
	END; {ShowPopnGraph}

END. {GControl}