UNIT GDriver;

INTERFACE
	USES
		GFiles, DialogUtils, GCommonDec, GGlobals, GDialogs, GATypes, GADialogs, Genetic, GControl, Simulation;



	PROCEDURE RunGenie (quiet: BOOLEAN);
	PROCEDURE ChangeMenuBar (mBarHdl: Handle);
	PROCEDURE UpdateMenus (theWD: WDHandle);


IMPLEMENTATION
{$S DriverSeg}

	VAR
		lastMousePt: Point;
		lastGRect: Rect;
		lastTargInv: BOOLEAN;

	PROCEDURE SetCursorType;
		VAR
			theWD: WDHandle;
			mousePt: Point;

		FUNCTION GrafCursor: BOOLEAN;

			VAR
				str: Str255;
				xVal, grNum, xPt, yPt: INTEGER;
				yVal: DOUBLE;
				theModel: TModelHdl;
				grafArray: TGraphHdl;
				r, r1: Rect;
				tmpBool: BOOLEAN;

		BEGIN
		tmpBool := FALSE;
		theModel := theWD^^.owner^^.theModel;
		grafArray := theModel^^.graphArray;
		grNum := theWD^^.grNum;
		IF grafArray <> NIL THEN
			IF grafArray^^[grNum].grID = theWD^^.grID THEN
				IF PtInRect(mousePt, grafArray^^[grNum].grRect) THEN
					BEGIN
					r1 := theWD^^.thisWindow^.portRect;
					WITH r1 DO
						BEGIN
						right := right - 15;
						bottom := bottom - 15;
						END;
					ClipRect(r1);
					tmpBool := TRUE;
					xPt := mousePt.h;
					yPt := mousePt.v;
					WITH grafArray^^[grNum] DO
						BEGIN
						xVal := ROUND((xPt - org.h) / xScale);
						yVal := (yPt - org.v) / yScale + yMin;
						END;
					NumToString(xVal, str);
					str := Concat(str, ',', NumToDecStr(3, yVal));
					r := grafArray^^[grNum].grRect;
					WITH r DO
						BEGIN
						left := right + 5;
						right := left + 90;
						top := mousePt.v;
						bottom := top + 15;
						END;
					EraseRect(lastGRect);
					lastGRect := r;
					MoveTo(r.left + 4, r.bottom - 4);
					DrawString(str);
					FrameRect(r);
					r1 := grafArray^^[grNum].grRect;
					WITH r1 DO
						left := right - 15;
					IF PtInRect(mousePt, r1) THEN
						BEGIN
						InvertRect(r);
						SetCursor(crossHdl^^);
						WITH grafArray^^[grNum].targPt DO
							SetRect(r, h - 4, v - 4, h + 4, v + 4);
						IF ptInRect(mousePt, r) THEN
							BEGIN
							IF NOT lastTargInv THEN
								BEGIN
								lastTargInv := TRUE;
								InvertRect(r);
								END;
							END
						ELSE IF lastTargInv THEN
							BEGIN
							lastTargInv := FALSE;
							InvertRect(r);
							END;
						END
					ELSE
						BEGIN
						IF lastTargInv THEN
							BEGIN
							WITH grafArray^^[grNum].targPt DO
								SetRect(r, h - 4, v - 4, h + 4, v + 4);
							InvertRect(r);
							lastTargInv := FALSE;
							END;
						SetCursor(bullsEyeHdl^^);
						END
					END
				ELSE IF NOT EmptyRect(lastGRect) THEN
					BEGIN
					EraseRect(lastGRect);
					SetRect(lastGRect, 0, 0, 0, 0);
					END;
		GrafCursor := tmpBool;
		END; {GrafCursor}

	BEGIN
	GetMouse(mousePt);
	theWD := OwnerWindowData(FrontWindow);
	IF theWD <> NIL THEN
		CASE theWD^^.windType OF
		modelWind: 
			IF NOT EqualPt(mousePt, lastMousePt) THEN
				IF PtInRect(mousePt, theWD^^.te1Rect) OR PtInRect(mousePt, theWD^^.te2Rect) THEN
					SetCursor(iBeamHdl^^)
				ELSE
					SetCursor(arrow);
		graphWind: 
			IF NOT GrafCursor THEN
				SetCursor(arrow);
		graphBrowse: 
			IF NOT EqualPt(mousePt, lastMousePt) THEN
				SetCursor(arrow);
		waitWind: 
			IF NOT EqualPt(mousePt, lastMousePt) THEN
				SetCursor(watchHdl^^);
		popnWind: 
			SetCursor(arrow);
		END
	ELSE
		SetCursor(arrow);
	lastMousePt := mousePt;
	END; {SetCursorType}


	VAR
		lastMask: INTEGER;

	PROCEDURE SetMenuByMask (maskID: INTEGER);

		TYPE
			MaskArry = ARRAY[1..5] OF RECORD
					menuID: INTEGER;
					mask: LONGINT;
				END;
			MaskRec = RECORD
					nMasks: INTEGER;
					masks: maskArry;
				END;
			MaskPtr = ^MaskRec;
			MaskHdl = ^MaskPtr;

		VAR
			maskInfo: MaskHdl;
			i, j, mID: INTEGER;
			flags: LONGINT;
			mHdl: MenuHandle;

	BEGIN
	maskInfo := MaskHdl(GetResource('MMSK', maskID));
	IF maskInfo = NIL THEN
		debugStr('Nil mask');
	IF (ResError = 0) AND (maskInfo <> NIL) THEN
		BEGIN
		FOR i := 1 TO maskInfo^^.nMasks DO
			BEGIN
			flags := maskInfo^^.masks[i].mask;
			mID := maskInfo^^.masks[i].menuID;
			mHdl := GetMHandle(mID);
			FOR j := 0 TO MIN(31, CountMItems(mHdl)) DO
				IF BitTst(@flags, j) THEN
					EnableItem(mHdl, j)
				ELSE
					DisableItem(mHdl, j);
			END;
		ReleaseResource(Handle(maskInfo));
		END;
	lastMask := maskID;
	END; {SetMenuByMask}

	PROCEDURE ChangeMenuBar (mBarHdl: Handle);
	BEGIN
	SetMenuBar(mBarHdl);
	InsertMenu(theMenus[popnSelM], -1);
	END; {ChangeMenuBar}

	PROCEDURE UpdateMenus (theWD: WDHandle);
		CONST
			noWindmask = 500;
			fileMMask = 501;
			modelMask1 = 1001;
			modelMask2 = 1002;
			modelMask3 = 1003;
			grafMask1 = 2001;
			grafMask2 = 2002;
			browseMask1 = 2010;
			browseMask2 = 2011;
			popnMask1 = 3001;
			popnMask2 = 3002;
			popnSelMask1 = 4001;
			popnSelMask2 = 4002;

		VAR
			nMItems: INTEGER;
			theCell: Cell;
			popn: TPopnHdl;

		FUNCTION CheckPaste: BOOLEAN;
			VAR
				offset: LONGINT;
		BEGIN
		CheckPaste := FALSE; {GetScrap(NIL, 'TEXT', offset) > 0}
		END; {CheckPaste}

		FUNCTION CheckCutCopy: BOOLEAN;
		BEGIN
		CheckCutCopy := FALSE;
		END; {CheckCutCopy}

	BEGIN
	IF theWD = NIL THEN
		BEGIN
		ChangeMenuBar(modelMBar);
		SetMenuByMask(noWindMask);
		END
	ELSE
		BEGIN
		SetMenuByMask(fileMMask);
		nMItems := CountMItems(theMenus[popnSelM]);
		IF nMItems > (delPopnWindCmd + 1) THEN
			SetMenuByMask(popnSelMask1)
		ELSE
			SetmenuByMask(popnSelMask2);
		CASE theWD^^.windType OF
		modelWind: 
			BEGIN
			ChangeMenuBar(modelMBar);
			IF (theWD^^.theModel = NIL) OR (theWD^^.theModel^^.nFacts = 0) THEN
				SetMenuBymask(modelMask1)
			ELSE IF theWD^^.curFactor <> 0 THEN
				SetMenuByMask(modelMask3)
			ELSE
				SetMenuByMask(modelMask2);
			END;
		graphWind: 
			BEGIN
			ChangeMenuBar(graphMBar);
			IF theWD^^.owner^^.theModel^^.lastResults = NIL THEN
				SetMenuByMask(grafMask2)
			ELSE
				SetMenuByMask(grafMask1);
			END;
		graphBrowse: 
			BEGIN
			ChangeMenuBar(graphMBar);
			IF theWD^^.owner^^.theModel^^.lastResults = NIL THEN
				SetMenuByMask(browseMask1)
			ELSE
				BEGIN
				SetPt(theCell, 0, 0);
				IF LGetSelect(TRUE, theCell, theWD^^.gbList) THEN
					SetMenuByMask(browseMask2)
				ELSE
					SetMenuByMask(browseMask1);
				END;
			END;
		waitWind: 
			BEGIN
			ChangeMenuBar(modelMBar);
			SetMenuByMask(noWindMask);
			END;
		popnWind: 
			BEGIN
			popn := TPopnHdl(theWD^^.popnHdl);
			ChangeMenuBar(popnMBar);
			IF popn^^.nChrom = 0 THEN
				SetMenuByMask(popnMask1)
			ELSE
				SetMenuBymask(popnMask2);
			END;
		END;
		IF theWD^^.windDirty THEN
			EnableItem(theMenus[fileM], saveCmd)
		ELSE
			DisableItem(theMenus[fileM], saveCmd)
		END;

	DrawMenuBar;
	END; {UpdateMenus}

	PROCEDURE DoWindMenu (menuItem: INTEGER);
		VAR
			twd: WDHandle;
			str: Str255;
	BEGIN
	GetItem(theMenus[windM], menuItem, str);
	twd := wdList;
	WHILE (twd <> NIL) AND (twd^^.wName <> str) DO
		twd := twd^^.next;
	IF twd <> NIL THEN
		SelectWindow(twd^^.thisWindow);
	END; {DoWindMenu}

	PROCEDURE DoCommand (mResult: LONGINT);
		VAR
			theItem: INTEGER; {menu item number from mResult low-order word}
			theMenu: INTEGER; {menu number from mResult high-order word}
			name: Str255; {desk accessory name}
			temp: INTEGER;
			templ: LONGINT;
			savePort: GrafPtr;
			theWD: WDHandle;
			critParm: BOOLEAN;

	BEGIN
	IF mResult <> 0 THEN
		BEGIN
		theWD := OwnerWindowData(FrontWindow);
		theItem := LoWord(mResult); {call Toolbox Utility routines to}
		theMenu := HiWord(mResult); {set menu item number and menu number}

		CASE theMenu OF {case on menu ID}
		appleID: 
			IF (theItem = aboutMeCmd) THEN
				BEGIN
				END
			ELSE
				BEGIN {call Menu Manager to get desk acc.name, and call Desk Mgr. to open accessory (OpenDeskAcc result not used) }
				GetPort(savePort); {Save current port}
				GetItem(theMenus[appleM], theItem, name);
				temp := OpenDeskAcc(name);
				SetPort(savePort); {Restore port}
				END; {of appleID}
		fileID: 
			CASE theItem OF
			newMCmd: 
				NewModel;
			openMCmd: 
				OpenModel;
			closeCmd: 
				IF CloseWindow(FrontWindow, TRUE) THEN
					;
			saveCmd: 
				SaveWindow(theWD);
			saveAsCmd: 
				SaveWindAs(theWD);
			quitCmd: 
				quitGenie := TRUE;
			END;

		editID: 
			IF NOT SystemEdit(theItem - 1) THEN {command if desk accessory window is the active window}
				CASE theItem OF {case on menu item (command) number call TextEdit to handle command}
				cutCmd: 
					DoCut(theWD);
				copyCmd: 
					DoCopy(theWD);
				pasteCmd: 
					DoPaste(theWD);
				clearCmd: 
					DoClear(theWD);
				SelAllCmd: 
					DoSelectAll(theWD);
				END;
		modelMenuID: 
			CASE theItem OF
			newFactCmd: 
				NewFactor(theWD);
			removeFactCmd: 
				RemoveFactor(theWD);
			resetFactCmd: 
				ResetFactor(theWD^^.theModel, theWD^^.curFactor);
			setLinksCmd: 
				FactListClick(theWD, TRUE);
			factAttribCmd: 
				FactorAttributes(theWD);
			modelPrefsCmd: 
				SetModelPrefs(theWD);
			startRunCmd: 
				DoSimulation(theWD);
			END;
		windMenuID: 
			DoWindMenu(theItem);
		graphMenuID: 
			CASE theItem OF
			rescaleCmd: 
				RescaleGraphWind(theWD, FALSE);
			autoScaleCmd: 
				RescaleGraphWind(theWD, TRUE);
			END;
		popnMenuID: 
			IF theWD^^.popnHdl <> NIL THEN
				CASE theItem OF
				resetPopnCmd: 
					ResetPopn(TRUE, TPopnHdl(theWD^^.popnHdl));
				popnParmsCmd: 
					IF SetPopnParms(TPopnHdl(theWD^^.popnHdl), critParm) THEN
						BEGIN
						IF critParm THEN
							ResetPopn(TRUE, TPopnHdl(theWD^^.popnHdl));
						InvalRect(theWD^^.thisWindow^.portRect);
						theWD^^.windDirty := TRUE;
						END;
				showGeneCmd: 
					ShowGenePool(TPopnHdl(theWD^^.popnHdl));
				goSearchCmd: 
					GoSearch(theWD);
				END;
		popnSelMenuID: 
			CASE theItem OF
			newPopnWindCmd: 
				CreatePopulation(theWD);
			renamePopnWindCmd: 
				PopnRenameDel(theWD, TRUE);
			delPopnWindCmd: 
				PopnRenameDel(theWD, FALSE);
			OTHERWISE
				GetPopulation(theWD, theItem);
			END;
		END;
		END;
	HiliteMenu(0);
	UpdateMenus(OwnerWindowData(FrontWindow));
	END; {DoCommand}


	PROCEDURE UpdateScrollBars (whichWindow: WindowPtr);
		VAR
			tRect: Rect;
	BEGIN
	tRect := whichWindow^.portRect;
	tRect.left := tRect.right - 16;
	InvalRect(tRect);
	tRect := whichWindow^.portRect;
	tRect.top := tRect.bottom - 16;
	InvalRect(tRect);
	END;


	PROCEDURE DoActivate (theWD: WDHandle;
									theEvent: EventRecord);

		PROCEDURE ActivateWind (active: BOOLEAN);
		BEGIN
		CASE theWD^^.windType OF
		modelWind: 
			BEGIN
			IF (theWD^^.curFactor > 0) AND active THEN
				TEActivate(theWD^^.teHdl)
			ELSE
				TEDeactivate(theWD^^.teHdl);
			LActivate(active, theWD^^.factList);
			LActivate(active, theWD^^.linkList);
			IF active THEN
				BEGIN
				DrawGrowIcon(WindowPtr(theEvent.message));
				ShowControl(theWD^^.vScroll);
				ShowControl(theWD^^.hScroll);
				END
			ELSE
				BEGIN
				HideControl(theWD^^.vScroll);
				HideControl(theWD^^.hScroll);
				END;
			END;
		graphBrowse: 
			BEGIN
			IF active THEN
				DrawGrowIcon(WindowPtr(theEvent.message));
			LActivate(active, theWD^^.gbList);
			END;
		graphWind: 
			IF active THEN
				BEGIN
				DrawGrowIcon(WindowPtr(theEvent.message));
				SetRect(lastGRect, 0, 0, 0, 0);
				LastTargInv := FALSE;
				ShowControl(theWD^^.vScroll);
				ShowControl(theWD^^.hScroll);
				END
			ELSE
				BEGIN
				HideControl(theWD^^.vScroll);
				HideControl(theWD^^.hScroll);
				END;
		waitWind: 
			;
		END;
		END; {ActivateWind}

	BEGIN
	SetPort(theWD^^.thisWindow);
	IF BitAnd(theEvent.modifiers, activeFlag) <> 0 THEN
		BEGIN
		ActivateWind(TRUE);
		UpdateMenus(theWD);
		END
	ELSE
		ActivateWind(FALSE);
	END; {DoActivate}


	PROCEDURE DrawWindow (theWD: WDHandle);
		VAR
			r: Rect;
			pmHdl: MenuHandle;
			str: Str255;
			item, i, prec: INTEGER;
			popHdl: TPopnHdl;
			nStep, stepNum: INTEGER;

		PROCEDURE SearchProgress (thisItn, nItns: INTEGER);
			VAR
				fraction: INTEGER;
				newTh, dInterval, last: INTEGER;
				seg: DOUBLE;

		BEGIN
		seg := (360 / nItns);
		IF thisItn > nItns THEN
			thisItn := nItns;
		last := 0;
		newTh := ROUND(thisItn * seg);
		IF (newTh - last > 1) OR (thisItn = nItns) THEN
			PaintArc(theWD^^.progRect, last, newTh - last);
		END; {SearchProgress}

	BEGIN
	IF theWD <> NIL THEN
		BEGIN
		r := theWD^^.thisWindow^.portRect;
		WITH r DO
			BEGIN
			right := right - 15;
			bottom := bottom - 15;
			END;
		ClipRect(r);
		CASE theWD^^.windType OF
		modelWind: 
			BEGIN
			EraseRect(theWD^^.thisWindow^.portRect);
			DrawPicture(theWD^^.modelPic, theWD^^.contRect);
			LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.factList);
			DrawPopMenu(theWD^^.showValPopM);
			r := theWD^^.factList^^.rView;
			InsetRect(r, -1, -1);
			FrameRect(r);
			LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.linkList);
			r := theWD^^.linkList^^.rView;
			InsetRect(r, -1, -1);
			FrameRect(r);
			TEUpdate(theWD^^.thisWindow^.portRect, theWD^^.teHdl);
			IF theWD^^.inNameTE THEN
				r := theWD^^.te2Rect
			ELSE
				r := theWD^^.te1Rect;
			InsetRect(r, 2, 2);
			TextBox(Ptr(LONGINT(@theWD^^.xStr) + 1), Length(theWD^^.xStr), r, teJustLeft);
			DrawGrowIcon(theWD^^.thisWindow);
			END;
		graphBrowse: 
			BEGIN
			EraseRect(theWD^^.thisWindow^.portRect);
			LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.gbList);
			DrawGrowIcon(theWD^^.thisWindow);
			END;
		graphWind: 
			BEGIN
			EraseRect(theWD^^.thisWindow^.portRect);
			ClipRect(theWD^^.pFrame);
			DrawPicture(theWD^^.picHdl, theWD^^.pFrame);
			DrawGrowIcon(theWD^^.thisWindow);
			END;
		waitWind: 
			BEGIN
			ClipRect(theWD^^.thisWindow^.portRect);
			DrawPicture(theWD^^.waitPic, theWD^^.thisWindow^.portRect);
			END;
		popnWind: 
			BEGIN
			EraseRect(theWD^^.thisWindow^.portRect);
			ClipRect(theWD^^.thisWindow^.portRect);
			DrawPicture(theWD^^.popnWindPic, theWD^^.popnWindPic^^.picFrame);
			TextFont(0);
			TextSize(12);
			TextFont(monaco);
			TextSize(9);
			IF theWD^^.popnHdl <> NIL THEN
				BEGIN
				popHdl := TPopnHdl(theWD^^.popnHdl);
				prec := popHdl^^.theModel^^.numPrec;
				DrawNum(prec, theWD^^.statRects[1], teJustRight, popHdl^^.fitMin);
				DrawNum(prec, theWD^^.statRects[2], teJustRight, popHdl^^.fitMax);
				DrawNum(prec, theWD^^.statRects[3], teJustRight, (popHdl^^.fitSum / popHdl^^.nChrom));
				DrawNum(0, theWD^^.statRects[4], teJustRight, popHdl^^.popMax);
				DrawNum(0, theWD^^.statRects[5], teJustRight, popHdl^^.nChrom);
				DrawNum(0, theWD^^.statRects[6], teJustRight, popHdl^^.maxGenerations);
				DrawNum(0, theWD^^.statRects[7], teJustRight, popHdl^^.genNum);
				nStep := 0;
				stepNum := 0;
				CASE popHdl^^.status OF
				searching: 
					BEGIN
					nStep := popHdl^^.maxGenerations;
					stepNum := popHdl^^.genNum;
					str := 'Searching';
					END;
				stopped: 
					str := 'Stopped';
				paused: 
					str := 'Paused';
				seeding: 
					BEGIN
					nStep := popHdl^^.nSeed;
					stepNum := popHdl^^.nChrom;
					str := 'Seeding';
					END;
				END;
				TextBox(Ptr(LONGINT(@str) + 1), Length(str), theWD^^.statRects[8], teJustLeft);
				END;
			IF theWD^^.showGraph THEN
				BEGIN
				DrawPicture(theWD^^.rscPict, theWD^^.rescaleRect);
				PlotIcon(theWD^^.iconRect, popnWindIcon2);
				END
			ELSE
				PlotIcon(theWD^^.iconRect, popnWindIcon1);
			IF theWD^^.showGraph AND (theWD^^.pGraphInfo.picHdl <> NIL) THEN
				BEGIN
				DrawPicture(theWD^^.pGraphInfo.picHdl, theWD^^.pGraphRect);
				PlotPopnStats(theWD);
				END;
			IF nStep > 0 THEN
				SearchProgress(stepNum, nStep);
			END;
		END;
		END;
	ClipRect(theWD^^.thisWindow^.portRect);
	DrawControls(theWD^^.thisWindow);
	END; { of DrawWindow }

	PROCEDURE DoUpdate (myWindow: WindowPtr);
		VAR
			savePort: GrafPtr;

	BEGIN
	GetPort(savePort);
	SetPort(myWindow);
	BeginUpdate(myWindow);
	DrawWindow(OwnerWindowData(myWindow));
	EndUpdate(myWindow);
	SetPort(savePort);
	END; {of DoUpdate}

	PROCEDURE OffsetWindow (theWD: WDHandle;
									dPt: Point);
		VAR
			tl: POINT;
	BEGIN
	CASE theWD^^.windType OF
	modelWind: 
		BEGIN
		OffsetRect(theWD^^.factList^^.rView, dPt.h, dPt.v);
		OffsetRect(theWD^^.linkList^^.rView, dPt.h, dPt.v);
		OffsetRect(theWD^^.teHdl^^.viewRect, dPt.h, dPt.v);
		OffsetRect(theWD^^.teHdl^^.destRect, dPt.h, dPt.v);
		OffsetRect(theWD^^.te1Rect, dPt.h, dPt.v);
		OffsetRect(theWD^^.te2Rect, dPt.h, dPt.v);
		OffsetRect(theWD^^.contRect, dPt.h, dPt.v);
		AdjustLists(theWD);
		AddPt(dPt, theWD^^.offsetPt);
		END;
	graphWind: 
		BEGIN
		OffsetRect(theWD^^.pFrame, dPt.h, dPt.v);
		AddPt(dPt, theWD^^.offsetPt);
		END;
	END;

	END; {OffsetWindow}


	PROCEDURE DoGrow (theWD: WDHandle;
									theEvent: EventRecord);
{ Handles growing and sizing the window and manipulating the update region. }

		VAR
			longResult: LongInt;
			height, width: INTEGER;
			tRect: Rect;
			dPt: Point;
			zPt: Point;

	BEGIN
	longResult := GrowWindow(theWD^^.thisWindow, theEvent.where, growRect);
	IF longResult <> 0 THEN
		BEGIN
		SetPt(dPt, 0, 0);
		height := HiWord(longResult);
		width := LoWord(longResult);
		UpdateScrollBars(theWD^^.thisWindow);
		SizeWindow(theWD^^.thisWindow, width, height, TRUE);
		CASE theWD^^.windType OF
		modelWind: 
			BEGIN
			WITH theWD^^.offsetPt DO
				SetPt(zPt, -h, -v);
			OffsetWindow(theWD, zPt);
			InvalRect(theWD^^.thisWindow^.portRect);
			MoveScrollBars(theWD);
			AdjustScrollBar(theWD);
			AdjustLists(theWD);
			END;
		graphBrowse: 
			BEGIN
			LSize(width - 15, height - 15, theWD^^.gbList);
			InvalRect(theWD^^.gbList^^.rView);
			END;
		graphWind: 
			BEGIN
			WITH theWD^^.offsetPt DO
				SetPt(zPt, -h, -v);
			OffsetWindow(theWD, zPt);
			MoveScrollBars(theWD);
			AdjustScrollBar(theWD);
			EraseRect(theWD^^.thisWindow^.portRect);
			InvalRect(theWD^^.thisWindow^.portRect);
			END;
		END;
		UpdateScrollBars(theWD^^.thisWindow);
		END; {of if}
	END; {DoGrow}

	PROCEDURE ScrollWind (theCtl: controlhandle;
									thePart: integer);
		CONST
			active = 0;
			inactive = 255;

		VAR
			delta: integer;
			oldValue: integer;
			theWD: WDHandle;
			dPt: Point;
			cRect, pRect: Rect;

	BEGIN
	theWD := WDHandle(theCtl^^.contrlRfCon);
	CASE thePart OF
	inupbutton: 
		delta := +5;
	indownbutton: 
		delta := -5;
	inpageup: 
		WITH theWD^^.thisWindow^.portRect DO
			delta := bottom - 16 - top;
	inpagedown: 
		WITH theWD^^.thisWindow^.portRect DO
			delta := top - bottom + 16;
	OTHERWISE
		;
	END;
	IF thePart <> 0 THEN
		BEGIN
		SetPt(dPt, 0, 0);
		oldValue := GetCtlValue(theCtl);
		IF (oldValue - delta) > GetCtlMax(theCtl) THEN
			delta := (oldvalue - GetCtlMax(theCtl))
		ELSE IF (oldValue - delta) < GetCtlMin(theCtl) THEN
			delta := (oldValue - GetCtlMin(theCtl));
		IF theCtl = theWD^^.vScroll THEN
			dPt.v := delta
		ELSE
			dPt.h := delta;
		IF delta = 0 THEN
			EXIT(ScrollWind);
		SetCtlValue(theCtl, oldValue - delta);
		cRect := theWD^^.thisWindow^.portRect;
		cRect.right := cRect.right - 15;
		cRect.bottom := cRect.bottom - 15;
		ClipRect(cRect);
		ScrollRect(cRect, dPt.h, dPt.v, WindowPeek(theWD^^.thisWindow)^.updateRgn);
		OffsetWindow(theWD, dPt);
		IF delta < 0 THEN
			BEGIN
			AddPt(cRect.topLeft, dPt);
			cRect.topLeft := dPt;
			END
		ELSE
			BEGIN
			AddPt(cRect.botRight, dPt);
			cRect.botRight := dPt;
			END;
		ClipRect(cRect);
		DrawWindow(theWD);
		END;
	END;{ScrollWind}


	PROCEDURE DoContent (theWD: WDHandle;
									theEvent: EventRecord);
		VAR
			thePart, i, grNum: INTEGER;
			theCtl: ControlHandle;
			done, newTarget: BOOLEAN;
			r, r1, grRect: Rect;
			theModel: TModelHdl;
			grafArray: TGraphHdl;
			yVal, upper, lower: DOUBLE;
			oErr: OSErr;
			itemSel, menuID: INTEGER;
			dblClick, inverted: BOOLEAN;
			aPoint: Point;

		PROCEDURE DoWScroll;
			VAR
				oldVal, newVal: INTEGER;
				dPt: Point;
		BEGIN
		oldVal := GetCtlValue(theCtl);
		SetPt(dPt, 0, 0);
		IF thePart = inThumb THEN
			BEGIN
			thePart := TrackControl(theCtl, theEvent.where, NIL);
			IF thePart <> 0 THEN
				BEGIN
				newVal := GetCtlValue(theCtl);
				IF theCtl = theWD^^.hScroll THEN
					SetPt(dPt, oldVal - newVal, 0)
				ELSE
					SetPt(dPt, 0, oldval - newVal);
				OffsetWindow(theWD, dPt);
				DrawWindow(theWD);
				END;
			END
		ELSE
			thePart := trackControl(theCtl, theEvent.where, @ScrollWind);
		END; {DoWScroll}


	BEGIN
	done := FALSE;
	aPoint := theEvent.where;
	GlobalToLocal(aPoint);
	CASE theWD^^.windType OF
	modelWind: 
		BEGIN
		IF PtInRect(aPoint, theWD^^.teHdl^^.viewRect) THEN
			BEGIN
			done := TRUE;
			TEClick(aPoint, (BITAND(theEvent.modifiers, shiftKey) <> 0), theWD^^.teHdl)
			END
		ELSE IF DoListEvent(theEvent, FALSE, TRUE, dblClick, theWD^^.factList) THEN
			BEGIN
			FactListClick(theWD, dblClick);
			done := TRUE;
			UpdateMenus(theWD);
			END
		ELSE IF DoListEvent(theEvent, FALSE, FALSE, dblClick, theWD^^.linkList) THEN
			BEGIN
			LinkListClick(theWD, dblClick);
			done := TRUE;
			END
		ELSE IF PtInRect(aPoint, theWD^^.showValPopM.mRect) THEN
			BEGIN
			itemSel := DoWPopUp(theWD^^.showValPopM, menuID);
			IF itemSel <> 0 THEN
				BEGIN
				theWD^^.showValPopM.lastSel := itemSel;
				CASE itemSel OF
				selfImpCmd: 
					TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal := showImpacts;
				srchValCmd: 
					TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal := showAltVal;
				startValCmd: 
					TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal := showStart;
				actTargCmd: 
					TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal := showActTarg;
				allTargCmd: 
					TLDataHdl(theWD^^.factList^^.userHandle)^^.whichVal := showAllTarg;
				END;
				LUpdate(theWD^^.thisWindow^.visRgn, theWD^^.factList);
				FactListClick(theWD, FALSE);
				END;
			InvalRect(theWD^^.showValPopM.mRect);
			done := TRUE;
			END
		ELSE IF theWD^^.inNameTE THEN
			BEGIN
			IF PtInRect(aPoint, theWD^^.te2Rect) THEN
				BEGIN
				done := TRUE;
				SwapTEdit(theWD);
				END;
			END
		ELSE IF PtInRect(aPoint, theWD^^.te1Rect) THEN
			BEGIN
			done := TRUE;
			SwapTEdit(theWD);
			END;
		IF NOT done THEN
			BEGIN
			thePart := 0;
			thePart := FindControl(aPoint, theWD^^.thisWindow, theCtl);
			IF (thePart > 0) AND ((theCtl = theWD^^.vScroll) OR (theCtl = theWD^^.hScroll)) THEN
				DoWScroll
			END;
		END;
	graphBrowse: 
		BEGIN
		IF LClick(aPoint, theEvent.modifiers, theWD^^.gbList) THEN
			GBrowseSelect(theWD);
		UpdateMenus(theWD);
		END;
	graphWind: 
		BEGIN
		thePart := 0;
		thePart := FindControl(aPoint, theWD^^.thisWindow, theCtl);
		IF (thePart > 0) AND ((theCtl = theWD^^.vScroll) OR (theCtl = theWD^^.hScroll)) THEN
			DoWScroll
		ELSE IF thePart = 0 THEN
			BEGIN
			theModel := theWD^^.owner^^.theModel;
			grafArray := theModel^^.graphArray;
			grNum := theWD^^.grNum;
			IF grafArray <> NIL THEN
				IF grafArray^^[grNum].grID = theWD^^.grID THEN
					BEGIN
					r := grafArray^^[grNum].grRect;
					r.left := r.right - 15;
					IF PtInRect(aPoint, r) THEN
						BEGIN
						WITH grafArray^^[grNum] DO
							BEGIN
							WITH targPt DO
								SetRect(r1, h - 4, v - 4, h + 4, v + 4);
							IF PtInRect(aPoint, r1) THEN
								BEGIN
								newTarget := FALSE;
								aPoint.v := targPt.v;
								END
							ELSE
								newTarget := TRUE;
							WITH aPoint DO
								yVal := (v - org.v) / yScale + yMin;
							END;
						IF FactIDIdx(theModel, grafArray^^[grNum].factID, i) THEN
							BEGIN
							theModel^^.facts^^[i].targetVal := yVal;
							SetFactFlag(theModel^^.facts^^[i].flags, targeted, newTarget);
							NewFactTarget(theModel, i);
							theWD^^.owner^^.windDirty := TRUE;
							END;
						END;
					END;
			END;
		END;
	waitWind: 
		SysBeep(5);
	popnWind: 
		BEGIN
		IF PtInRect(aPoint, theWD^^.iconRect) THEN
			BEGIN
			IF TrackRect(theWD^^.iconRect) THEN
				BEGIN
				InvalRect(theWD^^.iconRect);
				ShowPopnGraph(theWD, (NOT theWD^^.showGraph));
				END;
			END
		ELSE IF PtInRect(aPoint, theWD^^.pGraphInfo.grRect) THEN
			BEGIN
			r1 := theWD^^.pGraphInfo.grRect;
			r1.bottom := r1.bottom + 1;
			DragGrayRect(r1, aPoint, r);
			IF NOT EmptyRect(r) THEN
				ZoomPopnGraph(theWD, r);
			END
		ELSE IF PtInRect(aPoint, theWD^^.rescaleRect) THEN
			BEGIN
			WITH theWD^^.rescaleRect DO
				BEGIN
				aPoint.v := aPoint.v - top;
				aPoint.h := aPoint.h - left;
				END;
			r := theWD^^.rescaleRect;
			WITH r DO
				BEGIN
				IF aPoint.v < 11 THEN
					BEGIN
					bottom := top + 11;
					thePart := 1;
					END
				ELSE IF aPoint.h < 23 THEN
					BEGIN
					top := top + 10;
					right := left + 23;
					thePart := 2;
					END
				ELSE
					BEGIN
					top := top + 10;
					left := left + 22;
					thePart := 3;
					END
				END;
			InsetRect(r, 1, 1);
			IF TrackRect(r) THEN
				RescalePopnGraph(theWD, thePart);
			END;
		END;
	END;
	END; {DoContent}


	PROCEDURE DoWDKey (theWD: WDHandle;
									theEvent: EventRecord);
		VAR
			ch: CHAR;
			i, teLength: INTEGER;
			str: Str255;
			dblClick: BOOLEAN;
	BEGIN
	ch := CHR(BitAnd(theEvent.message, charCodeMask));
	IF theWD <> NIL THEN
		BEGIN
		IF theWD^^.windType = modelWind THEN
			BEGIN
			IF DoListEvent(theEvent, FALSE, TRUE, dblClick, theWD^^.factList) THEN
				FactListClick(theWD, dblClick)
			ELSE
				BEGIN
				IF ch = CHR($09) THEN
					SwapTEdit(theWD)
				ELSE IF ch = CHR($03) THEN
					FactListClick(theWD, TRUE)	{Set links}
				ELSE IF ch = CHR($0D) THEN
					BEGIN
					FactListClick(theWD, FALSE);
					FactorAttributes(theWD);
					END
				ELSE
					BEGIN
					IF ch = CHR($08) THEN
						TEKey(ch, theWD^^.teHdl)
					ELSE IF theWD^^.inNameTE THEN
						BEGIN
						IF theWD^^.teHdl^^.teLength >= cFactNameLen THEN
							SysBeep(5)
						ELSE
							TEKey(ch, theWD^^.teHdl);
						END
					ELSE IF NOT (ch IN ['0'..'9', '.', '-']) THEN
						SysBeep(5)
					ELSE
						BEGIN
						IF CheckTEKey(theWD^^.teHdl, ch) THEN
							TEKey(ch, theWD^^.teHdl)
						ELSE
							SysBeep(5);
						END;
					EnterText(theWD);
					theWD^^.windDirty := TRUE;
					END;
				END;
			END;
		END
	ELSE
		SysBeep(5);
	END; {DoWDKey}


	PROCEDURE IdleTE (theWD: WDHandle);
	BEGIN
	IF theWD <> NIL THEN
		IF theWD^^.windType = modelWind THEN
			TEIdle(theWD^^.teHdl);
	END; {IdleTE}


{WARNING! This declaration occurs in Simulation.p and Genetic.p as an EXTERNAL}
	PROCEDURE RunGenie (quiet: BOOLEAN);
		VAR
			theEvent: EventRecord;
			theWind: WindowPtr;
			theWD: WDHandle;
			fw: INTEGER;
			mSel: LONGINT;
			zPt: Point;

		PROCEDURE DoZoom (whichZoom: INTEGER);
			VAR
				width, height: INTEGER;
		BEGIN
		IF TrackBox(theWind, theEvent.where, whichZoom) THEN
			BEGIN
			SetPort(theWind);
			EraseRect(theWind^.portRect);
			ZoomWindow(theWind, whichZoom, TRUE);
			theWD := OwnerWindowData(theWind);
			CASE theWD^^.windType OF
			modelWind, graphWind: 
				BEGIN
				WITH theWD^^.offsetPt DO
					SetPt(zPt, -h, -v);
				OffsetWindow(theWD, zPt);
				MoveScrollBars(theWD);
				AdjustScrollBar(theWD);
				AdjustLists(theWD);
				END;
			popnWind: 
				BEGIN
				MoveScrollBars(theWD);
				AdjustScrollBar(theWD);
				END;
			graphBrowse: 
				BEGIN
				WITH theWD^^.thisWindow^.portRect DO
					BEGIN
					width := right - left - 15;
					height := bottom - top - 15;
					END;
				LSize(width, height, theWD^^.gbList);
				END;
			END;
			InvalRect(theWind^.portRect);
			END;
		END; {DoZoom}


	BEGIN
	lastUpdate := TickCount;
	SystemTask;
	IF NOT quiet THEN
		SetCursorType;
	IF NOT quiet THEN
		IdleTE(OwnerWindowData(FrontWindow));

	WHILE GetNextEvent(everyEvent, theEvent) DO
		BEGIN
		fw := FindWindow(theEvent.where, theWind);
		CASE theEvent.what OF
		mouseDown: 
			BEGIN
			IF quiet AND NOT (fw IN [inSysWindow, inMenuBar, inContent]) THEN
				BEGIN
				SysBeep(5);
				fw := 999;
				END;
			CASE fw OF
			inSysWindow: 
				SystemClick(theEvent, theWind);
			inMenuBar: 
				BEGIN
				mSel := MenuSelect(theEvent.where);
				IF NOT quiet THEN
					DoCommand(mSel);
				END;
			inDrag: 
				DragWindow(theWind, theEvent.where, dragRect);
			inGoAway: 
				IF TrackGoAway(theWind, theEvent.where) THEN
					BEGIN
					IF CloseWindow(theWind, TRUE) THEN
						;
					UpdateMenus(OwnerWindowData(FrontWindow));
					END;
			inGrow: 
				IF theWind = FrontWindow THEN
					BEGIN
					theWD := OwnerWindowData(theWind);
					IF theWD = NIL THEN
						DebugStr('NIL WD in inGrow')
					ELSE
						DoGrow(theWD, theEvent);
					END
				ELSE
					BEGIN
					SelectWindow(theWind);
					UpdateMenus(OwnerWindowData(FrontWindow));
					END;
			inContent: { body of application window: }
				IF (theWind <> FrontWindow) AND NOT quiet THEN
					BEGIN
					SelectWindow(theWind);
					UpdateMenus(OwnerWindowData(FrontWindow));
					END
				ELSE
					BEGIN
					theWD := OwnerWindowData(theWind);
					IF quiet AND (theWD^^.windType <> popnWind) THEN
						SysBeep(5)
					ELSE
						BEGIN
						SetPort(theWind);
						IF theWD = NIL THEN
							DebugStr('NIL WD inContent')
						ELSE
							DoContent(theWD, theEvent);
						END;
					END;
			inZoomIn: 
				DoZoom(inZoomIn);
			inZoomOut: 
				DoZoom(inZoomOut);
			OTHERWISE
				;
			END; {of Case}
			END; {of mouseDown}

		activateEvt: 
			BEGIN
			theWD := OwnerWindowData(WindowPtr(theEvent.message));
			IF theWD = NIL THEN
				BEGIN
				SetPort(WindowPtr(theEvent.message));
				debugStr('Not my activate');
				END
			ELSE
				BEGIN
				DoActivate(theWD, theEvent)
				END;
			END; {of activateEvt}

		updateEvt: {window appearance needs updating}
			BEGIN
			theWD := OwnerWindowData(WindowPtr(theEvent.message));
			IF theWD = NIL THEN
				BEGIN
				debugStr('Not my update');
				END
			ELSE
				BEGIN
				DoUpdate(WindowPtr(theEvent.message));
				END;
			END;
		keyDown, autoKey: {key pressed once or held down to repeat}
			BEGIN
			theWD := OwnerWindowData(FrontWindow);
			IF (BITAND(theEvent.modifiers, cmdKey) <> 0) THEN
				BEGIN
				IF CHR(BITAND(theEvent.message, charCodeMask)) = '.' THEN
					userBreak := TRUE
				ELSE IF NOT quiet THEN
					DoCommand(MenuKey(CHR(BITAND(theEvent.message, $0FF))));
				END
			ELSE IF NOT quiet THEN
				DoWDKey(theWD, theEvent);
			IF NOT quiet THEN
				UpdateMenus(OwnerWindowData(FrontWindow));
			END;
		OTHERWISE
			;
		END; {of CASE}
		END; {WHILE}
	END; {RunGenie}


END. {GDriver}