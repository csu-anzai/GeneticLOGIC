UNIT GDialogs;
INTERFACE

	USES
		SANE, GrafTypes, GrafPak, DialogUtils, GFiles, GCommonDec, GGlobals, DialogUtils;

{***** Utility Procedures *****}
	PROCEDURE CheckMemErr;

	PROCEDURE SetFactorCell (factNum, linkTo: INTEGER;
									theCell: Cell;
									lHandle: ListHandle);
	FUNCTION GetFactorCell (VAR growthVal: DOUBLE;
									theCell: Cell;
									lHandle: ListHandle): TFactRec;
	PROCEDURE SetIFactorCell (factNum, linkTo: INTEGER;
									indent: INTEGER;
									theCell: Cell;
									lHandle: ListHandle);
	FUNCTION GetIFactorCell (VAR growthVal: DOUBLE;
									VAR indent: INTEGER;
									theCell: Cell;
									lHandle: ListHandle): TFactRec;
	PROCEDURE SetFactFlag (VAR flags: INTEGER;
									whichFlag: TFactFlags;
									val: BOOLEAN);
	FUNCTION TstFactFlag (flags: INTEGER;
									whichFlag: TFactFlags): BOOLEAN;
	FUNCTION FactIDIdx (theModel: TModelHdl;
									fID: INTEGER;
									VAR factNo: INTEGER): BOOLEAN;
	FUNCTION DoWPopUp (popUpInfo: TPopMenuInfo;
									VAR menuID: INTEGER): INTEGER;
	FUNCTION UniqueWindName (windType: TWindType;
									title: Str255): Str255;
	PROCEDURE ChangeWindMenuName (oldname, newName: Str255);
	PROCEDURE BeachBallCursor;
	FUNCTION WDHeight (theWD: WDHandle): INTEGER;
	FUNCTION WDWidth (theWD: WDHandle): INTEGER;
	PROCEDURE DragGrayRect (boundsRect: Rect;
									stPt: Point;
									VAR r: Rect);
	FUNCTION TrackRect (r: Rect): BOOLEAN;

{***** Real Dialogs *****}
	FUNCTION SetFactLinks (theModel: TModelHdl;
									theFact: INTEGER): BOOLEAN;
	FUNCTION ModelPrefsDlog (theModel: TModelHdl): BOOLEAN;
	FUNCTION SetVBounds (VAR lower, upper: DOUBLE;
									min, max: DOUBLE;
									int: BOOLEAN): BOOLEAN;
	FUNCTION SetFactAttributes (theModel: TModelHdl;
									theFact: INTEGER;
									VAR newTarg: BOOLEAN): BOOLEAN;
	FUNCTION GetNameDlog (prompt: Str255;
									VAR name: Str255): BOOLEAN;
	FUNCTION SavePopnAsDlog (VAR sel: INTEGER;
									statsEnable: BOOLEAN): BOOLEAN;

IMPLEMENTATION
{$S DlogSeg}
	CONST
		gTypeMID = 1;
		addCmd = 1;
		multCmd = 2;
		expCmd = 3;


	VAR
		lastMPoint: Point;

	PROCEDURE CheckMemErr;
		VAR
			mErr: OSErr;
			str: Str255;
	BEGIN
	mErr := MemError;
	IF debugOn AND (mErr <> noErr) THEN
		BEGIN
		NumToString(mErr, str);
		OneBtnAlert(Concat('Memory Error : ', str, '.'));
		END;
	gotMemErr := gotMemErr OR (mErr <> noErr);
	END; {CheckMemErr}

	PROCEDURE SetFactFlag (VAR flags: INTEGER;
									whichFlag: TFactFlags;
									val: BOOLEAN);
	BEGIN
	IF val THEN
		BITSET(@flags, ORD(whichFlag))
	ELSE
		BITCLR(@flags, ORD(whichFlag));
	END; {SetFactFlag}

	FUNCTION TstFactFlag (flags: INTEGER;
									whichFlag: TFactFlags): BOOLEAN;
	BEGIN
	TstFactFlag := BITTST(@flags, ORD(whichFlag));
	END; {TstFactFlag}


	PROCEDURE SetFactorCell (factNum, linkTo: INTEGER;
									theCell: Cell;
									lHandle: ListHandle);
		VAR
			cellRec: TFactCellRec;
			modelHdl: TModelHdl;

	BEGIN
	modelHdl := TModelHdl(lHandle^^.refCon);
	cellRec.factID := modelHdl^^.facts^^[factNum].factID;
	cellRec.linkID := modelHdl^^.facts^^[linkTo].factID;
	cellRec.indent := 0;
	LSetCell(@cellRec, SizeOf(TFactCellRec), theCell, lHandle);
	END; {SetFactorCell}

	FUNCTION GetFactorCell (VAR growthVal: DOUBLE;
									theCell: Cell;
									lHandle: ListHandle): TFactRec;
		VAR
			fRec: TFactRec;
			factNum, linkNum: INTEGER;
			cellRec: TFactCellRec;
			dLen: INTEGER;
			theModel: TModelHdl;

	BEGIN
	theModel := TModelHdl(lHandle^^.refCon);
	dLen := 999;
	LGetCell(@cellRec, dLen, theCell, lHandle);
	IF FactIDIdx(theModel, cellRec.factID, factNum) THEN
		;
	IF FactIDIdx(theModel, cellRec.linkID, linkNum) THEN
		;
	fRec := theModel^^.facts^^[factNum];
	IF linkNum <> 0 THEN
		growthVal := theModel^^.crGrowths^^[linkNum, factNum]
	ELSE
		growthVal := theModel^^.crGrowths^^[factNum, factNum];
	GetFactorCell := fRec;
	END; {GetFactorCell}

	PROCEDURE SetIFactorCell (factNum, linkTo: INTEGER;
									indent: INTEGER;
									theCell: Cell;
									lHandle: ListHandle);
		VAR
			cellRec: TFactCellRec;
			modelHdl: TModelHdl;

	BEGIN
	modelHdl := TModelHdl(lHandle^^.refCon);
	cellRec.factID := modelHdl^^.facts^^[factNum].factID;
	cellRec.linkID := modelHdl^^.facts^^[linkTo].factID;
	cellRec.indent := indent;
	LSetCell(@cellRec, SizeOf(TFactCellRec), theCell, lHandle);
	END; {SetIFactorCell}

	FUNCTION GetIFactorCell (VAR growthVal: DOUBLE;
									VAR indent: INTEGER;
									theCell: Cell;
									lHandle: ListHandle): TFactRec;
		VAR
			fRec: TFactRec;
			cellRec: TFactCellRec;
			dLen: INTEGER;
			theModel: TModelHdl;
			factNum, linkNum: INTEGER;

	BEGIN
	theModel := TModelHdl(lHandle^^.refCon);
	dLen := SizeOf(TFactCellRec);
	LGetCell(@cellRec, dLen, theCell, lHandle);
	IF FactIDIdx(theModel, cellRec.factID, factNum) THEN
		;
	IF FactIDIdx(theModel, cellRec.linkID, linkNum) THEN
		;
	fRec := theModel^^.facts^^[factNum];
	IF cellRec.linkID <> 0 THEN
		growthVal := theModel^^.crGrowths^^[linkNum, factNum]
	ELSE
		growthVal := theModel^^.crGrowths^^[factNum, factNum];
	indent := cellRec.indent;
	GetIFactorCell := fRec;
	END; {GetIFactorCell}

	FUNCTION FactIDIdx (theModel: TModelHdl;
									fID: INTEGER;
									VAR factNo: INTEGER): BOOLEAN;
		VAR
			i: INTEGER;
	BEGIN
	i := 1;
	WHILE (theModel^^.facts^^[i].factID <> fID) AND (i <= theModel^^.nFacts) DO
		i := i + 1;
	IF i <= theModel^^.nFacts THEN
		factNo := i
	ELSE
		factNo := 0;
	FactIDIdx := i <= theModel^^.nFacts;
	END; {factIDIdx}

	FUNCTION DoWPopUp (popUpInfo: TPopMenuInfo;
									VAR menuID: INTEGER): INTEGER;
		VAR
			mResult: LONGINT;
			selItem: INTEGER;
			tl: Point;

	BEGIN
	InsertMenu(popUpInfo.mHdl, -1);
	tl := popUpInfo.mRect.topLeft;
	LocalToGlobal(tl);
	InvertRect(popUpInfo.tRect);
	mResult := PopUpMenuSelect(popUpInfo.mHdl, tl.v, tl.h, popUpInfo.lastSel);
	InvertRect(popUpInfo.tRect);
	DeleteMenu(popUpInfo.mID);
	DoWPopUp := LoWord(mResult);
	menuID := HiWord(MResult);
	END; {DoWPopUp}

	FUNCTION UniqueWindName (windType: TWindType;
									title: Str255): Str255;
{If its a population window then it assumes the resource file is opened}
		VAR
			i: INTEGER;
			str, numStr: Str255;

		FUNCTION IsUnique (str: Str255): BOOLEAN;
			VAR
				twd: WDHandle;
				rHdl: Handle;

		BEGIN
		rHdl := NIL;
		twd := wdList;
		IF twd <> NIL THEN
			WHILE (twd <> NIL) AND NOT EqualString(str, twd^^.wName, FALSE, FALSE) DO
				twd := twd^^.next;
		IF (twd = NIL) THEN
			BEGIN
			rHdl := Get1NamedResource(popnWindResType, str);
			IF rHdl <> NIL THEN
				BEGIN
				ReleaseResource(rHdl);
				END;
			END;
		IsUnique := (twd = NIL) AND (rHdl = NIL);
		END; {IsUnique}

	BEGIN
	IF IsUnique(title) THEN
		UniqueWindName := title
	ELSE
		BEGIN
		str := title;
		i := Length(str);
		IF str[i] IN ['0'..'9'] THEN
			BEGIN
			REPEAT
				i := i - 1;
			UNTIL (i <= 1) OR NOT (str[i] IN ['0'..'9']);
			IF str[i] = '-' THEN
				str[0] := CHR(i - 1);
			END;
		i := 0;
		REPEAT
			i := i + 1;
			NumToString(i, numStr);
		UNTIL IsUnique(Concat(str, '-', numStr));
		UniqueWindName := Concat(str, '-', numStr)
		END;
	END; {UniqueWindName}

	PROCEDURE ChangeWindMenuName (oldname, newName: Str255);
		VAR
			nItems, i: INTEGER;
			str: Str255;
	BEGIN
	nItems := CountMItems(theMenus[windM]);
	i := 0;
	REPEAT
		i := i + 1;
		GetItem(theMenus[windM], i, str);
	UNTIL (i = nItems) OR (str = oldName);
	IF str = oldName THEN
		IF newName = '' THEN
			DelMenuItem(theMenus[windM], i)
		ELSE
			SetItem(theMenus[windM], i, newName);
	END; {ChangeWindMenuName}

	PROCEDURE BeachBallCursor;

	BEGIN
	bBallState := bBallState + 1;
	IF bBallState > 4 THEN
		bballState := 1;
	SetCursor(bBalls[bBallState]^^);
	END; {BeachBallCursor}


	FUNCTION WDHeight (theWD: WDHandle): INTEGER;
	BEGIN
	WITH theWD^^.thisWindow^.portRect DO
		WDHeight := bottom - 16 - top;
	END; {WDHeight}

	FUNCTION WDWidth (theWD: WDHandle): INTEGER;
	BEGIN
	WITH theWD^^.thisWindow^.portRect DO
		WDWidth := right - 16 - left;
	END; {WDWidth}

	PROCEDURE DragGrayRect (boundsRect: Rect;
									stPt: Point;
									VAR r: Rect);
		VAR
			fRect: Rect;
			lastPt, aPt, mPt: Point;
			pnState: PenState;
	BEGIN
	GetPenState(pnState);
	PenNormal;
	PenMode(patXOR);
	PenPat(gray);
	WITH stPt DO
		SetRect(fRect, h, v, h, v);
	SetPt(lastPt, 0, 0);
	SetCursor(arrow);
	FrameRect(fRect);
	WHILE StillDown DO
		BEGIN
		GetMouse(mPt);
		aPt := POINT(PinRect(boundsRect, mPt));
		IF NOT EqualPt(lastPt, aPt) THEN
			BEGIN
			lastPt := aPt;
			FrameRect(fRect);
			Pt2Rect(stPt, aPt, fRect);
			FrameRect(fRect);
			END;
		END;
	FrameRect(fRect);
	r := fRect;
	SetPenState(pnState);
	END; {DragGrayRect}

	FUNCTION TrackRect (r: Rect): BOOLEAN;
		VAR
			inverted: BOOLEAN;
			aPoint: Point;
	BEGIN
	inverted := FALSE;
	WHILE StillDown DO
		BEGIN
		GetMouse(aPoint);
		IF PtInRect(aPoint, r) THEN
			BEGIN
			IF NOT inverted THEN
				InvertRect(r);
			inverted := TRUE;
			END
		ELSE IF inverted THEN
			BEGIN
			InvertRect(r);
			inverted := FALSE;
			END;
		END;
	IF inverted THEN
		BEGIN
		InvertRect(r);
		inverted := FALSE;
		END;
	TrackRect := PtInRect(aPoint, r);
	END; {TrackRect}

{***** Dialog Stuff *****}




	FUNCTION FactIndex (theModel: TModelHdl;
									fName: TFactName): INTEGER;
		VAR
			fIdx: INTEGER;
	BEGIN
	fIdx := 0;
	REPEAT
		fIdx := fIdx + 1;
	UNTIL EqualString(fName, theModel^^.facts^^[fIdx].name, TRUE, FALSE) OR (fIdx = theModel^^.nFacts);
	IF NOT EqualString(fName, theModel^^.facts^^[fIdx].name, TRUE, FALSE) THEN
		FactIndex := 0
	ELSE
		FactIndex := fIdx;
	END; {FactIndex}

	FUNCTION SetLinksFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;
		CONST
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			addBtn = 4;
			removeBtn = 5;
			linkList = 6;
			availList = 7;
			lwEdt = 8;
			addFact = 100;
			removeFact = 101;

		VAR
			listVars: ListVarsHand;
			tmpBool: BOOLEAN;
			dblClick: BOOLEAN;
			ch: CHAR;

	BEGIN
	listVars := ListVarsHand(GetWRefCon(theDialog));
	tmpBool := FALSE;
	IF DoListEvent(theEvent, FALSE, FALSE, dblClick, listVars^^.list1) THEN
		BEGIN
		IF dblClick THEN
			itemHit := addFact;
		tmpBool := TRUE;
		END
	ELSE IF DoListEvent(theEvent, FALSE, FALSE, dblClick, listVars^^.list2) THEN
		BEGIN
		IF dblClick THEN
			itemHit := removeFact;
		tmpBool := TRUE;
		END
	ELSE
		WITH theEvent DO
			BEGIN
			IF what IN [keyDown, autoKey] THEN
				BEGIN
				ch := CHR(BITAND(message, CharCodeMask));
				IF ch IN [CHR(3), CHR($0D)] THEN
					BEGIN
					itemHit := okBtn;
					tmpBool := TRUE;
					END
				ELSE IF ch = CHR($1B) THEN
					BEGIN
					itemHit := cancelBtn;
					tmpBool := TRUE;
					END
				ELSE IF ch = CHR($08) THEN
					BEGIN
					END
				ELSE IF (ch IN ['0'..'9', '-', '.']) AND (listVars^^.mult2) THEN
					BEGIN
					IF CheckTEKey(DialogPeek(theDialog)^.textH, ch) THEN
						BEGIN
						itemHit := lwEdt;
						TEKey(ch, DialogPeek(theDialog)^.textH);
						END
					ELSE
						SysBeep(5);
					tmpBool := TRUE;
					END
				ELSE
					BEGIN
					SysBeep(5);
					itemHit := 0;
					tmpBool := TRUE;
					END;
				END;
			END;
	IF itemHit = addBtn THEN
		BEGIN
		itemHit := addFact;
		tmpBool := TRUE;
		END
	ELSE IF itemHit = removeBtn THEN
		BEGIN
		itemHit := removeFact;
		tmpBool := TRUE;
		END;
	SetLinksFilter := tmpBool;
	END; {SetLinksFilter}


	FUNCTION SetFactLinks (theModel: TModelHdl;
									theFact: INTEGER): BOOLEAN;

		CONST
			SetLinkDlogID = 1001;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			addBtn = 4;
			removeBtn = 5;
			linkList = 6;
			availList = 7;
			lwEdt = 8;
			delayRad = 13;
			immedRad = 14;

			addFact = 100;
			removeFact = 101;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			i, fIdx: INTEGER;
			itemHdl: HANDLE;
			itemBox: Rect;
			dummy, curRad: INTEGER;
			ctlHdl: ControlHandle;
			listVars: ListVarsHand;
			theCell: Cell;
			str: Str255;
			fName: TFactName;
			fRec: TFactRec;
			grwth: DOUBLE;


		PROCEDURE BuildLists;
			VAR
				theCell: Cell;
				cSize: Point;
				dBnds: Rect;
				i: INTEGER;
				str, s: Str255;
		BEGIN
{Avail List}
		GetDItem(theDialog, availList, dummy, itemHdl, itemBox);
		listVars^^.item1 := availList;
		listVars^^.rect1 := itemBox;
		WITH listVars^^.rect1 DO
			right := right - 15;
		SetPt(cSize, 0, 0);
		SetRect(dBnds, 0, 0, 1, 0);
		listVars^^.list1 := LNew(listVars^^.rect1, dBnds, cSize, cGListProcID, theDialog, FALSE, FALSE, FALSE, TRUE);
		WITH TLDataHdl(listVars^^.list1^^.userHandle)^^ DO
			BEGIN
			lPics := listPics;
			whichVal := none;
			indentFlags := 0;
			showFlags := 0;
			SetFactFlag(showFlags, canMod, TRUE);
			SetFactFlag(showFlags, gotLinks, TRUE);
			SetFactFlag(showFlags, crossDelay, TRUE);
			END;
		listVars^^.list1^^.selFlags := lOnlyOne;
		listVars^^.list1^^.refcon := LONGINT(theModel);
		listVars^^.rect1 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, availList, dummy, Handle(@UpdateListItem), itemBox);

{Link List}
		GetDItem(theDialog, linkList, dummy, itemHdl, itemBox);
		listVars^^.item2 := linkList;
		listVars^^.rect2 := itemBox;
		listVars^^.mult2 := FALSE;
		WITH listVars^^.rect2 DO
			BEGIN
			right := right - 15;
			SetPt(cSize, (right - left), 0);
			END;
		SetRect(dBnds, 0, 0, 1, 0);
		listVars^^.list2 := LNew(listVars^^.rect2, dBnds, cSize, cGListProcID, theDialog, FALSE, FALSE, FALSE, TRUE);
		WITH TLDataHdl(listVars^^.list2^^.userHandle)^^ DO
			BEGIN
			lPics := listPics;
			whichVal := showImpacts;
			indentFlags := 0;
			showFlags := 0;
			SetFactFlag(showFlags, gotLinks, TRUE);
			SetFactFlag(showFlags, crossDelay, TRUE);
			END;
		listVars^^.list2^^.selFlags := lOnlyOne;
		listVars^^.list2^^.refcon := LONGINT(theModel);
		listVars^^.rect2 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, linkList, dummy, Handle(@UpdateListItem), itemBox);


		SetPt(theCell, 0, 0);
		FOR i := 1 TO theModel^^.nFacts DO
			IF (i <> theFact) THEN
				IF (theModel^^.crGrowths^^[theFact, i] = 0) THEN
					BEGIN
					theCell.v := LAddRow(1, 999, listVars^^.list1);
					SetFactorCell(i, 0, theCell, listVars^^.list1);
					END
				ELSE
					BEGIN
					theCell.v := LAddRow(1, 999, listVars^^.list2);
					SetFactorCell(i, theFact, theCell, listVars^^.list2);
					END;
		LDoDraw(TRUE, listVars^^.list1);
		LUpdate(theDialog^.visRgn, listVars^^.list1);
		LDoDraw(TRUE, listVars^^.list2);
		LUpdate(theDialog^.visRgn, listVars^^.list2);
		END; {BuildLists}

		PROCEDURE AddFactor;
			VAR
				fIdx: INTEGER;
				theCell: Cell;
				str, s: Str255;
				fRec: TFactRec;
				gVal: DOUBLE;

		BEGIN
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, theCell, listVars^^.list1) THEN
			BEGIN
			fRec := GetFactorCell(gVal, theCell, listVars^^.list1);
			LDelRow(1, theCell.v, listVars^^.list1);

			fIdx := FactIndex(theModel, fRec.name);
			SetEdtText(theDialog, lwEdt, '0.00');
			SelIText(theDialog, lwEdt, 0, 32000);
			SetPt(theCell, 0, 0);
			IF LGetSelect(TRUE, theCell, listVars^^.list2) THEN
				LSetSelect(FALSE, theCell, listVars^^.list2);
			theCell.v := LAddRow(1, 999, listVars^^.list2);
			SetFactorCell(fIdx, thefact, thecell, listVars^^.list2);
			LSetSelect(TRUE, theCell, listVars^^.list2);
			CheckListScroll(theCell, listVars^^.list2);
			END;
		END; {AddFactor}

		PROCEDURE RemoveFactor;
			VAR
				fIdx: INTEGER;
				theCell: Cell;
				gVal: DOUBLE;
				fRec: TFactRec;
		BEGIN
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, theCell, listVars^^.list2) THEN
			BEGIN
			fRec := GetFactorCell(gVal, theCell, listVars^^.list2);
			LDelRow(1, theCell.v, listVars^^.list2);
			fIdx := FactIndex(theModel, fRec.name);
			theModel^^.crGrowths^^[thefact, fIdx] := 0.0;
			theCell.v := LAddRow(1, 999, listVars^^.list1);
			SetFactorCell(fIdx, 0, theCell, listVars^^.list1);
			CheckListScroll(theCell, listVars^^.list1);
			END;
		END; {RemoveFactor}

		PROCEDURE GotKey;
			VAR
				str: Str255;
				val: DOUBLE;
				fIdx, dLen: INTEGER;
		BEGIN
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, theCell, listVars^^.list2) THEN
			BEGIN
			fRec := GetFactorCell(val, theCell, listVars^^.list2);
			fIdx := FactIndex(theModel, fRec.name);
			str := GetEdtText(theDialog, lwEdt);
			val := Str2Num(str);
			theModel^^.crGrowths^^[thefact, fIdx] := val;
			SetFactorCell(fIdx, theFact, theCell, listVars^^.list2);
			END;
		END; {GotKey}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(SetLinkDlogID, @stor, windowPtr(-1));
	listVars := ListVarsHand(NewHandle(SizeOf(ListVarsRec)));
	SetWRefCon(theDialog, LONGINT(listVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	BuildLists;
	SetDCtlHilite(theDialog, addBtn, FALSE);
	SetDCtlHilite(theDialog, removeBtn, FALSE);
	TEDeactivate(DialogPeek(theDialog)^.textH);

	ParamText(theModel^^.facts^^[thefact].name, '', '', '');
	IF TstFactFlag(theModel^^.facts^^[theFact].flags, crossDelay) THEN
		curRad := delayRad
	ELSE
		curRad := immedRad;
	SetDCtlValue(theDialog, curRad, 1);

	ShowWindow(theDialog);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@SetLinksFilter, itemHit);
		CASE itemHit OF
		addFact: 
			AddFactor;
		removeFact: 
			RemoveFactor;
		lwEdt: 
			GotKey;
		delayRad, immedRad: 
			IF itemHit <> curRad THEN
				BEGIN
				SetDCtlvalue(theDialog, curRad, 0);
				curRad := itemHit;
				SetDCtlValue(theDialog, curRad, 1);
				END;
		END;
		SetPt(theCell, 0, 0);
		SetDCtlHilite(theDialog, addBtn, LGetSelect(TRUE, theCell, listVars^^.list1));
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, theCell, listVars^^.list2) THEN
			BEGIN
			SetDCtlHilite(theDialog, removeBtn, TRUE);
			listVars^^.mult2 := TRUE;
			TEActivate(DialogPeek(theDialog)^.textH);
			END
		ELSE
			BEGIN
			SetDCtlHilite(theDialog, removeBtn, FALSE);
			listVars^^.mult2 := FALSE;
			TEDeactivate(DialogPeek(theDialog)^.textH);
			END;
	UNTIL itemHit IN [okBtn, cancelBtn];

	IF itemHit = OK THEN
		BEGIN
		SetFactLinks := TRUE;
		HLock(Handle(theModel));
		WITH theModel^^ DO
			BEGIN
			SetFactFlag(facts^^[thefact].flags, crossDelay, (GetDCtlValue(theDialog, delayRad) = 1));
			FOR i := 1 TO nFacts DO	{Clear existing links}
				IF i <> theFact THEN
					;
{crGrowths^^[theFact, i] := 0.0}
			SetFactFlag(facts^^[thefact].flags, gotLinks, FALSE);
			SetPt(theCell, 0, 0);
			IF listVars^^.list2^^.dataBounds.bottom > 0 THEN
				REPEAT
					fRec := GetFactorCell(grwth, theCell, listVars^^.list2);
					fIdx := FactIndex(theModel, fRec.name);
					IF FALSE THEN {fIdx <> 0}
						theModel^^.crGrowths^^[theFact, fIdx] := grwth;
					SetFactFlag(facts^^[thefact].flags, gotLinks, TRUE);
				UNTIL NOT LNextCell(FALSE, TRUE, theCell, listVars^^.list2);
			END;
		HUnlock(Handle(theModel));
		SetFactLinks := TRUE;
		END
	ELSE
		SetFactLinks := FALSE;
	LDispose(listVars^^.list1);
	LDispose(listVars^^.list2);
	DisposHandle(Handle(listVars));
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {SetFactLinks}


	FUNCTION ModelPrefsFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;
		CONST
			okBtn = 1;
			cancelBtn = 2;
			itnEdt = 9;
			precEdt = 11;
			dfltStEdt = 13;

		VAR
			tmpBool: BOOLEAN;
			ch: CHAR;
			theEdt: INTEGER;


	BEGIN
	tmpBool := FALSE;
	theEdt := DialogPeek(theDialog)^.editField + 1;
	WITH theEvent DO
		BEGIN
		IF what IN [keyDown, autoKey] THEN
			BEGIN
			ch := CHR(BITAND(message, CharCodeMask));
			IF ch IN [CHR(3), CHR($0D)] THEN
				BEGIN
				itemHit := okBtn;
				tmpBool := TRUE;
				END
			ELSE IF ch = CHR($1B) THEN
				BEGIN
				itemHit := cancelBtn;
				tmpBool := TRUE;
				END
			ELSE IF ch = CHR($09) THEN
				tmpBool := FALSE
			ELSE IF (theEdt IN [itnEdt, precEdt]) AND NOT (ch IN ['0'..'9', CHR($08)]) THEN
				BEGIN
				itemHit := 0;
				tmpBool := TRUE;
				SysBeep(5);
				END
			ELSE IF (theEdt = dfltStEdt) AND NOT (ch IN ['0'..'9', '.', '-', CHR($09)]) THEN
				BEGIN
				itemHit := 0;
				tmpBool := TRUE;
				SysBeep(5);
				END
			END;
		END;
	ModelPrefsFilter := tmpBool;
	END; {ModelPrefsFilter}

	FUNCTION ModelPrefsDlog (theModel: TModelHdl): BOOLEAN;

		CONST
			cgPMID = 2;
			fImmedCmd = 1;
			fDelayCmd = 2;
			noneCmd = 3;

			mPrefsID = 1002;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			itnEdt = 6;
			precEdt = 8;
			dfltStEdt = 10;
			shockChk = 13;
			boundChk = 14;
			threshChk = 15;
			gTypeTitle = 12;
			gTypePMItem = 16;
			cgTitle = 9;
			cgPMItem = 17;

		VAR
			theDialog: DialogPtr;
			stor: DialogRecord;
			savePort: GrafPtr;
			dummy, itemHit: INTEGER;
			itemHdl: Handle;
			itemBox: Rect;
			str: Str255;
			curCGSel, curGTypeSel: INTEGER;
			ctlHdl: ControlHandle;
			n: LONGINT;
			popVars: TPopMHdl;
			fInfo: FontInfo;
			fHeight: INTEGER;

		FUNCTION CheckOk: BOOLEAN;
			VAR
				itemBad: INTEGER;
		BEGIN
		itemBad := 0;
		str := GetEdtText(theDialog, itnEdt);
		StringToNum(str, n);
		IF (n < 2) OR (n > cMaxItns) THEN
			BEGIN
			NumToString(cMaxItns, str);
			OneBtnAlert(Concat('Iterations must be between 2 and ', str, '.'));
			itemBad := itnEdt;
			END
		ELSE IF (theModel^^.lastResults <> NIL) AND (n <> theModel^^.iteratns) THEN
			BEGIN
			IF NOT TwoBtnAlert('Changing the iterations will clear the last simulation results.', 'OK', 'CANCEL') THEN
				BEGIN
				NumToString(theModel^^.iteratns, str);
				SetEdtText(theDialog, itnEdt, str);
				itemBad := itnEdt;
				END
			ELSE
				BEGIN {Clear the lastResults - no longer valid}
				DisposHandle(Handle(theModel^^.lastResults));
				theModel^^.lastResults := NIL;
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			str := GetEdtText(theDialog, precEdt);
			StringToNum(str, n);
			IF (n < 0) OR (n > 8) THEN
				BEGIN
				OneBtnAlert('Precision must be between 0 and 8.');
				itemBad := precEdt;
				END
			ELSE IF NOT CheckNum(GetEdtText(theDialog, dfltStEdt)) THEN
				BEGIN
				OneBtnAlert('The start value is not a valid number.');
				itemBad := dfltStEdt;
				END
			END;
		IF itemBad > 0 THEN
			SelIText(theDialog, itemBad, 0, 32000);
		CheckOk := itemBad = 0;
		END; {CheckOk}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(mPrefsID, @stor, windowPtr(-1));
	popVars := TPopMHdl(NewHandle(SizeOf(TPopMVars)));
	SetWRefCon(theDialog, LONGINT(popVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, UserItem, HANDLE(@OutlineButton), itemBox);

{Set up popups}
	MoveHHi(Handle(popVars));
	HLock(Handle(popVars));
	GetFontInfo(fInfo);
	WITH fInfo DO
		fHeight := ascent + descent + leading;
{Growth Type}
	GetDItem(theDialog, gTypePMItem, dummy, itemHdl, itemBox);
	WITH itemBox DO
		BEGIN
		bottom := top + fHeight;
		right := left + StringWidth('  Multiplicative ') + 4;
		END;
	SetDItem(theDialog, gTypePMItem, userItem, HANDLE(@UpdatePopMItem), itemBox);
	WITH popVars^^[1] DO
		BEGIN
		pItem := gTypePMItem;
		pRect := itemBox;
		pID := gTypeMID;
		pHdl := GetMenu(gTypeMID);
		titleItem := gTypeTitle;
		CASE theModel^^.dfltGType OF
		add: 
			curGTypeSel := addCmd;
		mult: 
			curGTypeSel := multCmd;
		expon: 
			curGTypeSel := expCmd;
		END;
		pSel := curGTypeSel;
		END;

{Cross growth processing}
	GetDItem(theDialog, cgPMItem, dummy, itemHdl, itemBox);
	WITH itemBox DO
		BEGIN
		bottom := top + fHeight;
		right := left + StringWidth('  Immediate ') + 4;
		END;
	SetDItem(theDialog, cgPMItem, userItem, HANDLE(@UpdatePopMItem), itemBox);
	WITH popVars^^[2] DO
		BEGIN
		pItem := cgPMItem;
		pRect := itemBox;
		pID := cgPMID;
		pHdl := GetMenu(cgPMID);
		titleItem := cgTitle;
		IF theModel^^.forceDelay THEN
			curCGSel := fDelayCmd
		ELSE IF theModel^^.forceImmed THEN
			curCGSel := fImmedCmd
		ELSE
			curCGSel := noneCmd;
		popVars^^[2].pSel := curCGSel;
		END;

	HUnlock(Handle(popVars));

	IF TstFactFlag(theModel^^.enableFlags, shocks) THEN
		SetDCtlValue(theDialog, shockChk, 1);
	IF TstFactFlag(theModel^^.enableFlags, bounded) THEN
		SetDCtlValue(theDialog, boundChk, 1);
	IF TstFactFlag(theModel^^.enableFlags, threshholds) THEN
		SetDCtlValue(theDialog, threshChk, 1);

	NumToString(theModel^^.iteratns, str);
	SetEdtText(theDialog, itnEdt, str);
	NumToString(theModel^^.numPrec, str);
	SetEdtText(theDialog, precEdt, str);
	SetEdtText(theDialog, dfltStEdt, NumToDecStr(theModel^^.numPrec, theModel^^.dfltStartVal));
	SelIText(theDialog, itnEdt, 0, 32000);

	ShowWindow(theDialog);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@ModelPrefsFilter, itemHit);
		CASE itemHit OF
		shockChk..threshChk: 
			BEGIN
			GetDItem(theDialog, itemHit, dummy, Handle(ctlHdl), itemBox);
			SetCtlValue(ctlHdl, ABS(GetCtlValue(ctlHdl) - 1));
			END;
		gTypePMItem: 
			BEGIN
			IF DoDPopUpMenu(theDialog, popVars, gTypePMItem, curGTypeSel) THEN
				popVars^^[1].pSel := curGTypeSel;
			END;
		cgPMItem: 
			BEGIN
			IF DoDPopUpMenu(theDialog, popVars, cgPMItem, curCGSel) THEN
				popVars^^[2].pSel := curCGSel;
			END;
		okBtn: 
			IF NOT CheckOk THEN
				itemHit := 0;
		END;
	UNTIL itemHit IN [okBtn, cancelBtn];

	IF itemHit = OK THEN
		BEGIN
		HLock(Handle(theModel));
		WITH theModel^^ DO
			BEGIN
			CASE curGTypeSel OF
			addCmd: 
				dfltGtype := add;
			multCmd: 
				dfltGtype := mult;
			expCmd: 
				dfltGtype := expon;
			END;
			forceDelay := FALSE;
			forceImmed := FALSE;
			CASE curCGSel OF
			fImmedCmd: 
				forceImmed := TRUE;
			fDelayCmd: 
				forceDelay := TRUE;
			END;
			StringToNum(GetEdtText(theDialog, itnEdt), n);
			iteratns := n;
			str := GetEdtText(theDialog, precEdt);
			StringToNum(GetEdtText(theDialog, precEdt), n);
			numPrec := n;
			dfltStartVal := Str2Num(GetEdtText(theDialog, dfltStEdt));
			enableFlags := 0;
			IF GetDCtlvalue(theDialog, shockChk) = 1 THEN
				SetFactFlag(enableFlags, shocks, TRUE);
			IF GetDCtlvalue(theDialog, boundChk) = 1 THEN
				SetFactFlag(enableFlags, bounded, TRUE);
			IF GetDCtlvalue(theDialog, threshChk) = 1 THEN
				SetFactFlag(enableFlags, threshHolds, TRUE);
			END;
		HUnlock(Handle(theModel));
		ModelPrefsDlog := TRUE;
		END
	ELSE
		ModelPrefsDlog := FALSE;

	DisposDialog(theDialog);
	DisposeMenu(popVars^^[1].pHdl);
	DisposHandle(Handle(popVars));
	SetPort(savePort);
	END; {ModelPrefsDlog}

	FUNCTION VBoundsFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;

		CONST

			okBtn = 1;
			cancelBtn = 2;

		VAR
			tmpBool, int: BOOLEAN;
			ch: CHAR;
			chSet: SET OF CHAR;

	BEGIN
	tmpBool := FALSE;
	int := GetWRefCon(theDialog) = 1;
	WITH theEvent DO
		BEGIN
		IF what IN [keyDown, autoKey] THEN
			BEGIN
			ch := CHR(BITAND(message, CharCodeMask));
			IF ch IN [CHR(3), CHR($0D)] THEN
				BEGIN
				itemHit := okBtn;
				tmpBool := TRUE;
				END
			ELSE IF ch = CHR($1B) THEN
				BEGIN
				itemHit := cancelBtn;
				tmpBool := TRUE;
				END
			ELSE
				BEGIN
				IF int THEN
					chSet := ['0'..'9', '-', CHR($08), CHR($09)]
				ELSE
					chSet := ['0'..'9', '.', '-', CHR($08), CHR($09)];
				IF NOT (ch IN chSet) THEN
					BEGIN
					itemHit := 0;
					tmpBool := TRUE;
					SysBeep(5);
					END
				END;
			END;
		END;
	VBoundsFilter := tmpBool;
	END; {VBoundsFilter}

	FUNCTION SetVBounds (VAR lower, upper: DOUBLE;
									min, max: DOUBLE;
									int: BOOLEAN): BOOLEAN;
		CONST
			boundsID = 1005;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			lowerEdt = 6;
			upperEdt = 8;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			itemHdl: HANDLE;
			itemBox: Rect;
			dummy, prec: INTEGER;
			str: Str255;
			low, up: DOUBLE;

		FUNCTION CheckOk: BOOLEAN;
			VAR
				itemBad: INTEGER;
		BEGIN
		itemBad := 0;
		str := GetEdtText(theDialog, lowerEdt);
		IF NOT CheckNum(str) THEN
			BEGIN
			OneBtnAlert('Invalid number for lower bound.');
			itemBad := lowerEdt
			END
		ELSE
			BEGIN
			low := Str2Num(str);
			IF low < min THEN
				BEGIN
				OneBtnAlert(Concat('Lower bound must be >= ', NumToDecStr(prec, min)));
				itemBad := lowerEdt;
				END
			ELSE
				BEGIN
				str := GetEdtText(theDialog, upperEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number for upper bound.');
					itemBad := upperEdt
					END
				ELSE
					BEGIN
					up := Str2Num(str);
					IF low > max THEN
						BEGIN
						OneBtnAlert(Concat('Upper bound must be <= ', NumToDecStr(prec, max)));
						itemBad := upperEdt;
						END
					ELSE
						BEGIN
						IF low >= up THEN
							BEGIN
							OneBtnAlert('Lower bound must be LOWER than upper bound!!!');
							itemBad := lowerEdt;
							END;
						END;
					END;
				END;
			END;
		IF itemBad <> 0 THEN
			SelIText(theDialog, itemBad, 0, 32000);
		CheckOk := itemBad = 0;
		END; {CheckOK}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(boundsID, @stor, windowPtr(-1));
	SetPort(theDialog);
	IF int THEN
		BEGIN
		prec := 0;
		SetWRefCon(theDialog, 1);
		END
	ELSE
		BEGIN
		SetWRefCon(theDialog, 0);
		prec := 4;
		END;

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	str := NumToDecStr(prec, lower);
	SetEdtText(theDialog, lowerEdt, str);
	str := NumToDecStr(prec, upper);
	SetEdtText(theDialog, upperEdt, str);
	SelIText(theDialog, lowerEdt, 0, 32000);

	ShowWindow(theDialog);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@VBoundsFilter, itemHit);
		IF itemHit = okBtn THEN
			IF NOT CheckOk THEN
				itemHit := 0;
	UNTIL itemHit IN [okBtn, cancelBtn];

	IF itemHit = okBtn THEN
		BEGIN
		lower := low;
		upper := up;
		END;
	SetVBounds := itemHit = okBtn;
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {SetVBounds}

	CONST
		cBoxSize = 3;
	TYPE
		TPtArry = ARRAY[1..cmaxGPoints] OF Point;
		TVariRec = RECORD
				yMin, yMax: DOUBLE;
				xMax: INTEGER;
				yScale, xScale: DOUBLE;
				ptArray: TGpointArry;
				grafRect, activRect, coordRect: Rect;
				org: Point;
				points: TPtArry;
				grafPict: PicHandle;
				filledBox: INTEGER;
				allOk: BOOLEAN;
			END;
		TVariPtr = ^TVariRec;
		TVariHdl = ^TVariPtr;

	PROCEDURE DrawGrafAxes (theDialog: DialogPtr;
									theItem: INTEGER);
		VAR
			myVars: TVariHdl;
	BEGIN
	myVars := TVariHdl(GetWRefCon(theDialog));
	IF myVars^^.allOk THEN
		BEGIN
		EraseRect(myVars^^.grafRect);
		DrawPicture(myVars^^.grafPict, myVars^^.grafRect);
		END;
	END;{DrawGrafAxes}

	PROCEDURE DrawGrafLine (theDialog: DialogPtr;
									theItem: INTEGER);
		VAR
			myVars: TVariHdl;
			i: INTEGER;
			r: Rect;

		PROCEDURE BoxIt (thePt: Point;
										filled: BOOLEAN);
		BEGIN
		WITH thePt DO
			SetRect(r, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
		FrameRect(r);
		IF filled THEN
			FillRect(r, black);
		END; {BoxIt}

	BEGIN
	myVars := TVariHdl(GetWRefCon(theDialog));
	IF myVars^^.allOk THEN
		BEGIN
		EraseRect(myVars^^.activRect);
		ClipRect(myVars^^.activRect);
		IF myVars^^.points[1].h <> -32000 THEN
			BEGIN
			BoxIt(myVars^^.points[1], (myVars^^.filledBox = 1));
			MoveTo(myVars^^.points[1].h, myVars^^.points[1].v);
			i := 2;
			WHILE (myVars^^.points[i].h <> -32000) AND (i <= cMaxGPoints) DO
				BEGIN
				LineTo(myVars^^.points[i].h, myVars^^.points[i].v);
				BoxIt(myVars^^.points[i], (myVars^^.filledBox = i));
				i := i + 1;
				END;
			END;
		END;
	ClipRect(theDialog^.portRect);
	END;{DrawGrafLine}

	FUNCTION VariFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;
		CONST
			okBtn = 1;
			cancelBtn = 2;
			tooManyAlrt = 101;
			noRemove1BoxAlrt = 102;
			noRemoveLastBoxAlrt = 103;

		VAR
			myVars: TVarihdl;
			tmpBool, done: BOOLEAN;
			mPt: Point;
			pBox: Rect;
			ch: CHAR;
			i, j: INTEGER;
			xVal: INTEGER;
			yVal: DOUBLE;
			str: Str255;


	BEGIN
	myVars := TVarihdl(GetWRefCon(theDialog));
	ClipRect(theDialog^.portRect);
	tmpBool := FALSE;
	GetMouse(mPt);
	IF NOT EqualPt(mPt, lastMPoint) THEN
		IF PtInRect(mPt, myVars^^.activRect) THEN
			BEGIN
			SetCursor(crossHdl^^);
			xVal := mPt.h;
			i := 0;
			REPEAT
				i := i + 1;
				WITH myVars^^, points[i] DO
					SetRect(pBox, h - cBoxSize, activRect.top, h + cBoxSize, activRect.bottom);
			UNTIL PtInRect(mPt, pBox) OR (i >= cMaxGPoints) OR (myVars^^.points[i].h = -32000);
			IF PtInRect(mPt, pBox) THEN
				BEGIN
				xVal := myVars^^.points[i].h;
				IF i <> myVars^^.filledBox THEN
					BEGIN
					IF myVars^^.filledBox <> 0 THEN
						BEGIN
						WITH myVars^^.points[myVars^^.filledBox] DO
							SetRect(pBox, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
						IF SectRect(pBox, myVars^^.activRect, pBox) THEN
							;
						EraseRect(pBox);
						FrameRect(pBox);
						END;
					WITH myVars^^.points[i] DO
						SetRect(pBox, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
					IF SectRect(pBox, myVars^^.activRect, pBox) THEN
						;
					FillRect(pBox, black);
					myVars^^.filledBox := i;
					END;
				END
			ELSE IF myVars^^.filledBox <> 0 THEN
				BEGIN
				WITH myVars^^.points[myVars^^.filledBox] DO
					SetRect(pBox, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
				IF SectRect(pBox, myVars^^.activRect, pBox) THEN
					;
				EraseRect(pBox);
				FrameRect(pBox);
				myVars^^.filledBox := 0;
				END;

			WITH myVars^^ DO
				BEGIN
				xVal := ROUND((xVal - org.h) / xScale);
				yVal := (mPt.v - org.v) / yScale + yMin;
				END;
			EraseRect(myVars^^.coordRect);
			NumToString(xVal, str);
			str := Concat(str, ',', NumToDecStr(4, yVal));
			MoveTo(myVars^^.coordRect.left + 1, myVars^^.coordRect.bottom - 3);
			DrawString(str);
			END
		ELSE
			BEGIN
			IF myVars^^.filledBox <> 0 THEN
				BEGIN
				WITH myVars^^.points[myVars^^.filledBox] DO
					SetRect(pBox, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
				IF SectRect(pBox, myVars^^.activRect, pBox) THEN
					;
				EraseRect(pBox);
				FrameRect(pBox);
				myVars^^.filledBox := 0;
				END;
			EraseRect(myVars^^.coordRect);
			SetCursor(arrow);
			END;
	lastMPoint := mPt;

	WITH theEvent DO
		BEGIN
		IF what = mouseDown THEN
			BEGIN
			mPt := where;
			GlobalToLocal(mPt);
			IF PtInRect(mPt, myVars^^.activRect) THEN
				BEGIN
				i := 0;
				REPEAT
					i := i + 1;
					WITH myVars^^.points[i] DO
						SetRect(pBox, h - cBoxSize, v - cBoxSize, h + cBoxSize, v + cBoxSize);
				UNTIL (i >= cMaxGPoints) OR PtInRect(mPt, pBox);
				IF PtInRect(mPt, pBox) THEN
					BEGIN	{Remove Old Box}
					IF i = 1 THEN
						BEGIN
						itemHit := noRemove1BoxAlrt;
						tmpBool := TRUE;
						END
					ELSE IF (i = cMaxGPoints) OR (myVars^^.points[i + 1].h = -32000) THEN
						BEGIN
						itemHit := noRemoveLastBoxAlrt;
						tmpBool := TRUE;
						END
					ELSE
						BEGIN
						IF i = myVars^^.filledBox THEN
							myVars^^.filledBox := 0;
						FOR j := i TO (cMaxGPoints - 1) DO
							myVars^^.points[j] := myVars^^.points[j + 1];
						SetPt(myVars^^.points[cMaxGPoints], -32000, -32000);
						END;
					END
				ELSE
					BEGIN {New Box}
{Align mPt.h to nearest x}
					WITH myVars^^ DO
						mPt.h := TRUNC(ROUND((mPt.h - org.h) / xScale) * xScale) + org.h;
					i := 0;
					REPEAT
						i := i + 1;
					UNTIL (mPt.h <= myVars^^.points[i].h) OR (i >= cMaxGPoints);
					IF mPt.h = myVars^^.points[i].h THEN
						myVars^^.points[i].v := mPt.v
					ELSE IF myVars^^.points[cMaxGPoints].h <> -32000 THEN
						BEGIN
						itemHit := tooManyAlrt;
						tmpBool := TRUE;
						END
					ELSE IF myVars^^.points[i].h = -32000 THEN
						myVars^^.points[i] := mPt
					ELSE
						BEGIN
						FOR j := cMaxGPoints DOWNTO (i + 1) DO
							myVars^^.points[j] := myVars^^.points[j - 1];
						myVars^^.points[i] := mPt;
						END;
					END;
				InvalRect(myVars^^.grafRect);
				END;
			END
		ELSE IF what IN [keyDown, autoKey] THEN
			BEGIN
			ch := CHR(BITAND(message, CharCodeMask));
			IF ch IN [CHR(3), CHR($0D)] THEN
				BEGIN
				itemHit := okBtn;
				tmpBool := TRUE;
				END
			ELSE IF ch = CHR($1B) THEN
				BEGIN
				itemHit := cancelBtn;
				tmpBool := TRUE;
				END
			END;
		END;
	VariFilter := tmpBool;
	END; {VariFilter}

	FUNCTION SetVariGrowth (VAR dPoints: TScaledPtArry;
									nItns: INTEGER): BOOLEAN;
		CONST
			variDlogID = 1004;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			grafFrameItem = 5;
			boundsBtn = 4;
			grafLineItem = 6;
			coordItem = 7;
			tooManyAlrt = 101;
			noRemove1BoxAlrt = 102;
			noRemoveLastBoxAlrt = 103;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			itemHdl: HANDLE;
			itemBox: Rect;
			dummy, i: INTEGER;
			str: Str255;
			myVars: TVariHdl;
			quitIt: BOOLEAN;
			yVal, gMin, gMax: DOUBLE;

		PROCEDURE DrawAxes;
			VAR
				savePicSave: Handle;
				dummyX, dummyY: pVector;
				pErr: pError;
				tr: Point;
		BEGIN
		ClipRect(theDialog^.portRect);
		PlotInit;
		WITH myVars^^.grafRect DO
			SetPlotRect(left, top, right, bottom);
		NoXGrids;
		NoYGrids;
		SetTickFont(monaco, monaco);
		SetTickSizes(9, 9);
		SetTickPrecision(0, 2);
		XLabel('Iteration');
		YLabel('Growth Value');
		SetXTicks(0, 1, nItns);
		SetYTicks(myVars^^.yMin, (myVars^^.yMax - myVars^^.yMin), myVars^^.yMax);
		XTickLabelRange(1, 5, nItns);
		NoYTickMarks;
		NoXTickMarks;
		dummyX[1] := -1;	{plot off the graph. Only want the axes}
		dummyY[1] := myVars^^.yMin - 1;

		savePicSave := theDialog^.picSave;
		theDialog^.picSave := NIL;

		myVars^^.grafPict := OpenPicture(myVars^^.grafRect);
		PlotIn(1, dummyX, dummyY, pErr);
		ClosePicture;

		PlotReturn(0, myVars^^.yMin, myVars^^.org.h, myVars^^.org.v, pErr);
		PlotReturn(nItns, myVars^^.yMax, tr.h, tr.v, pErr);
		WITH myVars^^, activRect DO
			BEGIN
			top := tr.v;
			left := org.h;
			bottom := org.v;
			right := tr.h;
			xScale := (right - left) / nItns;
			yScale := (top - bottom) / (yMax - yMin);
			END;
		InvalRect(myVars^^.grafRect);
		GetDItem(theDialog, grafLineItem, dummy, itemHdl, itemBox);
		SetDItem(theDialog, grafLineItem, userItem, itemHdl, myVars^^.grafRect);
		theDialog^.picSave := savePicSave;
		END; {DrawAxes}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(variDlogID, @stor, windowPtr(-1));
	myVars := TVariHdl(NewHandle(SizeOf(TVariRec)));
	SetWRefCon(theDialog, LONGINT(myVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	GetDItem(theDialog, grafFrameItem, dummy, itemHdl, itemBox);
	myVars^^.grafRect := itemBox;
	SetDItem(theDialog, grafFrameItem, userItem, Handle(@DrawGrafAxes), itemBox);

	GetDItem(theDialog, grafLineItem, dummy, itemHdl, itemBox);
	SetDItem(theDialog, grafLineItem, dummy, Handle(@DrawGrafLine), itemBox);

	GetDItem(theDialog, coordItem, dummy, itemHdl, myVars^^.coordRect);

	SetPt(LastMPoint, -32000, -32000);

	MoveHHi(Handle(myVars));
	HLock(Handle(myVars));
	WITH myVars^^ DO
		BEGIN
		allOk := FALSE;
		filledBox := 0;
		FOR i := 1 TO cMaxGPoints DO
			SetPt(points[i], -32000, -32000);
		i := 1;
		yMin := dPoints[1].val;
		yMax := yMin;
		WHILE (dPoints[i].pt < dPoints[i + 1].pt) AND (i < (cMaxGPoints - 1)) DO
			BEGIN
			yVal := dPoints[i + 1].val;
			IF yVal < yMin THEN
				yMin := yVal
			ELSE IF yVal > ymax THEN
				yMax := yVal;
			i := i + 1;
			END;
		IF i = 1 THEN
			IF SetVBounds(yMin, yMax, -INF, INF, FALSE) THEN
				BEGIN
				gMin := yMin;
				gMax := yMax;
				quitIt := FALSE;
				dPoints[1].pt := 0;
				dPoints[1].val := gMin;
				dPoints[2].pt := 1;
				dPoints[2].val := gMin
				END
			ELSE
				quitIt := TRUE;
		END;
	HUnlock(Handle(myVars));
	IF NOT quitIt THEN
		BEGIN
		DrawAxes;

{Fill points Array}
		WITH myVars^^ DO
			BEGIN
			points[1].h := org.h;
			points[1].v := org.v + ROUND((dPoints[1].val - yMin) * yScale);
			i := 1;
			WHILE (dPoints[i].pt < dPoints[i + 1].pt) AND (i < cMaxGPoints) DO
				BEGIN
				points[i + 1].h := org.h + ROUND((dPoints[i + 1].pt * nItns) * xScale);
				points[i + 1].v := org.v + ROUND((dPoints[i + 1].val - yMin) * yScale);
				i := i + 1;
				END;
			END;
		myVars^^.allOk := TRUE;

		ShowWindow(theDialog);
		InitCursor;
		itemHit := 0;
		REPEAT
			ModalDialog(@VariFilter, itemHit);
			CASE itemHit OF
			tooManyAlrt: 
				OneBtnAlert('Too many points defined');
			noRemove1BoxAlrt: 
				OneBtnAlert('Cant remove first point');
			noRemoveLastBoxAlrt: 
				OneBtnAlert('Cant remove last point');
			boundsBtn: 
				BEGIN
				gMin := myVars^^.yMin;
				gMax := myVars^^.yMax;
				IF SetVBounds(gMin, gMax, -INF, INF, FALSE) THEN
					BEGIN
					myVars^^.yMin := gMin;
					myVars^^.yMax := gMax;
					KillPicture(myVars^^.grafPict);
					DrawAxes;
					END;
				END;
			END;
		UNTIL itemHit IN [okBtn, cancelBtn];

		IF itemHit = ok THEN
			BEGIN
			SetVariGrowth := TRUE;
			i := 1;
			WHILE (myVars^^.points[i].h <> -32000) AND (i <= cMaxGPoints) DO
				BEGIN
				WITH myVars^^ DO
					BEGIN
					IF points[i].h = -32000 THEN
						BEGIN
						dPoints[i].pt := 0;
						dPoints[i].val := 0;
						END
					ELSE
						BEGIN
						dPoints[i].pt := ROUND((points[i].h - org.h) / xScale) / nItns;
						dPoints[i].val := (points[i].v - org.v) / yScale + yMin;
						END;
					END;
				i := i + 1;
				END;
			END
		ELSE
			SetVariGrowth := FALSE;
		KillPicture(myVars^^.grafPict);

		END;
	DisposHandle(Handle(myVars));
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {SetVariGrowth}


	TYPE
		TFAVars = RECORD
				pVars: TPopMVars;		{This is first for the popup handlers}
				list1: ListHandle;
				lRect: Rect;
				lItem: INTEGER;
				tmpFact: TFactRec;
			END;
		TFAVarsPtr = ^TFAVars;
		TFAVarsHdl = ^TFAVarsPtr;


	FUNCTION SetFAFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;
		CONST
			okBtn = 1;
			cancelBtn = 2;

			shockChk = 7;
			shockList = 21;
			shockEdt = 12;


		VAR
			myVars: TFAVarsHdl;
			tmpBool: BOOLEAN;
			aPoint: Point;
			ch: CHAR;
			edtFld: INTEGER;
			theCell: Cell;
			nShocks, j: INTEGER;
			sCell: TShockCell;
			str, str1: Str255;
			gotShock, dblClick, noMore: BOOLEAN;

		PROCEDURE SetShocks (sVal: DOUBLE);
			VAR
				j: INTEGER;
		BEGIN
		SetPt(theCell, 0, 0);
		LDoDraw(FALSE, myVars^^.list1);
		WHILE LGetSelect(TRUE, theCell, myVars^^.list1) DO
			BEGIN
			sCell.pNum := theCell.v + 1;
			sCell.shockVal := sVal;
			LSetCell(@sCell, SizeOf(TShockCell), theCell, myVars^^.list1);
			theCell.v := theCell.v + 1;
			END;
		LDoDraw(TRUE, myVars^^.list1);
		InvalRect(myVars^^.list1^^.rView);
		END; {SetShocks}

	BEGIN
	myVars := TFAVarsHdl(GetWRefCon(theDialog));
	tmpBool := FALSE;
	IF DoListEvent(theEvent, FALSE, TRUE, dblClick, myVars^^.list1) THEN
		BEGIN
		SetEdtText(theDialog, shockEdt, '');
		SelIText(theDialog, shockEdt, 0, 32000);
		tmpBool := TRUE;
		END
	ELSE
		WITH theEvent DO
			BEGIN
			IF what IN [keyDown, autoKey] THEN
				BEGIN
				edtFld := DialogPeek(theDialog)^.editField + 1;
				ch := CHR(BITAND(message, CharCodeMask));
				IF ch IN [CHR(3), CHR($0D)] THEN
					BEGIN
					itemHit := okBtn;
					tmpBool := TRUE;
					END
				ELSE IF ch = CHR($1B) THEN
					BEGIN
					itemHit := cancelBtn;
					tmpBool := TRUE;
					END
				ELSE IF ch = CHR($09) THEN
					tmpBool := FALSE
				ELSE IF edtFld = shockEdt THEN
					BEGIN
					tmpBool := TRUE;
					IF NOT (ch IN ['0'..'9', '-', '.', CHR(8)]) OR NOT CheckTEKey(DialogPeek(theDialog)^.textH, ch) THEN
						SysBeep(5)
					ELSE
						BEGIN
						TEKey(ch, DialogPeek(theDialog)^.textH);
						str1 := GetEdtText(theDialog, shockEdt);
{Check for too many shocks}
						SetPt(theCell, 0, 0);
						nShocks := 0;		{How many existing shocks?}
						WITH myVars^^.tmpFact DO
							WHILE (nShocks < cMaxGPoints) AND (factShocks[nShocks + 1].period <> 0) DO
								nShocks := nShocks + 1;
						gotShock := FALSE;
						noMore := FALSE;
						WHILE LGetSelect(TRUE, theCell, myVars^^.list1) AND NOT noMore DO
							BEGIN
							FOR j := 1 TO nShocks DO
								gotShock := gotShock OR ((theCell.v + 1) = myVars^^.tmpFact.factShocks[j].period);
							IF NOT gotShock THEN
								BEGIN
								IF nShocks < cMaxGPoints THEN
									BEGIN
									nShocks := nShocks + 1;
									myVars^^.tmpFact.factShocks[nShocks].period := theCell.v + 1;
									myVars^^.tmpFact.factShocks[nShocks].gVal := Str2Num(str1);
									END
								ELSE
									noMore := TRUE;
								END;
							IF NOT noMore THEN
								theCell.v := theCell.v + 1;
							END;
						IF noMore THEN
							BEGIN
							WHILE LGetSelect(TRUE, theCell, myVars^^.list1) DO
								BEGIN
								LSetSelect(FALSE, theCell, myVars^^.list1);
								theCell.v := theCell.v + 1;
								END;
							NumToString(cMaxGPoints, str);
							OneBtnAlert(Concat('Too many shocks defined. Maximum of ', str, '. Try clearing some or use Variable Growths.'));
							END;
						SetShocks(Str2Num(str1))
						END;
					END
				ELSE IF ch = '*' THEN
					BEGIN
					SelIText(theDialog, edtFld, 0, 32000);
					TEKey(ch, DialogPeek(theDialog)^.textH);
					SelIText(theDialog, edtFld, 0, 32000);
					itemHit := 0;
					tmpBool := TRUE;
					END
				ELSE IF NOT (ch IN ['0'..'9', '-', '.', CHR(8)]) OR NOT CheckTEKey(DialogPeek(theDialog)^.textH, ch) THEN
					BEGIN
					SysBeep(5);
					itemHit := 0;
					tmpBool := TRUE;
					END
				END;
			END;
	SetFAFilter := tmpBool;
	END; {SetFAFilter}


	PROCEDURE UpdateShockList (theDialog: DialogPtr;
									theItem: INTEGER);
		VAR
			myVars: TFAVarsHdl;
			fRect: Rect;
			theType: INTEGER;
			itemHdl: Handle;
	BEGIN
	GetDItem(theDialog, theItem, theType, itemHdl, fRect);
	PenSize(1, 1);
	myVars := TFAVarsHdl(GetWRefCon(theDialog));
	IF DItemEnabled(theDialog, theItem) THEN
		PenPat(black)
	ELSE
		Penpat(gray);
	FrameRect(fRect);
	PenNormal;
	LUpdate(theDialog^.visRgn, myVars^^.list1);
	END;{ UpdateShockList}


	FUNCTION SetFactAttributes (theModel: TModelHdl;
									theFact: INTEGER;
									VAR newTarg: BOOLEAN): BOOLEAN;

		CONST
			faPMID = 3;
			delayImpCmd = 1;
			boundCmd = 2;
			threshCmd = 3;

			factAttrID = 1003;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			graphChk = 4;
			canModChk = 5;
			variChk = 6;
			gCurveBtn = 14;
			shockChk = 7;
			targChk = 8;
			firstChk = 4;
			lastChk = 8;
			clearBtn = 16;
			shockList = 21;
			shockEdt = 12;
			targEdt = 13;

			popMItem = 23;
			popMChk = 9;
			popEdt1 = 10;
			popEdt2 = 11;

			gTypePMItem = 22;
			gTypeTitle = 20;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			itemHdl: HANDLE;
			itemBox: Rect;
			dummy: INTEGER;
			ctlHdl: ControlHandle;
			theCell: Cell;
			str: Str255;
			fName: TFactName;
			myVars: TFAVarsHdl;
			sCell: TShockCell;
			chkOn, gotShock, wasTarg: BOOLEAN;
			edtFld, edt1, edt2: INTEGER;
			nShocks, i, j: INTEGER;
			sVal: DOUBLE;
			fInfo: FontInfo;
			fHeight: INTEGER;
			curGTypeSel, curPMSel: INTEGER;

		PROCEDURE BuildList;
			VAR
				theCell: Cell;
				cSize: Point;
				dBnds: Rect;
				i, sNum: INTEGER;
		BEGIN
		GetDItem(theDialog, shockList, dummy, itemHdl, itemBox);
		myVars^^.lItem := shockList;
		myVars^^.lRect := itemBox;
		WITH myVars^^.lRect DO
			BEGIN
			right := right - 15;
			SetPt(cSize, (right - left), 0);
			END;
		SetRect(dBnds, 0, 0, 1, theModel^^.iteratns);
		myVars^^.list1 := LNew(myVars^^.lRect, dBnds, cSize, cShockListID, theDialog, FALSE, FALSE, FALSE, TRUE);
		myVars^^.list1^^.selFlags := lNoDisjoint + lNoNilHilite;
		myVars^^.list1^^.refcon := LONGINT(theModel);
		myVars^^.lRect.right := myVars^^.lRect.right + 15;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, shockList, dummy, Handle(@UpdateShockList), itemBox);

		SetPt(theCell, 0, 0);
		FOR i := 1 TO theModel^^.iteratns DO
			BEGIN
			theCell.v := i - 1;
			sCell.pNum := i;
			sNum := 1;
			sCell.shockVal := -INF;
			WITH theModel^^.facts^^[theFact] DO
				REPEAT
					IF factShocks[sNum].period = i THEN
						sCell.shockVal := factShocks[sNum].gVal;
					sNum := sNum + 1;
				UNTIL (sCell.shockVal <> -INF) OR (sNum >= cMaxGPoints);
			LSetCell(@sCell, SizeOf(TShockCell), theCell, myVars^^.list1);
			END;
		LDoDraw(TRUE, myVars^^.list1);
		LUpdate(theDialog^.visRgn, myVars^^.list1);
		END; {BuildList}

		FUNCTION CheckOk: BOOLEAN;
			VAR
				itemBad: INTEGER;
				str: str255;

		BEGIN
		itemBad := 0;
		str := GetEdtText(theDialog, targEdt);
		IF NOT CheckNum(str) THEN
			BEGIN
			itemBad := targEdt;
			OneBtnAlert('Bad target value');
			END
		ELSE
			myVars^^.tmpFact.targetval := Str2Num(str);
		IF itemBad <> 0 THEN
			SelIText(theDialog, itemBad, 0, 32000);
		checkOk := itemBad = 0;
		END; {CheckOK}

		PROCEDURE SetPopUpFields;
			VAR
				val1, val2: DOUBLE;
				chkOn: BOOLEAN;
				prec: INTEGER;
				str: Str255;
		BEGIN
		MoveHHi(Handle(myVars));
		HLock(Handle(myVars));
		WITH myVars^^.tmpFact DO
			CASE curPMSel OF
			delayImpCmd: 
				BEGIN
				val1 := onLine.start;
				IF val1 <= 0 THEN
					val1 := -INF;
				val2 := onLine.finish;
				IF val2 >= theModel^^.iteratns THEN
					val2 := INF;
				chkOn := TstFactFlag(flags, delayImpact);
				prec := 0;
				END;
			boundCmd: 
				BEGIN
				val1 := gBounds.lower;
				val2 := gBounds.upper;
				chkOn := TstFactFlag(flags, bounded);
				prec := theModel^^.numPrec;
				END;
			threshCmd: 
				BEGIN
				val1 := gTHolds.lower;
				val2 := gTHolds.upper;
				chkOn := TstFactFlag(flags, threshholds);
				prec := theModel^^.numPrec;
				END;
			END;
		IF val1 = -INF THEN
			str := '*'
		ELSE
			str := NumToDecStr(prec, val1);
		SetEdtText(theDialog, popEdt1, str);
		IF val2 = INF THEN
			str := '*'
		ELSE
			str := NumToDecStr(prec, val2);
		SetEdtText(theDialog, popEdt2, str);
		IF ChkOn THEN
			SetDCtlValue(theDialog, popMChk, 1)
		ELSE
			SetDCtlValue(theDialog, popMChk, 0);
		HUnlock(Handle(myVars));
		END; {SetPopUpFields}

		FUNCTION SavePopUpFields: BOOLEAN;
			VAR
				itemBad: INTEGER;
				chkOn: BOOLEAN;
				str: Str255;
				val1, val2: DOUBLE;
		BEGIN
		itemBad := 0;
		str := GetEdtText(theDialog, popEdt1);
		IF NOT (CheckNum(str) OR (str = '*')) THEN
			BEGIN
			OneBtnAlert('Lower limit is not a valid number');
			itemBad := popEdt1;
			END
		ELSE
			BEGIN
			IF str = '*' THEN
				val1 := -INF
			ELSE
				val1 := Str2Num(str);
			str := GetEdtText(theDialog, popEdt2);
			IF NOT (CheckNum(str) OR (str = '*')) THEN
				BEGIN
				OneBtnAlert('Upper limit is not a valid number');
				itemBad := popEdt2;
				END
			ELSE
				BEGIN
				IF str = '*' THEN
					val2 := INF
				ELSE
					val2 := Str2Num(str);
				IF val1 >= val2 THEN
					BEGIN
					OneBtnAlert('Lower limit must be LESS than upper limit!');
					itemBad := popEdt1;
					END;
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			chkOn := GetDCtlValue(theDialog, popMChk) = 1;
			WITH myVars^^.tmpFact DO
				CASE curPMSel OF
				delayImpCmd: 
					BEGIN
					IF (val1 <> -INF) AND (val1 <> TRUNC(val1)) THEN
						BEGIN
						OneBtnAlert('Start value must be an integer');
						itemBad := popEdt1;
						END
					ELSE IF (val2 <> INF) AND (val2 <> TRUNC(val2)) THEN
						BEGIN
						OneBtnAlert('Start value must be an integer');
						itemBad := popEdt2;
						END
					ELSE
						BEGIN
						IF val1 = -INF THEN
							onLine.start := 0
						ELSE
							onLine.start := TRUNC(val1);
						IF val2 = INF THEN
							onLine.finish := cMaxItns + 1
						ELSE
							onLine.finish := TRUNC(val2);
						SetFactFlag(flags, delayImpact, chkOn);
						END;
					END;
				boundCmd: 
					BEGIN
					gBounds.lower := val1;
					gBounds.upper := val2;
					SetFactFlag(flags, bounded, chkOn);
					END;
				threshCmd: 
					BEGIN
					gTHolds.lower := val1;
					gTHolds.upper := val2;
					SetFactFlag(flags, threshholds, chkOn);
					END;
				END;

			END;
		IF itemBad <> 0 THEN
			SelIText(theDialog, itemBad, 0, 32000);
		SavePopUpFields := itemBad = 0;
		END; {SavePopUpFields}

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(factAttrID, @stor, windowPtr(-1));
	myVars := TFAVarsHdl(NewHandle(SizeOf(TFAVars)));
	SetWRefCon(theDialog, LONGINT(myVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	myVars^^.tmpFact := theModel^^.facts^^[theFact];
	BuildList;
	MoveHHi(Handle(myVars));
	HLock(Handle(myVars));
	WITH myVars^^, tmpFact DO
		BEGIN
		GetFontInfo(fInfo);
		WITH fInfo DO
			fHeight := ascent + descent + leading;
{Growth Type}
		GetDItem(theDialog, gTypePMItem, dummy, itemHdl, itemBox);
		WITH itemBox DO
			BEGIN
			bottom := top + fHeight;
			right := left + StringWidth('  Multiplicative ') + 4;
			END;
		SetDItem(theDialog, gTypePMItem, userItem, HANDLE(@UpdatePopMItem), itemBox);
		WITH pVars[1] DO
			BEGIN
			pItem := gTypePMItem;
			pRect := itemBox;
			pID := gTypeMID;
			pHdl := GetMenu(pID);
			titleItem := gTypeTitle;
			CASE gType OF
			add: 
				curGTypeSel := addCmd;
			mult: 
				curGTypeSel := multCmd;
			expon: 
				curGTypeSel := expCmd;
			END;
			pSel := curGTypeSel;
			END;

{The big one}
		GetDItem(theDialog, popMItem, dummy, itemHdl, itemBox);
		WITH itemBox DO
			BEGIN
			bottom := top + fHeight;
			right := left + StringWidth('   Delayed Impact ') + 4;
			END;

		SetDItem(theDialog, popMItem, userItem, HANDLE(@UpdatePopMItem), itemBox);
		WITH pVars[2] DO
			BEGIN
			pItem := popMItem;
			pRect := itemBox;
			pID := faPMID;
			pHdl := GetMenu(pID);
			pSel := delayImpCmd;
			curPMSel := delayImpCmd;
			titleItem := 0;
			END;
		CheckItem(pVars[2].pHdl, delayImpCmd, TstFactFlag(flags, delayImpact));
		CheckItem(pVars[2].pHdl, boundCmd, TstFactFlag(flags, bounded));
		CheckItem(pVars[2].pHdl, threshCmd, TstFactFlag(flags, threshholds));


		IF TstFactFlag(flags, graphIt) THEN
			SetDCtlValue(theDialog, graphChk, 1);
		IF TstFactFlag(flags, canMod) THEN
			SetDCtlValue(theDialog, canModChk, 1);
		IF TstFactFlag(flags, targeted) THEN
			SetDCtlValue(theDialog, targChk, 1);
		SetEdtText(theDialog, targEdt, NumToDecStr(theModel^^.numPrec, targetVal));
		IF TstFactFlag(flags, varigrowth) THEN
			SetDCtlValue(theDialog, variChk, 1)
		ELSE
			SetDCtlHilite(theDialog, gCurveBtn, FALSE);
		IF TstFactFlag(flags, shocks) THEN
			SetDCtlValue(theDialog, shockChk, 1)
		END;
	HUnlock(Handle(myVars));
	SetPopUpFields;
	SelIText(theDialog, popEdt1, 0, 32000);
	ParamText(theModel^^.facts^^[thefact].name, '', '', '');

	ShowWindow(theDialog);
	InvalRect(myVars^^.pVars[1].pRect);
	InvalRect(myVars^^.pVars[2].pRect);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@SetFAFilter, itemHit);
		CASE itemHit OF
		ok: 
			IF NOT SavePopUpFields OR NOT CheckOk THEN
				itemHit := 0;
		firstChk..lastChk: 
			BEGIN
			GetDItem(theDialog, itemHit, dummy, Handle(ctlHdl), itemBox);
			SetCtlValue(ctlHdl, ABS(GetCtlValue(ctlHdl) - 1));
			IF (itemHit = shockChk) AND (GetDctlValue(theDialog, shockChk) = 1) THEN
				SelIText(theDialog, shockEdt, 0, 32000)
			ELSE IF itemHit = variChk THEN
				SetDCtlHilite(theDialog, gCurveBtn, GetDCtlValue(theDialog, itemHit) = 1);
			END;
		gCurveBtn: 
			BEGIN
			MoveHHi(Handle(myVars));
			HLock(Handle(myVars));
			IF SetVariGrowth(myVars^^.tmpFact.vGrowths, theModel^^.iteratns) THEN
				;
			HUnlock(Handle(myVars));
			END;
		gTypePMItem: 
			BEGIN
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), gTypePMItem, curGTypeSel) THEN
				myVars^^.pVars[1].pSel := curGTypeSel;
			END;
		popMItem: 
			BEGIN
			i := curPMSel;
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), popMItem, i) THEN
				IF SavePopUpFields THEN
					BEGIN
					curPMsel := i;
					myVars^^.pVars[2].pSel := curPMSel;
					SetPopUpFields;
					SelIText(theDialog, popEdt1, 0, 32000);
					END;
			END;
		popMChk: 
			BEGIN
			GetDItem(theDialog, itemHit, dummy, Handle(ctlHdl), itemBox);
			SetCtlValue(ctlHdl, ABS(GetCtlValue(ctlHdl) - 1));
			chkOn := GetCtlValue(ctlHdl) = 1;
			CheckItem(myVars^^.pVars[2].pHdl, curPMSel, chkOn);
			InvalRect(myVars^^.pVars[2].pRect);
			END;
		clearBtn: 
			BEGIN
			nShocks := 0;		{How many existing shocks?}
			WITH myVars^^.tmpFact DO
				WHILE (nShocks < cMaxGPoints) AND (factShocks[nShocks + 1].gVal <> 0.0) DO
					nShocks := nShocks + 1;

			SetPt(theCell, 0, 0);
			WHILE LGetSelect(TRUE, theCell, myVars^^.list1) DO
				BEGIN
				i := 0;
				WITH myVars^^.tmpFact DO
					REPEAT
						i := i + 1;
					UNTIL (factShocks[i].period = (theCell.v + 1)) OR (i >= nShocks);
				IF myVars^^.tmpFact.factShocks[i].period = (theCell.v + 1) THEN
					BEGIN
					IF i < nShocks THEN
						BEGIN	{Close up shock array}
						WITH myVars^^.tmpFact DO
							FOR j := i TO (nShocks - 1) DO
								factShocks[j] := factShocks[j + 1];
						END;
					myVars^^.tmpFact.factShocks[nShocks].period := 0;
					myVars^^.tmpFact.factShocks[nShocks].gVal := -INF;
					nShocks := nShocks - 1;
					sCell.pNum := theCell.v + 1;
					sCell.shockVal := 0.0;
					LSetCell(@sCell, SizeOf(TShockCell), theCell, myVars^^.list1);
					END;
				theCell.v := theCell.v + 1;
				END;
			END;

		END;
		SetPt(theCell, 0, 0);
		IF LGetSelect(TRUE, thecell, myVars^^.list1) THEN
			SetDCtlHilite(theDialog, clearBtn, TRUE)
		ELSE
			SetDCtlHilite(theDialog, clearBtn, FALSE);
	UNTIL itemHit IN [okBtn, cancelBtn];

	IF itemHit = okBtn THEN
		BEGIN
		SetFactFlag(myVars^^.tmpFact.flags, graphIt, (GetDCtlValue(theDialog, graphChk) = 1));
		SetFactFlag(myVars^^.tmpFact.flags, canMod, (GetDCtlValue(theDialog, canModChk) = 1));
		SetFactFlag(myVars^^.tmpFact.flags, shocks, (GetDCtlValue(theDialog, shockChk) = 1));
		SetFactFlag(myVars^^.tmpFact.flags, variGrowth, (GetDCtlValue(theDialog, variChk) = 1));
		SetFactFlag(myVars^^.tmpFact.flags, targeted, (GetDCtlValue(theDialog, targChk) = 1));
		wasTarg := TstFactFlag(theModel^^.facts^^[theFact].flags, targeted);
		IF (GetDCtlValue(theDialog, targChk) = 1) THEN
			newTarg := NOT wasTarg OR (theModel^^.facts^^[theFact].targetVal <> myVars^^.tmpFact.targetVal)
		ELSE
			newTarg := wasTarg;
		theModel^^.facts^^[theFact] := myVars^^.tmpFact;
		END;
	SetFactAttributes := itemHit = okBtn;

	LDispose(myVars^^.list1);
	DisposeMenu(myVars^^.pVars[1].pHdl);
	DisposeMenu(myVars^^.pVars[2].pHdl);
	DisposHandle(Handle(myVars));
	DisposDialog(theDialog);
	SetPort(savePort);
	END;{SetFactAttributes}

	FUNCTION GetNameDlog (prompt: Str255;
									VAR name: Str255): BOOLEAN;

		CONST
			dlogID = 2100;

			ok = 1;
			cancel = 2;
			outlineItem = 3;
			edtFld = 4;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			curRad, dummy: INTEGER;
			itemBox: Rect;
			itemHdl: Handle;

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(dlogID, @stor, windowPtr(-1));
	SetPort(theDialog);

{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	SetEdtText(theDialog, edtFld, name);
	SelIText(theDialog, edtFld, 0, 32000);
	InitCursor;
	itemHit := 0;
	ShowWindow(theDialog);
	REPEAT
		ModalDialog(NIL, itemHit);
	UNTIL (itemHit IN [ok, cancel]);

	IF itemHit = ok THEN
		name := GetEdtText(theDialog, edtFld);
	GetNameDlog := itemHit = ok;
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {GetNameDlog}

	FUNCTION SavePopnAsDlog (VAR sel: INTEGER;
									statsEnable: BOOLEAN): BOOLEAN;
		CONST
			dlogID = 2101;

			ok = 1;
			cancel = 2;
			outlineItem = 3;
			cloneRad = 4;
			textRad = 5;
			statsRad = 6;

		VAR
			theDialog: DialogPtr;
			savePort: GrafPtr;
			stor: DialogRecord;
			itemHit: INTEGER;
			curRad, dummy: INTEGER;
			itemBox: Rect;
			itemHdl: Handle;

	BEGIN
	GetPort(savePort);
	theDialog := GetNewDialog(dlogID, @stor, windowPtr(-1));
	SetPort(theDialog);

	IF NOT statsEnable THEN
		BEGIN
		SetDCtlHilite(theDialog, statsRad, FALSE);
		IF sel = 3 THEN
			sel := 2;
		END;

	CASE sel OF
	1: 
		BEGIN
		SetDCtlValue(theDialog, cloneRad, 1);
		curRad := cloneRad;
		END;
	2: 
		BEGIN
		SetDCtlValue(theDialog, textRad, 1);
		curRad := textRad;
		END;
	3: 
		BEGIN
		SetDCtlValue(theDialog, statsRad, 1);
		curRad := statsRad;
		END;
	END;

{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, userItem, Handle(@OutlineButton), itemBox);

	InitCursor;
	itemHit := 0;
	ShowWindow(theDialog);
	REPEAT
		ModalDialog(NIL, itemHit);
		IF itemHit IN [cloneRad, textRad, statsRad] THEN
			IF itemHit <> curRad THEN
				BEGIN
				SetDCtlValue(theDialog, curRad, 0);
				curRad := itemHit;
				SetDCtlValue(theDialog, itemHit, 1);
				END;
	UNTIL (itemHit IN [ok, cancel]);

	IF itemHit = ok THEN
		sel := curRad - cloneRad + 1;
	SavePopnAsDlog := itemHit = ok;

	DisposDialog(theDialog);
	SetPort(savePort);
	END; {SavePopnAsDlog}

END. {GDialogs}