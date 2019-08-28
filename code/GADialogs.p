UNIT GADialogs;
INTERFACE

	USES
		SANE, DialogUtils, GFiles, GCommonDec, GGlobals, DialogUtils, GDialogs, GATypes;

	PROCEDURE PopnRenameDel (modelWD: WDHandle;
									rename: BOOLEAN);
	FUNCTION SetPopnParms (popn: TPopnHdl;
									VAR critParm: BOOLEAN): BOOLEAN;

IMPLEMENTATION
{$S GADlogs}

	FUNCTION PopnParmsFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;

		CONST
			phenoTyperID = 2000;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			listItem = 4;

			lowEdt = 5;
			upEdt = 6;
			resEdt = 7;
			maxPopEdt = 8;
			nSeedEdt = 9;
			maxGenEdt = 10;
			pCrossEdt = 11;
			pMutEdt = 12;
			convEdt = 13;

		VAR
			myVars: TPopMListHdl;
			tmpBool: BOOLEAN;
			dblClick: BOOLEAN;
			ch: CHAR;
			edtFld, saveStart, saveEnd, saveLen: INTEGER;
			cHdl: CharsHandle;
			teHdl: TEHandle;
			chSet: SET OF CHAR;
			str: Str255;
			theCell: Cell;
			oErr: OSErr;

	BEGIN
	myVars := TPopMListHdl(GetWRefCon(theDialog));
	tmpBool := FALSE;
	IF DoListEvent(theEvent, FALSE, TRUE, dblClick, myVars^^.lVars.list1) THEN
		BEGIN
		itemHit := listItem;
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
				ELSE IF NOT (ch IN [CHR($08), CHR($09)]) THEN
					BEGIN
					SetPt(theCell, 0, 0);
					IF LGetSelect(TRUE, theCell, myVars^^.lVars.list1) THEN
						BEGIN
						tmpBool := TRUE;
						IF (edtFld IN [lowEdt, upEdt, pCrossEdt, pMutEdt, convEdt]) THEN
							chSet := ['0'..'9', '.', '-']
						ELSE
							chSet := ['0'..'9'];
						IF NOT (ch IN chSet) THEN
							SysBeep(5)
						ELSE IF NOT CheckTEKey(DialogPeek(theDialog)^.textH, ch) THEN
							SysBeep(5)
						ELSE
							BEGIN
							TEKey(ch, DialogPeek(theDialog)^.textH);
							theCell.h := edtFld - lowEdt + 1;
							str := GetEdtText(theDialog, edtFld);
							LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, myVars^^.lVars.list1);
							END;
						END
					ELSE
						BEGIN
						SysBeep(5);
						tmpBool := TRUE;
						END;
					END;
				END;
			END;
	PopnParmsFilter := tmpBool;
	END; {PopnParmsFilter}

	FUNCTION SetPopnParms (popn: TPopnHdl;
									VAR critParm: BOOLEAN): BOOLEAN;
		CONST
			phenoTyperID = 1100;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			listItem = 4;

			lowEdt = 5;
			upEdt = 6;
			resEdt = 7;
			maxPopEdt = 8;
			nSeedEdt = 9;
			maxGenEdt = 10;
			pCrossEdt = 11;
			pMutEdt = 12;
			convEdt = 13;

			matePMItem = 15;
			matePMTitle = 14;
			mateMID = 5;
			oneSiteCmd = 1;
			twoSiteCmd = 2;

			convPMItem = 19;
			convPMTitle = 18;
			convMenuID = 7;
			fitSumCmd = 1;
			avgFitCmd = 2;
			bestChromCmd = 3;

			replPMItem = 21;
			replPMTitle = 20;
			replaceMenuID = 8;
			weakParentCmd = 1;
			bothParentCmd = 2;
			weakestChromCmd = 3;
			randomReplCmd = 4;

			selPMItem = 17;
			selPMTitle = 16;
			srchSelMenuID = 4;
			rouletteCmd = 1;
			randSelCmd = 2;
			fitFitCmd = 3;
			fitWeakCmd = 4;
			successiveCmd = 5;


		VAR
			theDialog: DialogPtr;
			stor: DialogRecord;
			savePort: GrafPtr;
			dummy, itemHit: INTEGER;
			itemHdl: Handle;
			itemBox: Rect;
			str: Str255;
			myVars: TPopMListHdl;
			curSelLine: INTEGER;
			theCell: Cell;
			i, j, dLen, pmSel: INTEGER;
			altPopn: TPopnHdl;
			oErr: OSErr;

		PROCEDURE BuildLists;
			VAR
				theCell: Cell;
				cSize: Point;
				dBnds: Rect;
				i, factNo: INTEGER;
				str: Str255;
		BEGIN
		GetDItem(theDialog, listItem, dummy, itemHdl, itemBox);
		myVars^^.lVars.item1 := listItem;
		myVars^^.lVars.rect1 := itemBox;
		WITH myVars^^.lVars.rect1 DO
			right := right - 15;
		SetPt(cSize, 0, 30);
		SetRect(dBnds, 0, 0, 4, 0);
		myVars^^.lVars.list1 := LNew(myVars^^.lVars.rect1, dBnds, cSize, cRowListID, theDialog, FALSE, FALSE, FALSE, TRUE);
		myVars^^.lVars.list1^^.selFlags := lOnlyOne;
		myVars^^.lVars.rect1 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, listItem, dummy, Handle(@UpdatePMListItem), itemBox);

		SetPt(theCell, 0, 0);
		FOR i := 1 TO popn^^.nGenes DO
			BEGIN
			IF FactIDIdx(popn^^.theModel, altPopn^^.phenoParms[i].factID, factNo) THEN
				BEGIN
				theCell.v := LAddRow(1, 999, myVars^^.lVars.list1);
				theCell.h := 0;
				str := popn^^.theModel^^.facts^^[factNo].name;
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, myVars^^.lVars.list1);

				theCell.h := 1;
				str := NumToDecStr(popn^^.theModel^^.numPrec, altPopn^^.phenoParms[i].lower);
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, myVars^^.lVars.list1);

				theCell.h := 2;
				str := NumToDecStr(popn^^.theModel^^.numPrec, altPopn^^.phenoParms[i].upper);
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, myVars^^.lVars.list1);

				theCell.h := 3;
				NumToString(altPopn^^.phenoParms[i].res, str);
				LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, myVars^^.lVars.list1);
				END;
			END;
		LDoDraw(TRUE, myVars^^.lVars.list1);
		LUpdate(theDialog^.visRgn, myVars^^.lVars.list1);
		END; {BuildLists}

		PROCEDURE SelListLine (line, item: INTEGER);
			VAR
				theCell: Cell;
				str: Str255;
				i: INTEGER;
		BEGIN
		LDoDraw(FALSE, myVars^^.lVars.list1);
		SetPt(theCell, 0, 0);
		WHILE LGetSelect(TRUE, theCell, myVars^^.lVars.list1) DO
			BEGIN
			LSetSelect(FALSE, theCell, myVars^^.lVars.list1);
			IF theCell.h <= 4 THEN
				theCell.h := theCell.h + 1
			ELSE
				BEGIN
				theCell.h := 0;
				theCell.v := theCell.v + 1;
				END;
			END;
		SetPt(theCell, 0, line);
		LSetSelect(TRUE, theCell, myVars^^.lVars.list1);
		LDoDraw(TRUE, myVars^^.lVars.list1);
		LUpdate(theDialog^.visRgn, myVars^^.lVars.list1);
		FOR i := 1 TO 3 DO
			BEGIN
			theCell.h := i;
			dLen := 255;
			LGetCell(Ptr(LONGINT(@str) + 1), dLen, theCell, myVars^^.lVars.list1);
			str[0] := CHR(dlen);
			SetEdtText(theDialog, (lowEdt + i - 1), str);
			END;
		IF item > 0 THEN
			SelIText(theDialog, (lowEdt + item - 1), 0, 32000)
		ELSE
			SelIText(theDialog, lowEdt, 0, 32000);
		CASE altPopn^^.phenoParms[curSelLine + 1].crossSites OF
		oneSiteC: 
			myVars^^.pVars[1].pSel := oneSiteCmd;
		twoSiteC: 
			myVars^^.pVars[1].pSel := twoSiteCmd;
		END;
		InvalRect(myVars^^.pVars[1].pRect);
		END; {SelListLine}

		FUNCTION CheckOk: BOOLEAN;
			VAR
				itemBad, i: INTEGER;
				str: Str255;
				num, num1: DOUBLE;
				lNum: LONGINT;
		BEGIN
		critParm := FALSE;
		itemBad := 0;
		str := GetEdtText(theDialog, lowEdt);
		IF NOT CheckNum(str) THEN
			BEGIN
			OneBtnAlert('Invalid number');
			itemBad := lowEdt;
			END
		ELSE
			BEGIN
			num := Str2Num(str);
			IF (num < -1) OR (num > 1) THEN
				BEGIN
				OneBtnAlert('Value range from -1 to 1 only.');
				itemBad := lowEdt;
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			altPopn^^.phenoParms[curSelLine + 1].lower := num;
			str := GetEdtText(theDialog, upEdt);
			IF NOT CheckNum(str) THEN
				BEGIN
				OneBtnAlert('Invalid number');
				itemBad := upEdt;
				END
			ELSE
				BEGIN
				num1 := Str2Num(str);
				IF (num1 < -1) OR (num1 > 1) THEN
					BEGIN
					OneBtnAlert('Value range from -1 to 1 only.');
					itemBad := upEdt;
					END
				ELSE IF num1 <= num THEN
					BEGIN
					OneBtnAlert('Lower bound must be LESS than upper bound');
					itemBad := lowEdt;
					END;
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			altPopn^^.phenoParms[curSelLine + 1].upper := num1;
			str := GetEdtText(theDialog, resEdt);
			IF NOT CheckNum(str) THEN
				BEGIN
				OneBtnAlert('Invalid number');
				itemBad := resEdt;
				END
			ELSE
				BEGIN
				num := Str2Num(str);
				IF (num < 1) OR (num > cMaxBitStr) THEN
					BEGIN
					OneBtnAlert('Resolution range from 1 to 10 only.');
					itemBad := resEdt;
					END
				ELSE
					altPopn^^.phenoParms[curSelLine + 1].res := TRUNC(num);
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			altPopn^^.phenoParms[curSelLine + 1].mateMethod := crossM;
			CASE myVars^^.pVars[1].pSel OF
			oneSiteCmd: 
				altPopn^^.phenoParms[curSelLine + 1].crossSites := oneSiteC;
			twoSiteCmd: 
				altPopn^^.phenoParms[curSelLine + 1].crossSites := twoSiteC;
			END;
			str := GetEdtText(theDialog, maxPopEdt);
			IF NOT CheckNum(str) THEN
				BEGIN
				OneBtnAlert('Invalid number');
				itemBad := maxPopEdt;
				END
			ELSE
				BEGIN
				StringToNum(str, lNum);
				IF (lNum < cPopulationMin) OR (lNum > cPopulationMax) THEN
					BEGIN
					OneBtnAlert('Population Size out of bounds');
					itemBad := maxPopEdt;
					END
				END;
			IF itemBad = 0 THEN
				BEGIN
				altPopn^^.popMax := lNum;
				str := GetEdtText(theDialog, nSeedEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number');
					itemBad := nSeedEdt;
					END
				ELSE
					BEGIN
					StringToNum(str, lNum);
					IF (lNum < cPopulationMin) OR (lNum > altPopn^^.popMax) THEN
						BEGIN
						OneBtnAlert('Starting seeds are out of bounds');
						itemBad := nSeedEdt;
						END;
					END;
				END;
			IF itemBad = 0 THEN
				BEGIN
				altPopn^^.nSeed := lNum;
				str := GetEdtText(theDialog, maxGenEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number');
					itemBad := maxGenEdt;
					END
				ELSE
					StringToNum(str, lNum);
				END;
			IF itemBad = 0 THEN
				BEGIN
				altPopn^^.maxGenerations := lNum;
				str := GetEdtText(theDialog, convEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number');
					itemBad := convEdt;
					END
				ELSE
					BEGIN
					num := Str2Num(str);
					IF (num < 0) THEN
						BEGIN
						OneBtnAlert('Invalid Convergence Value');
						itembad := convEdt;
						END;
					END;
				END;
			IF itemBad = 0 THEN
				BEGIN
				altPopn^^.convergeVal := num;
				str := GetEdtText(theDialog, pCrossEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number');
					itemBad := pCrossEdt;
					END
				ELSE
					BEGIN
					num := Str2Num(str);
					IF (num < 0) OR (num > 1) THEN
						BEGIN
						OneBtnAlert('Invalid Crossing Probabilty');
						itemBad := pCrossEdt;
						END;
					END;
				END;
			IF itemBad = 0 THEN
				BEGIN
				altPopn^^.pCross := num;
				str := GetEdtText(theDialog, pMutEdt);
				IF NOT CheckNum(str) THEN
					BEGIN
					OneBtnAlert('Invalid number');
					itemBad := pMutEdt;
					END
				ELSE
					BEGIN
					num := Str2Num(str);
					IF (num < 0) OR (num > 1) THEN
						BEGIN
						OneBtnAlert('Invalid Mutation Probabilty');
						itemBad := pMutEdt;
						END
					ELSE
						altPopn^^.pMut := num;
					END;
				END;
			END;
		IF itemBad = 0 THEN
			BEGIN
			CASE myVars^^.pVars[2].pSel OF
			rouletteCmd: 
				altPopn^^.selectMethod := rouletteS;
			randSelCmd: 
				altPopn^^.selectMethod := randomS;
			fitFitCmd: 
				altPopn^^.selectMethod := fitFitS;
			fitWeakCmd: 
				altPopn^^.selectMethod := fitWeakS;
			END;
			CASE myVars^^.pVars[3].pSel OF
			fitSumCmd: 
				altPopn^^.convMethod := fitSumV;
			avgFitCmd: 
				altPopn^^.convMethod := avgFitV;
			bestChromCmd: 
				altPopn^^.convMethod := bestChromV;
			END;
			CASE myVars^^.pVars[4].pSel OF
			weakParentCmd: 
				altPopn^^.replaceMethod := weakParentR;
			bothParentCmd: 
				altPopn^^.replaceMethod := bothParentR;
			weakestChromCmd: 
				altPopn^^.replaceMethod := weakestChromR;
			randomReplCmd: 
				altPopn^^.replaceMethod := randomR;
			END;
			IF popn^^.nGenes > 0 THEN
				BEGIN
				critParm := altPopn^^.selectMethod <> popn^^.selectMethod;
				IF critParm THEN
					IF NOT TwoBtnAlert('Changing the selection method will clear the population.', 'OK', 'CANCEL') THEN
						BEGIN
						altPopn^^.selectMethod := popn^^.selectMethod;
						critParm := FALSE;
						END;
				IF NOT critParm THEN
					BEGIN
					critParm := altPopn^^.popMax <> popn^^.popMax;
					IF critParm THEN
						IF NOT TwoBtnAlert('Changing the maximum population will clear the population.', 'OK', 'CANCEL') THEN
							BEGIN
							altPopn^^.popMax := popn^^.popMax;
							critParm := FALSE;
							END;
					END;
				IF NOT critParm THEN
					BEGIN
					FOR i := 1 TO altPopn^^.nGenes DO
						WITH altPopn^^.phenoParms[i] DO
							BEGIN
							critParm := critparm OR (upper <> popn^^.phenoParms[i].upper);
							critParm := critparm OR (lower <> popn^^.phenoParms[i].lower);
							critParm := critparm OR (res <> popn^^.phenoParms[i].res);
							END;
					IF critParm THEN
						IF NOT TwoBtnAlert('Changing the phenotype parameters will clear the population.', 'OK', 'CANCEL') THEN
							BEGIN
							altPopn^^.phenoParms := popn^^.phenoParms;
							critParm := FALSE;
							END;
					END;
				END;
			END;
		IF itemBad > 0 THEN
			SelIText(theDialog, itemBad, 0, 32000);
		CheckOk := itemBad = 0;
		END; {CheckOk}

	BEGIN
	IF popn^^.nGenes = 0 THEN
		BEGIN
		OneBtnAlert('No modifiable factors defined');
		SetPopnParms := FALSE;
		EXIT(SetPopnParms);
		END;
	altPopn := popn;
	oErr := HandToHand(Handle(altPopn));
	IF oErr <> noErr THEN
		BEGIN
		OneBtnAlert('Insufficient Memory');
		SetPopnParms := FALSE;
		EXIT(SetPopnParms);
		END;

	GetPort(savePort);
	theDialog := GetNewDialog(phenoTyperID, @stor, windowPtr(-1));
	myVars := TPopMListHdl(NewHandle(SizeOf(TPopMListRec)));
	SetWRefCon(theDialog, LONGINT(myVars));
	SetPort(theDialog);

	{ Set up user type procedures }
	GetDItem(theDialog, OK, dummy, itemHdl, itemBox);
	SetDItem(theDialog, outlineItem, UserItem, HANDLE(@OutlineButton), itemBox);

	SetDPopMenu(theDialog, myVars^^.pVars[1], mateMID, matePMitem, matePMTitle);
	SetDPopMenu(theDialog, myVars^^.pVars[2], srchSelMenuID, selPMitem, selPMTitle);
	CASE altPopn^^.selectMethod OF
	rouletteS: 
		myVars^^.pVars[2].pSel := rouletteCmd;
	randomS: 
		myVars^^.pVars[2].pSel := randSelCmd;
	fitFitS: 
		myVars^^.pVars[2].pSel := fitFitCmd;
	fitWeakS: 
		myVars^^.pVars[2].pSel := fitWeakCmd;
	END;
	SetDPopMenu(theDialog, myVars^^.pVars[3], convMenuID, convPMitem, convPMTitle);
	CASE altPopn^^.convMethod OF
	fitSumV: 
		myVars^^.pVars[3].pSel := fitSumCmd;
	avgFitV: 
		myVars^^.pVars[3].pSel := avgFitCmd;
	bestChromV: 
		myVars^^.pVars[3].pSel := bestChromCmd;
	END;
	SetDPopMenu(theDialog, myVars^^.pVars[4], replaceMenuID, replPMitem, replPMTitle);
	CASE altPopn^^.replaceMethod OF
	weakParentR: 
		myVars^^.pVars[4].pSel := weakParentCmd;
	bothParentR: 
		myVars^^.pVars[4].pSel := bothParentCmd;
	weakestChromR: 
		myVars^^.pVars[4].pSel := weakestChromCmd;
	randomR: 
		myVars^^.pVars[4].pSel := randomReplCmd;
	END;
	BuildLists;
	curSelLine := 0;
	SelListLine(curSelLine, 1);

	str := NumToDecStr(0, popn^^.popMax);
	SetEdtText(theDialog, maxPopEdt, str);
	str := NumToDecStr(0, popn^^.nSeed);
	SetEdtText(theDialog, nSeedEdt, str);
	str := NumToDecStr(0, popn^^.maxGenerations);
	SetEdtText(theDialog, maxGenEdt, str);
	str := NumToDecStr(4, popn^^.convergeVal);
	SetEdtText(theDialog, convEdt, str);
	str := NumToDecStr(4, popn^^.pCross);
	SetEdtText(theDialog, pCrossEdt, str);
	str := NumToDecStr(4, popn^^.pMut);
	SetEdtText(theDialog, pMutEdt, str);
	SelIText(theDialog, maxPopEdt, 0, 32000);

	ShowWindow(theDialog);
	InitCursor;
	itemHit := 0;
	REPEAT
		ModalDialog(@PopnParmsFilter, itemHit);
		CASE itemHit OF
		listItem: 
			BEGIN
			SetPt(theCell, 0, 0);
			IF LGetSelect(TRUE, theCell, myVars^^.lVars.list1) THEN
				BEGIN
				IF theCell.v = curSelLine THEN
					BEGIN
					IF myVars^^.lVars.list1^^.lastClick.h > 0 THEN
						SelIText(theDialog, (lowEdt + myVars^^.lVars.list1^^.lastClick.h - 1), 0, 32000)
					ELSE
						SelIText(theDialog, lowEdt, 0, 32000);
					END
				ELSE
					BEGIN
					IF CheckOk THEN
						BEGIN
						curSelLine := theCell.v;
						SelListLine(curSelLine, myVars^^.lVars.list1^^.lastClick.h);
						END
					ELSE
						BEGIN
						SetPt(theCell, 0, 0);
						IF LGetSelect(TRUE, theCell, myVars^^.lVars.list1) THEN
							LSetSelect(FALSE, theCell, myVars^^.lVars.list1);
						SetPt(theCell, 0, curSelLine);
						LSetSelect(TRUE, theCell, myVars^^.lVars.list1);
						END;
					END;
				END
			ELSE
				BEGIN
				str := '';
				SetEdtText(theDialog, lowEdt, str);
				SetEdtText(theDialog, upEdt, str);
				SetEdtText(theDialog, resEdt, str);
				END;
			END;
		matePMitem: 
			BEGIN
			pmSel := myVars^^.pVars[1].pSel;
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), itemHit, pmSel) THEN
				myVars^^.pVars[1].pSel := pmSel;
			END;
		selPMitem: 
			BEGIN
			pmSel := myVars^^.pVars[2].pSel;
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), itemHit, pmSel) THEN
				myVars^^.pVars[2].pSel := pmSel;
			END;
		convPMitem: 
			BEGIN
			pmSel := myVars^^.pVars[3].pSel;
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), itemHit, pmSel) THEN
				myVars^^.pVars[3].pSel := pmSel;
			END;
		replPMitem: 
			BEGIN
			pmSel := myVars^^.pVars[4].pSel;
			IF DoDPopUpMenu(theDialog, TPopMHdl(myVars), itemHit, pmSel) THEN
				myVars^^.pVars[4].pSel := pmSel;
			END;
		okBtn: 
			IF NOT CheckOk THEN
				itemHit := 0;
		END;
	UNTIL (itemHit IN [okBtn, cancelBtn]);

	IF itemHit = ok THEN
		BEGIN
		popn^^ := altPopn^^;
		SetPopnParms := TRUE;
		END
	ELSE
		SetPopnParms := FALSE;

	FOR i := 1 TO 4 DO
		DisposeMenu(myVars^^.pVars[i].pHdl);
	LDispose(myVars^^.lVars.list1);
	DisposHandle(Handle(myVars));
	DisposHandle(Handle(altPopn));
	DisposDialog(theDialog);
	SetPort(savePort);
	END; {SetPopnParms}


	FUNCTION PopnRenameFilter (theDialog: DialogPtr;
									VAR theEvent: EventRecord;
									VAR itemHit: INTEGER): BOOLEAN;

		CONST
			phenoTyperID = 2000;
			okBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			listItem = 4;

		VAR
			listVars: ListVarsHand;
			tmpBool, dblClick: BOOLEAN;
			ch: CHAR;

	BEGIN
	listVars := ListVarsHand(GetWRefCon(theDialog));
	tmpBool := FALSE;
	IF DoListEvent(theEvent, FALSE, TRUE, dblClick, listVars^^.list1) THEN
		BEGIN
		IF dblClick THEN
			itemHit := listItem;
		tmpBool := TRUE;
		END
	ELSE
		WITH theEvent DO
			BEGIN
			IF what IN [keyDown, autoKey] THEN
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
	PopnRenameFilter := tmpBool;
	END; {PopnRenameFilter}


	PROCEDURE PopnRenameDel (modelWD: WDHandle;
									rename: BOOLEAN);

		CONST
			renDelDlogID = 1103;
			dfltBtn = 1;
			cancelBtn = 2;
			outlineItem = 3;
			listItem = 4;
			edt1 = 5;
			edtTitle = 6;
			dlogTitle = 7;

		VAR
			theDialog: DialogPtr;
			stor: DialogRecord;
			savePort: GrafPtr;
			dummy, itemHit: INTEGER;
			itemHdl: Handle;
			itemBox: Rect;
			ctlHdl: ControlHandle;
			str: Str255;
			listVars: ListVarsHand;
			theCell: Cell;
			refNum, rErr: INTEGER;

		PROCEDURE BuildList;
			VAR
				theCell: Cell;
				cSize: Point;
				dBnds: Rect;
				i, factNo: INTEGER;
				str: Str255;
				nPopn: INTEGER;
		BEGIN
		GetDItem(theDialog, listItem, dummy, itemHdl, itemBox);
		listVars^^.item1 := listItem;
		listVars^^.rect1 := itemBox;
		WITH listVars^^.rect1 DO
			right := right - 15;
		SetPt(cSize, 0, 0);
		SetRect(dBnds, 0, 0, 1, 0);
		listVars^^.list1 := LNew(listVars^^.rect1, dBnds, cSize, 0, theDialog, FALSE, FALSE, FALSE, TRUE);
		IF rename THEN
			listVars^^.list1^^.selFlags := lOnlyOne;
		listVars^^.rect1 := itemBox;
		InsetRect(itemBox, -1, -1);
		itemBox.right := itemBox.right - 1;
		SetDItem(theDialog, listItem, dummy, Handle(@UpdateListItem), itemBox);

		SetPt(theCell, 0, 0);
		nPopn := CountMItems(theMenus[popnSelM]);
		FOR i := 5 TO nPopn DO
			BEGIN
			GetItem(theMenus[popnSelM], i, str);
			theCell.v := LAddRow(1, 999, listVars^^.list1);
			LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);
			END;
		LDoDraw(TRUE, listVars^^.list1);
		LUpdate(theDialog^.visRgn, listVars^^.list1);
		END; {BuildList}

		PROCEDURE DelPopn;
			VAR
				theCell: Cell;
				dLen: INTEGER;
				str: Str255;
				rHdl: Handle;
		BEGIN
		LDoDraw(FALSE, listVars^^.list1);
		SetPt(theCell, 0, 0);
		WHILE LGetSelect(TRUE, theCell, listVars^^.list1) DO
			BEGIN
			dLen := 255;
			LGetCell(Ptr(LONGINT(@str) + 1), dLen, theCell, listvars^^.list1);
			str[0] := CHR(dLen);
			rHdl := GetNamedResource(popnWindResType, str);
			IF rHdl <> NIL THEN
				BEGIN
				RmveResource(rHdl);
				DisposHandle(rHdl);
				END;
			DelMenuItem(theMenus[popnSelM], (theCell.v + 5));
			LDelRow(1, theCell.v, listVars^^.list1);
			END;
		UpdateResFile(refNum);
		LDoDraw(TRUE, listVars^^.list1);
		InvalRect(listVars^^.list1^^.rView);
		END; {DelPopn}

		PROCEDURE RenamePopn;
			VAR
				theCell: Cell;
				dLen: INTEGER;
				str: Str255;
				rHdl: Handle;
				rID: INTEGER;
				rType: ResType;
				rName: Str255;
				ok: BOOLEAN;
		BEGIN
		LDoDraw(FALSE, listVars^^.list1);
		ok := TRUE;
		str := GetEdtText(theDialog, edt1);
		rName := UniqueWindName(popnWind, str);
		IF NOT EqualString(str, rName, FALSE, FALSE) THEN
			IF TwoBtnAlert(Concat(str, ' is not unique. Use ', rName, '?'), 'OK', 'CANCEL') THEN
				str := rName
			ELSE
				ok := FALSE;
		IF ok THEN
			BEGIN
			SetPt(theCell, 0, 0);
			IF LGetSelect(TRUE, theCell, listVars^^.list1) THEN
				BEGIN
				dLen := 255;
				LGetCell(Ptr(LONGINT(@rName) + 1), dLen, theCell, listvars^^.list1);
				rName[0] := CHR(dLen);
				rHdl := GetNamedResource(popnWindResType, rName);
				IF rHdl <> NIL THEN
					BEGIN
					GetResInfo(rHdl, rID, rType, rName);
					SetResInfo(rHdl, rID, str);
					ChangedResource(rHdl);
					LSetCell(Ptr(LONGINT(@str) + 1), Length(str), theCell, listVars^^.list1);
					SetItem(theMenus[popnSelM], (theCell.v + 5), str);
					ReleaseResource(rHdl);
					END;
				LSetSelect(FALSE, theCell, listvars^^.list1);
				END;
			UpdateResFile(refNum);
			END;
		LDoDraw(TRUE, listVars^^.list1);
		LUpdate(theDialog^.visRgn, listVars^^.list1);
		END; {RenamePopn}

	BEGIN
	refNum := OpenResFork(modelWD^^.modelFile, rErr);
	IF refNum = 0 THEN
		OneBtnAlert('Cant open model resource fork.')
	ELSE
		BEGIN
		GetPort(savePort);
		theDialog := GetNewDialog(renDelDlogID, @stor, windowPtr(-1));
		listVars := ListVarsHand(NewHandle(SizeOf(ListVarsRec)));
		SetWRefCon(theDialog, LONGINT(listVars));
		SetPort(theDialog);

	{ Set up user type procedures }
		GetDItem(theDialog, dfltBtn, dummy, itemHdl, itemBox);
		SetDItem(theDialog, outlineItem, UserItem, HANDLE(@OutlineButton), itemBox);

		IF NOT rename THEN
			BEGIN
			HideDItem(theDialog, edt1);
			HideDItem(theDialog, edtTitle);
			SetEdtText(theDialog, dlogTitle, 'Delete Population');
			SizeWindow(theDialog, 372, 174, FALSE);
			GetDItem(theDialog, dfltBtn, dummy, Handle(ctlHdl), itemBox);
			SetCTitle(ctlHdl, 'Delete');
			END
		ELSE
			SetEdtText(theDialog, dlogTitle, 'Rename Population');
		BuildList;

		SetDCtlHilite(theDialog, dfltBtn, FALSE);

		ShowWindow(theDialog);
		InitCursor;
		itemHit := 0;
		REPEAT
			ModalDialog(@PopnRenameFilter, itemHit);
			CASE itemHit OF
			dfltBtn: 
				IF rename THEN
					RenamePopn
				ELSE
					DelPopn;
			listItem: 
				IF NOT rename THEN
					DelPopn;
			END;
			SetPt(theCell, 0, 0);
			IF LGetSelect(TRUE, theCell, listVars^^.list1) THEN
				SetDCtlHilite(theDialog, dfltBtn, TRUE)
			ELSE
				SetDCtlHilite(theDialog, dfltBtn, FALSE);
		UNTIL itemHit = cancelBtn;

		LDispose(listVars^^.list1);
		DisposHandle(Handle(listVars));
		DisposDialog(theDialog);
		SetPort(savePort);
		CloseResFile(refNum);
		END;
	END; {PopnRenameDel}

END. {GADialogs}