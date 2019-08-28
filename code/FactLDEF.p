UNIT GenieList;

INTERFACE

	USES
		SANE, DialogUtils, GFiles, GCommonDec;

	PROCEDURE MAIN (lMessage: INTEGER;
									lSelect: BOOLEAN;
									lRect: Rect;
									lCell: Cell;
									lDataOffset, lDataLen: INTEGER;
									lHandle: ListHandle);

IMPLEMENTATION

	PROCEDURE MAIN (lMessage: INTEGER;
									lSelect: BOOLEAN;
									lRect: Rect;
									lCell: Cell;
									lDataOffset, lDataLen: INTEGER;
									lHandle: ListHandle);

		VAR
			txFontSave, txSizeSave: INTEGER;

		FUNCTION NumToDecStr (prec: INTEGER;
										num: EXTENDED): Str255;
			VAR
				f: DecForm;
				dStr: DecStr;
		BEGIN
		f.Style := FixedDecimal;
		f.digits := prec;
		Num2Str(f, num, dStr);
		NumToDecStr := dStr;
		END; {NumToDecStr}


		PROCEDURE SaveFonts;
			VAR
				gPort: GrafPtr;
		BEGIN
		GetPort(gPort);
		txFontSave := gPort^.txFont;
		txSizeSave := gPort^.txSize;
		TextFont(monaco);
		TextSize(9);
		END; {SaveGPortFonts}

		PROCEDURE RestoreFonts;
		BEGIN
		TextFont(txFontSave);
		TextSize(txSizeSave);
		END; {RestoreFonts}

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

		PROCEDURE HiliteCell;
		BEGIN
		InvertRect(lRect);
		END;

		PROCEDURE InitList;
		BEGIN
		lHandle^^.userHandle := NewHandle(SizeOf(TLDataRec));
		WITH TLDataHdl(lHandle^^.userHandle)^^ DO
			BEGIN
{lHeight := lHandle^^.rView.bottom - lHandle^^.rView.top;}
			WITH lHandle^^.rView DO
				lWidth := right - left;
			vScroll := lHandle^^.vScroll;
			hScroll := lHandle^^.hScroll;
			END;
		END; {InitList}


		PROCEDURE DrawCell;
			VAR
				prec, i, factNum, linkNum: INTEGER;
				str: Str255;
				r: Rect;
				cellRec: TFactCellRec;
				lData: TLDataRec;
				theModel: TModelHdl;
				fRec: TFactRec;
				saveFlags, showFlags: TFlags;
				whichVal: TDisplayValue;

			PROCEDURE DoFlags;
			BEGIN
			IF BITTST(@showFlags, ORD(canMod)) THEN
				WITH r DO
					BEGIN
					right := left + 5;
					IF BITTST(@fRec.flags, ORD(canMod)) THEN
						DrawPicture(lData.lPics.modPic, r);
					left := right + 1;
					END;
			IF BITTST(@showFlags, ORD(crossDelay)) THEN
				BEGIN
				WITH r DO
					BEGIN
					right := left + 8;
					IF BITTST(@fRec.flags, ORD(crossDelay)) THEN
						DrawPicture(lData.lPics.delayPic, r);
					left := right + 1;
					END;
				END;
			IF BITTST(@showFlags, ORD(targeted)) THEN
				BEGIN
				WITH r DO
					BEGIN
					right := left + 8;
					IF BITTST(@fRec.flags, ORD(targeted)) THEN
						DrawPicture(lData.lPics.targetPic, r);
					left := right + 1;
					END;
				END;
			IF BITTST(@showFlags, ORD(graphIt)) THEN
				BEGIN
				WITH r DO
					BEGIN
					right := left + 8;
					IF BITTST(@fRec.flags, ORD(graphIt)) THEN
						DrawPicture(lData.lPics.grafPic, r);
					left := right + 1;
					END;
				END;
			IF BITTST(@showFlags, ORD(gotLinks)) THEN
				BEGIN
				WITH r DO
					BEGIN
					IF cellRec.indent >= 0 THEN
						right := left + 6
					ELSE
						right := left + 9;

					IF BITTST(@fRec.flags, ORD(gotLinks)) THEN
						IF cellRec.indent >= 0 THEN
							DrawPicture(lData.lPics.linkPic, r)
						ELSE
							DrawPicture(lData.lPics.circPic, r);
					left := right + 1;
					END;
				END;
			END; {DoFlags}

		BEGIN
		SaveFonts;
		EraseRect(lRect);
		IF lDataLen <> SizeOf(TFactCellRec) THEN
			EXIT(DrawCell);
		lData := TLDataHdl(lHandle^^.userHandle)^^;
		BlockMove(Ptr(LONGINT(lHandle^^.cells^) + lDataOffset), @cellRec, lDataLen);
		theModel := TModelHdl(lHandle^^.refCon);
		IF FactIDIdx(theModel, cellRec.factID, factNum) THEN
			;
		fRec := theModel^^.facts^^[factNum];
		prec := theModel^^.numPrec;
		showFlags := lData.showFlags;
		r := lRect;
		WITH r DO
			BEGIN
			top := top + 2;
			bottom := top + 9;
			END;
		IF cellRec.indent = 0 THEN
			DoFlags
		ELSE
			BEGIN
			saveFlags := fRec.flags;	{For the initial spacing}
			fRec.flags := 0;
			DoFlags;
			showFlags := lData.indentFlags;
			FOR i := 1 TO (ABS(cellRec.indent) - 1) DO
				BEGIN
				DoFlags;
				r.left := r.left + 10;
				END;
			r.left := r.left + 10;
			fRec.flags := saveFlags;
			DoFlags;
{r.left := r.left + (ABS(cellRec.indent) * 15)}
			END;
		WITH r DO
			BEGIN
			right := left + StringWidth(fRec.name) + 5;
			bottom := lRect.bottom;
			top := lRect.top
			END;
		TextBox(Ptr(LONGINT(@fRec.name) + 1), Length(fRec.name), r, teJustLeft);
		IF cellRec.indent = 0 THEN
			BEGIN
			whichVal := lData.whichVal;
			CASE whichVal OF
			showImpacts: 
				BEGIN
				IF FactIDIdx(theModel, cellRec.linkID, linkNum) THEN
					;
				IF linkNum = 0 THEN
					BEGIN
					IF BITTST(@fRec.flags, ORD(variGrowth)) THEN
						str := 'Vari'
					ELSE
						str := NumToDecStr(prec, theModel^^.crGrowths^^[factNum, factNum]);
					END
				ELSE
					str := NumToDecStr(prec, theModel^^.crGrowths^^[linkNum, factNum]);
				END;
			showAltVal: 
				IF BITTST(@fRec.flags, ORD(canMod)) THEN
					str := NumToDecStr(prec, fRec.altVal)
				ELSE
					whichVal := none;
			showStart: 
				str := NumToDecStr(prec, fRec.startVal);
			showAllTarg: 
				str := NumToDecStr(prec, fRec.targetVal);
			showActTarg: 
				IF BITTST(@fRec.flags, ORD(targeted)) THEN
					str := NumToDecStr(prec, fRec.targetVal)
				ELSE
					whichVal := none;
			END;
			IF whichVal <> none THEN
				BEGIN
				r := lRect;
				r.left := r.right - StringWidth(str) - 5;
				r.right := r.right - 3;
				TextBox(Ptr(LONGINT(@str) + 1), Length(str), r, teJustRight);
				END;
			END;
		IF lSelect THEN
			HiliteCell;
		RestoreFonts;
		END; {DrawCell}

		PROCEDURE CloseList;
		BEGIN
		DisposHandle(lHandle^^.userHandle);
		END; {CloseList}

	BEGIN
	CASE lMessage OF
	lInitMsg: 
		InitList;
	lDrawMsg: 
		DrawCell;
	lHiliteMsg: 
		HiliteCell;
	lCloseMsg: 
		CloseList;
	END;
	END; {GenieList}

END. {GenieList}