PROGRAM Genie;

	USES
		DialogUtils, GFiles, GCommonDec, GGlobals, GControl, GDriver;


	PROCEDURE InitGenie;

		PROCEDURE InitVars;
		BEGIN
		quitGenie := FALSE;
		WITH screenBits.bounds DO
			BEGIN
			SetRect(dragRect, 4, 24, right - 4, bottom - 4); {ensure at least 4 by 4 pixels will remain visible}
			SetRect(growRect, 105, 70, right, bottom - 41); { (min (hor,vert), max (hor,vert))}
			END;
		iBeamHdl := GetCursor(iBeamCursor);
		watchHdl := GetCursor(watchCursor);
		crossHdl := GetCursor(crossCursor);
		bullsEyeHdl := GetCursor(cBullsEyeID);
		bBalls[1] := GetCursor(cBBall1);
		bBalls[2] := GetCursor(cBBall1 + 1);
		bBalls[3] := GetCursor(cBBall1 + 2);
		bBalls[4] := GetCursor(cBBall1 + 3);
		bBallState := 1;
		listPics.modPic := GetPicture(cModPicID);
		listPics.linkPic := GetPicture(cLinkPicID);
		listPics.delayPic := GetPicture(cDelayPicID);
		listPics.targetPic := GetPicture(cTargetPicID);
		listPics.circPic := GetPicture(cCircPicID);
		listPics.grafPic := GetPicture(cGrafPicID);
		popnWindIcon1 := GetIcon(popnWindIcon1ID);
		popnWindIcon2 := GetIcon(popnWindIcon2ID);
		wdList := NIL;
		gotMemErr := FALSE;
		debugOn := TRUE;
		userBreak := FALSE;
		nextGrafID := 0;
		rndSeed := TickCount;
		lastUpdate := TickCount;
		crCh[0] := CHR(1);
		crCh[1] := CHR($0D);
		tabCh[0] := CHR(1);
		tabCh[1] := CHR($09);
		END; {InitVars}


		PROCEDURE InitTheMenus;
		BEGIN
		{Read menu descriptions from resource file into memory and store handles in myMenus array}
		ClearMenuBar;

		theMenus[appleM] := GetMenu(appleID); {read Apple menu from resource file}
		AddResMenu(theMenus[appleM], 'DRVR'); {add desk accessory names to Apple menu}
		theMenus[fileM] := GetMenu(fileID);
		theMenus[editM] := GetMenu(editID);
		theMenus[modelM] := GetMenu(modelMenuID);
		theMenus[windM] := GetMenu(windMenuID);
		theMenus[graphM] := GetMenu(graphMenuID);
		theMenus[popnM] := GetMenu(popnMenuID);
		theMenus[popnSelM] := GetMenu(popnSelMenuID);
		theMenus[showValM] := GetMenu(showValMenuID);

		modelMBar := GetNewMBar(modelMBarID);
		graphMBar := GetNewMBar(graphMBarID);
		popnMBar := GetNewMBar(popnMBarID);
		ChangeMenuBar(modelMBar);
		DrawMenuBar;
		END;

	BEGIN
	{ Initialization }
	MaxApplZone;
	MoreMasters;
	MoreMasters;
	MoreMasters;
	MoreMasters;
	InitGraf(@thePort);
	InitFonts;
	InitWindows;
	InitMenus;
	TEInit;
	InitDialogs(NIL);

	FlushEvents(everyEvent, 0);
	InitCursor;

	SetEventMask(everyEvent);
	InitVars;
	InitTheMenus;
	UpdateMenus(NIL);

	NewGWindow('Simulation', waitWind);
	waitWD := wdList;
	HideWindow(waitWD^^.thisWindow);
	END; {InitGenie}

	PROCEDURE CloseDown;
		VAR
			theWD: WDHandle;
			i: INTEGER;
	BEGIN
	theWD := wdList;
	WHILE (theWD^^.windType <> modelWind) AND (theWD <> NIL) DO
		theWD := theWD^^.next;
	IF theWD <> NIL THEN
		quitGenie := CloseWindow(theWD^^.thisWindow, TRUE);	{Close model window}
	IF quitGenie THEN
		BEGIN
		IF CloseWindow(waitWD^^.thisWindow, FALSE) THEN
			;
		ClearMenuBar;
		FOR i := 1 TO cNMenus DO
			ReleaseResource(HANDLE(theMenus[i]));
		DisposHandle(modelMBar);
		Disposhandle(graphMBar);
		DisposHandle(Handle(bullsEyeHdl));
		FOR i := 1 TO 4 DO
			DisposHandle(Handle(bBalls[i]));
		ReleaseResource(popnWindIcon1);
		ReleaseResource(popnWindIcon2);
		END;
	END; {CloseDown}



BEGIN
InitGenie;
REPEAT
	RunGenie(FALSE);
	IF quitGenie THEN
		CloseDown;
UNTIL quitGenie;
{DumpProfileToFile('Genie.Profile');}
END. {Genie}