UNIT GGlobals;

INTERFACE

	USES
		DialogUtils, GFiles, GCommonDec;

	CONST
		cNMenus = 9;
		appleID = 1001; {resource IDs/menu IDs for Apple, File and Edit menus}
		appleM = 1;
		aboutMeCmd = 1; {menu item in apple menu for About sample item}

		fileID = 1002;
		fileM = 2;
		newMCmd = 1;
		openMCmd = 2;
		closeCmd = 3;
		saveCmd = 5;
		saveAsCmd = 6;
		popnCmd = 8;
		quitCmd = 10;

		editID = 1003;
		editM = 3;
		undoCmd = 1;
		cutCmd = 3;
		copyCmd = 4;
		pasteCmd = 5;
		clearCmd = 6;
		selAllCmd = 8;

		modelMenuID = 1004;
		modelM = 4;
		newFactCmd = 1;
		removeFactCmd = 2;
		resetFactCmd = 4;
		setLinksCmd = 5;
		factAttribCmd = 6;
		modelPrefsCmd = 8;
		startRunCmd = 10;

		windMenuID = 1005;
		windM = 5;

		graphMenuID = 1007;
		graphM = 6;
		rescaleCmd = 1;
		autoScaleCmd = 2;

		popnMenuID = 1008;
		popnM = 7;
		resetPopnCmd = 1;
		popnParmsCmd = 3;
		showGeneCmd = 5;
		goSearchCmd = 7;

{Pop ups}
		showValMenuID = 9;
		showValM = 8;
		selfImpCmd = 1;
		srchValCmd = 2;
		startValCmd = 3;
		actTargCmd = 4;
		allTargCmd = 5;

		popnSelMenuID = 128;
		popnSelM = 9;
		newPopnWindCmd = 1;
		renamePopnWindCmd = 2;
		delPopnWindCmd = 3;

		modelFTypeMenu = 127;
		graphFTypeMenu = 126;

		modelMBarID = 128;
		graphMBarID = 130;
		popnMBarID = 131;

		cModPicID = 1000;
		cLinkPicID = 1001;
		cDelayPicID = 1002;
		cTargetPicID = 1003;
		cCircPicID = 1004;
		cGrafPicID = 1005;

		popnWindIcon1ID = 128;
		popnWindIcon2ID = 129;

		modelWID = 128;
		graphWID = 129;
		browseWID = 130;
		waitWID = 131;
		popnWID = 132;

		cPopnHalfHeight = 132;
		cPopnFullHeight = 290;

		cGListProcID = 128;
		cGraphList = 129;
		cShockListID = 130;
		cRowListID = 131;

		popnWindResType = 'GSrW';

		cBullsEyeID = -5760;
		cBBall1 = 25001;

		IOErrID = 32177; {resource ID for I/O Error alerts}

		cMinUpdateTime = 6;
		downArrow = CHR($1F);
		upArrow = CHR($1E);


	VAR
		theMenus: ARRAY[1..cNMenus] OF MenuHandle;
		wdList, waitWD: WDHandle;
		iBeamHdl, watchHdl, crossHdl, bullsEyeHdl: CursHandle;
		bBalls: ARRAY[1..4] OF CursHandle;
		bBallState: INTEGER;
		quitGenie: BOOLEAN;
		dragRect, growRect: Rect;
		listPics: TListPics;
		gotMemErr: BOOLEAN;
		debugOn, userBreak: BOOLEAN;
		nextGrafID: INTEGER;
		modelMBar, graphMBar, popnMBar: Handle;
		rndSeed: EXTENDED;
		lastUpdate: LONGINT;
		popnWindIcon1, popnWindIcon2: Handle;
		crCh, tabCh: STRING[1];


IMPLEMENTATION

END.