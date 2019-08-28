UNIT GrafTypes;

{ Interface file for the GrafPak Libraries }
{ Copyright ©1987 by Mark Stockwell.  All rights reserved }

INTERFACE
{    USES}
{MemTypes, QuickDraw, OSIntf, ToolIntf, PackIntf;}

	CONST
		maxDataPoints = 500;
		maxTicks = 200;
		pAlphaArraySize = maxTicks;

		skip0 = 0;
		skip1 = 1;
		skip2 = 2;
		skip3 = 3;
		skip4 = 4;
		skip5 = 5;
		skip6 = 6;
		skip7 = 7;
		skip8 = 8;
		skip9 = 9;
		skip10 = 10;

{*********************************}
{ GrafPak won't divide by numbers smaller }
{ than this numeric 'zero' }
{*********************************}
		eps = 0.0000000000000000000000000000001;

{*********************************}
{ GrafPak won't try to raise 10 to a power greater than maxPower }
{*********************************}
		maxPower = 35;


	TYPE
		pvector = ARRAY[1..maxDataPoints] OF Real;
		Coords = ARRAY[1..maxDataPoints] OF Integer;
		vectick = ARRAY[1..maxTicks] OF Real;
		pAlpha = ARRAY[1..pAlphaArraySize] OF STRING[20];
		grids = (solid, dot1space1, dot2space2, dot3space1, dot1space3, noGrid);
		pNotate = (regular, scientific);
		x_Plot_Position = (top, bottom, xboth);
		y_Plot_Position = (left, right, yboth);
		plotSymbol = (nothing, box, triangle, circle, cross, filledBox, filledTriangle, filledCircle);
		axesChoice = (xAxis, yAxis);

		pError = RECORD
				what: integer;
				index: integer;
			END;

		pgrids = (psolid, pdot1space1, pdot2space2, pdot3space1, pdot1space3, pnoGrid, pUser);

		FrameRecord = RECORD
				fRect: Rect;
				thickness: integer;
				Color: LongInt;
			END;

		LabelRecord = RECORD
				doLabel: Boolean;
				Text: Str255;
				Font: integer;
				Face: Style;
				Size: integer;
				Color: LongInt;
				Height: integer;
				Width: integer;
				Leading: integer;
				Descent: integer;
				CASE Boolean OF
					True: (
							xPosition: x_Plot_Position
					);
					False: (
							yPosition: y_Plot_Position
					)
			END;

		GridLineRecord = RECORD
				Thickness: integer;
				GridLines: pGrids;
				dots, spaces: integer;
				Color: LongInt;
				Min, Size, Max: integer;
			END;

		TickMarkRecord = RECORD
				Thickness: integer;
				tickMarkLength: integer;
				Color: LongInt;
			END;

		TickLabelRecord = RECORD
				Font: integer;
				Face: Style;
				Size: integer;
				Color: LongInt;
				Height: integer;
				Width: integer;
				Leading: integer;
				firstTick, skip, lastTick: integer;
			END;

		TickRecord = RECORD
				lo, hi, tickInt: Real;
				tickArray: vectick;
				Min, Size, Max: integer;
				numTickMarks: integer;
				doTickMarks: Boolean;
				AutoTicks, logAutoTicks: Boolean;
				labelTickMarks: Boolean;
				lastTickRegardless: Boolean;
				doGrids: Boolean;
				doLogScale: Boolean;
				doAlpha: Boolean;
				Alpha: pAlpha;
				Notate: pNotate;
				places: integer;
				zeroTick: integer;
				firstMajor, majorSkip, lastMajor: integer;
				majorTick: TickMarkRecord;
				minorTick: TickMarkRecord;
				majorGrid: GridLineRecord;
				minorGrid: GridLineRecord;
				tLabel: TickLabelRecord;
				CASE Boolean OF
					True: (
							xPosition: x_Plot_Position
					);
					False: (
							yPosition: y_Plot_Position
					)
			END;

		LogicRecord = RECORD
				doAxes: Boolean;
				doPlotFrame: Boolean;
				doLogMinors: Boolean;
				TicksOnFrame: Boolean;
				PlotOutType: Boolean;
				DoPattern: Boolean;
				DoRCheck: Boolean;
				LogScaleOnYAxis: Boolean;
			END;

		SymRecord = RECORD
				Symbol: plotSymbol;
				Opaque: Boolean;
				Size: integer;
				Color: LongInt;
			END;

		CharRecord = RECORD
				plotChar: Char;
				Size: integer;
				Font: integer;
				Color: LongInt;
			END;

		DataRecord = RECORD
				doSymbol: Boolean;
				SRec: SymRecord;
				CRec: CharRecord;

				connectLines: Boolean;
				lineWidth, lineHeight: integer;
				linePattern: Pattern;
				lineColor: LongInt;
				backColor: LongInt;

				doPattern: Boolean;
				Patrn: Pattern;
				patternColor: LongInt;

				colWidth: integer;
				colPattern: Pattern;
				xyThickness: integer;
			END;

		pQRec = RECORD
				currentPen: PenState;
			END;

		GPRecord = RECORD
				origRect, adjustRect: Rect;
				Frame: FrameRecord;
				xLabel: LabelRecord;
				yLabel: LabelRecord;
				Title: LabelRecord;
				xTick: TickRecord;
				yTick: TickRecord;
				L: LogicRecord;
				D: DataRecord;
				QHold: pQRec;
				pSynch1, pSynch2: LongInt;
				gErr: pError;
			END;

{    VAR}
{    pG: GPRecord;}

IMPLEMENTATION
END.