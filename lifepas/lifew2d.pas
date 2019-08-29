{$R-,I-,S-,G+}
{$DEFINE ASM}
{ $ DEFINE COUNT}
{ $ DEFINE TEST}
{ $ DEFINE WORD_UPDATE}

{$M $2000, $11000, $14000}
Program LifeW2D;

Uses
  HR_Timer,
  RandLFSR,
  KeyBoard;

CONST
  Guard = 4;
  MAXSIZE = 200 * 160;

Procedure Init;
BEGIN
{$IFNDEF TEST}
  ASM
    mov ax,13h
    int 10h		{ Mode 13h graphics! }
  END;
{$ENDIF}
END;

Procedure CleanUp;
BEGIN
  ASM
    mov ax,3h
    int 10h
  END;
END;

CONST
  MAX_X = 320;
  MAX_Y = 200;
  SIZE_X = 200;
  SIZE_Y = 200;

  CRT_SEG = $A000;
  CRT_FILL = 0;
  CRT_LEN = MAX_X * MAX_Y DIV 2;
  CRT_INC = 320;
  CRT_MUL = 1;

  OffColor   = 0;
  OnColor    = 15;
  ChOnColor  = 12;

  DEFAULT_GENERATIONS = 1000;

Procedure ClearScreen;
Assembler;
ASM
  cld
  mov ax,CRT_SEG
  mov es,ax
  xor di,di
  mov cx,CRT_LEN
  mov ax,CRT_FILL
  rep stosw
END;

Procedure MoveD(VAR fra, til; len : Word);
Assembler;
ASM
  cld
  push ds
  lds si,[fra]
  les di,[til]
  mov cx,[len]
  db 66h; rep movsw
  pop ds
END;

Procedure PutPixel(x, y, color : Word);
BEGIN
  Mem[CRT_SEG:y * CRT_INC + x * CRT_MUL] := color;
END;

TYPE
  CellArr = ARRAY [0..$FFF8] OF BYTE;
  pCell   = ^CellArr;
  CellMap = Object
    cp, source, save : pCell;
    offs, middle_words, xsize, ysize, ystep, size : Word;
    wrap : Boolean;

    Constructor Init(ix, iy : Word; iwrap : Boolean);
    Destructor Done;
    Procedure ClearMap;
    Function  GetC(x, y : Integer): Word;
    Function  GetB(x, y : Integer): Word;
    Function  SGetC(x, y : Integer): Word;
    Procedure IncC(x, y, v : Integer);
    Procedure SetC(x, y, v : Integer);
    Procedure SetB(x, y, v : Integer);
    Procedure NextGen;
    Procedure SetCounts;
    Procedure RandomMap(seed : LongInt);
    Function  Validate : Boolean;
    Procedure Dump(c : pCell; VAR f : TEXT);
  END;

VAR
  DestMap : ARRAY [1..MAXSIZE + Guard + 15] OF BYTE;

VAR
  Middle, Edge, Color : ARRAY [BYTE] OF LongInt;

Procedure DoLineA(VAR sr; S2G, len : WORD); External;
Procedure InitStep(above, below : WORD); External;
{$L dolineD.obj}

Constructor CellMap.Init(ix, iy : Word; iwrap : Boolean);
BEGIN
  IF ix > MAX_X THEN ix := MAX_X;
  IF ix < 8 THEN ix := 8;
  IF iy > MAX_Y THEN iy := MAX_Y;
  IF iy < 2 THEN iy := 2;

  wrap := iwrap;

  REPEAT
    xsize := ix AND $FFFC;
    ysize := iy;
    IF wrap THEN BEGIN
      ystep := xsize Shr 1;
      size  := ystep * iy;
      offs  := 0;
    END
    ELSE BEGIN
      ystep := xsize Shr 1 + 4;
      size  := ystep * (iy+2);
      offs  := ystep + 2;
    END;
    IF size+3 > SizeOf(DestMap) THEN
      Dec(ix,4);
  UNTIL size+3 <= SizeOf(DestMap);

  GetMem(source,$FFF0); save := source;
  cp := Ptr(DSeg,Ofs(DestMap) + 3 AND $FFFC);	{ DWORD align cell-map }

  IF Word(source) > Ofs(DestMap) THEN
    Word(cp) := Word(source)
  ELSE
    Word(source) := Ofs(DestMap);

  ClearMap;
  InitStep(-ystep,ystep);
END;

Destructor CellMap.Done;
BEGIN
  FreeMem(save,$FFF0);
END;

Procedure CellMap.ClearMap;
BEGIN
  FillChar(cp^,size,#0);
END;

Function CellMap.GetC(x, y : Integer): Word;
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  y := cp^[y * ystep + x Shr 1 + offs];
  IF Odd(x) THEN y := y Shr 4;
  GetC := y AND 15;
END;

Function CellMap.GetB(x, y : Integer): Word;
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  GetB := cp^[y * ystep + x Shr 1 + offs];
END;

Function CellMap.SGetC(x, y : Integer): Word;
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  y := source^[y * ystep + x Shr 1 + offs];
  IF Odd(x) THEN y := y Shr 4;
  SGetC := y AND 15;
END;

Procedure CellMap.IncC(x, y, v : Integer);
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  IF Odd(x) THEN v := v Shl 4;
  Inc(cp^[y * ystep + x Shr 1 + offs],v);
END;

Procedure CellMap.SetC(x, y, v : Integer);
VAR
 pb : ^BYTE;
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  pb := @cp^[y * ystep + x Shr 1 + offs];
  IF Odd(x) THEN
    pb^ := pb^ AND 15 + v Shl 4
  ELSE
    pb^ := pb^ AND $F0 + v;
END;

Procedure CellMap.SetB(x, y, v : Integer);
VAR
 pb : ^BYTE;
BEGIN
  WHILE y >= ysize DO Dec(y,ysize);
  WHILE y < 0 DO Inc(y,ysize);
  WHILE x >= xsize DO Dec(x,xsize);
  WHILE x < 0 DO Inc(x,xsize);

  cp^[y * ystep + x Shr 1 + offs] := v;
END;

VAR
  ErrX, ErrY : Integer;

Function CellMap.Validate : Boolean;
VAR
  x, y, n1, n2, l, a, r : Integer;
BEGIN
  Validate := FALSE;
  FOR y := 0 TO ysize-1 DO BEGIN
    x := 0;
    REPEAT
      n1 := GetC(x,y);
      n2 := GetC(x+1,y);
      l := GetC(x-1,y-1) Shr 3 +
	   GetC(x-1,y)   Shr 3 +
	   GetC(x-1,y+1) Shr 3 ;
      a := GetC(x,y-1) Shr 3 + GetC(x+1,y-1) Shr 3 +
	   GetC(x,y+1) Shr 3 + GetC(x+1,y+1) Shr 3 ;
      r := GetC(x+2,y-1) Shr 3 +
	   GetC(x+2,y)   Shr 3 +
	   GetC(x+2,y+1) Shr 3 ;

      IF n1 AND 7 <> l + a THEN BEGIN
	ErrX := x; ErrY := y;
	Exit;
      END;
      IF n2 AND 7 <> a + r THEN BEGIN
	ErrX := x+1; ErrY := y;
	Exit;
      END;

      Inc(x,2);
    UNTIL x >= xsize;
  END;
  Validate := TRUE;
END;

Procedure CellMap.SetCounts;
VAR
  x, y, n1, n2, l, a, r : Integer;
  ad, ys, xs, step, a1, a2 : Word;
  c : pCell;
BEGIN
  ys := ysize; xs := xsize; c := cp; step := ystep;

  FOR y := 0 TO ys-1 DO BEGIN
    x := 0;
    REPEAT
      IF (x = 0) OR (x >= xs-2) OR (y = 0) OR (y >= ys-1) THEN BEGIN
	n1 := GetC(x,y);
	n2 := GetC(x+1,y);
	l := GetC(x-1,y-1) Shr 3 +
	     GetC(x-1,y)   Shr 3 +
	     GetC(x-1,y+1) Shr 3 ;
	a := GetC(x,y-1) Shr 3 + GetC(x+1,y-1) Shr 3 +
	     GetC(x,y+1) Shr 3 + GetC(x+1,y+1) Shr 3 ;
	r := GetC(x+2,y-1) Shr 3 +
	     GetC(x+2,y)   Shr 3 +
	     GetC(x+2,y+1) Shr 3 ;

	SetC(x,y,  l + a + n1 AND 8);
	SetC(x+1,y,a + r + n2 AND 8);
      END
      ELSE BEGIN
	ad := y*step + x Shr 1 + offs;

	l := c^[ad-1-step] Shr 7 + c^[ad-1] Shr 7 + c^[ad-1+step] Shr 7;
{$IFDEF TEST}
	a1:= GetC(x-1,y-1) Shr 3 +
	     GetC(x-1,y)   Shr 3 +
	     GetC(x-1,y+1) Shr 3 ;
	IF l <> a1 THEN BEGIN
	  CleanUp;
	  WriteLn('Error in l-SetCount!');
	  Halt(1);
	END;
{$ENDIF}

	a1 := c^[ad-step] Shr 3; a2 := c^[ad+step] Shr 3;
	a := a1 AND 1 + a2 AND 1 + a1 Shr 4 + a2 Shr 4;
{$IFDEF TEST}
	a1:= GetC(x,y-1) Shr 3 + GetC(x+1,y-1) Shr 3 +
	     GetC(x,y+1) Shr 3 + GetC(x+1,y+1) Shr 3 ;
	IF a <> a1 THEN BEGIN
	  CleanUp;
	  WriteLn('Error in a-SetCount!');
	  Halt(1);
	END;
{$ENDIF}

	r := (c^[ad+1-step] Shr 3) AND 1 +
	     (c^[ad+1] Shr 3) AND 1 +
	     (c^[ad+1+step] Shr 3) AND 1;
{$IFDEF TEST}
	a1 := GetC(x+2,y-1) Shr 3 +
	     GetC(x+2,y)   Shr 3 +
	     GetC(x+2,y+1) Shr 3 ;
	IF r <> a1 THEN BEGIN
	  CleanUp;
	  WriteLn('Error in r-SetCount!');
	  Halt(1);
	END;
{$ENDIF}

	c^[ad] := c^[ad] AND $88 + (l + a) + (a + r) Shl 4;
      END;
      Inc(x,2);
    UNTIL x >= xsize;
  END;
END;

Procedure CellMap.Dump(c : pCell; VAR f : TEXT);
CONST
  HexCh : ARRAY [0..15] OF CHAR = '0123456789ABCDEF';
VAR
  a, n, x, y : Integer;
BEGIN
  a := 0;
  FOR y := 0 TO ysize - 1 DO BEGIN
    x := 0;
    REPEAT
      n := c^[a]; Inc(a);
      Write(f,HexCh[n AND 15]:2,HexCh[n Shr 4]:2);
      Inc(x,2);
    UNTIL x >= xsize;
    WriteLn(f);
  END;
  WriteLn(f);
END;

VAR
  f : TEXT;

VAR
  NewCell : Word;

Procedure DoEdge(n, a, l, r, above, below : Word)
{$IFDEF StonyBrook} [Alters(ax,bx,dx)] {$ENDIF};
Assembler;
ASM
  push cx; push si; push di
  mov bx,[n]
  shl bx,2
  db $66; mov cx,word ptr Middle[bx]
  db $66; mov dx,word ptr Edge[bx]

  mov bx,[a]
  mov si,[l]
  mov di,[r]

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,cx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah

  add bx,[above]

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,dx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah

  add bx,[below]

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,dx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah
  pop di; pop si; pop cx
END;

Procedure DoVertLine(VAR sr; caddr, len, ystep, left, right : Word);
Assembler;
ASM
  les bx,[sr]
  mov cx,[len]
  mov si,CRT_SEG

  CLI

  db $8E,$EE	{ mov gs,si }
  mov si,[NewCell]
  db $8E,$E6	{ mov fs,si }
  mov di,[caddr]
  
  sub bx,[ystep]
  sub di,CRT_INC
   jmp @loop

@neardone:
   jmp @done

@loop:
  mov ax,[ystep]
   jcxz @neardone
@l0:
  add bx,ax
  add di,CRT_INC
  mov si,[es:bx]
  or si,si
   loopz @l0
   jz @neardone

  db $64; mov al,[si]	{al := Mem[NewCell:si]}
  and ax,255
   jz @loop

  mov si,ax
  shl si,2

  push cx; push di

  db $66; mov ax,word ptr Color[si]
  db $65; db $66; mov [di],ax       { mov [gs:si],eax }

  db $66; mov cx,word ptr Middle[si]
  db $66; mov dx,word ptr Edge[si]

  mov si,[left]
  mov di,[right]

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,cx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah

  mov cx,[ystep]
  sub bx,cx

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,dx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah

  add bx,cx
  add bx,cx

  mov ah,[bx+di]
  db $66; shl ax,8
  mov ax,[bx]
  db $66; shl ax,8
  mov al,[bx+si]
  db $66; add ax,dx
  mov [bx+si],al
  db $66; shr ax,8
  mov [bx],ax
  db $66; shr ax,8
  mov [bx+di],ah
  sub bx,cx		{ Restore proper address! }
  
  pop di; pop cx

   jmp @loop
@done:

  STI

END;

Procedure DoEdgeLine(VAR sr; S2D, S2G, len, above, below : Word)
{$IFDEF StonyBrook} [Alters(ax,bx,dx)] {$ENDIF};
Assembler;
ASM
  push cx; push si; push di; push es
  cld
  les di,[sr]
  mov cx,[len]

  CLI

  mov si,CRT_SEG
  db $8E,$EE	{ mov gs,si }
  mov si,[NewCell]
  db $8E,$E6	{ mov fs,si }

@loop:
  xor ax,ax
@l0:
  repe scasw
   je @done
  mov bx,[es:di-2]
  db $64; mov bl,[bx]
  and bx,255
   jz @l0

  shl bx,2
  db $66; mov ax,word ptr Color[bx] { mov eax,Color[bx] }
  mov si,[S2G]
  add si,di
  add si,si
  db $65; db $66; mov [si],ax       { mov [gs:si],eax }

  mov si,[s2D]
  add si,di
  db $66; mov ax, word ptr Middle[bx]
  db $66; mov dx, word ptr Edge[bx]
  mov bx,[above]
  db $66; add [si],ax
  db $66; add [si+bx],dx
  add si,[below]
  db $66; add [si],dx
   jmp @loop
@done:
  pop es; pop di; pop si; pop cx

  STI

END;

{$IFDEF COUNT}
CONST
  ChangeC : LongInt = 0;
  ScanC   : LongInt = 0;

VAR
  CountTable : ARRAY [BYTE] OF LongInt;
{$ENDIF}

Procedure DoLine(VAR sr; S2D, S2G, len, ystep : Word)
{$IFDEF StonyBrook} [Alters(ax,bx,dx,es)] {$ENDIF};
Assembler;
ASM
  push cx; push si; push di; push es
  cld
  les di,[sr]
  mov bx,[len]
  lea cx,[bx+1]
  add bx,bx
  mov word ptr es:[bx+di],$FFFF  { Edge, already processed! }

  CLI

  mov si,CRT_SEG
  db $8E,$EE	{ mov gs,si }
  mov si,[NewCell]
  db $8E,$E6	{ mov fs,si }

@loop:
  xor ax,ax
  xor bx,bx
@l0:
  repe scasw
{$IFDEF COUNT}
  db $66; inc word ptr ScanC
{$ENDIF}
  mov si,es:[di-2]
  db $64; mov bl,[si]
  and bx,255
   jz @l0

  test cx,cx
   jz @done

  shl bx,2

{$IFDEF COUNT}
  db $66; inc word ptr ChangeC
  db $66; inc word ptr CountTable[bx]
{$ENDIF}

  mov si,[S2G]
  add si,di
  add si,si
  db $66; mov ax,word ptr Color[bx] { mov eax,Color[bx] }
  db $65; db $66; mov [si],ax       { mov [gs:si],eax }

  mov si,[S2D]
  add si,di
  db $66; mov ax, word ptr Middle[bx]
  db $66; mov dx, word ptr Edge[bx]
  mov bx,[ystep]
  db $66; add [si],ax
  db $66; add [si+bx],dx
  sub si,bx
  db $66; add [si],dx

  xor ax,ax
  repe scasw
{$IFDEF COUNT}
  db $66; inc word ptr ScanC
{$ENDIF}
  mov si,[es:di-2]
  db $64; mov bl,[si]
  or bx,bx
   jz @l0

  test cx,cx
   jz @done

  shl bx,2

{$IFDEF COUNT}
  db $66; inc word ptr ChangeC
  db $66; inc word ptr CountTable[bx]
{$ENDIF}

  mov si,[S2G]
  add si,di
  add si,si
  db $66; mov ax,word ptr Color[bx] { mov eax,Color[bx] }
  db $65; db $66; mov [si],ax       { mov [gs:si],eax }

  mov si,[S2D]
  add si,di
  db $66; mov ax, word ptr Middle[bx]
  db $66; mov dx, word ptr Edge[bx]
  mov bx,[ystep]
  db $66; add [si],ax
  db $66; add [si+bx],dx
  sub si,bx
  db $66; add [si],dx

   jmp @loop

@done:
  pop es; pop di; pop si; pop cx

  STI
END;

Procedure CellMap.NextGen;
VAR
  x, y, n1, n2, l, a, r, c1, c2, i1, i2 : Integer;
  pl : ^LongInt;
  m, e, m1, e1 : LongInt;
  n, c, w, addr, caddr, len, S2D, S2G, S2Ginc : Word;
  ysiz, xsiz, yste, siz : WORD;
  pw : ^Word;
BEGIN
  ysiz := ysize; xsiz := xsize; yste := ystep; siz := size;
{$IFDEF TEST}
  Assign(f,'DUMP.LIF'); ReWrite(f);
  Dump(cp,f);
{$ENDIF}

  MoveD(cp^,source^,(size+3) Shr 2);

  IF NOT wrap THEN BEGIN
    pw := @source^[offs];
    caddr := 0;
    S2D := Word(source) - Word(cp) - 3;
    S2G := caddr Shr 1 - Word(pw) - 2;
    S2Ginc := CRT_INC Shr 1 - yste;
    len := xsiz Shr 2;        { Nr of words (4-cells) /line }

    FOR y := 0 TO ysiz-1 DO BEGIN
      DoLine(pw^, S2D, S2G, len, yste);

      Inc(Word(pw),yste);
      Inc(S2G,S2Ginc);
    END;
    Exit;
  END;

{ Process left edge first: }

 { Start with upper left corner }
  pw := @source^[0];
  caddr := 0;
  n := Mem[NewCell:pw^];
  IF n <> 0 THEN BEGIN
    DoEdge(n,Word(pw),yste-1,2,siz-yste,yste*2-siz);
    MemL[CRT_SEG:caddr] := Color[n];
  END;
  Inc(Word(pw),yste);
  Inc(caddr,CRT_INC);

 { Then do the edge }

  DoVertLine(pw^,caddr,ysiz-2,yste,yste-1,2);
  Inc(Word(pw),yste * (ysiz-2));
  Inc(caddr,CRT_INC * (ysiz-2));

 { and finally the lower left corner }
  n := Mem[NewCell:pw^];
  IF n <> 0 THEN BEGIN
    DoEdge(n,Word(pw),yste-1,2,-yste,yste*2-siz);
    MemL[CRT_SEG:caddr] := Color[n];
  END;


{ Then the right-hand side: }
  x := xsiz - 4;

 { Upper right corner }
  pw := @source^[x Shr 1];
  caddr := x;
  n := Mem[NewCell:pw^];
  IF n <> 0 THEN BEGIN
    DoEdge(n,Word(pw),$FFFF,2-yste,siz-yste,yste*2-siz);
    MemL[CRT_SEG:caddr] := Color[n];
  END;
  Inc(Word(pw),yste);
  Inc(caddr,CRT_INC);

 { Right side }
  DoVertLine(pw^,caddr,ysiz-2,yste,$FFFF,2-yste);
  Inc(Word(pw),yste * (ysiz-2));
  Inc(caddr,CRT_INC * (ysiz-2));

 { and lower right corner }
  n := Mem[NewCell:pw^];
  IF n <> 0 THEN BEGIN
    DoEdge(n,Word(pw),$FFFF,2-yste,-yste,yste*2-siz);
    MemL[CRT_SEG:caddr] := Color[n];
  END;

{$IFDEF TEST}
  Dump(cp,f);
{$ENDIF}

{ Main part of cell array: top to bottom }

  IF xsiz > 8 THEN BEGIN
    caddr := 4;
    pw := @source^[2];
    S2D := Word(source) - Word(cp) - 3;
    S2G := caddr Shr 1 - Word(pw) - 2;
    S2Ginc := CRT_INC Shr 1 - yste;
    len := (xsiz - 8) Shr 2;        { Nr of words (4-cells) /line }

  { Top line }
    DoEdgeLine(pw^,S2D, S2G, len, siz-yste, yste);
    Inc(Word(pw),yste);
    Inc(S2G,S2Ginc);

  { Inner part, no edge processing for max speed }
    FOR y := 1 TO ysiz-2 DO BEGIN
      DoLine(pw^, S2D, S2G, len, yste);
{      DoLineA(pw^, S2G, len); }

      Inc(Word(pw),yste);
      Inc(S2G,S2Ginc);
    END;

  { Bottom line }
    DoEdgeLine(pw^,S2D, S2G, len, -yste, yste-siz);
  END;

{$IFDEF TEST}
  Dump(cp,f);
  Close(f);
{$ENDIF}
END;

(*
Procedure CellMap.SetCell(x, y : Integer; alive : Boolean);
VAR
  a : Boolean;
  e, m : Integer;
BEGIN
  a := GetC(x,y) >= 8;
  IF a <> alive THEN BEGIN
    m := 8; e := 1;
    IF a THEN BEGIN
      m := -8; e := -1;
    END;
    IncC(x-1,y-1,e);
    IncC(x,y-1,e);
    IncC(x+1,y-1,e);
    IF Odd(x) THEN
      IncC(x+1,y,e)
    ELSE
      IncC(x-1,y,e);
    IncC(x-1,y-1,e);
    IncC(x,y-1,e);
    IncC(x+1,y-1,e);

    IncC(x,y,m);
  END;
END;
*)

Function Get64K: Word;
Assembler;
ASM
  mov ah,48h
  mov bx,1000h
  mov dx,bx
  int 21h
  cmc
  sbb bx,bx
  and ax,bx
END;

CONST
  sx : WORD = SIZE_X;
  sy : Word = SIZE_Y;
  generations : LongInt = DEFAULT_GENERATIONS;
  wrap : Boolean = TRUE;
  SingleRepeat : Boolean = TRUE;
  HaltAtEnd : Boolean = FALSE;
  ChangeColor : Boolean = FALSE;

Procedure InitTables;
CONST
  Alive : ARRAY [0..17] OF BYTE = (
   0,0,0,$11,0,0,0,0,0, $10,$10,1,1,$10,$10,$10,$10,$10);
  Mi : ARRAY [1..4] OF LongInt = (
   $0810, $18000, $081000, $1800000);
  Ed : ARRAY [1..4] OF LongInt = (
   $1110, $11100, $111000, $1110000);
VAR
  i : Word;
  h, l, t, ht : Word;
  o, ChOn, ChOff : Word;
  NewTab : ARRAY [BYTE] OF BYTE;
  m, e, c : LongInt;
  ec : ARRAY [1..4] OF BYTE ABSOLUTE c;
BEGIN
  NewCell := Get64K;
  IF NewCell = 0 THEN BEGIN
    WriteLn('DOS RAM Error!');
    Halt(1);
  END;

  i := 0;
  FOR h := 0 TO 15 DO BEGIN
    t := h Shr 3;
    ht := Alive[h + t] * 2;
    FOR l := 0 TO 7 DO BEGIN
      NewTab[i] := Alive[l + t] + ht;
      Inc(i);
    END;
    Inc(t);
    ht := Alive[h + t] * 2;
    FOR l := 8 TO 15 DO BEGIN
      NewTab[i] := Alive[l + t] + ht;
      Inc(i);
    END;
  END;

  i := 0;
  FOR h := 0 TO 255 DO BEGIN
    ht := NewTab[h] * 4;
    FOR l := 0 TO 255 DO BEGIN
      o := NewTab[l] + ht;
      IF NOT ChangeColor AND (o < $10) THEN
	o := 0;		{ No Change! }
      Mem[NewCell:i] := o;
      Inc(i);
    END;
  END;

  ChOn  := OnColor;
  IF ChangeColor THEN BEGIN
    ChOn  := ChOnColor;
  END;

  FOR i := 0 TO 255 DO BEGIN
    e := 0; m := 0;
    l := i;
    FOR h := 4 DOWNTO 1 DO BEGIN
      CASE l AND $88 OF
	$00 : ec[h] := OffColor;
	$08 : ec[h] := OnColor;
	$80 : BEGIN
	  Dec(e,Ed[h]);
	  Dec(m,Mi[h]);
	  ec[h] := OffColor;
	END;
	$88 : BEGIN
	  Inc(e,Ed[h]);
	  Inc(m,Mi[h]);
	  ec[h] := ChOn;
	END;
      END;
      Inc(l,l);
    END;
    Middle[i] := m; Edge[i] := e; Color[i] := c;
  END;
END;

Procedure CellMap.RandomMap(seed : LongInt);
VAR
  x, y, r : Word;
  count : Word;
BEGIN
  IF seed <> 0 THEN
    Randomize(seed);

  ClearMap;
  ClearScreen;

  Count := xsize * ysize Shr 1;
  REPEAT
    x := Random(xsize);
    y := Random(ysize);

    SetC(x,y,8);
    PutPixel(x,y,OnColor);

    Dec(count);
  UNTIL Count = 0;

  SetCounts;			{ Count neighbours }
END;

Procedure Syntax;
BEGIN
  WriteLn('LIFE V3.0W (C) 1992 Terje Mathisen');
  WriteLn('Usage: LIFE [generations [width [height]]] | [option..]');
  WriteLn;
  WriteLn('Options:');
  WriteLn(' /NoWrap  | Do not wrap edges.');
  WriteLn(' /Repeat  | Repeat forever (Stop with Esc).');
  WriteLn(' /Step    | Single-step generations.');
  WriteLn(' /Limit   | Limit speed to 18.2 gps.');
  WriteLn(' /Wait    | Wait for a keypress before exiting.');
  WriteLn(' /Change  | Use different color for changing cells.');
  WriteLn;
  WriteLn('Defaults:');
  WriteLn('  ',DEFAULT_GENERATIONS,' generations');
  WriteLn('  ',SIZE_X,'x',SIZE_Y,' array');
  WriteLn('  Wrap edges');
  WriteLn('  Do a single run at max speed.');
  WriteLn;
  WriteLn('Generations = 0 means unlimited generations.');
  WriteLn('When running, ScrollLock toggles single-step mode and');
  WriteLn(' CapsLock toggles the speed limit.');
  Halt(1);
END;

VAR
  cell : CellMap;

  gen, ticks : Word;
  done : BOOLEAN;
  SHFlag : BYTE;

  HaltByte : BYTE {$IFDEF StonyBrook} VOLATILE {$ENDIF} ABSOLUTE $40:$17;
  Ticker   : WORD {$IFDEF StonyBrook} VOLATILE {$ENDIF} ABSOLUTE $40:$6C;

Procedure Parse;
VAR
  feil : Integer;
  num : LongInt;
  p : String;
  i, j, k : Integer;
BEGIN
  SHFlag := HaltByte AND $50;  { ScrollLock AND CapsLock }

  j := 1;
  FOR i := 1 TO ParamCount DO BEGIN
    p := ParamStr(i);
    FOR k := 1 TO Length(p) DO p[k] := UpCase(p[k]);
    IF (p[1] = '-') OR (p[1] = '/') THEN BEGIN
      Delete(p,1,1);
      IF Length(p) = 0 THEN Syntax;
      IF      Pos(p,'NOWRAP') = 1 THEN wrap := FALSE
      ELSE IF Pos(p,'REPEAT') = 1 THEN SingleRepeat := FALSE
      ELSE IF Pos(p,'LIMIT') = 1 THEN SHFlag := SHFlag XOR $40
      ELSE IF Pos(p,'STEP') = 1 THEN SHFlag := SHFlag XOR $10
      ELSE IF Pos(p,'WAIT') = 1 THEN HaltAtEnd := TRUE
      ELSE IF Pos(p,'CHANGE') = 1 THEN ChangeColor := TRUE
      ELSE
	Syntax;
    END
    ELSE BEGIN
      Val(p,num,feil);
      IF (feil <> 0) OR (num < 0) OR (num >= $10000) THEN Syntax;
      CASE j OF
	1 : IF num = 0 THEN
	      generations := $10000
	    ELSE
	      generations := num;
	2 : sx := num;
	3 : sy := num;
	ELSE
	  Syntax;
      END;
      Inc(j);
    END;
  END;
END;

FUNCTION GetCPU: WORD;
Assembler;
ASM
  pushf; xor ax,ax; push sp; pop bx; cmp bx,sp; jne @done { 8088! }
  inc ax;mov bh,$F0;push bx;popf;pushf;pop bx;and bh,$F0; jz @done{ 286! }
  inc ax						  { 386+ }
@done: popf
END;

Procedure Error(msg : String);
BEGIN
  CleanUp;
  WriteLn(msg);
  WriteLn('ErrX = ',ErrX,', ErrY = ',ErrY);
  Halt(1);
END;

VAR
  t : Word;

{$IFDEF COUNT}
CONST
  OldScan   : LongInt = 0;
  OldChange : LongInt = 0;

VAR
  cf : TEXT;
  g, rem : Word;

{$ENDIF}

VAR
  ti : LongInt;

BEGIN
{$IFDEF COUNT}
  FillChar(CountTable,SizeOf(CountTable),#0);
  Assign(cf,'LIFE.CNT'); ReWrite(cf);
{$ENDIF}

  IF GetCPU < 2 THEN BEGIN
    WriteLn('This program requires at least a 386 CPU!');
    Halt(1);
  END;

  Parse;

  InitTables;

  cell.Init(sx,sy,wrap);
  Init;

  done := FALSE;
  Randomize(1);
  REPEAT

    cell.RandomMap(0);		{ Do not restart random number generator }
{$IFDEF TEST}
    IF NOT cell.Validate THEN Error('Feil etter RandomMap');
{$ENDIF}

    gen := 0;
    t := Ticker;
    REPEAT ticks := Ticker; UNTIL ticks <> t;

    ti := Timer_Value;
    REPEAT
      t := Ticker;
      cell.NextGen;
      Inc(gen);
{$IFDEF COUNT}

  g := gen; rem := 1;
  WHILE g >= 10 DO BEGIN
    g := g DIV 10;
    rem := rem * 10;
  END;
  IF (gen MOD rem = 0) AND (g in [1,2,3,4,5,7]) THEN BEGIN
    WriteLn(cf,gen:5,',',ScanC-OldScan:8,',',ChangeC-OldChange:8);
    OldScan := ScanC;
    OldChange := ChangeC;
  END;

{$ENDIF}

{$IFDEF TEST}
      IF NOT cell.Validate THEN Error('Feil etter NextGen');
{$ENDIF}
      WHILE ((HaltByte XOR SHFlag) AND $40 <> 0) AND (t = Ticker) DO ;

      WHILE ((HaltByte XOR SHFlag) AND $10 <> 0) AND NOT KeyPressed DO ;

      IF KeyPressed THEN BEGIN
	CASE ReadKey OF
	  #27 : done := TRUE;
	  #13 : SingleRepeat := TRUE;
	END;
      END;
    UNTIL (gen >= generations) OR done;
    ti := Timer_Value - ti;
    ticks := Ticker - ticks;
  UNTIL done OR SingleRepeat;

  IF HaltAtEnd AND (ReadKey = #0) THEN ;
  CleanUp;

  IF (gen > 0) THEN BEGIN
    WriteLn(gen,' generations (',cell.xsize,'x',cell.ysize,') in ',
	  ticks,' ticks.');
    IF ticks > 0 THEN
      WriteLn(gen * 18.2065 / ticks:1:1,' generations/second.');
    IF ti <> 0 THEN 
      WriteLn('Total time = ',TickToUs(ti) DIV 1000,'ms.');
  END;

{$IFDEF COUNT}
  Close(cf);

  WriteLn('Total cells = ',LongInt(gen) * cell.xsize * cell.ysize);
  WriteLn('Scan = ',ScanC,',  Change = ',ChangeC);
  FOR gen := 0 TO 15 DO BEGIN
    ScanC := 0;
    FOR t := 0 TO 15 DO
      Inc(ScanC,CountTable[gen*16+t]);

    Write(ScanC:9);
    IF gen = 7 THEN WriteLn;
  END;
{$ENDIF}
END.
