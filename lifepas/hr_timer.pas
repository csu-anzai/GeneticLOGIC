{$R-,S-,N+,E-,G+}

Unit HR_Timer;  {High resolution timer!}

Interface

Uses
  Dos {, Dato };

type
  TimeStamp = record
    sec1970,
    fraq : LongInt;
  end;

FUNCTION Timer_Value : LongInt;

{ Procedure GetTStamp(var ts: TimeStamp); }

FUNCTION TickToMs(t : LongInt): REAL;

FUNCTION TickToUs(t : LongInt): LongInt;

PROCEDURE ENABLE; InLine($FB);

PROCEDURE DISABLE; InLine($FA);

VAR
  Timer_Low : WORD ABSOLUTE $40 : $6C;
  overhead : LongInt;

Implementation

CONST
  MsPrTick = 8.358636E-4;
  UsPrTickMul = 54779;

Procedure iodelay;
Assembler;
asm
  push cx
  mov cx,5
@loop:
  loop @loop
  pop cx
end;

FUNCTION Timer_Value: LongInt;
Assembler;
ASM
  mov al,0
  CLI
  out 43h,al
  mov ax,40h
  mov es,ax
  call iodelay
  in al,40h
  mov dx,[es:6Ch]
  mov ah,al
  call iodelay
  in al,40h
  xchg al,ah
  neg ax
  STI
END;

{$IFDEF TSTAMP}
var
  startOfDaySeconds : LongInt;

Procedure initdate;
var
  y,m,d, dow : word;
begin
  getdate(y,m,d,dow);
  startOfDaySeconds := (ymd2date(y,m,d)
			 - date1970) * 86400;
end;

const
  lastTick : word = 0;
  bios_timer : ^word = Ptr($40,$6C);

Procedure GetTStamp(var ts: TimeStamp);
{
  Algorithm:  If the current 32-bit tick count is less than the
  previous sample, a date rollover has occured, and initdate()
  is called to initialize the startOfDaySeconds variable.

  Interrupts are disabled, and the current 48-bit tick count, including
  the 16 bits inside the timer chip (at port 40/43) is moved into
  DX:EAX.

  The number of sw tick counts in a day is (18.2065 * 86400 =) 1573040,
  which has 21 significant bits.

  I combine this number with the upper 11 bits of the hw counter,
  discarding the 5 least significant bits.  This limits the resolution
  of my time stamps to approx. 27 micro-seconds, which is still more
  than good enough, and it speeds up the subsequent code quite a bit:

  I need to divide the tick count by 18.2065, which I do by multiplying
  by it's resiprocal (scaled by 16 to get maximum resolution), and
  then finally shift the resulting EDX value, to leave the upper 17
  bits in EDX (seconds), and the remainder in EAX.  Since the last bits
  of EAX has no significance, I zero them:
}
begin
  asm
    les si,[bios_timer]
    mov ax,es:[si]
    mov dx,[lastTick]
    mov [lastTick],ax
    cmp ax,dx
     jae @dateOK

    call initdate		{ Date rollover, so re-init! }

    les si,[bios_timer]

@dateOK:
    db $66; or dx,-1            { Flag value to force dual sampling}
@tryAgain:
    mov di,ax
    mov ax,es:[si]
    db $66; shl ax,16
    mov al,0
    CLI
     out 43h,al
     mov bx,dx
     mov dx,es:[si+2]
     call iodelay
     in  al,40h
     mov ah,al
     call iodelay
     in  al,40h
     xchg al,ah
    STI	                {AX has 16-bit fractional 18.2 hz tick count}
    cmp bx,dx
     jne @tryAgain	{Will sample at least twice to de-glitch }

    sub di,ax
     jc @tryAgain	{I do not accept rollover here! }

    neg ax		{The HW tick is counting down}
    les bx,[ts]

(*
    shrd eax,edx,5      { Scaled tick count; 21 + 11 bits }
    mov edx,3774451248  { 16 / 18.20648; Scaled resiprocal }
    mul edx
    shrd eax,edx,15	{ Scale back to get 32:32 fixed-point }
    xor ax,ax		{ Zero out non-significant lower bits }
    shr edx,15
    add edx,dword ptr [startOfDaySeconds] {Finally, add on the nr of}
			{ seconds since 1970 until today!}
*)
    db $66; mov es:[bx],dx
    db $66; mov es:[bx+4],ax
  end;
end;

{$ENDIF}


FUNCTION TickToMs(t : LongInt): REAL;
BEGIN
  TickToMs := (t - overhead) * MsPrTick;
END;

FUNCTION TickToUs(t : LongInt): LongInt;
Assembler;
ASM
  mov bx,UsPrTickMul
  mov ax,word ptr [t]
  mul bx
  mov cx,dx
  mov ax,word ptr [t+2]
  mul bx
  add ax,cx
  adc dx,0
END;

begin
  asm
    CLI
    mov al,34h
    out 43h,al
    call iodelay
    mov  al,0
    out  40h,al
    call iodelay
    out 40h,al
    STI
  end;
  overhead := Timer_Value;
  overhead := Timer_Value - overhead;
END.
