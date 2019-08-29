{$R-,S-}
Unit RandLFSR;

Interface

Function Rand8 : BYTE;
Function Rand16 : Word;
Function Rand32 : LongInt;
Function Random(modulus : Word): Word;
Procedure Randomize(seed : LongInt);

Implementation

VAR
  FeedbackRegister : LongInt;

Function Rand8 : BYTE;
Assembler;
ASM
  mov  ax,word ptr [FeedbackRegister]    { High order 16 bits }
  mov  dx,word ptr [FeedbackRegister+2]  { Low order 16 bits }
  mov  cx,8
@loop:
  mov bl,al
  test dh,0EAh
   jpe @ok
  xor bl,1		{ Invert low bit }
@ok:
  shr  bl,1
  adc  ax,ax
  adc  dx,dx
   loop @loop
  mov  word ptr [FeedbackRegister],ax
  mov  word ptr [FeedbackRegister+2],dx
END;

CONST
  FeedbackLoops = 16;
  
Function Random(modulus : Word): Word
{$IFDEF StonyBrook} [Alters()] {$ENDIF};
Assembler;
ASM
  push bx; push cx; push dx

  mov  bx,word ptr [FeedbackRegister]    { High order 16 bits }
  mov  ax,word ptr [FeedbackRegister+2]  { Low order 16 bits }
  mov  cx,FeedBackLoops
{  mov cx,[modulus]      }
@loop:
  mov dl,bl
  test ah,0EAh
   jpe @ok
  xor dl,1		{ Invert low bit }
@ok:
  shr  dl,1
  adc  bx,bx
  adc  ax,ax
   loop @loop
{  shr cx,1
   jnz @loop }

  mov  word ptr [FeedbackRegister],bx
  mov  word ptr [FeedbackRegister+2],ax
  
  xor dx,dx
  mov cx,[modulus]
  cmp cx,1
   jbe @done
  div cx
  mov ax,bx
  div cx
@done:
  mov ax,dx
  
  pop dx; pop cx; pop bx
END;

Function Rand16 : WORD;
Assembler;
ASM
  call Rand8
  call Rand8
END;

Function Rand32 : LongInt;
Assembler;
ASM
  call Rand8
  call Rand8
  call Rand8
  call Rand8
END;

FUNCTION Timer_Value: LongInt;
Assembler;
ASM
    mov ax,40h
    mov es,ax
    mov al,0
    cli
    out 43h,al
    in al,40h
    mov ah,al
    in al,40h
    xchg al,ah
    neg ax
    mov dx,[es:6Ch]
    sti
END;

Procedure Randomize(seed : LongInt);
BEGIN
  WHILE seed = 0 DO
    seed := Timer_Value;
  FeedbackRegister := seed;
END;

BEGIN
  Randomize(0);
END.

