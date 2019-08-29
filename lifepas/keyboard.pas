{$R-,S-}
UNIT Keyboard;

Interface

TYPE
  KBD_ID_TYPE = (OldKbd, EnhBiosOldKbd, EnhKbd);

VAR
  KBD_TYPE : KBD_ID_TYPE;
  KeyboardType : BYTE;

FUNCTION GetKey : WORD;

FUNCTION ReadKey : CHAR;

FUNCTION KeyPressed : BOOLEAN;

FUNCTION ShiftStatus : BYTE;

{*******************************************************************}

                             Implementation

{*******************************************************************}

CONST
  LastKey : WORD = $FFFF;

FUNCTION GetKey : WORD;
Assembler;
ASM
  mov ah,[KeyBoardType]
  int $16
END;

FUNCTION ReadKey : CHAR;
Assembler;
ASM
  mov ax,-1
  xchg ax,[LastKey]
  xchg al,ah
  or ah,ah
   je @done
  mov ah,[KeyBoardType]
  int $16
  mov [LastKey],ax
 @done:
END;

FUNCTION KeyPressed : BOOLEAN;
Assembler;
ASM
  mov bl,1
  cmp byte ptr [LastKey],0
   jz @done
  mov ah,[KeyBoardType]
  inc ah
  int $16
   jnz @done
  dec bx
@done:
  mov al,bl
END;

FUNCTION ShiftStatus : BYTE;
Assembler;
ASM
  mov ah,[KeyBoardType]
  add ah,2
  int $16
END;

BEGIN
  ASM
    xor dx,dx            { OldKbd, KeyboardType}
    mov ah,2
    int $16
    mov bx,ax
    not al
    mov ah,$12
    int $16
    cmp al,bl
     jne @done
    mov dx,$1001         { Enh. BIOS, use new calls}
    mov ax,$40
    mov es,ax
    test byte ptr [es:$96],$10
     jz @done            { But old keyboard! }
    inc dx
  @done:
    mov [KBD_TYPE],dl
    mov [KeyBoardType],dh
  END;
END.
