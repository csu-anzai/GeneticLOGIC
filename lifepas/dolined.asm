        .model tpascal

        .data
EXTRN   Color:DWORD
EXTRN   Middle:DWORD
EXTRN   Edge:DWORD
EXTRN   NewCell:WORD

        .code

        public DoLineA, InitStep

DoLineA proc near sr:DWORD, S2G:WORD, len:WORD

  cld
  .386
  les di,[sr]
  mov bx,[len]
  lea cx,[bx+1]
  add bx,bx
  mov word ptr es:[bx+di],-1

  CLI

  mov si,0A000h
  mov gs,si
  mov fs,[NewCell]

@loop:
  xor ax,ax
  mov bx,ax
@l0:
  repe scasw
  mov si,es:[di-2]
  mov bl,fs:[si]
  or bx,bx
   jz @l0

   jcxz @done

  shl bx,2

  mov si,[S2G]
  add si,di
  add si,si
;  lea esi,[esi+edi*2]
  mov eax,Color[bx]
  mov gs:[si],eax

  mov eax, Middle[bx]
  mov edx, Edge[bx]
  add [di-3],eax
  add [di-160-3],edx
Above1 label word
  add [di+160-3],edx
Below1 label word

  xor ax,ax
  repe scasw
  mov si,es:[di-2]
  mov bl,fs:[si]
  and bx,255
   jz @l0

   jcxz @done

  shl bx,2

  mov si,[S2G]
  add si,di
  add si,si
  mov eax,Color[bx]
  mov gs:[si],eax

  mov eax, Middle[bx]
  mov edx, Edge[bx]
  add [di-3],eax
  add [di-160-3],edx
Above2 label word
  add [di+160-3],edx
Below2 label word

   jmp @loop

@done:

  STI
  .8086
  ret
DoLineA endp

InitStep proc near above:WORD, below:WORD
  mov ax,[above]
  sub ax,3
  mov cs:[Above1-2],ax
  mov cs:[Above2-2],ax

  mov ax,[below]
  sub ax,3
  mov cs:[Below1-2],ax
  mov cs:[Below2-2],ax

  ret
InitStep endp

        end


