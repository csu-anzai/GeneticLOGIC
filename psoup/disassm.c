/* Disassembler for op-codes in the Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 26Jan92 MdG	0.3b	Added SCFn and SCBn instructions.
 * 26Jan92 MdG	0.3b	Added JMPB and JMPF instructions.
 * 16Jan92 MdG			File created.
 */

#include <stdio.h>
#include "soup.h"
#include "opcodes.h"

/*
 * Disassemble one instruction pointed to by p.  Index is the addr
 * at which the instuction resides (so jump addresses can be printed
 * correctly).  Wrap is TRUE if we're disassembling out of the soup,
 * and there can be an op-code at SOUPLEN - 1 with its operand at 0.
 * Onlyopcode is TRUE if we're disassembling the operand of a JMPF
 * or JMPB or similar instruction.  It suppresses the disassembly of
 * the second byte of a two-byte opcode, i. e. JMPF NNR3 instead of 
 * JMPF NNR3 0x1e.  Onlyopcode is only TRUE when disassemble() calls 
 * itself recursively.
 */
long
disassemble(p, index, wrap, onlyopcode)
unsigned char *p;
long index;
short wrap;
short onlyopcode;
{
	switch(*p) {
		case	JUMP:		/* Unconditional relative jump			*/
			printf("JUMP ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
					printf("0x%8.8lX", (long)(char) p[-(SOUPLEN - 1)]);
				} else {
					printf("0x%8.8lX", index + 1 + (long)(char) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	SPWN:		/* Spawn instruction				*/
			printf("SPWN");
			return 1;
			break;
		case	PCR1:		/* PC -> r1								*/
			printf("PCR1");
			return 1;
			break;
		case	PCR2:		/* PC -> r2								*/
			printf("PCR2");
			return 1;
			break;
		case	PCR3:		/* PC -> r3								*/
			printf("PCR3");
			return 1;
			break;
		case	PCR4:		/* PC -> r4								*/
			printf("PCR4");
			return 1;
			break;
		case	R2R1:		/* r2 -> r1								*/
			printf("R2R1");
			return 1;
			break;
		case	R3R1:		/* r3 -> r1								*/
			printf("R3R1");
			return 1;
			break;
		case	R4R1:		/* r4 -> r1								*/
			printf("R4R1");
			return 1;
			break;
		case	R1R2:		/* r1 -> r2								*/
			printf("R1R2");
			return 1;
			break;
		case	R1R3:		/* r1 -> r3								*/
			printf("R1R3");
			return 1;
			break;
		case	R1R4:		/* r1 -> r4								*/
			printf("R1R4");
			return 1;
			break;
		case	ADD1:		/* r1 + r1 -> r1						*/
			printf("ADD1");
			return 1;
			break;
		case	ADD2:		/* r1 + r2 -> r1						*/
			printf("ADD2");
			return 1;
			break;
		case	ADD3:		/* r1 + r3 ->  r1						*/
			printf("ADD3");
			return 1;
			break;
		case	ADD4:		/* r1 + r4 -> r1						*/
			printf("ADD4");
			return 1;
			break;
		case	SUB1:		/* r1 - r1 -> r1						*/
			printf("SUB1");
			return 1;
			break;
		case	SUB2:		/* r1 - r2 -> r1						*/
			printf("SUB2");
			return 1;
			break;
		case	SUB3:		/* r1 - r3 ->  r1						*/
			printf("SUB3");
			return 1;
			break;
		case	SUB4:		/* r1 - r4 -> r1						*/
			printf("SUB4");
			return 1;
			break;
		case	R2M1:		/* r2 -> (r1)							*/
			printf("R2M1");
			return 1;
			break;
		case	R3M1:		/* r3 -> (r1)							*/
			printf("R3M1");
			return 1;
			break;
		case	R4M1:		/* r4 -> (r1)							*/
			printf("R4M1");
			return 1;
			break;
		case	R1M2:		/* r1 -> (r2)							*/
			printf("R1M2");
			return 1;
			break;
		case	R3M2:		/* r3 -> (r2)							*/
			printf("R3M2");
			return 1;
			break;
		case	R4M2:		/* r4 -> (r2)							*/
			printf("R4M2");
			return 1;
			break;
		case	M1R2:		/* (r1) -> r2							*/
			printf("M1R2");
			return 1;
			break;
		case	M1R3:		/* (r1) -> r3							*/
			printf("M1R3");
			return 1;
			break;
		case	M1R4:		/* (r1) -> r4							*/
			printf("M1R4");
			return 1;
			break;
		case	M2R1:		/* (r2) -> r1							*/
			printf("M2R1");
			return 1;
			break;
		case	M2R3:		/* (r2) -> r3							*/
			printf("M2R3");
			return 1;
			break;
		case	M2R4:		/* (r2) -> r4							*/
			printf("M2R4");
			return 1;
			break;
		case	JPZ1:		/* Jump if r1 == 0						*/
			printf("JPZ1 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
					printf("0x%8.8lX", (long)(char) p[-(SOUPLEN - 1)]);
				} else {
					printf("0x%8.8lX", index + 1 + (long)(char) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	JPZ2:		/* Jump if r2 == 0						*/
			printf("JPZ2 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
					printf("0x%8.8lX", (long)(char) p[-(SOUPLEN - 1)]);
				} else {
					printf("0x%8.8lX", index + 1 + (long)(char) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	JPZ3:		/* Jump if r3 == 0						*/
			printf("JPZ3 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						printf("0x%8.8lX", (long)(char) p[-(SOUPLEN - 1)]);
				} else {
						printf("0x%8.8lX", index + 1 + (long)(char) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	JPZ4:		/* Jump if r4 == 0						*/
			printf("JPZ4 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						printf("0x%8.8lX", (long)(char) p[-(SOUPLEN - 1)]);
				} else {
						printf("0x%8.8lX", index + 1 + (long)(char) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	NNR1:
			printf("NNR1 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
					printf("0x%2.2lX", (unsigned long) p[-(SOUPLEN - 1)]);
				} else {
					printf("0x%2.2lX", (unsigned long) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	NNR2:
			printf("NNR2 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						printf("0x%2.2lX", (unsigned long) p[-(SOUPLEN - 1)]);
				} else {
						printf("0x%2.2lX", (unsigned long) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	NNR3:
			printf("NNR3 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						printf("0x%2.2lX", (unsigned long) p[-(SOUPLEN - 1)]);
				} else {
						printf("0x%2.2lX", (unsigned long) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	NNR4:
			printf("NNR4 ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						printf("0x%2.2lX", (unsigned long) p[-(SOUPLEN - 1)]);
				} else {
						printf("0x%2.2lX", (unsigned long) *(p + 1));
				}
				return 2;
			}
			return 1;
			break;
		case	INC1:
			printf("INC1");
			return 1;
			break;
		case	INC2:
			printf("INC2");
			return 1;
			break;
		case	INC3:
			printf("INC3");
			return 1;
			break;
		case	INC4:
			printf("INC4");
			return 1;
			break;
		case	DEC1:
			printf("DEC1");
			return 1;
			break;
		case	DEC2:
			printf("DEC2");
			return 1;
			break;
		case	DEC3:
			printf("DEC3");
			return 1;
			break;
		case	DEC4:
			printf("DEC4");
			return 1;
			break;
		case	JMPF:
			printf("JMPF ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	JMPB:
			printf("JMPB ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	PSHP:
			printf("PSHP");
			return 1;
			break;
		case	PSH1:
			printf("PSH1");
			return 1;
			break;
		case	PSH2:
			printf("PSH2");
			return 1;
			break;
		case	PSH3:
			printf("PSH3");
			return 1;
			break;
		case	PSH4:
			printf("PSH4");
			return 1;
			break;
		case	POP1:
			printf("POP1");
			return 1;
			break;
		case	POP2:
			printf("POP2");
			return 1;
			break;
		case	POP3:
			printf("POP3");
			return 1;
			break;
		case	POP4:
			printf("POP4");
			return 1;
			break;
		case	CLLF:
			printf("CLLF ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	CLLB:
			printf("CLLB ");
			if (onlyopcode == 0) {
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	RETN:
			printf("RETN");
			return 1;
			break;
		case	SCF1:
		printf("SCF1");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCF2:
		printf("SCF2");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCF3:
		printf("SCF3");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCF4:
		printf("SCF4");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCB1:
		printf("SCB1");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCB2:
		printf("SCB2");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCB3:
		printf("SCB3");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	SCB4:
		printf("SCB4");
			if (onlyopcode == 0) {
				printf(" ");
				if (wrap && (index == SOUPLEN - 1)) {
						disassemble(&p[-(SOUPLEN - 1)], 0L,
														(short) 0, (short) 1);
				} else {
						disassemble(p + 1, index + 1, (short) 0, (short) 1);
				}
				return 2;
			}
			return 1;
			break;
		case	LBL0:
			printf("LBL0");
			return 1;
			break;
		case	LBL1:
			printf("LBL1");
			return 1;
			break;
		case	LBL2:
			printf("LBL2");
			return 1;
			break;
		case	LBL3:
			printf("LBL3");
			return 1;
			break;
		case	LBL4:
			printf("LBL4");
			return 1;
			break;
		case	LBL5:
			printf("LBL5");
			return 1;
			break;
		case	LBL6:
			printf("LBL6");
			return 1;
			break;
		case	LBL7:
			printf("LBL7");
			return 1;
			break;
		case	LBL8:
			printf("LBL8");
			return 1;
			break;
		case	LBL9:
			printf("LBL9");
			return 1;
			break;
		default:			/* Default is a no-op					*/
			printf("0x%2.2X (NOOP)", *p);
			return 1;
			break;
	}
}
