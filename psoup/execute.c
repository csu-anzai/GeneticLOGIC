/* Interpreter for op-codes in the Primordial soup.
 * by Marc de Groot.
 * Copyright (c) 1992 by Marc de Groot.  All rights reserved.
 *
 * Modification log:
 * 26Jan92 MdG	0.3b	Added JMPB and JMPF instructions.
 */

#include "soup.h"
#include "organism.h"
#include "protos.h"
#include "opcodes.h"

/* Externs */
extern	unsigned char *soup;
extern	ORGANISM	organisms[];
extern	long		births;
extern	long		deaths;
extern	long		curr;
extern	long		instructions;
extern	short		monflag;
extern	long		mutate_counter;
extern	short		adjflag;
extern	long		heartbeat;

void
execute()
{
	long offset;					/* For jump instruction			*/
	long	r1;
	long	r2;
	long	r3;
	unsigned char byte;
	long	dummy1;					/* Used by POP macro			*/
	long	dummy2;					/* Used by POP macro			*/

	switch(soup[organisms[curr].pc]) {
		case	JUMP:		/* Unconditional relative jump			*/
			organisms[curr].pc++;
			ADJPC
			offset = soup[organisms[curr].pc];
			if (offset & 0x80) {
				offset |= 0xffffff00;
			}
			organisms[curr].pc += offset;
			ADJPC
			break;
		case	SPWN:		/* Blk cpy (r1) -> (r2) for r3 bytes,
							 * Spawn (r2), reset hearbeat counter.
							 */
			r1 = organisms[curr].r1 & SOUPLEN - 1;	/* get reg contents */
			r2 = organisms[curr].r2 & SOUPLEN - 1;
			r3 = organisms[curr].r3 & 0xff;

			/* Disallow bogus block copies and spawns */
			if ((r3 & 0xff) == 0 || r1 == r2) {
				organisms[curr].pc++;
				ADJPC
				break;
			}

			/* If the process table is full, the spawn
			 * will fail. Reset the heartbeat counter but
			 * don't do the block move.
			 */
			if (births - deaths == NORGANISMS) {
				organisms[curr].heartbeat = heartbeat;
				organisms[curr].pc++;
				ADJPC
				break;
			}

#ifdef	SPAWNCOST
			/* Subtract the length of the block copy from
			 * the organism's lifetime down counter.  If
			 * the result is < 1, set the lifetime counter to 1
			 * and don't do the spawn.
			 */
			organisms[curr].lifetime -= r3;
			if (organisms[curr].lifetime < 1) {
				organisms[curr].lifetime = 1;
				break;
			}
#endif

			/* Make sure we do a correct block copy */
			if ( ((r2 & SOUPLEN - 1) - (r1 & SOUPLEN - 1) & SOUPLEN - 1) <
																(r3 & 0xff)) {
				/* Copy backward */
				r1 += r3 - 1;  r1 &= SOUPLEN - 1;
				r2 += r3 - 1;  r2 &= SOUPLEN - 1;
				while (r3--) {
					soup[r2] = soup[r1];
					r1--;  r1 &= SOUPLEN - 1;
					r2--;  r2 &= SOUPLEN - 1;
				}
			} else {

				/* Copy forward */
				while (r3--) {
					soup[r2] = soup[r1];
					r1++;  r1 &= SOUPLEN - 1;
					r2++;  r2 &= SOUPLEN - 1;
				}
			}

			/* Give life to offspring */
			spawn(organisms[curr].r2);

			/* Reset heartbeat counter */
			organisms[curr].heartbeat = heartbeat;
			organisms[curr].pc++;
			ADJPC
			break;
		case	PCR1:		/* PC -> r1								*/
			organisms[curr].r1 = organisms[curr].pc;
			organisms[curr].pc++;
			ADJPC
			break;
		case	PCR2:		/* PC -> r2								*/
			organisms[curr].r2 = organisms[curr].pc;
			organisms[curr].pc++;
			ADJPC
			break;
		case	PCR3:		/* PC -> r3								*/
			organisms[curr].r3 = organisms[curr].pc;
			organisms[curr].pc++;
			ADJPC
			break;
		case	PCR4:		/* PC -> r4								*/
			organisms[curr].r4 = organisms[curr].pc;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R2R1:		/* r2 -> r1								*/
			organisms[curr].r1 = organisms[curr].r2;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R3R1:		/* r3 -> r1								*/
			organisms[curr].r1 = organisms[curr].r3;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R4R1:		/* r4 -> r1								*/
			organisms[curr].r1 = organisms[curr].r4;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R1R2:		/* r1 -> r2								*/
			organisms[curr].r2 = organisms[curr].r1;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R1R3:		/* r1 -> r3								*/
			organisms[curr].r3 = organisms[curr].r1;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R1R4:		/* r1 -> r4								*/
			organisms[curr].r4 = organisms[curr].r1;
			organisms[curr].pc++;
			ADJPC
			break;
		case	ADD1:		/* r1 + r1 -> r1						*/
			organisms[curr].r1 += organisms[curr].r1;
			organisms[curr].pc++;
			ADJPC
			break;
		case	ADD2:		/* r1 + r2 -> r1						*/
			organisms[curr].r1 += organisms[curr].r2;
			organisms[curr].pc++;
			ADJPC
			break;
		case	ADD3:		/* r1 + r3 ->  r1						*/
			organisms[curr].r1 += organisms[curr].r3;
			organisms[curr].pc++;
			ADJPC
			break;
		case	ADD4:		/* r1 + r4 -> r1						*/
			organisms[curr].r1 += organisms[curr].r4;
			organisms[curr].pc++;
			ADJPC
			break;
		case	SUB1:		/* r1 - r1 -> r1						*/
			organisms[curr].r1 -= organisms[curr].r1;
			organisms[curr].pc++;
			ADJPC
			break;
		case	SUB2:		/* r1 - r2 -> r1						*/
			organisms[curr].r1 -= organisms[curr].r2;
			organisms[curr].pc++;
			ADJPC
			break;
		case	SUB3:		/* r1 - r3 ->  r1						*/
			organisms[curr].r1 -= organisms[curr].r3;
			organisms[curr].pc++;
			ADJPC
			break;
		case	SUB4:		/* r1 - r4 -> r1						*/
			organisms[curr].r1 -= organisms[curr].r4;
			organisms[curr].pc++;
			ADJPC
			break;
		case	R2M1:		/* r2 -> (r1)							*/
			soup[organisms[curr].r1 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r2 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	R3M1:		/* r3 -> (r1)							*/
			soup[organisms[curr].r1 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r3 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	R4M1:		/* r4 -> (r1)							*/
			soup[organisms[curr].r1 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r4 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	R1M2:		/* r1 -> (r2)							*/
			soup[organisms[curr].r2 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r1 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	R3M2:		/* r3 -> (r2)							*/
			soup[organisms[curr].r2 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r3 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	R4M2:		/* r4 -> (r2)							*/
			soup[organisms[curr].r2 & SOUPLEN - 1] =
					(unsigned char) (organisms[curr].r4 & 0xff);
			organisms[curr].pc++;
			ADJPC
			break;
		case	M1R2:		/* (r1) -> r2							*/
			organisms[curr].r2 = (long) soup[organisms[curr].r1 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	M1R3:		/* (r1) -> r3							*/
			organisms[curr].r3 = (long) soup[organisms[curr].r1 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	M1R4:		/* (r1) -> r4							*/
			organisms[curr].r4 = (long) soup[organisms[curr].r1 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	M2R1:		/* (r2) -> r1							*/
			organisms[curr].r1 = (long) soup[organisms[curr].r2 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	M2R3:		/* (r2) -> r3							*/
			organisms[curr].r3 = (long) soup[organisms[curr].r2 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	M2R4:		/* (r2) -> r4							*/
			organisms[curr].r4 = (long) soup[organisms[curr].r2 & SOUPLEN - 1];
			organisms[curr].pc++;
			ADJPC
			break;
		case	JPZ1:		/* Jump if r1 == 0						*/
			if (organisms[curr].r1 == 0) {
				organisms[curr].pc++;
				ADJPC
				offset = soup[organisms[curr].pc];
				if (offset & 0x80) {
					offset |= 0xffffff00;
				}
				organisms[curr].pc += offset;
				ADJPC
				break;
			}
			organisms[curr].pc += 2;
			break;
		case	JPZ2:		/* Jump if r2 == 0						*/
			if (organisms[curr].r2 == 0) {
				organisms[curr].pc++;
				ADJPC
				offset = soup[organisms[curr].pc];
				if (offset & 0x80) {
					offset |= 0xffffff00;
				}
				organisms[curr].pc += offset;
				ADJPC
				break;
			}
			organisms[curr].pc += 2;
			break;
		case	JPZ3:		/* Jump if r3 == 0						*/
			if (organisms[curr].r3 == 0) {
				organisms[curr].pc++;
				ADJPC
				offset = soup[organisms[curr].pc];
				if (offset & 0x80) {
					offset |= 0xffffff00;
				}
				organisms[curr].pc += offset;
				ADJPC
				break;
			}
			organisms[curr].pc += 2;
			break;
		case	JPZ4:		/* Jump if r4 == 0						*/
			if (organisms[curr].r4 == 0) {
				organisms[curr].pc++;
				ADJPC
				offset = soup[organisms[curr].pc];
				if (offset & 0x80) {
					offset |= 0xffffff00;
				}
				organisms[curr].pc += offset;
				ADJPC
				break;
			}
			organisms[curr].pc += 2;
			break;
		case	NNR1:
			organisms[curr].pc++;
			ADJPC
			organisms[curr].r1 = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			break;
		case	NNR2:
			organisms[curr].pc++;
			ADJPC
			organisms[curr].r2 = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			break;
		case	NNR3:
			organisms[curr].pc++;
			ADJPC
			organisms[curr].r3 = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			break;
		case	NNR4:
			organisms[curr].pc++;
			ADJPC
			organisms[curr].r4 = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			break;
		case	INC1:
			organisms[curr].r1++;
			organisms[curr].pc++;
			ADJPC
			break;
		case	INC2:
			organisms[curr].r2++;
			organisms[curr].pc++;
			ADJPC
			break;
		case	INC3:
			organisms[curr].r3++;
			organisms[curr].pc++;
			ADJPC
			break;
		case	INC4:
			organisms[curr].r4++;
			organisms[curr].pc++;
			ADJPC
			break;
		case	DEC1:
			organisms[curr].r1--;
			organisms[curr].pc++;
			ADJPC
			break;
		case	DEC2:
			organisms[curr].r2--;
			organisms[curr].pc++;
			ADJPC
			break;
		case	DEC3:
			organisms[curr].r3--;
			organisms[curr].pc++;
			ADJPC
			break;
		case	DEC4:
			organisms[curr].r4--;
			organisms[curr].pc++;
			ADJPC
			break;
		case	JMPF:
			organisms[curr].pc++;
			ADJPC
			byte = soup[organisms[curr].pc];	/* Point at operand */
			do {
				organisms[curr].pc++;
				ADJPC
			} while (soup[organisms[curr].pc] != byte);
			break;
		case	JMPB:
			organisms[curr].pc++;
			ADJPC
			byte = soup[organisms[curr].pc];	/* Get operand */
			organisms[curr].pc--;				/* Point PC at JMPB */
			ADJPC
			do {
				organisms[curr].pc--;
				ADJPC
			} while (soup[organisms[curr].pc] != byte);
			break;
		case	PSHP:
			PUSH(organisms[curr].pc)
			organisms[curr].pc++;
			ADJPC
			break;
		case	PSH1:
			PUSH(organisms[curr].r1)
			organisms[curr].pc++;
			ADJPC
			break;
		case	PSH2:
			PUSH(organisms[curr].r2)
			organisms[curr].pc++;
			ADJPC
			break;
		case	PSH3:
			PUSH(organisms[curr].r3)
			organisms[curr].pc++;
			ADJPC
			break;
		case	PSH4:
			PUSH(organisms[curr].r4)
			organisms[curr].pc++;
			ADJPC
			break;
		case	POP1:
			organisms[curr].r1 = POP;
			organisms[curr].pc++;
			ADJPC
			break;
		case	POP2:
			organisms[curr].r2 = POP;
			organisms[curr].pc++;
			ADJPC
			break;
		case	POP3:
			organisms[curr].r3 = POP;
			organisms[curr].pc++;
			ADJPC
			break;
		case	POP4:
			organisms[curr].r4 = POP;
			organisms[curr].pc++;
			ADJPC
			break;
		case	CLLF:
			organisms[curr].pc++;
			ADJPC
			byte = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			PUSH(organisms[curr].pc)
			while (byte != soup[organisms[curr].pc]) {
				organisms[curr].pc++;
				ADJPC
			}
			break;
		case	CLLB:
			organisms[curr].pc++;
			ADJPC
			byte = soup[organisms[curr].pc];
			organisms[curr].pc++;
			ADJPC
			PUSH(organisms[curr].pc)
			organisms[curr].pc -= 3L;
			ADJPC
			while (byte != soup[organisms[curr].pc]) {
				organisms[curr].pc--;
				ADJPC
			}
			break;
		case	RETN:
			organisms[curr].pc = POP
			ADJPC
			break;
		case	SCF1:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search forward in soup for byte */
			do {
				organisms[curr].r1++;
				organisms[curr].r1 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r1]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCF2:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search forward in soup for byte */
			do {
				organisms[curr].r2++;
				organisms[curr].r2 &= SOUPLEN - 1;
			 } while (byte != soup[organisms[curr].r2]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCF3:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search forward in soup for byte */
			do {
				organisms[curr].r3++;
				organisms[curr].r3 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r3]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCF4:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search forward in soup for byte */
			do {
				organisms[curr].r4++;
				organisms[curr].r4 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r4]);

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCB1:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search backward in soup for byte */
			do {
				organisms[curr].r1--;
				organisms[curr].r1 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r1]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCB2:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search backward in soup for byte */
			do {
				organisms[curr].r2--;
				organisms[curr].r2 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r2]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCB3:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search backward in soup for byte */
			do {
				organisms[curr].r3--;
				organisms[curr].r3 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r3]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	SCB4:
			/* Point at operand */
			organisms[curr].pc++;
			ADJPC

			/* Get operand in byte */
			byte = soup[organisms[curr].pc];

			/* Search backward in soup for byte */
			do {
				organisms[curr].r4--;
				organisms[curr].r4 &= SOUPLEN - 1;
			} while (byte != soup[organisms[curr].r4]) ;

			/* Bump PC to next instruction */
			organisms[curr].pc++;
			ADJPC
			break;
		case	LBL1:
		case	LBL2:
		case	LBL3:
		case	LBL4:
		default:			/* Default is a no-op					*/
			organisms[curr].pc++;
			ADJPC
			break;
	}

	/* Decrement the heartbeat and lifetime counters */
	organisms[curr].heartbeat--;
	organisms[curr].lifetime--;

	/* Increment the number of interpreted instructions */
	instructions++;
}
