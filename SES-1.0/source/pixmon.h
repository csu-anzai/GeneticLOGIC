
/*
 *	header types:
 *		PIX_FLUSH:	draw (or redraw) given area
 *		PIX_DATA:	color data for given area follows
 *		PIX_KILL:	quit PixMon
 *
 */

#ifndef	__PIXMON__
#define	__PIXMON__


#define PIX_FLUSH	0x1
#define PIX_DATA	0x2
#define PIX_KILL	0x4

#define PIX_UNKNOWN	(~(PIX_FLUSH | PIX_DATA))

#define PIX_MAGIC	0x2308
#define	PIX_RLE		0xFF

#define INIT_IMGHDR(hdr) ((hdr).type=(PIX_DATA|PIX_FLUSH),\
			  (hdr).magic=PIX_MAGIC,\
			  (hdr).x=(hdr).y=(hdr).dx=(hdr).dy=0,\
			  (hdr).sizelo=(hdr).sizehi=0)

#define EXIT_IMGHDR(hdr) ((hdr).type=PIX_KILL,\
			  (hdr).magic=PIX_MAGIC,\
			  (hdr).x=(hdr).y=(hdr).dx=(hdr).dy=0,\
			  (hdr).sizelo=(hdr).sizehi=0)

#define	PIX_SIZE(hdr)	(int)((hdr).sizelo | (int)(hdr).sizehi << 16)

#define PIX_LO(val)	(unsigned short)(((val) & 0xffff))
#define PIX_HI(val)	(unsigned short)(((val) >> 16) & 0xffff)

typedef struct {
	unsigned short	magic;
	unsigned short  type;
	short		x;
	short		y;
	unsigned short	dx;
	unsigned short	dy;
	unsigned short	sizelo;
	unsigned short	sizehi;
} ImgHdr;

typedef struct {
	unsigned short  type;
	short		x;
	short		y;
	unsigned short	dx;
	unsigned short	dy;
	unsigned short	sizelo;
	unsigned short	sizehi;
} IMGHDR;


#endif	/*PIXMON*/
