/* tmonitor.h  92-3-19 */
/* Tierra Simulator V3.1: Copyright (c) 1991, 1992 Tom Ray & Virtual Life */

/*
 * tmonitor_h_sccsid: %W%    %G%
 */

#ifndef LTMONITOR_H
#define LTMONITOR_H


void		TRepBirth P_(( u_long ulStart, u_long ulLength ));
void		TRepDeath P_(( u_long ulStart, u_long ulLength ));
void		TMoveIP   P_(( u_long ulStart, u_long ulPosition));
void		TMoveD    P_(( u_long ulStart, u_long ulFrom, u_long ulTo));
void		TPlan     P_(( void ));

void		TInitOrgLifeEvents P_(( u_char ucOp, hMtLinkInfo hLink ));

void		TQueryGeneralStats P_(( u_char ucQ, pMtOpaque pData,
				        hMtLinkInfo hLink ));
void		TQueryOrganism P_(( u_char ucQ, pMtOpaque pData,
				    hMtLinkInfo hLink ));

void		TSimRuncontrol P_(( u_char ucM, pMtOpaque pData,
				    hMtLinkInfo hLink ));



#endif /* ifndef LTMONITOR_H; Add nothing past this point */
