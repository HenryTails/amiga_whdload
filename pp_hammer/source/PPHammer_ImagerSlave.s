***************************************************************************
*             /                                                           *
*       _____.__ _                                         .___.          *
*      /    /_____________.  _________.__________.________ |   |________  *
*  ___/____      /    ____|_/         |         /|        \|   ._      /  *
*  \     \/      \    \     \    /    |    :___/¯|    \    \   |/     /   *
*   \_____________\___/_____/___/_____|____|     |____|\_____________/    *
*     -========================/===========|______\================-      *
*                                                                         *
*   .---.----(*(       P.P. HAMMER IMAGER SLAVE             )*)---.---.   *
*   `-./                                                           \.-'   *
*                                                                         *
*                         (c)oded by StingRay                             *
*                         --------------------                            *
*                            November 2017                                *
*                                                                         *
*                                                                         *
***************************************************************************

***********************************
** History			***
***********************************

; 03-Nov-2017	- work started
;		- and finished a short while later

	INCDIR	SOURCES:INCLUDE/
	INCLUDE	RawDIC.i


	SLAVE_HEADER
	dc.b	1		; Slave version
	dc.b	0		; Slave flags
	dc.l	.disk1		; Pointer to the first disk structure
	dc.l	.text		; Pointer to the text displayed in the imager window


	dc.b	"$VER: "
.text	dc.b	"P.P. Hammer imager V1.0",10
	dc.b	"by StingRay/[S]carab^Scoopex "
	dc.b	"(03.11.2017)",0
	CNOP	0,4



.disk1	dc.l	0		; Pointer to next disk structure
	dc.w	1		; Disk structure version
	dc.w	DFLG_DOUBLEINC	; Disk flags
	dc.l	.tracks		; List of tracks which contain data
	dc.l	0		; UNUSED, ALWAYS SET TO 0!
	dc.l	FL_NOFILES	; List of files to be saved
	dc.l	0		; Table of certain tracks with CRC values
	dc.l	0		; Alternative disk structure, if CRC failed
	dc.l	0		; Called before a disk is read
	dc.l	SaveFiles	; Called after a disk has been read

.tracks	TLENTRY	001,159,$17fc,$8915,DecodeTrack
	TLENTRY	002,100,$17fc,$8915,DecodeTrack
	TLEND



SaveFiles
	moveq	#1,d0			; read directory track
	jsr	rawdic_ReadTrack(a5)
	move.b	$f1-4(a1),d0
	subq.w	#1,d0
	mulu.w	#$17fc,d0
	move.l	$f4-4(a1),d1
	lea	.name(pc),a0
	jsr	rawdic_SaveDiskFile(a5)

	moveq	#1,d0			; read directory track
	jsr	rawdic_ReadTrack(a5)
	move.b	$d1-4(a1),d0
	;subq.w	#1,d0			; not needed as we need to skip
	mulu.w	#$17fc,d0		; the directory track
	move.l	$d4-4(a1),d1
	lea	.name2(pc),a0
	jsr	rawdic_SaveDiskFile(a5)

	moveq	#IERR_OK,d0
	rts

.name	dc.b	"PPHammer",0
.name2	dc.b	"Disk.1",0
	CNOP	0,2


	

; d0.w: track number
; a0.l: ptr to mfm buffer
; a1.l: ptr to destination buffer

DecodeTrack
	cmp.l	#$AAAAAAAA,(a0)+
	bne.b	.error

	move.l	#$55555555,d6
	bsr.b	.decode

; decode first long
	moveq	#0,d5
	bsr.b	.decode
	eor.l	d3,d5

	move.w	#$17fc/4-1,d7
.loop	bsr.b	.decode
	eor.l	d3,d5
	move.l	d3,(a1)+
	dbf	d7,.loop

	bsr.b	.decode
	cmp.l	d3,d5
	bne.b	.error
	moveq	#IERR_OK,d0
	rts

.error	moveq	#IERR_CHECKSUM,d0
	rts

.decode	move.l	(a0)+,d2
	move.l	(a0)+,d3
	and.l	d6,d2
	and.l	d6,d3
	lsl.l	#1,d2
	or.l	d2,d3
	rts
