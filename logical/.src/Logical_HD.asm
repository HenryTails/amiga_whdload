
;_DebugAdress
;_DebugCopper

;*---------------------------------------------------------------------------
;  :Modul.	kick13.asm
;  :Contents.	kickstart 1.3 booter example
;  :Author.	Wepl, JOTD
;  :Version.	$Id: kick13.asm 1.23 2019/01/19 18:53:35 wepl Exp wepl $
;  :History.	19.10.99 started
;		20.09.01 ready for JOTD ;)
;		23.07.02 RUN patch added
;		04.03.03 full caches
;		20.06.03 rework for whdload v16
;		17.02.04 WHDLTAG_DBGSEG_SET in _cb_dosLoadSeg fixed
;		25.05.04 error msg on program loading
;		23.02.05 startup init code for BCPL programs fixed
;		04.11.05 Shell-Seg access fault fixed
;		03.05.06 made compatible to ASM-One
;		20.11.08 SETSEGMENT added (JOTD)
;		20.11.10 _cb_keyboard added
;		08.01.12 v17 config stuff added
;		10.11.13 possible endless loop in _cb_dosLoadSeg fixed
;		30.01.14 version check optimized
;		01.07.14 fix for Assign command via _cb_dosLoadSeg added
;		03.10.17 new options CACHECHIP/CACHECHIPDATA
;		28.12.18 segtracker added
;		19.01.19 test code for keyrepeat on osswitch added
;  :Requires.	kick13.s
;  :Copyright.	Public Domain
;  :Language.	68000 Assembler
;  :Translator.	BASM 2.16, ASM-One 1.44, Asm-Pro 1.17, PhxAss 4.38
;  :To Do.
;		Wings of Fury slave
;		V1.0 done by cfou
;		- only password protection removed
;		V1.1
;		- support for Rob Northen encrypted version (SPS 599)		
;	
;---------------------------------------------------------------------------*
;_Flash
;_Flash2
QUIT_AFTER_PROGRAM_EXIT

	INCDIR	Includes:
	INCLUDE	whdload.i
	INCLUDE	whdmacros.i
    IFND BARFLY ; conflict from "INCLUDE	Sources:whdload/kick13.s"
        INCLUDE     lvo/dos.i
    ENDC
	IFD	BARFLY

		OUTPUT	"Logical.Slave"
	BOPT	O+				;enable optimizing
	BOPT	OG+				;enable optimizing
	BOPT	ODd-				;disable mul optimizing
	BOPT	ODe-				;disable mul optimizing
	BOPT	w4-				;disable 64k warnings
	BOPT	wo-				;disable optimize warnings
	SUPER
	ENDC

;============================================================================

CHIPMEMSIZE	= $80000*1	;size of chip memory
FASTMEMSIZE	= $40000*1		;size of fast memory
NUMDRIVES	= 1		;amount of floppy drives to be configured
WPDRIVES	= %0001		;write protection of floppy drives

;BLACKSCREEN			;set all initial colors to black
;BOOTBLOCK			;enable _bootblock routine
BOOTDOS				;enable _bootdos routine
;BOOTEARLY			;enable _bootearly routine
;CBDOSLOADSEG			;enable _cb_dosLoadSeg routine
;CBDOSREAD			;enable _cb_dosRead routine
;CBKEYBOARD			;enable _cb_keyboard routine
;CACHE				;enable inst/data cache for fast memory with MMU
;CACHECHIP			;enable inst cache for chip/fast memory
;CACHECHIPDATA			;enable inst/data cache for chip/fast memory
;DEBUG				;add more internal checks
;DISKSONBOOT			;insert disks in floppy drives
DOSASSIGN			;enable _dos_assign routine
;FONTHEIGHT	= 8		;enable 80 chars per line
HDINIT				;initialize filesystem handler
;HRTMON				;add support for HrtMON
;IOCACHE		= 1024		;cache for the filesystem handler (per fh)
;MEMFREE	= $200		;location to store free memory counter
;NEEDFPU			;set requirement for a fpu
;POINTERTICKS	= 1		;set mouse speed
;SEGTRACKER			;add segment tracker
;SETPATCH			;enable patches from SetPatch 1.38
;SNOOPFS			;trace filesystem handler
;STACKSIZE	= 6000		;increase default stack
;TRDCHANGEDISK			;enable _trd_changedisk routine

;============================================================================

slv_Version	= 17
slv_Flags	= WHDLF_NoError|WHDLF_Examine|WHDLF_EmulPriv|WHDLF_ClearMem|WHDLF_EmulTrap
slv_keyexit	= $59	;F10

;============================================================================

	INCLUDE	Sources:whdload/kick13.s

;============================================================================

	IFD BARFLY
	IFND	.passchk
	DOSCMD	"WDate  >T:date"
.passchk
	ENDC
	ENDC

slv_CurrentDir	dc.b	"data",0
slv_name	dc.b	"Logical ",0
slv_copy	dc.b	"1991 Rainbow Arts.",0
slv_info	dc.b	"adapted for WHDLoad by CFou!",-1
            dc.b    "Trainer +3 by HenryTails",-1
            dc.b    "Version 1.3 "
	IFD BARFLY
		INCBIN	"T:date"
		dc.b 	-1
	ENDC
		dc.b	"using Wepl's kick13 emul"
		dc.b	0
	IFGE slv_Version-17
slv_config
        dc.b    "C1:B:Skip title screen;"
        dc.b    "C2:X:Unlimited lives:0;"
        dc.b    "C2:X:Disable next ball timer:1;"
        dc.b    "C2:X:Disable level timer:2;"
;        dc.b    "C2:L:Game Speed:Default,Very Fast,Fast,Normal,Slow,Very Slow;"
	ENDC
		dc.b	0
        EVEN




;============================================================================
; like a program from "startup-sequence" executed, full dos process,
; HDINIT is required, this will never called if booted from a diskimage, only
; works in conjunction with the virtual filesystem of HDINIT
; this routine replaces the loading and executing of the startup-sequence
;
; the following example is extensive because it preserves all registers and
; is able to start BCPL programs and programs build by MANX Aztec-C
;
; usually a simpler routine is sufficient, check kick31.asm for an simpler one
;
; D0 = ULONG argument line length, including LF
; D2 = ULONG stack size
; D4 = D0
; A0 = CPTR  argument line
; A1 = APTR  BCPL stack, low end
; A2 = APTR  BCPL
; A4 = APTR  return address, frame (A7+4)
; A5 = BPTR  BCPL
; A6 = BPTR  BCPL
; (SP)       return address
; (4,SP)     stack size
; (8,SP)     previous stack frame -> +4 = A1,A2,A5,A6

	IFD BOOTDOS

_bootdos
	lea	(_saveregs,pc),a0
		movem.l	d1-d3/d5-d7/a1-a2/a4-a6,(a0)
		move.l	(a7)+,(11*4,a0)
		move.l	(_resload,pc),a2	;A2 = resload

  		;get tags
		lea	(_tag2,pc),a0		; CF!
		jsr	(resload_Control,a2)
		;move.l	(_custom1,pc),d0


	;open doslib
		lea	(_dosname,pc),a1
		move.l	(4),a6
		jsr	(_LVOOldOpenLibrary,a6)
		lea	(_dosbase,pc),a0
		move.l	d0,(a0)
		move.l	d0,a6			;A6 = dosbase

	;assigns
		lea	(_disk1,pc),a0
		sub.l	a1,a1
		bsr	_dos_assign
		lea	(_disk2,pc),a0
		sub.l	a1,a1
		bsr	_dos_assign
		lea	(_disk3,pc),a0
		sub.l	a1,a1
		bsr	_dos_assign
		lea	(_disk4,pc),a0
		sub.l	a1,a1
		bsr	_dos_assign



	;check version
		lea	(_program2,pc),a0	;name
		move.l	#300,d3			;maybe 300 byte aren't enough for version compare...
		move.l	d3,d0			;length
		moveq	#0,d1			;offset
		sub.l	d3,a7
		move.l	a7,a1			;buffer
		jsr	(resload_LoadFileOffset,a2)
		move.l	d3,d0
		move.l	a7,a0

		jsr	(resload_CRC16,a2)
		add.l	d3,a7

	IFD _Flash
.t		move.w	#$f0,$dff180
		btst	#6,$bfe001
		bne	.t
	ENDC

		cmp.w	#$1A7E,d0	ECS	SPS 529
		beq	.versionok
		cmp.w	#$0C41,d0	CDTV
		beq	.versionok
;		cmp.w	#$655e,d0	; SPS ****
;		beq	.versionok
		pea	TDREASON_WRONGVER
		jmp	(resload_Abort,a2)
.versionok

                move.l 	_custom1(pc),d0
                tst.l 	d0
                bne 	.skip

     		movem.l d1-a6,-(a7)
       		lea     _program0(pc),a0
        	move.l 	(_resload,pc),a2
        	jsr 	(resload_GetFileSize,a2)
      		movem.l (a7)+,d1-a6
     		beq 	.noRAI

		; Launch Rainbow Art logo
		lea	(_program0,pc),a0
		bsr	.load
.noRAI



	; Launch Title

    		movem.l d1-a6,-(a7)
       		lea     _program1(pc),a0
        	move.l (_resload,pc),a2
        	jsr (resload_GetFileSize,a2)
      		movem.l (a7)+,d1-a6
     		beq .skip

		lea	(_program1,pc),a0
		bsr	.load
.skip
	; Launch game  game	
		lea	(_program2,pc),a0
		bsr	.load

.leave
	IFD QUIT_AFTER_PROGRAM_EXIT
		pea	TDREASON_OK
		move.l	(_resload,pc),a2
		jmp	(resload_Abort,a2)
	ELSE
	;remove exe

		move.l	d7,d1
		move.l	(_dosbase,pc),a6
		jsr	(_LVOUnLoadSeg,a6)

	;return to CLI
		moveq	#0,d0
		move.l	(_saverts,pc),-(a7)
		rts
	ENDC

.program_err	jsr	(_LVOIoErr,a6)
		pea	(_program0,pc)
		move.l	d0,-(a7)
		pea	TDREASON_DOSREAD
		jmp	(resload_Abort,a2)

.load
	;load exe
		move.l	a0,d1
		jsr	(_LVOLoadSeg,a6)
		move.l	d0,d7			;D7 = segment
		beq	.program_err

	;patch
		bsr 	_patch_game

		;lea	(_pl_program,pc),a0
		;move.l	d7,a1
		;jsr	(resload_PatchSeg,a2)

	IFD DEBUG
	;set debug
		clr.l	-(a7)
		move.l	d7,-(a7)
		pea	WHDLTAG_DBGSEG_SET
		move.l	a7,a0
		jsr	(resload_Control,a2)
		add.w	#12,a7
	ENDC

	IFD SETSEGMENT
	;store loaded segment list in current task
	;to make programs work which autodetach itself
	;but beware, kickstart will crash if the program does not
	;detach and dos will try to UnloadSeg it
		sub.l	a1,a1
		move.l	4,A6
		jsr	(_LVOFindTask,a6)
		move.l	d0,a0
		move.l	(pr_CLI,a0),d0
		asl.l	#2,d0			;BPTR -> APTR
		move.l	d0,a0
		move.l	d7,(cli_Module,a0)
	ENDC

	;call
		move.l	d7,d1
		moveq	#_args_end-_args,d0
		lea	(_args,pc),a0
		bsr	.call

	IFD KEYREPEAT
		bsr	_checkrepeat		;test code keyrepeat after osswitch
	ENDC
	rts


; D0 = ULONG arg length
; D1 = BPTR  segment
; A0 = CPTR  arg string

.call		lea	(_callregs,pc),a1
		movem.l	d2-d7/a2-a6,(a1)
		move.l	(a7)+,(11*4,a1)
		move.l	d0,d4
		lsl.l	#2,d1
		move.l	d1,a3
		move.l	a0,a4
	;create longword aligend copy of args
		lea	(_callargs,pc),a1
		move.l	a1,d2
.callca		move.b	(a0)+,(a1)+
		subq.w	#1,d0
		bne	.callca
	;set args
		move.l	(_dosbase,pc),a6
		jsr	(_LVOInput,a6)
		lsl.l	#2,d0		;BPTR -> APTR
		move.l	d0,a0
		lsr.l	#2,d2		;APTR -> BPTR
		move.l	d2,(fh_Buf,a0)
		clr.l	(fh_Pos,a0)
		move.l	d4,(fh_End,a0)
	;call
		move.l	d4,d0
		move.l	a4,a0
		nop
		movem.l	(_saveregs,pc),d1-d3/d5-d7/a1-a2/a4-a6
		jsr	(4,a3)
	;return
		movem.l	(_callregs,pc),d2-d7/a2-a6
		move.l	(_callrts,pc),a0
		jmp	(a0)

	IFD SIMPLE_CALL
.call		lsl.l	#2,d1
		move.l	d1,a3
		jmp	(4,a3)

	
	ENDC

_pl_program	PL_START
	;	PL_IFC2
	;		PL_P	$93a8-4,_END_VBL_INTERUPT
	;	PL_ENDIF
	;	PL_IFC1
	;		PL_W	$4342-4,$60f6			; Unlimited NRJ
	;		PL_W	$a7e-4,$6000			; Unlimited NRJ
	;	PL_ENDIF
		PL_END

_END_VBL_INTERUPT:
	BSR	BEAM_DELAY
;	move.w	#$90,$dff180
	MOVE.W	#$70,$dff09c
	movem.l	(a7)+,d0-a6
	rte
_Delay=14

BEAM_DELAY
        move.l  d0,-(a7)
	;move.l	_custom2(pc),d0
	;beq	.skip
	subq	#1,d0
	beq	.exit
        mulu  #_Delay,D0
	bra	.loop1
.skip
        move  #_Delay,D0
.loop1
        move.w  d0,-(a7)
        move.b  $dff006,d0      ; VPOS
	;move.w	d0,$dff180

.loop2  cmp.b   $dff006,d0
        beq 	  .loop2
        move.w  (a7)+,d0
        dbf     d0,.loop1
.exit
        move.l  (a7)+,d0
     	rts



_disk1		dc.b	"DF0",0		;for Assign
_disk2		dc.b	"cdtv",0		;for Assign
_disk3		dc.b	"LOGICAL",0		;for Assign
_disk4          dc.b    "LOG!CAL",0
_program0  	dc.b    "rai",0
_program1    	dc.b    "cppics",0
_program2	dc.b    "!LOGICAL",0
     		even

_args		dc.b	10,0		;must be LF terminated
_args_end

;_program	dc.b	"C/Echo",0
;_args		dc.b	"Test!",10	;must be LF terminated
;_args_end
	EVEN
	ENDC
_saveregs	ds.l	11
_saverts	dc.l	0
_dosbase	dc.l	0
_callregs	ds.l	11
_callrts	dc.l	0
_callargs	ds.b	208

_tag2
		dc.l	WHDLTAG_CUSTOM1_GET
_custom1	dc.l	0
		dc.l	WHDLTAG_CUSTOM2_GET
_custom2	dc.l	0
;		dc.l	WHDLTAG_CUSTOM3_GET
;_custom3	dc.l	0
;		dc.l	WHDLTAG_CUSTOM4_GET
;_custom4	dc.l	0
;		dc.l	WHDLTAG_CUSTOM5_GET
;_custom5	dc.l	0
;		dc.l	WHDLTAG_BUTTONWAIT_GET
;_ButtonWait	dc.l	0
		dc.l	0

;======================================================================
_FlushCache
		movem.l	d0-d1/a0-a2,-(a7)
		move.l	_resload(pc),a2
		jsr	resload_FlushCache(a2)
		movem.l	(sp)+,d0-d1/a0-a2
		rts
;======================================================================
_cb_keyboard
	cmp.B	#$58,d0
	bne	.no
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$f0,$dff180
	move.w #$0,$dff180
.no
	rts

_patch_game
	movem.l	d7/a1-a2,-(a7)
  	add.l 	d7,d7
  	add.l 	d7,d7
  	move.l	d7,a1
  	add.l 	#4,a1

	IFD	_Flash
.t
	move.w 	#$f,$dff180
	btst 	#$6,$bfe001
	bne 	.t
	ENDC

	bsr	_FixAccessFault68000
  	bsr 	patch_cdtv
  	bsr 	patch_ecs
	bsr	_FlushCache
      	movem.l	(a7)+,d7/a1-a2
  	rts



_FixAccessFault68000
                move.l 	_expmem(pc),a2
                add.l 	#$1cfea,a2
		patchs	0(a2),_FixAdress
		rts

_FixAdress
	move.l	a2,d0
	tst.l d0
	beq	.skip
	cmp.l	#$7f000,d0
	bhi 	.skip
	move.l	#-1,d0
	move.l	a6,a3	; original cofe
	move.l	a2,$38(a3)
	rts

.skip
	clr.l 	d0
	tst.l	d0
	rts
	

_adr	=	$10000		; instead of FD0000
;_adr2	=	_adr+$40000
	
patch_cdtv
                movem.l d0-d7/a0-a6,-(a7)
                sub.l #4,a1
                move.l a1,a0

	IFD	_DebugAdress
                move.l 	_expmem(pc),a2
                add.l	 #$1cffe-2,a2
                lea 	debugadr(pc),a3
                move.w 	#$4eb9,(a2)+
                move.l 	a3,(a2)+
                move.w 	#$4e71,(a2)    

	ENDC

;;;;;;;;;;;;;;;;*******  logical cdtv ******************

	IFD	_DebugCopper

                move.l a1,d0
                add.l #$f16c+4,d0
                move.l d0,a0
                cmp.l #$dff080,2(a0)
                bne .pas1
                lea patchcopper(pc),a3
                move.w #$4eb9,(a0)+; debug copper adr
                move.l a3,(a0)
.pas1
	ENDC

                move.l (a1),d0
                lsl.l #2,d0
                move.l d0,a2

                move.l a2,d0
                add.l #$345d8-$3411c,d0
                move.l d0,a0
                cmp.l #$fd0000,2(a0)
                bne .pas2
                move.l #_adr,2(a0);  one access FALSE
.pas2

                ; activate trainer, for more information read https://github.com/HenryTails/amiga_game_patches/blob/main/logical_cdtv.md

;                tst.w $fc0000 ; uae debugger trigger on "w 1 fc0000 2 R"

                ; Unlimited lives
                move.l  (_custom2,pc),d0
                btst    #0,d0
                beq     .skip_t1
                move.l  a1,d0
                add.l   #$056c-$24,d0
                move.l  d0,a0
                cmp.l   #$53ad8a32,(a0)
                bne     .skip_t1
                move.l  #$4e714e71,(a0)
.skip_t1

                ; Disable next ball timer
                move.l  (_custom2,pc),d0
                btst    #1,d0
                beq     .skip_t2
                move.l  a1,d0
                add.l   #$14fc-$24,d0
                move.l  d0,a0
                cmp.l   #$53ad83e6,(a0)
                bne     .skip_t2
                move.l  #$4e714e71,(a0)+
                move.l  #$4e714e71,(a0)
.skip_t2

                ; Disable level timer
                move.l  (_custom2,pc),d0
                btst    #2,d0
                beq     .skip_t3
                move.l  a1,d0
                add.l   #$0454-$24,d0
                move.l  d0,a0
                cmp.l   #$52ad839a,(a0)
                bne     .skip_t3
                move.l  #$4e714e71,(a0)
.skip_t3

                movem.l (a7)+,d0-d7/a0-a6
                rts


patch_ecs
                movem.l d0-d7/a0-a6,-(a7)
                sub.l #4,a1
                move.l a1,a0

;;;;;;;;;;;;;;;;******* Crack logical ecs ******************

                move.l a1,d0
                add.l #$28f06-$17904,d0
                move.l d0,a0
                cmp.l #$9081660a,(a0)
                bne .pas
                move.l #$2b4189ac,(a0); crack manual protection
.pas

	IFD	_DebugCopper
                move.l a1,d0
                add.l #$2c184-$1d4ec,d0
                move.l d0,a0
                cmp.l #$dff080,2(a0)
                bne .pas1
                lea patchcopper(pc),a3
                move.w #$4eb9,(a0)+; debug copper adr
                move.l a3,(a0)
.pas1

                move.l a1,d0
                add.l #$2ebba-$1d4ec,d0
                move.l d0,a0
                cmp.l #$dff080,2(a0)
                bne .pas11
                lea patchcopper(pc),a3
                move.w #$4eb9,(a0)+; debug copper adr
                 move.l a3,(a0)
.pas11
	ENDC

                move.l a1,d0
                add.l #$2a23a-$1d4ec,d0
                move.l d0,a0
                cmp.l #$70f7207c,(a0)
                bne .pas11b
                move.l #$700c6016,(a0); all drive + no write protect
.pas11b

                move.l a1,d0
                add.l #$6644,d0
                move.l d0,a0
                cmp.l #$672c45f9,(a0)
                bne .pas11bc
                move.b #$60,(a0); write on original disk possible
.pas11bc

                move.l a1,d0
                add.l #$2a258-$1d4ec,d0
                move.l d0,a0
                cmp.l #$70ff207c,(a0)
                bne .pas11bt
                move.l #$60124e71,(a0); ciadebug
.pas11bt

                move.l (a1),d0
                lsl.l #2,d0
                move.l d0,a2

                move.l a2,d0
                add.l #$345d8-$3411c,d0
                move.l d0,a0
                cmp.l #$fd0000,(a0)
                bne .pas2
                move.l #_adr,(a0);  one access FALSE
.pas2

                ; activate trainer, for more information read https://github.com/HenryTails/amiga_game_patches/blob/main/logical.md

;                tst.w $fc0000 ; uae debugger trigger on "w 1 fc0000 2 R"

                ; Unlimited lives
                move.l  (_custom2,pc),d0
                btst    #0,d0
                beq     .skip_t1
                move.l  a1,d0
                add.l   #$0560-$24,d0
                move.l  d0,a0
                cmp.l   #$53ad8a10,(a0)
                bne     .skip_t1
                move.l  #$4e714e71,(a0)
.skip_t1

                ; Disable next ball timer
                move.l  (_custom2,pc),d0
                btst    #1,d0
                beq     .skip_t2
                move.l  a1,d0
                add.l   #$14e0-$24,d0
                move.l  d0,a0
                cmp.l   #$53ad83c4,(a0)
                bne     .skip_t2
                move.l  #$4e714e71,(a0)+
                move.l  #$4e714e71,(a0)
.skip_t2

                ; Disable level timer
                move.l  (_custom2,pc),d0
                btst    #2,d0
                beq     .skip_t3
                move.l  a1,d0
                add.l   #$0454-$24,d0
                move.l  d0,a0
                cmp.l   #$52ad8378,(a0)
                bne     .skip_t3
                move.l  #$4e714e71,(a0)
.skip_t3

                movem.l (a7)+,d0-d7/a0-a6
                rts





	END



	IFD	_DebugAdress
                move.l 	_expmem(pc),a2
                add.l 	#$1cffe-2,a2
                lea 	debugadr(pc),a3
                move.w 	#$4eb9,(a2)+
                move.l 	a3,(a2)+
                move.w 	#$4e71,(a2)    
	ENDC


_cmp:    dc.l 0
debugadr:

        move.l 	a0,_adr2-4
        lea 	$c+4(a7),a0
        move.l 	a0,d0
        move.l 	_adr2-4,a0

        movem.l d1/a0-a2,-(a7)
        lea 	_cmp(pc),a1
        move.l 	(a1),d1
        cmp.l	#3,d1
        beq 	_skip
        cmp.l 	#2,d1
        bne 	_skip1
        add.l 	#1,(a1)
        movem.l (a7)+,d1/a0-a2

        move.l 	#_adr2,a2
        move.l 	a2,$34(a3)
        move.l 	d0,a0
        move.l 	a2,d4
        move.l 	a2,(a0)
        move.l 	#_adr2+$54,a0
        bra 	fin
_skip1
        add.l 	#1,(a1)
_skip
        movem.l (a7)+,d1/a0-a2
fin
        move.l	#$19*4-1,d0
.enc
        move.b 	(a0)+,(a1)+
        dbf 	d0,.enc
        rts

      

patchcopper:
    	move.l 	#_adr2,a0
    	move.l 	a0,d0
    	move.l 	#$fffffffe,(a0)
    	lea 	$dff080,a0
    	rts

