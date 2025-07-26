;----------------------------------------------------------------------------- 
;
; Game          Ugh!
;
; Author:       asman, JOTD, HenryTails
; Version       1.4
; History       in ReadMe
; Requires      Assembler, NDK, WHDLoad/Src/sources
; Copyright     Public Domain
; Language      68000 Assembler
; Translator    Barfly/Basm v2.17
; WHDLoad dev   v19.1
;----------------------------------------------------------------------------- 

	INCDIR	Includes:
	INCLUDE	whdload.i
	INCLUDE	whdmacros.i
	INCLUDE	libraries/lowlevel.i
;-----------------------------------------------------------------------------

RELEASE	;uncomment, if you want make release version of slave'a
IGNORE_JOY_DIRECTIONS
;-----------------------------------------------------------------------------

	IFD BARFLY

	IFD RELEASE

	OUTPUT	"Ugh.slave"

	BOPT	O+             ;enable optimizing
	BOPT	OG+            ;enable optimizing

	ELSE

	OUTPUT	"u.slave"
	BOPT	O-             ;disable optimizing
	BOPT	OG-            ;disable optimizing

	ENDC	;END IFD RELEASE

	BOPT   ODd-           ;disable mul optimizing
	BOPT   ODe-           ;disable mul optimizing
	BOPT   w4-            ;disable 64k warnings
	BOPT   wo-            ;disable optimize warnings
	SUPER

	ENDC	;END IFD BARFLY

;-----------------------------------------------------------------------------
;; macro: used to WHDLoad slaves
;
; \1 - name of routine without prefix ( resload_ )
;

WHDL	MACRO
			move.l	_resload(pc),a2
			jsr		resload_\1(a2)
		ENDM

;-----------------------------------------------------------------------------

Emulate_Key	MACRO

;\1 JPB bit from lowlevel.i ( for example JPB_BUTTON_PLAY )
;\2 key ( rawkey ) ( for example KEY_LEFT_ALT or $64 )

	;button or direct was pressed ?
		btst	#JPB_\1,d1
		beq.w	.free_key\@

	;if last time the same was pressed then jump to next key
		btst	#JPB_\1,d2
		bne.w	.next_key\@

	;set appropriate bit in prev / last cd32pad bit state
		bset	#JPB_\1,d2

	;emulate key ( press )
		move.b	#\2,d0

	;call routine
		movem.l	d0/a0/a1,-(sp)
		move.l	gamePortsRoutine(pc),a0
		jsr	(a0)
		movem.l	(sp)+,d0/a0/a1
		bra.w	.next_key\@

.free_key\@

	;is this real reason to emulate ( unpressed key )
		btst	#JPB_\1,d2
		beq.w	.next_key\@

	;clean proper bit in prev / last cd32pad bit state
		bclr	#JPB_\1,d2

	;emulate key ( unpress )
		move.b	#\2-$1,d0

	;call routine
		movem.l	d0/a0/a1,-(sp)
		move.l	gamePortsRoutine(pc),a0
		jsr	(a0)
		movem.l	(sp)+,d0/a0/a1

.next_key\@
	ENDM
;-----------------------------------------------------------------------------
;
; W H D L O A D   L A B E L S
;  

CHIPMEMSIZE	= $80000
FASTMEMSIZE	= $80000
NUMDRIVES	= 1
WPDRIVES	= %0000

CACHE
HDINIT
IOCACHE		= 1024
BOOTDOS

	IFD RELEASE

BLACKSCREEN

	ELSE

DEBUG
HRTMON

	ENDC	;IFD RELEASE 
;-----------------------------------------------------------------------------
slv_Version	= 17		; bump for slv_config
slv_Flags	= WHDLF_NoError|WHDLF_Examine
slv_keyexit	= $59	;F10
;-----------------------------------------------------------------------------
	INCLUDE	sources:whdload/kick13.s
;-----------------------------------------------------------------------------
	IFD BARFLY
	IFND	.passchk
	DOSCMD	"WDate  >T:date"
.passchk
	ENDC
	ENDC

slv_CurrentDir	dc.b	"data",0
slv_name	dc.b	"Ugh",0
slv_copy	dc.b	"1992 PlayByte - Ego Software",0
slv_info	dc.b	"installed & fixed by asman, JOTD & HenryTails",10
			dc.b	"Version 1.4 "
	IFD BARFLY
		INCBIN	"T:date"
	ENDC
		dc.b	10
		dc.b	"Thanks to Christian Sauer for original",10    ; we need vertical space
		dc.b	"=============< BUTTONS >===============",10
		dc.b	"blue   = F1  key ( start game )        ",10
		dc.b	"green  = ESC key ( back to title menu )",10
		dc.b	"yellow = F3  key ( easy, medium, hard )",10
		dc.b	"play   = P   key ( pause in game )     "
		dc.b	0
	EVEN

;-----------------------------------------------------------------------------
_bootdos:
		;move.l	_resload(pc),a2		;A2 = resload
		bsr	_detect_controller_types
 
	;open doslib
		lea	_dosname(pc),a1
		move.l	(4),a6
		jsr	_LVOOldOpenLibrary(a6)
		lea	_dosbase(pc),a0
		move.l	d0,(a0)
		move.l	d0,a6			;A6 = dosbase

	; skip intro on Custom2 set
		lea		(Control_for_Custom2_tag,pc),a0
		WHDL	Control
		move.l  (Control_for_Custom2_value,pc),d0	; D0 = value of Custom2
		tst.l	d0
		bne.s	skip_intro

	;load intro
		lea	intro(pc),a3
		move.l	a3,d1
		jsr	_LVOLoadSeg(a6)
		move.l	d0,d7			;D7 = seglist
		beq	error

	;patch intro
		nop

	;run intro
		lsl.l	#2,d0
		move.l	d0,a3
		jsr	4(a3)

	;remove intro
		move.l	d7,d1
		move.l	_dosbase(pc),a6
		jsr	_LVOUnLoadSeg(a6)

skip_intro:

	;load game
		lea	game(pc),a3
		move.l	a3,d1
		jsr	_LVOLoadSeg(a6)
		move.l	d0,d7
		beq	error

	;patch game
		lea	patch_hunk_0(pc),a0
		move.l	d7,a1
		WHDL	PatchSeg

PORTS_OFFSET	= $51a4
		move.l	d7,a1
		add.l	a1,a1
		add.l	a1,a1
		add.l	#PORTS_OFFSET+4,a1
		lea	gamePortsRoutine(pc),a3
		move.l	a1,(a3)

	; get Hiscore address ( hunk 7 + hiscore_offset )
HISCORE_HUNK	= 7
HISCORE_OFFSET	= $1fe
HISCORE_SIZE	= 106

		moveq	#HISCORE_HUNK,d0
		move.l	d7,a1
		bsr	find_hunk
		add.l	a1,a1
		add.l	a1,a1
		add.l	#HISCORE_OFFSET+4,a1
		lea	(pHiscore,pc),a0
		move.l	a1,(a0)

	; load highscore

		bsr	load_hiscore

	;run game
		lsl.l	#2,d7
		move.l	d7,a3
		jsr	4(a3)

_exit		pea	(TDREASON_OK).w
_quit		move.l	(_resload,pc),-(a7)
		addq.l	#resload_Abort,(a7)
		rts

_debug		pea	(TDREASON_DEBUG).w
		bra	_quit 

error		jsr	_LVOIoErr(a6)
		move.l	a3,-(sp)
		move.l	d0,-(sp)
		pea	TDREASON_DOSREAD
		bra	_quit

;-----------------------------------------------------------------------------
patch_hunk_0
		PL_START

	; fix access fault (movec vbr,a1 --> sub.l a1,a1 + nop)
		PL_L	$18,$93c94e71

		PL_P	$516a,setupKeyboard

	; change int PORTS to routine
		PL_NOP	$51b0,$10
		PL_R	$5224

	; CD32 support
		PL_PS	$399a,fixCD32_int		

	; activate trainer, for more information read https://github.com/HenryTails/amiga_game_patches/blob/main/ugh.md
		PL_IFC1

			PL_IFC1X 0	; Unlimited lives
				PL_W	$5698,$4a39
			PL_ENDIF

			PL_IFC1X 1	; Unlimited energy
				PL_NOPS	$53de,3
				PL_NOPS	$548a,3
				PL_NOPS	$54dc,3
			PL_ENDIF

			PL_IFC1X 2	; Disable pterodactyl
				PL_NOPS	$6600,2
			PL_ENDIF

			PL_IFC1X 3	; Invincible tree
				PL_NOPS	$6b10,2
			PL_ENDIF

	; allow for highscore save only when trainer is disabled
		PL_ELSE

		; save hiscore
			PL_PS	$2506,fixHiscore_Save

	; end PL_IFC1 section
		PL_ENDIF

		PL_END

;-----------------------------------------------------------------------------
fixCD32_int
		move.w	#$3e8,d0	;original
		nop			;original

		movem.l	d0-d2/a1,-(sp)

		bsr	_read_joysticks_buttons
		move.l	joy1_buttons(pc),d0 
		move.l	d0,d1

		btst	#JPB_BTN_REVERSE,d0
		beq.b	.state
		btst	#JPB_BTN_FORWARD,d0
		beq.b	.state

		bra	_exit

.state		lea	(cd32last_state,pc),a1
		move.l	(a1),d2

		Emulate_Key	BUTTON_BLUE,$5f		;F1
		Emulate_Key	BUTTON_GREEN,$75	;ESC
		Emulate_Key	BUTTON_PLAY,$CD		;P
		Emulate_Key	BUTTON_YELLOW,$5B	;F3

		move.l	d2,(a1)
		movem.l	(sp)+,d0-d2/a1
		rts

cd32last_state	dc.l	0

;-----------------------------------------------------------------------------

setupKeyboard
	;set the interrupt vector
		pea	(.int,pc)
		move.l	(a7)+,($68)
	;allow interrupts from the keyboard
		move.b	#CIAICRF_SETCLR|CIAICRF_SP,(ciaicr+_ciaa)
	;clear all ciaa-interrupts
		tst.b	(ciaicr+_ciaa)
	;set input mode
		and.b	#~(CIACRAF_SPMODE),(ciacra+_ciaa)
	;clear ports interrupt
		move.w	#INTF_PORTS,(intreq+_custom)
	;allow ports interrupt
		move.w	#INTF_SETCLR|INTF_INTEN|INTF_PORTS,(intena+_custom)
		rts 

	;check if keyboard has caused interrupt
.int		btst	#INTB_PORTS,(intreqr+1+_custom)
		beq	.end
		btst	#CIAICRB_SP,(ciaicr+_ciaa)
		beq	.end

		bsr	_KeyboardHandle

.end		move.w	#INTF_PORTS,(intreq+_custom)
	;to avoid timing problems on very fast machines we do another
	;custom access
		tst.w	(intreqr+_custom)
		rte 

_KeyboardHandle	movem.l	d0-d1/a0-a1,-(a7)
		lea	(_custom),a0
		lea	(_ciaa),a1
	;read keycode
		moveq	#0,d0
		move.b	(ciasdr,a1),d0
	;set output mode (handshake)
		or.b	#CIACRAF_SPMODE,(ciacra,a1)

		movem.l	d0/a0/a1,-(sp)
		move.l	gamePortsRoutine(pc),a0
		jsr	(a0)
		movem.l	(sp)+,d0/a0/a1

	;calculate rawkeycode
		not.b	d0
		ror.b	#1,d0

		cmp.b	(_keydebug,pc),d0
		bne	.1
		movem.l	(a7)+,d0-d1/a0-a1		;restore
	;transform stackframe to resload_Abort arguments
		move.w	(a7),(6,a7)			;sr
		move.l	(2,a7),(a7)			;pc
		clr.w	(4,a7)				;ext.l sr
		bra	_debug

.1		cmp.b	(_keyexit,pc),d0
		beq	_exit

	;better would be to use the cia-timer to wait, but we aren't know if
	;they are otherwise used, so using the rasterbeam
	;required minimum waiting is 85 µs, one rasterline is 63.5 µs
	;a loop of 3 results in min=127µs max=190.5µs
		moveq	#3-1,d1
.wait1		move.b	(vhposr,a0),d0
.wait2		cmp.b	(vhposr,a0),d0
		beq	.wait2
		dbf	d1,.wait1

	;set input mode
		and.b	#~(CIACRAF_SPMODE),(ciacra,a1)
		movem.l	(a7)+,d0-d1/a0-a1
		rts

;-----------------------------------------------------------------------------
load_hiscore
		movem.l	d0-a6,-(sp)

		lea	(file_hiscore,pc),a0
		WHDL	GetFileSize
		tst.l	d0
		beq	.end

		lea	(file_hiscore,pc),a0
		move.l	(pHiscore,pc),a1
		WHDL	LoadFile
	
		bsr	cryptrout	; <-- crypt routine
	
.end		movem.l	(sp)+,d0-a6
		rts


;-----------------------------------------------------------------------------;
fixHiscore_Save

		move.b	#13,(8,a2)	; orginal

		movem.l	d0-a6,-(sp)

		bsr	cryptrout	; <-- crypt routine for write 

		moveq	#HISCORE_SIZE,d0
		move.l	(pHiscore,pc),a1
		
		lea	(file_hiscore,pc),a0
		WHDL	SaveFile

		bsr	cryptrout	; <-- decrypt routine

		movem.l	(sp)+,d0-a6

		rts

;-----------------------------------------------------------------------------

cryptrout	movem.l	d0/a0,-(sp)

		move.l	(pHiscore,pc),a0

		moveq	#HISCORE_SIZE/4-1,d0

.loop		eor.l	#$9bedafec,(a0)+
		dbf	d0,.loop
		
		movem.l	(sp)+,d0/a0
		rts

;-----------------------------------------------------------------------------
find_hunk
		tst.l	d0	;test for hunk 0
		beq	.exit	;yes that's hunk is 0, so now we have 
				;in a1 - BCPL pointer to this hunk
		subq.l	#1,d0

.loop		adda.l	a1,a1
		adda.l	a1,a1	;BCPL * 4 = APTR of hunk
		move.l	(a1),a1	;get BCPL pointer to next hunk


	IFD DEBUG_MODE
		movem.l	d1,-(sp)

		move.l	a1,d1
		tst.l	d1
		bne	.ok

		movem.l	(sp)+,d1

		pea	(.msg_bad_hunk,pc)
		pea	TDREASON_FAILMSG
		bra	_exit
		
.msg_bad_hunk	dc.b	"Bad hunk number",0
	EVEN

.ok		movem.l	(sp)+,d1

	ENDC	;IFD DEBUG_MODE

		dbf	d0,.loop

.exit		rts

;-----------------------------------------------------------------------------
	INCLUDE	'ReadJoyPad.s' 
;-----------------------------------------------------------------------------

pHiscore	dc.l	0	;highscore adress
_dosbase:	dc.l	0
introSegList:	dc.l	0
gamePortsRoutine:	dc.l	0	;address of game keyboard decode routine
file_hiscore	dc.b	"Ugh.highs",0
	EVEN
intro:		dc.b	'loader',0
game:		dc.b	't',0

; ws_config menu
    EVEN

slv_config:
    dc.b    "C1:X:Unlimited lives:0;"
    dc.b    "C1:X:Unlimited energy:1;"
    dc.b    "C1:X:Disable pterodactyl:2;"
    dc.b    "C1:X:Invincible tree:3;"
    dc.b    "C2:B:Skip Intro;"
    dc.b    0

; skip intro related table
    EVEN

Control_for_Custom2_tag:
    dc.l    WHDLTAG_CUSTOM2_GET
Control_for_Custom2_value:
    dc.l    0
    dc.l    0			; TAG_DONE
