;	*****************************************************
;	*		CITADEL - main game code (v1.3).   			*
;	*	Copyrigt (C) Pawel Matusz (Kane) 1995,2022		*
;	* 	Version 1.3 coded 11.02.1994-xx.04.1995 		*
;	* 	Version 1.3 coded 08.01.2022-20.01.2022			*
;	*****************************************************
;Assembled using ASM-ONE 1.20+. Use "wb cyt.dat",s,end to create a deployment version
;After assembling this code is relocable although it uses some fixed addresses in CHIP

; for development set to 0. For deployment set to 1
IS_EXE:		equ	0

; build version, set to date+release for each deployed version
VERSION: 	SET	$22012601

STRUCTURE:	equ	$7f800			; 128 bytes available
ADDMEM:		equ	$7ffea			; 2nd 0.5 free memory
VBR_BASE:	equ	$7ffee			; VBR to use by the game
MC68000:	equ	$7fff2			; 0 - 68000, 1 - 68020+
MEMORY:		equ	$7fff8			; 1st 0.5 free memory
DEBUGDATA:	equ	$80000			; for debug only. Deployed versions should not use it

select_cache:	equ	0			;TODO: remove. 1-user selects cache

;BASEF1:		equ	$07e00000		; Fast on a real A4000
;BASEF1:		equ	$00400000		; Z2 fast (e.g. A500 which does not support Z3)
BASEF1:		equ	$40400000			; Z3 fast. First 0.5MB memory. Hopefully Fast 
BASEF2:		equ	BASEF1+$80000		; Z3 fast. Second 0.5MB memory. Hopefully Fast

CODESTART:	equ	$0000			; this is where the code is placed w.r.t. allocated memory start. CANNOT BE $1000 (???)

	IFEQ		IS_EXE
BASEC:		equ	$100000			; free 0.5 meg chip(A1200). If assembled on a machine with less than 1MB chip then change to 0
	ELSE
BASEC:		equ	$000000			; deployed (exe) version must always use lower chip to work with the rest of the game
	ENDC

UPDATE_TIME:	equ	6			; minimum period (in frames) between game logic updates. Default is 6.

	TTL		VIRTUAL_DESIGN_PRODUCTION
	JUMPPTR		S

;-----------------------------------------------------------------------------
ALL:		REG	d0-a6
WAITBLT:	MACRO
		btst.b	#14,2(a0)
		bne.s	*-6
		ENDM
VBLANK:		MACRO
		cmpi.b	#$ff,6(a0)
		bne.s	*-6
		cmpi.b	#$ff,6(a0)
		beq.s	*-6
		ENDM
SCROLL:		MACRO		; \1 - offset of text
		movem.l	d0/a1,-(sp)
		move	sv_TextOffsets+[[\1]*2],d0
		lea	sc_Text,a1
		lea	(a1,d0.w),a1
		move.l	a1,sc_TextAddr+4
		movem.l	(sp)+,d0/a1
		ENDM
SCROLL1:	MACRO			; d0 - offset of text
		movem.l	d0/a1,-(sp)
		add	d0,d0
		lea	sv_TextOffsets,a1
		move	(a1,d0.w),d0
		lea	sc_Text,a1
		lea	(a1,d0.w),a1
		move.l	a1,sc_TextAddr+4
		movem.l	(sp)+,d0/a1
		ENDM
SCROLL2:	MACRO		; \1 - address of LOCAL text
		move.l	a1,-(sp)
		lea		\1(pc),a1
		move.l	a1,sc_TextAddr+4
		move.l	(sp)+,a1
		ENDM
SOUND:		MACRO	;sample, voice(1,2,3,4), volume
		move.b	#\1,play_sample+[\2-1]
		move.b	#\3,play_volume+[\2-1]
		ENDM
SOUND2:		MACRO	;d0 - Xpos, d1 - Ypos of object
		bsr	Sound_Distance
		beq.s	.s1\@
		move.b	#\1,play_sample+[\2-1]
		move.b	d0,play_volume+[\2-1]
.s1\@:
		ENDM
SOUND3:		MACRO	;d0 - Xpos, d1 - Ypos of object, d2 - sample
		bsr	Sound_Distance2
		beq.s	.s2\@
		move.b	d2,play_sample+[\1-1]
		move.b	d0,play_volume+[\1-1]
.s2\@:
		ENDM
SOUND4:		MACRO	;d0 - Xpos, d1 - Ypos of object, d2 - sample
		bsr	Sound_Distance3
		beq.s	.s3\@
		move.b	d2,play_sample+[\1-1]
		move.b	d0,play_volume+[\1-1]
.s3\@:
		ENDM

; get timestamp
TIMESTAMP:	MACRO		; \1 - output register (long)
		move.l	$dff004,\1		; VHPOSR
		lsr.l	#8,\1
		andi	#$1ff,\1		; V position
		swap	\1
		move	lc_variables+lc_time+2(pc),\1		; frame nw
		swap	\1
ENDM

; get difference between 2 timestamps, in frames (M word) and scanlines (L word). Scanlines = 0..312
TSDELTA:	MACRO		;in: \1 - TS#1 (old), \2 - TS#2 (new)(longs). Out: \2 delta
		cmp		\1,\2
		bpl.s	.c1\@
		addi	#313,\2
		addi.l	#$10000,\1
.c1\@:	sub.l	\1,\2
ENDM

CNTSTART: MACRO		; \1 - counter index (0..15)
		lea		lc_variables(pc),a5
		tst		lc_debugOn(a5)
		beq.s	.c1\@
		moveq	#\1,d0
		bsr		DebugCntStart
.c1\@:		
ENDM


CNTSTOP: MACRO		; \1 - counter index (0..15)
		lea		lc_variables(pc),a5
		tst		lc_debugOn(a5)
		beq.s	.c1\@
		move	#\1,d0
		bsr		DebugCntStop
.c1\@:		
;		nop
;		nop
ENDM

; get random number (long). 
;in: \1 - ax register, \2 - dx register
;out: \2 - dx.l contains random value
RANDOM:	MACRO
		lea	seed(pc),\1
		move.l	(\1),\2
		add.l	\2,\2
		swap	\2
        add		$dff006,\2	; VHPOSR
		move.l	\2,(\1)
ENDM

;-----------------------------------------------------------------------------

		ORG		BASEF1+CODESTART
		LOAD	*
s:
		IFEQ	IS_EXE
		bra.s	.s1
		dc.b	0,"DEVELOPMENT VERSION - DOES NOT WORK WITH WHDLOAD",0
		even
.s1:	move.l	#BASEF1,MEMORY
		move.l	#BASEF2,ADDMEM
		move.l	#0,VBR_BASE
		move	#1,MC68000				; force 020
;		move	#0,MC68000				; force 68k
		lea		STRUCTURE,a1
		lea		(a1),a2
		moveq	#31,d0
.scs:	clr.l	(a2)+
		dbf		d0,.scs
		move	#6,(a1)			;size
		move	#1,2(a1)		;floor (1=floor, 0=no floor)
		move	#0,4(a1)		;details
		move	#-150,6(a1)		;energy
		move	#0,8(a1)		;bomb
		move	#0,10(a1)		;killed
		move	#0,12(a1)		;time
		move	#0,14(a1)		;guns - 18 words in total(6 guns * 3 bytes): damage, ammo provided from drops, current ammo
		move	#-10,16(a1)		; handgun ammo
		move.l	#0,18(a1)
		move	#-10,22(a1)		; shotgun ammo
		move.l	#0,24(a1)
		move	#-30,28(a1)		; machine gun ammo
		move.l	#0,30(a1)
		move	#-5,34(a1)		; flame thrower
		move.l	#0,36(a1)
		move	#-5,40(a1)		; blaster ammo
		move.l	#0,42(a1)
		move	#-2,46(a1)		; rocket launcher ammo
		move	#0,48(a1)		; last byte of guns
		move	#0,50(a1)		;difficulty (0=hard by default, 1=easy). 8 bytes free after this. Also 100-124 is free and initialised to 0
		move.b	#0,STR_DISTURBANCES(a1)		; ofs 52. On by default
		move.b	#0,STR_GODMODE(a1)			; ofs 53. Off by default
		; 52-58 free
		; 60-98 used by server (level progress data etc.)
		move	#1,60(a1)		; 60 - complex nr (1,2,...). 1 = dungeons
		move	#1,62(a1)		; 62 - level in complex (1,2,...)
		ENDC

		bra.s	.s2
		dc.b	0,"V1.30 BUILD ",VERSION>>28&$f+48, VERSION>>24&$f+48, ".", VERSION>>20&$f+48, VERSION>>16&$f+48, ".", VERSION>>12&$f+48, VERSION>>8&$f+48, ".", VERSION>>4&$f+48, VERSION&$f+48,0
		even
.s2:	lea		lc_FastMem1(pc),a2
		move.l	MEMORY,(a2)
		beq		.err					; memory cannot be 0
		lea		lc_FastMem2(pc),a2
		move.l	ADDMEM,(a2)
		beq		.err					; memory cannot be 0
		lea		lc_VbrBase(pc),a2
		move.l	VBR_BASE,(a2)
		lea		lc_CpuType(pc),a2
		move.l	MC68000,(a2)
		lea		STRUCTURE,a1
		lea		lc_Structure(pc),a2
		move.l	a1,(a2)
		
		move.l	lc_VbrBase(pc),a1
		lea		start(pc),a2
		move.l	a2,$bc(a1)
		trap	#15					; go to privileged mode
		move	do_JakiKoniec,d0	;- bad end, + good end, 0: quit by ESC
		rts
.err:	moveq	#0,d0				; quit code 0
		rts


start:		
		lea		$dff000,a0
		VBLANK
		move	#$7fff,$9a(a0)		;INTENA
		move	#$7fff,$9c(a0)		;INTREQ
		move	#$00ff,$9e(a0)		;ADKONR

		lea		start(pc),a1
		addi.l	#oryginal_data-start,a1
		lea		sv_DATA_AREA,a2
		move	#[[End_OData-Oryginal_Data]/2]-1,d0
.sc_CopyData:	move	(a1)+,(a2)+
		dbf		d0,.sc_CopyData

		move	lc_CpuType(pc),d0
		beq.s	.NoCache
		lea		lc_isCache(pc),a1
		move	#1,(a1)				; mark that there is a CPU cache present - will always be used
		lea		lc_c2pType(pc),a1
		move	#1,(a1)				; 1 = CPU C2P
		lea		lc_c2pTypePreferred(pc),a1
		move	#1,(a1)				; 1 = CPU C2P

		move.l	lc_FastMem2(pc),d0
		addi.l	#F2_ChunkyBufF,d0
		move.l	d0,sv_ChunkyBuffer

;		move	#1,sv_Mode		; cache draw_mode
		move	#1,sv_Buse		; buse and buse+2 : blitter off
		move	#1,sv_Buse+2

		clr.l	d0
		movec	CACR,d0			; switch on and clear cache
		lea		lc_cacr_copy(pc),a1
		move.l	d0,(a1)
        tst.w   d0            	;movec does not affect carry codes
        bmi.s   .cic_040       	;A 68040 with enabled cache!
		; do nto enable instruction burst as it actually slows the average speed down
        ;ori.w   #$0009,d0		; instruction cache enable + clear
        ;ori.w   #$0909,d0		; instruction cache enable + clear, 030 data cache enable + clear
        ;ori.w   #$1909,d0		; instruction cache enable + clear, 030 data cache enable + clear, 030 burst enable
        ;ori.w   #$3909,d0		; instruction cache enable + clear, 030 data cache enable + clear, 030 burst and writethrough enable
        ;ori.l   #$40003909,d0		; instruction cache enable + clear, 030 data cache enable + clear, 030 burst and writethrough enable, sync
        ori.w   #$0909,d0		; instruction cache enable + clear, 030 data cache enable + clear
		movec	d0,CACR
		bra.s   .cic_exit
.cic_040: dc.w    $f4b8         ;CPUSHA (IC)
.cic_exit:
		bra.s	.InitStruct

.NoCache:	; this is a plain 68000 so no cache etc.
		lea		lc_isCache(pc),a1
		move	#0,(a1)				; mark that there is no CPU cache present
		lea		lc_c2pType(pc),a1
		move	#0,(a1)				; 0 = blitter C2P
		lea		lc_c2pTypePreferred(pc),a1
		move	#0,(a1)				; 0 = blitter C2P
;		move	#0,sv_Mode		; cache draw_mode
		move.l	#0,sv_Buse		; buse and buse+2 : blitter on
		move.l	#sv_ChunkyBufC,sv_ChunkyBuffer	; use chip chunky buffer with blitter

.InitStruct:
		move.l	lc_Structure(pc),a1
		move	(a1),sv_size
		move	(a1),sv_size+2
		move	(a1),cc_requesttab+2
		move	2(a1),sv_Floor
		move	4(a1),sv_Details
		move	6(a1),sv_Energy
		move	8(a1),sv_Glowica
		move	50(a1),sv_Difficult
		move	#-10,16(a1)		; handgun ammo
		move	#-10,22(a1)		; shotgun ammo
		move	#-30,28(a1)		; machine gun ammo
		move	#-5,34(a1)		; flame thrower ammo
		move	#-5,40(a1)		; blaster ammo
		move	#-2,46(a1)		; rocket launcher ammo
		lea		14(a1),a1
		lea		sv_GUNS,a2
		moveq	#17,d0
.GunCop: move	(a1)+,(a2)+
		dbf	d0,.GunCop

		lea		lc_ledChange(pc),a1		; mark leds to be updated
		st		(a1)

		bsr		SeedRandom

		lea	lc_soundList(pc),a3		;fix sounds- put a 0 on the first word
.fix_s:	move.l	(a3)+,d0
		beq.s	.fe
		move.l	d0,a1
		move	#0,(a1)
		lea	4(a3),a3		; skip freq/len
		bra.s	.fix_s
.fe:

		lea	sv_UserMap,a1		;clr user map
		moveq	#127,d0
.ClrMap: move.l	#0,(a1)+
		dbf	d0,.ClrMap

		move.l	#0,sv_cards+2		;clear cards - add number and current count
		move.l	#0,sv_cards+8
		move.l	#0,sv_cards+14

		lea		sv_LevelData,a1			; set player starting position based on the one in the new map
		move	16(a1),d0
		mulu	#1024,d0
		addi	#512,d0
		move	d0,sv_PosX
		move	18(a1),d0
		mulu	#1024,d0
		addi	#512,d0
		move	d0,sv_PosY
		move	20(a1),sv_Angle

		move	sv_Energy,d0
		neg		d0
		cmpi	#999,d0			; limit HP to 999 - just in case as it should never be more anyway
		bmi.s	.ne1
		move	#999,d0
.ne1:	tst		sv_DIFFICULT		;ustaw poziom trudnosci
		beq.s	.ne2			; 0 = hard (default)
		cmpi	#150,d0			; on easy start with no less than 150 energy
		bpl.s	.ne2
		move	#150,d0
.ne2:	neg	d0
		move	d0,sv_Energy

		tst		sv_DIFFICULT		;ustaw poziom trudnosci
		beq.s	.sv_DIF			; 0 = hard (default)
	; easy settings
		lea		sv_enemyDATA,a1
		lea		sv_MAP,a2
		moveq	#62,d7
.sv_UsunEn:	move	(a1),d0			;usun co 4 przeciwnika
		beq.s	.sv_u1
		move.l	#0,(a1)
		moveq	#0,d0
		move	4(a1),d0
		moveq	#0,d1
		move	6(a1),d1
		divu	#1024,d0
		divu	#1024,d1
		mulu	#512,d1
		lsl	#3,d0
		addi	#7,d0
		add	d0,d1
		move.b	#0,(a2,d1.w)
.sv_u1:		lea	16*4(a1),a1
		dbf	d7,.sv_UsunEn

		lea		sv_levelDATA,a1
		move	8(a1),d0		;EASY agresja na 1/3 (was: 4/5)
		mulu	#1,d0
		divu	#3,d0
		move	d0,8(a1)
		move	14(a1),d0
		mulu	#1,d0
		divu	#3,d0
		move	d0,14(a1)
		bra.s	.sv_setdone

	; difficult settings
.sv_DIF:
		lea		sv_levelDATA,a1
		move	8(a1),d0		;DIFFICULT agresja na 50% (was: 4/5)
		lsr		#1,d0
		move	d0,8(a1)
		move	14(a1),d0
		lsr		#1,d0
		move	d0,14(a1)


		move.l	lc_FastMem2(pc),a1		; clear FAST2 tables
		move.l	#F2_ClearTo1/16,d1		; might go a bit above the end address but that should also be empty
		clr.l	d0
.cf2a:	REPT	4
		move.l	d0,(a1)+
		ENDR
		dbf		d1,.cf2a

.sv_setdone:
		move.l	sv_Screen,a1		;copy window to screen 2
		move.l	sv_Screen+4,a2
		move	#2000-1,d7			; 160*40*5 in longs and each long is copied 4x in the loop
.sv_CopWindow:	rept	4
		move.l	(a1)+,(a2)+
		endr
		dbf	d7,.sv_CopWindow

		lea		Screen1+[sv_Upoffset*5*row],a1
		lea		sv_WindowSav,a2
		move	#[130*5]-1,d7
.sv_SavWindow:	move.l	(a1)+,(a2)+		;save only outer parts of the middle part of the window
		move.l	(a1)+,(a2)+
		lea		24(a1),a1
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		dbf	d7,.sv_SavWindow

		lea	sv_ScrollArea-34,a1	;clear scroll area
		moveq	#0,d0
		moveq	#6,d1
.sc_c0: moveq	#7,d2
.sc_Clear: move.l	d0,(a1)+
		dbf		d2,.sc_Clear
		move	d0,(a1)+
		lea	[5*40]-34(a1),a1
		dbf	d1,.sc_c0

		lea	sv_ObjectTab,a1		;clr objects
		moveq	#29,d1
.sc_Clear2:	move	#0,(a1)
		lea	12(a1),a1
		dbf	d1,.sc_Clear2


		lea	sv_Counter1,a1		;save counters
		lea	sv_Counter2,a2
		lea	sv_C1Save,a3
		cmpi.l	#"KANE",18*6*5(a3)		; do not save counters if they have already been saved. Required by the server which does not reload all gfx.
		beq.w	.sc_NoSave
		lea	sv_C2Save,a4
		moveq	#[18*5]-1,d0
.sc_sc0: moveq	#5,d1
.sc_SaveCou:
		move.b	(a1)+,(a3)+
		move.b	(a2)+,(a4)+
		dbf		d1,.sc_SaveCou
		lea	40-6(a1),a1
		lea	40-6(a2),a2
		dbf	d0,.sc_sc0
		move.l	#"KANE",(a3)

		lea	sv_Weapon,a1
		lea	sv_ItemSav,a2
		move	#[27*5]-1,d0
.sc_SavItem:	move.b	(a1),(a2)+		;save Item
		move.b	1(a1),(a2)+
		move.b	2(a1),(a2)+
		move.b	3(a1),(a2)+
		lea	row(a1),a1
		dbf	d0,.sc_SavItem

		lea	sv_Compas,a1
		lea	sv_CompasSav,a2
		moveq	#26,d0
.sc_SavComp:	move.l	(a1),(a2)+		;save compass
		move.l	row(a1),(a2)+
		move.l	2*row(a1),(a2)+
		move.l	3*row(a1),(a2)+
		move.l	4*row(a1),(a2)+
		lea	5*row(a1),a1
		dbf	d0,.sc_SavComp

		lea	sv_CardCnt,a1
		lea	sv_CardSav,a2
		moveq	#23,d0
.sc_SavCard:	move.b	(a1),(a2)+		;save card counter
		move.b	row(a1),(a2)+
		move.b	2*row(a1),(a2)+
		move.b	3*row(a1),(a2)+
		move.b	4*row(a1),(a2)+
		lea	5*row(a1),a1
		dbf	d0,.sc_SavCard

		lea	sv_Heart,a1
		lea	sv_HeartSav,a2
		moveq	#[12*5]-1,d0
.sc_SavHt:	move.b	(a1)+,(a2)+		;save Heart backgnd
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		lea	row-5(a1),a1
		dbf	d0,.sc_SavHt
.sc_NoSave:

		lea	sv_C2Save,a1		;zero counter first
		lea	sv_Counter2,a2
		moveq	#[18*5]-1,d0
.ci_ResCou:	move.l	(a1)+,(a2)+
		move.w	(a1)+,(a2)+
		lea	40-6(a2),a2
		dbf	d0,.ci_ResCou

		lea	sv_ItemSav,a1
		lea	sv_Weapon,a2
		move	#[27*5]-1,d0
.sc_ResItem:	move.b	(a1)+,(a2)		;zero Item
		move.b	(a1)+,1(a2)
		move.b	(a1)+,2(a2)
		move.b	(a1)+,3(a2)
		lea	row(a2),a2
		dbf	d0,.sc_ResItem

		lea		start(pc),a1		;copy copper to chip
		addi.l	#copper-start,a1
;		move.l	#copper-start,d0
;		lea	(a1,d0.l),a1
;		lea	copper(pc),a1		;copy copper to chip
		lea		RealCopper,a2
		move	#[[EndCopper-Copper]/2]-1,d0
.sc_CopyCop:	move	(a1)+,(a2)+
		dbf		d0,.sc_CopyCop

		lea		start(pc),a1		;copy offsets to chip
		addi.l	#st_offsets-start,a1
;		move.l	#st_offsets-start,d0
;		lea	(a1,d0.l),a1
		lea		sv_Offsets,a2
		move	#[[End_offsets-st_Offsets]/2],d0
.sc_CopyOff:	move	(a1)+,(a2)+
		dbf		d0,.sc_CopyOff


		bsr		make_PLANES_pass
		bsr		sv_SetWindowSize_pass	;Window prefs
		move	#$fd,d0				; hand
		bsr		ci_NewWeapon		;set hand
		bsr		tc_DrawCardCnt

		lea	$dff000,a0
		move	#$7fff,$96(a0)
		move	#$8240,$96(a0)		;DMACON blitter on
		lea		sv_OldMouse,a1
		move	10(a0),(a1)			;remember starting mouse position
		waitblt

		VBLANK
		move	#$7fff,$96(a0)		;DMACON
		move	#$7fff,$9a(a0)		;INTENA
		move	#$7fff,$9c(a0)
		move.l	lc_VbrBase(pc),a1
		lea		lc_OldLev3(pc),a2
		move.l	$6c(a1),(a2)		;set lev3 interrupt
		lea		NewLev3(pc),a2
		move.l	a2,$6c(a1)

		lea		lc_OldLev2(pc),a2
		move.l	$68(a1),(a2)		;set lev2 key interrupt
		lea		NewLev2(pc),a2
		move.l	a2,$68(a1)

		VBLANK
		move.l	#Copper0,$80(a0)	; blank copper - black screen
		move	#0,$88(a0)
		bsr		DrawBomb_pass
		move	#1,db_napisz

		VBLANK
		move	#$83c0,$96(a0)		; DMACON dma and stuff
		move	#$c028,$9a(a0)		; SET+INTEN, VERTB and PORTS

		lea		lc_variables(pc),a6
		clr.l	lc_time(a6)			; zero seconds in level
		clr		lc_updateTimer(a6)
		move.l	#RealCopper,$80(a0)
		move	#0,$88(a0)
		bsr		p_SetColors			; fade in colours
;		SCROLL	0					; "welcome"
		move.l	lc_Structure(pc),a1
		cmpi.l	#$00010001,60(a1)
		bne.s	.nw1				; show "Citadel Remastered" only before Dungeon level 1
		SCROLL2	txt_welcome
		bra.s	.nw2
.nw1:	SCROLL2	txt_welcome_bk
.nw2:
		bsr		swapScreen			; this sets up copper and prepares the screen for "teleporting" in
	
		TIMESTAMP	d0
		move.l	d0,lc_fps+4(a6)		; get and store initial game frame start timestamp

	
;-------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------
MAIN_LOOP:
;-------------------------------------------------------------------------------------------------
;-------------------------------------------------------------------------------------------------
		lea		$dff000,a0
		lea		lc_variables(pc),a6
;		move	#0,lc_fps(a6)				; reset fps count
		move	#0,lc_updateFrame(a6)		; clear logic update flag
		cmpi	#UPDATE_TIME,lc_updateTimer(a6)
		bmi.s	.sv_noActionUpdate0
		move	#1,lc_updateFrame(a6)		; mark frame for anim/enemy/other updates
.sv_d1:	subi	#UPDATE_TIME,lc_updateTimer(a6)
		bpl.s	.sv_d1
		addi	#UPDATE_TIME,lc_updateTimer(a6)
.sv_noActionUpdate0:

		lea		cc_RequestTab,a1
		tst		(a1)			;was ESC pressed?
		beq.s	ml2
		moveq	#0,d0
		bra.w	sv_quit
ml2:
		; this is (I think) to detect release of RMB before next press
		tst		8(a1)			; rf2 flag? used ONLY here
		beq.s	.nl_4
		btst.b	#2,$16(a0)		; right mouse/joy buttons
		beq.s	.nl_5
		btst.b	#7,$bfe001
		beq.s	.nl_5
		tst		cc_MoveTab+12	; enter / fire from keyboard
		bne.s	.nl_5
		move	#0,8(a1)
		bra.s	.nl_5
.nl_4:	btst.b	#2,$16(a0)		;pressed?
		beq.s	.nl_6
		tst		cc_MoveTab+12
		bne.s	.nl_6
		btst.b	#7,$bfe001
		bne.s	.nl_5
.nl_6:	move	#1,6(a1)		; RMB flag
		move	#1,8(a1)
.nl_5:

		tst		sv_MapOn
		bne		ServeMap		;if map initialized

		tst		sv_PAUSE
		beq.s	NOT_Paused
		VBLANK					; since the game loop is not running now, skip a frame
		TIMESTAMP	d0
		move.l	d0,lc_fps+4(a6)					; get and store start timestamp after pause
		subi	#1,sv_Pause+2
		bne		MAIN_LOOP
		move	#750,sv_Pause+2
		SCROLL	41				;"game is paused"
		bra		MAIN_LOOP

NOT_Paused:	
		move	cc_RequestTab+2,d0	;window size button (F1-F8) pressed? Returns 2..6 for (F1-F5) and 7..9 for (F6-F8)
		cmp		sv_Size+2,d0		; is it the same as current size?
		beq.w	sv_SizeOk
		move	d0,sv_Size+2		; size can be 1-8. 6,7 and 8 are stretched. Note that size 1-> sv_size=2 , size 5 -> sv_size=6
		moveq	#0,d1				; by default no screen stretch
		cmpi	#7,d0
		bmi.s	sv_SizeNoStr		;if <1,6>
		cmpi	#7,d0
		bne.s	.scr7
		move	sv_NtscPal,sv_NtscPal+2	; this is for scr 7
		bra.s	.scr8
.scr7:	move	#32,sv_NtscPal+2	;force pal on scr 6

.scr8:	move	#1,sv_Buse			; if stretching in place then do not use blitter
		move	#-2,cc_RequestTab+4	; blitter use - prepare to fix later
		eori	#1,sv_Buse+2
		bsr		sv_MakeWidthTab_pass
		bsr		mk_FixFloorMod_pass
		moveq	#5,d0		; size 5
		moveq	#1,d1		; stretch on
		bra.s	sv_sns
sv_SizeNoStr:
		move	sv_NtscPal,sv_NtscPal+2
sv_sns:	move	d0,sv_Size
		move	d1,sv_StrFlag		;do screen stretch?
		beq.s	sv_Sizeok0
		move	d1,sv_StrFlag+2		;change request
sv_SizeOk0:	
		bsr		sv_SetWindowSize_pass			; change window size if requested
		move	sv_NtscPal+2,$dff1dc
		move	cc_RequestTab+2,d0
		addi	#21,d0
		SCROLL1							; "window size X"
		TIMESTAMP	d0
		move.l	d0,lc_fps+4(a6)					; refresh - get and store new fps start timestamp after screen change

sv_SizeOk:	
		tst		sv_StrFlag				; if stretching happening then blitter cannot be used
		bne.w	sv_BlitUok
		move	cc_RequestTab+4,d0		; user triggered blitter on/off change - allow user to change blitter use
		beq.w	sv_BlitUoK
		move	#0,cc_RequestTab+4
		eori	#1,sv_Buse+2
		tst		d0
		bmi.s	.sv_SO2					;no scroll
		tst		sv_Buse+2
		beq.s	.sv_SO1
		SCROLL	36
		bra.s	.sv_SO2
.sv_SO1:	SCROLL	35
.sv_SO2:	move	sv_Buse+2,sv_Buse		; execute blitter use change
;		move	lc_CpuType(pc),d1
;		beq.s	.sv_no20
;		move	#1,sv_Buse		;blitter off if not 020+to force using code tabs
;		move	#1,sv_Buse+2
;.sv_no20:
		bsr		sv_MakeWidthTab_pass
		bsr		mk_FixFloorMod_pass

sv_BlitUoK:	
		move	sv_flag+6,d0
		beq.s	.noNewWeapon
		clr		sv_flag+6
		bsr		ci_NewWeapon
.noNewWeapon:

		tst		sv_WalkSpeed+6		;max speed if potion !
		beq.s	sv_NotSpeed
		move	sv_WalkSpeed+2,sv_WalkSpeed
		subi	#1,sv_WalkSpeed+6
		bne.s	sv_NotSpeed
		move	sv_WalkSpeed+4,sv_WalkSpeed+2
		bra.s	sv_startframe
sv_NotSpeed:
		lea		sc_PosTab(pc),a1	;speed & exhaust
		move	2(a1),d0
		addi	#288,d0				;max 512
		move	sv_WalkSpeed+2,d1
		mulu	d0,d1
		lsr.l	#8,d1
		lsr	d1						;/512
		move	d1,sv_WalkSpeed
		
;---------------------------------------------------------
;------- here starts the actual frame drawing code ------
sv_startFrame:
	CNTSTART 3
		bsr		mc_clearScreen	; clear screen
	CNTSTOP 3
		bsr		cc_FixKeys		; moved from L3 - hopefully will not cause issues 
		bsr		sv_joystick		; get additional joy values and move player
		bsr		sv_Border		; clip movements - no wall passing etc.

		lea		lc_variables(pc),a6
		tst		lc_updateFrame(a6)
		beq		.sv_noActionUpdate1
	CNTSTART 4
		bsr		CheckCodes
		bsr		OpenCloseDoors
		bsr		Take_Items
		bsr		Test_Counters
		bsr		Move_Enemy
	CNTSTOP 4

		lea		lc_variables(pc),a6
		move	lc_kickback(a6),d0
		beq.s	.sv_noKickback
		subi	#2,d0
		bpl.s	.sv_k2
		clr		d0
.sv_k2:	move	d0,lc_kickback(a6)
.sv_noKickback:

		tst		sv_SpaceOn
		beq.s	.sv_NoSpace
		bsr		Check_RMB		;if space - hand used
		bra.s	.sv_NotRMB
.sv_NoSpace:	tst.l	cc_RequestTab+6
		beq.s	.sv_NotRMB
		bsr		Check_RMB		;shots, etc.
.sv_NotRMB:	
		bsr		ServePriorDoor

		tst		sv_Flag+8		; launcher reload timer
		beq.s	.sv_Lok
		subi	#1,sv_Flag+8
.sv_Lok:

		tst		do_bron				; check reload weapon delay on finding a weapon (so it does not play at the same some as "ooo")
		beq.s	.sv_noBronSound
		subi	#1,do_bron
		bne.s	.sv_noBronSound
		SOUND	12,1,50				; reload sound
.sv_noBronSound:

.sv_noActionUpdate1:

		; Remove markers for all collumns drawn - so that new ones can be drawn
		move.l	sv_Consttab+8,a4	;scr tab middle
		lea		64*192(a4),a4
		move	sv_Consttab+28,d0
		moveq.l	#-1,d1
sv_Cloop0:	rept	4			;clear row map.
		move.l	d1,(a4)+
		endr
		dbf	d0,sv_Cloop0

		move.l	#sv_ZeroTab,sv_ZeroPtr				; tab for ZeroWalls

		lea		$dff000,a0
		waitblt									; make sure any operations such as floor filling etc. completed.
	CNTSTART 5
		bsr		ShowFloor							; draw floors
	CNTSTOP 5
	CNTSTART 6
		bsr		DrawAll								; draw walls and all - but not transparent (zero) walls.
	CNTSTOP 6
		move.l	sv_ZeroPtr,d0
		cmpi.l	#sv_ZeroTab+[14*8*192],d0
		bmi.s	.NicTo
		move.l	#sv_ZeroTab+[14*8*192],sv_ZeroPtr
.NicTo:		
	CNTSTART 7
		bsr		ShowZeroWalls							;add zero (transparent) walls
	CNTSTOP 7

		lea		lc_variables(pc),a6
		tst		sv_SzumTime
		beq.s	.sv_NoSzum
		tst		lc_updateFrame(a6)
		beq.s	.sv_noActionUpdate2
		subq	#1,sv_SzumTime
.sv_noActionUpdate2:
		move.l	lc_Structure(pc),a1
		tst.b	STR_DISTURBANCES(a1)
		bne.s	.sv_NoSzum
		bsr.w	sv_MAKE_SZUM
.sv_NoSzum:

		move.l	lc_Structure(pc),a2
		tst.b	STR_CROSSHAIRS(a2)
		bne.s	.noCrosshairs
		bsr		drawCrosshairs
.noCrosshairs:

	CNTSTOP 1			; this measures everything without the wait for sync
		; make sure previous screen swap completed before doing the c2p
.sv_ws:	tst		lc_swapScreen(a6)				; check if screen has been swapped so that the next C2P can be executed
		bne.s	.sv_ws
	CNTSTART 1

	CNTSTART 9
		move.l	sv_Screen,a1					; always draw on the first screen (second is displayed)
		add.l	sv_offset,a1					; start of the draw window - offset Y*5 + X
		bsr		sv_C2PCcopy						; C2P copy to screen
	CNTSTOP 9

		; calculate FPS
		lea		lc_variables(pc),a6
		TIMESTAMP	d1							; get game frame end timestamp
		move.l	lc_fps+4(a6),d0					; previously stored frame start timestamp
		move.l	d1,lc_fps+4(a6)					; store end timestamp as new start timestamp
		TSDELTA		d0,d1						; difference in frames (H word) and scanlines (L word)
		move.l	d1,lc_fps(a6)					; store fps in frames (H word) and scanlines (L word)
		move.l	d1,d0
		clr		d1
		bsr		TsAverageAdd					; add TS to average table

		tst		sv_Teleport
		beq.s	.sv_NoTel
		bsr		TELEPORT						; has to be done before screen swap
		TIMESTAMP	d0
		move.l	d0,lc_fps+4(a6)					; refresh - get and store new start timestamp after teleport
.sv_NoTel:	
		st		lc_swapScreen(a6)				; trigger screen swap in next L3 interrupt

		; ---- anim and compass updates are done only every UPDATE frame
		tst		lc_updateFrame(a6)
		beq.s	.sv_noActionUpdate3
		tst		lc_fpsOn(a6)
		beq.s	.sv_nofps
		bsr		FPSShow
.sv_nofps:
		tst		lc_debugOn(a6)
		beq.s	.sv_nodebug
		bsr		DebugCntShow
.sv_nodebug:
	CNTSTART 8
		bsr		compass			; refresh compass
		bsr		sv_DoAnims		; animate walls (change between frames) and advance enemy walk animation
		bsr		Anim_Objects	; animate moving objects such as doors, projectiles etc.
		bsr		UpdateMap		; update user display-map
	CNTSTOP 8

.sv_noActionUpdate3:

		lea		lc_variables(pc),a6		; maybe not needed?
		; scale walking and rotation speed
		move.l	lc_fps(a6),d0		; take just the full frames part
		bsr		TsRebaseTo256		; convert TC so word with frames on MSB and fractions on LSB
		add		#$100,d0			; to prevent having 0 frames on fast machines
		move	d0,d1
		cmpi	#$700,d0			; scale walkspeed based on fps - max 7
		bmi.s	.sv_nofpscorrect1
		move	#$700,d0
.sv_nofpscorrect1:
		mulu	#40,d0
		lsr.l	#8,d0				; remove fractions
		move	d0,sv_WalkSpeed+2
		lsr		#7,d1
		cmpi	#13,d1				; scale rotspeed based on fps
		bmi.s	.sv_nofpscorrect2
		moveq	#12,d1
.sv_nofpscorrect2:
		move	d1,sv_RotSpeed

	
		tst		sv_EndLEvel
		BEQ		MAIN_LOOP
		bsr		EndLevel		;returns +1 finished OK, -1 dead (d0)
		BEQ		MAIN_LOOP


sv_quit:
		lea		$dff000,a0
		lea		lc_variables(pc),a6
		move	d0,do_JakiKoniec		; +1 finished OK, -1 dead, 0: quit by pressing ESC
		move	#0,sv_EndLevel
		VBLANK
		move	#$7fff,$9a(a0)			; INTENA - disable all interrupts
		move	#$7fff,$9c(a0)			; INTREQ - clearpending interrupts
		move.l	#copper0,$80(a0)
		move	#0,$88(a0)
		move	#32,$1dc(a0)		;fix PAL
		VBLANK

		lea		sv_WindowSav,a1
		move.l	sv_Screen,a2
		move.l	sv_Screen+4,a3
		addi.l	#[sv_Upoffset*5*row],a2
		addi.l	#[sv_Upoffset*5*row],a3
		moveq	#0,d0
		move	#[130*5]-1,d7
.sv_GetWindow:	move.l	(a1),(a2)+		;get background
		move.l	(a1)+,(a3)+
		move.l	(a1),(a2)+
		move.l	(a1)+,(a3)+
		REPT	6
		move.l	d0,(a2)+
		move.l	d0,(a3)+
		ENDR
		move.l	(a1),(a2)+
		move.l	(a1)+,(a3)+
		move.l	(a1),(a2)+
		move.l	(a1)+,(a3)+
		dbf	d7,.sv_GetWindow

		move.l	lc_Structure(pc),a1		;update structure in prep for exit
		move	sv_size,(a1)
		move	sv_Floor,2(a1)
		move	sv_Details,4(a1)
		move	sv_Energy+2,6(a1)
		move	sv_Glowica,8(a1)
		move	sv_Nrkilled,10(a1)
		move.l	lc_time(a6),d0
		divu	#50,d0					; calculate seconds in level (assumes PAL = 50Hz)
		move	d0,12(a1)
		lea		14(a1),a1
		lea		sv_GUNS,a2
		moveq	#17,d0
.GunCop:	move	(a2)+,(a1)+
		dbf	d0,.GunCop

		move	lc_CpuType(pc),d0
		beq.s	.noCache2
		move.l	lc_cacr_copy(pc),d0
		movec	d0,CACR
.noCache2:

		IFEQ	IS_EXE
		move.l	lc_VbrBase(pc),a1
		move.l	lc_OldLev2(pc),$68(a1)
		move.l	lc_OldLev3(pc),$6c(a1)
		move	#$83f0,$96(a0)
		move	#$e02c,$9a(a0)
		ENDC
		rte


;-------------------------------------------------------------------
;-------------------------------------------------------------------
;-------------------------------------------------------------------
NewLev3:	movem.l ALL,-(sp)

		move	$dff01e,d0				; INTREQQ - which L3 interrupt was raised?
		andi	#$20,d0
		bne.s	interruptL3Vertb

;blitter finished interrupt
		;blitter code here
		andi	#$40,d0
		beq.s	interruptL3Coper
		movem.l	(sp)+,ALL
		move	#$40,$dff09c			; clear BLIT INREREQ
		move	#$40,$dff09c			; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

; coprocessor L3 interrupt do nothing
interruptL3Coper:
		movem.l	(sp)+,ALL
		move	#$10,$dff09c			; clear COPER INREREQ
		move	#$10,$dff09c			; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;vertical blank interrupt
interruptL3Vertb:
		lea		lc_variables(pc),a6
		tst		lc_swapScreen(a6)		; swap in-gae screens?
		beq		.noScreenSwap
		clr		lc_swapScreen(a6)
		bsr		swapScreen
.noScreenSwap:

		addi.l	#1,lc_time(a6)			; total frames elapsed in level
		addi	#1,lc_updateTimer(a6)		; frames - this is used to decide on UPDATE time in the main loop. Reset in the main loop.

	CNTSTART 2
		tst		do_Flash
		beq.s	.nl_2
		move	#$0f0,RealCopper+2		; flash green
		subi	#1,do_Flash
		bne.s	.nl_2
		move	#0,RealCopper+2
.nl_2:	

		bsr		sc_DoScroll				; move scroll

		tst		sv_PAUSE
		bne.s	.nl_3
		bsr		sv_CheckMouse			; check mouse movements
		bsr		Draw_Heart				; TODO review performance of this
.nl_3:	

		move	sv_ChaosAddr,d0
		beq.s	.noWeaponAnim
		bsr		ci_DrawWeapon			; by-pixel drawing of new weapon. This has to stay here.
.noWeaponAnim:

		tst		do_pikaj			; check/decrease beeping timer - play if energy is very low
		beq.s	.nl_9
		subi	#1,do_pikaj+2
		bne.s	.nl_9
		move	#55,do_pikaj+2
		SOUND	22,1,50				
.nl_9:	
		bsr	PLAY_SOUND

.nl_end:
	CNTSTOP 2

		movem.l	(sp)+,ALL
		move	#$20,$dff09c
		move	#$20,$dff09c		; double just in case to prevent any error in emu or fast CPU from calling the int again too fast
		nop
		rte

;-------------------------------------------------------------------
; swap screens and set up address in copperlist accordingly
swapScreen:
		move.l	sv_screen,d0			; swap screens. Screen will be the new buffer and screen+4 the one currently displayed
		move.l	sv_screen+4,sv_screen
		move.l	d0,sv_screen+4

		tst		sv_StrFlag				; is screen sretched?
		bne.s	.stretched

		lea		copper1_std,a1			; modify standard copper
		cmpi	#screen1&$ffff,d0
		bne.s	.c1s2
		move.l	#cop1_screen1,d0
		bra.s	.c1c
.c1s2:	move.l	#cop1_screen2,d0
		bra.s	.c1c

.stretched:
		lea		copper2_str,a1			; modify stretched copper
		cmpi	#screen1&$ffff,d0
		bne.s	.c2s2
		move.l	#cop2_screen1,d0
		bra.s	.c1c
.c2s2:	move.l	#cop2_screen2,d0
.c1c:	move	d0,6(a1)				; only modify copper jump addresses (this is faster and more "atomic" than changing all addresses)
		swap	d0
		move	d0,2(a1)
		
		rts

;-------------------------------------------------------------------
;if RMB has been pressed...
; shoot, use weapons
check_RMB:	movem.l	ALL,-(sp)
		move	#1,sv_HitFlag
		lea	sv_Items,a1
		move	(a1),d0					; selected item (*6)
		tst	sv_SpaceOn
		bne.s	rm_HandUsed
		cmpi	#3*6,d0					; is it machine gun?
		bne.s	RMB_other
		move	#0,cc_RequestTab+6
		bra	rm_MachineGun

RMB_other:	tst	cc_RequestTab+6
		beq.s	RMB_End
		move	#0,cc_RequestTab+6
		tst	d0
		beq.s	rm_HandUsed
		cmpi	#6,d0
		beq	rm_HandGunUsed
		cmpi	#2*6,d0
		beq	rm_ShotGunUsed
		cmpi	#4*6,d0
		beq	rm_FlamerUsed
		cmpi	#5*6,d0
		beq	rm_BolterUsed
		cmpi	#6*6,d0
		beq	rm_LauncherUsed
		cmpi	#7*6,d0
		beq	rm_CardUsed
		cmpi	#8*6,d0
		beq	rm_CardUsed
		cmpi	#9*6,d0
		beq	rm_CardUsed

RMB_End:	move	#0,sv_HitFlag
		movem.l	(sp)+,ALL
		rts

;-------------------------------------------------------------------
rm_HandUsed:	move	#0,sv_SpaceOn		;clr space_used
		move	sv_Angle,d0
		addi	#64,d0
		andi	#$1fe,d0
		moveq	#3-1,d1			;W for 0 degrees
.rm_FindDir:	addq	#1,d1
		subi	#128,d0
		bpl.s	.rm_FindDir
		andi	#3,d1

		lea	sv_Map,a1
		addi	sv_MapPos,d1		;your map location
		move.b	(a1,d1.w),d0		;wall you're facing
		bne	rm_FacingWall
		andi	#$fffc,d1		;eliminate direction
		move.b	7(a1,d1.w),d0
		beq.s	rm_Item
		SCROLL	70			;enemy
		bra	rm_HandEnd
rm_item:	move.b	6(a1,d1.w),d0
		andi	#31,d0
		beq.s	rm_Trup
		SCROLL	43			;item
		bra	rm_HandEnd
rm_Trup:	move.b	5(a1,d1.w),d0
		andi	#$e0,d0
		beq.s	rm_Column
		SCROLL	46			;trup
		bra	rm_HandEnd
rm_Column:	move.b	5(a1,d1.w),d0
		andi	#$1f,d0
		beq.s	rm_Nothing
		cmpi	#3,d0
		bpl.s	rm_Column2
		SCROLL	45			;non-passable column
		bra	rm_HandEnd
rm_Column2:	SCROLL	44			;normal column
		bra	rm_HandEnd
rm_Nothing:	SCROLL	49			;nothing here
		bra	rm_HandEnd


rm_FacingWall:	move	d0,d2
		andi	#$3e,d0			;only wall
		cmpi	#54,d0
		beq.s	rm_Nothing		;passable wall
		cmpi	#30,d0
		beq.s	rm_ClosedDoor
		cmpi	#36,d0
		bne.s	rm_OpenedDoor
rm_ClosedDoor:	SCROLL	50			;closed door
		bra	rm_HandEnd
rm_OpenedDoor:	cmpi	#32,d0
		beq.s	rm_OpenedDoor1
		cmpi	#38,d0
		bne.s	rm_AnimDoor
rm_OpenedDoor1:	SCROLL	51			;opened door
		bra.w	rm_HandEnd
rm_AnimDoor:	cmpi	#34,d0
		beq.s	rm_AnimDoor1
		cmpi	#40,d0
		bne.s	rm_SwitchOUT
rm_AnimDoor1:	SCROLL	52			;opening door
		bra.w	rm_HandEnd


rm_SwitchOUT:	cmpi	#44,d0
		bne.s	rm_SwitchIN
		moveq	#46,d0
		moveq	#0,d2			;S.in table
		bra.w	rm_ServeSwitch
rm_SwitchIN:	cmpi	#46,d0
		bne.s	rm_ChkBlood
		moveq	#44,d0
		moveq	#-2,d2			;seek S.out table
		bra.w	rm_ServeSwitch


rm_ChkBlood:	move	d1,d0
		andi	#$fffc,d1
		move.b	4(a1,d1.w),d3		;get slots
		not	d0
		andi	#3,d0
		add	d0,d0
		lsr	d0,d3
		andi	#3,d3
		subq	#1,d3			;if 1
		bne.s	rm_NotSlot1
		SCROLL	55			;empty card slot
		bra.s	rm_HandEnd
rm_NotSlot1:	subq	#1,d3			;if 2
		bne.s	rm_NotSlot2
		SCROLL	56			;full card slot
		bra.s	rm_HandEnd
rm_NotSlot2:
		andi	#$c0,d2
		beq.s	rm_NoBlood
		SCROLL	48			;blooded wall
		bra.s	rm_HandEnd
rm_NoBlood:	SCROLL	47			;normal wall
rm_HandEnd:	bra	RMB_End

;-------------------------------------------------------------------
rm_ServeSwitch:	tst	sv_DoorFlag1+22		;last prior lasting?
		bne.s	.rm_CantUse
		tst	sv_DoorFlag2+22
		bne.s	.rm_CantUse
		cmpi	#-1,sv_DoorFlag1+26	;is last prior made?
		bne.s	.rm_CantUse
		cmpi	#-1,sv_DoorFlag2+26	;door 2 too?
		beq.s	.rm_DoUse
.rm_CantUse:	SCROLL	54
		bra	rm_HandEnd

.rm_DoUse:	SCROLL	53
		SOUND	2,1,63
		andi.b	#%11000001,(a1,d1.w)
		ori.b	d0,(a1,d1.w)		;switch in!
		lea	sv_SwitchData,a2	;command tab
.rm_SeekPos:	move	(a2)+,d0
		cmpi	#-1,d0
		bne.s	.rm_SeekPos
		move	(a2)+,d0
		bmi.w	rm_HandEnd		;not found server
		cmp	d0,d1			;chk offest
		bne.s	.rm_SeekPos

		tst	d2
		beq.s	rm_DoCommands
rm_SeekOut:	cmp	(a2)+,d2
		bne.s	rm_SeekOut

rm_DoCommands:	bsr.s	rm_CommandLoop
		bra	rm_HandEnd

;a2 - command table
rm_CommandLoop:	move	(a2)+,d0		;get flag_byte
		move	(a2)+,d1		;offset
		tst.b	d0
		bpl.s	rm_CommandCont		;end of command tab
		rts
rm_CommandCont:	beq.s	rm_OpenDoor		;0
		subi.b	#1,d0
		beq.s	rm_CloseDoor		;1
		subi.b	#1,d0
		beq.w	rm_SetBlock		;2
		subi.b	#1,d0
		beq.w	rm_RemBlock		;3
		subi.b	#1,d0
		beq.w	rm_SetWall		;4
		subi.b	#1,d0
		beq.w	rm_SetItem		;5
		subi.b	#1,d0
		beq.w	rm_SetColumn		;6

		bra.s	rm_CommandLoop


;---------------
rm_OpenDoor:	move.b	(a1,d1.w),d2
		andi.b	#-2,d2
		cmpi.b	#32,d2			;maybe already open?
		beq.s	rm_CommandLoop
		cmpi.b	#38,d2
		beq.s	rm_CommandLoop
		move	#$0106,d0
		bra.s	rm_FixDoor
rm_CloseDoor:	move.b	(a1,d1.w),d2
		andi.b	#-2,d2
		cmpi.b	#30,d2			;maybe already closed?
		beq.s	rm_CommandLoop
		cmpi.b	#36,d2
		beq.s	rm_CommandLoop
		move	#$00ff,d0
rm_FixDoor:	lea	sv_DoorFlag1+24,a3
		cmpi.b	#34,d2			;maybe animated doors
		beq.s	rm_CommandLoop
		cmpi.b	#40,d2
		beq.s	rm_CommandLoop
		cmpi.b	#30,d2			;chk if door 1
		beq.s	rm_OkDoor1
		cmpi.b	#32,d2
		beq.s	rm_OkDoor1
		lea	sv_DoorFlag2+24,a3	;no - door 2
rm_OkDoor1:	move.b	#50,44-24(a3)		;sound flag
		move	d0,(a3)+		;save open/close fl.& CNT
.rm_FindLast:	cmpi	#-1,(a3)+
		bne.s	.rm_FindLast
		move	d1,d0
		move.b	(a1,d1.w),d2
		lsr	d2
		roxl	d0			;dir of door
		move	d0,-2(a3)		;save offset 01
		move	d1,d0
		andi	#3,d0
		bne.s	rm_cE
		addi	#512+2,d1
		bra.s	rm_c0
rm_cE:		subq	#1,d0
		bne.s	rm_cS
		addi	#8+2,d1
		bra.s	rm_c0
rm_cS:		subq	#1,d0
		bne.s	rm_cW
		subi	#512-2,d1
		bra.s	rm_c0
rm_cW:		subq	#8-2,d1
rm_c0:		andi	#$7ffb,d1
		move.b	(a1,d1.w),d2
		lsr	d2
		roxl	d1
		move	d1,(a3)+		;save offset 02
		move	#-1,(a3)		;end table
		bra.w	rm_CommandLoop

;---------------				;command functions...
rm_SetBlock:	andi	#$fff8,d1
		ori.b	#%01000000,6(a1,d1.w)
		bra.w	rm_CommandLoop

;---------------
rm_RemBlock:	andi	#$fff8,d1
		andi.b	#%10111111,6(a1,d1.w)
		bra.w	rm_CommandLoop

;---------------
rm_SetWall:	lsr	#8,d0
		move.b	d0,(a1,d1.w)
		bra.w	rm_CommandLoop

;---------------
rm_SetItem:	lsr	#8,d0
		andi	#$fff8,d1
		andi.b	#%11100000,6(a1,d1.w)
		or.b	d0,6(a1,d1.w)
		bra.w	rm_CommandLoop

;---------------
rm_SetColumn:	lsr	#8,d0
		andi	#$fff8,d1
		andi.b	#%11100000,5(a1,d1.w)
		or.b	d0,5(a1,d1.w)
		bra.w	rm_CommandLoop


;-------------------------------------------------------------------
;-------------------------------------------------------------------
rm_CardUsed:	tst	4+4(a1,d0.w)		;card cnt
		bmi.s	rm_CardExists
		SCROLL	61
		bra.w	rm_CardEnd

rm_CardExists:	move	sv_Angle,d0
		addi	#64,d0
		andi	#$1fe,d0
		moveq	#3-1,d1			;W for 0 degrees
.rm_FindDir:	addq	#1,d1
		subi	#128,d0
		bpl.s	.rm_FindDir

		lea	sv_Map,a1
		move	sv_MapPos,d0		;your map location
		move	d0,d2
		move.b	4(a1,d0.w),d0		;get slots
		not	d1
		andi	#3,d1
		add	d1,d1
		lsr	d1,d0
		andi	#3,d0
		cmpi	#1,d0
		beq.s	rm_FreeSlot
		cmpi	#2,d0
		beq.s	rm_UsedSlot
		SCROLL	57			;nowhere to put card
		bra.w	rm_CardEnd
rm_UsedSlot:	SCROLL	58			;full slot
		bra.w	rm_CardEnd
rm_FreeSlot:
		lea	sv_SwitchData,a2	;command tab
		moveq	#0,d4
		move	sv_Items,d4
		divu	#6,d4
		subq	#6,d4			;color: 1-R, 2-G, 3-B
		moveq	#0,d3			;color flag
.rm_SeekPos:	move	(a2)+,d0
		cmpi	#-1,d0
		bne.s	.rm_SeekPos
		move	(a2)+,d0
		bmi.s	rm_BadSlot		;not found server
		subq	#5,d0
		cmp	d0,d2			;chk offest
		bne.s	.rm_SeekPos
		cmp	(a2)+,d4		;chk color
		beq.s	rm_FoundPos
		moveq	#1,d3
		bra.s	.rm_SeekPos

rm_FoundPos:	lea	sv_Items+4,a3
		move	-4(a3),d0
		addi	#1,4(a3,d0.w)		;dec card CNT
		bsr		tc_DrawCardCnt
		
		move.b	4(a1,d2.w),d0		;slots
		move	#%11111100,d3
		rol.b	d1,d3
		and.b	d3,d0
		moveq	#2,d3
		lsl.b	d1,d3
		or.b	d3,d0			;change to slot 2
		move.b	d0,4(a1,d2.w)

		SCROLL	62
		SOUND	21,1,63
		bsr	rm_CommandLoop
		bra.s	rm_CardEnd

rm_BadSlot:	tst	d3
		bne.s	rm_BadColor
		SCROLL	60			;inactive slot
		bra.s	rm_CardEnd
rm_BadColor:	SCROLL	59			;wrong color
rm_CardEnd:	bra.w	RMB_End


;-------------------------------------------------------------------
; add crosshairs in the middle of the screen
drawCrosshairs:
		; move.l	sv_ChunkyBuffer,a1
		; move	#0,d0
		; clr.l	d1
		; move	sv_ViewWidth,d1				; in bytes = pixes / 8
		; lsl		#3,d1

		; move	#15,d2
; .wwww:
		; rept	4
		; lea		(a1),a2

		; rept	16
		; move.b	d0,(a2)+
		; move.b	d0,(a2)+
		; move.b	d0,(a2)+
		; move.b	d0,(a2)+
		; addq	#1,d0
		; endr

		; subi	#16,d0
		; lea		(a1,d1.l),a1
		; endr
		; addi	#16,d0
		; dbf		d2,.wwww

		lea		lc_variables(pc),a6
		move.l	sv_ChunkyBuffer,a1
		move	sv_ViewWidth,d1				; in bytes = pixes / 8
		lsl		#3,d1						; in pixels (48..192)
		move	d1,d3
		lsr		#1,d1
;		clr.l	d4
		move	d1,d4
		mulu	sv_ViewHeigth,d1			; results in half the screen
;		add.l	d4,d1						; move to line middle
		lea		(a1,d1.l),a1				; screen centre

		; lsl		#3,d1						; in pixels (48..192)
		; move	d1,d3
		; lsr		#1,d1
		; clr.l	d2
		; move	d1,d2
		; mulu	sv_ViewHeigth,d1			; results in half the screen
		; add.l	d2,d1						; move to line middle
		; lea		(a1,d1.l),a1				; screen centre

		move	d3,d1
		lsl		#2,d1
		move	lc_time+2(a6),d2
		lsl		#4,d2
		lea		sv_sinus,a2
		andi	#$1fe,d2
		clr		d0
		move.b	(a2,d2.w),d0
		ext		d0
		asr		#5,d0						; limit amplitude to 3 bits
		
		muls	lc_momentum(a6),d0			; scale amplitude based on momentum
		asr		#8,d0
		
		sub		lc_kickback(a6),d0			; add kickback

		muls	d3,d0						; scale a bit to screen size
		divs	#screenMaxX,d0

		muls	d3,d0						; d3 - chunky buffer width
		add		d0,d1
		lea		(a1,d1.w),a1				; add 4 more lines + sinus

;		move	#$30,d0						; colour: yellow
		move	#$08,d0						; colour: white
;		move	#$50,d0						; colour: red

		tst		sv_Buse						; 1 - blitter not used, 0 - used
		beq.s	.translateX

		lea		(a1,d4),a1					; move to screen middle
		move.b	d0,(a1)						; draw 3x3 cross
		lea		-1(a1,d3.w),a1
		move.b	d0,(a1)+
		move.b	d0,(a1)+
		move.b	d0,(a1)+
		lea		-2(a1,d3.w),a1
		move.b	d0,(a1)
		rts

.translateX:
		lea		sv_WidthTable(pc),a2		; translation table from linear to non-linear X in case of blitter
		clr		d1
		move.b	(a2,d4),d1
		move.b	d0,(a1,d1)						; draw 3x3 cross
		lea		(a1,d3.w),a1
		move.b	d0,(a1,d1)
		clr		d2
		move.b	-1(a2,d4),d2
		move.b	d0,(a1,d2)
		move.b	1(a2,d4),d2
		move.b	d0,(a1,d2)
		lea		(a1,d3.w),a1
		move.b	d0,(a1,d1)
		rts


;-------------------------------------------------------------------
; Break/damage weapon
DamageWeapon:	
		move	d0,d3
		bsr		GetRandom
		not		d0
		andi	#1,d0
		sub		d0,4(a1,d3.w)		;weapon present/damage counter
		bpl.s	.dw1
		move	#0,4(a1,d3.w)
.dw1:	
;		tst		4(a1,d3.w)	
		rts


;-------------------------------------------------------------------
Kickback:
		lea		lc_variables(pc),a6
		move	lc_kickback(a6),d3
		addi	#10,d3					; each shot 20 kickback
		cmpi	#20,d3					; max 30
		bmi.s	.k1
		moveq	#20,d3
.k1:	move	d3,lc_kickback(a6)
		rts

;-------------------------------------------------------------------
;-------------------------------------------------------------------
; input: d0 = weapon index
rm_HandGunUsed:					; input: d0 = weapon index: 0 (hand), 6 (handgun) etc.
		tst	2(a1)				; do you have any ammo for it (check current counter)?
		bne.s	.rm_Gok
		SCROLL	65				; "no ammo"
		SOUND	8,1,63
		bra.w	rm_HGEnd
.rm_Gok:	
		bsr.s	DamageWeapon		; returns 0 if damaged
		bne.s	.rm0
		SCROLL	76				; "damaged"
		SOUND	8,1,63
		bra.w	rm_HGEnd
.rm0:		
		bsr		Kickback		; show kickback on crosshairs
		addq	#1,2(a1)			; decrease ammo count
		SOUND	7,1,63

		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra.w	rm_HGEnd

.Efound:	move	sv_PosX,6(a1)		;set object structure
		move	sv_PosY,8(a1)		;pos X,Y
		move	sv_MapPos,10(a1)
		movem.l	a1/a2,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		move	#256,d6
		sub	sv_angle,d6
		andi	#$1fe,d6
		moveq	#0,d0
		move	#400/8,d1		;vector length
		bsr	sv_Rotate
		movem.l	(sp)+,a1/a2
		move	d0,2(a1)		;add X,Y
		move	d1,4(a1)
		move.b	#1,(a1)

		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		moveq	#19,d7			;up to 8000
.ChkCollision:	bsr.s	Object_Collision	;seek collision
		bmi.s	rm_beczka1
		bne.s	.ColFound
		dbf	d7,.ChkCollision
		move.b	#0,(a1)
		bra.s	rm_HGEnd

.ColFound:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bsr.s	GetRandom
		andi	#3<<2,d0		;wys
		ori	#$0100,d0		;1 - odprysk
		or	d1,d0
		move	d0,(a1)			;set in structure
		bra.s	rm_HGEnd

rm_beczka1:	bsr	ro_SetExplode
rm_HGEnd:	bra	RMB_End

;-----------------------------------------------------------------------------
;
;out	d0 - random value
;
GetRandom:	movem.l	a4,-(sp)
		RANDOM	a4,d0
		movem.l	(sp)+,a4
		rts

SeedRandom:
		lea		$dff000,a0
		lea		seed(pc),a1
		move.l	(a1),d0
		move	6(a0),d1		; VHPOSR
		eor		d1,d0
		swap	d0
		move	10(a0),d1		; JOY0DAT
		eor		d1,d0
		move.l	d0,(a1)
		rts

seed:		dc.l	$12345678 


;-------------------------------------------------------------------
;check if object (a1-structure) hits anything...
;output:
;d0	-	0 - nothing, other - hit! (what hit)

;don't change a1,a2,a3,d7 (struct, HitTab, Map, cnt)
Object_Collision:
		move.l	a4,-(sp)
		moveq	#7,d6
oc_LOOP:	move	#0,(a2)			;zero hit poses
		move	#0,8(a2)
		ori.l	#$00010001,2(a1)
		move	6(a1),d0		;actual pos X
		move	d0,d1
		andi	#$fc00,d0
		add	2(a1),d1		;new pos X
		andi	#$fc00,d1
		cmp	d1,d0
		beq.s	.oc_NotInX
		smi	d2			;-1 if d1>d0
		bmi.s	.oc_x0
		move	d0,d1
.oc_x0:		ext	d2
		add	d2,d1			;fix right margin
		move	d1,2(a2)		;save x1 pos
		move	d1,d0
		moveq	#1,d2			;direction
		sub	6(a1),d1		;x
		bpl.s	.oc_x1
		moveq	#3,d2
.oc_x1:		muls	4(a1),d1		;x*dy
		divs	2(a1),d1		;(x*dy)/dx
		add	8(a1),d1
		move	d1,4(a2)		;save y1 pos

		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0			;map offset
		move	d0,6(a2)		;save offset
		add	d2,d0			;+dir
		move.b	(a3,d0.w),d4
		bsr	br_ChkDoors
		beq.s	.oc_NotInX		;no wall there
		move	#1,(a2)			;set hit1 flag
.oc_NotInX:
		move	8(a1),d0		;actual pos Y
		move	d0,d1
		andi	#$fc00,d0
		add	4(a1),d1		;new pos Y
		andi	#$fc00,d1
		cmp	d1,d0
		beq.s	.oc_NotInY
		smi	d2			;-1 if d1>d0
		bmi.s	.oc_y0
		move	d0,d1
.oc_y0:		ext	d2
		add	d2,d1			;fix up margin
		move	d1,12(a2)		;save y2 pos
		move	d1,d0
		moveq	#0,d2
		sub	8(a1),d0		;y
		bpl.s	.oc_y1
		moveq	#2,d2
.oc_y1:		muls	2(a1),d0		;y*dx
		divs	4(a1),d0		;(y*dx)/dy
		add	6(a1),d0
		move	d0,10(a2)		;save x2 pos

		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0
		move	d0,14(a2)		;save offset
		add	d2,d0			;+dir
		move.b	(a3,d0.w),d4
		bsr	br_ChkDoors
		beq.s	.oc_NotInY		;no wall there
		move	#1,8(a2)		;set hit1 flag
.oc_NotInY:
		tst	(a2)			;choose hit wall
		beq.s	.oc_FirstNot
		tst	8(a2)
		beq.s	.oc_OnlyFirst
		move	10(a1),d0		;old offset
		cmp	14(a1),d0		;second hit is diz sqare?
		beq.s	.oc_FN1
.oc_OnlyFirst:	move	2(a2),6(a1)		;copy pos
		move	4(a2),8(a1)
		move	6(a2),10(a1)
		bra.w	oc_WallHit
.oc_FirstNot:	tst	8(a2)
		beq.s	.oc_BothNot
.oc_FN1:	move	10(a2),6(a1)
		move	12(a2),8(a1)
		move	14(a2),10(a1)
		bra.w	oc_WallHit

.oc_BothNot:	movem	2(a1),d0/d1
		add	d0,6(a1)
		add	d1,8(a1)
		movem	6(a1),d0/d1
		move	d0,d2
		move	d1,d3
		lsr	#7,d2
		andi	#63*8,d2
		lsr	d3
		andi	#63*512,d3
		add	d3,d2
		move	d2,10(a1)		;save offset
		andi	#1023,d0		;in-square pos X
		andi	#1023,d1		;Y

		move.b	5(a3,d2.w),d3
		andi	#31,d3
		beq.w	oc_NoWallHit
		cmpi.b	#18,d3			;beczka
		beq.s	oc_okkk
		cmpi.b	#3,d3			;only col's 1,2
		bpl.w	oc_NoWallHit
oc_okkk:	cmpi	#512-190,d0		;chk if in column
		bmi.w	oc_NoWallHit
		cmpi	#512-190,d1
		bmi.w	oc_NoWallHit
		cmpi	#512+190,d0
		bpl.w	oc_NoWallHit
		cmpi	#512+190,d1
		bpl.w	oc_NoWallHit

		cmpi.b	#18,d3			;beczka
		bne	oc_WallHit
		tst	eh_FirePos+4		;if exploding beczka
		bne	oc_WallHit
		cmpi.b	#4,(a1)			;if prad
		beq	oc_WallHit
		movem.l	a2/d3/d4,-(sp)
		lea	oc_beczkas(pc),a2
		moveq	#0,d3
.bc_1:		cmp	(a2,d3.w),d2		;recorded beczka
		beq.s	.bc_found
		addq	#4,d3
		cmpi	#16*4,d3
		bne.s	.bc_1
		moveq	#0,d3
.bc_2:		tst	(a2,d3.w)		;found clear
		beq.s	.bc_clr
		addq	#4,d3
		cmpi	#16*4,d3
		bne.s	.bc_2
		moveq	#0,d3			;first if no clear
.bc_clr:	move	d2,(a2,d3.w)
.bc_found:	moveq	#30,d4
		cmpi.b	#1,(a1)
		bne.s	.bc_3
		moveq	#5,d4
.bc_3:		add	d4,2(a2,d3.w)
		cmpi	#30,2(a2,d3.w)
		bmi.s	.bc_4
		move.l	#0,(a2,d3.w)
		andi.b	#%11100000,5(a3,d2.w)	;beczka out of map
		andi.b	#%11000000,6(a3,d2.w)	;item out of map
		movem.l	(sp)+,a2/d3/d4
		move	#-1,-(sp)		;if beczka
		bra	oc_CheckError
.bc_4:		movem.l	(sp)+,a2/d3/d4
		bra	oc_WallHit

oc_beczkas:	blk.l	16,0

oc_NoWallHit:	tst	sv_HitFlag
		bne.s	oc_PlayerNot
		movem	6(a1),d0/d1		;player hit?
		movem	sv_PosX,d2/d3
		subi	#250,d2
		cmp	d0,d2
		bpl.s	oc_PlayerNot
		addi	#500,d2
		cmp	d0,d2
		bmi.s	oc_PlayerNot
		subi	#250,d3
		cmp	d1,d3
		bpl.s	oc_PlayerNot
		addi	#500,d3
		cmp	d1,d3
		bpl.s	oc_MeHit


oc_PlayerNot:	move	10(a1),d1		;Enemy hit?
		moveq	#0,d5
.oc_EnemyChk:	move	d1,d0
		add	oc_AddTab(pc,d5.w),d0
		move.b	7(a3,d0.w),d0
		beq.s	.oc_e9

		lea	sv_EnemyData,a4		;EnemyTab
		andi	#$ff,d0
		lsl	#4,d0
		lea	(a4,d0.w),a4
		movem	4(a4),d2/d3		;X,Y of enemy
		sub	6(a1),d2
		bpl.s	.oc_e1
		neg	d2
.oc_e1:		cmpi	#200,d2			;Xdelta < 256?
		bpl.s	.oc_e9
		sub	8(a1),d3
		bpl.s	.oc_e2
		neg	d3
.oc_e2:		cmpi	#200,d3			;Ydelta too?
		bmi.s	oc_EnemyHit
.oc_e9:		addq	#2,d5
		cmpi	#9*2,d5
		bne.s	.oc_EnemyChk


oc_NothingHit:	dbf	d6,oc_LOOP
		move	#0,-(sp)
		bra.s	oc_CheckError

oc_AddTab:	dc.w	0,8,-8,512,-512,512-8,512+8,-512-8,-512+8

oc_EnemyHit:	move	#3,-(sp)
		bsr	EnemyHitServe
		bra.s	oc_CheckError
oc_MeHit:	move	#2,-(sp)		;if player hit
		bsr	oc_PlayerHit
		bra.s	oc_CheckError
oc_WallHit:	move	#1,-(sp)

;a1-structure, a3-map
oc_CheckError:	move	6(a1),d0		;object pos
		move	d0,d2			;middle pos
		move	8(a1),d1
		move	d1,d3
		andi	#63*1024,d2
		andi	#63*1024,d3
		move	d2,d4
		move	d3,d5
		addi	#512,d2
		addi	#512,d3
		sub	d0,d2
		sub	d1,d3			;d2,d3 - vector 2
		sub	sv_PosX,d0
		muls	d2,d0
		sub	sv_PosY,d1		;d0,d1 - vector	1
		muls	d3,d1
		add.l	d1,d0
		bmi.w	oc_ColQuit		;quit if behind half

		move	sv_PosX,d0		;your pos
		move	d0,d2
		andi	#63*1024,d2
		move	sv_PosY,d1
		move	d1,d3
		andi	#63*1024,d3
		cmp	d2,d4			;if in the same square
		bne.s	oc_ErCont
		cmp	d3,d5
		beq.s	oc_SameSquare
oc_ErCont:
		sub	8(a1),d1		;dy
		moveq	#1,d2			;dir
		moveq	#-8,d3
		sub	6(a1),d0		;dx
		bne.s	.oc_No0
		addq	#1,d0
.oc_No0:	bmi.s	.oc_Xplus
		addi	#1024,d4
		moveq	#3,d2
		moveq	#8,d3
.oc_Xplus:	sub	6(a1),d4		;x
		muls	d1,d4
		divs	d0,d4
		add	8(a1),d4		;new y
		andi	#63*1024,d4
		cmp	d4,d5
		beq.s	.oc_Yok
		bmi.s	.oc_Yup
		moveq	#0,d2
		move	#-512,d3
		bra.s	.oc_Yok
.oc_Yup:	moveq	#2,d2
		move	#512,d3
.oc_Yok:	movem	6(a1),d0/d1
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0
		add	d3,d0			;shift
		move	d0,d1
		add	d2,d1			;wall dir
		move.b	(a3,d1.w),d4
		bsr	br_ChkDoors		;0 if no wall
		bne.s	oc_ColQuit		;wall there
		move	d0,10(a1)		;save offset

oc_ColQuit:	moveq	#0,d1
		bra.s	oc_bla

oc_SameSquare:	move	#$80,d1
oc_bla:		move	(sp)+,d0
		move.l	(sp)+,a4
		tst	d0
		rts

;-------------------------------------------------------------------
;a1 - object struct, a4 - enemy struct, a2,a3,d6,d7 - don't touch
EnemyHitServe:
;		cmpi.b	#1,12(a4)		;not if hit,kill,burn.
;		beq.s	eh_End
		cmpi.b	#2,12(a4)
		beq.w	eh_End
		cmpi.b	#3,12(a4)
		beq.w	eh_End

		btst.b	#0,1(a4)
		beq.s	.me01
		move.b	sv_LevelData+4,d2		;czy czuly na to?
		bra.s	.me02
.me01:		move.b	sv_LevelData+10,d2
.me02:		move.b	(a1),d0			;hit by what?
		cmpi.b	#1,d0			;pocisk
		bne.s	.eh1
		btst	#0,d2
		beq.w	eh_End
		moveq	#5,d1
		bra.s	eh_cont1
.eh1:		cmpi.b	#3,d0			;kula
		bne.s	.eh2
		btst	#1,d2
		beq.w	eh_End
		moveq	#25,d1
		bra.s	eh_cont1
.eh2:		cmpi.b	#4,d0			;prad
		bne.s	.eh3
		btst	#2,d2
		beq.w	eh_End
		moveq	#20,d1
		bra.s	eh_cont1
.eh3:		cmpi.b	#5,d0			;rakieta
		bne.s	eh_cont1
		btst	#3,d2
		beq.w	eh_End
		moveq	#40,d1

eh_cont1:	movem	2(a1),d2/d3
		move.b	d2,14(a4)		;odrzucenie
		move.b	d3,15(a4)
		move	sv_angle,d0
		addi	#256,d0
		andi	#$1fe,d0
		move	d0,8(a4)

		sub	d1,2(a4)
		bpl	eh_stillOk
		cmpi	#25,d1
		beq.w	eh_burning		;if burn
		move.b	#2,12(a4)		;zabity
		move.b	#72,13(a4)

		movem	4(a4),d0/d1
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d0,d1			;map pos
		tst	d2
		bpl.s	.eh1
		neg	d2
.eh1:		tst	d3
		bpl.s	.eh2
		neg	d3
.eh2:		cmp	d2,d3
		bmi.s	.eh_Ybig
		moveq	#0,d5			;strona N
		move	4(a1),d2
		bpl.s	.eh3
		moveq	#2,d5			;S
		bra.s	.eh3
.eh_Ybig:	moveq	#1,d5			;strona E
		move	2(a1),d2
		bpl.s	.eh3
		moveq	#3,d5			;W
.eh3:		bsr	GetRandom
		move	#$40,d3
		andi	#1,d0
		beq.s	.eh5
		move	#$80,d3
.eh5:
		add	d5,d1
		move.b	(a3,d1.w),d2
		andi	#$3e,d2
		beq.s	eh_E1			;if no wall
		cmpi.b	#30,d2
		bmi.s	.eh4
		cmpi.b	#42,d2
		bmi.s	eh_E1
		cmpi.b	#48,d2
		bmi.s	.eh4
		cmpi.b	#56,d2
		bmi.s	eh_E1
.eh4:		andi.b	#$3f,(a3,d1.w)
		or.b	d3,(a3,d1.w)		;blood on map
eh_E1:		addq	#1,d1
		bsr.s	eh_SideBlood
		bsr.s	eh_SideBlood
		rts
eh_stillOk:	move.b	#1,12(a4)		;trafiony
		move.b	#72,13(a4)
eh_End:		rts

eh_SideBlood:	addq	#2,d1
		andi	#$fffb,d1
		bsr	GetRandom
		andi	#3,d0
		beq.s	eh2_End
eh6:		move.b	(a3,d1.w),d2
		andi	#$3e,d2
		beq.s	eh2_End			;if no wall
		cmpi.b	#30,d2
		bmi.s	eh7
		cmpi.b	#42,d2
		bmi.s	eh2_End
		cmpi.b	#48,d2
		bmi.s	eh7
		cmpi.b	#56,d2
		bmi.s	eh2_End
eh7:		andi.b	#$3f,(a3,d1.w)
		ori.b	#$c0,(a3,d1.w)		;blood on map
eh2_End:	rts

eh_Burning:	move.b	#3,12(a4)
		move.b	#80,13(a4)
		move	#55,2(a4)		;CNT to energy
		movem.l	d3/d4/a0/a3,-(sp)	;SOUND
		lea	$dff000,a0
		move	#%0100,$96(a0)
		moveq	#5,d4
.play_delay:	move.b	6(a0),d3		;delay change
.play_d1:	cmpi.b	6(a0),d3
		beq.s	.play_d1
		dbf		d4,.play_delay

		lea		lc_soundList(pc),a3
		moveq	#13*8,d3			; fire/burning sound
		move.l	(a3,d3.w),$c0(a0)	;adress
		move	4(a3,d3.w),$c0+4(a0)	;length in words
		move	6(a3,d3.w),$c0+6(a0)	;period
		move	#0,$c0+8(a0)		;volume
		move	#$8004,$96(a0)
		move.l	a4,eh_FirePos
		movem.l	(sp)+,d3/d4/a0/a3
		rts

;-------------------------------------------------------------------
oc_PlayerHit:
		SCROLL	72
		SOUND	13,1,63
		bsr	EXCITE			;quicker beat
		moveq	#0,d1
		move.b	(a1),d0			;hit by what?
		cmpi.b	#1,d0			;pocisk
		bne.s	.eh1
		moveq	#5,d1
		bra.s	eh_cont2
.eh1:		cmpi.b	#3,d0			;kula
		bne.s	.eh2
		moveq	#25,d1
		bra.s	eh_cont2
.eh2:		cmpi.b	#4,d0			;prad
		bne.s	.eh3
		moveq	#20,d1
		bra.s	eh_cont2
.eh3:		cmpi.b	#5,d0			;rakieta
		bne.s	eh_cont2
		moveq	#40,d1

eh_cont2:	add	d1,sv_Energy		;loose energy
		movem	2(a1),d2/d3
		add	d2,d2
		add	d2,d2
		add	d3,d3
		add	d3,d3
		add	d2,sv_AddMove
		add	d3,sv_AddMove+2
		bsr	GetRandom
		asr.b	#4,d0
		ext	d0
		add	d0,sv_Angle
		andi	#$1fe,sv_Angle
		move	#2,sv_SzumTime
		rts


;-------------------------------------------------------------------
rm_ShotGunUsed:	
		tst	2(a1)
		bne.s	.rm_Gok
		SCROLL	65			; "weapon damaged"
		SOUND	8,1,63
		bra.w	rm_SGEnd
.rm_Gok:	bsr	DamageWeapon
		bne.s	.rm0
		SCROLL	76
		SOUND	8,1,63
		bra.w	rm_SGEnd
.rm0:		
		bsr		Kickback		; show kickback on crosshairs
		addq	#1,2(a1)
		SOUND	25,1,63
		moveq	#2,d7

rm_AmmoLoop:	move	d7,-(sp)
		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		lea	2(sp),sp
		bra	rm_SGEnd

.Efound:	move	sv_PosX,6(a1)		;set object structure
		move	sv_PosY,8(a1)
		move	sv_MapPos,10(a1)
		movem.l	a1/a2,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		move	#256,d6
		sub	sv_angle,d6

		bsr	GetRandom
		andi	#7<<5,d0
		asr.b	#4,d0
		ext	d0
		add	d0,d6
		andi	#$1fe,d6
		moveq	#0,d0
		move	#400/8,d1		;vector length
		bsr	sv_Rotate
		movem.l	(sp)+,a1/a2
		move	d0,2(a1)
		move	d1,4(a1)
		move.b	#1,(a1)

		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		moveq	#24,d7			;up to 10000
.ChkCollision:	bsr.w	Object_Collision	;seek collision
		bmi.s	rm_beczka2
		bne.s	.ColFound
		dbf	d7,.ChkCollision
		move.b	#0,(a1)
		bra.s	.rm_SGCont

.ColFound:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bsr	GetRandom
		andi	#3<<2,d0		;wys
		ori	#$0100,d0		;1 - odprysk
		or	d1,d0			;before/behind
		move	d0,(a1)			;set in structure
.rm_SGCont:	move	(sp)+,d7
		dbf	d7,rm_AmmoLoop
		bra.s	rm_SGEnd

rm_beczka2:	move	(sp)+,d7
		bsr	ro_SetExplode
rm_SGEnd:	bra	RMB_End

;-------------------------------------------------------------------
rm_MachineGun:	tst	2(a1)
		bne.s	.rm_Gok
		SCROLL	65
		SOUND	8,1,63
		bra.w	RMB_End
.rm_Gok:	bsr	DamageWeapon
		bne.s	.rm0
		SCROLL	76
		SOUND	8,1,63
		bra.w	RMB_End
.rm0:		addq	#1,2(a1)
;		SOUND	4,1,63
		SOUND	24,1,63

;		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#1,d7
		bra	rm_AmmoLoop


;-------------------------------------------------------------------
rm_BolterUsed:	tst	2(a1)
		bne.s	.bo_Gok
		SCROLL	65
		SOUND	8,1,63
		bra	RMB_End
.bo_Gok:	bsr	DamageWeapon
		bne.s	.rm0
		SCROLL	76
		SOUND	8,1,63
		bra.w	RMB_End
.rm0:
		bsr		Kickback		; show kickback on crosshairs
		addq	#1,2(a1)
		SOUND	6,1,63

		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	RMB_End

.Efound:	move.b	#4,(a1)
		move	#600/8,d1		;vector length
		bsr.s	PrepareStruct
		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		bsr.w	Object_Collision	;seek collision
		bne.s	.FirstHit		;if hit first time

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0400,d0
		or	d1,d0			;before/behind
		move	d0,(a1)			;set in structure
		bra	RMB_End

.FirstHit:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0200,d0
		or	d1,d0
		move	d0,(a1)			;wyladowanie

		move	#190,sv_LastMove+4
		move	sv_Angle,d0
		neg	d0
		andi	#$1fe,d0
		move	d0,sv_LastMove+6
		move	#2,sv_szumtime
		addi	#1,sv_Energy		;loose energy
		bsr	EXCITE			;quicker beat

		bra	RMB_End


PrepareStruct:	move	sv_PosX,6(a1)		;set object structure
		move	sv_PosY,8(a1)
		move	sv_MapPos,10(a1)
		movem.l	a1/a2,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		move	#256,d6
		sub	sv_angle,d6
	bsr	GetRandom
	andi	#6,d0
	ext	d0
	add	d0,d6
		andi	#$1fe,d6
		moveq	#0,d0
;		move	#600,d1			;vector length
		bsr	sv_Rotate
		movem.l	(sp)+,a1/a2
		move	d0,2(a1)
		move	d1,4(a1)
		rts

;-------------------------------------------------------------------
rm_FlamerUsed:	tst	2(a1)
		bne.s	.bo_Gok
		SCROLL	65
		SOUND	8,1,63
		bra	RMB_End
.bo_Gok:	bsr	DamageWeapon
		bne.s	.rm0
		SCROLL	76
		SOUND	8,1,63
		bra.w	RMB_End
.rm0:
		bsr		Kickback		; show kickback on crosshairs
		addq	#1,2(a1)
		SOUND	5,1,63

		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	RMB_End

.Efound:	move.b	#3,(a1)
		move	#400/8,d1		;vector length
		bsr.w	PrepareStruct
		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		bsr.w	Object_Collision	;seek collision
		bmi.s	.rm_beczka3
		bne.s	.FirstHit		;if hit first time

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0300,d0
		or	d1,d0			;before/behind
		move	d0,(a1)			;set in structure
		bra	RMB_End

.rm_beczka3:	bsr	ro_SetExplode
		bra	RMB_End
.FirstHit:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0603,d0
		or	d1,d0
		move	d0,(a1)			;wyladowanie

		move	#190,sv_LastMove+4
		move	sv_Angle,d0
		neg	d0
		andi	#$1fe,d0
		move	d0,sv_LastMove+6
		addi	#5,sv_Energy		;loose energy
		move	#2,sv_szumtime
		bsr	EXCITE			;quicker beat

		bra	RMB_End

;-------------------------------------------------------------------
rm_LauncherUsed:
		tst		sv_Flag+8		;reload
		beq.s	.bo_33
		SCROLL	68				; still reloading
		bra	RMB_End
.bo_33:		tst	2(a1)
		bne.s	.bo_Gok
		SCROLL	65
		SOUND	8,1,63
		bra	RMB_End
.bo_Gok:	bsr	DamageWeapon
		bne.s	.rm0
		SCROLL	76
		SOUND	8,1,63
		bra.w	RMB_End
.rm0:
		bsr		Kickback		; show kickback on crosshairs
		addq	#1,2(a1)
		SOUND	3,1,63

		move	#10,sv_Flag+8	; reload timer
		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	RMB_End

.Efound:	move.b	#5,(a1)
		move	#600/8,d1		;vector length
		bsr.w	PrepareStruct
		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		bsr.w	Object_Collision	;seek collision
		bne.s	.FirstHit		;if hit first time

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0500,d0
		or	d1,d0			;before/behind
		move	d0,(a1)			;set in structure
		bra	RMB_End

.FirstHit:	bsr.w	ro_SetExplode
		move	#250,sv_LastMove+4
		move	sv_Angle,d0
		neg	d0
		andi	#$1fe,d0
		move	d0,sv_LastMove+6
		addi	#5,sv_Energy		;loose energy
		move	#2,sv_szumtime
		bsr	EXCITE			;quicker beat

		bra	RMB_End

;-------------------------------------------------------------------
;-------------------------------------------------------------------
; animate dynamic objects such as shots etc.
Anim_Objects:	
		movem.l	ALL,-(sp)
		lea	sv_ObjectTab,a1
		lea	sv_MAP,a3
		moveq	#29,d7
ao_Seek:	move.b	(a1),d0
		beq.s	ao_StillSeek
		cmpi.b	#1,d0			;odprysk
		beq.s	ao_OBJECT1
		cmpi.b	#2,d0			;wyladowanie
		beq.s	ao_OBJECT1
		cmpi.b	#4,d0			;prad
		beq.s	ao_PRAD
		cmpi.b	#3,d0			;fireball
		beq	ao_FLAME
		cmpi.b	#6,d0			;explosion
		beq	ao_EXPLODE
		cmpi.b	#5,d0			;rocket
		beq	ao_ROCKET

ao_StillSeek:	lea	12(a1),a1
		dbf	d7,ao_seek
		movem.l	(sp)+,ALL
		rts


ao_OBJECT1:	move	(a1),d0
		addq	#1,d0
		move	d0,d1
		andi	#3,d1
		cmpi	#3,d1
		bne.s	.ao_o11
		move	#0,(a1)
		bra.s	ao_StillSeek
.ao_o11:	move	d0,(a1)
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bra.s	ao_StillSeek

;---------------
ao_PRAD:	move	(a1),d0
		addq	#2,d0
		move	d0,d1
		andi	#$3e,d1
		cmpi	#40,d1
		bne.s	.ao_p1
		move	#0,(a1)			;hit nothing
		bra.s	ao_StillSeek
.ao_p1:		eori	#1,d0			;anim
		move	d0,(a1)

		lea	oc_HitPos,a2
		bsr.w	Object_Collision	;seek collision
		bne.s	.ao_p2			;if hit

		andi	#$ff7f,(a1)
		or	d1,(a1)			;before/behind
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bra.s	ao_StillSeek

.ao_p2:		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		move	#$0200,d0
		or	d1,d0
		move	d0,(a1)			;wyladowanie
		bra.w	ao_StillSeek

;---------------
ao_FLAME:	move	(a1),d0
		addq	#2,d0
		move	d0,d1
		andi	#$3e,d1
		cmpi	#60,d1
		bne.s	.ao_f1
		move	#0,(a1)			;hit nothing
		bra.w	ao_StillSeek
.ao_f1:		eori	#1,d0			;anim
		move	d0,(a1)

		lea	oc_HitPos,a2
		bsr.w	Object_Collision	;seek collision
		bmi.w	ao_f2			;if hit beczka
		bne.s	.ao_f2			;if hit
		andi	#$ff7f,(a1)
		or	d1,(a1)			;before/behind
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		bra.w	ao_StillSeek
.ao_f2:		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0604,d0
		or	d1,d0
		move	d0,(a1)			;wybuch
		movem	6(a1),d0/d1
		SOUND2	11,2			;explode sound
		bra.w	ao_StillSeek

;----------------
ao_EXPLODE:	move	(a1),d0
		addq	#1,d0
		move	d0,d1
		andi	#7,d1
		cmpi	#7,d1
		bne.s	.ao_e1
		move	#0,(a1)
		bra.w	ao_StillSeek
.ao_e1:		move	d0,(a1)
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bra.w	ao_StillSeek

;---------------
ao_ROCKET:	move	(a1),d0
		addq	#2,d0
		move	d0,d1
		andi	#$3e,d1
		cmpi	#40,d1
		bne.s	.ao_f1
		move	#0,(a1)			;hit nothing
		bra.w	ao_StillSeek
.ao_f1:		eori	#1,d0			;anim
		move	d0,(a1)

		lea	oc_HitPos,a2
		bsr.w	Object_Collision	;seek collision
		bne.s	ao_f2			;if hit
		andi	#$ff7f,(a1)
		or	d1,(a1)			;before/behind
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		bra.w	ao_StillSeek
ao_f2:		cmpi	#2,d0			;player hit?
		seq	sv_HitFlag		;no more hits
		bsr.s	ro_SetExplode
		move	#0,sv_HitFlag
		bra.w	ao_StillSeek


;---------------
ro_SetExplode:	movem.l	ALL,-(sp)		;a2,a3 - don't touch
		move	#1,eh_FirePos+4		;not destroy beczkas
		move	2(a1),d0		;cofnij wybuch
		asr	#3,d0
		sub	d0,6(a1)
		move	4(a1),d0
		asr	#3,d0
		sub	d0,8(a1)
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0603,d0
		or	d1,d0
		move	d0,(a1)			;wybuch
		movem	6(a1),d0/d1
		SOUND2	11,2			;explode sound
		lea	(a1),a4			;BASEF strucrure
		lea	ro_DirTab(pc),a5

		move	#$0601,d5
		moveq	#1,d6
ro_DelayLoop:	moveq	#3,d7
ro_SetLoop:	lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	ro_End

.Efound:	move.l	6(a4),6(a1)		;pos
		move.l	(a5)+,2(a1)		;add
		move	#$300,(a1)		;kula ognia
		movem.l	a2/a4/a5/d5/d6,-(sp)
		lea	oc_HitPos,a2
		bsr.w	Object_Collision	;seek collision
		movem.l	(sp)+,a2/a4/a5/d5/d6
		tst	d0
		bne.s	.ro_20			;if hit
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	d5,d0
		or	d1,d0
		move	d0,(a1)
		bra.s	.ro_2
.ro_20:		move	#0,(a1)
.ro_2:		dbf	d7,ro_SetLoop
		subq	#1,d5
		dbf	d6,ro_DelayLoop


		lea	ro_DirTab(pc),a5
		moveq	#3,d7
ro_SetLoop2:	lea	sv_ObjectTab,a1
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	ro_End
.Efound:	move.l	6(a4),6(a1)		;pos
		move.l	(a5)+,2(a1)		;add
		move	#$300,(a1)
		moveq	#1,d6
.ro_twice:	movem.l	a2/a4/a5/d6,-(sp)
		lea	oc_HitPos,a2
		bsr.w	Object_Collision	;seek collision
		movem.l	(sp)+,a2/a4/a5/d6
		tst	d0
		bne.s	.ro_30			;if hit
		dbf	d6,.ro_Twice
		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0600,d0
		or	d1,d0
		move	d0,(a1)
		bra.s	.ro_3
.ro_30:		move	#0,(a1)
.ro_3:		dbf	d7,ro_SetLoop2

ro_END:		move	#0,eh_FirePos+4
		movem.l	(sp)+,ALL
		rts

ro_DirTab:
	dc.w	600/8,0,-600/8,0,0,600/8,0,-600/8	;x,y add
	dc.w	700/8,700/8,700/8,-700/8,-700/8,-700/8,-700/8,700/8
		
;-------------------------------------------------------------------
;-------------------------------------------------------------------
me_Enemies:	ds.b	256
Move_Enemy:
		lea	sv_MAP,a1
		lea	sv_EnemyData,a2		;EnemyTab
		lea	me_Enemies(pc),a3
		move	sv_MapPos,d0	;your offset
		subi	#[12*512]+[12*8]+1,d0
		moveq	#23,d7			; only move enemies within a 24x24 square around player. FOW is 22.
me_Loop1:	moveq	#23,d6
		moveq	#0,d1
me_Loop2:	addi	#8,d0
		bmi.s	me_ContLoop
		move.b	(a1,d0.w),d1
		beq.s	me_ContLoop
		move.b	d1,(a3)+
me_ContLoop:	dbf	d6,me_loop2
		addi	#40*8,d0
		dbf	d7,me_loop1
		move.b	#0,(a3)+

		lea	me_Enemies(pc),a4
me_Found:	moveq	#0,d1
		move.b	(a4)+,d1
		bne.s	.me_f2
		rts
.me_f2:		lsl	#4,d1
		lea	(a2,d1.w),a3
;a1,a2,a3,a4 - don't touch
		cmpi.b	#1,12(a3)		;hit
		beq	me_hited
		cmpi.b	#2,12(a3)		;killed
		beq.w	me_killed
		cmpi.b	#3,12(a3)		;burning
		beq.w	me_burning
		cmpi.b	#4,12(a3)		;shoot on
		beq.w	me_shootON
		cmpi.b	#0,12(a3)		;walking
		bne.s	me_Found

;---------------
me_walk:	move	4(a3),d0		;enemy attack player
		sub	sv_PosX,d0
		move	d0,d2
		move	6(a3),d1
		sub	sv_PosY,d1
		move	d1,d3
		muls	d0,d0
		muls	d1,d1
		add.l	d1,d0
		bsr	sq_SQRT			;dist. from enemy

	;	move.l	sv_RomAddr,a5
	;	move	(a5)+,d1
	;	move.l	a5,d4
	;	andi.l	#$ffff,d4
	;	or.l	#$000000,d4		;$f90000
	;	move.l	d4,sv_RomAddr
		RANDOM	a5,d1

		andi.l	#$1fff,d1		;to 8191


		move	d0,d4
		btst.b	#0,1(a3)
		beq.s	.me01
		add	sv_LevelData+6,d4
		bra.s	.me02
.me01:		add	sv_LevelData+12,d4
.me02:		cmp	d1,d4
;		bpl.w	me_w4			;not to player or shoot
		bpl.s	me_ShootIt		;not to player or shoot

		ext.l	d2
		lsl.l	#4,d2
		divs	d0,d2			;(x*16)/r
		add	d2,d2
		move	me_angletab+32(pc,d2),d2
		tst	d3
		bpl.w	me_a1
		neg	d2
		bra.s	me_a1
me_angletab:
	dc.w	0,22,40,50,58,66,72,80,86,92,96,102,106,112,118,124,128
	dc.w	134,138,144,150,154,160,164,170,176,184,190,198,206,216,230,256

;dc.w	0,40,58,72,86,96,106,118,128
;dc.w	138,150,160,170,184,198,216,256

me_ShootIt:				;only shoot
	cmpi	#5000,d1
	bmi.s	me_w4
	ext.l	d2
	lsl.l	#4,d2
	divs	d0,d2			;(x*16)/r
	add	d2,d2
	move	me_angletab+32(pc,d2),d2
	tst	d3
	bpl.s	me_a11
	neg	d2
me_a11:	addi	#256,d2
	andi	#$1fe,d2
	move	d2,8(a3)		;new angle - to player
	bsr	CanShoot
	bne.s	me_w4
	move.b	#4,12(a3)		;shoot on
	move.b	#60,13(a3)
	bra	me_Found


me_a1:		addi	#256,d2
		andi	#$1fe,d2
		move	d2,8(a3)		;new angle - to player

me_w3:		move	d0,d2			;DIST en-pl in d0
		btst.b	#0,1(a3)
		beq.s	.me03
		add	sv_LevelData+8,d2	;prawd. delta
		bra.s	.me04
.me03:		add	sv_LevelData+14,d2
.me04:		divu	d2,d1
		swap	d1			;reszta
		cmp	d1,d0
		bpl.s	me_w4

		bsr	CanShoot
		bne.s	me_w4
;		move	d0,14(a3)		;save dist
		move.b	#4,12(a3)		;shoot on
		move.b	#60,13(a3)
		bra	me_Found

me_w4:		movem.l	a1-a4,-(sp)

		bsr	GetRandom		;random roars
			andi	#$ff,d0	; was not here
		cmpi	#5,d0
		bmi.s	.me_h3
		cmpi	#8,d0		; was 10
		bpl.s	.me_h3
		move	d0,d2
		andi	#3,d2
		cmpi	#3,d2
		bne.s	.me_h1
		moveq	#0,d2
.me_h1:		addi	#18,d2
		movem	4(a3),d0/d1
		SOUND4	4
.me_h3:
		lea	sv_sinus,a1
		lea	$80(a1),a2
		move	#256,d6
		sub	8(a3),d6		;angle
		andi	#$1fe,d6
		moveq	#0,d0
		move	10(a3),d1		;vector length
		bsr	sv_Rotate
		movem.l	(sp)+,a1-a4
		move	d0,sv_MovSav
		move	d1,sv_MovSav+2
		bsr	me_MOVE
		tst	d0
		beq.s	me_w2
		moveq	#2,d5
me_RepTurn:
	
	;	move.l	sv_RomAddr,a5		;turn if hit
	;	move	(a5)+,d0
	;	move	(a5)+,d1
	;	move.l	a5,d4
	;	andi.l	#$ffff,d4
	;	or.l	#$000000,d4		;f90000-fa0000
	;	move.l	d4,sv_RomAddr
		RANDOM	a5,d0
		move.l	d0,d1
		swap	d1


		eor	d0,d1
		andi	#$fe,d1
		andi	#1,d0
		bne.s	.m2
		neg	d1
.m2:		add	d1,8(a3)
		andi	#$1fe,8(a3)
		move	sv_MovSav,d0
		move	sv_MovSav+2,d1
		bsr	me_MOVE
		tst	d0
		dbeq	d5,me_RepTurn
		bra	me_Found

me_w2:	;	move.l	sv_RomAddr,a5		;losowa zmiana kier.
	;	move	(a5)+,d0
	;	move	(a5)+,d1
	;	move.l	a5,d4
	;	andi.l	#$ffff,d4
	;	or.l	#$000000,d4		;f90000-fa0000
	;	move.l	d4,sv_RomAddr
		RANDOM	a5,d0
		move.l	d0,d1
		swap	d1

		andi	#$ff,d0
		cmpi	#$f0,d0
		bmi.s	.m3			;nie wylosowano
		eor	d0,d1
		andi	#$7e,d1
		andi	#1,d0
		bne.s	.m4
		neg	d1
.m4:		add	d1,8(a3)
		andi	#$1fe,8(a3)
.m3:		bra.w	me_Found

;---------------
me_shootON:	addi.b	#4,13(a3)
		cmpi.b	#68,13(a3)
		beq.w	es_EnemyShoot
		cmpi.b	#68+4,13(a3)
		bne	me_Found
		move.b	#60,13(a3)
		move.b	#0,12(a3)
		bne	me_Found

;---------------
me_hited:	cmpi.b	#72,13(a3)
		bne.s	.me_h2
		bsr	GetRandom
		move	d0,d2
		andi	#3,d2
		cmpi	#3,d2
		beq.s	.me_h2
		addi	#18,d2
		movem	4(a3),d0/d1
		SOUND4	4

.me_h2:		moveq	#0,d0
		moveq	#0,d1
		move.b	14(a3),d0
		ext	d0
		move.b	15(a3),d1
		ext	d1
		lsl	#1,d0
		lsl	#1,d1
		bsr	me_MOVE
		addi.b	#4,13(a3)
		cmpi.b	#80,13(a3)
		bne.w	me_Found
		move	#0,12(a3)
		bra.w	me_Found

me_burning:	subi	#1,2(a3)
		bpl.s	me_b2
		cmpa.l	eh_FirePos,a3
		bne.s	.me_b3
		move	#$0004,$dff096		;fire off
		move.l	#0,eh_FirePos
.me_b3:		movem	4(a3),d0/d1
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0
		addi	#1,sv_NrKilled
		move	#0,12(a3)
		move.b	#0,7(a1,d0.w)		;zero enemy
		andi.b	#31,5(a1,d0.w)
		ori.b	#64,5(a1,d0.w)		;spalony trup1
		move.b	1(a3),d1
		move	#0,(a3)
		andi	#3,d1
		cmpi	#2,d1
		bne.w	me_Found
		andi.b	#31,5(a1,d0.w)
		ori.b	#128,5(a1,d0.w)		;spalony trup2
		bra.w	me_Found
me_b2:		cmpa.l	eh_FirePos,a3
		bne.s	.me_b3
		movem	4(a3),d0/d1
		bsr	Sound_Distance2
		move	d0,$dff0c0+8		;volume of fire
.me_b3:		eori.b	#4,13(a3)		;80,84
		movem.l	a1-a4,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		bsr	GetRandom
		move	d0,d6
		andi	#$1fe,d6
		moveq	#0,d0
		move	10(a3),d1		;vector length
		bsr	sv_Rotate
		movem.l	(sp)+,a1-a4
		bsr	me_MOVE
		bra.w	me_Found

me_killed:	cmpi.b	#72,13(a3)
		bne.s	.me_h2
		bsr	GetRandom
		move	d0,d2
		andi	#3,d2
		addi	#17,d2
		movem	4(a3),d0/d1
		SOUND4	4

.me_h2:		moveq	#0,d0
		moveq	#0,d1
		move.b	14(a3),d0
		ext	d0
		move.b	15(a3),d1
		ext	d1
		lsl	#1,d0
		lsl	#1,d1
		bsr	me_MOVE
		addi.b	#4,13(a3)
		cmpi.b	#76,13(a3)
		bne.s	.m1
		move.b	#88,13(a3)
.m1:		cmpi.b	#96,13(a3)
		bne.w	me_Found
		movem	4(a3),d0/d1
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add		d1,d0			;map pos
		addi	#1,sv_NrKilled
		move	#0,12(a3)
		move.b	#0,7(a1,d0.w)		;zero enemy
		andi.b	#31,5(a1,d0.w)
		ori.b	#32,5(a1,d0.w)		;trup1
		move.b	1(a3),d1
		move	#0,(a3)
		andi	#3,d1
		move.l	a2,-(sp)
		lea		sv_LevelData+5,a2
		cmpi	#2,d1
		bne.s	.m2
		ori.b	#64+32,5(a1,d0.w)	;trup2
		lea		sv_LevelData+11,a2

.m2:	move	d0,d2			; Dead enemy item drops
		bsr		GetRandom
		andi	#$ff,d0
		tst		sv_DIFFICULT
		beq.s	.m2_DIF				; 0 - hard (default)
		cmpi	#120,d0				; easy: 120/256 % chance for drop
		bpl.s	.m3
		bra.s	.m2_ct
.m2_DIF:
		cmpi	#80,d0				; 80/256 % chance for drop
		bpl.s	.m3
.m2_ct:
		move.b	6(a1,d2.w),d0
		andi	#31,d0
		bne.s	.m3					;already item in that map position
		moveq	#0,d1
		move.b	(a2),d1				; enemy's weapon type (1,2..6)
		move	d1,d6
		mulu	#6,d6
		move.l	a2,-(sp)
		lea		sv_ITEMS,a2
		move	4(a2,d6.w),d6		;u have diz weapon?
		move.l	(sp)+,a2
		tst		d6
		bpl.s	.m4				; you have this item
		bsr		GetRandom
		andi	#$ff,d0
		cmpi	#20,d0			;dodatkowa bariera dla pozyskania amunicjo do broni ktorej nie masz
		bpl.s	.m3
.m4:	addi	#7,d1			; change type from weapon to ammo
		andi	#31,d1
		or.b	d1,6(a1,d2.w)		;put ammo on map

.m3:	move.l	(sp)+,a2
		bra.w	me_Found

;-------------------------------------------------------------------
;a1 - MAP, a3 - ENEMY STRUCTURE, d5 - CNT
me_MOVE:
;		movem.l	a2/a4,-(sp)
		movem	4(a3),d2/d3
		lsr	#7,d2
		andi	#63*8,d2
		lsr	d3
		andi	#63*512,d3
		add	d3,d2			;map pos
		move.b	7(a1,d2.w),d3		;enemy nr
		move	d3,-(sp)
		move.b	#0,7(a1,d2.w)		;zero old enemy

		movem	d0/d1,-(sp)
		add	4(a3),d0
		add	6(a3),d1
		move	d0,d6
		move	d1,d7			;d6,d7 - new pos

		move	d0,d2
		andi	#1023,d0
		move	d1,d3
		andi	#1023,d1		;d0,d1 - insquare
		lsr	#7,d2
		andi	#63*8,d2
		lsr	d3
		andi	#63*512,d3
		add	d3,d2			;d2 - map pos


		tst.b	7(a1,d2.w)		;if other enemy
		bne.w	me_HIT
		cmpi	#300,d0
		bpl.s	me_Xr
		move.b	3(a1,d2.w),d4
		andi	#62,d4			;if nothing
		beq.s	me_Xr
		bsr	me_ChkDoors
		bne.w	me_HIT
me_Xr:		cmpi	#1024-300,d0
		bmi.s	me_Yd
		move.b	1(a1,d2.w),d4
		andi	#62,d4
		beq.s	me_Yd
		bsr	me_ChkDoors
		bne.w	me_HIT
me_Yd:		cmpi	#300,d1
		bpl.s	me_Yu
		move.b	2(a1,d2.w),d4
		andi	#62,d4
		beq.s	me_Yu
		bsr.w	me_ChkDoors
		bne.w	me_HIT
me_Yu:		cmpi	#1024-300,d1
		bmi.s	me_COLUMN
		move.b	(a1,d2.w),d4
		andi	#62,d4
		beq.s	me_COLUMN
		bsr.w	me_ChkDoors
		bne.w	me_HIT

me_COLUMN:	move.b	5(a1,d2.w),d4
		andi	#31,d4
		beq.s	me_PLAYER		;if no column
		cmpi	#18,d4
		beq.s	.br2p			;if beczka
		cmpi	#5,d4
		bpl.s	me_PLAYER
		cmpi	#3,d4
		beq.s	me_PLAYER
.br2p:		cmpi	#512-210,d0
		bmi.s	me_PLAYER
		cmpi	#512-210,d1
		bmi.s	me_PLAYER
		cmpi	#512+210,d0
		bpl.s	me_PLAYER
		cmpi	#512+210,d1
		bmi.w	me_HIT

me_PLAYER:	move	sv_PosX,d4		;X,Y of player
		move	sv_PosY,d3
		sub	d6,d4
		bpl.s	.oc_e1
		neg	d4
.oc_e1:		cmpi	#300,d4
		bpl.s	me_CORNERS
		sub	d7,d3
		bpl.s	.oc_e2
		neg	d3
.oc_e2:		cmpi	#300,d3
		bpl.w	me_CORNERS
		movem	(sp),d3/d4
		asr	#2,d3
		asr	#2,d4
		add	d3,sv_AddMove
		add	d4,sv_AddMove+2
		bsr	GetRandom
		asr.b	#5,d0
		ext	d0
		add	d0,sv_Angle
		andi	#$1fe,sv_Angle
		cmpi.b	#3,12(a3)		;burning?
		bne	me_HIT
		SOUND	13,1,63
		addi	#1,sv_ENERGY
		SCROLL	71
		bra	me_HIT

me_CORNERS:	cmpi	#300,d0			;corner Left Down
		bpl.s	me_cRD
		cmpi	#300,d1			;Yd - control corners
		bpl.s	me_cLU
		move	d2,d4
		subi	#8,d4
		andi	#$7fff,d4
		tst.b	2(a1,d4.w)		;S
		bne	me_HIT
		addi	#8-512,d4
		andi	#$7fff,d4
		tst.b	3(a1,d4.w)		;W
		bne.w	me_HIT
		bra.s	me_NOTHIT
me_cLU:		cmpi	#1024-300,d1		;Left Up
		bmi.s	me_NOTHIT
		move	d2,d4
		subi	#8,d4
		andi	#$7fff,d4
		tst.b	(a1,d4.w)		;N
		bne.s	me_HIT
		addi	#8+512,d4
		andi	#$7fff,d4
		tst.b	3(a1,d4.w)		;W
		bne.s	me_HIT
		bra.s	me_NOTHIT
me_cRD:		cmpi	#1024-300,d0		;Right Down
		bmi.s	me_NOTHIT
		cmpi	#300,d1
		bpl.s	me_cRU
		move	d2,d4
		addi	#8,d4
		andi	#$7fff,d4
		tst.b	2(a1,d4.w)		;S
		bne	me_HIT
		addi	#-8-512,d4
		andi	#$7fff,d4
		tst.b	1(a1,d4.w)		;E
		bne.s	me_HIT
		bra.s	me_NOTHIT
me_cRU:		cmpi	#1024-300,d1		;Right Up
		bmi.s	me_NOTHIT
		move	d2,d4
		addi	#8,d4
		andi	#$7fff,d4
		tst.b	(a1,d4.w)		;N
		bne.s	me_HIT
		addi	#-8+512,d4
		andi	#$7fff,d4
		tst.b	1(a1,d4.w)		;E
		bne.s	me_HIT

me_NOTHIT:	move	d6,4(a3)
		move	d7,6(a3)
		moveq	#0,d0			;0 in d0 if OK.
		bra.s	me_1
me_HIT:		moveq	#-1,d0			;-1 in d0 if hit
me_1:		lea	4(sp),sp
		movem	4(a3),d1/d2
		lsr	#7,d1
		andi	#63*8,d1
		lsr	d2
		andi	#63*512,d2
		add	d2,d1
		move	(sp)+,d2
		move.b	d2,7(a1,d1.w)		;put enemy to map

;fix collumn error...
		andi.b	#$7f,1(a3)
		move.b	5(a1,d1.w),d2
		bne.s	me_2
		move.b	6(a1,d1.w),d2
		andi	#$31,d2
		beq.s	me_ColOK
me_2:		move	4(a3),d4		;enemy pos
		move	d4,d2			;middle pos
		move	6(a3),d1
		move	d1,d3
		andi	#63*1024,d2
		andi	#63*1024,d3
		addi	#512,d2			;collumn pos
		addi	#512,d3
		sub	d4,d2
		sub	d1,d3			;d2,d3 - vector 2
		sub	sv_PosX,d4
		muls	d2,d4
		sub	sv_PosY,d1		;d4,d1 - vector	1
		muls	d3,d1
		add.l	d1,d4
		bpl.s	me_ColOK
		ori.b	#$80,1(a3)
me_colOK:
;		movem.l	(sp)+,a2/a4
		rts

me_ChkDoors:	cmpi	#32,d4			;if door 1
		beq.s	.m1
		cmpi	#38,d4			;if door 2
		beq.s	.m1
		cmpi	#54,d4			;if bad door
.m1:		rts

;------------------------------------------------------------------------
;check if Enemy can shoot... (i.e. if no wall in the way)
;input: d0 - dist Enemy-Player, a3 - enemy, a1 - MAP
CanShoot:
;		move	d0,-(sp)
		move	sv_PosX,d1
		sub	4(a3),d1
		ext.l	d1
		move	sv_PosY,d2
		sub	6(a3),d2
		ext.l	d2
		lsr	#7,d0
		lsr	#3,d0			;/1024
		beq.s	.cs1
		addq	#1,d0
		divs	d0,d1
		divs	d0,d2
		subq	#1,d0			;d1 - CNT
.cs1:		movem	4(a3),d3/d4		;start pos
cs_LOOP:	move	d3,d5
		move	d4,d6			;d5/d6 - old
		add	d1,d3
		add	d2,d4			;d3/d4 - new pos
		movem	d3/d4,-(sp)

		move	d5,d7
		lsr	#7,d7
		andi	#63*8,d7
		lsr	d6
		andi	#63*512,d6
		add	d6,d7			;map pos
		add	d6,d6
		andi	#63*1024,d3		;X new
		andi	#63*1024,d4
		andi	#63*1024,d5		;X old
		andi	#63*1024,d6


		cmp	d3,d5			;LR
		beq.w	cs_UD
		bmi.s	cs_RIGHT
		cmp	d4,d6
		beq.s	cs_Lonly
		bmi.s	cs_L_UP

		moveq	#0,d5			;L-DN
		move.b	2(a1,d7.w),d4
		bsr	cs_ChkDoors2
		move.b	3(a1,d7.w),d4
		bsr	cs_ChkDoors2
		move.b	2-8(a1,d7.w),d4
		bsr	cs_ChkDoors2
		subi	#512,d7
		move.b	3(a1,d7.w),d4
		bsr	cs_ChkDoors2
		cmpi	#2,d5
		bmi.w	cs_ContLoop
		bra.w	cs_hit
cs_Lonly:	move.b	3(a1,d7.w),d4		;L
		bsr	cs_ChkDoors
		beq.w	cs_ContLoop
		bra.w	cs_hit
cs_L_UP:	moveq	#0,d5			;L-UP
		move.b	(a1,d7.w),d4
		bsr	cs_ChkDoors2
		move.b	3(a1,d7.w),d4
		bsr	cs_ChkDoors2
		subi	#8,d7
		move.b	(a1,d7.w),d4
		bsr	cs_ChkDoors2
		addi	#8+512,d7
		move.b	3(a1,d7.w),d4
		bsr	cs_ChkDoors2
		cmpi	#2,d5
		bmi.w	cs_ContLoop
		bra.w	cs_hit

cs_RIGHT:	cmp	d4,d6
		beq.s	cs_Ronly
		bmi.s	cs_R_UP
		moveq	#0,d5			;R-DN
		move.b	2(a1,d7.w),d4
		bsr	cs_ChkDoors2
		move.b	1(a1,d7.w),d4
		bsr	cs_ChkDoors2
		addi	#8,d7
		move.b	2(a1,d7.w),d4
		bsr	cs_ChkDoors2
		subi	#8+512,d7
		move.b	1(a1,d7.w),d4
		bsr	cs_ChkDoors2
		cmpi	#2,d5
		bmi.s	cs_ContLoop
		bra.s	cs_hit
cs_Ronly:	move.b	1(a1,d7.w),d4		;R
		bsr.s	cs_ChkDoors
		beq.s	cs_ContLoop
		bra.s	cs_hit
cs_R_UP:	moveq	#0,d5			;R-UP
		move.b	(a1,d7.w),d4
		bsr.s	cs_ChkDoors2
		move.b	1(a1,d7.w),d4
		bsr.s	cs_ChkDoors2
		addi	#8,d7
		move.b	(a1,d7.w),d4
		bsr.s	cs_ChkDoors2
		addi	#512-8,d7
		move.b	1(a1,d7.w),d4
		bsr.s	cs_ChkDoors2
		cmpi	#2,d5
		bmi.s	cs_ContLoop
		bra.s	cs_hit

cs_UD:		cmp	d4,d6			;only Up or Down
		beq.s	cs_ContLoop
		bmi.s	cs_UP
		move.b	2(a1,d7.w),d4		;DN
		bsr.s	cs_ChkDoors
		beq.s	cs_ContLoop
		bra.s	cs_hit
cs_UP:		move.b	(a1,d7.w),d4		;UP
		bsr.s	cs_ChkDoors
		bne.s	cs_hit

cs_ContLoop:	movem	(sp)+,d3/d4
		dbf	d0,cs_LOOP
;		move	(sp)+,d0
		moveq	#0,d1			;ok
		rts
cs_hit:		lea	4(sp),sp
;		move	(sp)+,d0
		moveq	#1,d1			;wall on way
		rts

cs_ChkDoors:	andi	#$3e,d4
		beq.s	.m1
		cmpi	#32,d4			;if door 1
		beq.s	.m1
		cmpi	#38,d4			;if door 2
		beq.s	.m1
		cmpi	#52,d4			;przezroczysta
		beq.s	.m1
		cmpi	#54,d4			;if bad door
.m1:		rts

cs_ChkDoors2:	andi	#$3e,d4
		beq.s	.m2
		cmpi	#32,d4			;if door 1
		beq.s	.m2
		cmpi	#38,d4			;if door 2
		beq.s	.m2
		cmpi	#52,d4			;przezroczysta
		beq.s	.m2
		cmpi	#54,d4			;if bad door
		beq.s	.m2
		addq	#1,d5
.m2:		rts

;------------------------------------------------------------------------
;a3 - enemy, a1 - map
es_SND:		dc.b	0,7,25,24,5,6,3,3
es_EnemyShoot:
		movem.l	ALL,-(sp)
		moveq	#0,d3
		move.b	sv_LEVELDATA+5,d3
		btst.b	#0,1(a3)
		bne.s	.me01
		move.b	sv_LEVELDATA+11,d3		;if enemy 2
.me01:		move.b	es_SND(pc,d3.w),d2
		movem	4(a3),d0/d1
		SOUND3	4				;gives dist in d4
		lea	(a3),a4				;enemy struct

		moveq	#0,d7
		cmpi.b	#1,d3
		beq.s	es_HandGun			;normal
		moveq	#2,d7
		cmpi.b	#2,d3
		beq.s	es_HandGun			;shotgun
		moveq	#1,d7
		cmpi.b	#3,d3
		beq.s	es_HandGun			;machine
		cmpi.b	#4,d3
		beq	es_Flamer
		cmpi.b	#5,d3
		beq	es_Bolter
		cmpi.b	#6,d3
		beq	es_Launcher

es_END:		movem.l	(sp)+,ALL
		bra	me_Found


;------------------------------------------------------------------------
es_HandGun:	move	d7,-(sp)
		lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		lea	2(sp),sp
		bra	es_END

.Efound:	move	#$100,(a1)
		move	#256,d6
		sub	8(a4),d6
		bsr	GetRandom
		move	d0,d1
		bsr	GetRandom
		eor	d1,d0
		asr.b	#3,d0
		ext	d0
		add	d0,d6
		move	#400/8,d1		;vector length
		bsr	es_PrepStruct0

		moveq	#19,d7			;up to 8000
.ChkCollision:	bsr.w	Object_Collision	;seek collision
		bmi.s	es_beczka1
		bne.s	.ColFound
		dbf	d7,.ChkCollision
		move.b	#0,(a1)
		bra	es_Cont

.ColFound:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)	;put object on MAP
		bsr.w	GetRandom
		andi	#3<<2,d0		;wys
		ori	#$0100,d0		;1 - odprysk
		or	d1,d0
		move	d0,(a1)			;set in structure
es_Cont:	move	(sp)+,d7
		dbf	d7,es_HandGun
		bra	es_END

es_beczka1:	lea	2(sp),sp
		bsr	ro_SetExplode
		bra	es_END

es_PrepStruct0:	move.l	a1,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		andi	#$1fe,d6
		moveq	#0,d0
		bsr	sv_Rotate
		move.l	(sp)+,a1
		move	d0,2(a1)		;add X,Y
		move	d1,4(a1)
		add	d0,d0
		move	d0,d2
		add	d0,d0
		add	d1,d1
		move	d1,d3
		add	d1,d1
		add	d2,d0
		add	d3,d1			;*6 = 300
		add	4(a4),d0
		add	6(a4),d1		;add pos
		move	d0,6(a1)		;set object structure
		move	d1,8(a1)		;pos X,Y
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0			;map pos
		move	d0,10(a1)
		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		rts

;------------------------------------------------------------------------
es_Flamer:	lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	es_END

.Efound:	move	#400/8,d1		;vector length
		move	#$300,(a1)
		move	#256,d6
		sub	8(a4),d6
	bsr	GetRandom
	asr.b	#5,d0
	ext	d0
	add	d0,d6
		bsr	es_PrepStruct0
		bsr	Object_Collision	;seek collision
		bmi.s	.es_beczka2
		bne.s	.FirstHit

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0300,d0
		or	d1,d0
		move	d0,(a1)
		bra	es_END
.FirstHit:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0603,d0
		or	d1,d0
		move	d0,(a1)
		bra	es_END
.es_beczka2:	bsr	ro_SetExplode
		bra	es_END


es_PrepStruct:	move.l	a1,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2
		move	#256,d6
		sub	8(a4),d6
	bsr	GetRandom
	asr.b	#5,d0
	ext	d0
	add	d0,d6
		andi	#$1fe,d6
		moveq	#0,d0
		bsr	sv_Rotate
		move.l	(sp)+,a1
		move	d0,2(a1)		;add X,Y
		move	d1,4(a1)
		add	d0,d0
		add	d0,d0
		add	d1,d1
		add	d1,d1
		add	4(a4),d0
		add	6(a4),d1		;add pos
		move	d0,6(a1)		;set object structure
		move	d1,8(a1)		;pos X,Y
		lsr	#7,d0
		andi	#63*8,d0
		lsr	d1
		andi	#63*512,d1
		add	d1,d0			;map pos
		move	d0,10(a1)
		lea	oc_HitPos,a2
		lea	sv_MAP,a3
		bsr	Object_Collision	;seek collision
		rts

;------------------------------------------------------------------------
es_Bolter:	lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	es_END

.Efound:	move	#600/8,d1		;vector length
		move	#$400,(a1)
		bsr	es_PrepStruct
		bne.s	.FirstHit

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0400,d0
		or	d1,d0
		move	d0,(a1)
		bra	es_END
.FirstHit:	move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0200,d0
		or	d1,d0
		move	d0,(a1)
		bra	es_END

;------------------------------------------------------------------------
es_Launcher:	lea	sv_ObjectTab,a1		;empty place in tab
		moveq	#29,d0
.SeekEmpty:	tst	(a1)
		beq.s	.Efound
		lea	12(a1),a1
		dbf	d0,.SeekEmpty
		bra	es_END

.Efound:	move	#600/8,d1		;vector length
		move	#$500,(a1)
		bsr	es_PrepStruct
		bne.s	.FirstHit

		move	10(a1),d0
		ori.b	#%10000000,6(a3,d0.w)
		move	#$0500,d0
		or	d1,d0
		move	d0,(a1)
		bra	es_END
.FirstHit:	cmpi	#2,d0			;player hit?
		seq	sv_HitFlag		;no more hits
		bsr.w	ro_SetExplode
		move	#0,sv_HitFlag
		bra	es_END


;------------------------------------------------------------------------
;d0 - Xpos, d1 - Ypos of object
Sound_Distance:	sub	sv_PosX,d0
		sub	sv_PosY,d1
		muls	d0,d0
		muls	d1,d1
		add.l	d1,d0
		bsr.s	sq_SQRT
		cmpi.l	#9990,d0
		bmi.s	.s21
		move	#0,d0
		rts
.s21:		lsl.l	#6,d0
		divu	#10000,d0
		eori	#63,d0
		rts

Sound_Distance2:sub	sv_PosX,d0
		sub	sv_PosY,d1
		muls	d0,d0
		muls	d1,d1
		add.l	d1,d0
		bsr.s	sq_SQRT
		cmpi.l	#7990,d0
		bmi.s	.s21
		move	#0,d0
		rts
.s21:		lsl.l	#6,d0
		divu	#8000,d0
		eori	#63,d0
		rts

Sound_Distance3:sub	sv_PosX,d0
		sub	sv_PosY,d1
		muls	d0,d0
		muls	d1,d1
		add.l	d1,d0
		bsr.s	sq_SQRT
		cmpi.l	#6990,d0
		bmi.s	.s22
		move	#0,d0
		rts
.s22:		lsl.l	#6,d0
		divu	#7000,d0
		eori	#63,d0
		rts

;------------------------------------------------------------------------
;Pierwiastkowanie - by Kane of Suspect, 22.12.1994
;-------------------------------------------------
;VALUE RANGE:  0 - $7fffffff (0 - 2147483647)  !!!
;Input:  d0 - value, Output: d0 - square root of value

sq_SQRT:	movem.l	d1-d6,-(sp)
		rol.l	#8,d0
		moveq	#$f,d1
sq_1:		cmp.b	sq_tab1(pc,d1.w),d0	;a*a
		beq.s	sq_2
		dbhi	d1,sq_1
sq_2:		sub.b	sq_tab1(pc,d1.w),d0

		lsl	#4,d1			;a
		move	d1,d2
		add	d2,d2			;a*2 *16
		rol.l	#8,d0
		
		moveq	#0,d4
		moveq	#0,d3
sq_3:		addq	#1,d3
		move	d4,d5
		move	d2,d4
		add	d3,d4
		mulu	d3,d4			;ab * b
		cmp	d4,d0
		bpl.s	sq_3
		sub	d5,d0
		subq	#1,d3
		add	d3,d1			;a,b

		rol.l	#8,d0			;3-rd figure
		move.l	d0,d6
		andi.l	#$ffffff,d6
		lsl	#4,d1
		move	d1,d2
		add	d2,d2

		moveq	#0,d4
		moveq	#0,d3
sq_4:		addq	#1,d3
		move.l	d4,d5
		move	d2,d4
		add	d3,d4
		mulu	d3,d4
		cmp.l	d4,d6
		bpl.s	sq_4
		sub.l	d5,d0
		subq	#1,d3
		add	d3,d1			;a,b,c

		rol.l	#8,d0			;4-th figure
		lsl	#4,d1
		move	d1,d2
		add	d2,d2

		moveq	#0,d3
sq_5:		addq	#1,d3
		move	d2,d4
		add	d3,d4
		mulu	d3,d4
		cmp.l	d4,d0
		bpl.s	sq_5
		subq	#1,d3
		add	d3,d1			;a,b,c,d
		move.l	d1,d0
		movem.l	(sp)+,d1-d6
		rts

sq_tab1:	dc.b	0,1,4,9,16,25,36,49,64,81,100,121,144,169,196,225

;-------------------------------------------------------------------
sv_DoAnims:	lea	sv_AnimOffsets,a1		;animate walls...
		lea	sv_WallOffsets,a2
		move	2(a1),d0
		addq	#4,d0
		cmpi	#8,d0
		bne.s	sv_DA1
		moveq	#0,d0
sv_DA1:		move	d0,2(a1)
		move.l	4(a1,d0.w),23*4(a2)
		lea	3*4(a1),a1
		lea	sv_BloodOffsets,a2
		move	2(a1),d0
		addq	#4,d0
		cmpi	#8,d0
		bne.s	sv_DA2
		moveq	#0,d0
sv_DA2:		move	d0,2(a1)
		move.l	4(a1,d0.w),5*4(a2)

		lea	3*4(a1),a1
		lea	sv_CollumnOffsets,a2
		moveq	#4*4,d1
		bsr.s	sv_DAdo
		lea	5*4(a1),a1
		moveq	#12*4,d1
		bsr.s	sv_DAdo
		lea	5*4(a1),a1
		moveq	#20*4,d1

sv_DAdo:	move	2(a1),d0
		addq	#4,d0
		cmpi	#4*4,d0
		bne.s	sv_DA3
		moveq	#0,d0
sv_DA3:		move	d0,2(a1)
		move.l	4(a1,d0.w),(a2,d1.w)

		addi	#2,sv_WalkState		;enemy walk anim
		andi	#7,sv_WalkState
		rts

;-------------------------------------------------------------------
;Main draw walls & objects loop - scaning from sv_MAP. No input.
DrawAll:
		movem.l	ALL,-(sp)
		lea	sv_MAP,a0
		lea	sv_sinus,a1
		lea	$80(a1),a2		;cosinus
		lea	sv_RotTable,a3
		lea	dr_sideWNES(pc),a4

		move	sv_angle,d0
		move	d0,d6			;d6 - angle - this has to remain in D6 throughout this procedure!
		rept	3
		sub	#128,d0
		bmi.s	dr_DirSet
		lea	18(a4),a4
		endr
dr_DirSet:
		move	d6,d0
		andi	#127,d0
		add		d0,d0			;*4
		move.l	(a3,d0.w),a3		;good cell addr
		move	(a3)+,d7		;nr of locs to check
		
dr_ROTLOOP:
		move	(a4),d4			;dir number
		move	sv_SquarePos,d0		;X square
		move	sv_SquarePos+2,d1	;Y square
		move.b	(a3)+,d2		;X pos
		asr.b	#2,d2
		move.b	(a3)+,d3		;Y pos
		asr.b	#2,d3

		subq	#1,d4			;make real X,Y pos
		bmi.s	dr_d4
		subq	#1,d4
		bpl.s	dr_d2
		exg	d2,d3
		neg.b	d3
		bra.s	dr_d4
dr_d2:		subq	#1,d4
		bpl.s	dr_d3
		neg.b	d2
		neg.b	d3
		bra.s	dr_d4
dr_d3:		exg	d2,d3
		neg.b	d2
dr_d4:		add.b	d2,d0			;add offset to position
		add.b	d3,d1
		andi	#63,d0			;border pos to 64
		andi	#63,d1

		lsl	#3,d0			;x*8
		lsl	#8,d1
		add	d1,d1			;y*512
		add	d1,d0			;d5 - MAP OFFSET
		move	d0,d5

		ext.w	d2
		lsl	#8,d2
		add	d2,d2
		add	d2,d2			;*1024 - X offset
		sub	sv_InSquarePos,d2	;add square pos
		move	d2,10(a4)
		addi	#1024,d2
		move	d2,14(a4)
		ext.w	d3
		lsl	#8,d3
		add	d3,d3
		add	d3,d3			;Y offset
		sub	sv_InSquarePos+2,d3
		move	d3,12(a4)
		addi	#1024,d3
		move	d3,16(a4)

;--------------------------------

dr_checkEnemy:	move	#32,sv_CollumnWid
		move	#0,sv_SecondEnemy
		move.b	7(a0,d5.w),d4		;get enemy in front
		beq.s	dr_checkITEM
		lea	sv_EnemyData,a5		;EnemyTab
		andi	#$ff,d4
		lsl	#4,d4
		move.b	1(a5,d4.w),d0
		andi	#$80,d0
		sne	sv_SecondEnemy
		bne.s	dr_checkITEM
		bsr	dr_DrawEnemy

dr_checkITEM:	move	#0,sv_Flag+4
		move	d5,-(sp)
		move.b	6(a0,d5.w),d4		;get item
		andi	#63,d4
		beq.s	dr_checkDEAD
		move	d4,-(sp)
		move	10(a4),d0		;x of collumn
		addi	#512,d0
		move	12(a4),d1		;y
		addi	#512,d1
		bsr	sv_rotate
		move	(sp)+,d4
		move	d4,d5
		andi	#32,d5			;chk heith 0-dn, 1-up
		beq.s	dr_scok3
		moveq	#0,d5
		or	#$8000,d4		;if up
		move	#1,sv_Flag+4		;draw column
		bra.s	dr_scok4
dr_scok3:	moveq	#-32,d5
		or	#$c000,d4		;if down
dr_scok4:	andi	#$c01f,d4
		addi	#28,d4
		bsr	ShowCollumns
		move	(sp),d5


dr_checkDEAD:	cmpi	#2,sv_DETAILS
		beq.s	dr_checkOBJECT
		move.b	5(a0,d5.w),d4		;get trup
		andi	#$e0,d4
		beq.s	dr_checkOBJECT
		rol.b	#3,d4
		addi	#24,d4			;after col's in table
		move	d4,-(sp)
		move	10(a4),d0
		addi	#512,d0
		move	12(a4),d1
		addi	#512,d1
		bsr	sv_rotate
		move	(sp)+,d4
		moveq	#-32,d5
		or	#$c000,d4		;always down
		bsr	ShowCollumns
		move	(sp),d5


dr_checkOBJECT:	move.b	6(a0,d5.w),d4		;get object before COL
		andi	#$80,d4
		beq.s	dr_checkCOL

		move	d7,-(sp)
		lea	sv_ObjectTab+[29*12],a5
		moveq	#29,d7
dr_SeekUsed:	move	(a5),d0
		beq.s	dr_moreSeek
		andi	#$80,d0
		beq.s	dr_moreSeek		;if behind column
		cmp	10(a5),d5		;compare offset
		bne.s	dr_moreSeek
		bsr	dr_AddObject
		move	2(sp),d5		;get offset back
dr_moreSeek:	lea	-12(a5),a5
		dbf	d7,dr_SeekUsed
		move	(sp)+,d7


dr_checkCOL:	move	#32,sv_CollumnWid
		move.b	5(a0,d5.w),d4		;get collumn
		andi	#31,d4
		beq.s	dr_checkENEMY2
		tst	sv_Flag+4
		bne.s	.dr_ccol2		;if item here
		tst	sv_DETAILS		;test detsils
		beq.s	.dr_ccol2
		cmpi	#3,d4			;low detail columns
		beq.s	dr_checkENEMY2
		cmpi	#18,d4			;beczka
		beq.s	.dr_ccol2
		cmpi	#5,d4
		bpl.s	dr_checkENEMY2

.dr_ccol2:	move	d4,-(sp)
		move	10(a4),d0		;x of collumn
		addi	#512,d0
		move	12(a4),d1		;y
		addi	#512,d1
		bsr		sv_rotate
		move	(sp)+,d4
		moveq	#0,d5
		cmpi	#9,d4
		bmi.s	dr_scok1		;if norm. collumn
		cmpi	#17,d4
		bmi.s	dr_scok2
		or		#$c000,d4		;if down col
		moveq	#-32,d5
		bra.s	dr_scok1
dr_scok2:	or	#$8000,d4		;if up col
dr_scok1:	bsr	ShowCollumns
		move	(sp),d5					;get offset back

dr_checkEnemy2:	tst	sv_SecondEnemy
		beq.s	dr_checkOBJECT2
		move.b	7(a0,d5.w),d4		;get enemy
		andi	#$ff,d4
		lsl	#4,d4
		bsr	dr_DrawEnemy

dr_checkOBJECT2:move.b	6(a0,d5.w),d4		;objects behind COLUMN
		andi	#$80,d4
		beq.s	dr_checkN

		move	d7,-(sp)
		lea		sv_ObjectTab+[29*12],a5
		moveq	#29,d7
dr_SeekUsed2:	
		move	(a5),d0
		beq.s	dr_moreSeek2
		andi	#$80,d0
		bne.s	dr_moreSeek2		;if in front of col
		cmp		10(a5),d5			;compare offset
		bne.s	dr_moreSeek2
		bsr		dr_AddObject
		move	2(sp),d5		;get offset back
dr_moreSeek2:	lea	-12(a5),a5
		dbf		d7,dr_SeekUsed2
		move	(sp)+,d7

		lea		lc_variables(pc),a6
		tst		lc_updateFrame(a6)
		beq.s	.sv_noActionUpdate1			; this is needed as otherwise shots do not show correctly as they get wiped too early
		andi.b	#$7f,6(a0,d5.w)
.sv_noActionUpdate1:

dr_checkN:	move	-2(a3),d1
		move	2(a4),d0		;check N
		btst	d0,d1
		beq.s	dr_checkE
		move.b	(a0,d5.w),d4		;get wall nr - includes blood index on 2 MSB
		beq.s	dr_checkE

		move	d4,-(sp)
		move	10(a4),d0		;x1
		move	16(a4),d1		;y1
		bsr	sv_rotate
		move	d0,d2
		move	d1,d3			;the same Y
		move	14(a4),d0
		move	16(a4),d1
		bsr	sv_rotate
		move	(sp)+,d4
		move	(sp),d5
		move.b	4(a0,d5.w),d5		;get tables
		andi	#$c0,d5
		beq.s	dr_cN0
		rol.b	#2,d5
		bsr	dr_AddTables
dr_cN0:		move	d4,d5
		andi	#$c0,d5			; filter out blood index to d5
		beq.s	dr_cN
		bsr.w	dr_AddBlood		;add blood to wall
dr_cN:		andi	#63,d4			;eliminate blood index
		bsr	ShowWalls		;draw walls
		move	(sp),d5


dr_checkE:	move	-2(a3),d1
		move	4(a4),d0		;check E
		btst	d0,d1
		beq.s	dr_checkS
		move.b	1(a0,d5.w),d4
		beq.s	dr_checkS

		move	d4,-(sp)
		move	14(a4),d0
		move	16(a4),d1
		bsr	sv_rotate
		move	d0,d2
		move	d1,d3
		move	14(a4),d0
		move	12(a4),d1
		bsr	sv_rotate
		move	(sp)+,d4
		move	(sp),d5
		move.b	4(a0,d5.w),d5		;get tables
		andi	#$30,d5
		beq.s	dr_cE0
		lsr.b	#4,d5
		bsr	dr_AddTables
dr_cE0:		move	d4,d5
		andi	#$c0,d5
		beq.s	dr_cE
		bsr.w	dr_AddBlood		;add blood to wall
dr_cE:		andi	#63,d4
		bsr	ShowWalls
		move	(sp),d5


dr_checkS:	move	-2(a3),d1
		move	6(a4),d0		;check S
		btst	d0,d1
		beq.s	dr_checkW
		move.b	2(a0,d5.w),d4
		beq.s	dr_checkW

		move	d4,-(sp)
		move	14(a4),d0
		move	12(a4),d1
		bsr	sv_rotate
		move	d0,d2
		move	d1,d3
		move	10(a4),d0
		move	12(a4),d1
		bsr	sv_rotate
		move	(sp)+,d4
		move	(sp),d5
		move.b	4(a0,d5.w),d5		;get tables
		andi	#$0c,d5
		beq.s	dr_cS0
		lsr.b	#2,d5
		bsr	dr_AddTables
dr_cS0:		move	d4,d5
		andi	#$c0,d5
		beq.s	dr_cS
		bsr.w	dr_AddBlood		;add blood to wall
dr_cS:		andi	#63,d4
		bsr	ShowWalls
		move	(sp),d5


dr_checkW:	move	-2(a3),d1
		move	8(a4),d0		;check W
		btst	d0,d1
		beq.s	dr_checkEnd
		move.b	3(a0,d5.w),d4
		beq.s	dr_checkEnd

		move	d4,-(sp)
		move	10(a4),d0
		move	12(a4),d1
		bsr	sv_rotate
		move	d0,d2
		move	d1,d3
		move	10(a4),d0
		move	16(a4),d1
		bsr	sv_rotate
		move	(sp)+,d4
		move	(sp),d5
		move.b	4(a0,d5.w),d5		;get tables
		andi	#$03,d5
		beq.s	dr_cW0
		bsr	dr_AddTables
dr_cW0:		move	d4,d5
		andi	#$c0,d5
		beq.s	dr_cW
		bsr.w	dr_AddBlood		;add blood to wall
dr_cW:		andi	#63,d4
		bsr	ShowWalls
		move	(sp),d5


dr_checkEnd:	move	(sp)+,d5
		move.l	sv_Consttab+44,a5
		move	sv_Consttab+30,d0
dr_ChkLoop0:	tst.l	(a5)+			;all rows drawn?
		bne.s	dr_DrawOn
		dbf	d0,dr_ChkLoop0
		bra.w	dr_EndRot
dr_DrawOn:
		dbf	d7,dr_ROTLOOP
dr_EndRot:
		movem.l	(sp)+,ALL
		rts


dr_sideWNES:	dc.w	0,9,8,1,0,0,0,0,0	;direction nr & bits
		dc.w	1,0,9,8,1,0,0,0,0	;+ x,y offsets, +1024
		dc.w	2,1,0,9,8,0,0,0,0
		dc.w	3,8,1,0,9,0,0,0,0
;-------------------------------------------------------------------
;add object to screen.
; in: d6 - rotation angle
dr_AddObject:	
		move	6(a5),d0		;x of object
		sub		sv_PosX,d0		;rot round observer
		move	8(a5),d1		;y
		sub		sv_PosY,d1
		bsr		sv_rotate

		move.b	(a5),d4			;object definition
		cmpi.b	#1,d4			;1 - odprysk
		bne.s	.dr_Object2
		move	#16,sv_CollumnWid
		move	(a5),d4
		move	d4,d5
		andi	#3,d4
		addi	#58,d4			;58
		lsr	#2,d5
		andi	#3,d5
		bne.s	.dr_O11
		moveq	#-16,d5			;odprysk up
		ori	#$a000,d4
		bra.w	dr_SetObject
.dr_O11:	cmpi.b	#1,d5
		bne.s	.dr_O12
		moveq	#0,d5			;odprysk down
		ori	#$e000,d4
		bra.w	dr_SetObject
.dr_O12:	moveq	#-8,d5			;odprysk middle
		ori	#$2000,d4
		bra.w	dr_SetObject

.dr_object2:	cmpi.b	#4,d4			;4 - prad
		bne.s	.dr_Object3
		move	#16,sv_CollumnWid
		move	(a5),d4
		andi	#1,d4
		addi	#51,d4
		moveq	#-8,d5			;middle
		ori	#$2000,d4
		bra.w	dr_SetObject
.dr_object3:	cmpi.b	#2,d4			;2 - wyladowanie
		bne.s	.dr_Object4
		move	#16,sv_CollumnWid
		move	(a5),d4
		andi	#3,d4
		addi	#55,d4
		moveq	#-8,d5			;middle
		ori	#$2000,d4
		bra.s	dr_SetObject

.dr_object4:	cmpi.b	#3,d4			;3 - fireball
		bne.s	.dr_Object5
		move	#16,sv_CollumnWid
		move	(a5),d4
		andi	#1,d4
		addi	#49,d4
		moveq	#-8,d5
		ori	#$2000,d4
		bra.s	dr_SetObject
.dr_object5:	cmpi.b	#6,d4			;6 - explosion
		bne.s	.dr_Object6
		move	#32,sv_CollumnWid
		move	(a5),d4
		andi	#7,d4
		subi	#3,d4			;0,1,2 - nothing
		bmi.s	dr_SOEnd
		addi	#61,d4
		moveq	#-16,d5
		ori	#$4000,d4
		bra.s	dr_SetObject

.dr_object6:	cmpi.b	#5,d4			;5 - rocket
		bne.s	.dr_Object7
		move	#16,sv_CollumnWid
		move	(a5),d4
		andi	#1,d4
		addi	#53,d4
		moveq	#-8,d5
		ori	#$2000,d4
;		bra.s	dr_SetObject

.dr_object7:
;		nop

dr_SetObject:	bsr	ShowCollumns
dr_SOEnd:	rts

;-------------------------------------------------------------------
;d0,a1,a2 - don't change (for sv_rotate)

dr_DrawEnemy:	movem.l	ALL,-(sp)
		lea	sv_EnemyData,a4		;EnemyTab
;		andi	#$ff,d4
;		lsl	#4,d4
		lea	(a4,d4.w),a4		;enemy structure
		movem	4(a4),d0/d1		;x,y
		sub	sv_posX,d0
		sub	sv_posY,d1
		bsr	sv_rotate

		tst.b	12(a4)
		beq.s	de_WALK
		moveq	#0,d4
		move.b	13(a4),d4
		bra.s	de_CONT

de_WALK:	lea	sv_EnemyDirTab,a3
		move	8(a4),d2
		sub	sv_angle,d2
		addi	#64,d2
		andi	#$1fe,d2
		lea	sv_EDirSub(pc),a5
.de_DirChoose:	sub	(a5)+,d2
		bmi.s	de_DirOK
		lea	8(a3),a3
		bra.s	.de_DirChoose
de_dirOK:
		move	sv_WalkState,d4
		move	(a3,d4.w),d4

de_CONT:	lea	sv_Enemy1Offsets,a3
		btst.b	#0,1(a4)
		bne.s	de_enemy2
		lea	sv_Enemy2Offsets,a3
de_enemy2:
		bsr	ShowEnemy

		movem.l	(sp)+,ALL
		rts

sv_EDirSub:	dc.w	128,64+32,64,64,64,64+32
;-------------------------------------------------------------------
; determine if blood needs to be added
; in: d4 - wall index, d5 - blood index (from map)
; out: d4 - wall index (this is the blood buffer if blood is to be added)
dr_AddBlood:
		cmpi	#2,SV_DETAILS
		bne.s	.dr_AB1
		rts
.dr_AB1:	
		movem.l	a1-a3,-(sp)
		rol.b	#2,d5
		subq	#1,d5
		add		d5,d5
		add		d5,d5
		lea		sv_Bloodoffsets,a1
		move.l	(a1,d5.w),d5			; offset of blood on texture map
		move.l	sv_Consttab+12,a1		; this is the start of the Walls2 textures
		lea		(a1,d5.l),a1			; blood start
		move.l	sv_WallOffsets+[27*4],d5	; why 27th wall? Unless narrow collumns count as 1
		move.l	sv_Consttab+12,a2
		lea		(a2,d5.l),a2			;buffor start - this is the last "square" in the textures
		andi	#$3f,d4
		move	d4,d5
		andi	#1,d5
		move	d5,-(sp)				;save LSB - wall direction from map
		lsr		d4
		subq	#1,d4
		add		d4,d4
		add		d4,d4
		lea		sv_WallOffsets,a3
		move.l	(a3,d4.w),d4
		move.l	sv_Consttab+12,a3
		lea		(a3,d4.l),a3		;wall start

		movem.l	a1-a3,ab_BloodAdr	;save regs		a1-blood, a2-dst buffer, a3-orig texture.

		moveq	#56,d4
		or		(sp)+,d4
		movem.l	(sp)+,a1-a3
		rts

;-------------------------------------------------------------------
dr_AddTables:	movem.l	a1-a4,-(sp)
		addq	#2,d5
		add	d5,d5
		add	d5,d5
		lea	sv_Bloodoffsets,a1
		move.l	(a1,d5.w),d5
		move.l	sv_Consttab+12,a1
		lea	(a1,d5.l),a1		;blood start
		move.l	sv_WallOffsets+[27*4],d5
		move.l	sv_Consttab+12,a2
		lea	(a2,d5.l),a2		;buffor start
		move	d4,d5
		andi	#$c1,d5
		move	d5,-(sp)		;save LSB - wall dir
		andi	#$3f,d4
		lsr	d4
		subq	#1,d4
		add	d4,d4
		add	d4,d4
		lea	sv_WallOffsets,a3
		move.l	(a3,d4.w),d4
		move.l	sv_Consttab+12,a3
		lea	(a3,d4.l),a3		;wall start
		lea	1056(a2),a4		;table start on wall


		move	#64,d5
AT_loop1:	rept	4
		move.l	(a3)+,(a2)+
		move.l	(a3)+,(a2)+
		move.l	(a3)+,(a2)+
		move.l	(a3)+,(a2)+
		endr
		dbf	d5,AT_loop1

		move	#31,d5
		moveq	#0,d4
AT_loop2:	rept	4
		move.b	(a1)+,d4
		beq.s	*+4
		move.b	d4,(a4)
		move.b	(a1)+,d4
		beq.s	*+6
		move.b	d4,1(a4)
		move.b	(a1)+,d4
		beq.s	*+6
		move.b	d4,2(a4)
		move.b	(a1)+,d4
		beq.s	*+6
		move.b	d4,3(a4)
		lea	4(a4),a4
		endr
		eori	#$8000,d4
		bmi	AT_loop2
		lea	33(a1),a1
		lea	33(a4),a4
		dbf	d5,AT_loop2

		moveq	#56,d4
		or	(sp)+,d4
		movem.l	(sp)+,a1-a4
		rts


;-------------------------------------------------------------------
;Swap screens and clear tables in preparation for the next frame
mc_clearScreen:	
	; ------------- clear screen area, if no textured floor then fill
		move	lc_CpuType(pc),d2
		beq.s	.NoCache
		move.l	sv_ChunkyBuffer,a1
		move.l	sv_Fillcols,d0
		move	sv_ViewWidth,d1				; in bytes = pixes / 8
		mulu	sv_ViewHeigth,d1			; results in half the screen in longs
		tst		sv_Floor					; 1 = draw floor. 0 = no floor (fill whole screen)
		bne.s	.FillFloor
		subq	#1,d1
		move	d1,d2
										; TODO - make better use of cache, or maybe use blitter + CPU?
.ClrVga1:	move.l	d0,(a1)+			; TODO - check if it's not too much as it's in longs?
		dbf	d1,.ClrVga1
		move.l	sv_Fillcols+4,d0
.ClrVga2:	move.l	d0,(a1)+
		dbf	d2,.ClrVga2
		rts

.FillFloor:	
		move	d1,d2
		lsr		#2,d2
		add		d2,d1						; add 1/4 more?
.ClrVga3:	move.l	d0,(a1)+
		dbf	d1,.ClrVga3
		rts

		; blitter fill ceiling/floor
.NoCache:	move	sv_ViewWidth,d2
		add	d2,d2				; width in words. E.g. max 24 scren bytes -> 192 pixels, i.e. 192 bytes in the chunky table
		move	sv_ViewHeigth,d1
		tst	sv_Floor
		beq.s	.NoFloor
		move	d1,d0
		lsr	#2,d0
		add	d0,d1				; add 1/4 ? why?
.NoFloor:	lsl	#6,d1			;*heigth
		or	d2,d1
		lea	$dff000,a0
		waitblt				;clr SVGA table
		move	#$8440,$96(a0)		;blitter NASTY & DMA on..
		move.l	#-1,$44(a0)
		move	sv_Fillcols,$74(a0)			; ceiling colour
		move	#0,$66(a0)
		move.l	#$01f00000,$40(a0)
		move.l	sv_ChunkyBuffer,$54(a0)
		move	d1,$58(a0)
		tst	sv_Floor						; floor colour
		beq.s	.NF2
		rts
.NF2:
		waitblt				;clr SVGA table - down
		move	sv_Fillcols+4,$74(a0)
		move	d1,$58(a0)
;		waitblt				;cls
		rts

;-------------------------------------------------------------------
; show user map
ServeMap:	movem.l	ALL,-(sp)
		eori	#1,sv_Pause
		lea		$dff000,a0
		bsr		SetLocation
		bsr		p_FadeColors

		lea		sv_WindowSav,a1
		lea		Screen1,a2
		addi.l	#[sv_Upoffset*5*row],a2
		moveq	#0,d0
		move	#[130*5]-1,d7
.sv_GetWindow:	move.l	(a1)+,(a2)+		;get background
		move.l	(a1)+,(a2)+
		REPT	6
		move.l	d0,(a2)+
		ENDR
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		dbf	d7,.sv_GetWindow

		lea		cop_ACTUAL,a1
		move.l	(a1),sv_OldCop
		move.l	4(a1),sv_OldCop+4	;save old
		move.l	cop_borders,sv_OldCop+8
		VBLANK
		move.l	#$90f2c4,cop_borders
		move.l	#copper1_std,d0				; set non-stretched copper
		move	d0,6(a1)
		swap	d0
		move	d0,2(a1)

		lea		copper1_std,a1
		move.l	#cop1_screen1,d0			; draw map on screen 1
		move	d0,6(a1)
		swap	d0
		move	d0,2(a1)
		
		bsr		DrawUserMap
		bsr		p_SetColors
		SCROLL	74
sm_Wait:	tst	sv_MapOn
		beq.s	sm_Wait2
		btst.b	#6,$bfe001
		beq.s	sm_wait2
		btst.b	#2,$dff016
		beq.s	sm_wait2
		btst.b	#7,$bfe001
		bne.s	sm_Wait
sm_Wait2:	move	#0,sv_MapOn
		bsr	p_FadeColors


		VBLANK
		lea		cop_ACTUAL,a1
		move.l	sv_OldCop,(a1)
		move.l	sv_OldCop+4,4(a1)
		move.l	sv_OldCop+8,cop_borders
		lea		Screen1,a1			; screen back
		lea		Screen2,a2		; second screen
		addi.l	#[sv_Upoffset*5*row],a1
		addi.l	#[sv_Upoffset*5*row],a2
		move	#[130*5]-1,d7
.sv_CopWin:	REPT	10
		move.l	(a2)+,(a1)+
		ENDR
		dbf	d7,.sv_CopWin

		bsr	p_SetColors
		eori	#1,sv_Pause

		lea		lc_variables(pc),a6
		TIMESTAMP	d0
		move.l	d0,lc_fps+4(a6)					; get and store start timestamp after map
		movem.l	(sp)+,ALL
		bra	MAIN_LOOP


;-------------------------------------------------------------------
; draw user map on screen
DrawUserMap:
		lea		sv_Map,a1
		lea		sv_UserMap,a2
		lea		Screen1+[sv_Upoffset*5*row]+sv_LeftOffset,a3
		move	sv_SquarePos,d0			;X pos
		move	sv_SquarePos+2,d1		;Y
		subi	#12,d0
		addi	#8,d1
		moveq	#15,d2
du_YLoop:	moveq	#23,d3
du_XLoop:	tst	d0
		bmi.s	du_X2
		tst	d1
		bmi.s	du_X2
		cmpi	#64,d0
		bpl.s	du_X2
		cmpi	#64,d1			;x,y on map?
		bpl.s	du_X2
		bsr.s	DU_PUT
du_x2:		addq	#1,d0
		lea	1(a3),a3
		dbf	d3,du_XLoop
		subi	#24,d0
		subq	#1,d1
		lea	1600-24(a3),a3
		dbf	d2,du_YLoop
		rts

;-------------------------------------------------------------------
DU_PUT:		movem	d0-d7,-(sp)
		move	d0,d4
		lsl	#3,d4
		move	d1,d5
		lsl	#7,d5
		lsl	#2,d5
		add	d5,d4			;map offset

		lsl	#3,d1
		move	d0,d2
		lsr	#3,d0
		add	d0,d1
		not	d2
		btst.b	d2,(a2,d1.w)
		beq.s	du_NotVisit		;if not visited
;		tst.l	(a1,d4.w)
;		beq.s	du_NotVisit		;if not normal map square

		moveq	#0,d1
DU_F1:		move.b	#-1,(a3,d1.w)
		move.b	#-1,40(a3,d1.w)
		move.b	#-1,80(a3,d1.w)
		addi	#200,d1
		cmpi	#1600,d1
		bne.s	DU_F1

		moveq	#0,d1
		move.b	(a1,d4.w),d0
		beq.s	du_S
		lea	(a3),a4
		bsr.w	du_WALL
du_S:		move.b	2(a1,d4.w),d0
		beq.s	du_E
		lea	7*200(a3),a4
		bsr.w	du_WALL
du_E:		move.b	1(a1,d4.w),d0
		beq.s	du_W
		move	#$fe,d1
		bsr.w	du_WALL
du_W:		move.b	3(a1,d4.w),d0
		beq.s	du_Col
		move	#$7f,d1
		bsr.w	du_WALL
du_Col:		move.b	5(a1,d4.w),d0
		bsr.w	du_Collumn
du_NotVisit:	movem	(sp)+,d0-d7
		cmp	sv_SquarePos,d0			;X pos
		bne.w	.du_0
		cmp	sv_SquarePos+2,d1		;Y
		bne.w	.du_0
		lea	200(a3),a4
		ori.b	#%01000010,(a4)			;make cross
		andi.b	#%10111101,40(a4)
		andi.b	#%10111101,80(a4)
		ori.b	#%01000010,120(a4)
		lea	200(a4),a4
		ori.b	#%00100100,(a4)
		andi.b	#%11011011,40(a4)
		andi.b	#%11011011,80(a4)
		ori.b	#%00100100,120(a4)
		lea	200(a4),a4
		ori.b	#%00011000,(a4)
		andi.b	#%11100111,40(a4)
		andi.b	#%11100111,80(a4)
		ori.b	#%00011000,120(a4)
		lea	200(a4),a4
		ori.b	#%00011000,(a4)
		andi.b	#%11100111,40(a4)
		andi.b	#%11100111,80(a4)
		ori.b	#%00011000,120(a4)
		lea	200(a4),a4
		ori.b	#%00100100,(a4)
		andi.b	#%11011011,40(a4)
		andi.b	#%11011011,80(a4)
		ori.b	#%00100100,120(a4)
		lea	200(a4),a4
		ori.b	#%01000010,(a4)
		andi.b	#%10111101,40(a4)
		andi.b	#%10111101,80(a4)
		ori.b	#%01000010,120(a4)
.du_0:		rts

du_WALL:	andi	#$3e,d0
		cmpi.b	#32,d0
		beq.s	du_No
		cmpi.b	#34,d0
		beq.s	du_No
		cmpi.b	#38,d0
		beq.s	du_No
		cmpi.b	#40,d0
		beq.s	du_No
		cmpi.b	#54,d0
		beq.s	du_No
		cmpi.b	#30,d0
		beq.s	du_door
		cmpi.b	#36,d0
		beq.s	du_door

		tst	d1
		bne.s	.du_2
		move.b	#0,40(a4)
		move.b	#0,80(a4)
		rts
.du_2:		moveq	#0,d2
.DU_3:		andi.b	d1,40(a3,d2.w)
		andi.b	d1,80(a3,d2.w)
		addi	#200,d2
		cmpi	#1600,d2
		bne.s	.DU_3
		rts
du_door:	tst	d1
		bne.s	.du_4
		move.b	#0,40(a4)
		rts
.du_4:		moveq	#0,d2
.DU_5:		andi.b	d1,40(a3,d2.w)
		addi	#200,d2
		cmpi	#1600,d2
		bne.s	.DU_5
du_No:		rts

du_Collumn:	andi	#31,d0
		cmpi.b	#1,d0
		beq.s	du_c2
		cmpi.b	#2,d0
		beq.s	du_c2
		cmpi.b	#18,d0
		beq.s	du_c2
		cmpi.b	#4,d0
		beq.s	du_c3
		rts
du_c2:		andi.b	#%11100111,400+80(a3)
		andi.b	#%11000011,600+80(a3)
		andi.b	#%11000011,800+80(a3)
		andi.b	#%11100111,1000+80(a3)
		rts
du_c3:		andi.b	#%11100111,400+80(a3)
		andi.b	#%11011011,600+80(a3)
		andi.b	#%11011011,800+80(a3)
		andi.b	#%11100111,1000+80(a3)
		rts

;-------------------------------------------------------------------
; Update areas shown on user map
UpdateMap:
		lea	sv_MAP,a1
		lea	sv_UserMap,a2
		move	sv_SquarePos,d0			;X pos
		move	sv_SquarePos+2,d1		;Y

;		move	sv_MapPos,d7			;map offset
		move	d0,d2
		move	d1,d7
		lsl	#7,d7
		lsl	#2,d7
		lsl	#3,d2
		add	d2,d7				;map offset

		bsr.w	um_Update
		tst.b	(a1,d7.w)
		bne.s	um_S
		addi	#512,d7
		addq	#1,d1
		bsr.w	um_Update
		addq	#1,d0
		tst.b	1(a1,d7.w)
		bne.s	.u1
		bsr.w	um_Update
.u1:		subq	#2,d0
		tst.b	3(a1,d7.w)
		bne.s	.u2
		bsr.w	um_Update
.u2:		addq	#1,d0
		subq	#1,d1
		subi	#512,d7
um_S:
		tst.b	2(a1,d7.w)
		bne.s	um_E
		subi	#512,d7
		subq	#1,d1
		bsr.s	um_Update
		addq	#1,d0
		tst.b	1(a1,d7.w)
		bne.s	.u3
		bsr.s	um_Update
.u3:		subq	#2,d0
		tst.b	3(a1,d7.w)
		bne.s	.u4
		bsr.s	um_Update
.u4:		addq	#1,d0
		addq	#1,d1
		addi	#512,d7
um_E:
		tst.b	1(a1,d7.w)
		bne.s	um_W
		addi	#8,d7
		addq	#1,d0
		bsr.s	um_Update
		addq	#1,d1
		tst.b	(a1,d7.w)
		bne.s	.u5
		bsr.s	um_Update
.u5:		subq	#2,d1
		tst.b	2(a1,d7.w)
		bne.s	.u6
		bsr.s	um_Update
.u6:		addq	#1,d1
		subq	#1,d0
		subi	#8,d7
um_W:
		tst.b	3(a1,d7.w)
		bne.s	um_End
		subi	#8,d7
		subq	#1,d0
		bsr.s	um_Update
		addq	#1,d1
		tst.b	(a1,d7.w)
		bne.s	.u7
		bsr.s	um_Update
.u7:		subq	#2,d1
		tst.b	2(a1,d7.w)
		bne.s	um_End
		bsr.s	um_Update
um_End:		rts

um_Update:	move	d0,d2
		move	d1,d3
		lsl	#3,d3
		lsr	#3,d2
		add	d2,d3
		move	d0,d2
		not	d2
		bset.b	d2,(a2,d3.w)
		rts

;-------------------------------------------------------------------
SetLocation:
		movem.l	d0/a1,-(sp)
		move	sv_TextOffsets+74*2,d0
		lea	sc_Text,a1
		lea	15(a1,d0.w),a1
		moveq	#0,d0
		move	sv_Squarepos,d0
		divu	#10,d0
		addi	#48,d0
		move.b	d0,(a1)
		swap	d0
		addi	#48,d0
		move.b	d0,1(a1)
		moveq	#0,d0
		move	sv_Squarepos+2,d0
		divu	#10,d0
		addi	#48,d0
		move.b	d0,3(a1)
		swap	d0
		addi	#48,d0
		move.b	d0,4(a1)
		movem.l	(sp)+,d0/a1
		rts

;-------------------------------------------------------------------
p_SetColors:	move	#0,d0
p_set1:		bsr.s	p_SetC
		addq	#1,d0
		cmpi	#17,d0
		bne.s	p_Set1
		rts

p_FadeColors:	move	#16,d0
p_Fad1:		bsr.s	p_SetC
		subq	#1,d0
		bpl.s	p_Fad1
		rts

p_SetC:		lea	$dff000,a0
		lea	RealCopper,a1		;copper
		lea	sc_COLORS,a2		;color tab
		moveq	#31,d5			;color nr. - 1
p_SC1:		move	(a2)+,d1
		move	d1,d2
		andi	#$f,d2
		mulu	d0,d2
		lsr	#4,d2
		move	d1,d3
		andi	#$f0,d3
		mulu	d0,d3
		lsr	#4,d3
		andi	#$f0,d3
		andi	#$f00,d1
		mulu	d0,d1
		lsr	#4,d1
		andi	#$f00,d1
		or	d3,d1
		or	d2,d1
		move	d1,2(a1)
		lea	4(a1),a1
		dbf	d5,p_SC1
		VBLANK
		VBLANK
		rts

;-------------------------------------------------------------------
;Draw all walls.
;Those collumns which are transparentput in a "zero table" to add to the screen later, on top of the background

ShowWalls:	
		move	#0,sv_consttab+48
		addi	#2^SHLeft,d1		;center ROT point (z+256)
		addi	#2^SHLeft,d3
		cmp	d3,d1			;d1 - Zw
		bpl.s	sh_W04			;if ok
		exg	d0,d2
		exg	d1,d3
		move	#1,sv_consttab+48
sh_W04:		cmpi	#Min_Distance,d1	;chk borders
		ble.w	sh_exit3
		cmpi	#Max_Distance,d3
		bpl.w	sh_exit3
		cmpi	#Max_Distance,d1
		bpl.w	sh_exit3

		movem.l	ALL,-(sp)

		move.l	#700,-(sp)		;plane width
		cmpi	#Min_Distance,d3
		bpl.s	sh_DrawOn		;if in range

		move	d4,a3			;save d4
		move	#Min_Distance,d7	;Cut wall to border
		sub	d3,d7			;z'
		move	d1,d6
		sub	d3,d6			;dZ
		move	d0,d5
		sub	d2,d5			;dX
		move	d5,d4			;dX
		muls	d7,d5
		divs	d6,d5			;x'=(dX*z')/dZ

		add	d5,d2			;new x2
		move	#Min_Distance,d3	;new y2

		tst	d4
		bpl.s	sh_WXorZ
		neg	d4
sh_WXorZ:	cmp	d6,d4
		bmi.s	sh_WZwX			;if dZ>dX - for accuracy

		tst	d5			;for calculation accuracy
		bpl.s	sh_WXwZ			;here are two algorithms
		neg	d5
sh_WXwZ:	move	d4,d6
		sub	d5,d6
		mulu	#700,d6
		divu	d4,d6			;w=700(dX-x')/dX
		addq	#1,d6			;cant be 0
		ext.l	d6
		move.l	d6,(sp)			;new width
		move	a3,d4
		bra.s	sh_DrawOn
sh_WZwX:	move	d6,d5
		sub	d7,d5
		mulu	#700,d5
		divu	d6,d5			;w=700(dZ-z')/dZ
		addq	#1,d5			;cant be 0
		ext.l	d5
		move.l	d5,(sp)			;new width
		move	a3,d4

;----------------------------
sh_DrawOn:	subq	#2,d4
		lea	sh_Walldir1+2(pc),a3
		lsr	d4
		bcc.s	sh_WD0

		;move	#63,(a3)		;invert direction
		;move	#0,sh_WallDir2-sh_WallDir1(a3)
		lea	sh_EorWallDir1(pc),a3
		move.w	#63,(a3)
		lea	sh_EorWallDir2(pc),a3
		move.w	#0,(a3)


		IFNE	SELECT_CACHE
		move	#63,shS_WallDir1-sh_WallDir1(a3)
		move	#0,shS_WallDir2-sh_WallDir1(a3)
		ENDC
		bra.s	sh_WD1
sh_WD0:
		;move	#0,(a3)			;do not invert
		;move	#63,sh_WallDir2-sh_WallDir1(a3)

		lea	sh_EorWallDir1(pc),a3
		move.w	#0,(a3)
		lea	sh_EorWallDir2(pc),a3
		move.w	#63,(a3)


		IFNE	SELECT_CACHE
		move	#0,shS_WallDir1-sh_WallDir1(a3)
		move	#63,shS_WallDir2-sh_WallDir1(a3)
		ENDC
sh_WD1:
		add	d4,d4
		add	d4,d4
		lea	sv_WallOffsets,a3
		move.l	(a3,d4.w),d4
		move.l	sv_Consttab+12,a3
		lea	32(a3,d4.l),a3		;required wall start

		muls	sv_Size,d0
		divs	#6,d0
		muls	sv_Size,d2
		divs	#6,d2

		ext.l	d0
		ext.l	d2
		lsl.l	#SHLeft,d0		;x1*256
		divs	d1,d0			;x1*256/(z+256)
		lsl.l	#SHLeft,d2		;x2*256
		divs	d3,d2			;x2*256/(z+256)

		tst	sv_consttab+48		;not draw 'back' walls
		bne.s	sh_cXd2
		cmp	d2,d0
		bmi	sh_exit2
		bra.s	sh_cXdOK
sh_cXd2:	cmp	d0,d2
		bmi	sh_exit2
sh_cXdOK:
		move.l	sv_Consttab+2,d4
		move.l	d4,d5
		divu	d1,d4			;y1
		divu	d3,d5			;y2

		cmp	d2,d0			;d0 - Xw
		bpl.s	sh_W0			;if ok
		exg	d0,d2
		exg	d4,d5
sh_W0:		move	sv_Consttab,d7
		add	d7,d0
		bmi	sh_exit2		;if < left border
		cmp	d7,d2
		bpl	sh_exit2		;if > right border
		add	d7,d2			;center x

		tst.l	ab_BloodAdr				; if blood needs to be added then this contains addressses to be used
		beq		sh_NoBlood
		movem.l	ab_BloodAdr,a1/a2/a4	;add blood		- a1-blood, a2-dst buffer, a4-orig texture. TODO rewrite this as it's slow??
		move	#259,d1
AB_loop1:	
		; total = 20 + 4*30 per 4 pixels = 140
		; alternative:
		; move.l (a4)+,d3		; src wall	12
		; and.l	 (a5)+,d3		; mask		6+8 = 14
		; or.l   (a1)+,d3		; blood		6+8 = 14
		; move.l d3,(a2)+		; dst		12
		; total = 52
		rept	4
		move.l	(a4)+,(a2)+				; copy 4 orig pixels (20)
		move.b	(a1)+,d3				; load blood pixel (8)
		beq.s	*+6						; clear - skip pixel (10)
		move.b	d3,-4(a2)				; 12
		move.b	(a1)+,d3
		beq.s	*+6	
		move.b	d3,-3(a2)
		move.b	(a1)+,d3
		beq.s	*+6
		move.b	d3,-2(a2)
		move.b	(a1)+,d3
		beq.s	*+6
		move.b	d3,-1(a2)
		endr
		dbf	d1,AB_loop1


sh_NoBlood:	cmp	d5,d4			;d4 - Yw
		bpl.s	sh_W01			;if ok
		exg	d0,d2			;exchange X 
		exg	d4,d5			;exg Yw and Ym
sh_W01:
		move	d5,d1
		move	#350,d3
		mulu	d3,d1
		divu	d4,d1
		sub	d1,d3			;dY (91-350)
		andi	#$fffc,d3		;cut 2 LSbits
		move.l	sv_Consttab+20,a5
		move.l	(a5,d3.w),a5		;700 tab



		exg	d0,d2
		sub	d0,d2			;dX
		move	d2,-(sp)
		bpl.s	sh_Dok
		neg	d2
sh_Dok:		sub	d5,d4
		ext.l	d4
		addq	#1,d2			;to prevent div 0 error

		divu	d2,d4
		move	d4,d3			;C
		move	#0,d4
		divu	d2,d4			;R
		moveq	#0,d6
		moveq	#0,d1			;wybrana
		subq	#1,d2

		move	d5,d7
		move.l	sv_Consttab+16,a0	;fast code tab
		move.l	sv_Consttab+24,a1	;slow Htab
		lea	sv_LineTab,a4
		add	d4,d6
sh_Lloop0:	addx	d3,d1			;interpolate Y
		move	d7,d5			;Y start
		add	d1,d5			;add delta Y
		add	d5,d5
		add	d5,d5
		move.l	(a1,d5.w),4*700(a4)
		add	d4,d6			;to gain mem access speed
		move.l	(a0,d5.w),(a4)+		;Xcode not changed!
		dbf	d2,sh_Lloop0


		move.l	sv_Consttab+8,a0	;scr tab middle
		lea	sv_LineTab,a4		; addresses of pre-generated code to draw lines

		move	(sp)+,d6
		bmi.w	sh_Left			;go left...
sh_Right:
		move.l	(sp),d3			;norm 700
		addq	#2,d6

		divu	d6,d3
		move	d3,d1			;d1=C
		move	#0,d3
		divu	d6,d3			;x=R*65536/wybierz
		moveq	#0,d2
		moveq	#0,d4
		subq	#1,d6

		move	d6,d7			;cut to R_border
		add		d0,d7
		sub		sv_Consttab+6,d7
		ble.s	sh_BorOK1
		sub		d7,d6
sh_BorOK1:	
		subq	#1,d6
		subq	#1,d0

	IFNE	SELECT_CACHE
		move	lc_isCache(pc),d7
		beq		sh_Mloop1		;goto no-cache mode (pregenerated code)
;		tst	sv_Mode
;		beq.w	sh_Mloop1		;goto pre-generated code mode
		lea	4*700(a4),a4		;Htab addrss

shS_Mloop1:	add	d3,d2			;interpolation
		addx	d1,d4

		move.l	(a4)+,a6		;Htab cell start
		addq	#1,d0
		bmi.w	shS_Noline1

		moveq	#0,d7
		lea		sv_widthTable(pc),a2			; TODO: if blitter is not used then this is not necessary as chunky is linear
		move.b	(a2,d0.w),d7
		lea		(a0,d7.w),a2		;screen
		tst.b	64*192(a2)		;is column drawn?
		beq.s	shS_NoLine1

		move.b	(a5,d4.w),d7
shS_Walldir1:	eori	#0,d7			;fix wall direction
		move	d7,d5
		lsl		#6,d7
		add		d5,d7
		lea		(a3,d7.w),a1		;wall
		tst.b	32(a1)			;zero wall byte
		bne.w	shS_SaveZero1
		move.b	#0,64*192(a2)		;mark column as drawn

		movem	d0-d6,-(sp)		; TODO: d5 is not used
		moveq	#0,d0			;wall count
		moveq	#0,d4			;screen count down
		move	sv_Consttab+6,d6
		move	d6,d5
		neg	d5			;screen count up
shS_PixLoop1:	moveq	#0,d3
		move.b	(a6)+,d3
		beq.s	shS_NOPix1
		bmi.s	shS_PixEnd1		;end of cell
		move.b	(a1,d0.w),d1		;take pixel down
		not	d0
		move.b	(a1,d0.w),d2		;pixel up
		not	d0
		subq	#1,d3
shS_InnerPix1:	move.b	d1,(a2,d4.w)	; draw symetrically up and down
		add		d6,d4
		move.b	d2,(a2,d5.w)
		sub		d6,d5
		dbf		d3,shS_InnerPix1
shS_NOPix1:	addq	#1,d0
		bra.s	shS_Pixloop1
shS_PixEnd1:	movem	(sp)+,d0-d6

shS_NoLine1:	dbf	d6,shS_Mloop1
		bra		sh_exit2

shS_SaveZero1:	move.l	sv_ZeroPtr,a6
		move.l	a1,(a6)+		;wall addr
		move.l	a2,(a6)+		;screen addr
		move.l	-4(a4),(a6)+
		move	#0,(a6)+
		move.l	a6,sv_ZeroPtr
		bra.w	shS_NoLine1
	ENDC

sh_Mloop1:	add	d3,d2			;interpolation
		addx	d1,d4

		move.l	(a4)+,a6		; line drawing code address for this line
		addq	#1,d0
		bmi.s	sh_Noline1

		moveq	#0,d7
		move.b	sv_widthTable(pc,d0.w),d7
		lea	(a0,d7.w),a2		;screen
		tst.b	64*192(a2)		;is column drawn?
		beq.s	sh_NoLine1

		move.b	(a5,d4.w),d7
sh_Walldir1:	;eori	#0,d7			;fix wall direction
		move.w	sh_EorWallDir1(pc),d5
		eor.w	d5,d7

		move	d7,d5
		lsl	#6,d7
		add	d5,d7
		lea	(a3,d7.w),a1		;wall
		tst.b	32(a1)
		bne.s	sh_SaveZero1
		move.b	#0,64*192(a2)		;mark column as drawn

		jsr	(a6)				; copy 1 collumn with required Y stretching

sh_NoLine1:	dbf	d6,sh_Mloop1
		bra	sh_exit2

; save collumn for adding later because it has clear pixels
sh_SaveZero1:	
		move.l	sv_ZeroPtr,a6
		move.l	a1,(a6)+		;wall addr
		move.l	a2,(a6)+		;screen addr
		move.l	[4*700]-4(a4),(a6)+	;cell addr
		move	#0,(a6)+
		move.l	a6,sv_ZeroPtr
		bra.w	sh_NoLine1

sh_EorWallDir1:	dc.w	0
sh_EorWallDir2:	dc.w	0
sv_widthTable:	ds.b	192
;-----------------------------------------------

sh_Left: neg	d6
		move.l	(sp),d3			;norm. 700
		addq	#2,d6

		divu	d6,d3			;d1=C
		move	d3,d1
		move	#0,d3
		divu	d6,d3			;x=R*256/wybierz
		moveq	#0,d2
		moveq	#0,d4
		subq	#2,d6

		move	d0,d7			;cut to L_border
		sub	d6,d7
		bpl.s	sh_BorOK2
		add	d7,d6			;shorten wall
sh_BorOK2:
		addq	#1,d0
		move	sv_Consttab+6,-(sp)

	IFNE	SELECT_CACHE
		move	lc_isCache(pc),d7
		beq		sh_Mloop1		;goto no-cache mode (pregenerated code)
;		tst	sv_Mode
;		beq.w	sh_Mloop2		;goto fast mode
		lea		4*700(a4),a4		;Htab addrss

shS_Mloop2:	add	d3,d2			;interpolation
		addx	d1,d4

		move.l	(a4)+,a6
		subq	#1,d0
		cmp	(sp),d0
		bpl.s	shS_Noline2

		moveq	#0,d7
		lea	sv_widthTable2(pc),a2				; TODO:  not necessary for CPU mode as this is then a linear tabls 
		move.b	(a2,d0.w),d7
		lea	(a0,d7.w),a2		;screen
		tst.b	64*192(a2)		;is column drawn?
		beq.s	shS_NoLine2

		move.b	(a5,d4.w),d7
shS_Walldir2:	eori	#63,d7			;fix wall direction
		move	d7,d5
		lsl	#6,d7			;*64
		add	d5,d7			;add CL byte
		lea	(a3,d7.w),a1		;wall
		tst.b	32(a1)
		bne.w	shS_SaveZero2
		move.b	#0,64*192(a2)		;mark column as drawn

		movem	d0-d6,-(sp)
		moveq	#0,d0			;wall count
		moveq	#0,d4			;screen count down
		move	sv_Consttab+6,d6
		move	d6,d5
		neg	d5			;screen count up
shS_PixLoop2:	moveq	#0,d3
		move.b	(a6)+,d3
		beq.s	shS_NOPix2
		bmi.s	shS_PixEnd2		;end of cell
		move.b	(a1,d0.w),d1
		not	d0
		move.b	(a1,d0.w),d2
		not	d0
		subq	#1,d3
shS_InnerPix2:	move.b	d1,(a2,d4.w)
		add	d6,d4
		move.b	d2,(a2,d5.w)
		sub	d6,d5
		dbf	d3,shS_InnerPix2
shS_NOPix2:	addq	#1,d0
		bra.s	shS_Pixloop2
shS_PixEnd2:	movem	(sp)+,d0-d6

shS_NoLine2:	dbf	d6,shS_Mloop2
		lea	2(sp),sp
		bra	sh_exit2

shS_SaveZero2:	move.l	sv_ZeroPtr,a6
		move.l	a1,(a6)+		;wall addr
		move.l	a2,(a6)+		;screen addr
		move.l	-4(a4),(a6)+
		move	#0,(a6)+
		move.l	a6,sv_ZeroPtr
		bra.w	shS_NoLine2
	ENDC

sh_Mloop2:	add	d3,d2
		addx	d1,d4

		move.l	(a4)+,a6
		subq	#1,d0
		cmp	(sp),d0
		bpl.s	sh_Noline2

		moveq	#0,d7
		move.b	sv_widthTable2(pc,d0.w),d7
		lea	(a0,d7.w),a2		;screen
		tst.b	64*192(a2)		;is column drawn?
		beq.s	sh_NoLine2

		move.b	(a5,d4.w),d7
sh_Walldir2:	;eori	#63,d7			;fix wall direction
		move.w	sh_EorWallDir2(pc),d5
		eor.w	d5,d7

		move	d7,d5
		lsl	#6,d7			;*64
		add	d5,d7			;add CL byte
		lea	(a3,d7.w),a1		;wall
		tst.b	32(a1)
		bne.w	sh_SaveZero2
		move.b	#0,64*192(a2)		;mark column as drawn

		jsr	(a6)

sh_NoLine2:	dbf	d6,sh_Mloop2
		lea	2(sp),sp

sh_exit2:	
		lea		4(sp),sp
		move.l	sv_ZeroPtr,d0
		cmpi.l	#sv_ZeroTab+[14*8*192],d0
		bmi.s	.NicTo
		move.l	#sv_ZeroTab+[14*8*192],sv_ZeroPtr
.NicTo:
		movem.l	(sp)+,ALL
sh_exit3:
		move.l	#0,ab_BloodAdr
		rts

sh_SaveZero2:	move.l	sv_ZeroPtr,a6
		move.l	a1,(a6)+		;wall addr
		move.l	a2,(a6)+		;screen addr
		move.l	[4*700]-4(a4),(a6)+	;cell addr
		move	#0,(a6)+		;Y pos flag
		move.l	a6,sv_ZeroPtr
		bra.s	sh_NoLine2

sv_widthTable2:	ds.b	192
;-------------------------------------------------------------------
;a3 - enemy 1 or 2 offsets
ShowEnemy:	addi	#[2^SHLeft]-70,d1	;center ROT point (z+256)
		cmpi	#Min_Distance,d1	;chk borders
		bmi.w	sc_exit1
		cmpi	#Max_Distance,d1
		bpl.w	sc_exit1

		movem.l	ALL,-(sp)
		moveq	#0,d2			;norm coll. flag
		moveq	#0,d5
		bra.s	sc_EnemyCont

;-------------------------------------------------------------------
; This adds the collumn's vertical rows to the zerotab
ShowCollumns:	
		addi	#[2^SHLeft]-70,d1	;center ROT point (z+256)
		cmpi	#Min_Distance,d1	;chk borders
		bmi.w	sc_exit1
		cmpi	#Max_Distance,d1
		bpl.w	sc_exit1

		movem.l	ALL,-(sp)
		move	d4,d2
		andi	#$e000,d2
		andi	#$1fff,d4
		subq	#1,d4
		add	d4,d4
		add	d4,d4
		lea	sv_CollumnOffsets,a3
sc_EnemyCont:	move	d2,-(sp)		;up/down/norm coll. flag
		move.l	(a3,d4.w),d4
		move.l	sv_Consttab+12,a3
		lea	32(a3,d4.l),a3		;required collumn start
		lea	(a3,d5.w),a3		;fix up/down offset

		move	d0,d2
		subi	#256,d0			;x left - 512 wide
		addi	#256,d2			;x right
		cmpi	#32,sv_CollumnWid
		beq.s	.sc_wide32
		addi	#128,d0			;256 wide
		subi	#128,d2
.sc_wide32:	muls	sv_Size,d2
		divs	#6,d2
		muls	sv_Size,d0
		divs	#6,d0

		ext.l	d2
		ext.l	d0
		lsl.l	#SHLeft,d2		;x1*256
		divs	d1,d2			;x1*256/(z+256)
		lsl.l	#SHLeft,d0		;x2*256
		divs	d1,d0			;x2*256/(z+256)
		move.l	sv_Consttab+2,d3
		divu	d1,d3			;y

		move.l	sv_Consttab+24,a1	;slow Htab
		add	d3,d3
		add	d3,d3
		move.l	(a1,d3.w),a2		;cell addr
		move	sv_Consttab,d7
		add	d7,d2
		bmi.w	sc_exit2		;if < left border
		cmp	d7,d0
		bpl.w	sc_exit2		;if > right border
		add	d7,d0			;center x


		move	d2,d6
		sub	d0,d6			;x delta

		moveq	#0,d3
		move	sv_CollumnWid,d3	;32 or 16 wide
		addq	#2,d6
		divu	d6,d3
		move	d3,d1			;d1=C
		move	#0,d3
		divu	d6,d3			;x=R*65536/wybierz
		moveq	#0,d2
		moveq	#0,d4
		subq	#1,d6

		move	d6,d7			;cut to R_border
		add	d0,d7
		sub	sv_Consttab+6,d7
		ble.s	sc_BorOK
		sub	d7,d6
sc_BorOK:	subq	#1,d6
		subq	#1,d0

		moveq	#0,d7
		move.l	sv_Consttab+8,a0	;scr tab middle
		lea		sv_widthTable(pc),a1
		move.l	sv_ZeroPtr,a6		;zero wall table

;a0 - screen center
;a1 - width table
;a2 - cell addr
;a3 - collumn addr

sc_ColLoop:	add	d3,d2			;interpolation
		addx	d1,d4

		addq	#1,d0
		bmi.s	sc_Noline
		move.b	(a1,d0.w),d7
		lea	(a0,d7.w),a4		;screen
		tst.b	64*192(a4)		;is column drawn?
		beq.s	sc_NoLine

		move	d4,d5
		lsl		#6,d5
		add		d4,d5			;*65
		lea		(a3,d5.w),a5		;wall

		tst		(sp)
		bne.s	sc_NoChk
		tst.b	32(a5)
		bne.s	sc_NoLine		;don't draw translucient
sc_NoChk:
		move.l	a5,(a6)+		;wall addr
		move.l	a4,(a6)+		;screen addr
		move.l	a2,(a6)+		;cell addr
		move	(sp),(a6)+		;up/down/norm coll. flag

sc_NoLine:	
		dbf		d6,sc_ColLoop
		move.l	a6,d5
		cmpi.l	#sv_ZeroTab+[14*8*192],d5
		bmi.s	.NicTo
		move.l	#sv_ZeroTab+[14*8*192],d5
.NicTo:		
		move.l	d5,sv_ZeroPtr

sc_exit2:	
		lea	2(sp),sp
		movem.l	(sp)+,ALL
sc_exit1:	
		rts

;-------------------------------------------------------------------
;Add translucient parts of walls...
ShowZeroWalls:
		movem.l	a0-a6/d0-d7,-(sp)

		move.l	sv_ZeroPtr,a0		;Zero lines table
		moveq	#0,d6
		move	sv_Consttab+6,d6
		lea	sv_Zerotab,a4
		lea	shZ_WMulTab(pc),a5

shZ_ZeroLoop:
		cmpa.l	a4,a0
		beq.w	shZ_PixEnd		;if reached beg of table

		move	-(a0),d0		;what object
		beq.s	shZ_NotMinus
		cmpi	#$8000,d0
		beq	shZ_UpObject
		cmpi	#$c000,d0
		beq	shZ_DownObject
		cmpi	#$a000,d0
		beq	shZ_Down16
		cmpi	#$e000,d0
		beq	shZ_Up16
		moveq	#8,d5			;middle 16 object
		cmpi	#$2000,d0
		beq	shZ_MID_16_32
		moveq	#16,d5			;mid. 32 object
		bra	shZ_MID_16_32

shZ_NotMinus:	move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		move.l	-(a0),a1		;wall
		moveq	#0,d0			;wall count
		moveq	#0,d4			;screen count down
		move	d6,d5
		neg		d5			;screen count up

shZ_PixLoop:
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_NOPix
		bmi.s	shZ_ZeroLoop		;end of cell
		subq	#1,d3

		move.b	(a1,d0.w),d1		;take down pixel
		beq.s	shZ_Pix1_0
		not	d0
		move.b	(a1,d0.w),d2		;up pixel
		beq.s	shZ_Pix2_0
		not	d0
shZ_InnerPix12:	move.b	d1,(a2,d4.w)
		add		d6,d4
		move.b	d2,(a2,d5.w)
		sub		d6,d5
		dbf		d3,shZ_InnerPix12
shZ_NOPix:	addq	#1,d0
		bra.s	shZ_Pixloop

shZ_Pix1_0:	not	d0
		move.b	(a1,d0.w),d2
		beq.s	shZ_PixBoth0
		not		d0
		move	d3,d7
		add		d7,d7
		add		shZ_WmulTab(pc,d7.w),d4
shZ_InnerPix1:	move.b	d2,(a2,d5.w)		;draw up pixel
		sub		d6,d5
		dbf		d3,shZ_InnerPix1
		addq	#1,d0
		bra.s	shZ_Pixloop

shZ_Pix2_0:		not	d0
		move	d3,d7
		add		d7,d7
		sub		shZ_WmulTab(pc,d7.w),d5
shZ_InnerPix2:	move.b	d1,(a2,d4.w)		;draw down pixel
		add		d6,d4
		dbf		d3,shZ_InnerPix2
		addq	#1,d0
		bra.s	shZ_Pixloop

shZ_PixBoth0:	not	d0			;both are 0
		add		d3,d3
		add		shZ_WmulTab(pc,d3.w),d4	;only add on screen
		sub		shZ_WmulTab(pc,d3.w),d5
		addq	#1,d0
		bra.s	shZ_Pixloop

shZ_PixEnd:	movem.l	(sp)+,a0-a6/d0-d7
		rts

shZ_WmulTab:	ds.w	16			;1-16 * width
;-------------------------------------------------------------------
shZ_UpObject:
		move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		move.l	-(a0),a1		;wall
shZ_UpPix:	move.b	-(a1),d0
		bne.s	shZ_Upix1
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_UpPix
		bmi	shZ_ZeroLoop
		add	d3,d3
		sub	shZ_WmulTab-2(pc,d3.w),a2
		bra.s	shZ_UpPix
shZ_Upix1:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_UpPix
		bmi	shZ_ZeroLoop		;end of cell
		subq	#1,d3
shZ_UPix2:	sub.l	d6,a2
		move.b	d0,(a2)
		dbf	d3,shZ_Upix2
		bra.s	shZ_UpPix

shZ_DownObject:	move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		move.l	-(a0),a1		;wall
shZ_DownPix:	move.b	(a1)+,d0
		bne.s	shZ_Dpix1
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_DownPix
		bmi	shZ_ZeroLoop
		add	d3,d3
		add	shZ_WmulTab-2(pc,d3.w),a2
		bra.s	shZ_DownPix
shZ_Dpix1:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_DownPix
		bmi	shZ_ZeroLoop		;end of cell
		subq	#1,d3
shZ_DPix2:	move.b	d0,(a2)
		lea	(a2,d6.w),a2
		dbf	d3,shZ_Dpix2
		bra.s	shZ_DownPix

;---------------
shZ_Up16:	move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		move.l	-(a0),a1		;wall
		moveq	#16,d4
shZ_U16Pix:	subq	#1,d4
		bmi.w	shZ_ZeroLoop
		move.b	-(a1),d0
		bne.s	shZ_U16pix1
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_U16Pix
		bmi	shZ_ZeroLoop
		add	d3,d3
		sub	-2(a5,d3.w),a2
		bra.s	shZ_U16Pix
shZ_U16pix1:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_U16Pix
		bmi	shZ_ZeroLoop
		subq	#1,d3
shZ_U16pix2:	sub.l	d6,a2
		move.b	d0,(a2)
		dbf	d3,shZ_U16pix2
		bra.s	shZ_U16Pix


shZ_Down16:	move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		move.l	-(a0),a1		;wall
		moveq	#16,d4
shZ_D16Pix:	subq	#1,d4
		bmi.w	shZ_ZeroLoop
		move.b	(a1)+,d0
		bne.s	shZ_D16pix1
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_D16Pix
		bmi	shZ_ZeroLoop
		add	d3,d3
		add	-2(a5,d3.w),a2
		bra.s	shZ_D16Pix
shZ_D16pix1:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_D16Pix
		bmi	shZ_ZeroLoop
		subq	#1,d3
shZ_D16Pix2:	move.b	d0,(a2)
		lea	(a2,d6.w),a2
		dbf	d3,shZ_D16pix2
		bra.s	shZ_D16Pix


shZ_MID_16_32:	move.l	-(a0),a3		;cell
		move.l	-(a0),a2		;screen
		lea		(a2),a6
		move.l	-(a0),a1		;wall
		moveq	#-1,d4
shZ_M16Pix:	addq	#1,d4
		cmp	d5,d4			;d5= 8+1 or 16+1
		bpl	shZ_ZeroLoop
		not	d4
		move.b	(a1,d4.w),d0		;up
		not	d4
		move.b	(a1,d4.w),d1		;down
		bne.s	shZ_M16pix1
		tst.b	d0
		bne.s	shZ_M16up
		moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_M16Pix
		bmi	shZ_ZeroLoop
		add	d3,d3
		sub	-2(a5,d3.w),a2		;up scr
		add	-2(a5,d3.w),a6		;down scr
		bra.s	shZ_M16Pix
shZ_M16pix1:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_M16Pix
		bmi	shZ_ZeroLoop
		subq	#1,d3
		tst.b	d0
		bne.s	shZ_M16both
shZ_M16down:	move.b	d1,(a6)
		lea	(a6,d6.w),a6
		dbf	d3,shZ_M16down
		bra.s	shZ_M16Pix
	
shZ_M16up:	moveq	#0,d3
		move.b	(a3)+,d3
		beq.s	shZ_M16Pix
		bmi	shZ_ZeroLoop
		subq	#1,d3
shZ_M16up1:	sub.l	d6,a2
		move.b	d0,(a2)
		dbf	d3,shZ_M16up1
		bra.s	shZ_M16Pix

shZ_M16both:	move.b	d1,(a6)
		lea	(a6,d6.w),a6
		sub.l	d6,a2
		move.b	d0,(a2)
		dbf	d3,shZ_M16both
		bra.s	shZ_M16Pix

;-------------------------------------------------------------------
; JUMP EXTENSIONS
;-------------------------------------------------------------------

make_planes_pass:		bra	make_planes
sv_SetWindowSize_pass:	bra	sv_SetWindowSize
sv_MakeWidthTab_pass:	bra	sv_MakeWidthTab
mk_FixFloorMod_pass:	bra	mk_FixFloorMod
DrawBomb_pass:			bra	DrawBomb

;-------------------------------------------------------------------
; LOCAL STRUCTURES
;-------------------------------------------------------------------
;-------------------------------------------------------------------
; Local data which shoudl all be referenced by x(pc)
; it might have to be moved into the middle of the code later if exceeds addressable space
lc_structure:			dc.l	0				; pointer to STRUCTURE
lc_fastmem1:			dc.l	0				; first 0.5 of mem
lc_fastmem2:			dc.l	0				; second 0.5MD of mem
lc_vbrbase:				dc.l	0
lc_cputype:				dc.w	0				; 0 - MC68000, 1 - 020 or better

lc_OldLev2:				dc.l	0
lc_OldLev3:				dc.l	0
lc_cacr_copy:			dc.l	0
lc_isCache:				dc.w	0				; 0 - no cache, 1 - cache present
lc_c2pType:				dc.w	0				; 0 - blitter C2P, 1 - CPU C2P. Actual C2P being used at the moment
lc_c2pTypePreferred:	dc.w	0				; 0 - blitter C2P, 1 - CPU C2P. Preferred C2P for the system. This allows to return to the preferred one if the game has to switch e.g. to CPU for stretching
lc_ledChange:			dc.w	0				; 1 - update debug LED status

; local variables
lc_variables:
OFFSET	0
lc_swapScreen:			dc.w	0				; 1 - trigger screen swap
lc_time:				dc.l	0				; frames on level, frames since last update
lc_updateTimer:			dc.w	0				; timer counting frames since last game logic update - controls frequency of updates
lc_updateFrame:			dc.w	0				; 0 - no anim/enemy update, 1 - update in this loop
lc_fps:					dc.l	0,0				; current fps (frames.w, scanlines.w), start frame timestamp used to calculate fps after getting end one
lc_fpsOn:				dc.w	0				; show fps flag (0-no, 1-yes)
lc_debugOn:				dc.w	0				; show debug info (averages) flag (0-no, 1-yes)
lc_drunk:				dc.w	0				; 0 - all OK, >0 time left drunk
lc_momentum:			dc.w	0				; 0 - not moving, >0 moving momentum which builds up to 255 while moving
lc_kickback:			dc.w	0				; additional crosshair kickback effect strength after shooting
ENDOFF

lc_soundList:	;list of all sounds used. Sound addr.l, length.w, freq.w
		dc.l	sv_SAMPLES
		dc.w	2680/2,$200
		dc.l	sv_SAMPLES+2680
		dc.w	750/2,300
		dc.l	sv_SAMPLES+3430
		dc.w	5270/2,$240
		dc.l	sv_SAMPLES+8700
		dc.w	902/2,500
		dc.l	sv_SAMPLES+9602
		dc.w	6416/2,$110
		dc.l	sv_SAMPLES+16018
		dc.w	1794/2,500
		dc.l	sv_SAMPLES+17812
		dc.w	4964/2,$160
		dc.l	sv_SAMPLES+22776
		dc.w	488/2,400
		dc.l	sv_SAMPLES+23264
		dc.w	4842/2,$340
		dc.l	sv_SAMPLES+28106
		dc.w	7028/2,$340
		dc.l	sv_SAMPLES+35134
		dc.w	4164/2,$240
		dc.l	sv_SAMPLES+39298
		dc.w	2806/2,$240
		dc.l	sv_SAMPLES+42104
		dc.w	3336/2,$200
		dc.l	sv_SAMPLES+45440			; fire
		dc.w	8870/2,$240
		dc.l	sv_SAMPLES+45440+8870
		dc.w	3570/2,$340
		dc.l	sv_SAMPLES+45440+12440
		dc.w	7028/2,$1e0
		dc.l	sv_SAMPLES+45440+19468
		dc.w	3746/2,$340
		dc.l	sv_SAMPLES+45440+23214
		dc.w	4798/2,$340
		dc.l	sv_SAMPLES+45440+28012
		dc.w	3268/2,$340
		dc.l	sv_SAMPLES+45440+31280
		dc.w	5306/2,$340
		dc.l	sv_SAMPLES+45440+36586
		dc.w	5218/2,$230
		dc.l	sv_SAMPLES+45440+41804
		dc.w	610/2,$210
		dc.l	sv_SAMPLES+45440+42414+100
		dc.w	[1506-100]/2,$380
		dc.l	sv_SAMPLES+45440+43920
		dc.w	8176/2,$180
		dc.l	sv_SAMPLES+45440+52096
		dc.w	4572/2,$290
		dc.l	0,0

; local additional texts
txt_welcome:			dc.b	"CITADEL (REMASTERED)",0
txt_welcome_bk:			dc.b	"WELCOME BACK!",0
txt_fps_on:				dc.b	"FPS: ON",0
txt_fps_off:			dc.b	"FPS: OFF",0
txt_crosshairs_on:		dc.b	"CROSSHAIRS: ON",0
txt_crosshairs_off:		dc.b	"CROSSHAIRS: OFF",0
txt_cache_on:			dc.b	"CPU CACHE: ON",0
txt_cache_off:			dc.b	"CPU CACHE: OFF",0
txt_c2p_cpu:			dc.b	"C2P: CPU",0
txt_c2p_blitter:		dc.b	"C2P: BLITTER",0
txt_version:			dc.b	"V1.30 BUILD ",VERSION>>28&$f+48, VERSION>>24&$f+48, ".", VERSION>>20&$f+48, VERSION>>16&$f+48, ".", VERSION>>12&$f+48, VERSION>>8&$f+48, ".", VERSION>>4&$f+48, VERSION&$f+48,0
even

EVEN

;-------------------------------------------------------------------
;-------------------------------------------------------------------
;Draw textured floors and ceilings...
; The main loop fits in 020 cache ($a0)

DRAW_CEILING: equ 1		;0 - draw only floors, 1 - draw floors and ceilings(default)
ShowFloor:	
		tst		sv_Floor			; 0 - no floors, 1 - textured floors
		beq.w	fl_quit

		movem.l	ALL,-(sp)
		lea		sv_sinus,a1
		lea		$80(a1),a2		;cosinus
		move	#256,d6
		sub		sv_angle,d6
		andi	#$1fe,d6		;d6 - inverted angle
		lea		fl_Flcoords(pc),a6
		move	sv_InSquarePos,d2	;player position x
;		sub	#512,d2			;center floor tile
		move	sv_InSquarePos+2,d3	;player position y

		rept	4
		move	(a6)+,d0		;rotate view coords
		move	(a6)+,d1
		bsr		sv_rotate
		sub		d2,d0
		asr		#4,d0			;/16
		move	d0,12(a6)
		sub		d3,d1
		asr		#4,d1			;/16
		move	d1,14(a6)
		endr


		lea		sv_FloorTab,a0
		lea		sv_LineTab,a1
		movem	(a6)+,a2-a5		;x1,y1, x2,y2 - start pos
		move	a2,d0			;x1,y1, x2,y2
		move	a3,d1
		move	a4,d2
		move	a5,d3
		sub		(a6)+,d0		;dX1
		sub		(a6)+,d1		;dY1
		sub		(a6)+,d2		;dX2
		sub		(a6)+,d3		;dY2
		move	(a0)+,d7		;nr of rows to draw
		move	d7,(a1)+
fl_MkDeltas:
		move	d0,d4			;x0
		muls	(a0),d4
		add.l	d4,d4
		swap	d4				;rescale from $8000
		add		a2,d4			;add x1 start pos
		move	d4,(a1)+
		andi	#$3f,-2(a1)		; 20 cycles

		move	d1,d5			;y0
		muls	(a0),d5
		add.l	d5,d5
		swap	d5			;rescale
		add		a3,d5			;add y1 start pos
		move	d5,(a1)+
		andi	#$3f,-2(a1)

		move	d2,d6			;x2
		muls	(a0),d6
		add.l	d6,d6
		swap	d6
		add		a4,d6			;add x2 start pos
		sub		d4,d6			;dx
		add		d6,d6			;*2
		add		d6,d6			;*2
		move	d6,(a1)+

		move	d3,d6			;y2
		muls	(a0)+,d6
		add.l	d6,d6
		swap	d6
		add		a5,d6			;add y2 start pos
		sub		d5,d6			;dy
		add		d6,d6
		add		d6,d6
		move	d6,(a1)+
		dbf		d7,fl_MkDeltas


;-----------------------------------------------
		lea		sv_LineTab,a0		;x0,y0, dx,dy
		move.l	sv_Consttab+32,a1	;floor addr
		move.l	sv_Consttab+36,a2	;ceiling addr
		move.l	sv_Consttab+40,a3	;SVGA tab floor addr - starts middle of the screen i.e.screentab+half
		move.l	sv_ChunkyBuffer,a4	;SVGA tab ceiling addr


		move	(a0)+,d7		;H counter - number of rows to draw
		lea		-2(sp),sp
		moveq	#63,d4			;AND mask
;		tst		sv_Buse
;		beq.s	fl_DRAW
		move	lc_CpuType(pc),d0
		bne.w	fl_DRAW_CACHE

; draw floor line by line using pre-generated code. Only use if cache not available
fl_DRAW:					;MAIN DRAW FLOOR LOOP
		move	d7,(sp)			;save H counter

		movem	(a0)+,d0-d3		;d0-x, d1-y  (starting texel x0,y0, vector to move left-> right dx,dy)
		move.l	a0,-(sp)

		lea		sv_DeltaTab+[600*4],a0
		move	(a0,d2.w),a5		;Rx
		moveq	#0,d6			;RLx
		move	2(a0,d2.w),d2		;Cx
		bpl.s	fl_D2
		sub		a5,d6
fl_D2:
		move	(a0,d3.w),a6		;Ry
		moveq	#0,d7			;RLy
		move	2(a0,d3.w),d3		;Cy
		bpl.s	fl_D3
		sub		a6,d7
fl_D3:	lea		fl_64MulTab(pc),a0	;64 mul tab
		moveq	#0,d5

		add		a5,d6
		addx.b	d2,d0			;inc X

fl_DoJsr:	jsr	0			;draw floor line

fl_Dcont:	
		lea		2(a3),a3	; row size, -192 for size 5. This code is self-modified by make_tables to add the proper width
		lea		4(a4),a4

		move.l	(sp)+,a0
		move	(sp),d7
		dbf		d7,fl_DRAW

		lea		2(sp),sp
		movem.l	(sp)+,ALL
fl_quit:	rts

; this floor draw proc is $a0 long so fits in cache
; This WORKS ONLY with 020+ due to the Dx*y addressing mode beind used
;CNOP 0,16  ; do not use here as for some reason it slows things down
fl_DRAW_CACHE:
flc_DRAW:	
		move	d7,(sp)
		movem	(a0)+,d0-d3		;d0-x, d1-y (starting texel x0,y0, vector to move left-> right dx,dy)
		move.l	a0,-(sp)

		lea		sv_DeltaTab+[600*4],a0
		move	(a0,d2.w),a5		;Rx
		moveq	#0,d6			;RLx
		move	2(a0,d2.w),d2		;Cx
		bpl.s	flc_D2
		sub		a5,d6
flc_D2:	move	(a0,d3.w),a6		;Ry
		moveq	#0,d7			;RLy
		move	2(a0,d3.w),d3		;Cy
		bpl.s	flc_D3
		sub		a6,d7
flc_D3:	
		lea		fl_64MulTab(pc),a0	;64 mul tab
		add		a5,d6
		addx.b	d2,d0			;inc X
		move	sv_ConstTab+30,d5	;X counter/4 -1 (48 for size 5)

flc_DCode:	
		REPT	4			; iterate over X (row) of the screen
		add		a6,d7
		addx	d3,d1			;inc texel Y offset
		and		d4,d1			; do not replace by #63 immediate as it would be slower
		add		(a0,d0.w*2),d1		;inc texel X offset = d0*64 (cycles: min 10+4=14)
		add		a5,d6
		move.b	(a1,d1.w),(a3)+	; copy pixel to floor
		addx.b	d2,d0			;inc X in texture
		IFNE	DRAW_CEILING
		move.b	(a2,d1.w),(a4)+	; copy pixel to ceiling
		ENDC
		ENDR
		dbf		d5,flc_DCode
flc_Dcont:	
		lea		2(a3),a3			; row size, -192 for size 5. This code is self-modified by make_tables to add the proper width

		move.l	(sp)+,a0
		move	(sp),d7
		dbf		d7,flc_DRAW
flc_Endloop:
		lea		2(sp),sp
		movem.l	(sp)+,ALL
		rts


fl_Flcoords:	dc.w	-96,512,96,512,-4596,12512,4596,12512
		dc.w	0,0,0,0,0,0,0,0

fl_64MulTab:
		REPT	4
VALUE:		SET	0
		REPT	64
		dc.w	VALUE
VALUE:		SET	VALUE+64
		ENDR
		ENDR

fl_DCode:	; length: 30 single segment, *192 = 5760 all
;		rept	192
		add		a6,d7
		addx	d3,d1			;inc Y
		and		d4,d1
		move	d0,d5
		add		d5,d5
		add		(a0,d5.w),d1		;wall pixel offset
		add		a5,d6			;here to omit Mem Wait
		move.b	(a1,d1.w),2(a3)
		addx.b	d2,d0			;inc X
		IFNE	DRAW_CEILING
		move.b	(a2,d1.w),4(a4)
		ENDC
;		endr
fl_DCodeEnd:
;------------------------------------------------------------------------
;Copy SVGA format to Amiga screen (blitter) - by Kane/SCT, 07.02.1994
; this is a blitter based chunky 2 planar
;a1 - screen addr to start

sv_C2PCcopy:
		move	lc_CpuType(pc),d0
		bne.w	sv2_copy
		tst	sv_Buse
		bne.w	sv2_Copy		;if use CPU only

		movem.l	ALL,-(sp)
		lea		$dff000,a0
		move.l	sv_ChunkyBuffer,a6
		move	sv_ViewWidth,d1	;view window dim.
		move	sv_ViewHeigth,d2
		subq	#1,d2
		move.l	sv_Consttab+40,a2
		lea		-2(a2,d1.w),a2		;SVGA tab end addr

		move	d2,d0
		mulu	#5*row,d0
		add	d1,d0
		lea	-2(a1,d0.w),a3		;screen end addr
		movem.l	a6/a2,-(sp)		;save oryginal tab

		move	#5*row,d0
		sub	d1,d0
		waitblt
		move	d0,$62(a0)		;B mod
		move	d0,$66(a0)		;D mod

		move	d1,d0
		lsl	#3,d0
		sub	d1,d0			;*7
		move	d0,$64(a0)		;A mod
		move.l	#-1,$44(a0)		;WLWmasks

		move	d1,d3			;d3 - width
		addq	#1,d2
		lsl	#6,d2
		lsr	#1,d1
		add	d1,d2			;d2 - Blit Size


;Copy table to screen loop...
		lea	-1,a4			;shift start
		sub	a5,a5			;0 in a5
		move	#$0de4,d1		;Bltcon0 or value

		moveq	#4,d7			;plane nr-1
		move	#$8440,$96(a0)		;blitter NASTY & DMA on..
sv_PlanesCopy:
		move	a4,d6			;shift pointer
		move	#$8080,d5
		moveq	#7,d4
sv_Bits:
		addq	#1,d6
		bmi.s	sv_BitMin
		move	d6,d0
		ror	#4,d0
		or	d1,d0			;bltcon0
		waitblt
		move	d0,$40(a0)		;bltcon0,1
		move	a5,$42(a0)
		move	d5,$70(a0)		;C dat
		move.l	a6,$50(a0)		;A addr
		move.l	a1,$4c(a0)		;B addr
		move.l	a1,$54(a0)		;D addr
		move	d2,$58(a0)
		bra.s	sv_NextBit

sv_BitMin:	move	d6,d0
		neg	d0
		ror	#4,d0
		or	d1,d0			;bltcon0
		waitblt
		move	d0,$40(a0)		;bltcon0,1
		move	#$0002,$42(a0)
		move	d5,$70(a0)		;C dat
		move.l	a2,$50(a0)		;A addr
		move.l	a3,$4c(a0)		;B addr
		move.l	a3,$54(a0)		;D addr
		move	d2,$58(a0)
sv_NextBit:
		lea	(a6,d3.w),a6		;next bit line
		lea	(a2,d3.w),a2
		lsr	d5			;mask next bit
		dbf	d4,sv_bits		;7 bit loop

		movem.l	(sp),a6/a2		;restore bit tab
		lea	row(a1),a1		;next screen plane
		lea	row(a3),a3
		subq.w	#1,a4			;shift next bit more
		dbf	d7,sv_PlanesCopy

;		move	#$440,$96(a0)		;blitter NASTY & DMA off
		lea	8(sp),sp		;fix stack
;		waitblt						; TODo it seems to hang here, maybe because hte previous write to blitter without a wait??
		movem.l	(sp)+,ALL
		rts

;-------------------------------------------------------------------
; C2P - Copy SVGA format to Amiga screen (CPU) - by Kane/SCT, 06.02.1994
; Tha main loop just about fits in cache: $f4
;a1 - screen addr to start

sv2_copy:
		movem.l	ALL,-(sp)	;use only CPU
		tst	sv_StrFlag
		bne.w	sv3_copy		;if stretch

		move.l	sv_ChunkyBuffer,a0
		lea	row(a1),a2
		lea	row(a2),a3
		lea	row(a3),a4
		lea	row(a4),a5

		move	#5*row,d0
		sub		sv_ViewWidth,d0
		move	d0,a6			;scr modulo
		move	sv_ViewWidth,d0
		subq	#1,d0
		move	d0,-(sp)		;width for dbf

		move	sv_ViewHeigth,d7
		subq	#1,d7
		lea		-2(sp),sp
		bra.s	sv2_Vertical

		nop
CNOP 0,16		; cache line alignment (16 bytes)
sv2_Vertical:
		move	d7,(sp)			;save heigth
		move	2(sp),d7		;width
sv2_Horizontal:	move.b	(a0)+,d0		;all 1 or all 0
		move.b	d0,d6
		smi	d1
		add.b	d0,d0
		smi	d2
		add.b	d0,d0
		smi	d3
		add.b	d0,d0
		smi	d4
		add.b	d0,d0
		smi	d5

		move.b	(a0)+,d0		;next bit similar?
		cmp.b	d0,d6
		bne.s	sv2_bit2
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv2_bit3
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv2_bit4
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv2_bit5
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv2_bit6
		bra	sv2_bit7_1

;		move.b	(a0)+,d0
;		cmp.b	d0,d6
;		bne.s	sv2_bit7

;		move.b	(a0)+,d0
;		cmp.b	d0,d6
;		bne.s	sv2_bit8
;		bra	sv2_SetByte

ConvBits:	macro
		add.b	d0,d0			;convert bits
		addx.b	d1,d1
		add.b	d0,d0
		addx.b	d2,d2
		add.b	d0,d0
		addx.b	d3,d3
		add.b	d0,d0
		addx.b	d4,d4
		add.b	d0,d0
		addx.b	d5,d5
		endm
		
sv2_bit2:	ConvBits
		move.b	(a0)+,d0
sv2_bit3:	ConvBits
		move.b	(a0)+,d0
sv2_bit4:	ConvBits
		move.b	(a0)+,d0
sv2_bit5:	ConvBits
		move.b	(a0)+,d0
sv2_bit6:	ConvBits
sv2_bit7_1:	move.b	(a0)+,d0
sv2_bit7:	ConvBits
sv2_bit8_1:	move.b	(a0)+,d0
sv2_bit8:	ConvBits

;sv2_SetByte:
		move.b	d1,(a1)+		;copy to screen
		move.b	d2,(a2)+
		move.b	d3,(a3)+
		move.b	d4,(a4)+
		move.b	d5,(a5)+
		dbf	d7,sv2_Horizontal
		add	a6,a1			;add modulo
		add	a6,a2
		add	a6,a3
		add	a6,a4
		add	a6,a5
		move	(sp),d7
		dbf	d7,sv2_Vertical

sv2_endloop:
		lea	4(sp),sp
		movem.l	(sp)+,ALL
		rts

;PRINTT "C2P CPU loop size"
;PRINTV sv2_endloop-sv2_Vertical

;-------------------------------------------------------------------
;CPU C2P copy to Amiga screen + stretch
; This version does NOT fit in a 256byte cache

sv3_copy:	move.l	sv_ChunkyBuffer,a0
		move.l	sv_screen,a1
		lea	sv_UpOffset*row*5(a1),a1
		lea	row(a1),a2
		lea	row(a2),a3
		lea	row(a3),a4
		lea	row(a4),a5

		move	#4*row,a6		;scr modulo
		move	sv_ViewWidth,d0
		subq	#1,d0
		move	d0,-(sp)		;width for dbf

		move	sv_ViewHeigth,d7
		subq	#1,d7
		lea		-2(sp),sp
		move	lc_CpuType(pc),d0
		bne.w	sv4_Copy		;if MC68020+cache
sv3_Vertical:
		move	d7,(sp)			;save heigth
		move	2(sp),d7		;width
sv3_Horizontal:	moveq	#0,d1
		moveq	#0,d2
		moveq	#0,d3
		moveq	#0,d4
		moveq	#0,d5
		move.b	(a0)+,d0		;all 1 or all 0
		move.b	d0,d6
		smi	d1
		add.b	d0,d0
		smi	d2
		add.b	d0,d0
		smi	d3
		add.b	d0,d0
		smi	d4
		add.b	d0,d0
		smi	d5

		move.b	(a0)+,d0		;next bit similar?
		cmp.b	d0,d6
		bne.s	sv3_bit2
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv3_bit3
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv3_bit4
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv3_bit5
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv3_bit6
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv3_bit7
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne	sv3_bit8
		bra	sv3_SetByte

sv3_bit2:	ConvBits
		move.b	(a0)+,d0
sv3_bit3:	ConvBits
		move.b	(a0)+,d0
sv3_bit4:	ConvBits
		move.b	(a0)+,d0
sv3_bit5:	ConvBits
		move.b	(a0)+,d0
sv3_bit6:	ConvBits
		move.b	(a0)+,d0
sv3_bit7:	ConvBits
		move.b	(a0)+,d0
sv3_bit8:	ConvBits

sv3_SetByte:	add	d1,d1
		move	sv3_DoubleTab(pc,d1.w),(a1)+	;copy to screen
		add	d2,d2
		move	sv3_DoubleTab(pc,d2.w),(a2)+
		add	d3,d3
		move	sv3_DoubleTab(pc,d3.w),(a3)+
		add	d4,d4
		move	sv3_DoubleTab(pc,d4.w),(a4)+
		add	d5,d5
		move	sv3_DoubleTab(pc,d5.w*2),(a5)+
		dbf	d7,sv3_Horizontal
		add	a6,a1			;add modulo
		add	a6,a2
		add	a6,a3
		add	a6,a4
		add	a6,a5
		move	(sp),d7
		dbf	d7,sv3_Vertical

		lea	4(sp),sp
		movem.l	(sp)+,ALL
		rts

sv3_DoubleTab:	ds.w	256

;-------------------------------------------------------------------
;CPU copy to Amiga screen + stretch... for 20++ and cache only!
; Fits in cache: $fa

sv4_Copy:
sv4_Vertical:	move	d7,(sp)			;save heigth
		move	2(sp),d7		;width
sv4_Horizontal:	moveq	#0,d1
		moveq	#0,d2
		moveq	#0,d3
		moveq	#0,d4
		moveq	#0,d5
		
		move.b	(a0)+,d0		;all 1 or all 0
		move.b	d0,d6
		smi	d1
		add.b	d0,d0
		smi	d2
		add.b	d0,d0
		smi	d3
		add.b	d0,d0
		smi	d4
		add.b	d0,d0
		smi	d5

		move.b	(a0)+,d0		;next bit similar?
		cmp.b	d0,d6
		bne.s	sv4_bit2
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv4_bit3
		move.b	(a0)+,d0
		cmp.b	d0,d6
		bne.s	sv4_bit4
;		move.b	(a0)+,d0
;		cmp.b	d0,d6
		bra.s	sv4_bit5_1
;		move.b	(a0)+,d0
;		bra.s	sv4_bit6

sv4_bit2:	ConvBits
		move.b	(a0)+,d0
sv4_bit3:	ConvBits
		move.b	(a0)+,d0
sv4_bit4:	ConvBits
sv4_bit5_1:	move.b	(a0)+,d0
sv4_bit5:	ConvBits
		move.b	(a0)+,d0
sv4_bit6:	ConvBits
		move.b	(a0)+,d0
sv4_bit7:	ConvBits
		move.b	(a0)+,d0
sv4_bit8:	ConvBits

sv4_SetByte:	move	sv4_DoubleTab(pc,d1.w*2),(a1)+
		move	sv4_DoubleTab(pc,d2.w*2),(a2)+
		move	sv4_DoubleTab(pc,d3.w*2),(a3)+
		move	sv4_DoubleTab(pc,d4.w*2),(a4)+
		move	sv4_DoubleTab(pc,d5.w*2),(a5)+
		dbf	d7,sv4_Horizontal
		add	a6,a1			;add modulo
		add	a6,a2
		add	a6,a3
		add	a6,a4
		add	a6,a5
		move	(sp),d7
		dbf	d7,sv4_Vertical
sv4_endloop:
		lea	4(sp),sp
		movem.l	(sp)+,ALL
		rts

;PRINTT "C2P CPU stretched loop size"
;PRINTV sv4_endloop-sv4_Vertical

sv4_DoubleTab:	ds.w	256

;-------------------------------------------------------------------
;-------------------------------------------------------------------
sv_joystick:	
		lea		sv_sinus,a1
		lea		$80(a1),a2		;cosinus
		move	#0,sv_Flag+2		;regenerate normally
		lea		cc_MoveTab,a3		;keys pressed

		lea		$dff000,a0
		move	$c(a0),d2
		move	sv_RotSpeed,d7
		move	sv_WalkSpeed,d1
		moveq	#0,d6			;joy_used flag

		btst	#1,d2
		beq.s	joy_left
		move	#1,6(a3)		; right
joy_left:	btst	#9,d2
		beq.s	joy_up
		move	#1,4(a3)		; left
joy_up:		move	d2,d3
		lsr	d3
		eori	d2,d3
		move	d3,d2
		andi	#$100,d3
		beq.s	joy_down
		move	#1,(a3)			; up
		bra.s	joy_no
joy_down:	andi	#1,d2
		beq.s	joy_no
		move	#1,2(a3)		; down

joy_no:
		tst	6(a3)			;test keys
		beq.s	.ke_Tleft
		add		d7,sv_angle
		andi	#$1fe,sv_angle	; right
		bra.s	.ke_Up
.ke_Tleft:	tst	4(a3)
		beq.s	.ke_Up
;		lsr		#1,d7
;		bne		.ke_tl2
;		moveq	#1,d7
.ke_tl2: sub		d7,sv_angle
		andi	#$1fe,sv_angle
.ke_Up:		tst	(a3)
		beq.s	.ke_Dn
		moveq	#4,d3			;up-left
		tst	8(a3)
		bne.w	sv_DoMMove
		moveq	#5,d3			;up-right
		tst	10(a3)
		bne.w	sv_DoMMove
		moveq	#0,d3			;forward
		bra.w	sv_DoMMove
.ke_Dn:		tst	2(a3)
		beq.s	.ke_Right
		moveq	#6,d3			;dn-left
		tst	8(a3)
		bne.w	sv_DoMMove
		moveq	#7,d3			;dn-right
		tst	10(a3)
		bne.w	sv_DoMMove
		moveq	#1,d3			;backward
		bra.w	sv_DoMMove
.ke_Right:	tst	10(a3)
		beq.s	.ke_Left
		moveq	#2,d3
		bra.w	sv_DoMMove
.ke_Left:	tst	8(a3)
		beq.s	.ke_No
		moveq	#3,d3
		bra.w	sv_DoMMove
.ke_No:
		bra.s	sv_MouseMove

;---------------------------------------------------------------------
;non-system read mouse routine by KANE of SUSPECT

sv_CheckMouse:	
		lea		sv_oldmouse,a3
		move	$dff00a,d0
		move	d0,d1
		andi	#$ff,d0			;x
		lsr		#8,d1			;y
		move.b	1(a3),d2
		move.b	(a3),d3
		move.b	d0,1(a3)
		move.b	d1,(a3)
		sub.b	d2,d0
		sub.b	d3,d1

		ext		d0			;dx
		ext		d1			;dy
		add		d0,sv_mouseDxy
		add		d1,sv_mouseDxy+2
		rts

sv_MouseMove:	move	sv_mouseDxy,d0

		moveq	#0,d3
		btst.b	#6,$bfe001		;LMB - move forward
		bne.s	sv_RollMouse
		move	sv_WalkSpeed,d1
		bra.s	sv_Mou3

sv_RollMouse:	move	sv_mouseDxy+2,d1
		moveq	#1,d3
		add	d1,d1
		add	d1,d1
		bpl.s	sv_Mou1
		neg	d1
		moveq	#0,d3
sv_Mou1:	move	sv_WalkSpeed,d2
		cmp	d2,d1
		bmi.s	sv_Mou3
		move	d2,d1
sv_Mou3:	
		ext.l	d0
		divs	#16,d0
		add		d0,d0

		add		d0,sv_angle		;rot observer
		andi	#$1fe,sv_angle

		move	sv_MouseDxy,d0			; clear previous mouse deltas
		bpl.s	.sv1
		neg		d0
		andi	#$000f,d0
		neg		d0
		bra.s	.sv2
.sv1:
		andi	#$000f,d0
.sv2:
		move	d0,sv_MouseDxy
		move	#0,sv_MouseDxy+2		;zero mouse move		
		bra.s	sv_DoMove_NoMouseCheck
		
sv_DoMMove:	
		; check mouse rotation so that you can move by keys and rotate by mouse simultaneously
		move	sv_mouseDxy,d0
		bra.s	sv_Mou3
		
sv_DoMove_NoMouseCheck:
		lea		lc_variables(pc),a6
		move	lc_momentum(a6),d6
		tst		d1
		beq.s	.sv_NotMoved
		move	#1,sv_Flag+2		;slow regenerating
		move	lc_fps(a6),d5		; moving - increase momentum
		addi	#1,d5
		add		d5,d5
		add		d5,d6
		cmpi	#256,d6
		bmi.s	.sv_dalej
		move	#255,d6
		bra.s	.sv_dalej
.sv_NotMoved:
		move	lc_fps(a6),d5		; not moving - decrease momentum
		addi	#1,d5
		add		d5,d5
		add		d5,d5
		sub		d5,d6
		bpl.s	.sv_dalej
		clr		d6
.sv_dalej:
		move	d6,lc_momentum(a6)	; update movement momentum
		
		move	#256,d6
		sub		sv_angle,d6
		andi	#$1fe,d6

		tst	d3
		beq.s	sv_Mou4
		move	#256,d0
		cmpi	#1,d3
		beq.s	.sv_KORYGUJ		;turn 128 degrees
		move	#-128,d0
		cmpi	#2,d3
		beq.s	.sv_KORYGUJ		;64 right
		move	#128,d0
		cmpi	#3,d3
		beq.s	.sv_KORYGUJ		;64 left
		move	#64,d0
		cmpi	#4,d3
		beq.s	.sv_KORYGUJ		;32 left
		move	#-64,d0
		cmpi	#5,d3
		beq.s	.sv_KORYGUJ		;32 right
		move	#256-64,d0
		cmpi	#6,d3
		beq.s	.sv_KORYGUJ		;back-left
		move	#256+64,d0

.sv_KORYGUJ:	add	d0,d6
		andi	#$1fe,d6
sv_Mou4:
		moveq	#0,d0
		lea		lc_variables(pc),a6
		tst		lc_drunk(a6)
		beq.s	sv_NoVodka
		tst		lc_updateFrame(a6)		; only update in update frames
		beq.s	sv_NoVodka
		subi	#1,lc_drunk(a6)

	;	move.l	sv_RomAddr,a3		;if drunk...
	;	move.b	(a3)+,d4
		RANDOM	a3,d4
		move.l	d4,d5
		swap	d5

	
	;	move.b	30(a3),d5
		rol.b	#2,d5
		eor.b	d5,d4
		andi.b	#127,d4
		lsl.b	d4
		asr.b	d4			;znak
		ext	d4
		add	d4,d0			;X add

		RANDOM	a3,d4
		move.l	d4,d5
		swap	d5
	;	move.b	(a3)+,d4
	;	move.b	40(a3),d5
		ror.b	#2,d5
		eor.b	d5,d4
		andi.b	#127,d4
		lsl.b	d4
		asr.b	d4
		ext		d4
		add		d4,d1			;Y add
		asr		#3,d4
		add		d4,d6
		andi	#$1fe,d6		;angle
		sub		d4,sv_angle
		andi	#$1fe,sv_angle

	;	move.l	a3,d4
	;	andi.l	#$ffff,d4
	;	or.l	#$000000,d4		;f90000-fa0000
	;	move.l	d4,sv_RomAddr

sv_NOVodka:	move	d1,sv_LastMove		;save vector length
		move	d6,sv_LastMove+2	;save angle
		tst		sv_LastMove+4		;if bumped wall
		beq.w	sv_DoRotIt
		subi	#60,sv_LastMove+4
		bpl.s	.sv_DM2
		move	#0,sv_LastMove+4
		bra.s	sv_DoRotIt
.sv_DM2:	
		move	sv_LastMove+4,d1
		move	sv_LastMove+6,d6
sv_DoRotIt:	
		bsr		sv_rotate
		move.l	sv_PosX,sv_LastPos
		tst		sv_AddMove
		beq.s	sv_dr2
		movem	sv_AddMove,d0/d1
		andi	#$ff,d0
		andi	#$ff,d1
sv_dr2:		add	d0,sv_PosX
		add	d1,sv_PosY
		move.l	#0,sv_AddMove
		rts

;-------------------------------------------------------------------
; search through the item table to find the next item (up or down) to select. If found select, otherwise do nothing.
; a2: sv_Items+4, d1: current item (*6), d3: direction (+6 forwards, -6 backwards)
SelectNextItem:
		move	d1,d2
.sw0:	add		d3,d1					; + or - 6
		bmi.s	.sw1
		cmpi	#10*6,d1
		bne.s	.sw2
		clr		d1
		bra.s	.sw2
.sw1:	move	#9*6,d1
.sw2:	cmp		d1,d2					; been roudn the loop?
		beq.s	.sw4_noswitch
		tst		(a2,d1.w)				; weapon/card present?
		beq.s	.sw0
		tst		4(a2,d1.w)				; amount of ammo/cards
		beq.s	.sw0

		ext		d1						; found next item
		divu	#6,d1
		moveq	#9,d4
		sub		d1,d4
		lsl		#1,d4
		addi	#$eb,d4
		move	d4,sv_Flag+6			; set item to change to
.sw4_noswitch:
		rts
		
;-------------------------------------------------------------------
;interrupt level 2 - test keys

NewLev2:	
		movem.l	d0-d4/a1/a2/a6,-(sp)
		moveq	#0,d0
		tst.b	$bfed01
		move.b	$bfec01,d0
		move	#$0008,$dff09c		;zero interrupt
		tst.b	d0
		beq.w	cc_NoSav

		lea		cc_RequestTab,a1
		lea		lc_variables(pc),a6
		move	d0,-(sp)

		tst	sv_EndLevel		;if killed
		bmi	cc_NoKey

cc_m:	cmpi.b	#$91,d0			;m - map
		bne.s	cc_IsMap
cc_m1:	eori	#1,sv_MapOn
		bra		cc_NoKey
cc_IsMap:
		tst		sv_MapOn
		bne		cc_NoKey		;if map on

cc_p:	cmpi.b	#$cd,d0			;p - pause
		bne.s	cc_esc
		eori	#1,sv_Pause
		beq.s	cc_p2
		SCROLL	37
		move	#750,sv_Pause+2
		bra		cc_NoKey
cc_p2:	SCROLL	38
		bra	cc_NoKey
cc_esc:		cmpi.b	#$75,d0			;esc - quit
		bne.s	cc_ntsc
		move	#1,(a1)
		bra		cc_NoKey
cc_ntsc:	cmpi.b	#$93,d0			;n - ntsc/pal
		bne.s	cc_cont
		cmpi	#8,sv_size+2
		bpl.s	cc_cont
		eori	#32,sv_NtscPal
		move	sv_NtscPal,$dff1dc
		bra		cc_NoKey
cc_cont:

cc_pmode:	tst	sv_PAUSE		;don't check if paused
		bne.w	cc_NoKey

		moveq	#1,d1			;pressed
		bsr.w	chk_MoveKeys	; check several move keys
		bmi.w	cc_NoKey
		addi.b	#1,d0
		moveq	#0,d1			;released
		bsr.w	chk_MoveKeys
		bmi.w	cc_NoKey
		subi.b	#1,d0

cc_z:
		cmpi.b	#$9d,d0			;z - zaklocenia on/off
		bne.s	cc_c
		move.l	lc_Structure(pc),a2
		eori.b	#1,STR_DISTURBANCES(a2)
		beq.s	.cc_z2
		SCROLL	91
		bra	cc_NoKey
.cc_z2:	SCROLL	90
		bra	cc_NoKey

cc_c:
		cmpi.b	#$99,d0			;c - crosshairs on/off
		bne.s	cc_d
		move.l	lc_Structure(pc),a2
		eori.b	#1,STR_CROSSHAIRS(a2)
		beq.s	.cc_c2
		SCROLL2	txt_crosshairs_off
		bra	cc_NoKey
.cc_c2:	SCROLL2	txt_crosshairs_on
		bra	cc_NoKey
		
cc_d:	cmpi.b	#$b7,d0			;g - details on/off
		bne.w	cc_F1_F8
		addi	#1,sv_DETAILS
		beq.s	.cc_d2
		cmpi	#2,sv_DETAILS
		beq.s	.cc_d3
		cmpi	#3,sv_DETAILS
		beq.s	.cc_d4
		SCROLL	69
		bra	cc_NoKey
.cc_d3:		SCROLL	64
		bra	cc_NoKey
.cc_d4:		move	#0,sv_DETAILS
.cc_d2:		SCROLL	63
		bra	cc_NoKey
cc_F1_F8:		cmpi.b	#$60,d0			;F1-F5 - window size (F6,F7,F8 stretched)
		bpl.s	cc_1_0
		cmpi.b	#$51,d0
		bmi.s	cc_1_0
		btst	#0,d0
		beq.s	cc_1_0
		addi	#$9e,d0			; turn into numeric
		ext	d0
		andi	#$fffe,d0
		neg	d0
		lsr	d0
		move	d0,2(a1)
		bra		cc_NoKey
cc_1_0:	cmpi.b	#$fe,d0			;1-0 - weapons + cards
		bpl.s	cc_f
		cmpi	#$eb,d0
		bmi.s	cc_f
		btst	#0,d0
		beq.s	cc_f
;		addi	#$9e,d0			;(coz prev. numeric!)
		move	d0,sv_Flag+6
		bra	cc_NoKey
cc_f:	cmpi.b	#$b9,d0			;f - floor on/off
		bne.s	cc_TAB
		eori	#1,sv_Floor
		beq.s	.cc_f2
		SCROLL	33				;"on"
		bra	cc_NoKey
.cc_f2:	SCROLL	34				;"off"
		bra	cc_NoKey

cc_TAB:	cmpi.b	#$d7,d0			;TAB, T - next weapon
		beq.s	.cc_t1
		cmpi.b	#$7b,d0			;tab - map
		bne.s	cc_r
.cc_t1:
		lea		sv_Items+4,a2
		move	-4(a2),d1				; selected item (*6 = offset)
		move	#6,d3					; search forwards
		bsr		SelectNextItem
		bra		cc_NoKey
cc_r:	cmpi.b	#$d9,d0			;R - previous weapon
		bne.s	cc_minus
		lea		sv_Items+4,a2
		move	-4(a2),d1				; selected item (*6 = offset)
		move	#-6,d3					; search backwards
		bsr		SelectNextItem
		bra		cc_NoKey
cc_minus:	cmpi.b	#$e9,d0			;- window size
		beq.s	.cc_m2
		cmpi.b	#$6b,d0
		bne		cc_plus
.cc_m2:	subi	#1,2(a1)
		cmpi	#1,2(a1)
		bne		cc_NoKey
		move	#2,2(a1)
		bra		cc_NoKey
cc_plus:	cmpi.b	#$e7,d0			;+ window size
		beq.s	.cc_p2
		cmpi.b	#$43,d0
		bne		cc_space
.cc_p2:	addi	#1,2(a1)
		cmpi	#10,2(a1)
		bne		cc_NoKey
		move	#9,2(a1)
		bra		cc_NoKey
cc_SPACE:	cmpi.b	#$7f,d0			;hand use
		bne.s	cc_Tylda
		move	#1,sv_SpaceOn
		bra		cc_NoKey
cc_Tylda:	
		cmpi.b	#$ff,d0				;quote (`) - FPS
		bne.w	cc_Backspace
		eori	#1,lc_fpsOn(a6)		; toggle fps display on/off
		beq.s	.cc_t2
		SCROLL2	txt_fps_on
		bsr		ClearBomb
		lea		lc_ledChange(pc),a2		; mark leds to be updated
		st		(a2)
		bra		cc_NoKey
.cc_t2:	SCROLL2	txt_fps_off
		move	#0,db_napisz		; do not print any bomb info
		bsr		DrawBomb
		move	#1,db_napisz
		bra		cc_NoKey
cc_Backspace:	
		cmpi.b	#$7d,d0				; backspace - debug info (only when fps is on)
		bne		cc_Version
		tst		lc_debugOn(a6)
		bne.s	.cc_b1
		tst		lc_fpsOn(a6)		; only when fps display on
		beq.s	cc_Version
.cc_b1:	eori	#1,lc_debugOn(a6)
		beq.s	.cc_b2
		bsr		DebugCntClearArea
		bra		cc_NoKey
.cc_b2:	bsr		DebugCntRedrawArea
		bra		cc_NoKey
cc_Version:
		cmpi.b	#$97,d0				; v - build version info (only when fps is on)
		bne		cc_b
		tst		lc_fpsOn(a6)
		beq.s	cc_b
		SCROLL2	txt_version
		bra		cc_NoKey
cc_b:
		cmpi.b	#$95,d0			;b - c2p mode: cpu or blitter (only when fps is on)
		bne.s	cc_cache
		tst		lc_fpsOn(a6)
		beq.s	cc_cache
		move	#1,4(a1)		; cc_RequestTab+4 - notify the main loop that the change happened 
		lea		lc_c2pTypePreferred(pc),a2
		eori	#1,(a2)
		beq.w	.cc_b2
		SCROLL2	txt_c2p_cpu		; cpu c2p
		bra.s	cc_NoKey
.cc_b2:	SCROLL2	txt_c2p_blitter	; blitter c2p
		bra.s	cc_NoKey
cc_cache:
		cmpi.b	#$b5,d0			;c - cache mode: (only when fps is on)
		bne.s	cc_cont2
		tst		lc_fpsOn(a6)
		beq.s	cc_cont2
		lea		lc_ledChange(pc),a2		; mark leds to be updated
		st		(a2)
		lea		lc_isCache(pc),a2
		eori	#1,(a2)
		beq.w	.cc_c2
		SCROLL2	txt_cache_on	; cache on
		bra.s	cc_NoKey
.cc_c2:	SCROLL2	txt_cache_off	; cache off
;		bra.s	cc_NoKey


cc_cont2:

cc_NoKey:	
		lea		sv_TextBuffer,a1	;save letter for code
		move	(a1),d0
		move	(sp)+,d1
		btst	#0,d1
		beq.s	cc_NoSav
		move.b	d1,4(a1,d0.w)
		addq	#1,d0
		andi	#15,d0
		move	d0,(a1)
		move	#1,2(a1)			; mark buffer as updated

cc_NoSav:
		move.b	#$41,$bfee01
		moveq	#2,d0
.loop1:	move.b	$dff006,d1
.loop2:	cmp.b	$dff006,d1
		beq.b	.loop2
		dbf		d0,.loop1
;		bclr	#6,$bfee01

		move.b	#0,$bfec01
		move.b	#0,$bfee01
cc_EndInt2:	
		movem.l	(sp)+,d0-d4/a1/a2/a6
		rte

; a1 - cc_RequestTab
; d1: 0 released, 1 pressed
; d0: keycode
chk_MoveKeys:	
		lea		cc_KeyTab(pc),a2
		moveq	#8,d3				; offset in cc_RequestTab
.chk_Kloop:	move	(a2)+,d2		;key code
		bmi.s	chk_KNotFound
		addq	#2,d3			;table offset - effectively starts with 10
		cmpi.b	d0,d2
		bne.s	.chk_Kloop
		move	d1,(a1,d3.w)		;set key in table, 0 released, 1 pressed
		moveq	#-1,d1
		rts
chk_KNotFound:	moveq	#0,d1
		rts

; requesttab offsets:
;  10 12 14 16 18 20 22
;  24 26 28 30 32 34 36
cc_KeyTab:	dc.w	$67,$65,$61,$63,$df,$dd,$db
		dc.w	$bf,$bd,$bb,$77,$3f,$3d,$79,-1

;-------------------------------------------------------------------
cc_FixKeys:	
		lea	cc_RequestTab,a1
		lea	cc_MoveTab,a2		;fix move keys pressed
		move	10(a1),d1		;up
		or	20(a1),d1
		move	d1,(a2)+
		move	12(a1),d1		;dn
		or	26(a1),d1
		move	d1,(a2)+
		move	14(a1),d1		;turn l.
		or	18(a1),d1
		move	d1,(a2)+
		move	16(a1),d1		;turn r.
		or	22(a1),d1
		move	d1,(a2)+
		move	24(a1),(a2)+		;left
		move	28(a1),(a2)+		;right
		move	30(a1),d1		;fire
		or	32(a1),d1
		or	34(a1),d1
		or	36(a1),d1
		move	d1,(a2)
		rts

;-------------------------------------------------------------------
;If level finished (or dead)
EndLevel:	

		; make sure previous screen swap completed before doing the c2p
		lea		lc_variables(pc),a6
.sv_ws:	tst		lc_swapScreen(a6)				; check if screen has been swapped so that any further end level stuff can be drawn
		bne.s	.sv_ws
		
		tst		sv_EndLEvel
		bmi.s	sv_Death
; survived exit
		move	cc_RequestTab+2,d0	;anything pressed?
		cmpi	#7,d0
		bmi.s	.NoStr			;if <1,6>
		move.l	sv_screen,a2
		lea		[sv_Upoffset*5*row](a2),a2
		moveq	#0,d0
		move	#[130*5]-1,d7
.Clrscr:	REPT	10
		move.l	d0,(a2)+
		ENDR
		dbf		d7,.Clrscr
		bra.s	.E2
.NoStr:		lea	sv_WindowSav,a1
		move.l	sv_screen,a2
		addi.l	#[sv_Upoffset*5*row],a2
		moveq	#0,d0
		move	#[130*5]-1,d7
.sv_GetWindow:	move.l	(a1)+,(a2)+		;get background
		move.l	(a1)+,(a2)+
		REPT	6
		move.l	d0,(a2)+
		ENDR
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		dbf		d7,.sv_GetWindow
.E2:	
		bsr		TELEPORT
		bsr		p_FadeColors
		moveq	#1,d0
		rts

; dead exit
sv_Death:	subi	#1,sv_EndLevel+2
		beq.s	sv_Death2
		moveq	#0,d0
		rts

sv_Death2:	move	#4,sv_EndLevel+2
		lea	$dff000,a0
		moveq	#60,d7
.loopY:		move	#319,d6			;Blood on screen
		lea	DeathTab(pc),a1
		move.l	sv_screen+4,a3
		move	#$8000,d3
		move	#$7fff,d4
.loopX:
		RANDOM	a2,d0
		andi	#3,d0
		addq	#1,d0			;new Y

		move	(a1),d2			;old Y
		move	d2,d1
		add		d0,d1
		cmpi	#160,d1
		bpl.s	.noNewBlood
		add		d0,(a1)+
		subq	#1,d0
		mulu	#40*5,d2
		lea		(a3,d2.w),a4
.loopYin:	
		and		d4,(a4)			; red colour
		or		d3,40(a4)
		and		d4,2*40(a4)
		or		d3,3*40(a4)
		and		d4,4*40(a4)
		lea		40*5(a4),a4
		dbf		d0,.loopYin
		bra.s	.nbc
.noNewBlood:
		lea		2(a1),a1		; move to next blood collumn
.nbc:	ror		d4
		ror		d3
		bcc.s	.de1
		lea		2(a3),a3
.de1:	dbf	d6,.loopX
		REPT 3
		VBLANK
		ENDR
		dbf	d7,.loopY

		bsr	p_FadeColors
		moveq	#-1,d0
		rts

DeathTab:	blk.w	320,0
;-------------------------------------------------------------------
;border movements - no passing thru walls, etc... + pos fix

sv_Border:	lea	sv_SquarePos,a1
		move	sv_PosX,d3		;make in-square pos.
		move	d3,d2
		rol	#6,d2			;/1024
		andi	#63,d2
		move	d2,(a1)			;X
		andi	#1023,d3		;X pos
		move	sv_PosY,d0
		move	d0,d1
		rol	#6,d1
		andi	#63,d1
		move	d1,2(a1)		;Y
		andi	#1023,d0		;Y pos
		cmp	sv_LevelData+22,d2
		bne.s	.sv_b1
		cmp	sv_LevelData+24,d1
		bne.s	.sv_b1
		move	#1,sv_EndLEvel
.sv_b1:
		lsl	#3,d2
		lsl	#8,d1
		add	d1,d1			;d1-Z, d2-X
		add	d2,d1			;d1 - pos offset
		move	d1,sv_MapPos
		lea	sv_MAP,a1


		movem	d0-d3,-(sp)
		move	d1,d6
		tst	sv_DoorFlag1+22		;not use if in prior_use!
		bne.s	br_PriorD1
		lea	sv_DoorFlag1,a2		;open/close doors
		moveq	#30,d2
		moveq	#36,d3
		bsr	br_Chk_OPEN
br_PriorD1:	move	d6,d1
		tst	sv_DoorFlag2+22
		bne.s	br_PriorD2
		lea	sv_DoorFlag2,a2
		moveq	#36,d2
		moveq	#42,d3
		bsr	br_Chk_OPEN
br_PriorD2:	movem	(sp)+,d0-d3



br_Xl:		cmpi	#min_distance-60,d3
		bpl.s	br_Xr
		move.b	3(a1,d1.w),d4
		bsr.w	br_ChkDoors
		beq.s	br_Xr
		move	#min_distance-60,d3

		move	sv_LastMove+2,d5
		move	#512,d4			;quantant nr
; --- patch start ---
		;subi	#256-62,d5
		subi	#256-7,d5
; --- patch stop ---		
		bsr.w	br_BumpWall
		bra.s	br_Yd
br_Xr:
		cmpi	#1024-min_distance+60,d3
		bmi.s	br_Yd
		move.b	1(a1,d1.w),d4
		bsr.w	br_ChkDoors
		beq.s	br_Yd
		move	#1024-min_distance+60,d3
		move	sv_LastMove+2,d5
		moveq	#0,d4
; --- patch start ---
		;addi	#62,d5
		addi	#7,d5
; --- patch stop ---
		andi	#$1fe,d5
		bsr.w	br_BumpWall
br_Yd:
		cmpi	#min_distance-60,d0
		bpl.s	br_Yu
		move.b	2(a1,d1.w),d4
		bsr.w	br_ChkDoors
		beq.s	br_Yu
		move	#min_distance-60,d0
		move	sv_LastMove+2,d5
		move	#378+378,d4
; --- patch start ---
		;subi	#378-62,d5
		subi	#378-7,d5
; --- patch stop ---
		bsr.w	br_BumpWall
		bra.s	br2_COLUMN
br_Yu:
		cmpi	#1024-min_distance+60,d0
		bmi.s	br2_COLUMN
		move.b	(a1,d1.w),d4
		bsr.w	br_ChkDoors
		beq.s	br2_COLUMN
		move	#1024-min_distance+60,d0
		move	sv_LastMove+2,d5
		move	#256,d4
; --- patch start ---
		;subi	#128-62,d5
		subi	#128-7,d5
; --- patch stop ---
		bsr.w	br_BumpWall

;---------------
br2_COLUMN:	move.b	7(a1,d1.w),d4		;if hit enemy...
		beq.w	.br2_COLUMN2
		lea	sv_EnemyData,a2		;EnemyTab
		andi	#$ff,d4
		lsl	#4,d4
		lea	(a2,d4.w),a2
		movem	4(a2),d4/d5		;X,Y of enemy
		sub	sv_PosX,d4
		bpl.s	.oc_e1
		neg	d4
.oc_e1:		cmpi	#300,d4			;Xdelta < 256?
		bpl.w	.br2_COLUMN2
		sub	sv_PosY,d5
		bpl.s	.oc_e2
		neg	d5
.oc_e2:		cmpi	#300,d5			;Ydelta too?
		bpl.w	.br2_COLUMN2

		cmpi.b	#3,12(a2)		;burning?
		bne.s	.oc_e3
		addi	#1,sv_ENERGY
		SOUND	13,1,63
		SCROLL	71
.oc_e3:		move.l	sv_LastPos,sv_PosX
		lea	sv_SquarePos,a2
		move	sv_PosX,d3		;make in-square pos.
		move	d3,d2
		rol	#6,d2			;/1024
		andi	#63,d2
		move	d2,(a2)			;X
		andi	#1023,d3
		move	d3,4(a2)		;insquare
		move	sv_PosY,d0
		move	d0,d1
		rol	#6,d1
		andi	#63,d1
		move	d1,2(a2)		;Y
		andi	#1023,d0
		move	d0,6(a2)
		bra	br2_END2

.br2_COLUMN2:	lea	sv_InSquarePos,a6	;if hitable column...
		move.b	5(a1,d1.w),d4
		andi	#31,d4
		beq.s	br2_UP			;if no column
		cmpi	#18,d4
		beq.s	.br2p			;if col $12 (beczka)
		cmpi	#5,d4
		bpl.s	br2_UP			;if not col 1 or 2 or 3
		cmpi	#3,d4			;column 3 - passable
		beq.s	br2_UP
		cmpi	#512-208,d3		;chk if in column
.br2p:		bmi.s	br2_UP
		cmpi	#512-208,d0
		bmi.s	br2_UP
		cmpi	#512+208,d3
		bpl.s	br2_UP
		cmpi	#512+208,d0
		bpl.s	br2_UP
		cmpi	#4,d4
		beq		br_FindTeleport		;if stepped into teleport
		move	(a6),d4			;old pos X
		move	2(a6),d5		;old Y

		cmpi	#512-209,d4
		bpl.s	br2_c1
		move	#512-210,d3
		bra.s	br2_c2
br2_c1:		cmpi	#512+209,d4
		bmi.s	br2_c2
		move	#512+210,d3
;		bra.s	br2_UP
br2_c2:		cmpi	#512-209,d5
		bpl.s	br2_c3
		move	#512-210,d0
		bra.s	br2_UP
br2_c3:		cmpi	#512+209,d5
		bmi.s	br2_UP
		move	#512+210,d0


br2_UP:						;check corners
		andi	#63*512,d1
		move	d1,d4			;get neighbouring posis
		addi	#512,d4
		andi	#63*512,d4
		add	d2,d4
		lea	(a1,d4.w),a2		;N
		move	d2,d4
		addi	#8,d4
		andi	#511,d4
		add	d1,d4
		lea	(a1,d4.w),a3		;E
		move	d1,d4
		subi	#512,d4
		andi	#63*512,d4
		add	d2,d4
		lea	(a1,d4.w),a4		;S
		move	d2,d4
		subi	#8,d4
		andi	#511,d4
		add	d1,d4
		lea	(a1,d4.w),a1		;W

		cmpi	#1024-min_distance+60,d0
		bmi.s	br2_DOWN
		cmpi	#min_distance-60,d3
		bpl.s	br2_Uright
		tst.b	3(a2)			;any border walls?
		bne.s	br2_U2
		tst.b	(a1)
		beq.s	br2_Uright
br2_u2:		move	#min_distance-60,d4
		sub	d3,d4
		move	d0,d5
		subi	#1024-min_distance+60,d5
		cmp	d4,d5
		bpl.s	br2_u3			;if x<z
		move	#1024-min_distance+60,d0
		bra.s	br2_Uright
br2_u3:		move	#min_distance-60,d3

br2_Uright:	cmpi	#1024-min_distance+60,d3
		bmi.s	br2_DOWN
		tst.b	1(a2)
		bne.s	br2_U4
		tst.b	(a3)
		beq.s	br2_DOWN
br2_u4:		move	d3,d4
		subi	#1024-min_distance+60,d4
		move	d0,d5
		subi	#1024-min_distance+60,d5
		cmp	d4,d5
		bpl.s	br2_u5
		move	#1024-min_distance+60,d0
		bra.s	br2_DOWN
br2_u5:		move	#1024-min_distance+60,d3

br2_DOWN:	cmpi	#min_distance-60,d0
		bpl.s	br2_END
		cmpi	#min_distance-60,d3
		bpl.s	br2_Dright
		tst.b	3(a4)
		bne.s	br2_D2
		tst.b	2(a1)
		beq.s	br2_Dright
br2_D2:		move	#min_distance-60,d4
		sub	d3,d4
		move	#min_distance-60,d5
		sub	d0,d5
		cmp	d4,d5
		bpl.s	br2_D3
		move	#min_distance-60,d0
		bra.s	br2_Dright
br2_D3:		move	#min_distance-60,d3

br2_Dright:	cmpi	#1024-min_distance+60,d3
		bmi.s	br2_END
		tst.b	1(a4)
		bne.s	br2_D4
		tst.b	2(a3)
		beq.s	br2_END
br2_D4:		move	d3,d4
		sub	#1024-min_distance+60,d4
		move	#min_distance-60,d5
		sub	d0,d5
		cmp	d4,d5
		bpl.s	br2_D5
		move	#min_distance-60,d0
		bra.s	br2_END
br2_D5:		move	#1024-min_distance+60,d3


br2_END:	move	d3,(a6)			;X pos (insquare)
		move	d0,2(a6)		;Y pos
		move	sv_PosX,d1		;fix absolute positions
		andi	#$fc00,d1
		or	d3,d1
		move	d1,sv_PosX
		move	sv_PosY,d2
		andi	#$fc00,d2
		or	d0,d2
		move	d2,sv_PosY
br2_END2:	rts


;eliminate passable walls...
;-1  - wall hit
br_ChkDoors:	move	d4,sv_BumpedWall	;remember wall
		andi	#62,d4			;if nothing
		beq.s	br_CDok
		cmpi	#32,d4			;if door 1
		beq.s	br_CDok
		cmpi	#38,d4			;if door 2
		beq.s	br_CDok
		cmpi	#54,d4			;if bad door
br_CDok:	rts

br_BumpWall:	;>0, ^128, <256, v378 (degrees of rotation)

		bmi.w	br_BW1
; --- patch start ---
		;cmpi	#124,d5
		cmpi	#124/8,d5		; bump only if < ~11 deg
; --- patch stop ---
		bpl.w	br_BW1
; --- patch start ---
		tst		sv_SzumTime			; prevent from this happening too often
		bne.w	br_BW1
; --- patch stop ---
	
		move	sv_LastMove,d5		;bump into wall
		cmpi	#120,d5
		bmi.w	br_BW1
		move	d5,sv_LastMove+4
		move	sv_LastMove+2,d5
		addi	#256,d5			;turn 180 degrees
		sub	d5,d4
		andi	#$1fe,d4
		move	d4,sv_LastMove+6
; --- patch start ---
		;move	#2,sv_SzumTime
		move	#3,sv_SzumTime
; --- patch stop ---
; --- patch start ---
		; do not lose energy on bumping walls
		;tst	sv_difficult
		;bne.s	.NieOd
		;addi	#1,sv_Energy		;loose energy
; --- patch stop ---
.NieOd:		SOUND	1,1,63
		bsr	EXCITE			;quicker beat
		move	sv_BumpedWall,d5
		andi	#62,d5
		cmpi	#30,d5
		beq.s	br_BW2
		cmpi	#36,d5
		beq.s	br_BW2
		cmpi	#34,d5			;opening doors
		beq.s	br_BW3
		cmpi	#40,d5
		beq.s	br_BW3
		SCROLL	1			;if bumped normal wall
		bra.s	br_BW1
br_BW2:		SCROLL	40			;if bumped door
		bra.s	br_BW1
br_BW3:		SCROLL	42
br_BW1:		rts

;---------------
;check doors to close/open
br_Chk_OPEN:	lea	2(a2),a3
		move.b	#0,(a2)			;close door flag
		move.b	6(a1,d1.w),d5
		andi.b	#%01000000,d5		;block door flag
		bne	br_DW1
		move.b	(a1,d1.w),d5
		cmpi.b	d2,d5			;przedzial
		bmi.s	br_DN1
		cmpi.b	d3,d5
		bpl.s	br_DN1
		move	d1,d4
		lsr	d5			;LSB
		roxl	d4			;fix 'zwrot' of wall
		move	d4,(a3)+		;offset 01
		bsr	br_ChkDoorStatus	;opened or closed?
		move	d1,d4
		addi	#512+2,d4		;oposite direction
		andi	#$7ffb,d4		;x,y, and dir only
		move.b	(a1,d4.w),d5
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		move	#-1,(a3)		;end of offsets
		move.b	#1,(a2)			;open door flag
		move	#1,20(a2)		;set Door_In_Use
br_DN1:		addq	#1,d1
		move.b	(a1,d1.w),d5
		cmpi.b	d2,d5
		bmi.s	br_DE1
		cmpi.b	d3,d5
		bpl.s	br_DE1
		move	d1,d4
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		bsr.w	br_ChkDoorStatus
		move	d1,d4
		addi	#8+2,d4
		andi	#$7ffb,d4
		move.b	(a1,d4.w),d5
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		move	#-1,(a3)
		move.b	#1,(a2)
		move	#1,20(a2)
br_DE1:		addq	#1,d1
		move.b	(a1,d1.w),d5
		cmpi.b	d2,d5
		bmi.s	br_DS1
		cmpi.b	d3,d5
		bpl.s	br_DS1
		move	d1,d4
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		bsr.s	br_ChkDoorStatus
		move	d1,d4
		subi	#512-2,d4
		andi	#$7ffb,d4
		move.b	(a1,d4.w),d5
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		move	#-1,(a3)
		move.b	#1,(a2)
		move	#1,20(a2)
br_DS1:		addq	#1,d1
		move.b	(a1,d1.w),d5
		cmpi.b	d2,d5
		bmi.s	br_DW1
		cmpi.b	d3,d5
		bpl.s	br_DW1
		move	d1,d4
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		bsr.s	br_ChkDoorStatus
		move	d1,d4
		subi	#8-2,d4
		andi	#$7ffb,d4
		move.b	(a1,d4.w),d5
		lsr	d5
		roxl	d4
		move	d4,(a3)+
		move	#-1,(a3)
		move.b	#1,(a2)
		move	#1,20(a2)
br_DW1:		rts

br_ChkDoorStatus:
		add	d5,d5
		cmp.b	d2,d5		;closed
		bne.s	br_CDS1
		move.b	#6,1(a2)	;set door CNT closed (5 or 6)
		rts
br_CDS1:	addq	#2,d2
		cmp.b	d2,d5
		bne.s	br_CDS2
		st	1(a2)		;door CNT opened (-1)
br_CDS2:	subq	#2,d2
		rts

;---------------
;find teleport in table & jump

br_FindTeleport:
		lea	sv_SwitchData,a2	;CommandTab
.rm_SeekPos:	move	(a2)+,d4
		cmpi	#-1,d4
		bne.s	.rm_SeekPos
		move	(a2)+,d4
		bmi.s	br_InactTel		;not found server
		subq	#4,d4
		cmp	d4,d1			;chk offest
		bne.s	.rm_SeekPos

		move	(a2)+,d3
		move	d3,d0
		lsr	#3,d3
		andi	#63,d3			;X
		lsr	#7,d0
		lsr	#2,d0
		andi	#63,d0			;Y

;		move	(a2)+,d3		;new x
		move	d3,sv_SquarePos
;		move	(a2)+,d0		;new y
		move	d0,sv_SquarePos+2
		ror	#6,d3			;*1024
		move	d3,sv_PosX
		ror	#6,d0			;*1024
		move	d0,sv_PosY
		move	#512,d3
		move	d3,d0
		move	#1,sv_Teleport		;do teleport
br_InactTel:	bra.w	br2_END


;-------------------------------------------------------------------
sc_DoScroll:	movem.l	a1/a2/d0/d7,-(sp)	;Move & print scroll

		move.l	sc_TextAddr,d0
		bne.s	.sc_DoText
		move.l	sc_TextAddr+4,d0
		move.l	d0,sc_TextAddr
		move.l	#0,sc_TextAddr+4
		tst.l	d0
		bne.s	.sc_DoText

		lea	sv_ScrollArea,a1		; Scroll the screen area with the text
		moveq	#6,d7
.sc_DS1:	move	#%00000,CCR		;XNZVC
		REPT	17
		roxl	-(a1)
		ENDR
		lea	[5*40]+34(a1),a1
		dbf	d7,.sc_DS1
		bra.w	.sc_EndScroll

.sc_DoText:	subi	#1,sc_TextAddr+8
		bpl.w	.sc_MakeSpace		;insert spaces
		move	#0,sc_TextAddr+8
		move.l	d0,a1
		moveq	#0,d0
		move.b	(a1),d0
		beq.w	.sc_EndTextP
		cmpi.b	#10,d0
		beq.w	.sc_EndTextP
		cmpi.b	#";",d0			;comment on text
		beq.w	.sc_EndTextP
		cmpi.b	#"^",d0
		beq.s	.sc_ChooseText
		lea	1(a1),a1		;next letter
		move.l	a1,sc_TextAddr

.sc_DoPrint:	bsr.w	.sc_MoveText
		subi	#32,d0
		lsl	#3,d0
		lea	sv_Fonts,a1
		lea	(a1,d0.w),a1
		lea	sv_ScrollArea-1,a2
		REPT	7
		move.b	(a1)+,(a2)
		lea	40*5(a2),a2
		ENDR
		bra.s	.sc_EndScroll

.sc_ChooseText:
		move.b	1(a1),d0
		cmpi.b	#"^",d0
		bne.s	.sc_CT1
		moveq	#3,d0
		bra.s	.sc_CT2
.sc_CT1:	lsl	#8,d0
		move.b	2(a1),d0
.sc_CT2:	move	d0,d7
		addq	#1,d0			;save next offset
		move.b	d0,2(a1)
		lsr	#8,d0
		move.b	d0,1(a1)
		lea	(a1,d7.w),a2

		moveq	#0,d0
		move.b	(a2),d0
		beq.s	.sc_EndTextP
		cmpi.b	#10,d0
		beq.s	.sc_EndTextP
		cmpi.b	#"@",d0			;go to beggining
		bne.w	.sc_DoPrint
		move.b	#"^",1(a1)
		bra.s	.sc_ChooseText

.sc_MakeSpace:	moveq	#32,d0
		bra.w	.sc_DoPrint

.sc_EndTextP:	move.l	sc_TextAddr+4,d0
		beq.s	.sc_ETP1
		move	#2,sc_TextAddr+8
.sc_ETP1:	move.l	d0,sc_TextAddr
		move.l	#0,sc_TextAddr+4

.sc_EndScroll:	
;do_protect:	equ	0
;	IFNE	do_protect
;		PRINTT	," INTERRUPT PROTECTION ON.",
;		move.l	VBR_base,a1
;		lea	-20(a1),a1
;		addi	#3,$24+20+2(a1)
;		moveq	#7,d7
;.sc_act1:	addi	#1,$a0+20+2(a1)
;		lea	4(a1),a1
;		dbf	d7,.sc_act1
;	ENDC
		movem.l	(sp)+,a1/a2/d0/d7
		rts


.sc_MoveText:	lea	sv_ScrollArea-34,a2
		moveq	#6,d7
.sc_DS2:	REPT	33
		move.b	1(a2),(a2)+
		ENDR
		lea	[5*40]-33(a2),a2
		dbf	d7,.sc_DS2
		rts

;-------------------------------------------------------------------
COMPASS:	movem.l	d0-d7/a1-a3,-(sp)
		lea	sv_sinus,a1
		lea	$80(a1),a2		;cosinus
		move	sv_angle,d6
		andi	#$1fe,d6
		moveq	#0,d0			;wskazowka
		moveq	#-11,d1
		bsr	sv_rotate
		addi	#16,d0
		addi	#13,d1

		lea	sv_CompasClr,a1
		lea	(a1),a2
		moveq	#0,d2
;		REPT	27
		moveq	#26,d3
.cclr:	move.l	d2,(a2)+
		dbf		d3,.cclr
;		ENDR

		moveq	#16,d2			;center of compas
		moveq	#13,d3

		lea	.l_Octant(pc),a3
		cmpi	d1,d3
		bpl.s	.l_lineok
		exg	d0,d2
		exg	d1,d3
.l_lineok:	moveq	#3,d4
		move	d0,d5
		move	d1,d6
		sub	d3,d1
		bpl.s	.l_dr1
		neg	d1
.l_dr1:		sub	d2,d0
		bpl.s	.l_dr2
		eori	#%01,d4
		neg	d0
.l_dr2:		cmp	d0,d1
		bmi.s	.l_dr3
		exg	d0,d1
		eori	#%10,d4
.l_dr3:		move	d5,d7
		and.l	#$f,d7
		ror	#4,d7
		ori	#$0b4a,d7
		swap	d7
		move.b	(a3,d4.w),d7		;octant
		add	d1,d1
		add	d6,d6
		add	d6,d6			;*4
		and.l	#$fff0,d5
		lsr	#3,d5
		add	d6,d5
		add.l	a1,d5				; address to use: draw the line in the clear area (sv_CompasClr)
		lea		$dff000,a0
		waitblt
		move	#$8440,$96(a0)		;blitter NASTY & DMA on..
		move.l	#$ffff8000,$72(a0)
		move	#4,$60(a0)		;width
		move	d1,$62(a0)
		move.l	d5,$48(a0)
		move.l	d5,$54(a0)
		sub		d0,d1
		bpl.s	.l_dr4
		ori	#$40,d7
.l_dr4:		move	d1,$52(a0)
		move.l	d7,$40(a0)
		sub	d0,d1
		move	d1,$64(a0)
		addq	#1,d0
		lsl	#6,d0
		addq	#2,d0
		move	d0,$58(a0)

		lea	sv_CompasSav,a3
		lea	sv_Compas,a2
		moveq	#26,d6
		waitblt
		move	#$0440,$96(a0)		;blitter off...
		; copy compass back from the saved copy
.l_RetComp:	
		move.l	(a1)+,d0
		move.l	d0,d1
		not.l	d1
		move.l	(a3)+,d2
		or.l	d0,d2
		move.l	d2,(a2)
		move.l	(a3)+,d2
		and.l	d1,d2
		move.l	d2,1*row(a2)
		move.l	(a3)+,d2
		and.l	d1,d2
		move.l	d2,2*row(a2)
		move.l	(a3)+,d2
		and.l	d1,d2
		move.l	d2,3*row(a2)
		move.l	(a3)+,d2
		and.l	d1,d2
		move.l	d2,4*row(a2)
		lea		5*row(a2),a2
		dbf		d6,.l_RetComp

		; moveq	#1,d7			;color
; .l_RetComp:	
		; move.l	(a1)+,d0
		; move.l	d0,d1
		; not.l	d1

		; move.l	(a3)+,d2

		; btst	#0,d7
		; bne.s	.l_1
		; and.l	d1,d2
		; bra.s	.l_11
; .l_1:		or.l	d0,d2
; .l_11:		move.l	d2,(a2)
		; move.l	(a3)+,d2
		; btst	#1,d7
		; bne.s	.l_2
		; and.l	d1,d2
		; bra.s	.l_22
; .l_2:		or.l	d0,d2
; .l_22:		move.l	d2,row(a2)
		; move.l	(a3)+,d2
		; btst	#2,d7
		; bne.s	.l_3
		; and.l	d1,d2
		; bra.s	.l_33
; .l_3:		or.l	d0,d2
; .l_33:		move.l	d2,2*row(a2)
		; move.l	(a3)+,d2
		; btst	#3,d7
		; bne.s	.l_4
		; and.l	d1,d2
		; bra.s	.l_44
; .l_4:		or.l	d0,d2
; .l_44:		move.l	d2,3*row(a2)
		; move.l	(a3)+,d2
		; btst	#4,d7
		; bne.s	.l_5
		; and.l	d1,d2
		; bra.s	.l_55
; .l_5:		or.l	d0,d2
; .l_55:		move.l	d2,4*row(a2)

		; lea	5*row(a2),a2
		; dbf	d6,.l_RetComp

		movem.l	(sp)+,d0-d7/a1-a3
		rts

.l_octant:	dc.b	1,8+1,16+1,20+1
;-------------------------------------------------------------------
TEST_COUNTERS:
		movem.l	ALL,-(sp)
		lea		sv_Energy,a1
		move	(a1),d0
		cmp		2(a1),d0
		beq.s	.tc_EnOK
		move.l	lc_Structure(pc),a2		; god mode
		tst.b	STR_GODMODE(a2)
		beq.s	.tc_E0
		move	2(a1),d0
		move	d0,(a1)
		bra.s	.tc_EnOK
.tc_E0:		
		move	d0,2(a1)
		neg		d0
		bgt.s	.tc_E1
		moveq	#0,d0
		move.l	d0,(a1)			;zero if minus or zero
		SCROLL	39
		move	#0,do_pikaj
		move	#-1,sv_EndLevel		;death
		moveq	#9,d1			;red
		bra.s	.tc_E2
.tc_E1:		moveq	#16,d1			;color - white
		move	#0,do_pikaj
		cmpi	#20,d0
		bpl.s	.tc_E2
		move	#1,do_pikaj		;pikaj if energy low!!!
		moveq	#9,d1			;red
.tc_E2:		lea	sv_C1Save,a2
		lea	sv_Counter1,a3
		bsr.s	tc_DrawCounter


.tc_EnOK:	tst	sv_ChaosAddr
		bne.s	.tc_EndCou		;not draw if changing

		lea	sv_ITEMS,a1
		move	(a1),d0
		move	2(a1),d1
		lea	8(a1,d0.w),a1
		tst	sv_ChaosAddr+2		;if must print
		bne.s	.tc_I1
		cmp	(a1),d1
		beq.s	.tc_EndCou		;if counter not changed
.tc_I1:		move	#0,sv_ChaosAddr+2

		cmpi	#7*6,d0			;not use ch.cnt if cards
		beq.s	.tc_NoSet
		cmpi	#8*6,d0
		beq.s	.tc_NoSet
		cmpi	#9*6,d0
		beq.s	.tc_NoSet
		move	d1,(a1)
.tc_NoSet:	move	(a1),d0
		neg	d0
		moveq	#16,d1
		lea	sv_C2Save,a2
		lea	sv_Counter2,a3
		bsr.s	tc_DrawCounter

.tc_EndCou:	movem.l	(sp)+,ALL
		rts


; d0 - value
; d1 - colour (16 = white)
; a2 - counter save area
; a3 - counter on-screen ares
tc_DrawCounter:	lea	sv_Numbers,a4
		andi.l	#$ffff,d0
		divu	#100,d0
		bsr.s	.tc_DoDraw
		move.w	#0,d0
		swap	d0
		divu	#10,d0
		lea	2(a2),a2
		lea	2(a3),a3
		bsr.s	.tc_DoDraw
		swap	d0
		lea	2(a2),a2
		lea	2(a3),a3

.tc_DoDraw:	movem.l	a2/a3,-(sp)
		add	d0,d0
		lea	(a4,d0.w),a5		;font addr

		moveq	#17,d4
.tc_DLoop1:	move	(a5),d0			;font
		move	d0,d3
		not	d3
		move	(a2),d2			;bufor
		btst	#0,d1
		bne.s	.tc_1
		and	d3,d2
		bra.s	.tc_11
.tc_1:		or	d0,d2
.tc_11:		move.b	d2,1(a3)
		lsr	#8,d2
		move.b	d2,(a3)
		move	6(a2),d2
		btst	#1,d1
		bne.s	.tc_2
		and	d3,d2
		bra.s	.tc_22
.tc_2:		or	d0,d2
.tc_22:		move.b	d2,40+1(a3)
		lsr	#8,d2
		move.b	d2,40(a3)
		move	2*6(a2),d2
		btst	#2,d1
		bne.s	.tc_3
		and	d3,d2
		bra.s	.tc_33
.tc_3:		or	d0,d2
.tc_33:		move.b	d2,80+1(a3)
		lsr	#8,d2
		move.b	d2,80(a3)
		move	3*6(a2),d2
		btst	#3,d1
		bne.s	.tc_4
		and	d3,d2
		bra.s	.tc_44
.tc_4:		or	d0,d2
.tc_44:		move.b	d2,120+1(a3)
		lsr	#8,d2
		move.b	d2,120(a3)
		move	4*6(a2),d2
		btst	#4,d1
		bne.s	.tc_5
		and	d3,d2
		bra.s	.tc_55
.tc_5:		or	d0,d2
.tc_55:		move.b	d2,160+1(a3)
		lsr	#8,d2
		move.b	d2,160(a3)
		lea	20(a5),a5
		lea	5*6(a2),a2
		lea	5*40(a3),a3
		dbf	d4,.tc_Dloop1
		movem.l	(sp)+,a2/a3
		rts


;-------------------------------------------------------------------
;DRAW CARD COUNTERS...
tc_DrawCardCnt:	
		movem.l	d0-d5/a0-a4,-(sp)
		lea	sv_Cards,a1
		lea	sv_Numbers+[20*18],a2		; small numbers
		lea	sv_CardSav,a3
		lea	sv_CardCnt,a4
		
		move	4(a1),d0		;minus value
		moveq	#16,d1			;color to draw
		bsr.s	.tc_DrawCnt
		move	4+6(a1),d0
		bsr.s	.tc_DrawCnt
		move	4+12(a1),d0
		bsr.s	.tc_DrawCnt
		
.tc_enddraw:
		movem.l	(sp)+,d0-d5/a0-a4
		rts

.tc_DrawCnt:	
		neg	d0
		moveq	#6,d5
.tc_DCLoop:	move.b	(a2,d0.w),d2		;or
		move.b	d2,d3
		not.b	d3			;and
		move.b	(a3)+,d4
		btst	#0,d1
		bne.s	.tc_D00
		and.b	d3,d4
		bra.s	.tc_D01
.tc_D00:	or.b	d2,d4
.tc_D01:	move.b	d4,(a4)
		move.b	(a3)+,d4
		btst	#1,d1
		bne.s	.tc_D10
		and.b	d3,d4
		bra.s	.tc_D11
.tc_D10:	or.b	d2,d4
.tc_D11:	move.b	d4,40(a4)
		move.b	(a3)+,d4
		btst	#2,d1
		bne.s	.tc_D20
		and.b	d3,d4
		bra.s	.tc_D21
.tc_D20:	or.b	d2,d4
.tc_D21:	move.b	d4,80(a4)
		move.b	(a3)+,d4
		btst	#3,d1
		bne.s	.tc_D30
		and.b	d3,d4
		bra.s	.tc_D31
.tc_D30:	or.b	d2,d4
.tc_D31:	move.b	d4,120(a4)
		move.b	(a3)+,d4
		btst	#4,d1
		bne.s	.tc_D40
		and.b	d3,d4
		bra.s	.tc_D41
.tc_D40:	or.b	d2,d4
.tc_D41:	move.b	d4,160(a4)
		lea	200(a4),a4
		addi	#10,d0
		dbf	d5,.tc_DCLoop
		move.b	(a3)+,(a4)		;add blank row
		move.b	(a3)+,40(a4)
		move.b	(a3)+,80(a4)
		move.b	(a3)+,120(a4)
		move.b	(a3)+,160(a4)
		lea	200(a4),a4
		rts

;-------------------------------------------------------------------
; Get start timestamp
; in: d0: counter index
DebugCntStart:
		movem.l	d1/a1,-(sp)
		TIMESTAMP	d1
		move.l	lc_FastMem2(pc),a1
		addi.l	#F2_AvLastVal,a1
		lsl		#2,d0
		move.l	d1,(a1,d0.w)					; store first value
		movem.l	(sp)+,d1/a1
		rts

; Get stop timestamp
; in: d0: counter index
DebugCntStop:
		movem.l	d1/d2/a1,-(sp)
		TIMESTAMP	d2
		move.l	lc_FastMem2(pc),a1
		addi.l	#F2_AvLastVal,a1
		move	d0,d1
		lsl		#2,d0
		move.l	(a1,d0.w),d0					; get first value
		TSDELTA	d0,d2							; difference in frames (H word) and scanlines (L word)
		move.l	d2,d0
		bsr		TsAverageAdd					; add TS to average table (d0: val, d1: cnt index)
		movem.l	(sp)+,d1/d2/a1
		rts

DebugCntClearArea:
		movem.l	ALL,-(sp)
		lea		sv_DebugPos1,a1
		lea		sv_DebugPos2,a2
		clr.l	d0
		move	#8+5*10*10-1,d1
.c1:	move.l	d0,(a1)+
		move	d0,(a1)
		move.l	d0,(a2)+
		move	d0,(a2)
		lea		36(a1),a1
		lea		36(a2),a2
		dbf		d1,.c1


		lea		sv_Numbers+[20*18],a1	; small numbers
		lea		sv_DebugPos1+[2*40*5]+40*2,a3
		lea		sv_DebugPos2+[2*40*5]+40*2,a4
		clr		d2
.d1:	move	d2,d0
		lea		(a3),a2
		bsr		PrintDigit
		ori.b	#1,40*5*6-1(a2)			; dot
		lea		10*40*5(a3),a3			; next row
		lea		(a4),a2
		bsr		PrintDigit
		ori.b	#1,40*5*6-1(a2)
		lea		10*40*5(a4),a4
		addi	#1,d2
		cmpi	#10,d2
		bne.s	.d1
		movem.l	(sp)+,ALL
		rts

DebugCntRedrawArea:
		movem.l	d0/a1-a3,-(sp)
		lea		sv_WindowSav,a1
		move.l	sv_Screen,a2
		move.l	sv_Screen+4,a3
		addi.l	#[sv_Upoffset*5*row],a2
		addi.l	#[sv_Upoffset*5*row],a3
		move	#[130*5]-1,d0
.c1:	move.l	(a1),(a2)+		;get background
		move.l	(a1)+,(a3)+
		move	(a1),(a2)+
		move	(a1)+,(a3)+
		lea		10(a1),a1
		lea		34(a2),a2
		lea		34(a3),a3
		dbf		d0,.c1
		movem.l	(sp)+,d0/a1-a3
		rts

DebugCntShow:
		lea		sv_Numbers+[20*18],a1	; small numbers
		lea		sv_DebugPos1+[2*40*5]+1,a3
		lea		sv_DebugPos2+[2*40*5]+1,a4
		move.l	lc_FastMem2(pc),a5
		addi.l	#F2_AvPtrs,a5
		clr		d7
.d1:	move	(a5)+,d0					; check if counter used at all
		beq.s	.d2
		
		move	d7,d1
		bsr		CounerAverageGet			; in: d1 average counter index, out: d0
		lea		(a4),a2
		bsr		CounerAveragePrint			; in: d0
		lea		(a4),a2
		bsr		copyCounter					; a2 source, a3 target

.d2:	lea		10*40*5(a3),a3			; next counter row
		lea		10*40*5(a4),a4
		addq	#1,d7
		cmpi	#10,d7
		bne.s	.d1
		rts
		
		
;-------------------------------------------------------------------
;-------------------------------------------------------------------
; Add Timestamp to average counters. Includes converting the TS to counter (re-basing to 256)
; in: d0.l - TS (frames:scanlines), d1.w - AV index (0..15)
TsAverageAdd:
		movem.l	d0-d3/a1,-(sp)
		move.l	lc_FastMem2(pc),a1
		addi.l	#F2_AvPtrs,a1
		lsl		#1,d1
		move	(a1,d1.w),d3				; free slot index in the average data table
		move	d3,d2
		addi	#1,d2
		andi	#7,d2						; wrap around - 8 data samples possible
		ori		#$8000,d2					; "used" indicator
		move	d2,(a1,d1.w)				; store next data index
		lsl		#1,d3						; index in words - also removes the "used" indicator
		lsl		#3,d1						; d1 is *16 at this point
		lea		F2_AvData-F2_AvPtrs(a1,d1.w),a1		; correct data buffer
		clr.l	d2
		move	d0,d2
		lsl.l	#8,d2					; = *256/313 -> rebasing to 256
		divu	#313,d2					; scale scanlines from 0..312 to 0..255
		swap	d0
		lsl		#8,d0					; frame, move it to be on the MSB
		or		d2,d0					; add fractions to the LSB
		move	d0,(a1,d3.w)
		movem.l	(sp)+,d0-d3/a1
		rts

; Convert frame:scanline TS to 256-based value with frames on MSB and fractions on LSB
; in: d0.l TS, out: d0.w same but re-based on 256
TsRebaseTo256:
		move.l	d1,-(sp)
		clr.l	d1
		move	d0,d1
		lsl.l	#8,d1					; = *256/313 -> rebasing to 256
		divu	#313,d1					; scale scanlines from 0..312 to 0..255
		swap	d0
		lsl		#8,d0					; frame, move it to be on the MSB
		or		d1,d0					; add fractions to the LSB
		move.l	(sp)+,d1
		rts


; Get average counter
; in: d1 - AV index (0..15), out: d0 average.w (msb = frames, lsb = fraction 256-based)
CounerAverageGet:
		movem.l	d1/a1,-(sp)
		move.l	lc_FastMem2(pc),a1
		lsl		#4,d1
		lea		F2_AvData(a1,d1.w),a1		; correct data buffer
		clr		d0
		move	#7,d1
.t1:	add		(a1)+,d0
		dbf		d1,.t1
		lsr		#3,d0						; average of 8 values
		movem.l	(sp)+,d1/a1
		rts
		
; Show FPS counter
FPSShow:
		lea		sv_Numbers+[20*18],a1	; small numbers
		clr		d1				; fps is couter 0
		bsr.s	CounerAverageGet			; in: d1 average counter index
		move	d0,d6
		move.l	#$320000,d1				; 50 fps <<16
		divu	d0,d1
		lea		sv_BombPos+[4*40*5]+[4*40]+1,a2
		move	d1,d0
		bsr		CounerAveragePrint

		lea		sv_BombPos+[4*40*5]+[10*40*5]+[2*40]+1,a2		; next row
		move	d6,d0
		bsr.s	CounerAveragePrint

		move	lc_ledChange(pc),d0		; 1 - update LEDs
		beq.s	.noLC
		bsr.s	StatusLedsShow
		lea		lc_ledChange(pc),a1
		clr		(a1)
.noLC:
		rts


; show status "LEDs"
StatusLedsShow:
		lea		sv_BombPos+[4*40*5]+[25*40*5]+1,a2
		move	lc_CpuType(pc),d0		; 0 - 68k, 1 - 020+
		bsr.s	StatusLedsShowCol
		move	lc_isCache(pc),d0		; 0 - no cache, 1 - cache
		bsr.s	StatusLedsShowCol
		move	lc_c2pType(pc),d0		; 0 - blitter, 1 - cpu
		eori	#1,d0
		bsr.s	StatusLedsShowCol
		move.l	lc_FastMem1(pc),d0		; memory type
		cmpi.l	#$1ffffff,d0
		bpl.s	.fast
		moveq	#0,d0
.fast:	bsr.s	StatusLedsShowCol
		rts

; in: Z=1 red, Z=0 green
StatusLedsShowCol:
		bne.s	.green
		move.b	#$7e,d0
		move.b	d0,(a2)
		clr.b	1*40(a2)
		clr.b	2*40(a2)
		move.b	d0,3*40(a2)
		clr.b	4*40(a2)
		lea		1(a2),a2
		rts
.green:
		move.b	#$7e,d0
		move.b	d0,(a2)
		move.b	d0,1*40(a2)
		clr.b	2*40(a2)
		clr.b	3*40(a2)
		move.b	d0,4*40(a2)
		lea		1(a2),a2
		rts

; Print timestamp in H byte:frames,  L byte: fractions 256-based
; in: d0.w: TS, a1: numbers bitmap, a2: start pos on screen
CounerAveragePrint:
		move	d0,d3
		andi.l	#$ffff,d0
		lsr		#8,d0					; full frames
		divu	#10,d0
		cmpi	#9,d0
		bmi.s	.s1
		move	#9,d0
.s1:	tst		d0
		bne.s	.s2
		bsr		PrintSpace
		bra.s	.s3
.s2:	bsr.s	PrintDigit
.s3:	swap	d0
		bsr.s	PrintDigit
		ori.b	#1,40*5*6-1(a2)			; dot

		move	d3,d0
		andi	#$ff,d0					; fractions only
		mulu	#100,d0
		lsr		#8,d0					; scale from 0..255 to 0..99
		divu	#10,d0
		bsr.s	PrintDigit
		swap	d0
		bsr.s	PrintDigit
		rts


; in: d0: number, a1: numbers bitmap, a2: start pos on screen
PrintDigit:
		move.b	(a1,d0.w),d1
		move.b	d1,(a2)
		move.b	10(a1,d0.w),d1
		move.b	d1,40*5*1(a2)
		move.b	20(a1,d0.w),d1
		move.b	d1,40*5*2(a2)
		move.b	30(a1,d0.w),d1
		move.b	d1,40*5*3(a2)
		move.b	40(a1,d0.w),d1
		move.b	d1,40*5*4(a2)
		move.b	50(a1,d0.w),d1
		move.b	d1,40*5*5(a2)
		move.b	60(a1,d0.w),d1
		move.b	d1,40*5*6(a2)
		lea		1(a2),a2
		rts

PrintSpace:
		clr		d1
		clr		d2
		move	#6*[40*5],d2	; do 7 lines
.p1:	move.b	d1,(a2,d2.w)
		subi	#40*5,d2
		bpl.s	.p1
		lea		1(a2),a2
		rts

; a2: source start pos on screen, a3 - target
copyCounter:
		move	#6*[40*5],d2	; do 7 lines
.p1:	move.b	(a2,d2.w),(a3,d2.w)
		move.b	1(a2,d2.w),1(a3,d2.w)
		move.b	2(a2,d2.w),2(a3,d2.w)
		move.b	3(a2,d2.w),3(a3,d2.w)
		subi	#40*5,d2
		bpl.s	.p1
		rts

;-------------------------------------------------------------------
; CHANGE WEAPON
; in: d0 new weapon
ci_NewWeapon:	
		lea		sv_Items,a1
		ext		d0
		andi	#$fffe,d0
		neg		d0
		subq	#4,d0
		move	d0,d1
		add		d0,d0
		add		d1,d0			;*6
		tst		4(a1,d0.w)
		beq.w	ci_NotPresent
		cmp		(a1),d0
		beq.w	ci_NotPresent		;if actually chosen
		move	d0,(a1)			;change to new weapon

		move	#[32*27*2]-2,sv_ChaosAddr
		move	2(a1),sv_AmmoChg	;fade & show CNT's
		move	#0,sv_AmmoChg+2
		move	4+4(a1,d0.w),sv_AmmoChg+4
		move	#9+10,sv_AmmoChg+6

		move	4+4(a1,d0.w),2(a1)	;fix counter
		tst	d0
		beq.s	.ci_Hand		;no counter if hand
		move	#1,sv_ChaosAddr+2	;must print new counter
.ci_Hand:

		lea	sv_ItemSav,a1
		lea	sv_ItemBuf,a2
		moveq	#26,d0
.ci_BufItem:	move.l	(a1)+,(a2)+		;background to buffer
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		dbf	d0,.ci_BufItem


		lea	sv_ItemOffsets,a1
		cmpi	#14,d1			;Fix card icon
		bmi.s	ci_NotCards
		addi	#12,d1
ci_NotCards:	add	d1,d1			;*4
		move.l	(a1,d1.w),d0
		move.l	lc_FastMem1(pc),a1
		addi.l	#co_Walls,a1
		lea	(a1,d0.l),a1		;item start
		lea	sv_ItemBuf,a2

		move.l	#$80000000,d0		;for or
		moveq	#31,d1
ci_DecodeI1:	lea	(a2),a3			;add item to background
		lea	10(a1),a1
		move.l	d0,d4
		not.l	d4			;for and
		move	#21,d2
ci_DecodeI2:	move.b	(a1)+,d3
		beq.s	ci_DecodeI3

		add.b	d3,d3
		bcs.s	.ci_di00
		and.l	d4,(a3)
		bra.s	.ci_di01
.ci_di00:	or.l	d0,(a3)
.ci_di01:	add.b	d3,d3
		bcs.s	.ci_di10
		and.l	d4,4(a3)
		bra.s	.ci_di11
.ci_di10:	or.l	d0,4(a3)
.ci_di11:	add.b	d3,d3
		bcs.s	.ci_di20
		and.l	d4,8(a3)
		bra.s	.ci_di21
.ci_di20:	or.l	d0,8(a3)
.ci_di21:	add.b	d3,d3
		bcs.s	.ci_di30
		and.l	d4,12(a3)
		bra.s	.ci_di31
.ci_di30:	or.l	d0,12(a3)
.ci_di31:	add.b	d3,d3
		bcs.s	.ci_di40
		and.l	d4,16(a3)
		bra.s	ci_DecodeI3
.ci_di40:	or.l	d0,16(a3)

ci_DecodeI3:	lea	4*5(a3),a3
		dbf	d2,ci_DecodeI2
		lea	33(a1),a1		;vir to 65 - next row
		lsr.l	d0
		dbf	d1,ci_DecodeI1

ci_NotPresent:	rts

;---------------
; Draw new selected weapon
; in: d0: sv_ChaosAddr
ci_DrawWeapon:
		addi	#1,sv_AmmoChg+2
		move	sv_AmmoChg+2,d1
		cmpi	#9,d1
		bpl.s	ci_Wait
		move	sv_AmmoChg,d0
		neg	d0
		bmi.s	ci_DWMain		;no fade if f.e. hand
		lea		sv_C2Save,a2
		lea		sv_Counter2,a3
		bsr		tc_DrawCounter
		bra.s	ci_DWMain
ci_Wait:
		move	#8,sv_AmmoChg+2

		subi	#1,sv_AmmoChg+6
		move	sv_AmmoChg+6,d1
		cmpi	#9,d1
		bpl.s	ci_DWmain
		move	sv_AmmoChg+4,d0
		neg	d0
		bmi.s	ci_DWMain		;no show if f.e. hand
		tst	d1
		bne.s	ci_Wt2
		moveq	#16,d1
ci_Wt2:		lea	sv_C2Save,a2
		lea	sv_Counter2,a3
		bsr	tc_DrawCounter

ci_DWMain:
		move	sv_ChaosAddr,d0
		lea	sv_ItemBuf,a1
		lea	sv_Weapon,a2
		lea	sv_ChaosTab,a3
		lea	ci_200MulTab(pc),a4
		move	#31,d1
ci_DWLoop:	move	(a3,d0.w),d2
		move	d2,d3
		lsr	#5,d3
		add	d3,d3
		move	ci_20MulTab-ci_200MulTab(a4,d3.w),d5
		move	(a4,d3.w),d3
		andi	#31,d2
		move	d2,d4
		lsr	#3,d4
		add	d4,d5			;bytes in buf
		add	d4,d3			;bytes
		not	d2			;bits
		andi	#7,d2

		move.b	(a1,d5.w),d4
		btst	d2,d4
		bne.s	.ci_00
		bclr.b	d2,(a2,d3.w)
		bra.s	.ci_01
.ci_00:		bset.b	d2,(a2,d3.w)
.ci_01:		move.b	4(a1,d5.w),d4
		btst	d2,d4
		bne.s	.ci_10
		bclr.b	d2,40(a2,d3.w)
		bra.s	.ci_11
.ci_10:		bset.b	d2,40(a2,d3.w)
.ci_11:		move.b	8(a1,d5.w),d4
		btst	d2,d4
		bne.s	.ci_20
		bclr.b	d2,80(a2,d3.w)
		bra.s	.ci_21
.ci_20:		bset.b	d2,80(a2,d3.w)
.ci_21:		move.b	12(a1,d5.w),d4
		btst	d2,d4
		bne.s	.ci_30
		bclr.b	d2,120(a2,d3.w)
		bra.s	.ci_31
.ci_30:		bset.b	d2,120(a2,d3.w)
.ci_31:		move.b	16(a1,d5.w),d4
		addi	#160,d3
		btst	d2,d4
		bne.s	.ci_40
		bclr.b	d2,(a2,d3.w)
		bra.s	.ci_41
.ci_40:		bset.b	d2,(a2,d3.w)
.ci_41:
		subq	#2,d0
		dbf	d1,ci_DWLoop

		move	d0,sv_ChaosAddr
		bpl.s	ci_DWEnd
		move	#0,sv_ChaosAddr
ci_DWEnd:	rts


ci_200MulTab:
	dc.w	0,200,400,600,800,1000,1200,1400,1600,1800
	dc.w	2000,2200,2400,2600,2800,3000,3200,3400,3600,3800
	dc.w	4000,4200,4400,4600,4800,5000,5200
ci_20MulTab:
	dc.w	0,20,40,60,80,100,120,140,160,180
	dc.w	200,220,240,260,280,300,320,340,360,380
	dc.w	400,420,440,460,480,500,520

;-------------------------------------------------------------------
; Draw heart plotter

DRAW_HEART:	lea	sv_HeartSav,a1
		lea	sv_Heart,a2
		moveq	#11,d0
sc_RetHt:	REPT	5
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		move.b	(a1)+,(a2)+
		lea	row-5(a2),a2
		ENDR
		dbf	d0,sc_RetHt

		lea	sc_PosTab(pc),a1
		lea	sc_PosTab2(pc),a2
		subi	#1,sv_Flag
		bne.s	sc_HD5
		move	#25,sv_Flag		;*11 = regeneration time
		tst		sv_Flag+2		; 0 - normal, 1 - slow regeneration
		beq.s	sc_HD6
		move	#50,sv_Flag		;regen. slower if moving
sc_HD6:		addi	#4,2(a1)
		cmpi	#57*4,2(a1)
		bmi.s	sc_HD5
		move	#56*4,2(a1)		;max beat
sc_HD5:
		move	(a1)+,d0
		move	(a1)+,d1
		addq	#4,d0
		cmpi	d1,d0			;is actual cnt > max?
		bmi.s	sc_HD3
		moveq	#0,d0			;cnt restart
sc_HD3:		move	d0,-4(a1)
		move	(a2),d1			;cnt2
		addi	#29*4,d1
		andi	#127,d1

		move.l	(a1,d0.w),2(a2,d1.w)	;next part of plot
		tst	sv_EndLevel
		bpl.s	.sc00
		move.l	#$04b00000,2(a2,d1.w)	;next part of plot

.sc00:		lea	sv_Heart,a1
		addi	#4,(a2)
		andi	#127,(a2)
		move	(a2)+,d1		;start offset in table
		moveq	#2,d0			;x pos
		moveq	#28,d2			;X max
sc_HDrawLoop:	move	(a2,d1.w),d3
		subq	#1,d0
		bpl.s	sc_HD1
		moveq	#7,d0
		lea	1(a1),a1
sc_HD1:
		move	2(a2,d1.w),d4
		lea	(a1,d3.w),a3
sc_HD2:		bset	d0,(a3)			;color 19 - green
		bset	d0,40(a3)
		bclr	d0,80(a3)
		bclr	d0,120(a3)
		bset	d0,160(a3)
		lea	200(a3),a3
		dbf	d4,sc_HD2

		addq	#4,d1			;next pos
		andi	#127,d1
		dbf	d2,sc_HDrawLoop
		rts

;---------------
EXCITE:		move.l	a1,-(sp)
		lea	sc_PosTab+2(pc),a1
		subi	#11*4,(a1)
		cmpi	#11*4,(a1)		;min Heart beat
		bpl.s	sc_HD4
		move	#11*4,(a1)
sc_HD4:		move.l	(sp)+,a1
		rts

;---------------
sc_PosTab2:
	dc.w	0
	blk.l	32,$04b00000				;dc.w	6*200,0

sc_PosTab:
	dc.w	30*4,56*4			;act.cnt, max cnt (exhaust)
	dc.w	6*200,0,5*200,0,4*200,0,2*200,1,0*200,1	;start row, rept-1
	dc.w	2*200,3,6*200,3,10*200,1,8*200,1,7*200,0
	blk.l	46,$04b00000				;dc.w	6*200,0

;-------------------------------------------------------------------
;open/close priority doors...
ServePriorDoor:	lea	sv_DoorFlag1,a1
		bsr.s	oc_DoServe
		lea	sv_DoorFlag2,a1
oc_DoServe:	tst	20(a1)			;not if door in_use
		bne.s	oc_sdpEND
		cmpi	#-1,24+2(a1)		;not if no prior doors!
		beq.s	oc_sdpEND
		move.l	24(a1),(a1)		;copy door structure
		move.l	28(a1),4(a1)
		move.l	32(a1),8(a1)
		move.l	36(a1),12(a1)
		move.l	40(a1),16(a1)
		move	#-1,24+2(a1)		;zero prior tab
		move.l	#-1,20(a1)		;set in_use & prior
oc_sdpEND:	rts

;-------------------------------------------------------------------
;serve all door anims...
OpenCloseDoors:	lea	sv_MAP,a2
		tst	sv_DoorFlag1+20		;door in use?
		beq.w	oc_DOOR2
		lea	sv_DoorFlag1+2,a1
		move.b	-2(a1),d0
		cmp.b	42(a1),d0
		beq.s	oc_7
		move	(a1),d0
		move	d0,d1
		lsl	#10-4,d0
		andi	#63*1024,d0		;x
		addi	#512,d0
		andi	#63*1024,d1		;y
		addi	#512,d1
		tst	sv_levelDATA+26
		beq.s	oc_k2
		SOUND2	16,2
		bra.s	oc_k1
oc_k2:		SOUND2	9,2
oc_k1:		move.b	-2(a1),42(a1)
oc_7:		lea	(a1),a3
		tst.b	-2(a1)			;1-open, 0-close
		beq.s	oc_CLOSE
		cmpi.b	#6,-1(a1)		;if CNT > 4, fix
		bmi.s	oc_5
		move.b	#5,-1(a1)
oc_5:		subi.b	#1,-1(a1)
		bpl.s	oc_1
		st	-1(a1)
		moveq	#32,d0			;opened
		bsr	oc_FixMap
		tst	22-2(a1)		;is prior_use on?
		beq.s	oc_DOOR2
		move	#-1,(a1)		;zero door tab
		move.l	#0,20-2(a1)		;not use & not prior use
		bra.s	oc_DOOR2
oc_CLOSE:	addi.b	#1,-1(a1)
		cmpi.b	#5,-1(a1)
		bpl.s	oc_2
oc_1:		moveq	#34,d0			;opening door
		bsr.w	oc_FixMap
		bsr.w	oc_DrawOpen
		bra.s	oc_DOOR2
oc_2:		move.b	#5,-1(a1)
		moveq	#30,d0			;closed
		bsr	oc_FixMap
		move	#-1,(a1)
		move.l	#0,20-2(a1)

oc_DOOR2:	tst	sv_DoorFlag2+20		;door 2 in use?
		beq.w	oc_END
		lea	sv_DoorFlag2+2,a1	;serve door 2
		move.b	-2(a1),d0
		cmp.b	42(a1),d0
		beq.s	oc_6
		move	(a1),d0
		move	d0,d1
		lsl	#10-4,d0
		andi	#63*1024,d0		;x
		addi	#512,d0
		andi	#63*1024,d1		;y
		addi	#512,d0
		SOUND2	10,2
		move.b	-2(a1),42(a1)
oc_6:		lea	(a1),a3
		tst.b	-2(a1)
		beq.s	oc_CLOSE2
		subi.b	#1,-1(a1)
		bpl.s	oc_3
		st	-1(a1)
		moveq	#38,d0			;opened
		bsr	oc_FixMap
		tst	22-2(a1)		;is prior_use on?
		beq.s	oc_End
		move	#-1,(a1)
		move.l	#0,20-2(a1)
		bra.s	oc_End
oc_CLOSE2:	addi.b	#1,-1(a1)
		cmpi.b	#6,-1(a1)
		bpl.s	oc_4
oc_3:		moveq	#40,d0			;opening
		bsr.s	oc_FixMap
		bsr	oc_DrawOpen2
		bra.s	oc_End
oc_4:		move.b	#6,-1(a1)
		moveq	#36,d0			;closed
		bsr	oc_FixMap
		move	#-1,(a1)
		move.l	#0,20-2(a1)
oc_End:		rts


oc_FixMap:	move	(a3)+,d1
		cmpi	#-1,d1
		bne.s	oc_FMCont
		rts
oc_FMCont:	move	d1,d2
		lsr	d1
		andi	#1,d2			;wall dir bit
		or	d0,d2			;wall nr.
		move.b	d2,(a2,d1.w)
		bra.s	oc_FixMap

oc_Mul10Tab:	dc.w	10,20,30,40,50

oc_DrawOpen:
		moveq	#0,d0
		move.b	-1(a1),d0		;open value for DBF
		move.l	sv_WallOffsets+[4*14],d1
		move.l	sv_Consttab+12,a1
		lea	(a1,d1.l),a1		;required wall start
		lea	[13*65]+3(a1),a1	;closed door
		lea	128*65(a1),a4		;anim door
		moveq	#4,d2
		sub	d0,d2			;clear for DBF
		move	d2,d3
		add	d3,d3
		move	oc_Mul10Tab(pc,d3.w),d3
		lea	(a1,d3.w),a1		;down of door
		lea	(a1),a3
		moveq	#0,d4
		moveq	#18,d3			;width of door
oc_DOLoop:	move	d0,d1
		lea	(a3),a1
oc_L1:		move.l	(a1)+,(a4)+		;copy
		move.l	(a1)+,(a4)+
		move	(a1)+,(a4)+
		dbf	d1,oc_L1
		move	d2,d1
oc_L2:		move.l	d4,(a4)+		;clear
		move.l	d4,(a4)+
		move	d4,(a4)+
		dbf	d1,oc_L2
		lea	65(a3),a3
		lea	5(a4),a4		;skip bit & 3 upper rows
		move	d0,d1
		lea	(a3),a1
oc_L3:		move.b	(a1)+,(a4)+
		move.l	(a1)+,(a4)+		;copy
		move.l	(a1)+,(a4)+
		move.b	(a1)+,(a4)+
		dbf	d1,oc_L3
		move	d2,d1
oc_L4:		move.b	d4,(a4)+		;clear
		move.l	d4,(a4)+
		move.l	d4,(a4)+
		move.b	d4,(a4)+
		dbf	d1,oc_L4
		lea	65(a3),a3
		lea	5(a4),a4		;skip bit & 3 upper rows
		dbf	d3,oc_DOLoop
		rts

oc_MulD2Tab:	dc.b	0,2,4,6,8,10

oc_DrawOpen2:	moveq	#0,d0
		move.b	-1(a1),d0
		move.b	oc_MulD2Tab(pc,d0.w),d0	;real open value
		move.l	sv_WallOffsets+[4*17],d1
		move.l	sv_Consttab+12,a1
		lea	(a1,d1.l),a1
		lea	[7*65]+3(a1),a1
		lea	128*65(a1),a4
		moveq	#21,d2
		sub	d0,d2
		sub	d0,d2			;clear for DBF
		move	d2,d3
		addq	#1,d3
		move	d3,d4
		lsl	#6,d3
		add	d4,d3			;*65
		lea	(a1,d3.w),a1		;right of door

		moveq	#0,d4
		move	d0,d1
oc_L5:		REPT	15			;15*4=60 bytes
		move.l	(a1)+,(a4)+		;copy
		ENDR
		move.b	(a1)+,(a4)+		;61 bytes
		lea	4(a1),a1
		lea	4(a4),a4
		move.b	(a1)+,(a4)+
		REPT	15
		move.l	(a1)+,(a4)+
		ENDR
		lea	4(a1),a1
		lea	4(a4),a4
		dbf	d1,oc_L5
		REPT	15			;+ 1 row
		move.l	(a1)+,(a4)+
		ENDR
		move.b	(a1)+,(a4)+
		lea	4(a1),a1
		lea	4(a4),a4

		move	d2,d1
oc_L6:		move.b	d4,(a4)+
		REPT	15
		move.l	d4,(a4)+		;clear
		ENDR
		lea	4(a4),a4
		REPT	15
		move.l	d4,(a4)+
		ENDR
		move.b	d4,(a4)+
		lea	4(a4),a4
		dbf	d1,oc_L6

		move	d0,d1
oc_L7:		move.b	(a1)+,(a4)+
		REPT	15
		move.l	(a1)+,(a4)+		;copy second half
		ENDR
		lea	4(a1),a1
		lea	4(a4),a4
		REPT	15
		move.l	(a1)+,(a4)+
		ENDR
		move.b	(a1)+,(a4)+
		lea	4(a1),a1
		lea	4(a4),a4
		dbf	d1,oc_L7
		move.b	(a1)+,(a4)+
		REPT	15			;+ 1 row
		move.l	(a1)+,(a4)+
		ENDR
		rts


;-------------------------------------------------------------------
;transform picture while teleporting...

TELEPORT:	movem.l	ALL,-(sp)
		move	#0,sv_teleport
		move.l	sv_Screen,a1		;hiden - with destination picture
		move.l	sv_Screen+4,a2		;shown - to which copy will be made
		lea		sv_ChaosTac,a3
		move	sv_Size+2,d0
		cmpi	#7,d0
		bpl.s	te_stretched
		lea		[5*sv_UpOffset*row]+sv_LeftOffset(a1),a1
		lea		[5*sv_UpOffset*row]+sv_LeftOffset(a2),a2
		moveq	#24*2,d0		;width
		move	#3-1,a4			;repeat (for dbf)
		bra.s	te_doit
te_stretched:	lea	5*sv_UpOffset*row(a1),a1
		lea		5*sv_UpOffset*row(a2),a2
		moveq	#40*2,d0
		move	#5-1,a4
te_doit:
		lea		$dff000,a0
		moveq	#31,d7			;16*64=1024
te_LOOP:	VBLANK
		moveq	#31,d2			;nr. points in frame
te_loop2:	move	(a3)+,d4
		move	a4,d6
te_loop3:	moveq	#0,d5
		move	d4,d5
		divu	d0,d5			;div row
		move	d5,d1
		mulu	#5*2*row,d1		;5 planes * 2 rows
		swap	d5
		lsr	d5
		bcs.s	te_right
		add	d5,d1
		moveq	#9,d3			;serve all planes
.te_pix1:	move.b	(a1,d1.w),d5
		andi	#$f0,d5
		andi.b	#$0f,(a2,d1.w)
		or.b	d5,(a2,d1.w)
		addi	#row,d1
		dbf	d3,.te_pix1
		bra.s	te_ok1
te_right:	add	d5,d1
		moveq	#9,d3
.te_pix2:	move.b	(a1,d1.w),d5
		andi	#$0f,d5
		andi.b	#$f0,(a2,d1.w)
		or.b	d5,(a2,d1.w)
		addi	#row,d1
		dbf	d3,.te_pix2
te_ok1:
		addi	#1024,d4
		dbf	d6,te_loop3
		dbf	d2,te_loop2
		dbf	d7,te_LOOP

		movem.l	(sp)+,ALL
		rts
;-------------------------------------------------------------------
CheckCodes:	movem.l	ALL,-(sp)
		tst		sv_TextBuffer+2		;test only after key
		beq.w	cco_End
		move	#0,sv_TextBuffer+2

		lea		sv_TextBuffer+4,a1
		lea		EnergyCode,a2
		bsr		cco_Check
		bne.s	chk_Ammo
		lea		sv_ENERGY,a1
		move	#-666,(a1)
		SCROLL	66
		bra.w	cco_End
chk_Ammo:	lea	AmmoCode,a2
		bsr		cco_Check
		bne.s	chk_Card

		lea		sv_Items,a1
		moveq	#5,d7
.cco_3:		
		move	#20000,10(a1)		;weapon durability/damage
		move	#-666,14(a1)		;fix ammo
		lea		6(a1),a1
		dbf		d7,.cco_3

		lea		sv_Items,a1
		tst		(a1)			; check selected weapon
		bne.s	.cco_nohand		; if hand then select handgun by default
		move	#$fb,sv_Flag+6
		bra.s	.cco_1
.cco_nohand:
		move	#-666,d0
		move	d0,2(a1)		; displayed ammo
		neg		d0
		moveq	#16,d1
		lea		sv_C2Save,a2
		lea		sv_Counter2,a3
		bsr		tc_DrawCounter	; update displayed counter of existing weapon
.cco_1:
		SCROLL	67
		bra.w	cco_End

chk_Card:	lea	CardCode,a2
		bsr		cco_Check
		bne.s	chk_WallJump

		lea		sv_Cards,a1
		move	#-6,4(a1)		;fix cards
		move	#-6,10(a1)
		move	#-6,16(a1)
		SCROLL	67
		bsr		tc_DrawCardCnt
		bra.w	cco_End

chk_WallJump:	lea	WallCode,a2
		bsr.w	cco_Check
		bne.s	chk_Godmode

		lea		sv_sinus,a1
		lea		$80(a1),a2
		move	#256,d6
		sub		sv_angle,d6
		andi	#$1fe,d6
		moveq	#0,d0
		move	#1024,d1		;vector length
		bsr		sv_Rotate
		add		d0,sv_PosX
		add		d1,sv_PosY
		bra.w	cco_End

chk_Godmode:	lea	GodModeCode,a2
		bsr.w	cco_Check
		bne.s	chk_MapShow
		SCROLL	66
		move.l	lc_Structure(pc),a2		; god mode on/off
		eori.b	#1,STR_GODMODE(a2)
		bra.s	cco_End
chk_MapShow:
		lea	MapCode,a2
		bsr.s	cco_Check
		bne.s	cco_level

		lea	sv_UserMap,a1		;clr user map
		moveq	#127,d0
.ClrMap:	move.l	#-1,(a1)+
		dbf	d0,.ClrMap
		SCROLL	75
		bra.s	cco_End

cco_Level:	lea	LevelCode,a2		;end level
		bsr.s	cco_Check
		bne.s	cco_Bomb
		move	#1,sv_endlevel
		bra.s	cco_End

cco_Bomb:	lea	BombCode,a2		;give bomb
		bsr.s	cco_Check
		bne.s	cco_End
		move	#-12,sv_Glowica
		bsr	DrawBomb
cco_End:	movem.l	(sp)+,ALL
		rts


cco_check:	moveq	#31,d7
		moveq	#0,d0
.cco_1:		move	d0,d1
		moveq	#-1,d2
.cco_2:		addq	#1,d2
		addq	#1,d1
		andi	#15,d1
		move.b	(a2,d2.w),d3
		neg.b	d3
		beq.s	.cco_Found
		cmp.b	(a1,d1.w),d3
		beq.s	.cco_2
		addq	#1,d0
		andi	#15,d0
		dbf	d7,.cco_1
		moveq	#1,d0			;not found
		rts
.cco_Found:	move.l	#0,(a1)
		move.l	#0,4(a1)
		move.l	#0,8(a1)
		move.l	#0,12(a1)
		rts

;-------------------------------------------------------------------
; Take item from map
Take_Items:	
		lea		lc_variables(pc),a6
		move	sv_MapPos,d0
		lea		sv_Map,a1
		move.b	6(a1,d0.w),d1
		andi	#31,d1
		beq.w	ti_NoItem
		lea	sv_InSquarePos,a2
		movem	(a2),d2/d3
		cmpi	#512-220,d2		;chk if in column
		bmi.w	ti_NoItem
		cmpi	#512-220,d3
		bmi.w	ti_NoItem
		cmpi	#512+220,d2
		bpl.w	ti_NoItem
		cmpi	#512+220,d3
		bpl.w	ti_NoItem

		andi.b	#$c0,6(a1,d0.w)		;delete item from map
		move	#5,do_flash		;flash screen

		lea		sv_ITEMS+4,a2
		subq	#2,d1
		cmpi	#6,d1			;if < 6
		bpl		ti_ammo
; taken weapon (0..5)
		moveq	#2,d0
		add		d1,d0			; (2..7)
		move	d0,-(sp)
		SCROLL1					;print "taken..."
		addq	#1,d1
		bsr.w	ti_mul6
		bsr		GetRandom		; random weapon durability (damage)
		andi	#127,d0
		tst		sv_DIFFICULT
		beq.s	.sv_DIF				; 0 - hard (default)
		addi	#250,d0
.sv_DIF:
		addi	#250,d0
		move	d0,(a2,d1.w)		;weapon durability/damage
		SOUND	23,1,55				; "oooo"
		move	#5,do_Bron			; delay for weapon reload sound
		; set weapon as active if none chosen
		move	(sp)+,d0
		tst		-4(a2)				; check selected weapon
		bne		ti_NoItem
		move	2(a2,d1.w),d2
		add		d2,4(a2,d1.w)		; add ammo if this is the only weapon so that you start with some ammo

		moveq	#7,d2
		sub		d0,d2
		lsl		#1,d2
		addi	#$f1,d2
		move	d2,sv_Flag+6
		bra.w	ti_NoItem
ti_ammo:	subq	#6,d1
		cmpi	#6,d1			;if ammo
		bpl		ti_card
		moveq	#8,d0
		add		d1,d0
		SCROLL1
		SOUND	12,2,63
		addq	#1,d1
		bsr.w	ti_mul6
		move	2(a2,d1.w),d2
		tst		sv_DIFFICULT
		beq.s	.sv_DIF2				; 0 - hard (default)
		lsl		d2				; double ammo provided on EASY mode (2022 addition)
.sv_DIF2:
		cmp		-4(a2),d1		;actual weapon held in hand?
		bne.s	.ti_am2
		add		d2,-2(a2)		; update displayed ammo
		cmpi	#-999,-2(a2)
		bpl.w	ti_NoItem
		move	#-999,-2(a2)
		bra.w	ti_NoItem
.ti_am2:	add	d2,4(a2,d1.w)	; update weapon ammo
		cmpi	#-999,4(a2,d1.w)
		bpl.w	ti_NoItem
		move	#-999,4(a2,d1.w)
		bra.w	ti_NoItem
ti_card:	subq	#6,d1
		cmpi	#3,d1
		bpl.s	ti_aid
		moveq	#14,d0
		add	d1,d0
		SCROLL1
		SOUND	23,1,55
		lea	sv_Cards,a2
		bsr.w	ti_mul6
		subi	#1,4(a2,d1.w)		;add card
		bsr		tc_DrawCardCnt
		bra.w	ti_NoItem
ti_aid:		subq	#3,d1			; medkit
		bne.s	ti_power
		SCROLL	17
		tst		sv_DIFFICULT
		beq.s	.sv_DIF3				; 0 - hard (default)
		subi	#30,sv_Energy
.sv_DIF3:
		subi	#50,sv_Energy			; changed from always 30 to 50, plus 50 on EASY
		SOUND	15,1,63
		cmpi	#-999,sv_Energy
		bpl.w	ti_NoItem
		move	#-999,sv_Energy
		bra.w	ti_NoItem
ti_power:	subq	#1,d1
		bne.s	ti_Vodka
		move	sv_WalkSpeed+2,sv_WalkSpeed+4
		move	#300,sv_WalkSpeed+2	;max speed
		move	#300,sv_WalkSpeed+6	;CNT
		SCROLL	18
		SOUND	15,1,63
		bra.s	ti_NoItem
ti_Vodka:	subq	#1,d1
		bne.s	ti_Head
		addi	#190,lc_drunk(a6)
		SCROLL	19
		SOUND	15,1,63
		bra.s	ti_NoItem
ti_Head:	subq	#1,d1
		bne.s	ti_NoItem
		subi	#2,sv_Glowica
		SOUND	23,1,55
		bsr	DrawBomb

ti_NoItem:	rts

ti_mul6:	move	d1,d2
		add	d1,d1
		add	d1,d1
		add	d2,d2
		add	d2,d1			;*6
		rts

;-------------------------------------------------------------------
;a1 - sinus, a2- cosinus
;d6-kat, d0-x , d1-y
sv_rotate:	move	d0,d4
		move	d1,d5
		muls	(a1,d6.w),d0
		muls	(a2,d6.w),d5
		add.l	d5,d0		;x'=x*sin+y*cos
		add.l	d0,d0
		swap	d0
		muls	(a2,d6.w),d4	; x*cos
 		muls	(a1,d6.w),d1	; y*sin
		sub.l	d4,d1		;y'=y*sin-x*cos
		add.l	d1,d1
		swap	d1
		rts

;-------------------------------------------------------------------
;make szum on screen if hit...
sv_MAKE_SZUM:
		movem.l	a1-a4/d0-d4,-(sp)
		move.l	sv_ChunkyBuffer,a1
		lea		sv_ScrOffTab,a3
		move	sv_ViewWidth,d4
		lsr		#2,d4
		subq	#1,d4
		move	sv_size,d0
		subq	#1,d0
		lsl		#3,d0					; nr of lines depends on window size. E.g. size 5 (sv_size=6) has 5 * 8 = 40 lines
.sv_MS02:
		RANDOM	a2,d1
		move.l	d1,d2
		swap	d2

		eor		d1,d2
		andi	#$7f,d2
		add		d2,d2
		move	(a3,d2.w),d1			; pick a start offset based on random number. Note the offsets in the table wrap around so to always choose one of the rows actually on the screen.
		lea		(a1,d1.w),a4			; start of row
		move	d4,d2
.sv_MS03:	
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		move.l	#$88888888,(a4)+
		dbf	d2,.sv_MS03
		dbf	d0,.sv_MS02
		movem.l	(sp)+,a1-a4/d0-d4
		rts

;-------------------------------------------------------------------
;Sound replayer... by Kane of Suspect, 30.11.1994
;macro input:	sound nr, channel, volume

play_sound:	
		;movem.l	a1-a3/d0-d4,-(sp)
		lea		play_sample,a2
		tst.l	(a2)
		beq.s	.play_quit			; if all sounds == 0 then quit straight away
		lea		$dff000,a0
		lea		$a0(a0),a1
		lea		lc_soundList(pc),a3
		moveq	#3,d2
		move	#$8001,d1

.play_s1:	
		moveq	#0,d0
		move.b	(a2)+,d0
		beq.s	.play_s2
		move.b	#0,-1(a2)		;zero sound data
		subq	#1,d0
		lsl		#3,d0

		andi	#$f,d1
		move	d1,$96(a0)

		moveq	#5,d4
.play_delay:	
		move.b	$6(a0),d3		;delay change
.play_d1:	
		cmpi.b	$6(a0),d3
		beq.s	.play_d1
		dbf		d4,.play_delay
;		nop

		move.l	(a3,d0.w),(a1)		;adress
		move.l	4(a3,d0.w),4(a1)	;length in words, period
		move.b	3(a2),d0
		andi	#63,d0
		move	d0,8(a1)		;volume

		ori		#$8000,d1
		move	d1,$96(a0)

		moveq	#1,d4
.play_delay2:	
		move.b	$6(a0),d3		;delay change
.play_d2:	
		cmpi.b	$6(a0),d3
		beq.s	.play_d2
		dbf		d4,.play_delay2
;		nop
		
		move	#1,4(a1)		;length to 0

.play_s2:	
		lea		$10(a1),a1
		add		d1,d1
		dbf		d2,.play_s1
.play_quit:
		;movem.l	(sp)+,a1-a3/d0-d4
		rts

;-------------------------------------------------------------------
;fix appropriate part of bomb on screen

DrawBomb:	movem.l	ALL,-(sp)
		move	sv_Glowica,d0
		beq.s	db_1
		neg		d0
		cmpi	#13,d0
		bpl.s	db_1
		subq	#2,d0
		lsr		d0
		mulu	#6,d0
		lea		sv_Bomba,a1
		lea		(a1,d0.w),a1
		lea		sv_BombPos,a2
		move	#[31*5]-1,d1
db_2:	move.l	(a1),(a2)
		move	4(a1),4(a2)
		lea		36(a1),a1
		lea		40(a2),a2
		dbf		d1,db_2
		move	sv_Glowica,d0
		neg		d0
		subq	#2,d0
		lsr		d0
		addi	#77,d0
		tst		db_napisz
		beq.s	db_3
		SCROLL1				;print "bomba..."
		bra.s	db_3
db_1:	bsr		ClearBomb
db_3:	movem.l	(sp)+,ALL
		rts

ClearBomb:
		movem.l	d1/a2,-(sp)
		lea		sv_BombPos,a2		;only clear
		move	#[31*5]-1,d1
db_4:	andi	#$f000,(a2)
		move	#0,2(a2)
		andi	#$000f,4(a2)
		lea		40(a2),a2
		dbf		d1,db_4
		movem.l	(sp)+,d1/a2
		rts

;-------------------------------------------------------------------
;Make code for copying columns to bit-table... Kane/SCT, 09.02.1994
;No input...
mc_MakeCode:
		movem.l	ALL,-(sp)
		move.l	lc_FastMem1(pc),a1
;		lea	(a1),a5
		addi.l	#mc_code,a1		;addr table
		lea	[mc_MaxHeigth*4](a1),a2	;code table
;		addi.l	#mc_Htab,a5		;Heigth table
		lea	mc_Htab,a5		;Heigth table
		lea	[mc_MaxHeigth*4](a5),a6
		move.l	a2,(a1)+		;fix zero
		move	#$4e75,(a2)+		;rts
		move.l	a6,(a5)+
		move.b	#-1,(a6)+
		lea	sv_LineTab,a3

		moveq	#0,d4			;down offset
		move	sv_ViewWidth,d3
		lsl	#3,d3			;view window width
		move	d3,d2
		neg	d2			;up offset
		move	d2,d1			;store offsets
		move	d4,d5
		moveq	#1,d7			;linii
mc_loop32:
		move	d1,d2
		move	d5,d4
		move.l	a2,(a1)+		;code part addr
		move.l	a6,(a5)+
		moveq	#31,d6
mc_cl32loop:	move.b	#0,(a6)+		;clear Htab cell
		dbf	d6,mc_cl32loop
		move.b	#-1,(a6)+		;end cell
		move.l	-4(a5),a6

		moveq	#1,d6			;n
mc_l322:	moveq	#0,d0
		move	d6,d0
		lsl	#5,d0
		divu	d7,d0

		subq	#1,d0
		move.b	#1,(a6,d0.w)		;set bar in Htab cell
		move	#$1569,(a2)+		;move.b
		move	d0,(a2)+		; X1 X2
		move	d4,(a2)+		;down - Y1 Y2
		addq	#1,d0
		neg	d0
		move	#$1569,(a2)+
		move	d0,(a2)+
		move	d2,(a2)+		;up
		add	d3,d4
		sub	d3,d2

		addq	#1,d6
		cmp	d6,d7
		bpl.s	mc_l322

		lea	33(a6),a6		;set to next cell
		move	#$4e75,(a2)+		;rts
		addq	#1,d7
		cmpi	#32,d7
		bne.s	mc_loop32


;---------------Bigger than 32 lines...
;		moveq	#32,d7			;linii
		move	sv_ViewHeigth,d1
		lsr	d1
mc_loopMore:
		lea	sv_LineTab,a3
		moveq	#0,d5			;line lumber
		moveq	#0,d4			;how many these?
		moveq	#1,d2			;wall lines
		moveq	#0,d6			;n
mc_lM2:		moveq	#0,d0
		move	d6,d0
		lsl	#5,d0
		divu	d7,d0

		cmp	d0,d5
		bne.s	mc_M3
		addq	#1,d4
		bra.s	mc_M4
mc_M3:
		move	d4,(a3)+
		moveq	#1,d4
		move	d0,d5
		addq	#1,d2			;next wall line
mc_M4:		addq	#1,d6
		cmp	d6,d1
		beq.s	mc_M5
		cmp	d6,d7
		bne.s	mc_lM2
mc_M5:		move	d4,(a3)+
		move	#-1,(a3)		;end of tab


		move.l	a2,(a1)+		;code part addr
		move.l	a6,(a5)+		;H cell addr
		move	d2,d0
		neg	d0
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_1
		move	#$43e9,(a2)+		;lea x(a1),a1
		move	d0,(a2)+
.mc_1:		move	d6,d0
		mulu	d3,d0
		neg	d0			;SVGA first offset
		subq	#1,d2
mc_c2loop:	move	-(a3),d6
		move	d6,-(sp)
mc_c21:		subq	#1,d6
		beq.s	mc_m6
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_2
		move	#$1551,(a2)+		;move	(a1),y(a2)
		move	d0,(a2)+
.mc_2:		add	d3,d0
		bra.s	mc_c21
mc_M6:
		move	(sp)+,d6
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_3
		bsr.w	mc_optim
		move	#$1559,(a2)+		;move	(a1)+,y(a2)
		move	d0,(a2)+
.mc_3:		add	d3,d0
		dbf	d2,mc_c2loop

mc_c2loop2:	move	(a3)+,d6
		bmi.s	mc_M8
		move.b	d6,(a6)+		;set row repeats in cell
		move	d6,-(sp)
mc_c22:		subq	#1,d6
		beq.s	mc_m7
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_4
		move	#$1551,(a2)+		;move	(a1),y(a2)
		move	d0,(a2)+
.mc_4:		add	d3,d0
		bra.s	mc_c22
mc_M7:
		move	(sp)+,d6
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_5
		bsr.s	mc_optim
		move	#$1559,(a2)+		;move	(a1)+,y(a2)
		move	d0,(a2)+
.mc_5:		add	d3,d0
		bra	mc_c2loop2

mc_M8:
		cmpi	#mc_maxHeigth-90,d7
		bpl.s	.mc_6
		move	#$4e75,(a2)+		;rts
.mc_6:		move.b	#-1,(a6)+		;end cell


		move.l	-4(a1),a3		;compress code
		move.l	a2,d6
		move.l	a3,d5
		sub.l	d5,d6
		lsr	d6
		beq.s	mc_notequ
		subq	#1,d6			;nr of words
		move.l	-8(a1),a4
mc_check1:	move	(a4)+,d0
		cmp	(a3)+,d0
		bne.s	mc_notequ		;if not the same
		dbf	d6,mc_check1
		move.l	-4(a1),a2
		move.l	-8(a1),-4(a1)
		move.l	-4(a5),a6		;remove last cell
		move.l	-8(a5),-4(a5)
mc_notequ:	addq	#1,d7
		cmpi	#mc_maxHeigth,d7
		bne.L	mc_loopMore

;move.l	a2,ddd
		movem.l	(sp)+,ALL
		rts


mc_optim:
		cmpi	#4,d6
		bmi.s	.mc_o1
		subq	#2,d6
		lea	(a2),a4
.mc_s4:		move	-(a4),2(a4)
		move	#$1547,(a4)
		lea	-2(a4),a4
		dbf	d6,.mc_s4
		lea	2(a2),a2
		move.l	#$1e111547,(a4)		;m.(a1)+,d7   m.d7,x(a1)
.mc_o1:		rts

;ddd:	dc.l	0
;-------------------------------------------------------------------
;Make plane tables ... no input
make_PLANES:
		movem.l	ALL,-(sp)
		move.l	lc_FastMem1(pc),a1
		addi.l	#sv_PLANES,a1			;addr table
		lea	260(a1),a2			;plane table
		lea	(a2),a3				;p.t. 2
		move	#350,d6				;y' (max)
		move.l	#500*[2^SHLeft],d7
		divu	d6,d7				;z min(365)
		addq	#1,d7				;366
		move	#350,d6				;min Y = 91
mp_loop1:
		move.l	#500*[2^SHLeft],d5
		divu	d6,d5				;z'
		addq	#1,d5
		move	d5,d1				;save Z odl
		sub	d7,d5				;dZ

		move.l	a2,(a1)+			;save addr
		moveq	#0,d3				;first x''
		moveq	#1,d0				;a <1-64>
mp_loop2:
		move	d5,d4
		mulu	d0,d4
		lsr.l	#6,d4				;dZ*[a/64]
		move	d1,d2
		sub	d4,d2				;z'
		move	#1000,d4
		mulu	d0,d4
		lsr.l	#6,d4				;x'
		lsl.l	#SHLeft,d4
		divu	d2,d4				;x''
		move	d4,d2				;store
		sub	d3,d4				;dX''
		subq	#1,d0
		subq	#1,d4
		move	d0,-(sp)
;		add	d0,d0
;		add	d0,d0				;*4
mp_colloop:	move.b	d0,(a2)+			;pixel width
		dbf	d4,mp_colloop
		move	(sp)+,d0
		move	d2,d3				;new x''
		addq	#2,d0
		cmpi	#65,d0
		bne.s	mp_loop2

		move.l	a2,d0
		sub.l	a3,d0
		subi	#700,d0
		beq.s	mp_LenOK
		bmi.s	mp_Shorter
		sub.l	d0,a2				;if >700
		bra.s	mp_LenOK
mp_Shorter:	neg	d0
		subq	#1,d0
		move	#$3f,d2
mp_AddCloop:	move.b	d2,(a2)+			;if <700
		dbf	d0,mp_AddCloop			;add $3f...
mp_LenOK:	lea	(a2),a3
		subq	#4,d6
		cmpi	#90,d6
		bne.s	mp_loop1

		movem.l	(sp)+,ALL
		rts

;-------------------------------------------------------------------
;Make various tables... no input
make_tables:
		movem.l	ALL,-(sp)

		; --- generate code to draw floors for the no-cache mode. It generates the whole floor in one go.
		lea		fl_DCode(pc),a1			; floor code template
		move.l	lc_FastMem1(pc),a2
		addi.l	#fl_floors,a2			; floor code
		lea		fl_DoJsr(pc),a3
		move.l	a2,2(a3)				;fix jsr addr for floors
		move	#191,d7					; copy the code 192 times - once for each pixel
mk_DCD1:lea		(a1),a3
		moveq	#[[fl_DcodeEnd-fl_DCode]/2]-1,d6
mk_DCD2:move.w	(a3)+,(a2)+
		dbf		d6,mk_DCD2
		dbf		d7,mk_DCD1
		move	#$4e75,(a2)+		;rts

		bsr		sv_MakeWidthTab		;make sv_width_tables

		lea	sv_ConstTab,a1
		move.l	sv_ChunkyBuffer,a2
		move	sv_ViewWidth,d1
		move	d1,d2
		lsr	d2
		subq	#1,d2
		move	d2,28(a1)		;width/16 - 1
		add	d1,d1
		move	d1,30(a1)		;width/4 - 1
		subi	#1,30(a1)
		add	d1,d1			;*4
		move	d1,(a1)			;width/2
		add	d1,d1			;*8
		move	d1,6(a1)		;width

		lea	shZ_WmulTab(pc),a3
		moveq	#15,d0
		move	d1,d2
mk_Wmt2:	move	d2,(a3)+
		add	d1,d2
		dbf	d0,mk_Wmt2
		move	sv_ViewHeigth,d2
		lsr	#1,d2
		mulu	d2,d1
		lea	(a2,d1.w),a2		;SCR tab middle
		move.l	a2,8(a1)
		lea	64*192(a2),a3		;zero wall tab start
		move.l	a3,44(a1)

		move	sv_WallHeigth,d1
		muls	sv_Size,d1		;scale screen
		divs	#6,d1
		ext.l	d1
		lsl.l	#SHLeft,d1		;*256
		move.l	d1,2(a1)

		; make statis floor perspective tab
		lea	sv_FloorTab,a3
		lea	2(a3),a4
		moveq	#-1,d0
		move	sv_ViewHeigth,d2
		lsr	#1,d2
mk_FlTab:	move.l	d1,d3			;Floor perspective tab
		divu	d2,d3
		addi	#[2^SHLeft],d3
		cmpi	#max_Distance,d3
		bpl.s	mk_FTend
		addq	#1,d0
		swap	d3
		move	#0,d3
		lsr.l	#1,d3
		divu	#max_Distance,d3	;rescale to x/$8000
		move	d3,(a4)+
		subq	#1,d2
		bne.s	mk_FlTab
mk_FTend:	move	d0,(a3)


		move.l	lc_FastMem1(pc),a3
		lea	(a3),a4
		lea	(a3),a5
		addi.l	#co_Walls,a3
		move.l	a3,12(a1)
		lea	(a4),a3
		addi.l	#mc_code,a4
		move.l	a4,16(a1)
		addi.l	#sv_PLANES,a5
		move.l	a5,20(a1)
;		addi.l	#mc_Htab,a3
		lea	mc_Htab,a3
		move.l	a3,24(a1)

		lea		sv_DeltaTab+[600*4],a2
		lea		(a2),a3
		moveq	#0,d7			;calosc
		move	6(a1),d2		;wybierz
mk_DelTab:	move.l	d7,d3			;make DELTA const tab
		divu	d2,d3			; (for floor)
		move	d3,d1			;C
		move	#0,d3
		divu	d2,d3			;R
		bne.s	mk_DT2
		addq	#1,d3			;R can't be 0

mk_DT2:		move	d3,(a3)+
		move	d1,(a3)+
		neg		d1
		subq	#1,d1
		move	d1,-(a2)		;minus deltas
		neg		d3
		move	d3,-(a2)
		addq	#1,d7
		cmpi	#600,d7
		bne.s	mk_DelTab

		lea		sv_DeltaTab+[600*4],a2	;fix 0 error
		move.l	4(a2),(a2)
		move.l	-8(a2),-4(a2)


		lea		sv_WallOffsets,a2	;remove zero wall bytes
		move.l	wall_floor1(a2),d0
		move.l	12(a1),a2
		lea		(a2,d0.l),a2
		move.l	a2,32(a1)		;floor addr - store in constants
		bsr.w	mk_FixFloors
		lea		sv_WallOffsets,a2
		move.l	wall_floor2(a2),d0
		move.l	12(a1),a2
		lea		(a2,d0.l),a2
		move.l	a2,36(a1)		;ceiling addr - store in constants
		bsr.w	mk_FixFloors

		move.l	sv_ChunkyBuffer,a2
		move	sv_ViewWidth,d1		;view window dim.
		move	sv_ViewHeigth,d2
		lsl		#3,d1
		subq	#1,d2
		mulu	d2,d1
		lea		(a2,d1.w),a2
		move.l	a2,40(a1)		;SVGA end addr - store in constants

		bsr		mk_FixFloorMod		;set fl. pixel offsets

		lea		fl_Dcont(pc),a2		;floor row modulos - SELF MODIFYING CODE!!
		lea		flc_Dcont(pc),a3
		move	6(a1),d0		;width (a1=sv_ConstTab)
		move	d0,6(a2)
;		move	d0,6(a3)
		neg		d0
		move	d0,2(a2)
		asl		d0				; * 2 to go 2 lines back
		move	d0,2(a3)

		lea		sv_RotTable,a2
		cmpi.l	#$50100,(a2)
		bne.s	mk_NotFR
		move.l	a2,d2
		moveq	#63,d0
mk_FixRot:	move.l	(a2),d1
		subi.l	#$50000,d1
		add.l	d2,d1
		move.l	d1,(a2)+
		dbf		d0,mk_FixRot
mk_NotFR:

;eliminate zero-line collumn & enemy drawing
		lea		sv_CollumnOffsets,a2
		move.l	(a2),d0
		move.l	sv_Consttab+12,a2
		lea		(a2,d0.l),a2		;first col.addr
		move	#[8*32]-1,d7		;collumn nr.
		bsr	mk_coll1

		move.l	sv_Consttab+12,a2	;same with enemy
		addi.l	#40*65*64,a2		;enemy 1 addr
		move	#[24*32]-1,d7
		bsr	mk_coll1
		move.l	sv_Consttab+12,a2	;same with enemy 2
		addi.l	#105*65*32,a2		;enemy 2 addr
		move	#[24*32]-1,d7
		bsr	mk_coll1

		lea		sv_BloodOffsets,a2	;blood not zero-wall! - clear zero wall indicators
		move.l	(a2),d0
		move.l	sv_Consttab+12,a2
		lea		(a2,d0.l),a2		;first blood.addr
		move	#[3*64]-1,d7		; repeat for all 3 blood textures
mk_fixblood:	move.b	#0,64(a2)
		lea		65(a2),a2
		dbf		d7,mk_fixblood

		lea	sv3_DoubleTab(pc),a2	;double table for stretch
		lea	sv4_DoubleTab(pc),a3
		moveq	#0,d0
mk_DoublT:	move	d0,d1
		moveq	#0,d2
		moveq	#7,d3
mk_DTloop:	add	d2,d2			;free 2 bits
		add	d2,d2
		add.b	d1,d1
		bcc.w	mk_DouT2
		ori	#3,d2			;if 1 set 2 bits
mk_DouT2:	dbf	d3,mk_DTloop
		move	d2,(a2)+
		move	d2,(a3)+
		addq	#1,d0
		cmpi	#256,d0
		bne.s	mk_DoublT


		lea	sc_Text,a2		;make text offsets
		move.l	a2,d1
		lea	sv_TextOffsets,a3
.MTO1:		move.b	(a2)+,d0
		cmpi.b	#"@",d0
		bne.s	.MTO1
		move.l	a2,d0
		sub.l	d1,d0
		move	d0,(a3)+
		cmpi.b	#"@",(a2)
		bne.s	.MTO1

		movem.l	(sp)+,ALL
		rts


;in a2 - col addr, d7 - width?
mk_Coll1:	
		moveq	#63,d6			;col.heigth
		moveq	#1,d0			;test byte 1-not draw
mk_coll2:
		tst.b	(a2)+
		beq.s	mk_coll3
		moveq	#0,d0
mk_coll3:
		dbf		d6,mk_coll2
		move.b	d0,(a2)+
		dbf		d7,mk_Coll1
		rts

;-------------------------------------------------------------------
; remove the 65-th collumn (the transpacency one) from floors as drawing needs to iterate through 64-pixel textures
mk_FixFloors:	
		lea		64(a2),a2		;floor row1 end
		cmpi.l	#"KANE",63*64(a2)		; why do it only once?
		beq.s	mk_FFquit
		lea		1(a2),a3
		moveq	#62,d0
mk_FixFloor1:	moveq	#63,d1
mk_FF1:		move.b	(a3)+,(a2)+			; just move them left by 1 pixel
		dbf		d1,mk_FF1
		lea		1(a3),a3
		dbf		d0,mk_FixFloor1
		move.l	#"KANE",(a2)
mk_FFquit:	
		rts

;-------------------------------------------------------------------
; width table - not that this depends on whether CPU or BLITTER is used for C2P
sv_MakeWidthTab:
		lea		sv_WidthTable(pc),a3
		lea		sv_WidthTable2(pc),a2		; this is required only because otherwise some x(pc,dy) branches exceed 8 bit offset
		move	sv_ViewWidth,d3
		moveq	#0,d0
		tst		sv_Buse
		bne.s	mk_W2tab		;if CPU use only

; BLITTER version
mk_Wtab:
		move	d0,d1
		move	d0,d2
		lsr		#3,d1			;single add	 - multiples of 8
		andi	#7,d2			;8 add		 - 3 lowe bits (up to 8)
		mulu	d3,d2			; 0..7 * width
		add		d1,d2			; add the oryginally more significant bits
		move.b	d2,(a3)+		;for ScrTab pos...
		move.b	d2,(a2)+		;for ScrTab2 pos...
		addq	#1,d0
		cmpi	#192,d0
		bne.s	mk_Wtab
		rts

; CPU version (straightforward 1:1 - maybe not even needed?)
mk_W2tab:	
		move.b	d0,(a3)+		;for ScrTab pos...
		move.b	d0,(a2)+		;for ScrTab2 pos...
		addq	#1,d0
		cmpi	#192,d0
		bne.s	mk_W2tab
		rts

;-------------------------------------------------------------------
; fix pixel offsets into the chunky table in the auto-generated floor code 
mk_FixFloorMod:	
		lea		sv_ConstTab,a1
		move.l	lc_FastMem1(pc),a2
		addi.l	#fl_Floors,a2				; floor code
		move	mk_BraPos(pc),d0			; old addx position
		move	#$de4e,(a2,d0.w)			; "add.w a6,d7" replaces old rts
		lea		sv_WidthTable(pc),a3		; 1..192 for no CPU and more complex for blitter
		move	6(a1),d7					;width in pixels (up to 192)
		subq	#1,d7
mk_DCoffsets:	
		move.b	(a3),21(a2)		;FIX FLOOR OFFSETS
		move.b	(a3)+,29(a2)
		lea		30(a2),a2		;next pixel
		dbf		d7,mk_DCoffsets

		move	#$4e75,(a2)		;add rts
		lea		fl_Dcode(pc),a3
		sub.l	a3,a2
		move	a2,d0
		lea		mk_BraPos(pc),a2
		move	d0,(a2)
		rts

mk_BraPos:	dc.w	0
;-------------------------------------------------------------------
sv_SetWindowSize:
		movem.l	ALL,-(sp)
		moveq.l	#0,d0
		move	sv_Size,d0
		add		d0,d0
		add		d0,d0			;max 24
		move	d0,sv_ViewWidth			; in bytes (pixels/8)
		tst		sv_StrFlag
		beq.s	sv_SWS1
		move	#128,d1
		bra.s	sv_SWS2
sv_SWS1:	
		move.l	d0,d1
		lsl		#7,d1			;*128
		divu	#24,d1			;max
		andi	#-2,d1			;clr bit 0
sv_SWS2:	
		move	d1,sv_ViewHeigth

		move	d0,d3
		lsl		#3,d3
		moveq	#0,d2
		moveq	#0,d4
		moveq	#0,d5

		lea		sv_ScrOffTab,a1
.sv_SWSL:	
		move	d2,(a1)+			; d2 - offset of every new row in chunky mode
		add		d3,d2				; d3 - screen width in bits
		addq	#1,d5
		cmpi	#128,d5				; repeat max max-eigth times
		beq.s	.sv_SWSL2
		addq	#1,d4
		cmp		d1,d4
		bne.s	.sv_SWSL
		moveq	#0,d4				; if heigth less than 128 then it wraps around, why?
		moveq	#0,d2
		bra.s	.sv_SWSL
.sv_SWSL2:

		bsr		mc_MakeCode		;generate raster code
		bsr		make_tables		;different tables

		exg		d0,d1
		move	d0,d7			;store Y
		lsr		#1,d0			;center Y
		move	#128/2,d2
		sub		d0,d2
		move	d2,d6			;store Y empty
		addi	#sv_Upoffset,d2		;16 from top
		mulu	#row*5,d2
		addq.l	#sv_Leftoffset,d2	;2 from left
		lsr		#1,d1			;center X
		move	#12,d3
		sub		d1,d3
		add		d3,d2
		move.l	d2,sv_offset

		lea		$dff000,a0
;		move.l	#$90f2c4,cop_borders
		move.b	#$c9,cop_cont		;panel pos
		move.b	#$ca,cop_cont2
		move.b	#$f2,cop_cont2+8
		tst		sv_StrFlag
		beq		sv_SWS7
		tst		sv_StrFlag+2		;last was stretched?
		beq		.sv_lastnot		;no
		VBLANK
		move.l	#copper0,$80(a0)
		move	#0,$88(a0)		
.sv_lastnot:
		waitblt				;clear big screen
		move	#$8040,$96(a0)		;blitter NASTY & DMA on
		move.l	#-1,$44(a0)
		move.l	#$1000000,$40(a0)
		move	#0,$66(a0)		;modulo
		move.l	sv_Screen,d2
		addi.l	#[16*row*5],d2
		move.l	d2,$54(a0)
		move	#[128*5*64]+20,$58(a0)	;clear screen 1
		waitblt
		move.l	sv_Screen+4,d2
		addi.l	#[16*row*5],d2
		move.l	d2,$54(a0)
		move	#[128*5*64]+20,$58(a0)	;clear screen 2
		waitblt

		lea		cop2_area,a2
		move.l	#$0108FFD8,d1
		move.l	#$010aFFD8,d2
		move.l	#$010800a0,d3
		move.l	#$010a00a0,d4
		cmpi	#8,sv_Size+2
		bpl.s	sv_SWS6
		move.l	#$2901ff00,d0			; window 7
		moveq	#31,d7
mk_cop2:	addi.l	#$04000000,d0
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		dbf		d7,mk_cop2
		move.l	#cop_cont,d0
		move	#$80,(a2)+
		swap	d0
		move	d0,(a2)+
		swap	d0
		move	#$82,(a2)+
		move	d0,(a2)+
		move.l	#$880000,(a2)+		;jump to copper 1
		move.l	#$90f2c4,cop_borders
		bra.w	sv_SWS5
sv_SWS6:					;biggest windows (7, 8)
		move.l	#$9029c4,cop_borders
		move.l	#$29dffffe,d0
		cmpi	#8,sv_Size+2
		beq.s	sv_Big_and_Panel

		moveq	#127,d7
mk_cop3:	addi.l	#$01000000,d0		;big/no panel
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		dbf	d7,mk_cop3
		move.l	#$2a01ff00,(a2)+
		move.l	#$01000300,(a2)+
		move.l	#RealCopper,d0		;next copper 1
		move	#$80,(a2)+
		swap	d0
		move	d0,(a2)+
		swap	d0
		move	#$82,(a2)+
		move	d0,(a2)+
		move.l	#-2,(a2)+		;end of cop
;		lea	RealCopper,a4
;		move.l	a4,$80(a0)
		bra.w	sv_SWS5

sv_Big_and_Panel:				;big window + panel
		move.b	#$ff,cop_cont		;panel pos
		move.b	#$00,cop_cont2
		move.b	#$28,cop_cont2+8

		moveq	#20,d7
mk_cop4:	addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		REPT	3
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		ENDR
		dbf	d7,mk_cop4

		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+
		addi.l	#$01000000,d0
		move.l	d0,(a2)+
		move.l	d1,(a2)+
		move.l	d2,(a2)+
		move.l	#$ff01ff00,(a2)+
		move.l	d3,(a2)+
		move.l	d4,(a2)+

		lea	cop2_area+4,a1		;fix first double
		move.l	d1,(a1)+
		move.l	d2,(a1)

		move.l	#cop_cont,d0		;next copper 1
		move	#$80,(a2)+
		swap	d0
		move	d0,(a2)+
		swap	d0
		move	#$82,(a2)+
		move	d0,(a2)+
		move.l	#$880000,(a2)+		;jump to copper 1
;		lea	RealCopper,a4
;		move.l	a4,$80(a0)
		bra.w	sv_SWS5


sv_SWS7:	tst	sv_StrFlag+2		;last was stretched?
		beq	sv_SWS9
		move	#0,sv_StrFlag+2

		lea		sv_WindowSav,a1
		move.l	sv_Screen,a2
		addi.l	#[sv_Upoffset*5*row],a2
		moveq	#0,d0
		move	#[130*5]-1,d7	; should this be 128?
.sv_GetWindow:	
		move.l	(a1)+,(a2)+		;copy background from buffer to screen
		move.l	(a1)+,(a2)+		; 8 bytes, then clear 24, then copy 8 again
		REPT	6
		move.l	d0,(a2)+
		ENDR
		move.l	(a1)+,(a2)+
		move.l	(a1)+,(a2)+
		dbf	d7,.sv_GetWindow


		move.l	sv_screen,a2
		addi.l	#sv_UPoffset*row*5,a2
		move.l	sv_screen+4,a1
		addi.l	#sv_UPoffset*row*5,a1
		move	#[[130*row*5]/16]-1,d0
sv_SWS8:	move.l	(a2)+,(a1)+		;copy it to scr 2
		move.l	(a2)+,(a1)+
		move.l	(a2)+,(a1)+
		move.l	(a2)+,(a1)+
		dbf	d0,sv_SWS8
		move.l	#$90f2c4,cop_borders
		bra.s	sv_SWS5

sv_SWS9:	
		lea	$dff000,a0
		waitblt
		move	#$8040,$96(a0)		;blitter NASTY & DMA on
		move.l	#-1,$44(a0)
		move.l	#$1000000,$40(a0)
		move	#40-24,$66(a0)		;modulo
		move.l	sv_Screen,d2
		addi.l	#[16*row*5]+8,d2
		move.l	d2,$54(a0)
		move	#[128*5*64]+12,$58(a0)	;clear screen 1
		waitblt
		move.l	sv_Screen+4,d2
		addi.l	#[16*row*5]+8,d2
		move.l	d2,$54(a0)
		move	#[128*5*64]+12,$58(a0)	;clear screen 2
		move.l	#$90f2c4,cop_borders

sv_SWS5:
		lea		$dff000,a0
		waitblt
		VBLANK

		lea		cop_ACTUAL,a1
		move.l	#copper1_std,d0				; cop_screen is the usual non-stretched copperlist
		tst		sv_StrFlag
		beq.s	sv_SWS3
		move.l	#copper2_str,d0				; if screen stretched then use this one
sv_SWS3:
		move	d0,6(a1)
		swap	d0
		move	d0,2(a1)
		move.l	#RealCopper,$80(a0)
		move	#0,$88(a0)
		movem.l	(sp)+,ALL
		rts


;-------------------------------------------------------------------
;-------------------------------------------------------------------
copper:		;this copper is ORG from RealCoper but put here.

	RealCopper:	equ	BASEC+$58630		;$da0
	OFFSET	RealCopper

	dc.w	$0180,0,$0182,0,$0184,0,$0186,0
	dc.w	$0188,0,$018a,0,$018c,0,$018e,0
	dc.w	$0190,0,$0192,0,$0194,0,$0196,0
	dc.w	$0198,0,$019a,0,$019c,0,$019e,0
	dc.w	$01a0,0,$01a2,0,$01a4,0,$01a6,0
	dc.w	$01a8,0,$01aa,0,$01ac,0,$01ae,0
	dc.w	$01b0,0,$01b2,0,$01b4,0,$01b6,0
	dc.w	$01b8,0,$01ba,0,$01bc,0,$01be,0

	dc.l	$1fc0000,$1060000,$10c0000		;AGA OFF!!!

	dc.l	$920038,$9400d0,$8e2a84
cop_borders:
	dc.l	$90f2c4
	dc.l	$1020033,$1040000
	dc.w	$108,4*row,$10a,4*row		; modulo

	dc.l	$2001ff00
cop_ACTUAL:
	dc.w	$80,copper1_std/$10000,$82,copper1_std&$ffff,$88,0		; this points to the next part of the copper list - choice between cop_screen (default) and copper2 (stretched)

copper1_std:	; standard copper
	dc.w	$80,cop1_screen1/$10000,$82,cop1_screen1&$ffff,$88,0
cop1_screen1:
	dc.w	$e0,screen1/$10000,$e2,screen1&$ffff
	dc.w	$e4,[screen1+row]/$10000,$e6,[screen1+row]&$ffff
	dc.w	$e8,[screen1+[2*row]]/$10000,$ea,[screen1+[2*row]]&$ffff
	dc.w	$ec,[screen1+[3*row]]/$10000,$ee,[screen1+[3*row]]&$ffff
	dc.w	$f0,[screen1+[4*row]]/$10000,$f2,[screen1+[4*row]]&$ffff
	dc.w	$80,cop1_postaddr/$10000,$82,cop1_postaddr&$ffff,$88,0

cop1_screen2:
	dc.w	$e0,screen2/$10000,$e2,screen2&$ffff
	dc.w	$e4,[screen2+row]/$10000,$e6,[screen2+row]&$ffff
	dc.w	$e8,[screen2+[2*row]]/$10000,$ea,[screen2+[2*row]]&$ffff
	dc.w	$ec,[screen2+[3*row]]/$10000,$ee,[screen2+[3*row]]&$ffff
	dc.w	$f0,[screen2+[4*row]]/$10000,$f2,[screen2+[4*row]]&$ffff

cop1_postaddr:
	dc.l	$2a01ff00,$01005300
	
cop_cont:		; all copperlists finally jump here
	dc.l	$c9e1fffe,$01000300
	dc.w	$e0,[screen1+$7d00]/$10000,$e2,[screen1+$7d00]&$ffff
	dc.w	$e4,[screen1+$7d00+row]/$10000,$e6,[screen1+$7d00+row]&$ffff
	dc.w	$e8,[screen1+$7d00+[2*row]]/$10000,$ea,[screen1+$7d00+[2*row]]&$ffff
	dc.w	$ec,[screen1+$7d00+[3*row]]/$10000,$ee,[screen1+$7d00+[3*row]]&$ffff
	dc.w	$f0,[screen1+$7d00+[4*row]]/$10000,$f2,[screen1+$7d00+[4*row]]&$ffff
cop_cont2:
	dc.l	$ca01ff00,$01005300
	dc.l	$f201ff00,$01000300
	dc.w	$80,RealCopper/$10000,$82,RealCopper&$ffff
	dc.l	-2

copper0:
	dc.l	$01000300,$1800000,-2

copper2_str:	; stretched copper
	dc.w	$80,cop2_screen1/$10000,$82,cop2_screen1&$ffff,$88,0

cop2_screen1:
	dc.w	$e0,[screen1+sv_UpOffset*5*row]/$10000,$e2,[screen1+sv_UpOffset*5*row]&$ffff
	dc.w	$e4,[screen1+sv_UpOffset*5*row+row]/$10000,$e6,[screen1+sv_UpOffset*5*row+row]&$ffff
	dc.w	$e8,[screen1+sv_UpOffset*5*row+[2*row]]/$10000,$ea,[screen1+sv_UpOffset*5*row+[2*row]]&$ffff
	dc.w	$ec,[screen1+sv_UpOffset*5*row+[3*row]]/$10000,$ee,[screen1+sv_UpOffset*5*row+[3*row]]&$ffff
	dc.w	$f0,[screen1+sv_UpOffset*5*row+[4*row]]/$10000,$f2,[screen1+sv_UpOffset*5*row+[4*row]]&$ffff
	dc.w	$80,cop2_postaddr/$10000,$82,cop2_postaddr&$ffff,$88,0

cop2_screen2:
	dc.w	$e0,[screen2+sv_UpOffset*5*row]/$10000,$e2,[screen2+sv_UpOffset*5*row]&$ffff
	dc.w	$e4,[screen2+sv_UpOffset*5*row+row]/$10000,$e6,[screen2+sv_UpOffset*5*row+row]&$ffff
	dc.w	$e8,[screen2+sv_UpOffset*5*row+[2*row]]/$10000,$ea,[screen2+sv_UpOffset*5*row+[2*row]]&$ffff
	dc.w	$ec,[screen2+sv_UpOffset*5*row+[3*row]]/$10000,$ee,[screen2+sv_UpOffset*5*row+[3*row]]&$ffff
	dc.w	$f0,[screen2+sv_UpOffset*5*row+[4*row]]/$10000,$f2,[screen2+sv_UpOffset*5*row+[4*row]]&$ffff

cop2_postaddr:
	dc.l	$2a01ff00,$01005300

cop2_area:
	; beyond this point is a dynamically generated copperlist
	
	ENDOFF

	dc.l	0,0,0,0
;ds.l	[260*3]
EndCopper:

;-------------------------------------------------------------------
st_Offsets:
	sv_Offsets:	equ	BASEC+$2a000		;$2e0
	OFFSET	sv_Offsets

sv_WallOffsets:		;start offsets of walls in table
	dc.l	0,65*64,2*65*64,3*65*64,4*65*64
	dc.l	5*65*64,6*65*64,7*65*64,8*65*64,9*65*64
	dc.l	10*65*64,11*65*64,12*65*64,13*65*64,14*65*64
	dc.l	15*65*64,16*65*64,17*65*64,18*65*64,19*65*64

	dc.l	20*65*64,21*65*64,22*65*64,23*65*64,24*65*64
	dc.l	25*65*64,26*65*64,39*65*64				;+buffer

sv_CollumnOffsets:
	dc.l	27*65*64,27*65*64+[65*32],28*65*64,28*65*64+[65*32]	;collumns
	dc.l	29*65*64,29*65*64+[65*32],30*65*64,30*65*64+[65*32]
	dc.l	31*65*64,31*65*64+32,31*65*64+[65*32],31*65*64+[65*32]+32 ;up
	dc.l	32*65*64,32*65*64+32,32*65*64+[65*32],32*65*64+[65*32]+32
	dc.l	33*65*64,33*65*64+32,33*65*64+[65*32],33*65*64+[65*32]+32 ;down
	dc.l	34*65*64,34*65*64+32,34*65*64+[65*32],34*65*64+[65*32]+32

	dc.l	52*65*64,52*65*64+32			;25-28 killed enemies
	dc.l	64*65*64+[65*32],64*65*64+[65*32]+32

sv_ItemOffsets:
; items, 29-48
	dc.l	65*65*64,65*65*64+32,65*65*64+[65*32],65*65*64+[65*32]+32
	dc.l	66*65*64,66*65*64+32,66*65*64+[65*32],66*65*64+[65*32]+32
	dc.l	67*65*64,67*65*64+32,67*65*64+[65*32],67*65*64+[65*32]+32
	dc.l	68*65*64,68*65*64+32,68*65*64+[65*32],68*65*64+[65*32]+32
	dc.l	69*65*64,69*65*64+32,69*65*64+[65*32],69*65*64+[65*32]+32
	;objects, 49-64
	dc.l	70*65*64-16,70*65*64
	dc.l	70*65*64+[65*32]-16,70*65*64+[65*32]
	dc.l	70*65*64+[65*48]-16,70*65*64+[65*48]
	dc.l	70*65*64+[65*16]-16,70*65*64+[65*16],71*65*64+[65*32]+32
	dc.l	71*65*64+[65*32]-16,71*65*64+[65*32],71*65*64+[65*32]+16
	dc.l	70*65*64+32,70*65*64+[65*32]+32,71*65*64,71*65*64+32

sv_BloodOffsets:
	dc.l	35*65*64,36*65*64,37*65*64		;blood & tables
	dc.l	38*65*64,38*65*64+32,38*65*64+[65*32],38*65*64+[65*32]+32

sv_AnimOffsets:
	dc.l	0,23*65*64,24*65*64
	dc.l	0,38*65*64+[65*32],38*65*64+[65*32]+32
	dc.l	0,29*65*64,29*65*64+[65*32],30*65*64,30*65*64+[65*32]
	dc.l	0,32*65*64,32*65*64+32,32*65*64+[65*32],32*65*64+[65*32]+32
	dc.l	0,34*65*64,34*65*64+32,34*65*64+[65*32],34*65*64+[65*32]+32

sv_Enemy1Offsets:
	dc.l	40*65*64,40*65*64+[65*32],41*65*64,41*65*64+[65*32]
	dc.l	42*65*64,42*65*64+[65*32],43*65*64,43*65*64+[65*32]
	dc.l	44*65*64,44*65*64+[65*32],45*65*64,45*65*64+[65*32]
	dc.l	46*65*64,46*65*64+[65*32],47*65*64,47*65*64+[65*32]
	dc.l	48*65*64,48*65*64+[65*32],49*65*64,49*65*64+[65*32]
	dc.l	50*65*64,50*65*64+[65*32],51*65*64,51*65*64+[65*32]
sv_Enemy2Offsets:
	dc.l	52*65*64+[65*32]
	dc.l	53*65*64,53*65*64+[65*32],54*65*64,54*65*64+[65*32]
	dc.l	55*65*64,55*65*64+[65*32],56*65*64,56*65*64+[65*32]
	dc.l	57*65*64,57*65*64+[65*32],58*65*64,58*65*64+[65*32]
	dc.l	59*65*64,59*65*64+[65*32],60*65*64,60*65*64+[65*32]
	dc.l	61*65*64,61*65*64+[65*32],62*65*64,62*65*64+[65*32]
	dc.l	63*65*64,63*65*64+[65*32],64*65*64

sv_EnemyDirTab:
	dc.w	52,56,52,56
	dc.w	44,40,48,40
	dc.w	32,36,32,36
	dc.w	4,0,8,0
	dc.w	12,16,12,16
	dc.w	24,20,28,20

	dc.l	0
	ENDOFF
end_offsets:

;-------------------------------------------------------------------
;---------------CONSTANTS:
row:		equ	40
heigth:		equ	200
mc_maxHeigth:	equ	350		;max wall heigth (max 350)
SHLeft:		equ	8		;2^SHLeft=D factor for
					;perspective (7 - optimal)
min_distance:	equ	[500*[2^SHLeft]/mc_maxHeigth]+2
					;min dist from screen
					;(min 96 for 6, 370 for 8)
max_distance:	equ	12000		;max distance (ok.22000)
sv_Upoffset:	equ	16		;view window pos offsets - in lines from top
sv_Leftoffset:	equ	8		; in bytes from left
wall_floor1:	equ	[13-1]*4	;nr. of floor wall *4
wall_floor2:	equ	[14-1]*4	;nr. of ceiling wall

;---------------CHIP LOCATIONS:
;sv_Offsets:	equ	BASEC+$2a000		;$300   - defined upper
sv_UserMap:	equ	BASEC+$2a300		;$200 (64*8)

sv_MAP:		equ	BASEC+$2b600		;$8000
sv_LEVELDATA:	equ	BASEC+$2b600+$8000
sv_EnemyDATA:	equ	BASEC+$2b600+$8000+40
sv_SwitchDATA:	equ	BASEC+$2b600+$8000+4136	;all $9400 (37800)

; samples. These are read together with tables in STD
sv_SAMPLES:	equ	BASEC+$34e58		;102108 ($18edc)

; tables (tables.dat)
sv_Bomba:	equ	BASEC+$4dd34		;$15cc (addr is $34e58 + $18edc)
sv_RotTable:	equ	BASEC+$4f300		;rot table ($41f2)
sv_ChaosTab:	equ	BASEC+$534f2		;$6c0;nie rozbijac bloku!
sv_ChaosTac:	equ	BASEC+$53bb2		;$800
sv_Numbers:	equ	BASEC+$543b2		;$1ae	- bitmap numbers to print nr of cards
sv_Fonts:	equ	BASEC+$54560		;$300
sv_sinus:	equ	BASEC+$54860		;$280
; end tables

mc_Htab:	equ	BASEC+$54b00		;$1ba0
sv_CompasClr:	equ	BASEC+$56770		;$6c
sv_CompasSav:	equ	BASEC+$567e0		;$21c
sv_DATA_AREA:	equ	BASEC+$56a00		;$600
sv_ConstTab:	equ	BASEC+$57000		;$46
sv_FloorTab:	equ	BASEC+$57050		;$8c for 140 lines, $80 for 128. Allocated $b0 =176 here.
sv_DeltaTab:	equ	BASEC+$57100		;$12c0 (600*4 *2 = 4800)
sv_ScrOffTab:	equ	BASEC+$58410		;$100
sv_TextOffsets:	equ	BASEC+$58520		;$100 (to 128 texts)
;RealCopper:	equ	BASEC+$58630		;$da0	defined upper!
sv_C1Save:	equ	BASEC+$593d0		;$21c
sv_C2Save:	equ	BASEC+$595f0		;$21c

sv_ZeroTab:	equ	BASEC+$59820		;[192*8]*14 $5400	- this stores data aboue transparent items which need to be added later

sv_ChunkyBufC:	equ	BASEC+$60a20		;max $6000	- chunky screen (192 width * 128 heigth). This is the chip buffer for blitter
									; TODO - move permanently to FAST if no blitter used

sv_WindowSav:	equ	BASEC+$66d30		;$28a0 (do $695d0)
sv_ItemBuf:	equ	BASEC+$69600		;$21c (do $634bc)
screen2:	equ	BASEC+$69d00		;160*40*5=$7d00
screen1:	equ	BASEC+$71a00		;($9c40 razem) - $7d00 main screen + bottom panel
; different areas on screen
sv_ScrollArea:	equ	screen1+$7d00+$1770+40
sv_Counter1:	equ	screen1+$7d00+$640+7
sv_Counter2:	equ	screen1+$7d00+$640+28
sv_Compas:	equ	screen1+$7d00+200+18
sv_Weapon:	equ	screen1+$7d00+200+23
sv_CardCnt:	equ	screen1+$7d00+600+39
sv_Heart:	equ	screen1+$7d00+$640+13
sv_BombPos:	equ	screen1+$7d00+$320
sv_DebugPos1:	equ	screen1+[20*40*5]		; debug counter positions for screens 1 and 2
sv_DebugPos2:	equ	screen2+[20*40*5]

sv_LineTab:	equ	BASEC+$7b800		;$15e0 (heigths) ($1900)
sc_colors:	equ	BASEC+$7d600		;$40 (64)
sc_Text:	equ	BASEC+$7d642		;$1b58 (7000)
ChangeCopper:	equ	BASEC+$7f1a0		;$1c
sv_ObjectTab:	equ	BASEC+$7f1d0		;$168 (30 cells)
sv_HeartSav:	equ	BASEC+$7f340		;$12c
sv_CardSav:	equ	BASEC+$7f470		;$78
sv_ItemSav:	equ	BASEC+$7f4f0		;$21c

;---------------OFFSETS in FAST1:
basef_data: equ	$f570			; start of data in fast memory, following code
fl_Floors:	equ	basef_data		; len 30*192 = 5760 ($1680) + 2 for RTD. END: $10bf2
co_Walls:	equ	$10c00			;[320*4*65]*4=$14500 * 4
co_Items:	equ	co_Walls+270400	; $52c40. Length 28080 = $6db0. End: $599f0
sv_PLANES:	equ	$59e40			;$b1bc(700*[260/4]). End: $64ffc
mc_code:	equ	$65000			;$1af00 - raster code. End $7ff00

; 83200+83200+52000+52000		; all: 270400 = $42040

; $14500 + $14500 + $19640 + $71c0 = $49200
; wallsA   wallsB   enemy    items

;-------------------------------------------------------------------
Oryginal_Data:

	OFFSET	sv_DATA_AREA			; size is about $300


;---------------PREFERENCES:
screenMaxX:	equ		192		; in pixels
screenMaxY:	equ		128		; in pixels


sv_Size:	dc.w	6,6		;actual size value for calculating pixel width: 2-6, user selected value including stretch options 2-9
;sv_Size:	dc.w	3,3		;math value, real
sv_Floor:	dc.w	1		;0-off,  1-floors on
;sv_Mode:	dc.w	0		;0-fast, 1-slow (cache)
sv_Buse:	dc.w	0,0		;0-use , 1-not use blitter
sv_DETAILS:	dc.w	0		;2-low,	 1-medium,  0-high
sv_DIFFICULT:	dc.w	0		;0-difficult(normal), 1-easy

sv_ViewWidth:	dc.w	screenMaxX/8			;view window dims in bytes - max 192 pixels = 24 bytes
sv_ViewHeigth:	dc.w	screenMaxY				;24/128 - maximum
sv_WallHeigth:	dc.w	450			;max 500 - percent

sv_FillCols:	dc.l	$10101010,$e0e0e0e0	;background filling for ceiling and floor

;---------------DATA AREA:
sv_ChunkyBuffer:	dc.l	0		; SVGA (chunky) screen - by default in chip

sv_StrFlag:	dc.w	0,0			;0-no stretch, 1-stretch
sv_MapPos:	dc.w	0			;user offset on map
sv_SzumTime:	dc.w	0
sc_TextAddr:	dc.l	0,0,0			;adr 1,2,flag
sv_pause:	dc.w	0,0			;1- pause, 0- no,text CNT
do_FLASH:	dc.w	0			;nr of frames to flash
sv_ZeroPtr:	dc.l	sv_ZeroTab
sv_screen:	dc.l	screen1,screen2	;160*40*5=$7d00 each
sv_offset:	dc.l	[sv_Upoffset*row*5]+sv_Leftoffset		;view start
sv_BumpedWall:	dc.w	0			;nr. of bumped wall
sv_ChaosAddr:	dc.w	[32*27*2]-2,0		; offset in randomised table used e.g. for weapon change drawing. 
sv_AmmoChg:	dc.w	0,0,0,0			;old/cnt, new/cnt
sv_Flag:	dc.w	1,0,0,0,0		;different flags: ?, regeneration speed (0 norm, 1 slow)

sv_Teleport:	dc.w	1			;1-teleport on at start to nicely show screen
sv_CollumnWid:	dc.w	0			;width (16 or 32)
sv_Length:	dc.l	0			;length of read file
sv_HitFlag:	dc.w	0
sv_WalkState:	dc.w	0			;0,2,4,6 - enemy walk
sv_NtscPal:	dc.w	32,0
sv_MovSav:	dc.w	0,0
sv_SecondEnemy:	dc.w	0
sv_AddMove:	dc.w	0,0			;x,y external add
ab_BloodAdr:	dc.l	0,0,0
eh_FirePos:	dc.l	0,0
sv_MapOn:	dc.w	0			;0-off, 1-on
sv_OldCop:	dc.l	0,0,0
sv_EndLevel:	dc.w	0,4			;1 - end of lev, -1 death
sv_SpaceOn:	dc.w	0			;1 - space pressed (hand)
do_pikaj:	dc.w	0,50
do_bron:	dc.w	0
do_JakiKoniec:	dc.w	0			;+ good, - bad
sv_Opoznienie:	dc.w	0			; frames to remain idle on faster machines - WTF!!!!
db_napisz:	dc.w	0

sv_DoorFlag1:	dc.w	5, -1,0, 0,0, 0,0, 0,0, 0;flag,offset01,02..32,-1
		dc.w	0,0			;+20 in_use, prior in_use
		dc.w	0,-1,0,0,0,0,0,0,0,0	;+24 - Prior_Table
		dc.w	0			;+44 - sound flag
sv_DoorFlag2:	dc.w	6, -1,0, 0,0, 0,0, 0,0, 0;for door 2
		dc.w	0,0			;+20
		dc.w	0,-1,0,0,0,0,0,0,0,0	;+24
		dc.w	0			;+44

sv_oldmouse:	dc.w	0
sv_MouseDxy:	dc.w	0,0
sv_LastMove:	dc.w	0,0,0,0			;vec len, angle, bump l,a
sv_LastPos:	dc.w	0,0			;last X,Y pos
cc_MoveTab:	dc.w	0,0,0,0,0,0,0		;(boolean) key pressed: ;0 up, 2 dn, 4 turn_left, 6 turn_right, 8 left, 10 right, 12 fire

cc_RequestTab:	dc.w	0,6,0,0,0		; 5*2 = 10 bytes
;0 quit, 2 sv_Size, 4 blit_use, 6 RMB_flag, 8 Rf2
		dc.w	0,0,0,0,0,0,0, 0,0,0,0,0,0,0	;key pressed - 14 keys

oc_HitPos:	dc.w	0,0,0,0, 0,0,0,0	;flag,Xpos,Ypos,Offset

play_sample:	dc.b	0,0,0,0
play_volume:	dc.b	63,63,63,63
sv_TextBuffer:	ds.b	20			;text for code

sv_ITEMS:	;list of items: item, add ammo, ammo
		dc.w	1	;which chosen (hand=0)
		dc.w	1111	;changable counter - displayed AMMO
		dc.w	30671,0,31111		; hand: present, power, ammo

sv_GUNS:	
		dc.w	0,-10,0			;handgun (dmg/present, power, ammo)
		dc.w	0,-5,0			;shotgun
		dc.w	0,-15,0			;machinegun
		dc.w	0,-5,0			;flamer
		dc.w	0,-5,0			;blaster
		dc.w	0,-1,0			;launcher

sv_Cards:	
		dc.w	$9de0,0,0		;7*6	;cards - R,G,B
		dc.w	$0b4a,0,0		;8*6
		dc.w	$73e1,0,0		;9*6

sv_ENERGY:	dc.w	-225,0			;changable CNT, real CNT
sv_NrKilled:	dc.w	0
sv_Glowica:	dc.w	0			;zmiana co -2 (do -12)

;---------------POSITION:
sv_PosX:	dc.w	512
sv_PosY:	dc.w	512
sv_angle:	dc.w	90
;sv_WalkSpeed:	dc.w	0,250,0,0		;(up to 300),val,buf,CNT
sv_WalkSpeed:	dc.w	0,50,0,0		;(up to 300),val,buf,CNT
sv_RotSpeed:	dc.w	12				; nominal for old versions is 12 but it's scaled to fps

sv_SquarePos:	dc.w	0,0			;x,y of 1024-square
sv_InSquarePos:	dc.w	0,0			;x,y in square

;---------------OTHER TABLES:

GodModeCode:	dc.b	-$b5,-$cf,-$d7,-$b1,-$d1,-$dd,-$d1,0	; hotkiwi - god mode
EnergyCode:		dc.b	-$b5,-$cf,-$d7,-$91,-$bf,-$91,-$bf,0 	; hotmama
AmmoCode:		dc.b	-$b5,-$cf,-$d7,-$b1,-$d1,-$bd,-$bd,0	; hotkiss
CardCode:		dc.b	-$b5,-$cf,-$d7,-$cd,-$cf,-$cd,-$bd,0	; hotkpops
WallCode:		dc.b	-$d1,-$bb,-$bf,-$b5,-$cf,0				; idaho
MapCode:		dc.b	-$af,-$cf,-$d9,-$d1,-$db,-$93,0			; lorien
LevelCode:		dc.b	-$b1,-$d1,-$d7,-$d1,-$bf,-$d9,-$bf,0 	; kitiara
BombCode:		dc.b	-$bf,-$af,-$d1,-$95,-$bf,-$95,-$bf,0 	; alibaba
even

				dc.l	0,$01020304
	ENDOFF

End_OData:

; --------- STRUCTURE offsets -------------
STR_DISTURBANCES:	equ	52				; byte, on-creen disturbaces, 0=on, 1=off
STR_GODMODE:		equ	53				; byte, god mode 0=off, 1=on (hex $35 in Structure)
STR_CROSSHAIRS:		equ	54				; 0=on, 1=off

; --------- FAST 2 offsets -------------
AVCNTS:		equ		16					; number of average counters

RSRESET
F2_Empty:			rs.b	16							; this is for any temp data
F2_AvLastVal:		rs.l	AVCNTS						; last read start_value for debug counters (l)
F2_AvPtrs:			rs.w	AVCNTS						; rolling pointers into 16 debug counters (averages). Next free slot.w for each
F2_AvData:			rs.w	AVCNTS*8					; space for debug data (averages). buffer for 8*data.w (each is 16 bytes)
F2_ClearTo1:		rs.w	0							; clear up to here on start
F2_ChunkyBufF:		rs.b	screenMaxX*screenMaxY		; chunky buffer ($6000 for 192x128)

F2_TopMem:			rs.w	0

end:

;PRINTT	"EXTENDED FAST 2 USED"
;PRINTV  F2_TopMem

IF [end-s+CODESTART]>basef_data
PRINTT	"CODE TOO LONG!"
PRINTV  end-s, basef_data-CODESTART
ENDC

IFNE IS_EXE
PRINTT	"EXE version - only to be used to create the final binary: wb <cyt.dat> s end"
ENDC

;-------------------------------------------------------------------
;-------------------------------------------------------------------

>extern	"Assets/W1A",BASEF1+co_Walls,-1
>extern	"Assets/W1B",BASEF1+co_Walls+83200,-1
>extern	"Assets/E1A",BASEF1+co_Walls+83200+83200,-1
>extern	"Assets/E1B",BASEF1+co_Walls+83200+83200+52000,-1
>extern	"Assets/M1A",sv_MAP,-1
>extern	"Assets/C1A",sc_colors,-1

>extern	"Assets/ITEMS01.VIR",BASEF1+co_Walls+270400
>extern	"Assets/WINDOW1.RAW",screen1,-1
>extern	"Assets/TABLES.DAT",sv_bomba,-1			; chaostab, fonts etc.
>extern	"Assets/sounds",sv_samples,-1
>extern	"Assets/TEXT.ENG",sc_text,-1

;-------------------------------------------------------------------

