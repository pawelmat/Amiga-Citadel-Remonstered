;-----------------------------------------------------------------------------

		INCDIR	Includes:
		INCLUDE	whdload.i
		INCLUDE	whdmacros.i

		IFD BARFLY
		OUTPUT	"c.slave"
		BOPT	O+
		BOPT	OG+			;enable optimizing
		BOPT	ODd-
		BOPT	ODe-			;disable mul optimizing
		BOPT	w4-			;disable 64k warnings
		BOPT	wo-
		SUPER				;disable supervisor warnings
		ENDC


CITADEL_VBR		equ	$7ffee
CITADEL_PROCESSOR	equ	$7fff2


CITADEL_LOADER		equ	$7d000
;-----------------------------------------------------------------------------
;
;; macro: used to WHDLoad slaves
;
; \1 - name of routine without prefix ( resload_ )
;
WHDL	MACRO
		move.l	_resload(pc),a2
		jsr	resload_\1(a2)
	ENDM

;-----------------------------------------------------------------------------

_base		SLAVE_HEADER			;ws_Security + ws_ID
		dc.w	17			;ws_Version
		dc.w	WHDLF_NoError|WHDLF_EmulTrap|WHDLF_ClearMem
		dc.l	$80000			;ws_BaseMemSize
		dc.l	0			;ws_ExecInstall
		dc.w	_Start-_base		;ws_GameLoader
		dc.w	0			;ws_CurrentDir
		dc.w	0			;ws_DontCache
_keydebug	dc.b	0			;ws_keydebug
_keyexit	dc.b	0			;ws_keyexit = F10
_expmem		dc.l	_expmem_size		;ws_ExpMem
		dc.w	_name-_base		;ws_name
		dc.w	_copy-_base		;ws_copy
		dc.w	_info-_base		;ws_info
		dc.w	0			;ws_kickname
		dc.l	0			;ws_kicksize
		dc.w	0			;ws_kickcrc
		dc.w	slv_config-_base	;ws_config

_name		dc.b	"Citadel (Remonstered)",0
_copy		dc.b	"1995, 2022 VD",0 
_info		
		dc.b	"Adapted by Asman",10,10
		dc.b	"Previous version installed & fixed:",10
		dc.b	"Psygore, Boread Seal, JOTD, Wepl",10,10

		dc.b	"V1.3 "
	DOSCMD	"WDate  >T:date"
	INCBIN	"T:date"
		dc.b	0

slv_config	dc.b	"C1:B:Enable Unlimited Energy",0
		EVEN

_expmem_size	=	$80000*2+$1000
_usp_size	=	$400

_mem_cache	=	WCPUF_Base_NC|WCPUF_Exp_CB|WCPUF_Slave_CB
_cpu_cache	=	WCPUF_IC|WCPUF_SS|WCPUF_BC|WCPUF_SB|WCPUF_DC

;-----------------------------------------------------------------------------

_Start



	;save resload for later use
		lea	_resload(pc),a1
		move.l	a0,(a1)


		lea	_tags(pc),a0
		WHDL	Control

	;--- sp & usp to fastmem ---

		move.l	_expmem(pc),a0
		add.l	#_expmem_size-$200,a0
		move.l	a0,sp

		lea	-_usp_size(a0),a0
		move.l	a0,usp

	;--- set caches---
		move.l	#_mem_cache|_cpu_cache,d0
		move.l	#WCPUF_All,d1
		WHDL	SetCPU

		moveq	#2,d1
		moveq	#$14,d2
		lea	CITADEL_LOADER,a0
		bsr	LoadRNCTracks

		move.l	d2,d0
		mulu.w	#$200,d0
		WHDL	CRC16
		cmp.w	#$647d,d0		;Black Legend english release
		beq	Euro

	;unsupported version
		pea	TDREASON_WRONGVER
Quit		move.l	_resload(pc),-(a7)
		add.l	#resload_Abort,(a7)
		rts

Exit		pea	TDREASON_OK.w
		bra	Quit 

Euro

;--- processors ---


;.lmb		btst	#6,$bfe001
;		bne	.lmb

		moveq	#0,d1

		move.l	flags(pc),d0
		tst.b	d0		
		beq	_68000

		moveq	#1,d1
		move.w	d1,$7d000+$38		; >68000 processors
		

_68000
		move.w	d1,CITADEL_PROCESSOR

		btst	#AFB_68020,d0
		beq	.fpu

		move.l	#'DETE',$7d596
		move.l	#'CTED',$7d59a


.fpu		moveq	#16+32+64,d1
		and.w	d0,d1
		beq	.go

		move.l	#'DETE',$7d5b6
		move.l	#'CTED',$7d5ba


.go

		bsr	_SetupKeyboard

		lea	CITADEL_LOADER,a1
		lea	patchLoader(pc),a0
		WHDL	Patch



		move.l	_expmem,d1
		move.l	d1,d2
		add.l	#$80000,d2

		bsr	FlushCache

		lea	CITADEL_LOADER,a0


		jmp	4(a0)


;-----------------------------------------------------------------------------
patchLoader:
		PL_START

		PL_P	$87c,FixLoadTrack

		PL_P	$382,FixPatchIntro

		PL_W	$2e8,$c028		;remove early keyboard init

		PL_W	$82,$6002		;skip exec.supervisor

		PL_PS	$3e,GetFlags

		PL_NOP	$114,8			;remove sp,usp relocation

		PL_PSS	$336,fixWaitLoader,2

		PL_L	CITADEL_VBR-CITADEL_LOADER,0	;vbr = 0


		PL_END

fixWaitLoader:
		move.l	a0,-(sp)

.loop		btst	#6,$bfe001
		beq	.exit
		btst	#7,$bfe001
		beq	.exit
		
		lea	_keycode(pc),a0
		cmp.b	#$45,(a0)		;KEY_ESC
		beq	.exit

		cmp.b	#$40,(a0)		;KEY_SPACE
		beq	.exit

		cmp.b	#$44,(a0)		;KEY_ENTER
		beq	.exit

		cmp.b	#$4c,(a0)		;KEY_CRSR_UP
		beq	.exit
		cmp.b	#$4d,(a0)		;KEY_CRSR_DOWN
		beq	.exit
		cmp.b	#$4f,(a0)		;KEY_CRSR_LEFT
		beq	.exit
		cmp.b	#$4e,(a0)		;KEY_CRSR_RIGHT
		beq	.exit

		tst.w	$7d4da
		bpl	.loop

.exit		move.l	(sp)+,a0
		rts

;-----------------------------------------------------------------------------

FixPatchIntro:

		lea	$3f000,a1
		lea	_patchIntro,a0
		WHDL	Patch
		bsr	FlushCache
		jmp	$3f000

_patchIntro
		PL_START

		PL_P	$19f8,FixLoadTrack
		PL_P	$1ec,FixPatchMain

		PL_W	$170,$6016		;fix - credits can be skipped with LMB


		PL_W	$94,$c028		;allow keyboard int

	;add possibility to skip with joy button
		PL_PSS	$188,FixIntroJoyButton,2	
	
		PL_W	$8,$6004		;skip cacr operation

		PL_END

;-----------------------------------------------------------------------------

FixIntroJoyButton:
		move.l	a0,-(sp)

		btst	#6,$bfe001
		beq.b	.exit

		btst	#7,$bfe001
		beq.b	.exit

		lea	_keycode(pc),a0
		cmp.b	#$45,(a0)		;KEY_ESC
		beq	.exit

		cmp.b	#$40,(a0)		;KEY_SPACE
		beq	.exit

		cmp.b	#$44,(a0)		;KEY_ENTER
		beq	.exit

		cmp.b	#$4c,(a0)		;KEY_CRSR_UP
		beq	.exit
		cmp.b	#$4d,(a0)		;KEY_CRSR_DOWN
		beq	.exit
		cmp.b	#$4f,(a0)		;KEY_CRSR_LEFT
		beq	.exit
		cmp.b	#$4e,(a0)		;KEY_CRSR_RIGHT

.exit		move.b	#$ff,(a0)

		move.l	(sp)+,a0
		rts


;-----------------------------------------------------------------------------

FixPatchMain


		lea	$3ab00,a1
		lea	_patchRncTrack(pc),a0
		WHDL	Patch

		lea	$69000,a1
		lea	_patchMain(pc),a0
		WHDL	Patch

		bsr	FlushCache
		jmp	$69000


_patchRncTrack
		PL_START
		PL_P	$0,LoadRNCTracks
		PL_END

_patchMain:
		PL_START

		PL_P	$2bb4,FixLoadTrack
		PL_P	$2ade,Insert
		PL_P	$938,FixPatchServer
		
		PL_PS	$818,Crack		;remove manual protection
		PL_PS	$c46,PatchKbDelay	;patch keyboard


		PL_PS	$bee,FixStoreKeyInInt

		PL_NOP	$c4c,8

		PL_PSS	$bb0,FixMainMouseButton,22


		PL_P	$7a8,FixTitleWait


		PL_END

FixTitleWait
		move.l	a0,-(sp)

		btst	#6,$bfe001
		beq.b	.press

		btst	#7,$bfe001
		beq.b	.press

		lea	_keycode(pc),a0
		cmp.b	#$45,(a0)		;KEY_ESC
		beq	.press

		cmp.b	#$40,(a0)		;KEY_SPACE
		beq	.press

		cmp.b	#$44,(a0)		;KEY_ENTER
		beq	.press

		cmp.b	#$4c,(a0)		;KEY_CRSR_UP
		beq	.press
		cmp.b	#$4d,(a0)		;KEY_CRSR_DOWN
		beq	.press
		cmp.b	#$4f,(a0)		;KEY_CRSR_LEFT
		beq	.press
		cmp.b	#$4e,(a0)		;KEY_CRSR_RIGHT
		beq	.press

.exit		move.l	(sp)+,a0
		jmp	$697b2

.press		move.l	(sp)+,a0
		jmp	$697b6
		




		


FixStoreKeyInInt:
		move.b	$bfec01,d0
		move.w	d0,-(sp)

		not.b	d0
		ror.b	#1,d0

		move.l	a0,-(sp)
		lea	_keycode(pc),a0
		move.b	d0,(a0)
		move.l	(sp)+,a0

		move.w	(sp)+,d0
		rts

FixMainMouseButton:
		move.l	a0,-(sp)

		btst	#6,$bfe001
		beq.b	.press

		btst	#7,$bfe001
		beq.b	.press

		lea	_keycode(pc),a0
		cmp.b	#$45,(a0)		;KEY_ESC
		beq	.press

		cmp.b	#$40,(a0)		;KEY_SPACE
		beq	.press

		cmp.b	#$44,(a0)		;KEY_ENTER
		beq	.press

		cmp.b	#$4c,(a0)		;KEY_CRSR_UP
		beq	.press
		cmp.b	#$4d,(a0)		;KEY_CRSR_DOWN
		beq	.press
		cmp.b	#$4f,(a0)		;KEY_CRSR_LEFT
		beq	.press
		cmp.b	#$4e,(a0)		;KEY_CRSR_RIGHT
		beq	.press

.exit		move.l	(sp)+,a0
		rts

.press		move.w	#$1,$69bdc
		bra.b	.exit


;-----------------------------------------------------------------------------

FixPatchServer:

		lea	$5100,a1
		lea	_patchServer(pc),a0
		WHDL	Patch

;.lmb		btst	#6,$bfe001
;		bne	.lmb


		lea	$5100,a1
		bsr	FixPatch68000Exit

		bsr	FlushCache
		jmp	$5100

_patchServer:
		PL_START

		PL_PS	$7a92-$5100,Crack
		PL_PS	$852c-$5100,Crack

		PL_P	$9904-$5100,LoadRNCTracks
		PL_P	$1999e-$5100,LoadRNCTracks

		PL_P	$95b2-$5100,Insert

		PL_B	$57dd-$5100,$20		;' '
		PL_W	$57de-$5100,$4d41	;'MA;
		PL_B	$57e0-$5100,$58		;'X'

		PL_L	$609a-$5100,$204d4158	;' MAX'
		PL_W	$57ca-$5100,$4e20	;' N'

		PL_NOP	$5174-$5100,2	;remove code checksums
		PL_NOP	$80f6-$5100,2
		PL_B	$60d8-$5100,$60

		PL_PS	$5a64-$5100,PatchKbDelay
		PL_PS	$765c-$5100,IntBug

		PL_W	$5e1a-$5100,$0000		;offset changed from $6000 to $1000
		PL_PS	$5e1e-$5100,PatchEngine

		PL_W	$7c26-$5100,$0000		;offset changed from $6000 to $1000
		PL_PS	$7c2a-$5100,PatchEngine

		PL_W	$69818-$5100,$6100	;remove manual protection
		PL_L	$6981a-$5100,$e424e92

		PL_END

;hack the game to think you collected all six bomb parts so you can enter Centre
;		move.w	#$33fc,$63be		;put right values
;		move.w	#$337c,$63ce
;		move.w	#$337c,$63ec
;		move.b	#$60,$63d4		;don't check if bomb is complete
;		move.w	#$4e71,$63c6
;		move.w	#$4e71,$63f2


;-----------------------------------------------------------------------------

PatchEngine
		;in a1 - addr
		;   a2 - copy of addr
		;;jsr	$8144		;decrunch



		movem.l	d0-a2,-(sp)

		lea	nameEngine(pc),a0
		move.l	(_resload,pc),a2
		jsr	(resload_LoadFile,a2)


		lea	trainer(pc),a0
		tst.l	(a0)
		beq	.no

		move.b #1,$7f835

.no		movem.l	(sp)+,d0-a2


		bsr	FixPatch68000Exit


		bsr	FlushCache
		jmp	(a2)
;-----------------------------------------------------------------------------
;
; a1 - start address
FixPatch68000Exit:
		movem.l	d0/d1/a1/a3,-(sp)
		move.l	flags(pc),d0
		tst.b	d0
		bne	.exit

	;a1 - address
	;find out 'move.b $bfec01'
		moveq	#-1,d0
		move.l	a1,a3
		move.l	#$103900bf,d1
.loop
		cmp.l	(a3),d1
		beq.b	.found
		addq.l	#2,a3
		dbf	d0,.loop

.found		cmp.w	#$ec01,4(a3)
		bne	.exit

		move.w	#$4eb9,(a3)+
		lea	FixEngineExit(pc),a1
		move.l	a1,(a3)

.exit		movem.l	(sp)+,d0/d1/a1/a3
		rts


FixEngineExit
		move.b	$bfec01,d0
		move.w	d0,-(sp)

	;calculate rawkeycode
		not.b	d0
		ror.b	#1,d0

		cmp.b	_keyexit(pc),d0
		beq	Exit

		move.w	(sp)+,d0
		rts


;-----------------------------------------------------------------------------

IntBug		lea	$dff000,a0
		move.w	10(a0),d0
		rts

;-----------------------------------------------------------------------------

Insert		move.l	a0,-(sp)		;d3=requested disk
		lea	disknum(pc),a0
		move.b	d3,(a0)
		move.l	(sp)+,a0

		movem.l	d1-d2,-(sp)		;read ID track, necessary!
		moveq	#0,d1
		moveq	#11,d2
		bsr	LoadRNCTracks
		movem.l	(sp)+,d1-d2
		rts

;-----------------------------------------------------------------------------

FixLoadTrack:
		movem.l a0-a2/d0-d3,-(sp)
		mulu.w	#$1600,d0
		mulu.w	#$1600,d1
		move.b	disknum(pc),d2
		WHDL	DiskLoad
		movem.l (sp)+,a0-a2/d0-d3
		clr.l	d0
		rts

;-----------------------------------------------------------------------------

LoadRNCTracks	movem.l a0-a2/d0-d3,-(sp)
		tst.w	d2			;skip empty disk check
		beq	.exit
		and.l	#$0000FFFF,d1
		and.l	#$0000FFFF,d2
		mulu.w	#$200,d1		;get offset
		mulu.w	#$200,d2		;get size
		cmp.w	#$8001,d3		;save or load ?
		beq	.save
		move.l	d1,d0
		move.l	d2,d1
		lea	disknum,a2
		move.b	(a2),d2
		move.l	(_resload,pc),a2
		jsr	(resload_DiskLoad,a2)
.exit		movem.l (sp)+,a0-a2/d0-d3
		clr.l	d0
		rts

.save		move.l	d2,d0
		move.l	a0,a1
		lea	savedisk,a0
		WHDL	SaveFileOffset
		bra	.exit

;-----------------------------------------------------------------------------

Crack		move.l	#$CDBFCDBF,d0
		move.l	#$b1000004,d1
		move.l	#$ad100,d2
		rts

;-----------------------------------------------------------------------------

GetFlags	move.l	a0,-(sp)
		lea	flags,a0
		move.l	(a0),d0
		move.w	d0,d1
		move.l	(sp)+,a0
		rts

;-----------------------------------------------------------------------------

FlushCache	move.l	a2,-(sp)
		WHDL	FlushCache
		move.l	(sp)+,a2
		rts

;-----------------------------------------------------------------------------

PatchKbDelay	move.l	d0,-(sp)
		moveq	#2,d0
loop1		move.l	d0,-(sp)
		move.b	$dff006,d0
loop2		cmp.b	$dff006,d0
		beq	loop2
		move.l	(sp)+,d0
		dbf	d0,loop1
		move.l	(sp)+,d0
		rts

;-----------------------------------------------------------------------------
_keycode:	dc.b	0
	EVEN 

		INCLUDE	'sources:WHDLoad/keyboard.s'  

;-----------------------------------------------------------------------------
_resload	dc.l	0
_tags		dc.l	WHDLTAG_ATTNFLAGS_GET
flags		dc.l	0
		dc.l	WHDLTAG_CUSTOM1_GET
trainer		dc.l	0
		dc.l	0


savedisk	dc.b	"Disk.4",0
disknum		dc.b	1

nameEngine:	dc.b	"CYT.DAT",0
