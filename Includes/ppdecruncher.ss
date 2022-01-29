;-----------------------------------------------------------------------
****************************************************
*** PowerPacker 2.0 FAST decrunch routine (v1.5) ***
*** Resourced by BC/LUZERS			 ***

;IFND	PPDECRUNCHER_S
;PPDECRUNCHER_S:	EQU	1

;a0.l - crunched data
;a1.l - output buffer (can overlap crunched - decrunching is done from the end)
;d0.l - length

	xdef	PPDecrunch
PPDecrunch:
	cmpi.l	#$50503230,(a0)		; check for PP20
	beq		PPDoDecrunch

	movem.l	d0-d7/a2-a6,-(sp)
	tst.l	d0
	beq		.PPocExit
	cmp.l	a0,a1
	beq		.PPocExit

	lea		(a0,d0.l),a0			; if file not packed copy it to the target location (starting from end)
	lea		(a1,d0.l),a1

	move	d0,d1
	lsr.l		#2,d0
	beq		.POocRem
.PPOnlyCopy:
	move.l	-(a0),-(a1)
	dbf		d0,.PPOnlyCopy
.POocRem:
	andi	#3,d1
	beq		.PPocExit
.POocRem2:
	move.b	-(a0),-(a1)
	dbf		d1,.POocRem2

.PPocExit:	
	movem.l	(sp)+,d0-d7/a2-a6
	rts

	xdef	PPDoDecrunch
PPDoDecrunch:
	movem.l	d1-d7/a2-a6,-(sp)
;	move.l	(a0),d0			;dlugosc
	lea	4(a0),a2
	add.l	d0,a0
	lea	l0494(pc),a5
	moveq	#$18,d6
	moveq	#0,d4
	move.w	#$00FF,d7
	moveq	#1,d5
	move.l	a1,a4
	move.l	-(a0),d1
	tst.b	d1
	beq.s	l0266
	lsr.l	#1,d5
	beq.s	l02A2
l0262:	subq.b	#1,d1
	lsr.l	d1,d5
l0266:	lsr.l	#8,d1
	add.l	d1,a1
l026A:	lsr.l	#1,d5
	beq.s	l02A8
l026E:	bcs	l0310
	moveq	#0,d2
l0274:	moveq	#0,d1
	lsr.l	#1,d5
	beq.s	l02AE
l027A:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l02B4
l0280:	roxl.w	#1,d1
	add.w	d1,d2
	subq.w	#3,d1
	beq.s	l0274
	moveq	#0,d0
l028A:	move.b	d5,d4
	lsr.l	#8,d5
	beq.s	l02C6
l0290:	move.b	-$0080(a5,d4.w),d0
	move.b	d0,-(a1)
	dbra	d2,l028A

	cmp.l	a1,a4
	bcs.s	l0310
	bra	l03F0

l02A2:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0262

l02A8:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l026E

l02AE:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l027A

l02B4:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0280

l02BA:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0316

l02C0:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l031C

l02C6:	move.b	$007F(a5,d4.w),d0
	move.l	-(a0),d5
	move.w	d5,d3
	lsl.w	d0,d3
	bchg	d0,d3
	eor.w	d3,d4
	and.w	d7,d4
	moveq	#8,d1
	sub.w	d0,d1
	lsr.l	d1,d5
	add.w	d6,d0
	bset	d0,d5
	bra.s	l0290

l02E2:	move.b	$007F(a5,d4.w),d0
	move.l	-(a0),d5
	move.w	d5,d3
	lsl.w	d0,d3
	bchg	d0,d3
	eor.w	d3,d4
	and.w	d7,d4
	moveq	#8,d1
	sub.w	d0,d1
	lsr.l	d1,d5
	add.w	d6,d0
	bset	d0,d5
	bra.s	l0324

l02FE:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l035E

l0304:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0364

l030A:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l036A

l0310:	moveq	#0,d2
	lsr.l	#1,d5
	beq.s	l02BA
l0316:	roxl.w	#1,d2
	lsr.l	#1,d5
	beq.s	l02C0
l031C:	roxl.w	#1,d2
	move.b	d5,d4
	lsr.l	#8,d5
	beq.s	l02E2
l0324:	moveq	#0,d3
	move.b	-$0080(a5,d4.w),d3
	cmp.w	#3,d2
	bne.s	l03AC
	bclr	#7,d3
	beq.s	l037E
	moveq	#13,d0
	sub.b	0(a2,d2.w),d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	add.w	d0,d0
	jmp	l035A(pc,d0.w)

l0348:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0370

l034E:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0376

l0354:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l037C

l035A:	lsr.l	#1,d5
	beq.s	l02FE
l035E:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0304
l0364:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l030A
l036A:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0348
l0370:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l034E
l0376:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0354
l037C:	roxl.w	#1,d3
l037E:	moveq	#0,d1
	lsr.l	#1,d5
	beq.s	l039A
l0384:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l03A0
l038A:	roxl.w	#1,d1
	lsr.l	#1,d5
	beq.s	l03A6
l0390:	roxl.w	#1,d1
	add.w	d1,d2
	subq.w	#7,d1
	beq.s	l037E
	bra.s	l03DC

l039A:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0384

l03A0:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l038A

l03A6:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l0390

l03AC:	moveq	#13,d0
	sub.b	0(a2,d2.w),d0
	move.w	d0,d1
	add.w	d0,d0
	add.w	d1,d0
	add.w	d0,d0
	jmp	l03BE(pc,d0.w)

l03BE:	lsr.l	#1,d5
	beq.s	l03F6
l03C2:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l03FC
l03C8:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0402
l03CE:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l0408
l03D4:	roxl.w	#1,d3
	lsr.l	#1,d5
	beq.s	l040E
l03DA:	roxl.w	#1,d3
l03DC:	move.b	0(a1,d3.w),-(a1)
l03E0:	move.b	0(a1,d3.w),-(a1)
	dbra	d2,l03E0

	cmp.l	a1,a4
	bcs	l026A
l03F0:	movem.l	(sp)+,d1-d7/a2-a6
	rts

l03F6:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03C2

l03FC:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03C8

l0402:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03CE

l0408:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03D4

l040E:	move.l	-(a0),d5
	roxr.l	#1,d5
	bra.s	l03DA

	or.l	#$40C020A0,d0
	bra.s	l03FC

	dc.l	$109050D0,$30B070F0,$088848C8,$28A868E8,$189858D8
	dc.l	$38B878F8,$048444C4,$24A464E4,$149454D4,$34B474F4
	dc.l	$0C8C4CCC,$2CAC6CEC,$1C9C5CDC,$3CBC7CFC,$028242C2
	dc.l	$22A262E2,$129252D2,$32B272F2,$0A8A4ACA,$2AAA6AEA
	dc.l	$1A9A5ADA,$3ABA7AFA,$068646C6,$26A666E6,$169656D6
	dc.l	$36B676F6,$0E8E4ECE,$2EAE6EEE,$1E9E5EDE,$3EBE7EFE
l0494:	dc.l	$018141C1,$21A161E1,$119151D1,$31B171F1,$098949C9
	dc.l	$29A969E9,$199959D9,$39B979F9,$058545C5,$25A565E5
	dc.l	$159555D5,$35B575F5,$0D8D4DCD,$2DAD6DED,$1D9D5DDD
	dc.l	$3DBD7DFD,$038343C3,$23A363E3,$139353D3,$33B373F3
	dc.l	$0B8B4BCB,$2BAB6BEB,$1B9B5BDB,$3BBB7BFB,$078747C7
	dc.l	$27A767E7,$179757D7,$37B777F7,$0F8F4FCF,$2FAF6FEF
	dc.l	$1F9F5FDF,$3FBF7FFF,$00010102,$02020203,$03030303
	dc.l	$03030304,$04040404,$04040404,$04040404,$04040405
	dc.l	$05050505,$05050505,$05050505,$05050505,$05050505
	dc.l	$05050505,$05050505,$05050506,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060606,$06060606
	dc.l	$06060606,$06060606,$06060606,$06060607,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070707,$07070707,$07070707,$07070707,$07070707
	dc.l	$07070700
even
;-----------------------------------------------------------------------

;ENDC
