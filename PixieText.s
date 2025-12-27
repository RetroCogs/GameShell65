.segment Zeropage "Pixie Text"
TextPosX:		.byte $00,$00
TextPosY:		.byte $00
TextPtr:		.word $0000
TextOffs:		.byte $00
TextEffect:		.byte $00

.segment Code "Pixie Text"

.macro TextSetPos(x,y) 
{
	lda #<x
	sta.zp TextPosX+0
	lda #>x
	sta.zp TextPosX+1
	lda #y
	sta.zp TextPosY
}

.macro TextSetMsgPtr(message) 
{
    lda #<message
    sta TextPtr+0
    lda #>message
    sta TextPtr+1
}

.macro TextDrawSpriteMsg(center, sinoffs, applysin) 
{
	.if (applysin)
	{
	    clc
	    lda Irq.VBlankCount
	    adc #sinoffs
	    sta TextOffs
	    lda #$01
	    sta TextEffect
	}
	else
	{
		lda #$00
	    sta TextOffs
	    sta TextEffect
	}

	.if(center)
	{
		jsr SprCenterXPos
	}

    jsr SprPrintMsg
}

// ----------------------------------------------------------------------------
//

chrWide:
	.byte $10,$10,$10,$0f,$10,$0f,$0d,$10,$10,$07,$0c,$10,$0c,$11,$10,$11
	.byte $10,$11,$10,$10,$0f,$10,$10,$11,$0f,$10,$10,$10,$08,$10,$10,$10
	.byte $08,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10
	.byte $10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10,$10

SprCenterXPos: 
{
	lda #$00
	sta TextPosX+0
	sta TextPosX+1

 	ldy #$00
oloop:
	lda (TextPtr),y
	cmp #$ff
	beq endtxt

	tax
 	lda chrWide,x
 	sta cwidth

 	clc
 	lda TextPosX+0
 	adc cwidth:#$00
 	sta TextPosX+0
 	lda TextPosX+1
 	adc #$00
 	sta TextPosX+1

 	iny
 	bra oloop

endtxt:
	// divide by 2
	lsr TextPosX+1
	ror TextPosX+0

	_set16(Layout.LayoutWidth, Tmp)
	_half16(Tmp)

	sec
	lda Tmp+0
	sbc TextPosX+0
	sta TextPosX+0
	lda Tmp+1
	sbc TextPosX+1
	sta TextPosX+1

	rts
}

SprPrintMsg: 
{
	lda #$00
	sta DrawSChr

	lda #<sprFont.baseChar
	sta DrawBaseChr+0
	lda #>sprFont.baseChar
	sta DrawBaseChr+1

	lda TextPosX+0
	sta DrawPosX+0
	lda TextPosX+1
	sta DrawPosX+1

 	lda TextPosY
 	sta DrawPosY+0
	lda #$00
	sta DrawPosY+1

 	ldy #$00

oloop:
	lda (TextPtr),y
	cmp #$ff
	beq endtxt

	// mult by 3 to get RRB sprite index
	sta mult2
	asl
	sta DrawSChr

	lda TextEffect
	beq _noeffect

	clc
	tya
	adc TextOffs
	asl
	tax

	clc
	lda sintable2,x
	cmp #$80
	ror
	adc TextPosY
	sta DrawPosY+0
	lda #$00
	sta DrawPosY+1

	bra _dodraw

_noeffect:
	lda TextPosY
	sta DrawPosY+0
	lda #$00
	sta DrawPosY+1

_dodraw:
	ldx #PIXIE_16x16
 	jsr DrawPixie

 	lda mult2:#$00
 	tax
 	lda chrWide,x
 	sta letterWidth

	clc
	lda DrawPosX+0
	adc letterWidth:#$10
	sta DrawPosX+0
	lda DrawPosX+1
	adc #$00
	sta DrawPosX+1

	iny
	bra oloop

endtxt:

	rts
}

// ---
.segment Data "Pixie Text"

sintable2:
	.fill 256, (sin((i/256) * PI * 2) * 31)
costable2:
	.fill 256, (cos((i/256) * PI * 2) * 31)

