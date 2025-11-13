// ------------------------------------------------------------
//
.const NUM_OBJS1 = 256

.segment Zeropage "GameState Play"

.segment Code "GameState Play"

// ------------------------------------------------------------
//
// Titles State - show titles screen
//
gsIniPlay: {

	lda #$00
	sta Irq.VBlankCount

	lda #$00
	sta GameSubState
	sta GameStateTimer

	lda #$00
	sta GameStateData+0
	sta GameStateData+1
	sta GameStateData+2

	_set16im($0000, Camera.YScroll)
	_set16im($0000, Camera.CamVelY)

	_set16im($0000, Camera.XScroll)
	_set16im($0002, Camera.CamVelX)

	jsr InitObjData

	// Ensure layer system is initialized
	ldx #Layout2.id
	jsr Layout.SelectLayout

	Layer_SetRenderFunc(Layout2_BG0.id, RenderLayout2BG0)
	Layer_SetRenderFunc(Layout2_BG1.id, RenderLayout2BG1)
	Layer_SetRenderFunc(Layout2_Pixie.id, Layers.UpdateData.UpdatePixie)
	Layer_SetRenderFunc(Layout2_EOL.id, RenderNop)

	_set16(Layout.LayoutWidth, Tmp)
	
	ldx #Layout2_EOL.id
	lda Tmp+0
	jsr Layers.SetXPosLo
	lda Tmp+1
	jsr Layers.SetXPosHi

	rts
}

// ------------------------------------------------------------
//
gsUpdPlay: {
	// Inc the game state timer
	_add16im(GameStateData, 1, GameStateData)
	lda GameStateData+0
	cmp #$c0
	lda GameStateData+1
	sbc #$02
	bcc !+
	_set16im(0, GameStateData)

	clc
	lda GameStateData+2
	adc #$01
	and #$03
	sta GameStateData+2
!:

//	_add16im(Camera.XScroll, 1, Camera.XScroll)

	lda Irq.VBlankCount
	and #$00
	lbne donemove

	_add16(Camera.XScroll, Camera.CamVelX, Camera.XScroll)
	_and16im(Camera.XScroll, $1ff, Camera.XScroll)
	_add16(Camera.YScroll, Camera.CamVelY, Camera.YScroll)

	// Min Y bounds
	lda Camera.YScroll+1
	bpl !+

	_set16im($0000, Camera.YScroll)
	_set16im($0001, Camera.CamVelY)

!:

	// Max Y bounds
	sec
	lda Camera.YScroll+0
	sbc #<MAXYBOUNDS
	lda Camera.YScroll+1
	sbc #>MAXYBOUNDS
	bmi !+

	_set16im(MAXYBOUNDS, Camera.YScroll)
	_set16im($ffff, Camera.CamVelY)

!:

donemove:

	jsr UpdateObjData

	// Copy Camera.XScroll into Tmp
	_set16(Camera.XScroll, Tmp)

	// Update scroll values for the next frame
	ldx #Layout2_BG1.id

	lda Tmp+0
	jsr Layers.SetXPosLo
	lda Tmp+1
	jsr Layers.SetXPosHi

	lda Tmp+0
	jsr Layers.SetFineScrollX

	lda Camera.YScroll+0
	jsr Layers.SetYPosLo
	lda Camera.YScroll+1
	jsr Layers.SetYPosHi

	// divide Tmp by 2
	_half16(Tmp)

	// Update scroll values for the next frame
	ldx #Layout2_BG0.id

	lda Tmp+0
	jsr Layers.SetXPosLo
	lda Tmp+1
	jsr Layers.SetXPosHi

	lda Tmp+0
	jsr Layers.SetFineScrollX

	lda Camera.YScroll+0
	jsr Layers.SetYPosLo
	lda Camera.YScroll+1
	jsr Layers.SetYPosHi

	lda System.DPadClick
	and #$10
	beq _not_fire

	lda #GStateCredits
	sta RequestGameState

_not_fire:

	rts
}

// ------------------------------------------------------------
//
gsDrwPlay: 
{
	jsr DrawObjData

	rts
}

// ------------------------------------------------------------
//
RenderLayout2BG0: 
{
	// 
	ldx #Layout2_BG0.id
	ldy #<BgMap1
	ldz #>BgMap1
	lda #$00
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout2BG1: 
{
	// 
	ldx #Layout2_BG1.id
	ldy #<BgMap2
	ldz #>BgMap2
	lda #$00
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
UpdateObjData:
{
	// Add Objs into the work ram here
	//
	ldx #$00
!:
	clc
	lda Objs1PosXLo,x
	adc Objs1VelXLo,x
	sta Objs1PosXLo,x
	lda Objs1PosXHi,x
	adc Objs1VelXHi,x
	and #$01
	sta Objs1PosXHi,x

	clc
	lda Objs1PosYLo,x
	adc Objs1VelY,x
	sta Objs1PosYLo,x

	inx
	cpx #NUM_OBJS1
	bne !-

	rts
}

// ------------------------------------------------------------
//
DrawObjData:
{
	_set16im(sprite32x32Chars.baseChar, DrawBaseChr)			// Start charIndx with first pixie char

	_set8im((PAL_SPR << 4) | $0f, DrawPal)

	// Add Objs into the work ram here
	//
	ldx #$00
!:
	phx

	sec
	lda Objs1PosYLo,x
	sbc #$20
	sta DrawPosY+0
	lda #$00
	sbc #$00
	sta DrawPosY+1

	sec
	lda Objs1PosXLo,x
	sbc #$20
	sta DrawPosX+0
	lda Objs1PosXHi,x
	sbc #$00
	sta DrawPosX+1

	lda Objs1Spr,x
	sta DrawSChr

	clc
	lda $d020
	adc #$01
	and #$0f
	sta $d020

	ldx #PIXIE_32x32
	jsr DrawPixie

	plx
	inx
	cpx #NUM_OBJS1
	bne !-

	rts
}

// ------------------------------------------------------------
//
initXVel:	.word $ffff,$0001
initYVel:	.byte $fe,$ff,$01,$02

InitObjData:
{
    .var xpos = Tmp       // 16bit
    .var ypos = Tmp+2     // 8bit

	// Init Obj group 1
	//
	//
	_set16im(0, xpos)
	_set8im(0, ypos)

	ldx #$00
iloop1:
	lda xpos+0
	sta Objs1PosXLo,x
	lda xpos+1
	sta Objs1PosXHi,x
	lda ypos
	sta Objs1PosYLo,x

	txa
	and #$03
	clc
	adc #$00
	asl
	asl
	asl
	sta Objs1Spr,x

	txa
	and #$01
	asl
	tay
	lda initXVel+0,y
	sta Objs1VelXLo,x
	lda initXVel+1,y
	sta Objs1VelXHi,x

	txa
	and #$03
	tay
	lda initYVel,y
	sta Objs1VelY,x

	_add16im(xpos, -28, xpos)
	_and16im(xpos, $1ff, xpos)
	_add8im(ypos, 10, ypos)

	inx
	cpx #NUM_OBJS1
	bne iloop1

	rts
}

// ---
.segment Data "GameState Play"

// ------------------------------------------------------------
//
.segment BSS "Obj Data"

Objs1PosXLo:
	.fill NUM_OBJS1, 0
Objs1PosXHi:
	.fill NUM_OBJS1, 0
Objs1PosYLo:
	.fill NUM_OBJS1, 0
Objs1VelXLo:
	.fill NUM_OBJS1, 0
Objs1VelXHi:
	.fill NUM_OBJS1, 0
Objs1VelY:
	.fill NUM_OBJS1, 0
Objs1Spr:
	.fill NUM_OBJS1, 0




