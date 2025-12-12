// ------------------------------------------------------------
//
.segment Zeropage "GameState Credits"

.segment Code "GameState Credits"

// ------------------------------------------------------------
//
// Titles State - show titles screen
//
gsIniCredits: {

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
	_set16im($0001, Camera.CamVelY)

	_set16im($0000, Camera.XScroll)
	_set16im($0002, Camera.CamVelX)

	jsr InitObjData

	// Ensure layer system is initialized
	ldx #Layout3.id
	jsr Layout.SelectLayout

	Layer_SetRenderFunc(Layout3_BG0a.id, RenderLayout3BG0a)
	Layer_SetRenderFunc(Layout3_BG0b.id, RenderLayout3BG0b)
	Layer_SetRenderFunc(Layout3_BG1a.id, RenderLayout3BG1a)
	Layer_SetRenderFunc(Layout3_BG1b.id, RenderLayout3BG1b)
	Layer_SetRenderFunc(Layout3_Pixie.id, Layers.UpdateData.UpdatePixie)
	Layer_SetRenderFunc(Layout3_BG2a.id, RenderLayout3BG2a)
	Layer_SetRenderFunc(Layout3_BG2b.id, RenderLayout3BG2b)
	Layer_SetRenderFunc(Layout3_EOL.id, RenderNop)

	_set16(Layout.LayoutWidth, Tmp)
	
	ldx #Layout3_EOL.id
	lda Tmp+0
	jsr Layers.SetXPosLo
	lda Tmp+1
	jsr Layers.SetXPosHi

	jsr InitPixies

	rts
}

// ------------------------------------------------------------
//
gsUpdCredits: {
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
	_and16im(Camera.XScroll, $7ff, Camera.XScroll)
	_add16(Camera.YScroll, Camera.CamVelY, Camera.YScroll)
	_and16im(Camera.YScroll, $7ff, Camera.YScroll)

// 	// Min Y bounds
// 	lda Camera.YScroll+1
// 	bpl !+

// 	_set16im($0000, Camera.YScroll)
// 	_set16im($0001, Camera.CamVelY)

// !:

// 	// Max Y bounds
// 	sec
// 	lda Camera.YScroll+0
// 	sbc #<MAXYBOUNDS
// 	lda Camera.YScroll+1
// 	sbc #>MAXYBOUNDS
// 	bmi !+

// 	_set16im(MAXYBOUNDS, Camera.YScroll)
// 	_set16im($ffff, Camera.CamVelY)

// !:

donemove:

	jsr UpdateObjData

	// Copy Camera.XScroll into Tmp
	_set16(Camera.XScroll, Tmp)
	_set16(Camera.YScroll, Tmp1)

	// Update scroll values for the next frame
	{
		ldx #Layout3_BG2a.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi

		lda Tmp1+0
		jsr Layers.SetFineScrollY		// this sets both layers

		ldx #Layout3_BG2b.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi

	}

	// divide Tmp and Tmp1 by 2
	_half16(Tmp)
	_half16(Tmp1)

	{
		ldx #Layout3_BG1a.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi

		lda Tmp1+0
		jsr Layers.SetFineScrollY		// this sets both layers

		ldx #Layout3_BG1b.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi

	}

	// divide Tmp and Tmp1 by 2
	_half16(Tmp)
	_half16(Tmp1)

	{
		// Update scroll values for the next frame
		ldx #Layout3_BG0a.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi

		lda Tmp1+0
		jsr Layers.SetFineScrollY		// this sets both layers

		ldx #Layout3_BG0b.id

		lda Tmp+0
		jsr Layers.SetXPosLo
		lda Tmp+1
		jsr Layers.SetXPosHi

		lda Tmp+0
		jsr Layers.SetFineScrollX

		lda Tmp1+0
		jsr Layers.SetYPosLo
		lda Tmp1+1
		jsr Layers.SetYPosHi
	}

	lda System.DPadClick
	and #$10
	beq _not_fire

	lda #GStateTitles
	sta RequestGameState

_not_fire:

	rts
}

// ------------------------------------------------------------
//
gsDrwCredits: 
{
	_set8im($0f, DrawPal)

	jsr DrawObjData

	rts
}

// ------------------------------------------------------------
//
RenderLayout3BG0a: 
{
	// 
	ldx #Layout3_BG0a.id
	ldy #<BgMap1
	ldz #>BgMap1
	lda #$00
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout3BG0b: 
{
	// 
	ldx #Layout3_BG0b.id
	ldy #<BgMap1
	ldz #>BgMap1
	lda #$08							// layer b is offset by 8 pixels to read next row
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout3BG1a: 
{
	// 
	ldx #Layout3_BG1a.id
	ldy #<BgMap2
	ldz #>BgMap2
	lda #$00
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout3BG1b: 
{
	// 
	ldx #Layout3_BG1b.id
	ldy #<BgMap2
	ldz #>BgMap2
	lda #$08							// layer b is offset by 8 pixels to read next row
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout3BG2a: 
{
	// 
	ldx #Layout3_BG2a.id
	ldy #<BgMap3
	ldz #>BgMap3
	lda #$00
	jsr Layers.UpdateData.UpdateLayer

	rts	
}

// ------------------------------------------------------------
//
RenderLayout3BG2b: 
{
	// 
	ldx #Layout3_BG2b.id
	ldy #<BgMap3
	ldz #>BgMap3
	lda #$08							// layer b is offset by 8 pixels to read next row
	jsr Layers.UpdateData.UpdateLayer

	rts	
}




