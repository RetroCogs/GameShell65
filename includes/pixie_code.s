// ------------------------------------------------------------
//
.enum 
{
	PIXIE_16x8,
	PIXIE_16x16,
	PIXIE_16x24,
	PIXIE_16x32,
	PIXIE_32x8,
	PIXIE_32x16,
	PIXIE_32x24,
	PIXIE_32x32
}

.segment Zeropage "Pixie ZP"

DrawPosX:		.byte $00,$00
DrawPosY:		.byte $00,$00
DrawBaseChr:    .byte $00,$00
DrawPal:        .byte $00
DrawSChr:		.byte $00
DrawMode:		.byte $00
PixieYShift:	.byte $00

// ------------------------------------------------------------
//
.segment Code "Pixie Code"

// ------------------------------------------------------------
//
ClearWorkPixies: 
{
	.var rowScreenPtr = Tmp		// 16bit
	.var rowAttribPtr = Tmp+2	// 16bit

	lda #$00
	sta PixieYShift
	
	_set16im(PixieWorkTiles, rowScreenPtr)
	_set16im(PixieWorkAttrib, rowAttribPtr)

	// Clear the RRBIndex list
	ldx #0
!:
	lda #NUM_PIXIEWORDS
	sta PixieUseCount,x

	lda rowScreenPtr+0
	sta PixieRowScreenPtrLo,x
	lda rowScreenPtr+1
	sta PixieRowScreenPtrHi,x

	lda rowAttribPtr+0
	sta PixieRowAttribPtrLo,x
	lda rowAttribPtr+1
	sta PixieRowAttribPtrHi,x

	_add16im(rowScreenPtr, Layout1_Pixie.DataSize, rowScreenPtr)
	_add16im(rowAttribPtr, Layout1_Pixie.DataSize, rowAttribPtr)
	
	inx
	cpx Layout.NumRows
	bne !-

	// Clear the working pixie data using DMA
	RunDMAJob(JobFill)

	_set8im(8, DrawMode)

	rts 

JobFill:
	// We fill ONLY the attrib0 byte with a GOTOX + TRANS token, note the 2 byte step value
	DMAHeader(0, PixieWorkAttrib>>20)
	DMADestStep(2, 0)
	.for(var r=0; r<MAX_NUM_ROWS; r++) {
		// Atrib
		DMAFillJob(
			$90,
			PixieWorkAttrib + (r * Layout1_Pixie.DataSize),
			Layout1_Pixie.DataSize / 2,
			(r!=(MAX_NUM_ROWS-1)))
	}
}	

// ------------------------------------------------------------
//
yShiftTable:	.byte (0<<5)|$10,(1<<5)|$10,(2<<5)|$10,(3<<5)|$10,(4<<5)|$10,(5<<5)|$10,(6<<5)|$10,(7<<5)|$10
yMaskTable:		.byte %11111111,%11111110,%11111100,%11111000,%11110000,%11100000,%11000000,%10000000

pixieLayoutH:	.byte 1,2,3,4,1,2,3,4
pixieLayoutW:	.byte 1,1,1,1,2,2,2,2

DrawPixie:
{
	.var tilePtr 	= Tmp					// 32 bit
	.var attribPtr 	= Tmp1					// 32 bit

	.var charIndx 	= Tmp2+0				// 16 bit
	.var yShift 	= Tmp2+2				// 8 bit
	.var gotoXmask 	= Tmp2+3				// 8 bit

	.var charHigh 	= Tmp3+0				// 8 bit
	.var charStep 	= Tmp3+1				// 8 bit
	.var charWidth 	= Tmp3+2				// 8 bit
	.var charBytes	= Tmp3+3				// 8 bit

	phx
	phy
	phz

	sec
	lda DrawPosX+0
	sbc Layout.LayoutWidth+0
	lda DrawPosX+1
	sbc Layout.LayoutWidth+1
	lbpl done

	// Grab all of the params from the pixie layout
	//
	lda pixieLayoutH,x					
	sta charStep						// Number of chars between columns
	dec
	sta charHigh						// Value for row loop = (num chars high - 1)

	lda pixieLayoutW,x					
	sta charWidth						// Value for column loop = (num chars wide - 1)
	inc
	asl
	sta charBytes						// number of bytes to add = ((num chars wide + 1)*2)

	_set16(DrawBaseChr, charIndx)		// Start charIndx with first pixie char

	_set32im(PixieWorkTiles, tilePtr)	// Set base full 32 bit pointers
	_set32im(PixieWorkAttrib, attribPtr)

	clc
	lda charIndx+0
	adc DrawSChr
	sta charIndx+0
	lda charIndx+1
	adc #$00
	sta charIndx+1

	lda PixieYShift					// This game doesn't use vertical scrolling
	and #$07
	sta lshift

	lda DrawPosY+0
	clc
	adc lshift:#$00
	sta DrawPosY+0
	lda DrawPosY+1
	adc #0
	sta DrawPosY+1

	lda DrawPosY+0						// Find sub row y offset (0 - 7)
	and #$07
	tay	

	lda yMaskTable,y					// grab the rowMask value
	sta gotoXmask

	lda yShiftTable,y					// grab the yShift value 
	sta yShift

	// Calculate which row to add pixie data to, put this in X,
    // we use this to index the row tile / attrib ptrs
 	// 
	lda DrawPosY+0
	sta posy

	lda DrawPosY+1						// row index = drawPosY / 8 (and handle for -ve)
	cmp #$80
	ror
	ror posy
	cmp #$80
	ror
	ror posy
	cmp #$80
	ror
	ror posy

	lda posy:#$00
	tax									// move yRow into X reg
	bmi middleRow
	cpx Layout.NumRows
	lbcs done

	// See if number of words has been exhausted
	lda PixieUseCount,x
	beq middleRow
	dec PixieUseCount,x

	// Top character, this uses the first mask from the tables above,
    // grab tile and attrib ptr for this row and advance by the N bytes
    // that we will write per row.
	//
	clc                                 // grab and advance tilePtr
	lda PixieRowScreenPtrLo,x
	sta tilePtr+0
	adc charBytes
	sta PixieRowScreenPtrLo,x
	lda PixieRowScreenPtrHi,x
	sta tilePtr+1
	adc #$00
	sta PixieRowScreenPtrHi,x
	clc                                 // grab and advance attribPtr
	lda PixieRowAttribPtrLo,x
	sta attribPtr+0
	adc charBytes
	sta PixieRowAttribPtrLo,x
	lda PixieRowAttribPtrHi,x
	sta attribPtr+1
	adc #$00
	sta PixieRowAttribPtrHi,x

	// GOTOX
	ldz #$00
	lda DrawPosX+0						// tile = <xpos,>xpos | yShift
	sta ((tilePtr)),z
	lda #$98							// attrib = $98 (transparent+gotox+rowmask), gotoXmask
	sta ((attribPtr)),z
	inz
	lda DrawPosX+1
	and #$03
	ora yShift
	sta ((tilePtr)),z
	lda gotoXmask
	sta ((attribPtr)),z
	inz

	jsr addRowOfChars

middleRow:
	dec charHigh
	lda charHigh
	bmi bottomRow

	// Advance to next row and charIndx
    inw charIndx
	inx
	bmi middleRow								// If still above the first row then try another middle row
	cpx Layout.NumRows
	lbcs done
    
	// See if number of words has been exhausted
	lda PixieUseCount,x
	beq bottomRow
	dec PixieUseCount,x

	// Middle character, yShift is the same as first char but full character is drawn so disable rowmask,
    // grab tile and attrib ptr for this row and advance by the 4 bytes
    // that we will write per row.
	//
	clc                                 // grab and advance tilePtr
	lda PixieRowScreenPtrLo,x
	sta tilePtr+0
	adc charBytes
	sta PixieRowScreenPtrLo,x
	lda PixieRowScreenPtrHi,x
	sta tilePtr+1
	adc #$00
	sta PixieRowScreenPtrHi,x
	clc                                 // grab and advance attribPtr
	lda PixieRowAttribPtrLo,x
	sta attribPtr+0
	adc charBytes
	sta PixieRowAttribPtrLo,x
	lda PixieRowAttribPtrHi,x
	sta attribPtr+1
	adc #$00
	sta PixieRowAttribPtrHi,x	

	// GOTOX
	ldz #$00
	lda DrawPosX+0						// tile = <xpos,>xpos | yShift
	sta ((tilePtr)),z
	lda #$90							// attrib = $98 (transparent+gotox), $00
	sta ((attribPtr)),z
	inz
	lda DrawPosX+1
	and #$03
	ora yShift
	sta ((tilePtr)),z
	lda #$ff
	sta ((attribPtr)),z
	inz

	jsr addRowOfChars

	bra middleRow

bottomRow:
	// If we have a yShift of 0 we only need to add to 2 rows, skip the last row!
	//
	lda yShift
	and #$e0
	beq done

	// Advance to next row and charIndx
    inw charIndx
	inx
	bmi done
	cpx Layout.NumRows
	bcs done

	// See if number of words has been exhausted
	lda PixieUseCount,x
	beq done
	dec PixieUseCount,x

	// Bottom character, yShift is the same as first char but flip the bits of the gotoXmask,
    // grab tile and attrib ptr for this row and advance by the 4 bytes
    // that we will write per row.
	//
	clc                                 // grab and advance tilePtr
	lda PixieRowScreenPtrLo,x
	sta tilePtr+0
	adc charBytes
	sta PixieRowScreenPtrLo,x
	lda PixieRowScreenPtrHi,x
	sta tilePtr+1
	adc #$00
	sta PixieRowScreenPtrHi,x
	clc                                 // grab and advance tilePtr
	lda PixieRowAttribPtrLo,x
	sta attribPtr+0
	adc charBytes
	sta PixieRowAttribPtrLo,x
	lda PixieRowAttribPtrHi,x
	sta attribPtr+1
	adc #$00
	sta PixieRowAttribPtrHi,x

	lda gotoXmask
	eor #$ff
	sta gotoXmask

	// GOTOX
	ldz #$00
	lda DrawPosX+0						// tile = <xpos,>xpos | yShift	
	sta ((tilePtr)),z
	lda #$98							// attrib = $98 (transparent+gotox+rowmask), gotoXmask
	sta ((attribPtr)),z
	inz
	lda DrawPosX+1
	and #$03
	ora yShift
	sta ((tilePtr)),z
	lda gotoXmask
	sta ((attribPtr)),z
	inz

	jsr addRowOfChars

done:

	plz
	ply
	plx

	rts

	// For each GOTOX we can add multiple chars so loop through and add them
	// each new char will skip charSkip indexes so we need to store and restore
	// that value
	//
	addRowOfChars:
	{
		lda charIndx+0
		pha
		lda charIndx+1
		pha

		ldy charWidth						// loop to add charWidth chars
	cloop:
		// Char
		lda charIndx+0
		sta ((tilePtr)),z
		lda DrawMode
		sta ((attribPtr)),z
		inz	
		lda charIndx+1
		sta ((tilePtr)),z
		lda DrawPal
		sta ((attribPtr)),z
		inz
		
		clc									// move to the next column's char
		lda charIndx+0
		adc charStep
		sta charIndx+0
		lda charIndx+1
		adc #$00
		sta charIndx+1

		dey
		bne cloop

		pla
		sta charIndx+1
		pla
		sta charIndx+0

		rts
	}
}

// ------------------------------------------------------------
//
.segment BSS "Pixie Work Lists"
PixieRowScreenPtrLo:
	.fill MAX_NUM_ROWS, $00
PixieRowScreenPtrHi:
	.fill MAX_NUM_ROWS, $00

PixieRowAttribPtrLo:
	.fill MAX_NUM_ROWS, $00
PixieRowAttribPtrHi:
	.fill MAX_NUM_ROWS, $00

.segment BSS "Pixie Use Count"
PixieUseCount:
	.fill MAX_NUM_ROWS, $00
