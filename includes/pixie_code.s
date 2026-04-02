// ------------------------------------------------------------
//
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
InitPixies:
{
	RunDMAJob(JobClearTiles)
	rts

	JobClearTiles:
		// We clear ALL tiles because if there is a sneaky $ff,$ff in there the line will stop drawing
		DMAHeader(0, PixieWorkTiles>>20)
		.for(var r=0; r<MAX_NUM_ROWS; r++) {
			// Tiles
			DMAFillJob(
				$00,
				PixieWorkTiles + (r * Layout1_Pixie.DataSize),
				Layout1_Pixie.DataSize,
				(r!=(MAX_NUM_ROWS-1)))
		}
}

// ------------------------------------------------------------
//
ClearWorkPixies: 
{
	.var rowScreenPtr = Tmp		// 16bit
	.var rowAttribPtr = Tmp+2	// 16bit

	lda #$00
	sta PixieYShift
	
	_set16im(MappedPixieWorkTiles, rowScreenPtr)
	_set16im(MappedPixieWorkAttrib, rowAttribPtr)

	// Clear the RRBIndex list
	ldx #0
!:
	lda #0
	sta PixieUseOffset,x

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
	RunDMAJob(JobFillA)

	_set8im(8, DrawMode)

	rts 

	JobFillA:
		// We fill ONLY the attrib0 byte with a GOTOX + TRANS token, note the 2 byte step value
		DMAHeader(0, PixieWorkAttrib>>20)
		DMADestStep(2, 0)
		.for(var r=0; r<MAX_NUM_ROWS; r++) {
			// Atrib
			DMAFillJob(
				$90,							// GOTOX + Transparent
				PixieWorkAttrib + (r * Layout1_Pixie.DataSize),
				Layout1_Pixie.DataSize / 2,
				(r!=(MAX_NUM_ROWS-1)))
		}
}	

// ------------------------------------------------------------
//
yShiftTable:		.byte (0<<5)|$10,(1<<5)|$10,(2<<5)|$10,(3<<5)|$10,(4<<5)|$10,(5<<5)|$10,(6<<5)|$10,(7<<5)|$10
yMaskTable:			.byte %11111111,%11111110,%11111100,%11111000,%11110000,%11100000,%11000000,%10000000

// Number of chars wide and high for each of the pixie layouts
pixieLayoutH:		.fill PixieLayoutList.size(), PixieLayoutList.get(i).charsHigh
pixieLayoutW:		.fill PixieLayoutList.size(), PixieLayoutList.get(i).charsWide
pixieLayoutB:		.fill PixieLayoutList.size(), PixieLayoutList.get(i).numBytesPerRow
pixieMaxRowOffs:	.fill PixieLayoutList.size(), PixieLayoutList.get(i).maxRowOffs

DrawPixie:
{
	.var tilePtr 	= Tmp					// 16 bit
	.var attribPtr 	= Tmp+2					// 16 bit

	.var tcharIndx 	= Tmp1+0				// 16 bit
											// unused 16 bits = Tmp1+2

	.var charIndx 	= Tmp2+0				// 16 bit
	.var yShift 	= Tmp2+2				// 8 bit
	.var gotoXmask 	= Tmp2+3				// 8 bit

	.var charHigh 	= Tmp3+0				// 8 bit
	.var charWidth 	= Tmp3+1				// 8 bit
	.var maxRowOffs	= Tmp3+2				// 8 bit
											// unused 8 bits = Tmp3+3

	phx
	phy
	phz

	// Grab all of the params from the pixie layout
	//
	lda pixieLayoutH,x					
	sta addRowOfChars.charStep			// Number of chars between columns
	dec
	sta charHigh						// Value for row loop = (num chars high - 1)

	lda pixieLayoutW,x					
	sta charWidth						// Value for column loop = (num chars wide - 1)

	lda pixieMaxRowOffs,x
	sta maxRowOffs

	// Map in the pixie working buffer to allow 16bit access
	//
	mapHi(PixieWorkTiles, MappedPixieWorkTiles, $03)	// $8000-bfff is mapped
	mapLo(0, 0, 0)
	map
	eom

	// Check to see if the xpos > current layout width
	//
	sec
	lda DrawPosX+0
	sbc Layout.LayoutWidth+0
	lda DrawPosX+1
	sbc Layout.LayoutWidth+1
	lbpl done

	clc									// Start charIndx with first pixie char
	lda DrawBaseChr+0
	adc DrawSChr
	sta charIndx+0
	lda DrawBaseChr+1
	adc #$00
	sta charIndx+1

	lda PixieYShift						// Shift the pixies down if base layer is vertically scrolling
	beq no_vertical_scrolling
	and #$07
	sta lshift

	lda DrawPosY+0
	clc
	adc lshift:#$00
	sta DrawPosY+0
	lda DrawPosY+1
	adc #0
	sta DrawPosY+1

no_vertical_scrolling:

	lda DrawPosY+0						// Find sub row y offset (0 - 7)
	and #$07
	tay	

	lda yMaskTable,y					// grab the rowMask value
	sta gotoXmask

	lda yShiftTable,y					// grab the yShift value 
	sta yShift

	lda DrawPosX+0
	sta addRowOfChars.xposLo
	lda DrawPosX+1
	and #$03
	ora yShift
	sta addRowOfChars.xposHi

	lda DrawMode
	sta addRowOfChars.usedrawmode
	lda DrawPal
	sta addRowOfChars.usedrawpal

	// Calculate which row to add pixie data to, put this in X,
    // we use this to index the row tile / attrib ptrs
 	// 
	lda DrawPosY+1
	bmi above_FirstRow
	bne done						// if in first 256 pixels vertically then we are on row 0, otherwise we need to calculate the row index
	
	lda DrawPosY+0
	lsr	
	lsr	
	lsr	
	tax
	bra test_firstRow

above_FirstRow:

	// Do full signed / 8 to get the row index
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
test_firstRow:
	bmi middleRow						// if above first row then skip to middle section
	cpx Layout.NumRows					// if below last row then done
	bcs done

	// Top character, this uses the first mask from the tables above
	lda gotoXmask
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
	bcs done
    
	// Middle character, yShift is the same as first char but full character is drawn so disable rowmask,
	lda #$ff
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

	// Bottom character, yShift is the same as first char but flip the bits of the gotoXmask
	lda gotoXmask
	eor #$ff
	jsr addRowOfChars

done:

	unmapMemory()

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
		sta rowMask

		// grab tile and attrib ptr and offset for this row.
		//
		// See if number of words has been exhausted
		//
		lda PixieUseOffset,x				//
		cmp maxRowOffs
		beq do_draw
		bcs skip

	do_draw:	
		taz

		lda PixieRowScreenPtrLo,x			// grab tilePtr
		sta tilePtr+0
		lda PixieRowScreenPtrHi,x
		sta tilePtr+1
		lda PixieRowAttribPtrLo,x			// grab attribPtr
		sta attribPtr+0
		lda PixieRowAttribPtrHi,x
		sta attribPtr+1

		// GOTOX
		lda xposLo:#$00						// tile = <xpos,>xpos | yShift
		sta (tilePtr),z
		lda #$98							// attrib = $98 (transparent+gotox), $00
		sta (attribPtr),z
		inz
		lda xposHi:#$00
		sta (tilePtr),z
		lda rowMask:#$ff
		sta (attribPtr),z
		inz	// Store the tile and attrib offset for this row now that we've added some chars

		lda charIndx+0						// Make a copy of charIndx because it's modified per char
		sta tcharIndx+0
		lda charIndx+1
		sta tcharIndx+1

		ldy charWidth						// loop to add charWidth chars
	cloop:
		// Char
		lda tcharIndx+0
		sta (tilePtr),z
		lda usedrawmode:#$00
		sta (attribPtr),z
		inz	
		lda tcharIndx+1
		sta (tilePtr),z
		lda usedrawpal:#$00
		sta (attribPtr),z
		inz
				
		clc									// move to the next column's char
		lda tcharIndx+0
		adc charStep:#$00
		sta tcharIndx+0
		bcc !+
		inc tcharIndx+1
	!:

		dey
		bne cloop

		//
		stz PixieUseOffset,x

	skip:
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

.segment BSS "Pixie Use Offset"
PixieUseOffset:
	.fill MAX_NUM_ROWS, $00
