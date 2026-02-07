//--------------------------------------------------------
// Macros (can't put macros in namespace)
.macro Layer_SetRenderFunc(layerId, renderFunc)
{
 	ldx #layerId
	lda #<renderFunc
	sta Layers.RenderFuncLo,x
	lda #>renderFunc
	sta Layers.RenderFuncHi,x
}

//--------------------------------------------------------
// Layers
//--------------------------------------------------------
.namespace Layers
{

//--------------------------------------------------------
//
.segment Code "Layer Code"

// -------------------------------------------------------
// X = Layer Id
// A = Layer position Lo
SetXPosLo:
{
	sta ScrollXLo,x
	sta FineScrollXLo,x
	lda #$01
	sta ScrollUpdate,x
	rts
}

// -------------------------------------------------------
// X = Layer Id
// A = Layer position Hi
SetXPosHi:
{
	sta ScrollXHi,x
	sta FineScrollXHi,x
	lda #$01
	sta ScrollUpdate,x
	rts
}

// -------------------------------------------------------
// X = Layer Id
// A = Layer position Y Lo
SetYPosLo:
{
	sta ScrollYLo,x
	rts
}

// -------------------------------------------------------
// X = Layer Id
// A = Layer position Y Hi
SetYPosHi:
{
	sta ScrollYHi,x
	rts
}

// ------------------------------------------------------------
// X = Layer Id
// A = Fine scroll value
SetFineScrollX: 
{
	and #$0f
	sta ul2xscroll
	sec
	lda #0
	sbc ul2xscroll:#$00
	sta FineScrollXLo,x
	lda #0
	sbc #0
	and #$03
	sta FineScrollXHi,x

	lda #$01
	sta ScrollUpdate,x

	rts
}

SetFineScrollY:
{
	// Parallax layer a & b
	and #$07
	tay
	lda shiftOffsets1,y
	sta YShift,x
	lda shiftMasks,y
	sta YMask,x
	inx
	eor #$ff
	sta YMask,x
	lda shiftOffsets2,y
	sta YShift,x
	dex

	rts
}

// ------------------------------------------------------------
//
UpdateScrollPositions: 
{
	.var tile_ptr = Tmp			// 32bit
	.var attrib_ptr = Tmp1		// 32bit
	.var gotoOffs = Tmp2		// 16bit

	ldx Layout.BeginLayer

!layerloop:
	lda Layers.ScrollUpdate,x
	lbeq !layerskip+

	lda #$00
	sta Layers.ScrollUpdate,x

	lda Layers.Trans,x
	ora #$08				// Add rowmask flag
	sta transFlag

	// setup the gotox offset
	lda Layers.LogOffsLo,x
	sta gotoOffs+0
	lda Layers.LogOffsHi,x
	sta gotoOffs+1

    _set32im(ScreenRam, tile_ptr)
    _add16(tile_ptr, gotoOffs, tile_ptr)
    _set32im(COLOR_RAM, attrib_ptr)
    _add16(attrib_ptr, gotoOffs, attrib_ptr)

	phx

		ldy Layout.NumRows
!loop:
		// Set GotoX position
		ldz #0
		lda Layers.FineScrollXLo,x
		sta ((tile_ptr)), z
		lda transFlag:#$10
		sta ((attrib_ptr)),z
		inz
		lda YShift,x
		ora Layers.FineScrollXHi,x
		sta ((tile_ptr)), z
		lda YMask,x
		sta ((attrib_ptr)),z

	    _add16(tile_ptr, Layout.LogicalRowSize, tile_ptr)
	    _add16(attrib_ptr, Layout.LogicalRowSize, attrib_ptr)

		dey
		lbne !loop-

	plx

!layerskip:

	inx
	cpx Layout.EndLayer
	lbne !layerloop-

	rts
}

// -------------------------------------------------------
// X = Layer Id
// Y = BG Desc Lo
// Z = BG Desc Hi
//
UpdateData: 
{
	.var src_tile_ptr = Tmp			// 32bit o
	.var src_attrib_ptr = Tmp1		// 32bit o

	.var src_x_size = Tmp2			// 16bit
	.var src_y_size = Tmp2+2		// 16bit

	.var bgDesc = Tmp3				// 16bit
	.var dst_offset = Tmp3+2		// 16bit

	.var src_x_offset = Tmp4		// 16bit
	.var src_y_offset = Tmp4+2		// 16bit

	.var copy_width = Tmp5			// 16bit
	.var copy_height = Tmp5+2		// 16bit

	.var dst_x_size = Tmp6			// 16bit
	.var src_x_and = Tmp6+2			// 16bit

	.var dst_y_size = Tmp7			// 16bit
	.var src_y_and = Tmp7+2			// 16bit

	UpdatePixie: 
	{
		_set32im(PixieWorkTiles, src_tile_ptr)
		_set32im(PixieWorkAttrib, src_attrib_ptr)

		_set16(Layout.PixieGotoOffs, dst_offset)
		_set16im(0, src_x_offset)
		_set16im(0, src_y_offset)

		_set16im(Layout1_Pixie.DataSize, src_x_size)

		_set16(src_x_size, copy_width)
		_set8(Layout.NumRows, copy_height)

		jsr CopyLayerChunks

		rts
	}

	InitEOL: 
	{
		.var dst_tile_ptr = Tmp			// 32bit
		.var dst_attrib_ptr = Tmp1		// 32bit
		.var chrOffs = Tmp2				// 16bit

		_set32im(ScreenRam, dst_tile_ptr)
		_set32im(COLOR_RAM, dst_attrib_ptr)

		// get the selected layout's last layer
		ldx Layout.EndLayer
		dex

		lda ChrOffsLo,x
		sta chrOffs+0
		lda ChrOffsHi,x
		sta chrOffs+1

		_add16(dst_tile_ptr, chrOffs, dst_tile_ptr)
		_add16(dst_attrib_ptr, chrOffs, dst_attrib_ptr)

		ldy #$00
	!:
		ldz #$00
		lda #$00
		sta ((dst_tile_ptr)),z
		lda #$00
		sta ((dst_attrib_ptr)),z
		inz
		lda #$00
		sta ((dst_tile_ptr)),z
		lda #$00
		sta ((dst_attrib_ptr)),z

		_add16(dst_tile_ptr, Layout.LogicalRowSize, dst_tile_ptr)
		_add16(dst_attrib_ptr, Layout.LogicalRowSize, dst_attrib_ptr)

		iny
		cpy Layout.NumRows
		bne !-

		rts
	}

	UpdateLayer: 
	{
		sta lineOffs
		sty bgDesc+0
		stz bgDesc+1

		ldy #$00
		lda (bgDesc),y				// map data - tile data pointer
		sta src_tile_ptr+0
		iny
		lda (bgDesc),y
		sta src_tile_ptr+1
		iny
		lda (bgDesc),y
		sta src_tile_ptr+2
		iny
		lda (bgDesc),y
		sta src_tile_ptr+3
		iny

		lda (bgDesc),y				// map data - attrib data pointer
		sta src_attrib_ptr+0
		iny
		lda (bgDesc),y
		sta src_attrib_ptr+1
		iny
		lda (bgDesc),y
		sta src_attrib_ptr+2
		iny
		lda (bgDesc),y
		sta src_attrib_ptr+3
		iny

		lda (bgDesc),y				// map data - number of bytes wide
		sta src_x_size+0
		iny
		lda (bgDesc),y
		sta src_x_size+1
		iny

		lda (bgDesc),y				// map data - number of char lines high (pixels / 8)
		sta src_y_size+0
		iny
		lda (bgDesc),y
		sta src_y_size+1
		iny

		_sub16im(src_x_size, $0002, src_x_and)
		_sub16im(src_y_size, $0001, src_y_and)

		lda Layout.NumRows
		sta dst_y_size+0
		lda #$00
		sta dst_y_size+1

		// Calculate which row data to add this character to, we
		// are using the MUL hardware here to avoid having a row table.
		// 
		// This translates to $d778-A = (yScroll>>3) * mapLogicalWidth
		//
		clc
		lda ScrollYLo,x
		adc lineOffs:#$00
		sta src_y_offset+0			// mul A lsb
		lda ScrollYHi,x
		adc #$00
		sta src_y_offset+1			// mul A msb

		// divide mul A by 8 to get number of chars to shift by
		lsr src_y_offset+1
		ror src_y_offset+0
		lsr src_y_offset+1
		ror src_y_offset+0
		lsr src_y_offset+1
		ror src_y_offset+0

		_and16(src_y_offset, src_y_and, src_y_offset)

		// 
		_set16(src_y_size, copy_height)
		_sub16(copy_height, src_y_offset, copy_height)

		lda dst_y_size+0
		cmp copy_height+0
		lda dst_y_size+1
		sbc copy_height+1
		bcs !ee+
		_set16(dst_y_size, copy_height)
!ee:

		lda ChrSizeLo,x
		sta dst_x_size+0
		lda ChrSizeHi,x
		sta dst_x_size+1


		// fetch the left column of the dest
		lda ChrOffsLo,x
		sta dst_offset+0
		lda ChrOffsHi,x
		sta dst_offset+1

		lda dst_offset+0
		pha
		lda dst_offset+1
		pha

		jsr CopyScrollingLayerChunks

		pla
		sta dst_offset+1
		pla	
		sta dst_offset+0

		// See how many more lines to copy
		lda dst_y_size+0
		cmp copy_height+0
		bne !next+
		lda dst_y_size+1
		cmp copy_height+1
		beq !done+

!next:

		// need to offset down dst_offset by copy_height lines
		_mul16(copy_height, Layout.LogicalRowSize, dst_offset, dst_offset)
		_set16im(0, src_y_offset)

		_sub16(dst_y_size, copy_height, copy_height)

		jsr CopyScrollingLayerChunks

!done:
		rts
	}

	// Copy a column of tile/attrib data to target buffers
	// 
	// inputs:	x = layer id
	//			src_x_size
	//			copy_height
	//
	CopyScrollingLayerChunks: 
	{
		// fetch the left column of the source
		lda ScrollXLo,x
		sta src_x_offset+0
		lda ScrollXHi,x
		sta src_x_offset+1

		lsr src_x_offset+1
		ror src_x_offset+0
		lsr src_x_offset+1
		ror src_x_offset+0
		lsr src_x_offset+1
		ror src_x_offset+0

		_and16(src_x_offset, src_x_and, src_x_offset)

		// 
		_set16(src_x_size, copy_width)
		_sub16(copy_width, src_x_offset, copy_width)

		lda dst_x_size+0
		cmp copy_width+0
		lda dst_x_size+1
		sbc copy_width+1
		bcs !ee+
		_set16(dst_x_size, copy_width)
!ee:

		jsr CopyLayerChunks

		// need to fix this with >255 byte wide maps?
		lda dst_x_size+0
		cmp copy_width+0
		bne !next+
		lda dst_x_size+1
		cmp copy_width+1
		beq !done+

!next:
		_add16(dst_offset, copy_width, dst_offset)
		_set16im(0, src_x_offset)

		_sub16(dst_x_size, copy_width, copy_width)

		jsr CopyLayerChunks

!done:
		rts
	}

	// Copy a column of tile/attrib data to target buffers
	// 
	// inputs:	src_tile_ptr
	//			src_attrib_ptr
	//			src_x_offset
	//			dst_offset
	//			copy_height
	//
	CopyLayerChunks: 
	{
		_set16(copy_width, tileLength)
		_set16(copy_width, attribLength)

		// Tiles are copied from Bank 0 to (SCREEN_RAM>>20)
		lda #$00
		sta tileSourceBank
		lda #PIXIEANDSCREEN_RAM>>20
		sta tileDestBank
		lda src_tile_ptr+2
		sta tileSource+2
		lda #[PIXIEANDSCREEN_RAM >> 16]
		and #$0f
		sta tileDest+2

		// Attribs are copied from Bank 0 to (COLOR_RAM>>20)
		lda #$00
		sta attribSourceBank
		lda #COLOR_RAM>>20
		sta attribDestBank
		lda src_attrib_ptr+2
		sta attribSource+2
		lda #[COLOR_RAM >> 16]
		and #$0f
		sta attribDest+2

		// DMA tile rows
		//
		_mul16(src_y_offset, src_x_size, src_tile_ptr, tileSource)
		_add16(tileSource, src_x_offset, tileSource)
		_add16im(dst_offset, ScreenRam, tileDest)

		RunDMAJobHi(TileJob)

		ldz #$00
	!tloop:
		RunDMAJobLo(TileJob)

		_add16(tileSource, src_x_size, tileSource)
		_add16(tileDest, Layout.LogicalRowSize, tileDest)

		inz
		cpz copy_height
		bne !tloop-

		// DMA attribute rows
		//
		_mul16(src_y_offset, src_x_size, src_attrib_ptr, attribSource)
		_add16(attribSource, src_x_offset, attribSource)
		_add16im(dst_offset, COLOR_RAM, attribDest)

		RunDMAJobHi(AttribJob)

		ldz #$00
	!aloop:
		RunDMAJobLo(AttribJob)

		_add16(attribSource, src_x_size, attribSource)
		_add16(attribDest, Layout.LogicalRowSize, attribDest)

		inz
		cpz copy_height
		bne !aloop-

		rts 

	TileJob:
		.byte $0A 						// Request format is F018A
		.byte $80
	tileSourceBank:
		.byte $00						// Source BANK
		.byte $81
	tileDestBank:
		.byte $00						// Dest BANK

		.byte $00 						// No more options
		.byte $00 						// Copy and last request
	tileLength:
		.word $0000						// Size of Copy

		//byte 04
	tileSource:	
		.byte $00,$00,$00				// Source

		//byte 07
	tileDest:
		.byte $00,$00,$00				// Destination & $ffff, [[Destination >> 16] & $0f]

	AttribJob:
		.byte $0A 						// Request format is F018A
		.byte $80
	attribSourceBank:
		.byte $00						// Source BANK
		.byte $81
	attribDestBank:
		.byte $00						// Dest BANK

		.byte $00 						// No more options
		.byte $00 						// Copy and last request
	attribLength:
		.word $0000						// Size of Copy

		//byte 04
	attribSource:	
		.byte $00,$00,$00				// Source

		//byte 07
	attribDest:
		.byte $00,$00,$00				// Destination & $ffff, [[Destination >> 16] & $0f]
	}
}

.segment Data "Layer Data"
Trans:			.fill LayerList.size(), LayerList.get(i).firstLayer ? $10|$04 : $90|$04
LogOffsLo:		.fill LayerList.size(), <LayerList.get(i).GotoXOffs
LogOffsHi:		.fill LayerList.size(), >LayerList.get(i).GotoXOffs
ChrOffsLo:		.fill LayerList.size(), <LayerList.get(i).ChrOffs
ChrOffsHi:		.fill LayerList.size(), >LayerList.get(i).ChrOffs
ChrSizeLo:		.fill LayerList.size(), <LayerList.get(i).ChrSize
ChrSizeHi:		.fill LayerList.size(), >LayerList.get(i).ChrSize

.segment Data "Layer Data YScroll"
YShift:			.fill LayerList.size(), $00
YMask:			.fill LayerList.size(), $ff

shiftMasks: .byte %11111111,%01111111,%00111111,%00011111,%00001111,%00000111,%00000011,%00000001,%00000000
shiftOffsets1: .byte (0<<5),(1<<5),(2<<5),(3<<5),(4<<5),(5<<5),(6<<5),(7<<5)
shiftOffsets2: .byte (0<<5),(7<<5)|$10,(6<<5)|$10,(5<<5)|$10,(4<<5)|$10,(3<<5)|$10,(2<<5)|$10,(1<<5)|$10

.segment BSS "Layer BSS"

ScrollUpdate:	.fill LayerList.size(), $00

RenderFuncLo:	.fill LayerList.size(), $00
RenderFuncHi:	.fill LayerList.size(), $00

ScrollXLo:		.fill LayerList.size(), $40
ScrollXHi:		.fill LayerList.size(), $01

FineScrollXLo:	.fill LayerList.size(), $40
FineScrollXHi:	.fill LayerList.size(), $01

ScrollYLo:		.fill LayerList.size(), $00
ScrollYHi:		.fill LayerList.size(), $00

}
