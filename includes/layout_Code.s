//--------------------------------------------------------
// Layout
//--------------------------------------------------------
.namespace Layout
{

//--------------------------------------------------------
//
.segment Code "Layout Code"

// ------------------------------------------------------------
ConfigureHW:
{
	ldx LayoutId

	_set16(LogicalRowSize, Tmp)

	// set HW row width (in bytes)
	_set16(LogicalRowSize, $d058)

	// Divide Tmp by 2 to get number of characters
	_half16(Tmp)

	// Shift Tmp+1 up by 4 
	asl Tmp+1
	asl Tmp+1
	asl Tmp+1
	asl Tmp+1

	// set HW number of characters
	lda Tmp+0
	sta $d05e
	lda $d063
	and #$cf
	ora Tmp+1
	sta $d063

	// set HW number of rows
	lda NumRows
	sta $d07b 

	jsr System.CenterFrameHorizontally
	jsr System.CenterFrameVertically

	rts
}

// ------------------------------------------------------------
// X = Layout Id
SelectLayout:
{
	stx LayoutId

	// grab the limits of the active layers
	lda LayerBegin,x
	sta BeginLayer
	lda LayerEnd,x
	sta	EndLayer

	// grab the logical size in bytes of each line
	lda LayerLogSizeLo,x
	sta LogicalRowSize+0
	lda LayerLogSizeHi,x
	sta LogicalRowSize+1

	lda LayoutWidthLo,x
	sta LayoutWidth+0
	lda LayoutWidthHi,x
	sta LayoutWidth+1

	lda LayoutHeightLo,x
	sta LayoutHeight+0
	lda LayoutHeightHi,x
	sta LayoutHeight+1

	lda LayoutNumRows,x
	sta NumRows

	// grab the pixie layer and then grab the offset in bytes to the gotox token
	lda LayerPixieId,x
	tay
	lda Layers.LogOffsLo,y
	sta PixieGotoOffs+0
	lda Layers.LogOffsHi,y
	sta PixieGotoOffs+1

	rts
}

// ------------------------------------------------------------
//
UpdateBuffers:
{
	// For each of the layers, call the render function
	//
	ldx BeginLayer

!layerloop:
	phx
	lda Layers.RenderFuncLo,x
	sta Tmp+0
	lda Layers.RenderFuncHi,x
	sta Tmp+1
	jsr (Tmp)
	plx

	inx
	cpx EndLayer
	bne !layerloop-

	// Update all of the (horizontal) scroll positions
 	jsr Layers.UpdateScrollPositions

	jsr Layers.UpdateData.InitEOL

	rts
}

.segment Data "Layout Data"
LayerBegin:		.fill LayoutList.size(), LayoutList.get(i).begin
LayerEnd:		.fill LayoutList.size(), LayoutList.get(i).end
LayerLogSizeLo:	.fill LayoutList.size(), <LayoutList.get(i).logicalSize
LayerLogSizeHi:	.fill LayoutList.size(), >LayoutList.get(i).logicalSize
LayerPixieId:	.fill LayoutList.size(), LayoutList.get(i).pixieId
LayoutWidthLo:	.fill LayoutList.size(), <LayoutList.get(i).width
LayoutWidthHi:	.fill LayoutList.size(), >LayoutList.get(i).width
LayoutHeightLo:	.fill LayoutList.size(), <LayoutList.get(i).height
LayoutHeightHi:	.fill LayoutList.size(), >LayoutList.get(i).height
LayoutNumRows:	.fill LayoutList.size(), LayoutList.get(i).numRows

.segment BSS "Layout BSS"
LayoutId:		.byte	$00

BeginLayer:		.byte	$00
EndLayer:		.byte	$00

LogicalRowSize:	.word 	$00
PixieGotoOffs:	.word 	$00

LayoutWidth:	.word 	$00
LayoutHeight:	.word 	$00

NumRows:		.byte 	$00

}
