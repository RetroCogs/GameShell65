// ------------------------------------------------------------
//
.segment BSS "Keyb"
ScanResult:
	.byte	$ff,$ff,$ff,$ff,$ff,$ff,$ff,$ff

// ------------------------------------------------------------
//
.segment Code "Keyb"
ScanKeyMatrix: 
{
	ldx $dc02
	ldy $dc03
	phx
	phy

	lda #$ff
	sta $dc02
	lda #$00
	sta $dc03

    // Scan Keyboard Matrix
    //
    lda #%11111110
    sta $dc00
    ldy $dc01
    sty ScanResult+0
    sec

    .for (var i = 1 ; i < 8 ; i++) 
	{
        rol
        sta $dc00
        ldy $dc01
        sty ScanResult+i
    }

	ply
	plx
	stx $dc02
	sty $dc03

	rts
}

