// ByteBoozer Decruncher    /HCL May.2003
// B2 Decruncher            December 2014

// call: Y = AddrLo
//       X = AddrHi

//Variables..        				#Bytes
.const zp_base		= $02       	// -
.const bits			= zp_base   	// 1
.const put			= bits + 2 		// 4
.const mlda			= put + 4 		// 4
.const get			= mlda + 4 		// 4

// ------------------------------------------------------------------
//
Tab:
	// Short offsets
	.byte %11011111 		// 3
	.byte %11111011 		// 6
	.byte %00000000 		// 8
	.byte %10000000 		// 10
	// Long offsets
	.byte %11101111 		// 4
	.byte %11111101 		// 7
	.byte %10000000 		// 10
	.byte %11110000 		// 13

// ------------------------------------------------------------------
//
.macro	GetNextBit()
{
	asl bits
	bne !+
	jsr GetNewBits
!:
}

// ------------------------------------------------------------------
//
.macro	GetLen()
{
	lda #1
_loop:
	GetNextBit()
	bcc _end
	GetNextBit()
	rol
	bpl _loop
_end:
}

// ------------------------------------------------------------------
//
GetNewBits:
Get1:
	ldz $feed,x
	stz bits
	rol bits
	inx
	bne GnbEnd
GnbInc:	
	inc Get1+2
	inc Get2+2
	inc Get3+2
GnbEnd:
	rts

// ------------------------------------------------------------------
//
Decrunch:
	stx Get1+1
	stx Get2+1
	stx Get3+1
	sty Get1+2
	sty Get2+2
	sty Get3+2

	// Get the first 2 DWORDS from the header: load address and original size
	ldx #0
!:	
	jsr GetNewBits
	// sty put-1,x
	cpx #8
	bcc !-

	// Start decrunching the payload
	lda #$80
	sta bits
DLoop:
	inc $d020
	
	GetNextBit()
	bcs Match

Literal:
	// Literal run.. get length.
	GetLen()
	sta LLen+1

	ldz #0
LLoop:
Get3:	
	lda $feed,x
	inx
	bne L1
	jsr GnbInc
L1:
	sta ((put)),z
	inz
LLen:
	cpz #0
	bne LLoop

	clc
	tza
	adc put
	sta put
	bcc !+
	inc put+1
	bne !+
	inc put+2
	bne !+
	inc	put+3
!:
	inz
	beq DLoop

	// Has to continue with a match..

Match:
	// Match.. get length.
	GetLen()
	sta MLen+1

	// Length 255 -> EOF
	cmp #$ff
	beq End

	// Get num bits
	cmp #2
	lda #0
	rol
	GetNextBit()
	rol
	GetNextBit()
	rol
	tay
	lda Tab,y
	beq M8

	// Get bits < 8
M_1:
	GetNextBit()
	rol
	bcs M_1
	bmi MShort
M8:
	// Get byte
	eor #$ff
	taz
Get2:
	lda $feed,x
	inx
	bne *+5
	jsr GnbInc
	jmp Mdone
MShort:
	ldz #$ff
Mdone:
	//clc
	adc put
	sta mlda+0
	tza
	adc put+1
	sta mlda+1
	lda put+2
	sta mlda+2
	lda put+3
	sta mlda+3

	ldz #$ff
MLoop:
	inz
MLda:
	lda ((mlda)),z
	sta ((put)),z
MLen:
	cpz #0
	bne MLoop

	//sec
	tza
	adc put
	sta put
	bcc !+
	inc put+1
	bne !+
	inc put+2
	bne !+
	inc put+3
!:
	jmp DLoop

End:
	rts

