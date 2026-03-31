// ByteBoozer Decruncher    /HCL May.2003
// B2 Decruncher            December 2014

// ------------------------------------------------------------------
// Algorithm Overview
// ------------------------------------------------------------------
// This routine expands a ByteBoozer/B2 packed stream into memory using a
// backward-reference (LZ-style) token stream with bit-level coding.
//
// Stream layout (as consumed here):
// 1) 8-byte header (currently skipped by startup loop):
//    - 4 bytes: load address (little-endian)
//    - 4 bytes: original uncompressed size (little-endian)
// 2) Bit/byte-coded payload of tokens.
//
// Core decode loop (DLoop):
// - Read a control bit.
// - 0 => Literal token:
//        * Decode length with GetLen.
//        * Copy that many raw bytes from packed stream to output.
// - 1 => Match token:
//        * Decode length with GetLen.
//        * If length == $FF: EOF marker, stop.
//        * Decode offset class and offset bits using Tab + extra bits.
//        * Copy `length` bytes from already-written output at `put+offset`.
//
// Variable-length length coding (GetLen):
// - Starts from accumulator value 1.
// - Consumes 1-2 bits per iteration.
// - A terminating pattern ends the loop; otherwise more bits are rotated in.
// - Same decoder is used for literal lengths and match lengths.
//
// Bitstream handling:
// - bits = current bit reservoir byte.
// - GetNextBit shifts one bit from `bits`.
// - When exhausted, GetNewBits fetches next source byte via self-modifying
//   absolute reads ($feed,x placeholders patched at runtime).
//
// Offset decoding for matches:
// - A short selector chooses one of 8 table entries in Tab.
// - The selected encoding determines how many offset bits to read and whether
//   a low byte must be fetched from stream.
// - The computed offset is combined with `put` to form source pointer MLda.
//
// Copy semantics:
// - Literal loop writes bytes directly from stream to output.
// - Match loop copies from output history (back-reference), enabling repeated
//   sequences without storing them explicitly in packed data.
//
// Pointer model:
// - `put` is a 32-bit destination pointer in zero page.
// - It is advanced after each literal/match block with explicit carry-chain.
// - Source reads use Get1/Get2/Get3 self-modifying operands + X index;
//   GnbInc increments high bytes when X wraps.
//
// Termination:
// - Decoding ends when match length decodes to $FF, then End: rts.
// ------------------------------------------------------------------

// call: Y = AddrLo
//       X = AddrHi

//Variables..        				#Bytes
.const zp_base		= $02       	// -
.const bits			= zp_base   	// 1
.const put			= bits + 2 		// 4
.const size			= put + 4 		// 4
.const get			= size + 4 		// 4

// ------------------------------------------------------------------
// Offset decode table for match offsets.
// The selector built during match decoding indexes this table to choose
// how many offset bits to read and how to interpret short/long forms.
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
// GetNextBit macro:
// Shift one bit out of the local bit reservoir in `bits`.
// When reservoir empties, call GetNewBits to fetch another source byte.
.macro	GetNextBit()
{
	asl bits
	bne !+
	jsr GetNewBits
!:
}

// ------------------------------------------------------------------
// GetLen macro:
// Decode ByteBoozer variable-length code into A.
// This is used for both literal run lengths and match lengths.
.macro	GetLen()
{
	lda #1
_loop:
	// Read continuation flag; clear means end of code.
	GetNextBit()
	bcc _end
	// Read payload bit and rotate into current length accumulator.
	GetNextBit()
	rol
	bpl _loop
_end:
}

// ------------------------------------------------------------------
// GetNewBits routine:
// Read next packed byte into bit reservoir (`bits`) and advance source ptr.
// GnbInc handles page-crossing by incrementing high bytes of all source
// self-modifying absolute reads (Get1/Get2/Get3).
GetNewBits:
Get1:
	ldy $feed,x
	sty bits
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
// Decrunch entry point.
// Initializes self-modifying source pointers and expands packed stream
// into destination until end marker (match length $FF).
Decrunch:
	stx Get1+1
	stx Get2+1
	stx Get3+1
	sty Get1+2
	sty Get2+2
	sty Get3+2

	// Startup loop:
	// consume the 8-byte stream header (load address + original size).
	ldx #0
!:	
	jsr GetNewBits
	// sty put-1,x
	cpx #8
	bcc !-

	// Prime bit reservoir state and enter token decode loop.
	lda #$80
	sta bits
	// DLoop: dispatch each token as either literal run or match copy.
DLoop:
	GetNextBit()
	bcs Match

Literal:
	// Literal token:
	// decode literal run length and patch loop terminator compare.
	GetLen()
	sta LLen+1

	ldy #0
LLoop:
Get3:	
	// LLoop: copy literal bytes directly from packed stream to output.
	lda $feed,x
	inx
	bne L1
	jsr GnbInc
L1:
	sta (put),y
	iny
LLen:
	// Exit when Y reaches decoded literal length.
	cpy #0
	bne LLoop

	// Advance destination pointer by literal count (Y), with carry to high byte.
	clc
	tya
	adc put
	sta put
	bcc !+
	inc put+1
!:
	// If Y overflowed to zero, continue with next top-level token.
	iny
	beq DLoop

	// Has to continue with a match..

Match:
	// Match token:
	// decode match length and patch loop terminator compare.
	GetLen()
	sta MLen+1

	// Length 255 -> EOF
	cmp #$ff
	beq End

	// Decode offset selector (2 control bits + len class) and index table.
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

	// M_1: collect offset bits (< 8 or prelude for >= 8 case).
M_1:
	GetNextBit()
	rol
	bcs M_1
	bmi MShort
M8:
	// Long-offset path: fetch low byte from stream.
	eor #$ff
	tay
Get2:
	lda $feed,x
	inx
	bne *+5
	jsr GnbInc
	jmp Mdone
MShort:
	// Short-offset path: implicit high byte = $FF.
	ldy #$ff
Mdone:
	// Build source pointer for back-reference copy (MLda target).
	adc put
	sta MLda+1
	tya
	adc put+1
	sta MLda+2

	ldy #$ff
	// MLoop: copy match bytes from already written output (LZ back-reference).
MLoop:
	iny
MLda:
	lda $beef,y
	sta (put),y
MLen:
	// Exit when Y reaches decoded match length.
	cpy #0
	bne MLoop

	// Advance destination pointer by match length, carrying into high byte.
	tya
	adc put
	sta put
	bcc !+
	inc put+1
!:
	jmp DLoop

End:
	rts

