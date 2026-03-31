// ------------------------------------------------------------------
// 32-bit ByteBoozer/B2 decruncher for 45GS02.
//
// This is a 45GS02 adaptation of build/RCPacker/Decruncher.inc.
// Unlike the original self-modifying 16-bit variant, this version keeps
// packed source pointer, output pointer, and match source pointer as full
// 32-bit zero-page values and accesses memory through ((ptr)),z.
//
// Usage options:
// 1) Decomp32(src, dst)
//    - src points at the start of a .b2 stream, including 8-byte header.
//    - dst is the caller-chosen 32-bit destination address.
//    - The header is skipped; dst is not taken from header.
//
// 2) DecompRaw32(src, dst)
//    - src points directly at the compressed payload (header already skipped).
//    - dst is the caller-chosen 32-bit destination address.
//
// Clobbers: A, X, Y, Z, flags.
// ------------------------------------------------------------------

.const decomp_base         = $02
.const decomp_bits         = decomp_base       // 1 byte bit reservoir
.const decomp_put          = decomp_base + 2   // 4 byte output pointer
.const decomp_get          = decomp_base + 6   // 4 byte packed input pointer
.const decomp_msrc         = decomp_base + 10  // 4 byte match source pointer
.const decomp_len          = decomp_base + 14  // 1 byte loop counter
.const decomp_lenfield     = decomp_base + 15  // 1 byte raw decoded length field
.const decomp_offhi        = decomp_base + 16  // 1 byte signed high offset byte
.const decomp_offsign      = decomp_base + 17  // 1 byte sign-extension helper

.macro decomp_set32im(value, dst)
{
	lda #<value
	sta dst + 0
	lda #>value
	sta dst + 1
	lda #[value >> 16]
	sta dst + 2
	lda #[value >> 24]
	sta dst + 3
}

.macro decomp_inc32(ptr)
{
	inc ptr + 0
	bne !+
	inc ptr + 1
	bne !+
	inc ptr + 2
	bne !+
	inc ptr + 3
!:
}

.macro GetNextBit()
{
	asl decomp_bits
	bne !+
	jsr GetNewBits
!:
}

.macro GetLen()
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

.macro Decomp32(srcAddr, dstAddr)
{
	decomp_set32im(srcAddr, decomp_get)
	decomp_set32im(dstAddr, decomp_put)
	jsr DecompSkipHeader
	jsr Decomp
}

.macro DecompRaw32(srcAddr, dstAddr)
{
	decomp_set32im(srcAddr, decomp_get)
	decomp_set32im(dstAddr, decomp_put)
	jsr Decomp
}

// Offset decode table copied from the original decruncher.
DecompOffsetTab:
	.byte %11011111		// short: 3
	.byte %11111011		// short: 6
	.byte %00000000		// short: 8
	.byte %10000000		// short: 10
	.byte %11101111		// long: 4
	.byte %11111101		// long: 7
	.byte %10000000		// long: 10
	.byte %11110000		// long: 13

// Read next packed byte from 32-bit source pointer and advance it.
GetNextByte:
	ldz #$00
	lda ((decomp_get)),z
	decomp_inc32(decomp_get)
	rts

// Refill bit reservoir from packed stream.
GetNewBits:
	pha
	jsr GetNextByte
	rol
	sta decomp_bits
	pla
	rts

// Skip the 8-byte .b2 header (load address + original size).
DecompSkipHeader:
	ldx #$08
!:
	jsr GetNextByte
	dex
	bne !-
	rts

// Main decrunch routine. Expects decomp_get to point at payload and decomp_put to
// point at caller-selected destination address.
Decomp:
	lda #$80
	sta decomp_bits
	
DLoop:
	GetNextBit()
	bcs Match

Literal:
	GetLen()
	sta decomp_lenfield
	sta decomp_len

LLoop:
	jsr GetNextByte
	ldz #$00
	sta ((decomp_put)),z
	decomp_inc32(decomp_put)
	dec decomp_len
	bne LLoop
	lda decomp_lenfield
	cmp #$ff
	beq DLoop
	jmp Match

Match:
	GetLen()
	sta decomp_lenfield
	cmp #$ff
	lbeq DecompEnd

	clc
	adc #$01
	sta decomp_len

	// Build offset selector exactly like the original routine.
	lda decomp_lenfield
	cmp #$02
	lda #$00
	rol
	GetNextBit()
	rol
	GetNextBit()
	rol
	tay
	lda DecompOffsetTab,y
	beq M8

M_1:
	GetNextBit()
	rol
	bcs M_1
	bmi MShort

M8:
	// Long form: A becomes signed high byte, stream byte becomes low byte.
	eor #$ff
	sta decomp_offhi
	jsr GetNextByte
	jmp MDone

MShort:
	// Short form: signed high byte is always $FF, low byte remains in A.
	sta decomp_lenfield
	lda #$ff
	sta decomp_offhi
	lda decomp_lenfield

MDone:
	// msrc = put + signed 16-bit offset in (A=lo, decomp_offhi=hi).
	clc
	adc decomp_put + 0
	sta decomp_msrc + 0
	lda decomp_offhi
	adc decomp_put + 1
	sta decomp_msrc + 1

	lda decomp_offhi
	bmi MSignNegative
	lda #$00
	bra MSignReady
MSignNegative:
	lda #$ff
MSignReady:
	sta decomp_offsign
	lda decomp_offsign
	adc decomp_put + 2
	sta decomp_msrc + 2
	lda decomp_offsign
	adc decomp_put + 3
	sta decomp_msrc + 3

MLoop:
	ldz #$00
	lda ((decomp_msrc)),z
	sta ((decomp_put)),z
	decomp_inc32(decomp_msrc)
	decomp_inc32(decomp_put)
	dec decomp_len
	bne MLoop
	jmp DLoop

DecompEnd:
	rts