// ------------------------------------------------------------------
// 32-bit ByteBoozer/B2 decruncher for 45GS02.
//
// This is a 45GS02 adaptation of build/RCPacker/Decruncher.inc.
// Unlike the original self-modifying 16-bit variant, this version keeps
// packed source pointer, output pointer, and match source pointer as full
// 32-bit zero-page values and accesses memory through ((ptr)),z.
//
// Algorithm overview:
//
// The compressed stream is a sequence of tokens encoded at bit granularity.
// Each top-level token begins with one control bit:
// - 0 = literal run
// - 1 = match/back-reference
//
// Literal token flow:
// 1) Decode a variable-length count with GetLen.
// 2) Copy exactly that many raw bytes from packed source to output.
// 3) If the literal length was $FF, the next token starts at DLoop.
// 4) Otherwise the format immediately continues with a match token, so the
//    code jumps directly into Match without consuming another top-level bit.
//
// Match token flow:
// 1) Decode a variable-length length field with GetLen.
// 2) If the field is $FF, this is EOF and the routine returns.
// 3) Otherwise actual copy length is `lenField + 1`.
// 4) Read selector bits to choose one of 8 offset encodings from
//    DecompOffsetTab.
// 5) Decode the match offset, build a 32-bit match source pointer, and copy
//    bytes from already-written output into the current output position.
//
// Length coding:
// - GetLen starts with A = 1.
// - It repeatedly reads a continuation bit.
// - If that bit is 0, decoding ends.
// - If it is 1, another bit is rotated into A and the process repeats.
// - This is the same coding used by the original ByteBoozer decruncher.
//
// Bit handling:
// - decomp_bits is a one-byte shift register.
// - GetNextBit shifts one bit out with ASL.
// - When the register empties, GetNewBits fetches a new packed byte and uses
//   ROL so the fetched byte enters with the same carry semantics as the
//   original routine.
// - GetNewBits must preserve A, because GetLen depends on A holding the
//   in-progress decoded length.
//
// 32-bit adaptation details:
// - decomp_get is the 32-bit packed source pointer.
// - decomp_put is the 32-bit output pointer.
// - decomp_msrc is the 32-bit source pointer used for match copies.
// - All memory accesses are done through ((ptr)),z with Z normally set to 0.
// - Pointer advancement is done explicitly with decomp_inc32.
// - Match offsets are decoded as signed 16-bit deltas and then sign-extended
//   into a full 32-bit address calculation against decomp_put.
//
// Header handling:
// - Decomp32 expects a full .b2 stream and skips the initial 8-byte header
//   (load address + original size).
// - DecompRaw32 expects decomp_get to already point at the packed payload.
// - In both cases, the destination address is supplied by the caller and is
//   not taken from the stream header.
//
// Important behavioral constraint:
// - This routine mirrors the original token sequencing exactly.
// - In particular, normal literal runs fall through into a match token rather
//   than restarting token decode, which is required to stay in sync with the
//   stream format.
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
	_set32im(srcAddr, decomp_get)
	_set32im(dstAddr, decomp_put)
	jsr DecompSkipHeader
	jsr Decomp
}

.macro DecompRaw32(srcAddr, dstAddr)
{
	_set32im(srcAddr, decomp_get)
	_set32im(dstAddr, decomp_put)
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
	_inc32(decomp_get)
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
	_inc32(decomp_put)
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
	_inc32(decomp_msrc)
	_inc32(decomp_put)
	dec decomp_len
	bne MLoop
	jmp DLoop

DecompEnd:
	rts