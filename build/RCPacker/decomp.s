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
// - GetNewBits inlines GetNextByte to avoid the jsr/rts overhead (saves ~12
//   cycles per refill, i.e. roughly 1.5 cycles per bit on average).
//
// 32-bit adaptation details:
// - decomp_get is the 32-bit packed source pointer.
// - decomp_put is the 32-bit output pointer.
// - decomp_msrc is the 32-bit source pointer used for match copies.
// - All memory accesses are done through ((ptr)),z. Z is set to 0 once at
//   Decomp entry and is never modified by any instruction in the main loop,
//   so ldz #$00 before each access is unnecessary and is omitted.
// - Pointer advancement is done explicitly with decomp_inc32.
// - Match offsets are decoded as signed 16-bit deltas and then sign-extended
//   into a full 32-bit address calculation against decomp_put.
// - The sign-extension byte is held in X rather than a ZP variable
//   (decomp_offsign eliminated), saving 5 cycles per match.
// - LLoop and MLoop use Y as the iteration counter (dey vs dec zp saves
//   3 cycles per byte copied in both loops).
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
// decomp_offsign removed: sign extension byte is kept in X instead

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
	ldz #$00               // Z=0 for all 32-bit operations
	_set32im(srcAddr, decomp_get)
	_set32im(dstAddr, decomp_put)
	jsr DecompSkipHeader
	jsr Decomp
}

.macro DecompRaw32(srcAddr, dstAddr)
{
	ldz #$00               // Z=0 for all 32-bit operations
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
// Z must be 0 on entry (guaranteed by Decomp entry point; nothing in the main loop changes Z).
GetNextByte:
	lda ((decomp_get)),z
	_inc32(decomp_get)
	rts

// Refill bit reservoir from packed stream.
// Inlines GetNextByte to avoid jsr/rts overhead (saves 12 cycles per refill).
// Preserves A: GetLen depends on A holding the in-progress decoded length.
GetNewBits:
	pha
	lda ((decomp_get)),z    // Z=0 (set at Decomp entry, never modified)
	_inc32(decomp_get)
	rol
	sta decomp_bits
	pla
	rts

// Skip the 8-byte .b2 header (load address + original size).
DecompSkipHeader:
	ldz #$00               // Z must be 0 for ((decomp_get)),z accesses in GetNextByte
	ldx #$08
!:
	jsr GetNextByte
	dex
	bne !-
	rts

// Main decrunch routine. Expects decomp_get to point at payload and decomp_put to
// point at caller-selected destination address.
Decomp:
	ldz #$00               // Z must be 0 for all ((ptr)),z accesses; never modified in main loop
	lda #$80
	sta decomp_bits

DLoop:
	lda $d020
	inc
	and #$0f
	sta $d020
	
	GetNextBit()
	bcs Match

Literal:
	// Literal run: get length.
	GetLen()
	sta decomp_lenfield    // saved for $ff check after loop
	tay                    // Y = loop count (dey saves 3 cycles/iter vs dec zp)

LLoop:
	jsr GetNextByte
	sta ((decomp_put)),z   // Z=0
	_inc32(decomp_put)
	dey
	bne LLoop
	lda decomp_lenfield
	cmp #$ff
	beq DLoop

	// Has to continue with a match..

Match:
	// Match: get length.
	GetLen()
	sta decomp_lenfield
	// Length $FF -> EOF.
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

	// Get bits < 8.
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
	tax                    // save low offset byte in X (avoids ZP store/reload, saves 2 cycles)
	lda #$ff
	sta decomp_offhi
	txa                    // restore low offset byte

MDone:
	// msrc = put + signed 16-bit offset in (A=lo, decomp_offhi=hi).
	clc
	adc decomp_put + 0
	sta decomp_msrc + 0
	lda decomp_offhi
	adc decomp_put + 1
	sta decomp_msrc + 1

	// Sign-extend decomp_offhi into bytes 2 and 3 of decomp_msrc.
	// X holds the sign byte (0x00 or 0xFF); avoids ZP store/reload, saves 5 cycles.
	lda decomp_offhi
	bmi MSignNeg
	ldx #$00
	bra MSignReady
MSignNeg:
	ldx #$ff
MSignReady:
	txa                    // carry from byte 1 add still valid
	adc decomp_put + 2
	sta decomp_msrc + 2
	txa
	adc decomp_put + 3
	sta decomp_msrc + 3

	ldy decomp_len         // Y = loop count for dey (saves 3 cycles/iter vs dec zp)
MLoop:
	lda ((decomp_msrc)),z  // Z=0
	sta ((decomp_put)),z
	_inc32(decomp_msrc)
	_inc32(decomp_put)
	dey
	bne MLoop
	jmp DLoop

DecompEnd:
	rts