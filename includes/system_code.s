//--------------------------------------------------------
//
.const FlEnableScreen = $01

// byte0
.const PAD_UP		= $01
.const PAD_DOWN		= $02
.const PAD_LEFT		= $04
.const PAD_RIGHT	= $08
.const PAD_FIRE		= $10
.const PAD_F5		= $20
.const PAD_F7		= $40

// byte1
.const PAD_ESC		= $01
.const PAD_YES		= $02

//--------------------------------------------------------
// System
//
.namespace System
{

//--------------------------------------------------------
//
.segment BSS "System ZP"
TopBorder:		.word $0000
BotBorder:		.word $0000
TextYPos:		.word $0000

IRQTopPos:		.word $0000
IRQBotPos:		.word $0000

Flags:			.byte $00

//--------------------------------------------------------
//
.segment BSS "System BSS"
DPad:				.word $00
DPadClick:			.word $00

//--------------------------------------------------------
//
.segment Code "System Code"
Initialization1:
{
	sei
	
	lda #$35
	sta $01

	enable40Mhz()
	enableVIC4Registers()
	disableCIAInterrupts()
	disableC65ROM()

	//Disable IRQ raster interrupts
	//because C65 uses raster interrupts in the ROM
	lda #$00
	sta $d01a

	//Disable hot register so VIC2 registers 
	lda #$80		
	trb $d05d			//Clear bit7=HOTREG

	// Inital IRQs at position $08 to avoid visible line 
	// when disabling the screen
	_set16im($08, IRQBotPos)

	cli

	rts
}

Initialization2:
{
	//Change VIC2 stuff here to save having to disable hot registers
	lda #%00000111
	trb $d016

    // Set RASLINE0 to 0 for the first VIC-II rasterline
    lda #%00111111
    trb $d06f

	// Disable VIC3 ATTR register to enable 8bit color
	lda #$20			//Clear bit5=ATTR
	trb $d031

	// Enable RAM palettes
	lda #$04			//Set bit2=PAL
	tsb $d030

	// Enable RRB double buffer
	lda #$80			//Clear bit7=NORRDEL
	trb $d051

	// Enable double line RRB to double the time for RRB operations 
	lda #$08			//Set bit3=V400
	tsb $d031
	lda #$40    		//Set bit6=DBLRR
	tsb $d051
	lda #$00    		//Set CHRYSCL = 0
	sta $d05b

	// Enable H320 mode, Super Extended Attributes and mono chars < $ff
	lda #%10000000		//Clear bit7=H640
	trb $d031

	// set offset to colour ram so we can use the first 8kb for something else and $10000-$60000 is a continuous playground without the colour ram in the middle
	// this causes a 1 pixel bug in the bottom right of the screen, so commenting it out again for now.
	VIC4_SetColorOffset(COLOR_OFFSET)

	lda #%00000101		//Set bit2=FCM for chars >$ff,  bit0=16 bit char indices
	tsb $d054

    // Disable RSTDELENS
    lda #%01000000
    trb $d05d

	rts
}

DisableScreen:
{
	lda #FlEnableScreen
	trb Flags

	lda #$00
	sta $d011
	rts
}

EnableScreen:
{
	lda #FlEnableScreen
	tsb Flags

	lda #$1b
	sta $d011
	rts
}

CenterFrameHorizontally:
{
	.var charXPos = Tmp				// 16bit

	_set16im(HORIZONTAL_CENTER, charXPos)
	_sub16(charXPos, Layout.LayoutWidth, charXPos)

	// SDBDRWDLSB,SDBDRWDMSB - Side Border size
	lda charXPos+0
	sta $d05c
	lda #%00111111
	trb $d05d
	lda charXPos+1
	and #%00111111
	tsb $d05d

	// TEXTXPOS - Text X Pos
	lda #%00001111
	trb $d04d

	lda charXPos+0
	sta $d04c
	lda charXPos+1
	and #%00001111
	sta $d04d

	rts
}

CenterFrameVertically: 
{
	.var verticalCenter = Tmp			// 16bit
	.var halfCharHeight = Tmp+2			// 16bit

	// The half height of the screen in rasterlines is (charHeight / 2) * 2
	_set16(Layout.LayoutHeight, halfCharHeight)

	// Figure out the vertical center of the screen

	// PAL values
	_set16im(304, verticalCenter)

	bit $d06f
	bpl isPal

	// NTSC values
	_set16im(242, verticalCenter)

isPal:

	_sub16(verticalCenter, halfCharHeight, TopBorder)
	_add16(verticalCenter, halfCharHeight, BotBorder)

	_set16(TopBorder, TextYPos)

	// hack!!
	// If we are running on real hardware then adjust char Y start up to avoid 2 pixel Y=0 bug
	lda $d60f
	and #%00100000
	beq !+

	_sub16im(TextYPos, 4, TextYPos)
	_sub16im(BotBorder, 1, BotBorder)

!:

	// Set these values on the hardware
	// TBDRPOS - Top Border
	lda TopBorder+0
	sta $d048
	lda #%00001111
	trb $d049
	lda TopBorder+1
	tsb $d049

	// BBDRPOS - Bot Border
	lda BotBorder+0
	sta $d04a
	lda #%00001111
	trb $d04b
	lda BotBorder+1
	tsb $d04b

	// TEXTYPOS - CharYStart
	lda TextYPos+0
	sta $d04e
	lda #%00001111
	trb $d04f
	lda TextYPos+1
	tsb $d04f

	_sub16im(TopBorder, 4, IRQTopPos)

	lsr IRQTopPos+1
	ror IRQTopPos+0

	_add16im(verticalCenter, (MAX_HEIGHT), IRQBotPos)

	lsr IRQBotPos+1
	ror IRQBotPos+0

	rts
}

// ------------------------------------------------------------
//
InitDPad: 
{
	lda #$00
	sta DPad+0
	sta DPad+1
	sta DPadClick+0
	sta DPadClick+1

	rts
}

UpdateDPad: 
{
	// Scan the keyboard
	jsr ScanKeyMatrix

	lda DPad+0
	sta oldDPad0
	lda DPad+1
	sta oldDPad1

	lda #$00
	sta DPad+0
	sta DPad+1

	jsr CheckForUp
	jsr CheckForDown
	jsr CheckForLeft
	jsr CheckForRight
	jsr CheckForFire
	jsr CheckForEsc
	jsr CheckForYes
	
	lda ScanResult+0
	and #$40
	bne _not_F5

	lda #PAD_F5
	tsb DPad+0

_not_F5:

	lda ScanResult+0
	and #$08
	bne _not_F7

	lda #PAD_F7
	tsb DPad+0

_not_F7:

	lda oldDPad0:#$00
	eor DPad+0
	and DPad+0
	sta DPadClick+0
	lda oldDPad1:#$00
	eor DPad+1
	and DPad+1
	sta DPadClick+1
	
	rts
}

// ------------------------------------------------------------
CheckForUp:
{
	lda #$01
	bit $dc00
	bne _not_j2_up

	lda #PAD_UP
	tsb DPad+0
	bra _done

_not_j2_up:

	lda ScanResult+1
	and #$04
	bne _not_A

	lda #PAD_UP
	tsb DPad+0
	bra _done

_not_A:

	lda ScanResult+6			// Up cursor = shift + down
	and #$10
	bne _not_up

	lda ScanResult+0
	and #$80
	bne _not_up

	lda #PAD_UP
	tsb DPad+0
	bra	_done

_not_up:

_done:

	rts
}

CheckForDown:
{
	lda #$02
	bit $dc00
	bne _not_j2_down

	lda #PAD_DOWN
	tsb DPad+0
	bra _done

_not_j2_down:

	lda ScanResult+1
	and #$10
	bne _not_Z

	lda #PAD_DOWN
	tsb DPad+0
	bra _done

_not_Z:

	lda ScanResult+6			// Down cursor = NOT shift + down
	and #$10
	beq _not_down

	lda ScanResult+0
	and #$80
	bne _not_down

	lda #PAD_DOWN
	tsb DPad+0
	bra	_done

_not_down:

_done:

	rts
}

CheckForLeft:
{
	lda #$04
	bit $dc00
	bne _not_j2_left

	lda #PAD_LEFT
	tsb DPad+0
	bra _done

_not_j2_left:

	lda ScanResult+5
	and #$80
	bne _not_less

	lda #PAD_LEFT
	tsb DPad+0
	bra _done

_not_less:

	lda ScanResult+6			// Right cursor = shift + right
	and #$10
	bne _not_left

	lda ScanResult+0
	and #$04
	bne _not_left

	lda #PAD_LEFT
	tsb DPad+0
	bra	_done

_not_left:

_done:

	rts
}

CheckForRight:
{
	lda #$08
	bit $dc00
	bne _not_j2_right

	lda #PAD_RIGHT
	tsb DPad+0
	bra _done

_not_j2_right:

	lda ScanResult+5
	and #$10
	bne _not_greater

	lda #PAD_RIGHT
	tsb DPad+0
	bra	_done

_not_greater:

	lda ScanResult+6			// Right cursor = NOT shift + right
	and #$10
	beq _not_right

	lda ScanResult+0
	and #$04
	bne _not_right

	lda #PAD_RIGHT
	tsb DPad+0
	bra	_done

_not_right:

_done:

	rts
}

CheckForFire:
{
	lda #$10
	bit $dc00
	bne _not_j2_fire

	lda #PAD_FIRE
	tsb DPad+0
	bra _done

_not_j2_fire:

	lda ScanResult+6
	and #$80
	bne _not_slash

	lda #PAD_FIRE
	tsb DPad+0
	bra _done

_not_slash:

	lda ScanResult+7
	and #$10
	bne _not_space

	lda #PAD_FIRE
	tsb DPad+0
	bra _done

_not_space:

_done:
	rts	
}

CheckForEsc:
{
	lda ScanResult+7
	and #$80
	bne _not_RunStop

	lda #PAD_ESC
	tsb DPad+1
	bra _done

_not_RunStop:

_done:
	rts
}

CheckForYes:
{
	lda ScanResult+3
	and #$02
	bne _not_Yes

	lda #PAD_YES
	tsb DPad+1
	bra _done

_not_Yes:

_done:
	rts
}

// ------------------------------------------------------------
}

