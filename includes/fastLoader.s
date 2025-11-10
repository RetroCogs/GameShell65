// -----------------------------------------------------------------------------------------------
// original code from MirageBD https://github.com/MirageBD
//
// -----------------------------------------------------------------------------------------------
//
// 0. fastload_request = 1 (fl_new_request)
// 1. fastload_request = 2 (fl_directory_scan)
// 2. fastload_request = 3 (fl_read_file_block)
// 3. jump to 2

// ----------------------------------------------------------------------------------------------------

// D080        IRQ     LED     MOTOR   SWAP    SIDE    DS      DS      DS      
//
// IRQ         The floppy controller has generated an interrupt (read only). Note that interrupts are not currently implemented on the 45GS27
// LED         Drive LED blinks when set
// MOTOR       Activates drive motor and LED (unless LED signal is also set, causing the drive LED to blink)
// SWAP        Swap upper and lower halves of data buffer (i.e. invert bit 8 of the sector buffer)
// DS          Drive select (0 to 7). Internal drive is 0. Second floppy drive on internal cable is 1. Other values reserved for C1565 external drive interface

// ----------------------------------------------------------------------------------------------------

// D081        WRCMD   RDCMD   FREE    STEP    DIR     ALGO    ALT     NOBUF   
//
// WRCMD       Command is a write operation if set
// RDCMD       Command is a read operation if set
// FREE        Command is a free-format (low level) operation
// STEP        Writing 1 causes the head to step in the indicated direction
// DIR         Sets the stepping direction (inward vs
// ALGO        Selects reading and writing algorithm (currently ignored)
// ALT         Selects alternate DPLL read recovery method (not implemented)
// NOBUF       Reset the sector buffer read/write pointers

// ----------------------------------------------------------------------------------------------------

// D082        BUSY    DRQ     EQ      RNF     CRC     LOST    PROT    TK0     
//
// BUSY        F011 FDC busy flag (command is being executed) (read only)
// DRQ         F011 FDC DRQ flag (one or more bytes of data are ready) (read only)
// EQ          F011 FDC CPU and disk pointers to sector buffer are equal, indicating that the sector buffer is either full or empty. (read only)
// RNF         F011 FDC Request Not Found (RNF), i.e., a sector read or write operation did not find the requested sector (read only)
// CRC         F011 FDC CRC check failure flag (read only)
// LOST        F011 LOST flag (data was lost during transfer, i.e., CPU did not read data fast enough) (read only)
// PROT        F011 Disk write protect flag (read only)
// TK0         F011 Head is over track 0 flag (read only)

// ----------------------------------------------------------------------------------------------------

// D083        RDREQ   WTREQ   RUN     WGATE   DISKIN  INDEX   IRQ     DSKCHG  
// 
// RDREQ       F011 Read Request flag, i.e., the requested sector was found during a read operation (read only)
// WTREQ       F011 Write Request flag, i.e., the requested sector was found during a write operation (read only
// RUN         F011 Successive match. A synonym of RDREQ on the 45IO47 (read only)
// WGATE       F011 write gate flag. Indicates that the drive is currently writing to media. Bad things may happen if a write transaction is aborted (read only)
// DISKIN      F011 Disk sense (read only)
// INDEX       F011 Index hole sense (read only)
// IRQ         The floppy controller has generated an interrupt (read only). Note that interrupts are not currently implemented on the 45GS27.
// DSKCHG      G F011 disk change sense (read only)

// ----------------------------------------------------------------------------------------------------

// D084        TRACK
// D085        SECTOR
// D086        SIDE
// D087        DATA
// D088        CLOCK
// D089        STEP
// D08A        PCODE

// ----------------------------------------------------------------------------------------------------
  /** Drive select (0 to 7). Internal drive is 0. Second floppy drive on internal cable 
      is 1. Other values reserved for C1565 external drive interface. */
.const FDC_DS_MASK = %00000111
  /** Directly controls the SIDE signal to the floppy drive, i.e., selecting which side of the media is active. */
.const FDC_SIDE_MASK = %00001000
  /** Swap upper and lower halves of data buffer (i.e. invert bit 8 of the sector buffer) */
.const FDC_SWAP_MASK = %00010000
  /** Activates drive motor and LED (unless LED signal is also set, causing the drive LED to blink) */
.const FDC_MOTOR_MASK = %00100000
  /** Drive LED blinks when set */
.const FDC_LED_MASK = %01000000
  /** The floppy controller has generated an interrupt (read only). Note that in- terrupts are not currently implemented on the 45GS27. */
.const FDC_IRQ_MASK = %10000000

// $d082
//
  /** F011 Head is over track 0 flag (read only) */
.const FDC_TK0_MASK = $01
  /** F011 FDC CRC check failure flag (read only) */
.const FDC_CRC_MASK = $08
  /** F011 FDC Request Not Found (RNF), i.e., a sector read or write operation did not find the requested sector (read only) */
.const FDC_RNF_MASK = $10
  /** F011 FDC CPU and disk pointers to sector buffer are equal, indicating that the sector buffer is either full or empty. (read only) */
.const  FDC_EQ_MASK = $20
  /** F011 FDC DRQ flag (one or more bytes of data are ready) (read only) */
.const FDC_DRQ_MASK = $40
  /** F011 FDC busy flag (command is being executed) (read only) */
.const FDC_BUSY_MASK = $80

// $d083
.const FDC_WGATE_MASK = $10
  /** F011 Write Request flag, i.e., the requested sector was found during a write operation (read only) */
.const FDC_WTREQ_MASK = $40
  /** F011 Read Request flag, i.e., the requested sector was found during a read operation (read only) */
.const FDC_RDREQ_MASK = $80

.const FDC_CMD_CLR_BUFFER_PTRS = $01
.const FDC_CMD_STEP_OUT = $10
.const FDC_CMD_STEP_IN = $18
.const FDC_CMD_SPINUP = $20
.const FDC_CMD_READ_SECTOR = $40
.const FDC_CMD_WRITE_SECTOR = $84

// ----------------------------------------------------------------------------------------------------

.enum
{
	FL_IDLE,			// $00 = fl_idle				// idle
	FL_SEEK_TRACK_0,	// $01 = fl_seek_track_0		// seek to track 0
	FL_NEW_REQUEST,		// $02 = fl_new_request			// requested
	FL_DIR_SCAN,		// $03 = fl_directory_scan		// scan directory
	FL_READING_SECTOR,	// $04 = fl_reading_sector		// track stepping/sector reading state
	FL_READ_FILE_BLOCK,	// $05 = fl_read_file_block		// read file block
	FL_WRITE_BLOCK,		// $06 = fl_write_block			// write just one sector (for saving scores)
	FL_STATE_COUNT,

	FL_ERROR = $80		// $80 = File not found			// file not found
}

// ----------------------------------------------------------------------------------------------------
//
fl_init:
{
	lda #$80
	trb $d696									// disable auto-tune

	lda #FDC_LED_MASK|FDC_MOTOR_MASK			// Start motor
	sta $d080


	lda #FL_SEEK_TRACK_0						// Start with seeking to track 0
	sta fastload_request

	lda #$00
	sta fl_current_track

	sta fl_file_next_track
	sta fl_file_next_sector
	sta fl_prev_track
	sta fl_prev_sector
	sta fl_prev_side

	rts
}

// ----------------------------------------------------------------------------------------------------
//
fl_exit:
{
	lda #$00				// Stop motor and turn off LED
	sta $d080

	rts
}

// ----------------------------------------------------------------------------------------------------
//
fl_set_filename:
{
	stx fl_fnptr
	sty fl_fnptr+1

	ldx #$0f
	lda #$a0				// pad name with $a0
clearfilename:	
	sta fastload_filename,x
	dex
	bpl clearfilename

	ldx #$ff
filenamecopyloop:
	inx
	cpx #$10
	beq endofname

	lda fl_fnptr:$babe,x
	beq endofname
	sta fastload_filename,x
	bne filenamecopyloop

endofname:
	inx
	stx fastload_filename_len

	rts
}

// ----------------------------------------------------------------------------------------------------
//
.macro LoadFile(addr, fname)
{
	ldx #<fname
	ldy #>fname
	jsr fl_set_filename
	
	// Set load address (32-bit)
	// = $07ff ($0801 - 2 bytes for BASIC header)
	lda #<addr
	sta fastload_address+0
	lda #>addr
	sta fastload_address+1
	lda #[addr >> 16]
	sta fastload_address+2
	lda #[addr >> 24]
	sta fastload_address+3

	lda #0
	sta fastload_dowrite

	// Give the fastload time to get itself sorted
	// (largely seeking to track 0 on first access)
	jsr fl_waiting

	// Request fastload job
	lda #FL_NEW_REQUEST
	sta fastload_request
	
	jsr fl_waiting
}

// ----------------------------------------------------------------------------------------------------
//
.macro UpdateFile(addr, fname)
{
	ldx #<fname
	ldy #>fname
	jsr fl_set_filename
	
	// Set load address (32-bit)
	// = $07ff ($0801 - 2 bytes for BASIC header)
	lda #<addr
	sta fastload_address+0
	lda #>addr
	sta fastload_address+1
	lda #[addr >> 16]
	sta fastload_address+2
	lda #[addr >> 24]
	sta fastload_address+3

	lda #1
	sta fastload_dowrite

	// Give the fastload time to get itself sorted
	// (largely seeking to track 0 on first access)
	jsr fl_waiting

	// Request fastload job
	lda #FL_NEW_REQUEST
	sta fastload_request
	
	jsr fl_waiting

}

// ----------------------------------------------------------------------------------------------------
//
fl_waiting:
	jsr fastload_update			// Then just wait for the request byte to

	lda fastload_request		// go back to $00, or to report an error by having the MSB
	bmi fl_error				// set. The request value will continually update based on the
	bne fl_waiting				// state of the loading.

	rts

fl_error:
	inc $d021
	bra fl_error

// ------------------------------------------------------------------------------------------------------------------------------
// Actual fast-loader code
// ------------------------------------------------------------------------------------------------------------------------------

.const fl_sector_buffer	= $0200

fastload_dowrite:
		.byte 0

fastload_filename:
		.fill 16,0

fastload_filename_len:
		.byte 0

fastload_address:
		.byte 0, 0, 0, 0

fastload_request:
		.byte FL_SEEK_TRACK_0			// Start with seeking to track 0

fastload_request_stashed:				// Remember the state that requested a sector read
		.byte FL_IDLE

		// Variables for the logical track and sector of the next 256 byte block of the file.
		// These have to get translated into the physical track and sector of the drive, which like the 1581,
		// stores two blocks in each physical sector.

fl_current_track:
		.byte 0
fl_file_next_track:
		.byte 0
fl_file_next_sector:
		.byte 0

fl_prev_track:
		.byte 0
fl_prev_sector:
		.byte 0
fl_prev_side:
		.byte 0

// ------------------------------------------------------------------
// Utility functions
//

fl_locate_track:
{
	lda $d084						// Check if we are already on the correct track/side
	cmp fl_current_track			// and if not, select/step as required
	beq found_track
	bcc step_in

step_out:
	lda #FDC_CMD_STEP_IN			// We need to step first
	sta $d081
	inc fl_current_track
	clc
	rts

step_in:
	lda #FDC_CMD_STEP_OUT			// We need to step first
	sta $d081
	dec fl_current_track
	clc
	rts
  
found_track:
	sec
	rts
}

// -----------------------------------
//
fl_check_same_as_prev:
{
	// See if the newly requested track / side / sector are
	// the same as last time, if not then we must issue read command
	//
	lda $d084
	cmp fl_prev_track
	bne not_same
	lda $d086
	cmp fl_prev_side
	bne not_same
	lda $d085
	cmp fl_prev_sector
	bne not_same
	sec
	rts

not_same:
	clc
	rts
}

// -----------------------------------
//
fl_select_side1:  

	lda #$01
	sta $d086  										// requested side
	lda #FDC_LED_MASK|FDC_MOTOR_MASK				// Sides are inverted on the 1581
	sta $d080  										// physical side selected of mechanical drive
	rts

// -----------------------------------
//
fl_select_side0:  

	lda #$00
	sta $d086 										// requested side
	lda #FDC_LED_MASK|FDC_MOTOR_MASK|FDC_SIDE_MASK	// Sides are inverted on the 1581
	sta $d080										// physical side selected of mechanical drive
	rts

// -----------------------------------
//
fl_set_done_state:

	lda #FL_IDLE
	sta fastload_request
	lda #$00										// turn off motor and LED
	sta $d080
	rts  

// -----------------------------------
//
fl_set_error_state:

	lda #FL_ERROR									// $80 = File not found
	sta fastload_request
	lda #$00										// turn off motor and LED
	sta $d080
	rts  

// -----------------------------------
//
fl_logical_to_physical_sector:
	lda $d084						// Remember current loaded sector, so that we can optimise when asked
	sta fl_prev_track				// to read other half of same physical sector
	lda $d085
	sta fl_prev_sector
	lda $d086
	sta fl_prev_side
									// Convert 1581 sector numbers to physical ones on the disk.
	jsr fl_select_side0				// Side = 0

	lda fl_file_next_track
	dec								// Track = Track - 1
	sta $d084

	lda fl_file_next_sector			// Sector = 1 + (Sector/2)
	lsr
	inc
	cmp #11							// If sector > 10, then sector=sector-10, side=1
	bcs fl_on_second_side			// but sides are inverted
	sta $d085

	rts

fl_on_second_side:
	sec
	sbc #10
	sta $d085
	jsr fl_select_side1
	rts

// -----------------------------------
//
fl_read_next_sector:
	lda fl_file_next_track			// Check if we reached the end of the file first
	bne fl_not_end_of_file
	rts

fl_not_end_of_file: 
	jsr fl_logical_to_physical_sector // Read next sector of file 
	jsr fl_read_sector
	rts

// -----------------------------------
//
fl_read_sector:
{
	lda fastload_request			// Remember the state that we need to return to
	sta fastload_request_stashed
	
	lda #FL_READING_SECTOR			// and then set ourselves to the track stepping/sector reading state
	sta fastload_request

	jmp fl_reading_sector
}

// -----------------------------------
//
fl_dma_read_bytes:

	inc $d020

	lda fastload_address+3			// Update destination address
	asl
	asl
	asl
	asl
	sta fl_data_read_dmalist+2		// update destination MB
	lda fastload_address+2
	lsr
	lsr
	lsr
	lsr
	ora fl_data_read_dmalist+2
	sta fl_data_read_dmalist+2		// update destination MB
	lda fastload_address+2
	and #$0f
	sta fl_data_read_dmalist+12		// update Dest bank
	lda fastload_address+1
	sta fl_data_read_dmalist+11		// update Dest Address high
	lda fastload_address+0
	sta fl_data_read_dmalist+10		// update Dest Address low

	lda #$00						// Copy sector buffer data to final address
	sta $d704
	lda #>fl_data_read_dmalist
	sta $d701
	lda #<fl_data_read_dmalist
	sta $d705

	clc
	lda fastload_address+0			// Update load address
	adc fl_bytes_to_copy
	sta fastload_address+0
	lda fastload_address+1
	adc #0
	sta fastload_address+1
	lda fastload_address+2
	adc #0
	sta fastload_address+2
	lda fastload_address+3
	adc #0
	sta fastload_address+3

	jsr fl_read_next_sector			// Schedule reading of next block

	dec $d020

	rts

// -----------------------------------
// Copy fl_sector_buffer -> load destination
//
fl_data_read_dmalist:
	.byte $0b                		// F011A type list
	.byte $81,$00             		// Destination MB
	.byte 0                     	// no more options
	.byte 0                      	// copy
fl_bytes_to_copy:
	.word 0                       	// size of copy
fl_read_page:
	.word (fl_sector_buffer+2)		// Source address. +2 is to skip track/header link
	.byte $00						// Source bank
	.word 0							// Dest address
	.byte $00             			// Dest bank
	.byte $00                		// sub-command
	.word 0							// modulo (unused)

// -----------------------------------
// 
fl_copy_sector_to_buffer:
	inc $d020

	lda #$80						// Make sure FDC sector buffer is selected
	trb $d689
	lda #$00						// Copy FDC data to our buffer
	sta $d704
	lda #>fl_sector_read_dmalist
	sta $d701
	lda #<fl_sector_read_dmalist
	sta $d705

	dec $d020
	
	rts

// -----------------------------------
// Copy FDC buffer into fl_sector_buffer
//
fl_sector_read_dmalist:
	.byte $0b						// F011A type list
	.byte $80,$ff					// MB of FDC sector buffer address ($FFD6C00)
	.byte 0							// no more options
	.byte 0							// copy
	.word 512						// size of copy
	.word $6c00						// low 16 bits of FDC sector buffer address
	.byte $0d						// next 4 bits of FDC sector buffer address
	.word fl_sector_buffer			// Dest address 
	.byte $00						// Dest bank
	.byte $00						// sub-command
	.word 0							// modulo (unused)

// ------------------------------------------------------------------
//
//
fastload_update:
{
	lda fastload_request		// are we in idle state?
	bne not_idle				// nope, go and check if the FDC is busy
	rts							// yep, back out

not_idle:
	lda $d082					// is the FDC busy?
	and #FDC_BUSY_MASK
	bne is_busy

	lda $d083					// is FDC writing?
	and #FDC_WGATE_MASK
	bne is_busy
	
	bra fl_fdc_not_busy			// nope, continue with request

is_busy:
	rts							// yep, back out

fl_fdc_not_busy:  
	lda fastload_request		// are we in error state?
	bpl fl_not_in_error_state	// nope, continue
	rts							// yep, back out

fl_not_in_error_state:
	cmp #FL_STATE_COUNT			// is the request smaller than 6 (change to 8 when IFFL support added)?
	bcc fl_job_ok				// yep, continue
	rts							// nope, something must have gone wrong ($80 (file not found) is bigger than number testing against)

fl_job_ok:  
	asl							// shift state left one bit, so that we can use it as a lookup
	tax							// into a jump table. Everything else is handled by the jump table
	jmp (fl_jumptable,x)

fl_jumptable:
	.word fl_idle				// FL_IDLE
	.word fl_seek_track_0		// FL_SEEK_TRACK_0
	.word fl_new_request		// FL_NEW_REQUEST
	.word fl_directory_scan		// FL_DIR_SCAN
	.word fl_reading_sector		// FL_READING_SECTOR
	.word fl_read_file_block	// FL_READ_FILE_BLOCK
	.word fl_write_block		// FL_WRITE_BLOCK
}

// -----------------------------------
// FL_IDLE
//
fl_idle:
{
	rts
}

// -----------------------------------
// FL_SEEK_TRACK_0
//
fl_seek_track_0:
{
	lda $d082
	and #FDC_TK0_MASK				// TK0 - F011 Head is over track 0 flag (read only)
	beq not_on_track_0

	lda #FL_IDLE
	sta fastload_request
	lda #$00
	sta fl_current_track
	rts

not_on_track_0:
	lda #FDC_CMD_STEP_OUT			// Step back towards track 0
	sta $d081
	rts
}

// -----------------------------------
// FL_NEW_REQUEST
//
fl_new_request:
{
	lda #FL_DIR_SCAN				// Acknowledge fastload request
	sta fastload_request

	lda #40-1						// Request Track 40 Sector 3 to start directory scan
	sta $d084						// (remember we have to do silly translation to real sectors)
	lda #(3/2)+1
	sta $d085

	jsr fl_select_side0

	jsr fl_read_sector				// Request read

	rts
}

// -----------------------------------
// FL_DIR_SCAN
//
fl_directory_scan:
{
	jsr fl_copy_sector_to_buffer	// Check if our filename we want is in this sector

									// (XXX we scan the last BAM sector as well, to keep the code simple.)
									// filenames are at offset 4 in each 32-byte directory entry, padded at
									// the end with $A0
	lda #<fl_sector_buffer
	sta fl_buffaddr
	lda #>fl_sector_buffer
	sta fl_buffaddr+1

fl_check_logical_sector:
	ldx #$05
fl_filenamecheckloop:
	ldy #$00

fl_check_loop_inner:

	lda fl_buffaddr:fl_sector_buffer+$100,x

	cmp fastload_filename,y 
	bne fl_filename_differs
	inx
	iny
	cpy #$10
	bne fl_check_loop_inner
		
	// Filename matches
	//
	txa
	sec
	sbc #$12
	tax
	lda fl_buffaddr+1
	cmp #>fl_sector_buffer
	bne fl_file_in_2nd_logical_sector

	lda fl_sector_buffer,x			// Y=Track, A=Sector
	tay
	lda fl_sector_buffer+1,x
	jmp fl_got_file_track_and_sector

fl_file_in_2nd_logical_sector:
	lda fl_sector_buffer+$100,x		// Y=Track, A=Sector
	tay
	lda fl_sector_buffer+$101,x

fl_got_file_track_and_sector:
	sty fl_file_next_track			// Store track and sector of file
	sta fl_file_next_sector
	
	lda fastload_dowrite
	beq fl_do_read

	lda #FL_WRITE_BLOCK
	sta fastload_request

	jsr fl_read_next_sector			// Request reading of next track and sector

	rts

fl_do_read:
	lda #FL_READ_FILE_BLOCK			// Advance to next state (6=fl_iffl_read_file_block_init)
	sta fastload_request

	jsr fl_read_next_sector			// Request reading of next track and sector
	rts
  
fl_filename_differs:
	cpy #$10						// Skip same number of chars as though we had matched
	beq fl_end_of_name
	inx
	iny
	jmp fl_filename_differs

fl_end_of_name:
	txa								// Advance to next directory entry
	clc
	adc #$10
	tax
	bcc fl_filenamecheckloop
	inc fl_buffaddr+1
	lda fl_buffaddr+1
	cmp #(>fl_sector_buffer)+1
	bne fl_checked_both_halves
	jmp fl_check_logical_sector

fl_checked_both_halves: 
	inc $d085						// No matching name in this 512 byte sector.
	lda $d085						// Load the next one, or give up the search
	cmp #11
	bne fl_load_next_dir_sector
									// Ran out of sectors in directory track
									// (XXX only checks side 0, and assumes DD disk)

	// !!!!!!!!!!!!!!!!!!!
	// Mark load as failed
	//
	jsr fl_set_error_state			
	rts

fl_load_next_dir_sector:  
	jsr fl_read_sector				// Request read. No need to change state
	rts
}

// -----------------------------------
// FL_READING_SECTOR
//
fl_reading_sector:

	// Do the stepping logic to get to the requested track
	// (returns with C = found track)
	//
	jsr fl_locate_track
	bcs fl_on_correct_track
	rts

fl_on_correct_track:

	jsr fl_check_same_as_prev
	bcc fl_not_prev_sector

	// We are being asked to read the sector we already have in the buffer
	//
	// Restore the previous command (before the read sector)
	//
	lda fastload_request_stashed	
	sta fastload_request			

	// Jump immediately to the correct routine
	jmp fastload_update.fl_fdc_not_busy

fl_not_prev_sector: 

	// ISSUE ACTUAL READ COMMAND
	//
	lda #FDC_CMD_READ_SECTOR				
	sta $d081

	// Now that we are finally reading the sector,
	// restore the stashed state ID
	//
	lda fastload_request_stashed	
	sta fastload_request			

	rts

// -----------------------------------
// FL_READ_FILE_BLOCK
//
fl_read_file_block:
									// We have a sector from the floppy drive.
									// Work out which half and how many bytes, and copy them into place.

	jsr fl_copy_sector_to_buffer	// Get sector from FDC

	lda #254						// Assume full sector initially
	sta fl_bytes_to_copy

	lda fl_file_next_sector			// Work out which half we care about
	and #$01
	bne fl_read_from_second_half	// odd next sector number, so second half

	lda #(>fl_sector_buffer)+0		// fl_read_from_first_half
	sta fl_read_page+1

	lda fl_sector_buffer+1
	sta fl_file_next_sector

	lda fl_sector_buffer+0
	sta fl_file_next_track

	// if next track is 0 then this is a partial sector and 'sector' now becomes
	// the number of bytes left in this sector
	//
	bne fl_1st_half_full_sector		

	lda fl_sector_buffer+1			// fl_1st_half_partial_sector. track is 0, so sector contains number of bytes left
	sec								// subtract 1, because the byte that contains the size is included
	sbc #1
	sta fl_bytes_to_copy	

	jsr fl_set_done_state			// Mark end of loading

fl_1st_half_full_sector:
	jmp fl_dma_read_bytes			// copy bytes to destination

fl_read_from_second_half:
	lda #(>fl_sector_buffer)+1
	sta fl_read_page+1

	lda fl_sector_buffer+$101
	sta fl_file_next_sector

	lda fl_sector_buffer+$100
	sta fl_file_next_track

	bne fl_2nd_half_full_sector

fl_2nd_half_partial_sector:

	// fl_2nd_half_partial_sector. next track is 0, so sector contains number of bytes left
	//
	lda fl_sector_buffer+$101
	sec								// subtract 1, because the byte that contains the size is included
	sbc #1
	sta fl_bytes_to_copy

	jsr fl_set_done_state			// Mark end of loading

fl_2nd_half_full_sector:
	jmp fl_dma_read_bytes			// copy bytes to destination

// -----------------------------------
//
set_fdc_swap:
{
	lda #FDC_CMD_CLR_BUFFER_PTRS			// reset FDC read pointer
	sta $d081

	lda #FDC_SWAP_MASK						// disable swap
	trb $d080

	rts
}

wait_for_busy_clear:
{
not_idle:
	lda $d082					// is the FDC busy?
	and #FDC_BUSY_MASK
	bne not_idle

	lda $d083					// is FDC writing?
	and #FDC_WGATE_MASK
	bne not_idle

	rts
}  

// -----------------------------------
// FL_WRITE_BLOCK
//
fl_write_block:
{
	.var track 			= Tmp1 + 0			// 8 bit
	.var sector 		= Tmp1 + 1			// 8 bit
	.var side 			= Tmp1 + 2			// 8 bit

	// Ensure the FDC buffer is filled with the current sector pair AND the data to update
	//
	//
	{
		// FDC buffer has sector pair in it, copy into our fl_sector_buffer
		//
		jsr fl_copy_sector_to_buffer

		// Let's set writing to first half by default

		lda #(>fl_sector_buffer)+0		// fl_write_from_first_half
		sta fl_write_page+1

		// Work out which half we care about
		lda fl_file_next_sector			
		and #$01
		beq fl_write_to_first_half

		lda #(>fl_sector_buffer)+1		// fl_write_from_first_half
		sta fl_write_page+1

	fl_write_to_first_half:

		// Copy the bytes to update into the correct half of fl_sector_buffer
		//
		jsr fl_dma_write_bytes

		// Copy the sector buffer into the FDC sector buffer
		jsr fl_copy_buffer_to_sector
	}

	// From the logical track / sector information of the sector to be modified
	// figure out the physical track / sector / side
	//
	lda fl_file_next_track			// logical (1-80) to physical (0-79) track
	dec
	sta track

	lda fl_file_next_sector			// logical (0-19) to physical (1-20) sector
	lsr
	inc
	cmp #11
	bcs fl_on_side_1

	sta sector

	lda #0							// sectors 1-10 are on side 0
	sta side

	lda $d080
	ora #FDC_SIDE_MASK; 			// select side 0
	sta $d080

	bra fl_got_track_sector

fl_on_side_1:
	sec
	sbc #10
	sta sector

	lda #1 							// sectors 11-20 are on side 1
	sta side

	lda $d080
	and #~FDC_SIDE_MASK; 			// select side 1
	sta $d080

fl_got_track_sector:

	jsr step_to_track				// moves the head to 'track'

	lda track
	sta $d084
	lda sector
	sta $d085
	lda side
	sta $d086

	jsr set_fdc_swap 

	// ISSUE ACTUAL WRITE COMMAND
	//
	lda #FDC_CMD_WRITE_SECTOR					
	sta $d081

	jsr wait_for_busy_clear

	// check for error
	lda $d082
	and #(FDC_CRC_MASK | FDC_RNF_MASK)
	beq !+

	infLoop()

!:

	jsr fl_set_done_state			// Mark end of loading

	rts


	step_to_track:
	{
		lda track						// Check if we are already on the correct track/side
		cmp fl_current_track			// and if not, select/step as required
		beq found_track
		bcc step_in

	step_out:
		lda #FDC_CMD_STEP_IN			// We need to step first
		sta $d081
		inc fl_current_track
		bra step_to_track

	step_in:
		lda #FDC_CMD_STEP_OUT			// We need to step first
		sta $d081
		dec fl_current_track
		bra step_to_track
	
	found_track:	
		rts
	}
}

// -----------------------------------
//
fl_dma_write_bytes:

	inc $d020

	lda #$00						// Copy sector buffer data to final address
	sta $d704
	lda #>fl_data_write_dmalist
	sta $d701
	lda #<fl_data_write_dmalist
	sta $d705

	dec $d020

	rts

// -----------------------------------
// Copy fl_sector_buffer -> load destination
//
fl_data_write_dmalist:
	.byte $0b                		// F011A type list
	.byte $81,$00             		// Destination MB
	.byte 0                     	// no more options
	.byte 0                      	// copy
fl_bytes_to_write:
	.word (SaveStateEnd - SaveState)// size of copy
	.word SaveState					// Source address. 
	.byte $00						// Source bank
fl_write_page:
	.word (fl_sector_buffer+2)		// Dest address		+2 is to skip track/header link
	.byte $00             			// Dest bank
	.byte $00                		// sub-command
	.word 0							// modulo (unused)

// -----------------------------------
// 
fl_copy_buffer_to_sector:
	inc $d020

	lda #$80						// Make sure FDC sector buffer is selected
	trb $d689
	lda #$00						// Copy FDC data to our buffer
	sta $d704
	lda #>fl_sector_write_dmalist
	sta $d701
	lda #<fl_sector_write_dmalist
	sta $d705

	dec $d020
	
	rts

// -----------------------------------
// Copy FDC buffer into fl_sector_buffer
//
fl_sector_write_dmalist:
	.byte $0b						// F011A type list
	.byte $81,$ff					// MB of FDC sector buffer address ($FFD6C00)
	.byte 0							// no more options
	.byte 0							// copy
	.word 512						// size of copy
	.word fl_sector_buffer			// Src address 
	.byte $00						// Src bank
	.word $6c00						// low 16 bits of FDC sector buffer address
	.byte $0d						// next 4 bits of FDC sector buffer address
	.byte $00						// sub-command
	.word 0							// modulo (unused)
  
// ------------------------------------------------------------------------------------------------------------------------------
