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

// .feature pc_assignment
// .feature labels_without_colons
// .feature c_comments

// ----------------------------------------------------------------------------------------------------

fl_init:
		lda #$60								// Start motor
		sta $d080

		rts

// ----------------------------------------------------------------------------------------------------

fl_exit:
		lda #$00								// Stop motor
		sta $d080

		rts

// ----------------------------------------------------------------------------------------------------

//fl_set_filename

//		stx fl_fnptr+1
//		sty fl_fnptr+2

//		ldx #$0f
//		lda #$a0
//:		sta fastload_filename,x

//		dex
//		bpl :-

//		ldx #$ff
//filenamecopyloop
//		inx
//		cpx #$10
//		beq endofname
//fl_fnptr
//		lda $1000,x
//		beq endofname
//		sta fastload_filename,x
//		bne filenamecopyloop

//endofname
//		inx
//		stx fastload_filename_len

//		rts

// ----------------------------------------------------------------------------------------------------

//fl_set_startaddress

//		lda #$00										// Set load address (32-bit) $07ff ($0801 - 2 bytes for BASIC header)
//		sta fastload_address + 0
//		lda #$80
//		sta fastload_address + 1
//		lda #$00
//		sta fastload_address + 2
//		lda #$00
//		sta fastload_address + 3

//		rts

// ----------------------------------------------------------------------------------------------------

.macro LoadFile(addr, fname) {
		// copy filename, expected to be PETSCII and $A0 padded at end, and exactly 16 chars
		ldx #$0f
		lda #$a0
clearfilename:
		sta fastload_filename,x
		dex
		bpl clearfilename

		ldx #$ff
filenamecopyloop:
		inx
		cpx #$10
		beq endofname
		lda fname,x
		beq endofname
		sta fastload_filename,x
		bne filenamecopyloop
endofname:  
		inx
		stx fastload_filename_len
  
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

		// Give the fastload time to get itself sorted
		// (largely seeking to track 0)
wait_for_fastload:
		jsr fastload_irq

		lda fastload_request
		bne wait_for_fastload
  
		// Request fastload job
		lda #$01
		sta fastload_request
		
		jsr fl_waiting
}

// ----------------------------------------------------------------------------------------------------

fl_waiting:
		jsr fastload_irq								// Then just wait for the request byte to
		lda fastload_request							// go back to $00, or to report an error by having the MSB
		bmi fl_error									// set. The request value will continually update based on the
		bne fl_waiting									// state of the loading.
		beq fl_waiting_done

fl_error:
		inc $d020
		jmp fl_error

fl_waiting_done:
		rts

// ----------------------------------------------------------------------------------------------------

fastload_irq_handler:
		php
		pha
		txa
		pha
		tya
		pha

		inc $d020
		inc $d021

		lda $d012
		adc #$20
		sta $d012

		; nop

		pla
		tay
		pla
		tax
		pla
		plp
		asl $d019
		rti

// ------------------------------------------------------------------------------------------------------------------------------
// Actual fast-loader code
// ------------------------------------------------------------------------------------------------------------------------------

.const fastload_sector_buffer	= $0200

fastload_filename:
		.fill 16,0

fastload_filename_len:
		.byte 0

fastload_address:
		.byte 0, 0, 0, 0

fastload_iffl_counter:
		.byte 0, 0, 0, 0

fastload_request:
		.byte 4							// Start with seeking to track 0
				
		// $00 = fl_idle				// idle
		// $01 = fl_new_request			// requested
		// $02 = fl_directory_scan		// scan directory
		// $03 = fl_read_file_block		// read file block
		// $04 = fl_seek_track_0		// seek to track 0
		// $05 = fl_reading_sector		// track stepping/sector reading state
		// $80 = File not found			// file not found

fastload_request_stashed:				// Remember the state that requested a sector read
		.byte 0

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

fastload_iffl_start_address_and_size:
fastload_directory_entries:
		.fill 256,0

fl_iffl_numfiles:
		.byte 0

fl_iffl_currentfile:
		.byte 0

fl_iffl_sizeremaining:
		.byte 0, 0, 0, 0

fl_iffl_bytecounter:
		.byte 0

// ----------------------------------------------------------------------------------------------------

fastload_irq:
		lda fastload_request							// are we in idle state?
		bne todo										// nope, go and check if the FDC is busy
		rts												// yep, back out

todo: 
		lda $d082										// is the FDC busy?
		bpl fl_fdc_not_busy								// nope, continue with request
		rts												// yep, back out

fl_fdc_not_busy:  
		lda fastload_request							// are we in error state?
		bpl fl_not_in_error_state						// nope, continue
		rts												// yep, back out

fl_not_in_error_state:
		cmp #6											// is the request smaller than 6 (change to 8 when IFFL support added)?
		bcc fl_job_ok									// yep, continue
		rts												// nope, something must have gone wrong ($80 (file not found) is bigger than number testing against)

fl_job_ok:  
		asl												// shift state left one bit, so that we can use it as a lookup
		tax												// into a jump table. Everything else is handled by the jump table
		jmp (fl_jumptable,x)
  
fl_jumptable:
		.word fl_idle									// 0
		.word fl_new_request							// 1
		.word fl_directory_scan							// 2
		.word fl_read_file_block						// 3
		.word fl_seek_track_0							// 4
		.word fl_reading_sector							// 5
		// .word fl_iffl_read_file_block_init				// 6
		// .word fl_iffl_read_file_block					// 7

fl_idle:
		rts

fl_seek_track_0:
		lda $d082
		and #$01										// TK0 - F011 Head is over track 0 flag (read only)
		beq fl_not_on_track_0
		lda #$00
		sta fastload_request
		sta fl_current_track
		rts

fl_not_on_track_0:
		lda #$10										// Step back towards track 0
		sta $d081
		rts

fl_select_side1:  
		lda #$01
		sta $d086  										// requested side
		lda #$60										// Sides are inverted on the 1581
		sta $d080  										// physical side selected of mechanical drive
		rts

fl_select_side0:  
		lda #$00
		sta $d086 									    // requested side
		lda #$68										// Sides are inverted on the 1581
		sta $d080										// physical side selected of mechanical drive
		rts

fl_set_done_state:
		lda #$00
		sta fastload_request
		lda #$00
		sta $d080
		rts  

fl_set_error_state:
		lda #$80										// $80 = File not found
		sta fastload_request
		lda #$00
		sta $d080
		rts  
  
fl_new_request:
		lda #2											// Acknowledge fastload request
		sta fastload_request
		lda #40-1										// Request Track 40 Sector 3 to start directory scan
		sta $d084										// (remember we have to do silly translation to real sectors)
		lda #(3/2)+1
		sta $d085
		jsr fl_select_side0

		jsr fl_read_sector								// Request read
		rts
  
fl_directory_scan:
		jsr fl_copy_sector_to_buffer					// Check if our filename we want is in this sector

														// (XXX we scan the last BAM sector as well, to keep the code simple.)
														// filenames are at offset 4 in each 32-byte directory entry, padded at
														// the end with $A0
		lda #<fastload_sector_buffer
		sta fl_buffaddr+1
		lda #>fastload_sector_buffer
		sta fl_buffaddr+2

fl_check_logical_sector:
		ldx #$05
fl_filenamecheckloop:
		ldy #$00

fl_check_loop_inner:

fl_buffaddr:
		lda fastload_sector_buffer+$100,x
	
		cmp fastload_filename,y 
		bne fl_filename_differs
		inx
		iny
		cpy #$10
		bne fl_check_loop_inner

		
fl_found_file:											// Filename matches
		txa
		sec
		sbc #$12
		tax
		lda fl_buffaddr+2
		cmp #>fastload_sector_buffer
		bne fl_file_in_2nd_logical_sector

		lda fastload_sector_buffer,x					// Y=Track, A=Sector
		tay
		lda fastload_sector_buffer+1,x
		jmp fl_got_file_track_and_sector

fl_file_in_2nd_logical_sector:
		lda fastload_sector_buffer+$100,x				// Y=Track, A=Sector
		tay
		lda fastload_sector_buffer+$101,x

fl_got_file_track_and_sector:
		sty fl_file_next_track							// Store track and sector of file
		sta fl_file_next_sector

		lda #3											// Advance to next state (6=fl_iffl_read_file_block_init)
		sta fastload_request
	
		jsr fl_read_next_sector							// Request reading of next track and sector
		rts
  
fl_filename_differs:
		cpy #$10										// Skip same number of chars as though we had matched
		beq fl_end_of_name
		inx
		iny
		jmp fl_filename_differs

fl_end_of_name:
		txa												// Advance to next directory entry
		clc
		adc #$10
		tax
		bcc fl_filenamecheckloop
		inc fl_buffaddr+2
		lda fl_buffaddr+2
		cmp #(>fastload_sector_buffer)+1
		bne fl_checked_both_halves
		jmp fl_check_logical_sector

fl_checked_both_halves: 
		inc $d085										// No matching name in this 512 byte sector.
		lda $d085										// Load the next one, or give up the search
		cmp #11
		bne fl_load_next_dir_sector
														// Ran out of sectors in directory track
														// (XXX only checks side 0, and assumes DD disk)

		jsr fl_set_error_state							// Mark load as failed
		rts

fl_load_next_dir_sector:  
		jsr fl_read_sector								// Request read. No need to change state
		rts

// ------------------------------------------------------------------------------------------------------------------------------

fl_read_sector:
		lda fastload_request							// Remember the state that we need to return to
		sta fastload_request_stashed
		
		lda #5											// and then set ourselves to the track stepping/sector reading state
		sta fastload_request
														// FALLTHROUGH
  
  		// ----------------------------------------------------------------------------------------------------------------------

fl_reading_sector:
		lda $d084										// Check if we are already on the correct track/side
		cmp fl_current_track							// and if not, select/step as required
		beq fl_on_correct_track
		bcc fl_step_in

fl_step_out:
		lda #$18										// We need to step first
		sta $d081
		inc fl_current_track
		rts

fl_step_in:
		lda #$10										// We need to step first
		sta $d081
		dec fl_current_track
		rts
  
fl_on_correct_track:
		lda $d084
		cmp fl_prev_track
		bne fl_not_prev_sector
		lda $d086
		cmp fl_prev_side
		bne fl_not_prev_sector
		lda $d085
		cmp fl_prev_sector
		bne fl_not_prev_sector

		lda fastload_request_stashed					// We are being asked to read the sector we already have in the buffer
		sta fastload_request							// Jump immediately to the correct routine
		jmp fl_fdc_not_busy

fl_not_prev_sector: 
		lda #$40										// ISSUE ACTUAL READ COMMAND
		sta $d081

		lda fastload_request_stashed					// Now that we are finally reading the sector,
		sta fastload_request							// restore the stashed state ID

		rts

fl_step_track:
		lda #3											// advance to next state (3=fl_read_file_block)
		sta fastload_request
														// FALL THROUGH
  
fl_read_next_sector:
		lda fl_file_next_track							// Check if we reached the end of the file first
		bne fl_not_end_of_file
		rts

fl_not_end_of_file: 
		jsr fl_logical_to_physical_sector				// Read next sector of file 
		jsr fl_read_sector
		rts
  
fl_logical_to_physical_sector:
		lda $d084										// Remember current loaded sector, so that we can optimise when asked
		sta fl_prev_track								// to read other half of same physical sector
		lda $d085
		sta fl_prev_sector
		lda $d086
		sta fl_prev_side
														// Convert 1581 sector numbers to physical ones on the disk.
		jsr fl_select_side0								// Side = 0
		lda fl_file_next_track
		dec												// Track = Track - 1
		sta $d084

		lda fl_file_next_sector							// Sector = 1 + (Sector/2)
		lsr
		inc
		cmp #11											// If sector > 10, then sector=sector-10, side=1
		bcs fl_on_second_side							// but sides are inverted
		sta $d085

		rts
  
fl_on_second_side:
		sec
		sbc #10
		sta $d085
		jsr fl_select_side1
		rts
  
// ------------------------------------------------------------------------------------------------------------------------------

fl_iffl_read_file_block_init:

		lda #$07										// Set state to fl_iffl_read_file_block
		sta fastload_request

		jsr fl_copy_sector_to_buffer					// Get sector from FDC

		lda fl_file_next_sector							// Work out which half we care about
		and #$01
		bne fl_iffl_init_read_from_second_half			// odd next sector number, so second half

		lda fastload_sector_buffer+2					// read number of file entries in iffl from first sector
		sta fl_iffl_numfiles

		ldx #$00

		ldy #$00
!:		lda fastload_sector_buffer+$003,x					// get start addresses for files in iffl
		sta fastload_iffl_start_address_and_size+0,x
		lda fastload_sector_buffer+$004,x
		sta fastload_iffl_start_address_and_size+1,x
		lda fastload_sector_buffer+$005,x
		sta fastload_iffl_start_address_and_size+2,x
		lda fastload_sector_buffer+$006,x
		sta fastload_iffl_start_address_and_size+3,x
		lda fastload_sector_buffer+$007,x					// get sizes for files in iffl
		sta fastload_iffl_start_address_and_size+4,x
		lda fastload_sector_buffer+$008,x
		sta fastload_iffl_start_address_and_size+5,x
		lda fastload_sector_buffer+$009,x
		sta fastload_iffl_start_address_and_size+6,x
		lda fastload_sector_buffer+$00a,x
		sta fastload_iffl_start_address_and_size+7,x
		clc
		txa
		adc #$08
		tax
		iny
		cpy fl_iffl_numfiles
		bne !-

		jmp fl_iffl_read_file_block_init_end

fl_iffl_init_read_from_second_half:
		lda fastload_sector_buffer+$102					// read number of file entries in iffl from second sector
		sta fl_iffl_numfiles

		ldx #$00

		ldy #$00
!:		lda fastload_sector_buffer+$103,x				// get start addresses for files in iffl
		sta fastload_iffl_start_address_and_size+0,x
		lda fastload_sector_buffer+$104,x
		sta fastload_iffl_start_address_and_size+1,x
		lda fastload_sector_buffer+$105,x
		sta fastload_iffl_start_address_and_size+2,x
		lda fastload_sector_buffer+$106,x
		sta fastload_iffl_start_address_and_size+3,x
		lda fastload_sector_buffer+$107,x				// get sizes for files in iffl
		sta fastload_iffl_start_address_and_size+4,x
		lda fastload_sector_buffer+$108,x
		sta fastload_iffl_start_address_and_size+5,x
		lda fastload_sector_buffer+$109,x
		sta fastload_iffl_start_address_and_size+6,x
		lda fastload_sector_buffer+$10a,x
		sta fastload_iffl_start_address_and_size+7,x
		clc
		txa
		adc #$08
		tax
		iny
		cpy fl_iffl_numfiles
		bne !-

		jmp fl_iffl_read_file_block_init_end

fl_iffl_read_file_block_init_end:

		clc												// set iffl byte counter to 1(nexttrack)+1(nextsector)+1(numfiles)+numfiles*8
		lda fl_iffl_numfiles
		asl
		asl
		asl
		adc #$03
		sta fl_iffl_bytecounter

		ldx #$00
		stx fl_iffl_currentfile

		lda fastload_iffl_start_address_and_size+0
		sta fastload_address+0
		lda fastload_iffl_start_address_and_size+1
		sta fastload_address+1
		lda fastload_iffl_start_address_and_size+2
		sta fastload_address+2
		lda fastload_iffl_start_address_and_size+3
		sta fastload_address+3
		lda fastload_iffl_start_address_and_size+4
		sta fl_iffl_sizeremaining+0
		lda fastload_iffl_start_address_and_size+5
		sta fl_iffl_sizeremaining+1
		lda fastload_iffl_start_address_and_size+6
		sta fl_iffl_sizeremaining+2
		lda fastload_iffl_start_address_and_size+7
		sta fl_iffl_sizeremaining+3

		lda #$00										// Mark end of loading
		sta fastload_request

		rts

// ------------------------------------------------------------------------------------------------------------------------------

fl_iffl_read_file_block:

		jsr fl_copy_sector_to_buffer					// Get sector from FDC

		lda fl_iffl_sizeremaining+3
		bne fl_iffl_fullcopy
		lda fl_iffl_sizeremaining+2
		bne fl_iffl_fullcopy
		lda fl_iffl_sizeremaining+1
		bne fl_iffl_fullcopy
		sec
		lda #0											// (256 - counter) > sizeremaining?
		sbc fl_iffl_bytecounter
		sec
		sbc fl_iffl_sizeremaining+0
		bcc fl_iffl_fullcopy							// yes, copy remaining buffer

fl_iffl_partialcopy:									// no, copy until remaining size

		lda fl_file_next_sector							// Work out which half we care about
		and #$01
		bne fl_iffl_partial_read_from_second_half		// odd next sector number, so second half
		lda #(>fastload_sector_buffer)+0
		sta fl_read_page+1
		bra fl_iffl_dopartialcopy
fl_iffl_partial_read_from_second_half:
		lda #(>fastload_sector_buffer)+1
		sta fl_read_page+1

fl_iffl_dopartialcopy:

		lda fl_iffl_bytecounter							// set offset for DMA copy
		sta fl_read_page+0

		lda fl_iffl_sizeremaining+0
		sta fl_bytes_to_copy

		clc
		lda fl_iffl_bytecounter
		adc fl_bytes_to_copy
		sta fl_iffl_bytecounter

		lda #$00										// Mark end of loading
		sta fastload_request
		jsr fl_iffl_performcopy

		rts

fl_iffl_fullcopy:

		lda fl_file_next_sector							// Work out which half we care about
		and #$01
		bne fl_iffl_read_from_second_half				// odd next sector number, so second half

		lda #(>fastload_sector_buffer)+0				// fl_read_from_first_half
		sta fl_read_page+1
		lda fastload_sector_buffer+1
		sta fl_file_next_sector
		lda fastload_sector_buffer+0
		sta fl_file_next_track
		jmp fl_iffl_dma_read_bytes

fl_iffl_read_from_second_half:
		lda #(>fastload_sector_buffer)+1
		sta fl_read_page+1
		lda fastload_sector_buffer+$101
		sta fl_file_next_sector
		lda fastload_sector_buffer+$100
		sta fl_file_next_track
		jmp fl_iffl_dma_read_bytes

fl_iffl_dma_read_bytes:

		sec
		lda #0
		sbc fl_iffl_bytecounter
		sta fl_bytes_to_copy
		sec
		lda fl_iffl_sizeremaining+0
		sbc fl_bytes_to_copy
		sta fl_iffl_sizeremaining+0
		lda fl_iffl_sizeremaining+1
		sbc #0
		sta fl_iffl_sizeremaining+1
		lda fl_iffl_sizeremaining+2
		sbc #0
		sta fl_iffl_sizeremaining+2
		lda fl_iffl_sizeremaining+3
		sbc #0
		sta fl_iffl_sizeremaining+3

		lda fl_iffl_bytecounter							// set offset for DMA copy
		sta fl_read_page+0
		jsr fl_iffl_performcopy

		clc
		lda fl_iffl_bytecounter
		adc fl_bytes_to_copy
		sta fl_iffl_bytecounter
		clc
		lda fl_iffl_bytecounter
		adc #$02
		sta fl_iffl_bytecounter

		jsr fl_read_next_sector							// Schedule reading of next block

		rts

fl_iffl_performcopy:

		lda fastload_address+3							// Update destination address
		asl
		asl
		asl
		asl
		sta fl_data_read_dmalist+2						// update destination MB
		lda fastload_address+2
		lsr
		lsr
		lsr
		lsr
		ora fl_data_read_dmalist+2
		sta fl_data_read_dmalist+2						// update destination MB
		lda fastload_address+2
		and #$0f
		sta fl_data_read_dmalist+12						// update Dest bank
		lda fastload_address+1
		sta fl_data_read_dmalist+11						// update Dest Address high
		lda fastload_address+0
		sta fl_data_read_dmalist+10						// update Dest Address low

		lda #$00										// Copy sector buffer data to final address
		sta $d704
		lda #>fl_data_read_dmalist
		sta $d701
		lda #<fl_data_read_dmalist
		sta $d705

		clc
		lda fastload_address+0							// Update load address
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

		rts

// ------------------------------------------------------------------------------------------------------------------------------

fl_read_file_block:
														// We have a sector from the floppy drive.
														// Work out which half and how many bytes, and copy them into place.

		jsr fl_copy_sector_to_buffer					// Get sector from FDC

		lda #254										// Assume full sector initially
		sta fl_bytes_to_copy
	
		lda fl_file_next_sector							// Work out which half we care about
		and #$01
		bne fl_read_from_second_half					// odd next sector number, so second half

		lda #(>fastload_sector_buffer)+0				// fl_read_from_first_half
		sta fl_read_page+1
		lda fastload_sector_buffer+1
		sta fl_file_next_sector
		lda fastload_sector_buffer+0
		sta fl_file_next_track
		bne fl_1st_half_full_sector						// if next track is 0 then this is a partial sector and 'sector' now becomes the number of bytes left in this sector

		lda fastload_sector_buffer+1					// fl_1st_half_partial_sector. track is 0, so sector contains number of bytes left
		sta fl_bytes_to_copy  
		jsr fl_set_done_state							// Mark end of loading

fl_1st_half_full_sector:
		jmp fl_dma_read_bytes
  
fl_read_from_second_half:
		lda #(>fastload_sector_buffer)+1
		sta fl_read_page+1
		lda fastload_sector_buffer+$101
		sta fl_file_next_sector
		lda fastload_sector_buffer+$100
		sta fl_file_next_track
		bne fl_2nd_half_full_sector
fl_2nd_half_partial_sector:
		lda fastload_sector_buffer+$101
		sta fl_bytes_to_copy
		// Mark end of loading
		jsr fl_set_done_state

fl_2nd_half_full_sector:
		// FALLTHROUGH

// ------------------------------------------------------------------------------------------------------------------------------

fl_dma_read_bytes:
		lda fastload_address+3							// Update destination address
		asl
		asl
		asl
		asl
		sta fl_data_read_dmalist+2						// update destination MB
		lda fastload_address+2
		lsr
		lsr
		lsr
		lsr
		ora fl_data_read_dmalist+2
		sta fl_data_read_dmalist+2						// update destination MB
		lda fastload_address+2
		and #$0f
		sta fl_data_read_dmalist+12						// update Dest bank
		lda fastload_address+1
		sta fl_data_read_dmalist+11						// update Dest Address high
		lda fastload_address+0
		sta fl_data_read_dmalist+10						// update Dest Address low

		lda #$00										// Copy sector buffer data to final address
		sta $d704
		lda #>fl_data_read_dmalist
		sta $d701
		lda #<fl_data_read_dmalist
		sta $d705

		clc
		lda fastload_address+0							// Update load address
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
	
		jsr fl_read_next_sector							// Schedule reading of next block
	
		rts

// ------------------------------------------------------------------------------------------------------------------------------

fl_data_read_dmalist:
		.byte $0b                					    // F011A type list
		.byte $81,$00             					    // Destination MB
		.byte 0                     					// no more options
		.byte 0                      					// copy
fl_bytes_to_copy:
		.word 0                       					// size of copy
fl_read_page:
		.byte >(fastload_sector_buffer+2)				// Source address. +2 is to skip track/header link
		.byte $00										// Source bank
		.word 0											// Dest address
		.byte $00             							// Dest bank
		.byte $00                						// sub-command
		.word 0											// modulo (unused)
		rts	// LV - FIX ME IN ORIGINAL

// ------------------------------------------------------------------------------------------------------------------------------

fl_copy_sector_to_buffer:
		lda #$80										// Make sure FDC sector buffer is selected
		trb $d689
		lda #$00										// Copy FDC data to our buffer
		sta $d704
		lda #>fl_sector_read_dmalist
		sta $d701
		lda #<fl_sector_read_dmalist
		sta $d705
		rts

fl_sector_read_dmalist:
		.byte $0b										// F011A type list
		.byte $80,$ff									// MB of FDC sector buffer address ($FFD6C00)
		.byte 0											// no more options
		.byte 0											// copy
		.word 512										// size of copy
		.word $6c00										// low 16 bits of FDC sector buffer address
		.byte $0d										// next 4 bits of FDC sector buffer address
		.word fastload_sector_buffer					// Dest address 
		.byte $00										// Dest bank
		.byte $00										// sub-command
		.word 0											// modulo (unused)
  
// ------------------------------------------------------------------------------------------------------------------------------
