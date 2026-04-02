// -----------------------------------------------------------------------------------------------
// Mega65 floppy loader
//
// see this link for .D81 fileformat - unusedino.de/ec64/technical/formats/d81.html
//
// This is a compact assembly implementation of the same basic ideas:
// - prepare and spin up the drive
// - search the directory for a filename
// - convert logical track/block to physical track/sector/side
// - load blocks into the FDC buffer
// - copy file bytes to a true 32-bit destination address
//
// Notes:
// - It is meant as a small stand-alone loader module for GameShell65.
// - Filenames are expected to be 0-terminated PETSCII strings.
// - Default file type used by Loader_LoadFile() is PRG ($82).
// -----------------------------------------------------------------------------------------------

.const LDR_FDC_CONTROL         = $d080
.const LDR_FDC_COMMAND         = $d081
.const LDR_FDC_STATUS          = $d082
.const LDR_FDC_STATUS2         = $d083
.const LDR_FDC_TRACK           = $d084
.const LDR_FDC_SECTOR          = $d085
.const LDR_FDC_SIDE            = $d086
.const LDR_FDC_DATA            = $d087

.const LDR_BUFSEL              = $d689
.const LDR_AUTOTUNE            = $d696

.const LDR_FDC_SIDE_MASK       = %00001000
.const LDR_FDC_SWAP_MASK       = %00010000
.const LDR_FDC_MOTOR_MASK      = %00100000
.const LDR_FDC_LED_MASK        = %01000000

.const LDR_FDC_TK0_MASK        = $01
.const LDR_FDC_CRC_MASK        = $08
.const LDR_FDC_RNF_MASK        = $10
.const LDR_FDC_BUSY_MASK       = $80
.const LDR_FDC_WGATE_MASK      = $10
.const LDR_FDC_RDREQ_MASK      = $80

.const LDR_CMD_CLR_BUFFER_PTRS = $01
.const LDR_CMD_STEP_OUT        = $10
.const LDR_CMD_STEP_IN         = $18
.const LDR_CMD_SPINUP          = $20
.const LDR_CMD_READ_SECTOR     = $40

.const LDR_FILETYPE_SEQ        = $81
.const LDR_FILETYPE_PRG        = $82

// loader_error_code values
.const LDR_ERR_FILE_NOT_FOUND  = $01
.const LDR_ERR_BAD_LOCATION    = $02
.const LDR_ERR_SECTOR_ERROR    = $03

.segment Zeropage "Loader ZP"
loader_target_ptr:     .byte 0,0,0,0
loader_filename_ptr:   .word 0
loader_copy_count:     .byte 0
loader_match:          .byte 0
loader_entry_type:     .byte 0
loader_entry_track:    .byte 0
loader_entry_block:    .byte 0
loader_req_track:      .byte 0
loader_req_block:      .byte 0
loader_phys_track:     .byte 0
loader_phys_sector:    .byte 0
loader_phys_side:      .byte 0

.print ("loader_filename_ptr is at $" + toHexString(loader_filename_ptr))

.segment BSS "Loader State"
loader_disk_num:       .byte 0
loader_file_type:      .byte 0
loader_next_track:     .byte 0
loader_next_block:     .byte 0
loader_current_track:  .byte 0
loader_last_disk:      .byte 0
loader_last_track:     .byte 0
loader_last_sector:    .byte 0
loader_last_side:      .byte 0
loader_drive_spinning: .byte 0
loader_drive_in_use:   .byte 0
loader_initialized:    .byte 0
loader_error_code:     .byte 0
loader_filename_buf:   .fill 16, $a0

.print ("loader_error_code is at $" + toHexString(loader_error_code))

.segment Code "Loader Code"

// -----------------------------------------------------------------------------------------------
// Convenience macro:
//
//   Loader_LoadFile($20000, myFilename)
//
.macro Loader_LoadFile(addr, fname)
{
	lda #$00
	sta loader_disk_num

	lda #LDR_FILETYPE_PRG
	sta loader_file_type

	_set32im(addr, loader_target_ptr)

	ldx #<fname
	ldy #>fname
	jsr loader_setup_filename

	jsr loader_load_file
}

// -----------------------------------------------------------------------------------------------
// Public routines
// -----------------------------------------------------------------------------------------------

loader_init:
{
	stz loader_disk_num

	lda #LDR_FILETYPE_PRG
	sta loader_file_type

	stz loader_next_track
	stz loader_next_block
	stz loader_current_track
	stz loader_drive_spinning
	stz loader_drive_in_use
	stz loader_initialized

	lda #$ff
	sta loader_last_disk
	sta loader_last_track
	sta loader_last_sector
	sta loader_last_side

	lda #$00
	sta LDR_FDC_CONTROL
	rts
}

loader_setup_filename:
{
	stx loader_filename_ptr+0
	sty loader_filename_ptr+1

	// Build a fixed 16-byte filename buffer padded with $0A.
	lda #$a0
	ldx #$0f
!:
	sta loader_filename_buf,x
	dex
	bpl !-

	ldy #$00
!:
	cpy #$10
	beq done_copy
	lda (loader_filename_ptr),y
	beq done_copy
	sta loader_filename_buf,y
	iny
	bra !-

done_copy:
	rts
}

loader_load_file:
{
	jsr loader_prepare_drive
	jsr loader_search_file

	lda loader_next_track
	bne found_file
	lda #LDR_ERR_FILE_NOT_FOUND
	jmp loader_error

found_file:
file_loop:
	lda loader_next_track
	sta loader_req_track
	lda loader_next_block
	sta loader_req_block
	jsr loader_load_block

	lda LDR_FDC_DATA			// next track of file chain
	sta loader_next_track
	lda LDR_FDC_DATA			// next block of file chain, or final byte count+1
	sta loader_next_block

	lda loader_next_track
	bne full_block
	lda loader_next_block
	sec
	sbc #$01
	bra have_count

full_block:
	lda #254

have_count:
	jsr loader_copy_bytes_from_fdc

	lda loader_next_track
	bne file_loop

	jsr loader_release_drive
	rts
}

// -----------------------------------------------------------------------------------------------
// Internal helpers
// -----------------------------------------------------------------------------------------------

// Wait until the floppy controller reports that its current command is finished.
//
// The FDC sets BUSY while a step, spin-up, read, or write command is in progress.
// Most higher-level routines issue a command to $D081 and then call this helper
// before touching the FDC again.
loader_wait_for_busy_clear:
{
!:	lda LDR_FDC_STATUS
	and #LDR_FDC_BUSY_MASK
	bne !-
	rts
}

// Ensure the drive is ready for FDC operations.
//
// This routine:
// - marks the drive as in use
// - disables auto-tune
// - spins up motor + LED if needed
// - homes the head to track 0 the first time it is used
//
// Homing is done by issuing repeated STEP_OUT commands until TK0 is observed.
loader_prepare_drive:
{
	lda #$01
	sta loader_drive_in_use

	lda #$80					// disable auto-tune
	trb LDR_AUTOTUNE

	lda loader_drive_spinning
	bne maybe_home

	lda #(LDR_FDC_LED_MASK | LDR_FDC_MOTOR_MASK)
	sta LDR_FDC_CONTROL
	lda #LDR_CMD_SPINUP
	sta LDR_FDC_COMMAND
	jsr loader_wait_for_busy_clear
	lda #$01
	sta loader_drive_spinning

maybe_home:
	lda loader_initialized
	bne done

home_loop:
	lda LDR_FDC_STATUS
	and #LDR_FDC_TK0_MASK
	bne homed
	lda #LDR_CMD_STEP_OUT
	sta LDR_FDC_COMMAND
	jsr loader_wait_for_busy_clear
	bra home_loop

homed:
	stz loader_current_track
	lda #$01
	sta loader_initialized

done:
	rts
}

loader_release_drive:
{
	stz loader_drive_in_use
	stz loader_drive_spinning
	stz LDR_FDC_CONTROL
	rts
}

// Move the head from loader_current_track to loader_phys_track.
//
// `loader_phys_track` is already in physical 0..79 form here.
// The FDC only moves one track per command, so we step in/out until the current
// track matches the requested one.
loader_step_to_track:
{
step_loop:
	lda loader_phys_track
	cmp loader_current_track
	beq done
	bcc step_out

step_in:
	lda #LDR_CMD_STEP_IN
	sta LDR_FDC_COMMAND
	jsr loader_wait_for_busy_clear
	inc loader_current_track
	bra step_loop

step_out:
	lda #LDR_CMD_STEP_OUT
	sta LDR_FDC_COMMAND
	jsr loader_wait_for_busy_clear
	dec loader_current_track
	bra step_loop

done:
	rts
}

// Select which half of the 512-byte sector buffer the CPU will see when reading
// through $D087.
//
// On a 1581 disk each physical sector contains two logical 256-byte blocks.
// The MEGA65 FDC exposes either the first half or second half depending on the
// SWAP bit. We also reset the CPU buffer read pointer before changing the view.
loader_set_fdc_swap:
{
	pha
	lda #LDR_CMD_CLR_BUFFER_PTRS
	sta LDR_FDC_COMMAND
	pla
	and #$01
	beq clear_swap

	lda #LDR_FDC_SWAP_MASK
	tsb LDR_FDC_CONTROL
	rts

clear_swap:
	lda #LDR_FDC_SWAP_MASK
	trb LDR_FDC_CONTROL
	rts
}

// Load the requested logical file block into the FDC sector buffer.
//
// Inputs:
// - `loader_req_track` = logical file track (1..80)
// - `loader_req_block` = logical file block (0..39)
//
// Behaviour:
// - validates track/block
// - converts logical block to physical track/sector/side
// - avoids re-reading if the same physical sector is already buffered
// - issues FDC read command when needed
// - checks RNF/CRC status bits for failure
// - finally sets the SWAP state so CPU reads from the correct 256-byte half
loader_load_block:
{
	lda loader_req_track
	lbeq bad_location
	cmp #81
	lbcs bad_location

	lda loader_req_block
	cmp #40
	lbcs bad_location

	// Convert logical 1581 file location to physical disk location.
	//
	// Logical blocks are numbered 0..39 per track.
	// Each PHYSICAL sector holds two logical blocks, so:
	//   physical_sector = (block / 2) + 1
	// and sectors 1..10 are on side 0, sectors 11..20 are on side 1.
	lda loader_req_block
	lsr
	sta loader_phys_sector			// 0..19

	lda loader_req_track
	dec
	sta loader_phys_track			// logical 1..80 -> physical 0..79

	inc loader_phys_sector			// physical sectors are 1..20
	lda loader_phys_sector
	cmp #11
	bcc side0

	sub10:
	sec
	sbc #10
	sta loader_phys_sector
	lda #$01
	sta loader_phys_side
	bra have_phys

side0:
	stz loader_phys_side

have_phys:
	lda #$80					// select floppy buffer, not SD buffer
	trb LDR_BUFSEL

	// If the same physical sector is already in the FDC buffer, we do not need to
	// read the disk again. We only need to reset the CPU read pointer and select
	// the correct half via SWAP.
	lda loader_disk_num
	cmp loader_last_disk
	bne do_read
	lda loader_phys_track
	cmp loader_last_track
	bne do_read
	lda loader_phys_sector
	cmp loader_last_sector
	bne do_read
	lda loader_phys_side
	cmp loader_last_side
	bne do_read

	lda loader_req_block
	jsr loader_set_fdc_swap
	rts

do_read:
	jsr loader_prepare_drive

	lda loader_phys_side
	beq select_side0
	lda #LDR_FDC_SIDE_MASK
	trb LDR_FDC_CONTROL
	bra side_done

select_side0:
	lda #LDR_FDC_SIDE_MASK
	tsb LDR_FDC_CONTROL

side_done:
	jsr loader_step_to_track

	lda loader_phys_track
	sta LDR_FDC_TRACK
	lda loader_phys_sector
	sta LDR_FDC_SECTOR
	lda loader_phys_side
	sta LDR_FDC_SIDE

	// Disable SWAP before the actual read so the sector buffer is filled and the
	// CPU pointer starts from the beginning of the 512-byte sector.
	lda #LDR_FDC_SWAP_MASK			// disable swap before reading the physical sector
	trb LDR_FDC_CONTROL
	lda #LDR_CMD_CLR_BUFFER_PTRS
	sta LDR_FDC_COMMAND
	lda #LDR_CMD_READ_SECTOR
	sta LDR_FDC_COMMAND

wait_found:
	// Wait until either:
	// - RDREQ says the requested sector has been found, or
	// - RNF/CRC indicates a failure.
	lda LDR_FDC_STATUS2
	and #LDR_FDC_RDREQ_MASK
	bne sector_found
	lda LDR_FDC_STATUS
	and #(LDR_FDC_RNF_MASK | LDR_FDC_CRC_MASK)
	bne read_error
	bra wait_found

sector_found:
	jsr loader_wait_for_busy_clear

	lda LDR_FDC_STATUS
	and #(LDR_FDC_RNF_MASK | LDR_FDC_CRC_MASK)
	beq read_ok

read_error:
	lda #LDR_ERR_SECTOR_ERROR
	jmp loader_error

read_ok:
	lda loader_disk_num
	sta loader_last_disk
	lda loader_phys_track
	sta loader_last_track
	lda loader_phys_sector
	sta loader_last_sector
	lda loader_phys_side
	sta loader_last_side

	lda loader_req_block
	jsr loader_set_fdc_swap
	rts

bad_location:
	lda #LDR_ERR_BAD_LOCATION
	jmp loader_error
}

// Search the disk directory for the filename stored in `loader_filename_buf`.
//
// The 1581 directory starts at logical track 40, block 3. Each logical directory
// block contains 8 directory entries of 32 bytes each. We iterate over the chain,
// and for each entry compare:
// - file type
// - first track / first block validity
// - 16-byte filename field
//
// On success, `loader_next_track` / `loader_next_block` become the first block of
// the file data chain.
loader_search_file:
{
	lda #40
	sta loader_next_track
	lda #3
	sta loader_next_block

dir_loop:
	lda loader_next_track
	beq not_found
	sta loader_req_track
	lda loader_next_block
	sta loader_req_block
	jsr loader_load_block

	lda LDR_FDC_DATA			// directory chain next track
	sta loader_next_track
	lda LDR_FDC_DATA			// directory chain next block
	sta loader_next_block

	ldx #8						// 8 directory entries per logical block
entry_loop:
	lda LDR_FDC_DATA
	sta loader_entry_type
	lda LDR_FDC_DATA
	sta loader_entry_track
	lda LDR_FDC_DATA
	sta loader_entry_block

	lda #$01
	sta loader_match

	ldy #$00
name_loop:
	lda loader_filename_buf,y
	cmp LDR_FDC_DATA
	beq name_ok
	stz loader_match
name_ok:
	iny
	cpy #$10
	bne name_loop

	ldy #$0d					// consume the rest of the 32-byte directory entry
skip_rest:
	lda LDR_FDC_DATA
	dey
	bne skip_rest

	lda loader_entry_type
	cmp loader_file_type
	bne next_entry
	lda loader_entry_track
	beq next_entry
	cmp #81
	bcs next_entry
	lda loader_entry_block
	cmp #40
	bcs next_entry
	lda loader_match
	beq next_entry

	lda loader_entry_track
	sta loader_next_track
	lda loader_entry_block
	sta loader_next_block
	rts

next_entry:
	dex
	bne entry_loop
	bra dir_loop

not_found:
	rts
}

// Copy `A` bytes of payload from the currently selected FDC logical block half
// to the 32-bit destination pointer in `loader_target_ptr`.
//
// Important detail:
// - The first two bytes of the block (next track / next block) have already been
//   consumed before this routine is called.
// - Therefore this routine copies only the data payload bytes remaining in the
//   block: normally 254 bytes, or fewer on the final EOF block.
loader_copy_bytes_from_fdc:
{
	sta loader_copy_count
	beq done
	ldz #$00

copy_loop:
	lda LDR_FDC_DATA
	sta ((loader_target_ptr)),z

	inc loader_target_ptr+0
	bne no_carry0
	inc loader_target_ptr+1
	bne no_carry0
	inc loader_target_ptr+2
	bne no_carry0
	inc loader_target_ptr+3
no_carry0:

	dec loader_copy_count
	bne copy_loop

done:
	rts
}

// Fatal loader error handler.
//
// The error code is stored in `loader_error_code`, border colours are changed for
// visible debugging feedback, and execution is trapped in an infinite loop.
loader_error:
{
	sta loader_error_code
	lda #$02
	sta $d020
	lda #$00
	sta $d021

error_loop:
	bra error_loop
}
