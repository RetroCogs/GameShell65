# rcPacker - ByteBoozer Compression Tool for MEGA65

rcPacker is a command-line utility for compressing multiple input files into a single ByteBoozer V2 (B2) packed file. It supports concatenating multiple data files, padding for alignment, optional validation, and detailed console reporting.

## Building

To build rcPacker from source:

```bash
cd build/RCPacker
make clean
make
```

The executable will be created as `rcPacker` in the current directory.

## Usage

```
rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-v]
```

## Command-Line Options

### Positional Arguments

#### `<input_file> [input_file2 ...]`
**Description:**  
One or more input files to be packed. Files are concatenated in the order they appear on the command line into a single source buffer, which is then compressed as one operation.

**Details:**
- Minimum one file is required.
- Files can be any binary data (raw data, code, etc.).
- Each file is appended to the combined source in order.
- If `-p` is enabled, padding bytes are inserted between each file (except after the last file) to align the next file to a 256-byte boundary.

**Example:**
```bash
rcpacker file1.bin file2.bin file3.bin
```
This concatenates all three files and packs them together.

### Optional Arguments

#### `-o <output_file>`
**Description:**  
Explicitly specify the output filename for the packed data.

**Default Behavior:**  
If `-o` is not provided, the output filename is derived from the first input file with a `.b2` extension appended.

**Details:**
- Allows arbitrary output paths and names.
- Overrides the automatic `.b2` suffix generation.
- Must be followed by exactly one filename argument.

**Example:**
```bash
rcpacker file1.bin file2.bin -o packed_data.b2
```
Packs `file1.bin` and `file2.bin` into `packed_data.b2`.

```bash
rcpacker tileset.bin
```
Packs `tileset.bin` into `tileset.bin.b2` (automatic suffix).

#### `-p` (Padding to 256-byte boundaries)
**Description:**  
Enable alignment padding so that each concatenated input file (except the last one) is followed by zero-filled padding bytes up to the next 256-byte boundary.

**Default Behavior:**  
Without `-p`, files are concatenated with no padding between them.

**Details:**
- Padding bytes are always zero (`0x00`), making them compress efficiently.
- Alignment starts from the end of each file, not the beginning.
- The **last input file is not followed by padding**, only intermediate files are padded.
- This is useful when runtime code needs to fetch decompressed data from fixed aligned storage locations.

**Example:**
```bash
rcpacker file1.bin file2.bin -p
```
If `file1.bin` is 300 bytes:
- `file1.bin` occupies bytes 0x00–0xB3 (300 bytes).
- Padding of 212 bytes (0xD4) is inserted: 0xB4–0xFF (all zeros).
- `file2.bin` starts at 0x0100 (256-byte boundary).

#### `-v` (Decrunch and Validate)
**Description:**  
After packing, decompress the packed data and compare it byte-for-byte against the original combined source buffer to verify compression integrity.

**Default Behavior:**  
Without `-v`, packing completes and only writes the compressed file; no validation is performed.

**Details:**
- Decompression uses the built-in ByteBoozer decompressor.
- The decompressed output is compared against the full combined source (including any padding from `-p`).
- If validation succeeds, the tool exits with code 0.
- If validation fails, an error is reported and the tool exits with a non-zero code.
- This is useful for development/testing to catch compression bugs early.

**Example:**
```bash
rcpacker file1.bin file2.bin -v
```
Packs the files and then validates the compressed data by decompressing and comparing.

```bash
rcpacker file1.bin file2.bin -p -v
```
Packs with padding, then validates the entire padded+packed result.

## Console Output

When rcPacker runs, it prints a detailed summary to stdout:

```
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
input[02]  start=$00001234  size=$00004567  pad=$000000CC  "file2.bin"
ByteBoozer summary:
  files         : $00000002
  source bytes  : $00005679
  padding bytes : $000000CC
  packed bytes  : $00003ABC
  packed ratio  : 52.44%
  output: "mydata.b2"
```

**Field Explanations:**

- **input[##]**: Sequential input file number (1-based).
- **start=**: Hexadecimal offset in the combined buffer where this file begins.
- **size=**: Hexadecimal size in bytes of this input file (raw, before any padding).
- **pad=**: Hexadecimal number of zero padding bytes inserted after this file (only present if `-p` is used and not the last file).
- **Filename**: The input file path.
- **files**: Total number of input files concatenated.
- **source bytes**: Combined size of all input files (without padding).
- **padding bytes**: Total zero padding bytes inserted (0 if `-p` not used).
- **packed bytes**: Size of the final compressed `.b2` output.
- **packed ratio**: Percentage of original size after compression (100% = no compression, lower = better).
- **output**: Path to the generated `.b2` file.

## Examples

### Example 1: Simple Single-File Compress
```bash
./rcpacker sprite_tileset.bin
```
Output: `sprite_tileset.bin.b2`

### Example 2: Multi-File with Custom Output
```bash
./rcpacker bg_tiles.bin sprites.bin palettes.bin -o graphics_data.b2
```
Concatenates all three graphics files into `graphics_data.b2`.

### Example 3: Multi-File with Padding
```bash
./rcpacker level_data.bin enemy_data.bin audio_data.bin -p -o game_pack.b2
```
Packs with 256-byte alignment between files and produces `game_pack.b2`.

### Example 4: Development Build with Validation
```bash
./rcpacker asset1.bin asset2.bin -p -v
```
Packs with padding and validates that decompression recovers all original bytes exactly.

## Technical Details

### ByteBoozer V2 Format
rcPacker uses the ByteBoozer V2 compression algorithm, which is optimized for 6502/65C02 decompression on the MEGA65.

Original sources and references:
- https://csdb.dk/release/?id=145031
- https://github.com/luigidifraia/ByteBoozer2

### Memory and Performance
- Input files are read and concatenated in memory before compression.
- Compression is performed on the combined buffer.
- `-O3` optimization is enabled in the build for performance.

### Exit Codes
- `0`: Success (or successful validation with `-v`).
- Non-zero: Error during reading, compression, validation, or file write.

## License
Part of the GameShell65 project.
