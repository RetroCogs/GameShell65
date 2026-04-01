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
rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-q] [-v]
```

## Command-Line Options

### Input Files

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
```
**Details:**
```
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
#### `-q` (Quiet Mode)
**Description:**  
Reduce console output by hiding the detailed crunch, decrunch, and successful validation chatter.

**Default Behavior:**  
Without `-q`, rcPacker prints the input lines, lower-level crunch/decrunch details, and the final summary.

**Details:**
- Hides the extra `[Crunch]`, `[Decrunch]`, and successful `[Validate]` informational output.
- Keeps the `input[...]` lines visible.
- Keeps the final `ByteBoozer summary` block visible.
- Does not suppress error messages.
- Useful when you still want the file layout summary but do not want the lower-level packer diagnostics.

**Example:**
```bash
rcpacker file1.bin file2.bin file3.bin -q
```
This packs all three files, keeps the `input[...]` lines, and suppresses the lower-level diagnostic output.
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
# rcPacker - ByteBoozer Compression Tool for MEGA65

rcPacker is a command-line utility for compressing one or more input files into a single ByteBoozer V2 `.b2` packed file. It supports concatenating multiple input files, optional 256-byte padding between files, optional round-trip validation, and both normal and quiet console output modes.

## Building

To build rcPacker from source:

```bash
cd build/RCPacker
make clean
make
```

The executable is created as `rcPacker` in the same directory.

## Usage

```text
rcpacker <input_file> [input_file2 ...] [-o <output_file>] [-p] [-q] [-v]
```

## Command-Line Options

### Positional Arguments

#### `<input_file> [input_file2 ...]`
One or more input files to pack. Files are concatenated in the exact order they appear on the command line, then compressed as one combined stream.

Details:
- At least one input file is required.
- Files can contain any binary data.
- The combined source buffer is built entirely in memory before crunching.

Example:

```bash
rcpacker file1.bin file2.bin file3.bin
```

### `-o <output_file>`
Write the packed output to the given filename.

Default behavior:
- If `-o` is omitted, rcPacker uses the first input filename with `.b2` appended.

Details:
- Accepts any valid output path.
- Overrides the default `<first-input>.b2` naming behavior.
- Must be followed by exactly one filename.

Examples:

```bash
rcpacker file1.bin file2.bin -o packed_data.b2
```

```bash
rcpacker tileset.bin
```

### `-p`
Pad each input file except the last one so the next file begins on a 256-byte boundary.

Default behavior:
- Without `-p`, files are concatenated directly with no padding.

Details:
- Padding is added after each file, not before the next one.
- Padding bytes are always `0x00` so they compress well.
- The last input file is not padded.
- This is useful when the decompressed data needs aligned file start offsets.

Example:

```bash
rcpacker file1.bin file2.bin -p
```

If `file1.bin` is 300 bytes long:
- `file1.bin` starts at `0x00000000`
- 212 bytes of zero padding are added after it
- `file2.bin` starts at `0x00000200`

### `-q`
Quiet mode.

Behavior:
- Keeps the per-file `input[...]` lines visible.
- Hides lower-level `[Crunch]`, `[Decrunch]`, and successful `[Validate]` informational output.
- Replaces the normal multi-line final summary with a single concise `summary ...` line.
- Does not suppress error messages.

Example:

```bash
rcpacker file1.bin file2.bin file3.bin -q
```

### `-v`
Decrunch and validate the packed output after compression.

Behavior:
- Decompresses the generated `.b2` output in memory.
- Compares the decrunched bytes against the original combined source buffer.
- Includes any zero padding added by `-p` in the comparison.
- Returns a non-zero exit code if validation fails.

Examples:

```bash
rcpacker file1.bin file2.bin -v
```

```bash
rcpacker file1.bin file2.bin -p -v
```

## Console Output

### Normal Mode

Normal mode prints one line per input file plus a final multi-line summary:

```text
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
input[02]  start=$00001234  size=$00004567  pad=$000000CC  "file2.bin"
ByteBoozer summary:
  files         : $00000002
  source bytes  : $00005679
  padding bytes : $000000CC
  packed bytes  : $00003ABC
  packed ratio  : 52.44%
  output        : "mydata.b2"
```

### Quiet Mode

Quiet mode keeps the `input[...]` lines but reduces the final summary to a single line:

```text
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
input[02]  start=$00001234  size=$00004567  pad=$000000CC  "file2.bin"
summary files=$00000002 source=$00005679 pad=$000000CC packed=$00003ABC ratio=52.44% output="mydata.b2"
```

### Field Meanings

- `input[##]`: 1-based input file number.
- `start=`: Start offset of that file in the combined source buffer.
- `size=`: Raw size of the file before padding.
- `pad=`: Zero padding added after that file.
- `files`: Number of input files packed.
- `source`: Combined size of all input files before padding.
- `pad`: Total padding bytes inserted between files.
- `packed`: Final compressed `.b2` size.
- `ratio`: Packed size as a percentage of the padded combined source size.
- `output`: Output filename written by rcPacker.

## Examples

### Single File

```bash
./rcpacker sprite_tileset.bin
```

### Multi-File with Custom Output

```bash
./rcpacker bg_tiles.bin sprites.bin palettes.bin -o graphics_data.b2
```

### Multi-File with Padding

```bash
./rcpacker level_data.bin enemy_data.bin audio_data.bin -p -o game_pack.b2
```

### Quiet Summary Output

```bash
./rcpacker asset1.bin asset2.bin asset3.bin -q -o assets.b2
```

### Validation

```bash
./rcpacker asset1.bin asset2.bin -p -v
```

## Technical Details

### ByteBoozer V2 Format

rcPacker uses the ByteBoozer V2 compression algorithm, adapted here for the GameShell65 tooling flow.

Original sources and references:
- https://csdb.dk/release/?id=145031
- https://github.com/luigidifraia/ByteBoozer2

### Memory and Performance

- Input files are read and concatenated in memory before compression.
- Compression is performed on the combined buffer.
- The build uses `-O3` optimization.

### Exit Codes

- `0`: Success.
- Non-zero: Error during reading, compression, validation, or output writing.

## License

Part of the GameShell65 project.
