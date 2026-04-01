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

## Examples

### Single File

```bash
./rcpacker sprite_tileset.bin
```
This compresses `sprite_tileset.bin` by itself and writes the default output file `sprite_tileset.bin.b2`.

### Single File with Custom Output

```bash
./rcpacker sprite_tileset.bin -o sprite_pack.b2
```
This compresses `sprite_tileset.bin` by itself and writes the packed output to `sprite_pack.b2`.

### Multi-File with Custom Output

```bash
./rcpacker bg_tiles.bin sprites.bin palettes.bin -o graphics_data.b2
```
This concatenates the three input files in the given order, compresses them as one stream, and writes the result to `graphics_data.b2`.

### Multi-File with Padding

```bash
./rcpacker level_data.bin enemy_data.bin audio_data.bin -p -o game_pack.b2
```
This concatenates and compresses all three files, adding zero padding after each file except the last one so the next file starts on a 256-byte boundary, then writes `game_pack.b2`.

### Quiet Summary Output

```bash
./rcpacker asset1.bin asset2.bin asset3.bin -q -o assets.b2
```
This compresses the three files into `assets.b2` while hiding low-level crunch/decrunch chatter and ending with a concise one-line summary.

### Validation

```bash
./rcpacker asset1.bin asset2.bin -p -v
```
This compresses the two files with 256-byte alignment padding between them, then decrunches and validates the output against the original combined data.

## Command-Line Options

### Input Files

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
Writes the packed output to `packed_data.b2` instead of using the default filename.

```bash
rcpacker tileset.bin
```
Uses the default output naming and writes to `tileset.bin.b2`.


### `-q`

Quiet mode.

Behavior:
- Keeps per-file `input[...]` lines visible.
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
- Compares decrunched bytes against the original combined source buffer.
- Includes any zero padding added by `-p` in the comparison.
- Returns a non-zero exit code if validation fails.

Example:

```bash
rcpacker file1.bin file2.bin -p -v
```

Validation output looks like this on success:

```text
[Validate] Original size:   0x00005679 bytes
[Validate] Round-trip size: 0x00005679 bytes
Validation OK: round-trip data matches original input.
[Validate] Compared bytes:  0x00005679
```

## Console Output

### Normal Mode

Normal mode prints input lines, `[Crunch]` diagnostics, and a final multi-line summary:

```text
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
input[02]  start=$00001234  size=$00004567  pad=$000000CC  "file2.bin"

[Crunch] Input file size:   0x00005745 bytes
[Crunch] Packed stream:     0x00003AB4 bytes (65.13% of input)
[Crunch] Margin:            24 bytes
[Crunch] Mode:              data (raw binary)
[Crunch] Load address:      0x00001CA1
[Crunch] Original size:     0x00005745 bytes
[Crunch] Output size:       0x00003ABC bytes (full .b2)

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

## Technical Details

Original sources and references:
- https://csdb.dk/release/?id=145031
- https://github.com/luigidifraia/ByteBoozer2

### Memory and Performance

- Input files are read and concatenated in memory.
- Compression is performed on the combined buffer.
- The build uses `-O3` optimization.

### Exit Codes

- `0`: Success.
- Non-zero: Error during reading, compression, validation, or output writing.

## License

Part of the GameShell65 project.
