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

```bash
rcpacker tileset.bin
```

### `-p`
Pad each input file except the last one so the next file begins on a 256-byte boundary.

Default behavior:
- Without `-p`, files are concatenated directly with no padding.

Details:
- Padding is added after each file, not before the next one.

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
- Includes any zero padding added by `-p` in the comparison.
- Returns a non-zero exit code if validation fails.
rcpacker file1.bin file2.bin -v
```

```bash
rcpacker file1.bin file2.bin -p -v
```

## Console Output
Normal mode prints one line per input file plus a final multi-line summary:

```text
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
input[02]  start=$00001234  size=$00004567  pad=$000000CC  "file2.bin"
  packed bytes  : $00003ABC
  packed ratio  : 52.44%
  output        : "mydata.b2"
```

### Quiet Mode

Quiet mode keeps the `input[...]` lines but reduces the final summary to a single line:

```text
input[01]  start=$00000000  size=$00001234  pad=$00000000  "file1.bin"
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
