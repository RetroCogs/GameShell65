# Cruncher.c - Function Documentation

This document provides detailed explanations of how each function in `cruncher.c` works. The cruncher implements a compression algorithm for ByteBoozer format, optimizing data using an LZ77-style matching algorithm.

---

## Overview

The cruncher processes data in two main phases:
1. **Analysis Phase**: Identifies all possible matches and calculates optimal compression paths
2. **Encoding Phase**: Writes the compressed data using variable-length encoding

---

## Bit and Byte Writing Functions

### `void wBit(uint bit)`
**Purpose**: Write a single bit to the output buffer.

**How it works**:
- Maintains a byte buffer (`curByte`) with a current bit position (`curCnt`)
- Each bit is shifted into the byte from the most significant bit
- When 8 bits have been accumulated, the byte is written to `obuf` and a fresh byte is started
- Tracks output position in `put` variable

**Key Variables**:
- `curByte`: The current byte being assembled
- `curCnt`: Number of bits remaining in current byte (0-8)
- `curIndex`: Position in output buffer where current byte will be stored

---

### `void wFlush()`
**Purpose**: Complete the current byte by padding with zeros and write it to output.

**How it works**:
- Shifts remaining bits to align to byte boundary
- Clears the bit count (`curCnt = 0`)
- Writes the final byte to `obuf[curIndex]`
- Called at the end of compression to finalize output

---

### `void wByte(uint b)`
**Purpose**: Write a complete byte directly to the output buffer.

**How it works**:
- Writes byte `b` at position `put` in `obuf`
- Increments `put` to point to next available position
- Used for raw byte data and inverted offset bytes

---

### `void wBytes(uint get, uint len)`
**Purpose**: Write multiple consecutive bytes from input buffer to output.

**How it works**:
- Iterates `len` times, starting from position `get` in `ibuf`
- Each byte from input is written to output via `wByte()`
- Used for writing literal runs without compression

**Parameters**:
- `get`: Starting position in input buffer
- `len`: Number of bytes to copy

---

## Length and Offset Encoding Functions

### `void wLength(uint len)`
**Purpose**: Encode a length value using variable-length encoding.

**How it works**:
- Uses Golomb-Rice style encoding with leading 1-bits
- Finds the most significant bit of the length
- Writes 1-bits for each bit position above MSB, then 0
- Continues writing remaining bits after MSB

**Example**:
- Length 1 (binary: 0001): Writes `0` (special case)
- Length 2 (binary: 0010): Writes `1 0` (MSB at position 1)
- Length 3 (binary: 0011): Writes `1 1` (MSB at position 1)
- Length 4 (binary: 0100): Writes `1 0 0` (MSB at position 2)

---

### `void wOffset(uint offset, uint len)`
**Purpose**: Encode an offset value with adaptive bit-width based on length category.

**How it works**:
- Determines offset category (SHORT or LONG) based on match length
- SHORT offsets use fewer bits (3-10 bits) for length-1 matches
- LONG offsets use more bits (4-13 bits) for matches length ≥ 2
- Writes 2-bit category selector, then offset bits
- For offsets ≥ 256 bytes, writes bits then inverted low byte

**Offset Categories**:

| Category | SHORT Bits | LONG Bits | Range |
|----------|-----------|----------|-------|
| 0        | 3         | 4        | 0-7(short), 0-15(long) |
| 1        | 6         | 7        | 8-63(short), 16-127(long) |
| 2        | 8         | 10       | 64-255(short), 128-1023(long) |
| 3        | 10        | 13       | 256+(short), 1024+(long) |

**Special Cases**:
- Single-byte matches (len=1) limited to SHORT_MAX_OFFSET (4095)
- Multi-byte matches can use LONG offsets up to 8191
- Bits < 8 are stored directly, high byte is inverted and written as byte

---

## Cost Calculation Functions

These functions determine the bit cost of encoding various elements. The compression algorithm uses these to find the optimal sequence.

### `uint costOfLength(uint len)`
**Purpose**: Calculate the bit cost to encode a given length.

**Cost Table**:
- Length 1: 1 bit
- Length 2-3: 3 bits
- Length 4-7: 5 bits
- Length 8-15: 7 bits
- Length 16-31: 9 bits
- Length 32-63: 11 bits
- Length 64-127: 13 bits
- Length 128-255: 14 bits

---

### `uint costOfOffset(uint offset, uint len)`
**Purpose**: Calculate the bit cost to encode a given offset for a match of given length.

**How it works**:
- Determines offset category based on whether length is 1 or > 1
- Returns the bit count for the appropriate category
- Uses conditional macros (COND_SHORT_*, COND_LONG_*) to find category

**Returns**: Number of bits needed to encode the offset

---

### `uint calculateCostOfMatch(uint len, uint offset)`
**Purpose**: Calculate total bit cost of encoding a match operation.

**Formula**:
```
Total Cost = 1 (copy-bit) 
           + costOfLength(len - 1)
           + 2 (offset category bits)
           + costOfOffset(offset - 1, len - 1)
```

**Parameters**:
- `len`: Match length
- `offset`: Distance back to match source

---

### `uint calculateCostOfLiteral(uint oldCost, uint litLen)`
**Purpose**: Calculate cost incrementally for adding one more literal byte.

**How it works**:
- Base cost: 8 bits per byte
- Additional bonuses for lengths at powers/multiples of 2
- Cost of 1 literal: +8 bits
- Cost of 128 literals: +8 bits (already counted)
- Lengths like 2, 4, 8, 16, 32, 64 get +2 extra bits

**Note**: Function has FIXME comment about cost model issues with short matches being prioritized over longer literal runs.

---

## Data Structure Setup Functions

### `void setupHelpStructures()`
**Purpose**: Pre-process the input data to enable fast match finding.

**Creates two data structures**:

#### RLE Information (`rleInfo[]`)
- Scans from end to beginning for run-length encoded sequences
- For each position with repeated bytes:
  - `length`: How many consecutive identical bytes
  - `value`: The byte value
  - `valueAfter`: The byte that follows the run
- Used to optimize matching of repeated data

#### Linked List Structure (`first[]`, `last[]`, `link[]`)
- Creates a hash table of 2-byte sequences (65536 possible combinations)
- For each unique 2-byte pattern, maintains a linked list of all positions where it occurs
- `first[pattern]`: First occurrence of pattern
- `last[pattern]`: Last occurrence to quickly add new links
- `link[position]`: Points to next position with same 2-byte pattern
- Enables O(1) lookup to find all potential match locations

**Algorithm**:
1. Iterate through input backwards
2. For RLE runs, record length and skip past run
3. For non-RLE bytes:
   - Build 2-byte key from current and previous byte
   - Add position to linked list for that key
   - Move to next position

---

## Match Finding Function

### `void findMatches()`
**Purpose**: Find all possible matches at each position and use dynamic programming to find optimal compression path.

**Overall Strategy**:
- Uses dynamic programming with cost-based pathfinding
- At each position, calculates:
  - Cost of using matches of various lengths
  - Cost of using literal bytes
  - Selects the cheapest path to reach each position
- Process works backwards through the input

**Main Algorithm**:

1. **For each position** (from end to beginning):

2. **Find all match candidates**:
   - Regular matches: Scan backward through linked list
     - Compare bytes backwards to find full match length
     - Store only longest match for each length (2-255 bytes)
     - Candidates limited by MAX_OFFSET distance and longest match so far
   
   - RLE matches: If current position is in RLE run:
     - Match with self-reference (1-byte offset)
     - Search for other matching RLE sequences
     - Check for matches extending beyond RLE run

3. **Calculate costs for all match paths**:
   - For each discovered match of length `len` at offset `offset`:
     - Calculate total cost: `lastNode.cost + costOfMatch(len, offset)`
     - If cheaper than current best path to that position, record it

4. **Calculate cost for literal path**:
   - Incrementally adds 8 bits per byte
   - Uses `calculateCostOfLiteral()` for special cases

5. **Update node information**:
   - For each position storing: cost, next position, literal run length, offset
   - These form the optimal path through the data

---

## Output Writing Function

### `int writeOutput()`
**Purpose**: Traverse the optimal path found by `findMatches()` and write compressed output.

**Output Format**:
- Copy-bit (0 = literal, 1 = match)
- For literals:
  - Copy-bit = 0
  - Length-encoded byte count
  - Raw bytes
- For matches:
  - Copy-bit = 1
  - Length-encoded match length
  - Offset-encoded distance
  - Next match requires another copy-bit

**How it works**:

1. Initialize bit writer state
2. Iterate from position 0 to end using `context[i].next` chain:
   - If `litLen == 0`: It's a match
     - Write copy-bit (1)
     - Write length via `wLength(len - 1)`
     - Write offset via `wOffset(offset - 1, len - 1)`
     - Jump to next position
   - If `litLen > 0`: It's a literal run
     - Write copy-bit (0)
     - Encode bytes in chunks (max 255 per chunk)
     - Write length via `wLength(len)`
     - Write raw bytes via `wBytes()`

3. **Margin calculation**:
   - Tracks how far ahead of compression pointer we are
   - Ensures compressed data doesn't catch up to uncompressed
   - Returns margin for file layout purposes

4. **Termination**:
   - Final copy-bit (1) + length 0xff signals end
   - Flush remaining bits

---

## Main Compression Function

### `bool crunch(File *aSource, File *aTarget, uint address, bool isExecutable, bool isRelocated)`
**Purpose**: Main entry point that orchestrates the entire compression process.

**Parameters**:
- `aSource`: Source file structure with data and size
- `aTarget`: Destination file structure (populated)
- `address`: Target address for execution (if executable)
- `isExecutable`: Whether to include decruncher code
- `isRelocated`: Whether to use relocation addressing

**Process**:

1. **Memory allocation**:
   - `ibuf`: Input buffer (skip first 2 bytes - load address)
   - `context`: DP nodes for pathfinding
   - `link`: Linked list structure pointers
   - `rleInfo`: RLE run information
   - `obuf`: Output buffer

2. **Data preparation**:
   - Copy input data (skip first 2 bytes which are load address)
   - Clear context structures
   - Build help structures (RLE, linked lists)
   - Find optimal matches via dynamic programming
   - Generate compressed output

3. **Output assembly**:
   
   **If executable**:
   - Include 213-byte (0xd5) decruncher code
   - Modify decruncher addresses:
     - `decrCode[0x1f:0x20]`: Transfer address
     - `decrCode[0xbc:0xbd]`: Depack-from address
     - `decrCode[0x85:0x86]`: Depack-to address (original load addr)
     - `decrCode[0xca:0xcb]`: Jump address
   - Output format: `[BASIC header] [Decruncher] [Packed data]`
   
   **If not executable (data)**:
   - Calculate start address based on:
     - Original load address
     - Compression ratio (margin)
     - Relocation flag
   - Output format: `[New load addr (2 bytes)] [Original load addr (2 bytes)] [Packed data]`

4. **Memory cleanup**:
   - Free all allocated structures

5. **Return**: True if successful

---

## Constants and Macros

### Offset Bit Widths
```c
NUM_BITS_SHORT_0 = 3    NUM_BITS_LONG_0 = 4
NUM_BITS_SHORT_1 = 6    NUM_BITS_LONG_1 = 7
NUM_BITS_SHORT_2 = 8    NUM_BITS_LONG_2 = 10
NUM_BITS_SHORT_3 = 10   NUM_BITS_LONG_3 = 13
```

### Maximum Offsets
```c
MAX_OFFSET = 8191 (2^13 - 1)              // Long offset limit
MAX_OFFSET_SHORT = 4095 (2^12 - 1)        // Short offset limit
```

### Decruncher
```c
DECRUNCHER_LENGTH = 0xd5 (213 bytes)      // Standalone decrunch code
```

---

## Algorithm Characteristics

**Time Complexity**: O(n × m) where n = input size, m = average match list length
**Space Complexity**: O(n) for buffers and structures

**Optimization Techniques**:
- Linked lists for O(1) pattern lookup
- RLE pre-detection for faster repeated data matching
- Dynamic programming for optimal path selection
- Adaptive bit widths based on match length
- Variable-length encoding for lengths and offsets

**Limitations**:
- Single-byte matches limited to short offset range
- Cost model may prioritize short matches over optimal literal runs
- No adaptive dictionary or context modeling
- Fixed decruncher keeps executable overhead reasonable (213 bytes)
