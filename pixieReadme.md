# Pixie System README

## Overview

The pixie system is the project's lightweight software sprite drawing layer for the MEGA65. It builds per-row pixie command streams into work RAM, which are then consumed by the VICIV's Raster Rewrite Buffer.

At a high level:
- `DrawPixie` adds one pixie object into the per-row work buffers
- `ClearWorkPixies` resets those buffers each frame
- callers set global draw parameters such as position, palette, and base character

---

## Core state

The pixie system uses these zero-page variables in `includes/pixie_code.s`:

- `DrawPosX` / `DrawPosY` — 16-bit screen position
- `DrawBaseChr` — first character index for the pixie graphic set
- `DrawPal` — palette selector (palIndex << 4) | $0f 
- `DrawSChr` — sprite character offset within the graphic set
- `DrawMode` — attribute mode byte used to select between NCM ($08) and FCM ($00)
- `PixieYShift` — vertical compensation when using TextPosY scrolling

These are treated as the input parameters to `DrawPixie`.

---

## Pixie layouts

The available pixie layout IDs are:

- `Pixie_16x8.id`
- `Pixie_16x16.id`
- `Pixie_16x24.id`
- `Pixie_16x32.id`
- `Pixie_32x8.id`
- `Pixie_32x16.id`
- `Pixie_32x24.id`
- `Pixie_32x32.id`
- `Pixie_48x48.id`

Each layout determines:
- character width
- character height
- bytes used per row in the pixie work list

### Example char layout for `Pixie_32x32.id`

Each char is 64 bytes and the index used for DrawBaseChar is the physical address of the graphics data / 64.

A `32x32` NCM pixie is `2` chars wide by `4` chars high. The char order starts at the **top-left**, increments **down the column**, then proceeds to the **next column**:

```text
+-------+-------+
|   0   |   4   |
+-------+-------+
|   1   |   5   |
+-------+-------+
|   2   |   6   |
+-------+-------+
|   3   |   7   |
+-------+-------+
```

So the left column uses chars `0,1,2,3`, and the right column uses chars `4,5,6,7`.

### Example char layout for `64x16`

A `64x16` NCM pixie would be `4` chars wide by `2` chars high. Using the same ordering rule — start at the **top-left**, increment **down the column**, then move to the **next column** — the layout would be:

```text
+-------+-------+-------+-------+
|   0   |   2   |   4   |   6   |
+-------+-------+-------+-------+
|   1   |   3   |   5   |   7   |
+-------+-------+-------+-------+
```

So each column is filled top-to-bottom before moving right.

---

## Frame flow

Typical usage each frame is:

1. Clear the pixie work buffers
2. Set draw parameters for each object
3. Call `DrawPixie`
4. Let the layer/hardware update code consume the prepared work buffers

Example:

```kickassembler
// call this at the start of the new frame
jsr ClearWorkPixies

// set up one pixie
lda #<sprite32x32Chars.baseChar
sta DrawBaseChr+0
lda #>sprite32x32Chars.baseChar
sta DrawBaseChr+1

lda #(PAL_SPR << 4) | $0f
sta DrawPal

lda #$00
sta DrawSChr

lda #$40
sta DrawPosX+0
lda #$00
sta DrawPosX+1

lda #$50
sta DrawPosY+0
lda #$00
sta DrawPosY+1

ldx #Pixie_32x32.id
jsr DrawPixie
```

---

## What `DrawPixie` does

`DrawPixie`:
- looks up the selected layout
- clips against layout width and row bounds
- computes row placement and sub-row shift
- emits a `GOTOX` command and one or more character words into the row's tile/attribute buffers
- handles top, middle, and bottom row masking for vertically shifted pixies

It writes into:
- `MappedPixieWorkTiles`
- `MappedPixieWorkAttrib`

and tracks row usage through:
- `PixieUseOffset`
- `PixieRowScreenPtrLo/Hi`
- `PixieRowAttribPtrLo/Hi`

---

## Initialization and clearing

### `InitPixies`
Initializes the pixie tile workspace by clearing it.

```kickassembler
jsr InitPixies
```

### `ClearWorkPixies`
Call once per frame before adding new pixies.

```kickassembler
jsr ClearWorkPixies
```

This resets:
- per-row write pointers
- row usage offsets
- attribute bytes to `GOTOX + transparent`

---

## Notes and constraints

- `DrawPixie` expects the global draw variables to be set before calling it.
- The selected layout ID must be passed in `X`.
- The routine clips vertically and horizontally against the active layout.
- `PixieYShift` is used when the base layer scrolls vertically.
- The work buffers must be cleared each frame or old commands may remain visible.

---

## Minimal recipe

For a single pixie:

```kickassembler
jsr ClearWorkPixies

_set16im(mySpriteChars.baseChar, DrawBaseChr)
_set8im((PAL_SPR << 4) | $0f, DrawPal)
_set8im(0, DrawSChr)
_set16im(100, DrawPosX)
_set16im(60, DrawPosY)

ldx #Pixie_32x32.id
jsr DrawPixie
```

That is the essential usage pattern.
