// pixie function
// 

.struct PixieLayout { id, width, height, charsWide, charsHigh, numBytesPerRow, maxRowOffs }

.var PixieLayoutList = List()

.function Pixie_AddLayout (width, height) 
{
	//id, name, address, spriteSet, startFrame, endFrame 
	.var pid = PixieLayoutList.size()

	.var charsWide = (width / 16)
	.var charsHigh = (height / 8)
	.var numBytesPerRow = ((charsWide * 2) + 2)
	.var maxRowOffs = ((NUM_PIXIEWORDS * 2) - ((charsWide * 2) + 2))

	.eval PixieLayoutList.add(PixieLayout(
		pid,
		width,
		height,
		charsWide,
		charsHigh,
		numBytesPerRow,
		maxRowOffs
	))

	.return PixieLayoutList.get(pid)
}

.const Pixie_16x8 = Pixie_AddLayout(16, 8)
.const Pixie_16x16 = Pixie_AddLayout(16, 16)
.const Pixie_16x24 = Pixie_AddLayout(16, 24)
.const Pixie_16x32 = Pixie_AddLayout(16, 32)
.const Pixie_32x8 = Pixie_AddLayout(32, 8)
.const Pixie_32x16 = Pixie_AddLayout(32, 16)
.const Pixie_32x24 = Pixie_AddLayout(32, 24)
.const Pixie_32x32 = Pixie_AddLayout(32, 32)
.const Pixie_48x48 = Pixie_AddLayout(48, 48)

