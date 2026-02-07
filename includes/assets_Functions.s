// ------------------------------------------------------------
.var 	Asset_CurrPtr = 0
.var    Asset_CurrMax = 0
.var	Section_Current = 0

.struct Section { id, name, baseAddr, currAddr, size }

.struct Asset { id, name, baseChar, addr, crunchAddress, size, filenamePtr, binary }

// ------------------------------------------------------------
.var SectionList = List()

// ------------------------------------------------------------
.function StartSection (name, addr, size) 
{
	.var id = SectionList.size()

	.var newSection = Section(id,name,addr,addr,size)
	.eval Section_Current = newSection

	.eval SectionList.add( newSection )

	.eval Asset_CurrPtr = addr
	.eval Asset_CurrMax = addr + size

	.print "adding '" + name + "' at [$" + toHexString(Asset_CurrPtr) + "] -> [$" + toHexString(Asset_CurrMax-1) + "]"

	.return SectionList.get(id)
}

.function EndSection ()
{
	.print "ending '" + Section_Current.name + "' end addr = [$" + toHexString(Section_Current.currAddr) + "]"
}

// ------------------------------------------------------------
.var AssetList = List()

// ------------------------------------------------------------
.function AddAsset (name, path) 
{
	.var id = AssetList.size()
	.var bin = LoadBinary(path)

	.var crunchAddress = bin.uget(0) + (bin.uget(1) << 8) + (bin.uget(2) << 16) + (bin.uget(3) << 24)

	.var newAsset = Asset(id, name, (Asset_CurrPtr/64), Asset_CurrPtr, crunchAddress, bin.getSize(), 0, bin)

	.eval AssetList.add( newAsset )

	.if (Asset_CurrPtr + newAsset.binary.getSize() > Asset_CurrMax)
	{
		.error "    Adding " + path + " ran out of space: $" + toHexString((Asset_CurrPtr + newAsset.binary.getSize()) - Asset_CurrMax) + " bytes too big"
	}

	.print "    adding asset '" + path + "' at baseChar [$" + toHexString(newAsset.baseChar) + "] " + newAsset.name + " at $" + toHexString(newAsset.addr) + " size = $" + toHexString(newAsset.binary.getSize()) + " crunchaddress = $" + toHexString(newAsset.crunchAddress)
	
	.eval Asset_CurrPtr += newAsset.binary.getSize()
	.eval Asset_CurrPtr = (Asset_CurrPtr + $00ff) & $fffff00

	.eval Section_Current.currAddr = Asset_CurrPtr

	.return AssetList.get(id)
}

// ------------------------------------------------------------
