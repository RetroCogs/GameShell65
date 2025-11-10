//--------------------------------------------------------
// Assets
//--------------------------------------------------------
.namespace Assets
{
.segment Code "Assets NameList"

Names:
	.for(var i=0; i<AssetList.size(); i++)
	{
		.eval AssetList.get(i).filenamePtr = *
		.text AssetList.get(i).name
		.byte $00
	}
}

// ------------------------------------------------------------
