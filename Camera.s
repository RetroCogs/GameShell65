.namespace Camera
{

// ------------------------------------------------------------
//
.segment BSS "Camera"
XScroll:		.byte $00,$00
YScroll:		.byte $00,$00

CamVelX:		.word $0000
CamVelY:		.word $0000

// ------------------------------------------------------------
//
.segment Code "Camera"
Init: 
{
	_set16im(0, XScroll)
	_set16im(0, YScroll)

	rts
}

// ------------------------------------------------------------

}