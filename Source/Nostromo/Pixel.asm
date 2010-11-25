.module Pixel
Code:

;-----> Get pixel information
; input:	e=y coordinate
;		a=x coordinate
; output:	a holds data for pixel (e.g. %00100000)
;		hl->byte where pixel is on the gbuf
GetInformation:
	ld	d,$00
	ld	h,d
	ld	l,e
	add	hl,de
	add	hl,de
	add	hl,hl
	add	hl,hl
	ld	de,plotSScreen
	add	hl,de
	ld	b,$00
	ld	c,a
	and	%00000111
	srl	c
	srl	c
	srl	c
	add	hl,bc
	ld	b,a
	inc	b
	ld	a,%00000001
getPixelLoop:
	rrca
	djnz getPixelLoop
	ret

.if Options.ReportModuleSizes \ .echoln strformat("Pixel module: {0:N0} bytes.", $-Code) \ .endif
.endmodule