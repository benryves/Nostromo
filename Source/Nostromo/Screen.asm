.module Screen

; ==========================================================================
; Copy
; --------------------------------------------------------------------------
; Copies the screen buffer to the display.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Copy:
	di
-:	in a,($10) \ rla \ jr c,-
	ld a,$80
	out ($10),a
	ld hl,plotSScreen-12-(-(12*64)+1)
	ld c,$20
fastCopyAgain:
	ld b,64
	ld de,-(12*64)+1
-:	in a,($10) \ rla \ jr c,-
	ld a,c
	out ($10),a
	add hl,de
	ld de,12
fastCopyLoop:
	add hl,de
-:	in a,($10) \ rla \ jr c,-
	ld a,(hl)
	out ($11),a
	djnz fastCopyLoop
	inc c
	ld a,c
	cp $2B+1
	jr nz,fastCopyAgain
	ret

.endmodule