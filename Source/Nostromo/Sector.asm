.module Sector

; ==========================================================================
; Draw
; --------------------------------------------------------------------------
; Draws a complete sector on the screen.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the sector data.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Draw:

	ld a,(ix) ; Number of walls in the sector.
	or a
	ret z
	ld b,a
	inc ix

Draw.Loop:
	push bc
	
	ld l,(ix)
	inc ix
	ld h,(ix)
	inc ix
	
	; HL points to the vertex indices.
	
	push hl
	
	; Start vertex.
	
	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,saveSScreen
	add hl,de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Wall.Start.X),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Wall.Start.Y),de
	
	pop hl
	inc hl
	
	; End vertex.

	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,saveSScreen
	add hl,de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Wall.End.X),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Wall.End.Y),de

	push ix
	
	call Wall.ClipAndDraw
	
	pop ix
	
	pop bc
	djnz Draw.Loop
	
	ret
	

.endmodule