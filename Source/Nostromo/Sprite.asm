.module Sprite
Code:

; ==========================================================================
; DrawColumn
; --------------------------------------------------------------------------
; Draws a column of a sprite.
; --------------------------------------------------------------------------
; Inputs:    L: X coordinate of the column. This can be out of bounds.
;            Column.Top.Clipped: Clipped row of the top of the column.
;            Column.Bottom.Clipped: Clipped row of the bottom of the column.
;            Column.Top: Full row of the top of the column.
;            Column.DestinationHeight: Height of the column to draw.
;            Column.SourceHeight: Height of the sprite.
;            Column.SourceData: Pointer to the sprite column data.
; Destroyed: AF, BC, DE, HL, AF', BC', DE', HL'.
; ==========================================================================
DrawColumn:

; --------------------------------------------------------------------------
; Is the column on the screen?
; --------------------------------------------------------------------------

	ld a,l
	or a
	ret m
	cp 96
	ret nc

; --------------------------------------------------------------------------
; Clip to the top edge.
; --------------------------------------------------------------------------

	ld a,(Column.Top.Clipped)
	ld h,TopEdgeClip >> 8
	cp (hl)
	jr nc,+
	ld a,(hl)
+:	
	inc h
	cp (hl)
	jr c,+
	ld a,(hl)
+:
	dec a
	ld c,a

; --------------------------------------------------------------------------
; Clip to the bottom edge.
; --------------------------------------------------------------------------

	ld a,(Column.Bottom.Clipped)
	ld h,TopEdgeClip >> 8
	cp (hl)
	jr nc,+
	ld a,(hl)
+:	
	inc h
	cp (hl)
	jr c,+
	ld a,(hl)
+:
	dec a
	ld b,a

; --------------------------------------------------------------------------
; Is the height <= 0?
; --------------------------------------------------------------------------

	sub c
	ret c
	ret z
	
	ld b,a

; --------------------------------------------------------------------------
; Store the number of rows to render to the stack.
; --------------------------------------------------------------------------
	
	push bc
	
; --------------------------------------------------------------------------
; Calculate where on the screen the first pixel should go.
; --------------------------------------------------------------------------

	ld a,l
	ld e,c
	
	call Pixel.GetInformation
	ld c,a
	push hl
	push bc
	
	exx
	pop bc
	pop hl
	ld de,12
	exx

; --------------------------------------------------------------------------
; Restore the height, ready to draw row-by-row.
; --------------------------------------------------------------------------

	pop bc
	
; --------------------------------------------------------------------------
; Load the offset to the source row.
; --------------------------------------------------------------------------

Column.SourceData = $+1
	ld hl,0

; --------------------------------------------------------------------------
; Load the scale factor into DE.
; --------------------------------------------------------------------------

Column.DestinationHeight = $+1
Column.SourceHeight = $+2
	ld de,0

; --------------------------------------------------------------------------
; Is the top of the thing clipped?
; --------------------------------------------------------------------------

	ld a,c
	xor $80
	ld c,a
	ld a,(Column.Top)
	xor $80
	sub c
	jr nc,TopNotClipped
	
; --------------------------------------------------------------------------
; The top of the thing is clipped, so advance source pointer accordingly.
; --------------------------------------------------------------------------
	
	push bc
	
	neg
	ld b,a
	
	ld a,e
	
	ld c,%10000000

--:	sub d
	jp p,++
-:	rrc c
	rrc c
	jr nc,+
	inc hl
+:	add a,e
	jp m,-
++:	djnz --
	
	ex af,af'
	ld a,c
	pop bc
	ld c,a
	ex af,af'
	
	jr RowLoop

TopNotClipped:

; --------------------------------------------------------------------------
; Initialise the error.
; --------------------------------------------------------------------------

	ld a,e

; --------------------------------------------------------------------------
; Start from the first pixel in the source row.
; --------------------------------------------------------------------------

	ld c,%10000000

; --------------------------------------------------------------------------
; Draw all of the rows of the sprite.
; --------------------------------------------------------------------------

RowLoop:

; --------------------------------------------------------------------------
; Are we drawing a pixel?
; --------------------------------------------------------------------------

	ex af,af'
	ld a,(hl)
	and c
	jr z,Pixel.Skip
	
	rrc c
	ld a,(hl)
	and c
	jr nz,Pixel.Set

; --------------------------------------------------------------------------
; Clears the pixel (white).
; --------------------------------------------------------------------------
Pixel.Clear:
	rlc c
	exx
	ld a,c
	cpl
	and (hl)
	ld (hl),a
	add hl,de
	exx
	jr Pixel.Drawn

; --------------------------------------------------------------------------
; Sets the pixel (black).
; --------------------------------------------------------------------------
Pixel.Set:
	rlc c
	exx
	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	exx
	jr Pixel.Drawn

; --------------------------------------------------------------------------
; Skips the pixel (transparent).
; --------------------------------------------------------------------------
Pixel.Skip:
	exx
	add hl,de
	exx

; --------------------------------------------------------------------------
; We have drawn the pixel.
; --------------------------------------------------------------------------
Pixel.Drawn:

	ex af,af'

; --------------------------------------------------------------------------
; Do we need to advance to the next row in the source?
; --------------------------------------------------------------------------

	sub d
	jp p,NoAdvanceRow

; --------------------------------------------------------------------------
; If so, advance to the next row.
; --------------------------------------------------------------------------

AdvanceRow:

-:	rrc c
	rrc c
	jr nc,+
	inc hl
+:	add a,e
	jp m,-

NoAdvanceRow:

; --------------------------------------------------------------------------
; Draw the next row.
; --------------------------------------------------------------------------

	djnz RowLoop

; --------------------------------------------------------------------------
; We're done.
; --------------------------------------------------------------------------
	
	ret

.if Options.ReportModuleSizes \ .echoln strformat("Sprite module: {0:N0} bytes.", $-Code) \ .endif
.endmodule