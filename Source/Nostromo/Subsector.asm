.module Subsector

; ==========================================================================
; Draw
; --------------------------------------------------------------------------
; Draws a complete subsector on the screen.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the subsector data.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Draw:

	inc ix
	inc ix
	
	ld b,(ix)
	inc ix

Draw.Loop:
; --------------------------------------------------------------------------
; Load the pointer to the wall.
; --------------------------------------------------------------------------

	ld l,(ix)
	inc ix
	ld h,(ix)
	inc ix

; --------------------------------------------------------------------------
; Load the wall flags.
; --------------------------------------------------------------------------

	ld a,(hl)

; --------------------------------------------------------------------------
; Have we already drawn this wall?
; --------------------------------------------------------------------------

	bit Wall.DrawFlag.DrawnThisFrame,a
	jp nz,AlreadyDrawnWall

; --------------------------------------------------------------------------
; Copy the wall flags over.
; --------------------------------------------------------------------------

	ld (iy+Wall.DrawFlags),a

; --------------------------------------------------------------------------
; Flag the wall as being drawn.
; --------------------------------------------------------------------------

	set Wall.DrawFlag.DrawnThisFrame,(hl)
	inc hl
	
	push bc
	
; --------------------------------------------------------------------------
; Read the start vertex information.
; --------------------------------------------------------------------------
	
	push hl
	
	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	ld bc,TransformedVertices
	add hl,bc
	
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
	push hl
	
; --------------------------------------------------------------------------
; Read the end vertex information.
; --------------------------------------------------------------------------

	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	add hl,bc
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Wall.End.X),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Wall.End.Y),de
	
	pop hl
	inc hl
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Sector.Front),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Sector.Back),de

; --------------------------------------------------------------------------
; Clip and draw the wall.
; --------------------------------------------------------------------------

	push ix
	
	call Wall.ClipAndDraw
	
	pop ix
	pop bc

; --------------------------------------------------------------------------
; Advance to the next wall.
; --------------------------------------------------------------------------
AlreadyDrawnWall:
	djnz Draw.Loop
	
	ret
	

.endmodule