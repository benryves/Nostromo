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

; --------------------------------------------------------------------------
; How many things do we have to draw?
; --------------------------------------------------------------------------
	
	ld a,(ix+2)
	or a
	jr z,NoThings

; --------------------------------------------------------------------------
; We will have a number of things to draw, so push the subsector and current
; clipping information onto thing stack.
; --------------------------------------------------------------------------

	bit RenderFlag.DrawThings,(iy+RenderFlags)
	call nz,Things.SubSectorStack.Push

; --------------------------------------------------------------------------
; Skip past the things to draw in the subsector definition before drawing
; the walls.
; --------------------------------------------------------------------------

	ld l,(ix+2)
	ld h,0
	add hl,hl
	ex de,hl
	add ix,de

NoThings:
	inc ix
	inc ix
	inc ix
	ld a,(ix)
	or a
	ret z
	ld b,a
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
	ld bc,(Level.TransformedVertices)
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