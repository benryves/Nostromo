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
	
; --------------------------------------------------------------------------
; Get the angle.
; --------------------------------------------------------------------------

	ld a,(hl)
	ld (Wall.Angle),a
	ld e,a
	ld a,(Render.Camera.Angle)
	add a,e
	ld (Wall.Angle.Transformed),a
	inc hl
	
	push bc
	
; --------------------------------------------------------------------------
; Read the start vertex information.
; --------------------------------------------------------------------------
	
	push hl
	
	call GetTransformedVertex
	ld de,Wall.Start.X
	ldi
	ldi
	ldi
	ldi
	
	pop hl
	inc hl
	
; --------------------------------------------------------------------------
; Read the end vertex information.
; --------------------------------------------------------------------------

	push hl

	call GetTransformedVertex
	ld de,Wall.End.X
	ldi
	ldi
	ldi
	ldi
	
	pop hl
	inc hl

; --------------------------------------------------------------------------
; Get the sector pointers.
; --------------------------------------------------------------------------
	
	.if Sector.DataSize != 4
		.echoln "Sectors are no longer 4 bytes (fix this)"
	.endif

	ld e,(hl)
	inc hl
	push hl
	ld d,0
	ex de,hl
	
	add hl,hl
	add hl,hl
	
	ld bc,(Level.Sectors)
	add hl,bc
	ld (Sector.Front),hl
	ld (Sector.Back),hl
	
	pop hl
	
	bit Wall.DrawFlag.FillMiddle,(iy+Wall.DrawFlags)
	jr nz,+
	
	ld l,(hl)
	ld h,0
	
	add hl,hl
	add hl,hl
	
	ld de,(Level.Sectors)
	add hl,de
	
	ld (Sector.Back),hl
+:

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

; ==========================================================================
; GetTransformedVertex
; --------------------------------------------------------------------------
; Gets a transformed vertex. Vertices may be cached.
; --------------------------------------------------------------------------
; Inputs:    HL: Pointer to vertex number.
; Outputs:   HL: Pointer to transformed vertex.
; Destroyed: AF, BC, DE.
; ==========================================================================
GetTransformedVertex:

	ld l,(hl)
	ld e,l
	ld h,0
	add hl,hl
	add hl,hl
	push hl
	ld bc,(Level.TransformedVertices)
	add hl,bc
	pop bc
	
	ld d,Vertices.AlreadyTransformed >> 8
	ld a,(de)
	or a
	ret nz
	
	dec a
	ld (de),a

	push hl
	
	ld hl,(Level.Vertices)
	add hl,bc
	
	ld c,(hl) \ inc hl
	ld b,(hl) \ inc hl
	ld e,(hl) \ inc hl
	ld d,(hl)
	
	call Vertices.Transform
	
	pop hl
	push hl
	
	ld (hl),c \ inc hl
	ld (hl),b \ inc hl
	ld (hl),e \ inc hl
	ld (hl),d
	
	pop hl
	
	ret


.endmodule