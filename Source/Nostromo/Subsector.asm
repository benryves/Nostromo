.module Subsector
Code:

; ==========================================================================
; Draw
; --------------------------------------------------------------------------
; Draws a complete subsector on the screen.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the subsector data.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Draw:

	.if Options.KeepStatistics
	ld hl,(Statistics.SubSectorsDrawn)
	inc hl
	ld (Statistics.SubSectorsDrawn),hl
	.endif

; --------------------------------------------------------------------------
; Is there a thing to draw?
; --------------------------------------------------------------------------
	
	ld a,(ix+1)
	or a
	jr z,NoThings

; --------------------------------------------------------------------------
; We will have at least one thing to draw, so push the subsector and current
; clipping information onto thing stack.
; --------------------------------------------------------------------------

	bit RenderFlag.DrawThings,(iy+RenderFlags)
	call nz,Things.SubSectorStack.Push

NoThings:

; --------------------------------------------------------------------------
; Skip over the sector index and first thing index.
; --------------------------------------------------------------------------

	inc ix
	inc ix

; --------------------------------------------------------------------------
; Are there any walls to draw?
; --------------------------------------------------------------------------

	ld a,(ix)
	or a
	ret z
	ld b,a
	inc ix
	
; --------------------------------------------------------------------------
; Get the implicit sector pointer.
; --------------------------------------------------------------------------
	
	.if Sector.DataSize != 4
		.echoln "Sectors are no longer 4 bytes (fix this)"
	.endif


	ld l,(ix-3)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,(Level.Sectors)
	add hl,de

	ld (Sector.Implicit),hl

Draw.Loop:
; --------------------------------------------------------------------------
; Load the pointer to the wall.
; --------------------------------------------------------------------------

	ld l,(ix)
	inc ix
	ld h,0
	
	add hl,hl
	add hl,hl
	add hl,hl
	
	ld de,(Level.Walls)
	add hl,de

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

	ld a,(hl)
	ld (Wall.Start.VertexIndex),a
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

	ld a,(hl)
	ld (Wall.End.VertexIndex),a
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

	bit Wall.DrawFlag.FillMiddle,(iy+Wall.DrawFlags)
	jr z,Sector.Wall.IsLowerUpper
	
	ld de,(Sector.Implicit)
	ld (Sector.Front),de
	
	jr Sector.Wall.SetSectorPointers

Sector.Wall.IsLowerUpper:

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
	
	pop hl
	
	
	ld l,(hl)
	ld h,0
	
	add hl,hl
	add hl,hl
	
	ld de,(Level.Sectors)
	add hl,de
	
	ld (Sector.Back),hl

Sector.Wall.SetSectorPointers:

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
	djnz +
	ret
+:	jp Draw.Loop

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

.if Options.ReportModuleSizes \ .echoln strformat("Subsector module: {0:N0} bytes.", $-Code) \ .endif
.endmodule