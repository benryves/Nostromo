.module Things

SubSectorStack.Top:
	.dw 0

SubSectorStack.Current:
	.dw 0

SubSectorStack.MaximumCapacity:
	.db 3

SubSectorStack.EntriesFree:
	.db 3

SubSectorStack.EntrySize = (96 * 2) + 2

DrawingSubSector: .dw 0

Transformed.X: .dw 0
Transformed.Y: .dw 0

Projected.X: .db 0

Projected.Y.Bottom: .dw 0
Projected.Y.Bottom.Clipped: .db 0

Projected.Y.Top: .dw 0
Projected.Y.Top.Clipped: .db 0

Projected.Width: .db 0

; ==========================================================================
; SubSectorStack.Push
; --------------------------------------------------------------------------
; Pushes information about the subsector to draw to the stack.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the list of things to draw within the subsector.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
SubSectorStack.Push:

; --------------------------------------------------------------------------
; Check that we have space on the stack.
; --------------------------------------------------------------------------

	ld a,(SubSectorStack.EntriesFree)
	or a
	ret z
	dec a
	ld (SubSectorStack.EntriesFree),a

; --------------------------------------------------------------------------
; Move the stack pointer.
; --------------------------------------------------------------------------
	
	ld hl,(SubSectorStack.Current)
	ld de,-SubSectorStack.EntrySize
	add hl,de
	ld (SubSectorStack.Current),hl

; --------------------------------------------------------------------------
; Store the subsector pointer.
; --------------------------------------------------------------------------

	push ix
	pop de
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl

; --------------------------------------------------------------------------
; Store the top edge clipping regions.
; --------------------------------------------------------------------------

	ex de,hl
	ld hl,TopEdgeClip
	ld bc,96
	ldir

; --------------------------------------------------------------------------
; Store the bottom edge clipping regions.
; --------------------------------------------------------------------------

	ld hl,BottomEdgeClip
	ld bc,96
	ldir
	
	ret

; ==========================================================================
; Draw
; --------------------------------------------------------------------------
; Draws all things.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Draw:
	ld a,(SubSectorStack.EntriesFree)
	ld b,a
	ld a,(SubSectorStack.MaximumCapacity)
	sub b
	ret z
	
	ld b,a
Draw.Loop:
	push bc
	
; --------------------------------------------------------------------------
; Fetch (and advance for the next loop) the stack pointer.
; --------------------------------------------------------------------------
	
	ld hl,(SubSectorStack.Current)
	push hl
	ld de,SubSectorStack.EntrySize
	add hl,de
	ld (SubSectorStack.Current),hl
	pop hl

; --------------------------------------------------------------------------
; Retrieve the pointer to the subsector to draw.
; --------------------------------------------------------------------------

	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (DrawingSubSector),de
	
; --------------------------------------------------------------------------
; Restore the upper clipping region.
; --------------------------------------------------------------------------

	ld de,TopEdgeClip
	ld bc,96
	ldir

; --------------------------------------------------------------------------
; Restore the lower clipping region.
; --------------------------------------------------------------------------

	ld de,BottomEdgeClip
	ld bc,96
	ldir

; --------------------------------------------------------------------------
; Draw the things in turn.
; --------------------------------------------------------------------------

	ld hl,(DrawingSubSector)
	ld de,3
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ex de,hl

; --------------------------------------------------------------------------
; Transform the position of the thing.
; --------------------------------------------------------------------------

	ld de,Transformed.X
	call Vertices.TransformSingle

; --------------------------------------------------------------------------
; Is it behind the camera?
; --------------------------------------------------------------------------

	ld a,(Transformed.Y+1)
	or a
	jp m,Draw.Skip

; --------------------------------------------------------------------------
; Is it outside Y=+X?
; --------------------------------------------------------------------------

	ld hl,(Transformed.X)
	ld de,(Transformed.Y)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jp nc,Draw.Skip

; --------------------------------------------------------------------------
; Is it outside Y=-X?
; --------------------------------------------------------------------------

	ld hl,(Transformed.X)
	ld de,(Transformed.Y)
	neg_de()
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	jp c,Draw.Skip

; --------------------------------------------------------------------------
; Project to X.
; --------------------------------------------------------------------------

	; 48 * X / Y
	ld de,(Transformed.X)
	ld bc,48
	call Maths.Mul.S16S16
	ld a,e
	ld b,h
	ld c,l
	ld de,(Transformed.Y)
	call Maths.Div.S24S16
	
	; Offset by the centre of the screen.
	ld a,c
	add a,48
	
	; Clip to the bounds of the screen.
	jp p,+
	xor a
+:	cp 96
	jr c,+
	ld a,95
+:

	ld (Projected.X),a
	
; --------------------------------------------------------------------------
; Project to Y.
; --------------------------------------------------------------------------
	
	ld hl,(DrawingSubSector)
	ld e,(hl)
	inc hl
	ld d,(hl)
	ex de,hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ld hl,(Render.Camera.Z)
	add hl,de

	ld de,(Transformed.Y)
	call Maths.Div.S16S16
	call Wall.Clip24To16
	ld hl,(Render.Camera.YShear)
	or a
	sbc hl,bc
	ld (Projected.Y.Bottom),hl
	call Wall.Clip16ToRowPlusOne
	inc a
	ld (Projected.Y.Bottom.Clipped),a

; --------------------------------------------------------------------------
; Calculate the height and therefore top.
; --------------------------------------------------------------------------

	ld hl,128
	call Maths.Div.S16S16
	call Wall.Clip24To16
	ld a,c
	srl a
	ld (Projected.Width),a
	or a
	jr z,Draw.Skip
	ld hl,(Projected.Y.Bottom)
	sbc hl,bc
	ld (Projected.Y.Top),hl
	call Wall.Clip16ToRowPlusOne
	inc a
	ld (Projected.Y.Top.Clipped),a

; --------------------------------------------------------------------------
; Clip and draw a column.
; --------------------------------------------------------------------------

	ld a,(Projected.X)
	ld l,a
	
	ld a,(Projected.Width)
	ld h,a
	
	srl a
	neg
	add a,l
	ld l,a

ColumnLoop:
	push hl
	
	ld a,l
	or a
	jp m,SkipColumn
	cp 96
	jr nc,SkipColumn

	ld a,(Projected.Y.Top.Clipped)
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

	ld a,(Projected.Y.Bottom.Clipped)
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
	
	sub c
	jr c,SkipColumn
	jr z,SkipColumn
	
	ld b,a
	push bc
	
	ld a,l
	ld e,c
	
	call Pixel.GetInformation

	pop bc
	
	ld c,a
	ld de,12

-:	ld a,c
	or (hl)
	ld (hl),a
	add hl,de
	djnz -

SkipColumn:
	pop hl
	inc l
	dec h
	jr nz,ColumnLoop

Draw.Skip:

	pop bc
	djnz +
	ret
+:	jp Draw.Loop


.endmodule