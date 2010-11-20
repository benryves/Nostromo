.module Things

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
	ld de,2
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ex de,hl

; --------------------------------------------------------------------------
; Transform the position of the thing.
; --------------------------------------------------------------------------

	ld c,(hl) \ inc hl
	ld b,(hl) \ inc hl
	ld e,(hl) \ inc hl
	ld d,(hl) \ inc hl
	push de
	ld e,(hl) \ inc hl
	ld d,(hl)
	ld (Appearance.Offset),de
	pop de
	call Vertices.Transform

; --------------------------------------------------------------------------
; Is it behind the camera?
; --------------------------------------------------------------------------

	ld a,d
	or a
	jp m,Draw.Skip

; --------------------------------------------------------------------------
; Store the transormed position.
; --------------------------------------------------------------------------

	ld (Transformed.X),bc
	ld (Transformed.Y),de

; --------------------------------------------------------------------------
; Is it outside Y=+X?
; --------------------------------------------------------------------------


	ld l,c
	ld a,b \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jp nc,Draw.Skip

; --------------------------------------------------------------------------
; Is it outside Y=-X?
; --------------------------------------------------------------------------

	add hl,de
	neg_de()
	or a
	sbc hl,de
	jp c,Draw.Skip

; --------------------------------------------------------------------------
; Get the sprite appearance information.
; --------------------------------------------------------------------------

	ld hl,(Appearance.Offset)
	ld de,Appearance
	ld bc,Appearance.Size
	ldir
	ld (Sprite.Data),hl

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
	ld l,(hl)
	ld h,0
	
	.if Sector.DataSize != 4
		.echoln "Sectors are no longer 4 bytes (fix this)"
	.endif

	
	add hl,hl
	add hl,hl
	ld de,(Level.Sectors)
	add hl,de
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
	ld (Sprite.Column.Bottom),hl
	call Wall.Clip16ToRowPlusOne
	inc a
	ld (Sprite.Column.Bottom.Clipped),a


; --------------------------------------------------------------------------
; Calculate the height and therefore top.
; --------------------------------------------------------------------------

	ld hl,(Appearance.WorldHeight)
	call Maths.Div.S16S16
	call Wall.Clip24To16
	ld a,b
	or a
	jp nz,Draw.Skip
	ld a,c
	or a
	jp m,Draw.Skip
	ld (Projected.Height),a
	
	or a
	jp z,Draw.Skip
	ld hl,(Sprite.Column.Bottom)
	sbc hl,bc
	ld (Sprite.Column.Top),hl
	call Wall.Clip16ToRowPlusOne
	inc a
	ld (Sprite.Column.Top.Clipped),a

; --------------------------------------------------------------------------
; Calculate the width.
; --------------------------------------------------------------------------

	ld hl,(Appearance.WorldWidth)
	ld de,(Transformed.Y)
	call Maths.Div.S16S16
	call Wall.Clip24To16
	ld a,b
	or a
	jp nz,Draw.Skip
	ld a,c
	or a
	jp m,Draw.Skip
	ld (Projected.Width),a
	
; --------------------------------------------------------------------------
; Copy the width and height values over.
; --------------------------------------------------------------------------

	ld a,(Projected.Height)
	ld (Sprite.Column.DestinationHeight),a

	ld a,(Appearance.SpriteHeight)
	ld (Sprite.Column.SourceHeight),a
	
	ld a,(Projected.Width)
	ld (Delta.DestinationWidth),a	
	
	ld a,(Appearance.SpriteWidth)
	ld (Delta.SourceWidth),a

	ld a,(Delta.DestinationWidth)
	ld (ColumnError),a

; --------------------------------------------------------------------------
; Draw the thing.
; --------------------------------------------------------------------------

	ld a,(Projected.X)
	ld l,a
	
	ld a,(Projected.Width)
	ld h,a
	
	srl a
	neg
	add a,l
	ld l,a

; --------------------------------------------------------------------------
; Initialise the per-row source offset.
; --------------------------------------------------------------------------
	
	ld de,(Sprite.Data)
	ld (Sprite.Column.SourceData),de

; --------------------------------------------------------------------------
; Draw a column.
; --------------------------------------------------------------------------
ColumnLoop:
	push hl
	
	
	call Sprite.DrawColumn
	
SkipColumn:

; --------------------------------------------------------------------------
; Advance to the next column if required.
; --------------------------------------------------------------------------
ColumnError = $+1
	ld a,0

Delta.DestinationWidth = $+1
Delta.SourceWidth = $+2
	ld de,0
	sub d
	jp p,NoAdvanceColumn
	
	ld hl,(Sprite.Column.SourceData)
	ld bc,(Appearance.ColumnStride)

-:	add hl,bc	
	add a,e
	jp m,-

	ld (Sprite.Column.SourceData),hl	

NoAdvanceColumn:

	ld (ColumnError),a
	

	pop hl
	inc l
	dec h
	jp nz,ColumnLoop

Draw.Skip:

	pop bc
	djnz +
	ret
+:	jp Draw.Loop
.endmodule