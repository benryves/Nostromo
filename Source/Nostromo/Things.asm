.module Things
Code:

; ==========================================================================
; SubSectorStack.Push
; --------------------------------------------------------------------------
; Pushes information about the subsector to draw to the stack.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the subsector containing the things to draw.
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
; We have not yet added anything to the sorted sprite buffer.
; --------------------------------------------------------------------------
	
	xor a
	ld (SortedSpriteBuffer.Count),a

; --------------------------------------------------------------------------
; Find the things to draw in turn.
; --------------------------------------------------------------------------

	ld hl,(DrawingSubSector)
	
	inc hl

	ld l,(hl)

; --------------------------------------------------------------------------
; We have the index of the thing to draw in L.
; --------------------------------------------------------------------------

SortNextThing:

	ld h,0
	
	add hl,hl
	add hl,hl
	add hl,hl
	
	ld de,(Level.Things)
	add hl,de

; --------------------------------------------------------------------------
; Fetch the pointer to the next thing.
; --------------------------------------------------------------------------

	ld a,(hl)
	ld (NextThing.Index),a
	inc hl

; --------------------------------------------------------------------------
; Look up the thing's type.
; --------------------------------------------------------------------------

	push hl
	ld l,(hl)
	ld h,0
	add hl,hl
	ld de,Thing.Types
	add hl,de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Appearance.Offset),de
	pop hl
	inc hl

; --------------------------------------------------------------------------
; Skip over the two reserved bytes.
; --------------------------------------------------------------------------

	inc hl
	inc hl

; --------------------------------------------------------------------------
; Get the coordinates of the thing.
; --------------------------------------------------------------------------

	ld c,(hl) \ inc hl
	ld b,(hl) \ inc hl
	ld e,(hl) \ inc hl
	ld d,(hl)

; --------------------------------------------------------------------------
; Transform it.
; --------------------------------------------------------------------------

	call Vertices.Transform

; --------------------------------------------------------------------------
; Is it behind the camera?
; --------------------------------------------------------------------------

	ld a,d
	or a
	jp m,Buffer.Skip

; --------------------------------------------------------------------------
; Store the transformed position.
; --------------------------------------------------------------------------

	ld (Transformed.X),bc
	ld (Transformed.Y),de

; --------------------------------------------------------------------------
; Fudge the distance factor to allow the centre of the sprite object to
; appear beyond the left or right edge of the display rather than vanish
; when there's still half of it to display.
; --------------------------------------------------------------------------

	inc d

; --------------------------------------------------------------------------
; Is it outside Y=+X?
; --------------------------------------------------------------------------


	ld l,c
	ld a,b \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jp nc,Buffer.Skip

; --------------------------------------------------------------------------
; Is it outside Y=-X?
; --------------------------------------------------------------------------

	add hl,de
	neg_de()
	or a
	sbc hl,de
	jp c,Buffer.Skip

; --------------------------------------------------------------------------
; Now we have the thing to draw, we need to add it to our sorting buffer.
; --------------------------------------------------------------------------

	ld de,SortedSpriteBuffer
	ld a,(SortedSpriteBuffer.Count)
	or a
	jr z,SortedSpriteBuffer.Add

; --------------------------------------------------------------------------
; We need to find somewhere to add the sprite.
; --------------------------------------------------------------------------

	ld b,a
FindInsertionPoint:
	
	ex de,hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	dec hl
	ex de,hl
	push bc
	ld bc,(Transformed.Y)
	or a
	sbc hl,bc
	pop bc
	jr nc,IsCloser

; --------------------------------------------------------------------------
; The sprite is further from the camera than the one at (DE).
; --------------------------------------------------------------------------
IsFurther:
	
	push de
	
	ld l,b
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld b,h \ ld c,l
	
	; BC = number of bytes to move.	
	
	ld a,(SortedSpriteBuffer.Count)
	ld l,a
	ld h,0
	add hl,hl
	add hl,hl
	add hl,hl
	ld de,SortedSpriteBuffer-1
	add hl,de
	
	; HL->Current end of buffer.
	
	push hl
	ld de,8
	add hl,de
	ex de,hl
	pop hl
	
	; DE->End of buffer + 8
	
	lddr
	
	pop de
	jr SortedSpriteBuffer.Add	

IsCloser:
	ld hl,8
	add hl,de
	ex de,hl
	djnz FindInsertionPoint

; --------------------------------------------------------------------------
; Add the sorted sprite to the buffer.
; --------------------------------------------------------------------------
SortedSpriteBuffer.Add:

	push bc

	ld hl,Transformed.Y
	ldi \ ldi ; Y
	ldi \ ldi ; X
	ld hl,Appearance.Offset
	ldi \ ldi
	ldi \ ldi ; (Dummy)
	
	pop bc

; --------------------------------------------------------------------------
; We have one more item in the sprite buffer.
; --------------------------------------------------------------------------

	ld a,(SortedSpriteBuffer.Count)
	inc a
	ld (SortedSpriteBuffer.Count),a

; --------------------------------------------------------------------------
; Do we have any more things to sort?
; --------------------------------------------------------------------------
Buffer.Skip:
	ld a,(NextThing.Index)
	or a
	jr z,+
	ld l,a
	jp SortNextThing
+:

; --------------------------------------------------------------------------
; We now have a buffer full of sorted sprites. Draw them!
; --------------------------------------------------------------------------

	ld a,(SortedSpriteBuffer.Count)
	or a
	jp z,AdvanceToNextSubsector
	
	ld b,a
	ld hl,SortedSpriteBuffer

DrawSortedSprite.Loop:

	push bc
	
	ld de,Transformed.Y
	ldi \ ldi ; Y
	ldi \ ldi ; X
	ld de,Appearance.Offset
	ldi \ ldi
	inc hl \ inc hl
	
	push hl


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
	ld hl,(Transformed.X)
	call Maths.Mul.S48
	ld b,h \ ld c,l
	ld de,(Transformed.Y)
	call Maths.Div.S24S16
	
	; Offset by the centre of the screen.
	ld a,c
	add a,48

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

	pop hl
	pop bc
	djnz +
	jr AdvanceToNextSubsector
+:	jp DrawSortedSprite.Loop

AdvanceToNextSubsector:

	pop bc
	djnz +
	ret
+:	jp Draw.Loop

.if Options.ReportModuleSizes \ .echoln strformat("Things module: {0:N0} bytes.", $-Code) \ .endif
.endmodule