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

Projected.Height: .db 0

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
	ld (Projected.Height),a
	srl a
	ld (Projected.Width),a
	or a
	jp z,Draw.Skip
	ld hl,(Projected.Y.Bottom)
	sbc hl,bc
	ld (Projected.Y.Top),hl
	call Wall.Clip16ToRowPlusOne
	inc a
	ld (Projected.Y.Top.Clipped),a

	ld a,(Projected.Height)
	ld (Delta.DestinationHeight),a
	
	ld a,(Projected.Width)
	ld (Delta.DestinationWidth),a
	
	ld a,32
	ld (Delta.SourceHeight),a
	
	ld a,16
	ld (Delta.SourceWidth),a
	ld (MaximumWidth),a

	ld a,(Delta.DestinationWidth)
	srl a
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
	
	ld de,Sprite
	ld (SourceRowOffset),de

; --------------------------------------------------------------------------
; Draw a column.
; --------------------------------------------------------------------------
ColumnLoop:
	push hl
	
; --------------------------------------------------------------------------
; Is the column on the screen?
; --------------------------------------------------------------------------

	ld a,l
	or a
	jp m,SkipColumn
	cp 96
	jp nc,SkipColumn

; --------------------------------------------------------------------------
; Clip to the top edge.
; --------------------------------------------------------------------------

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

; --------------------------------------------------------------------------
; Clip to the bottom edge.
; --------------------------------------------------------------------------

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

; --------------------------------------------------------------------------
; Is the height <= 0?
; --------------------------------------------------------------------------

	sub c
	jr c,SkipColumn
	jr z,SkipColumn
	
; --------------------------------------------------------------------------
; Store the number of rows to render to the stack.
; --------------------------------------------------------------------------
	
	ld b,a
	push bc
	
	ld a,l
	ld e,c
	
; --------------------------------------------------------------------------
; Calculate where on the screen the first pixel should go.
; --------------------------------------------------------------------------
	
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

SourceRowOffset = $+1
	ld hl,0

; --------------------------------------------------------------------------
; Load the scale factor into DE.
; --------------------------------------------------------------------------

Delta.DestinationHeight = $+1
Delta.SourceHeight = $+2
	ld de,0

; --------------------------------------------------------------------------
; Is the top of the thing clipped?
; --------------------------------------------------------------------------

	ld a,c
	xor $80
	ld c,a
	ld a,(Projected.Y.Top)
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
	srl a
	
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
	
	ld a,c
	pop bc
	ld c,a
	
	jr RowLoop

TopNotClipped:

; --------------------------------------------------------------------------
; Initialise the error.
; --------------------------------------------------------------------------

	ld a,e
	srl a

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
	
	ld b,a
MaximumWidth = $+1
	ld a,0
	dec a
	jr nz,+
	pop hl
	jr Draw.Skip
+:	ld (MaximumWidth),a
	ld a,b

	ld hl,(SourceRowOffset)
	ld bc,8

-:	add hl,bc	
	add a,e
	jp m,-

	ld (SourceRowOffset),hl	

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


Sprite:
.db %00111111, %11111111, %11111100, %00000000, %00000000, %00000000, %00000000, %11110000
.db %11111110, %10101010, %10111111, %00000000, %00000000, %00000000, %00000011, %11111100
.db %11101111, %11111111, %11111011, %00000000, %00000000, %00000011, %00110011, %10111111
.db %11111111, %11111111, %11111111, %11111100, %00000000, %11111111, %11111111, %10111111
.db %11101110, %10101010, %10111011, %10101111, %11111111, %11101011, %10111011, %10101111
.db %11101110, %10101010, %10111011, %10101110, %10101010, %11101011, %10111011, %10101111
.db %11111110, %10101010, %10111111, %11111110, %10101010, %11111111, %11111111, %10101111
.db %11111110, %10101010, %10111111, %10101111, %11111111, %11101011, %10111011, %10101111
.db %11111110, %10101010, %10111111, %11111111, %11111111, %11111111, %11111111, %10111111
.db %11111110, %10101010, %10111111, %11111111, %11111111, %11111111, %11111111, %11111111
.db %11111110, %10101010, %10111111, %11111110, %10101010, %11111111, %11111111, %11111111
.db %11111110, %10101010, %10111111, %11111111, %11111111, %11111111, %11111111, %11111111
.db %11111111, %11111111, %11111111, %11111100, %00000000, %11111111, %11111111, %11111111
.db %11111111, %11111111, %11111111, %00000000, %00000000, %00000011, %00110011, %11111111
.db %11111110, %10101010, %10111111, %00000000, %00000000, %00000000, %00000011, %11111100
.db %00111111, %11111111, %11111100, %00000000, %00000000, %00000000, %00000000, %11110000
.endmodule