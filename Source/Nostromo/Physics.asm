.module Physics

; ==========================================================================
; MoveActor
; --------------------------------------------------------------------------
; Moves an actor through the world, clipping and sliding against walls.
; --------------------------------------------------------------------------
; Inputs:    Actor.StartPosition - Start position of the actor.
;            Actor.StartPosition.Z - Z coordinate of the actor.
;            Actor.EndPosition - Desired end position of the actor.
; Outputs:   Actor.EndPosition - End position of the actor.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
MoveActor:

; --------------------------------------------------------------------------
; Look up the block in the block map.
; --------------------------------------------------------------------------

	ld hl,(Actor.EndPosition.X)
	ld de,(Actor.EndPosition.Y)
	
	call BlockMap.GetBlockFromPoint
	
; --------------------------------------------------------------------------
; Are we outside the map?
; --------------------------------------------------------------------------

	ret c

; --------------------------------------------------------------------------
; Are there any walls in this block?
; --------------------------------------------------------------------------

	ld a,(de)
	or a
	ret z

; --------------------------------------------------------------------------
; Treat each wall in turn.
; --------------------------------------------------------------------------

	ld b,a

-:	inc de
	push bc
	push de

; --------------------------------------------------------------------------
; Get the wall index.
; --------------------------------------------------------------------------
	
	ld a,(de)
	ld l,a
	ld h,0

; --------------------------------------------------------------------------
; Calculate the address of the wall from its index.
; --------------------------------------------------------------------------

	.if Wall.DataSize != 8
		.echoln "Walls are no longer 8 bytes (fix this)"
	.endif

	add hl,hl
	add hl,hl
	add hl,hl
	ex de,hl

	ld ix,(Level.Walls)
	add ix,de
	
; --------------------------------------------------------------------------
; Clip movement against the wall.
; --------------------------------------------------------------------------
	
	call ClipAgainstWall

; --------------------------------------------------------------------------
; Advance to the next wall.
; --------------------------------------------------------------------------
	
	pop de
	pop bc
	djnz -
	ret

; ==========================================================================
; ClipAgainstWall
; --------------------------------------------------------------------------
; Clips an actor against a single wall.
; --------------------------------------------------------------------------
; Inputs:    IX - Pointer to the wall to clip against.
;            Actor.StartPosition - Start position of the actor.
;            Actor.StartPosition.Z - Z coordinate of the actor.
;            Actor.EndPosition - Desired end position of the actor.
; Outputs:   Actor.EndPosition - End position of the actor.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
ClipAgainstWall:

	bit Wall.DrawFlag.FillMiddle,(ix+Wall.Data.Flags)
	jr nz,ClipAgainstWall.Clip

; --------------------------------------------------------------------------
; The wall is an "upper/lower" wall. Check if we can fit through the hole.
; --------------------------------------------------------------------------


	set Wall.DrawFlag.DrawnThisFrame,(ix+Wall.Data.Flags)

	.if Sector.DataSize != 4
		.echoln "Sectors are no longer 4 bytes (fix this)"
	.endif

	ld l,(ix+Wall.Data.FrontSector)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,(Level.Sectors)
	add hl,de
	call CheckUpperLowerSectorHeights
	jr c,ClipAgainstWall.Clip
	
	res Wall.DrawFlag.DrawnThisFrame,(ix+Wall.Data.Flags)
	
	ld l,(ix+Wall.Data.BackSector)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,(Level.Sectors)
	add hl,de
	call CheckUpperLowerSectorHeights
	ret nc

ClipAgainstWall.Clip:

; --------------------------------------------------------------------------
; Load the start vertex.
; --------------------------------------------------------------------------
	
	ld l,(ix+Wall.Data.StartVertex)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,(Level.Vertices)
	add hl,de
	ld de,Wall.Start.X
	ldi \ ldi \ ldi \ ldi

; --------------------------------------------------------------------------
; Load the end vertex.
; --------------------------------------------------------------------------

	ld l,(ix+Wall.Data.EndVertex)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,(Level.Vertices)
	add hl,de
	ld de,Wall.End.X
	ldi \ ldi \ ldi \ ldi

; --------------------------------------------------------------------------
; Calculate the wall deltas.
; --------------------------------------------------------------------------

	ld hl,(Wall.End.X)
	ld de,(Wall.Start.X)
	or a
	sbc hl,de
	ld (Wall.Delta.X),hl

	ld hl,(Wall.End.Y)
	ld de,(Wall.Start.Y)
	or a
	sbc hl,de
	ld (Wall.Delta.Y),hl

; --------------------------------------------------------------------------
; u = (
;        ((Actor.EndPosition.X - Wall.Start.X) * Wall.Delta.X) / 256 +
;        ((Actor.EndPosition.Y - Wall.Start.Y) * Wall.Delta.Y) / 256
;     ) / (Wall.Length² / 65536)
; --------------------------------------------------------------------------

; --------------------------------------------------------------------------
; ((Actor.EndPosition.X - Wall.Start.X) * Wall.Delta.X) / 256
; --------------------------------------------------------------------------

	ld hl,(Actor.EndPosition.X)
	ld de,(Wall.Start.X)
	or a
	sbc hl,de
	ex de,hl
	
	ld bc,(Wall.Delta.X)

	call Maths.Mul.S16S16
	
	ld l,h
	ld h,e
	ld e,d
	
	push de
	push hl

; --------------------------------------------------------------------------
; ((Actor.EndPosition.Y - Wall.Start.Y) * Wall.Delta.Y) / 256
; --------------------------------------------------------------------------
	
	ld hl,(Actor.EndPosition.Y)
	ld de,(Wall.Start.Y)
	or a
	sbc hl,de
	ex de,hl
	
	ld bc,(Wall.Delta.Y)

	call Maths.Mul.S16S16
	
	ld l,h
	ld h,e
	ld e,d

; --------------------------------------------------------------------------
; Add the two together.
; --------------------------------------------------------------------------

	pop bc	
	add hl,bc
	
	pop bc
	
	ld a,c
	adc a,e
	
	ld b,h
	ld c,l

; --------------------------------------------------------------------------
; Divide by the wall length squared.
; --------------------------------------------------------------------------
	
	ld e,(ix+Wall.Data.LengthSquared+0)
	ld d,(ix+Wall.Data.LengthSquared+1)
	
	call Maths.Div.S24S16
	
; --------------------------------------------------------------------------
; Are we in line with the wall segment?
; --------------------------------------------------------------------------

	or a
	ret nz
	
	ld a,b
	or a
	ret nz

; --------------------------------------------------------------------------
; We are in line with the line segment, unfortunately.
; --------------------------------------------------------------------------

; --------------------------------------------------------------------------
; Intersection = Start + (Delta * u) / 256
; --------------------------------------------------------------------------

	ld (Collision.U),bc
	
	ld de,(Wall.Delta.X)
	
	call Maths.Mul.S16S16
	
	ld d,e
	ld e,h
	
	ld hl,(Wall.Start.X)
	add hl,de
	ld (Collision.X),hl
	
	ld bc,(Collision.U)
	
	ld de,(Wall.Delta.Y)
	
	call Maths.Mul.S16S16
	
	ld d,e
	ld e,h
	
	ld hl,(Wall.Start.Y)
	add hl,de
	ld (Collision.Y),hl

; --------------------------------------------------------------------------
; How far away is the collision?
; --------------------------------------------------------------------------

	ld de,(Actor.EndPosition.Y)
	or a
	sbc hl,de
	
	ex de,hl
	ld b,d
	ld c,e
	
	call Maths.Mul.S16S16

	ld l,h
	ld h,e
	ld e,d

	push de
	push hl

	ld hl,(Collision.X)
	ld de,(Actor.EndPosition.X)
	or a
	sbc hl,de
	
	ex de,hl
	ld b,d
	ld c,e
	
	call Maths.Mul.S16S16

	ld l,h
	ld h,e
	ld e,d

; --------------------------------------------------------------------------
; Add the two together.
; --------------------------------------------------------------------------

	pop bc	
	add hl,bc
	
	pop bc
	
	ld a,c
	adc a,e
	
; --------------------------------------------------------------------------
; Is it within the radius of the player?
; --------------------------------------------------------------------------
	
	ret nz
	
	ld a,h
	or a
	ret nz
	
; --------------------------------------------------------------------------
; We have collided with the wall!
; --------------------------------------------------------------------------

; --------------------------------------------------------------------------
; Did we bump into the front or back of the wall?
; --------------------------------------------------------------------------

	bit Wall.DrawFlag.FillMiddle,(ix+Wall.Data.Flags)
	jr nz,CollidedWithFront

	bit Wall.DrawFlag.DrawnThisFrame,(ix+Wall.Data.Flags)
	jr z,CollidedWithFront

CollidedWithBack:

	ld a,(ix+Wall.Data.Angle)
	xor $80
	
	jr PushBackFromWall

CollidedWithFront:

	ld a,(ix+Wall.Data.Angle)

PushBackFromWall:
	
	push af
	call Maths.Trig.Sin
	ld hl,(Collision.X)
	add hl,bc
	ld (Actor.EndPosition.X),hl

	pop af
	call Maths.Trig.Cos
	ld hl,(Collision.Y)
	add hl,bc
	ld (Actor.EndPosition.Y),hl

	ret

; ==========================================================================
; CheckUpperLowerSectorHeights
; --------------------------------------------------------------------------
; Check if the actor is above the sector ceiling or below the sector floor.
; --------------------------------------------------------------------------
; Inputs:    Actor.StartPosition.Z - Z coordinate of the actor.
;            HL - Pointer to sector to check.
; Outputs:   Carry flag set if we are out of bounds of the sector.
; Destroyed: AF, DE, HL.
; ==========================================================================
CheckUpperLowerSectorHeights:

; --------------------------------------------------------------------------
; Are we below the floor?
; --------------------------------------------------------------------------
	
	ld e,(hl)
	inc hl
	ld a,(hl)
	xor $80
	ld d,a
	inc hl
	
	push hl
	
	ld hl,(Actor.Z.Knees)
	ld a,h \ xor $80 \ ld h,a
	or a
	sbc hl,de
	
	pop hl
	
	ret c

; --------------------------------------------------------------------------
; Are we above the ceiling?
; --------------------------------------------------------------------------
	
	ld e,(hl)
	inc hl
	ld a,(hl)
	xor $80
	ld d,a
	
	ld hl,(Actor.Z.Head)
	ld a,h \ xor $80 \ ld h,a
	or a
	sbc hl,de
	
	ccf
	ret

.endmodule