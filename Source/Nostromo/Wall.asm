.module Wall

; ==========================================================================
; ClipFlags
; --------------------------------------------------------------------------
; Provide flags describing the clipping operation.
; ==========================================================================
ClipFlags = asm_Flag1
ClipFlag.StartOutsideLeft = 0
ClipFlag.StartOutsideRight = 1
ClipFlag.EndOutsideLeft = 2
ClipFlag.EndOutsideRight = 3
ClipFlag.Steep = 4


Start.X: .dw 0
Start.Y: .dw 0

End.X: .dw 0
End.Y: .dw 0

Delta.X: .dw 0
Delta.AbsX: .dw 0

Delta.Y: .dw 0
Delta.AbsY: .dw 0

Gradient: .dw 0

DrawFlags: .db 0

; ==========================================================================
; ClipAndDraw
; --------------------------------------------------------------------------
; Clips the wall to the view and draws it on the screen.
; ==========================================================================
ClipAndDraw:
; --------------------------------------------------------------------------
; Is the wall entirely behind the camera?
; --------------------------------------------------------------------------

	ld a,(Start.Y+1)
	ld b,a
	ld a,(End.Y+1)
	and b
	jp m,SkipWall

; --------------------------------------------------------------------------
; Calculate the wall delta values.
; --------------------------------------------------------------------------

	ld hl,(End.X)
	ld de,(Start.X)
	or a
	sbc hl,de
	ld (Delta.X),hl
	bit 7,h
	jr nz,+
	neg_hl()
+:	ld (Delta.AbsX),hl
	
	ld hl,(End.Y)
	ld de,(Start.Y)
	or a
	sbc hl,de
	ld (Delta.Y),hl
	bit 7,h
	jr nz,+
	neg_hl()
+:	ld (Delta.AbsY),hl

; --------------------------------------------------------------------------
; Clear the GetYIntercept and GetXIntercept cache.
; --------------------------------------------------------------------------

	ld hl,$0118 ; JR $+1
	ld (GetYIntercept+0),hl
	ld (GetXIntercept+0),hl

; --------------------------------------------------------------------------
; Clear the clipping flags.
; --------------------------------------------------------------------------

	ld (iy+ClipFlags),0

; --------------------------------------------------------------------------
; Clip to Y=0.
; --------------------------------------------------------------------------

	; Does the start intersect the Y axis?

	; Clip the start of the wall to Y=0.
	ld a,(Start.Y+1)
	bit 7,a
	jr z,Start.DoesNotIntersectY

	ld de,(Delta.X)
	ld a,d
	or e
	jr z,+

	call GetYIntercept	
	ld (Start.X),hl

+:	ld hl,0
	ld (Start.Y),hl

	; We know that the end can't intersect Y,
	; as we would have already culled the 
	; (At least one end must have Y>=0).
	jr End.DoesNotIntersectY

Start.DoesNotIntersectY:

	; Clip the end of the wall to Y=0.
	ld a,(End.Y+1)
	bit 7,a
	jr z,End.DoesNotIntersectY

	ld de,(Delta.X)
	ld a,d
	or e
	jr z,+

	call GetYIntercept

	ld (End.X),hl

+:	ld hl,0
	ld (End.Y),hl

End.DoesNotIntersectY:

ClippedToY:

; --------------------------------------------------------------------------
; Does the wall need to be clipped to Y=+X or Y=-X?
; --------------------------------------------------------------------------

	; Count the number of times Y=+X (B) and Y=-X (C) are clipped against.
	ld bc,$0202

	; Check the start against Y=+X.

	ld hl,(Start.X)
	ld de,(Start.Y)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr c,+
	dec b
	set ClipFlag.StartOutsideRight,(iy+ClipFlags)
+:

	; Check the start against Y=-X.

	ld hl,(Start.X)
	ld de,(Start.Y)
	neg_de()
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr nc,+
	dec c
	set ClipFlag.StartOutsideLeft,(iy+ClipFlags)
+:

	; Check the end against Y=+X.

	ld hl,(End.X)
	ld de,(End.Y)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr c,+
	dec b
	jp z,SkipWall ; Both ends are outside the right - bail out.
	set ClipFlag.EndOutsideRight,(iy+ClipFlags)
+:

	; Check the end against Y=-X.

	ld hl,(End.X)
	ld de,(End.Y)
	neg_de()
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr nc,+
	dec c
	jp z,SkipWall ; Both ends are outside the left - bail out.
	set ClipFlag.EndOutsideLeft,(iy+ClipFlags)
+:

; --------------------------------------------------------------------------
; Do we need to do any clipping?
; --------------------------------------------------------------------------
	
	ld a,(iy+ClipFlags)
	and $0F
	jp z,NoViewClippingRequired

; --------------------------------------------------------------------------
; Some clipping is, alas, required.
; --------------------------------------------------------------------------
	
	; Is the wall steep or shallow?
	; A "steep" wall is one in which |dY| > |dX|.
	
	ld hl,(Delta.AbsX)
	ld de,(Delta.AbsY)
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	ld hl,(Delta.Y)
	ld de,(Delta.X)
	
	jr c,IsShallow

IsSteep:
	set ClipFlag.Steep,(iy+ClipFlags)
	ex de,hl

IsShallow:

; --------------------------------------------------------------------------
; Calculate the gradient of the line.
; --------------------------------------------------------------------------

	call Nostromo.Maths.Div.S16S16
	ld (Gradient),bc

; --------------------------------------------------------------------------
; Clip the start to Y=+X.
; --------------------------------------------------------------------------

	bit ClipFlag.StartOutsideRight,(iy+ClipFlags)
	jr z,ClippedStartRight
	
	; If dY == 0, Start.X = Start.Y.
	ld hl,(Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(Start.Y)
		ld (Start.X),hl
		jp ClippedStartRight
	+:
	
	; If dX == 0, Start.Y = Start.X.
	ld hl,(Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(Start.X)
		ld (Start.Y),hl
		jp ClippedStartRight
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr z,ClipStartRight.Shallow

ClipStartRight.Steep:

	call GetYIntercept
	jr ClipStartRight.Clip
	
ClipStartRight.Shallow:

	call GetXIntercept

ClipStartRight.Clip:

	; X = -c * 256 / m - 256
	ld de,(Gradient)
	dec d
	call Nostromo.Maths.Div.S16S16
	neg_bc()
	
	ld (Start.X),bc
	ld (Start.Y),bc

ClippedStartRight:

; --------------------------------------------------------------------------
; Clip the end to Y=+X.
; --------------------------------------------------------------------------

	bit ClipFlag.EndOutsideRight,(iy+ClipFlags)
	jr z,ClippedEndRight
	
	; If dY == 0, End.X = End.Y.
	ld hl,(Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(End.Y)
		ld (End.X),hl
		jp ClippedEndRight
	+:
	
	; If dX == 0, End.Y = End.X.
	ld hl,(Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(End.X)
		ld (End.Y),hl
		jp ClippedEndRight
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr z,ClipEndRight.Shallow

ClipEndRight.Steep:

	call GetYIntercept
	jr ClipEndRight.Clip
	
ClipEndRight.Shallow:

	call GetXIntercept

ClipEndRight.Clip:

	; X = -c * 256 / m - 256
	ld de,(Gradient)
	dec d
	call Nostromo.Maths.Div.S16S16
	neg_bc()
	
	ld (End.X),bc
	ld (End.Y),bc

ClippedEndRight:

; --------------------------------------------------------------------------
; Clip the start to Y=-X.
; --------------------------------------------------------------------------

	bit ClipFlag.StartOutsideLeft,(iy+ClipFlags)
	jr z,ClippedStartLeft
	
	; If dY == 0, Start.X = -Start.Y.
	ld hl,(Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(Start.Y)
		neg_hl()
		ld (Start.X),hl
		jp ClippedStartLeft
	+:
	
	; If dX == 0, Start.Y = -Start.X.
	ld hl,(Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(Start.X)
		neg_hl()
		ld (Start.Y),hl
		jp ClippedStartLeft
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr z,ClipStartLeft.Shallow

ClipStartLeft.Steep:

	call GetYIntercept
	jr ClipStartLeft.Clip
	
ClipStartLeft.Shallow:

	call GetXIntercept

ClipStartLeft.Clip:

	; X = c * 256 / m + 256
	ld de,(Gradient)
	inc d
	call Nostromo.Maths.Div.S16S16
	
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr nz,+
	ld (Start.Y),bc
	neg_bc()
	ld (Start.X),bc
	jr ClippedStartLeft
+:
	ld (Start.X),bc
	neg_bc()
	ld (Start.Y),bc

ClippedStartLeft:

; --------------------------------------------------------------------------
; Clip the end to Y=-X.
; --------------------------------------------------------------------------

	bit ClipFlag.EndOutsideLeft,(iy+ClipFlags)
	jr z,ClippedEndLeft
	
	; If dY == 0, End.X = -End.Y.
	ld hl,(Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(End.Y)
		neg_hl()
		ld (End.X),hl
		jp ClippedEndLeft
	+:
	
	; If dX == 0, End.Y = -End.X.
	ld hl,(Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(End.X)
		neg_hl()
		ld (End.Y),hl
		jp ClippedEndLeft
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr z,ClipEndLeft.Shallow

ClipEndLeft.Steep:

	call GetYIntercept
	jr ClipEndLeft.Clip
	
ClipEndLeft.Shallow:

	call GetXIntercept

ClipEndLeft.Clip:

	; X = c * 256 / m + 256
	ld de,(Gradient)
	inc d
	call Nostromo.Maths.Div.S16S16
	
	bit ClipFlag.Steep,(iy+ClipFlags)
	jr nz,+
	ld (End.Y),bc
	neg_bc()
	ld (End.X),bc
	jr ClippedEndLeft
+:
	ld (End.X),bc
	neg_bc()
	ld (End.Y),bc

ClippedEndLeft:

NoViewClippingRequired:

; --------------------------------------------------------------------------
; The wall is now clipped to the view.
; --------------------------------------------------------------------------


; --------------------------------------------------------------------------
; If the wall starts on Y=0, skip it.
; --------------------------------------------------------------------------

	ld hl,(Start.Y)
	ld a,h
	or l
	jp z,SkipWall

; --------------------------------------------------------------------------
; If the wall ends on Y=0, skip it.
; --------------------------------------------------------------------------

	ld hl,(End.Y)
	ld a,h
	or l
	jp z,SkipWall

; --------------------------------------------------------------------------
; Project the end X of the wall.
; --------------------------------------------------------------------------

	; If we clipped to the left, project to the left.
	xor a
	bit ClipFlag.EndOutsideLeft,(iy+ClipFlags)
	jr nz,Project.End.X
	
	; If we clipped to the right , project to the right.
	ld a,95
	bit ClipFlag.EndOutsideRight,(iy+ClipFlags)
	jr nz,Project.End.X

	; 48 * X / Y
	ld de,(End.X)
	ld bc,48
	call Maths.Mul.S16S16
	ld a,e
	ld b,h
	ld c,l
	ld de,(End.Y)
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

Project.End.X:
	ld (Trapezium.Start.Column),a

; --------------------------------------------------------------------------
; Project the start X of the wall.
; --------------------------------------------------------------------------

	; If we clipped to the left, project to the left.
	xor a
	bit ClipFlag.StartOutsideLeft,(iy+ClipFlags)
	jr nz,Project.Start.X
	
	; If we clipped to the right , project to the right.
	ld a,95
	bit ClipFlag.StartOutsideRight,(iy+ClipFlags)
	jr nz,Project.Start.X

	; 48 * X / Y
	ld de,(Start.X)
	ld bc,48
	call Maths.Mul.S16S16
	ld a,e
	ld b,h
	ld c,l
	ld de,(Start.Y)
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

Project.Start.X:
	ld (Trapezium.End.Column),a

; --------------------------------------------------------------------------
; Are we looking at the back of the wall?
; --------------------------------------------------------------------------

	ld b,a
	ld a,(Trapezium.Start.Column)
	cp b
	jp c,SkipWall

; --------------------------------------------------------------------------
; Fetch the ceiling and floor height for the front sector.
; --------------------------------------------------------------------------

	ld a,(Wall.DrawFlags)
	bit 1,a
	jr nz,Wall.DrawMiddle

Wall.DrawUpperAndLower:
	
	ld hl,(Sector.Front)
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.FloorHeight),de
	ld hl,(Sector.Back)
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.CeilingHeight),de

	call DrawWallPart

	ld hl,(Sector.Front)
	inc hl
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.CeilingHeight),de
	ld hl,(Sector.Back)
	inc hl
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.FloorHeight),de

	call DrawWallPart

	jr Wall.Drawn

Wall.DrawMiddle:
	ld hl,(Sector.Front)
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (WallPart.FloorHeight),de
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.CeilingHeight),de
	
	call DrawWallPart

Wall.Drawn



SkipWall:

	ret

; ==========================================================================
; DrawWallPart
; --------------------------------------------------------------------------
; Draws a wall "part".
; Multiple parts have differing floor and ceiling heights.
; ==========================================================================
DrawWallPart:
; --------------------------------------------------------------------------
; Calculate the height of the start of the wall's floor.
; --------------------------------------------------------------------------

WallPart.FloorHeight = $+1
	ld hl,-64
	ld de,(Camera.Z)
	or a
	sbc hl,de
	ld (WallPart.FloorHeight),hl
	ld de,(Start.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,32
	or a
	sbc hl,bc
	ld (Trapezium.End.Floor),hl

; --------------------------------------------------------------------------
; Calculate the height of the end of the wall's floor.
; --------------------------------------------------------------------------

	ld hl,(WallPart.FloorHeight)
	ld de,(End.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,32
	or a
	sbc hl,bc
	ld (Trapezium.Start.Floor),hl

; --------------------------------------------------------------------------
; Calculate the height of the start of the wall's ceiling.
; --------------------------------------------------------------------------

WallPart.CeilingHeight = $+1
	ld hl,128
	ld de,(Camera.Z)
	or a
	sbc hl,de
	ld (WallPart.CeilingHeight),hl
	ld de,(Start.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,32
	or a
	sbc hl,bc
	ld (Trapezium.End.Ceiling),hl

; --------------------------------------------------------------------------
; Calculate the height of the end of the wall's ceiling.
; --------------------------------------------------------------------------

	ld hl,(WallPart.CeilingHeight)
	ld de,(End.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,32
	or a
	sbc hl,bc
	ld (Trapezium.Start.Ceiling),hl

; --------------------------------------------------------------------------
; Draw the bottom edge of the wall.
; --------------------------------------------------------------------------
	
	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.End.Floor)
	ld (Clip.g_line16Y1),hl
	
	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.Start.Floor)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	
	push bc
	pop hl

	call nc,lineDraw

; --------------------------------------------------------------------------
; Draw the top edge of the wall.
; --------------------------------------------------------------------------
	
	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.End.Ceiling)
	ld (Clip.g_line16Y1),hl
	
	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.Start.Ceiling)
	ld (Clip.g_line16Y2),hl	
	
	call Clip.Clip2DLine16Ex
	
	push bc
	pop hl
	call nc,lineDraw

; --------------------------------------------------------------------------
; Draw the lines between the floor and ceiling at the start.
; --------------------------------------------------------------------------

	bit ClipFlag.StartOutsideLeft,(iy+ClipFlags)
	jr nz,+
	bit ClipFlag.StartOutsideRight,(iy+ClipFlags)
	jr nz,+

	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.End.Floor)
	ld (Clip.g_line16Y1),hl

	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.End.Ceiling)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	
	push bc
	pop hl
	call nc,lineDraw
+:

; --------------------------------------------------------------------------
; Draw the lines between the floor and ceiling at the end.
; --------------------------------------------------------------------------

	bit ClipFlag.EndOutsideLeft,(iy+ClipFlags)
	jr nz,+
	bit ClipFlag.EndOutsideRight,(iy+ClipFlags)
	jr nz,+

	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.Start.Floor)
	ld (Clip.g_line16Y1),hl

	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.Start.Ceiling)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	
	push bc
	pop hl
	call nc,lineDraw
+:
	
	ret

; ==========================================================================
; ClearColumn
; --------------------------------------------------------------------------
; Clears a 8 pixels wide to white.
; --------------------------------------------------------------------------
; Inputs:    L: The column to clear.
; Destroyed: AF, B, DE, HL.
; ==========================================================================
ClearColumn:
	ld h,0
	ld de,plotSScreen
	add hl,de
	ld b,64
	ld de,12
	xor a
-:	ld (hl),a
	add hl,de
	djnz -
	ret

; ==========================================================================
; AndColumn
; --------------------------------------------------------------------------
; ANDs a value against a column 8 pixels wide.
; --------------------------------------------------------------------------
; Inputs:    L: The column to AND. C: the value to AND.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
AndColumn:
	ld h,0
	ld de,plotSScreen
	add hl,de
	ld b,64
	ld de,12
-:	ld a,(hl)
	and c
	ld (hl),a
	add hl,de
	djnz -
	ret
	

; ==========================================================================
; Clip24To16
; --------------------------------------------------------------------------
; Clips a 24-bit number to a 16-bit one.
; --------------------------------------------------------------------------
; Inputs:    ABC: The value to clip.
; Outputs:   BC: The clipped value (between -32768 and +32767).
; Destroyed: AF.
; ==========================================================================
Clip24To16:
	or a
	jr z,Clip24To16.SmallPositive
	inc a
	jr z,Clip24To16.SmallNegative
	dec a
	ld bc,32767
	ret p
	ld bc,-32768
	ret

Clip24To16.SmallPositive:
	; BC is in the range 0..65535
	bit 7,b
	ret z
	ld bc,32767
	ret

Clip24To16.SmallNegative:
	; BC is in the range -1..-65536
	bit 7,b
	ret nz
	ld bc,-32768
	ret


; ==========================================================================
; GetYIntercept
; --------------------------------------------------------------------------
; Calculates the Y intercept of a wall (the point where Y=0).
; The returned value is cached.
; --------------------------------------------------------------------------
; Inputs:    Start.X, Start.Y, End.X, End.Y, Delta.X, Delta.Y.
; Outputs:   HL: The X coordinate on the wall for Y=0.
; Destroyed: AF, BC, DE.
; ==========================================================================
GetYIntercept:
	jr $+1
	nop
	
	; Result = Start.X - (Delta.X * Start.Y) / Delta.Y
	ld de,(Delta.X)
	ld bc,(Start.Y)
	call Nostromo.Maths.Mul.S16S16
	
	ld a,e
	ld b,h
	ld c,l
	ld de,(Delta.Y)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Start.X)
	or a
	sbc hl,bc

	ld a,$21 ; LC HL,nn
	ld (GetYIntercept+0),a
	ld (GetYIntercept+1),hl
	ret

; ==========================================================================
; GetXIntercept
; --------------------------------------------------------------------------
; Calculates the X intercept of a wall (the point where X=0).
; The returned value is cached.
; --------------------------------------------------------------------------
; Inputs:    Start.X, Start.Y, End.X, End.Y, Delta.X, Delta.Y.
; Outputs:   HL: The Y coordinate on the wall for X=0.
; Destroyed: AF, BC, DE.
; ==========================================================================
GetXIntercept:
	jr $+1
	nop
	
	; Result = Start.Y - (Delta.Y * Start.X) / Delta.X
	ld de,(Delta.Y)
	ld bc,(Start.X)
	call Nostromo.Maths.Mul.S16S16
	
	ld a,e
	ld b,h
	ld c,l
	ld de,(Delta.X)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Start.Y)
	or a
	sbc hl,bc
	
	ld a,$21 ; LC HL,nn
	ld (GetXIntercept+0),a
	ld (GetXIntercept+1),hl
	
	ret

.endmodule