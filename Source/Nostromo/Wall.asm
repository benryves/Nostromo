.module Wall


Start.X: .dw 0
Start.Y: .dw 0
End.X: .dw 0
End.Y: .dw 0
Delta.X: .dw 0
Delta.AbsX: .dw 0
Delta.Y: .dw 0
Delta.AbsY: .dw 0
Gradient: .dw 0

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

	ld (iy+PlotWalls.ClipFlags),0

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
	set PlotWalls.ClipFlag.StartOutsideRight,(iy+PlotWalls.ClipFlags)
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
	set PlotWalls.ClipFlag.StartOutsideLeft,(iy+PlotWalls.ClipFlags)
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
	set PlotWalls.ClipFlag.EndOutsideRight,(iy+PlotWalls.ClipFlags)
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
	set PlotWalls.ClipFlag.EndOutsideLeft,(iy+PlotWalls.ClipFlags)
+:

; --------------------------------------------------------------------------
; Do we need to do any clipping?
; --------------------------------------------------------------------------
	
	ld a,(iy+PlotWalls.ClipFlags)
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
	set PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
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

	bit PlotWalls.ClipFlag.StartOutsideRight,(iy+PlotWalls.ClipFlags)
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
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipStartRight.Shallow

PlotWalls.ClipStartRight.Steep:

	call GetYIntercept
	jr PlotWalls.ClipStartRight.Clip
	
PlotWalls.ClipStartRight.Shallow:

	call GetXIntercept

PlotWalls.ClipStartRight.Clip:

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

	bit PlotWalls.ClipFlag.EndOutsideRight,(iy+PlotWalls.ClipFlags)
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
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipEndRight.Shallow

PlotWalls.ClipEndRight.Steep:

	call GetYIntercept
	jr PlotWalls.ClipEndRight.Clip
	
PlotWalls.ClipEndRight.Shallow:

	call GetXIntercept

PlotWalls.ClipEndRight.Clip:

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

	bit PlotWalls.ClipFlag.StartOutsideLeft,(iy+PlotWalls.ClipFlags)
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
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipStartLeft.Shallow

PlotWalls.ClipStartLeft.Steep:

	call GetYIntercept
	jr PlotWalls.ClipStartLeft.Clip
	
PlotWalls.ClipStartLeft.Shallow:

	call GetXIntercept

PlotWalls.ClipStartLeft.Clip:

	; X = c * 256 / m + 256
	ld de,(Gradient)
	inc d
	call Nostromo.Maths.Div.S16S16
	
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
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

	bit PlotWalls.ClipFlag.EndOutsideLeft,(iy+PlotWalls.ClipFlags)
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
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipEndLeft.Shallow

PlotWalls.ClipEndLeft.Steep:

	call GetYIntercept
	jr PlotWalls.ClipEndLeft.Clip
	
PlotWalls.ClipEndLeft.Shallow:

	call GetXIntercept

PlotWalls.ClipEndLeft.Clip:

	; X = c * 256 / m + 256
	ld de,(Gradient)
	inc d
	call Nostromo.Maths.Div.S16S16
	
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
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
	
	ld a,(Start.X+1)
	add a,48
	ld d,a
	ld a,(Start.Y+1)
	neg
	add a,32
	ld e,a
	
	ld a,(End.X+1)
	add a,48
	ld h,a
	ld a,(End.Y+1)
	neg
	add a,32
	ld l,a

	call lineClipAndDrawLong

SkipWall:

	ret

; ==========================================================================
; GetYIntercept
; --------------------------------------------------------------------------
; Calculates the Y intercept of a wall (the point where Y=0).
; The returned value is cached.
; --------------------------------------------------------------------------
; Inputs:    Start.X, Start.Y, End.X, End.Y, Delta.X, Delta.Y.
; Ouptuts:   HL: The X coordinate on the wall for Y=0.
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
; Ouptuts:   HL: The Y coordinate on the wall for X=0.
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
