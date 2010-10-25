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

DrawFlags = asm_Flag2
DrawFlag.StrokeStart = 0
DrawFlag.StrokeEnd = 1
DrawFlag.FillUpper = 2
DrawFlag.FillMiddle = 3
DrawFlag.FillLower = 4

Trapezium.Start.Column: .db 0
Trapezium.End.Column: .db 0

Trapezium.Start.Ceiling: .dw 0
Trapezium.Start.Floor: .dw 0
Trapezium.End.Ceiling: .dw 0
Trapezium.End.Floor: .dw 0

Start.X: .dw 0
Start.Y: .dw 0

End.X: .dw 0
End.Y: .dw 0

Delta.X: .dw 0
Delta.AbsX: .dw 0

Delta.Y: .dw 0
Delta.AbsY: .dw 0

Gradient: .dw 0

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
	res DrawFlag.StrokeStart,(iy+DrawFlags)
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
	res DrawFlag.StrokeStart,(iy+DrawFlags)
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
	res DrawFlag.StrokeEnd,(iy+DrawFlags)
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
	res DrawFlag.StrokeEnd,(iy+DrawFlags)
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
	ld (Trapezium.End.Column),a

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
	ld (Trapezium.Start.Column),a

; --------------------------------------------------------------------------
; Are we looking at the back of the wall?
; --------------------------------------------------------------------------

	ld b,a
	ld a,(Trapezium.End.Column)
	cp b
	jp c,SkipWall

; --------------------------------------------------------------------------
; Fetch the ceiling and floor height for the front sector.
; --------------------------------------------------------------------------

	ld a,(iy+DrawFlags)
	bit DrawFlag.FillMiddle,a
	jr nz,Wall.DrawMiddle

Wall.DrawUpperAndLower:
	
; --------------------------------------------------------------------------
; The upper part uses the back's ceiling height as the floor
; and the front's ceiling height as the ceiling.
; --------------------------------------------------------------------------
	
	ld hl,Line.Clip.Default
	ld (WallPart.UpperClipper),hl
	ld hl,Line.Clip.UpperFloor
	ld (WallPart.LowerClipper),hl
	
	ld hl,(Sector.Back)
	inc hl
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.FloorHeight),de
	ld hl,(Sector.Front)
	inc hl
	inc hl
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (WallPart.CeilingHeight),de

	call DrawWallPart
	
; --------------------------------------------------------------------------
; The lower part uses the front's floor height as the floor
; and the back's floor height as the ceiling.
; --------------------------------------------------------------------------
	
	ld hl,Line.Clip.LowerCeiling
	ld (WallPart.UpperClipper),hl
	ld hl,Line.Clip.Default
	ld (WallPart.LowerClipper),hl

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

; --------------------------------------------------------------------------
; Update the clipped columns.
; --------------------------------------------------------------------------

	ld hl,(Trapezium.Start.Column)
	ld h,TopEdgeClip >> 8
	ld a,(Trapezium.End.Column)
	sub l
	ld b,a
	inc b
-:	inc h
	ld a,(hl)
	dec h
	ld (hl),a
	inc h
	inc h
	inc h
	ld a,(hl)
	dec h
	ld (hl),a
	dec h
	dec h
+:	inc l
	djnz -
	
	jr Wall.Drawn

Wall.DrawMiddle:
	
	; Use the default clipper for upper/lower.
	ld hl,Line.Clip.Default
	ld (WallPart.UpperClipper),hl
	ld (WallPart.LowerClipper),hl

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

; --------------------------------------------------------------------------
; Flag the "middle" columns as being drawn.
; --------------------------------------------------------------------------

	ld hl,(Trapezium.Start.Column)
	ld h,CompletedColumns >> 8
	ld a,(Trapezium.End.Column)
	sub l
	ld b,a
	inc b
-:	ld a,(hl)
	or a
	jr nz,+
	dec a
	ld (hl),a
	ld a,(ColumnsToDraw)
	dec a
	jp z,Render.Finish ; Quickly bail out if we've finished.
	ld (ColumnsToDraw),a
+:	inc l
	djnz -

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
	ld (Trapezium.Start.Floor),hl

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
	ld (Trapezium.End.Floor),hl

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
	ld (Trapezium.Start.Ceiling),hl

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
	ld (Trapezium.End.Ceiling),hl

; --------------------------------------------------------------------------
; Draw the bottom edge of the wall.
; --------------------------------------------------------------------------

WallPart.LowerClipper = $+1
	ld hl,Line.Clip.Default
	ld (Line.ClipPixel),hl
	
	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.Start.Floor)
	ld (Clip.g_line16Y1),hl
	
	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.End.Floor)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	jr c,WallPart.Lower.Culled
	call Line.Draw

	; Do we need to handle clipped regions of the line?
	
	ld a,(Clip.g_line16X2)
	ld b,a
	ld a,(Clip.g_line16X1)

	cp b
	jr c,+
	ld (Clip.g_line16X2),a
	ld a,b
	ld (Clip.g_line16X1),a
+:
	
	; Try the start.
	ld a,(Trapezium.Start.Column)
	ld b,a
	ld a,(Clip.g_line16X1)
	sub b
	jr z,WallPart.Lower.StartNotClipped
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The start has been clipped.
	ld hl,(WallPart.LowerClipper)
	ld (WallPart.Lower.StartClipper),hl
	
	ld hl,(Trapezium.Start.Floor)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.Start.Column)
	ld l,a
	
-:	
WallPart.Lower.StartClipper = $+1
	call Line.Clip.Default
	inc l
	djnz -
	jr WallPart.Lower.Done

WallPart.Lower.StartNotClipped:

	; Try the end.
	ld a,(Clip.g_line16X2)	
	ld b,a
	ld a,(Trapezium.End.Column)
	sub b
	jr z,WallPart.Lower.Done
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The end has been clipped.
	ld hl,(WallPart.LowerClipper)
	ld (WallPart.Lower.EndClipper),hl
	
	ld hl,(Trapezium.End.Floor)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.End.Column)
	ld l,a
	
-:	
WallPart.Lower.EndClipper = $+1
	call Line.Clip.Default
	dec l
	djnz -
	
	jr WallPart.Lower.Done

WallPart.Lower.Culled:

	ld hl,(WallPart.LowerClipper)
	ld (WallPart.Lower.Culled.Clipped),hl

	ld hl,(Trapezium.Start.Floor)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.Start.Column)
	ld l,a
	ld a,(Trapezium.End.Column)
	sub l
	ld b,a
	inc b
-:	
WallPart.Lower.Culled.Clipped = $+1
	call Line.Clip.Default
	inc l
	djnz -

WallPart.Lower.Done:

; --------------------------------------------------------------------------
; Draw the top edge of the wall.
; --------------------------------------------------------------------------
	
WallPart.UpperClipper = $+1
	ld hl,Line.Clip.Default
	ld (Line.ClipPixel),hl
	
	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.Start.Ceiling)
	ld (Clip.g_line16Y1),hl
	
	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.End.Ceiling)
	ld (Clip.g_line16Y2),hl	
	
	call Clip.Clip2DLine16Ex
	jr c,WallPart.Upper.Culled
	call Line.Draw
	
	; Do we need to handle clipped regions of the line?
	
	ld a,(Clip.g_line16X2)
	ld b,a
	ld a,(Clip.g_line16X1)

	cp b
	jr c,+
	ld (Clip.g_line16X2),a
	ld a,b
	ld (Clip.g_line16X1),a
+:
	
	; Try the start.
	ld a,(Trapezium.Start.Column)
	ld b,a
	ld a,(Clip.g_line16X1)
	sub b
	jr z,WallPart.Upper.StartNotClipped
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The start has been clipped.
	ld hl,(WallPart.UpperClipper)
	ld (WallPart.Upper.StartClipper),hl
	
	ld hl,(Trapezium.Start.Ceiling)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.Start.Column)
	ld l,a
	
-:	
WallPart.Upper.StartClipper = $+1
	call Line.Clip.Default
	inc l
	djnz -
	jr WallPart.Upper.Done

WallPart.Upper.StartNotClipped:

	; Try the end.
	ld a,(Clip.g_line16X2)	
	ld b,a
	ld a,(Trapezium.End.Column)
	sub b
	jr z,WallPart.Upper.Done
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The end has been clipped.
	ld hl,(WallPart.UpperClipper)
	ld (WallPart.Upper.EndClipper),hl
	
	ld hl,(Trapezium.End.Ceiling)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.End.Column)
	ld l,a
	
-:	
WallPart.Upper.EndClipper = $+1
	call Line.Clip.Default
	dec l
	djnz -
	
	jr WallPart.Upper.Done

WallPart.Upper.Culled:

	ld hl,(WallPart.UpperClipper)
	ld (WallPart.Upper.Culled.Clipper),hl

	ld hl,(Trapezium.Start.Ceiling)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.Start.Column)
	ld l,a
	ld a,(Trapezium.End.Column)
	sub l
	ld b,a
	inc b
-:	
WallPart.Upper.Culled.Clipper = $+1
	call Line.Clip.Default
	inc l
	djnz -

WallPart.Upper.Done:

; --------------------------------------------------------------------------
; Draw the lines between the floor and ceiling at the start.
; --------------------------------------------------------------------------

	bit DrawFlag.StrokeStart,(iy+DrawFlags)
	jr z,WallPart.SkipStrokeStart
	
	ld de,(Trapezium.Start.Column)
	ld d,CompletedColumns >> 8
	ld a,(de)
	or a
	jr nz,WallPart.SkipStrokeStart

	ld hl,(Trapezium.Start.Ceiling)
	call Clip16ToRow
	inc a
	ld b,a
	
	inc d
	ld a,(de)
	cp b
	jr c,+
	ld b,a
+:	
	
	ld hl,(Trapezium.Start.Floor)
	call Clip16ToRow
	inc a
	ld c,a
	
	inc d
	inc d
	ld a,(de)
	cp c
	jr c,+
	ld a,c
+:
	
	sub b
	jr c,WallPart.SkipStrokeStart
	
	ld e,b
	ld b,a
	inc b

	ld a,(Trapezium.Start.Column)
	
	push bc
	dec e
	call ionGetPixel
	pop bc
	
	ld de,12
	ld c,a
	
-:	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	djnz -
	
WallPart.SkipStrokeStart:

; --------------------------------------------------------------------------
; Draw the lines between the floor and ceiling at the end.
; --------------------------------------------------------------------------

	bit DrawFlag.StrokeEnd,(iy+DrawFlags)
	jr z,WallPart.SkipStrokeEnd
	
	ld de,(Trapezium.End.Column)
	ld d,CompletedColumns >> 8
	ld a,(de)
	or a
	jr nz,WallPart.SkipStrokeEnd

	ld hl,(Trapezium.End.Ceiling)
	call Clip16ToRow
	inc a
	ld b,a
	
	inc d
	ld a,(de)
	cp b
	jr c,+
	ld b,a
+:	
	
	ld hl,(Trapezium.End.Floor)
	call Clip16ToRow
	inc a
	ld c,a
	
	inc d
	inc d
	ld a,(de)
	cp c
	jr c,+
	ld a,c
+:
	
	sub b
	jr c,WallPart.SkipStrokeEnd
	
	ld e,b
	ld b,a
	inc b

	ld a,(Trapezium.End.Column)
	
	push bc
	dec e
	call ionGetPixel
	pop bc
	
	ld de,12
	ld c,a
	
-:	ld a,(hl)
	or c
	ld (hl),a
	add hl,de
	djnz -

WallPart.SkipStrokeEnd:

	ret

; ==========================================================================
; Line.Clip.Default
; --------------------------------------------------------------------------
; Clips line pixels against the completed columns and the top/bottom bounds.
; --------------------------------------------------------------------------
; Inputs:    (L,H): The pixel to clip.
; Outputs:   Carry set if clipped, cleared if not clipped.
; Destroyed: AF, D.
; ==========================================================================
Line.Clip.Default:
	ld d,h
	; Has the column been completed?
	ld h,CompletedColumns >> 8
	ld a,(hl)
	or a
	jr z,+
	ld h,d
	scf
	ret

+:	; Can we clip against the top edge?
	ld a,d
	inc h
	cp (hl)
	jr nc,+
	ld h,d
	ret
	
+:	; Can we clip against the bottom edge?

	inc h
	inc h
	cp (hl)
	jr z,+
	ccf

+:	ld h,d
	ret

; ==========================================================================
; Line.Clip.UpperFloor
; --------------------------------------------------------------------------
; Clips line pixels against the completed columns and the top bounds. If
; below the upper bounds, these are amended to clip later lines.
; --------------------------------------------------------------------------
; Inputs:    (L,H): The pixel to clip.
; Outputs:   Carry set if clipped, cleared if not clipped.
; Destroyed: AF, D.
; ==========================================================================
Line.Clip.UpperFloor:
	ld d,h
	; Has the column been completed?
	ld h,CompletedColumns >> 8
	ld a,(hl)
	or a
	jr z,+
	ld h,d
	scf
	ret

+:	; Can we clip against the top edge?
	ld a,d
	inc h
	cp (hl)
	jr c,+
	
	inc h
	ld (hl),d
	
	ld h,d
	ret

+:	ld h,d
	ret

; ==========================================================================
; Line.Clip.LowerCeiling
; --------------------------------------------------------------------------
; Clips line pixels against the completed columns and the bottom bounds. If
; above the bottom bounds, these are amended to clip later lines.
; --------------------------------------------------------------------------
; Inputs:    (L,H): The pixel to clip.
; Outputs:   Carry set if clipped, cleared if not clipped.
; Destroyed: AF, D.
; ==========================================================================
Line.Clip.LowerCeiling:
	ld d,h
	; Has the column been completed?
	ld h,CompletedColumns >> 8
	ld a,(hl)
	or a
	jr z,+
	ld h,d
	scf
	ret

+:	; Can we clip against the bottom edge?
	ld a,d
	inc h
	inc h
	inc h
	cp (hl)
	jr z,+
	ccf
	jr c,+
	
	inc h
	ld (hl),d
	
	ld h,d
	ret

+:	ld h,d
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
; Clip16ToRow
; --------------------------------------------------------------------------
; Clips a 16-bit number to an 8-bit one in the range of screen rows (0..63).
; --------------------------------------------------------------------------
; Inputs:    HL: The value to clip.
; Outputs:   A: The clipped value (between 0 and 63).
; Destroyed: F.
; ==========================================================================
Clip16ToRow:
	bit 7,h
	jr z,+
	xor a
	ret
+:	ld a,h
	or a
	jr z,+
	ld a,63
	ret
+:	ld a,l
	cp 64
	ret c
	ld a,63
	ret

; ==========================================================================
; Clip16ToRowPlusOne
; --------------------------------------------------------------------------
; Clips a 16-bit number to an 8-bit one in the range of screen rows plus one
; (-1..64).
; --------------------------------------------------------------------------
; Inputs:    HL: The value to clip.
; Outputs:   A: The clipped value (between -1 and 64).
; Destroyed: F.
; ==========================================================================
Clip16ToRowPlusOne:
	bit 7,h
	jr z,+
	ld a,-1
	ret
+:	ld a,h
	or a
	jr z,+
	ld a,64
	ret
+:	ld a,l
	cp 65
	ret c
	ld a,64
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