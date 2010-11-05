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
DrawFlag.FillMiddle = 2
DrawFlag.DrawnThisFrame = 7

; ==========================================================================
; ClipAndDraw
; --------------------------------------------------------------------------
; Clips the wall to the view and draws it on the screen.
; ==========================================================================
ClipAndDraw:

; --------------------------------------------------------------------------
; Can we quickly backface cull the wall by its angle?
; --------------------------------------------------------------------------

	bit DrawFlag.FillMiddle,(iy+DrawFlags)
	jr z,+
	
	ld a,(Angle)
	cp 32
	ret c
	cp 224
	ret nc
+:

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

	ld a,(iy+ClipFlags)
	and %11100000
	ld (iy+ClipFlags),a

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

	; Run-on to the draw function.

; ==========================================================================
; Draw
; --------------------------------------------------------------------------
; Draws the wall.
; --------------------------------------------------------------------------
; Inputs:    DrawFlags: Flags to control drawing.
;            Trapezium.Start.Column: X coordinate of the projected start
;                of the wall.
;            Trapezium.End.Column: X coordinate of the projected end of the
;                wall.
;            Start.Y: Y coordinate of the wall's start vertex.
;            End.Y: Y coordinate of the wall's end vertex.
;            Sector.Front: Pointer to the sector in front of the wall.
;            Sector.Back: Pointer to the sector behind the wall.
; ==========================================================================
Draw:

; --------------------------------------------------------------------------
; Are we looking at the back of the wall?
; --------------------------------------------------------------------------

	ld b,a
	ld a,(Trapezium.End.Column)
	cp b
	jp nc,StartDrawing

; --------------------------------------------------------------------------
; We are looking at the back of the wall. Is it a middle wall? If so, skip.
; --------------------------------------------------------------------------

	bit DrawFlag.FillMiddle,(iy+DrawFlags)
	jp nz,SkipWall

; --------------------------------------------------------------------------
; It's a middle wall. Swap over the ends before rendering.
; --------------------------------------------------------------------------

	ld hl,(Start.Y)
	ld de,(End.Y)
	ld (End.Y),hl
	ld (Start.Y),de
	
	ld a,(Trapezium.Start.Column)
	ld b,a
	ld a,(Trapezium.End.Column)
	ld (Trapezium.Start.Column),a
	ld a,b
	ld (Trapezium.End.Column),a
	
	ld hl,(Sector.Front)
	ld de,(Sector.Back)
	ld (Sector.Back),hl
	ld (Sector.Front),de
	
	ld a,(iy+DrawFlags)
	and (1 << DrawFlag.StrokeStart) | (1 << DrawFlag.StrokeEnd)
	jr z,+
	cp (1 << DrawFlag.StrokeStart) | (1 << DrawFlag.StrokeEnd)
	jr z,+
	xor (1 << DrawFlag.StrokeStart) | (1 << DrawFlag.StrokeEnd)
	ld (iy+DrawFlags),a
+:

; --------------------------------------------------------------------------
; Begin drawing the wall.
; --------------------------------------------------------------------------

StartDrawing:

; --------------------------------------------------------------------------
; Are we drawing a "middle" or an "upper/lower" wall type?
; --------------------------------------------------------------------------

	bit DrawFlag.FillMiddle,(iy+DrawFlags)
	jp nz,Wall.DrawMiddle

; --------------------------------------------------------------------------
; Draw an "upper and lower" wall.
; --------------------------------------------------------------------------
Wall.DrawUpperAndLower:
	
; --------------------------------------------------------------------------
; Get the front sector floor and ceiling heights.
; --------------------------------------------------------------------------

	ld hl,(Sector.Front)
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	ld (UpperLower.FrontFloorHeight),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ld (UpperLower.FrontCeilingHeight),de
	
; --------------------------------------------------------------------------
; Get the back sector floor and ceiling heights.
; --------------------------------------------------------------------------

	ld hl,(Sector.Back)
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	ld (UpperLower.BackFloorHeight),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	
	ld (UpperLower.BackCeilingHeight),de

; --------------------------------------------------------------------------
; Draw the "upper" (connects ceilings between adjacent sectors).
; --------------------------------------------------------------------------

	ld hl,(UpperLower.FrontCeilingHeight)
	ld de,(UpperLower.BackCeilingHeight)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr z,Upper.Done
	jr c,Upper.FrontCeilingBelowBackCeiling

; --------------------------------------------------------------------------
; The front sector's ceiling is above the back sector's ceiling.
; --------------------------------------------------------------------------
Upper.FrontCeilingAboveBackCeiling:	

	ld hl,(UpperLower.BackCeilingHeight)
	call ProjectHorizontalEdge

	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Floor),hl
	push hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Floor),hl
	push hl

	ld hl,(UpperLower.FrontCeilingHeight)
	call ProjectHorizontalEdge

	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Ceiling),hl
	push hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Ceiling),hl
	push hl
	
	call DrawVerticalEdges	
	
	pop hl
	ld (HorizontalEdge.End.Y),hl
	pop hl
	ld (HorizontalEdge.Start.Y),hl
	ld de,Line.Clip.Default
	call DrawHorizontalEdge

	pop hl
	ld (HorizontalEdge.End.Y),hl
	pop hl
	ld (HorizontalEdge.Start.Y),hl
	ld de,Line.Clip.UpperFloor
	set Line.LineFlag.TopDown,(iy+Line.LineFlags)
	call DrawHorizontalEdge

	jr Upper.Done

; --------------------------------------------------------------------------
; The front sector's ceiling is below the back sector's ceiling.
; --------------------------------------------------------------------------
Upper.FrontCeilingBelowBackCeiling:

	ld hl,(UpperLower.FrontCeilingHeight)
	
	call ProjectHorizontalEdge
	ld de,Line.Clip.UpperFloor
	set Line.LineFlag.TopDown,(iy+Line.LineFlags)
	call DrawHorizontalEdge

Upper.Done:

; --------------------------------------------------------------------------
; Draw the "lower" (connects floors between adjacent sectors).
; --------------------------------------------------------------------------

	ld hl,(UpperLower.FrontFloorHeight)
	ld de,(UpperLower.BackFloorHeight)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr z,Lower.Done
	jr nc,Lower.FrontFloorAboveBackFloor

; --------------------------------------------------------------------------
; The front sector's floor is below the back sector's floor.
; --------------------------------------------------------------------------
Lower.FrontFloorBelowBackFloor:

	ld hl,(UpperLower.BackFloorHeight)
	call ProjectHorizontalEdge

	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Ceiling),hl
	push hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Ceiling),hl
	push hl

	ld hl,(UpperLower.FrontFloorHeight)
	call ProjectHorizontalEdge

	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Floor),hl
	push hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Floor),hl
	push hl
	
	call DrawVerticalEdges	
	
	pop hl
	ld (HorizontalEdge.End.Y),hl
	pop hl
	ld (HorizontalEdge.Start.Y),hl
	ld de,Line.Clip.Default
	call DrawHorizontalEdge

	pop hl
	ld (HorizontalEdge.End.Y),hl
	pop hl
	ld (HorizontalEdge.Start.Y),hl
	ld de,Line.Clip.LowerCeiling
	res Line.LineFlag.TopDown,(iy+Line.LineFlags)
	call DrawHorizontalEdge

	jr Lower.Done

; --------------------------------------------------------------------------
; The front sector's floor is below the back sector's floor.
; --------------------------------------------------------------------------
Lower.FrontFloorAboveBackFloor:

	ld hl,(UpperLower.FrontFloorHeight)
	call ProjectHorizontalEdge
	ld de,Line.Clip.LowerCeiling
	res Line.LineFlag.TopDown,(iy+Line.LineFlags)
	call DrawHorizontalEdge

Lower.Done:

	jr Wall.Drawn

; --------------------------------------------------------------------------
; Draw a "middle" wall.
; --------------------------------------------------------------------------
Wall.DrawMiddle:

; --------------------------------------------------------------------------
; "Middle" walls always use the front subsector for heights.
; --------------------------------------------------------------------------
	
	ld hl,(Sector.Front)

; --------------------------------------------------------------------------
; Draw the "middle" wall's floor edge.
; --------------------------------------------------------------------------

	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	
	push hl
	
	ex de,hl
	call ProjectHorizontalEdge
	ld de,Line.Clip.Default
	call DrawHorizontalEdge

; --------------------------------------------------------------------------
; Copy over the projected Y coordinates for the floor.
; --------------------------------------------------------------------------
	
	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Floor),hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Floor),hl
	
	pop hl

; --------------------------------------------------------------------------
; Draw the "middle" wall's ceiling edge.
; --------------------------------------------------------------------------

	ld e,(hl)
	inc hl
	ld d,(hl)

	ex de,hl
	call ProjectHorizontalEdge
	ld de,Line.Clip.Default
	call DrawHorizontalEdge

; --------------------------------------------------------------------------
; Copy over the projected Y coordinates for the ceiling.
; --------------------------------------------------------------------------
	
	ld hl,(HorizontalEdge.Start.Y)
	ld (Trapezium.Start.Ceiling),hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Trapezium.End.Ceiling),hl

; --------------------------------------------------------------------------
; Draw the vertical edges of the wall.
; --------------------------------------------------------------------------

	call DrawVerticalEdges

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
	
	inc h
	ld (hl),64+1
	inc h
	ld (hl),-1+1
	
	ld h,CompletedColumns >> 8
	
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
; ProjectHorizontalEdge
; --------------------------------------------------------------------------
; Projects a horizontal edge to the screen.
; "Horizontal" refers to a line with constant height in 3D (when projected
; to 2D it will appear sloped).
; --------------------------------------------------------------------------
; Inputs:    HL: Height of the edge.
; Outputs:   HorizontalEdge.Start.Y, HorizontalEdge.End.Y: Projected Y
;            coordinates of the ends of the wall edge.
; ==========================================================================
ProjectHorizontalEdge:

; --------------------------------------------------------------------------
; Calculate the height relative to the camera position.
; --------------------------------------------------------------------------	

	ld de,(Render.Camera.Z)
	add hl,de

; --------------------------------------------------------------------------
; Project the height of the wall start to the screen.
; --------------------------------------------------------------------------

	push hl
	ld de,(Start.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,(Render.Camera.YShear)
	or a
	sbc hl,bc
	ld (Clip.g_line16Y1),hl
	ld (HorizontalEdge.Start.Y),hl
	pop hl

; --------------------------------------------------------------------------
; Project the height of the wall end to the screen.
; --------------------------------------------------------------------------

	ld de,(End.Y)
	call Maths.Div.S16S16
	call Clip24To16
	ld hl,(Render.Camera.YShear)
	or a
	sbc hl,bc
	ld (HorizontalEdge.End.Y),hl
	
	ret
	
; ==========================================================================
; DrawHorizontalEdge
; --------------------------------------------------------------------------
; Draws a horizontal edge of a wall.
; "Horizontal" refers to a line with constant height in 3D (when projected
; to 2D it will appear sloped).
; --------------------------------------------------------------------------
; Inputs:    DE: Pointer to pixel clipping routine.
; Outputs:   HorizontalEdge.Start.Y, HorizontalEdge.End.Y: Projected Y
;            coordinates of the ends of the wall edge.
; ==========================================================================
DrawHorizontalEdge:

	ld hl,(HorizontalEdge.Start.Y)
	ld (Clip.g_line16Y1),hl
	
	ld hl,(HorizontalEdge.End.Y)
	ld (Clip.g_line16Y2),hl

; --------------------------------------------------------------------------
; Set the pixel clipping routine.
; --------------------------------------------------------------------------

	ld (Line.ClipPixel),de

; --------------------------------------------------------------------------
; Clip the line.
; --------------------------------------------------------------------------

	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a	
	call Clip.Clip2DLine16Ex
	
; --------------------------------------------------------------------------
; Was it entirely culled?
; --------------------------------------------------------------------------
	
	jr c,HorizontalEdge.Culled

; --------------------------------------------------------------------------
; The wall was not culled, so draw it.
; --------------------------------------------------------------------------
	
	call Line.Draw

; --------------------------------------------------------------------------
; Do we need to handle clipped regions of the line?
; --------------------------------------------------------------------------

	
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
	jr z,HorizontalEdge.StartNotClipped
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The start has been clipped.
	ld hl,(Line.ClipPixel)
	ld (HorizontalEdge.StartClipper),hl
	
	ld hl,(HorizontalEdge.Start.Y)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.Start.Column)
	ld l,a
	
-:	
HorizontalEdge.StartClipper = $+1
	call Line.Clip.Default
	inc l
	djnz -
	jr HorizontalEdge.Done

HorizontalEdge.StartNotClipped:

	; Try the end.
	ld a,(Clip.g_line16X2)	
	ld b,a
	ld a,(Trapezium.End.Column)
	sub b
	jr z,HorizontalEdge.Done
	
	; The number of columns to fix.
	ld b,a
	inc b

	; The end has been clipped.
	ld hl,(Line.ClipPixel)
	ld (HorizontalEdge.EndClipper),hl
	
	ld hl,(HorizontalEdge.End.Y)
	call Clip16ToRowPlusOne
	inc a
	ld h,a
	ld a,(Trapezium.End.Column)
	ld l,a
	
-:	
HorizontalEdge.EndClipper = $+1
	call Line.Clip.Default
	dec l
	djnz -
	
	jr HorizontalEdge.Done

HorizontalEdge.Culled:

	ld hl,(Line.ClipPixel)
	ld (HorizontalEdge.Culled.Clipper),hl

	ld hl,(HorizontalEdge.Start.Y)
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
HorizontalEdge.Culled.Clipper = $+1
	call Line.Clip.Default
	inc l
	djnz -

HorizontalEdge.Done:
	ret

; ==========================================================================
; DrawVerticalEdges
; --------------------------------------------------------------------------
; Draws the vertical edges of a wall.
; ==========================================================================
DrawVerticalEdges:

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
	ld a,(de)
	cp c
	jr c,+
	ld a,c
+:
	
	sub b
	jr c,WallPart.SkipStrokeStart
	jr z,WallPart.SkipStrokeStart
	
	ld e,b
	ld b,a
	inc b

	ld a,(Trapezium.Start.Column)
	
	push bc
	dec e
	call Pixel.GetInformation
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
	inc hl
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
	dec hl
	call Clip16ToRow
	inc a
	ld c,a
	
	inc d
	ld a,(de)
	cp c
	jr c,+
	ld a,c
+:
	
	sub b
	jr c,WallPart.SkipStrokeEnd
	jr z,WallPart.SkipStrokeEnd	
	
	ld e,b
	ld b,a
	inc b

	ld a,(Trapezium.End.Column)
	
	push bc
	dec e
	call Pixel.GetInformation
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
	ld a,h
	
	; Can we clip against the top edge?
	ld h,TopEdgeClip >> 8
	cp (hl)
	jr nc,+
	ld h,a
	ret
	
+:	; Can we clip against the bottom edge?
	inc h
	cp (hl)
	ccf
	ld h,a
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
	ld a,h
	
	; Can we clip against the top edge?
	ld h,TopEdgeClip >> 8
	cp (hl)
	jr c,+
	
	ld (hl),a

	; Can we clip against the bottom edge?

	inc h
	cp (hl)
	ccf

+:	ld h,a
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
	ld a,h
	
	; Can we clip against the bottom edge?
	ld h,BottomEdgeClip >> 8
	cp (hl)
	ccf
	jr c,+
	
	ld (hl),a

	; Can we clip against the top edge?
	dec h
	cp (hl)

+:	ld h,a
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
	
	push de
	
	ld de,(Delta.Y)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Start.X)
	
	pop de
	ld a,d
	xor e
	jp m,+
	neg_bc()
+:	add hl,bc

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
	
	push de
	
	ld de,(Delta.X)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Start.Y)
	
	pop de
	ld a,d
	xor e
	jp m,+
	neg_bc()
+:	add hl,bc
	
	ld a,$21 ; LC HL,nn
	ld (GetXIntercept+0),a
	ld (GetXIntercept+1),hl
	
	ret

.endmodule