.module Program

	jp Main

#include "Level.inc"
#include "Nostromo/Nostromo.asm"
#include "LINECLIP.INC"
#include "LINEDRAW.INC"

Main:

Loop:

	.bcall _GrBufClr
	
	ld hl,48*256+32
	ld de,16*256
	call lineClipAndDraw
	
	ld hl,48*256+32
	ld de,80*256
	call lineClipAndDraw

	call TransformVertices
	;call PlotVertices
	call PlotWalls
	
	; Walk the tree.
	xor a
	ld (penCol),a
	ld (penRow),a
	ld ix,Tree
	;call WalkTree
	
	call ionFastCopy
	
	.bcall _GetCSC
	cp skClear	
	ret z
	
	ld a,$FF
	out (1),a
	nop
	ld a,$FE
	out (1),a
	nop
	nop
	in a,(1)
	ld c,a
	
	ld hl,Nostromo.Camera.Angle
	
	
	bit 1,c
	jr nz,+
	inc (hl)
+:

	bit 2,c
	jr nz,+
	dec (hl)
+:

	push bc
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Sin
	sra b \ rr c
	sra b \ rr c
	ld (Forwards.X),bc
	
	ld a,(Nostromo.Camera.Angle)
	call Nostromo.Maths.Trig.Cos
	sra b \ rr c
	sra b \ rr c
	ld (Forwards.Y),bc
	
	pop bc
	
	bit 3,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	add hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	add hl,de
	ld (Nostromo.Camera.Y),hl
	
+:

	bit 0,c
	jr nz,+
	
	ld hl,(Nostromo.Camera.X)
	ld de,(Forwards.X)
	or a
	sbc hl,de
	ld (Nostromo.Camera.X),hl
	
	ld hl,(Nostromo.Camera.Y)
	ld de,(Forwards.Y)
	or a
	sbc hl,de
	ld (Nostromo.Camera.Y),hl
	
+:
	jp Loop


TransformVertices:
	ld hl,Vertices
	ld de,saveSScreen
	ld bc,Vertices.Count
	jp Nostromo.Vertices.Transform	

PlotVertices:
	ld hl,saveSScreen
	ld b,Vertices.Count
	
-:	inc hl
	ld a,(hl)
	inc hl
	
	add a,48
	ld d,a
	
	inc hl
	ld a,(hl)
	inc hl
	
	neg
	add a,32
	ld e,a
	
	push bc
	push hl
	
	cp 64
	jr nc,+
	
	ld a,d
	cp 96
	jr nc,+
	
	call ionGetPixel
	or (hl)
	ld (hl),a
	
+:

	pop hl
	pop bc
	
	djnz -
	ret

PlotWalls.ClipFlags = asm_Flag1
PlotWalls.ClipFlag.StartOutsideLeft = 0
PlotWalls.ClipFlag.StartOutsideRight = 1
PlotWalls.ClipFlag.EndOutsideLeft = 2
PlotWalls.ClipFlag.EndOutsideRight = 3
PlotWalls.ClipFlag.Steep = 4

PlotWalls:
	ld hl,Walls
	ld b,Walls.Count

PlotWalls.Loop:
	push hl
	
	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,saveSScreen
	add hl,de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Wall.Start.X),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Wall.Start.Y),de
	
	pop hl
	inc hl
	push hl
	
	ld l,(hl)
	ld h,0
	add hl,hl
	add hl,hl
	ld de,saveSScreen
	add hl,de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld (Wall.End.X),de
	
	ld e,(hl)
	inc hl
	ld d,(hl)
	ld (Wall.End.Y),de
	
	pop hl
	inc hl

	push hl
	push bc

; --------------------------------------------------------------------------
; Is the wall entirely behind the camera?
; --------------------------------------------------------------------------

	ld a,(Wall.Start.Y+1)
	ld b,a
	ld a,(Wall.End.Y+1)
	and b
	jp m,SkipWall

; --------------------------------------------------------------------------
; Calculate the wall delta values.
; --------------------------------------------------------------------------

	ld hl,(Wall.End.X)
	ld de,(Wall.Start.X)
	or a
	sbc hl,de
	ld (Wall.Delta.X),hl
	bit 7,h
	jr nz,+
	neg_hl()
+:	ld (Wall.Delta.AbsX),hl
	
	ld hl,(Wall.End.Y)
	ld de,(Wall.Start.Y)
	or a
	sbc hl,de
	ld (Wall.Delta.Y),hl
	bit 7,h
	jr nz,+
	neg_hl()
+:	ld (Wall.Delta.AbsY),hl

; --------------------------------------------------------------------------
; Clear the GetYIntercept and GetXIntercept cache.
; --------------------------------------------------------------------------

	ld hl,$0118 ; JR $+1
	ld (Wall.GetYIntercept+0),hl
	ld (Wall.GetXIntercept+0),hl

; --------------------------------------------------------------------------
; Clear the clipping flags.
; --------------------------------------------------------------------------

	ld (iy+PlotWalls.ClipFlags),0

; --------------------------------------------------------------------------
; Clip to Y=0.
; --------------------------------------------------------------------------

	; Does the start intersect the Y axis?

	; Clip the start of the wall to Y=0.
	ld a,(Wall.Start.Y+1)
	bit 7,a
	jr z,Wall.Start.DoesNotIntersectY

	ld de,(Wall.Delta.X)
	ld a,d
	or e
	jr z,+

	call Wall.GetYIntercept	
	ld (Wall.Start.X),hl

+:	ld hl,0
	ld (Wall.Start.Y),hl

	; We know that the end can't intersect Y,
	; as we would have already culled the wall.
	; (At least one end must have Y>=0).
	jr Wall.End.DoesNotIntersectY

Wall.Start.DoesNotIntersectY:

	; Clip the end of the wall to Y=0.
	ld a,(Wall.End.Y+1)
	bit 7,a
	jr z,Wall.End.DoesNotIntersectY

	ld de,(Wall.Delta.X)
	ld a,d
	or e
	jr z,+

	call Wall.GetYIntercept

	ld (Wall.End.X),hl

+:	ld hl,0
	ld (Wall.End.Y),hl

Wall.End.DoesNotIntersectY:

Wall.ClippedToY:

; --------------------------------------------------------------------------
; Does the wall need to be clipped to Y=+X or Y=-X?
; --------------------------------------------------------------------------

	; Count the number of times Y=+X (B) and Y=-X (C) are clipped against.
	ld bc,$0202

	; Check the start against Y=+X.

	ld hl,(Wall.Start.X)
	ld de,(Wall.Start.Y)
	
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	jr c,+
	dec b
	set PlotWalls.ClipFlag.StartOutsideRight,(iy+PlotWalls.ClipFlags)
+:

	; Check the start against Y=-X.

	ld hl,(Wall.Start.X)
	ld de,(Wall.Start.Y)
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

	ld hl,(Wall.End.X)
	ld de,(Wall.End.Y)
	
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

	ld hl,(Wall.End.X)
	ld de,(Wall.End.Y)
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
	jp z,Wall.NoViewClippingRequired

; --------------------------------------------------------------------------
; Some clipping is, alas, required.
; --------------------------------------------------------------------------
	
	; Is the wall steep or shallow?
	; A "steep" wall is one in which |dY| > |dX|.
	
	ld hl,(Wall.Delta.AbsX)
	ld de,(Wall.Delta.AbsY)
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	
	ld hl,(Wall.Delta.Y)
	ld de,(Wall.Delta.X)
	
	jr c,Wall.IsShallow

Wall.IsSteep:
	set PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	ex de,hl

Wall.IsShallow:

; --------------------------------------------------------------------------
; Calculate the gradient of the line.
; --------------------------------------------------------------------------

	call Nostromo.Maths.Div.S16S16
	ld (Wall.Gradient),bc

; --------------------------------------------------------------------------
; Clip the start to Y=+X.
; --------------------------------------------------------------------------

	bit PlotWalls.ClipFlag.StartOutsideRight,(iy+PlotWalls.ClipFlags)
	jr z,Wall.ClippedStartRight
	
	; If dY == 0, Start.X = Start.Y.
	ld hl,(Wall.Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(Wall.Start.Y)
		ld (Wall.Start.X),hl
		jp Wall.ClippedStartRight
	+:
	
	; If dX == 0, Start.Y = Start.X.
	ld hl,(Wall.Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(Wall.Start.X)
		ld (Wall.Start.Y),hl
		jp Wall.ClippedStartRight
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipStartRight.Shallow

PlotWalls.ClipStartRight.Steep:

	call Wall.GetYIntercept
	jr PlotWalls.ClipStartRight.Clip
	
PlotWalls.ClipStartRight.Shallow:

	call Wall.GetXIntercept

PlotWalls.ClipStartRight.Clip:

	; X = -c * 256 / m - 256
	ld de,(Wall.Gradient)
	dec d
	call Nostromo.Maths.Div.S16S16
	neg_bc()
	
	ld (Wall.Start.X),bc
	ld (Wall.Start.Y),bc

Wall.ClippedStartRight:

; --------------------------------------------------------------------------
; Clip the end to Y=+X.
; --------------------------------------------------------------------------

	bit PlotWalls.ClipFlag.EndOutsideRight,(iy+PlotWalls.ClipFlags)
	jr z,Wall.ClippedEndRight
	
	; If dY == 0, End.X = End.Y.
	ld hl,(Wall.Delta.Y)
	ld a,h
	or l
	jr nz,+
		ld hl,(Wall.End.Y)
		ld (Wall.End.X),hl
		jp Wall.ClippedEndRight
	+:
	
	; If dX == 0, End.Y = End.X.
	ld hl,(Wall.Delta.X)
	ld a,h
	or l
	jr nz,+
		ld hl,(Wall.End.X)
		ld (Wall.End.Y),hl
		jp Wall.ClippedEndRight
	+:
	
	; We can't take a shortcut, so perform a slow clip.
	bit PlotWalls.ClipFlag.Steep,(iy+PlotWalls.ClipFlags)
	jr z,PlotWalls.ClipEndRight.Shallow

PlotWalls.ClipEndRight.Steep:

	call Wall.GetYIntercept
	jr PlotWalls.ClipEndRight.Clip
	
PlotWalls.ClipEndRight.Shallow:

	call Wall.GetXIntercept

PlotWalls.ClipEndRight.Clip:

	; X = -c * 256 / m - 256
	ld de,(Wall.Gradient)
	dec d
	call Nostromo.Maths.Div.S16S16
	neg_bc()
	
	ld (Wall.End.X),bc
	ld (Wall.End.Y),bc

Wall.ClippedEndRight:

Wall.NoViewClippingRequired:
	
	ld a,(Wall.Start.X+1)
	add a,48
	ld d,a
	ld a,(Wall.Start.Y+1)
	neg
	add a,32
	ld e,a
	
	ld a,(Wall.End.X+1)
	add a,48
	ld h,a
	ld a,(Wall.End.Y+1)
	neg
	add a,32
	ld l,a

	call lineClipAndDrawLong

SkipWall:

	pop bc
	pop hl
	djnz PlotWalls.Loop.Branch
	ret
PlotWalls.Loop.Branch:
	jp PlotWalls.Loop


Wall.Start.X: .dw 0
Wall.Start.Y: .dw 0
Wall.End.X: .dw 0
Wall.End.Y: .dw 0
Wall.Delta.X: .dw 0
Wall.Delta.AbsX: .dw 0
Wall.Delta.Y: .dw 0
Wall.Delta.AbsY: .dw 0
Wall.Gradient: .dw 0

; ==========================================================================
; Wall.GetYIntercept
; --------------------------------------------------------------------------
; Calculates the Y intercept of a wall (the point where Y=0).
; The returned value is cached.
; --------------------------------------------------------------------------
; Inputs:    Wall.Start.X, Wall.Start.Y, Wall.End.X, Wall.End.Y,
;            Wall.Delta.X, Wall.Delta.Y.
; Ouptuts:   HL: The X coordinate on the wall for Y=0.
; Destroyed: AF, BC, DE.
; ==========================================================================
Wall.GetYIntercept:
	jr $+1
	nop
	
	; Result = Wall.Start.X - (Wall.Delta.X * Wall.Start.Y) / Wall.Delta.Y
	ld de,(Wall.Delta.X)
	ld bc,(Wall.Start.Y)
	call Nostromo.Maths.Mul.S16S16
	
	ld a,e
	ld b,h
	ld c,l
	ld de,(Wall.Delta.Y)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Wall.Start.X)
	or a
	sbc hl,bc

	ld a,$21 ; LC HL,nn
	ld (Wall.GetYIntercept+0),a
	ld (Wall.GetYIntercept+1),hl
	ret

; ==========================================================================
; Wall.GetXIntercept
; --------------------------------------------------------------------------
; Calculates the X intercept of a wall (the point where X=0).
; The returned value is cached.
; --------------------------------------------------------------------------
; Inputs:    Wall.Start.X, Wall.Start.Y, Wall.End.X, Wall.End.Y,
;            Wall.Delta.X, Wall.Delta.Y.
; Ouptuts:   HL: The Y coordinate on the wall for X=0.
; Destroyed: AF, BC, DE.
; ==========================================================================
Wall.GetXIntercept:
	jr $+1
	nop
	
	; Result = Wall.Start.Y - (Wall.Delta.Y * Wall.Start.X) / Wall.Delta.X
	ld de,(Wall.Delta.Y)
	ld bc,(Wall.Start.X)
	call Nostromo.Maths.Mul.S16S16
	
	ld a,e
	ld b,h
	ld c,l
	ld de,(Wall.Delta.X)
	call Nostromo.Maths.Div.S24S16

	ld hl,(Wall.Start.Y)
	or a
	sbc hl,bc
	
	ld a,$21 ; LC HL,nn
	ld (Wall.GetXIntercept+0),a
	ld (Wall.GetXIntercept+1),hl
	
	ret
	

WalkTree:
	ld a,(ix+0)
	or a
	jr nz,WalkTree.Partition

WalkTree.Leaf:
	; We've encountered a leaf.
	ld a,(ix+1)
	.bcall _SetXXOP1
	ld a,2
	set textWrite,(iy+sGrFlags)
	.bcall _DispOP1A
	ret

WalkTree.Partition:
	; Load the partition position.
	ld l,(ix+1)
	ld h,(ix+2)
	
	; Is it a horizontal or vertical partition?
	dec a
	jr nz,WalkTree.HorizontalPartition

WalkTree.VerticalPartition:
	; We've encountered a vertical partition.
	ld de,(Nostromo.Camera.X)
	jr WalkTree.CheckPartitionSide
	
WalkTree.HorizontalPartition:
	; We've encountered a horizontal partition.
	ld de,(Nostromo.Camera.Y)

WalkTree.CheckPartitionSide:
	; Which side of the partition are we on?
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	jr c,WalkTree.BehindPartition

WalkTree.InFrontOfPartition:
	push ix
	ld l,(ix+5)
	ld h,(ix+6)
	push hl
	pop ix
	call WalkTree
	pop ix
	ld l,(ix+3)
	ld h,(ix+4)
	push hl
	pop ix
	jp WalkTree

WalkTree.BehindPartition:
	push ix
	ld l,(ix+3)
	ld h,(ix+4)
	push hl
	pop ix
	call WalkTree
	pop ix
	ld l,(ix+5)
	ld h,(ix+6)
	push hl
	pop ix
	jp WalkTree

Forwards.X: .dw 0
Forwards.Y: .dw 0

.endmodule