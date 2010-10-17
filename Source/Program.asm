.module Program

	jp Main

#include "Level.inc"
#include "Nostromo/Nostromo.asm"
#include "LINECLIP.INC"
#include "LINEDRAW.INC"

Main:

Loop:

	.bcall _GrBufClr

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
	
	; Is the wall entirely behind the camera?
	; if (startVertex.Y <= 0 && endVertex.Y <= 0) continue;
	
	ld a,(Wall.Start.Y+1)
	ld b,a
	ld a,(Wall.End.Y+1)
	and b
	jp m,SkipWall
	
	; Calculate wall deltas.
	
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

	; Clear the GetYIntercept cache.
	ld hl,$0118 ; JR $+1
	ld (Wall.GetYIntercept+0),hl
	
	; Does the start intersect the Y axis?
	
	; Clip the start of the line to Y=0.
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

	; We know that the end doesn't intersect Y, as we would have already culled the line.
	; (At least one end must have Y>=0).
	jr Wall.End.DoesNotIntersectY

Wall.Start.DoesNotIntersectY:

	; Clip the end of the line to Y=0.
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

Wall.GetYIntercept:
	jr $+1
	nop
	
	; Wall.Start.X - (Wall.Delta.X * Wall.Start.Y) / Wall.Delta.Y
	ld de,(Wall.Delta.X)
	ld bc,(Wall.Start.Y)
	call Nostromo.Maths.Mul.S16S16
	
	; DEHL = dX [DE] * startVertex.Y [BC]
	
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