.module Nostromo

Camera.X: .dw 768
Camera.Y: .dw 896
Camera.Z: .dw 0
Camera.Angle: .db 0
Camera.YShear: .db 0

#define neg_hl() ld a,h \ cpl \ ld h,a \ ld a,l \ cpl \ ld l,a \ inc hl
#define neg_de() ld a,d \ cpl \ ld d,a \ ld a,e \ cpl \ ld e,a \ inc de
#define neg_bc() ld a,b \ cpl \ ld b,a \ ld a,c \ cpl \ ld c,a \ inc de

#define cpl_hl() ld a,h \ cpl \ ld h,a \ ld a,l \ cpl \ ld l,a
#define cpl_de() ld a,d \ cpl \ ld d,a \ ld a,e \ cpl \ ld e,a
#define cpl_bc() ld a,b \ cpl \ ld b,a \ ld a,c \ cpl \ ld c,a

#include "Maths.asm"
#include "Vertices.asm"
#include "Wall.asm"
#include "Clip.asm"
#include "Subsector.asm"
#include "Tree.asm"
#include "Line.asm"
#include "Interrupt.asm"

Render.Camera.Z: .dw 0
Render.Camera.YShear: .dw 0

Sector.Front: .dw 0
Sector.Back: .dw 0

Previous.Camera.X: .dw 0
Previous.Camera.Y: .dw 0
Previous.Camera.Angle: .db 0

; ==========================================================================
; Render
; --------------------------------------------------------------------------
; Renders the level.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Render:

; --------------------------------------------------------------------------
; Clear the screen.
; --------------------------------------------------------------------------

	ld hl,plotSScreen
	ld (hl),0
	ld de,plotSScreen+1
	ld bc,767
	ldir

; --------------------------------------------------------------------------
; Clear the completed columns.
; --------------------------------------------------------------------------

	ld hl,CompletedColumns
	ld (hl),0
	ld de,CompletedColumns+1
	ld bc,95
	ldir

; --------------------------------------------------------------------------
; We have 96 columns to draw.
; --------------------------------------------------------------------------
	
	ld a,96
	ld (ColumnsToDraw),a

; --------------------------------------------------------------------------
; Set the the per-column clipping ranges.
; --------------------------------------------------------------------------

	ld hl,TopEdgeClip
	ld (hl),1
	ld de,TopEdgeClip+1
	ld bc,95
	ldir

	ld hl,BottomEdgeClip
	ld (hl),64
	ld de,BottomEdgeClip+1
	ld bc,95
	ldir

; --------------------------------------------------------------------------
; Adjust the camera variables for the "render" versions.
; --------------------------------------------------------------------------

	ld hl,(Camera.Z)
	neg_hl()
	ld (Render.Camera.Z),hl
	
	ld a,(Camera.YShear)
	ld l,a
	add a,32
	sbc a,a
	ld h,a
	ld de,32
	add hl,de
	ld (Render.Camera.YShear),hl

; --------------------------------------------------------------------------
; Transform the vertices.
; --------------------------------------------------------------------------
	
	ld a,(Previous.Camera.Angle)
	ld b,a
	ld a,(Camera.Angle)
	cp b
	jr z,+
	ld (Previous.Camera.Angle),a
	jr TransformVertices
+:	ld hl,(Previous.Camera.X)
	ld de,(Camera.X)
	or a
	sbc hl,de
	jr z,+
	ld (Previous.Camera.X),de
	jr TransformVertices
+:	ld hl,(Previous.Camera.Y)
	ld de,(Camera.Y)
	or a
	sbc hl,de
	jr z,SkipTransformVertices
	ld (Previous.Camera.Y),de
	
TransformVertices:
	ld hl,Vertices
	ld de,TransformedVertices
	ld bc,Vertices.Count
	call Vertices.Transform

SkipTransformVertices:

; --------------------------------------------------------------------------
; Mark all walls as not drawn this frame.
; --------------------------------------------------------------------------

	ld hl,Walls
	ld de,7 ; size of a wall.
	ld b,Walls.Count
-:	res Wall.DrawFlag.DrawnThisFrame,(hl)
	add hl,de
	djnz -

; --------------------------------------------------------------------------
; Walk the BSP tree to render the level.
; --------------------------------------------------------------------------

	ld (Render.Finish+1),sp

	ld ix,Tree
	ld hl,(Camera.X)
	ld de,(Camera.Y)
	ld bc,Render.RenderTreeNodeFunction
	call Tree.Walk

Render.Finish:
	ld sp,0
	
	ret

; ==========================================================================
; Render.RenderTreeNodeFunction
; --------------------------------------------------------------------------
; Renders a tree node.
; ==========================================================================
Render.RenderTreeNodeFunction:
	ld l,(ix+Tree.Node.Leaf+0)
	ld h,(ix+Tree.Node.Leaf+1)
	push hl
	pop ix
	jp Subsector.Draw

.fill (($+$FF)&$FF00)-$
CompletedColumns:
	.fill 96

.fill (($+$FF)&$FF00)-$
TopEdgeClip:
	.fill 96

.fill (($+$FF)&$FF00)-$
BottomEdgeClip:
	.fill 96

ColumnsToDraw:
	.db 0

.endmodule