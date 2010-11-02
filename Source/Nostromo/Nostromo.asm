.module Nostromo

CameraVariables:
Camera.X: .dw 768
Camera.Y: .dw 896
Camera.Z: .dw 0
Camera.Angle: .db 0
Camera.YShear: .db 0
CameraVariables.Size = $ - CameraVariables

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
#include "Pixel.asm"
#include "Screen.asm"
#include "Level.asm"
#include "Things.asm"

Render.Camera.Z: .dw 0
Render.Camera.YShear: .dw 0

Sector.Front: .dw 0
Sector.Back: .dw 0

Previous.Camera.X: .dw 0
Previous.Camera.Y: .dw 0
Previous.Camera.Angle: .db 0

AllocatedTableMemory: .dw 0
ClipTableAddress: .dw 0

RenderFlags = asm_Flag1
RenderFlag.DrawThings = 6

; ==========================================================================
; Initialise
; --------------------------------------------------------------------------
; Initialises the engine.
; --------------------------------------------------------------------------
; Outputs:   Carry flag set on failure.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Initialise:

; --------------------------------------------------------------------------
; We need to use page-aligned memory.
; --------------------------------------------------------------------------

	ld de,End
	ld a,e
	or a
	jr z,+
	inc d
	ld e,0
+:	ld (ClipTableAddress),de

; --------------------------------------------------------------------------
; The table is 768 bytes long.
; --------------------------------------------------------------------------
	
	ld hl,768
	add hl,de

; --------------------------------------------------------------------------
; The level's dynamic memory will appear after our tables.
; --------------------------------------------------------------------------

	ld (Level.DynamicMemory),hl

; --------------------------------------------------------------------------
; And so, how many bytes do we need to allocate?
; --------------------------------------------------------------------------

	ld de,End
	or a
	sbc hl,de
	ld (AllocatedTableMemory),hl

; --------------------------------------------------------------------------
; Do we have enough memory?
; --------------------------------------------------------------------------

	.bcall _EnoughMem
	ret c

; --------------------------------------------------------------------------
; Allocate the memory.
; --------------------------------------------------------------------------

	ex de,hl
	ld de,End
	
	.bcall _InsertMem

; --------------------------------------------------------------------------
; Load the interrupt service routine.
; --------------------------------------------------------------------------

	call Nostromo.Interrupt.Load

; --------------------------------------------------------------------------
; Set default flags.
; --------------------------------------------------------------------------

	ld a,1<<RenderFlag.DrawThings
	ld (iy+RenderFlags),a

; --------------------------------------------------------------------------
; All is well, so return safely.
; --------------------------------------------------------------------------

	or a
	ret

; ==========================================================================
; ShutDown
; --------------------------------------------------------------------------
; Shuts down the engine.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
ShutDown:
	im 1
	; Deallocate memory used for look-up tables.
	ld hl,End
	ld de,(AllocatedTableMemory)
	.bcall _DelMem
	ret

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
	ld (hl),65
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
	ld hl,(Level.Vertices)
	ld de,(Level.TransformedVertices)
	ld bc,(Level.Vertices.Count)
	call Vertices.Transform

SkipTransformVertices:

; --------------------------------------------------------------------------
; Mark all walls as not drawn this frame.
; --------------------------------------------------------------------------

	ld hl,(Level.Walls)
	ld de,7 ; size of a wall.
	ld bc,(Level.Walls.Count)
	ld a,b
	ld b,c
	ld c,a
	inc c
-:	res Wall.DrawFlag.DrawnThisFrame,(hl)
	add hl,de
	djnz -
	dec c
	jr nz,-

; --------------------------------------------------------------------------
; Reset the thing subsector stack.
; --------------------------------------------------------------------------

	ld hl,(Things.SubSectorStack.Top)
	ld (Things.SubSectorStack.Current),hl
	
	ld a,(Things.SubSectorStack.MaximumCapacity)
	ld (Things.SubSectorStack.EntriesFree),a

; --------------------------------------------------------------------------
; Walk the BSP tree to render the level.
; --------------------------------------------------------------------------

	ld (Render.Finish+1),sp

	ld ix,(Level.Tree)
	ld hl,(Camera.X)
	ld de,(Camera.Y)
	ld bc,Render.RenderTreeNodeFunction
	call Tree.Walk

Render.Finish:
	ld sp,0

; --------------------------------------------------------------------------
; Draw all things.
; --------------------------------------------------------------------------

	bit RenderFlag.DrawThings,(iy+RenderFlags)
	call nz,Things.Draw
	
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


ColumnsToDraw:
	.db 0

.endmodule