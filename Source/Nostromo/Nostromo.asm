.module Nostromo

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
#include "Sprite.asm"
#include "Physics.asm"
#include "BlockMap.asm"
#include "Key.asm"

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
; The clipping table is 512 bytes long.
; The trig table is 512 bytes long.
; The transformed vertices table is 256 bytes long.
; --------------------------------------------------------------------------
	
	ld hl,512+512+256
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
; Unpack the trig table.
; --------------------------------------------------------------------------

	ld ix,Maths.Trig.Table
	ld de,Maths.Trig.PackedTable
	
	ld b,64
	
	ld hl,0
	
-:	ld a,(de)
	push de
	
	sra a
	sra a
	sra a
	sra a
	
	ld e,a
	
	add a,a
	sbc a,a
	ld d,a
	add hl,de
	pop de
	
	call WriteUnpackedAngle
	
	ld a,(de)
	push de
	
	ld d,0
	
	and $0F
	cp 8
	jr c,+
	or $F0
	dec d

+:	ld e,a

	add hl,de
	pop de
	
	call WriteUnpackedAngle

	inc de
	
	djnz -

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
; WriteUnpackedAngle
; --------------------------------------------------------------------------
; Internal routine used to write an unpacked angle to the trig table.
; ==========================================================================
WriteUnpackedAngle:

; --------------------------------------------------------------------------
; Store the angle in place.
; --------------------------------------------------------------------------

	ld (ix+0),l
	ld (ix+1),h

; --------------------------------------------------------------------------
; The sine wave between 0..pi is inverted and repeated between pi..2pi.
; --------------------------------------------------------------------------

	push hl
	neg_hl()
	inc ixh
	ld (ix+0),l
	ld (ix+1),h
	dec ixh
	pop hl

; --------------------------------------------------------------------------
; Advance to the next slot in the table.
; --------------------------------------------------------------------------

	inc ix
	inc ix
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
	
	ld a,(Camera.Angle)
	neg
	ld (Render.Camera.Angle),a

; --------------------------------------------------------------------------
; Prepare for vertex transformations.
; --------------------------------------------------------------------------

	call Vertices.Transform.Begin

; --------------------------------------------------------------------------
; Transform the vertices.
; --------------------------------------------------------------------------
	
	ld a,(Previous.Camera.Angle)
	ld b,a
	ld a,(Camera.Angle)
	cp b
	jr z,+
	ld (Previous.Camera.Angle),a
	jr MarkVerticesUntransformed
+:	ld hl,(Previous.Camera.X)
	ld de,(Camera.X)
	or a
	sbc hl,de
	jr z,+
	ld (Previous.Camera.X),de
	jr MarkVerticesUntransformed
+:	ld hl,(Previous.Camera.Y)
	ld de,(Camera.Y)
	or a
	sbc hl,de
	jr z,SkipTransformVertices
	ld (Previous.Camera.Y),de
	
; --------------------------------------------------------------------------
; Mark all vertices as not transformed this frame.
; --------------------------------------------------------------------------

MarkVerticesUntransformed:

	ld a,(Level.Vertices.Count)
	ld b,a
	ld hl,Vertices.AlreadyTransformed
	xor a
-:	ld (hl),a
	inc l
	djnz -

SkipTransformVertices:

; --------------------------------------------------------------------------
; Mark all walls as not drawn this frame.
; --------------------------------------------------------------------------

	ld hl,(Level.Walls)
	ld de,Wall.DataSize ; size of a wall.
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

.endmodule