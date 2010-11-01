.module Level

StructurePointers:

Vertices:
	.dw 0
Vertices.Count:
	.dw 0

Walls:
	.dw 0
Walls.Count:
	.dw 0

SubSectors:
	.dw 0

Sectors:
	.dw 0

Tree:
	.dw 0

StructurePointers.Size = $ - StructurePointers

DynamicMemory:
	.dw 0

TransformedVertices:
	.dw 0

AllocatedMemory:
	.dw 0

; ==========================================================================
; Load
; --------------------------------------------------------------------------
; Loads a level from a pointer in HL.
; --------------------------------------------------------------------------
; Inputs:    HL: Pointer to the level to load.
; Outputs:   Carry: Set if there was an error (insufficient memory), cleared
;                if the level was loaded.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Load:

; --------------------------------------------------------------------------
; Copy pointers from the level file.
; --------------------------------------------------------------------------

	ld de,StructurePointers
	ld bc,StructurePointers.Size
	ldir
	ld de,CameraVariables
	ld bc,CameraVariables.Size
	ldir

; --------------------------------------------------------------------------
; Invert the camera angle to ensure that the first frame is transformed.
; --------------------------------------------------------------------------	

	ld a,(Camera.Angle)
	cpl
	ld (Previous.Camera.Angle),a
	
; --------------------------------------------------------------------------
; How much dynamic memory will we need for the level?
; --------------------------------------------------------------------------
	
	ld hl,(Vertices.Count)
	add hl,hl
	add hl,hl
	ld (AllocatedMemory),hl

; --------------------------------------------------------------------------
; Do we have enough memory?
; --------------------------------------------------------------------------

	.bcall _EnoughMem
	ret c

; --------------------------------------------------------------------------
; Allocate the memory.
; --------------------------------------------------------------------------

	ex de,hl
	ld de,(DynamicMemory)
	ld (TransformedVertices),de
	
	.bcall _InsertMem

; --------------------------------------------------------------------------
; Return successfully.
; --------------------------------------------------------------------------

	or a
	ret

; ==========================================================================
; Unload
; --------------------------------------------------------------------------
; Unloads the current level.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Unload:
	ld hl,(DynamicMemory)
	ld de,(AllocatedMemory)
	.bcall _DelMem
	ret

.endmodule