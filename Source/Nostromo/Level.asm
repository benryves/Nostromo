.module Level
Code:

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
	
	ld a,5
	ld (Things.SubSectorStack.MaximumCapacity),a
	
	ld hl,(Vertices.Count)
	add hl,hl
	add hl,hl
	ld de,Things.SubSectorStack.EntrySize*5 ; Five thing stack entries.
	add hl,de
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
; Update variables pointing to memory as appropriate.
; --------------------------------------------------------------------------

	ld hl,(DynamicMemory)
	ld de,(AllocatedMemory)
	add hl,de
	ld (Things.SubSectorStack.Top),hl

; --------------------------------------------------------------------------
; Initialise the block map.
; --------------------------------------------------------------------------

	call BlockMap.IntialiseFromLevel

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

.if Options.ReportModuleSizes \ .echoln strformat("Level module: {0:N0} bytes.", $-Code) \ .endif
.endmodule