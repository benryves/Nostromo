.module Tree

; ==========================================================================
; Walk
; --------------------------------------------------------------------------
; Walks the BSP tree and renders it on the display.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the BSP tree node.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Walk:
	ld a,(ix+0)
	or a
	jr nz,Walk.Partition

Walk.Leaf:
	; We've encountered a leaf.
	ld l,(ix+1)
	ld h,(ix+2)
	push hl
	pop ix
	jp Nostromo.Sector.Draw

Walk.Partition:
	; Load the partition position.
	ld l,(ix+1)
	ld h,(ix+2)
	
	; Is it a horizontal or vertical partition?
	dec a
	jr nz,Walk.HorizontalPartition

Walk.VerticalPartition:
	; We've encountered a vertical partition.
	ld de,(Nostromo.Camera.X)
	jr Walk.CheckPartitionSide
	
Walk.HorizontalPartition:
	; We've encountered a horizontal partition.
	ld de,(Nostromo.Camera.Y)

Walk.CheckPartitionSide:
	; Which side of the partition are we on?
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	jr nc,Walk.BehindPartition

Walk.InFrontOfPartition:
	push ix
	ld l,(ix+5)
	ld h,(ix+6)
	push hl
	pop ix
	call Walk
	pop ix
	ld l,(ix+3)
	ld h,(ix+4)
	push hl
	pop ix
	jp Walk

Walk.BehindPartition:
	push ix
	ld l,(ix+3)
	ld h,(ix+4)
	push hl
	pop ix
	call Walk
	pop ix
	ld l,(ix+5)
	ld h,(ix+6)
	push hl
	pop ix
	jp Walk
	
.endmodule