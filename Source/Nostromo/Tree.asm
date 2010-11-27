.module Tree
Code:

; ==========================================================================
; Node offsets
; --------------------------------------------------------------------------
; These refer to offsets to fields within a tree node.
; ==========================================================================
Node.BoundingCircle.X = 0
Node.BoundingCircle.Y = 2
Node.BoundingCircle.Radius = 4
Node.Type = 6
Node.Leaf = 7
Node.FrontNode = 7
Node.BackNode = 9
Node.PartitionPosition = 11
Node.PartitionGradient = 13

; ==========================================================================
; Node types
; ==========================================================================
Node.Type.Leaf = 0
Node.Type.VerticalPartition = 1
Node.Type.HorizontalPartition = 2
Node.Type.ShallowSlopePartition = 3
Node.Type.SteepSlopePartition = 4

; ==========================================================================
; Walk
; --------------------------------------------------------------------------
; Walks the BSP tree.
; --------------------------------------------------------------------------
; Inputs:    IX: Pointer to the BSP tree node.
;            HL: X coordinate of the thing walking the tree.
;            DE: Y coordinate of the thing walking the tree.
;            BC: Pointer to the function to execute when a leaf is reached.
; Destroyed: AF, BC, DE, HL, IX.
; ==========================================================================
Walk:

	ld (Walker.X),hl
	ld (Walker.Y),de
	ld (Walker.Leaf.Function),bc

Walk.SkipInit:

	.if Options.KeepStatistics
	ld hl,(Statistics.TreeNodesVisited)
	inc hl
	ld (Statistics.TreeNodesVisited),hl
	.endif

Walker.Cull.Function = $+1
	call 0

	ret c

; --------------------------------------------------------------------------
; What type is the node?
; --------------------------------------------------------------------------

	ld a,(ix+Node.Type)
	or a
	jr nz,Walk.Partition

; --------------------------------------------------------------------------
; We've encountered a leaf, so operate some function on it.
; --------------------------------------------------------------------------
Walk.Leaf:

Walker.Leaf.Function = $+1
	jp 0

; --------------------------------------------------------------------------
; We've encountered a partition, so work out which side we're on.
; --------------------------------------------------------------------------
Walk.Partition:
	dec a
	jr z,Walk.VerticalPartition
	dec a
	jr z,Walk.HorizontalPartition
	dec a
	jr z,Walk.ShallowSlopePartition
	dec a
	jr z,Walk.SteepSlopePartition

; --------------------------------------------------------------------------
; We've encountered a vertical partition (dX = 0).
; --------------------------------------------------------------------------
Walk.VerticalPartition:
	ld l,(ix+Node.PartitionPosition+0)
	ld h,(ix+Node.PartitionPosition+1)
	ld de,(Walker.X)
	jr Walk.CheckPartitionSide

; --------------------------------------------------------------------------
; We've encountered a horizontal partition (dY = 0).
; --------------------------------------------------------------------------
Walk.HorizontalPartition:
	ld l,(ix+Node.PartitionPosition+0)
	ld h,(ix+Node.PartitionPosition+1)
	ld de,(Walker.Y)
	jr Walk.CheckPartitionSide

; --------------------------------------------------------------------------
; We've encountered a shallow slope (|dY| < |dX|).
; --------------------------------------------------------------------------
Walk.ShallowSlopePartition:
	ld e,(ix+Node.PartitionGradient+0)
	ld d,(ix+Node.PartitionGradient+1)
	ld bc,(Walker.X)
	call Maths.Mul.S16S16
	ld d,e
	ld e,h
	ld l,(ix+Node.PartitionPosition+0)
	ld h,(ix+Node.PartitionPosition+1)
	add hl,de
	ld de,(Walker.Y)
	jr Walk.CheckPartitionSide

; --------------------------------------------------------------------------
; We've encountered a steep slope (|dY| > |dX|).
; --------------------------------------------------------------------------
Walk.SteepSlopePartition:
	ld e,(ix+Node.PartitionGradient+0)
	ld d,(ix+Node.PartitionGradient+1)
	ld bc,(Walker.Y)
	call Maths.Mul.S16S16
	ld d,e
	ld e,h
	ld l,(ix+Node.PartitionPosition+0)
	ld h,(ix+Node.PartitionPosition+1)
	add hl,de
	ld de,(Walker.X)
	jr Walk.CheckPartitionSide

; --------------------------------------------------------------------------
; Which side of the partition are we really on?
; --------------------------------------------------------------------------
Walk.CheckPartitionSide:
	ld a,h \ xor $80 \ ld h,a
	ld a,d \ xor $80 \ ld d,a
	or a
	sbc hl,de
	jr nc,Walk.BehindPartition

; --------------------------------------------------------------------------
; We're in front of the partition, so walk the node in front first.
; --------------------------------------------------------------------------
Walk.InFrontOfPartition:
	
	push ix
	ld l,(ix+Node.FrontNode+0)
	ld h,(ix+Node.FrontNode+1)
	push hl
	pop ix
	call Walk.SkipInit
	
	; Walk the node behind the partition afterwards.
	
	pop ix
	ld l,(ix+Node.BackNode+0)
	ld h,(ix+Node.BackNode+1)
	push hl
	pop ix
	jp Walk.SkipInit

; --------------------------------------------------------------------------
; We're behind the partition, so walk the node behind it first.
; --------------------------------------------------------------------------
Walk.BehindPartition:
	
	push ix
	ld l,(ix+Node.BackNode+0)
	ld h,(ix+Node.BackNode+1)
	push hl
	pop ix
	call Walk.SkipInit
	
	; Walk the node in front of the partition afterwards.
	
	pop ix
	ld l,(ix+Node.FrontNode+0)
	ld h,(ix+Node.FrontNode+1)
	push hl
	pop ix
	jp Walk.SkipInit

; ==========================================================================
; Walk.DoNotCull
; --------------------------------------------------------------------------
; Stub function to load into Walker.Cull.Function to never cull nodes.
; ==========================================================================
Walk.DoNotCull:
	or a
	ret

.if Options.ReportModuleSizes \ .echoln strformat("Tree module: {0:N0} bytes.", $-Code) \ .endif
.endmodule