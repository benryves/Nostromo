.module Vertices

; ==========================================================================
; Transform.Begin
; --------------------------------------------------------------------------
; Initialises the vertex transformation code. Should be called at the start
; of the frame.
; --------------------------------------------------------------------------
; Destroyed: AF, BC, HL.
; ==========================================================================
Transform.Begin:
	
	; Get sin(a)
	ld a,(Camera.Angle)
	call Maths.Trig.Sin
	ld (SinA),bc

	; Get cos(a)
	ld a,(Camera.Angle)
	call Maths.Trig.Cos
	ld (CosA),bc
	
	; Negate the camera position.
	ld hl,(Camera.X) \ neg_hl() \ ld (NegCameraX),hl
	ld hl,(Camera.Y) \ neg_hl() \ ld (NegCameraY),hl
	
	ret

; ==========================================================================
; Transform
; --------------------------------------------------------------------------
; Transforms a single vertex.
; --------------------------------------------------------------------------
; Inputs:    (BC,DE) Vertex to transform.
; Outputs:   (BC,DE) Transformed vertex.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
Transform:

NegCameraX = $+1
	ld hl,$0000
	add hl,bc
	ld (OriginalX),hl

NegCameraY = $+1
	ld hl,$0000
	add hl,de
	ld (OriginalY),hl

	ld de,(SinA)
	OriginalY = $+1 \ ld bc,0
	call Maths.Mul.S16S16
		
	ld l,h \ ld h,e ; (>>= 8)

	push hl ; Oy*sin(a)	
		ld de,(OriginalX)
		ld bc,(CosA)
		call Maths.Mul.S16S16
		; (>>= 8)
		ld d,e
		ld e,h
	pop hl
	
	or a
	sbc hl,de
	
	push hl
		OriginalX = $+1 \ ld de,0
		ld bc,(SinA)
		call Maths.Mul.S16S16
		ld l,h \ ld h,e
		push hl
			ld de,(OriginalY)
			ld bc,(CosA)
			call Maths.Mul.S16S16
			ld l,h \ ld h,e
		pop de
		add hl,de
		ex de,hl	
	pop bc
	
	ret

; ==========================================================================
; Transform.Multiple
; --------------------------------------------------------------------------
; Transforms multiple vertices.
; --------------------------------------------------------------------------
; Inputs:    HL: Pointer to start of vertices to transform.
;            DE: Pointer to place to store transformed vertices.
;            BC: Number of vertices to transform.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
Transform.Multiple:

	; Temp variables
	
	ld (VertexCount),bc
	ld (WriteDataPointer),de
	ld (ReadDataPointer),hl
	
TransformLoop:

ReadDataPointer = $+1
	ld hl,0
	
	ld c,(hl) \ inc hl
	ld b,(hl) \ inc hl
	ld e,(hl) \ inc hl
	ld d,(hl) \ inc hl	
	ld (ReadDataPointer),hl
	
	call Transform
	
WriteDataPointer = $+1
	ld hl,0
	ld (hl),c \ inc hl
	ld (hl),b \ inc hl
	ld (hl),e \ inc hl
	ld (hl),d \ inc hl
	ld (WriteDataPointer),hl
	
VertexCount = $+1
	ld hl,0
	dec hl
	ld (VertexCount),hl
	
	ld a,h
	or l
	jp nz,TransformLoop	
	ret

.endmodule