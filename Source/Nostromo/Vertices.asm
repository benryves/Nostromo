.module Vertices

; ===============================================================
; Transform: transform/rotate the vertices around the camera
; hl -> original vertices (each vertex is <short>,<short>)
; de -> output vertices
; bc =  number of vertices to transform
; ===============================================================

SinA: .dw 0
CosA: .dw 0

Transform

	; Temp variables
	
		
	ld (VertexCount),bc
	ld (WriteDataPointer),de
	ld (ReadDataPointer),hl
	
	; Get sin(a)
			
	ld a,(Parent.Camera.Angle)
	call Parent.Maths.Trig.Sin
	ld (SinA),bc

	; Get cos(a)

	ld a,(Parent.Camera.Angle)
	call Parent.Maths.Trig.Cos
	ld (CosA),bc
	
	ld hl,(Parent.Camera.X) \ neg_hl() \ ld (NegCameraX),hl
	ld hl,(Parent.Camera.Y) \ neg_hl() \ ld (NegCameraY),hl
	
TransformLoop


ReadDataPointer = $+1
	ld hl,0
	ld e,(hl)
	inc hl
	ld d,(hl)
	inc hl
	ld c,(hl)
	inc hl
	ld b,(hl)
	inc hl	
	ld (ReadDataPointer),hl

NegCameraX = $+1
	ld hl,$0000
	add hl,de
	ld (OriginalX),hl

NegCameraY = $+1
	ld hl,$0000
	add hl,bc
	ld (OriginalY),hl

	ld de,(SinA)
	OriginalX = $+1 \ ld bc,0
	call Parent.Maths.Mul.S16S16
		
	ld l,h \ ld h,e ; (>>= 8)

	push hl ; Ox*sin(a)	
		ld de,(OriginalY)
		ld bc,(CosA)
		call Parent.Maths.Mul.S16S16
		ld l,h \ ld h,e ; (>>= 8)
	pop de
	add hl,de
	
	push hl

		OriginalY = $+1 \ ld de,0
		ld bc,(SinA)
		call Parent.Maths.Mul.S16S16
		ld l,h \ ld h,e
		push hl
			ld de,(OriginalX)
			ld bc,(CosA)
			call Parent.Maths.Mul.S16S16
			ld l,h \ ld h,e
		pop de
		or a
		sbc hl,de
		ld d,h
		ld e,l
			
	pop bc
	
WriteDataPointer = $+1
	ld hl,0
	ld (hl),c
	inc hl
	ld (hl),b
	inc hl
	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
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