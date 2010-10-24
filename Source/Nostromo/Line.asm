.module Line

LineFlags = asm_Flag1
LineFlag.Steep = 7

; ==========================================================================
; Line.Draw
; --------------------------------------------------------------------------
; Draws a line.
; --------------------------------------------------------------------------
; Inputs:    (B,C) One end of the line.
;            (D,E) The other end of the line.
; Destroyed: AF, BC, DE, HL, asm_Flag1.7.
; ==========================================================================
Draw:

	; Calculate absolute deltas.

	ld a,b
	sub d
	jp p,+
	neg
+:	ld (Delta.X),a

	ld a,c
	sub e
	jp p,+
	neg
+:	
	ld l,a
	ld a,(Delta.X)
	sub l
	jr nc,NotSteep
	
Steep:
	set LineFlag.Steep,(iy+LineFlags)
	ld a,b \ ld b,c \ ld c,a
	ld a,d \ ld d,e \ ld e,a
	jr SetSteepness
	
NotSteep:
	res LineFlag.Steep,(iy+LineFlags)

SetSteepness:

	; dx = x1 - x0
	ld a,d
	sub b
	jp p,+

	neg
	ld l,a
	ld a,b \ ld b,d \ ld d,a
	ld a,c \ ld c,e \ ld e,a
	ld a,l
+:
	ld (Delta.X),a
	
	; error = dx / 2
	srl a
	ld (Error),a
	
	; dy = |y1 - y0|
	; ystep = sgn(y1 - y0)
	ld l,$2C ; inc l
	ld a,e
	sub c
	jp p,+
	neg
	inc l ; dec l
+:	ld (Delta.Y),a

	ld a,l
	ld (YStep),a

	; a = (x1 - x0) + 1 (number of steps).
	ld a,d
	sub b
	inc a
	
	ld h,b ; Start X.
	ld l,c ; Start Y.
	
	ld b,a ; b = number of steps.
	
-:	push hl
	push bc
	
	bit LineFlag.Steep,(iy+LineFlags)
	jr z,+
	ld a,h \ ld h,l \ ld l,a	
+:

	; Plot HL
	ld a,h
	ld e,l
	call ionGetPixel
	or (hl)
	ld (hl),a
	
	pop bc
	pop hl

Delta.X = $+2
Delta.Y = $+1
	ld de,0

Error = $+1
	ld a,0

	sub e
	jp p,+
YStep:
	inc l
	add a,d
+:	ld (Error),a

	inc h ; ++x
	djnz -

	ret

.endmodule