.module Line

LineFlags = asm_Flag1
LineFlag.Steep = 7

Error: .db 0

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

	inc c
	inc e

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
	ld a,e
	sub c
	jp m,Delta.Y.Negative

Delta.Y.Positive:
	ld (Delta.Y),a
	
	ld a,$2C ; inc l
	ld (YStep),a
	
	ld hl,+12
	ld (AdvanceY.Shallow.Stride),hl

	ld a,$0F ; rrca
	ld (AdvanceY.Steep.Shift),a
	ld a,$23 ; inc hl
	ld (AdvanceY.Steep.Increment),a
	
	jr Delta.Y.Set

Delta.Y.Negative:
	neg
	ld (Delta.Y),a
	
	ld a,$2D ; dec l
	ld (YStep),a
	
	ld hl,-12
	ld (AdvanceY.Shallow.Stride),hl

	ld a,$07 ; rlca
	ld (AdvanceY.Steep.Shift),a
	ld a,$2B ; dec hl
	ld (AdvanceY.Steep.Increment),a

Delta.Y.Set:

	; a = (x1 - x0) + 1 (number of steps).
	ld a,d
	sub b
	inc a
	
	ld h,b ; Start X.
	ld l,c ; Start Y.
	
	ld b,a ; b = number of steps.
	
	ld a,(Error)
	ld c,a
	
	push hl
	push bc
	
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,+
	ld a,h
	ld e,l
	jr ++
+:	ld a,l
	ld e,h
++:	
	
	dec e
	call ionGetPixel
	ld (PixelBufferOffset),hl
	ld (PixelMask),a
	
	pop bc
	pop hl
	
-:	push hl
	push bc
	
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,+
	ld a,h \ ld h,l \ ld l,a
+:

ClipPixel = $+1
	call NoClip
	jr c,Line.PixelClipped

PixelBufferOffset = $+1
	ld hl,0
	
	ld a,(hl)
PixelMask = $+1
	or 0	
	
	ld (hl),a
	
Line.PixelClipped:

	; Advance X.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,AdvanceX.Steep

AdvanceX.Shallow:
	ld a,(PixelMask)
	rrca
	ld (PixelMask),a
	jr nc,+
	ld hl,(PixelBufferOffset)
	inc hl
	ld (PixelBufferOffset),hl
+:
	jr AdvanceX.Done

AdvanceX.Steep:
	ld hl,(PixelBufferOffset)
	ld bc,12
	add hl,bc
	ld (PixelBufferOffset),hl

AdvanceX.Done:
	
	pop bc
	pop hl

Delta.X = $+2
Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,NoAdvanceY
	
	; Advance Y.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,AdvanceY.Steep

AdvanceY.Shallow:
	push hl
	push bc
	ld hl,(PixelBufferOffset)
AdvanceY.Shallow.Stride = $+1
	ld bc,12
	add hl,bc
	ld (PixelBufferOffset),hl
	pop bc
	pop hl
	jr AdvanceY.Done

AdvanceY.Steep:
	push af
	ld a,(PixelMask)
AdvanceY.Steep.Shift:
	rrca
	ld (PixelMask),a
	jr nc,+
	push hl
	ld hl,(PixelBufferOffset)
AdvanceY.Steep.Increment:
	inc hl
	ld (PixelBufferOffset),hl
	pop hl
+:
	pop af

AdvanceY.Done:	
	
YStep:
	inc l
	add a,d	
	
NoAdvanceY:
	ld c,a

	inc h ; ++x
	djnz +

	ret
+:	jp -

; ==========================================================================
; Line.NoClip
; --------------------------------------------------------------------------
; Dummy clipping routine that never clips the line.
; ==========================================================================
NoClip
	or a
	ret

.endmodule