.module Line

LineFlags = asm_Flag1
LineFlag.Steep = 7

Error: .db 0

ClipPixel: .dw 0
Delta.X: .db 0

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
	jp Draw.Steep
	
NotSteep:
	res LineFlag.Steep,(iy+LineFlags)
	jp Draw.Shallow
	

; --------------------------------------------------------------------------
; Draw a shallow line (|dX| > |dY|)
; --------------------------------------------------------------------------
Draw.Shallow:
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
	ld (Shallow.Delta.X),a
	
	; error = dx / 2
	srl a
	ld (Error),a
	
	; dy = |y1 - y0|
	; ystep = sgn(y1 - y0)
	ld a,e
	sub c
	jp m,Shallow.Delta.Y.Negative

Shallow.Delta.Y.Positive:
	ld (Shallow.Delta.Y),a
	
	ld a,$2C ; inc l
	ld (Shallow.YStep),a
	
	ld hl,+12
	ld (Shallow.AdvanceY.Shallow.Stride),hl

	ld a,$0F ; rrca
	ld (Shallow.AdvanceY.Steep.Shift),a
	ld a,$23 ; inc hl
	ld (Shallow.AdvanceY.Steep.Increment),a
	
	jr Shallow.Delta.Y.Set

Shallow.Delta.Y.Negative:
	neg
	ld (Shallow.Delta.Y),a
	
	ld a,$2D ; dec l
	ld (Shallow.YStep),a
	
	ld hl,-12
	ld (Shallow.AdvanceY.Shallow.Stride),hl

	ld a,$07 ; rlca
	ld (Shallow.AdvanceY.Steep.Shift),a
	ld a,$2B ; dec hl
	ld (Shallow.AdvanceY.Steep.Increment),a

Shallow.Delta.Y.Set:

	ld hl,(ClipPixel)
	ld (Shallow.ClipPixel),hl

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
	ld (Shallow.PixelBufferOffset),hl
	ld (Shallow.PixelMask),a
	
	pop bc
	pop hl
	
-:	push hl
	push bc
	
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,+
	ld a,h \ ld h,l \ ld l,a
+:

Shallow.ClipPixel = $+1
	call NoClip
	jr c,Shallow.Line.PixelClipped

Shallow.PixelBufferOffset = $+1
	ld hl,0
	
	ld a,(hl)
Shallow.PixelMask = $+1
	or 0	
	
	ld (hl),a
	
Shallow.Line.PixelClipped:

	; Advance X.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,Shallow.AdvanceX.Steep

Shallow.AdvanceX.Shallow:
	ld a,(Shallow.PixelMask)
	rrca
	ld (Shallow.PixelMask),a
	jr nc,+
	ld hl,(Shallow.PixelBufferOffset)
	inc hl
	ld (Shallow.PixelBufferOffset),hl
+:
	jr Shallow.AdvanceX.Done

Shallow.AdvanceX.Steep:
	ld hl,(Shallow.PixelBufferOffset)
	ld bc,12
	add hl,bc
	ld (Shallow.PixelBufferOffset),hl

Shallow.AdvanceX.Done:
	
	pop bc
	pop hl

Shallow.Delta.X = $+2
Shallow.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Shallow.NoAdvanceY
	
	; Advance Y.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,Shallow.AdvanceY.Steep

Shallow.AdvanceY.Shallow:
	push hl
	push bc
	ld hl,(Shallow.PixelBufferOffset)
Shallow.AdvanceY.Shallow.Stride = $+1
	ld bc,12
	add hl,bc
	ld (Shallow.PixelBufferOffset),hl
	pop bc
	pop hl
	jr Shallow.AdvanceY.Done

Shallow.AdvanceY.Steep:
	push af
	ld a,(Shallow.PixelMask)
Shallow.AdvanceY.Steep.Shift:
	rrca
	ld (Shallow.PixelMask),a
	jr nc,+
	push hl
	ld hl,(Shallow.PixelBufferOffset)
Shallow.AdvanceY.Steep.Increment:
	inc hl
	ld (Shallow.PixelBufferOffset),hl
	pop hl
+:
	pop af

Shallow.AdvanceY.Done:
	
Shallow.YStep:
	inc l
	add a,d	
	
Shallow.NoAdvanceY:
	ld c,a

	inc h ; ++x
	djnz -
	ret

; --------------------------------------------------------------------------
; Draw a steep line (|dX| > |dY|)
; --------------------------------------------------------------------------
Draw.Steep:
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
	ld (Steep.Delta.X),a
	
	; error = dx / 2
	srl a
	ld (Error),a
	
	; dy = |y1 - y0|
	; ystep = sgn(y1 - y0)
	ld a,e
	sub c
	jp m,Steep.Delta.Y.Negative

Steep.Delta.Y.Positive:
	ld (Steep.Delta.Y),a
	
	ld a,$2C ; inc l
	ld (Steep.YStep),a
	
	ld hl,+12
	ld (Steep.AdvanceY.Shallow.Stride),hl

	ld a,$0F ; rrca
	ld (Steep.AdvanceY.Steep.Shift),a
	ld a,$23 ; inc hl
	ld (Steep.AdvanceY.Steep.Increment),a
	
	jr Steep.Delta.Y.Set

Steep.Delta.Y.Negative:
	neg
	ld (Steep.Delta.Y),a
	
	ld a,$2D ; dec l
	ld (Steep.YStep),a
	
	ld hl,-12
	ld (Steep.AdvanceY.Shallow.Stride),hl

	ld a,$07 ; rlca
	ld (Steep.AdvanceY.Steep.Shift),a
	ld a,$2B ; dec hl
	ld (Steep.AdvanceY.Steep.Increment),a

Steep.Delta.Y.Set:

	ld hl,(ClipPixel)
	ld (Steep.ClipPixel),hl

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
	ld (Steep.PixelBufferOffset),hl
	ld (Steep.PixelMask),a
	
	pop bc
	pop hl
	
-:	push hl
	push bc
	
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,+
	ld a,h \ ld h,l \ ld l,a
+:

Steep.ClipPixel = $+1
	call NoClip
	jr c,Steep.Line.PixelClipped

Steep.PixelBufferOffset = $+1
	ld hl,0
	
	ld a,(hl)
Steep.PixelMask = $+1
	or 0	
	
	ld (hl),a

Steep.Line.PixelClipped:

	; Advance X.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,Steep.AdvanceX.Steep

Steep.AdvanceX.Shallow:
	ld a,(Steep.PixelMask)
	rrca
	ld (Steep.PixelMask),a
	jr nc,+
	ld hl,(Steep.PixelBufferOffset)
	inc hl
	ld (Steep.PixelBufferOffset),hl
+:
	jr Steep.AdvanceX.Done

Steep.AdvanceX.Steep:
	ld hl,(Steep.PixelBufferOffset)
	ld bc,12
	add hl,bc
	ld (Steep.PixelBufferOffset),hl

Steep.AdvanceX.Done:
	
	pop bc
	pop hl

Steep.Delta.X = $+2
Steep.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Steep.NoAdvanceY
	
	; Advance Y.
	bit LineFlag.Steep,(iy+LineFlags)
	jr nz,Steep.AdvanceY.Steep

Steep.AdvanceY.Shallow:
	push hl
	push bc
	ld hl,(Steep.PixelBufferOffset)
Steep.AdvanceY.Shallow.Stride = $+1
	ld bc,12
	add hl,bc
	ld (Steep.PixelBufferOffset),hl
	pop bc
	pop hl
	jr Steep.AdvanceY.Done

Steep.AdvanceY.Steep:
	push af
	ld a,(Steep.PixelMask)
Steep.AdvanceY.Steep.Shift:
	rrca
	ld (Steep.PixelMask),a
	jr nc,+
	push hl
	ld hl,(Steep.PixelBufferOffset)
Steep.AdvanceY.Steep.Increment:
	inc hl
	ld (Steep.PixelBufferOffset),hl
	pop hl
+:
	pop af

Steep.AdvanceY.Done:
	
Steep.YStep:
	inc l
	add a,d	
	
Steep.NoAdvanceY:
	ld c,a

	inc h ; ++x
	djnz -
	ret

; ==========================================================================
; Line.NoClip
; --------------------------------------------------------------------------
; Dummy clipping routine that never clips the line.
; ==========================================================================
NoClip
	or a
	ret

.endmodule