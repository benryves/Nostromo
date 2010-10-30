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
	jr nc,Draw.Shallow
	
Steep:
	ld a,b \ ld b,c \ ld c,a
	ld a,d \ ld d,e \ ld e,a
	jp Draw.Steep
	
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
	
	ld a,$24 ; inc l
	ld (Shallow.YStep),a
	
	ld hl,+12
	ld (Shallow.AdvanceY.Shallow.Stride),hl

	jr Shallow.Delta.Y.Set

Shallow.Delta.Y.Negative:
	neg
	ld (Shallow.Delta.Y),a
	
	ld a,$25 ; dec h
	ld (Shallow.YStep),a
	
	ld hl,-12
	ld (Shallow.AdvanceY.Shallow.Stride),hl

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
	
	ld a,h
	ld e,l
	
	dec e
	call ionGetPixel
	ld (Shallow.PixelBufferOffset),hl
	ld (Shallow.PixelMask),a
	
	pop bc
	pop hl

	; Swap H and L.
	ld a,h \ ld h,l \ ld l,a
	
-:	push hl
	push bc
	
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
	ld a,(Shallow.PixelMask)
	rrca
	ld (Shallow.PixelMask),a
	jr nc,+
	ld hl,(Shallow.PixelBufferOffset)
	inc hl
	ld (Shallow.PixelBufferOffset),hl
+:
	
	pop bc
	pop hl

Shallow.Delta.X = $+2
Shallow.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Shallow.NoAdvanceY
	
	; Advance Y.
	push hl
	push bc
	ld hl,(Shallow.PixelBufferOffset)
Shallow.AdvanceY.Shallow.Stride = $+1
	ld bc,12
	add hl,bc
	ld (Shallow.PixelBufferOffset),hl
	pop bc
	pop hl
	
Shallow.YStep:
	inc h
	add a,d	
	
Shallow.NoAdvanceY:
	ld c,a

	inc l ; ++x
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
	
	ld a,l
	ld e,h
	
	dec e
	call ionGetPixel
	ld (Steep.PixelBufferOffset),hl
	ld (Steep.PixelMask),a
	
	pop bc
	pop hl
	
-:	push hl
	push bc
	
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
	ld hl,(Steep.PixelBufferOffset)
	ld bc,12
	add hl,bc
	ld (Steep.PixelBufferOffset),hl

	pop bc
	pop hl

Steep.Delta.X = $+2
Steep.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Steep.NoAdvanceY
	
	; Advance Y.
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