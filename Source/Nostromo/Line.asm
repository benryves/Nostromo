.module Line

LineFlags = asm_Flag1
LineFlag.TopDown = 7

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
	call Pixel.GetInformation
	ld (Shallow.PixelBufferOffset),hl
	ld (Shallow.PixelMask),a
	
	pop bc
	pop hl

	; Swap H and L.
	ld a,h \ ld h,l \ ld l,a
	
-:		
Shallow.ClipPixel = $+1
	call NoClip
	jr c,Shallow.Line.PixelClipped

Shallow.PixelBufferOffset = $+1
	ld de,0
	
	ld a,(de)
Shallow.PixelMask = $+1
	or 0	
	
	ld (de),a
	
Shallow.Line.PixelClipped:

	; Advance X.
	ld a,(Shallow.PixelMask)
	rrca
	ld (Shallow.PixelMask),a
	jr nc,+
	ld de,(Shallow.PixelBufferOffset)
	inc de
	ld (Shallow.PixelBufferOffset),de
+:

Shallow.Delta.X = $+2
Shallow.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Shallow.NoAdvanceY

	add a,d
	
	; Advance Y.
	push hl
	ld hl,(Shallow.PixelBufferOffset)
Shallow.AdvanceY.Shallow.Stride = $+1
	ld de,12
	add hl,de
	ld (Shallow.PixelBufferOffset),hl
	pop hl
	
Shallow.YStep:
	inc h
	
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
	jp m,Draw.Steep.NegativeDX

Draw.Steep.PositiveDX:

Draw.Steep.PositiveDX.TopDown:

	ld (Steep.Delta.X),a
	
	bit LineFlag.TopDown,(iy+LineFlags)
	jr nz,Draw.Steep.Draw.TopDown
	
	ld a,b \ ld b,d \ ld d,a
	ld a,c \ ld c,e \ ld e,a
	
	jr Draw.Steep.Draw.BottomUp

Draw.Steep.NegativeDX:

	neg
	ld (Steep.Delta.X),a
	
	bit LineFlag.TopDown,(iy+LineFlags)
	jr z,Draw.Steep.Draw.BottomUp
	
	ld a,b \ ld b,d \ ld d,a
	ld a,c \ ld c,e \ ld e,a
	
	jr Draw.Steep.Draw.TopDown


Draw.Steep.Draw.TopDown:

	ld hl,+12
	ld (Steep.AdvanceX.Stride),hl
	ld a,$24 ; inc h
	ld (Steep.AdvanceY.AdvanceY),a
	ld (Steep.NoAdvanceY.AdvanceY),a
	
	jr Draw.Steep.Draw

Draw.Steep.Draw.BottomUp:
	
	ld hl,-12
	ld (Steep.AdvanceX.Stride),hl
	ld a,$25 ; dec h
	ld (Steep.AdvanceY.AdvanceY),a
	ld (Steep.NoAdvanceY.AdvanceY),a
	
Draw.Steep.Draw:

	; error = dx / 2
	ld a,(Steep.Delta.X)
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
	ld a,$13 ; inc de
	ld (Steep.AdvanceY.Steep.Increment),a
	
	jr Steep.Delta.Y.Set

Steep.Delta.Y.Negative:
	neg
	ld (Steep.Delta.Y),a
	
	ld a,$2D ; dec l
	ld (Steep.YStep),a

	ld a,$07 ; rlca
	ld (Steep.AdvanceY.Steep.Shift),a
	ld a,$1B ; dec de
	ld (Steep.AdvanceY.Steep.Increment),a

Steep.Delta.Y.Set:

	ld hl,(ClipPixel)
	ld (Steep.ClipPixel),hl

	; a = (x1 - x0) + 1 (number of steps).
	ld a,(Steep.Delta.X)
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
	call Pixel.GetInformation
	ld (Steep.PixelBufferOffset),hl
	ld (Steep.PixelMask),a
	
	pop bc
	pop hl
	
-:	push hl
	
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
Steep.AdvanceX.Stride = $+1
	ld de,12
	add hl,de
	ld (Steep.PixelBufferOffset),hl

	pop hl

Steep.Delta.X = $+2
Steep.Delta.Y = $+1
	ld de,0
	ld a,c
	sub e
	jp p,Steep.NoAdvanceY
	
	; Advance Y.
	
	add a,d
	ld c,a
	
	ld a,(Steep.PixelMask)
Steep.AdvanceY.Steep.Shift:
	rrca
	ld (Steep.PixelMask),a
	jr nc,+
	ld de,(Steep.PixelBufferOffset)
Steep.AdvanceY.Steep.Increment:
	inc de
	ld (Steep.PixelBufferOffset),de
+:

Steep.YStep:
	inc l

Steep.AdvanceY.AdvanceY:
	inc h ; ++x
	djnz -
	ret
	
Steep.NoAdvanceY:
	ld c,a

Steep.NoAdvanceY.AdvanceY:
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