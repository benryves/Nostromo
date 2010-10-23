.module Trapezium

Edges: .fill 128

Columns:
Start.Column: .db 0
End.Column: .db 0

Start.Ceiling: .dw 0
Start.Floor: .dw 0
End.Ceiling: .dw 0
End.Floor: .dw 0

Rows:
Row.End: .db 0
Row.Start: .db 0
Row.Count: .db 0

Trace.X: .db 0
Trace.Count: .db 0
Trace.Delta.X: .db 0
Trace.Delta.Y: .db 0

; ==========================================================================
; Fill
; --------------------------------------------------------------------------
; Fills a trapezium-shaped wall section.
; --------------------------------------------------------------------------
; Inputs:    Start.Column: Column of the wall start.
;            Start.Floor: Row of the wall start's floor.
;            Start.Ceiling: Row of the wall start's ceiling.
;            End.Column: Column of the wall end.
;            End.Floor: Row of the wall end's floor.
;            End.Ceiling: Row of the wall end's ceiling.
; Destroyed: AF, BC, DE, HL.
; ==========================================================================
Fill:
; --------------------------------------------------------------------------
; Are we currently using that colour?
; --------------------------------------------------------------------------
	and %01111000
FillCurrentColour = $+1
	cp 0
	jr z,+
	ld (FillCurrentColour),a
	ld l,a
	ld h,0
	ld de,Fills
	add hl,de
	ld de,FillPattern
	ld bc,8
	ldir
+:
	
; --------------------------------------------------------------------------
; Calculate the rows for the trapezium's bounding rectangle.
; --------------------------------------------------------------------------
	
	ld hl,(Start.Ceiling)
	call Clip16ToRow
	ld b,a
	ld c,a
	
	ld hl,(Start.Floor)
	call Clip16ToRow
	call MinMaxBC
	
	ld hl,(End.Ceiling)
	call Clip16ToRow
	call MinMaxBC

	ld hl,(End.Floor)
	call Clip16ToRow
	call MinMaxBC
	
	ld (Rows),bc
	ld a,c
	sub b
	inc a
	ld (Row.Count),a	

; --------------------------------------------------------------------------
; Fill the edge table with the column extents.
; --------------------------------------------------------------------------

	
	ld l,b
	ld h,0
	add hl,hl
	ld de,Edges	
	add hl,de	
	
	ld b,a

	ld de,(Columns)
-:	ld (hl),e
	inc hl
	ld (hl),d
	inc hl
	djnz -

; --------------------------------------------------------------------------
; Trace the top edge of the wall.
; --------------------------------------------------------------------------

	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.Start.Ceiling)
	ld (Clip.g_line16Y1),hl

	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.End.Ceiling)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	
	jr c,CulledCeilingEdge

	ld h,b
	ld l,c
	
	ld bc,Edges+1
	ld a,e
	sub l
	jr z,CulledCeilingEdge
	jr nc,+
	neg
	ex de,hl
+:	ld (Trace.Count),a

	ld a,h
	ld (Trace.X),a
	
	ld a,d
	sub h
	ld h,$0C ; INC C
	jp p,+
	neg
	dec bc
	inc h ; DEC C
+:	ld (Trace.Delta.X),a

	ld a,h
	ld (CeilingTraceDirection),a

	ld h,0
	add hl,hl
	add hl,bc
	
	ld a,(Trace.Count)
	ld b,a
	
	ld a,(Trace.X)
	ld c,a
	
	ld a,(Trace.Delta.X)
	ld d,a
	ld e,b
	
	srl a

CeilingTraceLoop:

	ld (hl),c
	inc hl
	inc hl
	
	sub d
	jp p,+
	
-:	
CeilingTraceDirection:
	dec c
	add a,e
	jp m,-
	
+:

	djnz CeilingTraceLoop	

CulledCeilingEdge:

; --------------------------------------------------------------------------
; Trace the bottom edge of the wall.
; --------------------------------------------------------------------------

	ld a,(Trapezium.Start.Column)
	ld (Clip.g_line16X1),a
	
	ld hl,(Trapezium.Start.Floor)
	ld (Clip.g_line16Y1),hl

	ld a,(Trapezium.End.Column)
	ld (Clip.g_line16X2),a
	
	ld hl,(Trapezium.End.Floor)
	ld (Clip.g_line16Y2),hl
	
	call Clip.Clip2DLine16Ex
	
	jr c,CulledFloorEdge

	ld h,b
	ld l,c
	
	ld bc,Edges+1
	ld a,l
	sub e
	jr z,CulledFloorEdge
	jr nc,+
	neg
	ex de,hl
+:	ld (Trace.Count),a

	ld a,h
	ld (Trace.X),a
	
	ld a,d
	sub h
	ld h,$0C ; INC C
	jp p,+
	neg
	dec bc
	inc h ; DEC C
+:	ld (Trace.Delta.X),a

	ld a,h
	ld (FloorTraceDirection),a

	ld h,0
	add hl,hl
	add hl,bc
	
	ld a,(Trace.Count)
	ld b,a
	
	ld a,(Trace.X)
	ld c,a
	
	ld a,(Trace.Delta.X)
	ld d,a
	ld e,b
	
	srl a

FloorTraceLoop:

	ld (hl),c
	dec hl
	dec hl
	
	sub d
	jp p,+
	
-:	
FloorTraceDirection:
	dec c
	add a,e
	jp m,-
	
+:

	djnz FloorTraceLoop	

CulledFloorEdge:

; --------------------------------------------------------------------------
; Calculate where to start and how many rows to draw.
; --------------------------------------------------------------------------

	ld a,(Row.Count)
	ld b,a
	ld a,(Row.Start)
	
	ld l,a
	ld h,0
	add hl,hl
	ld de,Edges
	add hl,de
	
	dec a
	ld (FillRow),a

; --------------------------------------------------------------------------
; Draw the trapezium row by row.
; --------------------------------------------------------------------------

-:	push bc
	
	ld b,(hl)
	inc hl
	ld c,(hl)
	inc hl

	push hl

FillRow = $+1
	ld a,0
	inc a
	ld (FillRow),a
	
	call HLineFill
	
	pop hl
	pop bc
	djnz -
	
	ret


;-------------------------------------------------------------------------------
;
; === HLineFill ===
;
;  Fills a horizontal line segment with the specified pattern.
;
; INPUTS:
;
;  REGISTERS
;  * A -  Y co-ordinate
;  * B  - Line x1 (pixel offset from DE)
;  * C  - Line x2 (pixel offset from DE)
;
;  MEMORY
;  * FillPattern - address of 8x8 pixel (8 byte) pattern
;
;-------------------------------------------------------------------------------
HLineFill:

	;-------------------------------------------------------------------
	; Calculate A * 12 and offset into canvas
	;-------------------------------------------------------------------
	push    bc                      ; [11]
	push    af                      ; [11]
	ld      l, a                    ; [4]
	add     a, a                    ; [4] * 2
	add     a, l                    ; [4] * 3
	ld      l, a                    ; [4]
	ld      h, 0                    ; [7]
	add     hl, hl                  ; [11] * 6
	add     hl, hl                  ; [11] * 12
	ld      de, plotSScreen         ; [10/20]
	add     hl, de                  ; [11]
	ex      de, hl                  ; [4]
	pop     af                      ; [10]
	
	;-------------------------------------------------------------------
	; Load pattern byte
	;-------------------------------------------------------------------
	and     $07
	ld      hl, FillPattern         ; [10]
	add     a, l                    ; [4]
	ld      l, a                    ; [4]
	ld      a, 0                    ; [7]
	adc     a, h                    ; [4]
	ld      h, a                    ; [4]
	ld      h, (hl)                 ; [7]
	
	;-------------------------------------------------------------------
	; Calculate the starting byte offset and add to DE.
	;-------------------------------------------------------------------
	ld      a, b                    ; [4]
	and     $f8                     ; [7] mask out low 3 bits to
	rra                             ; [4] prevent and chance of a 
	rra                             ; [4] carry during RRA.
	rra                             ; [4]
	add     a, e                    ; [4] add computed offset to DE.
	ld      e, a                    ; [4]
	ld      a, d                    ; [4]
	adc     a, 0                    ; [7]
	ld      d, a                    ; [4]

	;-------------------------------------------------------------------
	; Calculate the width of the line and store the result in C.
	;-------------------------------------------------------------------
	ld      a, c                    ; [4] calculate width of line and
	sub     b                       ; [4] store result in C.
	ld      c, a                    ; [4]

	ld      a, b                    ; [4] mask out the high 5 bits of
	and     $07                     ; [7] B to get the bit offset.
	ld      b, a                    ; [4]
	ld      l, hFillPlotMask & $ff  ; [10]
	add     a, l                    ; [4]
	ld      l, a                    ; [4]

	ld      a, 7                    ; [7] if this result doesn't set
	sub     b                       ; [4] the carry flag then the
	sub     c                       ; [4] line fits into one screen
	ld      b, h                    ; [4] byte.
	ld      h, hFillPlotMask >> 8   ; [7]
	jp      nc, _byteLine           ; [10] special case - 1 byte line

	;-------------------------------------------------------------------
	; Draw left edge of line [57]
	;-------------------------------------------------------------------
	neg                             ; [8] negate A to get bits
	ld      c, a                    ; [4] remaining after this draw.
	ld      h, (hl)                 ; [7]
	
	ld      a, h
	cpl
	ld      l, a

	; h    = mask of area to retain.
	; l    = mask of area to write.
	; b    = pattern.
	; (de) = byte to modify.			
	
	ld a,(de)
	and h
	ld l,a
	ld a,h
	cpl
	and b
	or l
	ld (de),a
	
	;-------------------------------------------------------------------
	; Draw middle segment of line [45 + 26 per byte]
	;-------------------------------------------------------------------
	ld      a, c                    ; [4]
	and     $f8                     ; [7] jump if there no middle bytes.
	jp      z, _lineEnd             ; [10] 
	rra                             ; [4] othwerwise rotate number of 
	rra                             ; [4] bits remaining right 3 
	rra                             ; [4] places (divide by 8).
	ld      h, b                    ; [4]
	ld      b, a                    ; [4] copy the middle bytes
	ld      a, h                    ; [4]

	; Mid-section: h->(de)
-:	inc de
	ld (de),a
	djnz -

	ld      b, h                    ; [4]

	;-------------------------------------------------------------------
	; Draw right edge of line [107]
	;-------------------------------------------------------------------
_lineEnd:   ld      a, c                    ; [4] mask low 3 bits of C to check
	and     $07                     ; [7] how many bits to write.
	jp      z, _hFillRet            ; [11/5] return if nothing to do.
	ld      hl, hFillPlotMask       ; [10] load the bitmask for the right
	add     a, l                    ; [4]
	ld      l, a                    ; [4]
	ld      a, (hl)                 ; [7]
	ld      l, a                    ; [4]
	cpl                             ; [4]
	inc     de                      ; [6]
	ld      h, a                    ; [4] plot mask -> H.

	; h    = mask of area to retain.
	; l    = mask of area to write.
	; b    = pattern.
	; (de) = byte to modify.

	ld a,(de)
	and h
	ld l,a
	ld a,h
	cpl
	and b
	or l
	ld (de),a
		
_hFillRet:
	pop     bc                      ; [10]
	ret                             ; [10]

	;-------------------------------------------------------------------
	; This code is to handle the special case of x1 and x2 both
	; residing in the same byte of screen RAM.
	;-------------------------------------------------------------------
_byteLine:
	ld a,c
	inc a
	add a,l
	ld c,(hl)
	ld l,a
	ld a,(hl)
	xor c

	ld h,a
	and b
	ld l,a
	ld a,h
	cpl
	ex de,hl
	and (hl)
	or e
	ld (hl),a

	pop     bc                      ; [10]
	ret                             ; [10]


FillPattern:
.fill 8

; ==========================================================================
; Clip16ToRow
; --------------------------------------------------------------------------
; Clips a 16-bit number to an 8-bit one in the range of screen rows (0..63).
; --------------------------------------------------------------------------
; Inputs:    HL: The value to clip.
; Outputs:   A: The clipped value (between 0 and 63).
; Destroyed: F.
; ==========================================================================
Clip16ToRow:
	bit 7,h
	jr z,+
	xor a
	ret
+:	ld a,h
	or a
	jr z,+
	ld a,63
	ret
+:	ld a,l
	cp 64
	ret c
	xor a
	bit 7,l
	ret nz
	ld a,63
	ret

; ==========================================================================
; MinMaxBC
; --------------------------------------------------------------------------
; Updates B and C with A to ensure that B=min(A,B) and C=max(A,C).
; --------------------------------------------------------------------------
; Inputs:    B: The current minimum.
;            C: The current maximum.
;            A: The value to update B and C with.
; Outputs:   B: min(A, B).
;            C: max(A, C).
; Destroyed: F.
; ==========================================================================
MinMaxBC:
	cp b
	jr nc,+
	ld b,a
+:	cp c
	ret c
	ld c,a
	ret


.fill (($+$F)&$FFF0)-$
hFillPlotMask:
.db %00000000
.db %10000000
.db %11000000
.db %11100000
.db %11110000
.db %11111000
.db %11111100
.db %11111110
.db %11111111

Fills:

.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %00000000
.db %00000000

.db %01000100
.db %00000000
.db %00000000
.db %00000000
.db %01000100
.db %00000000
.db %00000000
.db %00000000

.db %01000100
.db %00000000
.db %00010001
.db %00000000
.db %01000100
.db %00000000
.db %00010001
.db %00000000

.db %01010101
.db %00000000
.db %00010001
.db %00000000
.db %01010101
.db %00000000
.db %00010001
.db %00000000

.db %01010101
.db %00000000
.db %01010101
.db %00000000
.db %01010101
.db %00000000
.db %01010101
.db %00000000

.db %01010101
.db %00100010
.db %01010101
.db %00000000
.db %01010101
.db %00100010
.db %01010101
.db %00000000

.db %01010101
.db %00100010
.db %01010101
.db %10001000
.db %01010101
.db %00100010
.db %01010101
.db %10001000

.db %01010101
.db %10101010
.db %01010101
.db %10001000
.db %01010101
.db %10101010
.db %01010101
.db %10001000

.db %01010101
.db %10101010
.db %01010101

.db %10101010
.db %01010101
.db %10101010
.db %01010101
.db %10101010

.db %01110111
.db %10101010
.db %11011101
.db %10101010
.db %01110111
.db %10101010
.db %11011101
.db %10101010

.db %11111111
.db %10101010
.db %11011101
.db %10101010
.db %11111111
.db %10101010
.db %11011101
.db %10101010

.db %11111111
.db %10101010
.db %11111111
.db %10101010
.db %11111111
.db %10101010
.db %11111111
.db %10101010

.db %11111111
.db %11101110
.db %11111111
.db %10101010
.db %11111111
.db %11101110
.db %11111111
.db %10101010

.db %11111111
.db %11101110
.db %11111111
.db %10111011
.db %11111111
.db %11101110
.db %11111111
.db %10111011

.db %11111111
.db %11111111
.db %11111111
.db %10111011
.db %11111111
.db %11111111
.db %11111111
.db %10111011

.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %11111111
.db %11111111

.endmodule