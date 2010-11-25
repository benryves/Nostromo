.module Interrupt
Code:

.if outputwriteris('ti8x')
Base = $98
.else
Base = $82
.endif
Table = (Base << 8) + $200
Location = Base | (Base << 8)

; ==========================================================================
; Loads the interrupt handler into RAM and enables it.
; ==========================================================================
Load:
	di

	ld hl,Handler
	ld de,Location
	ld bc,HandlerEnd-Handler
	ldir
	
	ld hl,Table
	ld a,Base
	ld (hl),a
	ld d,h
	ld e,l
	inc de
	ld bc,256
	ldir
	
	ld a,Table >> 8
	ld i,a

	ld a,%00001000 ; Disable/ack all interrupts; keep calc powered.
	out (3),a
	ld a,%00001011 ; Enable timer interrupt 1 and ON key.
	out (3),a
	ld a,%00000110 ; Slowest speed.
	out (4),a
	
	im 2
	ei
	ret

Handler:
$ = Location
	push af
	
	ld a,8
	out (3),a
	
	push hl
	Ticks = $+1
	ld hl,0
	inc hl
	ld (Ticks),hl
	pop hl
	
	ld a,$0F
	out (3),a
	
	pop af
	ei
	reti
$ = @
HandlerEnd:
	
.if Options.ReportModuleSizes \ .echoln strformat("Interrupt module: {0:N0} bytes.", $-Code) \ .endif
.endmodule