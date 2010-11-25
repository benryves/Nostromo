; Maths module
.module Maths
Code:

	.module Trig
		Cos
			add a,64
		Sin
			add a,a
			ld l,a
			ld a,0
			adc a,Table >> 8
			ld h,a
			ld c,(hl)
			inc hl
			ld b,(hl)
			ret
		
		PackedTable		
		PreviousValue = 0
		Delta = 0
		.for angle = 0 to 127
			NewValue = round(256 * sin(angle / 256 * 2 * pi()))
			Delta <<= 4
			Delta |= (NewValue - PreviousValue) & $0F
			.if angle & 1
				.db Delta
				Delta = 0
			.endif
			PreviousValue = NewValue
		.loop
	.endmodule

	.module Mul

		U8U8: ; HL = H * E
			ld l,0
			ld d,l
			
			sla	h
			jr	nc,$+3
			ld	l,e
			
			.rept 7
				add	hl,hl
				jr	nc,$+3
				add	hl,de
			.loop
			
			ret
			
		U16U16 ; DEHL = sDE*sBC
			ld hl,0
			.rept 16
			add hl,hl
			rl e
			rl d
			jp nc,{+}
			add hl,bc
			jp nc,{+}
			inc de
		+	.loop
			ret

		S16S16 ; sDEHL = sDE*sBC
			ld l,0
			bit 7,d
			jp z,{+}
			; DE is -ve
			inc l
			
			dec de
			ld a,d \ cpl \ ld d,a
			ld a,e \ cpl \ ld e,a
		+

			bit 7,b
			jp z,{+}
			; BC is signed.
			inc l
			dec bc
			ld a,b \ cpl \ ld b,a
			ld a,c \ cpl \ ld c,a
		+
			ld a,l
			call U16U16
	
			and 1
			ret z ; No need to flip sign of DEHL
			ld a,h \ or l
			
			; dec dehl
			
			dec l
			jr nc,+
			dec h
			jr nc,+
			dec e
			jr nc,+
			dec d
		+:

			ld a,d \ cpl \ ld d,a
			ld a,e \ cpl \ ld e,a
			ld a,h \ cpl \ ld h,a
			ld a,l \ cpl \ ld l,a
	
			ret		
		
	.endmodule
	
	.module Div
		U16U8 ; HL rA = uHL / uC
			xor a
			.rept 16
				add hl,hl
				rla
				cp c
				jr c,$+4
				sub c
				inc l
			.loop
			ret
	

		U16S8 ; HL = uHL / sC
			bit 7,c
			jp z,U16U8
			
			ld a,c
			neg
			ld c,a
		
			xor a
			.rept 16
				add hl,hl
				rla
				cp c
				jr c,$+4
				sub c
				inc l
			.loop
			
			dec hl
			ld a,h \ cpl \ ld h,a
			ld a,l \ cpl \ ld l,a
			ret

		U24U16 ; ABC rHL = ABC/DE (unsigned) (HL=0)
			.rept 24
				sll c
				rl b
				rl a
				adc hl,hl
				sbc hl,de
				jr nc,$+4
				add hl,de
				dec c
			.loop
			ret
		
		S24S16 ; ABC = ABC / DE
			ld l,a		
			ld h,0
			bit 7,d \ jp z,{+} \ inc h \ neg_de() \+
			
			bit 7,l \ jp z,{+}
			inc h
			cpl_bc()
			ld a,l \ cpl \ ld l,a
			inc c \ jp nz,{+}
			inc b \ jp nz,{+}
			inc l
		+
			bit 0,h
			ld a,l
			ld hl,$0000

			jp z,U24U16
			
			.rept 24
				sll c
				rl b
				rl a
				adc hl,hl
				sbc hl,de
				jr nc,$+4
				add hl,de
				dec c
			.loop
			
			ld l,a
			cpl_bc()
			ld a,l
			cpl
			inc c \ ret nz
			inc b \ ret nz
			inc a			
			ret
		
		
		S16S16 ; ABC rHL = HL/(DE/256) (signed)
		
			ld c,0
			
			bit 7,h \ jp z,{+} \ inc c \ neg_hl() \+
			bit 7,d \ jp z,{+} \ inc c \ neg_de() \+	
			
			bit 0,c
			
			ld a,h \ ld b,l
			ld hl,$0000
			ld c,h
			
			jp z,U24U16
			
			.rept 24
				sll c
				rl b
				rl a
				adc hl,hl
				sbc hl,de
				jr nc,$+4
				add hl,de
				dec c
			.loop
			
			; Now, negate abc
			ld l,a
			cpl_bc()
			ld a,l
			cpl
			inc c \ ret nz
			inc b \ ret nz
			inc a
			ret
			
		;Reciprocal16: ; BC = 65536 / DE.
		;	xor a
		;	ld h,a
		;	ld c,a
		;	ld l,a
		;	inc l
		;	.rept 16
		;		sl1 c
		;		rla
		;		adc	hl,hl
		;		sbc	hl,de
		;		jr	nc,$+4
		;		add	hl,de
		;		dec	c
		;	.loop
		;	ld b,a
		;	ret
		
	.endmodule

	.module Compare
	
		HL.NegDE.Signed:
			
			neg_de()
			
		HL.DE.Signed:
			
			ld a,d
			xor $80
			ld d,a
			
			ld a,h
			xor $80
			ld h,a
			
			or a
			sbc hl,de
			ret
			
			cp d
			
			ret nz
			
			ld a,l
			cp e
			
			ret
	
	.endmodule

.if Options.ReportModuleSizes \ .echoln strformat("Maths module: {0:N0} bytes.", $-Code) \ .endif
.endmodule