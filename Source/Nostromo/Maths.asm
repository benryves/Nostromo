; Maths module
.module Maths

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
		.fill (($+$FF)&$FF00)-$
		Table
		.for angle = 0 to 255
			.dw 256 * sin(angle / 256 * 2 * pi())
		.loop
	.endmodule

	.module Mul			

		U8U8 ; HL = E*A
			ld hl,0
			ld d,h
			.rept 8
				rrca
				jp nc,{+}
				add hl,de
			+	sla e
				rl d
			.loop
			ret

		S8S8 ; HL = E*B

			ld d,0
			ld h,d
			
			
			bit 7,e
			jp z,{+}
			inc h
			ld a,e
			neg
			ld e,a
		+
			ld a,b	
			bit 7,a
			jp z,{+}
			inc h
			neg
		+	
			
			bit 0,h
			jp z,U8U8
			call U8U8
			dec hl
			ld a,h \ cpl \ ld h,a
			ld a,l \ cpl \ ld l,a
			ret
			
		S8U8 ; sHL = sE*uA
			
			bit 7,e
			jp z,U8U8

			ld b,a
			ld a,e
			neg
			ld e,a
			ld a,b
			call U8U8
			dec hl
			ld a,h \ cpl \ ld h,a
			ld a,l \ cpl \ ld l,a
			ret
		
		U16U8 ; AHL = uA*uDE
			ld hl,0
			ld c,0
			add a,a
			jr nc,$+4
			ld h,d
			ld l,e

			.rept 7
				add hl,hl
				rla
				jr nc,$+4
				add hl,de
				adc a,c
			.loop
			ret
			
		S16S8 ; AHL = sA*sDE
			ld h,0
			bit 7,a
			jp z,{+}
			inc h
			neg
		+	
			bit 7,d
			jp z,{+}
			inc h
			push af
			dec de
			ld a,d \ cpl \ ld d,a
			ld a,e \ cpl \ ld e,a
			pop af
		+	bit 0,h
			jp z,U16U8
			call U16U8
			ld e,a
			dec hl
			ld a,h \ cpl \ ld h,a
			ld a,l \ cpl \ ld l,a
			ld a,e
			neg
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
			dec hl
			jp nz,{+}
			dec de
		+	

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



	.endmodule
.endmodule