; -------------------- lineClipAndDraw     --------------------
; -------------------- lineClipAndDrawLong --------------------
;
; Draws to the graph buffer a line between two points.
; Parts of the line that are off-screen will be clipped.
;
; Version: 1.0
; Author: Badja <http://badja.calc.org> <badja@calc.org>
; Date: 19 June 2001
; Size: 251 bytes (169 bytes if not using long lines)
;
; Input:
;   (D, E) = (x0, y0), the first point
;   (H, L) = (x1, y1), the second point
;   Values are interpreted as signed bytes, i.e. they can range
;   from -128 to 127.
;
; Input constraints:
;   Use the lineClipAndDraw routine if you are drawing lines
;   whose width and height don't exceed 128 pixels, i.e.:
;     |x1 - x0| < 128  and  |y1 - y0| < 128
;
;   Use the lineClipAndDrawLong routine if your lines are longer.
;   Note that this routine is slower. If you don't use this
;   routine, add to the start of your program the line
;   "#define lcd_noLongLines" to save 82 bytes.
;
; Output:
;   A line between the two points is clipped at the boundary of
;   the screen and ORed to the graph buffer.
;
; Destroys:
;   AF, BC, DE, HL, IX
;
; Requires:
;   The lineDraw routine (in lineDraw.inc)
;
; Comments:
;   This routine is based on the Cohen-Sutherland line-clipping
;   algorithm. If a line is entirely on (or entirely off) the
;   screen, the routine quickly realises this and immediately
;   draws the line (or no line) without trying to calculate any
;   boundary intersection points.

#ifndef lcd_noLongLines
lineClipAndDrawLong:
      ld    a,d               ; compare signs of x0 and x1
      xor   h
      and   $80
      jr    z,lcd_notTooWide  ; if same, line is not too wide
      bit   7,d
      jr    z,lcd_dPositive
      ld    a,d
      neg
      add   a,h
      jr    lcd_checkWidth
lcd_dPositive:
      ld    a,h
      neg
      add   a,d
lcd_checkWidth:               ; A = |x1 - x0|
      and   $80
      jr    nz,lcd_longLine   ; check if width exceeds 128
lcd_notTooWide:
      ld    a,e               ; compare signs of y0 and y1
      xor   l
      and   $80
      jr    z,lineClipAndDraw ; if same, line is not too high
      bit   7,e
      jr    z,lcd_ePositive
      ld    a,e
      neg
      add   a,l
      jr    lcd_checkHeight
lcd_ePositive:
      ld    a,l
      neg
      add   a,e
lcd_checkHeight:              ; A = |y1 - y0|
      and   $80
      jr    z,lineClipAndDraw ; check if height exceeds 128
lcd_longLine:
      push  de                ; line is too long
      push  hl
      call  lcd_bisect        ; bisect line so rest of routine can handle it
      call  lineClipAndDraw   ; draw first half of line
      pop   de
      pop   hl
      call  lcd_bisect        ; bisect again with endpoints swapped, then draw second half
                              ; (second bisection can return a different midpoint - this
                              ; is required in case of lines being 256 pixels wide or high)
#endif

lineClipAndDraw:
      call  lcd_compOutCode
      ex    de,hl
      ld    c,b               ; C = ????, outcode0
lcd_subdivide:
      call  lcd_compOutCode   ; B = outcode0, outcode1
      ld    a,b
      or    a                 ; are both outcodes zero?
      jp    z,lineDraw        ; if so, trivial accept and draw line
      ld    a,b
      and   c                 ; is logical AND of outcodes non-zero?
      and   $0f               ; (mask off irrelevant bits)
      ret   nz                ; if so, trivial reject and return
      ld    a,b               ; failed both tests, so clip the line segment...
      and   $0f               ; at least one endpoint is off-screen, is point 1 off-screen?
      jr    nz,lcd_clip       ; if so, pick this one
      ex    de,hl             ; otherwise, the other endpoint must be off-screen, so exchange them
      ld    b,c
      ld    c,a
lcd_clip:
      push  hl
      bit   0,b
      jr    z,lcd_notBottom
      ld    b,63              ; divide line at bottom of screen (y = 63)
      jr    lcd_divideHoriz
lcd_notBottom:
      bit   1,b
      jr    z,lcd_notTop
      ld    b,0               ; divide line at top of screen (y = 0)
lcd_divideHoriz:
      push  bc
      ld    a,h
      sub   d
      ld    h,a               ; H = x1 - x0
      ld    a,d
      jr    z,lcd_vertical    ; if x1 - x0 == 0, then intersection point is x0 == A
      ld    a,l
      sub   e
      ld    c,a               ; C = y1 - y0
      ld    a,b
      sub   e
      ld    l,a               ; L = y - y0
      ld    b,c               ; B = y1 - y0
      ld    c,d               ; C = x0
      call  lcd_divideLine    ; A = x
lcd_vertical:
      pop   bc                ; B = y
      ld    d,a
      ld    e,b               ; DE = intersection point
      jr    lcd_nextPass
lcd_notTop:
      bit   2,b
      ld    b,95              ; divide line at right edge of screen (x = 95)
      jr    nz,lcd_divideVert
      ld    b,0               ; divide line at left edge of screen (x = 0)
lcd_divideVert:
      push  bc
      ld    a,l
      sub   e
      ld    l,a               ; L = y1 - y0
      ld    a,d
      jr    z,lcd_horizontal  ; if y1 - y0 == 0, then intersection point is y0 == A
      ld    a,h
      sub   d
      ld    c,a               ; C = x1 - x0
      ld    a,b
      sub   d
      ld    h,a               ; H = x - x0
      ld    b,c               ; B = x1 - x0
      ld    c,e               ; C = y0
      call  lcd_divideLine    ; A = y
lcd_horizontal:
      pop   bc                ; B = x
      ld    d,b
      ld    e,a               ; DE = intersection point
lcd_nextPass:
      ld    b,c
      pop   hl
      jr    lcd_subdivide     ; repeat process for new line segment

lcd_divideLine:               ; return A = C + H * L / B
      ld    a,h
      xor   l
      xor   b                 ; bit 7 of A = sign of H * L / B
      push  af
      bit   7,h
      jr    z,lcd_positive1
      ld    a,h
      neg
      ld    h,a
lcd_positive1:                ; H = |H|
      bit   7,l
      jr    z,lcd_positive2
      ld    a,l
      neg
      ld    l,a
lcd_positive2:                ; L = |L|
      ld    a,b
      bit   7,a
      jr    z,lcd_positive3
      neg
lcd_positive3:                ; A = |B|
      bcall(_htimesl)         ; HL = H * L
      bcall(_divhlbya)        ; HL = HL / A
      pop   af                ; bit 7 of A = sign of H * L / B
      and   $80               ; mask off irrelevent bits
      ld    a,l               ; A = result of |H| * |L| / |B|
      jr    z,lcd_positive4
      neg                     ; make result negative if necessary
lcd_positive4:
      add   a,c               ; A = C + result
      ret

lcd_compOutCode:              ; left-rotate 4-bit outcode for point DE into B
      ld    a,d
      rla
      jr    c,lcd_negative1   ; if D < 0, then D must be < 96
      rl    b
      ld    a,95
      sub   d
      rla
      jr    lcd_notNegative1
lcd_negative1:
      rl    b
      and   a
lcd_notNegative1:
      rl    b

      ld    a,e
      rla
      jr    c,lcd_negative2   ; if E < 0, then E must be < 64
      rl    b
      ld    a,63
      sub   e
      rla
      jr    lcd_notNegative2
lcd_negative2:
      rl    b
      and   a
lcd_notNegative2:
      rl    b
      ret

#ifndef lcd_noLongLines
lcd_bisect:                   ; set (H, L) to midpoint of (D, E) and (H, L)
      ld    a,d
      sra   a
      jr    nc,lcd_even1
      inc   a                 ; over-compensate for truncation on D only (valid)
lcd_even1:
      sra   h
      add   a,h
      ld    h,a               ; H = D/2 + H/2
      ld    a,e
      sra   a
      jr    nc,lcd_even2
      inc   a                 ; over-compensate for truncation on E only (valid)
lcd_even2:
      sra   l
      add   a,l
      ld    l,a               ; L = E/2 + L/2
      ret
#endif
