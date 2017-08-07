#include "hcs12.inc"
lcd_dat         equ     portk
lcd_dir         equ     ddrk
lcd_E           equ     $02
lcd_RS          equ     $01

                org     $10ff            ;loads RMB safely away from stack

hrs2            rmb     1                ;reserves space for hours main clock
hrs1            rmb     1                ;reserves space for hours main clock
                fcb     $3A              ;forces ASCII value for ":"
min2            rmb     1                ;reserves space for minute main clk
min1            rmb     1                ;reserves space for minute main clk
                fcb     $3A
scn2            rmb     1                ;reserves space for sec main clk
scn1            rmb     1                ;reserves space for sec main clk
ampm            rmb     1                ;reserves either A or P for A.M. P.M.
                fcb     $4D              ; ASCII value for "M"
                fcb     $20              ;blank
                fcb     $20              ;blank
d               rmb     1                ;D or space
o               rmb     1                ;0 or space
n               rmb     1                ;N, F, or space
e               rmb     1                ;F or space
last            rmb     1

                org     $1110
ahrs2           rmb     1                ;reserves space for hours alarm clock
ahrs1           rmb     1                ;reserves space for hours alarm clock
                fcb     $3A              ;forces ASCII value for ":"
amin2           rmb     1                ;reserves space for hours alarm clock
amin1           rmb     1                ;reserves space for hours alarm clock
                fcb     $3A
ascn2           rmb     1                ;reserves space for hours alarm clock
ascn1           rmb     1                ;reserves space for hours alarm clock
aampm           rmb     1
                fcb     $4D              ; ASCII value for "M"
                fcb     $20
                fcb     $20
ao              rmb     1                ;O or space
an              rmb     1                ;F, N, or space
af              rmb     1                ;F or space
                fcb     $3E              ; ">"
alast           rmb     1

screen1         fcb     $53              ;S
                fcb     $31              ;1
                fcb     $3A              ;:
                fcb     $54              ;letters for screen
                fcb     $49              ;"S1: TIME S2: ALARM
                fcb     $4D
                fcb     $45
                fcb     $20
                fcb     $53
                fcb     $32
                fcb     $3A
                fcb     $41
                fcb     $4C
                fcb     $41
                fcb     $52
                fcb     $4D
blast           rmb     1

screen2         fcb     $53              ;characters for screen 2
                fcb     $31              ;"S1:RIGHT S2: INCR"
                fcb     $3A
                fcb     $52
                fcb     $49
                fcb     $47
                fcb     $48
                fcb     $54
                fcb     $20
                fcb     $53
                fcb     $32
                fcb     $3A
                fcb     $49
                fcb     $4E
                fcb     $43
                fcb     $52
clast           rmb     1

screen3         fcb     $53              ;Characters for screen 3
                fcb     $31              ;"S1: SILENCE ALARM
                fcb     $3A
                fcb     $53
                fcb     $49
                fcb     $4C
                fcb     $45
                fcb     $4E
                fcb     $43
                fcb     $45
dlast           rmb     1

chk12           rmb     2                ;rmb for checking 12:00 or 13:00
chk13           rmb     2

unit            rmb     1                ;all of these are units that affect
unit2           rmb     1                ;the functions of the clock
unit3           rmb     1                ;used so the clock knows what the user
unita           rmb     1                ;is currently doing and functions
unitaa          rmb     1                ;accordingly
unitaaa         rmb     1

zero            rmb     1		 ;rmbs just for 0
zero1           rmb     1



                org     $2000		 ;starts at 2000
                lds     #$1500           ; stack
                movb    #$18,ATD0DIEN	 ;starts atd0
                movb    #$C0,ddrm	 ;turns on portm pins for output
                movb    #$C0,ptm
                jsr     openLCD		 ;starts lcd
                ldx     #$3132		 ;everything below here is data
                ldy     #$3133		 ;storing for the RMB's above
                stx     chk12
                sty     chk13
                ldaa    #0                ; load A w/ 0
                staa    last             ;stores 0 so putslcd stops
                staa    alast
                staa    blast
                staa    clast
                staa    dlast
                staa    zero
                staa    zero1
                staa    unit
                staa    unit2
                staa    unita
                staa    unitaa
                staa    unitaaa
                ldaa    #$30
                staa    min2
                staa    min1
                staa    scn2
                staa    scn1
                staa    ahrs2
                staa    ahrs1
                staa    amin2
                staa    amin1
                staa    ascn2
                staa    ascn1
                ldaa    #$31
                staa    hrs2
                ldaa    #$31
                staa    hrs1
                ldaa    #$41
                staa    ampm
                staa    aampm
                ldd     #$4F46
                std     ao
                ldaa    #$46
                std     an
                jsr     lcd1		;updates LCD after all values
                lbra    seconds		;stored

t32nd           ldd     #$2020		;loads blanks were "done" is
                std     d
                std     n
                jsr     lcdin
                ldd     #$4F4E		;checks to see if alarm is "ON"
                cpd     ao		;by checking for "o"+"n" in rmb
                lbeq    alarmcmp	;breaks to cmp the crnt time to alarmset
rafter
                jsr     lcd1		;updates lcd1
                ldaa    #18		;problems with just using acc Y so
                staa    unita		;used rmb to store delay count
                ldy     #1		;delay 1/32 of a second
lap             jsr     delay32nd
                ;;;;;;;;;;;;;;;;;;;;;;;;
yo              brset   ptim,$40,go             ;checks s1
                brclr   ptim,$40,*		;debounce

                ldaa    #1			;if s1 pressed
                staa    unit			;sets variables so loop
                ldaa    #0			;branches accordingly
                staa    unit2
                lbra    settime			;branches to settime routine
go              brset   ptim,$80,cont           ;checks s2
                brclr   ptim,$80,*		;debounce
                ldaa    #2			;if s2 pressed
                staa    unit			;sets variables so loop
                ldaa    #0			;branches accordingly
                staa    unit2
                lbra    setalarm		;branches to setalarm routine
cont            dec     unita			;dec rmb used for delay count
                ldaa    #0
                cmpa    unita
                bne     lap			;loops if hasnt been 1 second
                lbra    insc1			;jumps to increment seconds
						;min-hrs-ampm-timeoperations
settime

sett            ldd     #$444F			;loads "DONE"
                std     d			;in spaces reserved for it
                ldd     #$4E45			;for setting the time
                std     n
                jsr     lcdin			;updates "DONE" to lcd
                jsr     lcd2			;loads lcd2

settr           ldaa    #9			;memory count rmb
                staa    unita			;youknow
settrr          ldy     #1
                jsr     delay32nd
                brset   ptim,$40,goo            ;checks s1
                brclr   ptim,$40,*		;debounce
                inc     unit2			;there are 8 spaces on the lcd
                ldaa    #7			;where the cursor can land and
                cmpa    unit2			;increment location, thus setting
                lbne    contt			;time or returning for normal operation
                ldaa    #0
                staa    unit2
                lbra    contt

contu           lbra    contt			
goo             brset   ptim,$80,contu          ;checks s2
                brclr   ptim,$80,*		;debounce

;if s2 is pressed checks unit2 and increments location

t11             ldaa   #0		;if unit2=0 increment hrs1			
                cmpa   unit2
                bne    t22
                inc    hrs1
                ldd    #$303A
                cpd    hrs2		;checks for 12:00 or 13:00
                bne    t111		;nstuff
                ldd    #$3130
                std    hrs2
t111            ldd    #$3133
                cpd    hrs2
                lbne   contt
                ldd    #$3031
                std    hrs2
                lbra   contt

t22             ldaa    #1		;if unit2=1 inc min2
                cmpa    unit2
                bne     t33
                inc     min2
                ldaa    #$36
                cmpa    min2
                lbne     contt
                ldaa    #$30
                staa    min2
                bra     contt


t33             ldaa    #2		;if unit2=2 inc min1
                cmpa    unit2
                bne     t44
                inc     min1
                ldaa    #$3A
                cmpa    min1
                lbne    contt
                ldaa    #$30
                staa    min1
                bra     contt

t44             ldaa    #3		;if unit2=3 inc scn2
                cmpa    unit2
                bne     t55
                inc     scn2
                ldaa    #$36
                cmpa    scn2
                lbne    contt
                ldaa    #$30
                staa    scn2
                bra     contt

t55             ldaa    #4		;if unit2=4 inc scn1
                cmpa    unit2
                bne     t66
                inc     scn1
                ldaa    #$3A
                cmpa    scn1
                lbne    contt
                ldaa    #$30
                staa    scn1
                bra     contt

t66             ldaa    #5		;if unit2=5 change ampm
                cmpa    unit2
                bne     t77
                lbra    taco

t77             ldaa    #6		;returns to norm operation	
                cmpa    unit2		;if unit2=6
                lbne    contt
                ldaa    #0
                staa    unit
                lbra    contt
                
contt           jsr     lcdin3		;updates lcd

                
                dec     unita
                ldaa    #0
                cmpa    unita
                lbne    settrr
                lbra    insc1


; below does the exact same as the SETTIME subroutine but with the 
; added "ON" or "OFF" toggle and uses the ALARM RMB time spaces
 
setalarm        jsr     lcdin2
setta           jsr     lcd2
settra          ldaa    #5
                staa    unita
settrra         ldy     #1
                jsr     delay32nd
                brset   ptim,$40,gooa             ;s1
                brclr   ptim,$40,*
                inc     unit2
                ldaa    #8
                cmpa    unit2
                lbne    contta
                ldaa    #0
                staa    unit2
                lbra    contta

contua          lbra    contta
gooa            brset   ptim,$80,contua         ;s2
                brclr   ptim,$80,*


t11a            ldaa   #0
                cmpa   unit2
                bne    t22a
                inc    ahrs1
                ldd    #$303A
                cpd    ahrs2
                bne    t111a
                ldd    #$3130
                std    ahrs2
t111a           ldd    #$3133
                cpd    ahrs2
                lbne   contta
                ldd    #$3031
                std    ahrs2
                lbra   contta

t22a            ldaa    #1
                cmpa    unit2
                bne     t33a
                inc     amin2
                ldaa    #$36
                cmpa    amin2
                lbne    contta
                ldaa    #$30
                staa    amin2
                lbra    contta


t33a            ldaa    #2
                cmpa    unit2
                bne     t44a
                inc     amin1
                ldaa    #$3A
                cmpa    amin1
                lbne    contta
                ldaa    #$30
                staa    amin1
                lbra     contta

t44a            ldaa    #3
                cmpa    unit2
                bne     t55a
                inc     ascn2
                ldaa    #$36
                cmpa    ascn2
                lbne    contta
                ldaa    #$30
                staa    ascn2
                lbra     contta

t55a            ldaa    #4
                cmpa    unit2
                bne     t66a
                inc     ascn1
                ldaa    #$3A
                cmpa    ascn1
                lbne    contta
                ldaa    #$30
                staa    ascn1
                bra     contta

t66a            ldaa    #5
                cmpa    unit2
                bne     t77a
                ldab    #$41
                cmpb    aampm
                beq     pma
                stab    aampm
                jmp     seconds
pma             ldab    #$50
                stab    aampm
                jmp     seconds
                
t77a            ldaa    #6
                cmpa    unit2
                lbne    ohno
                
                ldaa    #0
                cmpa    unitaa
                beq     ayyo
                ldaa    #0
                staa    unitaa
                ldd     #$4F46
                std     d
                std     ao
                ldd     #$4620
                std     n
                staa    af
                bra     ohno
                
ayyo            ldaa    #1
                staa    unitaa
                ldd     #$4F4E
                std     d
                std     ao
                ldd     #$2020
                std     n
                staa    af
                bra     ohno

ohno            ldaa    #7
                cmpa    unit2
                lbne    contta
                ldaa    #0
                staa    unit
                lbra    contta

contta          jsr     lcdin2

                
                dec     unita
                ldaa    #0
                cmpa    unita
                lbne    settrra
                lbra    insc1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is the master branch checkpoint, gets rmb's and branches to specified
;subroutines!

seconds
                ldaa    #1		;checks if the alarm is going off
                cmpa    unitaaa
                lbeq    beepbeep	;ifyes goes to beepbeep
                
                ldaa    #0		;if rmb unit=0 normal operation
                cmpa    unit
normop          lbeq    t32nd
                ldaa    #1
                cmpa    unit
stim            lbeq    settime		;if 1 --> settime
                ldaa    #2
                cmpa    unit
sala            lbeq    setalarm	;if 2 --> setalarm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is the normal time-running operation, seconds, minutes, hours ticking by

insc1
                inc     scn1
                ldaa    #$3A
                cmpa    scn1
                bgt     seconds
                ldaa    #$30
                staa    scn1
                
insc2           inc     scn2
                ldab    #$36
                cmpb    scn2
                bgt     seconds
                ldaa    #$30
                staa    scn1
ss2             staa    scn2

inmn1           inc     min1
                ldaa    #$3A
                cmpa    min1
                bgt     seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
sm1             staa    min1

                
inmn2           inc     min2
                ldab    #$36
                cmpb    min2
                lbgt     seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
                staa    min1
sm2             staa    min2
                lbeq    contt
                
inhr1           inc     hrs1
                ldaa    hrs2
                ldab    hrs1
                cpd     chk12
                beq     taco

                ldaa    hrs2
                ldab    hrs1
                cpd     chk13
                beq     tacor
                
                ldab    #$3A
                cmpb    hrs1
                lbne    seconds
                ldaa    #$30
                staa    scn1
                staa    scn2
                staa    min1
                staa    min2
sh1             staa    hrs1
                
inhr2           inc     hrs2
                jmp     seconds

tacor           ldaa    #$31
                staa    hrs1
                ldab    #$30
                stab    hrs2
                stab    min2
                stab    min1
                stab    scn2
                stab    scn1
                jmp     seconds

taco
                ldab    #$41
                cmpb    ampm
                beq     pm
                stab    ampm
                jmp     seconds
pm              ldab    #$50
                stab    ampm
                jmp     seconds

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;when alarm is "on" branches here and compares current time to alarm time

alarmcmp        ldd     hrs2
                cpd     ahrs2
                lbne    rafter
                ldd     min2
                cpd     amin2
                lbne    rafter
                ldd     scn2
                cpd     ascn2
                lbne    rafter
                ldaa    ampm
                cmpa    aampm
                lbne    rafter
                ldaa    #1
                staa    unitaaa
beepbeep        jsr     lcd3
                ldaa    #5
                staa    unita
beep1           ldy     #2
ayyy            jsr     delay32nd
                brset   ptim,$40,gooo             ;is s1 pressed
                brclr   ptim,$40,*		;turns silences alarm
                ldaa    #0			;but still have to turn
                staa    unitaa			;alarm off in alarm menu
                staa    unitaaa
                lbra    seconds

;this is the alarm sound thingyy
                
gooo
buzzer          movb    #$20,DDRT           ; set PT5 as output
forever         movb    #$20,PTT            ; set PT5 high
                ldy     #10
                jsr     delay32nd
                movb    #$00,PTT            ; set PT5 low
                ldy     #2
                jsr     delay32nd

fffttt
                dec     unita
                ldaa    #0
                cmpa    unita
                bne     beep1
                lbra    insc1



;calculated values to LCD
lcdin
                ldaa    #$80            ;value specifies top line of lcd
                jsr     cmd2LCD         ;uses value in LCD cmd
                ldx     #hrs2           ;loads inch ASCII value
                jsr     putsLCD         ;uses value and displays it on LCD
                ldy     #$01            ;short delay
                jsr     delayby10ms
                rts
lcdin3
                ldaa    #$80            ;value specifies top line of lcd
                jsr     cmd2LCD         ;uses value in LCD cmd
                ldx     #hrs2           ;loads inch ASCII value
                jsr     putsLCD         ;uses value and displays it on LCD
                ldaa    #$0F		;turns cursor on
                jsr     cmd2LCD
                ldaa    #$02
                jsr     cmd2LCD

;routine that shifts cursor to the position being edited
                ldaa    unit2
                staa    unit3
                inc     unit3
                ldaa    #0
                cmpa    unit2
                beq     same
                ldaa    #3
                cmpa    unit2
                bgt     plus1
                ldaa    #6
                cmpa    unit2
                bgt     plus2
                inc     unit3
                inc     unit3
                inc     unit3
plus2           inc     unit3
plus1           inc     unit3

same            ldaa    #$17
                jsr     cmd2LCD
                dec     unit3
                ldaa    #0
                cmpa    unit3
                bne     same
                
                ldy     #$01            ;short delay
                jsr     delayby10ms
                rts
                
lcdin2
                ldaa    #$80            ;value specifies top line of lcd
                jsr     cmd2LCD         ;uses value in LCD cmd
                ldx     #ahrs2           ;loads inch ASCII value
                jsr     putsLCD         ;uses value and displays it on LCD
                ldaa    #$0F		;turns cursor on
                jsr     cmd2LCD
                ldaa    #$02
                jsr     cmd2LCD

;routine that shifts cursor the position being edited
                ldaa    unit2
                staa    unit3
                inc     unit3
                ldaa    #0
                cmpa    unit2
                beq     asame
                ldaa    #3
                cmpa    unit2
                bgt     aplus1
                ldaa    #6
                cmpa    unit2
                bgt     aplus2
                ldaa    #7
                cmpa    unit2
                bgt     aplus5

                inc     unit3
                inc     unit3
aplus5          inc     unit3
                inc     unit3
                inc     unit3
aplus2          inc     unit3
aplus1          inc     unit3

asame           ldaa    #$17
                jsr     cmd2LCD
                dec     unit3
                ldaa    #0
                cmpa    unit3
                bne     asame
                ldy     #$01            ;short delay
                jsr     delayby10ms
                rts

lcd1
                ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen1        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts

lcd2            ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen2        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts

lcd3
                ldaa    #$C0            ;value specifies bottom line of lcd
                jsr     cmd2LCD         ;uses value in lcd cmd
                ldx     #screen3        ;loads cm ASCII value
                jsr     putsLCD         ;uses value and displays it on lcd
                ldy     #$28            ;delays $28 * 10ms
                jsr     delayby10ms
                rts



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
cmd2LCD     psha
            bclr    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #lcd_E
            staa    lcd_dat
            nop
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #lcd_e
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
openLCD     movb    #$FF,lcd_dir
            ldy     #10
            jsr     delayby1ms
            ldaa    #$28
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldaa    #$06
            jsr     cmd2lcd
            ldaa    #$01
            jsr     cmd2lcd
            ldy     #2
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putcLCD     psha
            bset    lcd_dat,lcd_RS
            bset    lcd_dat,lcd_E
            anda    #$F0
            lsra
            lsra
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            pula
            anda    #$0F
            lsla
            lsla
            bset    lcd_dat,lcd_E
            oraa    #$03
            staa    lcd_dat
            nop
            nop
            nop
            bclr    lcd_dat,lcd_E
            ldy     #1
            jsr     delayby1ms
            rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
putsLCD     ldaa    1,x+                    ; get one char from string
            beq     donePS                  ; reach NULL character
            jsr     putcLCD
            bra     putsLCD
donePS      rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
delayby1ms  movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again0      addd    #375                    ; start an o/c operation
            std     TC0                     ; with 50 ms time delay
wait_lp0    brclr   TFLG1,$01,wait_lp0
            ldd     TC0
            dbne    y,again0
            rts

delayby10ms movb    #$90,TSCR1              ; enable TCNT & ffclr
            movb    #$06,TSCR2              ; config prs factor to 64
            movb    #$01,TIOS               ; enable OC0
            ldd     TCNT
again2      addd    #3750                   ; start an output compare operation
            std     TC0                     ; with 50 ms time delay
wait_lp     brclr   TFLG1,$01,wait_lp
            ldd     TC0
            dbne    y,again2
            rts

delay20us   movb    #$90,tscr1              ; enable timer and ffclr
            movb    #$00,tscr2              ; prescale set to 1
            movb    #$01,TIOS               ; enable OC0
            ldd     tcnt                    ; enable time counter
            addd    #480                    ; add 480 to register D
            std     tc0                     ; enable ch.0 for o/c
loop        brclr   tflg1,$01,loop          ; branch clear
            rts

delay32nd   movb    #$90,TSCR1         ; enable TCNT and fast flags clear
            movb    #$06,TSCR2         ; configure prescale factor to 8
            movb    #$01,TIOS          ; enable OC0
            ldd     TCNT                    ; enable time counter
            addd    #11700
            std     TC0
            brclr   TFLG1,$01,*
            rts