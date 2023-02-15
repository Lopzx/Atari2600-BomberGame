        processor 6502

        include "vcs.h"
        include "macro.h"


	seg.u Var
        org $80
        
pXPos ds 1
pYPos ds 1
eXPos ds 1
eYPos ds 1
pHeight ds 1
Random byte
DIGITS_HEIGHT = 5

ScoreSprite     byte         ; store the sprite bit pattern for the score
TimerSprite     byte         ; store the sprite bit pattern for the timer
Score           byte         ; 2-digit score stored as BCD
Timer           byte         ; 2-digit timer stored as BCD
Temp            byte         ; auxiliary variable to store temp values
OnesDigitOffset word         ; lookup table offset for the score Ones digit
TensDigitOffset word         ; lookup table offset for the score Tens digit
MissileYPos byte
MissileXPos byte

	MAC DRAW_MISSILE
        ldy #%00000000
        cpx MissileYPos      ; compare X (current scanline) we Y pos
        bne .SkipMissileDraw ; if (X != missile Y position), then skip draw
.DrawMissile:                ; else:
        ldy #%00000010       ;     enable missile 0 display
        inc MissileYPos      ;     MissileYPos++
.SkipMissileDraw:
        sty ENAM0            ; store correct value in the TIA missile register
   	 ENDM
        seg; Define a new segment named "Code"
        org $F000 ; Define the origin of the ROM code at memory address $F000
SetHorizontal subroutine
    sta WSYNC                ; start a fresh new scanline
    sec                      ; 2 C make sure carry-flag is set before subtracion
.Div15Loop
    sbc #15                  ; 2 C subtract 15 from accumulator
    bcs .Div15Loop           ; 3 C loop until carry-flag is clear
    eor #7                   ; 2 C handle offset range from -8 to 7
    asl
    asl
    asl
    asl
    sta HMP0,Y               ; 4C store the fine offset to the correct HMxx
    sta RESP0,Y              ; 4C fix object position in 15-step increment
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Declare a MACRO to check if we should display the missile 0
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    
Start:
        CLEAN_START
        
        ldy #0
        sty pYPos
        
        ldy #25
        sty pXPos
        
        ldy #1
        sty eYPos
        
        ldy #92
        sty eXPos
        
        ldy #9
        sty pHeight
        
        lda #%11010100
    	sta Random               ; Random = $D4
           
    lda #0
    sta Score                ; Score = 0
    sta Timer                ; Timer = 0
        
NextFrame:    
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1   
        lda #0
    	sta COLUBK
	
SetDisp:
      ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display VSYNC and VBLANK
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #2
    sta VBLANK               ; turn on VBLANK
    sta VSYNC                ; turn on VSYNC
    REPEAT 3
        sta WSYNC            ; display 3 recommended lines of VSYNC
    REPEND
    lda #0
    sta VSYNC                ; turn off VSYNC
    REPEAT 35
        sta WSYNC            ; display the 37 recommended lines of VBLANK
    REPEND
    sta VBLANK               ; turn off VBLANK
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Calculations and tasks performed in the VBlank
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda pXPos
    ldy #0
    jsr SetHorizontal        ; set player0 horizontal position

    lda eXPos
    ldy #1
    jsr SetHorizontal         ; set player1 horizontal position
    
    lda MissileXPos
    ldy #2
    jsr SetHorizontal         ; set missile0 horizontal position

    jsr CalculateDigitOffset ; calculate scoreboard digits lookup table offset

    sta WSYNC
    sta HMOVE                ; apply the horizontal offsets previously set

    lda #0
    sta VBLANK               ; turn off VBLANK

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    sta CTRLPF; reset TIA registers before displaying the score

    ldx #DIGITS_HEIGHT       ; start X counter with 5 (height of digits)
 
 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Display the scoreboard lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    lda #0                   ; clear TIA registers before each new frame
    sta PF0
    sta PF1
    sta PF2
    sta GRP0
    sta GRP1
    
    REPEAT 10
        sta WSYNC            ; display 20 scanlines where the scoreboard goes
    REPEND
  
  lda #$1E
  sta COLUPF
.ScoreDigitLoop:
    ldy TensDigitOffset      ; get the tens digit offset for the Score
    lda Digits,Y             ; load the bit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta ScoreSprite          ; save the score tens digit pattern in a variable

    ldy OnesDigitOffset      ; get the ones digit offset for the Score
    lda Digits,Y             ; load the digit bit pattern from lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora ScoreSprite          ; merge it with the saved tens digit sprite
    sta ScoreSprite          ; and save it
    sta WSYNC                ; wait for the end of scanline
    sta PF1                  ; update the playfield to display the Score sprite

    ldy TensDigitOffset+1    ; get the left digit offset for the Timer
    lda Digits,Y             ; load the digit pattern from lookup table
    and #$F0                 ; mask/remove the graphics for the ones digit
    sta TimerSprite          ; save the timer tens digit pattern in a variable

    ldy OnesDigitOffset+1    ; get the ones digit offset for the Timer
    lda Digits,Y             ; load digit pattern from the lookup table
    and #$0F                 ; mask/remove the graphics for the tens digit
    ora TimerSprite          ; merge with the saved tens digit graphics
    sta TimerSprite          ; and save it

    jsr Sleep12Cycles        ; wastes some cycles

    sta PF1                  ; update the playfield for Timer display

    ldy ScoreSprite          ; preload for the next scanline
    sta WSYNC                ; wait for next scanline

    sty PF1                  ; update playfield for the score display
    inc TensDigitOffset
    inc TensDigitOffset+1
    inc OnesDigitOffset
    inc OnesDigitOffset+1    ; increment all digits for the next line of data

    jsr Sleep12Cycles        ; waste some cycles

    dex                      ; X--
    sta PF1                  ; update the playfield for the Timer display
    bne .ScoreDigitLoop      ; if dex != 0, then branch to ScoreDigitLoop

    sta WSYNC
    lda #0
    sta COLUPF
    lda #$FF
    sta PF0
    sta PF1
    sta PF2
    REPEAT 4
    	sta WSYNC
    REPEND

    ldx #$AF
    stx COLUBK ;; Background Color

    ldy #$C5
    sty COLUPF ; PF Color
    ldx #1
    stx CTRLPF


    ldy #%11110000
    sty PF0
    ldy #%11110000
    sty PF1
    ldy #0
    sty PF2

    ldx #85
    
  
DrawLoop:
	DRAW_MISSILE
        sta WSYNC
	txa
        sec       
	sbc pYPos
        cmp pHeight
        bcs CheckEnemy
        
        tay
	lda PlayerSprite,Y 
	sta GRP0
        lda #%00000101
    	sta NUSIZ1               ; stretch player 1 sprite
        ldy #$F1
        sty COLUP0 ; Player Color
CheckEnemy:    
        txa
        sec
	sbc eYPos
        cmp pHeight
        bcs continue
        tay
	lda EnemySprite,Y         
	sta GRP1
        ldy #$8F
        sty COLUP1 ; Player Color
continue:
	
        
        sta WSYNC
        dex
        bne DrawLoop
	
SetVBLANK:
	lda #2
        sta VBLANK
        ldy #0
        sty GRP1
        sty GRP0
        REPEAT 29
        	sta WSYNC
        REPEND
        lda #0
        sta VBLANK
PUP:
	lda #%00010000
        bit SWCHA
        bne PDOWN
        lda pYPos
        cmp #70
        bpl PDOWN
        inc pYPos
PDOWN:
	lda #%00100000
        bit SWCHA
        bne PLEFT
        lda pYPos
        cmp #2
        bmi PLEFT
        dec pYPos
PLEFT:
	lda #%01000000
        bit SWCHA
        bne PRIGHT
        lda pXPos
        cmp #23
        bmi PRIGHT
        dec pXPos
PRIGHT:
	
	lda #%10000000
        bit SWCHA
        bne CheckButtonPressed
        lda pXPos
        cmp #105
        sta WSYNC
        bpl CheckButtonPressed
        inc pXPos
CheckButtonPressed:	
    lda #%10000000           ; if button is pressed
    bit INPT4
    bne EndInputCheck
.ButtonPressed:
    lda pXPos
    clc
    adc #5
    sta MissileXPos          ; set the missile X position equal to the player 0
    lda pYPos
    clc
    adc #8
    sta MissileYPos          ; set the missile Y position equal to the player 0

EndInputCheck:               ; fallback when no input was performed
EndCalc:
	
	dec eYPos 
        lda eYPos
        cmp #0
        bmi ResetBomber
        sta WSYNC
GoNext:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Check for object collision
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CheckCollisionP0P1:
    lda #%10000000           ; CXPPMM bit 7 detects P0 and P1 collision
    bit CXPPMM               ; check CXPPMM bit 7 with the above pattern
    bne .CollisionP0P1       ; if collision between P0 and P1 happened, branch
    jmp CheckCollisionP0PF   ; else, skip to next check
.CollisionP0P1:
    jsr GameOver             ; call GameOver subroutine

CheckCollisionP0PF:
    lda #%10000000           ; CXP0FB bit 7 detects P0 and PF collision
    bit CXP0FB               ; check CXP0FB bit 7 with the above pattern
    bne .CollisionP0PF       ; if collision P0 and PF happened, branch
    jmp EndCollisionCheck    ; else, skip to next check
.CollisionP0PF:
    jsr GameOver             ; call GameOver subroutine

EndCollisionCheck:           ; fallback
    sta CXCLR                ; clear all collision flags before the next frame

        jmp NextFrame
ResetBomber:
	sed
        lda Score
        clc
        adc #1
        sta Score
        
        lda Timer
        clc
        adc #1
        sta Timer

	cld
	jsr GetRandomBomberPos
        jmp GoNext


GameOver subroutine
    lda #$30
    sta COLUBK               ; Set background color to red on game over
    
    lda #0
    sta Score
    
    rts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Subroutine to handle scoreboard digits to be displayed on the screen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The scoreboard is stored using BCD, so the display shows hex numbers.
;; This converts the high and low nibbles of the variable Score and Timer
;; into the offsets of digits lookup table so the values can be displayed.
;; Each digit has a height of 5 bytes in the lookup table.
;;
;; For the low nibble we need to multiply by 5
;;   - we can use left shifts to perform multiplication by 2
;;   - for any number N, the value of N*5 = (N*2*2)+N
;;
;; For the upper nibble, since its already times 16, we need to divide it
;; and then multiply by 5:
;;   - we can use right shifts to perform division by 2
;;   - for any number N, the value of (N/16)*5 is equal to (N/4)+(N/16)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
CalculateDigitOffset subroutine
    ldx #1                   ; X register is the loop counter
.PrepareScoreLoop            ; this will loop twice, first X=1, and then X=0

    lda Score,X              ; load A with Timer (X=1) or Score (X=0)
    and #$0F                 ; remove the tens digit by masking 4 bits 00001111
    sta Temp                 ; save the value of A into Temp
    asl                      ; shift left (it is now N*2)
    asl                      ; shift left (it is now N*4)
    adc Temp                 ; add the value saved in Temp (+N)
    sta OnesDigitOffset,X    ; save A in OnesDigitOffset+1 or OnesDigitOffset

    lda Score,X              ; load A with Timer (X=1) or Score (X=0)
    and #$F0                 ; remove the ones digit by masking 4 bits 11110000
    lsr                      ; shift right (it is now N/2)
    lsr                      ; shift right (it is now N/4)
    sta Temp                 ; save the value of A into Temp
    lsr                      ; shift right (it is now N/8)
    lsr                      ; shift right (it is now N/16)
    adc Temp                 ; add the value saved in Temp (N/16+N/4)
    sta TensDigitOffset,X    ; store A in TensDigitOffset+1 or TensDigitOffset

    dex                      ; X--
    bpl .PrepareScoreLoop    ; while X >= 0, loop to pass a second time

    rts


GetRandomBomberPos subroutine
    lda Random
    asl
    eor Random
    asl
    eor Random
    asl
    asl
    eor Random
    asl
    rol Random               ; performs a series of shifts and bit operations

    lsr
    lsr                      ; divide the value by 4 with 2 right shifts
    sta eXPos           ; 
    lda #30
    adc eXPos            ; adds 30 to compensate for left PF
    sta eXPos            ; and sets the new value to the bomber x-position

    lda #96
    sta eYPos            ; set the y-position to the top of the screen

    rts

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Game Over subroutine
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
Sleep12Cycles subroutine
    rts


Digits:
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00110011          ;  ##  ##
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #
    .byte %00010001          ;   #   #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %00010001          ;   #   #
    .byte %01110111          ; ### ###

    .byte %00100010          ;  #   #
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #

    .byte %01110111          ; ### ###
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01100110          ; ##  ##
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01010101          ; # # # #
    .byte %01100110          ; ##  ##

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01110111          ; ### ###

    .byte %01110111          ; ### ###
    .byte %01000100          ; #   #
    .byte %01100110          ; ##  ##
    .byte %01000100          ; #   #
    .byte %01000100          ; #   #

PlayerSprite:
    .byte #%00000000         ;
    .byte #%00010100         ;   # #
    .byte #%01111111         ; #######
    .byte #%00111110         ;  #####
    .byte #%00011100         ;   ###
    .byte #%00011100         ;   ###
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
        
EnemySprite:
    .byte #%00000000         ;
    .byte #%00001000         ;    #
    .byte #%00001000         ;    #
    .byte #%00101010         ;  # # #
    .byte #%00111110         ;  #####
    .byte #%01111111         ; #######
    .byte #%00101010         ;  # # #
    .byte #%00001000         ;    #
    .byte #%00011100         ;   ###
	
	

    org $FFFC ; End the ROM always at position $FFFC
    .word Start ; Put 2 bytes with reset address at memory position $FFFC
    .word Start ; Put 2 bytes with break address at memory position $FFFE