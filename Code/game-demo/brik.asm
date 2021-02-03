;
; 	PIC16F84A VIDEO Flappy Block Game
;	RAY CLEMENS JAN 2021
;

processor 16F84A                ; Define processor
#include <xc.inc>

; CONFIG
    CONFIG  FOSC = HS           ; Oscillator Selection bits (HS oscillator)
    CONFIG  WDTE = OFF          ; Watchdog Timer (WDT disabled)
    CONFIG  PWRTE = OFF         ; Power-up Timer Enable bit (Power-up Timer is disabled)
    CONFIG  CP = OFF            ; Code Protection bit (Code protection disabled)

; Destination bits
#define W 0
#define F 1

; Register bits
#define C   0                   ; Carry flag (STATUS)
#define Z   2                   ; Zero flag (STATUS)
#define RP0	5                   ; Register bank select (STATUS)

#define T0IF 2                  ; Timer 0 IRQ status (INTCON)
#define T0IE 5                  ; Timer 0 IRQ enable (INTCON)
#define GIE	7                   ; Global IRQ enable (INTCON)


; Misc vars (Leading '_' indicates define, not memory location)
#define _NUM_PILLARS 4
#define _PLAYER_ACCEL 0x01
#define _PLAYER_JMP_VEL 0x0B    ; Player Jump Velocity

    ; Game States
#define _GAMES_TITLE 0
#define _GAMES_ACTIVE 1
#define _GAMES_DEAD 2

    ; Timing vars
#define _DEBOUNCE_TIMEOUT 15     ; Timeout between DEAD and ACTIVE states (# OF FRAMES)

; ------------------------------------------------------------------------------
;
; RAM VARIABLES
;
; ------------------------------------------------------------------------------

; RAM objects, BANK 0
    PSECT MainData,global,class=RAM,space=1,delta=1,noexec

; Vars for sync generation
LINE_L:	    DS	1		; Current Line Low
LINE_H:	    DS	1		; Current Line High
	    ; 0..1 - High part of line counter
	    ; 2..6 - Unused
	    ; 7 - Active video region if high, low for display blanking

FRAME:	    DS	1		; Stores frame number
FRAME_SFT:  DS	1		; Stores the frame used for line shifting
OBS_POS:    DS	_NUM_PILLARS	; Obstacle positions
	    ; [0..2] - Pixel location
	    ; [3..4] - H-byte position
	    ; [5..7] - Hole location
TEMP_PTRN:  DS	1		; Temporary storage for pattern calculation
CNT_PTRN:   DS	1		; Count for pattern calculation
PTRN_BUF:   DS  4		; Pattern buffer for next 7 lines
PRAND:	    DS	1		; Pseudo-random numbers
PLAYER_POS: DS	1		; Player y-coord
PLAYER_VEL: DS	1		; Player y-velocity
GAME_STATE: DS	1		; Game State (_GAMES_xx defines)
DBNCE_TMOT: DS	1		; Debounce timeout for going from DEAD to ACTIVE states
PILLAR_CNT: DS	1		; Number of currently active pillars in the game
SCORE_L:    DS	1		; Low byte of score counter
SCORE_H:    DS	1		; High byte of score counter


; ------------------------------------------------------------------------------
;
; Reset vector
;
; ------------------------------------------------------------------------------
; -pRESET_VEC=0h
    PSECT RESET_VEC,global,class=CODE,reloc=2
    GLOBAL RESET_VEC
RESET_VEC:
    GOTO INIT                   ; Jump to initialization


; ------------------------------------------------------------------------------
;
; Interrupt vector
; Must be in BANK 0 when this is called!
; Does not preserve W register!
;
; ------------------------------------------------------------------------------

; Take a look at the following for PAL programming/timing info:
; http://www.nailed-barnacle.co.uk/avr/video.html
; https://www.avrfreaks.net/forum/ntsc-vsync-confusion
; http://blog.retroleum.co.uk/electronics-articles/previous/z80-project-the-early-years/

; -pISR_VEC=4h
    PSECT ISR_VEC,global,class=CODE,delta=2
    GLOBAL ISR_VEC
ISR_VEC:
    CLRF PORTA                  ; Start sync period

    MOVLW (0xFF - 0x9F)         ; Active video and back porch combined
    MOVWF TMR0                  ; Start counter

    MOVLW 0x7F                  ; Clear active video region flag
    ANDWF LINE_H

    BTFSS LINE_H, 1             ; If line high == 2, check line number
    GOTO ISR_CHECK_BLANK1       ; If not, just output standard 4.8uS sync
    MOVLW (0x39 - 2)            ; "First" 2+1 lines are VSYNC
    SUBWF LINE_L, W             ; Are we doing VSYNC?
    BTFSC STATUS, C
    GOTO ISR_UPDATE_LINE        ; Yes, don't turn back on the sync
    GOTO ISR_CHECK_BLANK        ; No, use normal 4.8uS sync

ISR_CHECK_BLANK1:               ; This affects h-hold for the following 2 lines
    NOP                         ; after LINE_H goes from 2 to 1
    NOP                         ; (A.K.A.: you need this (potentially))
    NOP
    NOP
ISR_CHECK_BLANK:

    ; Check if we are in active frame area
    BTFSS LINE_H, 1
    GOTO ISR_END_SYNC_EOF
    MOVLW (0x39 - 55)           ; Ignore first 55 lines
    SUBWF LINE_L, W
    BTFSS STATUS, C             ; If in active video region, set the flag
    GOTO ISR_END_SYNC_AVR
    GOTO ISR_END_SYNC
ISR_END_SYNC_EOF:               ; EOF: (Towards) End Of Frame
    MOVLW 18                    ; Ignore last 18 lines
    SUBWF LINE_L, W
    BTFSC STATUS, C             ; Should the screen blank?
    GOTO ISR_END_SYNC_AVR       ; No
    GOTO ISR_END_SYNC           ; Yes
ISR_END_SYNC_AVR:               ; AVR: Active Video Region (What, did you think I
    BSF LINE_H, 7               ; meant an Arduino or something? ;)
ISR_END_SYNC:
    NOP
    MOVLW 0X14                  ; End sync period
    MOVWF PORTA

ISR_UPDATE_LINE:
    DECFSZ LINE_L               ; Next Line
    GOTO ISR_RESET_SYNC1        ; If not 0, don't bother with high byte
    DECF LINE_H                 ; High byte of line
    MOVF LINE_H, W
    ANDLW 0x03                  ; Mask off status bits
    BTFSS STATUS, Z
    GOTO ISR_RESET_SYNC         ; If not 0, no need for ending of frame

    ; End of frame
    MOVLW 0x39                  ; Load the low count value to row counter
    MOVWF LINE_L                ; There are ~312.5 lines per frame in PAL
    MOVLW 0x02                  ; Reset the high byte of line counter
    MOVWF LINE_H
    INCF FRAME                  ; Next frame
    GOTO ISR_RESET_SYNC

ISR_RESET_SYNC1:
    MOVLW 0xFF                  ; If LINE_L is 0xFF, it means that the above code
    XORWF LINE_L, W             ; improperly set the "Active video region" flag,
    BTFSC STATUS, Z             ; so set it here.
    BSF LINE_H, 7
    NOP
    NOP
ISR_RESET_SYNC:

    BCF INTCON, T0IF            ; Clear interrupt

    BTFSS LINE_H, 7             ; Only display video in AVR
    GOTO IVR_SERVICE            ; Do stuff in non-active video region

    ; -----------------------------------------------------
    ; The following is for generating scanlines
    ; -----------------------------------------------------

DISPLAY_LINE:

    MOVLW 0X07
    ANDWF LINE_L, W
    BTFSC STATUS, Z             ; Skip every 8th line to calculate the next 7
    GOTO CALC_NEXT_LINE

    ; PUT YOUR DRAWING CODE HERE ---------------------------

	; Check game mode
    MOVLW (1 << _GAMES_ACTIVE) | (1 << _GAMES_DEAD)
    ANDWF GAME_STATE, W
    BTFSS STATUS, Z
    GOTO DISPLAY_ACTIVE

    NOP                         ; Move non-active regions over a (few) bits
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    GOTO LINE_BITS              ; Go draw that line

DISPLAY_ACTIVE:
    MOVF PLAYER_POS, W          ; Check and make sure that the line number
;    ANDLW 0xE0                 ; Is "close" to the location of the PLAYER
    SUBWF LINE_L, W
;    ANDLW 0xE0
    BTFSS STATUS, C
    GOTO ISR_NORM_LINE_OFFSET   ; No Hole

    SUBLW 0x08                  ; Defines height of player
    BTFSS STATUS, C
    GOTO ISR_NORM_LINE          ; No Hole

    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP

    NOP                         ; Move player to column 1 so collisions
    NOP	                        ;  are displayed in the right place
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP

    MOVLW 0x1C
    MOVWF PORTA
    NOP
    NOP
    MOVLW 0x14
    MOVWF PORTA

    CALL LINE_DELAY

    GOTO ISR_PLAYER_LINE_ENTRY


ISR_NORM_LINE_OFFSET:
    NOP
    NOP
    NOP
ISR_NORM_LINE:
    NOP
    CALL LINE_DELAY



LINE_BITS:
    MOVLW 0X14                  ; Byte 0
    BTFSC PTRN_BUF, 7
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 6
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 5
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
ISR_PLAYER_LINE_ENTRY:
    MOVLW 0X14
    BTFSC PTRN_BUF, 4
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 3
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 2
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 1
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF, 0
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP

    MOVLW 0X14                  ; Byte 1
    BTFSC PTRN_BUF+1, 7
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 6
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 5
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 4
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 3
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 2
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 1
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+1, 0
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP

    MOVLW 0X14                  ; Byte 2
    BTFSC PTRN_BUF+2, 7
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 6
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 5
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 4
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 3
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 2
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 1
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+2, 0
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP

    MOVLW 0X14                  ; Byte 3
    BTFSC PTRN_BUF+3, 7
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 6
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 5
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 4
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 3
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 2
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 1
    IORLW 0x08
    MOVWF PORTA, F
    NOP
    NOP
    NOP
    NOP
    MOVLW 0X14
    BTFSC PTRN_BUF+3, 0
    IORLW 0x08
    MOVWF PORTA, F

    ; END YOUR DRAWING CODE HERE

    RETFIE                      ; Done with line


; ------------------------------------------------------------------------------
;
; Executed during vertical blanking time
; This handles updating game variables and checking for user input
;
; ------------------------------------------------------------------------------
IVR_SERVICE:

    ; Only run when we get a timeout
    MOVF DBNCE_TMOT
    BTFSC STATUS, Z             ; Only update game stuff when timer is 0
    GOTO IVR_STATE_CHECK

    MOVLW 0x01                  ; Once per frame, update the timeout count
    SUBWF LINE_L, W             ;  this has to happen after all updates
    BTFSS STATUS, Z             ;  occurring during the blanking period
    RETFIE                      ;  otherwise the pillar spacing will be incorrect
    DECFSZ DBNCE_TMOT
    RETFIE
    CLRF FRAME                  ; Reset frame counter (needed for pillar placement)
    RETFIE

IVR_STATE_CHECK:
    ; Check the game state
    BTFSC GAME_STATE, _GAMES_TITLE
    GOTO IVR_TITLE
    BTFSC GAME_STATE, _GAMES_ACTIVE
    GOTO IVR_ACTIVE
    BTFSC GAME_STATE, _GAMES_DEAD
    GOTO IVR_DEAD
    RETFIE  ; default: break;

; ----------------- TITLE ----------------------
IVR_TITLE:
    BTFSC PORTB, 0              ; Check for new game
    RETFIE

    MOVLW 1 << _GAMES_ACTIVE    ; If user pushed button, start active state
    MOVWF GAME_STATE

    CLRF FRAME                  ; Reset frame counter (needed for pillar placement)
    RETFIE

; ----------------- ACTIVE ---------------------
IVR_ACTIVE:

    MOVF FRAME, W               ; Copy the frame to the frame shift var
    MOVWF FRAME_SFT

    MOVLW 0x37                  ; Check for collisions on line 0x137
    SUBWF LINE_L, W
    BTFSC STATUS, Z
    GOTO IVR_CHECK_COLLISION

    MOVLW 0x01                  ; Every 2 frames, update player movement
    ANDWF FRAME, W
    BTFSS STATUS, Z
    GOTO IVR_CHECK_FRAME
    MOVLW 0x35                  ; Only move player once per frame
    SUBWF LINE_L, W
    BTFSC STATUS, Z
    GOTO IVR_MOVE_PLAYER

IVR_CHECK_FRAME:
    MOVLW 0x07                  ; Every 8 frames, move the pillars
    ANDWF FRAME, W
    BTFSS STATUS, Z
    RETFIE

    MOVLW 0x36                  ; Only move pillars once per frame
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    RETFIE                      ; Don't move pillars

    MOVLW 0x3F                  ; Every 64 frames, add a new pillar
    ANDWF FRAME, W
    BTFSS STATUS, Z
    GOTO IVR_MOVE_PILLARS

	; Add a pillar
    MOVLW _NUM_PILLARS          ; See if we have all the pillars
    SUBWF PILLAR_CNT, W
    BTFSC STATUS, Z
    GOTO IVR_MOVE_PILLARS       ; We have all of them, don't add any more
                                ; Don't have all of them, add one
    INCF PILLAR_CNT

IVR_MOVE_PILLARS:
    MOVF PILLAR_CNT, W          ; Setup pillar update loop
    MOVWF CNT_PTRN

IVR_PILLAR_MOVE_LOOP:
    MOVF CNT_PTRN, W
    ADDLW OBS_POS-1
    MOVWF FSR

    MOVF INDF, W
    ANDLW 0x1F ;3F
    BTFSS STATUS, Z             ; Is this the end of the line?
    GOTO IVR_PILLAR_MOVE_LOOP_CONT  ; No, don't change hole position
                                ; Yes,
    CALL PRAND_GEN              ; Move hole
    INCF PRAND                  ; Seed next number
    ADDLW 0x20
    MOVWF INDF                  ; Save hole location

    INCF SCORE_L                ; Update score (counting in BCD)
    MOVLW 0x0F
    ANDWF SCORE_L, W
    SUBLW 0x09
    BTFSC STATUS, C             ; Check for overflow
    GOTO IVR_PILLAR_MOVE_LOOP_CONT  ; No overflow
    MOVLW 0xF0                  ; Carry
    ANDWF SCORE_L
    MOVLW 0x10
    ADDWF SCORE_L
    BTFSS STATUS, C             ; Check for overflow
    GOTO IVR_PILLAR_MOVE_LOOP_CONT  ; No overflow
    INCF SCORE_H                ; Carry
    MOVLW 0x0F
    ANDWF SCORE_H, W
    SUBLW 0x09
    BTFSC STATUS, C             ; Check for overflow
    GOTO IVR_PILLAR_MOVE_LOOP_CONT  ; No overflow
    MOVLW 0xF0                  ; Carry
    ANDWF SCORE_H
    MOVLW 0x10
    ADDWF SCORE_H

IVR_PILLAR_MOVE_LOOP_CONT:
    DECF INDF                   ; Move pillar left
    DECF CNT_PTRN               ; Are we done with the pillar updates?
    BTFSS STATUS, Z
    GOTO IVR_PILLAR_MOVE_LOOP   ; No
    RETFIE                      ; Yes


IVR_MOVE_PLAYER:
    MOVLW _PLAYER_JMP_VEL
    BTFSS PORTB, 0              ; If the player has pressed the button, "flap"
    MOVWF PLAYER_VEL
    MOVLW _PLAYER_ACCEL
    SUBWF PLAYER_VEL
    MOVLW 0xF0                  ; Limit player velocity
    SUBWF PLAYER_VEL, W
    BTFSC STATUS, C
    GOTO IVR_MOD_POS
    BTFSS PLAYER_VEL, 7         ; Bit 7 indicates a negative if set
    GOTO IVR_MOD_POS            ; So only limit player velocity in negative dir
    MOVLW 0xF0
    MOVWF PLAYER_VEL

IVR_MOD_POS:
    RLF PLAYER_VEL, W           ; Save velocity sign bit
    RRF PLAYER_VEL, W           ; Divide velocity by 2
    ADDWF PLAYER_POS            ; Update player position

	; Keep player within bounds
    MOVLW 0xFF - _PLAYER_JMP_VEL    ; To prevent tunneling, use max vel as compare val
    SUBWF PLAYER_POS, W
    BTFSC STATUS, C
    GOTO IVR_UPPER_LIMIT        ; Upper limit on position, cap it
    MOVLW 0x10                  ; Bottom position on screen
    SUBWF PLAYER_POS, W
    BTFSC STATUS, C
    RETFIE                      ; No need to bound the player position
    MOVLW 0x10                  ; Bound player at bottom of screen
    MOVWF PLAYER_POS
    RETFIE

IVR_UPPER_LIMIT:
    MOVLW 0xFF - _PLAYER_JMP_VEL    ; Set player to max y position
    MOVWF PLAYER_POS
    CLRF PLAYER_VEL             ; Also reset velocity so player starts falling
    RETFIE                      ;  as soon as they release the button


IVR_CHECK_COLLISION:
;    BTFSC PORTB, 0              ; Debug, disable collision checking
;    RETFIE                      ;  until button is pressed
    MOVF PILLAR_CNT, W
    MOVWF CNT_PTRN

IVR_COLLISION_LOOP:
    MOVF CNT_PTRN, W            ; Get the current count
    ADDLW OBS_POS-1             ; Add it to the Object Position array base
    MOVWF FSR                   ; Set this as the FSR location

    MOVLW 0x1F                  ; Only want h-pos attribute
    ANDWF INDF, W               ; Get the object position
    BTFSS STATUS, Z             ; Only check for collisions if it is on the
    GOTO IVR_COLLISION_LOOP_CONT    ; same column as the player

    MOVF INDF, W
    ANDLW 0xE0                  ; Top of hole
    SUBWF PLAYER_POS, W         ; Check and if that the player pos
    BTFSS STATUS, C             ;  Is "close" to the location of the hole
    GOTO IVR_HANDLE_COLLISION   ; Collision

    SUBLW 0x20                  ; Height of hole (top to bottom)
    BTFSC STATUS, C             ;  Sets the bottom location of the hole
    GOTO IVR_COLLISION_LOOP_CONT    ; No collision
				    ; Collision
IVR_HANDLE_COLLISION:

    MOVLW 1 << _GAMES_DEAD      ; Set game state to dead
    MOVWF GAME_STATE

    MOVLW _DEBOUNCE_TIMEOUT     ; Wait a bit before accepting new clicks
    MOVWF DBNCE_TMOT

    RETFIE

    ; Repeat for each pillar
IVR_COLLISION_LOOP_CONT:
    DECF CNT_PTRN
    BTFSS STATUS, Z
    GOTO IVR_COLLISION_LOOP
    RETFIE

; ----------------- DEAD -----------------------
IVR_DEAD:

    BTFSC PORTB, 0              ; Check to see if player restarts game
    RETFIE                      ; No button press
                                ; Restart game
    CALL INIT_GAME

    MOVLW 1 << _GAMES_ACTIVE    ; Go to active gameplay
    MOVWF GAME_STATE

    MOVLW _DEBOUNCE_TIMEOUT     ; Set timeout
    MOVWF DBNCE_TMOT

    RETFIE

; ------------------------------------------------------------------------------
;
; Calculates the next (7) lines of display
; Stores the result in PTRN_BUF
;
; ------------------------------------------------------------------------------
CALC_NEXT_LINE:
    ; Clear the pattern buffer
    CLRF PTRN_BUF
    CLRF PTRN_BUF+1
    CLRF PTRN_BUF+2
    CLRF PTRN_BUF+3

    BTFSC GAME_STATE, _GAMES_TITLE
    GOTO CALC_TITLE
    BTFSC GAME_STATE, _GAMES_ACTIVE
    CALL CALC_ACTIVE
    BTFSC GAME_STATE, _GAMES_DEAD
    GOTO CALC_DEAD
    RETFIE

; ----------------- TITLE ----------------------
CALC_TITLE:
    MOVLW 0xD0 - 0x38           ; Check if we are two lines below the logo
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    GOTO CALC_TITLE_TOP         ;
    GOTO CALC_TITLE_BAR

CALC_TITLE_TOP:
    MOVLW 0xD0 + 0x08           ; Check if we are two lines above the logo
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    GOTO CALC_TITLE_LOGO        ; If not, check if we need to draw the logo

CALC_TITLE_BAR:
    MOVLW 0xFF                  ; If we are, draw a line below the text
    MOVWF PTRN_BUF+1
    ADDLW 0xFF                  ; I won't admit that the logo is only 15 bits wide
    MOVWF PTRN_BUF+2
    RETFIE

CALC_TITLE_LOGO:
    MOVLW 0xD0                  ; Top of title position
    SUBWF LINE_L, W             ; Are we there yet?
    BTFSC STATUS, C
    RETFIE                      ; No
                                ; Yes, check bottom bound
    BCF STATUS, C

    MOVLW 0xD0 - 0x28           ; Title is 5 lines tall
    SUBWF LINE_L, W             ; Still within bounds?
    BTFSS STATUS, C
    RETFIE                      ; No, exit
                                ; Yes, draw line of title

    BCF STATUS, C
    RRF LINE_L, W               ; Get the line we are on, divide it by 3
    MOVWF TEMP_PTRN             ; In this case, we only care about bits [3..5]
    RRF TEMP_PTRN               ; So shift those to the bottom
;    RRF TEMP_PTRN
    MOVLW ((0xA0 + 0x08) >> 2) - 1
    SUBWF TEMP_PTRN
    MOVLW 0x0E
    ANDWF TEMP_PTRN

    MOVF TEMP_PTRN, W
    CALL TITLE_BITMAP
    MOVWF PTRN_BUF+1
    INCF TEMP_PTRN, W
    CALL TITLE_BITMAP
    MOVWF PTRN_BUF+2
    RETFIE

; ----------------- ACTIVE ---------------------
; NOTE: this returns with the "RETURN" instruction, NOT "RETFIE"
CALC_ACTIVE:
    BTFSC LINE_H, 1             ; If still above line 0xFF, don't display
    RETFIE

        ; Then take each OBS_POS and use it as an offset from the base of the pattern
        ;  buffer. Shift in a 1 for the position of the pillar
    MOVF PILLAR_CNT, W
    MOVWF CNT_PTRN

CALC_NEXT_LINE_LOOP:
    MOVF CNT_PTRN, W            ; Get the current count
    ADDLW OBS_POS-1             ; Add it to the Object Position array base
    MOVWF FSR                   ; Set this as the FSR location
    RRF INDF, W                 ; Get the object's position
    MOVWF TEMP_PTRN             ; In this case, we only care about bits [3..5]
    RRF TEMP_PTRN               ; So shift those to the bottom
    RRF TEMP_PTRN
    MOVF TEMP_PTRN, W           ; Get now add those bits as an offset to the
    ANDLW 0x03 ;7               ;  pattern buffer base address to set the
    ADDLW PTRN_BUF              ;  byte in which to store the bit for the
    MOVWF TEMP_PTRN             ;  pillar.

    MOVF INDF, W                ; Check and make sure that the line number
    ANDLW 0xE0                  ; Is "close" to the location of the hole
    SUBWF LINE_L, W
;    ANDLW 0xE0
    BTFSS STATUS, C
    GOTO CALC_NEXT_LINE_DISP    ; No Hole

    SUBLW 0x20                  ; Height of hole (multiple of 8)
    BTFSC STATUS, C
    GOTO CALC_NEXT_LINE_LOOP_CONT   ; Hole
                                    ; No Hole

CALC_NEXT_LINE_DISP:

        ; Find the bit location of the pillar in the display byte
    MOVLW 0x07
    ANDWF INDF, W
    CALL SHIFT_BIT
    XORWF TEMP_PTRN, W          ; Swap W and F
    XORWF TEMP_PTRN
    XORWF TEMP_PTRN, W

    MOVWF FSR
    MOVF TEMP_PTRN, W
    IORWF INDF                  ; Save the pillar's bit pattern

    ; Repeat for each pillar
CALC_NEXT_LINE_LOOP_CONT:
    DECF CNT_PTRN
    BTFSS STATUS, Z
    GOTO CALC_NEXT_LINE_LOOP
    RETURN

; ----------------- DEAD -----------------------
CALC_DEAD:

    CALL CALC_ACTIVE            ; Display the position of the player at impact

        ; If we need to display the "DEAD" text, overlay that:
    MOVLW 0xD0 - 0x38 - 0x30    ; Check if we are two lines below the score
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    GOTO CALC_DEAD_TOP          ;
    GOTO CALC_DEAD_BAR

CALC_DEAD_TOP:
    MOVLW 0xD0 + 0x08           ; Check if we are two lines above the logo
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    GOTO CALC_DEAD_BLANK        ; If not, check if we need to draw the logo

CALC_DEAD_BAR:
    MOVLW 0xFF                  ; If we are, draw a line below the text
    MOVWF PTRN_BUF+1
    ADDLW 0xFF                  ; I won't admit that the logo is only 15 bits wide
    MOVWF PTRN_BUF+2
    RETFIE

    ; Check to see if we need to blank the line above or below the "DEAD" text
CALC_DEAD_BLANK:
    MOVLW 0xD0 - 0x30 - 0x30    ; Check if we are one line below the score
    SUBWF LINE_L, W
    BTFSC STATUS, Z
    GOTO CALC_DEAD_BLANK_BAR

    MOVLW 0xD0                  ; Check if we are one line above the logo
    SUBWF LINE_L, W
    BTFSC STATUS, Z
    GOTO CALC_DEAD_BLANK_BAR

    MOVLW 0xD0 - 0x30           ; Check for blank line between "DEAD" and score
    SUBWF LINE_L, W
    BTFSS STATUS, Z
    GOTO CALC_DEAD_SCORE        ; If not, check if we need to draw the logo

CALC_DEAD_BLANK_BAR:
    MOVLW 0x00                  ; If we are, clear the line below the text
    MOVWF PTRN_BUF+1
    MOVWF PTRN_BUF+2
    RETFIE

CALC_DEAD_SCORE:

    MOVLW 0xD0 - 0x30           ; Are we in the range of the score text?
    SUBWF LINE_L, W
    BTFSC STATUS, C
    GOTO CALC_DEAD_LOGO         ; No

    MOVLW 0xD0 - 0x30 - 0x30    ; Text is 5 lines tall
    SUBWF LINE_L, W
    BTFSS STATUS, C	            ; Bottom of range
    GOTO CALC_DEAD_LOGO	        ; No


    MOVLW 0xD0 - 0x30 - 0x28    ; Sets up the line of the number bitmap
    SUBWF LINE_L, W
    ANDLW 0x38
    MOVWF CNT_PTRN
    BCF STATUS, C
    RLF CNT_PTRN

    SWAPF SCORE_L, W            ; 10s place
    ANDLW 0x0F
    IORWF CNT_PTRN, W
    CALL NUM_BITMAP
    MOVWF PTRN_BUF+2

    MOVF SCORE_L, W             ; 1s place
    ANDLW 0x0F
    IORWF CNT_PTRN, W
    CALL NUM_BITMAP
    SWAPF PTRN_BUF+2
    IORWF PTRN_BUF+2

    SWAPF SCORE_H, W            ; 1000s place
    ANDLW 0x0F
    IORWF CNT_PTRN, W
    CALL NUM_BITMAP
    MOVWF PTRN_BUF+1

    MOVF SCORE_H, W             ; 100s place
    ANDLW 0x0F
    IORWF CNT_PTRN, W
    CALL NUM_BITMAP
    SWAPF PTRN_BUF+1
    IORWF PTRN_BUF+1
    RETFIE


    ; Display the "DEAD" text
CALC_DEAD_LOGO:
    MOVLW 0xD0                  ; Top of title position
    SUBWF LINE_L, W             ; Are we there yet?
    BTFSC STATUS, C
    RETFIE                      ; No
                                ; Yes, check bottom bound

    MOVLW 0xD0 - 0x28           ; Title is 5 lines tall
    SUBWF LINE_L, W             ; Still within bounds?
    BTFSS STATUS, C
    RETFIE                      ; No, exit
                                ; Yes, draw line of title

    BCF STATUS, C
    RRF LINE_L, W               ; Get the line we are on, divide it by 3
    MOVWF TEMP_PTRN             ; In this case, we only care about bits [3..5]
    RRF TEMP_PTRN               ; So shift those to the bottom
;    RRF TEMP_PTRN
    MOVLW ((0xA0 + 0x08) >> 2) - 1
    SUBWF TEMP_PTRN
    MOVLW 0x0E
    ANDWF TEMP_PTRN

    MOVF TEMP_PTRN, W           ; Store the character patterns in the pattern buffer
    CALL DEAD_BITMAP
    MOVWF PTRN_BUF+1
    INCF TEMP_PTRN, W
    CALL DEAD_BITMAP
    MOVWF PTRN_BUF+2
    RETFIE

; ------------------------------------------------------------------------------
;
; Initialization
;
; ------------------------------------------------------------------------------
;PSECT code
INIT:
    ; PORT A setup
    BSF STATUS, RP0             ; BANK 1
    MOVLW 0x03                  ; PA[0..1] are inputs
                                ; PA[2..4] are outputs
    MOVWF TRISA                 ; Store directions into direction register

    ; Port B setup
    MOVLW 0x01                  ; PB[0] are inputs
                                ; PB[1..7] are outputs
    MOVWF TRISB
    MOVWF PORTB                 ; Enable pullups

    ; Setup Timer 0
    MOVLW 0x50                  ; Use internal CLKOUT and prescaler
                                ; set to /2 for Timer 0
                                ; Also, PORTB has pullups enabled
    MOVWF OPTION_REG            ; Set options for timer

    MOVLW 0x01                  ; Initialize line counter
    MOVWF LINE_L
    MOVWF LINE_H                ; Also clears status bits

    BCF STATUS, RP0             ; BANK 0
    CLRF PORTA                  ; Clear output data latches for A
    MOVWF TMR0                  ; Initialize Timer 0 to a known value

    BSF INTCON, T0IE            ; Enable Timer 0 IRQ
    BSF INTCON, GIE             ; Global interrupt enable

    MOVLW 0x03
    MOVWF PCLATH                ; For lookup tables

    CALL INIT_GAME              ; Initialize game vars

    MOVLW 1 << _GAMES_TITLE     ; Start on the title screen
    MOVWF GAME_STATE

; Main loop
MAIN:
    ; Wait around for next line, maybe get some tea...
    GOTO MAIN


; ------------------------------------------------------------------------------
;
; Initializes the game variables (mostly, see below)
; Initializes:
;    the number of pillars active,
;    the score
;    the debounce timer to 1
;    the player velocity
;    the player position
;
; DOES NOT initialize:
;    the gamestate
;    the frame counter (either one)
;
; ------------------------------------------------------------------------------

INIT_GAME:  ; Does not set game state

    CLRF PLAYER_VEL
    MOVLW 0x80
    MOVWF PLAYER_POS

    CLRF SCORE_L
    CLRF SCORE_H

    MOVLW 0x01
    MOVWF DBNCE_TMOT

    MOVWF PILLAR_CNT            ; Reset to 1 visible pillar

    MOVLW _NUM_PILLARS          ; Init Pillar locations
    MOVWF CNT_PTRN

INIT_PIL_LOOP:
    MOVF CNT_PTRN, W
    ADDLW OBS_POS-1
    MOVWF FSR

    CALL PRAND_GEN
    ADDLW PRAND                 ; Seed next number
    IORLW 0x1F
    MOVWF INDF

INIT_PIL_LOOP_CONT:
    DECF CNT_PTRN               ; Are we done with the updates?
    BTFSS STATUS, Z
    GOTO INIT_PIL_LOOP          ; No

    DECF OBS_POS                ; Correct spacing for the first pillar

    RETURN

; ------------------------------------------------------------------------------
;
; Delays for a set number of machine cycles, as determined by the value in W[0..2]
;
; NOTE: this also includes a 4-cycle overhead for delay calculation
;
; ------------------------------------------------------------------------------
; -pLINE_DELAY=300h
    PSECT LINE_DELAY,global,class=CODE,delta=2
LINE_DELAY:
    MOVF FRAME_SFT, W
    ANDLW 0x07                  ; The pattern repeats
    ADDLW LINE_foo & 0xFF       ; Add jump offset
    MOVWF PCL                   ; Jump to proper delay

LINE_foo:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    RETURN

; ------------------------------------------------------------------------------
;
; Delays for a inverted number of machine cycles, as determined by the
;  value in W[0..2]
; Useful for resyncing the h-position of elements on the screen after
;  LINE_DELAY has been called
;
; NOTE: this also includes a 5-cycle overhead for delay calculation
;
; ------------------------------------------------------------------------------
LINE_ANTI_DELAY:
    MOVF FRAME_SFT, W
    XORLW 0x07                      ; Opposite count direction
    ANDLW 0x07                      ; The pattern repeats
    ADDLW LINE_foo2 & 0xFF          ; Add jump offset
    MOVWF PCL                       ; Jump to proper delay

LINE_foo2:
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    NOP
    RETURN

; ------------------------------------------------------------------------------
;
; Shift a bit. Returns with the bit set corresponding to the inverted value of
;  W[0..2]
;
; NOTE: There is no sanity check, so do not call with a value greater than
;  0x07 in W otherwise bad things might happen
;
; ------------------------------------------------------------------------------

SHIFT_BIT:
    ADDLW SHIFT_BIT_LUT & 0xFF
    MOVWF PCL

SHIFT_BIT_LUT:
    RETLW 0x80
    RETLW 0x40
    RETLW 0x20
    RETLW 0x10
    RETLW 0x08
    RETLW 0x04
    RETLW 0x02
    RETLW 0x01


; ------------------------------------------------------------------------------
;
; Pseudo-random number "loopup table" - Very random :)
; Call to get a number
; W affects the value returned
; Be sure to re-seed the number in PRAND after calling, or you will always get
;  the same number for a given W
;
; ------------------------------------------------------------------------------
PRAND_GEN:
    ADDWF PRAND, W
    ANDLW 0x1F
    ADDLW PRAND_GEN_LUT & 0xFF
    MOVWF PCL

PRAND_GEN_LUT:	; Random.org
    RETLW 6 << 5
    RETLW 4 << 5
    RETLW 6 << 5
    RETLW 5 << 5
    RETLW 6 << 5
    RETLW 5 << 5
    RETLW 5 << 5
    RETLW 3 << 5
    RETLW 4 << 5
    RETLW 5 << 5
    RETLW 4 << 5
    RETLW 3 << 5
    RETLW 6 << 5
    RETLW 4 << 5
    RETLW 5 << 5
    RETLW 6 << 5
    RETLW 2 << 5
    RETLW 5 << 5
    RETLW 4 << 5
    RETLW 6 << 5
    RETLW 4 << 5
    RETLW 5 << 5
    RETLW 2 << 5
    RETLW 2 << 5
    RETLW 2 << 5
    RETLW 6 << 5
    RETLW 2 << 5
    RETLW 6 << 5
    RETLW 3 << 5
    RETLW 5 << 5
    RETLW 3 << 5
    RETLW 5 << 5


; ------------------------------------------------------------------------------
;
; Returns the bit patterns for displaying the title "BRIK" logo.
;
; Odd values of W return the bit patterns for "IK" and even for "BR"
;
; ------------------------------------------------------------------------------

; Call this initially with W holding 0xff
TITLE_BITMAP:
    ANDLW 0x0F                      ; Sanity check
    ADDLW TITLE_BITMAP_LUT & 0xFF
    MOVWF PCL

TITLE_BITMAP_LUT:
    RETLW 0b11001010
    RETLW 0b11101010
    RETLW 0b10101100
    RETLW 0b01001010
    RETLW 0b11001010
    RETLW 0b01001100
    RETLW 0b10101010
    RETLW 0b01001010
    RETLW 0b11001100
    RETLW 0b11101000
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00

; ------------------------------------------------------------------------------
;
; Returns the bit patterns for displaying the "SCOR" text.
;
; Odd values of W return the bit patterns for "OR" and even for "SC"
;
; ------------------------------------------------------------------------------
DEAD_BITMAP:
    ANDLW 0x0F                      ; Sanity check
    ADDLW DEAD_BITMAP_LUT & 0xFF
    MOVWF PCL

DEAD_BITMAP_LUT:
    RETLW 0b11101110
    RETLW 0b11101010
    RETLW 0b00101000
    RETLW 0b10101100
    RETLW 0b11101000
    RETLW 0b10101010
    RETLW 0b10001000
    RETLW 0b10101010
    RETLW 0b11101110
    RETLW 0b11101100
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00


; ------------------------------------------------------------------------------
;
; Returns the bit pattern for displaying numbers 0 through 9
; Bit patterns are returned in W[0..3]
;
; W[0..3] - Number to display
; W[4..6] - Bitmap line number
; W[7] - Unused
;
; ------------------------------------------------------------------------------
NUM_BITMAP:
    ANDLW 0x7F
    ADDLW NUM_BITMAP_LUT & 0xFF
    MOVWF PCL

NUM_BITMAP_LUT:

    ; Line 4
    RETLW 0b1110 ;0
    RETLW 0b1110 ;1
    RETLW 0b1110 ;2
    RETLW 0b1110 ;3
    RETLW 0b0010 ;4
    RETLW 0b1110 ;5
    RETLW 0b1110 ;6
    RETLW 0b0100 ;7
    RETLW 0b1110 ;8
    RETLW 0b0010 ;9
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00

    ; Line 3
    RETLW 0b1010 ;0
    RETLW 0b0100 ;1
    RETLW 0b1000 ;2
    RETLW 0b0010 ;3
    RETLW 0b0010 ;4
    RETLW 0b0010 ;5
    RETLW 0b1010 ;6
    RETLW 0b0100 ;7
    RETLW 0b1010 ;8
    RETLW 0b0010 ;9
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00

    ; Line 2
    RETLW 0b1010 ;0
    RETLW 0b0100 ;1
    RETLW 0b0100 ;2
    RETLW 0b0110 ;3
    RETLW 0b1110 ;4
    RETLW 0b1110 ;5
    RETLW 0b1110 ;6
    RETLW 0b0010 ;7
    RETLW 0b1110 ;8
    RETLW 0b1110 ;9
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00

    ; Line 1
    RETLW 0b1010 ;0
    RETLW 0b1100 ;1
    RETLW 0b0010 ;2
    RETLW 0b0010 ;3
    RETLW 0b1010 ;4
    RETLW 0b1000 ;5
    RETLW 0b1000 ;6
    RETLW 0b0010 ;7
    RETLW 0b1010 ;8
    RETLW 0b1010 ;9
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00

    ; Line 0
    RETLW 0b1110 ;0
    RETLW 0b0100 ;1
    RETLW 0b1110 ;2
    RETLW 0b1110 ;3
    RETLW 0b1010 ;4
    RETLW 0b1110 ;5
    RETLW 0b1110 ;6
    RETLW 0b1110 ;7
    RETLW 0b1110 ;8
    RETLW 0b1110 ;9
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00
    RETLW 0x00


end
