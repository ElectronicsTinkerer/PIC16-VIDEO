
processor 16F84A	    ; Define processor
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
#define C	0		; Carry flag (STATUS)
#define Z	2		; Zero flag (STATUS)
#define RP0	5		; Register bank select (STATUS)

#define T0IF	2		; Timer 0 IRQ status (INTCON)
#define T0IE	5		; Timer 0 IRQ enable (INTCON)
#define GIE	7		; Global IRQ enable (INTCON)


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


; ------------------------------------------------------------------------------
;
; Reset vector
;
; ------------------------------------------------------------------------------
; -pRESET_VEC=0h
    PSECT RESET_VEC,global,class=CODE,reloc=2
    GLOBAL RESET_VEC
RESET_VEC:
    GOTO INIT		    ; Jump to initialization


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
    CLRF PORTA		    ; Start sync period

    MOVLW (0xFF - 0x9F)	    ; Active video and back porch combined
    MOVWF TMR0		    ; Start counter

    MOVLW 0x7F		    ; Clear active video region flag
    ANDWF LINE_H

    BTFSS LINE_H, 1	    ; If line high == 2, check line number
    GOTO ISR_CHECK_BLANK1   ; If not, just output standard 4.8uS sync
    MOVLW (0x39 - 2)	    ; "First" 2+1 lines are VSYNC
    SUBWF LINE_L, W	    ; Are WE DOING VSYNC?
    BTFSC STATUS, C
    GOTO ISR_UPDATE_LINE    ; Yes, don't turn back on the sync
    GOTO ISR_CHECK_BLANK    ; No, use normal 4.8uS sync

ISR_CHECK_BLANK1:	    ; This affects h-hold for the following 2 lines
    NOP			    ; after LINE_H goes from 2 to 1
    NOP			    ; (A.K.A.: you need this (potentially))
    NOP
    NOP
ISR_CHECK_BLANK:

    ; Check if we are in active frame area
    BTFSS LINE_H, 1
    GOTO ISR_END_SYNC_EOF
    MOVLW (0x39 - 47)	    ; Ignore first 47 lines
    SUBWF LINE_L, W
    BTFSS STATUS, C	    ; If in active video region, set the flag
    GOTO ISR_END_SYNC_AVR
    GOTO ISR_END_SYNC
ISR_END_SYNC_EOF:	    ; EOF: (Towards) End Of Frame
    MOVLW 0x11		    ; Ignore last 18 lines
    SUBWF LINE_L, W
    BTFSC STATUS, C	    ; Should the screen blank?
    GOTO ISR_END_SYNC_AVR   ; No
    GOTO ISR_END_SYNC	    ; Yes
ISR_END_SYNC_AVR:	    ; AVR: Active Video Region (What, did you think I
    BSF LINE_H, 7	    ; meant an Arduino or something? ;)
ISR_END_SYNC:
    NOP
    MOVLW 0X14		    ; End sync period
    MOVWF PORTA

ISR_UPDATE_LINE:
    DECFSZ LINE_L	    ; Next Line
    GOTO ISR_RESET_SYNC1    ; If not 0, don't bother with high byte
    DECF LINE_H		    ; High byte of line
    MOVF LINE_H, W
    ANDLW 0X03		    ; Mask off status bits
    BTFSS STATUS, Z
    GOTO ISR_RESET_SYNC	    ; If not 0, no need for ending of frame

    ; End of frame
    MOVLW 0x39		    ; Load the low count value to row counter
    MOVWF LINE_L	    ; There are ~312.5 lines per frame in PAL
    MOVLW 0X02		    ; Reset the high byte of line counter
    MOVWF LINE_H
    GOTO ISR_RESET_SYNC

ISR_RESET_SYNC1:
    MOVLW 0xFF		    ; If LINE_L is 0xFF, it means that the above code
    XORWF LINE_L, W	    ; improperly set the "Active video region" flag,
    BTFSC STATUS, Z	    ; so set it here.
    BSF LINE_H, 7
    NOP
ISR_RESET_SYNC:

    BCF INTCON, T0IF	    ; Clear interrupt

    BTFSS LINE_H, 7	    ; Only display video in AVR
    RETFIE

    ; -----------------------------------------------------
    ; Place your scanline generation code below
    ; -----------------------------------------------------

DISPLAY_LINE:

    ; YOUR LINE-DRAWING CODE HERE!
    ; Be sure that your code is not longer than about 52us otherwise it could
    ; the sync-timing
    
    RETFIE		    ; Done with line


; ------------------------------------------------------------------------------
;
; Initialization
;
; ------------------------------------------------------------------------------
;PSECT code
INIT:
    ; PORT A setup
    BSF STATUS, RP0	    ; BANK 1
    MOVLW 0x03		    ; PA[0..1] are inputs
			    ; PA[2..4] are outputs
    MOVWF TRISA		    ; Store directions into direction register

    ; Setup Timer 0
    MOVLW 0XD0		    ; Use internal CLKOUT and prescaler
			    ; set to /2 for Timer 0
    MOVWF OPTION_REG	    ; Set options for timer

    MOVLW 0X01		    ; Initialize line counter
    MOVWF LINE_L
    MOVWF LINE_H	    ; Also clears status bits

    BCF STATUS, RP0	    ; BANK 0
    CLRF PORTA		    ; Clear output data latches for A
    MOVWF TMR0		    ; Initialize Timer 0 to a known value

    BSF INTCON, T0IE	    ; Enable Timer 0 IRQ
    BSF INTCON, GIE	    ; Global interrupt enable


; Main loop
MAIN:
    ; Various background tasks can be put here.
    ; If you plan on using the W register, it might be worth adding a save W
    ; to the interrupt code (since it currently does not save W)
    GOTO MAIN



end
