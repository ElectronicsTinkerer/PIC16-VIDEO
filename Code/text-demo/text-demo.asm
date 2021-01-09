
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

; Vars for character rendering
CHAR_ROW:   DS	1		; Stores current pixel row of char line
CHARS:	    DS 32		; Char array
PTRN_BUF:   DS  8		; Pattern buffer for next 7 lines

; Vars for serial port
IN_CHAR:    DS	1   ; Stores the current byte being shifted in
CHAR_CNT:   DS	1   ; Stores the current count delay until the next bit reading
CHAR_POS:   DS	1   ; Current cursor location (0-indexed) of the next char
		    ; to be put on the screen
STATUS_IN:  DS	1   ; Status bits for the shift-in code
	    ; 0 - Actively shifting in a byte if high
	    ; 1 - Validating start bit if high (not cleared until byte is fully
	    ;     shifted in)
	    ; 2..7 - Unused

; Vars for screen scrolling
SC_CHAR:    DS	1   ; Stores the character during a move operation
SC_POS:	    DS	1   ; Stores the final position of the cursor for after scroll

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

    ; --------------------------------------------------------------
    ; Start serial port receiver code ----------
    ; --------------------------------------------------------------
    BTFSC STATUS_IN, 0	    ; Are we currently in the middle of recieving a byte?
    GOTO SERIAL_SHIFT_BIT   ; Yes
    BTFSC PORTA, 0	    ; No, see if there is a start bit
    GOTO SERIAL_RESET_START ; Pulse length check is over, the pulse was
			    ; too short or there was no pulse at all

    BTFSS STATUS_IN, 1	    ; Are we checking for a start bit?
    GOTO SERIAL_NEW_BYTE    ; No, this is new, set flag and count
			    ; Yes, check if we are done
    DECF CHAR_CNT
    BTFSC STATUS, Z	    ; Is the negatice pulse of sufficient length?
    GOTO SERIAL_NEXT_BIT    ; Yes
    GOTO SERIAL_EXIT_DELAY  ; Not sure, wait for timeout

SERIAL_NEW_BYTE:
    CLRF IN_CHAR
    BSF IN_CHAR, 7	    ; Set bit 7 so that we know when we have all 8 bits
			    ; (The carry will be set when we whift the 8th bit
			    ; in "SERIAL_SHIFT_BIT")

    BSF STATUS_IN, 1	    ; Indicate that we are validating a start bit

    MOVLW 0X05		    ; Start bit only has a delay of 5 lines
    MOVWF CHAR_CNT	    ; So initialize the delay and wait for next line
    GOTO SERIAL_EXIT_DELAY

SERIAL_SHIFT_BIT:
    DECF CHAR_CNT	    ; Are we done with this delay?
    BTFSS STATUS, Z
    GOTO SERIAL_EXIT_DELAY  ; No, keep counting
			    ; Yes, get bit

    BTFSC PORTA, 0	    ; Read the bit
    GOTO SERIAL_SET_CARRY   ; Set if set
    BCF STATUS, C	    ; Clear is clear
    GOTO SERIAL_BIT_CONT

SERIAL_SET_CARRY:
    BSF STATUS, C
SERIAL_BIT_CONT:
    RRF IN_CHAR		    ; Previously set bit 7 to 1, so if C is 1, we are done
    BTFSS STATUS, C
    GOTO SERIAL_NEXT_BIT    ; Not done receiving, just continue with line
			    ; Done receiving byte, display it:

    ; ---------------------
    ; If you really just wanted the byte received by the serial port and did not
    ; want to display it, ignore the code from here to a similar message below
    ; ---------------------
    MOVF FSR, W		    ; Save current FSR position
    MOVWF CHAR_CNT	    ; (at this point, CHAR_CNT is no longer in use)

    MOVLW ' '		    ; Is in the character set?
    SUBWF IN_CHAR, W
    BTFSS STATUS, C
    GOTO SERIAL_CLR_SCRN    ; No.
    RLF IN_CHAR		    ; Yes: just rotate
    RLF IN_CHAR
    RLF IN_CHAR		    ; Bit 1 now holds bit 6, indicating the char's "bank"
    GOTO SERIAL_DISP_CHAR

SERIAL_CLR_SCRN:
    MOVLW 0x09		    ; Is this a tab? - This clears the screen
    SUBWF IN_CHAR, W
    BTFSS STATUS, Z
    GOTO SERIAL_RETURN	    ; No
    CALL CLEAR_SCREEN	    ; Yes, clear the screen and reset cursor
    GOTO SERIAL_EXIT_CLR_STATUS
SERIAL_RETURN:
    MOVLW 0x0a		    ; Is this a newline?
    SUBWF IN_CHAR, W
    BTFSS STATUS, Z
    GOTO SERIAL_BACKSPC	    ; No
    MOVLW 0x18		    ; Yes, add a newline
    SUBWF CHAR_POS, W
    BTFSC STATUS, C	    ; Check if this is the last line
    CALL SCROLL_SCREEN	    ; Yes, scroll the screen up one line
    MOVLW 0x18		    ; Move cursor to start of next line
    ANDWF CHAR_POS
    MOVLW 0x08
    ADDWF CHAR_POS
    GOTO SERIAL_EXIT_CLR_STATUS
SERIAL_BACKSPC:
    MOVLW 0x08		    ; Is this a backspace?
    SUBWF IN_CHAR, w
    BTFSS STATUS, Z
    GOTO SERIAL_UNK_CHAR    ; No
			    ; Yes, go back and erase previous character
    MOVF CHAR_POS	    ; Set Z if CHAR_POS cannot be decremented
    BTFSS STATUS, Z
    DECF CHAR_POS
    MOVLW CHARS		    ; Get character array offset
    MOVWF FSR
    MOVF CHAR_POS, W	    ; Get cursor offset
    ADDWF FSR
    CLRF INDF		    ; Erase the character
    GOTO SERIAL_EXIT_CLR_STATUS

SERIAL_UNK_CHAR:
    ; Period or unknown character, just print a period.
    MOVLW 0xF0		    ; Yes
    GOTO SERIAL_PUT_CHAR

SERIAL_PUT_CHAR:
    MOVWF IN_CHAR
SERIAL_DISP_CHAR:

    MOVLW 0x20
    SUBWF CHAR_POS, W	    ; Is this the last place on the screen?
    BTFSS STATUS, Z
    GOTO SERIAL_DISP_CHAR1  ; No, just inc cursor
    CALL SCROLL_SCREEN	    ; Yes, scroll screen

SERIAL_DISP_CHAR1:
    MOVF CHAR_POS, W	    ; Get the cursor position
    MOVWF FSR		    ; And use it to set the cursor position in memory
    MOVLW CHARS
    ADDWF FSR

    MOVF IN_CHAR, W
    MOVWF INDF		    ; Output char

    INCF CHAR_POS	    ; Next cursor location

    MOVF CHAR_CNT, W	    ; Restore FSR location
    MOVWF FSR

    ; ---------------------
    ; Hello! This is the end of the display byte section, the code below is now
    ; part of the serial receive code once again.
    ; ---------------------

SERIAL_EXIT_CLR_STATUS:
    CLRF STATUS_IN	    ; Reset shift in status bits

    GOTO SERIAL_EXIT_NO_DELAY

SERIAL_NEXT_BIT:
    BSF STATUS_IN, 0	    ; (Say that we are actively reading a byte)
			    ; This is really only necessary for the first bit

    MOVLW 0X06		    ; Delay between bits is 6 lines
    MOVWF CHAR_CNT	    ; + (@2400 baud, 6.51 scanlines/bit)
    GOTO SERIAL_EXIT_DELAY  ; + Due to delay of 5 for start bit, 6 scanlines
			    ;   *should* suffice to handle 8 bits of data

SERIAL_RESET_START:
    BCF STATUS_IN, 1	    ; Not enough cycles to be start bit, reset check flag

    ; --------------------------------------------------------------
    ; End serial port receiver code ----------
    ; --------------------------------------------------------------

    ; The below delay is purely for the display, it is not needed for the
    ; serial receiver
SERIAL_EXIT_DELAY:
    MOVLW (0x100 - 0x05)    ; Move everything over a bit
SERIAL_DELAY:
    ADDLW 0x01
    NOP
    BTFSS STATUS, Z	    ; Are done with delay?
    GOTO SERIAL_DELAY	    ; No.
			    ; Yes
SERIAL_EXIT_NO_DELAY:

    ; Officially done with the serial and display delay stuff

    BTFSS LINE_H, 7	    ; Only display video in AVR
    RETFIE

    ; -----------------------------------------------------
    ; The following is for generating scanlines
    ; -----------------------------------------------------

DISPLAY_LINE:

    MOVLW 0X07
    ANDWF LINE_L, W
    BTFSC STATUS, Z	    ; Skip every 8th line to calculate the next 7
    GOTO CALC_NEXT_LINE



    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB
    CALL DISPLAY_CHAR_SUB

    MOVLW 0x08		    ; Reset FSR pointer location
    SUBWF FSR
    RETFIE		    ; Done with line


; ------------------------------------------------------------------------------
;
; Spits out the data for this line of the frame
; Make sure to have the character row in CHAR_ROW
; and the character code in W
; Auto increments FSR
;
; ------------------------------------------------------------------------------
DISPLAY_CHAR_SUB:
    MOVLW 0X14
    BTFSC INDF, 7
    IORLW 0x08
    MOVWF PORTA, F

    MOVLW 0X14
    BTFSC INDF, 6
    IORLW 0x08
    MOVWF PORTA, F

    MOVLW 0X14
    BTFSC INDF, 5
    IORLW 0x08
    MOVWF PORTA, F

    MOVLW 0X14
    BTFSC INDF, 4
    IORLW 0x08
    MOVWF PORTA, F

    MOVLW 0X14
    BTFSC INDF, 3
    IORLW 0x08
    MOVWF PORTA, F

    MOVLW 0X14
    BTFSC INDF, 2
    IORLW 0x08
    MOVWF PORTA, F

;    MOVLW 0X14		    ; Not needed since we are using 5x7 chars
;    BTFSC INDF, 1
;    IORLW 0x08
;    MOVWF PORTA, F
;
;    MOVLW 0X14
;    BTFSC INDF, 0
;    IORLW 0x08
;    MOVWF PORTA, F

    INCF FSR
    RETURN


; ------------------------------------------------------------------------------
;
; Calculates the next (7) lines of display
; Stores the result in PTRN_BUF
;
; ------------------------------------------------------------------------------
CALC_NEXT_LINE:
    RRF LINE_H, W	    ; Get the high bit of the row counter
    MOVWF CHAR_ROW
    RRF CHAR_ROW	    ; Move LINE_H[1] into carry

    RRF LINE_L, W	    ; Now get the lower byte of row counter
    MOVWF CHAR_ROW
    RRF CHAR_ROW
    RRF CHAR_ROW

    ; Counter
    MOVF CHAR_ROW, W	    ; Get the text row
    MOVWF FSR
    MOVLW 0x02		    ; Subtract 2 to compensate for line offset
    SUBWF FSR
    MOVLW 0x18		    ; Only need bits 3..4 (there is 4 lines of text)
    ANDWF FSR
    XORWF FSR		    ; Flip the FSR count (this makes inverts the count
			    ; direction) - needed to make it count starting
			    ; at the base of the char buffer instead of at the
			    ; beginning of the last row. - If that did not make
			    ; sense, it basically is needed because the LINE_L
			    ; counts down and the data in CHAR_BUF "counts up"
    MOVLW CHARS		    ; Add offset for the beginning of the text array
    ADDWF FSR
    ; End counter

    MOVLW 0X06		    ; Apply char scan row offset
    ADDWF CHAR_ROW
    MOVLW 0X07		    ; Ignore wraparound
    ANDWF CHAR_ROW

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+1	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+2	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+3	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+4	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+5	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+6	    ; Save bit pattern
    INCF FSR		    ; Next char

    CALL CHAR_LUT	    ; Get bit pattern
    MOVWF PTRN_BUF+7	    ; Save bit pattern
    INCF FSR		    ; Next char

    MOVLW PTRN_BUF	    ; Set FSR to beginning of pattern buffer
    MOVWF FSR
    RETFIE


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

    CALL CLEAR_SCREEN

    BSF INTCON, T0IE	    ; Enable Timer 0 IRQ
    BSF INTCON, GIE	    ; Global interrupt enable


; Main loop
MAIN:
    GOTO MAIN


; ------------------------------------------------------------------------------
;
; Clears the screen and resets the cursor position
;
; ------------------------------------------------------------------------------
CLEAR_SCREEN:
    CLRF CHAR_POS

    MOVLW (CHARS + 0x1F)    ; Set FSR to END of character array
    MOVWF FSR

    MOVLW (0X100 - 0x20)    ; 32 chars

CLEAR_LOOP:
    CLRF INDF		    ; Clear array spot
    DECF FSR		    ; Next spot
    ADDLW 0x01
    BTFSS STATUS, Z	    ; Are we done?
    GOTO CLEAR_LOOP	    ; No.

    MOVLW CHARS		    ; Yes, set FSR to beginning of char array
    MOVWF FSR

    RETURN		    ; Done


; ------------------------------------------------------------------------------
;
; Scrolls the screen one line, cursor is moved up one line
;
; ------------------------------------------------------------------------------
SCROLL_SCREEN:
    MOVLW 8
    SUBWF CHAR_POS, W	    ; Is the cursor in the first line?
    BTFSS STATUS, C
    GOTO SCROLL_COPY	    ; Yes
			    ; No, move it up one line
    MOVLW 8
    SUBWF CHAR_POS
SCROLL_COPY:

    MOVLW (CHARS + 8)	    ; Move FST to second line
    MOVWF FSR

SCROLL_NEXT_CHAR:
    MOVF INDF, W	    ; Get character
    MOVWF SC_CHAR	    ; Save character
    MOVLW 0x08		    ; Offset write pointer
    SUBWF FSR
    MOVF SC_CHAR, W	    ; Move the character
    MOVWF INDF
    MOVLW 0x09		    ; Increment read pointer to next char
    ADDWF FSR
    MOVLW (CHARS + 0x20)    ; Get value of CHARS buffer end address
    SUBWF FSR, W
    BTFSS STATUS, C	    ; Are we done copying?
    GOTO SCROLL_NEXT_CHAR   ; No
			    ; Yes

    MOVLW 0x08		    ; Now clear the last line
    DECF FSR		    ; Move FSR to end of CHARS array
SCROLL_BLANK_LINE:
    CLRF INDF		    ; Reset char to a space
    DECF FSR		    ; Next char
    ADDLW 0xFF		    ; "DECW"
    BTFSS STATUS, Z
    GOTO SCROLL_BLANK_LINE

    RETURN


; ------------------------------------------------------------------------------
;
; Character bit-pattern look-up table.
; Character code is read from INDF
; Character codes are right-justified, 5 bits
; CHAR_ROW must contain the current sub-character pixel row
; Character bit patterns are returned via the W register
;
; ------------------------------------------------------------------------------

; -pCHAR_LUT=1F8h
; If the address of this is changed, update the bits set in CHAR_LUT for the bank
    PSECT CHAR_LUT,global,class=CODE,delta=2
CHAR_LUT:
    CLRF PCLATH
    BTFSC INDF, 0	; Check which character bank we are in
    BSF PCLATH, 0	; High bank if carry clear
    BSF PCLATH, 1	; Low bank otherwise

    MOVF INDF, W	; Get character
    ANDLW 0xF8		; Only want upper 5 bits
    ADDWF CHAR_ROW, W	; Get row
    ADDWF PCL

    ; ' ' [Space]
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000

    ; !
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000

    ; "
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b01010000
    RETLW 0b01010000
    RETLW 0b01010000

    ; #
    RETLW 0b00000000
    RETLW 0b01010000
    RETLW 0b01010000
    RETLW 0b11111000
    RETLW 0b01010000
    RETLW 0b11111000
    RETLW 0b01010000
    RETLW 0b01010000

    ; $
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b11110000
    RETLW 0b00101000
    RETLW 0b01110000
    RETLW 0b10100000
    RETLW 0b01111000
    RETLW 0b00100000

    ; %
    RETLW 0b00000000
    RETLW 0b00011000
    RETLW 0b10011000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b11001000
    RETLW 0b11000000

    ; &
    RETLW 0b00000000
    RETLW 0b01101000
    RETLW 0b10010000
    RETLW 0b10101000
    RETLW 0b01000000
    RETLW 0b10100000
    RETLW 0b10010000
    RETLW 0b01100000

    ; '
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b01100000

    ; (
    RETLW 0b00000000
    RETLW 0b00010000
    RETLW 0b00100000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000

    ; )
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00100000
    RETLW 0b01000000

    ; *
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b10101000
    RETLW 0b01110000
    RETLW 0b10101000
    RETLW 0b00100000
    RETLW 0b00000000

        ; +
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b11111000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00000000

    ; ,
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b01100000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000

        ; -
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000

    ; .
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b01100000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000

    ; /
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b10000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b00000000

    ; 0
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b11001000
    RETLW 0b10101000
    RETLW 0b10011000
    RETLW 0b10001000
    RETLW 0b01110000

    ; 1
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b01100000
    RETLW 0b00100000

    ; 2
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; 3
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b00001000
    RETLW 0b00010000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b11111000

    ; 4
    RETLW 0b00000000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b11111000
    RETLW 0b10010000
    RETLW 0b01010000
    RETLW 0b00110000
    RETLW 0b00010000

    ; 5
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b00001000
    RETLW 0b00001000
    RETLW 0b11110000
    RETLW 0b10000000
    RETLW 0b11111000

    ; 6
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b11110000
    RETLW 0b10000000
    RETLW 0b01000000
    RETLW 0b00110000

    ; 7
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b11111000

    ; 8
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; 9
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b01111000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; :
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b01100000
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b01100000
    RETLW 0b00000000

    ; ;
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b01100000
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b01100000
    RETLW 0b00000000

    ; <
    RETLW 0b00000000
    RETLW 0b00010000
    RETLW 0b00100000
    RETLW 0b01000000
    RETLW 0b10000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000

    ; =
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b00000000
    RETLW 0b00000000

    ; >
    RETLW 0b00000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b00010000
    RETLW 0b00100000
    RETLW 0b01000000

    ; ?
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; @
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10101000
    RETLW 0b10101000
    RETLW 0b01101000
    RETLW 0b00001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; A
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b11111000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000;

    ; B
    RETLW 0b00000000
    RETLW 0b11110000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b01110000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b11110000

    ; C
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10001000
    RETLW 0b01110000

    ; D
    RETLW 0b00000000
    RETLW 0b11110000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b01001000
    RETLW 0b11110000

    ; E
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b11110000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b11111000

    ; F
    RETLW 0b00000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b11110000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b11111000

    ; G
    RETLW 0b00000000
    RETLW 0b01111000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10011000
    RETLW 0b10000000
    RETLW 0b10001000
    RETLW 0b01110000

    ; H
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b11111000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; I
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b01110000

    ; J
    RETLW 0b00000000
    RETLW 0b01100000
    RETLW 0b10010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00111000

    ; K
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10010000
    RETLW 0b10100000
    RETLW 0b11000000
    RETLW 0b10100000
    RETLW 0b10010000
    RETLW 0b10001000

    ; L
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000

    ; M
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10101000
    RETLW 0b10101000
    RETLW 0b11011000
    RETLW 0b10001000

    ; N
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10011000
    RETLW 0b10101000
    RETLW 0b11001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; O
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; P
    RETLW 0b00000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b10000000
    RETLW 0b11110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b11110000

    ; Q
    RETLW 0b00000000
    RETLW 0b01101000
    RETLW 0b10010000
    RETLW 0b10101000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01110000

    ; R
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10010000
    RETLW 0b10100000
    RETLW 0b11110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b11110000

    ; S
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b00001000
    RETLW 0b01110000
    RETLW 0b10000000
    RETLW 0b10001000
    RETLW 0b01110000

    ; T
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b11111000

    ; U
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; V
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b01010000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; W
    RETLW 0b00000000
    RETLW 0b01010000
    RETLW 0b10101000
    RETLW 0b10101000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; X
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b01010000
    RETLW 0b00100000
    RETLW 0b01010000
    RETLW 0b10001000
    RETLW 0b10001000

    ; Y
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b01010000
    RETLW 0b10001000
    RETLW 0b10001000
    RETLW 0b10001000

    ; Z
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b10000000
    RETLW 0b01000000
    RETLW 0b00100000
    RETLW 0b00010000
    RETLW 0b00001000
    RETLW 0b11111000

    ; [
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01000000
    RETLW 0b01110000

    ; |	 (Actually the character code for \ but I wanted it for drawing boxes)
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00000000
    RETLW 0b00100000
    RETLW 0b00100000
    RETLW 0b00100000

    ; ]
    RETLW 0b00000000
    RETLW 0b01110000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b00010000
    RETLW 0b01110000

    ; ^
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b10001000
    RETLW 0b01010000
    RETLW 0b00100000

    ; _
    RETLW 0b00000000
    RETLW 0b11111000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000
    RETLW 0b00000000

end
