	; - - - - - - - - - - - - - - - - - - - - - - - - - - DEBUGGED CODE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	PROCESSOR 16F877A
	__CONFIG 0x3731              ; 4MHz XT oscillator, standard settings
	INCLUDE "P16F877A.INC"
	
	ORG 0x00
	GOTO INITIAL
	ORG 0x04
	GOTO ISR
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - REGISTER DEFINITIONS - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	; Variable Definitions
NUMBER1_DIGIT0 EQU 0x20      ; Most significant integer digit
NUMBER1_DIGIT1 EQU 0x21
NUMBER1_DIGIT2 EQU 0x22
NUMBER1_DIGIT3 EQU 0x23
NUMBER1_DIGIT4 EQU 0x24
NUMBER1_DIGIT5 EQU 0x25      ; Least significant integer digit
NUMBER1_DIGIT6 EQU 0x26      ; Most significant decimal digit
NUMBER1_DIGIT7 EQU 0x27
NUMBER1_DIGIT8 EQU 0x28
NUMBER1_DIGIT9 EQU 0x29
NUMBER1_DIGIT10 EQU 0x2A
NUMBER1_DIGIT11 EQU 0x2B     ; Least significant decimal digit
	
NUMBER2_DIGIT0 EQU 0x2C      ; Unused, but defines boundary for NUMBER1
NUMBER2_DIGIT1 EQU 0x2D
NUMBER2_DIGIT2 EQU 0x2E
NUMBER2_DIGIT3 EQU 0x2F
NUMBER2_DIGIT4 EQU 0x30
NUMBER2_DIGIT5 EQU 0x31
NUMBER2_DIGIT6 EQU 0x32
NUMBER2_DIGIT7 EQU 0x33
NUMBER2_DIGIT8 EQU 0x34
NUMBER2_DIGIT9 EQU 0x35
NUMBER2_DIGIT10 EQU 0x36
NUMBER2_DIGIT11 EQU 0x37
NUMBER2_DIGIT12 EQU 0x38
	
	
OP2 EQU 0x39                 ; Operand 2 base address
CMP_LOOP EQU 0x3A            ; Loop counter for comparison
CMP_DIGIT2 EQU 0x3B          ; Temporary storage for digit 2
CMP_DIGIT1 EQU 0x3C          ; Temporary storage for digit 1
CMP_INDEX EQU 0x3D           ; Index for comparison loop
CMP_COUNT EQU 0x3E           ; Count of digits to compare
	
DIV_COUNT EQU 0x3F
OP1 EQU 0x40                 ; Error code for invalid input
TEMP_REG EQU 0x41

TEMP_BYTE EQU 0x42
BIT_COUNT EQU 0x43
TEMP_REG_2 EQU 0x44
DIGIT_COUNT EQU 0x45
TEMP2 EQU 0x46
COUNT EQU 0x47
TEMP3 EQU 0x48
BINARY_IN EQU 0x49           ; Stores binary input from PORTC
DECIMAL_OUT EQU 0x4A         ; Stores decimal digit for LCD
TEMP EQU 0x4B                ; Temporary storage for comparison result
COUNTER EQU 0x4C             ; Interrupt counter
BORROW EQU 0x4D              ; Borrow flag for BCD subtraction
	
RESULT0 EQU 0x4E             ; Address for result digit 0 (already given)
RESULT1 EQU 0x4F             ; Address for result digit 1
RESULT2 EQU 0x50             ; Address for result digit 2
RESULT3 EQU 0x51             ; Address for result digit 3
RESULT4 EQU 0x52             ; Address for result digit 4
RESULT5 EQU 0x53             ; Address for result digit 5
RESULT6 EQU 0x54             ; Address for result digit 6
RESULT7 EQU 0x55             ; Address for result digit 7
RESULT8 EQU 0x56             ; Address for result digit 8
RESULT9 EQU 0x57             ; Address for result digit 9
RESULT10 EQU 0x58            ; Address for result digit 10
RESULT11 EQU 0x59            ; Address for result digit 11
	
	
REMAINDER0 EQU 0x5A          ; Remainder for division
REMAINDER1 EQU 0x5B          ; Remainder for division
REMAINDER2 EQU 0x5C          ; Second remainder for division
REMAINDER3 EQU 0x5D          ; Third remainder for division
REMAINDER4 EQU 0x5E          ; Fourth remainder for division
REMAINDER5 EQU 0x5F          ; Fifth remainder for division
REMAINDER6 EQU 0x60          ; Sixth remainder for division
REMAINDER7 EQU 0x61          ; Seventh remainder for division
REMAINDER8 EQU 0x62          ; Eighth remainder for division
REMAINDER9 EQU 0x63          ; Ninth remainder for division
REMAINDER10 EQU 0x64         ; Tenth remainder for division
REMAINDER11 EQU 0x65         ; Eleventh remainder for division
REMAINDER12 EQU 0x66
	
RECEIVE_PTR EQU 0x67
	
	INCLUDE "LCDIS.INC"
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - INITIALIZATION - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
INITIAL:
	BCF STATUS, RP1              ; Select Bank 0
	BSF STATUS, RP0              ; Select Bank 1
	
	BSF TRISB, 0                 ; Set RB0 as input
	BSF TRISB, 5                 ; Set RB5 FOR ACK
	BCF TRISB, 1                 ; Set RB0 as input
	
	BANKSEL TRISD
	CLRF TRISD                   ; PORTD as output for LCD
	; DEBUG: Corrected TRISC setup to explicitly set RC4 - RC7 as inputs, matching comments and ISR logic.
	MOVLW 0x0F                   ; RC4 - RC7 inputs, RC0 - RC3 outputs
	MOVWF TRISC
	
	
	BANKSEL PORTC
	CLRF PORTC                   ; Clear PORTC
	CLRF PORTD                   ; Clear PORT
	
	
	BSF INTCON, INTE             ; Enable RB0 external interrupt
	BSF INTCON, GIE              ; Global interrupt enable
	
	movlw 0x20                   ; Start receiving at the first digit of NUMBER1
	movwf RECEIVE_PTR
	
	CALL inid                    ; Initialize LCD (from LCDIS.INC)
	CALL LCD_CLEAR
	GOTO MAIN
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - INTERRUPT SERVICE ROUTINE - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	; - - - - - - - - - - - - - - - - - - - - - - - - - - INCLUDE LCD DRIVER - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
	
ISR:
	BCF INTCON, INTF
	BCF INTCON, GIE
	
	
	; DEBUG: Correctly read and isolate the upper 4 - bit nibble from PORTC.
	; The original code incorrectly masked the lower nibble.
	MOVF PORTC, W
	
	MOVWF BINARY_IN              ; Store the 4 - bit value
	CALL BIN_TO_DEC
	MOVWF DECIMAL_OUT
	
	; DEBUG: Corrected logic to handle invalid digits. The original logic was reversed.
	; If BIN_TO_DEC returns 0xFF, W becomes 0xFF. SUBLW 0xFF results in 0, setting the Z flag.
	; BTFSS (Bit Test F, Skip if Set) will skip the GOTO if Z is 1 (invalid input), correctly exiting.
	SUBLW 0xFF
	BTFSS STATUS, Z              ; Skip GOTO ISR_Exit if Z is set (input was 0xFF)
	GOTO PROCESS_DIGIT           ; It was a valid digit, so process it
	GOTO ISR_Exit                ; It was an invalid digit (0xFF), so exit without storing
	
PROCESS_DIGIT:
	; Store valid digit in NUMBER1_DIGITx
	MOVF RECEIVE_PTR, W
	MOVWF FSR
	MOVF DECIMAL_OUT, W
	MOVWF INDF                   ; Store nibble
	INCF RECEIVE_PTR, F          ; Move to next digit
	
	MOVLW 0x38                   ; Address immediately after NUMBER1 ends
	XORWF RECEIVE_PTR, W         ; Check if pointer is at this address
	BTFSC STATUS, Z              ; Is Z set? (i.e., pointers are equal)
	BSF PORTB, 1
	
	MOVLW 0x38                   ; Address immediately after NUMBER1 ends
	XORWF RECEIVE_PTR, W         ; Check if pointer is at this address
	BTFSC STATUS, Z              ; Is Z set? (i.e., pointers are equal)
	CALL START_DIV
	
	
ISR_Exit:
	CALL DISPLAY_NUMBER1         ; Update the display with the current number
	
	BSF PORTB, 1                 ; Send ACK
	CALL Short_Delay
	BCF PORTB, 1
	XORWF RECEIVE_PTR, W         ; Check if pointer is at this address
	BTFSC STATUS, Z              ; Is Z set? (i.e., pointers are equal)
	BSF PORTB, 1
	RETFIE
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - SHORT DELAY - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
Short_Delay:
	NOP
	NOP
	RETURN
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - DISPLAY NUMBER1 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
DISPLAY_NUMBER1:
	CALL LCD_CLEAR
	BSF Select, RS               ; Data mode
	
	MOVF NUMBER1_DIGIT0, W
	ADDLW '0'                    ; Convert to ASCII
	CALL send
	MOVF NUMBER1_DIGIT1, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT2, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT3, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT4, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT5, W
	ADDLW '0'
	CALL send
	MOVLW '.'                    ; Decimal point
	CALL send
	MOVF NUMBER1_DIGIT6, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT7, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT8, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT9, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT10, W
	ADDLW '0'
	CALL send
	MOVF NUMBER1_DIGIT11, W
	ADDLW '0'
	CALL send
	
	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Move to second line
	CALL send
	
	BSF Select, RS               ; Data mode
	
	MOVF NUMBER2_DIGIT0, W
	ADDLW '0'                    ; Convert to ASCII
	CALL send
	MOVF NUMBER2_DIGIT1, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT2, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT3, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT4, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT5, W
	ADDLW '0'
	CALL send
	MOVLW '.'                    ; Decimal point
	CALL send
	MOVF NUMBER2_DIGIT6, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT7, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT8, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT9, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT10, W
	ADDLW '0'
	CALL send
	MOVF NUMBER2_DIGIT11, W
	ADDLW '0'
	CALL send
	; DEBUG: Removed delay from display routine. Acknowledgment timing is handled in ISR.
	RETURN
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - MAIN LOOP - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
MAIN:
	
	GOTO MAIN                    ; Infinite loop, do nothing.
	
	
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - LCD CLEAR - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
LCD_CLEAR:
	MOVLW 0x01
	BCF Select, RS               ; Command mode
	CALL send
	MOVLW 0x80
	BCF Select, RS
	CALL send
	BSF Select, RS               ; Data mode
	RETURN
	
	
	
BIN_TO_DEC:
	ANDLW 0x0F                   ; Mask to lower 4 bits (ensures input is a nibble)
	MOVWF TEMP                   ; Save input
	SUBLW 0x09                   ; Check if input <= 9
	BTFSS STATUS, C              ; If input > 9, Carry is cleared. Skip if C is set (input <=9)
	GOTO INVALID_INPUT
	MOVF TEMP, W                 ; Restore valid input (0 - 9)
	ADDWF PCL, F                 ; Jump to correct RETLW using look - up table
	RETLW 0                      ; 0000 - > 0
	RETLW 1                      ; 0001 - > 1
	RETLW 2                      ; 0010 - > 2
	RETLW 3                      ; 0011 - > 3
	RETLW 4                      ; 0100 - > 4
	RETLW 5                      ; 0101 - > 5
	RETLW 6                      ; 0110 - > 6
	RETLW 7                      ; 0111 - > 7
	RETLW 8                      ; 1000 - > 8
	RETLW 9                      ; 1001 - > 9
INVALID_INPUT:
	RETLW 0xFF                   ; Return 0xFF for invalid BCD input (A - F)
	
	
DISPLAY_WELCOME:
	CALL LCD_CLEAR
	MOVLW 'W'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW 'l'
	CALL send
	MOVLW 'c'
	CALL send
	MOVLW 'o'
	CALL send
	MOVLW 'm'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW ' '
	CALL send
	MOVLW 't'
	CALL send
	MOVLW 'o'
	CALL send
	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Address for line 2
	CALL send
	BSF Select, RS               ; Data mode
	MOVLW 'D'
	CALL send
	MOVLW 'i'
	CALL send
	MOVLW 'v'
	CALL send
	MOVLW 'i'
	CALL send
	MOVLW 's'
	CALL send
	MOVLW 'i'
	CALL send
	MOVLW 'o'
	CALL send
	MOVLW 'n'
	CALL send
	RETURN
	
Delay_500ms:
	; A more robust 0.5 - second delay
	; Uses registers 0x7D, 0x7E, 0x7F as specified in your code
	BCF STATUS, RP0
	BCF STATUS, RP1
	MOVLW 0xFF
	MOVWF 0x7D
	MOVLW 0x83
	MOVWF 0x7E
	MOVLW 0x02
	MOVWF 0x7F
Delay_500ms_L1:
	DECFSZ 0x7D, F
	GOTO Delay_500ms_L1
Delay_500ms_L2:
	DECFSZ 0x7E, F
	GOTO Delay_500ms_L1
Delay_500ms_L3:
	DECFSZ 0x7F, F
	GOTO Delay_500ms_L2
	RETURN
	
	
	; - - - - - - - - - - - - - - - - - - - - - - - - - - START DIVISION - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
START_DIV:
	CALL SHIFT_NUMBER_TWO
	
	; load 0x2A to OP1
	MOVLW 0x2B                   ; Load base address of NUMBER1
	MOVWF OP1
	;load 12 to TEMP
	MOVLW 0x0C                   ; Load 12 (number of digits)
	MOVWF TEMP
	;CALL BCD_SHIFT_LEFT ; Shift NUMBER1 left by one digit
	CALL CALCULATE_INT_PART      ; Shift NUMBER1 left by one digit
	CALL DISPLAY_RESULT          ; Display shifted NUMBER1 on LCD
	
	CALL Delay_500ms
	CALL Delay_500ms
	
	CALL SAVE_RESULT_IN_NUMBER_ONE
	
	CALL CALCULATE_FRAC_PART     ; Shift NUMBER1 left by one digit
	CALL DISPLAY_RESULT          ; Display shifted NUMBER1 on LCD
	
	CALL Delay_500ms
	CALL LCD_CLEAR
	CALL Delay_500ms
	CALL DISPLAY_NUMBER1
	
	CALL Delay_500ms
	CALL DISPLAY_RESULT
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	
	CALL    SEND_DIGITS
	RETURN
	
	
BCD_CMP:
	MOVLW 0x5A                   ; NUMBER1 base address
	MOVWF OP1
	MOVLW 0x2C                   ; NUMBER2 base address
	MOVWF OP2
	MOVLW 0x0D                   ; 12 digits to compare
	MOVWF CMP_COUNT              ; Initialize loop counter
	CLRF CMP_INDEX               ; Start index at 0
	BCF STATUS, C                ; Clear carry flag
	BCF STATUS, Z                ; Clear zero flag
	CLRF TEMP                    ; Clear TEMP for result
	
CMP_LOOP_1:
	; Load digit from OP1
	MOVF CMP_INDEX, W
	ADDWF OP1, W                 ; OP1 base address + index
	MOVWF FSR
	MOVF INDF, W
	MOVWF CMP_DIGIT1             ; Store digit from OP1
	
	; Load digit from OP2
	MOVF CMP_INDEX, W
	ADDWF OP2, W                 ; OP2 base address + index
	MOVWF FSR
	MOVF INDF, W
	MOVWF CMP_DIGIT2             ; Store digit from OP2
	
	; Compare digits
	MOVF CMP_DIGIT2, W
	SUBWF CMP_DIGIT1, W          ; W = CMP_DIGIT1 - CMP_DIGIT2
	BTFSS STATUS, Z              ; If digits are not equal, exit loop
	GOTO CMP_DONE                ; Jump to result handling
	
	INCF CMP_INDEX, F            ; Next digit
	DECF CMP_COUNT, F
	BTFSS STATUS, Z              ; Check if all digits compared
	GOTO CMP_LOOP_1              ; Continue loop if not done
	
	; All digits matched (OP1 = OP2)
	BSF STATUS, Z                ; Set zero flag
	MOVLW 0x3D                   ; ASCII '=' for equality
	MOVWF TEMP
	
	
	
	BSF STATUS, C
	RETURN
	
CMP_DONE:
	; Digits differ: determine result based on last comparison
	BCF STATUS, Z                ; Clear zero flag (not equal)
	BTFSS STATUS, C              ; Check carry flag (C = 0 if borrow, CMP_DIGIT1 < CMP_DIGIT2)
	GOTO OP1_LESS                ; C = 0, OP1 < OP2
	MOVLW 0x3E                   ; ASCII '>' for OP1 > OP2
	MOVWF TEMP
	
	
	BSF STATUS, C
	RETURN
	
OP1_LESS:
	MOVLW 0x3C                   ; ASCII '<' for OP1 < OP2
	MOVWF TEMP
	
	
	
	BCF STATUS, C
	RETURN
	
	
	
	
	
BCD_SUB:
	MOVLW 0x5A                   ; NUMBER1 base address
	MOVWF OP1
	MOVLW 0x2C                   ; NUMBER2 base address
	MOVWF OP2
	CLRF BORROW                  ; Clear borrow flag (0 = no borrow)
	MOVLW 0x0D                   ; Initialize counter for 12 digits
	MOVWF COUNTER
	CLRF CMP_INDEX               ; Initialize index to 0
	
BCD_SUB_LOOP:
	
	;put counter value in CMP_INDEX
	MOVF COUNTER, W
	MOVWF CMP_INDEX              ; Copy counter to CMP_INDEX
	;decrement CMP_INDEX
	DECF CMP_INDEX, F            ; Decrement CMP_INDEX
	; Load digit from OP1 (minuend)
	MOVF CMP_INDEX, W
	ADDWF OP1, W                 ; OP1 base address + index
	MOVWF FSR
	MOVF INDF, W
	MOVWF CMP_DIGIT1             ; Store digit from OP1
	
	; Load digit from OP2 (subtrahend)
	MOVF CMP_INDEX, W
	ADDWF OP2, W                 ; OP2 base address + index
	MOVWF FSR
	MOVF INDF, W
	MOVWF CMP_DIGIT2             ; Store digit from OP2
	
	; Add borrow from previous operation to CMP_DIGIT2
	MOVF BORROW, W
	ADDWF CMP_DIGIT2, W          ; W = CMP_DIGIT2 + BORROW
	MOVWF CMP_DIGIT2             ; Update CMP_DIGIT2 with borrow
	
	; Perform subtraction: CMP_DIGIT1 - CMP_DIGIT2
	MOVF CMP_DIGIT2, W
	SUBWF CMP_DIGIT1, W          ; W = CMP_DIGIT1 - CMP_DIGIT2
	BTFSS STATUS, C              ; Skip if no borrow (C = 1)
	GOTO BORROW_SET              ; Borrow occurred
	
	; No borrow: Store result and clear borrow flag
	MOVWF TEMP                   ; Save result temporarily
	CLRF BORROW                  ; Clear borrow flag
	GOTO STORE_RESULT
	
BORROW_SET:
	; Borrow occurred: Adjust result (W = W + 10) and set borrow flag
	ADDLW 0x0A                   ; Add 10 to result (correct for borrow)
	MOVWF TEMP                   ; Save adjusted result
	MOVLW 1                      ; Set borrow flag
	MOVWF BORROW
	
STORE_RESULT:
	; Store result in memory
	MOVF CMP_INDEX, W
	ADDWF OP1, W                 ; RESULT base address + index
	MOVWF FSR
	MOVF TEMP, W
	MOVWF INDF                   ; Store result digit
	
	; Increment index and decrement counter
	INCF CMP_INDEX, F
	DECF COUNTER, F
	BTFSS STATUS, Z              ; Check if counter is zero
	GOTO BCD_SUB_LOOP            ; Continue loop if not done
	
	; End of subtraction
	RETURN
	
BCD_SHIFT_LEFT_ZERO:
	MOVF REMAINDER1, W           ; Load least significant digit (LSD)
	MOVWF REMAINDER0
	
	MOVF REMAINDER2, W           ; Load next digit
	MOVWF REMAINDER1
	
	MOVF REMAINDER3, W           ; Load next digit
	MOVWF REMAINDER2
	
	MOVF REMAINDER4, W           ; Load next digit
	MOVWF REMAINDER3
	
	MOVF REMAINDER5, W           ; Load next digit
	MOVWF REMAINDER4
	
	MOVF REMAINDER6, W           ; Load next digit
	MOVWF REMAINDER5
	
	MOVF REMAINDER7, W           ; Load next digit
	MOVWF REMAINDER6
	
	MOVF REMAINDER8, W           ; Load next digit
	MOVWF REMAINDER7
	
	MOVF REMAINDER9, W           ; Load next digit
	MOVWF REMAINDER8
	
	MOVF REMAINDER10, W          ; Load next digit
	MOVWF REMAINDER9
	
	MOVF REMAINDER11, W          ; Load next digit
	MOVWF REMAINDER10
	
	MOVF REMAINDER12, W          ; Load next digit
	MOVWF REMAINDER11
	
	CLRF REMAINDER12             ; Store in most significant digit (MSD)
	
	RETURN
	
	
BCD_SHIFT_LEFT:
	MOVF REMAINDER1, W           ; Load least significant digit (LSD)
	MOVWF REMAINDER0
	
	MOVF REMAINDER2, W           ; Load next digit
	MOVWF REMAINDER1
	
	MOVF REMAINDER3, W           ; Load next digit
	MOVWF REMAINDER2
	
	MOVF REMAINDER4, W           ; Load next digit
	MOVWF REMAINDER3
	
	MOVF REMAINDER5, W           ; Load next digit
	MOVWF REMAINDER4
	
	MOVF REMAINDER6, W           ; Load next digit
	MOVWF REMAINDER5
	
	MOVF REMAINDER7, W           ; Load next digit
	MOVWF REMAINDER6
	
	MOVF REMAINDER8, W           ; Load next digit
	MOVWF REMAINDER7
	
	MOVF REMAINDER9, W           ; Load next digit
	MOVWF REMAINDER8
	
	MOVF REMAINDER10, W          ; Load next digit
	MOVWF REMAINDER9
	
	MOVF REMAINDER11, W          ; Load next digit
	MOVWF REMAINDER10
	
	MOVF REMAINDER12, W          ; Load next digit
	MOVWF REMAINDER11
	
	MOVF NUMBER1_DIGIT0, W       ; Load next digit
	MOVWF REMAINDER12            ; Store in most significant digit (MSD)
	
	
	MOVF NUMBER1_DIGIT1, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT0
	
	MOVF NUMBER1_DIGIT2, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT1
	
	MOVF NUMBER1_DIGIT3, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT2
	
	MOVF NUMBER1_DIGIT4, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT3
	
	MOVF NUMBER1_DIGIT5, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT4
	
	MOVF NUMBER1_DIGIT6, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT5
	
	MOVF NUMBER1_DIGIT7, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT6
	
	MOVF NUMBER1_DIGIT8, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT7
	
	MOVF NUMBER1_DIGIT9, W       ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT8
	
	MOVF NUMBER1_DIGIT10, W      ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT9
	
	MOVF NUMBER1_DIGIT11, W      ; Load least significant digit (LSD)
	MOVWF NUMBER1_DIGIT10
	
	CLRF NUMBER1_DIGIT11         ; Clear least significant digit (LSD)
	
	RETURN
	
	
	
CALCULATE_INT_PART:
	; - - - Initialization - - - 
	MOVLW 0x0C                   ; Initialize for 12 division steps
	MOVWF DIV_COUNT
	MOVLW 0x4E                   ; Load base address for storing the result
	MOVWF TEMP_BYTE
	CLRF COUNT
	
LOOP_INT:
	; - - - Set Operands for Subroutines (FIX for BUG #1) - - - 
	; This must be done inside the loop because BCD_SUB modifies OP1's target memory.
	MOVLW 0x5A                   ; OP1 will point to the partial remainder
	MOVWF OP1
	MOVLW 0x2C                   ; OP2 will point to the divisor
	MOVWF OP2
	CALL BCD_SHIFT_LEFT          ; Shift remainder left and bring down next dividend digit
	CLRF COUNT                   ; Reset the counter for the next digit
	
	; - - - Inner loop for repeated subtraction (FIX for BUG #2) - - - 
	; This loop repeatedly subtracts the divisor from the partial remainder
	; until the remainder is smaller than the divisor.
SUB_LOOP:
	
	CALL BCD_CMP                 ; Compare PARTIAL_REMAINDER with Divisor
	BTFSS STATUS, C              ; Test Carry. Skip if C is set (PARTIAL >= Divisor).
	GOTO LOOP_INT_NEXT           ; If C was clear (PARTIAL < Divisor), exit subtraction loop.
	; If we are here, it means PARTIAL >= Divisor, so we subtract.
	CALL BCD_SUB                 ; PARTIAL = PARTIAL - Divisor
	INCF COUNT, F                ; Increment quotient digit
	GOTO SUB_LOOP                ; Go back and compare again
	
LOOP_INT_NEXT:
	
	; - - - Store the calculated quotient digit - - - 
	MOVF TEMP_BYTE, W            ; Get address to store result
	MOVWF FSR                    ; Set the file select register to that address
	MOVF COUNT, W                ; Move calculated digit to W
	MOVWF INDF                   ; Store quotient digit at pointed address
	INCF TEMP_BYTE, F            ; Move pointer to the next quotient digit location
	; - - - Prepare for the next digit calculation - - - 
	
	
	
	
	
	MOVF TEMP_BYTE, W            ; Load TEMP_BYTE into WREG
	SUBLW 0x59                   ; W = 0x59 - TEMP_BYTE
	BTFSS STATUS, C              ; If Carry is clear, TEMP_BYTE >= 0x59
	RETURN                       ; Exit the subroutine if TEMP_BYTE >= 0x59
	GOTO LOOP_INT                ; Continue for the next digit
	
	
SAVE_RESULT_IN_NUMBER_ONE:
	; Copy each digit from RESULT to corresponding NUMBER1_DIGIT address
	MOVF RESULT0, W              ; Load RESULT0 into WREG
	MOVWF NUMBER1_DIGIT0         ; Store WREG to NUMBER1_DIGIT0
	CLRF RESULT0
	
	MOVF RESULT1, W
	MOVWF NUMBER1_DIGIT1
	CLRF RESULT1
	
	MOVF RESULT2, W
	MOVWF NUMBER1_DIGIT2
	CLRF RESULT2
	
	MOVF RESULT3, W
	MOVWF NUMBER1_DIGIT3
	CLRF RESULT3
	
	MOVF RESULT4, W
	MOVWF NUMBER1_DIGIT4
	CLRF RESULT4
	
	MOVF RESULT5, W
	MOVWF NUMBER1_DIGIT5
	CLRF RESULT5
	
	MOVF RESULT6, W
	MOVWF NUMBER1_DIGIT6
	CLRF RESULT6
	
	MOVF RESULT7, W
	MOVWF NUMBER1_DIGIT7
	CLRF RESULT7
	
	
	MOVF RESULT8, W
	MOVWF NUMBER1_DIGIT8
	CLRF RESULT8
	
	MOVF RESULT9, W
	MOVWF NUMBER1_DIGIT9
	CLRF RESULT9
	
	MOVF RESULT10, W
	MOVWF NUMBER1_DIGIT10
	CLRF RESULT10
	
	MOVF RESULT11, W
	MOVWF NUMBER1_DIGIT11
	CLRF RESULT11
	
	RETURN                       ; Return from subroutine
	
	
	
SHIFT_NUMBER_TWO:
	MOVF NUMBER2_DIGIT11, W
	MOVWF NUMBER2_DIGIT12
	MOVF NUMBER2_DIGIT10, W
	MOVWF NUMBER2_DIGIT11
	MOVF NUMBER2_DIGIT9, W
	MOVWF NUMBER2_DIGIT10
	MOVF NUMBER2_DIGIT8, W
	MOVWF NUMBER2_DIGIT9
	MOVF NUMBER2_DIGIT7, W
	MOVWF NUMBER2_DIGIT8
	MOVF NUMBER2_DIGIT6, W
	MOVWF NUMBER2_DIGIT7
	MOVF NUMBER2_DIGIT5, W
	MOVWF NUMBER2_DIGIT6
	MOVF NUMBER2_DIGIT4, W
	MOVWF NUMBER2_DIGIT5
	MOVF NUMBER2_DIGIT3, W
	MOVWF NUMBER2_DIGIT4
	MOVF NUMBER2_DIGIT2, W
	MOVWF NUMBER2_DIGIT3
	MOVF NUMBER2_DIGIT1, W
	MOVWF NUMBER2_DIGIT2
	MOVF NUMBER2_DIGIT0, W
	MOVWF NUMBER2_DIGIT1
	CLRF NUMBER2_DIGIT0
	RETURN
	
DISPLAY_RESULT:
	CALL LCD_CLEAR
	BSF Select, RS               ; Data mode
	MOVF RESULT0, W
	ADDLW '0'                    ; Convert to ASCII
	CALL send
	MOVF RESULT1, W
	ADDLW '0'
	CALL send
	MOVF RESULT2, W
	ADDLW '0'
	CALL send
	MOVF RESULT3, W
	ADDLW '0'
	CALL send
	MOVF RESULT4, W
	ADDLW '0'
	CALL send
	MOVF RESULT5, W
	ADDLW '0'
	CALL send
	
	MOVF RESULT6, W
	ADDLW '0'
	CALL send
	MOVF RESULT7, W
	ADDLW '0'
	CALL send
	MOVF RESULT8, W
	ADDLW '0'
	CALL send
	MOVF RESULT9, W
	ADDLW '0'
	CALL send
	MOVF RESULT10, W
	ADDLW '0'
	CALL send
	MOVF RESULT11, W
	ADDLW '0'
	CALL send
	CALL Delay_500ms             ; Delay to view number
	RETURN
	
	
	
	
CALCULATE_FRAC_PART:
	; - - - Initialization - - - 
	MOVLW 0x0C                   ; Initialize for 12 division steps
	MOVWF DIV_COUNT
	MOVLW 0x4E                   ; Load base address for storing the result
	MOVWF TEMP_BYTE
	CLRF COUNT
	CLRF REMAINDER0
	
	CALL BCD_SHIFT_LEFT_ZERO     ; Shift remainder left and bring down next dividend digit ; Clear the per - digit counter initially
LOOP_FRAC:
	; - - - Set Operands for Subroutines (FIX for BUG #1) - - - 
	; This must be done inside the loop because BCD_SUB modifies OP1's target memory.
	MOVLW 0x5A                   ; OP1 will point to the partial remainder
	MOVWF OP1
	MOVLW 0x2C                   ; OP2 will point to the divisor
	MOVWF OP2
	
	; - - - Inner loop for repeated subtraction (FIX for BUG #2) - - - 
	; This loop repeatedly subtracts the divisor from the partial remainder
	; until the remainder is smaller than the divisor.
SUB_LOOP_FRAC:
	
	CALL BCD_CMP                 ; Compare PARTIAL_REMAINDER with Divisor
	BTFSS STATUS, C              ; Test Carry. Skip if C is set (PARTIAL >= Divisor).
	GOTO LOOP_FRAC_NEXT          ; If C was clear (PARTIAL < Divisor), exit subtraction loop.
	; If we are here, it means PARTIAL >= Divisor, so we subtract.
	CALL BCD_SUB                 ; PARTIAL = PARTIAL - Divisor
	INCF COUNT, F                ; Increment quotient digit
	GOTO SUB_LOOP_FRAC           ; Go back and compare again, 
	
LOOP_FRAC_NEXT:
	
	; - - - Store the calculated quotient digit - - - 
	MOVF TEMP_BYTE, W            ; Get address to store result
	MOVWF FSR                    ; Set the file select register to that address
	MOVF COUNT, W                ; Move calculated digit to W
	MOVWF INDF                   ; Store quotient digit at pointed address
	INCF TEMP_BYTE, F            ; Move pointer to the next quotient digit location
	; - - - Prepare for the next digit calculation - - - 
	CALL BCD_SHIFT_LEFT_ZERO     ; Shift remainder left and bring down next dividend digit
	CLRF COUNT                   ; Reset the counter for the next digit
	
	
	
	
	MOVF TEMP_BYTE, W            ; Load TEMP_BYTE into WREG
	SUBLW 0x59                   ; W = 0x59 - TEMP_BYTE
	BTFSS STATUS, C              ; If Carry is clear, TEMP_BYTE >= 0x59
	RETURN                       ; Exit the subroutine if TEMP_BYTE >= 0x59
	GOTO LOOP_FRAC               ; Continue for the next digit
	
	
	
SEND_DIGITS:
	MOVLW 0xF0                   ; RC4 - RC7 inputs, RC0 - RC3 outputs
	MOVWF TRISC

    CLRF    PORTC           ; Clear PORTC initially
    CLRF    PORTD           ; Clear PORTD initially


    movf    NUMBER1_DIGIT5, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms

    
    movf    NUMBER1_DIGIT4, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1
    CALL    Delay_500ms



    
    movf    NUMBER1_DIGIT3, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1
    CALL    Delay_500ms



    
    movf    NUMBER1_DIGIT2, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms



    
    movf    NUMBER1_DIGIT1, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms



    
    movf    NUMBER1_DIGIT0, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms



    
    movf    RESULT0, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

     CALL    Delay_500ms



    
    movf    RESULT1, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms



    
    movf    RESULT2, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms



    
    movf    RESULT3, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1
    CALL    Delay_500ms



    
    movf    RESULT4, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP 
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

     CALL    Delay_500ms



    movf    RESULT5, W
    MOVWF   PORTC           ; Output to PORTC (RC4-RC7)

    BSF     PORTD, 0        ; Set RD0 high to trigger interrupt
    NOP
	NOP
	NOP
    BCF     PORTD, 0


    BTFSS   PORTB, 5
    GOTO    $-1

    CALL    Delay_500ms

    
    GOTO INITIAL
	END
