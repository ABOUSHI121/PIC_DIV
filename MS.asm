; ================ TEST PROGRAM =====================
    LIST P=16F877A
    #include <p16f877a.inc>
    __CONFIG 0x3731

; ================ VARIABLE ALLOCATION ==============

; Number 1 input
DIGIT1      EQU 0x30
DIGIT2      EQU 0x31
DIGIT3      EQU 0x32
DIGIT4      EQU 0x33
DIGIT5      EQU 0x34
DIGIT6      EQU 0x35
DECIMAL1    EQU 0x36
DECIMAL2    EQU 0x37
DECIMAL3    EQU 0x38
DECIMAL4    EQU 0x39
DECIMAL5    EQU 0x3A
DECIMAL6    EQU 0x3B

; Number 2 input
DIGITB1     EQU 0x3C
DIGITB2     EQU 0x3D
DIGITB3     EQU 0x3E
DIGITB4     EQU 0x3F
DIGITB5     EQU 0x41
DIGITB6     EQU 0x42
DECIMALB1   EQU 0x43
DECIMALB2   EQU 0x44
DECIMALB3   EQU 0x45
DECIMALB4   EQU 0x46
DECIMALB5   EQU 0x47
DECIMALB6   EQU 0x48
DECIMALB7   EQU 0x49

; 64-bit Results
RESULT0     EQU 0x50
RESULT1     EQU 0x51
RESULT2     EQU 0x52
RESULT3     EQU 0x53
RESULT4     EQU 0x54
RESULT5     EQU 0x55
RESULT6     EQU 0x56
RESULT7     EQU 0x57
RESULT8     EQU 0x58
RESULT9     EQU 0x59
RESULT10    EQU 0x5A
RESULT11    EQU 0x5B


REMAINDER0    EQU 0x70
REMAINDER1    EQU 0x71
REMAINDER2    EQU 0x72
REMAINDER3    EQU 0x73
REMAINDER4    EQU 0x74
REMAINDER5    EQU 0x75
REMAINDER6    EQU 0x76
REMAINDER7    EQU 0x77
REMAINDER8    EQU 0x79
REMAINDER9    EQU 0x7A
REMAINDER10   EQU 0x7B
REMAINDER11   EQU 0x7C
REMAINDER12   EQU 0x7D



CMP_LOOP        EQU 0x60
CMP_DIGIT2      EQU 0x61
CMP_DIGIT1      EQU 0x62
CMP_INDEX       EQU 0x63
CMP_COUNT       EQU 0x64
DIV_COUNT       EQU 0x65
TEMP            EQU 0x66
BORROW          EQU 0x67

POINTER_1   EQU 0x4A
POINTER_2   EQU 0x4B
BINARY_IN   EQU 0x4C
DECIMAL_OUT EQU 0x4D
RECEIVE_PTR EQU 0x4E

; ================ PROGRAM START ====================
    ORG 0x00
    GOTO INITIAL
    ORG 0x04
    GOTO ISR

;--------------------------INITIALIZATION-------------------------------
INITIAL:
    BANKSEL TRISC
    MOVLW 0xFF          ; PORTC as input (RC0-RC3 for binary input)
    MOVWF TRISC
    BANKSEL TRISB
    BSF TRISB, 0        ; RB0 as input for interrupt
    BSF TRISB, 5        ; RB5 as input for ACK
    BCF TRISB, 1
    CLRF TRISD          ; PORTD as output for LCD

    BANKSEL PORTC
    CLRF PORTC          ; Clear PORTC
    CLRF PORTD          ; Clear PORTD
    BSF INTCON, INTE    ; Enable RB0/INT external interrupt
    BSF INTCON, GIE     ; Enable global interrupts

    GOTO MAIN

;--------------------------TABLE FOR BINARY TO DECIMAL-------------------------------
BIN_TO_DEC:
    ANDLW 0x0F          ; Mask to lower 4 bits
    MOVWF TEMP          ; Save input
    SUBLW 0x09          ; Check if input <= 9
    BTFSS STATUS, C     ; If input > 9, return 0xFF
    GOTO INVALID_INPUT
    MOVF TEMP, W        ; Restore input
    ADDWF PCL, F
    RETLW 0             ; 0000 -> 0
    RETLW 1             ; 0001 -> 1
    RETLW 2             ; 0010 -> 2
    RETLW 3             ; 0011 -> 3
    RETLW 4             ; 0100 -> 4
    RETLW 5             ; 0101 -> 5
    RETLW 6             ; 0110 -> 6
    RETLW 7             ; 0111 -> 7
    RETLW 8             ; 1000 -> 8
    RETLW 9             ; 1001 -> 9
INVALID_INPUT:
    RETLW 0xFF          ; Invalid input




;--------------------------INTERRUPT SERVICE ROUTINE-------------------------------
ISR:
    BCF     INTCON, INTF
    BCF     INTCON, GIE

    MOVF    PORTC, W

    MOVWF   BINARY_IN       ; Store the 4-bit value
    CALL    BIN_TO_DEC
    MOVWF   DECIMAL_OUT

    SUBLW   0xFF
    BTFSS   STATUS, Z
    GOTO    PROCESS_DIGIT 
    GOTO    ISR_Exit

PROCESS_DIGIT:
    ; Store valid digit in NUMBER1_DIGITx
    MOVF    RECEIVE_PTR, W
    MOVWF   FSR
    MOVF    DECIMAL_OUT, W
    MOVWF   INDF            
    INCF    RECEIVE_PTR, F  


    MOVLW   0x49  
    XORWF   RECEIVE_PTR, W
    BTFSC   STATUS, Z
    MOVLW   0x30
    BTFSC   STATUS, Z
    MOVWF   RECEIVE_PTR

ISR_Exit:
    BSF     PORTB, 1        ; Send ACK
    CALL    Short_Delay
    BCF     PORTB, 1

    BSF     INTCON, INTF  
    BSF     INTCON, GIE    

    RETFIE

;--------------------------SHORT DELAY-------------------------------
Short_Delay:
    NOP
    NOP
    NOP
    NOP
    NOP
    RETURN


; ================ MAIN ============================
MAIN:
    MOVLW   0x49  
    XORWF   RECEIVE_PTR, W
    BTFSC   STATUS, Z
    GOTO    DIVISION
    GOTO    MAIN





DIVISION:
    BCF     INTCON, INTF
    BCF     INTCON, GIE

    GOTO    INITIAL



;--------------------------BCD COMPARISON ROUTINE-------------------------------
BCD_CMP:
    MOVLW   0x70        ; NUMBER1 base address
    MOVWF   POINTER_1
    MOVLW   0x3C        ; NUMBER2 base address
    MOVWF   POINTER_2
    MOVLW   0x0D        ; 13 digits to compare
    MOVWF   CMP_COUNT   ; Initialize loop counter
    CLRF    CMP_INDEX   ; Start index at 0
    BCF     STATUS, C   ; Clear carry flag
    BCF     STATUS, Z   ; Clear zero flag
    CLRF    TEMP        ; Clear TEMP for result

CMP_LOOP_1:
    ; Load digit from POINTER_1
    MOVF    CMP_INDEX, W
    ADDWF   POINTER_1, W      ; POINTER_1 base address + index
    MOVWF   FSR
    MOVF    INDF, W
    MOVWF   CMP_DIGIT1  ; Store digit from POINTER_1

    ; Load digit from POINTER_2
    MOVF    CMP_INDEX, W
    ADDWF   POINTER_2, W      ; POINTER_2 base address + index
    MOVWF   FSR
    MOVF    INDF, W
    MOVWF   CMP_DIGIT2  ; Store digit from POINTER_2

    ; Compare digits
    MOVF    CMP_DIGIT2, W
    SUBWF   CMP_DIGIT1, W ; W = CMP_DIGIT1 - CMP_DIGIT2
    BTFSS   STATUS, Z     ; If digits are not equal, exit loop
    GOTO    CMP_DONE      ; Jump to result handling

    INCF    CMP_INDEX, F  ; Next digit
    DECF    CMP_COUNT, F
    BTFSS   STATUS, Z     ; Check if all digits compared
    GOTO    CMP_LOOP_1    ; Continue loop if not done

    ; All digits matched (POINTER_1 = POINTER_2)
    BSF     STATUS, Z     ; Set zero flag
    BSF     STATUS, C
    RETURN

CMP_DONE:
    ; Digits differ: determine result based on last comparison
    BCF     STATUS, Z     ; Clear zero flag (not equal)
    BTFSS   STATUS, C     ; Check carry flag (C = 0 if borrow, CMP_DIGIT1 < CMP_DIGIT2)
    GOTO    POINTER_1_LESS      ; C = 0, POINTER_1 < POINTER_2
    BSF     STATUS, C
    RETURN

POINTER_1_LESS:
    BCF     STATUS, C
    RETURN





BCD_SUB:
    MOVLW   0x70         ; NUMBER1 base address
    MOVWF   POINTER_1
    MOVLW   0x3C        ; NUMBER2 base address
    MOVWF   POINTER_2
    CLRF    BORROW      ; Clear borrow flag (0 = no borrow)
    MOVLW   0x0D          ; Initialize counter for 13 digits
    MOVWF   COUNTER
    CLRF    CMP_INDEX   ; Initialize index to 0

BCD_SUB_LOOP:

    ;put counter value in CMP_INDEX
    MOVF    COUNTER, W
    MOVWF   CMP_INDEX   ; Copy counter to CMP_INDEX
    ;decrement CMP_INDEX
    DECF    CMP_INDEX, F ; Decrement CMP_INDEX
    ; Load digit from POINTER_1 (minuend)
    MOVF    CMP_INDEX, W
    ADDWF   POINTER_1, W      ; POINTER_1 base address + index
    MOVWF   FSR
    MOVF    INDF, W
    MOVWF   CMP_DIGIT1  ; Store digit from POINTER_1

    ; Load digit from POINTER_2 (subtrahend)
    MOVF    CMP_INDEX, W
    ADDWF   POINTER_2, W      ; POINTER_2 base address + index
    MOVWF   FSR
    MOVF    INDF, W
    MOVWF   CMP_DIGIT2  ; Store digit from POINTER_2

    ; Add borrow from previous operation to CMP_DIGIT2
    MOVF    BORROW, W
    ADDWF   CMP_DIGIT2, W ; W = CMP_DIGIT2 + BORROW
    MOVWF   CMP_DIGIT2  ; Update CMP_DIGIT2 with borrow

    ; Perform subtraction: CMP_DIGIT1 - CMP_DIGIT2
    MOVF    CMP_DIGIT2, W
    SUBWF   CMP_DIGIT1, W ; W = CMP_DIGIT1 - CMP_DIGIT2
    BTFSS   STATUS, C   ; Skip if no borrow (C = 1)
    GOTO    BORROW_SET  ; Borrow occurred

    ; No borrow: Store result and clear borrow flag
    MOVWF   TEMP        ; Save result temporarily
    CLRF    BORROW      ; Clear borrow flag
    GOTO    STORE_RESULT

BORROW_SET:
    ; Borrow occurred: Adjust result (W = W + 10) and set borrow flag
    ADDLW   0x0A        ; Add 10 to result (correct for borrow)
    MOVWF   TEMP        ; Save adjusted result
    MOVLW   1           ; Set borrow flag
    MOVWF   BORROW

STORE_RESULT:
    ; Store result in memory
    MOVF    CMP_INDEX, W
    ADDWF   POINTER_1, W ; RESULT base address + index
    MOVWF   FSR
    MOVF    TEMP, W
    MOVWF   INDF        ; Store result digit

    ; Increment index and decrement counter
    INCF    CMP_INDEX, F
    DECF    COUNTER, F
    BTFSS   STATUS, Z   ; Check if counter is zero
    GOTO    BCD_SUB_LOOP ; Continue loop if not done

    ; End of subtraction
    RETURN

REMAINDER_SHIFT_LEFT_ZERO:
    MOVF  REMAINDER1, W ; Load least significant digit (LSD)
    MOVWF REMAINDER0

    MOVF  REMAINDER2, W ; Load next digit
    MOVWF REMAINDER1

    MOVF  REMAINDER3, W ; Load next digit
    MOVWF REMAINDER2    

    MOVF  REMAINDER4, W ; Load next digit
    MOVWF REMAINDER3

    MOVF  REMAINDER5, W ; Load next digit
    MOVWF REMAINDER4

    MOVF  REMAINDER6, W ; Load next digit
    MOVWF REMAINDER5

    MOVF  REMAINDER7, W ; Load next digit
    MOVWF REMAINDER6

    MOVF  REMAINDER8, W ; Load next digit
    MOVWF REMAINDER7

    MOVF  REMAINDER9, W ; Load next digit
    MOVWF REMAINDER8

    MOVF  REMAINDER10, W ; Load next digit
    MOVWF REMAINDER9

    MOVF  REMAINDER11, W ; Load next digit
    MOVWF REMAINDER10

    MOVF  REMAINDER12, W ; Load next digit
    MOVWF REMAINDER11

    CLRF  REMAINDER12 ; Store in most significant digit (MSD)

    RETURN


BCD_SHIFT_LEFT:
    MOVF  REMAINDER1, W ; Load least significant digit (LSD)
    MOVWF REMAINDER0

    MOVF  REMAINDER2, W ; Load next digit
    MOVWF REMAINDER1

    MOVF  REMAINDER3, W ; Load next digit
    MOVWF REMAINDER2    

    MOVF  REMAINDER4, W ; Load next digit
    MOVWF REMAINDER3

    MOVF  REMAINDER5, W ; Load next digit
    MOVWF REMAINDER4

    MOVF  REMAINDER6, W ; Load next digit
    MOVWF REMAINDER5

    MOVF  REMAINDER7, W ; Load next digit
    MOVWF REMAINDER6

    MOVF  REMAINDER8, W ; Load next digit
    MOVWF REMAINDER7

    MOVF  REMAINDER9, W ; Load next digit
    MOVWF REMAINDER8

    MOVF  REMAINDER10, W ; Load next digit
    MOVWF REMAINDER9

    MOVF  REMAINDER11, W ; Load next digit
    MOVWF REMAINDER10

    MOVF  REMAINDER12, W ; Load next digit
    MOVWF REMAINDER11

    MOVF   DIGIT1, W ; Load next digit
    MOVWF  REMAINDER12 ; Store in most significant digit (MSD)


    MOVF    DIGIT2, W ; Load least significant digit (LSD)
    MOVWF   DIGIT1

    MOVF    DIGIT3, W ; Load least significant digit (LSD)
    MOVWF   DIGIT2

    MOVF    DIGIT4, W ; Load least significant digit (LSD)
    MOVWF   DIGIT3

    MOVF    DIGIT5, W ; Load least significant digit (LSD)
    MOVWF   DIGIT4

    MOVF    DIGIT6, W ; Load least significant digit (LSD)
    MOVWF   DIGIT5

    MOVF    DECIMAL1, W ; Load least significant digit (LSD)
    MOVWF   DIGIT6

    MOVF    DECIMAL2, W ; Load least significant digit (LSD)
    MOVWF   DECIMAL1

    MOVF    DECIMAL3, W ; Load least significant digit (LSD)
    MOVWF   DECIMAL2

    MOVF    DECIMAL4, W ; Load least significant digit (LSD)
    MOVWF   DECIMAL3

    MOVF    DECIMAL5, W ; Load least significant digit (LSD)
    MOVWF   DECIMAL4

    MOVF    DECIMAL6, W ; Load least significant digit (LSD)
    MOVWF   DECIMAL5

    CLRF    DECIMAL6 ; Clear least significant digit (LSD)

    RETURN



CALCULATE_INT_PART:
    ; --- Initialization ---
    MOVLW   0x0C                    ; Initialize for 12 division steps
    MOVWF   DIV_COUNT
    MOVLW   0x4E     ; Load base address for storing the result
    MOVWF   TEMP_BYTE
    CLRF    COUNT   

LOOP_INT:
    ; --- Set Operands for Subroutines (FIX for BUG #1) ---
    ; This must be done inside the loop because BCD_SUB modifies POINTER_1's target memory.
    MOVLW   0x5A  ; POINTER_1 will point to the partial remainder
    MOVWF   POINTER_1
    MOVLW   0x2C            ; POINTER_2 will point to the divisor
    MOVWF   POINTER_2
        CALL    BCD_SHIFT_LEFT          ; Shift remainder left and bring down next dividend digit
    CLRF    COUNT                   ; Reset the counter for the next digit

; --- Inner loop for repeated subtraction (FIX for BUG #2) ---
; This loop repeatedly subtracts the divisor from the partial remainder
; until the remainder is smaller than the divisor.
SUB_LOOP:

    CALL    BCD_CMP                 ; Compare PARTIAL_REMAINDER with Divisor
    BTFSS   STATUS, C               ; Test Carry. Skip if C is set (PARTIAL >= Divisor).
    GOTO    LOOP_INT_NEXT           ; If C was clear (PARTIAL < Divisor), exit subtraction loop.
    ; If we are here, it means PARTIAL >= Divisor, so we subtract.
    CALL    BCD_SUB                 ; PARTIAL = PARTIAL - Divisor
    INCF    COUNT, F                ; Increment quotient digit
    GOTO    SUB_LOOP                ; Go back and compare again

LOOP_INT_NEXT:
    
    ; --- Store the calculated quotient digit ---
    MOVF    TEMP_BYTE, W         ; Get address to store result
    MOVWF   FSR                     ; Set the file select register to that address
    MOVF    COUNT, W                ; Move calculated digit to W
    MOVWF   INDF                    ; Store quotient digit at pointed address
    INCF    TEMP_BYTE, F         ; Move pointer to the next quotient digit location
    ; --- Prepare for the next digit calculation ---


 


    MOVF    TEMP_BYTE, W      ; Load TEMP_BYTE into WREG
    SUBLW   0x59              ; W = 0x59 - TEMP_BYTE
    BTFSS   STATUS, C         ; If Carry is clear, TEMP_BYTE >= 0x59
    RETURN                    ; Exit the subroutine if TEMP_BYTE >= 0x59
    GOTO    LOOP_INT                ; Continue for the next digit




DISPLAY_HERE:

    CALL LCD_CLEAR
    BSF Select, RS      ; Data mode
    MOVLW 'H'           ; Decimal point
    CALL send
        MOVLW 'E'           ; Decimal point
    CALL send
        MOVLW 'R'           ; Decimal point
    CALL send
        MOVLW 'E'           ; Decimal point
    CALL send
    CALL HALF_SECOND_SLEEP
    CALL HALF_SECOND_SLEEP

	RETURN

SAVE_RESULT_IN_NUMBER_ONE:
    ; Copy each digit from RESULT to corresponding NUMBER1_DIGIT address
    MOVF RESULT0, W          ; Load RESULT0 into WREG
    MOVWF NUMBER1_DIGIT0     ; Store WREG to NUMBER1_DIGIT0
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

    RETURN                   ; Return from subroutine














CALCULATE_FRAC_PART:
    ; --- Initialization ---
    MOVLW   0x0C                    ; Initialize for 12 division steps
    MOVWF   DIV_COUNT
    MOVLW   0x4E     ; Load base address for storing the result
    MOVWF   TEMP_BYTE
    CLRF    COUNT   
        CLRF    REMAINDER0 

    CALL    REMAINDER_SHIFT_LEFT_ZERO          ; Shift remainder left and bring down next dividend digit                ; Clear the per-digit counter initially
LOOP_FRAC:
    ; --- Set Operands for Subroutines (FIX for BUG #1) ---
    ; This must be done inside the loop because BCD_SUB modifies POINTER_1's target memory.
    MOVLW   0x5A  ; POINTER_1 will point to the partial remainder
    MOVWF   POINTER_1
    MOVLW   0x2C            ; POINTER_2 will point to the divisor
    MOVWF   POINTER_2

; --- Inner loop for repeated subtraction (FIX for BUG #2) ---
; This loop repeatedly subtracts the divisor from the partial remainder
; until the remainder is smaller than the divisor.
SUB_LOOP_FRAC:

    CALL    BCD_CMP                 ; Compare PARTIAL_REMAINDER with Divisor
    BTFSS   STATUS, C               ; Test Carry. Skip if C is set (PARTIAL >= Divisor).
    GOTO    LOOP_FRAC_NEXT           ; If C was clear (PARTIAL < Divisor), exit subtraction loop.
    ; If we are here, it means PARTIAL >= Divisor, so we subtract.
    CALL    BCD_SUB                 ; PARTIAL = PARTIAL - Divisor
    INCF    COUNT, F                ; Increment quotient digit
    GOTO    SUB_LOOP_FRAC                ; Go back and compare again,

LOOP_FRAC_NEXT:
    
    ; --- Store the calculated quotient digit ---
    MOVF    TEMP_BYTE, W         ; Get address to store result
    MOVWF   FSR                     ; Set the file select register to that address
    MOVF    COUNT, W                ; Move calculated digit to W
    MOVWF   INDF                    ; Store quotient digit at pointed address
    INCF    TEMP_BYTE, F         ; Move pointer to the next quotient digit location
    ; --- Prepare for the next digit calculation ---
    CALL    REMAINDER_SHIFT_LEFT_ZERO          ; Shift remainder left and bring down next dividend digit
    CLRF    COUNT                   ; Reset the counter for the next digit

 


    MOVF    TEMP_BYTE, W      ; Load TEMP_BYTE into WREG
    SUBLW   0x59              ; W = 0x59 - TEMP_BYTE
    BTFSS   STATUS, C         ; If Carry is clear, TEMP_BYTE >= 0x59
    RETURN                    ; Exit the subroutine if TEMP_BYTE >= 0x59
    GOTO    LOOP_FRAC                ; Continue for the next digit



SHIFT_NUMBER_TWO:
    MOVF    DECIMALB6, W    ; Load DECIMALB6 into W
    MOVWF   DECIMALB7       ; DECIMALB6 -> DECIMALB7
    MOVF    DECIMALB5, W    ; Load DECIMALB5 into W
    MOVWF   DECIMALB6       ; DECIMALB5 -> DECIMALB6
    MOVF    DECIMALB4, W    ; Load DECIMALB4 into W
    MOVWF   DECIMALB5       ; DECIMALB4 -> DECIMALB5
    MOVF    DECIMALB3, W    ; Load DECIMALB3 into W
    MOVWF   DECIMALB4       ; DECIMALB3 -> DECIMALB4
    MOVF    DECIMALB2, W    ; Load DECIMALB2 into W
    MOVWF   DECIMALB3       ; DECIMALB2 -> DECIMALB3
    MOVF    DECIMALB1, W    ; Load DECIMALB1 into W
    MOVWF   DECIMALB2       ; DECIMALB1 -> DECIMALB2
    MOVF    DIGITB6, W      ; Load DIGITB6 into W
    MOVWF   DECIMALB1       ; DIGITB6 -> DECIMALB1
    MOVF    DIGITB5, W      ; Load DIGITB5 into W
    MOVWF   DIGITB6         ; DIGITB5 -> DIGITB6
    MOVF    DIGITB4, W      ; Load DIGITB4 into W
    MOVWF   DIGITB5         ; DIGITB4 -> DIGITB5
    MOVF    DIGITB3, W      ; Load DIGITB3 into W
    MOVWF   DIGITB4         ; DIGITB3 -> DIGITB4
    MOVF    DIGITB2, W      ; Load DIGITB2 into W
    MOVWF   DIGITB3         ; DIGITB2 -> DIGITB3
    MOVF    DIGITB1, W      ; Load DIGITB1 into W
    MOVWF   DIGITB2         ; DIGITB1 -> DIGITB2
    CLRF    DIGITB1         ; Clear DIGITB1 (set to 0)
    RETURN

END