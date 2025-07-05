	; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	; Project: Complex Calculator - Master CPU
	; Device: PIC16F877A
	; Oscillator: XT, 4.00 MHz
	; Compiler: MPASM
	;
	; HARDWARE CONNECTIONS:
	; PORTB.0 (INT): Push Button P (with 10k pull - up resistor)
	; PORTC: Data bus to Co - Processor (8 - bit bidirectional)
	; PORTD: 16x2 Character LCD in 4 - bit mode
	;
	; * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 
	
	PROCESSOR 16F877A
	#include <p16f877a.inc>
	; - XT_OSC: Crystal / Resonator on RA6 / OSC2 / CLKOUT and RA7 / OSC1 / CLKIN
	; - WDTDIS: Watchdog Timer disabled
	; - PWRTEN: Power - up Timer enabled
	; - BOREN: Brown - out Reset enabled
	; - LVP DIS: Low - Voltage (Single - Supply) In - Circuit Serial Programming disabled
	__CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _XT_OSC & _LVP_OFF
	
	;==============================================================================
	; 1. REGISTER DEFINITIONS
	;==============================================================================
	; General Purpose Registers for PIC16F877A (Bank 0: 0x20 - 0x7F)
	
    cblock  0x20        ; Start of General Purpose Register block
    ; --- System State & Control ---
    STATE_VAR           ; Main state machine variable (e.g., POWER_UP, ENTER_NUM1, etc.)
    DISPLAY_TOGGLE_STATE; For result screen: 0=Result, 1=Num1, 2=Num2

    ; --- Number Storage (6 int, 6 dec) ---
    ; Each digit is stored as a single byte (0-9)
    Number1_Int: 6      ; Buffer for integer part of the first number
    Number1_Dec: 6      ; Buffer for decimal part of the first number
    Number2_Int: 6      ; Buffer for integer part of the second number
    Number2_Dec: 6      ; Buffer for decimal part of the second number
    Result_Data: 24     ; Buffer to store result from co-processor 

    ; --- User Input Handling ---
    CURRENT_DIGIT_INDEX ; Index of the digit being edited (0-5 for int, 6-11 for dec)
    PREV_DIGIT_VALUE ; Value of the current digit (0-9)
    TIMEOUT_FLAG        ; Flag set by timer ISR to indicate 1-second timeout
    DOUBLE_CLICK_FLAG   ; Flag set by ISR if a double-click is detected

    ; --- Timers and Counters ---
    TIMER_1SEC_COUNT    ; Counter for 1-second timeout, decremented by Timer ISR
    TIMER_BLINK_COUNT   ; Counter for blinking welcome message
    TIMER_DBL_CLICK     ; Counter for double-click detection window

    ; --- ISR Save/Restore ---
    W_TEMP              ; Temp storage for W register during ISR
    STATUS_TEMP         ; Temp storage for STATUS register during ISR

    ; --- General Purpose / Delay ---
    Delay_Count1
    Delay_Count2
	TEMP
	NUMBER_ADDRESS
	TEMP_REG


	JUST_DICIMAL
	
    endc
	
	; - - - State Constants - - - 
	#define STATE_POWER_UP 0
	#define STATE_ENTER_NUM1 1
	#define STATE_ENTER_NUM2 2
	#define STATE_SEND 3
	#define STATE_RECIEVE 4
	#define STATE_DISPLAY_RESULT 5

	#define FIRST_PRESS FLAGS,0 ; Set on first button press
    #define WAITING     FLAGS,1 ; Set while waiting for second press
	
	;==============================================================================
	; PROGRAM START & INTERRUPT VECTORS
	;==============================================================================
	
	ORG 0x00                   ; Reset Vector
	goto Main                    ; Jump to the main program start
	
	ORG 0x04                   ; Interrupt Vector
	goto ISR                     ; Jump to Interrupt Service Routine
	
	;==============================================================================
	; INTERRUPT SERVICE ROUTINE (ISR)
	;==============================================================================
	; - - - INITIALIZATION ROUTINES - - - 
	
Initialize:
	; 1. Configure PORTB: Set RB0 as input for push button, enable weak pull-up
	BCF STATUS, RP1          ; Select Bank 0
	BSF STATUS, RP0          ; Select Bank 1
	BSF TRISB, 0             ; Set RB0 as input
	BSF TRISB, 5             ; Set RB5 FOR ACK
	BSF TRISB, 2             ; Set RB0 as input
	BCF TRISB, 1             ; Set RB0 as input


	; 2. Configure PORTD: Set as output for LCD
	CLRF TRISD               ; Set all PORTD pins as output
	BSF TRISD, 3

    MOVLW   0xF0            ; RC4-RC7 inputs, RC0-RC3 outputs
    MOVWF   TRISC

    ; Initialize Timer1.

    BSF     STATUS, RP0     ; Bank 1
    BCF     PIE1, TMR1IE    ; Disable Timer1 interrupt initially
    BCF     STATUS, RP0     ; Bank 0
    MOVLW   B'00000000'     ; Timer1: 1:8 prescaler, internal clock, off
    MOVWF   T1CON
    BCF     PIR1, TMR1IF    ; Clear Timer1 interrupt flag
	
	BCF STATUS, RP0          ; Back to Bank 0
	MOVLW 0xD8               ; Load TMR0 for ~10ms interrupt (256 - 100 = 156 = 0xD8)
	MOVWF TMR0               ; With 4 MHz / 4 = 1 MHz, 1:256 prescaler, 156 * 256 = ~10ms
	CLRF PORTD               ; Clear PORTD outputs
	CLRF PORTC               ; Clear PORTC outputs
	; 5. Configure Interrupts: Enable GIE, INTE, T0IE
	;BSF INTCON, GIE          ; Enable global interrupts
	;BSF INTCON, INTE         ; Enable RB0/INT external interrupt
	;BSF INTCON, T0IE         ; Enable Timer0 overflow interrupt
	;BCF INTCON, INTF         ; Clear RB0/INT flag
	;BCF INTCON, TMR0IF       ; Clear Timer0 flag
	
	; 6. Initialize all variables (0x20 to 0x52) to 0 and set SYS_STATE to STATE_POWER_UP
	MOVLW 0x20               ; Start of GPRs
	MOVWF FSR                ; Set FSR to point to 0x20

Init_Loop:
	CLRF INDF                ; Clear register pointed to by FSR
	INCF FSR, F              ; Move to next register
	MOVLW 0x53               ; End of GPRs (0x52 + 1)
	SUBWF FSR, W             ; Check if FSR = 0x53
	BTFSS STATUS, Z          ; If not zero, continue loop
	GOTO Init_Loop
	MOVLW STATE_POWER_UP     ; Set SYS_STATE to 0 (POWER_UP)
	MOVWF STATE_VAR
	
	; 7. Call LCD_Init to initialize the LCD in 4-bit mode
	CALL inid
    CALL CLEAR_THE_LCD

	RETURN
	


    ; Interrupt Service Routine
ISR:

	MOVWF   W_TEMP           ; Save W register
    SWAPF   STATUS, W
    MOVWF   STATUS_TEMP      ; Save STATUS register

    BCF     INTCON, GIE         ; Disable global interrupts
    BCF     INTCON, INTE        ; Clear external interrupt enable
    BCF     INTCON, T0IE        ; Clear Timer0 interrupt enable
    BCF     INTCON, INTF        ; Clear external interrupt flag
    BCF     INTCON, TMR0IF      ; Clear Timer0 interrupt flag
    CALL    CHECK_SECOND_CLICK
    MOVF    DOUBLE_CLICK_FLAG, W
    BTFSS   STATUS, Z
	GOTO    Handle_Button_Double_Press_ISR
    GOTO    Handle_Button_Press_ISR
   

; - - - INPUT HANDLING ROUTINES - - - 
Handle_Button_Press_ISR:

	MOVF JUST_DICIMAL, W
	BTFSC STATUS, Z
	GOTO CHOOSE_NUM

    MOVLW   0x00
    MOVWF   JUST_DICIMAL

	GOTO ISR_Exit


CHOOSE_NUM:
    ; Compare STATE_VAR with 1
    MOVF    STATE_VAR, W
    SUBLW   0x01
    BTFSC   STATUS, Z
    GOTO    Handle_BUTTON_1

    ; Compare STATE_VAR with 2
    MOVF    STATE_VAR, W
    SUBLW   0x02
    BTFSC   STATUS, Z
    GOTO    Handle_BUTTON_2
    GOTO    ISR_Exit            ; Exit if STATE_VAR is neither 1 nor 2

Handle_BUTTON_1:
    ; Calculate starting address (0x22 + CURRENT_DIGIT_INDEX)
    MOVLW   0x22
    ADDWF   CURRENT_DIGIT_INDEX, W
    MOVWF   TEMP                ; Store starting address
    MOVWF   FSR                 ; Initialize FSR

    ; Calculate end address (start + 12)
    MOVLW   12
    ADDWF   TEMP, W
    MOVWF   TEMP                ; TEMP holds end address

Loop_Handle_Button_Press_ISR_1:
    ; Check if register value is 9
    MOVF    INDF, W
    SUBLW   0x09
    BTFSC   STATUS, Z
    GOTO    Clear_Register_1

    ; If value < 9, increment
    INCF    INDF, F
    GOTO    Next_Address_1

Clear_Register_1:
    CLRF    INDF

Next_Address_1:
    INCF    FSR, F
    MOVF    FSR, W
    SUBWF   TEMP, W             ; Compare FSR with end address
    BTFSS   STATUS, Z
    GOTO    Loop_Handle_Button_Press_ISR_1
    CALL    NUMBER_ONE_DISPLAY
    GOTO    ISR_Exit

Handle_BUTTON_2:
    ; Calculate starting address (0x2E + CURRENT_DIGIT_INDEX)
    MOVLW   0x2E
    ADDWF   CURRENT_DIGIT_INDEX, W
    MOVWF   TEMP
    MOVWF   FSR

    ; Calculate end address (start + 12)
    MOVLW   12
    ADDWF   TEMP, W
    MOVWF   TEMP

Loop_Handle_Button_Press_ISR_2:
    ; Check if register value is 9
    MOVF    INDF, W
    SUBLW   0x09
    BTFSC   STATUS, Z
    GOTO    Clear_Register_2

    ; If value < 9, increment
    INCF    INDF, F
    GOTO    Next_Address_2

Clear_Register_2:
    CLRF    INDF

Next_Address_2:
    INCF    FSR, F
    MOVF    FSR, W
    SUBWF   TEMP, W
    BTFSS   STATUS, Z
    GOTO    Loop_Handle_Button_Press_ISR_2
    CALL    NUMBER_TWO_DISPLAY
    GOTO    ISR_Exit









Handle_Button_Double_Press_ISR:
    MOVF    CURRENT_DIGIT_INDEX, W
    SUBLW   0x05
    BTFSC   STATUS, C
    GOTO    SET_INDEX_6
    GOTO    SET_INDEX_12

SET_INDEX_6:
    MOVLW   0x06
    MOVWF   CURRENT_DIGIT_INDEX
   
    MOVF    STATE_VAR, W
    SUBLW   0x01
    BTFSC   STATUS, Z
    CALL    NUMBER_ONE_DISPLAY

    ; Compare STATE_VAR with 2
    MOVF    STATE_VAR, W
    SUBLW   0x02
    BTFSC   STATUS, Z
    CALL    NUMBER_TWO_DISPLAY

	MOVLW   0x01
    MOVWF   JUST_DICIMAL
   
    GOTO    ISR_Exit

SET_INDEX_12:
    MOVLW   0x0C                ; 11 in hex (corrected comment)
    MOVWF   CURRENT_DIGIT_INDEX

    MOVF    STATE_VAR, W
    SUBLW   0x01
    BTFSC   STATUS, Z
    CALL    NUMBER_ONE_DISPLAY

    ; Compare STATE_VAR with 2
    MOVF    STATE_VAR, W
    SUBLW   0x02
    BTFSC   STATUS, Z
    CALL    NUMBER_TWO_DISPLAY
   
    GOTO    ISR_Exit








CHECK_SECOND_CLICK:
    CLRF    DOUBLE_CLICK_FLAG   ; Clear flag
	
    MOVLW   0xFF               ; Set counter to 255
    MOVWF   TEMP_REG
	BTFSS   PORTB, 0            ; Test bit 2 of PORTB
	GOTO    $-1
loop:
    BTFSS   PORTB, 0           ; Test bit 2 of PORTB
    GOTO    set_flag            ; If high, set flag and return
    CALL    Delay_1ms           ; Call 1ms delay
    CALL    Delay_1ms           ; Call 1ms delay
    DECFSZ  TEMP_REG, F         ; Decrement counter, skip if zero
    GOTO    loop
    RETURN                      ; Exit after 255 loops

set_flag:
    MOVLW   0x01
    MOVWF   DOUBLE_CLICK_FLAG
    RETURN

ISR_Exit:
	CALL    Delay_1ms
	CALL    Delay_1ms
	CALL    Delay_1ms

    MOVLW   0xFF        ; Load W with 0xFF
    MOVWF   Delay_Count2 ; Store in Delay_Count2

	MOVWF   W_TEMP           ; Save W register
    SWAPF   STATUS, W
    MOVWF   STATUS_TEMP      ; Save STATUS register


    RETFIE                      ; Return from interrupt








	;==============================================================================
	; 2. MAIN FUNCTION AND FLOW
	;==============================================================================
Main:
	call Initialize              ; Configure ports, timers, interrupts, and LCD

	;STATE_POWER_UP 
	CALL Handle_PowerUp          ; STATE_VAR = 0
	
MainLoop:
	
	;STATE_POWER_UP 
	CALL CLEAR_THE_LCD
	CALL Delay_2s

    MOVLW   0xFF        ; Load W with 0xFF
    MOVWF   Delay_Count2 ; Store in Delay_Count2

	;STATE_ENTER_NUM1
	CALL Handle_EnterNum1        ; STATE_VAR = 1

	BCF INTCON, INTF         ; Clear RB0/INT flag
	BCF INTCON, TMR0IF       ; Clear Timer0 flag

    movlw STATE_ENTER_NUM2
	movwf STATE_VAR

    CLRF CURRENT_DIGIT_INDEX

	BCF INTCON, INTF         ; Clear RB0/INT flag
	BCF INTCON, TMR0IF       ; Clear Timer0 flag

	;STATE_ENTER_NUM2
    CALL Delay_500ms
	CALL ClearBuffers
    MOVLW   0xFF        ; Load W with 0xFF
    MOVWF   Delay_Count2 ; Store in Delay_Count2
    CALL Handle_EnterNum2        ; STATE_VAR = 1

	BCF INTCON, INTF         ; Clear RB0/INT flag
	BCF INTCON, TMR0IF       ; Clear Timer0 flag

	;STATE_SEND
	
	movlw STATE_SEND
	movwf STATE_VAR

	CALL TRANSMIT_NUMBERS
	CALL ClearBuffers_RES

	movlw STATE_RECIEVE
	movwf STATE_VAR
	CALL ReceiveResult
	CALL 	Delay_500ms
	CALL RESULT_DISPLAY
	CALL 	Delay_500ms
	GOTO 	$-1


	CALL DISPLAY_WELCOME



	;STATE_RECIEVE
	CALL DISPLAY_RESULT
	CALL Initialize              ; Configure ports, timers, interrupts, and LCD


	goto MainLoop                ; Failsafe, loop back
	
	
	;==============================================================================
	; STATE HANDLER ROUTINES
	;==============================================================================
Handle_PowerUp:
	
	CALL CLEAR_THE_LCD
	CALL DISPLAY_WELCOME
	CALL Delay_500ms
	
	CALL CLEAR_THE_LCD
	CALL DISPLAY_WELCOME
	CALL Delay_500ms
	
	CALL CLEAR_THE_LCD
	CALL DISPLAY_WELCOME
	CALL Delay_500ms
	
	movlw STATE_ENTER_NUM1
	movwf STATE_VAR
	return
	
Handle_EnterNum1:

	MOVF    CURRENT_DIGIT_INDEX, W  ; Move CURRENT_DIGIT_INDEX into W register
    MOVWF   PREV_DIGIT_VALUE        ; Store W into PREV_DIGIT_VALUE


	MOVLW   0xFF        ; Load W with 0xFF
    MOVWF   Delay_Count2 ; Store in Delay_Count2

	; Step 1: Setup the screen for Number 1 entry
	CALL 	NUMBER_ONE_DISPLAY

    ; Initialize Interrupt Control
    BSF     INTCON, INTE    ; Enable RB0/INT interrupt
    BSF     INTCON, GIE     ; Enable global interrupts
    BSF     INTCON, PEIE    ; Enable peripheral interrupts


	
	; Display "Number 1" on the LCD
Delay_Loop:
    CALL    Delay_1ms           ; 1ms delay
    CALL    Delay_1ms
    CALL    Delay_1ms
    CALL    Delay_1ms           ; Total = 4 ms delay

    BSF     INTCON, GIE         ; Enable global interrupts
    BSF     INTCON, INTE        ; Enable RB0/INT external interrupt

    ; --- Compare CURRENT_DIGIT_INDEX with PREV_DIGIT_VALUE ---
    MOVF    CURRENT_DIGIT_INDEX, W
    XORWF   PREV_DIGIT_VALUE, W ; W = CURRENT_DIGIT_INDEX ^ PREV_DIGIT_VALUE
    BTFSS   STATUS, Z           ; If not equal, check if it's 6
    GOTO    Check_Index_6       ; Only break if not equal AND not 6

    ; --- Decrement counter ---
    DECFSZ  Delay_Count2, F
    GOTO    Delay_Loop
    GOTO    Delay_Loop_Exit     ; Exit loop if done

Check_Index_6:
    ; Check if CURRENT_DIGIT_INDEX == 6
    MOVF    CURRENT_DIGIT_INDEX, W
    XORLW   0x06
    BTFSC   STATUS, Z           ; If CURRENT_DIGIT_INDEX == 6, skip breaking
    GOTO    Continue_Loop

    GOTO    Delay_Loop_Exit     ; Else break the loop

Continue_Loop:
    DECFSZ  Delay_Count2, F
    GOTO    Delay_Loop

Delay_Loop_Exit:
    ; Continue after delay loop


	BCF INTCON, INTF         ; Clear RB0/INT flag
	BCF INTCON, TMR0IF       ; Clear Timer0 flag

	INCF CURRENT_DIGIT_INDEX,f

    MOVF    CURRENT_DIGIT_INDEX, W  ; Load CURRENT_DIGIT_INDEX into W
    SUBLW   0x01                      ; Subtract 11 from W (W = 11 - CURRENT_DIGIT_INDEX)
    BTFSC   STATUS, C               ; Test Carry flag (clear if CURRENT_DIGIT_INDEX > 11)
    GOTO    Handle_EnterNum1        ; Else, go to Handle_EnterNum1
    RETURN                          ; If CURRENT_DIGIT_INDEX > 11, return



Handle_EnterNum2:

	MOVF    CURRENT_DIGIT_INDEX, W  ; Move CURRENT_DIGIT_INDEX into W register
    MOVWF   PREV_DIGIT_VALUE        ; Store W into PREV_DIGIT_VALUE


	MOVLW   0xFF        ; Load W with 0xFF
    MOVWF   Delay_Count2 ; Store in Delay_Count2

	; Step 1: Setup the screen for Number 1 entry
	CALL 	NUMBER_TWO_DISPLAY

    ; Initialize Interrupt Control
    BSF     INTCON, INTE    ; Enable RB0/INT interrupt
    BSF     INTCON, GIE     ; Enable global interrupts
    BSF     INTCON, PEIE    ; Enable peripheral interrupts


	
	; Display "Number 1" on the LCD
Delay_Loop_2:
    CALL    Delay_1ms           ; 1ms delay
    CALL    Delay_1ms
    CALL    Delay_1ms
    CALL    Delay_1ms           ; Total = 4 ms delay

    BSF     INTCON, GIE         ; Enable global interrupts
    BSF     INTCON, INTE        ; Enable RB0/INT external interrupt

    ; --- Compare CURRENT_DIGIT_INDEX with PREV_DIGIT_VALUE ---
    MOVF    CURRENT_DIGIT_INDEX, W
    XORWF   PREV_DIGIT_VALUE, W ; W = CURRENT_DIGIT_INDEX ^ PREV_DIGIT_VALUE
    BTFSS   STATUS, Z           ; If not equal, check if it's 6
    GOTO    Check_Index_6_2       ; Only break if not equal AND not 6

    ; --- Decrement counter ---
    DECFSZ  Delay_Count2, F
    GOTO    Delay_Loop_2
    GOTO    Delay_Loop_Exit_2     ; Exit loop if done

Check_Index_6_2:
    ; Check if CURRENT_DIGIT_INDEX == 6
    MOVF    CURRENT_DIGIT_INDEX, W
    XORLW   0x06
    BTFSC   STATUS, Z           ; If CURRENT_DIGIT_INDEX == 6, skip breaking
    GOTO    Continue_Loop_2

    GOTO    Delay_Loop_Exit_2     ; Else break the loop

Continue_Loop_2:
    DECFSZ  Delay_Count2, F
    GOTO    Delay_Loop_2

Delay_Loop_Exit_2:
    ; Continue after delay loop


	BCF INTCON, INTF         ; Clear RB0/INT flag
	BCF INTCON, TMR0IF       ; Clear Timer0 flag

	INCF CURRENT_DIGIT_INDEX,f

    MOVF    CURRENT_DIGIT_INDEX, W  ; Load CURRENT_DIGIT_INDEX into W
    SUBLW   0x01                      ; Subtract 11 from W (W = 11 - CURRENT_DIGIT_INDEX)
    BTFSC   STATUS, C               ; Test Carry flag (clear if CURRENT_DIGIT_INDEX > 11)
    GOTO    Handle_EnterNum2        ; Else, go to Handle_EnterNum1
    RETURN                          ; If CURRENT_DIGIT_INDEX > 11, return


	;==============================================================================
	; 3. HELPER ROUTINE LABELS AND DESCRIPTIONS
	;==============================================================================
	








	; - - - NUMBER MANAGEMENT ROUTINES - - - 
	
NUMBER_ONE_DISPLAY:
	; DESC: Prepares the LCD for number entry.
	; e.g., clears line 2, displays "000000.000000", places cursor at start.
	; Clears line 2 by writing 16 spaces, then displays "000000.000000" at 0xC0
	; Display "Number 1" on the LCD
	CALL CLEAR_THE_LCD
    	; Hide the cursor (Display ON, Cursor OFF, Blink OFF)


	BSF Select, RS               ; Data mode
	MOVLW 'N'
	CALL send
	MOVLW 'u'
	CALL send
	MOVLW 'm'
	CALL send
	MOVLW 'b'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW 'r'
	CALL send
	MOVLW ' '
	CALL send
	MOVLW '1'
	CALL send
	
	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Move to second line
	CALL send

	BSF Select, RS               ; Data mode

	MOVF Number1_Int+0, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+1, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+2, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+3, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+4, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+5, W
	ADDLW '0'
	CALL send

	MOVLW '.'
	CALL send

	MOVF Number1_Dec+0, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+1, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+2, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+3, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+4, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+5, W
	ADDLW '0'
	CALL send

    BCF     Select,RS       ; Command mode
    MOVLW 0x0C
	CALL send
    MOVLW   0x0E            ; Command for "Display On, Cursor On"
    CALL    send
    ; Assume CURRENT_DIGIT_INDEX is in 0x20
    ; Calculate (13 - CURRENT_DIGIT_INDEX)

    MOVF    CURRENT_DIGIT_INDEX, W         ; W = CURRENT_DIGIT_INDEX
    SUBLW   0x0D            ; W = 13 - CURRENT_DIGIT_INDEX
    MOVWF   TEMP            ; Store result in LOOP_COUNTER (0x21)

    MOVF    CURRENT_DIGIT_INDEX, W  ; Load CURRENT_DIGIT_INDEX into W
    SUBLW   5                       ; Subtract 6 from W (W = 6 - CURRENT_DIGIT_INDEX)
    BTFSS   STATUS, C               ; Test Carry flag (clear if CURRENT_DIGIT_INDEX > 6)
    DECF    TEMP,F                 ; Increment W if CURRENT_DIGIT_INDEX > 6

MoveCursorLoop:
    BCF     Select,RS       ; Command mode
    MOVLW   0x10            ; Move cursor left command
    CALL    send

    DECFSZ  TEMP, F         ; Decrease loop counter and check if 0
    GOTO    MoveCursorLoop  ; Repeat if not zero

	RETURN


	; - - - DELAY ROUTINES - - - 

Delay_1ms:
    ; A very approximate 1ms delay. Requires precise calculation.
    ; For now, this is a placeholder. A more accurate way uses a timer.
    ; For example, 250 loops of 4 cycles each = 1000 cycles = 1ms @ 4MHz Fosc.
    MOVLW   d'249'
    MOVWF   Delay_Count1
Delay_1ms_Loop:
    NOP
    DECFSZ  Delay_Count1, F
    GOTO    Delay_1ms_Loop
    RETURN

Delay_500ms:
    ; A more robust 0.5-second delay
    ; Uses registers 0x7D, 0x7E, 0x7F as specified in your code
    BCF     STATUS, RP0
    BCF     STATUS, RP1
    MOVLW   0xFF
    MOVWF   0x7D
    MOVLW   0x83
    MOVWF   0x7E
    MOVLW   0x02
    MOVWF   0x7F
Delay_500ms_L1:
    DECFSZ  0x7D, F
    GOTO    Delay_500ms_L1
Delay_500ms_L2:
    DECFSZ  0x7E, F
    GOTO    Delay_500ms_L1
Delay_500ms_L3:
    DECFSZ  0x7F, F
    GOTO    Delay_500ms_L2
    RETURN
	
Delay_2s:
	; DESC: A fixed 2 - second delay for the welcome screen.
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	RETURN
	
	; * * * * * * * * * * * * * * * * * * * * * * * LCD DISPLAY * * * * * * * * * * * * * * * * * * * * * * * * * * 

	INCLUDE "LCDIS.INC"


CLEAR_THE_LCD:
	MOVLW 0x01
	BCF Select, RS               ; Command mode
	CALL send
	MOVLW 0x80
	BCF Select, RS
	CALL send
	BSF Select, RS               ; Data mode
	RETURN
	
DISPLAY_WELCOME:
	CALL CLEAR_THE_LCD
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

NUMBER_TWO_DISPLAY:
	CALL CLEAR_THE_LCD
    	; Hide the cursor (Display ON, Cursor OFF, Blink OFF)


	BSF Select, RS               ; Data mode
	MOVLW 'N'
	CALL send
	MOVLW 'u'
	CALL send
	MOVLW 'm'
	CALL send
	MOVLW 'b'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW 'r'
	CALL send
	MOVLW ' '
	CALL send
	MOVLW '2'
	CALL send
	
	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Move to second line
	CALL send

	BSF Select, RS               ; Data mode

	MOVF Number2_Int+0, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+1, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+2, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+3, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+4, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+5, W
	ADDLW '0'
	CALL send

	MOVLW '.'
	CALL send

	MOVF Number2_Dec+0, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+1, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+2, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+3, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+4, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+5, W
	ADDLW '0'
	CALL send

    BCF     Select,RS       ; Command mode
    MOVLW 0x0C
	CALL send
    MOVLW   0x0E            ; Command for "Display On, Cursor On"
    CALL    send
    ; Assume CURRENT_DIGIT_INDEX is in 0x20
    ; Calculate (13 - CURRENT_DIGIT_INDEX)

    MOVF    CURRENT_DIGIT_INDEX, W         ; W = CURRENT_DIGIT_INDEX
    SUBLW   0x0D            ; W = 13 - CURRENT_DIGIT_INDEX
    MOVWF   TEMP            ; Store result in LOOP_COUNTER (0x21)

    MOVF    CURRENT_DIGIT_INDEX, W  ; Load CURRENT_DIGIT_INDEX into W
    SUBLW   5                       ; Subtract 6 from W (W = 6 - CURRENT_DIGIT_INDEX)
    BTFSS   STATUS, C               ; Test Carry flag (clear if CURRENT_DIGIT_INDEX > 6)
    DECF    TEMP,F                 ; Increment W if CURRENT_DIGIT_INDEX > 6


MoveCursorLoop_2:
    BCF     Select,RS       ; Command mode
    MOVLW   0x10            ; Move cursor left command
    CALL    send

    DECFSZ  TEMP, F         ; Decrease loop counter and check if 0
    GOTO    MoveCursorLoop_2  ; Repeat if not zero


	RETURN


RESULT_DISPLAY:

	CALL CLEAR_THE_LCD
    	; Hide the cursor (Display ON, Cursor OFF, Blink OFF)


	BSF Select, RS               ; Data mode
	MOVLW 'R'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW 's'
	CALL send
	MOVLW 'u'
	CALL send
	MOVLW 'l'
	CALL send
	MOVLW 't'
	CALL send

	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Move to second line
	CALL send

	BSF Select, RS               ; Data mode

	MOVF Result_Data+0, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+1, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+2, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+3, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+4, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+5, W
	ADDLW '0'
	CALL send

	MOVLW '.'
	CALL send

	MOVF Result_Data+6, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+7, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+8, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+9, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+10, W
	ADDLW '0'
	CALL send

	MOVF Result_Data+11, W
	ADDLW '0'
	CALL send

	RETURN
    




TEMP_DISPLAY:

    CALL CLEAR_THE_LCD
    	; Hide the cursor (Display ON, Cursor OFF, Blink OFF)


	BSF Select, RS               ; Data mode
	MOVLW 'T'
	CALL send
	MOVLW 'e'
	CALL send
	MOVLW 'm'
	CALL send
	MOVLW 'p'
	CALL send
	
	BCF Select, RS               ; Command mode
	MOVLW 0xC0                   ; Move to second line
	CALL send

	BSF Select, RS               ; Data mode

	MOVF TEMP, W
	ADDLW '0'
	CALL send


	RETURN
    






TRANSMIT_NUMBERS:
    movlw   0x22  
    movwf   FSR

    ; Initialize counter for 24 digits.
    movlw   0x18
    movwf   TEMP


SendDataLoop:
    ; Get digit from memory.
    movf    INDF, W
    andlw   0x0F            ; Isolate the lower nibble (the digit value 0-9).

	movwf   PORTC

    bsf     PORTD, 0
	NOP
    bcf     PORTD, 0


Wait_For_ACK:
    btfss   PORTB, 5        ; Skip next instruction if RB5 is HIGH
    goto    Wait_For_ACK    ; Loop back if RB5 is still LOW


    call    Delay_1ms
    call    Delay_1ms
    call    Delay_1ms
    call    Delay_1ms

  


    ; Move to the next data register.
    incf    FSR, F

    ; Decrement and test loop counter.
    decfsz  TEMP, F
    goto    SendDataLoop    ; Jump back if more digits need to be sent.
    
    return







    ; Function to clear Number2_Int and Number2_Dec
ClearBuffers:
    ; Clear Number2_Int (6 bytes)
    movlw   0x2E     ; Load starting address of Number2_Int
    movwf   FSR             ; Set FSR to point to Number2_Int
    movlw   0x0C               ; Number of bytes to clear
    movwf   Delay_Count1            ; Use 0x70 as counter (general purpose register)
ClearIntLoop:
    clrf    INDF            ; Clear current register
    incf    FSR, F          ; Move to next register
    decfsz  Delay_Count1, F         ; Decrement counter
    goto    ClearIntLoop    ; Continue if not done



    return

	    ; Function to clear Number2_Int and Number2_Dec
ClearBuffers_RES:
    ; Clear Number2_Int (6 bytes)
    movlw   0x3A     ; Load starting address of Number2_Int
    movwf   FSR             ; Set FSR to point to Number2_Int
    movlw   0x18               ; Number of bytes to clear
    movwf   Delay_Count1            ; Use 0x70 as counter (general purpose register)
ClearIntLoop_RES:
    clrf    INDF            ; Clear current register
    incf    FSR, F          ; Move to next register
    decfsz  Delay_Count1, F         ; Decrement counter
    goto    ClearIntLoop    ; Continue if not done



    return

ReceiveResult:
	MOVLW 0x0F                   ; RC4 - RC7 inputs, RC0 - RC3 outputs
	MOVWF TRISC
	
	MOVLW   11
    MOVWF   TEMP

    MOVLW   0x3A
    MOVWF   FSR

	CALL  	ReceiveResult_LOOP
	CALL RESULT_DISPLAY
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	CALL Delay_500ms
	RETURN



ReceiveResult_LOOP:
    ; Wait for interrupt on RB0

    btfss   PORTD, 3         ; Check if RB0 is high (interrupt)
    goto    $-1             ; Loop until interrupt received

    ; Read data from PORTC
	MOVF 	PORTC, W
	ANDLW   0x0F
	MOVWF BINARY_IN              ; Store the 4 - bit value
	CALL BIN_TO_DEC
	MOVWF DECIMAL_OUT

    movwf   INDF            ; Store received byte

    ; Send ACK on RB1
    bsf     PORTB, 1        
    NOP
	NOP
	NOP
    bcf     PORTB, 1        
	CALL RESULT_DISPLAY


    ; Move to next register
    incf    FSR, F

    decfsz  TEMP, F
    goto    ReceiveResult_LOOP


    return



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






DISPLAY_RESULT:
	CALL RESULT_DISPLAY
	CLRF NUMBER_ADDRESS

DISPLAY_RESULT_LOOP:
    movf    NUMBER_ADDRESS, W
    XORLW   0x01
    movwf   NUMBER_ADDRESS
	btfss   PORTB, 0         ; Check if RB0 is high (interrupt)
    goto    $-1             ; Loop until interrupt received
	CALL 	CHECK_SECOND_CLICK
	MOVF	DOUBLE_CLICK_FLAG, W
	BTFSC	STATUS, Z
	GOTO 	MainLoop

	MOVF	NUMBER_ADDRESS, W
	BTFSC	STATUS, Z
	GOTO 	NUMBER_ONE_DISPLAY
	GOTO    NUMBER_TWO_DISPLAY








NUMBER_TWO_DISPLAY_FL:

	CALL CLEAR_THE_LCD

	BSF Select, RS               ; Data mode

	MOVF Number2_Int+0, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+1, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+2, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+3, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+4, W
	ADDLW '0'
	CALL send

	MOVF Number2_Int+5, W
	ADDLW '0'
	CALL send

	MOVLW '.'
	CALL send

	MOVF Number2_Dec+0, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+1, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+2, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+3, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+4, W
	ADDLW '0'
	CALL send

	MOVF Number2_Dec+5, W
	ADDLW '0'
	CALL send
	GOTO DISPLAY_RESULT_LOOP



NUMBER_ONE_DISPLAY_FL:

	CALL CLEAR_THE_LCD

	BSF Select, RS               ; Data mode

	MOVF Number1_Int+0, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+1, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+2, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+3, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+4, W
	ADDLW '0'
	CALL send

	MOVF Number1_Int+5, W
	ADDLW '0'
	CALL send

	MOVLW '.'
	CALL send

	MOVF Number1_Dec+0, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+1, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+2, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+3, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+4, W
	ADDLW '0'
	CALL send

	MOVF Number1_Dec+5, W
	ADDLW '0'
	CALL send
	GOTO DISPLAY_RESULT_LOOP





DISPLAY_EQUAL:

	CALL CLEAR_THE_LCD

	BSF Select, RS               ; Data mode
0
	MOVLW 0x3D
	CALL send
RETURN
	
   	END                          ; End of the program


