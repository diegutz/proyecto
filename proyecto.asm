; proyecto
:proyecto final de universidad brazo robot pic16f887 assembler
;Diego Gonzalez 15700
#include "p16f887.inc"

; CONFIG1
; __config 0xE0F4
 __CONFIG _CONFIG1, _FOSC_INTRC_NOCLKOUT & _WDTE_OFF & _PWRTE_OFF & _MCLRE_OFF & _CP_OFF & _CPD_OFF & _BOREN_OFF & _IESO_OFF & _FCMEN_OFF & _LVP_OFF
; CONFIG2
; __config 0xFFFF
 __CONFIG _CONFIG2, _BOR4V_BOR40V & _WRT_OFF
;***************************
GPR_VAR        UDATA
W_TEMP         RES       1      ; w register for context saving (ACCESS)
STATUS_TEMP    RES       1      ; status used for context saving
DELAY1	  RES	    1
DELAY2	  RES	    1
VALOR_ADC_1	  RES	    1
VALOR_ADC_2	  RES	    1
VALOR_ADC_3	  RES	    1
VALOR_ADC_4	  RES	    1

;***************************
; Reset Vector
;***************************

RES_VECT  CODE    0x0000            ; processor reset vector
    GOTO    START                   ; go to beginning of program

;***************************
;ISR       CODE    0x0004           ; interrupt vector location
;     RETFIE
;***************************
; MAIN PROGRAM
;***************************

MAIN_PROG CODE                      ; let linker place main program

START
;***************************
    CALL    CONFIG_IO  
    CALL    CONFIG_TX_RX		; 10417hz 
    CALL    CONFIG_RELOJ		; RELOJ INTERNO DE 500KHz
    CALL    CONFIG_ADC			; fosc/8, adc on, justificado a la izquierda, Vref interno (0-5V)
    CALL    CONFIG_INTERRUPCIONES
    CALL    CONFIG_PWM
    CALL    CONFIG_TIMER1
    BANKSEL PORTA

;***************************
; CICLO INFINITO
;***************************
LOOP:
;--------------------SERVO 1--------------------------------------------------
    CALL    CANAL_0
    CALL    DELAY_50MS
    BSF	    ADCON0, GO		    ; EMPIEZA LA CONVERSIÓN
CHECK_AD:
    BTFSC   ADCON0, GO	       	    ; revisa que terminó la conversión
    GOTO    $-1
    BCF	    PIR1, ADIF		    ; borramos la bandera del adc			; mueve adresh al puerto b
    MOVFW   ADRESH
    MOVWF   VALOR_ADC_1
    
CHECK_TXIF: 
    MOVFW   VALOR_ADC_1		    ; ENVÍA PORTB POR EL TX
    MOVWF   TXREG
   
    BTFSS   PIR1, TXIF
    GOTO    $-1
CHECK_RCIF:			    ; RECIBE EN RX y lo muestra en PORTD
    BTFSS   PIR1, RCIF
    GOTO    FIN_SERVO_1
    MOVF    VALOR_ADC_1, W
    MOVWF CCPR2L
    BCF STATUS,C
    RRF CCPR2L,F
    BCF STATUS,C
    RRF CCPR2L,F
    BCF STATUS,C
    RRF CCPR2L,F
    CLRF ADRESH
FIN_SERVO_1
;--------------------SERVO 2--------------------------------------------------
    CALL    CANAL_1
    CALL    DELAY_50MS
    BSF	    ADCON0, GO		    ; EMPIEZA LA CONVERSIÓN
CHECK_AD_2:
    BTFSC   ADCON0, GO	       	    ; revisa que terminó la conversión
    GOTO    $-1
    BCF	    PIR1, ADIF		    ; borramos la bandera del adc			; mueve adresh al puerto b
    MOVFW   ADRESH
    MOVWF   VALOR_ADC_2
  
CHECK_TXIF_2: 
    MOVFW   VALOR_ADC_2		    ; ENVÍA VALOR_ADC_2 POR EL TX
    MOVWF   TXREG
   
    BTFSS   PIR1, TXIF
    GOTO    $-1
    
CHECK_RCIF_2:			    ; RECIBE EN RX y lo muestra en PORTD
    BTFSS   PIR1, RCIF
    GOTO    FIN_SERVO_2
    MOVF    VALOR_ADC_2, W
    MOVWF   CCPR1L
    BCF	    STATUS,C
    RRF	    CCPR1L,F
    BCF	    STATUS,C
    RRF	    CCPR1L,F
    BCF	    STATUS,C
    RRF	    CCPR1L,F
    CLRF    ADRESH
  
FIN_SERVO_2
    


;--------------------SERVO 3--------------------------------------------------

;--------------------SERVO 4--------------------------------------------------

    GOTO LOOP
    
;------------------------------SUBRUTINAS---------------------------------------

DELAY_50MS
    MOVLW   .100		    ; 1MS 
    MOVWF   DELAY2
    CALL    DELAY_500US
    DECFSZ  DELAY2		    ;DECREMENTA CONT1
    GOTO    $-2			    ; IR A LA POSICION DEL PC - 1
    RETURN
    
DELAY_500US
    MOVLW   .250		    ; 1US 
    MOVWF   DELAY1	    
    DECFSZ  DELAY1		    ;DECREMENTA CONT1
    GOTO    $-1			    ; IR A LA POSICION DEL PC - 1
    RETURN
    
CANAL_0
    BANKSEL ADCON0
    BCF ADCON0, CHS3		; CH0
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BCF ADCON0, CHS0
    BANKSEL PORTA	
    RETURN
    
CANAL_1
    BANKSEL ADCON0
    BCF ADCON0, CHS3		; CH1
    BCF ADCON0, CHS2
    BCF ADCON0, CHS1
    BSF ADCON0, CHS0
    BANKSEL PORTA	
    RETURN
    
CANAL_2
    BANKSEL ADCON0
    BCF ADCON0, CHS3		; CH2
    BCF ADCON0, CHS2
    BSF ADCON0, CHS1
    BCF ADCON0, CHS0
    BANKSEL PORTA	
    RETURN
    
;---------------------------- CONFIGURACIONES ----------------------------------   

CONFIG_IO
    BANKSEL TRISA
    CLRF    TRISA
    BSF	    TRISA, RA0	; RA0 COMO ENTRADA
    BSF	    TRISA, RA1	; RA1 COMO ENTRADA
    BSF	    TRISA, RA2	; RA2 COMO ENTRADA
    BSF	    TRISA, RA3	; RA3 COMO ENTRADA
    CLRF    TRISB
    CLRF    TRISC
    CLRF    TRISD
    CLRF    TRISE
    BANKSEL ANSEL
    CLRF    ANSEL
    CLRF    ANSELH
    BSF	    ANSEL,ANS0	; ANS0 COMO ENTRADA ANALOGICA
    BSF	    ANSEL,ANS1	; ANS1 COMO ENTRADA ANALOGICA
    BSF	    ANSEL,ANS2	; ANS2 COMO ENTRADA ANALOGICA
    BANKSEL PORTA
    CLRF    PORTA
    CLRF    PORTB
    CLRF    PORTC
    CLRF    PORTD
    CLRF    VALOR_ADC_1
    CLRF    VALOR_ADC_2
    CLRF    VALOR_ADC_3
    CLRF    VALOR_ADC_4
    RETURN   
    
CONFIG_RELOJ
    BANKSEL OSCCON   
    BSF OSCCON, IRCF2
    BCF OSCCON, IRCF1
    BCF OSCCON, IRCF0		    ; FRECUECNIA DE 1MHz
    RETURN
 
CONFIG_ADC
    BANKSEL PORTA
    BCF ADCON0, ADCS1
    BSF ADCON0, ADCS0		; FOSC/8 RELOJ TAD
    
    BANKSEL TRISA
    BCF ADCON1, ADFM		; JUSTIFICACIÓN A LA IZQUIERDA
    BCF ADCON1, VCFG1		; VSS COMO REFERENCIA VREF-
    BCF ADCON1, VCFG0		; VDD COMO REFERENCIA VREF+
    BANKSEL PORTA
    BSF ADCON0, ADON		; ENCIENDO EL MÓDULO ADC
    
    RETURN
    
CONFIG_PWM
    BANKSEL TRISC
    BSF	    TRISC, RC1		; ESTABLEZCO RC1 / CCP2 COMO ENTRADA	
    BSF	    TRISC, RC2
    MOVLW   .120
    MOVWF   PR2			; COLOCO EL VALOR DEL PERIODO DE MI SENAL 20mS  
    
    BANKSEL PORTA
    BSF	    CCP2CON, CCP2M3
    BSF	    CCP2CON, CCP2M2
    BSF	    CCP2CON, CCP2M1
    BSF	    CCP2CON, CCP2M0	    ; MODO PWM				
    ;ciclo de trabajo
    MOVLW   B'00011011'
    MOVWF   CCPR2L		    ; MSB   DEL DUTY CICLE
    BSF	    CCP2CON, DC2B0
    BSF	    CCP2CON, DC2B1	    ; LSB del duty cicle
    
    MOVLW b'00001100'
    MOVWF CCP1CON	    ; MSB   DEL DUTY CICLE
    
    BCF	    PIR1, TMR2IF	    ;Borrando bandera del TMR2 / Config. e Inicializar TMR2		 
    
    BSF	    T2CON, T2CKPS1
    BSF	    T2CON, T2CKPS0	    ; PRESCALER 1:16				 
    
    BSF	    T2CON, TMR2ON	    ; HABILITAMOS EL TMR2			
    BTFSS   PIR1, TMR2IF
    GOTO    $-1
    BCF	    PIR1, TMR2IF
    
    BANKSEL TRISC		    ;Habilitamos la salida del PWM Después de que haya un nuevo ciclo empezado
    BCF	    TRISC, RC1		    ; RC1 / CCP2 SALIDA PWM	
    BCF	    TRISC, RC2
    RETURN
    
CONFIG_TX_RX
    BANKSEL TXSTA
    BCF	    TXSTA, SYNC		    ; ASINCRÓNO
    BSF	    TXSTA, BRGH		    ; LOW SPEED
    BANKSEL BAUDCTL
    BSF	    BAUDCTL, BRG16	    ; 8 BITS BAURD RATE GENERATOR
    BANKSEL SPBRG
    MOVLW   .25	    
    MOVWF   SPBRG		    ; CARGAMOS EL VALOR DE BAUDRATE CALCULADO
    CLRF    SPBRGH
    BANKSEL RCSTA
    BSF	    RCSTA, SPEN		    ; HABILITAR SERIAL PORT
    BCF	    RCSTA, RX9		    ; SOLO MANEJAREMOS 8BITS DE DATOS
    BSF	    RCSTA, CREN		    ; HABILITAMOS LA RECEPCIÓN 
    BANKSEL TXSTA
    BSF	    TXSTA, TXEN		    ; HABILITO LA TRANSMISION
    
    BANKSEL PORTD
    CLRF    PORTD
    RETURN  

 CONFIG_TIMER1:
    BANKSEL T1CON
    BCF	T1CON,TMR1ON
    BCF	T1CON,T1CKPS1	;PRESCALER DE 8
    BSF	T1CON,T1CKPS0
    BCF	T1CON,T1OSCEN
    BCF	T1CON,TMR1CS
    BSF	T1CON,TMR1ON
    MOVLW   0FFH
    MOVWF   TMR1H
    MOVLW   0FCH
    MOVWF   TMR1L
    BCF	    PIR1,TMR1IF
    RETURN
    
    CONFIG_INTERRUPCIONES:
    ;INTERRUPCION TIMER1
    BANKSEL PIE1
    BSF	PIE1,TMR1IE
    BSF	PIE1,TMR2IE
    BANKSEL PIR1
    BCF	PIR1,TMR1IF
    BCF	PIR1,TMR2IF
    RETURN
;-------------------------------------------------------------------------------
    END
