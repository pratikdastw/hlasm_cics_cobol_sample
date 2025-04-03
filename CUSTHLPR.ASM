****************************************************************
* 2. HLASM HELPER ROUTINE
****************************************************************
CUSTHLPR CSECT
         STM   R14,R12,12(R13)     SAVE REGISTERS
         LR    R12,R15             ESTABLISH BASE REGISTER
         USING CUSTHLPR,R12        TELL ASSEMBLER
         LA    R11,SAVEAREA        ADDRESS OF MY SAVEAREA
         ST    R13,4(R11)          STORE BACKWARD POINTER
         ST    R11,8(R13)          STORE FORWARD POINTER
         LR    R13,R11             R13 POINTS TO MY SAVEAREA
         
* Get parameters (COBOL passes by reference)
         LM    R2,R6,0(R1)         LOAD PARAMETER ADDRESSES
         
* R2 = BASE AMOUNT
* R3 = INTEREST RATE
* R4 = TAX RATE
* R5 = RESULT ADDRESS
* R6 = DATE RESULT ADDRESS

* Load the values
         ZAP   BASEAMT,0(8,R2)     LOAD BASE AMOUNT
         ZAP   INTRATE,0(4,R3)     LOAD INTEREST RATE
         ZAP   TAXRATE,0(4,R4)     LOAD TAX RATE
         
* Perform complex calculation (For example: ((Base + Interest) * (1-Tax)) rounded special way)
         ZAP   WORKFLD1,BASEAMT    COPY BASE
         MP    WORKFLD1,INTRATE    MULTIPLY BY INTEREST
         SRP   WORKFLD1,64-3,5     SCALE AND ROUND
         AP    WORKFLD1,BASEAMT    ADD ORIGINAL BASE
         
         ZAP   WORKFLD2,P100       CONSTANT 1.00
         SP    WORKFLD2,TAXRATE    SUBTRACT TAX (1-TAX)
         
         ZAP   WORKFLD3,WORKFLD1   COPY INTERIM RESULT
         MP    WORKFLD3,WORKFLD2   FINAL MULTIPLICATION
         SRP   WORKFLD3,64-2,5     SCALE AND ROUND
         
* Store result
         MVC   0(8,R5),WORKFLD3    STORE CALCULATION RESULT
         
* Get current date in Julian format - use TIME macro
         TIME  BIN                 GET SYSTEM TIME AND DATE
         ST    R1,BINDATE          STORE DATE
         MVC   0(4,R6),BINDATE     RETURN DATE TO CALLER
         
* Return to caller
         L     R13,4(R13)          RESTORE R13
         LM    R14,R12,12(R13)     RESTORE REGISTERS
         SR    R15,R15             SET RETURN CODE TO ZERO
         BR    R14                 RETURN TO CALLER
         
* Data areas
SAVEAREA DS    18F                 REGISTER SAVE AREA
BASEAMT  DS    PL8                 BASE AMOUNT
INTRATE  DS    PL4                 INTEREST RATE
TAXRATE  DS    PL4                 TAX RATE
WORKFLD1 DS    PL16                CALCULATION WORK FIELD 1
WORKFLD2 DS    PL8                 CALCULATION WORK FIELD 2
WORKFLD3 DS    PL8                 RESULT FIELD
BINDATE  DS    F                   BINARY DATE
P100     DC    P'100'              CONSTANT FOR 1.00
         
         LTORG
         END   CUSTHLPR