

Parts of this application are written in HLASM, and COBOL, all within a CICS environment using DB2 as database. 

```plaintext
****************************************************************
* FILE STRUCTURE: COMBINED APPLICATION COMPONENTS
****************************************************************
* CUSTMAIN.CBL  - Main COBOL CICS program (driver)
* CUSTHLPR.ASM  - HLASM helper routine for complex calculations
* CUSTDB2.CBL   - COBOL DB2 data access component
* CUSTDEF.CPY   - Copybook with shared data definitions
* CUSTMAP.BMS   - BMS map definition for screen
****************************************************************

****************************************************************
* 1. MAIN PROGRAM (COBOL)
****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTMAIN.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * Common data definitions
       COPY CUSTDEF.
       
      * Local working storage
       01  WS-COMMAREA.
           05  CA-CUSTOMER-ID       PIC X(6).
           05  CA-ACTION-FLAG       PIC X(1).
           05  CA-RETURN-STATUS     PIC X(1).
           
       01  CALCULATION-FIELDS.
           05  WS-INTEREST-RATE     PIC S9(3)V9(5) COMP-3 VALUE 0.05.
           05  WS-TAX-RATE          PIC S9(3)V9(5) COMP-3 VALUE 0.08.
           05  WS-BASE-AMOUNT       PIC S9(9)V99   COMP-3.
           05  WS-CALC-RESULT       PIC S9(9)V99   COMP-3.
           05  WS-DATE-CONV-RESULT  PIC S9(8)      COMP.
           
       PROCEDURE DIVISION.
       0000-MAIN-PROCESS.
           EXEC CICS 
               HANDLE AID
               CLEAR(9000-CLEAR-EXIT)
               PF3(9000-EXIT)
               PF12(9000-EXIT)
           END-EXEC.
           
           EXEC CICS
               HANDLE CONDITION
               ERROR(8000-ERROR-ROUTINE)
           END-EXEC.
           
           EXEC CICS
               RECEIVE MAP('CUSTMAP')
               MAPSET('CUSTSET')
           END-EXEC.
           
           IF EIBAID = DFHENTER
               PERFORM 1000-PROCESS-CUSTOMER
           END-IF.
           
           EXEC CICS
               SEND MAP('CUSTMAP')
               MAPSET('CUSTSET')
               ERASE
           END-EXEC.
           
           EXEC CICS
               RETURN TRANSID('CUST')
               COMMAREA(WS-COMMAREA)
               LENGTH(8)
           END-EXEC.
           
       1000-PROCESS-CUSTOMER.
           MOVE IN-CUSTOMER-ID TO CA-CUSTOMER-ID.
           
           EXEC CICS
               LINK PROGRAM('CUSTDB2')
               COMMAREA(CUSTOMER-RECORD)
               LENGTH(150)
           END-EXEC.
           
           IF CUST-STATUS = 'F'
               MOVE CUST-NAME TO OUT-CUSTOMER-NAME
               MOVE CUST-ADDRESS TO OUT-CUSTOMER-ADDRESS
               MOVE CUST-BALANCE TO OUT-CUSTOMER-BALANCE
               
              * Call HLASM routine for complex calculation
               MOVE CUST-BALANCE TO WS-BASE-AMOUNT
               CALL 'CUSTHLPR' USING WS-BASE-AMOUNT,
                                     WS-INTEREST-RATE,
                                     WS-TAX-RATE,
                                     WS-CALC-RESULT,
                                     WS-DATE-CONV-RESULT
                                     
               MOVE WS-CALC-RESULT TO OUT-CALC-AMOUNT
               MOVE WS-DATE-CONV-RESULT TO OUT-JULIAN-DATE
               
               MOVE 'Customer found' TO OUT-MESSAGE
           ELSE
               MOVE 'Customer not found' TO OUT-MESSAGE
           END-IF.
           
       8000-ERROR-ROUTINE.
           MOVE 'Transaction error occurred' TO OUT-MESSAGE.
           EXEC CICS SEND MAP('CUSTMAP') MAPSET('CUSTSET') ERASE END-EXEC.
           EXEC CICS RETURN END-EXEC.
           
       9000-CLEAR-EXIT.
           MOVE LOW-VALUES TO CUSTMAPO.
           MOVE 'Enter customer ID' TO OUT-MESSAGE.
           EXEC CICS SEND MAP('CUSTMAP') MAPSET('CUSTSET') ERASE END-EXEC.
           EXEC CICS RETURN END-EXEC.
           
       9000-EXIT.
           EXEC CICS XCTL PROGRAM('CUSTMENU') END-EXEC.

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

****************************************************************
* 3. DB2 ACCESS COMPONENT (COBOL)
****************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. CUSTDB2.
       
       ENVIRONMENT DIVISION.
       
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * DB2 communication area
       EXEC SQL 
           INCLUDE SQLCA
       END-EXEC.
       
      * Host variables for DB2
       01  DB2-CUSTOMER-RECORD.
           05  DB-ID               PIC X(6).
           05  DB-NAME             PIC X(30).
           05  DB-ADDRESS          PIC X(50).
           05  DB-BALANCE          PIC S9(9)V99 COMP-3.
           05  DB-LAST-PAYMENT     PIC S9(9)V99 COMP-3.
           05  DB-ACCOUNT-TYPE     PIC X(1).
           05  DB-CREDIT-LIMIT     PIC S9(9)V99 COMP-3.
       
       01  SQL-ERROR-MESSAGE       PIC X(80).
       
       LINKAGE SECTION.
      * Common data structure shared with calling program
       COPY CUSTDEF.
       
       PROCEDURE DIVISION USING CUSTOMER-RECORD.
       0000-MAIN-PARA.
           MOVE CUST-ID TO DB-ID.
           
           EXEC SQL
               SELECT CUST_NAME, CUST_ADDRESS, CUST_BALANCE,
                      LAST_PAYMENT, ACCOUNT_TYPE, CREDIT_LIMIT
               INTO :DB-NAME, :DB-ADDRESS, :DB-BALANCE,
                    :DB-LAST-PAYMENT, :DB-ACCOUNT-TYPE, :DB-CREDIT-LIMIT
               FROM CUSTOMER
               WHERE CUST_ID = :DB-ID
           END-EXEC.
           
           EVALUATE SQLCODE
               WHEN 0
                   MOVE 'F' TO CUST-STATUS
                   MOVE DB-NAME TO CUST-NAME
                   MOVE DB-ADDRESS TO CUST-ADDRESS
                   MOVE DB-BALANCE TO CUST-BALANCE
                   MOVE DB-LAST-PAYMENT TO CUST-LAST-PAYMENT
                   MOVE DB-ACCOUNT-TYPE TO CUST-ACCOUNT-TYPE
                   MOVE DB-CREDIT-LIMIT TO CUST-CREDIT-LIMIT
               WHEN 100
                   MOVE 'N' TO CUST-STATUS
               WHEN OTHER
                   MOVE 'E' TO CUST-STATUS
                   MOVE SQLCODE TO CUST-ERROR-CODE
           END-EVALUATE.
           
           EXEC CICS
               RETURN
           END-EXEC.
           
****************************************************************
* 4. COMMON DATA DEFINITIONS (COPYBOOK)
****************************************************************
      * CUSTDEF.CPY - Common Data Definitions
       01  CUSTOMER-RECORD.
           05  CUST-ID             PIC X(6).
           05  CUST-NAME           PIC X(30).
           05  CUST-ADDRESS        PIC X(50).
           05  CUST-BALANCE        PIC S9(9)V99 COMP-3.
           05  CUST-LAST-PAYMENT   PIC S9(9)V99 COMP-3.
           05  CUST-ACCOUNT-TYPE   PIC X(1).
           05  CUST-CREDIT-LIMIT   PIC S9(9)V99 COMP-3.
           05  CUST-STATUS         PIC X(1).
               88  CUST-FOUND        VALUE 'F'.
               88  CUST-NOT-FOUND    VALUE 'N'.
               88  CUST-ERROR        VALUE 'E'.
           05  CUST-ERROR-CODE     PIC S9(9) COMP.

      * Map fields
       01  CUSTMAPI.
           05  FILLER              PIC X(12).
           05  IN-CUSTOMER-ID      PIC X(6).
           05  FILLER              PIC X(263).
       
       01  CUSTMAPO REDEFINES CUSTMAPI.
           05  FILLER              PIC X(12).
           05  OUT-CUSTOMER-ID     PIC X(6).
           05  OUT-CUSTOMER-NAME   PIC X(30).
           05  OUT-CUSTOMER-ADDRESS PIC X(50).
           05  OUT-CUSTOMER-BALANCE PIC Z,ZZZ,ZZ9.99.
           05  OUT-CALC-AMOUNT     PIC Z,ZZZ,ZZ9.99.
           05  OUT-JULIAN-DATE     PIC 9(7).
           05  OUT-MESSAGE         PIC X(50).
           05  FILLER              PIC X(100).

****************************************************************
* 5. BMS MAP DEFINITION
****************************************************************
CUSTSET  DFHMSD TYPE=&SYSPARM,                                         X
               LANG=COBOL,                                             X
               MODE=INOUT,                                             X
               TERM=3270-2,                                            X
               CTRL=FREEKB,                                            X
               STORAGE=AUTO,                                           X
               TIOAPFX=YES
*
CUSTMAP  DFHMDI SIZE=(24,80)
*
         DFHMDF POS=(1,1),                                             X
               LENGTH=20,                                              X
               ATTRB=(NORM,PROT),                                      X
               COLOR=BLUE,                                             X
               INITIAL='CUSTOMER INQUIRY'
*
         DFHMDF POS=(3,1),                                             X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Customer ID:'
*
CUSTID   DFHMDF POS=(3,14),                                            X
               LENGTH=6,                                               X
               ATTRB=(NORM,UNPROT,IC),                                 X
               INITIAL='______'
*
         DFHMDF POS=(5,1),                                             X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Name:'
*
NAME     DFHMDF POS=(5,14),                                            X
               LENGTH=30,                                              X
               ATTRB=(NORM,PROT)
*
         DFHMDF POS=(6,1),                                             X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Address:'
*
ADDR     DFHMDF POS=(6,14),                                            X
               LENGTH=50,                                              X
               ATTRB=(NORM,PROT)
*
         DFHMDF POS=(7,1),                                             X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Balance:'
*
BALANCE  DFHMDF POS=(7,14),                                            X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT)
*
         DFHMDF POS=(8,1),                                             X
               LENGTH=15,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Calc Amount:'
*
CALCAMT  DFHMDF POS=(8,17),                                            X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT)
*
         DFHMDF POS=(9,1),                                             X
               LENGTH=13,                                              X
               ATTRB=(NORM,PROT),                                      X
               INITIAL='Julian Date:'
*
JDATE    DFHMDF POS=(9,15),                                            X
               LENGTH=7,                                               X
               ATTRB=(NORM,PROT)
*
         DFHMDF POS=(22,1),                                            X
               LENGTH=12,                                              X
               ATTRB=(NORM,PROT),                                      X
               COLOR=RED,                                              X
               INITIAL='Message:'
*
MESSAGE  DFHMDF POS=(22,14),                                           X
               LENGTH=50,                                              X
               ATTRB=(BRT,PROT)
*
         DFHMDF POS=(24,1),                                            X
               LENGTH=79,                                              X
               ATTRB=(NORM,PROT),                                      X
               COLOR=BLUE,                                             X
               INITIAL='F3=Exit  F12=Cancel'
*
         DFHMSD TYPE=FINAL
         END

```

## Mixed-Language CICS Application with HLASM, COBOL, and DB2

Let me explain how the mixed-language approach works in this example application:

### Component Structure

1. **CUSTMAIN.CBL (COBOL)**: Main driver program that:
   - Handles the CICS user interface
   - Controls program flow
   - Links to other components
   - Calls the HLASM routine for specialized calculations

2. **CUSTHLPR.ASM (HLASM)**: Specialized helper routine that:
   - Performs complex financial calculations efficiently
   - Accesses system services like the TIME macro
   - Returns results to the COBOL program

3. **CUSTDB2.CBL (COBOL)**: DB2 database interface that:
   - Contains all SQL statements
   - Handles database errors
   - Communicates via a shared data structure

4. **CUSTDEF.CPY**: Copybook with common data definitions used by both COBOL programs

5. **CUSTMAP.BMS**: Screen definition for the user interface

### How the Integration Works

#### 1. Data Sharing

Common data structures are defined in the CUSTDEF.CPY copybook and shared between programs. This ensures consistent data formats across components.

#### 2. COBOL-to-HLASM Interface

```cobol
CALL 'CUSTHLPR' USING WS-BASE-AMOUNT,
                      WS-INTEREST-RATE,
                      WS-TAX-RATE,
                      WS-CALC-RESULT,
                      WS-DATE-CONV-RESULT
```

This COBOL statement calls the HLASM routine, passing parameters by reference (addresses). The HLASM routine needs to:
1. Save registers using standard linkage conventions
2. Extract parameter addresses from register R1
3. Load and process data
4. Store results to the addresses provided
5. Restore registers and return

#### 3. COBOL-to-COBOL Interface (CICS LINK)

```cobol
EXEC CICS
    LINK PROGRAM('CUSTDB2')
    COMMAREA(CUSTOMER-RECORD)
    LENGTH(150)
END-EXEC.
```

This uses CICS LINK to transfer control to the DB2 interface program, passing a communication area (COMMAREA) with customer data.

#### 4. DB2 Integration

The CUSTDB2 program contains SQL statements embedded in COBOL:

```cobol
EXEC SQL
    SELECT CUST_NAME, CUST_ADDRESS, CUST_BALANCE,
           LAST_PAYMENT, ACCOUNT_TYPE, CREDIT_LIMIT
    INTO :DB-NAME, :DB-ADDRESS, :DB-BALANCE,
         :DB-LAST-PAYMENT, :DB-ACCOUNT-TYPE, :DB-CREDIT-LIMIT
    FROM CUSTOMER 
    WHERE CUST_ID = :DB-ID
END-EXEC.
```

Before compilation, this code is processed by the DB2 precompiler, which converts SQL statements into COBOL calls to DB2 services.

### Reasons for Using Mixed Languages

1. **COBOL for Business Logic and UI**:
   - Better readability for business rules
   - Standard CICS functions like screen handling
   - Easier maintenance for frequently changing business logic
   - Natural fit for DB2 database access

2. **HLASM for Performance-Critical Code**:
   - Precise control over CPU instructions
   - Direct access to system services (TIME macro)
   - Performance optimization for complex calculations
   - Ability to handle bit-level operations easily

### Compilation and Execution Flow

1. Each component is compiled separately:
   - COBOL programs through the COBOL compiler
   - HLASM through the assembler
   - Both with appropriate CICS and DB2 options

2. The components are link-edited together into a load module

3. At runtime:
   - The CICS transaction manager loads the main program
   - It handles calls and links between components
   - DB2 processes SQL requests
   - The user interacts through the BMS map

This approach combines the strengths of each language while maintaining a modular design. Organizations often use this strategy to leverage existing code while adding new functionality.

Would you like me to explain any specific aspect of this mixed-language approach in more detail?