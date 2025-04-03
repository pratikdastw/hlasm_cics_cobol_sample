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