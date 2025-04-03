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