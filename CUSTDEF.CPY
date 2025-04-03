           
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