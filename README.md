

# CICS Application with HLASM, COBOL, and DB2
Parts of this application are written in HLASM, and COBOL, all within a CICS environment using DB2 as database. 


## CICS Application with HLASM, COBOL, and DB2

Explanation of the sample application:

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

