REPORT Z_EMPLOYEE_LIST_02.

TABLES ZEMPLOYEES.

************************************************************************
*ULINE.
*
*SELECT * FROM ZEMPLOYEES.         " Basic Select Loop with a LINE-BREAK
*  WRITE ZEMPLOYEES.               " after the first row is output.
*  WRITE /.
*ENDSELECT.
*
*CONSTANTS MYCONSTANT01 TYPE P DECIMALS 1 VALUE '6.6'.
*CONSTANTS MYCONSTANT02 TYPE P VALUE 6.


DATA INTEGER01        TYPE I VALUE 22.
DATA PACKED_DECIMAL01 TYPE P DECIMALS 1 VALUE '5.5'.
DATA RESULT           LIKE PACKED_DECIMAL01.

RESULT  = INTEGER01 + PACKED_DECIMAL01.
WRITE / RESULT.

RESULT  = INTEGER01 - PACKED_DECIMAL01.
WRITE / RESULT.

RESULT  = INTEGER01 / PACKED_DECIMAL01.
WRITE / RESULT.

RESULT  = INTEGER01 * PACKED_DECIMAL01.
WRITE / RESULT.

************************************************************************

DATA NUM1           TYPE P DECIMALS 2 VALUE '5.55'.
DATA RESULT1        TYPE I.

RESULT1 = NUM1.

ULINE.
WRITE / RESULT1.

************************************************************************
*Standard Division
DATA NUMA           TYPE P DECIMALS 2 VALUE '5.45'.
DATA NUMB           TYPE P DECIMALS 2 VALUE '1.48'.
DATA RESULT2        TYPE P DECIMALS 2.

RESULT2 = NUMA / NUMB.

ULINE.
WRITE / RESULT2.

************************************************************************
*Integer Division
DATA NUMC           TYPE P DECIMALS 2 VALUE '5.45'.
DATA NUMD           TYPE P DECIMALS 2 VALUE '1.48'.
DATA RESULT3        TYPE P DECIMALS 2.

RESULT3 = NUMC DIV NUMD.

ULINE.
WRITE / RESULT3.

************************************************************************
*Remainder Division
DATA NUME         TYPE P DECIMALS 2 VALUE '5.45'.
DATA NUMF         TYPE P DECIMALS 2 VALUE '1.48'.
DATA RESULT4      TYPE P DECIMALS 2.

RESULT4 = NUME MOD NUMF.

ULINE.
WRITE / RESULT4.
