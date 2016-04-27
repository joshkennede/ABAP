REPORT Z_EMPLOYEE_LIST_01 LINE-SIZE 132.

TABLES ZEMPLOYEES.

************************************************************************
SELECT * FROM ZEMPLOYEES.                     " Basic Select Loop
  WRITE ZEMPLOYEES.
ENDSELECT.

ULINE.

SELECT * FROM ZEMPLOYEES.                     " Basic Select Loop with a LINE-BREAK
  WRITE / ZEMPLOYEES.
ENDSELECT.

ULINE.

SELECT * FROM ZEMPLOYEES.                     " Basic Select Loop with a LINE-BREAK
  WRITE ZEMPLOYEES.                           " after the first row is output.
  WRITE /.
ENDSELECT.

ULINE.

SKIP 2.
SELECT * FROM ZEMPLOYEES.                     " Basic Select Loop with a SKIP statement
  WRITE / ZEMPLOYEES.
ENDSELECT.

ULINE.

SKIP 2.
SELECT * FROM ZEMPLOYEES.                     " Basic Select Loop with individual fields
  WRITE / ZEMPLOYEES-SURNAME.                 " being output
  WRITE / ZEMPLOYEES-FORENAME.
  WRITE / ZEMPLOYEES-DOB.
ENDSELECT.

ULINE.

SKIP 2.
SELECT * FROM ZEMPLOYEES.                     " Chain Statements
  WRITE: / ZEMPLOYEES-SURNAME,
           ZEMPLOYEES-FORENAME,
           ZEMPLOYEES-DOB.
ENDSELECT.
