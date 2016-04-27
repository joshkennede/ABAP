REPORT Z_ITERATIONS_1.

*TABLES: ZEMPLOYEES.
*
*
*SELECT * FROM ZEMPLOYEES WHERE SURNAME = 'JONES'.
*  WRITE / ZEMPLOYEES.
*ENDSELECT.

DATA: A   TYPE I,
      B   TYPE I,
      C   TYPE I.


A = 0.
C = 0.
************************************
* DO Loop Statement

*DO 15 TIMES.
*  A = A + 1.
*  WRITE / A.
*ENDDO.

************************************
* Nested DO Loop Statement

*DO 15 TIMES.
*  A = A + 1.
*  WRITE: / 'Outer Loop Cycle: ', A.
*  DO 10 TIMES.
*    B = B + 1.
*    WRITE: / 'Inner Loop Cycle: ', B.
*  ENDDO.
*  C = C + B.
*ENDDO.
*C = C + A.
*WRITE: / 'Total Iterations: ', C.

**************************************
* WHILE Statement

*WHILE A <> 15.
*  A = A + 1.
*  WRITE: / 'Loop cycle:  ', A.
*  B = 0.
*  WHILE B <> 10.
*    B = B + 1.
*    WRITE: / 'Loop cycle:   ', B.
*  ENDWHILE.
*  C = C + B.
*ENDWHILE.
*C = C + A.
*WRITE: / 'Total Iterations:   ', C.

**************************************
* Loop Termination - Continue

*DO 15 TIMES.
*  A = A + 1.
*  IF SY-INDEX = 2.
*    CONTINUE.
*  ENDIF.
*  WRITE: / 'Outer Loop cycle: ', A.
*ENDDO.

**************************************
* Loop Termination - Check

*CHECK A = 1.
*
*DO 15 TIMES.
*  A = A + 1.
*  CHECK SY-INDEX <> 2.
*  WRITE: / 'Outer Loop cycle: ', A.
*ENDDO.

**************************************
* Loop Termination - Exit
*
DO 15 TIMES.
  A = A + 1.
  IF SY-INDEX = 3.
    EXIT.
  ENDIF.
  WRITE: / 'Outer Loop cycle: ', A.
ENDDO.
WRITE: / 'Filler'.
WRITE: / 'Filler'.

**************************************

WRITE:/ 'ABAP System Variables'.
WRITE:/ 'Client : ', SY-MANDT.
WRITE:/ 'User   : ', SY-UNAME.
WRITE:/ 'Date   : ', SY-DATUM.
WRITE:/ 'Time   : ', SY-UZEIT.
