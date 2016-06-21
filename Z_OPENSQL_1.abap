REPORT Z_OPENSQL_1.

********************
*SELECT.
*
********************
*INSERT
*
********************
*UPDATE.
*
********************
*MODIFY.
*
********************
*DELETE.
*
*SY-SUBRC = 0

********************

DATA  WA_EMPLOYEES    LIKE ZEMPLOYEES.

*WA_EMPLOYEES-EMPLOYEE   = '10000006'.
*WA_EMPLOYEES-SURNAME    = 'WESTMORE'.
*WA_EMPLOYEES-FORENAME   = 'BRUCE'.
*WA_EMPLOYEES-TITLE      = 'MR'.
*WA_EMPLOYEES-DOB        = '20160101'.
*********************
** Insert Statement
*
*INSERT ZEMPLOYEES FROM WA_EMPLOYEES.
*
*IF SY-SUBRC = 0.
*  WRITE 'Record Inserted Correctly'.
*ELSE.
*  WRITE: 'We have a return code of ', SY-SUBRC.
*ENDIF.

*******************
* Update Statement

*WA_EMPLOYEES-EMPLOYEE   = '10000006'.
*WA_EMPLOYEES-SURNAME    = 'EASTMORE'.
*WA_EMPLOYEES-FORENAME   = 'ANDY'.
*WA_EMPLOYEES-TITLE      = 'MR'.
*WA_EMPLOYEES-DOB        = '20160101'.
*
*UPDATE ZEMPLOYEES FROM WA_EMPLOYEES.
*
*IF SY-SUBRC = 0.
*  WRITE 'Record Updated Correctly'.
*ELSE.
*  WRITE: 'We have a return code of ', SY-SUBRC.
*ENDIF.

*******************
* Modify Statement

*WA_EMPLOYEES-EMPLOYEE   = '10000006'.
*WA_EMPLOYEES-SURNAME    = 'SOUTHMORE'.
*WA_EMPLOYEES-FORENAME   = 'ANDY'.
*WA_EMPLOYEES-TITLE      = 'MR'.
*WA_EMPLOYEES-DOB        = '20160101'.
*
*MODIFY ZEMPLOYEES FROM WA_EMPLOYEES.
*
*IF SY-SUBRC = 0.
*  WRITE: / 'Record Modified Correctly'.
*ELSE.
*  WRITE: / 'We have a return code of ', SY-SUBRC.
*ENDIF.
*
*CLEAR WA_EMPLOYEES.
*******************

WA_EMPLOYEES-EMPLOYEE   = '10000006'.
WA_EMPLOYEES-SURNAME    = 'SOUTHMORE'.
WA_EMPLOYEES-FORENAME   = 'ANDY'.
WA_EMPLOYEES-TITLE      = 'MR'.
WA_EMPLOYEES-DOB        = '20160101'.

MODIFY ZEMPLOYEES FROM WA_EMPLOYEES.

IF SY-SUBRC = 0.
  WRITE: / 'Record Modified Correctly'.
ELSE.
  WRITE: / 'We have a return code of ', SY-SUBRC.
ENDIF.

CLEAR WA_EMPLOYEES.
*******************
* Delete Statement
CLEAR WA_EMPLOYEES.

WA_EMPLOYEES-EMPLOYEE   = '10000006'.

DELETE ZEMPLOYEES FROM WA_EMPLOYEES.

IF SY-SUBRC = 0.
  WRITE: / 'Record Deleted Correctly'.
ELSE.
  WRITE: / 'We have a return code of ', SY-SUBRC.
ENDIF.

CLEAR WA_EMPLOYEES.
