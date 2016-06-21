REPORT Z_LOGIC_1.

***************************************
*
*DATA  WA_EMPLOYEES    LIKE ZEMPLOYEES.
*
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
*  WRITE: 'We have a return code of', SY-SUBRC.
*ENDIF.

DATA: SURNAME(15)   TYPE C,
      FORNAME(15)   TYPE C,
      LOCATION(2)   TYPE C.

SURNAME   = 'Smith'.
FORNAME   = 'John'.
LOCATION  = 'UK'.

***************************
* IF Statement

IF SURNAME = 'Smith'.
  WRITE 'Youve won a car!'.
ELSEIF SURNAME = 'Brown'.
  WRITE 'Youve won a boat!'.
ELSEIF SURNAME = 'Jones'.
  WRITE 'Youve won a plane!'.
ELSEIF SURNAME = 'Andrews'.
  WRITE 'Youve won a house!'.
ELSE.
  WRITE 'Unlucky! You go home empty handed'.
ENDIF.

IF SURNAME = 'Smith'.
  IF FORNAME = 'John'.
    WRITE 'Youve won a car!'.
  ELSE.
    WRITE 'Oooo, so close'.
  ENDIF.
ENDIF.
***************************
* Nested IF Statement(s)

*IF SURNAME = 'Smith'.
*  WRITE 'Youve won a car!'.
*  WRITE 'Youve won a car!'.
*  IF FORNAME = 'John'.
*    WRITE 'Youve won a car!'.
*    WRITE 'Youve won a car!'.
*    IF LOCATION = 'UK'.
*      WRITE 'Youve won a car!'.
*      WRITE 'Youve won a car!'.
*    ELSE.
*      WRITE 'Oooo, so close'.
*    ENDIF.
*    WRITE 'Youve won a car'.
*  ENDIF.
*ENDIF.
*

***************************
* IF Statement with operators

IF SURNAME = 'Smith' AND FORNAME = 'John'.
  WRITE 'Youve won a car'.
ENDIF.

***************************
* CASE Statement              -checks 'SURNAME' against multiple cases and proceeds with an action.

CASE SURNAME.
  WHEN 'Smith'.
    WRITE 'Youve won a car!'.
  WHEN 'Jones'.
    WRITE 'Youve won a plane!'.
  WHEN 'Green'.
    WRITE 'Youve won a boat'.
  WHEN OTHERS.
    WRITE 'Unlucky!'.
ENDCASE.
