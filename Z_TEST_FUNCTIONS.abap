REPORT Z_TEST_FUNCTIONS.

************************************************
* Round Decimals Pt.1

  DATA: STARTING_RESULT TYPE P DECIMALS 2.
  DATA: ENDING_RESULT   TYPE P DECIMALS 2.

  MOVE '74.4' TO STARTING_RESULT.

  CALL FUNCTION 'ROUND'
    EXPORTING
     DECIMALS            = -1
     INPUT               = STARTING_RESULT
     SIGN                = '-'
   IMPORTING
     OUTPUT              = ENDING_RESULT
* EXCEPTIONS
*   INPUT_INVALID       = 1
*   OVERFLOW            = 2
*   TYPE_INVALID        = 3
*   OTHERS              = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WRITE: / STARTING_RESULT, '= Original Value'.
  WRITE: / ENDING_RESULT, '= New Value'.
****************************************************
* Round Decimals Pt. 2

  MOVE '76.4' TO STARTING_RESULT.

  CALL FUNCTION 'ROUND'
    EXPORTING
     DECIMALS            = 0
     INPUT               = STARTING_RESULT
     SIGN                = '+'
   IMPORTING
     OUTPUT              = ENDING_RESULT
* EXCEPTIONS
*   INPUT_INVALID       = 1
*   OVERFLOW            = 2
*   TYPE_INVALID        = 3
*   OTHERS              = 4
            .
  IF SY-SUBRC <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

  WRITE: / STARTING_RESULT, '= Original Value'.
  WRITE: / ENDING_RESULT, '= New Value'.
