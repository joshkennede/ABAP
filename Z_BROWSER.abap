*&---------------------------------------------------------------------*
*& Report  Z_BROWSER
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_browser.


SELECTION-SCREEN BEGIN OF BLOCK selectcrit WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
PARAMETER abc AS CHECKBOX DEFAULT 'X'.
SELECTION-SCREEN COMMENT 04(22) text-002.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN COMMENT 01(04) text-003.
PARAMETER urlbx(100).
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK selectcrit.

*****************************************
*****************************************

IF abc = 'X'.
  PERFORM exec_browser.
ELSEIF abc <> 'X'.
  PERFORM hello_loop.
ENDIF.

*&---------------------------------------------------------------------*
*&      Form  exec_browser
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM exec_browser.

  DATA: jk_url(100).

  jk_url = urlbx.

  CALL FUNCTION 'CALL_BROWSER'
    EXPORTING
      url = jk_url.


ENDFORM.                    "exec_browser

*&---------------------------------------------------------------------*
*&      Form  hello_loop
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM hello_loop.

  DO 10 TIMES.
    WRITE:/ 'Hello! This is not a webpage!'.
    ULINE.
  ENDDO.

ENDFORM.                    "hello_loop
