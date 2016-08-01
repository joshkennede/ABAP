*&---------------------------------------------------------------------*
*&  Report  Z_FIZZBUZZ
*&
*&---------------------------------------------------------------------*
*&
*&  Written by Josh Kennedy 07.26.16
*&
*&---------------------------------------------------------------------*

REPORT z_fizzbuzz.


*Global Variables

DATA: gv_fizzbuzz   TYPE c LENGTH 4.
DATA: gv_randnum    TYPE p LENGTH 2 DECIMALS 2.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ideas.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-001.
PARAMETERS: fzbz RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-002.
PARAMETERS: rndn RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-003.
PARAMETERS: helop RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-004.
PARAMETERS: delay RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-005.
PARAMETERS: prog RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-006.
PARAMETERS: syid RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK ideas.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

START-OF-SELECTION.

  IF fzbz = 'X'.
    PERFORM fizzbuzz.
    WRITE:/ gv_fizzbuzz.
  ENDIF.
  IF rndn = 'X'.
    PERFORM gen_rnd_num.
    WRITE:/ gv_randnum.
  ENDIF.
  IF helop = 'X'.
    PERFORM hello_loop.
  ENDIF.
  IF delay = 'X'.
    PERFORM delay_it.
  ENDIF.
  IF prog = 'X'.
    PERFORM prog_message.
  ENDIF.
  IF syid = 'X'.
    PERFORM syid_status.
  ENDIF.


END-OF-SELECTION.

*&----------------------------------------------------------------------*
*&      Form  fizzbuzz                                                  *
*&----------------------------------------------------------------------*
*&       Writing numbers up to 100 while replacing any int divisible by *
*&       3 with 'Fizz', any int divisible by 5 with 'Buzz' and any int  *
*&       divisible by both integers with 'FizzBuzz'.                    *
*&----------------------------------------------------------------------*
FORM fizzbuzz.

  DATA:   lv_mod3  TYPE i,
          lv_mod5  TYPE i.
  DATA:   lv_fizz  TYPE c LENGTH 4 VALUE 'Fizz',
          lv_buzz  TYPE c LENGTH 4 VALUE 'Buzz',
          lv_fb    TYPE c LENGTH 8.

  DO 100 TIMES.

    lv_mod3 = sy-index MOD 3.
    lv_mod5 = sy-index MOD 5.

    IF lv_mod3 = 0 AND lv_mod5 = 0.
      CONCATENATE lv_fizz lv_buzz INTO lv_fb.
      WRITE:/ lv_fb.
      CONTINUE.
    ENDIF.

    IF lv_mod3 = 0.
      WRITE:/ lv_fizz.
      CONTINUE.
    ENDIF.

    IF lv_mod5 = 0.
      WRITE:/ lv_buzz.
      CONTINUE.
    ENDIF.

    WRITE:/ sy-index.

  ENDDO.

  MOVE lv_buzz TO gv_fizzbuzz. " lv_buzz is '100' at this point so I am moving it to a global var
  " and printing it to the console.

ENDFORM.                    "fizzbuzz

*&---------------------------------------------------------------------*
*&      Form  GEN_RND_NUM                                              *
*&---------------------------------------------------------------------*
*       Generate random number for use elsewhere                       *
*----------------------------------------------------------------------*
FORM gen_rnd_num.

  DATA: randnum LIKE bbseg-wrbtr.

  CALL FUNCTION 'RANDOM_AMOUNT'
    EXPORTING
      rnd_min    = '1'
      rnd_max    = '100'
      valcurr    = 'DEM'
    IMPORTING
      rnd_amount = randnum
    EXCEPTIONS
      OTHERS     = 1.

  MOVE randnum TO gv_randnum. "Moving the local var to the global var and printing to the console.
  "WRITE:/ randnum.

ENDFORM.          " GEN_RND_NUM

*&---------------------------------------------------------------------*
*&      Form  hello_loop                                               *
*&---------------------------------------------------------------------*
*       Simple DO loop for iterating through a single idea.            *
*----------------------------------------------------------------------*
FORM hello_loop.

  DO 25 TIMES.
    WRITE:/ 'Hello!'.
  ENDDO.

ENDFORM.                    "hello_loop

*&---------------------------------------------------------------------*
*&      Form  delay_it
*&---------------------------------------------------------------------*
*       This program delays num(30) seconds
*----------------------------------------------------------------------*
FORM delay_it.

  DATA: time  LIKE sy-uzeit,
        num   TYPE i VALUE 5.

  time = sy-uzeit.
  time = time + num.

  DO.
    GET TIME.
    IF time < sy-uzeit.
      EXIT.
    ENDIF.

  ENDDO.

  WRITE:/ sy-uzeit, time, num.

ENDFORM.                    "delay_it

*&---------------------------------------------------------------------*
*&      Form  prog_message
*&---------------------------------------------------------------------*
*&      Progress message indicator
*&---------------------------------------------------------------------*
FORM prog_message.

  DATA: a LIKE sy-ucomm,
        b(10) TYPE c VALUE 'Testing...'.

  DO 999 TIMES.
    DO 999 TIMES.
      GET TIME.
      WRITE:/ b.
    ENDDO.
    a(3) = sy-index.a+3 = '%'.

    CALL FUNCTION 'SAPGUI_PROGRESS_INDICATOR'
      EXPORTING
        percentage = sy-index
        text       = a.
  ENDDO.

ENDFORM.                    "prog_message

*&---------------------------------------------------------------------*
*&      Form  syid_status
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM syid_status.

  WRITE:/10  sy-cprog,
        /10  sy-datum,
        /10  sy-dynnr,
        /10  sy-tleng,
        /10  sy-stepl,
        /10  sy-loopc,
        /10  sy-fdpos,
        /10  sy-dbsys.

ENDFORM.                    "syid_status
