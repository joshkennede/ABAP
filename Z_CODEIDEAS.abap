*&---------------------------------------------------------------------*
*&  Report  Z_CODEIDEAS
*&
*&---------------------------------------------------------------------*
*&
*&  Written by Josh Kennedy 07.26.16
*&
*&---------------------------------------------------------------------*

REPORT z_codeideas.


*Global Variables

DATA: gv_fizzbuzz   TYPE c LENGTH 4.
DATA: gv_randnum    TYPE p LENGTH 2 DECIMALS 2.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ideas WITH FRAME TITLE text-007.

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
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-008.
PARAMETERS: cpern RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-009.
PARAMETERS: calcu RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: pa_int1(5)  TYPE n DEFAULT 1,
            pa_op       TYPE c LENGTH 1 DEFAULT '+',
            pa_int2(5)  TYPE n DEFAULT 1.
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
  IF cpern = 'X'.
    PERFORM count_unique_pernr.
  ENDIF.
  IF calcu = 'X'.
    PERFORM calculator.
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

  DATA: a     LIKE sy-ucomm,
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

*&---------------------------------------------------------------------*
*&      Form  count_unique_pernr
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM count_unique_pernr.

  TABLES: zhr_mini_rtwpbp.

  TYPES: BEGIN OF type_in,
         pernr LIKE zhr_mini_rtwpbp-pernr,
         END OF type_in.

  DATA: itab_table  TYPE STANDARD TABLE OF type_in.
  DATA: wa_table    TYPE type_in.

  TYPES: BEGIN OF type_out,
         pernr LIKE zhr_mini_rtwpbp-pernr,
         count TYPE i,
         END OF type_out.

  DATA: it_main         TYPE TABLE OF type_out,
        wa_main         TYPE type_out.

  DATA: curr_pernr      LIKE zhr_mini_rtwpbp-pernr,
        prev_pernr      LIKE zhr_mini_rtwpbp-pernr.
  DATA: rec_tot         TYPE i VALUE 1.

  SELECT pernr
      FROM zhr_mini_rtwpbp UP TO 10000 ROWS
      INTO TABLE itab_table
      "WHERE begda <= sy-datum
      "AND   endda >= sy-datum
      ORDER BY pernr.

  LOOP AT itab_table INTO wa_table.
    curr_pernr = wa_table-pernr.
    IF prev_pernr <> curr_pernr.
      IF prev_pernr IS NOT INITIAL.
        CLEAR wa_main.
        wa_main-pernr = prev_pernr.
        wa_main-count = rec_tot.
        APPEND wa_main TO it_main.
        rec_tot = 1.
      ENDIF.
    ELSE.
      rec_tot = rec_tot + 1.
    ENDIF.
    prev_pernr = curr_pernr.
  ENDLOOP.
  IF curr_pernr = prev_pernr.
    CLEAR wa_main.
    wa_main-pernr = prev_pernr.
    wa_main-count = rec_tot.
    APPEND wa_main TO it_main.
    rec_tot = 1.
  ENDIF.

  LOOP AT itab_table INTO wa_table.
    wa_main-pernr = wa_table-pernr.
    wa_main-count = rec_tot.
    COLLECT wa_main INTO it_main.
  ENDLOOP.

* Sort internal table by record sum.
  SORT it_main BY count DESCENDING.

  WRITE: /1 'Pernr', 17 'Total Records'.
  ULINE.

* Loop through records based on highest record count and write to screen.
  LOOP AT it_main INTO wa_main.
    WRITE:/ wa_main-pernr,
            wa_main-count.
    CLEAR: wa_main.
  ENDLOOP.

  ULINE.

  WRITE:/ 'Date:', 17 sy-datum.
  WRITE:/ 'Environment:', 17 sy-sysid.
  WRITE:/ 'User:', 17 sy-uname.
  WRITE:/ 'Current Time:', 17 sy-uzeit.

ENDFORM.                    "count_unique_pernr

*&---------------------------------------------------------------------*
*&      Form  calculator
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calculator.

  DATA: lv_result   TYPE p LENGTH 16 DECIMALS 2.

  CASE pa_op.
    WHEN '+'.
      lv_result = pa_int1 + pa_int2.
    WHEN '-'.
      lv_result = pa_int1 - pa_int2.
    WHEN '*'.
      lv_result = pa_int1 * pa_int2.
    WHEN '/'.
      lv_result = pa_int1 / pa_int2.
  ENDCASE.

  WRITE: 'Result:', lv_result.

ENDFORM.                    "calculator
