*&---------------------------------------------------------------------*
*&  Report  Z_CODEIDEAS
*&
*&---------------------------------------------------------------------*
*&
*&  Written by Josh Kennedy 07.26.16
*&
*&---------------------------------------------------------------------*

REPORT z_codeideas.

TABLES: zhr_mini_rtwpbp.

*Global Variables

DATA: gv_fizzbuzz   TYPE c LENGTH 4.
DATA: gv_randnum    TYPE p LENGTH 2 DECIMALS 2.

*&---------------------------------------------------------------------*
*& Selection Screen
*&---------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ideas WITH FRAME TITLE text-007.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN PUSHBUTTON (8) text-012 USER-COMMAND press.
SELECTION-SCREEN END OF LINE.
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
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-010.
PARAMETERS: split RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
*PARAMETERS: tstrg(25) TYPE c DEFAULT '2016,08,01,1630,20081754'.
*PARAMETER: pa_word(15)  TYPE c.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-011.
PARAMETER: p_txnu RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETER: p_text(25) TYPE c DEFAULT sy-sysid.
PARAMETER: p_len(2)   TYPE n DEFAULT '1'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK ideas.

*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*

AT SELECTION-SCREEN ON p_len.
*  IF p_len > 25.
*    MESSAGE 'P_LEN should be less or equal 25' TYPE 'E'.
*  ENDIF.
  IF sy-ucomm = 'PRESS'.
    MESSAGE 'These are code ideas' TYPE 'I'.
  ENDIF.

START-OF-SELECTION.

  IF fzbz = 'X'.
    PERFORM fizzbuzz.
    WRITE:/ gv_fizzbuzz.
  ELSEIF rndn = 'X'.
    PERFORM gen_rnd_num.
    WRITE:/ gv_randnum.
    CLEAR: gv_randnum.
  ELSEIF helop = 'X'.
    PERFORM hello_loop.
  ELSEIF delay = 'X'.
    PERFORM delay_it.
  ELSEIF prog = 'X'.
    PERFORM prog_message.
  ELSEIF syid = 'X'.
    PERFORM syid_status.
  ELSEIF cpern = 'X'.
    PERFORM count_unique_pernr.
  ELSEIF calcu = 'X'.
    PERFORM calculator.
  ELSEIF split = 'X'.
    PERFORM split_word.
  ELSEIF p_txnu = 'X'.
    PERFORM text_num_count.
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
*&      Simple DO loop for iterating through a single idea.            *
*&      Captures system time after processing and determines if the    *
*&      Time is even or odd and prints it to the console.              *
*----------------------------------------------------------------------*
FORM hello_loop.

  DATA: lv_time_start   LIKE sy-uzeit,
        lv_time_end     LIKE sy-uzeit,
        lv_time_diff    LIKE sy-uzeit.
  DATA: lv_remainder    TYPE p DECIMALS 2.
  DATA: lv_time_value   TYPE i VALUE 13.

  GET TIME.
  lv_time_start = sy-uzeit.

*  DO 150 TIMES.
*    WRITE: 'Hello', '...', 'World', '...', 'it is:', '...', sy-uzeit.
*  ENDDO.

  PERFORM prog_message.   "This is to cause delay (even though it is always the same time) between GET TIME statements.

  GET TIME.
  lv_time_end = sy-uzeit.

  IF lv_time_end > lv_time_start.
    lv_time_diff = lv_time_end - lv_time_start.
  ENDIF.

  lv_time_value =  lv_time_diff.
  lv_remainder = ( lv_time_value MOD 2 ).

  CASE lv_remainder.
    WHEN 0.
      WRITE:/ 'Even - ', sy-uzeit.
    WHEN OTHERS.
      WRITE:/ 'Odd - ', sy-uzeit.
  ENDCASE.

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
*&      Progress message indicator using percentage
*&---------------------------------------------------------------------*
FORM prog_message.

  DATA: a     LIKE sy-ucomm,
        b(10) TYPE c VALUE 'Testing...'.

  DO 400 TIMES.
    DO 400 TIMES.
      GET TIME.
      "WRITE:/ b.
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
*       Print System Fields
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
*&      From another pgm - Count unique pernrs in ZHR_MINI_RTWPBP
*&----------------------------------------------------------------------*
FORM count_unique_pernr.

  TYPES: BEGIN OF type_in,
         pernr LIKE zhr_mini_rtwpbp-pernr,
         END OF type_in.

  DATA:  itab_table  TYPE STANDARD TABLE OF type_in.
  DATA:  wa_table    TYPE type_in.

  TYPES: BEGIN OF type_out,
         pernr LIKE zhr_mini_rtwpbp-pernr,
         count TYPE i,
         END OF type_out.

  DATA:  it_main         TYPE TABLE OF type_out,
         wa_main         TYPE type_out.

  DATA:  curr_pernr      LIKE zhr_mini_rtwpbp-pernr,
         prev_pernr      LIKE zhr_mini_rtwpbp-pernr.
  DATA:  rec_tot         TYPE i VALUE 1.

  SELECT pernr
      FROM zhr_mini_rtwpbp
      UP TO 10000 ROWS
      INTO TABLE itab_table
      "WHERE pernr = '20081754'
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
*&      Basic Calculator for Add, Subtract, Multiply, Divide
*&----------------------------------------------------------------------*
FORM calculator.

  DATA: lv_result   TYPE p DECIMALS 2.

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

*&---------------------------------------------------------------------*
*&      Form  split_word
*&---------------------------------------------------------------------*
*       Split a string into separate characters by a common delimiter
*----------------------------------------------------------------------*
FORM split_word.

  DATA: lv_mountains TYPE string VALUE 'M,O,U,N,T,A,I,N,S'.
  DATA: lv_word_len  TYPE i.
  DATA: lv_splitindc TYPE c VALUE ','.
  DATA: lv_s1 TYPE c,
        lv_s2 TYPE c,
        lv_s3 TYPE c,
        lv_s4 TYPE c,
        lv_s5 TYPE c,
        lv_s6 TYPE c,
        lv_s7 TYPE c,
        lv_s8 TYPE c,
        lv_s9 TYPE c.

*  lv_word_len = strlen( pa_word ).
*  WRITE:/ lv_word_len.

  WRITE:/ 'Before Parsing:', lv_mountains.

  SPLIT lv_mountains
  AT lv_splitindc
  INTO lv_s1 lv_s2 lv_s3 lv_s4 lv_s5 lv_s6 lv_s7 lv_s8 lv_s9.

  WRITE:/ 'After Parsing:',
          lv_s1,
          lv_s2,
          lv_s3,
          lv_s4,
          lv_s5,
          lv_s6,
          lv_s7,
          lv_s8,
          lv_s9.

ENDFORM.                    "split_word

*&---------------------------------------------------------------------*
*&      Form text_num_count
*&---------------------------------------------------------------------*
*       Printing a string as many times as it has been initialized by
*       a number
*----------------------------------------------------------------------*
FORM text_num_count.

  DATA: p_len_string LIKE p_text.

  p_len_string = strlen( p_text ).

  IF p_len_string = p_len.
    DO p_len TIMES.
      WRITE:/ 'Line', sy-index, p_text(sy-index).
    ENDDO.
  ELSE.
    WRITE:/ 'String Length is out of scope.'.
  ENDIF.


ENDFORM.                    "text_num_count
