*&----------------------------------------------------------------------*
*&  Report  Z_CODEIDEAS
*&
*&----------------------------------------------------------------------*
*&
*&  Written by Josh Kennedy 07.26.16
*&
*&----------------------------------------------------------------------*

REPORT z_codeideas.

*&----------------------------------------------------------------------*
*& Table Declaration
*&----------------------------------------------------------------------*

TABLES: zhr_mini_rtwpbp,
        pa0002.

*&----------------------------------------------------------------------*
*& Data Declaration
*&----------------------------------------------------------------------*

*Global Variables

DATA: gv_fizzbuzz   TYPE c LENGTH 4.
DATA: gv_randnum    TYPE p LENGTH 2 DECIMALS 2.

*Local Variables

*&----------------------------------------------------------------------*
*& Start-Of-Selection Screen Declaration
*&----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK ideas WITH FRAME TITLE text-007.

SELECTION-SCREEN BEGIN OF LINE.                                 "INFO
SELECTION-SCREEN PUSHBUTTON (8) text-012 USER-COMMAND press.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Fizzbuzz
SELECTION-SCREEN COMMENT  01(18) text-001.
PARAMETERS: fzbz RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Random Number
SELECTION-SCREEN COMMENT  01(18) text-002.
PARAMETERS: rndn RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Hello Loop
SELECTION-SCREEN COMMENT  01(18) text-003.
PARAMETERS: helop RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Delay n seconds
SELECTION-SCREEN COMMENT  01(18) text-004.
PARAMETERS: delay RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Progress
SELECTION-SCREEN COMMENT  01(18) text-005.
PARAMETERS: prog RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "System Fields
SELECTION-SCREEN COMMENT  01(18) text-006.
PARAMETERS: syid RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Count Pernr
SELECTION-SCREEN COMMENT  01(18) text-008.
PARAMETERS: cpern RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Calculator
SELECTION-SCREEN COMMENT  01(18) text-009.
PARAMETERS: calcu RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: pa_int1(5)  TYPE n DEFAULT 1,
            pa_op       TYPE c LENGTH 1 DEFAULT '+',
            pa_int2(5)  TYPE n DEFAULT 1.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Split Word
SELECTION-SCREEN COMMENT  01(18) text-010.
PARAMETERS: split RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "String Length
SELECTION-SCREEN COMMENT  01(18) text-011.
PARAMETER: p_txnu RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETER: p_text(25) TYPE c DEFAULT sy-sysid.
PARAMETER: p_len(2)   TYPE n DEFAULT '1'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Age Calculator
SELECTION-SCREEN COMMENT  01(18) text-013.
PARAMETER: p_dat RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_dat1 TYPE datum,
            p_dat2 TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Birthday Calculator
SELECTION-SCREEN COMMENT 01(18) text-014.
PARAMETER: p_dat5 RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_dat3 TYPE datum DEFAULT '19860910',
            p_dat4 TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Lightspeedish Calculator
SELECTION-SCREEN COMMENT 01(18) text-019.
PARAMETER: p_ligh RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETERS: p_lad1 TYPE datum DEFAULT '19860910',
            p_lad2 TYPE datum DEFAULT sy-datum.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(28) text-015.
SELECT-OPTIONS spernr FOR pa0002-pernr DEFAULT '20081754'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Split Word
SELECTION-SCREEN COMMENT  01(18) text-016.
PARAMETERS: p_ip RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Uppercase Test
SELECTION-SCREEN COMMENT  01(18) text-017.
PARAMETERS: p_letr RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
PARAMETER:  p_strg TYPE c LENGTH 26 DEFAULT 'ABCDEFGHIJKLMnOPQRSTUVWXYZ'.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Uppercase Test
SELECTION-SCREEN COMMENT  01(18) text-018.
PARAMETERS: p_iput RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.                                 "Unicode/Non Unicode Test
SELECTION-SCREEN COMMENT  01(18) text-020.
PARAMETER: p_ucod RADIOBUTTON GROUP rgrp.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK ideas.

*&----------------------------------------------------------------------*
*& End-Of-Selection Screen Declaration
*&----------------------------------------------------------------------*

*&----------------------------------------------------------------------*
*& Begin of At Selection-Screen Declaration
*&----------------------------------------------------------------------*

AT SELECTION-SCREEN ON p_len.
*  IF p_len > 25.
*    MESSAGE 'P_LEN should be less or equal 25' TYPE 'E'.
*  ENDIF.
  IF sy-ucomm = 'PRESS'.
    MESSAGE 'These are code ideas' TYPE 'I'.
  ENDIF.

*&----------------------------------------------------------------------*
*& End of At Selection-Screen Declaration
*&----------------------------------------------------------------------*

*&----------------------------------------------------------------------*
*& Start-Of-Selection Event Declaration
*&----------------------------------------------------------------------*
START-OF-SELECTION.

  IF fzbz = 'X'.
    PERFORM fizzbuzz.
    WRITE:/ gv_fizzbuzz.
  ELSEIF rndn = 'X'.
    PERFORM gen_rnd_num.
*    WRITE:/ gv_randnum.
*    CLEAR: gv_randnum.
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
  ELSEIF p_dat = 'X'.
    PERFORM agecalculation.
  ELSEIF p_ligh = 'X'.
    PERFORM calculate_age_by_lightyears.
  ELSEIF p_dat5 = 'X'.
    PERFORM calculate_bday.
  ELSEIF p_ip = 'X'.
    PERFORM get_ip_address.
  ELSEIF p_letr = 'X'.
    PERFORM string_uppercase.
  ELSEIF p_iput = 'X'.
    PERFORM get_user_input.
  ELSEIF p_ucod = 'X'.
    PERFORM unicode_check.
  ENDIF.

END-OF-SELECTION.
*&----------------------------------------------------------------------*
*& End-Of-Selection Event Declaration
*&----------------------------------------------------------------------*

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

  "MOVE lv_buzz TO gv_fizzbuzz. " lv_buzz is '100' at this point so I am moving it to a global var
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

*  MOVE randnum TO gv_randnum. "Moving the local var to the global var and printing to the console.
  WRITE: randnum.
  CLEAR: randnum.

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
*       This program delays num(x) seconds
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

  WRITE:/ sy-uzeit, time,
        / 'Delay in seconds:', num.

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

  WRITE:/10  'Calling Program: ',sy-cprog,
        /10  'Current Date of Application Server: ',sy-datum,
        /10  'Current Screen Number: ',sy-dynnr,
        /10  'Row Length of Internal Table: ',sy-tleng,
        /10  'Index of Current Step Loop Line: ',sy-stepl,
        /10  'Visible Lines of a Step Loop: ',sy-loopc,
        /10  'Found Location in Byte or Character String: ',sy-fdpos,
        /10  'Central Database System: ',sy-dbsys.



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
    WHEN '^'.
      lv_result = SQRT( pa_int1 ).
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
*&       Printing a string as many times as it has been initialized by
*&       a number
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

*&---------------------------------------------------------------------*
*&      Form  ageCalculation
*&---------------------------------------------------------------------*
*&       Calculating days, weeks, months, & years based on date range.
*&       Takes in two agruments, returns four.
*&---------------------------------------------------------------------*
FORM agecalculation.

  DATA: completedays   TYPE i,
        completeweeks  TYPE i,
        completemonths TYPE i,
        completeyears  TYPE i.

  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      begda    = p_dat1
      endda    = p_dat2
    IMPORTING
      days     = completedays
      c_weeks  = completeweeks
      c_months = completemonths
      c_years  = completeyears.

  WRITE:/  'Start Date:',p_dat1,
        /  'End Date:',p_dat2.
        SKIP 1.
  WRITE:/  completedays,'days old.',
        /  completeweeks,'weeks old.',
        /  completemonths,'months old.',
        /  completeyears,'years old.'.
        SKIP 1.
  WRITE:/  'Current Time:',sy-uzeit,
        /  'Todays Date:',sy-datum.


ENDFORM.                    "ageCalculation

*&---------------------------------------------------------------------*
*&      Form  calculate_bday
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM calculate_bday.

  TYPES:  BEGIN OF stats_data,
            pernr  LIKE pa0002-pernr,
            "gbdat  LIKE pa0002-gbdat,
            "begda  LIKE pa0002-begda,
            "endda  LIKE pa0002-endda,
          END OF stats_data.

  DATA: i_it_bday  TYPE STANDARD TABLE OF stats_data.
  DATA: i_wa_bday  TYPE stats_data.

  TYPES:  BEGIN OF stats_output,
            pernr  LIKE pa0002-pernr,
            gbdat  LIKE pa0002-gbdat,
          END OF stats_output.

  DATA: it_bday_final  TYPE STANDARD TABLE OF stats_output.
  DATA: wa_bday_final  TYPE stats_output.

  DATA: completeyears  TYPE i.

  SELECT DISTINCT pernr
    "UP TO 1000 ROWS
    INTO CORRESPONDING FIELDS OF TABLE i_it_bday
    FROM pa0002
    WHERE pernr IN spernr
    AND begda <= sy-datum
    AND endda >= sy-datum.

  "SORT i_it_bday BY pernr.

  LOOP AT i_it_bday INTO i_wa_bday.

    CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
      EXPORTING
        begda   = p_dat3
        endda   = p_dat4
      IMPORTING
        c_years = completeyears.

    APPEND wa_bday_final TO it_bday_final.

  ENDLOOP.

  LOOP AT it_bday_final INTO wa_bday_final.
    WRITE:/ wa_bday_final-pernr,
            completeyears,
            wa_bday_final-gbdat.
  ENDLOOP.

ENDFORM.                    "calculate_bday

*&---------------------------------------------------------------------*
*&      Form  get_ip_address
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_ip_address.

  DATA: term LIKE usr41-terminal.


  CALL FUNCTION 'TERMINAL_ID_GET'
    IMPORTING
      terminal = term.

  WRITE :/ term.


  DATA: client     TYPE REF TO if_http_client,
        ip_address TYPE string.

****Create the Call

  CALL METHOD cl_http_client=>create_by_url
    EXPORTING
      url    = 'http://www.google.com'
      ssl_id = ' '
    IMPORTING
      client = client
    EXCEPTIONS
      OTHERS = 1.

****Make the call

  client->send( ).

****Receive the Response Object

  CALL METHOD client->receive
    EXCEPTIONS
      http_communication_failure = 1
      http_invalid_state         = 2
      http_processing_failed     = 3
      OTHERS                     = 4.

  ip_address = client->response->get_header_field( name = '~remote_addr' ).

  WRITE:/ ip_address.

ENDFORM.                    "get_ip_address

*&---------------------------------------------------------------------*
*&      Form  string_uppercase
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM string_uppercase.

  DATA: lv_upperstring_temp TYPE string,
        lv_lowerstring_temp TYPE string.

  lv_upperstring_temp = p_strg.
  lv_lowerstring_temp = p_strg.

  CASE p_strg.
    WHEN lv_upperstring_temp.
      "IF lv_upperstring_temp CA sy-abcde.
      TRANSLATE lv_upperstring_temp TO UPPER CASE.
      WRITE: / lv_upperstring_temp,' has at least one capital letter'.
    WHEN lv_lowerstring_temp.
      "ELSEIF lv_lowerstring_temp CA sy-abcde.
      TRANSLATE lv_lowerstring_temp TO LOWER CASE.
      WRITE: / lv_lowerstring_temp,' has at least one small letters'.
    WHEN OTHERS.
      WRITE: / 'I dont know this string!'.
  ENDCASE.


ENDFORM.                    "string_uppercase
*&---------------------------------------------------------------------*
*&      Form  get_user_input
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM get_user_input.

  DATA: t_fields LIKE sval OCCURS 0 WITH HEADER LINE.

*Prepare parameters for FM
  t_fields-tabname = 'BKPF'.
  t_fields-fieldname = 'BUDAT'.
  APPEND t_fields.

  t_fields-tabname = 'BKPF'.
  t_fields-fieldname = 'BLART'.
  APPEND t_fields.

  CALL FUNCTION 'POPUP_GET_VALUES'
    EXPORTING
*     NO_VALUE_CHECK        = ' '
      popup_title           = 'Test Popup'
*     START_COLUMN          = '5'
*     START_ROW             = '5'
*   IMPORTING
*     RETURNCODE            =
    TABLES
      fields                = t_fields
   EXCEPTIONS
     error_in_fields       = 1
     OTHERS                = 2
            .
  IF sy-subrc <> 0.
* Implement suitable error handling here
  ENDIF.

*Display Report
  LOOP AT t_fields.
    WRITE:/ t_fields-value.
  ENDLOOP.

ENDFORM.                    "get_user_input

FORM calculate_age_by_lightyears.

  DATA: lightspeed     TYPE i VALUE 186000,
        lightday       TYPE p,
        completedays   TYPE i,
        completeweeks  TYPE i,
        completemonths TYPE i,
        completeyears  TYPE i.

  lightday = ( 24 * 3600 ) * lightspeed.
  WRITE: lightday, '= Distance traveled in 1 Light Day.'.

  CALL FUNCTION 'HR_99S_INTERVAL_BETWEEN_DATES'
    EXPORTING
      begda    = p_lad1
      endda    = p_lad2
    IMPORTING
      days     = completedays
      c_weeks  = completeweeks
      c_months = completemonths
      c_years  = completeyears.

  WRITE:/  completedays,'days old.',
        /  completeweeks,'weeks old.',
        /  completemonths,'months old.',
        /  completeyears,'years old.',
        /  'Time:',sy-uzeit,
        /  'Date:',sy-datum.

ENDFORM.

FORM unicode_check.

  IF cl_abap_char_utilities=>charsize = 1.
    WRITE: 'This system is non-Unicode'(000).
  ELSE.
    WRITE: 'This system is Unicode'(001).
  ENDIF.

ENDFORM.
