*----------------------------------------------------------------------*
* Program                : Z_PYCOUNT                                   *
* Created on             : 04.27.16                                    *
* Created by             : JOSHUA KENNEDY                              *
* Developer              : JOSHUA KENNEDY                              *
* Technical  Designer    : JOSHUA KENNEDY                              *
* Functional Designer    : JOSHUA KENNEDY                              *
* Change/Transport Number: Local Object                                *
* Report Description     : Payroll Record Count                        *
*----------------------------------------------------------------------*
*        MODIFICATION LOG                                              *
*----------------------------------------------------------------------*
* CTS #       Date        Modified by                                  *
*                         Description                                  *
* ==========  ==========  =======================                      *
* 1.0         04.27.16    Josh K - Initial Development                 *
* 1.1         05.10.16    Josh K - Lots of fixes/tweaks                *
* 1.2         05.11.16    Josh K - Fix to remove unecessary logic      *
*                                  to third logic                      *
* 1.3         05.12.16    Josh K - Fix bug with Bus Areas              *
* 1.4         05.19.16    Josh K - Logic 6 & 7                         *
* 1.5         05.23.16    Josh K - Logic fix for #7                    *
* 1.6         06.06.16    Josh K - Logic 8                             *
*                                                                      *
*                                                                      *
*----------------------------------------------------------------------*

REPORT z_pycount
        LINE-SIZE 255
        LINE-COUNT 60(0)
        NO STANDARD PAGE HEADING
        MESSAGE-ID zhr_reports.

*----------------------------------------------------------------------*
* Data Definitions                                                     *
*----------------------------------------------------------------------*
* Tables - Database Tables                                             *
*----------------------------------------------------------------------*

TABLES: zhr_mini_rtwpbp,
        pa0001,
        pa0002,
        tgsbt.

************************************************************************
* GENERIC DATA                                                         *
************************************************************************

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

************************************************************************
* Data - For 4th Logic                                                 *
************************************************************************

TYPES:  BEGIN OF type_old,
          pernr LIKE zhr_mini_rtwpbp-pernr,
          gsber LIKE zhr_mini_rtwpbp-gsber,
        END OF type_old.

DATA:   it_tab_old TYPE STANDARD TABLE OF type_old.
DATA:   wa_tab_old   TYPE type_old.

TYPES:  BEGIN OF type_new,
          pernr   LIKE zhr_mini_rtwpbp-pernr,
          gsber   LIKE zhr_mini_rtwpbp-gsber,
          count   TYPE i,
        END OF type_new.

DATA:   itab_new TYPE STANDARD TABLE OF type_new.
DATA:   wa_new   TYPE type_new.

************************************************************************
* Data - For 6th Logic                                                 *
************************************************************************

TYPES: BEGIN OF type_six,
       pernr    LIKE zhr_mini_rtwpbp-pernr,
       ename    LIKE pa0001-ename,
       gsber    LIKE zhr_mini_rtwpbp-gsber,
       gtext    LIKE tgsbt-gtext,
       END OF type_six.

DATA: itab_six TYPE STANDARD TABLE OF type_six.
DATA: wa_six   TYPE type_six.

TYPES: BEGIN OF sixth_type,
       pernr   LIKE zhr_mini_rtwpbp-pernr,
       ename   LIKE pa0001-ename,
       gsber   LIKE zhr_mini_rtwpbp-gsber,
       gtext   LIKE tgsbt-gtext,
       count   TYPE i,
       END OF sixth_type.

DATA: itab_sixth TYPE STANDARD TABLE OF sixth_type.
DATA: wa_sixth   TYPE sixth_type.

************************************************************************
* Data - For 7th Logic                                                 *
************************************************************************

TYPES: BEGIN OF wpbp_type,
       pernr LIKE zhr_mini_rtwpbp-pernr,
       gsber LIKE zhr_mini_rtwpbp-gsber,
       END OF wpbp_type.

DATA: itab_wpbp TYPE STANDARD TABLE OF wpbp_type.
DATA: wa_wpbp   TYPE wpbp_type.

************************************************************************

TYPES: BEGIN OF pa0001_type,
       pernr   LIKE pa0001-pernr,
       ename   LIKE pa0001-ename,
       END OF pa0001_type.

DATA: itab_pa1 TYPE STANDARD TABLE OF pa0001_type.
DATA: wa_pa1   TYPE pa0001_type.

************************************************************************

TYPES: BEGIN OF tgsbt_type,
       gsber   LIKE tgsbt-gsber,
       gtext   LIKE tgsbt-gtext,
       END OF tgsbt_type.

DATA: itab_tgsbt TYPE STANDARD TABLE OF tgsbt_type.
DATA: wa_tgsbt   TYPE tgsbt_type.

************************************************************************

TYPES: BEGIN OF main_type,
       pernr   LIKE zhr_mini_rtwpbp-pernr,
       ename   LIKE pa0001-ename,
       gsber   LIKE zhr_mini_rtwpbp-gsber,
       gtext   LIKE tgsbt-gtext,
       count   TYPE i,
       END OF main_type.

DATA: itab_final TYPE STANDARD TABLE OF main_type.
DATA: wa_final   TYPE main_type.

************************************************************************
* Data - For 8th Logic                                                 *
************************************************************************

TYPES:  BEGIN OF stats_data,
          pernr LIKE zhr_mini_rtwpbp-pernr,
          gesch LIKE pa0002-gesch,
          gbdat LIKE pa0002-gbdat,
        END OF stats_data.

DATA:   itab_stats TYPE STANDARD TABLE OF stats_data.
DATA:   wa_stats   TYPE stats_data.


TYPES:  BEGIN OF stats_output,
          gndr   TYPE c LENGTH 10,
          cat1   TYPE sy-dbcnt,
          cat2   TYPE sy-dbcnt,
          cat3   TYPE sy-dbcnt,
          cat4   TYPE sy-dbcnt,
          cat5   TYPE sy-dbcnt,
        END OF stats_output.

DATA:   itab_stats_final  TYPE STANDARD TABLE OF stats_output.
DATA:   wa_stats_final    TYPE stats_output.

************************************************************************
************************************************************************

DATA: curr_pernr      LIKE zhr_mini_rtwpbp-pernr,
      prev_pernr      LIKE zhr_mini_rtwpbp-pernr.
DATA: rec_tot         TYPE i VALUE 1.
DATA: time_start      LIKE sy-uzeit,
      time_end        LIKE sy-uzeit,
      time_diff       LIKE sy-uzeit,
      tot_rows        LIKE sy-dbcnt.
DATA: batxt(45)       TYPE c.
DATA: tab1            LIKE sy-dbcnt,
      tab2            LIKE sy-dbcnt,
      tab3            LIKE sy-dbcnt,
      dbcnt_tot       LIKE sy-dbcnt.
DATA: gender          LIKE pa0002-gesch.

**************************************************************************
*START ALV DATA                                                          *
**************************************************************************

TYPE-POOLS: slis.   "ALV

*Constants

CONSTANTS: gc_formname_top_of_page TYPE slis_formname VALUE 'TOP_OF_PAGE'.

DATA: gt_fieldcatalog     TYPE slis_t_fieldcat_alv WITH HEADER LINE,
      gd_layout           TYPE slis_layout_alv,
      gd_repid            LIKE sy-repid,
      g_save              TYPE c              VALUE 'X',
      g_variant           TYPE disvariant,
      gx_variant          TYPE disvariant,
      g_exit              TYPE c,
      gs_variant          TYPE disvariant,
      gs_heading          TYPE slis_t_listheader,
      gs_layout           TYPE slis_layout_alv,
      gs_print            TYPE slis_print_alv,
      gt_sort             TYPE slis_t_sortinfo_alv,
      gt_sp_group         TYPE slis_t_sp_group_alv,
      gt_events           TYPE slis_t_event,
      g_repid             LIKE sy-repid       VALUE sy-repid,
      gt_list_top_of_page TYPE slis_t_listheader,
      g_boxnam            TYPE slis_fieldname VALUE  'box',
      p_f2code            LIKE sy-ucomm       VALUE  '&eta',
      p_lignam            TYPE slis_fieldname VALUE  'lights',
      g_default(1)        TYPE c.

*ALV Grid Variants

DATA: alv_layout          TYPE slis_layout_alv,
      alv_variant         TYPE disvariant,
      alv_grid_title      TYPE lvc_title,
      alv_report_title    TYPE lvc_title.

DATA: reported_records_lines       TYPE i,
      reported_records_display(20) TYPE c.

**************************************************************************
*END ALV DATA                                                            *
**************************************************************************

*----------------------------------------------------------------------*
*  SELECTION SCREEN - User Selection screen.
*----------------------------------------------------------------------*

SELECTION-SCREEN BEGIN OF BLOCK jkpycount WITH FRAME TITLE text-001.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(16) text-002.
SELECT-OPTIONS sel_pern FOR zhr_mini_rtwpbp-pernr.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-003.
PARAMETER: rec_amt(3) TYPE n DEFAULT '200'.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK jkpycount.

SELECTION-SCREEN BEGIN OF BLOCK jklogicoptions WITH FRAME TITLE text-011.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-004.
PARAMETER: l1 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-005.
PARAMETER: l2 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-006.
PARAMETER: l3 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-007.
PARAMETER: l4 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-008.
PARAMETER: l5 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-009.
PARAMETER: l6 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-010.
PARAMETER: l7 RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT 01(19) text-012.
PARAMETER: stats RADIOBUTTON GROUP logc.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK jklogicoptions.


*----------------------------------------------------------------------*
*        INITIALIZATION                                                *
*----------------------------------------------------------------------*

INITIALIZATION.

**----------------------------------------------------------------------*
**  AT SELECTION-SCREEN ON VALUE-REQUEST                                *
**----------------------------------------------------------------------*
**AT SELECTION-SCREEN ON VALUE-REQUEST FOR
**<enter selection screen item here>.
*
*
**----------------------------------------------------------------------*
**  AT SELECTION-SCREEN OUTPUT                                          *
**----------------------------------------------------------------------*
*AT SELECTION-SCREEN OUTPUT.
*
*
*
**----------------------------------------------------------------------*
**  AT SELECTION-SCREEN.                                                *
**----------------------------------------------------------------------*
*AT SELECTION-SCREEN.
*
*
*
**----------------------------------------------------------------------*
**  TOP-OF-PAGE.                                                        *
**----------------------------------------------------------------------*
*TOP-OF-PAGE.
*  CALL FUNCTION 'Z_WRITE_HEADER_FOOTER'                                 "<-- This shows the Standard Header for Report Output
*    EXPORTING
*      type_head_foot   = 'H'
*      not_confidential = 'X'
*      title1           = 'Z_PYCOUNT'
*      progtype         = 'R'.


*---------------------------------------------------------------------*
*        START-OF-SELECTION                                           *
*---------------------------------------------------------------------*

START-OF-SELECTION.

* At program execution, capture start time.
  GET TIME.
  MOVE sy-uzeit TO time_start.

* Get all personnel numbers from table and store in internal table, ordered by pernr (ascending).
  SELECT pernr
    FROM zhr_mini_rtwpbp "UP TO 100 ROWS
    INTO TABLE itab_table
    WHERE pernr IN sel_pern
    ORDER BY pernr.

* First WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l1 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 17 'Total Records'.

    ULINE.

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
  ENDIF.

* Second WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l2 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 17 'Total Records'.

    ULINE.

    LOOP AT itab_table INTO wa_table.

      AT NEW pernr.
        rec_tot = 1.
      ENDAT.
      AT END OF pernr.
        wa_main-pernr = wa_table-pernr.
        wa_main-count = rec_tot.
        APPEND wa_main TO it_main.
      ENDAT.
      rec_tot = rec_tot + 1.

    ENDLOOP.
  ENDIF.

* Third WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l3 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 17 'Total Records'.

    ULINE.

    LOOP AT itab_table INTO wa_table.

      wa_main-pernr = wa_table-pernr.
      wa_main-count = rec_tot.
      COLLECT wa_main INTO it_main.

    ENDLOOP.
  ENDIF.

**********************************************************************************
**********************************************************************************

* Fourth WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l4 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 10 'Bus Area', 23 'Total Records'.

    ULINE.

* Get personnel numbers and business area from table, place in work area, and sort pernrs ascending.
    SELECT pernr gsber
      FROM zhr_mini_rtwpbp
      INTO TABLE it_tab_old
      WHERE pernr IN sel_pern
      ORDER BY pernr gsber.

    LOOP AT it_tab_old INTO wa_tab_old.

      AT NEW pernr.
        rec_tot = 0.
      ENDAT.
      rec_tot = rec_tot + 1.
      AT END OF gsber.
        wa_new-pernr = wa_tab_old-pernr.
        wa_new-gsber = wa_tab_old-gsber.
        wa_new-count = rec_tot.
        APPEND wa_new TO itab_new.
        rec_tot = 0.
      ENDAT.

    ENDLOOP.

    SORT itab_new BY pernr.

    LOOP AT itab_new INTO wa_new.
      IF sy-tabix = rec_amt.
        EXIT.
      ENDIF.
      WRITE: / wa_new-pernr, wa_new-gsber, wa_new-count.

      CLEAR: wa_new.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 23 sy-datum.
    WRITE:/ 'Environment:', 23 sy-sysid.
    WRITE:/ 'User:', 23 sy-uname.
    WRITE:/ 'Current Time:', 23 sy-uzeit.

    ULINE.

* Read internal table and write total unique employees and total rows processed.
    DESCRIBE TABLE itab_new LINES tot_rows.
    WRITE: /1 'Total Employees Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.
  ENDIF.

**********************************************************************************
**********************************************************************************

* Fifth WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l5 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 10 'Bus Area', 23 'Total Records'.

    ULINE.

    SELECT pernr gsber
    FROM zhr_mini_rtwpbp
    INTO CORRESPONDING FIELDS OF wa_new
    WHERE pernr IN sel_pern
    ORDER BY pernr.

      wa_new-count = 1.
      COLLECT wa_new INTO itab_new.

    ENDSELECT.

    SORT itab_new BY pernr.

    LOOP AT itab_new INTO wa_new.
      IF sy-tabix = rec_amt.
        EXIT.
      ENDIF.
      WRITE: / wa_new-pernr, wa_new-gsber, wa_new-count.

      CLEAR: wa_new.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 23 sy-datum.
    WRITE:/ 'Environment:', 23 sy-sysid.
    WRITE:/ 'User:', 23 sy-uname.
    WRITE:/ 'Current Time:', 23 sy-uzeit.

    ULINE.

* Read internal table and write total unique employees and total rows processed.
    DESCRIBE TABLE itab_new LINES tot_rows.
    WRITE: /1 'Total Employees Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.
  ENDIF.

**********************************************************************************
**********************************************************************************

* Sixth WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l6 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 10 'Employee Name', 39 'BA', 46 'BA Name', 93 'Total Records'.

    ULINE.

    SELECT t1~pernr
           t1~gsber
           t2~ename
           t3~gtext
    INTO CORRESPONDING FIELDS OF TABLE itab_six
    FROM  zhr_mini_rtwpbp AS t1
    JOIN  pa0001 AS t2
    ON    t1~pernr = t2~pernr
    JOIN  tgsbt AS t3
    ON    t1~gsber = t3~gsber
    "UP TO 100 ROWS
    WHERE t2~begda <= sy-datum AND
          t2~endda >= sy-datum AND
          t1~pernr IN sel_pern
    ORDER BY t1~pernr t1~gsber t2~ename.

    LOOP AT itab_six INTO wa_six.

      AT NEW pernr.
        rec_tot = 0.
      ENDAT.
      rec_tot = rec_tot + 1.
      wa_sixth-ename = wa_six-ename.
      wa_sixth-gtext = wa_six-gtext.
      AT END OF gsber.
        wa_sixth-pernr = wa_six-pernr.
        wa_sixth-gsber = wa_six-gsber.
        wa_sixth-count = rec_tot.
        APPEND wa_sixth TO itab_sixth.
        rec_tot = 0.
      ENDAT.

    ENDLOOP.

    SORT itab_sixth BY pernr.

    LOOP AT itab_sixth INTO wa_sixth.

      CONCATENATE wa_sixth-gsber wa_sixth-gtext INTO batxt SEPARATED BY ' - '.

      IF sy-tabix = rec_amt.
        EXIT.
      ENDIF.

      WRITE:/1  wa_sixth-pernr,
             10 wa_sixth-ename,
             39 batxt,
             85 wa_sixth-count.

      CLEAR: batxt.
      CLEAR: wa_sixth.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 23 sy-datum.
    WRITE:/ 'Environment:', 23 sy-sysid.
    WRITE:/ 'User:', 23 sy-uname.
    WRITE:/ 'Current Time:', 23 sy-uzeit.

    ULINE.

* Read internal table and write total unique employees and total rows processed.
    DESCRIBE TABLE itab_sixth LINES tot_rows.
    WRITE: /1 'Total Employees Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.
  ENDIF.

**********************************************************************************
**********************************************************************************

* Seventh WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF l7 = 'X'.

* Write Temporary Header.
    WRITE: /1 'Pernr', 10 'Employee Name', 39 'BA', 46 'BA Name', 93 'Total Records'.

    ULINE.

*Select statement for payroll records
    SELECT pernr gsber
        "UP TO 100000 ROWS
        FROM zhr_mini_rtwpbp
        INTO TABLE itab_wpbp
        WHERE pernr IN sel_pern
        ORDER BY pernr.

    SORT itab_wpbp BY pernr ASCENDING
                      gsber ASCENDING.

    tab1 = sy-dbcnt.  "Keep count of database record count for output

*Select statement for fully formatted employee name
    SELECT pernr ename
      FROM pa0001
      INTO TABLE itab_pa1
      WHERE begda <= sy-datum
      AND   endda >= sy-datum
      ORDER BY pernr.

    SORT itab_pa1 BY pernr ASCENDING.

    "tab2 = sy-dbcnt. "Keep count of database record count for output

*Select statement for business area text
    SELECT gsber gtext
      FROM tgsbt
      INTO TABLE itab_tgsbt.

    SORT itab_tgsbt BY gsber ASCENDING.

    "tab3 = sy-dbcnt. "Keep count of database record count for output
    "dbcnt_tot = tab1 + tab2 + tab3.
    dbcnt_tot = tab1.
    sy-dbcnt = dbcnt_tot.

*Logic for comparing and connecting records for employee name and business area text.
    LOOP AT itab_wpbp INTO wa_wpbp.

      READ TABLE itab_pa1 INTO wa_pa1 WITH KEY pernr = wa_wpbp-pernr BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-ename = wa_pa1-ename.
      ELSE.
        wa_final-ename = ''.
      ENDIF.

      READ TABLE itab_tgsbt INTO wa_tgsbt WITH KEY gsber = wa_wpbp-gsber BINARY SEARCH.
      IF sy-subrc = 0.
        wa_final-gtext = wa_tgsbt-gtext.
      ELSE.
        wa_final-gtext = ''.
      ENDIF.

      wa_final-pernr = wa_wpbp-pernr.
      wa_final-gsber = wa_wpbp-gsber.
      wa_final-count = 1.
      COLLECT wa_final INTO itab_final.

    ENDLOOP.

    SORT itab_final BY count DESCENDING.

    LOOP AT itab_final INTO wa_final.

      CONCATENATE wa_final-gsber wa_final-gtext INTO batxt SEPARATED BY ' - '.

      IF sy-tabix = rec_amt.
        EXIT.
      ENDIF.
      WRITE: /1  wa_final-pernr,
              10 wa_final-ename,
              39 batxt,
              86 wa_final-count.

      CLEAR: batxt.
      CLEAR: wa_final.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 23 sy-datum.
    WRITE:/ 'Environment:', 23 sy-sysid.
    WRITE:/ 'User:', 23 sy-uname.
    WRITE:/ 'Current Time:', 23 sy-uzeit.

    ULINE.

* Read internal table and write total unique employees and total rows processed.
    DESCRIBE TABLE itab_final LINES tot_rows.
    WRITE: /1 'Total Employees Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.
  ENDIF.

* Eighth WAY! - Loop through all sorted pernrs to determine unique row amount and store in work area for processing.
  IF stats = 'X'.

    SELECT DISTINCT a~pernr gbdat gesch
      "UP TO 100000 ROWS
      INTO CORRESPONDING FIELDS OF TABLE itab_stats
      FROM pa0002
      AS a
      JOIN zhr_mini_rtwpbp
      AS b
      ON a~pernr = b~pernr
      WHERE b~pernr IN sel_pern
      AND a~begda <= sy-datum
      AND a~endda >= sy-datum.

    SORT itab_stats BY pernr.

    LOOP AT itab_stats INTO wa_stats.

      CASE wa_stats-gesch.
        WHEN '1'.
          wa_stats_final-gndr = 'Male'.
        WHEN '2'.
          wa_stats_final-gndr = 'Female'.
        WHEN ''.
          wa_stats_final-gndr = 'Unknown'.
      ENDCASE.

* I am having a hard time figuring out a better way to do the below logic.
* I think I could do it with a function module, but am not experienced enough with FM's.

      IF sy-datum - wa_stats-gbdat < 9124.          "Category <25yrs
        wa_stats_final-cat1 = 1.
      ELSEIF sy-datum - wa_stats-gbdat < 12774.     "Category 25 to <35yrs
        wa_stats_final-cat2 = 1.
      ELSEIF sy-datum - wa_stats-gbdat < 18249.     "Category 35 to <50yrs
        wa_stats_final-cat3 = 1.
      ELSEIF sy-datum - wa_stats-gbdat < 36500.     "Category >50yrs    <-----Up to 100 Yrs
        wa_stats_final-cat4 = 1.
      ELSE.
        wa_stats_final-cat5 = 1.                    "Category No Age Data
      ENDIF.

      COLLECT wa_stats_final INTO itab_stats_final.

      wa_stats_final-gndr = 'Total'.                "This provides 'total' line for both genders.
      COLLECT wa_stats_final INTO itab_stats_final.

      CLEAR: wa_stats_final,
             wa_stats.

    ENDLOOP.

    SORT itab_stats_final BY gndr.

* Write Temporary Header.
    WRITE: /1 'Gender', 13 '<25', 27 '25 to <35', 41 '35 to <50', 55 '>50', 69 'No Age Data'.

    ULINE.

    LOOP AT itab_stats_final INTO wa_stats_final.

      IF wa_stats_final-gndr = 'Total'.
        ULINE.
      ENDIF.

      WRITE: /1 wa_stats_final-gndr,
              sy-vline,
              wa_stats_final-cat1,
              sy-vline,
              wa_stats_final-cat2,
              sy-vline,
              wa_stats_final-cat3,
              sy-vline,
              wa_stats_final-cat4,
              sy-vline,
              wa_stats_final-cat5,
              sy-vline.

      CLEAR: wa_stats_final.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 23 sy-datum.
    WRITE:/ 'Environment:', 23 sy-sysid.
    WRITE:/ 'User:', 23 sy-uname.
    WRITE:/ 'Current Time:', 23 sy-uzeit.

    ULINE.

* Read internal table and write total unique employees and total rows processed.
    DESCRIBE TABLE itab_stats_final LINES tot_rows.
    WRITE: /1 'Total Categories Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.

  ENDIF.

* Read internal table and write total unique employees and total rows processed.
  IF   l1 = 'X'
    OR l2 = 'X'
    OR l3 = 'X'.

* Sort internal table by record sum.
    SORT it_main BY count DESCENDING.

* Loop through records [User input or Pgm Default which is '200'] based on highest record count and write to screen.
    LOOP AT it_main INTO wa_main.
      IF sy-tabix = rec_amt.
        EXIT.
      ENDIF.
      WRITE: / wa_main-pernr, wa_main-count.

      CLEAR: wa_main.

    ENDLOOP.

    ULINE.

    WRITE:/ 'Date:', 17 sy-datum.
    WRITE:/ 'Environment:', 17 sy-sysid.
    WRITE:/ 'User:', 17 sy-uname.
    WRITE:/ 'Current Time:', 17 sy-uzeit.

    ULINE.

    DESCRIBE TABLE it_main LINES tot_rows.
    WRITE: /1 'Total Employees Processed:', 30 tot_rows.
    WRITE: /1 'Total Records Processed:', 30 sy-dbcnt.
  ENDIF.

* After processing all data, capture end time and calculate the total runtime of program.
  GET TIME.
  MOVE sy-uzeit TO time_end.
  IF time_end > time_start.
    time_diff = time_end - time_start.
  ENDIF.
  WRITE: /1 'Total Program Runtime:', 32 time_diff.

END-OF-SELECTION.



* After End Of Program Logic Goes in here
* Examples of End of Logic would be
* 1. Display Report
* 2. Clear and Release Internal Tables used in Program


*---------------------------------------------------------------------*
* Sub Routines Section - All FORM Definitions Goes here               *
*---------------------------------------------------------------------*

** Build ALV requirements and pass processed data to ALV.
  PERFORM build_fieldcatalog.
  PERFORM display_alv_report.
*
*&---------------------------------------------------------------------*
*&      Form  build_fieldcatalog
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM build_fieldcatalog.

  gt_fieldcatalog-fieldname = 'PERNR'.
  gt_fieldcatalog-seltext_m = 'Personnel Number'.
  gt_fieldcatalog-col_pos   = 0.
  gt_fieldcatalog-outputlen = '18'.
  gt_fieldcatalog-lzero     = 'X'.
  APPEND gt_fieldcatalog TO gt_fieldcatalog.
  CLEAR gt_fieldcatalog.

  gt_fieldcatalog-fieldname = 'ENAME'.
  gt_fieldcatalog-seltext_m = 'Employee Name'.
  gt_fieldcatalog-col_pos   = 1.
  gt_fieldcatalog-outputlen = '18'.

  APPEND gt_fieldcatalog TO gt_fieldcatalog.
  CLEAR gt_fieldcatalog.

  gt_fieldcatalog-fieldname = 'GSBER'.
  gt_fieldcatalog-seltext_m = 'Business Area'.
  gt_fieldcatalog-col_pos   = 2.
  gt_fieldcatalog-outputlen = '18'.

  APPEND gt_fieldcatalog TO gt_fieldcatalog.
  CLEAR gt_fieldcatalog.

  gt_fieldcatalog-fieldname = 'GTEXT'.
  gt_fieldcatalog-seltext_m = 'Business Area Text'.
  gt_fieldcatalog-col_pos   = 3.
  gt_fieldcatalog-outputlen = '18'.

  APPEND gt_fieldcatalog TO gt_fieldcatalog.
  CLEAR gt_fieldcatalog.

  gt_fieldcatalog-fieldname = 'COUNT'.
  gt_fieldcatalog-seltext_m = 'Number of Records'.
  gt_fieldcatalog-col_pos   = 4.
  gt_fieldcatalog-outputlen = '18'.

  APPEND gt_fieldcatalog TO gt_fieldcatalog.
  CLEAR gt_fieldcatalog.


ENDFORM.                    "build_fieldcatalog

*&---------------------------------------------------------------------*
*&      Form  display_alv_report
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
FORM display_alv_report.
  gd_repid = sy-repid.
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program      = gd_repid
      i_callback_top_of_page  = 'TOP-OF-PAGE' "see FORM
      i_callback_user_command = 'USER_COMMAND'
      it_fieldcat             = gt_fieldcatalog[]
      i_save                  = 'X'
      is_variant              = g_variant
    TABLES
      t_outtab                = itab_sixth                                  "<-------- Change this to applicable internal table
    EXCEPTIONS
      program_error           = 1
      OTHERS                  = 2.
ENDFORM.                    "display_alv_report

*---------------------------------------------------------------------*
*       Form  top_of_page
*---------------------------------------------------------------------*
*       This subroutine writes the list header to the top_of_page.
*---------------------------------------------------------------------*
FORM top_of_page.

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = gs_heading.

ENDFORM.                    "top_of_page

*---------------------------------------------------------------------*
* Free Tables                                                         *
*---------------------------------------------------------------------*

* FREE: itab_table,       " For logic 1-3
*        it_main,          " For logic 1-3
*        it_tab_old,       " For logic 4
*        itab_new,         " For logic 5
*        itab_sixth,       " For logic 6
*        itab_final,       " For logic 7
*        itab_stats_final. " For logic 8
