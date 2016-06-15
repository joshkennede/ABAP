*&---------------------------------------------------------------------*
*& Report  Z_JOSH_QUIZ6
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_josh_quiz6.

*   Purpose of this excersice
*   By now you know how to use a variable to get the contents stored in that variable
*   But how do you get the value stored in a variable whoese values references another variable?


DATA: car_count TYPE i VALUE 10,
      suv_count TYPE i VALUE 20.
*      CARNESS   TYPE i value 30.

TYPES: BEGIN OF t_auto,
        kind TYPE c LENGTH 10,
       END   OF t_auto.

DATA: itab_auto        TYPE STANDARD TABLE OF t_auto,
      wa_auto          TYPE t_auto.

FIELD-SYMBOLS <fdsym> TYPE i.
*FIELD-SYMBOLS: <fs_auto> TYPE t_auto.


INITIALIZATION.

  APPEND 'car_count' TO itab_auto.
  APPEND 'suv_count' TO itab_auto.


START-OF-SELECTION.

  LOOP AT itab_auto INTO wa_auto.

    ASSIGN (wa_auto-kind) TO <fdsym>.
    WRITE: /5 'TYPE OF COUNT:', wa_auto-kind, 'Total Count:', <fdsym>.

  ENDLOOP.


* - Ravi
*  LOOP AT itab_auto ASSIGNING <fs_auto>. "INTO wa_auto.
*
*    ASSIGN (<fs_auto>-kind) TO <fdsym>.
*    WRITE: /5 'TYPE OF COUNT:', <fs_auto>-kind, 'Total Count:', <fdsym>.
*
*    <fs_auto>-kind = 'CARNESS'.
*
*  ENDLOOP.


* - Ravi
*  WRITE: / 'Second Time'.
*  LOOP AT itab_auto ASSIGNING <fs_auto>. "INTO wa_auto.
*
*    TRY .
*      ASSIGN (<fs_auto>-kind) TO <fdsym>.
*
*    ENDTRY.
*
*    WRITE: /5 'TYPE OF COUNT:', <fs_auto>-kind, 'Total Count:', <fdsym>.
*
*  ENDLOOP.
*
*END-OF-SELECTION.
