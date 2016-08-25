*&---------------------------------------------------------------------*
*& Report  Z_TESTING
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

*REPORT demo_at_line_selection.

*START-OF-SELECTION.
*  WRITE 'Click me!' COLOR = 5 HOTSPOT.
*
*AT LINE-SELECTION.
*  WRITE: / 'You clicked list', sy-listi,
*         / 'You are on list', sy-lsind.
*  IF sy-lsind < 20.
*    SKIP.
*    WRITE: 'More ...' COLOR = 5 HOTSPOT.
*  ENDIF.

*DATA sum TYPE i.
*
*START-OF-SELECTION.
*  WRITE 'Click Me!' COLOR = 15 HOTSPOT.
*
*AT LINE-SELECTION.
*  WRITE:/ 'Hello!' COLOR = 10 HOTSPOT.
*
*  FORMAT COLOR COL_NORMAL.
*  DO 10 TIMES.
*    WRITE / sy-index.
*    sum = sum + sy-index.
*    WRITE sum COLOR COL_TOTAL.
*  ENDDO.
*  ULINE.
*  WRITE sum UNDER sum COLOR COL_GROUP.



*DATA: BEGIN OF it_str  OCCURS 0,
*              str(50),
*      END OF it_str.
*
*DATA: string1 TYPE string.

*********************************

*DATA: input(30) VALUE ' Network Technician'.
*
*DATA: current_char(1)  TYPE c,
*      total_len        TYPE i,
*      current_position TYPE i,
*      last_position    TYPE i.
*
*DATA: string1(15),
*      string2(15).

*********************************

*Selection Screen Dynamics

REPORT z_abap101_082.
*PARAMETER p_first RADIOBUTTON GROUP grp1 USER-COMMAND action.
*PARAMETER p_busin RADIOBUTTON GROUP grp1.
*PARAMETER p_econo LIKE p_first RADIOBUTTON GROUP grp1.
*PARAMETER p_input1 TYPE string.
*PARAMETER p_input2 TYPE i.
*DATA v_last_action LIKE sy-ucomm.
*
*AT SELECTION-SCREEN.
*  v_last_action = sy-ucomm.
*
*AT SELECTION-SCREEN OUTPUT.
*  IF v_last_action = 'ACTION'.
*    CASE 'X'.
*      WHEN p_first.
*        LOOP AT SCREEN.
*          IF screen-name = 'P_INPUT1' OR screen-name = 'P_INPUT2'.
*            screen-input = 1.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      WHEN p_busin.
*        LOOP AT SCREEN.
*          IF screen-name = 'P_INPUT1'.
*            screen-required = 1.
*            MODIFY SCREEN.
*          ENDIF.
*          IF screen-name = 'P_INPUT2'.
*            screen-input = 0.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*      WHEN p_econo.
*        LOOP AT SCREEN.
*          IF screen-name = 'P_INPUT1' OR screen-name = 'P_INPUT2'.
*            screen-input = 0.
*            screen-invisible = 1.
*            MODIFY SCREEN.
*          ENDIF.
*        ENDLOOP.
*    ENDCASE.
*  ENDIF.


*******************************************************
*DATA: i       TYPE i VALUE 5.
*DATA: result  TYPE abap_bool.
*
*result = boolc( i > 3 ).
*
*WRITE:/ result.
*******************************************************

*START-OF-SELECTION.

*  string1 = 'This is the test of the split'.
*  SPLIT string1 AT space INTO TABLE it_str.
*
*  LOOP AT it_str.
*    WRITE:/ it_str-str.
*  ENDLOOP.
*
**********************************
*
*  total_len = strlen( input ).
*  current_position = 1.
*
*  WHILE current_position LE total_len.
*    IF current_position EQ total_len.
*      string2 = input+last_position(current_position).
*      EXIT.
*    ENDIF.
*
*    current_char = input+current_position(1).
*
*    IF current_char EQ space.
*      CHECK string1 IS INITIAL.
*      string1 = input(current_position).
*      last_position = current_position + 1.
*    ENDIF.
*
*    ADD 1 TO current_position.
*  ENDWHILE.
*
*  WRITE: / string1,
*         / string2.

*END-OF-SELECTION.


SELECTION-SCREEN PUSHBUTTON 10(8) text-001 USER-COMMAND press. " text-001 = 'Press me'

AT SELECTION-SCREEN.
  IF sy-ucomm = 'PRESS'.
    MESSAGE 'Button was pressed' TYPE 'I'.
  ENDIF.
