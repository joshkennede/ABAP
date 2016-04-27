REPORT Z_MOD_1.

INCLUDE Z_EMPLOYEES_DEFINITIONS.

TABLES: ZEMPLOYEES.

**Declare a Line Type
*TYPES: BEGIN OF line01_typ,
*        surname LIKE zemployees-surname,
*        dob     LIKE zemployees-dob,
*       END OF line01_typ.

*Declare the 'Table Type' based on the 'Line Type'
TYPES ITAB02_TYP TYPE STANDARD TABLE OF LINE01_TYP.

*Declare the table based on the 'Table Type'
DATA ITAB02 TYPE ITAB02_TYP.

*Declare the Work Area to use with our Internal Table
DATA WA_ITAB02 TYPE LINE01_TYP.

DATA LINE_CNT TYPE I.

DATA Z_FIELD1 LIKE ZEMPLOYEES-SURNAME.
DATA Z_FIELD2 LIKE ZEMPLOYEES-FORENAME.

*****************************************

PERFORM ITAB02_FILL.

*PERFORM SUB_1 IN PROGRAM ZEMPLOYEE_HIRE USING Z_FIELD1 Z_FIELD2.     " This is sometimes seen in older SAP pgms

*PERFORM SUB_1(ZEMPLOYEE_HIRE) TABLES ITAB02 USING Z_FIELD1 Z_FIELD2. " This is sometimes seen in older SAP pgms

Z_FIELD1 = 'ANDREWS'.
Z_FIELD2 = 'SUSAN'.

PERFORM ITAB02_FILL_AGAIN USING Z_FIELD1 Z_FIELD2.

PERFORM ITAB02_WRITE TABLES ITAB02.

PERFORM ITAB02_MULTI TABLES ITAB02 USING Z_FIELD1 Z_FIELD2.

SELECT * FROM ZEMPLOYEES
  INTO CORRESPONDING FIELDS OF TABLE ITAB02.

LOOP AT ITAB02 INTO WA_ITAB02.
  WRITE WA_ITAB02-SURNAME.
ENDLOOP.

CLEAR: ITAB02, WA_ITAB02.

LOOP AT ITAB02 INTO WA_ITAB02.
  IF WA_ITAB02-SURNAME = 'JONES'.
    WA_ITAB02-SURNAME = 'SMITH'.
    MODIFY ITAB02 FROM WA_ITAB02.
  ENDIF.
ENDLOOP.

DESCRIBE TABLE ITAB02 LINES LINE_CNT.
IF LINE_CNT > 0.
  INSERT WA_ITAB02 INTO ITAB02 INDEX LINE_CNT.

  READ TABLE ITAB02 INDEX 5 INTO WA_ITAB02.

  READ TABLE ITAB02 INTO WA_ITAB02
       WITH KEY SURNAME = 'SMITH'.
ENDIF.


*
*&---------------------------------------------------------------------*
*&      Form  itab02_fill
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM ITAB02_FILL.

  DATA ZEMPL LIKE ZEMPLOYEES-SURNAME.

  SELECT * FROM ZEMPLOYEES
    INTO CORRESPONDING FIELDS OF TABLE ITAB02.

ENDFORM.                    " itab02_fill


*&---------------------------------------------------------------------*
*&      Form  itab02_fill_again
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_Z_FIELD1  text
*      -->P_Z_FIELD2  text
*----------------------------------------------------------------------*
FORM ITAB02_FILL_AGAIN  USING    P_ZSURNAME
                                 P_ZFORENAME.

  WRITE / P_ZSURNAME.
  WRITE / P_ZFORENAME.

  P_ZSURNAME = 'abcde'.

ENDFORM.                    " itab02_fill_again

*&---------------------------------------------------------------------*
*&      Form  itab02_write
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB02  text
*----------------------------------------------------------------------*
FORM ITAB02_WRITE  TABLES   P_ITAB02.

  DATA WA_TMP TYPE LINE01_TYP.

  LOOP AT P_ITAB02 INTO WA_TMP.
    WRITE WA_TMP-SURNAME.
  ENDLOOP.

ENDFORM.                    " itab02_write
*&---------------------------------------------------------------------*
*&      Form  ITAB02_MULTI
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*      -->P_ITAB02  text
*      -->P_Z_FIELD1  text
*      -->P_Z_FIELD2  text
*----------------------------------------------------------------------*
FORM ITAB02_MULTI  TABLES   P_ITAB02

                   USING    P_Z_FIELD1
                            P_Z_FIELD2.

ENDFORM.                    " ITAB02_MULTI
