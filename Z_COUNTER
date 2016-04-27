REPORT z_counter.

********************
* Data Declaration *
********************

DATA: a           TYPE n LENGTH 2,  " Declaring a data type which will be used for integer structure A
      b           TYPE n LENGTH 2,  " Declaring a data type which will be used for integer structure A
      i           TYPE i,           " Declaring a data type which will be used for integer structure B
      j           TYPE i,           " Declaring a data type which will be used for integer structure B
      position    TYPE i.           " Declaring a data type which will be used for integer structure B

********************
* Selection Screen *
********************

SELECTION-SCREEN BEGIN OF BLOCK jkpyrmd WITH FRAME TITLE text-001.

*User Input Field

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-002.
PARAMETERS: count(2) DEFAULT '10' OBLIGATORY.
SELECTION-SCREEN POSITION POS_HIGH.
SELECTION-SCREEN END OF LINE.

*Radiobutton Group - Three Options

SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-004.
PARAMETERS: apyr RADIOBUTTON GROUP otpt.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-005.
PARAMETERS: bpyr RADIOBUTTON GROUP otpt.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN BEGIN OF LINE.
SELECTION-SCREEN COMMENT  01(18) text-006.
PARAMETERS: both RADIOBUTTON GROUP otpt.
SELECTION-SCREEN POSITION POS_LOW.
SELECTION-SCREEN END OF LINE.

SELECTION-SCREEN END OF BLOCK jkpyrmd.

**************************************
* Num validation on selection screen *
**************************************

AT SELECTION-SCREEN ON count.
  IF count > 10.
    MESSAGE e000(zmes5).
  ENDIF.

*****************************************
* Logic for creating integer structures *
*****************************************

START-OF-SELECTION.

*INT Structure A - Single Sided Triagle

  a = count.
  IF apyr = 'X' OR both = 'X'.

    WHILE a > 0.                                   " Outer loop to write user input
      WRITE: / count NO-ZERO NO-GAP.
      b = count.

      WHILE b > a.                                 " Inner loop to write user input
        b = b - 1.                                 " descending iteratively on each subsequent row
        WRITE b NO-ZERO NO-GAP.
      ENDWHILE.

      a = a - 1.
    ENDWHILE.
    ULINE.

  ENDIF.



*INT Structure B - Double Sided Triangle

  IF bpyr = 'X' OR both = 'X'.

    DO count TIMES.                                " Outer loop to write each row/position
      i = sy-index.
      position = ( count - sy-index ) * 3 + 1.
      WRITE AT /position space.

      DO sy-index TIMES.                           " Inner loop to count up
        WRITE (2) sy-index.
      ENDDO.

      DO sy-index TIMES.                           " Inner loop to count down
        j = i - sy-index.
        WRITE (2) j NO-ZERO.
      ENDDO.
    ENDDO.
    ULINE.

  ENDIF.

END-OF-SELECTION.
