REPORT Z_CHARACTER_STRINGS.

TABLES ZEMPLOYEES.

DATA MYCHAR(10)       TYPE C.
DATA ZEMPLOYEES1(40)  TYPE C.
DATA ZEMPLOYEES2      LIKE ZEMPLOYEES-SURNAME.
************************************************************************

DATA ZNUMBER1         TYPE N.

************************************************************************

* Concatenateing String Fields

* Definition: CONCATENATE F1 F2 INTO D1 [SEPARATED BY SEP].

DATA: TITLE(15)                 TYPE C VALUE 'MR',
      SURNAME(40)               TYPE C VALUE 'Smith',
      FORNAME(40)               TYPE C VALUE 'Joe',
      SEP,
      DESTINATION(200)          TYPE C,
      SPACED_NAME(20)           TYPE C VALUE 'Mr  Joe  Smith',
      LEN                       TYPE I,
      SURNAME2(40),
      EMPL_NUM(10),
      MYSTRING(30)              TYPE C,
      A1(10)                    TYPE C,
      A2(10)                    TYPE C,
      A3(10)                    TYPE C,
      SEP2(2)                   TYPE C VALUE '**',
      INT_TELEPHONE_NUM(17)     TYPE C,
      COUNTRY_CODE(3)           TYPE C,
      TELEPHONE_NUM(14)         TYPE C.
*----

CONCATENATE TITLE SURNAME FORNAME INTO DESTINATION SEPARATED BY SEP.
WRITE DESTINATION.
ULINE.

CONCATENATE TITLE SURNAME FORNAME INTO DESTINATION SEPARATED BY SEP.
WRITE DESTINATION.
ULINE.
**********************************
* Condensing Character Strings
* Definition: CONDENSE c [NO-GAPS]

CONDENSE SPACED_NAME.
WRITE SPACED_NAME.
ULINE.

CONDENSE SPACED_NAME NO-GAPS.
WRITE SPACED_NAME.
ULINE.

****************************
* Find the Length of a String

LEN = STRLEN( SURNAME ).
WRITE: 'The length of the SURNAME field is', LEN.
ULINE.

****************************
* Replacing Character Strings

SURNAME2 = 'Mr, Joe Smith'.
REPLACE ',' WITH '.' INTO SURNAME2.
WRITE: SURNAME2.
ULINE.

WHILE SY-SUBRC = 0.
  REPLACE ',' WITH '.' INTO SURNAME2.
ENDWHILE.
WRITE: SURNAME2.
ULINE.

****************************
* Searching for Specific Characters

SURNAME2 = 'Mr Joe Smith'.

WRITE: / 'Searching: "Mr Joe Smith"'.
SKIP.

* Blank Spaces are ignored
SEARCH SURNAME2 FOR 'Joe    '.
WRITE: / 'Searching: "Joe     "'.
WRITE: / 'sy-subrc: ', SY-SUBRC, 'sy-fdpos: ', SY-FDPOS.
ULINE.

* Blank Spaces are taken into account
SEARCH SURNAME2 FOR '.Joe    .'.
WRITE: / 'Searching: ".Joe     ."'.
WRITE: / 'sy-subrc: ', SY-SUBRC, 'sy-fdpos: ', SY-FDPOS.
ULINE.

* Wild card search - word ending with 'mit'
SEARCH SURNAME2 FOR '*ith'.
WRITE: / 'Searching for "*ith"'.
WRITE: / 'sy-subrc: ', SY-SUBRC, 'sy-fdpos: ', SY-FDPOS.
ULINE.

* Wild card search - word ending with 'mit'
SEARCH SURNAME2 FOR 'Smi*'.
WRITE: / 'Searching for "Smi*"'.
WRITE: / 'sy-subrc: ', SY-SUBRC, 'sy-fdpos: ', SY-FDPOS.
ULINE.

****************************
* SHIFT Statement


*EMPL_NUM = '0000654321'.
*SHIFT EMPL_NUM LEFT DELETING LEADING '0'.     " Deletes all leading zeros and shifts the number to the left
*WRITE EMPL_NUM.
*ULINE.
*
*EMPL_NUM = '0000654321'.                      " Deletes default amount of zeros(1), but does not shift.
*SHIFT EMPL_NUM.
*WRITE EMPL_NUM.
*ULINE.
*
*EMPL_NUM = '0000654321'.                      " Moves the leading zero to the end of the number generated.
*SHIFT EMPL_NUM CIRCULAR.
*WRITE EMPL_NUM.

****************************
* Split Statement

*MYSTRING = ' 1234** ABCD **6789'.
MYSTRING = ' 1234** ABCD **6789**WXYZ'.
WRITE MYSTRING.
SKIP.

SPLIT MYSTRING AT SEP2 INTO A1 A2 A3.

WRITE / A1.
WRITE / A2.
WRITE / A3.
ULINE.
****************************
* SubFields

INT_TELEPHONE_NUM = '+44-(0)207-123456'.
WRITE INT_TELEPHONE_NUM.
SKIP.

COUNTRY_CODE = INT_TELEPHONE_NUM(3).

TELEPHONE_NUM = INT_TELEPHONE_NUM+4(13).

WRITE / COUNTRY_CODE.
WRITE / TELEPHONE_NUM.

COUNTRY_CODE+1(2) = '01'.

WRITE / COUNTRY_CODE.
