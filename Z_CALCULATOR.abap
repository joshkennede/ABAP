*&---------------------------------------------------------------------*
*& Report  Z_CALCULATOR
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*

REPORT z_calculator.

PARAMETERS: pa_int1(5)  TYPE n DEFAULT 1,
            pa_op       TYPE c LENGTH 1 DEFAULT '+',
            pa_int2(5)  TYPE n DEFAULT 1.

DATA:       gv_result   TYPE p LENGTH 16 DECIMALS 2.

  CASE pa_op.
    WHEN '+'.
      gv_result = pa_int1 + pa_int2.
    WHEN '-'.
      gv_result = pa_int1 - pa_int2.
    WHEN '*'.
      gv_result = pa_int1 * pa_int2.
    WHEN '/'.
      gv_result = pa_int1 / pa_int2.
  ENDCASE.

  WRITE: 'Result:', gv_result.
