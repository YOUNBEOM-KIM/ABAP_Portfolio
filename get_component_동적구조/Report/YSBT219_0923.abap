*&---------------------------------------------------------------------*
*& Report YSBT219_0923
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YSBT219_0923.

INCLUDE ysbt219_0923top.
INCLUDE ysbt219_0923scr.
INCLUDE ysbt219_0923f01.
INCLUDE ysbt219_0923f02.
INCLUDE ysbt219_0923i01.
INCLUDE ysbt219_0923o01.

*--------------------------------------------------------------------*
* INITIALIZATION
*--------------------------------------------------------------------*
  INITIALIZATION.
*    PERFORM init.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN
*--------------------------------------------------------------------*
  AT SELECTION-SCREEN.
*    PERFORM pai.

*--------------------------------------------------------------------*
* AT SELECTION-SCREEN OUTPUT
*--------------------------------------------------------------------*
  AT SELECTION-SCREEN OUTPUT.
    PERFORM pbo.
*--------------------------------------------------------------------*
* START-OF-SELECITON
*--------------------------------------------------------------------*
  START-OF-SELECTION.
    CHECK sy-batch IS INITIAL.

    CLEAR gv_subrc.
*    PERFORM check_params.
    PERFORM set_params.

    CHECK gv_subrc IS INITIAL.
    PERFORM get_data CHANGING gv_subrc.

    CASE gv_subrc.
      WHEN 0.
        PERFORM set_data.
        CALL SCREEN '0100'.
      WHEN OTHERS.
    ENDCASE.