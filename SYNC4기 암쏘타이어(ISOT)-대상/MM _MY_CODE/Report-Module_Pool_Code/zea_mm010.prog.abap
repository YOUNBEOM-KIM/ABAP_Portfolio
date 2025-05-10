*&---------------------------------------------------------------------*
*& Report ZALV_GRID_CRUD
*&---------------------------------------------------------------------*
*& [2024.04.21 / 김연범] 
*&---------------------------------------------------------------------*
REPORT ZEA_MM010 MESSAGE-ID ZEA_MSG.

INCLUDE ZEA_MM010_TOP.
INCLUDE ZEA_MM010_SCR.
INCLUDE ZEA_MM010_CLS.
INCLUDE ZEA_MM010_PBO.
INCLUDE ZEA_MM010_PAI.
INCLUDE ZEA_MM010_F01.


INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.

START-OF-SELECTION.



  PERFORM SELECT_DATA.
  PERFORM MAKE_DISPLAY_DATA.
  PERFORM DISPLAY_DATA.
