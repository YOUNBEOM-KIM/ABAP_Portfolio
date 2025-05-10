*&---------------------------------------------------------------------*
*& Report YE07_STRO
*&---------------------------------------------------------------------*
*& [2024.04.22 / 김연범]
*&---------------------------------------------------------------------*
REPORT ZEA_MM020 MESSAGE-ID ZEA_MSG.

INCLUDE ZEA_MM020TOP.
INCLUDE ZEA_MM020SCR.
INCLUDE ZEA_MM020CLS.
INCLUDE ZEA_MM020PBO.
INCLUDE ZEA_MM020PAI.
INCLUDE ZEA_MM020F01.

INITIALIZATION.

AT SELECTION-SCREEN OUTPUT.

AT SELECTION-SCREEN.


START-OF-SELECTION.

PERFORM SELECT_DATA.
PERFORM MAKE_DISPLAY_DATA.
PERFORM DISPLAY_DATA.
