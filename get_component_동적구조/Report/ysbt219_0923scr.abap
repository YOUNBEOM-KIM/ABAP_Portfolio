*&---------------------------------------------------------------------*
*& Include          YSBT219_0923SCR
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* SELECTION-SCREEN
*--------------------------------------------------------------------*
  SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.

    SELECTION-SCREEN BEGIN OF LINE.
      PARAMETERS: p_mara RADIOBUTTON GROUP rg1 USER-COMMAND uc DEFAULT 'X'.
      SELECTION-SCREEN COMMENT (10) TEXT-p01 FOR FIELD p_mara.
      PARAMETERS: p_marc RADIOBUTTON GROUP rg1.
      SELECTION-SCREEN COMMENT (10) TEXT-p02 FOR FIELD p_marc.
    SELECTION-SCREEN END OF LINE.
  SELECTION-SCREEN END OF BLOCK b01.

  SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
    SELECT-OPTIONS: s_matnr FOR mara-matnr no INTERVALS MODIF ID mar,
                    s_werks FOR marc-werks MODIF ID mac.
  SELECTION-SCREEN END OF BLOCK b02.