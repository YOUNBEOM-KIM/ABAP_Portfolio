*&---------------------------------------------------------------------*
*& Include          YSBT219_0923I01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*&      Module  USER_COMMAND_0100  INPUT
*----------------------------------------------------------------------*
MODULE exit INPUT.
  CLEAR: gv_ok_save.
  gv_ok_save = gv_ok_code.
  CASE gv_ok_save.
    WHEN 'BACK' OR 'EXIT' OR 'CANC'.
      LEAVE TO SCREEN 0.
  ENDCASE.
ENDMODULE.