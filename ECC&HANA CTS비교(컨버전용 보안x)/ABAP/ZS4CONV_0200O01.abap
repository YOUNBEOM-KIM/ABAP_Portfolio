*&---------------------------------------------------------------------*
*& Include          ZS4CONV_0200O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form pbo
*&---------------------------------------------------------------------*
FORM pbo .
  LOOP AT SCREEN.
    IF screen-name EQ 'SO_DEF-LOW'
    OR screen-name EQ 'SO_DEF-HIGH'.
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module STATUS_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0200 OUTPUT.
 SET PF-STATUS 'S0200'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.
 SET PF-STATUS 'S0100'.
 SET TITLEBAR  'T0100'.
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv_0100 OUTPUT.
  PERFORM create_alv.
ENDMODULE.