*&---------------------------------------------------------------------*
*& Include          YSBT219_0923O01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE status_0100 OUTPUT.

  APPEND cl_gui_alv_grid=>mc_fc_detail TO gt_exclude.
 data: it TYPE TABLE of sy-ucomm.
 clear it.
 SET PF-STATUS 'S0100' EXCLUDING gt_fcode[].
* SET PF-STATUS 'STLI' EXCLUDING GT_FCODE[].
*  SET PF-STATUS 'STANDAARD'.
 SET TITLEBAR 'T0100' WITH GV_LINES.
 call FUNCTION 'RS_SET_SELSCREEN_STATUS'
   EXPORTING
     p_status  =  ''                " Status To Be Set
     p_program = sy-repid            " Program to which the status belongs
   TABLES
     p_exclude =  it                " Table of OK codes to be excluded
   .
* DATA: lo_OBJECT TYPE REF TO cl_ctmenu.
* DATA: LT_STD_FCODES TYPE UI_FUNCTIONS.
*  CALL METHOD E_OBJECT->hide_functions
*   EXPORTING
*     fcodes = LT_STD_FCODES                 " Function code
*   .
*  call METHOD cl_gui_cfw=>
ENDMODULE.
*&---------------------------------------------------------------------*
*& Module CREATE_ALV_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE create_alv_0100 OUTPUT.
  PERFORM create_alv_0100 USING    gv_renew
                          CHANGING go_container
                                   go_grid.
ENDMODULE.