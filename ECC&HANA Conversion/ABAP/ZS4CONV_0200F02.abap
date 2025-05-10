*&---------------------------------------------------------------------*
*& Include          ZS4CONV_0200F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form create_alv
*&---------------------------------------------------------------------*
FORM create_alv.

  CHECK go_grid   IS INITIAL.

  IF go_container IS INITIAL.
    CREATE OBJECT go_container
      EXPORTING
        container_name = gc_con1.
    CREATE OBJECT go_grid
      EXPORTING
        i_parent       = go_container.

    PERFORM set_layout   CHANGING gs_layo.
    PERFORM set_fieldcat CHANGING gt_fcat.
    PERFORM set_event    CHANGING go_grid.

    go_grid->set_table_for_first_display(
      EXPORTING
        i_default       = 'X'
        is_layout       = gs_layo
      CHANGING
        it_outtab       = gt_result
        it_fieldcatalog = gt_fcat
      EXCEPTIONS
        OTHERS = 4 ).
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_layout
*&---------------------------------------------------------------------*
FORM set_layout CHANGING ps_layo TYPE lvc_s_layo.

  CLEAR ps_layo.
  ps_layo-cwidth_opt = 'X'.
  ps_layo-col_opt    = 'X'.
  ps_layo-zebra      = 'X'.
  ps_layo-sel_mode   = 'D'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fieldcat
*&---------------------------------------------------------------------*
FORM set_fieldcat CHANGING pt_fcat TYPE lvc_t_fcat.

  PERFORM get_fieldcat  USING    gt_result
                        CHANGING pt_fcat.
  PERFORM make_fieldcat CHANGING pt_fcat.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_fieldcat
*&---------------------------------------------------------------------*
FORM get_fieldcat USING gt_alv TYPE STANDARD TABLE
                  CHANGING pt_fcat TYPE lvc_t_fcat.

  pt_fcat = CORRESPONDING #( cl_salv_ddic=>get_by_dat( pt_alv )
                             MAPPING key = keyflag ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form make_fieldcat
*&---------------------------------------------------------------------*
FORM make_fieldcat CHANGING pt_fcat TYPE lvc_t_fcat.

  LOOP AT pt_fcat INTO DATA(ps_fcat).
    CASE ps_fcat-fieldname.
      WHEN 'DEPER'.
        ps_fcat-coltext = '�대떦��'.
        PS_fcat-key     = 'X'.
    ENDCASE.
    MODIFY pt_fcat FROM ps-fcat.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event
*&---------------------------------------------------------------------*
FORM set_event CHANGING po_grid TYPE REF TO cl_gui_alv_grid.

  CREATE OBJECT gcl_event_receiver.
  SET HANDLER : gcl_event_receiver->handle_toolbar       FOR po_grid.
  SET HANDLER : gcl_event_receiver->handle_user_command  FOR po_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_toolbar
*&---------------------------------------------------------------------*
FORM event_toolbar USING p_object TYPE REF TO cl_alv_event_toolbar_set
                         p_interactive.

  DATA : ls_toolbar TYPE stb_button
       , lt_toolbar TYPE stb_button
       , lv_tabix   TYPE sy=tabix.

  ls_toolbar-butn_type = 3.
  ls_toolbar-function  = '&&SEP01'.
  ls_toolbar-disabled  = 'X'.
  lv_tabix +=1.
  INSERT ls_toolbar INTO p_object->mt_toolbar INDEX lv_tabix.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_user_command
*&---------------------------------------------------------------------*
FORM event_user_command USING p_ucomm TYPE sy-ucomm.
    " **************蹂댁븞****************
ENDFORM.
*&---------------------------------------------------------------------*
*& Form event_double_click
*&---------------------------------------------------------------------*
FORM event_double_click USING p_row
                              p_column
                              ps_row_no.
    " **************蹂댁븞****************
ENDFORM.