*&---------------------------------------------------------------------*
*& Include          YSBT219_0923F02
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form create_alv_0100
*&---------------------------------------------------------------------*
FORM create_alv_0100 USING    pv_renew
                     CHANGING po_container TYPE REF TO cl_gui_custom_container
                              po_grid      TYPE REF TO cl_gui_alv_grid.

  CHECK po_grid   IS INITIAL
     OR pv_renew  IS NOT INITIAL.
  IF po_container IS INITIAL.
    CREATE OBJECT po_container
      EXPORTING
        container_name = gc_container.
    CREATE OBJECT po_grid
      EXPORTING
        i_parent = po_container.
  ENDIF.

  PERFORM set_layout.
  PERFORM set_event USING po_grid.
*  PERFORM set_sort.
  PERFORM set_fieldcat.
  PERFORM set_exclude USING po_grid->m_guid
                      CHANGING gt_exclude[].

  CALL METHOD go_grid->set_table_for_first_display
    EXPORTING
*     is_variant      =                  " Layout
*     i_save          =                  " Save Layout
      i_default       = 'X'              " Default Display Variant
      is_layout       = gs_layout                  " Layout
*     it_toolbar_excluding          = GT_EXCLUDE[]                 " Excluded Toolbar Standard Functions
    CHANGING
      it_outtab       = <gt_alv>                 " Output Table
      it_fieldcatalog = gt_fcat[]                 " Field Catalog
*     it_sort         =                  " Sort Criteria
*     it_filter       =                  " Filter Criteria
    .
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fieldcat
*&---------------------------------------------------------------------*
FORM set_fieldcat .
  PERFORM get_fieldcat USING    <gt_alv>
                       CHANGING gt_fcat[].

  PERFORM make_fieldcat_0100.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_fieldcat
*&---------------------------------------------------------------------*
FORM get_fieldcat  USING    pt_alv  TYPE STANDARD TABLE
                   CHANGING pt_fcat TYPE  lvc_t_fcat.

  DATA: lo_dref TYPE REF TO data.

  CREATE DATA lo_dref LIKE pt_alv.
  FIELD-SYMBOLS <lt_tab> TYPE STANDARD TABLE.
  ASSIGN lo_dref->* TO <lt_tab>.

  TRY.
      CALL METHOD cl_salv_table=>factory
        IMPORTING
          r_salv_table = DATA(lr_table)
        CHANGING
          t_table      = <lt_tab>.
    CATCH cx_salv_msg. " ALV: General Error Class with Message
  ENDTRY.

  pt_fcat = cl_salv_controller_metadata=>get_lvc_fieldcatalog(
            r_columns = lr_table->get_columns( )
            r_aggregations = lr_table->get_aggregations( ) ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_layout
*&---------------------------------------------------------------------*
FORM set_layout .
  CLEAR gs_layout.
  gs_layout-sel_mode   = 'D'.
  gs_layout-zebra      = 'X'.
  gs_layout-cwidth_opt = 'X'.
  gs_layout-col_opt    = 'X'.
  gs_layout-ctab_fname = 'SCOL'.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form make_fieldcat_0100
*&---------------------------------------------------------------------*
FORM make_fieldcat_0100.

  IF gv_subrc EQ 0.
    LOOP AT gt_fcat.
      CASE gt_fcat-fieldname.
        WHEN 'MATNR'.
          gt_fcat-style = alv_style_font_underlined.
        WHEN 'ICON'.
          gt_fcat-no_out = 'X'.
        WHEN 'STYL'.
          gt_fcat-no_out = 'X'.
        WHEN 'SCOL'.
          gt_fcat-no_out = 'X'.
        WHEN 'MODE'.
          gt_fcat-no_out = 'X'.
      ENDCASE.
      MODIFY gt_fcat.
    ENDLOOP.
  ENDIF.

ENDFORM.
*--------------------------------------------------------------------*
* EVENT HANDLERS
*--------------------------------------------------------------------*
CLASS gcl_receiver IMPLEMENTATION.
  METHOD: on_double_click.
    PERFORM double_click USING e_row
                               e_column
                               sender.
  ENDMETHOD.
  METHOD: on_handle_context_menu_request.
    PERFORM handle_context_menu_requset USING e_object
                                              sender.

  ENDMETHOD.
  METHOD: handle_menu_button.
*    PERFORM handle_menu_button USING e_object
*                                     e_ucomm
*                                     sender.
  ENDMETHOD.
ENDCLASS.
*&---------------------------------------------------------------------*
*& Form double_click
*&---------------------------------------------------------------------*
FORM double_click USING ps_row    TYPE lvc_s_row
                        ps_column TYPE lvc_s_col
                        sender    TYPE REF TO cl_gui_alv_grid.

  FIELD-SYMBOLS : <ls_data>  TYPE any,
                  <lv_value> TYPE any.

  CHECK ps_row-index IS NOT INITIAL.

  READ TABLE <gt_alv> ASSIGNING <ls_data> INDEX ps_row-index.

  CHECK sy-subrc IS INITIAL.

  ASSIGN COMPONENT ps_column-fieldname OF STRUCTURE <ls_data> TO <lv_value>.

  CASE ps_column-fieldname.
    WHEN 'MATNR'.
      IF <lv_value> IS NOT INITIAL.
        SET PARAMETER ID : 'MAT' FIELD <lv_value>.
        CALL TRANSACTION 'MM03'.
      ENDIF.
  ENDCASE.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form handle_context_menu_requset
*&---------------------------------------------------------------------*
FORM handle_context_menu_requset  USING po_object TYPE REF TO cl_ctmenu
                                        sender    TYPE REF TO cl_gui_alv_grid.

  DATA: lt_fcodes TYPE ui_funcattr,
        ls_fcode  TYPE uiattentry,
        ls_func   TYPE ui_func,
        lt_func   TYPE ui_functions.
*-- funciton list
  CALL METHOD po_object->get_functions
    IMPORTING
      fcodes = lt_fcodes. " Function List

  LOOP AT lt_fcodes INTO ls_fcode.
    ls_func = ls_fcode-fcode.
    APPEND ls_func TO lt_func.
  ENDLOOP.

  po_object->disable_functions( lt_func ).
  po_object->hide_functions( lt_func ).
*  po_object->clear( ).
ENDFORM.
*--------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form set_event
*&---------------------------------------------------------------------*
FORM set_event USING po_grid TYPE REF TO cl_gui_alv_grid.

  IF go_receiver IS INITIAL.
    CREATE OBJECT go_receiver.
  ENDIF.
  "enter 칠때 타는거?
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = po_grid->mc_evt_enter.
  " 값이 변경 되마자자 data changed
  CALL METHOD po_grid->register_edit_event
    EXPORTING
      i_event_id = po_grid->mc_evt_modified.

  SET HANDLER go_receiver->on_double_click FOR go_grid.
  SET HANDLER go_receiver->on_handle_context_menu_request FOR go_grid.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_exclude
*&---------------------------------------------------------------------*
FORM set_exclude USING pv_mguid
                 CHANGING pt_exclude  TYPE ui_functions.
  DATA: lv_class  TYPE string.
  DATA: lv_field  TYPE string.
  DATA: lv_parent TYPE c.
  DATA: lv_append TYPE c.
  DATA: lt_attr   TYPE abap_attrdescr_tab WITH HEADER LINE.
  FIELD-SYMBOLS: <lv_value> TYPE any.

  lv_class = 'CL_GUI_ALV_GRID'.
  PERFORM get_alv_toolbar_list USING    lv_class
                               CHANGING lt_attr[].

  CLEAR: pt_exclude.
  LOOP AT lt_attr.

    CONCATENATE lv_class '=>' lt_attr-name
           INTO lv_field.

    ASSIGN (lv_field) TO <lv_value>.
    CHECK <lv_value> IS ASSIGNED.

    lv_append = 'X'.

    CASE <lv_value>.
      WHEN cl_gui_alv_grid=>mc_fc_info.
      WHEN cl_gui_alv_grid=>mc_fc_loc_append_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_copy.
      WHEN cl_gui_alv_grid=>mc_fc_loc_copy_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_cut.
      WHEN cl_gui_alv_grid=>mc_fc_loc_delete_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_insert_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_move_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_paste.
      WHEN cl_gui_alv_grid=>mc_fc_loc_paste_new_row.
      WHEN cl_gui_alv_grid=>mc_fc_loc_undo.
      WHEN cl_gui_alv_grid=>mc_fc_maximum.
      WHEN cl_gui_alv_grid=>mc_fc_minimum.
      WHEN cl_gui_alv_grid=>mc_fc_pc_file.
      WHEN cl_gui_alv_grid=>mc_fc_print.
      WHEN cl_gui_alv_grid=>mc_fc_print_back.
      WHEN cl_gui_alv_grid=>mc_fc_print_prev.
      WHEN cl_gui_alv_grid=>mc_fc_refresh.
      WHEN cl_gui_alv_grid=>mc_fc_reprep.
      WHEN cl_gui_alv_grid=>mc_fc_select_all.
      WHEN cl_gui_alv_grid=>mc_fc_send.
      WHEN cl_gui_alv_grid=>mc_fc_separator.
      WHEN cl_gui_alv_grid=>mc_fc_to_office.
      WHEN cl_gui_alv_grid=>mc_fc_to_rep_tree.
      WHEN cl_gui_alv_grid=>mc_fc_unfix_columns.
      WHEN cl_gui_alv_grid=>mc_fc_url_copy_to_clipboard.
      WHEN cl_gui_alv_grid=>mc_fc_view_crystal.
      WHEN cl_gui_alv_grid=>mc_fc_view_excel.
      WHEN cl_gui_alv_grid=>mc_fc_view_grid.
      WHEN cl_gui_alv_grid=>mc_fc_view_lotus.
      WHEN cl_gui_alv_grid=>mc_fc_word_processor.
      WHEN OTHERS.

        CLEAR: lv_append.

    ENDCASE.

    CHECK lv_append IS NOT INITIAL.
    APPEND <lv_value> TO pt_exclude.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_alv_toolbar_list
*&---------------------------------------------------------------------*
FORM get_alv_toolbar_list  USING    pv_class
                           CHANGING pt_attr TYPE abap_attrdescr_tab.

  DATA: lo_cldesc TYPE REF TO cl_abap_classdescr.
  DATA: lo_tydesc TYPE REF TO cl_abap_typedescr.

  CLEAR: pt_attr[].
  CALL METHOD cl_abap_typedescr=>describe_by_name
    EXPORTING
      p_name         = pv_class
    RECEIVING
      p_descr_ref    = lo_tydesc
    EXCEPTIONS
      type_not_found = 1.

  lo_cldesc ?= lo_tydesc.
  pt_attr[] = lo_cldesc->attributes[].
  DELETE pt_attr WHERE visibility NE cl_abap_objectdescr=>public.
  DELETE pt_attr WHERE name NP 'MC_FC*'.

ENDFORM.
*&--------