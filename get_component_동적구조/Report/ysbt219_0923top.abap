*&---------------------------------------------------------------------*
*& Include          YSBT219_0923TOP
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* TALBES
*--------------------------------------------------------------------*
  TABLES: mara, marc.

*--------------------------------------------------------------------*
* CONSTANTS
*--------------------------------------------------------------------*
  CONSTANTS: gc_icon_gre  TYPE char30 VALUE 'ICON_LED_GREEN',
             gc_icon_red  TYPE char30 VALUE 'ICON_LED_RED',
             gc_container TYPE char30 VALUE 'GC_CONTAINER'.

  CONSTANTS: gc_mar_where TYPE char50 VALUE 'MATNR IN S_MATNR',
             gc_mac_where TYPE char50 VALUE 'MATNR IN S_MATNR AND WERKS IN S_WERKS'.

  CONSTANTS: gc_100_rows  TYPE char20 VALUE '100'.

*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*
  TYPES: BEGIN OF gty_s_alv_head,
           icon TYPE icon-id,
         END OF gty_s_alv_head.

  TYPES: BEGIN OF gty_s_alv_foot,
           styl TYPE lvc_t_styl,
           scol TYPE lvc_t_scol,
           mode TYPE c,
         END OF gty_s_alv_foot.

  TYPES: BEGIN OF gty_s_maktx,
           matnr TYPE makt-matnr,
           maktx TYPE makt-maktx,
           spras TYPE sy-langu,
         END OF gty_s_maktx.

  TYPES : BEGIN OF gty_s_alv_desc,
            maktx TYPE makt-maktx,
          END OF gty_s_alv_desc.

*--------------------------------------------------------------------*
* GLOBAL DATA
*--------------------------------------------------------------------*
  DATA: gs_header TYPE gty_s_alv_head,
        gs_foot   TYPE gty_s_alv_foot,
        gt_maktx  TYPE TABLE OF gty_s_maktx WITH HEADER LINE,
        gs_desc   TYPE gty_s_alv_desc.

  DATA: go_container TYPE REF TO cl_gui_custom_container,
        go_grid      TYPE REF TO cl_gui_alv_grid.
  DATA: gt_fcat      TYPE lvc_t_fcat WITH HEADER LINE.

  DATA: gv_table     TYPE tabname.

  DATA: gv_subrc   TYPE sy-subrc,
        gv_ok_code TYPE sy-ucomm,
        gv_ok_save TYPE sy-ucomm,
        gv_lines   TYPE sy-tfill,
        gs_variant TYPE disvariant,
        gs_layout  TYPE lvc_s_layo,
        gv_renew.
  DATA: gt_exclude   TYPE ui_functions WITH HEADER LINE.
  DATA: gt_fcode     TYPE ui_functions WITH HEADER LINE.

  DATA: alv_style_font_underlined(4) TYPE x VALUE '00000200'.
  CLASS: gcl_receiver DEFINITION DEFERRED.
  DATA: go_receiver   TYPE REF TO gcl_receiver .
*--------------------------------------------------------------------*
* FIELD-SYMBOLS
*--------------------------------------------------------------------*
  FIELD-SYMBOLS: <gt_alv> TYPE STANDARD TABLE,
                 <gs_alv> TYPE any,
                 <gs_sub> TYPE any.

*  DATA: lt_mara TYPE TABLE OF mara,
*        lt_marc TYPE TABLE OF marc.

*--------------------------------------------------------------------*
* EVENT HANDLER
*--------------------------------------------------------------------*
  CLASS: gcl_receiver DEFINITION.
    PUBLIC SECTION.
      METHODS:
        on_double_click
          FOR EVENT double_click OF cl_gui_alv_grid
          IMPORTING e_row
                    e_column
                    sender.
      METHODS:
        on_handle_context_menu_request
          FOR EVENT context_menu_request OF cl_gui_alv_grid
          IMPORTING e_object
                    sender.
      METHODS:
        handle_menu_button
          FOR EVENT menu_button OF cl_gui_alv_grid
          IMPORTING sender
                    e_object
                    e_ucomm.
  ENDCLASS.