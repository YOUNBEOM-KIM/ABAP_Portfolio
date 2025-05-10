*&---------------------------------------------------------------------*
*& Include          YSBT219_0923F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form init
*&---------------------------------------------------------------------*
FORM init.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form pai
*&---------------------------------------------------------------------*
FORM pai.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form pbo
*&---------------------------------------------------------------------*
FORM pbo.
  LOOP AT SCREEN.
    CASE 'X'.
      WHEN p_mara.
        CASE screen-group1.
          WHEN 'MAC'.
            screen-active = 0.
        ENDCASE.
      WHEN p_marc.
        CASE screen-group1.
          WHEN 'MAC'.
            screen-active = 1.
        ENDCASE.
    ENDCASE.
    MODIFY SCREEN.
  ENDLOOP.

  DATA: lt_restrict TYPE sscr_restrict,
        ls_opt      TYPE sscr_opt_list,
        ls_ass      TYPE sscr_ass.
  ls_opt-name = 'KEY1'.
  ls_opt-options-eq = 'X'.
  APPEND ls_opt TO lt_restrict-opt_list_tab.
  ls_ass-kind = 'S'.
  ls_ass-name = 'S_MATNR'.
  ls_ass-sg_main = 'I'.
  ls_ass-sg_addy = ''.
  ls_ass-op_main = 'KEY1'.
  APPEND ls_ass TO lt_restrict-ass_tab.
  CALL FUNCTION 'SELECT_OPTIONS_RESTRICT'
    EXPORTING
      program                = sy-repid                 " Program name (default SY-CPROG or SY-LDBPG)
      restriction            = lt_restrict                 " Description of restrictions
      db                     = space            " X: Call using logical database
    EXCEPTIONS
      too_late               = 1                " Call is too late
      repeated               = 2                " Multiple call using LDB or report
      selopt_without_options = 3                " One of the select-options contains no valid options
      selopt_without_signs   = 4                " One of the select-options does not have a valid sign
      invalid_sign           = 5                " Invalid sign
      empty_option_list      = 6                " One of the options lists is empty
      invalid_kind           = 7                " One line has a KIND value unequal to A, B, or S
      repeated_kind_a        = 8                " More than one line has KIND = 'A'
      OTHERS                 = 9.
  IF sy-subrc <> 0.
*   MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*     WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form CHECK_PARAMS
*&---------------------------------------------------------------------*
FORM check_params.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form SET_PARAMS
*&---------------------------------------------------------------------*
FORM set_params.
  DATA: lo_data   TYPE REF TO data.
  DATA: lo_ref    TYPE REF TO data.
  DATA: lo_tadesc TYPE REF TO cl_abap_tabledescr,
        lo_struct TYPE REF TO cl_abap_structdescr.
  DATA: lt_comps TYPE abap_component_tab WITH HEADER LINE,
        lt_temp  TYPE abap_component_tab WITH HEADER LINE.

  IF gv_subrc IS INITIAL.
    CASE 'X'.
      WHEN p_mara.
        gv_table = 'MARA'.
      WHEN p_marc.
        gv_table = 'MARC'.
    ENDCASE.
  ENDIF.
*--------------------------------------------------------------------*
  TABLES: bseg.
  CALL FUNCTION 'YB_RTTI'
    EXPORTING
      iv_table = gv_table "테이블이름
    IMPORTING
      et_data  = lo_data. " 데이터 참조

  ASSIGN lo_data->* TO <gs_sub>.
*--------------------------------------------------------------------*
  PERFORM get_component USING    gs_header
                        CHANGING lt_comps[].
  PERFORM get_component USING    <gs_sub>
                        CHANGING lt_comps[].
  PERFORM get_component USING    gs_desc
                        CHANGING lt_comps[].
  PERFORM get_component USING    gs_foot
                        CHANGING lt_comps[].
*--------------------------------------------------------------------*
  CALL FUNCTION 'YB_CREATE_STRUCT_BY_COMPONENTS'
    EXPORTING
      it_comps  = lt_comps[]
    IMPORTING
      eo_stdesc = lo_struct.

  CALL METHOD cl_abap_tabledescr=>create
    EXPORTING
      p_line_type = lo_struct
    RECEIVING
      p_result    = lo_tadesc.

  CREATE DATA lo_ref TYPE HANDLE lo_tadesc.
  ASSIGN lo_ref->* TO <gt_alv>.

  CREATE DATA lo_ref LIKE LINE OF <gt_alv>.
  ASSIGN lo_ref->* TO <gs_alv>.
*--------------------------------------------------------------------*
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data CHANGING pv_subrc.

  DATA: lv_where TYPE string.

  CLEAR: <gt_alv>[]
       , lv_where.

  CASE 'X'.
    WHEN p_mara.
      lv_where = gc_mar_where.
    WHEN p_marc.
      lv_where = gc_mac_where.
  ENDCASE.

  SELECT *
    FROM (gv_table) UP TO gc_100_rows ROWS
    INTO CORRESPONDING FIELDS OF TABLE <gt_alv>
    WHERE (lv_where).

  pv_subrc = sy-subrc.

  IF pv_subrc IS NOT INITIAL.
    MESSAGE '검색결과 없음' TYPE 'I'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_data
*&---------------------------------------------------------------------*
FORM set_data .
  TYPES: BEGIN OF lty_s_matnr,
           matnr TYPE mara-matnr,
         END OF lty_s_matnr.

  DATA: lt_matnr      TYPE TABLE OF lty_s_matnr WITH HEADER LINE.
  DATA: ls_cell_color TYPE lvc_s_scol.
  DATA: lc_select     TYPE char30.

  DEFINE set_cell_color.
    CLEAR ls_cell_color.
    ls_cell_color-fname = &1.
    ls_cell_color-color-col = &2.
    ls_cell_color-color-int = &3.
    ls_cell_color-color-inv = &4.
    APPEND ls_cell_color TO gs_foot-scol.
  END-OF-DEFINITION.

  FIELD-SYMBOLS: <lv_value> TYPE any.

  lc_select = ( gv_table ).
  CASE lc_select.
    WHEN 'MARA'.
      SELECT matnr
        FROM (gv_table)
        INTO CORRESPONDING FIELDS OF TABLE lt_matnr.
      IF gv_subrc EQ 0.
        SORT lt_matnr BY matnr.
      ELSE.
        gv_subrc = 4.
      ENDIF.
    WHEN 'MARC'.
      SELECT matnr
        FROM (gv_table)
        INTO CORRESPONDING FIELDS OF TABLE lt_matnr.
      IF gv_subrc EQ 0.
        SORT lt_matnr BY matnr.
      ELSE.
        gv_subrc = 4.
      ENDIF.
    WHEN OTHERS.
  ENDCASE.

  SELECT matnr spras maktx
    INTO CORRESPONDING FIELDS OF TABLE gt_maktx
    FROM makt
   ORDER BY matnr spras.

  APPEND lt_matnr.

  LOOP AT <gt_alv> INTO <gs_alv>.
    MOVE-CORRESPONDING <gs_alv> TO gs_header.
    MOVE-CORRESPONDING <gs_alv> TO gs_foot.

    UNASSIGN <lv_value>.
    ASSIGN COMPONENT 'MATNR' OF STRUCTURE <gs_alv> TO <lv_value>.

    IF <lv_value> IS ASSIGNED.
      READ TABLE lt_matnr WITH KEY matnr = <lv_value>.
      IF sy-subrc = 0.
        set_cell_color 'MATNR' 5 1 0.
        ASSIGN COMPONENT 'MTART' OF STRUCTURE <gs_alv> TO <lv_value>.
        IF <lv_value> IS ASSIGNED.
          IF <lv_value> = 'PART'.
            set_cell_color 'MTART' 6 1 0.
          ENDIF.
        ENDIF.
      ENDIF.
      READ TABLE gt_maktx WITH KEY   matnr = <lv_value>
                                     spras = sy-langu
                                     BINARY SEARCH.
      IF sy-subrc <> 0.
        READ TABLE gt_maktx WITH KEY matnr = <lv_value>
                                     BINARY SEARCH.
      ENDIF.
      gs_desc-maktx = gt_maktx-maktx.
    ENDIF.

    MOVE-CORRESPONDING gs_header TO <gs_alv>.
    MOVE-CORRESPONDING gs_foot   TO <gs_alv>.
    MOVE-CORRESPONDING gs_desc   TO <gs_alv>.

    MODIFY <gt_alv> FROM <gs_alv>.
  ENDLOOP.

  DESCRIBE TABLE <gt_alv> LINES gv_lines.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_component
*&---------------------------------------------------------------------*
FORM get_component  USING    iv_data  TYPE any
                    CHANGING ct_comps TYPE abap_component_tab.

  DATA: lt_temp TYPE abap_component_tab WITH HEADER LINE.

  CALL FUNCTION 'YB_GET_COMPONENT'
    EXPORTING
      it_data  = iv_data
    IMPORTING
      et_comps = lt_temp[].

  IF lt_temp[] IS NOT INITIAL.
    APPEND LINES OF lt_temp[] TO ct_comps[].
  ENDIF.

ENDFORM.