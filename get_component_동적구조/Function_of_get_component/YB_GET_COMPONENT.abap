FUNCTION yb_get_component.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_DATA) TYPE  ANY
*"  EXPORTING
*"     REFERENCE(ET_COMPS) TYPE  ABAP_COMPONENT_TAB
*"----------------------------------------------------------------------

  DATA : lv_type   TYPE abap_typekind.
  DATA : lo_stdesc TYPE REF TO cl_abap_structdescr.
  DATA : lo_tadesc TYPE REF TO cl_abap_tabledescr.
  DATA: lv_suffix TYPE string.

  CLEAR : et_comps[].

  DESCRIBE FIELD it_data TYPE lv_type.

  CASE lv_type.
    WHEN cl_abap_typedescr=>typekind_table.
      lo_tadesc ?= cl_abap_typedescr=>describe_by_data( it_data ).
      lo_stdesc ?= lo_tadesc->get_table_line_type( ).

    WHEN cl_abap_typedescr=>typekind_struct1
      OR cl_abap_typedescr=>typekind_struct2.
      lo_stdesc ?= cl_abap_typedescr=>describe_by_data( it_data ).

  ENDCASE.

  PERFORM get_subcomponent USING    lo_stdesc
                                    lv_suffix
                           CHANGING et_comps[].





ENDFUNCTION.