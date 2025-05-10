*----------------------------------------------------------------------*
***INCLUDE LYB_FCF01.
*----------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form get_subcomponent
*&---------------------------------------------------------------------*
FORM get_subcomponent  USING    po_stdesc TYPE REF TO cl_abap_structdescr
                                pv_suffix TYPE string
                       CHANGING pt_comps  TYPE abap_component_tab.

  DATA: lt_comps  LIKE pt_comps WITH HEADER LINE.
  DATA: lo_stdesc TYPE REF TO cl_abap_structdescr.
  DATA: lv_suffix TYPE string.

  CHECK po_stdesc IS NOT INITIAL.

  lt_comps[] = po_stdesc->get_components( ).

  LOOP AT lt_comps.
    IF pv_suffix IS NOT INITIAL.
      lt_comps-name = lt_comps-name && pv_suffix.
    ENDIF.
    CASE lt_comps-as_include.
      WHEN 'X'.
        PERFORM get_next_stdesc  USING    lt_comps-type
                                          lo_stdesc.
        PERFORM get_subcomponent USING    lo_stdesc
                                          lt_comps-suffix
                                 CHANGING pt_comps.

      WHEN OTHERS.
        APPEND lt_comps TO pt_comps.
    ENDCASE.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_next_stdesc
*&---------------------------------------------------------------------*
FORM get_next_stdesc  USING    po_c_type   TYPE REF TO cl_abap_datadescr
                      CHANGING po_stdesc TYPE REF TO cl_abap_structdescr.

  DATA : lv_class  TYPE        abap_abstypename.
  DATA : lo_tadesc TYPE REF TO cl_abap_tabledescr.

  lv_class = cl_abap_classdescr=>get_class_name( po_c_type ).
  REPLACE FIRST OCCURRENCE OF '\CLASS=' IN lv_class WITH space.
  CONDENSE lv_class.
  CASE lv_class.
    WHEN 'CL_ABAP_STRUCTDESCR'.
      po_stdesc ?= po_c_type.
    WHEN OTHERS.
      EXIT.
  ENDCASE.
ENDFORM.