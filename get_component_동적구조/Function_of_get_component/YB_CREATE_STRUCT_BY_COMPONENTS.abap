FUNCTION yb_create_struct_by_components.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IT_COMPS) TYPE  ABAP_COMPONENT_TAB
*"  EXPORTING
*"     REFERENCE(EO_STDESC) TYPE REF TO  CL_ABAP_STRUCTDESCR
*"----------------------------------------------------------------------

  DATA: lo_cldesc TYPE REF TO cl_abap_classdescr.
  DATA: lo_tydesc TYPE REF TO cl_abap_typedescr.

  IF it_comps[] IS INITIAL.
    EXIT.
  ENDIF.

  CALL METHOD cl_abap_typedescr=>describe_by_name
    EXPORTING
      p_name         = 'CL_ABAP_STRUCTDESCR'
    RECEIVING
      p_descr_ref    = lo_tydesc
    EXCEPTIONS
      type_not_found = 1.

  IF sy-subrc IS NOT INITIAL.
    EXIT.
  ENDIF.

  lo_cldesc ?= lo_tydesc.

*  TRY.
      CALL METHOD cl_abap_structdescr=>GET
        EXPORTING
          p_components = it_comps[]
        RECEIVING
          p_result     = eo_stdesc.
*    CATCH cx_sy_struct_creation.
*          TRY.
*      eo_stdesc = cl_abap_structdescr=>create( it_comps ).
*    CATCH cx_sy_struct_creation INTO DATA(lx_struct).
*  ENDTRY.
*      EXIT.
*  ENDTRY.

  IF sy-subrc IS NOT INITIAL.
    EXIT.
  ENDIF.




ENDFUNCTION.