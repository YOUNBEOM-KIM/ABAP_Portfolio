FUNCTION yb_rtti.
*"----------------------------------------------------------------------
*"*"Local Interface:
*"  IMPORTING
*"     REFERENCE(IV_TABLE) TYPE  TABNAME
*"  EXPORTING
*"     REFERENCE(ET_DATA) TYPE REF TO  DATA
*"----------------------------------------------------------------------

  DATA: lo_struct TYPE REF TO cl_abap_structdescr,
        lo_tadesc TYPE REF TO cl_abap_tabledescr.

  lo_struct ?= cl_abap_typedescr=>describe_by_name( iv_table ).
  lo_tadesc  = cl_abap_tabledescr=>create( lo_struct ).

  CREATE DATA et_data TYPE HANDLE lo_tadesc.

ENDFUNCTION.