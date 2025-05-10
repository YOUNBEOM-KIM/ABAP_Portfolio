*&---------------------------------------------------------------------*
*& Include          ZS4CONV_0200CLS
*&---------------------------------------------------------------------*
CLASS gcl_event_receiver DEFINITION.
  PUBLIC SECTION.
    METHODS : handle_toolbar
      FOR EVENT toolbar OF cl_gui_alv_grid
      IMPORTING e_object
                e_interactive.

    METHODS : handle_user_command
      FOR EVENT user_command OF cl_gui_alv_grid
      IMPORTING e_ucomm.

    METHODS : handle_double_click
      FOR EVENT double_click OF cl_gui_alv_grid
      IMPORTING e_row
                e_column
                es_row_no.
ENDCLASS.

CLASS gcl_event_receiver IMPLEMENTATION.

  METHOD handle_toolbar.
    PERFORM event_toolbar     USING e_object
                                    e_interactive.
  ENDMETHOD.

  METHOD handle_user_command.
    PERFORM event_user_command USING e_ucomm.
  ENDMETHOD.

  METHOD handle_double_click.
    PERFORM event_double_click USING e_row
                                     e_colum
                                     e_row_no.
  ENDMETHOD.

ENDCLASS.