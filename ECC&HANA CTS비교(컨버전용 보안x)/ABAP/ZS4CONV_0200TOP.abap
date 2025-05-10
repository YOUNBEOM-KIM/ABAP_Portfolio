*&---------------------------------------------------------------------*
*&  Include           ZS4CONV_0200TOP
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* CONSTANTS
*--------------------------------------------------------------------*

CONSTANTS : gc_con1     TYPE char20 VALUE 'CCON1'
          , gc_dia1     TYPE char20 VALUE 'DIALOG1'
          , gc_s        TYPE c      VALUE 'S'
          , gc_x        TYPE c      VALUE 'X'.

CONSTANTS : gc_idx_func TYPE i VALUE 0
          , gc_idx_reps TYPE i VALUE 1
          , gc_idx_wapp TYPE i VALUE 2
          , gc_idx_meth TYPE i VALUE 3
          , gc_idx_prog TYPE i VALUE 4
          , gc_idx_fugr TYPE i VALUE 5.

*--------------------------------------------------------------------*
* TABLES
*--------------------------------------------------------------------*
TABLES : e070, e071, e07t, tadir, enlfdir, sscrfields.

*--------------------------------------------------------------------*
* TYPES
*--------------------------------------------------------------------*

TYPES : BEGIN OF gty_s_result_data
      , deper(10)     TYPE c
      , devclass_ecc  TYPE tadir-devclass
      , pgmid_hana    TYPE e071-pgmid
      , object_hana   TYPE e071-object
      , obj_name_hana TYPE e071-obj_name
      , trkorr_ecc    TYPE e071-trkorr
      , as4text_ecc   TYPE e07t-as4text
      , as4user_ecc   TYPE e070-as4user
      , as4date_ecc   TYPE e070-as4date
      , as4time_ecc   TYPE e070-as4time
      , trkorr_hana   TYPE e071-trkorr
      , as4text_hana  TYPE e07t-as4text
      , as4user_hana  TYPE e070-as4user
      , as4date_hana  TYPE e070-as4date
      , as4time_hana  TYPE e070-as4time
      , END OF gty_s_result_data.

TYPES : BEGIN OF gty_s_data
      , devclass      TYPE tadir-devclass
      , trkorr        TYPE e071-trkorr
      , as4text       TYPE e07t-as4text
      , pgmid         TYPE e071-pgmid
      , object        TYPE e071-object
      , obj_name      TYPE e071-obj_name
      , funcgroup     TYPE tadir-obj_name
      , as4user       TYPE e070-as4user
      , as4date       TYPE e070-as4date
      , as4time       TYPE e070-as4time
      , END OF gty_s_data.

TYPES : BEGIN OF gty_s_alv_foot
      , scol          TYPE lvc_t_scol
      , styl          TYPE lvc_t_styl
      , mode          TYPE c
      , END OF gty_s_alv_foot.

TYPES : BEGIN OF gty_s_obj
      , low           TYPE e071-obj_name
      , obj_name      TYPE tadir-obj_name
      , END OF gty_s_obj.

TYPES : BEGIN OF gty_s_itab
      , obj_name      TYPE e071-obj_name
      , devclass      LIKE tadir-devclass
      , deper(10)     TYPE c
      , END OF gty_s_itab.

TYPES : BEGIN OF gty_s_devclass
      , devclass      TYPE tadir-devclass
      , developer     TYPE pa0001-ename
      , END OF gty_s_devclass.

TYPES : BEGIN OF gty_s_dialog
      , log_name      TYPE zs4_req_cts-log_name
      , log_date      TYPE zs4_req_cts-log_date
      , log_time      TYPE zs4_req_cts-log_time
      , log_system    TYPE zs4_req_cts-log_system
      , log_tkrorr    TYPE zs4_req_cts-log_trkorr
      , END OF gty_s_dialog.

TYPES : BEGIN OF gty_s_dialog_group
      , log_name      TYPE zs4_req_cts-log_name
      , log_date      TYPE zs4_req_cts-log_date
      , log_time      TYPE zs4_req_cts-log_time
      , END OF gty_s_dialog_group.

TYPES : BEGIN OF  gty_s_chk
      , funcname      TYPE enlfdir-funcname
      , END OF gty_s_chk.

TYPES : BEGIN OF gty_s_sub
      , sub_object    TYPE e071-object
      , sub_obj_name  TYPE e071-obj_name
      , END OF gty_s_sub.

TYPES : BEGIN OF gty_s_dev
      , object        TYPE e071-object
      , obj_name      TYPE e071-obj_name
      , devclass      TYPE tadir-devclass
      , END OF gty_s_dev.

TYPES : BEGIN OF gty_s_result.
          INCLUDE TYPE gty_s_result_data.
TYPES : END OF gty_s_result.

TYPES : BEGIN OF gty_s_version.
          INCLUDE TYPE gty_s_data.
          INCLUDE TYPE gty_s_alv_foot.
TYPES : END OF gty_s_version.

TYPES : BEGIN OF gty_s_fugr.
          INCLUDE TYPE gty_s_data.
          INCLUDE TYPE gty_s_sub.
TYPES : END OF gty_s_fugr.

TYPES : BEGIN OF gty_s_enlfdir.
          INCLUDE TYPE gty_s_data.
          INCLUDE TYPE gty_s_chk.
          INCLUDE TYPE gty_s_sub.
TYPES : END OF gty_s_enlfdir.

TYPES : BEGIN OF gty_s_dev_chk.
          INCLUDE TYPE gty_s_dev.
          INCLUDE TYPE gty_s_chk.
TYPES : END OF gty_s_dev_chk.

TYPES : gty_t_version  TYPE TABLE OF gty_s_version  WITH EMPTY KEY.
TYPES : gty_t_obj      TYPE TABLE OF gty_s_obj      WITH EMPTY KEY.
TYPES : gty_t_itab     TYPE TABLE OF gty_s_itab.
TYPES : gty_t_enlfdir  TYPE TABLE OF gty_s_enlfdir  WITH EMPTY KEY.
TYPES : gty_t_devclass TYPE TABLE OF gty_s_devclass WITH EMPTY KEY.
TYPES : gty_t_fugr     TYPE TABLE OF gty_s_fugr.
TYPES : gty_t_result   TYPE TABLE OF gty_s_result   WITH EMPTY KEY.
TYPES : gty_t_dev_chk  TYPE TABLE OF gty_s_dev_chk.
TYPES : gty_t_dialog   TYPE TABLE OF gty_s_dialog.
TYPES : gty_t_gdialog  TYPE STANDARD TABLE OF gty_s_dialog_group
                       WITH NON-UNIQUE KEY log_name
                                           log_date
                                           log_time.

*--------------------------------------------------------------------*
* GLOBAL DATA
*--------------------------------------------------------------------*

DATA : gt_ecc        TYPE gty_t_version
     , gt_hana       TYPE gty_t_version
     , gt_result     TYPE gty_t_result.

DATA : gt_devclass   TYPE gty_t_devclass.

DATA : gv_subrc      TYPE sy-subrc
     , gv_ok_code    TYPE sy-ucomm
     , gv_ok_save    TYPE sy-ucomm
     , gv_lines      TYPE sy-tfill.

DATA : go_container  TYPE REF TO cl_gui_custom_container
     , go_grid       TYPE REF TO cl_gui_alv_grid.

DATA : gt_dialog     TYPE gty_t_dialog
     , gt_gdialog    TYPE gty_t_gdialog.

DATA : go_dialog_con TYPE REF TO cl_gui_custom_container
     , go_dia_grid   TYPE REF TO cl_gui_alv_grid.

DATA : gs_variant    TYPE disvariant
     , gs_layo       TYPE lvc_s_layo
     , gt_fcat       TYPE lvc_t_fcat
     , gt_sort       TYPE lvc_t_sort
     , gt_exclude    TYPE ui_functions.

DATA : gsel_fkey     TYPE smp_dyntxt.

CLASS : gcl_event_receiver DEFINITION DEFERRED.
DATA : gcl_event_receiver TYPE REF TO gcl_event_receiver.