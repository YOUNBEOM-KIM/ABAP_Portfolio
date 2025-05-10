*&---------------------------------------------------------------------*
*& Include          ZS4CONV_0200F01
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form initialization
*&---------------------------------------------------------------------*
FORM initialization .

  CLEAR : so_def.

  gsel_fkey-icon_id     = icon_settings.
  gsel_fkey-icon_text   = TEXT-i01.
  gsel_fkey-text        = TEXT-t01.
  sscrfields-functxt_01 = gsel_fkey.

  so_def[] = VALUE #(
                     ( sign = 'I' option = 'EQ' low = 'CTS1~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS2~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS3~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS4~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS5~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS6~~' )
                     ( sign = 'I' option = 'EQ' low = 'CTS7~~' )
                    ).

  PERFORM set_devclass CHANGING gt_devclass.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_devclass
*&---------------------------------------------------------------------*
FORM set_devclass  CHANGING pt_devclass TYPE gty_t_devclass.

  CLEAR : pt_devclass[].
  DEFINE __append.
    APPEND VALUE #( devclass  = &1
                    developer = &2 ) TO pt_devclass.
  END-OF-DEFINITION.

  __append :
  ,  'pak이름1'  '담당이름1'
  ,  'pak이름2'  '담당이름2'
  ,  'pak이름3'  '담당이름3'
  ,  'pak이름4'  '담당이름4'
  ,  'pak이름5'  '담당이름5'
  ,  'pak이름6'  '담당이름6'
  ,  'pak이름7'  '담당이름7'
  ,  'pak이름8'  '담당이름8'
  ,  'pak이름9'  '담당이름9'.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_fkey
*&---------------------------------------------------------------------*
FORM set_fkey  CHANGING pv_subrc.
  CASE sscrfuekds-ucomm.
    WHEN 'FC01'.
      PERFORM get_list CHANGING pv_subrc.
      PERFORM show_list.
  ENDCASE.
  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_list
*&---------------------------------------------------------------------*
FORM get_list  CHANGING p_pv_subrc.

  WITH +group AS ( SELECT log_date
                        , log_time
                     FROM zs4_req_cts
                 GROUP BY log_date, log_time
                 ORDER BY log_date DESCENDING, log_time DESCENDING
                    UP TO 5 ROWS
                 )
   SELECT d~log_name
        , d~log_date
        , d~log_time
        , d~log_system
        , d~log_trkorr
     FROM zs4_req_cts AS d
     JOIN +group      AS g
       ON g~log_date EQ d~log_date
      AND g~log_time EQ d~log_time
     INTO CORRESPONDING FIELDS OF TABLE @gt_dialg.

  MOVE-CORRESPONDING gt_dialog TO gt_gdialog.
  SORT gt_gdialog BY log_name DESCENDING
                     log_date DESCENDING
                     log_time DESCENDING.
  DELETE ADJACENT DUPLICATES FROM gt_gdialog COMPARING log_name
                                                       log_date
                                                       log_time.

  pv_subrc = sy-subrc.
  CHECK  pv_subrc IS INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form show_list
*&---------------------------------------------------------------------*
FORM show_list .
  CALL SCREEN 0200 STARTING AT 10 5 ENDING AT 62 10.
ENDFORM.
*&---------------------------------------------------------------------*
*& Module CREATE_DIALOG_0200 OUTPUT
*&---------------------------------------------------------------------*
MODULE create_dialog_0200 OUTPUT.

  IF go_dialog_con IS NOT BOUND.
    CREATE OBJECT go_dialog_con
      EXPORTING
        container_name = gc_dia1.
    CREATE OBJECT go_dia_grid
      EXPORTING
        i_parent = go_dialog_con.

    PERFORM get_dia_layo  CHANGING gs_layo.
    PERFORM get_fieldcat  USING gt_gdialog
                          CHANGING gt_fcat.
    PERFORM make_dia_fcat USING gt_fcat.
    PERFORM set_dia_event CHANGING go_dia_grid.

    go_dia_grid->set_table_for_first_display(
      EXPORTING
        is_layout = gs_layo
      CHANGING
        it_outtab = gt_gdialog
        it_fieldcatalog = gt_fcat ).

  ENDIF.

ENDMODULE.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
FORM get_data  CHANGING pv_subrc.

  DATA : lr_object TYPE RANGE OF e071-object.

  IF (
       ( so_ecc  IS INITIAL )
    OR ( so_hana IS INITIAL )
    OR ( so_def  IS INITIAL )
     ).
    MESSAGE s003 DISPLAY LIKE 'E'.
    LEAVE LIST-PROCESSING.
  ENDIF.

  CLEAR : gt_ecc, gt_hana.

* LOG
  PERFORM save_log     CHANGING pv_subrc.

* CHECKBOX
  PERFORM set_checkbox CHANGING lr_object.

* ECC
  PERFORM select_ecc   USING lr_object
                       CHANGING pv_subrc.
  " ECC에서 DEV 결정한 상태로 가정후 작성
* HANA " DEV 없이 조건절 데이터 가져오기
  PERFORM select_hana  USING lr_object
                       CHANGING pv_subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form save_log
*&---------------------------------------------------------------------*
FORM save_log CHANGING pv_subrc.

  TYPES : BEGIN OF lty_s_range
        , sign   TYPE c LENGTH 1
        , option TYPE c LENGTH 2
        , low    TYPE e071-trkorr
        , high   TYPE e071-trkorr
        , END OF lty_s_range.

  FIELD-SYMBOLS <fs_req> TYPE lty_s_range.
  DATA : ls_log TYPE zs4_req_cts.

  DATA(lv_status) = |{ COND string( WHEN pa_func = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|
                  & |{ COND string( WHEN pa_reps = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|
                  & |{ COND string( WHEN pa_wapp = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|
                  & |{ COND string( WHEN pa_meth = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|
                  & |{ COND string( WHEN pa_prog = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|
                  & |{ COND string( WHEN pa_fugr = 'X' THEN 'X' ELSE space ) WIDTH = 1 }|.

  DEFINE save_req_log.

    LOOP AT &1 ASSIGNING <fs_req>.
      ls_log-log_name = sy-uname.
      ls_log-log_date = sy-datum.
      ls_log-log_time = sy-uzeit.
      ls_log-log_system = &2.
      ls_log-log_trkorr = <fs_req>-low.
      ls_log-log_chk = lv_status.
      INSERT zs4_req_cts FROM ls_log.
    ENDLOOP.
    pv_subrc = sy-subrc.
    CHECK pv_subrc IS INITIAL.
  END-OF-DEFINITION.

  IF pa_log IS NOT INITIAL.
    save_req_log_so_ecc 'ECC'.
    save_req_log_so_hana 'HANA'.
    save_req_log_so_def 'DEF'.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_checkbox
*&---------------------------------------------------------------------*
FORM set_checkbox CHANGING pr_object TYPE STANDARD TABLE.

  DATA : lv_high TYPE c.
  CLEAR : pr_object.

  IF pa_func IS NOT INITIAL.
    PERFORM set_ranges USING 'FUNC' lv_high CHANGING pr_object.
  ENDIF.
  IF pa_reps IS NOT INITIAL.
    PERFORM set_ranges USING 'REPS' lv_high CHANGING pr_object.
  ENDIF.
  IF pa_wapp IS NOT INITIAL.
    PERFORM set_ranges USING 'WAPP' lv_high CHANGING pr_object.
  ENDIF.
  IF pa_meth IS NOT INITIAL.
    PERFORM set_ranges USING 'METH' lv_high CHANGING pr_object.
  ENDIF.
  IF pa_prog IS NOT INITIAL.
    PERFORM set_ranges USING 'PROG' lv_high CHANGING pr_object.
  ENDIF.
  IF pa_fugr IS NOT INITIAL.
    PERFORM set_ranges USING 'FUGR' lv_high CHANGING pr_object.
  ENDIF.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_ranges
*&---------------------------------------------------------------------*
FORM set_ranges USING    VALUE(pv_low)
                         VALUE(pv_high)
                CHANGING pr_object TYPE STANDARD TABLE.

  DATA : lr_str TYPE REF TO  data.

  FIELD-SYMBOLS : <wa>     TYPE any
                , <sign>   TYPE any
                , <option> TYPE any
                , <low>    TYPE any
                , <high>   TYPE any.

  CHECK pv_low IS NOT INITIAL.
  CREATE DATA lr_str LIKE LINE OF pr_object.
  ASSIGN lr_str->* TO <wa>.

  ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <wa> TO <sign>.
  ASSIGN COMPONENT 'OPTION' OF STRUCTURE <wa> TO <option>.
  ASSIGN COMPONENT 'LOW'    OF STRUCTURE <wa> TO <low>.
  ASSIGN COMPONENT 'HIGH'   OF STRUCTURE <wa> TO <high>.

  <sign>   = 'I'.
  <option> = COND #( WHEN pv_high IS INITIAL THEN 'EQ' ELSE 'BT' ).
  <low>    = pv_low.

  IF pv_high IS NOT INITIAL.
    <high> = pv_high.
  ENDIF.

  APPEND <wa> TO pr_object.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_ecc
*&---------------------------------------------------------------------*
FORM select_ecc USING    pr_object TYPE STANDARD TABLE
                CHANGING pv_subrc.

  DATA : lt_obj       TYPE gty_t_obj
       , lt_itab      TYPE gty_t_itab
       , lt_e_enlfdir TYPE gty_t_enlfdir.

  PERFORM  select_lt_obj          USING pr_object
                                  CHANGING lt_obj
                                       gv_subrc.

  PERFORM select_itab_by_lt_obj   USING lt_obj
                                  CHANGING lt_itab
                                         pv_subrc.

  PERFORM select_base_of_gt_ecc   USING pr_object
                                  CHANGING gt_ecc.

  PERFORM get_prog_dev_in_ecc     USING lt_itab
                                  CHANGING gt_ecc
                                      pv_subrc.

  PERFORM select_fugr_to_func     USING pr_object
                                  CHANGING lt_e_enlfdir.

  PERFORM add_gt_ecc_with_enlfdir USING lt_e_enlfdir
                                  CHANGING gt_ecc
                                           pv_subrc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_lt_object
*&---------------------------------------------------------------------*
FORM select_lt_object USING    pr_object TYPE STANDARD TABLE
                      CHANGING pt_obj    TYPE gty_t_obj
                               pv_subrc.

  DATA : lv_space TYPE string VALUE ` `.

  CLEAR : pt_obj[].

  SELECT e1~obj_name AS low " WAPP, METH의 OBJ_NAME을 LOW에 담고
    FROM e071 AS e1
    JOIN e070 AS e0
      ON e1~trkorr  EQ e0~trkorr
    WHERE e1~trkorr IN @so_ecc
      AND e1~object IN @pr_obect
      AND e1~pgmid  EQ 'LIMU'
      AND e1~object IN ( 'WAPP', 'METH' )
    INTO CORRESPONDING FIELDS OF TABLE @pt_obj.

  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

  LOOP AT pt_obj ASSIGNING FIELD-SYMBOLS(<ps_obj>). " 여기서 offset을 활용해서 앞에 텍스트만 활용
    <ps_obj>-obj_name = <ps_obj>-low.
    CONDENSE <ps_obj>-obj_name NO-GAPS.
    CHECK <ps_obj>low <> <ps_obj>-obj_name.
    FIND lv_space IN <ps_obj>-low MATCH OFFSET DATA(offset).
    CHECK offset > 0.
    <ps_obj>-obj_name = <ps_obj>-low(offset).
    CLEAR offset.
  ENDLOOP.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_itab_by_lt_obj
*&---------------------------------------------------------------------*
FORM select_itab_by_lt_obj USING    pt_obj  TYPE gty_t_obj
                           CHANGING pt_itab TYPE gty_t_itab
                                    pv_subrc.

  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

  WITH +base AS ( SELECT i~low AS obj_name
                       , td~devclass
                    FROM @pt_obj AS i
                    LEFT JOIN tadir AS td
                      ON i~obj_name EQ td~obj_name
                ),
       +func AS ( SELECT v~funcname
                       , t~devclass
                    FROM v_fdir     AS v
                    LEFT JOIN tadir AS t
                      ON v~area EQ t~obj_name
                 )
   SELECT DISTINCT b~obj_name
        , CASE WHEN b~devclass IS NOT INITIAL THEN b~devclass
                                              ELSE f~devclass
                END AS devclass
      FROM +base    AS b
      LEFT JOIN +func AS f
        ON b~obj_name EQ f~funcname
      INTO CORRESPONDING FIELDS OF TABLE @pt_itab.

  SORT pt_itab BY obj_name.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_base_of_gt_ecc
*&---------------------------------------------------------------------*
FORM select_base_of_gt_ecc USING    pr_object TYPE STANDARD TABLE
                           CHANGING gt_ecc    TYPE gty_t_version.

  WITH +e071 AS ( SELECT trkorr
                       , pgmid
                       , object
                       , obj_name
                       , left( obj_name, ( instr( obj_name, ' ' ) - 1 ) ) AS sub_objanme
                    FROM e071
                   WHERE trkorr IN @so_ecc
                     AND object IN @pr_object
                     AND ( ( pgmid EQ 'LIMU' AND object IN ( 'FUNC', 'REPS', 'WAPP' , 'METH' ) )
                        OR ( pgmid EQ 'R3TR' AND object = 'PROG' )
                         )
                  )
   SELECT e1~trkorr
        , et~as4text
        , e1~pgmid
        , e1~object
        , ef~devclass
        , e1~obj_name
        , e0~as4user
        , e0~as4date
        , e0~as4time
     FROM +e071 AS e1
     JOIN e070  AS e0
       ON e1~trkorr    EQ e0~trkorr
     LEFT OUTER JOIN e07t AS et
       ON en~funcname  EQ e1~obj_name
      AND e1~object    EQ 'FUNC'
     LEFT OUTER JOIN tadir AS ta1
       ON ta1~obj_name EQ en~area
      AND ta1~object   EQ 'FUGR'
    WHERE ( et~langu EQ '3'
       OR ( et~langu EQ 'E' AND NOT EXISTS ( SELECT 1
                                              FROM e07t AS et2
                                             WHERE et2~trkorr EQ e1~trkorr
                                               AND et2~langu EQ '3' )
           )
                           OR ( NOT EXISTS ( SELECT 1
                                               FROM e07t AS et3
                                              WHERE et3~trkorr EQ e1~trkorr
                                                AND et3~langu IN ('3', 'E')
                                            )
                               )
           )
        INTO CORRESPONDING FIELDS OF TABLE @pt_ecc.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_prog_dev_in_ecc
*&---------------------------------------------------------------------*
FORM get_prog_dev_in_ecc USING    pt_itab TYPE gty_t_itab
                         CHANGING pt_ecc  TYPE gty_t_version
                                  pv_subrc.

  SORT pt_ecc BY obj_name.

  DELETE ADJACENT DUPLICATES FROM pt_ecc COMPARING pgmid object obj_name.

  LOOP AT pt_itab INTO DATA(ps_itab).
    READ TABLE pt_ecc ASSIGNING FIELD-SYMBOLS(<ps_ecc>)
                                WITH KEY obj_name = ps_itab-obj_name BINARY SEARCH.
    IF sy-subrc EQ 0.
      CLEAR <ps_ecc>-devclass.
      <ps_ecc>-devclass = ps_itab-devclass.
    ENDIF.
  ENDLOOP.

  LOOP AT pt_ecc ASSIGNING FIELD-SYMBOLS(<ps_dev_ecc>).
    CHECK <ps_dev_ecc>-devclass IS INITIAL.
    DATA : progname TYPE trdir-name.
    progname = <ps_dev_ecc>-obj_name(40).
    CALL FUNCTION 'RS_PROGRAM_GET_DEVCLASS'
      EXPORTING
        progname = progname
      IMPORTING
        devclass = <ps_dev_ecc>-devclass.
  ENDLOOP.

  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_fugr_to_func
*&---------------------------------------------------------------------*
FORM select_fugr_to_func USING    pr_object    TYPE STANDARD TABLE
                         CHANGING pt_e_enlfdir TYPE gty_t_enlfdir.

  CLEAR : pt_e_enlfdir[].

  SELECT DISTINCT
         ta_fugr~devclass AS devclass
       , e0~trkorr        AS trkorr
       , et~as4text       AS as4text
       , 'R3TR'           AS pgmid
       , 'FUGR'           AS object
       , e1~obj_name      AS obj_name
       , e1~obj_name      AS funcgroup
       , e0~as4user       AS as4user
       , e0~as4date       AS as4date
       , e0~as4time       AS as4time
    FROM e071 AS e1
    JOIN e070 AS e0
      ON e0~trkorr        EQ e1~trkorr
    LEFT JOIN tadir AS ta~fugr
      ON ta_fugr~pgmid    EQ 'R3TR'
     AND ta_fugr~object   EQ 'FUGR'
     AND ta_fugr~obj_name EQ e1~obj_name
    LEFT JOIN e07t AS et
      ON et~trkorr EQ e0~trkorr
   WHERE e1~pgmid  EQ 'R3TR'
     AND e1~object EQ 'FUGR'
     AND e1~trkorr IN @so_ecc
     AND e1~boejct IN @pr_object

    UNION

    SELECT DISTINCT
         ta_fugr~devclass AS devclass
       , e0~trkorr        AS trkorr
       , et~as4text       AS as4text
       , 'LIMU'           AS pgmid
       , 'FUNC'           AS object
       , e1~obj_name      AS obj_name
       , e1~obj_name      AS funcgroup
       , e0~as4user       AS as4user
       , e0~as4date       AS as4date
       , e0~as4time       AS as4time
    FROM e071 AS e1
    JOIN e070 AS e0
      ON e0~trkorr EQ e1~trkorr
    JOIN enlfdir    AS ef
      ON ef~area EQ e1~obj_name
    LEFT JOIN tadir AS ta_fugr
      ON ta_fugr~pgmid    EQ 'R3TR'
     AND ta_fugr~object   EQ 'FUGR'
     AND ta_fugr~obj_name EQ ef~area
    LEFT JOIN e07t AS et
      ON et~trkorr EQ e0~trkorr
   WHERE e1~pgmid  EQ 'R3TR'
     AND e1~object EQ 'FUGR'
     AND e1~trkorr IN @so_ecc
    INTO CORRESPONDING FIELDS OF TABLE @pt_e_enlfidr.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_gt_ecc_with_enlfdir
*&---------------------------------------------------------------------*
FORM select_gt_ecc_with_enlfdir USING    pt_e_enlfdir TYPE gty_t_enlfdir
                                CHANGING pt_ecc       TYPE gty_t_version
                                         pv_subrc.

  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

  pt_ecc = VALUE #( BASE pt_ecc FOR ps_e_enf IN pt_e_enlfdir
                    ( CORRESPONDING #( ps_e_enf ) )
                  ).
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_hana
*&---------------------------------------------------------------------*
FORM select_hana USING    pr_object TYPE STANDARD TABLE
                 CHANGING pv_subrc.

  DATA : lt_h_enlfdir TYPE gty_t_enlfdir.

  PERFORM select_base_of_gt_hana   USING    pr_object
                                   CHANGING gt_hana
                                            pv__subrc.

  PERFORM select_limu_form_gt_hana USING    gt_hana
                                   CHANGING lt_h_enlfdir.

  PERFORM add_gt_hana_with_enlfdir USING    lt_h_enlfdir
                                   CHANGING gt_hana
                                            pv_subrc.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_hana
*&---------------------------------------------------------------------*
FORM select_base_of_gt_hana USING    pr_object TYPE STANDARD TABLE
                            CHANGING pt_hana   TYPE gty_t_version
                                     pv_subrc.

  SELECT e0~trkorr
       , e1~pgmid
       , e1~object
       , e1~obj_name
       , et~as4text
       , e0~as4user
       , e0~as4date
       , e0~as4time
    FROM e071 AS e1
    JOIN e070 AS e0
      ON e0~trkorr EQ e1~trkorr
    LEFT OUTER JOIN e07t AS et
      ON et~trkorr = e1~trkorr
   WHERE (   e1~trkorr IN @so_hana
          OR e1~trkorr IN @so_def
         )
     AND e1~object IN @pr_object
     AND (
           (     e1~pgmid  EQ 'LIMU'
             AND e1~object IN ( 'FUNC', 'REPS', 'WAPP', 'METH' )
           )
          OR
           (     e1~pgmid  EQ 'R3TR'
             AND e1~object IN ('PROG', 'FUGR' )
           )
          )
     AND ( et~langu EQ '3'
       OR ( et~langu EQ 'E' AND NOT EXISTS ( SELECT 1
                                              FROM e07t AS et2
                                             WHERE et2~trkorr EQ e1~trkorr
                                               AND et2~langu EQ '3' )
           )
                           OR ( NOT EXISTS ( SELECT 1
                                               FROM e07t AS et3
                                              WHERE et3~trkorr EQ e1~trkorr
                                                AND et3~langu IN ('3', 'E')
                                            )
                               )
           )
   ORDER BY e0~as4date  DESCENDING
           , e0~as4time DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @pt_hana

   pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form select_limu_from_gt_hana
*&---------------------------------------------------------------------*
FORM select_limu_from_gt_hana USING    pt_hana      TYPE gty_t_version
                              CHANGING pt_h_enlfdir TYPE gty_t_enlfdir.

  SELECT DISTINCT
         ha~trkorr
       , ha~as4text
       , 'LIMU'      AS pgmid
       , 'FUNC'      AS object
       , ha~as4user
       , ha~as4date
       , ha~as4time
       , ef~funcanme AS obj_name
    FROM @pt_hana  AS ha
    JOIN enlfdir   AS ef
      ON ha~obj_name EQ ef~area
    LEFT JOIN e071 AS e1
      ON e1~obj_name EQ ef~funcname
   ORDER BY ha~as4date DESCENDING
          , ha~as4time DESCENDING
    INTO CORRESPONDING FIELDS OF TABLE @pt_h_enlfdir.

ENDFORM.
*&---------------------------------------------------------------------*
*& Form add_gt_hana_with_enlfdir
*&---------------------------------------------------------------------*
FORM add_gt_hana_with_enlfdir USING    pt_h_enlfdir TYPE gty_t_enlfdir
                              CHANGING pt_hana      TYPE gty_t_version
                                       pv_subrc.

  pv_subrc = sy-subrc.
  CHECK pv_subrc IS INITIAL.

  pt_hana = VALUE #( BASE pt_hana FOR ps_h_enlf IN pt_h_enlfdir
                     ( CORRESPONDING #( ps_h_enlf )
                    ).

ENDFORM.
*&---------------------------------------------------------------------*
*& Form set_data
*&---------------------------------------------------------------------*
FORM set_data .

  DATA : ls_result TYPE gty_s_result.

  SORT gt_ecc BY pgmid object obj_name  as4date DESCENDING
                                        as4time DESCENDING.
  SORT gt_hana BY pgmid object obj_name as4date DESCENDING
                                        as4time DESCENDING.

  DELETE ADJACENT DUPLICATES FROM gt_ecc  COMPARING pgmid object obj_name.
  DELETE ADJACENT DUPLICATES FROM gt_hana COMPARING pgmid object obj_name.

  LOOP AT gt_ecc INTO DATA(ls_ecc).

    CLEAR : ls_result.

    READ TABLE gt_hana INTO DATA(ls_hana) WITH KEY pgmid    = ls_ecc-pgmid
                                                   object   = ls_ecc-obejct
                                                   obj_name = ls_ecc obj_name BINARY SEARCH.

    CHECK sy-subrc IS INITIAL.

    ls_result-devclass_ecc = ls_ecc-devclass.

    ls_result-pgmid_hana    = ls_hana-pgmid.
    ls_result-object_hana   = ls_hana-object.
    ls_result-obj_name_hana = ls_hana-obj_name.

    ls_result-trkorr_ecc    = ls_ecc-trkorr.
    ls_result-as4text_ecc   = ls_ecc-as4text.
    ls_result-as4user_ecc   = ls_ecc-as4user.
    ls_result-as4date_ecc   = ls_ecc-as4date.
    ls_result-as4time_ecc   = ls_ecc-as4time.

    ls_result-trkorr_hana   = ls_hana-trkorr.
    ls_result-as4text_hana  = ls_hana-as4text.
    ls_result-as4user_hana  = ls_hana-as4user.
    ls_result-as4date_hana  = ls_hana-as4date.
    ls_result-as4time_hana  = ls_hana-as4time.

    READ TABLE gt_devclass INTO DATA(ls_cls)
                           WITH TABLE KEY idx01
                           COMPONENTS devclass = ls_result-devclass_ecc.
    IF sy-subrc IS INITIAL.
      ls_result-deper = ls_cls-developer.
      CLEAR ls_cls.
    ENDIF.
    APPEND ls_result  TO gt_result.
  ENDLOOP.

  SORT gt_result BY deper devclass_ecc pgmid_hana object_hana obj_name_hana.

  DESCRIBE TABLE gt_result LINES gv_lines.
  MESSAGE s006 WITH gv_lines.

ENDFORM.