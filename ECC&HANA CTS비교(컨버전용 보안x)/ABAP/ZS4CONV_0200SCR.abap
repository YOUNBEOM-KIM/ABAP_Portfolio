*&---------------------------------------------------------------------*
*& Include          ZS4CONV_0200SCR
*&---------------------------------------------------------------------*

*--------------------------------------------------------------------*
* ECC Request  - ECC CTS
* HANA Request - HANA CTS
* DEF Request - HANA OF Default CTS ( 프로젝트 시작점 부터 대상 )
* - DEF의 리스트 LOG에 기록 된거 or 엑셀 참조
*--------------------------------------------------------------------*
SELECTION-SCREEN : FUNCTION KEY 1.

SELECTION-SCREEN BEGIN OF BLOCK b01 WITH FRAME TITLE TEXT-t01.
  SELECT-OPTIONS : so_ecc  FOR e070-trkorr
                 , so_hana FOR e070-trkorr
                 , so_def  FOR e070-trkorr.
SELECTION-SCREEN END OF BLOCK b01.

SELECTION-SCREEN BEGIN OF BLOCK b02 WITH FRAME TITLE TEXT-t02.
SELECTION-SCREEN BEGIN OF BLOCK b03 WITH FRAME TITLE TEXT-t03.
  SELECTION-SCREEN  BEGIN OF LINE.
    PARAMETERS : pa_func AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_func
      MODIF ID obj.
    PARAMETERS : pa_reps AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_reps
      MODIF ID obj.
    PARAMETERS : pa_wapp AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_wapp
      MODIF ID obj.
    PARAMETERS : pa_meth AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_meth
      MODIF ID obj.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b03.

SELECTION-SCREEN BEGIN OF BLOCK b04 WITH FRAME TITLE text-t04.
  SELECTION-SCREEN BEGIN OF LINE.
    PARAMETERS : pa_prog AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_prog
      MODIF ID obj.
    PARAMETERS : pa_fugr AS CHECKBOX DEFAULT 'X'
                         MODIF ID  obj.
    SELECTION-SCREEN COMMENT (15) FOR FIELD pa_fugr
      MODIF ID obj.
  SELECTION-SCREEN END OF LINE.
SELECTION-SCREEN END OF BLOCK b04.

SELECTION-SCREEN END OF BLOCK b02.