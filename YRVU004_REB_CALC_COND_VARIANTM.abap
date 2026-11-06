*&---------------------------------------------------------------------*
*& Report  YRVU004_REB_CALC_COND_VARIANT                               *
*&                                                                     *
*&---------------------------------------------------------------------*
*&                                                                     *
*&                                                                     *
*&---------------------------------------------------------------------*
*---------------------------------------------------------------------*
*&--------------------------------------------------------------------*&
*&  Program Name          : YRVU004_REB_CALC_COND_VARIANTM            *&
*&  Transaction Code      : YRVU016                                   *&
*&  Module                : SD                                        *&
*&  Package               : YV01                                      *&
*&  Developed By          : Ujjwal Jain                               *&
*&  Version               : 1.0                                       *&
*&  Type                  : Executable                                *&
*&  Functional Consultant : Priyanka Singh/Anurag Mehta               *&
*&  Received Date         : 22.11.2019                                *&
*&  Fun. Specs Recived    : No                                        *&
*&  Transport Req         : DVRK9A0HRQ                                *&
*&--------------------------------------------------------------------*&
*& Change History:This Program is Copy of YRVU004_REB_CALC_COND_VARIANT
*&        with Additional changes suggested by priyanka/anurag        *&
*                   yrvu016 is for variant run program
*---------------------------------------------------------------------*
* PROGRAM DESCRIPTION: Executing the  rebate calculations program YRVU001_REB_CALC with variant
* selection based on the selection input                              *
*       DEVELOPER: sachin singh
*       CREATION DATE: 2011-01-12
*


REPORT  yrvu004_reb_calc_cond_variantm.
*TABLES
TABLES: s922.
*data declaration
** SOC by ujjwal/Priyanka on charm#400000157 on 18-09-2019 to Change the program name
*CONSTANTS: c_rep_name TYPE vari_reprt VALUE 'YRVU001_REB_CALC_M1'.
*CONSTANTS: c_rep_name TYPE vari_reprt VALUE 'YRVU001_REB_CALC_M2'.
CONSTANTS: c_rep_name TYPE vari_reprt VALUE 'YRVU001_REB_CALC_M2_N1'. " call new (6-level) calc program
** EOC by ujjwal/Priyanka on charm#400000157 on 18-09-2019 to Change the program name
DATA: w_flag(1).


DATA: yv_chk TYPE char4.
DATA: yv_tchk TYPE char4.

*INTERNAL TABLES
DATA: it_varid TYPE TABLE OF varid WITH HEADER LINE,
      BEGIN OF it_variant OCCURS 0,
        variant TYPE variant,
      END OF it_variant ,
      it_variant_values TYPE TABLE OF rsparams  WITH HEADER LINE.
*initilization
INITIALIZATION.
  PERFORM get_list_of_vairant.
*selection screen
  SELECTION-SCREEN : BEGIN OF BLOCK b1. "WITH FRAME TITLE TEXT-001." SOC commented by Chilukuri Tripura Reddy/Archna/Visahl Charm : 4000008133
*  SELECTION-SCREEN SKIP." SOC commented by Chilukuri Tripura Reddy/Archna/Visahl Charm : 4000008133
  SELECT-OPTIONS  :
    s_sptag  FOR s922-sptag OBLIGATORY NO-EXTENSION,
    s_vkbur  FOR s922-vkbur,
    s_kvgr2  FOR s922-kvgr2,
**** SOC BY ABHINAV/PRIYANKA CHARM:w4r5tg453r5tr43245rt65r4
    s_pkunag FOR s922-pkunag NO INTERVALS,
    s_pkuna1 FOR s922-pkunag NO-DISPLAY.

  PARAMETERS : p_vari TYPE variant OBLIGATORY.

  SELECTION-SCREEN SKIP.
  SELECTION-SCREEN : END OF BLOCK b1.
*AT SELECTION-SCREEN ON VALUE-REQUEST FOR S_VKBUR-LOW .
AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_vari .
  PERFORM get_variant.
*start of selection
START-OF-SELECTION.

END-OF-SELECTION.

    PERFORM get_list_of_vairant.
    IF sy-subrc = 0.
      READ TABLE it_varid WITH KEY variant =  p_vari.

****
      DATA: it_tab    TYPE TABLE OF rsparams,
            s_pkunag1 TYPE TABLE OF s922-pkunag,
            yv_temp   TYPE char10.

      CALL FUNCTION 'RS_VARIANT_CONTENTS'
        EXPORTING
          report               = 'YRVU001_REB_CALC_M2_N1'
          variant              = p_vari
        TABLES
          valutab              = it_tab[]
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.
******    .
      IF s_pkunag-low IS NOT INITIAL.

********
        LOOP AT it_tab INTO DATA(yt_tab) WHERE selname EQ 'S_PKUNAG'.
          CONCATENATE '00000' yt_tab-low INTO yv_temp.
          LOOP AT s_pkunag." INTO DATA(yv_emp).
            IF yv_temp EQ '00000'.
              yv_chk = 'X'.
            ELSEIF yv_temp = s_pkunag-low.
              s_pkuna1-option = 'EQ'.
              s_pkuna1-sign =  'I'.
              s_pkuna1-low = yv_temp.
              yv_chk = 'X'.
              APPEND s_pkuna1.
            ELSEIF yv_temp NE s_pkunag-low.
              yv_tchk = 'X'.
            ENDIF.
          ENDLOOP.
        ENDLOOP.

********

** SOC by ujjwal/Priyanka on charm#400000157 on 18-09-2019 to Change the program name
**      SUBMIT yrvu001_reb_calc_m1 USING SELECTION-SET p_vari  WITH s_vkbur IN s_vkbur
**                                                          WITH s_kvgr2 IN s_kvgr2
**                                                          WITH s_pkunag IN s_pkunag
**                                                          USING SELECTION-SETS OF PROGRAM  c_rep_name   AND RETURN.
        IF yv_tchk IS NOT INITIAL AND yv_chk NE 'X'.

          CALL FUNCTION 'POPUP_TO_INFORM'
            EXPORTING
              titel = 'Information'
              txt1  = 'No data has been selected for given Selection Criteria'
              txt2  = ''
*             TXT3  = ' '
*             TXT4  = ' '
            .
*        MESSAGE 'No data has been selected for given Selection Criteria' TYPE'E' DISPLAY LIKE 'I'.
          CALL TRANSACTION 'YRVU016'.
        ELSEIF s_pkuna1[] IS NOT INITIAL.
          SUBMIT yrvu001_reb_calc_m2_n1 USING SELECTION-SET p_vari  WITH s_vkbur IN s_vkbur
                                                              WITH s_kvgr2 IN s_kvgr2
                                                              WITH s_pkunag IN s_pkuna1
                                                              USING SELECTION-SETS OF PROGRAM  c_rep_name   AND RETURN.
        ELSEIF yv_chk IS NOT INITIAL.
          SUBMIT yrvu001_reb_calc_m2_n1 USING SELECTION-SET p_vari  WITH s_vkbur IN s_vkbur
                                                      WITH s_kvgr2 IN s_kvgr2
                                                      WITH s_pkunag IN s_pkunag
                                                   USING SELECTION-SETS OF PROGRAM  c_rep_name   AND RETURN.
        ENDIF.
      ELSE.
*      **      SUBMIT yrvu001_reb_calc_m1 USING SELECTION-SET p_vari  WITH s_vkbur IN s_vkbur
***                                                          WITH s_kvgr2 IN s_kvgr2
***                                                          WITH s_pkunag IN s_pkunag
***                                                          USING SELECTION-SETS OF PROGRAM  c_rep_name   AND RETURN.
        SUBMIT yrvu001_reb_calc_m2_n1 USING SELECTION-SET p_vari  WITH s_vkbur IN s_vkbur
                                                            WITH s_kvgr2 IN s_kvgr2
*                                                        with S_PKUNAG in S_PKUNAG
                                                            USING SELECTION-SETS OF PROGRAM  c_rep_name   AND RETURN.
** EOC by ujjwal/Priyanka on charm#400000157 on 18-09-2019 to Change the program name
      ENDIF.
    ELSE.
      MESSAGE TEXT-002 TYPE 'I'.
    ENDIF.


*&---------------------------------------------------------------------*
*&      Form  GET_LIST_OF_VAIRANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_list_of_vairant .
  SELECT * FROM varid INTO TABLE it_varid WHERE report EQ c_rep_name AND edat GE '20160101'.

ENDFORM.                    " GET_LIST_OF_VAIRANT
*&---------------------------------------------------------------------*
*&      Form  GET_VARIANT
*&---------------------------------------------------------------------*
*       text
*----------------------------------------------------------------------*
*  -->  p1        text
*  <--  p2        text
*----------------------------------------------------------------------*
FORM get_variant .
  CLEAR: it_variant,it_variant_values.
  REFRESH: it_variant,it_variant_values.

  DATA:l_vkbur   TYPE dynfieldvalue,
       l_d_from  TYPE dynfieldvalue,
       l_d_to    TYPE dynfieldvalue,
       l_fr_date TYPE d,
       l_to_date TYPE d,
       l_date1   TYPE d,
       l_date2   TYPE d.
**GET THE S_VKBUR'S VALUE
*  CALL FUNCTION 'FM_FYC_DYNPRO_VALUE_READ'
*    EXPORTING
*      I_REPID            = sy-repid
*      I_DYNNR            = sy-dynnr
*      I_FIELDNAME        = 'S_VKBUR-LOW'
*   IMPORTING
*     E_FIELDVALUE       = l_VKBUR            .

*GET THE S_SPTAG-LOW'S VALUE
  CALL FUNCTION 'FM_FYC_DYNPRO_VALUE_READ'
    EXPORTING
      i_repid      = sy-repid
      i_dynnr      = sy-dynnr
      i_fieldname  = 'S_SPTAG-LOW'
    IMPORTING
      e_fieldvalue = l_d_from.
*GET THE S_SPTAG-HIGH'S VALUE
  CALL FUNCTION 'FM_FYC_DYNPRO_VALUE_READ'
    EXPORTING
      i_repid      = sy-repid
      i_dynnr      = sy-dynnr
      i_fieldname  = 'S_SPTAG-HIGH'
    IMPORTING
      e_fieldvalue = l_d_to.

  CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
    EXPORTING
      datum = l_d_to
      dtype = 'DATS'
    IMPORTING
*     ERROR =
      idate = l_to_date
*     MESSG =
*     MSGLN =
    .
  CALL FUNCTION 'CONVERT_DATE_TO_INTERN_FORMAT'
    EXPORTING
      datum = l_d_from
      dtype = 'DATS'
    IMPORTING
*     ERROR =
      idate = l_fr_date
*     MESSG =
*     MSGLN =
    .


  IF  l_fr_date IS NOT INITIAL.
*    DELETE it_varid WHERE edat LT '20160101'.
    DELETE it_varid WHERE edat LT l_fr_date ."LT '20160101'.
*    DELETE it_varid WHERE edat GT l_to_date ."LT '20160101'.
    LOOP AT it_varid .
      CLEAR w_flag.
      CALL FUNCTION 'RS_VARIANT_VALUES_TECH_DATA'
        EXPORTING
          report               = c_rep_name
          variant              = it_varid-variant
*         SEL_TEXT             = ' '
*         MOVE_OR_WRITE        = 'W'
*         SORTED               = ' '
*         EXECUTE_DIRECT       =
*      IMPORTING
*         TECHN_DATA           =
        TABLES
          variant_values       = it_variant_values
*         VARIANT_TEXT         =
        EXCEPTIONS
          variant_non_existent = 1
          variant_obsolete     = 2
          OTHERS               = 3.
      IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
      ENDIF.
*     break-point.
      LOOP AT it_variant_values WHERE  selname EQ 'S_SPTAG'.
        CLEAR w_flag.
*       IF IT_VARIANT_VALUES-SELNAME EQ 'S_VKBUR'.
*         IF IT_VARIANT_VALUES-LOW NE L_VKBUR and IT_VARIANT_VALUES-LOW is NOT INITIAL.
*            W_FLAG  = 'X' .
*           EXIT.
*         ENDIF.
*       ENDIF.
        IF it_variant_values-selname EQ 'S_SPTAG'.
          CONCATENATE it_variant_values-low+6(4) it_variant_values-low+3(2) it_variant_values-low+0(2) INTO l_date1.
          CONCATENATE it_variant_values-high+6(4) it_variant_values-high+3(2) it_variant_values-high+0(2) INTO l_date2.
          IF  l_date1 LT l_fr_date OR l_date2 GT l_to_date.
            w_flag = 'X' .
            EXIT.
          ENDIF.
        ENDIF.
      ENDLOOP.
      IF w_flag NE 'X'.
        it_variant-variant = it_varid-variant.
        APPEND it_variant.
        CLEAR it_variant.
      ENDIF.

    ENDLOOP.
*   BREAK-POINT.


    CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
      EXPORTING
*       DDIC_STRUCTURE         = ' '
        retfield     = 'VARIANT'
*       PVALKEY      = ' '
        dynpprog     = sy-repid
        dynpnr       = sy-dynnr
        dynprofield  = 'P_VARI'
*       STEPL        = 0
        window_title = 'SELECT VARIANT'
*       VALUE        = ' '
        value_org    = 'S'
*       MULTIPLE_CHOICE        = ' '
*       DISPLAY      = ' '
*       CALLBACK_PROGRAM       = ' '
*       CALLBACK_FORM          = ' '
*       MARK_TAB     =
*    IMPORTING
*       USER_RESET   =
      TABLES
        value_tab    = it_variant[]
*       FIELD_TAB    =
*       RETURN_TAB   =
*       DYNPFLD_MAPPING        =
*    EXCEPTIONS
*       PARAMETER_ERROR        = 1
*       NO_VALUES_FOUND        = 2
*       OTHERS       = 3
      .
    IF sy-subrc <> 0.
* MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*         WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
    ENDIF.

  ENDIF.
ENDFORM.                    " GET_VARIANT