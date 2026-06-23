*&                                                                     *
*&  Include           YRVG004_QAIS_EXECUTE_SEL
*&                                                                     *


SELECTION-SCREEN : BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
SELECT-OPTIONS  :
  s_sptag    FOR s922-sptag OBLIGATORY NO-EXTENSION,
  s_sptag1    FOR s922-sptag NO-DISPLAY NO-EXTENSION,
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm no : 4000007184
  s_sptagn   FOR s922-sptag NO-DISPLAY NO-EXTENSION,
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm no : 4000007184
  lv_sptag    FOR s922-sptag NO-DISPLAY NO-EXTENSION,
  s_vkbur  FOR s922-vkbur  OBLIGATORY NO INTERVALS  , "OBLIGATORY
  s_pkunag FOR s922-pkunag NO INTERVALS, "no-extension,
  s_kvgr2  FOR s922-kvgr2 NO INTERVALS NO-EXTENSION.
SELECTION-SCREEN SKIP.
*DATA: R_MONTH(1).

PARAMETERS: r_month  RADIOBUTTON GROUP rd1 USER-COMMAND r1 DEFAULT 'X',   "Added by Sachin Kaul(TCS) n Atul Singhal on 12.1.16
            r_quater RADIOBUTTON GROUP rd1 ," SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
*            r_quater NO-DISPLAY, " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
            r_annual RADIOBUTTON GROUP rd1,
            r_consis RADIOBUTTON GROUP rd1. "Added by Adarsh/Archana against charm: 4000006427, TR: DVRK9A12BV
SELECTION-SCREEN SKIP.
PARAMETERS: r_month1 NO-DISPLAY. "RADIOBUTTON GROUP rd1." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:           r_rlld   RADIOBUTTON GROUP rd1.
*SELECTION-SCREEN COMMENT 2(40) TEXT-005." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
*SELECTION-SCREEN COMMENT 3(40) TEXT-005. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
**SELECTION-SCREEN COMMENT 3(69) TEXT-012. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024

SELECTION-SCREEN END OF LINE.
**SOC by ujjjwal/priyanka on charm 4000002906 on 10-10-2020 to create new additional MQAIS link discount
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS:  r_rhd RADIOBUTTON GROUP rd1.
**SELECTION-SCREEN COMMENT 2(40) TEXT-004." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
**SELECTION-SCREEN COMMENT 3(60) TEXT-004. " SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000924
**SELECTION-SCREEN COMMENT 3(60) TEXT-004. " SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000971
SELECTION-SCREEN END OF LINE.
*****SOC by Abhinav/Archna/Vishal on Charm  4000006149, TR DVRK9A112V
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint RADIOBUTTON GROUP rd1. "RADIOBUTTON GROUP rd1." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
*SELECTION-SCREEN COMMENT 2(40) TEXT-007. " SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
**SELECTION-SCREEN COMMENT 3(40) TEXT-006.  " SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000971
*SELECTION-SCREEN COMMENT 3(40) TEXT-007. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
SELECTION-SCREEN END OF LINE.
*****EOC by Abhinav/Archna/Vishal on Charm  4000006149, TR DVRK9A112V
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: c_maint1 RADIOBUTTON GROUP rd1. "RADIOBUTTON GROUP rd1." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
*SELECTION-SCREEN COMMENT 3(40) TEXT-008. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
**SELECTION-SCREEN COMMENT 3(42) TEXT-008.  " SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000971
SELECTION-SCREEN END OF LINE.
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023

**EOC by ujjjwal/priyanka on charm 4000002906 on 10-10-2020 to create new additional MQAIS link SELECTION-SCREEN BEGIN OF LINE.
* **SOC by Vivek/priyanka on charm 2000000329 on 26.04.2021 to create new additional MQAIS Annual discount to mew customer
SELECTION-SCREEN BEGIN OF LINE.
PARAMETERS: r_newcus NO-DISPLAY.  "RADIOBUTTON GROUP rd1." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024

*SELECTION-SCREEN COMMENT 2(50) TEXT-006." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007222 Date: 12/10/2023
*SELECTION-SCREEN COMMENT 3(50) TEXT-006. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
SELECTION-SCREEN END OF LINE.

* **EOC by Vivek/priyanka on charm 2000000329 on 26.04.2021 to create new additional MQAIS Annual discount to mew customer
SELECTION-SCREEN BEGIN OF LINE." SOC by chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007299
PARAMETERS:  r_rpd NO-DISPLAY. "RADIOBUTTON GROUP rd1." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
*SELECTION-SCREEN COMMENT 3(50) TEXT-009. " SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007907 Date: 12/03/2024
SELECTION-SCREEN END OF LINE." SOC by chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007299
SELECTION-SCREEN SKIP.
SELECTION-SCREEN SKIP.
SELECTION-SCREEN COMMENT  1(79) TEXT-002.
PARAMETERS: c_chk TYPE flag NO-DISPLAY DEFAULT ' '.  "CHARM #4000000388

SELECTION-SCREEN : END OF BLOCK b1.

AT SELECTION-SCREEN OUTPUT.

** SOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
  LOOP AT SCREEN.
**    IF r_month = 'X' OR r_rhd = 'X' OR r_rlld = 'X'." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008707
    IF r_month = 'X' OR r_quater = 'X' or r_annual = 'X' or r_rhd = 'X' OR r_rlld = 'X' OR c_maint = 'X' OR c_maint1 = 'X'." SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008973
**      IF screen-name = 'R_ANNUAL' OR screen-name = 'R_CONSIS' OR screen-name = 'R_RHD' OR screen-name = 'R_RLLD'." ** SOC Commeneted by Chiluk
** SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000009468
"$$
"$$
"$$
"$$
"$$
"$$
*      IF screen-name = 'R_ANNUAL' OR
        if screen-name = 'R_CONSIS'." SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm: 2000001050
        screen-input = 0.
        MODIFY SCREEN.
      ENDIF.
** EOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000009468
    ENDIF.
  ENDLOOP.
** EOC Commeneted by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008188
  LOOP AT SCREEN.
    IF  screen-name = 'R_RPD' . "By Kunal/Priyanka on 25/10/2018 for RMS:1207(removed screen-name = 'R_MONTH1')
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
    IF  screen-name = 'R_NEWCUS' . "By Abhinav Disabling discount
      screen-input = 0.
      MODIFY SCREEN.
    ENDIF.
** SOC by chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007299
** SOC Commenetd by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008973
**    IF  screen-name = 'C_MAINT1'.
**      screen-input = 0.
**      MODIFY SCREEN.
**    ENDIF.
** EOC Commenetd by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008973
** EOC by chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007299
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000924
** SOC Commenetd by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008973
** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000971
"$$
"$$
"$$
"$$
    IF  screen-name = 'R_RHD' OR  screen-name = 'R_RLLD' OR screen-name = 'C_MAINT' OR screen-name = 'C_MAINT1'.
      screen-invisible = 1.
      MODIFY SCREEN.
    ENDIF.
"$$
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm : 2000000971
** EOC Commenetd by Chilukuri Tripura Reddy/Archna/Vishal Charm : 4000008973
** EOC by chilukuri Tripura Reddy/Archna/Vishal Charm : 4000007299
  ENDLOOP.

****SOC BY SURBHI AND PRIYANKA SINGH ON 06-05-2019 FOR DEFINING RANGE FOR PIPE AND ROTO

INITIALIZATION.

** SOC by Chilukuri Tripura Reddy/Archna/Vishal Charm: 2000001050
  SELECT * FROM yrva_prs_grades INTO TABLE @DATA(lt_yrva_prs_grades).
  SELECT SINGLE * FROM yrva_cis_mstr INTO ls_yrva_cis_mstr.
  LOOP AT lt_yrva_prs_grades INTO DATA(ls_yrva_prs_grades).
    range_r-sign = 'I'. " I OR E
    range_r-option = 'EQ'.
    range_s-sign = 'I'. " I OR E
    range_s-option = 'EQ'.
    range_p-sign = 'I'. " I OR E
    range_p-option = 'EQ'.
    IF ls_yrva_prs_grades-yy_indicator = 'R'.
      range_r-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_r.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'S'.
      range_s-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_s.
    ELSEIF ls_yrva_prs_grades-yy_indicator = 'P'.
      range_p-low = ls_yrva_prs_grades-yy_grade.
      APPEND range_p.
    ENDIF.
    CLEAR: ls_yrva_prs_grades.
  ENDLOOP.
** EOC by Chilukuri Tripura Reddy/Archna/Vishal Charm: 2000001050
** SOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm: 2000001050
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '09' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '11' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '14' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '20' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '34' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '38' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '43' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '44' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '46' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '56' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = '60' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = 'G7' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = 'I1' .
**  APPEND range_p .
**
**  range_p-sign = 'I'. " I OR E
**  range_p-option = 'EQ'.
**  range_p-low = 'K2' .
**  APPEND range_p .
**
****************FOR ROTO*****************
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = '14' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = '21' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = '27' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = '29' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = 'G7' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = 'H9' .
**  APPEND range_r .
**
**  range_r-sign = 'I'. " I OR E
**  range_r-option = 'EQ'.
**  range_r-low = 'K6' .
**  APPEND range_r .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '09' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '11' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '14' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '20' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '34' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '38' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '43' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '44' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '46' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '56' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '60' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'G7' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'I1' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'K2' .
**  APPEND range_s .
**
****************FOR ROTO*****************
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '14' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '21' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '27' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = '29' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'G7' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'H9' .
**  APPEND range_s .
**
**  range_s-sign = 'I'. " I OR E
**  range_s-option = 'EQ'.
**  range_s-low = 'K6' .
**  APPEND range_s .
** EOC Commented by Chilukuri Tripura Reddy/Archna/Vishal Charm: 2000001
****EOC BY SURBHI AND PRIYANKA SINGH ON 06-05-2019 FOR DEFINING RANGE FOR PIPE AND ROTO
