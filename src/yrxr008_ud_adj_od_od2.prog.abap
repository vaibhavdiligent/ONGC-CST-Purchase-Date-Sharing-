*&                                                                      *
*& YRXR008_UD_ADJ_OD_OD2
*&
*&                                                                      *
*&
*&
*&                                                                      *

REPORT yrxr008_ud_adj_od_od2.
TYPE-POOLS: slis.

DATA:it_fcat TYPE slis_t_fieldcat_alv,
wa_fcat TYPE slis_fieldcat_alv.
* -> BEGIN OF CHANGES BY Arnav ON 30May25
TYPES: BEGIN OF ty_msg,
msg TYPE string,
type TYPE string,
END OF ty_msg.
* -> END OF CHANGES BY Arnav ON 30May25
DATA: it_msg TYPE STANDARD TABLE OF ty_msg,
wa_msg TYPE ty_msg.

DATA: ls_oijnomh       TYPE oijnomh,
ls_oijnomi       TYPE oijnomi,
ls_oijnomi_temp TYPE oijnomi,
ls_kna1          TYPE kna1,
lt_oijrra        TYPE TABLE OF oijrra,
days             TYPE i,
not_week         TYPE boolean,
not_month        TYPE boolean,
not_fnight       TYPE boolean,
week             TYPE boolean,
month            TYPE boolean,
fnight           TYPE boolean,
msgv             TYPE msgv1.
DATA: BEGIN OF ls_vbeln ,
vbeln TYPE vbeln,
knumv TYPE vbak-knumv,
END OF ls_vbeln,
lt_vbeln      LIKE TABLE OF ls_vbeln,
lt_oijnomr    TYPE TABLE OF oijnomr,
lt_oijnomh    TYPE TABLE OF oijnomh,
lt_oijnomi    TYPE TABLE OF oijnomi,
lt_oijnomi_zu TYPE TABLE OF oijnomi,
BEGIN OF ls_yro_nom_param,
nomtk TYPE oij_nomtk,
nomit TYPE oij_item,
ancv TYPE yyncv,
agcv TYPE yygcv,
END OF ls_yro_nom_param,
lt_yro_nom_param     LIKE TABLE OF ls_yro_nom_param,
ls_yro_nom_param_db TYPE yro_nom_param,
lt_yro_nom_param_db TYPE TABLE OF yro_nom_param,
cum_aod              TYPE menge13,
cum_ud               TYPE menge13,
wt_ncv               TYPE yyncv,
wt_gcv               TYPE yygcv,
weight               TYPE menge13,
wt_ncv1              TYPE yyncv,
wt_gcv1              TYPE yygcv,
wt_ncv_zu_s          TYPE yyncv,
wt_gcv_zu_s          TYPE yygcv,
wt_ncv_so            TYPE yyncv,
wt_gcv_so            TYPE yygcv,
weight1              TYPE menge13,
g_energy             TYPE yygcv,
n_energy             TYPE yyncv,
gv_nomtk             TYPE oijnomi-nomtk,
gv_nomit             TYPE oijnomi-nomit,
BEGIN OF ls_cum_aod,
locid    TYPE oijnomi-locid,
cum_aod TYPE oijnomi-menge,
END OF ls_cum_aod,
lt_cum_aod LIKE TABLE OF ls_cum_aod.
DATA: bdcdata_wa      TYPE bdcdata,
bdcdata         TYPE TABLE OF bdcdata WITH HEADER LINE,
lt_oijnomi_adj TYPE TABLE OF oijnomi,
ls_oijnomi_adj TYPE oijnomi,
lv_werk         TYPE oijts-werk.
*BOC CHARM 4000002027 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRATIBHA DANGWAL DT 11.05.2020*
DATA : gv_vbdat TYPE vbdat_veda .
DATA : gv_vndat TYPE vndat_veda .
DATA : gt_yrvt_cont_dcq TYPE TABLE OF yrvt_cont_dcq .
DATA : lt_vbeln2       LIKE TABLE OF ls_vbeln.
*EOC CHARM 4000002027 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRATIBHA DANGWAL DT 11.05.2020*
*SELECTION-SCREEN BEGIN OF BLOCK B1.

DATA: messtab                LIKE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE,
wa_msg1               TYPE bdcmsgcoll,
w_ticket_number1(50),
w_ticket_nr(50),
wt_gcv_a(15)          TYPE c.

TYPES:BEGIN OF ty_message,
id      TYPE symsgid,
number TYPE symsgno,
message TYPE bapi_msg,
END OF ty_message.

DATA: it_message TYPE TABLE OF ty_message,
wa_message TYPE ty_message.




SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE TEXT-001.
PARAMETERS : p_kunnr TYPE kna1-kunnr OBLIGATORY,
p_trans TYPE oijts-tsyst   OBLIGATORY,
p_locid TYPE oijnomi-locid OBLIGATORY.
SELECT-OPTIONS: so_date FOR sy-datum       NO-EXTENSION OBLIGATORY.
SELECTION-SCREEN END OF BLOCK b1.

AT SELECTION-SCREEN ON VALUE-REQUEST FOR p_locid."-LOW.
TYPES: BEGIN OF ty_oijts,
locid TYPE oij_locid,
END OF ty_oijts.

DATA i_oijts TYPE TABLE OF ty_oijts.

SELECT locid
*    TSNAM
*    TSYST
FROM oijloc
INTO TABLE i_oijts
WHERE loctyp LIKE 'YRD%'.

DATA: return_tab TYPE TABLE OF ddshretval,
wa_tab     LIKE LINE OF return_tab.

CALL FUNCTION 'F4IF_INT_TABLE_VALUE_REQUEST'
EXPORTING
retfield        = 'LOCID'
dynprofield     = 'P_LOCID'
window_title    = 'SEARCH HELP'
value_org       = 'S'
TABLES
value_tab       = i_oijts
return_tab      = return_tab
EXCEPTIONS
parameter_error = 1
no_values_found = 2.

READ TABLE return_tab INTO wa_tab INDEX '1'.
IF sy-subrc = 0.
p_locid = wa_tab-fieldval.
ENDIF.

AT SELECTION-SCREEN.

SELECT * FROM oijrra INTO TABLE lt_oijrra WHERE kunnr = p_kunnr AND locid = p_locid AND rtype = 'TSW005'.
IF sy-subrc <> 0.
MESSAGE e001(00) WITH 'THE CUSTOMER' p_kunnr 'IS NOT MAINTAINED AT GIVEN LOCATION AS A RECEIVER'. "#EC *
ENDIF.
IF so_date-high IS INITIAL.
MESSAGE e001(00) WITH 'ENTER DATE RANGE.'.
ENDIF.

START-OF-SELECTION.
DATA l_flag TYPE flag.
DATA l_date TYPE sy-datum.
CALL FUNCTION 'RP_LAST_DAY_OF_MONTHS'
EXPORTING
day_in             = so_date-low
IMPORTING
last_day_of_month = l_date
EXCEPTIONS
day_in_no_date     = 1
OTHERS             = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

IF so_date-low < 15.
IF so_date-low+6(2) <> '01' AND so_date-low+6(2) <> '15'.
l_flag = 'X'.
ENDIF.
ELSE.
IF so_date-high+6(2) <> 15 AND so_date-high+6(2) <> l_date+6(2)..
l_flag = 'X'.
*else.
ENDIF.
ENDIF.

**SOC BY RAVI/SHREYOSI ON 08.03.2026 Matix Billing Changes TR:DVRK9A1Q97
SELECT SINGLE setname, valfrom, valto
FROM setleaf
INTO @DATA(wa_setleaf)
WHERE setname EQ 'YGMS_CORE_TEAM'
AND   valfrom LE @sy-uname
AND   valto   GE @sy-uname
AND   setclass EQ '0000'.

SELECT SINGLE * FROM yrxa_gms_constnt INTO @DATA(lv_const) WHERE y_param = 'A7' AND y_low = 1.
IF lv_const IS NOT INITIAL OR wa_setleaf-valfrom EQ sy-uname.
CLEAR:l_flag.
ENDIF.
CLEAR:wa_setleaf.
**EOC BY RAVI/SHREYOSI ON 08.03.2026 Matix Billing Changes TR:DVRK9A1Q97

IF l_flag = 'X'.
**SOC BY RAVI/PRATIBHA ON 20.09.2025 GST BILLING CHANGES TR:DVRK9A1MHQ
MESSAGE: 'Billing date range does not cover a single and complete fortnight' TYPE 'E' DISPLAY LIKE 'E'.
**EOC BY RAVI/PRATIBHA ON 20.09.2025 GST BILLING CHANGES TR:DVRK9A1MHQ
DATA l_ans TYPE c.
CALL FUNCTION 'POPUP_TO_CONFIRM'
EXPORTING
*        TITLEBAR        = ' '
*        DIAGNOSE_OBJECT            = ' '
text_question = TEXT-010
text_button_1 = 'Yes'(011)
*        ICON_BUTTON_1 = ' '
text_button_2 = 'No'(012)
*        ICON_BUTTON_2 = ' '
*        DEFAULT_BUTTON = '1'
*        DISPLAY_CANCEL_BUTTON      = 'X'
*        USERDEFINED_F1_HELP        = ' '
*        START_COLUMN    = 25
*        START_ROW       = 6
*        POPUP_TYPE      =
*        IV_QUICKINFO_BUTTON_1      = ' '
*        IV_QUICKINFO_BUTTON_2      = ' '
IMPORTING
answer          = l_ans
* TABLES
*        PARAMETER       =
EXCEPTIONS
text_not_found = 1
OTHERS          = 2.
IF sy-subrc <> 0.
* Implement suitable error handling here
ENDIF.

IF l_ans <> '1'.
EXIT.
ENDIF.
ENDIF.


PERFORM check_existing_ticket.
** BEFOR PROCEDD CHECK AUTHORIZATION
IF p_trans IS NOT INITIAL.
SELECT SINGLE werk FROM oijts INTO lv_werk WHERE tsyst = p_trans.
IF sy-subrc = 0.
AUTHORITY-CHECK OBJECT 'YGMS_PLANT'
ID 'WERKS' FIELD lv_werk.
IF sy-subrc <> 0.
MESSAGE e001(00) WITH ' YOU DO NOT HAVE AUTHORIZATION F' 'OR TRA NSPORT SYSTEM' p_trans.
ENDIF.
ENDIF.
ENDIF.

" GET ALL GSA DOCUMENTS FOR THE CUSTOMER
SELECT vbeln knumv FROM vbak INTO TABLE lt_vbeln WHERE kunnr = p_kunnr AND vbtyp = 'G' AND auart <> 'ZGT1' .
*BOC CHARM 4000002027 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRATIBHA DANGWAL DT 11.05.2020*
*              AND ( ( GUEBG LE SO_DATE-LOW AND GUEEN GE SO_DATE-HIGH ) OR
*                    ( GUEBG LE SO_DATE-LOW AND GUEEN GE SO_DATE-LOW ) OR
*                    ( GUEBG GE SO_DATE-LOW AND GUEEN LE SO_DATE-HIGH ) OR
*                    ( GUEBG LE SO_DATE-HIGH AND GUEEN GE SO_DATE-HIGH )
*                  ) .
* IF SY-SUBRC <> 0.
LOOP AT lt_vbeln INTO ls_vbeln .
CALL FUNCTION 'YRV_CONTRACT_DETAILS'
EXPORTING
vbeln         = ls_vbeln-vbeln
IMPORTING
vbegdat       = gv_vbdat
venddat       = gv_vndat
TABLES
dcq_details = gt_yrvt_cont_dcq.
*    IF SO_DATE-LOW GE GV_VBDAT AND SO_DATE-HIGH LE GV_VNDAT
IF gv_vbdat LE so_date-high AND gv_vndat GE so_date-low.
APPEND ls_vbeln TO lt_vbeln2 .
ENDIF .
CLEAR : ls_vbeln , gv_vbdat , gv_vndat .
ENDLOOP .
REFRESH lt_vbeln .
lt_vbeln[] = lt_vbeln2[] .
IF lt_vbeln IS INITIAL .
*EOC CHARM 4000002027 TECHNICAL VIKRAM BAJAJ FUNCTIONAL PRATIBHA DANGWAL DT 11.05.2020*
MESSAGE e001(00) WITH ' NO GSA CONTRACTS F' 'OUND FOR THE GIVEN SELE CTION'. "#EC *
ELSE.
** GET ALL NOMINTATION ITEMS WHERE THIS DOCUMENT IS USED
SELECT * FROM oijnomi INTO TABLE lt_oijnomi FOR ALL ENTRIES IN lt_vbeln WHERE docnr = lt_vbeln-vbeln
AND idate IN so_date AND delind <> 'X'.
IF sy-subrc <> 0.
MESSAGE e018(oij_if).
ELSE.
SELECT * FROM oijnomh INTO TABLE lt_oijnomh FOR ALL ENTRIES IN lt_oijnomi WHERE
nomtk = lt_oijnomi-nomtk AND tsyst = p_trans AND
nomtyp = 'GISA'.
IF sy-subrc <> 0.
MESSAGE e018(oij_if).
ELSE.
** GET ALL GISA NOMINATIONS WITH ABOVE SELECTED LINE ITEMS
**      IF THERE IS ANY UNAUTHORIZED LINE ITEM, DONT CONSIDER THAT NOMIN ATION FOR OD1 CALCULATION
SELECT * FROM oijnomi INTO TABLE lt_oijnomi FOR ALL ENTRIES IN lt_oijnomh WHERE nomtk = lt_oijnomh-nomtk AND delind <> 'X'.
SORT lt_oijnomi BY idate DESCENDING.
READ TABLE lt_oijnomi INTO ls_oijnomi INDEX 1.
gv_nomtk = ls_oijnomi-nomtk.
SELECT * FROM oijnomi INTO TABLE lt_oijnomi_adj WHERE nomtk = gv _nomtk.
SORT lt_oijnomi_adj BY nomit DESCENDING.
READ TABLE lt_oijnomi_adj INTO ls_oijnomi_adj INDEX 1.
gv_nomit = ls_oijnomi_adj-nomit.
* -> BEGIN OF CHANGES BY Arnav ON 30May25
"       >"*SOC CHARM ID :4000009388 TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSHI DT:11.03.2025"
IF NOT lt_oijnomi IS INITIAL.
SELECT nomtk nomit ancv agcv FROM yro_nom_param INTO TABLE lt_yro_nom_param FOR ALL ENTRIES IN lt_oijnomi
WHERE nomtk = lt_oijnomi-nomtk AND
nomit = lt_oijnomi-nomit.
SORT lt_yro_nom_param BY nomtk ASCENDING nomit ASCENDING.
LOOP AT lt_yro_nom_param INTO DATA(ls_temp).
IF ls_temp-agcv = 0 OR ls_temp-ancv = 0.
DATA(ls_temp1) = VALUE #( lt_oijnomi[ nomtk = ls_temp-nomtk nomit = ls_temp-nomit ] OPTIONAL ) .
*       wa_msg-msg =    GCV and NCV are found to be incorrect for    && ls _temp1-idate &&    and    && ls_temp1-locid &&   &   && ls_temp1-partnr &&Re-run allocation    .
wa_msg-msg =    Re-RUN Allocation FOR   && ls_temp1-locid &&   /   && ls_temp1-partnr &&     AND   && ls_temp1-idate+6(2) && '.' && ls_temp1-idate+4(2) && '.' && ls_temp1-idate+0(4) &&
AS GCV AND NCV are found TO be incorrectly updated    .
wa_msg-type = 'Error'.
APPEND wa_msg TO it_msg.
CLEAR: wa_msg,ls_temp1.
ENDIF.
CLEAR ls_temp.
ENDLOOP.
IF it_msg[] IS NOT INITIAL.
PERFORM: error_alv.
ENDIF.
ENDIF.
"       >"*SOC CHARM ID :4000009388 TECHICAL : RAVINDER SINGH F UNCTIONAL : SHREYOSHI DT:11.03.2025"
* -> END OF CHANGES BY Arnav ON 29May25

IF NOT lt_oijnomi IS INITIAL.
SELECT nomtk nomit ancv agcv FROM yro_nom_param INTO TABLE lt_yro_nom_param FOR ALL ENTRIES IN lt_oijnomi
WHERE nomtk = lt_oijnomi-nomtk AND
nomit = lt_oijnomi-nomit.

IF sy-subrc <> 0.
MESSAGE w001(00) WITH 'NCV AND GCV WERE NOT FOUND FOR GIVEN SELECTION'. "#EC *
ENDIF.
ENDIF.

*************CHANGE IN OD1 BY VAIBHAV 29.08.2011
*DATA L_DEL TYPE FLAG.
*
*LOOP AT LT_OIJNOMI INTO LS_OIJNOMI WHERE IDATE = SO_DATE-HIGH AND
* ( SITYP = 'ZA' OR SITYP = 'ZJ' OR SITYP = 'ZU' ).
* IF LS_OIJNOMI-SITYP = 'ZJ' AND LS_OIJNOMI-DOCIND = 'G'.
*L_DEL = 'X'.
*    ELSEIF LS_OIJNOMI-SITYP = 'ZA' AND LS_OIJNOMI-DOCIND = 'G'.
*L_DEL = 'X'.
*
*      ELSEIF LS_OIJNOMI-SITYP = 'ZU' AND LS_OIJNOMI-DOCIND = 'S'.
*L_DEL = 'X'.
* ENDIF.
*
* ENDLOOP.








LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZU'.
IF ls_oijnomi-docind = 'X' AND ls_oijnomi-bloind NE 'X' AND ls _oijnomi-delind NE 'X'.
APPEND ls_oijnomi TO lt_oijnomi_zu.
ENDIF.
CLEAR ls_oijnomi.
ENDLOOP.

IF lt_oijnomi IS INITIAL.
MESSAGE s001(00) WITH ' NO DATA FOUND FOR U' 'NDER DRAWL AND O VER DRAWL ADJUSTMENT'.
ENDIF.
** BASED ON ZA LINE ITEMS GET THE CUMULATIVE AUTHORIZED OVER DRAWL
SORT lt_oijnomi BY locid.
LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZA' AND docind = 'X'." OR SITYP = 'ZU') AND DOCIND = 'X'.
ls_cum_aod-locid = ls_oijnomi-locid.
ls_cum_aod-cum_aod = ls_oijnomi-menge.
COLLECT ls_cum_aod INTO lt_cum_aod.
cum_aod =    cum_aod + ls_oijnomi-menge.
READ TABLE lt_yro_nom_param INTO ls_yro_nom_param WITH KEY nomtk = ls_oijnomi-nomtk nomit = ls_oijnomi-nomit.
IF sy-subrc = 0.
wt_ncv = wt_ncv + ls_oijnomi-menge * ls_yro_nom_param-ancv.
wt_gcv = wt_gcv + ls_oijnomi-menge * ls_yro_nom_param-agcv.
weight = weight + ls_oijnomi-menge.
ELSE.
READ TABLE lt_yro_nom_param INTO ls_yro_nom_param WITH KEY nomtk = ls_oijnomi-nomtk.
IF sy-subrc = 0.
wt_ncv = wt_ncv + ls_oijnomi-menge * ls_yro_nom_param-ancv .
wt_gcv = wt_gcv + ls_oijnomi-menge * ls_yro_nom_param-agcv .
weight = weight + ls_oijnomi-menge.
ENDIF.
ENDIF.
ENDLOOP.
IF weight IS NOT INITIAL.
wt_ncv = wt_ncv / weight.
wt_gcv = wt_gcv / weight.
ENDIF.
CLEAR ls_oijnomi.

** BASED ON ZU LINE ITEMS GET THE CUMULATIVE UNAUTHORIZED OVER DRAWL
SORT lt_oijnomi BY locid.
LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZU' AND docind = 'X'.
READ TABLE lt_yro_nom_param INTO ls_yro_nom_param WITH KEY nomtk = ls_oijnomi-nomtk nomit = ls_oijnomi-nomit.
IF sy-subrc = 0.
wt_ncv1 = wt_ncv1 + ls_oijnomi-menge * ls_yro_nom_param-ancv .
wt_gcv1 = wt_gcv1 + ls_oijnomi-menge * ls_yro_nom_param-agcv .
weight1 = weight1 + ls_oijnomi-menge.
ELSE.
READ TABLE lt_yro_nom_param INTO ls_yro_nom_param WITH KEY nomtk = ls_oijnomi-nomtk.
IF sy-subrc = 0.
wt_ncv1 = wt_ncv1 + ls_oijnomi-menge * ls_yro_nom_param-ancv.
wt_gcv1 = wt_gcv1 + ls_oijnomi-menge * ls_yro_nom_param-agcv.
weight1 = weight1 + ls_oijnomi-menge.
ENDIF.
ENDIF.
ENDLOOP.
IF weight1 IS NOT INITIAL.
wt_ncv1 = wt_ncv1 / weight1.
wt_gcv1 = wt_gcv1 / weight1.
ENDIF.
CLEAR ls_oijnomi.

IF cum_aod IS INITIAL.
MESSAGE s001(00) WITH ' NO AUTHORIZED OVER DRAWL FOUN' 'D FOR THE GIVEN PERIOD'. "#EC *
ENDIF.

IF cum_aod IS INITIAL AND lt_oijnomi_zu[] IS INITIAL.
MESSAGE s001(00) WITH ' NO AUTHORIZED AND UNAUTHORIZED OVER DR AWL FOUN' 'D FOR THE GIVEN PERIOD'. "#EC *
LEAVE TO TRANSACTION 'YRXU007'..
ENDIF.
CLEAR ls_oijnomi.
ENDIF.
ENDIF.
ENDIF.
* WT_NCV = WT_NCV / WEIGHT.
* WT_GCV = WT_GCV / WEIGHT.
SORT lt_oijnomi BY idate.

DATA: BEGIN OF ls_material_adjust ,
vbeln TYPE vbak-vbeln,
uom   TYPE meins,
locid TYPE oijnomi-locid,
matnr TYPE mara-matnr,
aod   TYPE yyoij_aod_qty,
ud     TYPE yyoij_un_qty, "UNDER DRAWLS IN SM3
cud    TYPE yyoij_un_qty, "UNDER DRAWL IN CONTRACTUAL UOM
csm3 TYPE menge_d, "IN SM3 @GCV, NCV
END OF ls_material_adjust,
lt_material_adjust LIKE TABLE OF ls_material_adjust,
lt_vbap             TYPE TABLE OF vbap,
ls_vbap             TYPE vbap,
lt_oijnomi_vbap     TYPE TABLE OF oijnomi.
IF NOT lt_oijnomi IS INITIAL.
APPEND LINES OF lt_oijnomi TO lt_oijnomi_vbap.
DELETE lt_oijnomi_vbap WHERE docind <> 'G'.
SELECT * FROM vbap INTO TABLE lt_vbap FOR ALL ENTRIES IN lt_oijnomi_vbap
WHERE vbeln = lt_oijnomi_vbap-docnr AND posnr = lt_oijnomi _vbap-docitm.
ENDIF.
SORT lt_oijnomi BY locid matnr_i.
LOOP AT lt_oijnomi INTO ls_oijnomi WHERE docind = 'G'.
ls_material_adjust-matnr = ls_oijnomi-s_matnr_i.
ls_material_adjust-locid = ls_oijnomi-locid.
READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_oijnomi-docnr posnr = ls_oijnomi-docitm.
ls_material_adjust-uom     = ls_vbap-zieme.
ls_material_adjust-vbeln = ls_oijnomi-docnr.
ls_material_adjust-aod     = ls_oijnomi-yyoij_od_qty.
ls_material_adjust-ud      = ls_oijnomi-yyoij_un_qty.
*APPEND LS_MATERIAL_ADJUST TO LT_MATERIAL_ADJUST.
**     CONVERT UD INTO CORRESPONDING CONTRACTUAL UOM USING THAT DAYS GCV AND NCV
READ TABLE lt_yro_nom_param INTO ls_yro_nom_param WITH KEY nomtk = ls_oijnomi-nomtk." NOMIT = LS_OIJNOMI-NOMIT.
IF sy-subrc = 0.
DATA lv_adqnt TYPE msego2-adqnt.
lv_adqnt = ls_material_adjust-ud.
CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
EXPORTING
i_trqty = lv_adqnt "I_TRQTY
i_truom = 'SM3' "I_TRUOM
i_tguom = ls_vbap-zieme "I_TGUOM
lv_gcv = ls_yro_nom_param-agcv "LV_GCV
lv_ncv = ls_yro_nom_param-ancv "LV_NCV
CHANGING
c_tgqty = lv_adqnt.
ls_material_adjust-cud = lv_adqnt.

CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
EXPORTING
i_trqty = lv_adqnt "I_TRQTY
i_truom = ls_vbap-zieme "I_TRUOM
i_tguom = 'SM3' "I_TGUOM
lv_gcv = wt_gcv
lv_ncv = wt_ncv "LV_NCV
CHANGING
c_tgqty = lv_adqnt.
ls_material_adjust-csm3 = lv_adqnt.
ENDIF.
COLLECT ls_material_adjust INTO lt_material_adjust.
CLEAR ls_material_adjust.
ENDLOOP.
CLEAR ls_oijnomi.
SORT lt_material_adjust BY matnr." IDATE.

DATA: ls_material_adjust_calc LIKE ls_material_adjust,
BEGIN OF ls_mat_ud,
vbeln TYPE vbak-vbeln,
uom   TYPE meins,
locid TYPE oijnomi-locid,
matnr TYPE mara-matnr,
cud   TYPE oijnomi-yyoij_un_qty, "IN CONTRACTUAL UOM
ud    TYPE oijnomi-yyoij_un_qty,                  " IN SM3
csm3 TYPE menge_d , " SM3 IN GCV,NCV
END OF ls_mat_ud,
lt_mat_ud LIKE TABLE OF ls_mat_ud.

DATA: BEGIN OF ls_adjusted,
vbeln TYPE vbak-vbeln,
locid TYPE oijnomi-locid,
matnr TYPE mara-matnr,
ud    TYPE yyoij_un_qty,
END OF ls_adjusted,

lt_adjusted        LIKE TABLE OF ls_adjusted,
ls_oij_el_ticket_i TYPE oij_el_ticket_i,
lt_oij_el_ticket_i TYPE TABLE OF oij_el_ticket_i,
lv_tabix           TYPE sy-tabix,
lv_caod_cuom       TYPE msego2-adqnt.

LOOP AT lt_material_adjust INTO ls_material_adjust WHERE ud <> 0.
MOVE-CORRESPONDING ls_material_adjust TO ls_mat_ud.
COLLECT ls_mat_ud INTO lt_mat_ud.
ENDLOOP.

*** SORT LT_MAT_UD BASED ON MATERIAL PRIORITY FROM SCHEMA
** CALL FM TO GET THIS PRIORITY
** GET TRANSACTION TYPE FROM OIJ_EL_CP_LAYT
DATA:BEGIN OF ls_trantype,
vbeln    TYPE vbak-vbeln,
trantyp TYPE oij_trantyp,
priority TYPE oij_el_cp_layt-priority,
servtype TYPE oij_el_cp_layt-servtype,
END OF ls_trantype,
lt_trantypes LIKE TABLE OF ls_trantype,
lv_schema    TYPE oij_schema,
ls_rank      TYPE yrxa_rank,
lt_ranks     TYPE yrxa_rank_tab,
lt_reltype   TYPE TABLE OF oijrelcriteria,
ls_reltype   TYPE oijrelcriteria,
lv_subrc     TYPE sy-subrc,
ls_msg       TYPE sy-msgv1.
SELECT vbeln trantyp priority servtype
FROM oij_el_cp_layt INTO TABLE lt_trantypes
FOR ALL ENTRIES IN lt_vbap WHERE vbeln = lt_vbap-vbeln.
* SELECT SINGLE SCHEMA_BUSCONF FROM OIJLOC INTO LV_SCHEMA WHERE LOCID = P_LOCID."SO_LOCID-LOW.
*CHANGE IN LOGIC BY VAIBHAV ON 03.11.2014
* SELECT SINGLE YY_SCHEMA INTO LV_SCHEMA
* FROM YRGA_GMS_TBS
* WHERE
*LOCATION_ID = P_LOCID "IS_AGG_NOM_ITEM-LOCID
* AND
**         BUSINESS_PROCESS = 'BUSINESS CONFIRMATION'
**         AND
* START_DATE <= SO_DATE-HIGH
* AND
* END_DATE >= SO_DATE-HIGH."IS_AGG_NOM_ITEM-IDATE.
* IF SY-SUBRC <> 0.
*     SELECT SINGLE SCHEMA_BUSCONF FROM OIJLOC INTO LV_SCHEMA WHERE LOCID = P_LOCID."SO_LOCID-LOW.
* ENDIF.

*CHANGE IN LOGIC BY VAIBHAV ON 03.11.2014
*
SELECT SINGLE schema_busconf
FROM oijloc INTO lv_schema        WHERE locid = p_locid."SO_LOCI D-LOW.

SELECT * FROM oijrelcriteria INTO TABLE lt_reltype FOR ALL ENTRIES INlt_trantypes WHERE transaction_type = lt_trantypes-trantyp.

LOOP AT lt_trantypes INTO ls_trantype.
LOOP AT lt_reltype INTO ls_reltype WHERE transaction_type = ls_trantype-trantyp
AND capacity         = ls_trantype-priority
AND service_type     = ls_trantype-servtype.
ls_rank-relvancy_type = ls_reltype-relvancy_type."LS_TRANTYPE-TRAN TYP.
ls_rank-schema_id = lv_schema.
APPEND ls_rank TO lt_ranks.
ENDLOOP.
ENDLOOP.
SORT lt_ranks BY relvancy_type schema_id.
DELETE ADJACENT DUPLICATES FROM lt_ranks.

CALL FUNCTION 'YRXF005_GET_RANK_1'
CHANGING
yrxa_rank_tab = lt_ranks
subrc         = lv_subrc.
IF lv_subrc IS INITIAL.
ELSE.
READ TABLE lt_ranks INTO ls_rank INDEX 1.
CONCATENATE 'CHECK SCHEMA   ' ls_rank-schema_id   ' RELEVANCY   ' ls_rank-relvancy_type INTO ls_msg RESPECTING BLANKS.
MESSAGE e001(00) WITH ls_msg.
ENDIF.
SORT lt_ranks BY yy_rank.

DATA : BEGIN OF ls_mat_ud_pro,
vbeln TYPE vbak-vbeln,
uom   TYPE meins,
locid TYPE oijnomi-locid,
matnr TYPE mara-matnr,
cud   TYPE oijnomi-yyoij_un_qty,
ud    TYPE oijnomi-yyoij_un_qty,
csm3 TYPE menge_d,
rank TYPE oij_level,
END OF ls_mat_ud_pro,
lt_mat_ud_pro LIKE TABLE OF ls_mat_ud_pro,
lv_cum_ud      TYPE menge_d,
ls_mat_ud_pro1 LIKE ls_mat_ud_pro,
ls_mat_ud1     LIKE ls_mat_ud,
lv_aod         TYPE menge_d,
lv_cum_zj      TYPE menge_d,
lv_bal_aod     TYPE menge_d.

LOOP AT lt_ranks INTO ls_rank.
READ TABLE lt_reltype INTO ls_reltype WITH KEY relvancy_type = ls_rank-relvancy_type.
**** SOC BY SACHIN TO MAKE
LOOP AT lt_trantypes INTO ls_trantype WHERE trantyp = ls_reltype-transaction_type.
*    READ TABLE LT_TRANTYPES INTO LS_TRANTYPE WITH KEY TRANTYP = LS_RELTY PE-TRANSACTION_TYPE. "LS_RANK-RELVANCY_TYPE.
*     IF SY-SUBRC = 0.
READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_trantype-vbeln .
IF sy-subrc = 0.
LOOP AT lt_mat_ud INTO ls_mat_ud WHERE matnr = ls_vbap-matnr AND vbeln = ls_trantype-vbeln.
ls_mat_ud_pro-rank = ls_rank-yy_rank.
MOVE-CORRESPONDING ls_mat_ud TO ls_mat_ud_pro.
APPEND ls_mat_ud_pro TO lt_mat_ud_pro.
ENDLOOP.
ENDIF.
*     ENDIF.
ENDLOOP.
ENDLOOP.


LOOP AT lt_ranks INTO ls_rank.
CLEAR: lv_cum_zj.
READ TABLE lt_reltype INTO ls_reltype WITH KEY relvancy_type = ls_rank-relvancy_type.
**** SOC BY SACHIN TO MAKE
LOOP AT lt_trantypes INTO ls_trantype WHERE trantyp = ls_reltype-transaction_type.
*     READ TABLE LT_TRANTYPES INTO LS_TRANTYPE WITH KEY TRANTYP = LS_RELT YPE-TRANSACTION_TYPE.
*     IF SY-SUBRC = 0.
READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_trantype-vbeln .
IF sy-subrc = 0.
LOOP AT lt_mat_ud INTO ls_mat_ud WHERE matnr = ls_vbap-matnr AND vbeln = ls_vbap-vbeln.
IF ls_mat_ud-csm3 <> 0.
READ TABLE lt_cum_aod INTO ls_cum_aod WITH KEY locid = ls_mat_ud-locid.
IF sy-subrc = 0.
IF ls_cum_aod-cum_aod LE 0.
EXIT.
ENDIF.
lv_tabix = sy-tabix.
** CHECK IF ANY OTHER MATERIAL WITH SAME RANK IS EXISTING.
** GET UNDERDRAWL OF MATERIALS WITH THIS RANK
lv_cum_ud = ls_mat_ud-csm3.
LOOP AT lt_mat_ud_pro INTO ls_mat_ud_pro1 WHERE rank = ls_rank-yy_rank AND vbeln <> ls_vbap-vbeln. "MATNR <> LS_VBAP-MATNR.
lv_cum_ud = ls_mat_ud_pro1-csm3 + lv_cum_ud.
ENDLOOP.
IF sy-subrc = 0.
** CONVERT CUM. AOD INTO UOM OF THE MATERIAL
**       CSM3
lv_aod = ls_cum_aod-cum_aod.
IF ls_cum_aod-cum_aod < lv_cum_ud.
**        ONLY NOW PRORATA IS REQUIRED
LOOP AT lt_mat_ud_pro INTO ls_mat_ud_pro1 WHERE rank =ls_rank-yy_rank.
READ TABLE lt_mat_ud INTO ls_mat_ud1 WITH KEY vbeln = ls_mat_ud_pro1-vbeln. " MATNR = LS_MAT_UD_PRO1-MATNR.
IF sy-subrc = 0.
ls_mat_ud1-csm3 = ( ls_mat_ud1-csm3 * ls_cum_aod-cum_aod ) / lv_cum_ud.
****        SOC LOGIC TO REMOVE ROUNDING ERROR OF .001 CHARM 4000005003
lv_cum_zj = lv_cum_zj + ls_mat_ud1-csm3 .
lv_bal_aod = ls_cum_aod-cum_aod - lv_cum_zj .
IF lv_bal_aod LT '.01' AND lv_bal_aod GE '.001'.
ls_mat_ud1-csm3 = ls_mat_ud1-csm3 + lv_bal_aod .
ELSEIF lv_bal_aod LE '-.0001' AND lv_bal_aod GT '-.01'.
ls_mat_ud1-csm3 = ls_mat_ud1-csm3 + lv_bal_aod .
ENDIF.
****      EOC LOGIC TO REMOVE ROUNDING ERROR OF .001 CHARM 4000005003

MODIFY lt_mat_ud FROM ls_mat_ud1 INDEX sy-tabix TRANSPORTING csm3 .
ENDIF.
ENDLOOP.
ls_cum_aod-cum_aod = lv_aod.
READ TABLE lt_mat_ud INTO ls_mat_ud WITH KEY matnr = ls_vbap-matnr vbeln = ls_vbap-vbeln.

ENDIF.
DELETE lt_mat_ud_pro WHERE rank = ls_rank-yy_rank.
ENDIF. " MATERIAL RANK WITH SAME RANK FOUND

ls_adjusted-matnr = ls_mat_ud-matnr.
ls_adjusted-vbeln = ls_mat_ud-vbeln.
ls_adjusted-locid = ls_mat_ud-locid.
** CHECK IF MAT CONTRACTUAL UOM NOT SM3. THEN CONVERT AOD TO SM3 ADJUST IT IN CONT. UOM
** UPDATE AOD ACCORDINGLY BACK TO SM3
ls_cum_aod-cum_aod = ls_cum_aod-cum_aod - ls_mat_ud-csm3.
MODIFY lt_cum_aod FROM ls_cum_aod INDEX lv_tabix.

IF ls_cum_aod-cum_aod LE 0.
IF NOT lv_caod_cuom IS INITIAL.
ls_adjusted-ud = lv_caod_cuom.
ELSE.
ls_adjusted-ud = ls_mat_ud-csm3 + ls_cum_aod-cum_aod .
ENDIF.
APPEND ls_adjusted TO lt_adjusted.
CLEAR cum_aod.
EXIT.
ELSE.
ls_adjusted-ud = ls_mat_ud-csm3.
APPEND ls_adjusted TO lt_adjusted.
ENDIF.
ENDIF.
ENDIF.
ENDLOOP.
ENDIF.
*    ENDIF.
ENDLOOP.
ENDLOOP.
IF sy-subrc <> 0.
***      NO UDS FOUND, BUT ODS ARE THERE.
ENDIF.
SORT lt_material_adjust BY matnr. "IDATE DESCENDING.
SORT lt_oijnomi BY idate DESCENDING.
READ TABLE lt_oijnomi INTO ls_oijnomi INDEX 1.

DATA: lv_kunnr TYPE kunnr,
BEGIN OF ls_knumv ,
vbeln TYPE vbak-vbeln,
knumv TYPE knumv,
END OF ls_knumv,
lt_knumv LIKE TABLE OF ls_knumv,
BEGIN OF ls_konv,
knumv TYPE knumv,
kwert TYPE kwert,
END OF ls_konv,
lt_konv LIKE TABLE OF ls_konv,
BEGIN OF ls_vbap_uaod_matnr,
vbeln TYPE vbak-vbeln,
matnr TYPE mara-matnr,
zmeng TYPE vbap-zmeng,
END OF ls_vbap_uaod_matnr,
lt_vbap_uaod_matnr LIKE TABLE OF ls_vbap_uaod_matnr,
ls_kwert            LIKE ls_konv,
lt_kwert            LIKE TABLE OF ls_konv,
BEGIN OF ls_material,
matnr TYPE vbap-matnr,
kwert TYPE konv-kwert,
kunnr TYPE kunnr,
vbeln TYPE vbak-vbeln,
batch TYPE charg_d,
END OF ls_material,
lt_material LIKE TABLE OF ls_material,
ref_val     TYPE menge_d.
TYPES: BEGIN OF ty_a985,
charg TYPE charg_d,
knumh TYPE knumh,
matnr TYPE matnr18,
END OF ty_a985,

BEGIN OF ty_a987_1,
matnr TYPE matnr,
knumh TYPE knumh,
END OF ty_a987_1,

BEGIN OF ty_a987,
matnr TYPE matnr,
END OF ty_a987.

DATA: i_a983 TYPE TABLE OF ty_a985,
i_a984 TYPE TABLE OF ty_a985,
i_a987 TYPE TABLE OF ty_a987_1,
i_a988 TYPE TABLE OF ty_a987_1,
wa_konp TYPE konp.

DATA: wa_a983   TYPE ty_a985,
wa_a984   TYPE ty_a985,
wa_a987   TYPE ty_a987_1,
wa_a988   TYPE ty_a987_1,
wa_mcha   TYPE ty_a987,
lv_loevm TYPE loevm_ko,
lv_bzirk TYPE bzirk,
new_nomit TYPE oijnomi-nomit.
SELECT * FROM oijnomi INTO TABLE lt_oijnomi_adj WHERE nomtk = ls_oijnomi-nomtk.

SORT lt_oijnomi_adj BY nomit DESCENDING.
READ TABLE lt_oijnomi_adj INTO ls_oijnomi_adj INDEX 1.

DATA :headerdata_in       LIKE bapitswnom02,
headerdata_inx      LIKE bapitswnom02x,
** -> BEGIN OF CHANGES BY OF <ADITI> ON <21/10/24> FOR ATC
nominationitem_in LIKE TABLE OF bapitswnom03 WITH HEADER LINE, "#EC CI_USAGE_OK[2215424]
nominationitem_inx LIKE TABLE OF bapitswnom03x WITH HEADER LINE , "#EC CI_USAGE_OK[2215424]
** <- END CHANGES BY OF <ADITI> ON <21/10/24> FOR ATC
return              TYPE TABLE OF bapiret2,
ls_return           TYPE bapiret2,
headerdata_out      TYPE bapitswnom02_o.
DATA: lv_menge            TYPE oijnomi-menge.


*    TYPES: BEGIN OF TY_MSG,
*             MSG TYPE STRING,
*             TYPE TYPE STRING,
*           END OF TY_MSG.
*
*    DATA: IT_MSG TYPE STANDARD TABLE OF TY_MSG,
*          WA_MSG TYPE TY_MSG.


IF ls_oijnomi-nomtk IS INITIAL.
ls_oijnomi-nomtk = gv_nomtk.
IF ls_oijnomi-nomit IS INITIAL.
SELECT * FROM oijnomi INTO TABLE lt_oijnomi WHERE nomtk = gv_nomtk .
SORT lt_oijnomi BY nomit DESCENDING.
READ TABLE lt_oijnomi INTO ls_oijnomi INDEX 1.
ENDIF.
ENDIF.
headerdata_in-nominationnumber_sap = ls_oijnomi-nomtk.
headerdata_inx-nominationnumber_sap = ls_oijnomi-nomtk.
headerdata_inx-updkz = 'U'.

LOOP AT lt_adjusted INTO ls_adjusted WHERE ud <> 0.
LOOP AT lt_material_adjust INTO ls_material_adjust WHERE matnr = ls_adjusted-matnr AND ud <> 0.
EXIT.
ENDLOOP.
IF new_nomit IS INITIAL.
new_nomit = gv_nomit + 10.
ELSE.
new_nomit = new_nomit + 10.
ENDIF.
** CREATE NEW LINE ITEM IN THE LAST GISA NOMINATION FOR THE ADJUSTED QTY FOR EACH MATERIAL
nominationitem_in-nominationnumber_sap = ls_oijnomi-nomtk.
nominationitem_in-itemnumber            = new_nomit.
nominationitem_in-itemtype              = 'ZJ'.
nominationitem_in-scheduleddate         = ls_oijnomi-idate.
READ TABLE lt_oijnomi INTO ls_oijnomi WITH KEY s_matnr_i = ls_adjusted-matnr.

nominationitem_in-locationid            = ls_oijnomi-locid.
nominationitem_in-schedulematerial      = ls_adjusted-matnr.
nominationitem_in-nominatedquantity     = ls_adjusted-ud.
nominationitem_in-quantityunit_sap      = 'SM3'.

READ TABLE lt_vbap INTO ls_vbap WITH KEY matnr = ls_adjusted-matnr vbeln = ls_adjusted-vbeln.
IF sy-subrc = 0.
nominationitem_in-documentnumber      = ls_vbap-vbeln.
nominationitem_in-documentitem        = ls_vbap-posnr.
nominationitem_in-documentindicator   = 'G'.
ELSE.
nominationitem_in-documentindicator   = 'X'. "IDEALLY SHOULDNOT HA PPEN
"IF OCCURSSOMETHING WORNG IN CAL
ENDIF.
nominationitem_in-autoconfirm           = 'X'.

nominationitem_inx-nominationnumber_sap = headerdata_in-nominationnumber_sap.
nominationitem_inx-itemnumber           = new_nomit.
nominationitem_inx-itemtype             = 'X'.
nominationitem_inx-scheduleddate        = 'X'.
nominationitem_inx-locationid           = 'X'.
nominationitem_inx-schedulematerial     = 'X'.
nominationitem_inx-nominatedquantity    = 'X'.
nominationitem_inx-quantityunit_sap     = 'X'.
nominationitem_inx-documentnumber       = 'X'.
nominationitem_inx-documentitem         = 'X'.
nominationitem_inx-documentindicator    = 'X'.
nominationitem_inx-updkz                = 'I'.
nominationitem_inx-autoconfirm          = 'X'.
APPEND nominationitem_inx.

APPEND nominationitem_in .
CLEAR: nominationitem_in, nominationitem_inx.
ENDLOOP.

** THERE WILL BE ONLY ONE LINE ITEM AT A LOCATION FOR A CUSTOMER
READ TABLE lt_cum_aod INTO ls_cum_aod INDEX 1.
IF sy-subrc = 0.
IF ls_cum_aod-cum_aod > 0.

** CHECK IF IT IS STILL AUTHORIZED OR UNAUTHORIZED

SORT lt_oijnomi BY idate DESCENDING.
IF p_kunnr = '0000010222' .
* SOC THRESHOLD LIMIT IN CASE OF RIL DAHEJ BASED ON SUM OF ALL NOMINATIO N OF FORTNIGHT INSTEAD OF LAST DAY BY SACHIN ON 20.05.2014
LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'DU' AND idate LE so_date-high
AND idate GE so_date-low.
lv_menge = lv_menge + ls_oijnomi-menge.
ENDLOOP.
*EOC THRESHOLD LIMIT IN CASE OF RIL DAHEJ BASED ON SUM OF ALL NOMINATION
ELSE.
** GET LAST KNOWN MENGE
*         BEGIN OF CODE CHANGE : INPUT PROVIDED BY CHRISTINA / MR. MONDAL
READ TABLE lt_oijnomi INTO ls_oijnomi_temp INDEX 1.
*         LOOP AT LT_OIJNOMI INTO LS_OIJNOMI WHERE SITYP = 'DU' AND IDATE = SO_DATE-HIGH.
LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'DU' AND idate = ls_oijnomi_temp-idate.
*         END OF CODE CHANGE : INPUT PROVIDED BY CHRISTINA / MR. MONDAL
lv_menge = lv_menge + ls_oijnomi-menge.
ENDLOOP.
ENDIF.

ls_oijnomi-menge = lv_menge.   CLEAR lv_menge.

DATA l_yrxa_od_pcnt TYPE yrxa_od_pcnt.
DATA it_yrxa_od_pcnt TYPE TABLE OF yrxa_od_pcnt.

SELECT SINGLE * FROM kna1 INTO ls_kna1 WHERE kunnr = p_kunnr.

SELECT * INTO TABLE it_yrxa_od_pcnt FROM yrxa_od_pcnt.
READ TABLE it_yrxa_od_pcnt INTO l_yrxa_od_pcnt WITH KEY yy_kukla =ls_kna1-kukla.
IF sy-subrc = 0.
ref_val = ( ls_oijnomi-menge / 100 ) * l_yrxa_od_pcnt-yy_od_pcnt _f.
ELSE.
IF ls_kna1-kukla = '11'.
ref_val = ls_oijnomi-menge * 6 / 100.              " 6% TO
ELSE.
ref_val = ls_oijnomi-menge * 3 / 100.              " 3% TOL.
ENDIF.
ENDIF.
IF ls_cum_aod-cum_aod > ref_val.
** UNAUTHORIZED OVERDRAWL
** FIND HIGHEST PRICED MATERIAL
nominationitem_in-itemtype = 'ZU'.

SELECT charg knumh matnr " MATNR added by Aishwarya/Sonika 02.08 .2025 2 UOM
FROM a983
INTO TABLE i_a983
WHERE kunnr     = p_kunnr
AND kappl    = 'V'
AND kschl    = 'ZGOD'
AND auart_sd = 'ZNG2'
AND datab LT ls_oijnomi-idate
AND datbi GT ls_oijnomi-idate.
IF sy-subrc EQ 0.
LOOP AT i_a983 INTO wa_a983.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a983 -knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a983.
ELSE.
DELETE i_a983 WHERE knumh = wa_a983-knumh. CLEAR wa_a983.
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a983 INTO wa_a983 INDEX 1.         "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc = 0.
REFRESH i_a983.
CLEAR sy-subrc.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
loevm_ko
FROM konp
INTO lv_loevm
UP TO 1 ROWS
WHERE knumh = wa_a983-knumh ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC

IF lv_loevm NE 'X' AND sy-subrc EQ 0.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
matnr
FROM mcha
INTO wa_mcha
UP TO 1 ROWS
WHERE werks = lv_werk AND charg = wa_a983-charg
AND matnr = wa_a983-matnr " Added by Aishwarya/Sonika 2 UO M 02.08.25
AND lvorm NE 'X' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
ls_material-matnr = wa_mcha-matnr.
ls_material-batch = wa_a983-charg.
ENDIF.
ENDIF.
ENDIF.

IF wa_a983 IS INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a984,
lv_loevm.

*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
bzirk
FROM knvv
INTO lv_bzirk
UP TO 1 ROWS
WHERE kunnr = p_kunnr AND vkorg = '2000' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
SELECT charg knumh matnr FROM a984 " Added Matnr by Aishwarya/ Sonika
INTO TABLE i_a984
WHERE bzirk     = lv_bzirk
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
ENDIF.
IF sy-subrc EQ 0.
LOOP AT i_a984 INTO wa_a984.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 84-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a984.
ELSE.
DELETE i_a984 WHERE knumh = wa_a984-knumh. CLEAR wa_a984 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a984 INTO wa_a984 INDEX 1.        "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH: i_a984.
CLEAR sy-subrc.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
loevm_ko
FROM konp
INTO lv_loevm
UP TO 1 ROWS
WHERE knumh = wa_a984-knumh ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF lv_loevm NE 'X' AND sy-subrc EQ 0.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
matnr
FROM mcha
INTO wa_mcha
UP TO 1 ROWS
WHERE werks = lv_werk AND charg = wa_a984-charg
AND matnr = wa_a984-matnr " Added by Aishwarya/Sonika 2 UOM 02.08.2025
AND lvorm NE 'X' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
ls_material-matnr = wa_mcha-matnr.
ls_material-batch = wa_a984-charg.
ENDIF.
ENDIF.
ENDIF.
ENDIF.

IF wa_a984 IS INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a987,
lv_loevm,
wa_mcha.

SELECT matnr knumh
FROM a987
INTO TABLE i_a987
WHERE kunnr     = p_kunnr
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
IF sy-subrc EQ 0.
LOOP AT i_a987 INTO wa_a987.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 87-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a987.
ELSE.
DELETE i_a987 WHERE knumh = wa_a987-knumh. CLEAR wa_a987 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a987 INTO wa_a987 INDEX 1.        "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH: i_a987.
ls_material-matnr = wa_a987-matnr.
ENDIF.
ENDIF.

IF wa_a987 IS INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a988,
lv_loevm,
lv_bzirk.

*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
bzirk
FROM knvv
INTO lv_bzirk
UP TO 1 ROWS
WHERE kunnr = p_kunnr AND vkorg = '2000' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.

SELECT matnr knumh
FROM a988
INTO TABLE i_a988
WHERE bzirk     = lv_bzirk
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
ENDIF.
IF sy-subrc EQ 0.
LOOP AT i_a988 INTO wa_a988.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 88-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a988.
ELSE.
DELETE i_a988 WHERE knumh = wa_a988-knumh. CLEAR wa_a988 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a988 INTO wa_a988 INDEX 1.       "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH i_a988.
ls_material-matnr = wa_a988-matnr.
ENDIF.
ENDIF.
IF ls_material-matnr IS INITIAL.
*      LS_MATERIAL-MATNR = 'T_NGPMT'.
MESSAGE 'MAINTAIN HIGHEST PRICE MATERIAL' TYPE 'S'.
LEAVE TO TRANSACTION 'YRXU007'.
ENDIF.

ELSE.
**   AUTHORIZED OVERDRAWL
**    FIND HIGHEST PRICED MATERIAL FOR THE CUSTOMER.
nominationitem_in-itemtype = 'ZA'.
REFRESH lt_material.
CLEAR ls_material.
SELECT vbeln matnr zmeng FROM vbap INTO TABLE lt_vbap_uaod_matnr FOR ALL ENTRIES IN lt_vbeln WHERE vbeln = lt_vbeln-vbeln."KNUMV ."LT_KNUMV
IF sy-subrc = 0.
** -> BEGIN OF CHANGES BY OF RITIKA ON 21.10.2024 FOR ATC
*            SELECT KNUMV KWERT FROM KONV INTO TABLE LT_KONV FOR ALL ENTRI ES IN LT_VBELN WHERE KNUMV = LT_VBELN-KNUMV AND ( KSCHL = 'ZPR1' OR KSC HL = 'ZPR3' ).
SELECT knumv kwert FROM v_konv_cds INTO TABLE lt_konv FOR ALL ENTRIES IN lt_vbeln WHERE knumv = lt_vbeln-knumv AND ( kschl = 'ZPR1' O R kschl = 'ZPR3' ).
** -> END OF CHANGES BY OF RITIKA ON 21.10.2024 FOR ATC
LOOP AT lt_konv INTO ls_konv.
*    SOC BY SACHIN ON 03.07.2012     HIGHEST PRICE CONTRACT DO NOT ADD ZPR1 AND ZPR3 IN CASE OF NGPMTJV MATERIAL
READ TABLE lt_vbeln INTO ls_knumv WITH KEY knumv = ls_konv-knumv.
IF sy-subrc EQ 0 .
READ TABLE lt_vbap INTO ls_vbap WITH KEY vbeln = ls_knumv-vbeln .
IF sy-subrc EQ 0 AND ls_vbap-matnr = 'GMS_NGPMTJV' .
READ TABLE lt_kwert INTO ls_kwert WITH KEY knumv = ls_konv-knumv.
IF sy-subrc EQ 0 .
IF ls_kwert-kwert LT ls_konv-kwert.
MOVE-CORRESPONDING ls_konv TO ls_kwert.
MODIFY TABLE lt_kwert FROM ls_kwert .
ENDIF.
ELSE.
MOVE-CORRESPONDING ls_konv TO ls_kwert.
COLLECT ls_kwert INTO lt_kwert.
ENDIF.
ELSE.
MOVE-CORRESPONDING ls_konv TO ls_kwert.
COLLECT ls_kwert INTO lt_kwert.
ENDIF.
ELSE.
MOVE-CORRESPONDING ls_konv TO ls_kwert.
COLLECT ls_kwert INTO lt_kwert.
ENDIF.
*             MOVE-CORRESPONDING LS_KONV TO LS_KWERT.
*             COLLECT LS_KWERT INTO LT_KWERT.
*    EOC BY SACHIN ON 03.07.2012   HIGHEST PRICE CONTRACT DO NOT ADD ZPR1 AND ZPR3 IN CASE OF NGPMTJV MATERIAL
ENDLOOP.

LOOP AT lt_kwert INTO ls_kwert.
READ TABLE lt_vbeln INTO ls_knumv WITH KEY knumv = ls_kwert-knumv.
IF sy-subrc = 0.
READ TABLE lt_vbap_uaod_matnr INTO ls_vbap_uaod_matnr WITH KEY vbeln = ls_knumv-vbeln.
IF sy-subrc = 0.
ls_material-matnr = ls_vbap_uaod_matnr-matnr.
ls_material-kwert = ls_kwert-kwert / ls_vbap_uaod_matnr-zmeng. "LS_VBAP-ZMENG.
ls_material-kunnr = p_kunnr.
ls_material-vbeln = ls_knumv-vbeln.
APPEND ls_material TO lt_material.
ENDIF.
ENDIF.
ENDLOOP.
SORT    lt_material DESCENDING BY kwert.
SORT lt_knumv DESCENDING BY vbeln.
READ TABLE lt_material INTO ls_material INDEX 1.
READ TABLE lt_knumv INTO ls_knumv INDEX 1.

nominationitem_in-schedulematerial = ls_material-matnr.
nominationitem_in-documentnumber    = ls_material-vbeln.
nominationitem_in-documentitem      = 10.
ENDIF.
ENDIF.
** BEFORE POSTING ZA OR ZU CHECK IF THE LINE ITEM ALREADY EXISTS, IF SO UPDATE THE EXISTING LINE.
** CHECK WITH GAURAV.
** CREATE NEW LINE ITEM IN THE LAST GISA NOMINATION FOR THE ADJUSTED QTY FOR EACH MATERIAL
DATA: lt_nomit_new TYPE TABLE OF oijnomi,
ls_nomit_new LIKE LINE OF lt_nomit_new..
nominationitem_in-nominationnumber_sap = headerdata_in-nominationnumber_sap.
IF new_nomit IS INITIAL.
new_nomit = gv_nomit + 10. "LS_NOMIT_NEW-NOMIT + 10.
ENDIF.
nominationitem_in-itemnumber             = new_nomit + 10.

nominationitem_in-scheduleddate         = ls_oijnomi-idate.
nominationitem_in-locationid            = ls_cum_aod-locid."LS_OIJ NOMI-LOCID.
nominationitem_in-nominatedquantity     = ls_cum_aod-cum_aod.
nominationitem_in-quantityunit_sap      = 'SM3'.
IF nominationitem_in-itemtype           = 'ZA'.
nominationitem_in-documentindicator   = 'G'.
ELSEIF nominationitem_in-itemtype       = 'ZU'.
nominationitem_in-documentindicator   = 'S'.
ENDIF.
nominationitem_in-autoconfirm           = 'X'.
APPEND nominationitem_in.

nominationitem_inx-nominationnumber_sap = headerdata_in-nominationnumber_sap.
nominationitem_inx-itemnumber           = nominationitem_in-itemnumber.
nominationitem_inx-itemtype             = 'X'.
nominationitem_inx-scheduleddate        = 'X'.
nominationitem_inx-locationid           = 'X'.
nominationitem_inx-schedulematerial     = 'X'.
nominationitem_inx-nominatedquantity    = 'X'.
nominationitem_inx-quantityunit_sap     = 'X'.
nominationitem_inx-documentnumber       = 'X'.
nominationitem_inx-documentitem         = 'X'.
nominationitem_inx-documentindicator    = 'X'.
nominationitem_inx-updkz                = 'I'.
nominationitem_in-autoconfirm           = 'X'.
APPEND nominationitem_inx.
ENDIF.
ENDIF.
CLEAR: nominationitem_in, nominationitem_inx.

SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_ticket_i FOR ALL EN TRIES IN lt_oijnomi_adj WHERE nomtk = lt_oijnomi_adj-nomtk
AND nomit = lt_oijnomi_adj-nomit.
** IF THERE ARE ANY Z* NOMINATION ITEMS, DELETE THEM.
LOOP AT lt_oijnomi_adj INTO ls_oijnomi WHERE sityp CP 'Z*' AND delind <> 'X'.
**     BEFORE DELETE CHECK IF ANY TICKET EXISTS
READ TABLE lt_oij_el_ticket_i INTO ls_oij_el_ticket_i WITH KEY nomtk = ls_oijnomi-nomtk nomit = ls_oijnomi-nomit.
IF sy-subrc <> 0.
nominationitem_in-nominationnumber_sap = headerdata_in-nominationnumber_sap.
nominationitem_in-itemnumber             = ls_oijnomi-nomit.
nominationitem_inx-nominationnumber_sap = headerdata_in-nominationnumber_sap.
nominationitem_inx-itemnumber            = ls_oijnomi-nomit.
nominationitem_inx-updkz                 = 'D'.
ENDIF.

ENDLOOP.

*** MODIFY THE NOMINATION LINE ITEM WITH ZU TYPE WITH THE OTHER ZU QUANT ITIES.
DATA: BEGIN OF ls_cum_menge_zu,
locid        TYPE oijnomi-locid,
cum_menge_zu TYPE menge_d,
END OF ls_cum_menge_zu,
lt_cum_menge_zu LIKE TABLE OF ls_cum_menge_zu,
cum_menge_zu    TYPE menge_d,
lv_meins        TYPE meins,
lv_trqty        TYPE msego2-adqnt,
lv_tgqty        TYPE msego2-adqnt.

SORT lt_oijnomi_zu BY locid.
LOOP AT lt_oijnomi_zu INTO ls_oijnomi.
ls_cum_menge_zu-locid = ls_oijnomi-locid.
** CONVERT UNAUTHORIZED TO CONTRACTUAL UOM USING GCV AND NCV
IF lv_meins IS INITIAL.
SELECT SINGLE meins FROM mara INTO lv_meins WHERE matnr = ls_oijno
ENDIF.
lv_trqty = ls_oijnomi-menge.
*    READ TABLE LT_YRO_NOM_PARAM INTO LS_YRO_NOM_PARAM WITH KEY NOMTK = LS_OIJNOMI-NOMTK.
*    IF SY-SUBRC = 0.
*      CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
*        EXPORTING
*           I_TRQTY = LV_TRQTY
*           I_TRUOM = 'SM3'
*           I_TGUOM = LV_MEINS
*           LV_GCV = LS_YRO_NOM_PARAM-AGCV
*           LV_NCV = LS_YRO_NOM_PARAM-ANCV
*        CHANGING
*           C_TGQTY = LV_TGQTY.
*      LS_OIJNOMI-MENGE = LV_TGQTY.
*      LV_TRQTY          = LV_TGQTY.
*** USING WEIGHTED AVG CONVERT TO SM3 AGAIN
*
*      CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
*        EXPORTING
*           I_TRQTY = LV_TRQTY
*           I_TRUOM = LV_MEINS
*           I_TGUOM = 'SM3' "LV_MEINS'
*           LV_GCV = WT_GCV1
*           LV_NCV = WT_NCV1
*        CHANGING
*           C_TGQTY = LV_TGQTY.
*
*      LS_OIJNOMI-MENGE = LV_TGQTY.
*    ENDIF.
ls_cum_menge_zu-cum_menge_zu = ls_oijnomi-menge.
COLLECT ls_cum_menge_zu INTO lt_cum_menge_zu.
cum_menge_zu = ls_oijnomi-menge + cum_menge_zu.
ENDLOOP.
** AT THIS POINT THERE WILL BE ALWAYS ONLY ONE LINE ITEM
READ TABLE lt_cum_menge_zu INTO ls_cum_menge_zu INDEX 1.
IF sy-subrc = 0.
IF ls_cum_menge_zu-cum_menge_zu IS NOT INITIAL.
READ TABLE nominationitem_in WITH KEY itemtype = 'ZU'.
IF sy-subrc = 0.
*        GET THE WEIGHTED GCV AND NCV FOR ZU AND ADJUSTED ZU
wt_ncv_zu_s = ( nominationitem_in-nominatedquantity * wt_ncv + cum_menge_zu * wt_ncv1 ) /
( nominationitem_in-nominatedquantity + cum_menge_zu ) .
wt_gcv_zu_s = ( nominationitem_in-nominatedquantity * wt_gcv + cum_menge_zu * wt_gcv1 ) /
( nominationitem_in-nominatedquantity + cum_menge_zu ) .

nominationitem_in-nominatedquantity    = nominationitem_in-nominatedquantity + cum_menge_zu.
nominationitem_in-documentindicator    = 'S'."'X'.
MODIFY nominationitem_in INDEX sy-tabix.

ELSE.
nominationitem_in-nominationnumber_sap = headerdata_in-nominationnumber_sap.
IF NOT new_nomit IS INITIAL.
nominationitem_in-itemnumber         = new_nomit + 10.
ELSE.
nominationitem_in-itemnumber         = gv_nomit + 10."LS_OIJNO
ENDIF.
*********** LOGIC TO FND HIGHEST PRICED MATERIAL
SELECT charg knumh matnr FROM a983 " Added matnr by Aishwarya /Sonika 04.08.2025 2UOM
INTO TABLE i_a983
WHERE kunnr     = p_kunnr
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND   datab LT ls_oijnomi-idate
AND   datbi GT ls_oijnomi-idate.
IF sy-subrc EQ 0.
LOOP AT i_a983 INTO wa_a983.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a983 -knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a983.
ELSE.
DELETE i_a983 WHERE knumh = wa_a983-knumh. CLEAR wa_a983.
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a983 INTO wa_a983 INDEX 1.          "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH i_a983.
CLEAR sy-subrc.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
loevm_ko
FROM konp
INTO lv_loevm
UP TO 1 ROWS
WHERE knumh = wa_a983-knumh ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC

IF lv_loevm NE 'X' AND sy-subrc EQ 0.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
matnr
FROM mcha
INTO wa_mcha
UP TO 1 ROWS
WHERE werks = lv_werk AND charg = wa_a983-charg
AND matnr = wa_a983-matnr " Added by Aishwarya/Sonika 2 UO M 02.08.2025
AND lvorm NE 'X' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
ls_material-matnr = wa_mcha-matnr.
ls_material-batch = wa_a983-charg.
ENDIF.
ENDIF.
ENDIF.

IF wa_a983 IS   INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a984,
lv_loevm.

*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
bzirk
FROM knvv
INTO lv_bzirk
UP TO 1 ROWS
WHERE kunnr = p_kunnr AND vkorg = '2000' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
SELECT charg knumh matnr "Added Matnr by Aishwarya / Sonika 04.08.2025 2 UOM
FROM a984
INTO TABLE i_a984
WHERE bzirk     = lv_bzirk
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
ENDIF.
IF sy-subrc EQ 0.
LOOP AT i_a984 INTO wa_a984.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 84-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a984.
ELSE.
DELETE i_a984 WHERE knumh = wa_a984-knumh. CLEAR wa_a984 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a984 INTO wa_a984 INDEX 1.        "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH: i_a984.
CLEAR sy-subrc.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
loevm_ko
FROM konp
INTO lv_loevm
UP TO 1 ROWS
WHERE knumh = wa_a984-knumh ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC

IF lv_loevm NE 'X' AND sy-subrc EQ 0.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT
matnr
FROM mcha
INTO wa_mcha
UP TO 1 ROWS
WHERE werks = lv_werk AND charg = wa_a984-charg
AND matnr = wa_a984-matnr " Added by Aishwarya/Sonika 2 UOM 02.08.2025
AND lvorm NE 'X' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
ls_material-matnr = wa_mcha-matnr.
ls_material-batch = wa_a984-charg.
ENDIF.
ENDIF.
ENDIF.
ENDIF.

IF wa_a984 IS INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a987,
lv_loevm,
wa_mcha.

SELECT matnr knumh
FROM a987
INTO TABLE i_a987
WHERE kunnr     = p_kunnr
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
IF sy-subrc EQ 0.
LOOP AT i_a987 INTO wa_a987.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 87-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a987.
ELSE.
DELETE i_a987 WHERE knumh = wa_a987-knumh. CLEAR wa_a987 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a987 INTO wa_a987 INDEX 1.        "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH i_a987.
ls_material-matnr = wa_a987-matnr.
ENDIF.
ENDIF.

IF wa_a987 IS INITIAL AND sy-subrc NE 0.
CLEAR:
sy-subrc,
wa_a988,
lv_loevm,
lv_bzirk.

*Begin of changes by of VIPUL on 20241030   for ATC
SELECT
bzirk
FROM knvv
INTO lv_bzirk
UP TO 1 ROWS
WHERE kunnr = p_kunnr AND vkorg = '2000' ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc EQ 0.
SELECT matnr knumh
FROM a988
INTO TABLE i_a988
WHERE bzirk     = lv_bzirk
AND    kappl    = 'V'
AND    kschl    = 'ZGOD'
AND    auart_sd = 'ZNG2'
AND    datab LE ls_oijnomi-idate
AND    datbi GE ls_oijnomi-idate.
ENDIF.
IF sy-subrc EQ 0.
LOOP AT i_a988 INTO wa_a988.
SELECT SINGLE * FROM konp INTO wa_konp WHERE knumh = wa_a9 88-knumh AND kappl = 'V' AND kschl = 'ZGOD' AND loevm_ko = ' '.
IF sy-subrc = 0.
CLEAR wa_a988.
ELSE.
DELETE i_a988 WHERE knumh = wa_a988-knumh. CLEAR wa_a988 .
ENDIF.
ENDLOOP.
ENDIF.
** -> BEGIN OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
READ TABLE i_a988 INTO wa_a988 INDEX 1.          "#EC CI_NOORDER
** -> END OF CHANGES BY KRITIKA ON 28.10.2024 FOR ATC
IF sy-subrc EQ 0.
REFRESH: i_a988.
ls_material-matnr = wa_a988-matnr.
ENDIF.
ENDIF.
*         """NEW LOGIC FOR NOMIT
*         NOMINATIONITEM_IN-NOMINATIONNUMBER_SAP = HEADERDATA_IN-NOMINAT IONNUMBER_SAP.
*         IF NEW_NOMIT IS INITIAL.
*           NEW_NOMIT = GV_NOMIT + 10. "LS_NOMIT_NEW-NOMIT + 10.
*         ENDIF.
nominationitem_in-itemnumber              = new_nomit + 10.
***********      END OF LOGIC FOR HIGHEST PRICE MATERIAL
nominationitem_in-scheduleddate           = ls_oijnomi-idate.
nominationitem_in-locationid              = ls_cum_menge_zu-locid. "LS_OIJNOMI-LOCID.
nominationitem_in-nominatedquantity       = cum_menge_zu.
nominationitem_in-quantityunit_sap        = 'SM3'.
nominationitem_in-documentindicator       = 'S'."'X'.
nominationitem_in-itemtype                = 'ZU'.
nominationitem_in-schedulematerial        = ls_material-matnr. "L S_OIJNOMI-MATNR_I.
APPEND nominationitem_in.

nominationitem_inx-nominationnumber_sap = headerdata_in-nominationnumber_sap.
nominationitem_inx-itemnumber           = nominationitem_in-itemnumber.
nominationitem_inx-itemtype            = 'X'.
nominationitem_inx-scheduleddate       = 'X'.
nominationitem_inx-locationid          = 'X'.
nominationitem_inx-schedulematerial    = 'X'.
nominationitem_inx-nominatedquantity   = 'X'.
nominationitem_inx-quantityunit_sap    = 'X'.
nominationitem_inx-documentnumber      = 'X'.
nominationitem_inx-documentitem        = 'X'.
nominationitem_inx-documentindicator   = 'X'.
nominationitem_inx-updkz = 'I'.
APPEND nominationitem_inx.
ENDIF.
ENDIF.
ENDIF.

*  * DATA DECLARATIONS.
DATA: v_vbeln              LIKE vbak-vbeln.
DATA: header               LIKE bapisdhd1. "BAPISDHEAD1.
DATA: headerx              LIKE bapisdhd1x. "BAPISDHEAD1X.
DATA: item                 LIKE bapisditm   OCCURS 0 WITH HEADER LINE."B APISDITEM
DATA: itemx                LIKE bapisditmx OCCURS 0 WITH HEADER LINE."BA PISDITEMX
DATA: partner              LIKE bapiparnr   OCCURS 0 WITH HEADER LINE. "B APIPARTNR
DATA: lt_schedules_inx     TYPE STANDARD TABLE OF bapischdlx
WITH HEADER LINE.
DATA: lt_schedules_in    TYPE STANDARD TABLE OF bapischdl
WITH HEADER LINE.
DATA: lv_satnr           TYPE mara-satnr,
i_sales_cfgs_value TYPE TABLE OF bapicuval WITH HEADER LINE,
i_sales_cfgs_ref   TYPE bapicucfg OCCURS 0 WITH HEADER LINE..
DATA wt_ncv_c            TYPE char15.
DATA ls_oij_el_doc_mot TYPE oij_el_doc_mot.
DATA wa_tabix            TYPE sy-tabix.

"BREAK-POINT.
"BOC 4000007694 TECHNICAL ANKUR ON 07.02.2024"
DATA:qty TYPE char20,
qty1 TYPE char20.
LOOP AT nominationitem_in.
*     *SOC CHARM ID :4000008639    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:21.08.2024
SELECT SINGLE y_low FROM yrxa_gms_constnt INTO @DATA(lv_qty) WHERE y _param = 'A4'.
IF nominationitem_in-nominatedquantity LE lv_qty.
*       *EOC CHARM ID :4000008639    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:21.08.2024
*     IF NOMINATIONITEM_IN-NOMINATEDQUANTITY LE '0.059'.    *COC CHARM ID :4000008639    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:21.0 8.2024
qty = nominationitem_in-nominatedquantity .
qty1 = lv_qty.
CONDENSE:qty,
qty1.
CONCATENATE 'TICKET FOR' nominationitem_in-itemtype 'NOT INITIATED AS QTY' qty 'IS LESS THAN/ EQUAL TO' qty1 'SM3'
INTO wa_msg-msg SEPARATED BY space.
wa_msg-type = 'ERROR'.
APPEND wa_msg TO it_msg.
CLEAR: wa_msg, qty.

ENDIF.
ENDLOOP.
* DELETE NOMINATIONITEM_IN WHERE NOMINATEDQUANTITY LE '0.059'. *COC CHA RM ID :4000008639    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI D T:21.08.2024
*****Commentd By Vaibhav on 8 April 2025
DELETE nominationitem_in WHERE nominatedquantity LE lv_qty.   "SOC CHA RM ID :4000008639    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI D T:21.08.2024
***************************end of comment
* CLEAR LV_QTY.
READ TABLE nominationitem_in WITH KEY itemtype = 'ZU'.
IF sy-subrc = 0." and NOMINATIONITEM_IN-NOMINATEDQUANTITY gE LV_QTY. .
wa_tabix = sy-tabix.
**       CREATE SALES ORDER
*       * SALES DOCUMENT TYPE
lv_tabix = sy-tabix.
header-doc_type = 'ZNG2'. "P_AUART'.
headerx-doc_type = 'X'.

* SALES ORGANIZATION
header-sales_org = 2000.
headerx-sales_org = 'X'.


* DISTRIBUTION CHANNEL
header-distr_chan = 10."P_VTWEG.
headerx-distr_chan = 'X'.

* DIVISION
header-division    = 10."P_SPART.
headerx-division   = 'X'.
headerx-updateflag = 'I'.

* PARTNER DATA
* SOLD TO
partner-partn_role = 'AG'.
partner-partn_numb = p_kunnr.
APPEND partner.
* SHIP TO
partner-partn_role = 'WE'.
partner-partn_numb = p_kunnr.
APPEND partner.

* ITEM DATA
itemx-updateflag = 'I'.
item-itm_number = '000010'.
itemx-itm_number = 'X'.

* MATERIAL
IF ls_material-matnr IS INITIAL.
MESSAGE e001(00) WITH ' MAINTAIN HIGHEST PRICED MATERIAL FOR CUSTO MER' p_kunnr.
ENDIF.
item-material = ls_material-matnr.
itemx-material = 'X'.

*** IF BATCH MANAGED
IF NOT ls_material-batch IS INITIAL.
item-batch = ls_material-batch.
itemx-batch = 'X'.
ENDIF.

DATA : lv_batch TYPE boolean.
IF ls_material-batch IS INITIAL.
SELECT SINGLE xchpf INTO lv_batch FROM mara WHERE matnr = ls_material-matnr.
IF sy-subrc = 0.
IF lv_batch IS NOT INITIAL.
MESSAGE e001(00) WITH 'MAINTAIN BATCH FOR' ls_material-matnr 'FOR CONDITION RECORD'.
ENDIF.
ENDIF.
ENDIF.

** IF CONFIG SPECIFIC
SELECT SINGLE satnr FROM mara INTO lv_satnr WHERE matnr = ls_material-matnr.
IF lv_satnr IS NOT INITIAL.
i_sales_cfgs_ref-posex = '000010'.
APPEND i_sales_cfgs_ref.

item-po_itm_no = '000010'.
itemx-po_itm_no = 'X'.

i_sales_cfgs_value-charc = 'CALVAL'. "'CAL VALUE'. "
*        IF NOT WT_NCV IS INITIAL.
*          CLEAR WT_NCV_C.
*          CALL FUNCTION 'FLTP_CHAR_CONVERSION'
*            EXPORTING
*              INPUT = WT_NCV
*            IMPORTING
*              FLSTR = WT_NCV_C.
*          WRITE WT_NCV_C(8) TO I_SALES_CFGS_VALUE-VALUE.
*          CONDENSE I_SALES_CFGS_VALUE-VALUE.
*        ELSE.

IF wt_ncv1 IS INITIAL.
wt_ncv1 = wt_ncv.
ENDIF.
IF wt_gcv1 IS INITIAL.
wt_gcv1 = wt_gcv.
ENDIF.


IF NOT wt_ncv1 IS INITIAL.
CLEAR wt_ncv_c.
CALL FUNCTION 'FLTP_CHAR_CONVERSION'
EXPORTING
input = wt_ncv1
IMPORTING
flstr = wt_ncv_c.
WRITE wt_ncv_c(8) TO i_sales_cfgs_value-value.
CONDENSE i_sales_cfgs_value-value.
ELSE.
CONDENSE i_sales_cfgs_value-value.
ENDIF.
*       ENDIF.
APPEND i_sales_cfgs_value.
ENDIF.
* PLANT
itemx-plant   = 'X'.

*******************CHANGE BY VAIBHAV ON 28.04.2022
SELECT * INTO TABLE @DATA(it_oij)
FROM oijnomi
WHERE
idate IN @so_date
AND
sityp = 'DU'
AND
docind = 'G'
AND
delind <> 'X'
AND
partnr = @p_kunnr.
IF sy-subrc = 0.
SELECT * INTO TABLE @DATA(it_layt)
FROM oij_el_cp_layt
FOR ALL ENTRIES IN @it_oij
WHERE
vbeln = @it_oij-docnr.
IF sy-subrc = 0.
TYPES : BEGIN OF ty_rank,
vbeln   TYPE oij_el_cpno,
rank(3) TYPE n,
END OF ty_rank.
DATA : wa_rank TYPE ty_rank.
DATA it_rank TYPE TABLE OF ty_rank.

LOOP AT it_layt INTO DATA(wa_layt1).
REPLACE 'RAK' IN wa_layt1-trantyp WITH ' ' .
REPLACE 'RANK' IN wa_layt1-trantyp WITH ' ' .
REPLACE 'RATAB' IN wa_layt1-trantyp WITH ' ' .
CONDENSE wa_layt1-trantyp.
IF wa_layt1-trantyp IS INITIAL OR wa_layt1-trantyp CA 'abcdefghijklmnopqrstuvwqyz'.
wa_layt1-trantyp = '0'.
ENDIF.
CLEAR wa_rank.
wa_rank-vbeln = wa_layt1-vbeln.
wa_rank-rank = wa_layt1-trantyp.
APPEND wa_rank TO it_rank.
*          MODIFY IT_LAYT FROM WA_LAYT1.
ENDLOOP.
*         SORT IT_LAYT BY TRANTYP DESCENDING.
SORT it_rank BY rank DESCENDING.

SELECT * INTO TABLE @DATA(it_vbkd)
FROM vbkd
FOR ALL ENTRIES IN @it_layt
WHERE
vbeln = @it_layt-vbeln
AND
posnr = '000010'
AND
kdkg5 <> 'BC'.
IF it_vbkd[] IS NOT INITIAL.
**           SORT IT_VBKD BY KDKG5.
*           DELETE IT_VBKD WHERE KDKG5 = ' '.
**           DELETE IT_VBKD WHERE KDKG5 <> 'BC'.
*           IF IT_VBKD[] IS INITIAL.
*            READ TABLE IT_LAYT INTO DATA(WA_LAYT) INDEX 1.
*            SELECT SINGLE WERKS INTO ITEM-PLANT
*               FROM VBAP
*               WHERE VBELN = WA_LAYT-VBELN.
*          ENDIF.
ELSE.
READ TABLE it_rank INTO wa_rank INDEX 1.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT werks INTO item-plant
FROM vbap
UP TO 1 ROWS
WHERE vbeln = wa_rank-vbeln ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
ENDIF.
ENDIF.
ENDIF.
**********************************************************
** COMMENTED BY SHWETA SONI TCS
*     SELECT SINGLE MEINS FROM MARA INTO LV_MEINS WHERE MATNR = LS_MATERI AL-MATNR.
*     LV_TRQTY = NOMINATIONITEM_IN-NOMINATEDQUANTITY.
**      CONSIDER ZU WT GCV IF ZA LINE ITEM IS NOT THERE FOR CREATING SALES ORDER
*     IF WT_NCV IS INITIAL OR WT_GCV IS INITIAL .
*        WT_NCV_SO = WT_NCV1.
*        WT_GCV_SO = WT_GCV1.
*     ELSE.
*        WT_NCV_SO = WT_NCV.
*        WT_GCV_SO = WT_GCV.
*     ENDIF.
*
*     CALL FUNCTION 'YRX_QTY_UOM_TO_QTY_UOM'
*        EXPORTING
*          I_TRQTY = LV_TRQTY
*          I_TRUOM = 'SM3' "I_TRUOM
*          I_TGUOM = LV_MEINS
*          LV_GCV = WT_GCV_SO
*          LV_NCV = WT_NCV_SO
*        CHANGING
*          C_TGQTY = LV_TGQTY.

lv_tgqty = lv_trqty = nominationitem_in-nominatedquantity.

** END OF CHANGES

* QUANTITY
item-target_qty = lv_tgqty."NOMINATIONITEM_IN-NOMINATEDQUANTITY.
itemx-target_qty = 'X'.
** ADDED BY SHWETA SONI TCS
item-target_qu = 'SM3'.
item-t_unit_iso = 'SM3'.
itemx-t_unit_iso = 'X'.
itemx-target_qu = 'X'.

item-sales_unit = 'SM3'.
item-s_unit_iso = 'SM3'.

itemx-sales_unit = 'X'.
*** END OF CHANGES
*************************change by vaibhav for Pricing Date 15.04.2025
IF so_date-high IS NOT INITIAL.
item-price_date = so_date-high .
item-bill_date = so_date-high .
itemx-price_date = 'X'.
itemx-bill_date = 'X'.
ENDIF.

APPEND item.
APPEND itemx.

*   FILL SCHEDULE LINES
lt_schedules_in-itm_number = '000010'.
lt_schedules_in-sched_line = '0001'.
lt_schedules_in-req_date   = sy-datum.
lt_schedules_in-req_qty    = lv_tgqty."NOMINATIONITEM_IN-NOMINATEDQU ANTITY.."P_MENGE.
APPEND lt_schedules_in.

*   FILL SCHEDULE LINE FLAGS
lt_schedules_inx-itm_number    = '000010'.
lt_schedules_inx-sched_line    = '0001'.
lt_schedules_inx-updateflag    = 'X'.
lt_schedules_inx-req_qty       = 'X'.
lt_schedules_inx-req_date      = 'X'.
APPEND lt_schedules_inx.
"BREAK-POINT.





IF nominationitem_in[] IS NOT INITIAL.
** -> BEGIN OF CHANGES BY KRITIKA ON 18.10.2024 FOR ATC
CALL FUNCTION 'BAPI_SALESORDER_CREATEFROMDAT2' "#EC CI_USAGE_OK[24 38131]
** <- END OF CHANGES BY KRITIKA ON 18.10.2024 FOR ATC
EXPORTING
order_header_in     = header
IMPORTING
salesdocument       = v_vbeln
TABLES
return              = return
order_items_in      = item
order_items_inx     = itemx " ADDED BY SHWETA SONI TCS
order_partners      = partner
order_schedules_in = lt_schedules_in
order_schedules_inx = lt_schedules_inx
order_cfgs_ref      = i_sales_cfgs_ref
order_cfgs_value    = i_sales_cfgs_value.
* CHECK THE RETURN TABLE.
LOOP AT return INTO ls_return WHERE type = 'E' OR type = 'A'.
EXIT.
ENDLOOP.

ELSE.

IF it_msg[] IS NOT INITIAL.
PERFORM: error_alv.
ENDIF.

ENDIF.


IF sy-subrc <> 0.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
EXPORTING
wait = 'X'.



DATA n TYPE i.
*** CODE CORRECTION TO END INFINITE LOOP , AS DISCUSSED WITH MR. A.MONDA L
*** BEGIN OF CHANGE... SINCE THIS CODE BLOCK (CREATION OF SO AND THE UP DATE OIJ_EL_DOC_MOT
*      HAPPEND ONLY FOR ITEMTYPE = ZU , AND ZU ITEMTYPES ARE NOT TSW REL EVANT , SO COMMENTING BELOW CODE)

*       DO .
*         SELECT SINGLE * FROM OIJ_EL_DOC_MOT INTO LS_OIJ_EL_DOC_MOT WHER E VBELN = V_VBELN.
*         IF SY-SUBRC = 0.
*            LS_OIJ_EL_DOC_MOT-FROMDATE = SO_DATE-LOW."NOMINATIONITEM_IN-SCHEDULEDDATE. " - 10.
*            LS_OIJ_EL_DOC_MOT-TODATE    = SO_DATE-HIGH."NOMINATIONITEM_IN -SCHEDULEDDATE." + 10. "LS_OIJ_EL_DOC_MOT-FROMDATE.
*            UPDATE OIJ_EL_DOC_MOT FROM LS_OIJ_EL_DOC_MOT.
*            COMMIT WORK.
*            EXIT.
*         ELSE.
*            WAIT UP TO 2 SECONDS.
*         ENDIF.
*       ENDDO.
*** END OF CHANGE...END OF CODE COMMENT.
ELSE.
MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH
ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
ENDIF.

nominationitem_in-documentnumber   = v_vbeln.
nominationitem_in-schedulematerial = ls_material-matnr.
nominationitem_in-documentitem     = 10.
nominationitem_in-autoconfirm      = 'X'.
IF NOT lv_tabix IS INITIAL.
MODIFY nominationitem_in INDEX wa_tabix.
ENDIF.
CLEAR lv_tabix.

READ TABLE nominationitem_inx WITH KEY nominationnumber_sap = nominationitem_in-nominationnumber_sap
itemnumber = nominationitem_in-itemnumber.
IF sy-subrc = 0.
nominationitem_inx-documentnumber = 'X'.
nominationitem_inx-documentitem   = 'X'.
nominationitem_in-autoconfirm     = 'X'.
MODIFY nominationitem_inx INDEX sy-tabix..
ENDIF.


ENDIF.

REFRESH return.

IF NOT nominationitem_in[] IS INITIAL.

READ TABLE lt_oijnomh INTO ls_oijnomh   WITH KEY nomtk = gv_nomtk.

headerdata_in-nominationnumber_sap = '$0000000000000000001'.
headerdata_inx-nominationnumber_sap = '$0000000000000000001'.
headerdata_inx-updkz = 'I'.

headerdata_in-transportsystem      = ls_oijnomh-tsyst.
headerdata_in-nominationstatus     = '1'.
headerdata_in-nominationtype       = 'GISA'.

LOOP AT nominationitem_in.
nominationitem_in-nominationnumber_sap = headerdata_inx-nominationnumber_sap.
nominationitem_in-itemnumber = sy-tabix * 10.
nominationitem_in-scheduleddate = so_date-high.
MODIFY nominationitem_in INDEX sy-tabix.
ENDLOOP.

*    "BOC 4000007694 TECHNICAL ANKUR ON 07.02.2024"
*    DATA:QTY TYPE CHAR20.
*    LOOP AT NOMINATIONITEM_IN.
*      IF NOMINATIONITEM_IN-NOMINATEDQUANTITY LE '0.059'.
*        QTY = NOMINATIONITEM_IN-NOMINATEDQUANTITY .
*        CONDENSE:QTY.
*        CONCATENATE 'TICKET FOR' NOMINATIONITEM_IN-ITEMTYPE 'NOT INITIA TED AS QTY' QTY 'IS LESS THAN/ EQUAL TO 0.059 SM3'
*                      INTO WA_MSG-MSG SEPARATED BY SPACE.
*        APPEND WA_MSG TO IT_MSG.
*        CLEAR: WA_MSG, QTY.
*        DELETE NOMINATIONITEM_IN WHERE NOMINATIONNUMBER_SAP = NOMINAT IONITEM_IN-NOMINATIONNUMBER_SAP
*                                  AND    ITEMNUMBER       = NOMINATIONI TEM_IN-ITEMNUMBER.
*      ENDIF.
*    ENDLOOP.

*       IF IT_MSG[] IS NOT INITIAL.
*         PERFORM: ERROR_ALV.
*       ENDIF.

"EOC 4000007694 TECHNICAL ANKUR ON 07.02.2024"

CALL FUNCTION 'RFC_TSW_NOM_CREATEFROMDATA'
EXPORTING
headerdata_in     = headerdata_in
IMPORTING
headerdata_out    = headerdata_out
TABLES
nominationitem_in = nominationitem_in
return            = return.
LOOP AT return INTO ls_return WHERE type = 'A' OR type = 'E'.
EXIT.
ENDLOOP.

IF sy-subrc <> 0.
CLEAR ls_yro_nom_param.
REFRESH lt_yro_nom_param.
LOOP AT nominationitem_in. "X WHERE UPDKZ = 'I'.
ls_yro_nom_param_db-nomtk     = headerdata_out-nominationnumber_sap."NOMINATIONITEM_INX-NOMINATIONNUMBER_SAP.
ls_yro_nom_param_db-nomit     = nominationitem_in-itemnumber.
ls_yro_nom_param_db-umrsl     = 'ZNGZ'.
IF nominationitem_in-itemtype = 'ZU'.
ls_yro_nom_param_db-ancv    = wt_ncv1.
ls_yro_nom_param_db-agcv    = wt_gcv1.
ELSE.
ls_yro_nom_param_db-ancv    = wt_ncv.
ls_yro_nom_param_db-agcv    = wt_gcv.
ENDIF.
APPEND ls_yro_nom_param_db TO lt_yro_nom_param_db.
ENDLOOP.
IF NOT lt_yro_nom_param_db IS INITIAL.
INSERT yro_nom_param FROM TABLE lt_yro_nom_param_db.
ENDIF.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
EXPORTING
wait = 'X'.

***     CREATE AND ACTUALIZE TICKET.
headerdata_in-nominationnumber_sap = headerdata_out-nominationnumber_sap.
PERFORM ticket_actualizate USING headerdata_in-nominationnumber_sap lv_qty.
ELSE.
MESSAGE ID ls_return-id TYPE ls_return-type NUMBER ls_return-number WITH
ls_return-message_v1 ls_return-message_v2 ls_return-message_v3 ls_return-message_v4.
ENDIF.

ENDIF.

IF it_msg[] IS NOT INITIAL.
PERFORM: error_alv.
ENDIF.

"ENDIF.



*&                                                                     *
*&      FORM TICKET_ACTUALIZATE
*&                                                                     *
*       TEXT
*                                                                      *
*        >P_HEADERDATA_IN_NOMINATIONNUMBER TEXT
*                                                                      *
FORM ticket_actualizate USING     p_nomtk p_qty.

DATA: lt_oijnomi     TYPE TABLE OF oijnomi,
ls_oijnomi     LIKE LINE OF lt_oijnomi,
lt_oijnomi_tkt TYPE TABLE OF oijnomi,
ls_oijnomi_tkt TYPE          oijnomi,
lt_oij_el_tkt TYPE TABLE OF oij_el_ticket_i,
ls_oij_el_tkt TYPE           oij_el_ticket_i.

DO 10 TIMES.
SELECT * FROM oijnomi INTO TABLE lt_oijnomi WHERE nomtk = p_nomtk AN D quickconf = 'X' AND delind <> 'X'
AND menge GE p_qty.

LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZU' AND docind = 'S'.
APPEND ls_oijnomi TO lt_oijnomi_tkt.
ENDLOOP.

LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZJ' AND docind = 'G'.
APPEND ls_oijnomi TO lt_oijnomi_tkt.
ENDLOOP.

LOOP AT lt_oijnomi INTO ls_oijnomi WHERE sityp = 'ZA' AND docind = 'G'.
APPEND ls_oijnomi TO lt_oijnomi_tkt.
ENDLOOP.
IF NOT lt_oijnomi_tkt IS INITIAL.
EXIT.
ELSE.
WAIT UP TO 2 SECONDS.
ENDIF.
ENDDO.
IF NOT lt_oijnomi_tkt IS INITIAL.
DATA l_tabix TYPE sy-tabix.
SELECT * FROM oij_el_ticket_i INTO TABLE lt_oij_el_tkt FOR ALL ENTRI ES IN
lt_oijnomi_tkt WHERE nomtk = lt_oijnomi_tkt-nomtk AND
nomit =   lt_oijnomi_tkt-nomit.
IF sy-subrc = 0.
LOOP AT lt_oij_el_tkt INTO ls_oij_el_tkt.
LOOP AT lt_oijnomi_tkt INTO ls_oijnomi_tkt WHERE nomtk = ls_oij_el_tkt-nomtk AND
nomit = ls_oij_el_tkt-nomit.
DELETE lt_oijnomi_tkt INDEX sy-tabix..
ENDLOOP.
ENDLOOP.
ENDIF.

DATA: l_gas_day_char(10),
l_sr_no              TYPE i,
w_qty_char(18),
i_ticket_header      TYPE oij_el_ticket_h,   "TYPE OIJ_MENGE.
l_item               TYPE i,
w_ticket_number(50).

*AJAY
DATA: wt_ncv_p(14) TYPE p DECIMALS 3.
DATA: wt_gcv_p(14) TYPE p DECIMALS 3.
DATA: wt_ncv_cc(15) TYPE c.
DATA: wt_gcv_cc(15) TYPE c.
DATA: wt_ncv_zu(15) TYPE c.
DATA: wt_gcv_zu(15) TYPE c.

wt_ncv_p = wt_ncv.
wt_gcv_p = wt_gcv.
wt_ncv_cc = wt_ncv_p.
wt_gcv_cc = wt_gcv_p.
CONDENSE wt_ncv_cc.
CONDENSE wt_gcv_cc.
CLEAR : wt_ncv_p, wt_gcv_p.
****CHANGES IF ZU'S GCV, NCV IS INITAIL********
IF wt_ncv1 IS INITIAL.
wt_ncv1 = wt_ncv.
ENDIF.
IF wt_gcv1 IS INITIAL.
wt_gcv1 = wt_gcv.
ENDIF.
***********************************************
IF wt_ncv_zu_s IS NOT INITIAL AND wt_gcv_zu_s IS NOT INITIAL.
wt_ncv_p = wt_ncv_zu_s .
wt_gcv_p = wt_gcv_zu_s .
ELSE.
wt_ncv_p = wt_ncv1.
wt_gcv_p = wt_gcv1.
ENDIF.
.                                                       "WT_GCV1.
wt_ncv_zu = wt_ncv_p.
wt_gcv_zu = wt_gcv_p.
CONDENSE wt_ncv_zu.
CONDENSE wt_gcv_zu.
CLEAR : wt_ncv_p, wt_gcv_p.
***
SORT lt_oijnomi_tkt BY nomtk nomit.
* GET THE TICKET HEADER AND ITEM FOR ACTUALIZATION OF TICKET
CLEAR: l_item,l_sr_no.
* GET ALL THE LINE ITEM OF THAT NOMINATION IN A INTERNAL TABLE

CONCATENATE 'C' sy-datum sy-uzeit INTO w_ticket_number.
DESCRIBE TABLE lt_oijnomi_tkt LINES l_item .
l_sr_no = 1.

PERFORM bdc_dynpro USING 'SAPMOIJTN'                '0101'.
PERFORM bdc_field USING 'BDC_CURSOR'                'ROIJTIC_IO-TICK ETNR'.
PERFORM bdc_field USING 'BDC_OKCODE'                '=ENT1'.
PERFORM bdc_field USING 'ROIJTIC_IO-TICKETNR'        w_ticket_number .
PERFORM bdc_field USING 'ROIJTIC_IO-TICKET_TYPE'    'C'.
PERFORM bdc_field USING 'ROIJTIC_IO-NOMNR'          ''.
PERFORM bdc_field USING 'ROIJTIC_IO-NOMTK'           ls_oijnomi-nomtk.

IF l_item GT 1.
PERFORM bdc_dynpro USING   'SAPLOIJT_IO'          '0500' .
PERFORM bdc_field USING    'BDC_CURSOR'           'OIJNOMH-NOMNR'.
PERFORM bdc_field USING    'BDC_OKCODE'           '=TC_OIJNOMI_MAR K' .

PERFORM bdc_dynpro USING   'SAPLOIJT_IO'          '0500' .
PERFORM bdc_field USING    'BDC_CURSOR'           'OIJNOMH-NOMNR'.
PERFORM bdc_field USING    'BDC_OKCODE'           '=COPY' .
ENDIF.

*LOOP PORTION
WHILE l_sr_no LT l_item.
READ TABLE lt_oijnomi_tkt INTO ls_oijnomi INDEX l_sr_no.
IF sy-subrc EQ 0 .
WRITE ls_oijnomi-idate TO l_gas_day_char.
CONDENSE l_gas_day_char.
WRITE ls_oijnomi-menge TO w_qty_char.
CONDENSE w_qty_char.
PERFORM bdc_dynpro     USING 'SAPMOIJTN'                    '0301'.
PERFORM bdc_field       USING   'BDC_OKCODE'                '=IOTAB 4' .
PERFORM bdc_field       USING   'BDC_SUBSCR'                'SAPMOI JTN'.
PERFORM bdc_field       USING   'BDC_SUBSCR'                'SAPMOI JTN'.
PERFORM bdc_field       USING   'BDC_CURSOR'                'ROIJTI C_IO-END_DATE'.
PERFORM bdc_field       USING   'ROIJTIC_IO-NET_QUANTITY'     w_qty_char.
PERFORM bdc_field       USING   'ROIJTIC_IO-UOM'            'SM3'.
PERFORM bdc_field       USING   'ROIJTIC_IO-START_TIME'     '00:00: 00'.
PERFORM bdc_field       USING   'ROIJTIC_IO-OIB_BLTIME'     '00:00: 00'.
PERFORM bdc_field       USING   'ROIJTIC_IO-END_DATE'         l_gas_day_char.
PERFORM bdc_field       USING   'ROIJTIC_IO-END_TIME'       '00:00: 00'.

PERFORM bdc_dynpro      USING   'SAPMOIJTN'                 '0301'.
PERFORM bdc_field       USING 'BDC_OKCODE'                  '/EENTE '.
PERFORM bdc_field       USING   'BDC_SUBSCR'                'SAPMOI JTN'.
PERFORM bdc_field       USING   'BDC_SUBSCR'                'SAPMOI JTN'.
PERFORM bdc_field      USING    'BDC_SUBSCR'                'SAPLOI B_QCI_SUB_SCREEN'.
PERFORM bdc_field      USING    'BDC_CURSOR'                'GT_PAR AM_SCREEN-VALUE(02)'.
IF ls_oijnomi-sityp = 'ZU'.
PERFORM bdc_field    USING    'GT_PARAM_SCREEN-VALUE(01)'    wt_ncv_zu.
PERFORM bdc_field    USING    'GT_PARAM_SCREEN-VALUE(02)'    wt_gcv_zu.
ELSE.
PERFORM bdc_field    USING    'GT_PARAM_SCREEN-VALUE(01)'    wt_ncv_cc.
PERFORM bdc_field    USING    'GT_PARAM_SCREEN-VALUE(02)'    wt_gcv_cc.
ENDIF.

PERFORM bdc_dynpro      USING   'SAPMOIJTN'                 '0301'.
PERFORM bdc_field       USING   'BDC_CURSOR'                'ROIJTI C_IO-TIC_ITEM(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'                  '=IOTAB 1'.
PERFORM bdc_dynpro      USING 'SAPMOIJTN'                   '0301'.
PERFORM bdc_field       USING 'BDC_CURSOR'                  'ROIJTI C_IO-TIC_ITEM(01)'.
PERFORM bdc_field       USING 'BDC_OKCODE'                  '=IT3'.
ENDIF.
l_sr_no = l_sr_no + 1 .
ENDWHILE.

READ TABLE lt_oijnomi_tkt INTO ls_oijnomi INDEX l_item.
IF sy-subrc EQ 0 .
WRITE ls_oijnomi-idate TO l_gas_day_char.
WRITE ls_oijnomi-menge TO w_qty_char.
CONDENSE w_qty_char.
PERFORM bdc_dynpro     USING 'SAPMOIJTN'                      '0301'.
PERFORM bdc_field     USING   'BDC_OKCODE'                  '=IOTAB 4' .
PERFORM bdc_field     USING   'BDC_SUBSCR'                  'SAPMOI JTN'.
PERFORM bdc_field     USING   'BDC_SUBSCR'                  'SAPMOI JTN'.
PERFORM bdc_field       USING   'BDC_CURSOR'                  'ROIJTI C_IO-NET_QUANTITY'.
PERFORM bdc_field       USING   'ROIJTIC_IO-NET_QUANTITY'      w_qty_char.
PERFORM bdc_field       USING   'ROIJTIC_IO-UOM'              'SM3'.

DATA lv_date TYPE char10.
WRITE ls_oijnomi-idate TO lv_date.
PERFORM bdc_field      USING 'ROIJTIC_IO-START_DATE'                l _gas_day_char.
PERFORM bdc_field      USING 'ROIJTIC_IO-START_TIME'               '0 0:00:00'.
PERFORM bdc_field      USING 'ROIJTIC_IO-BUDAT'                     l _gas_day_char.
PERFORM bdc_field      USING 'ROIJTIC_IO-OIB_BLTIME'               '0 0:00:00'.
PERFORM bdc_field      USING 'ROIJTIC_IO-END_DATE'                  l _gas_day_char.
PERFORM bdc_field      USING 'ROIJTIC_IO-END_TIME'                 '0 0:00:00'.

PERFORM bdc_dynpro    USING   'SAPMOIJTN'                        '0 301' .
PERFORM bdc_field       USING   'BDC_OKCODE'                       '= ACTL' .
PERFORM bdc_field       USING   'BDC_SUBSCR'                       'S APMOIJTN'.
PERFORM bdc_field       USING   'BDC_SUBSCR'                       'S APMOIJTN'.
PERFORM bdc_field       USING   'BDC_SUBSCR'                       'S APLOIB_QCI_SUB_SCREEN'.
PERFORM bdc_field       USING   'BDC_CURSOR'                       'G T_PARAM_SCREEN-VALUE(02)'.
IF ls_oijnomi-sityp = 'ZU'.
PERFORM bdc_field     USING   'GT_PARAM_SCREEN-VALUE(01)'         wt_ncv_zu.
PERFORM bdc_field     USING   'GT_PARAM_SCREEN-VALUE(02)'         wt_gcv_zu.
wt_gcv_a = wt_ncv_zu.

ELSE.
PERFORM bdc_field   USING     'GT_PARAM_SCREEN-VALUE(01)'         wt_ncv_cc.
PERFORM bdc_field   USING     'GT_PARAM_SCREEN-VALUE(02)'         wt_gcv_cc.
wt_gcv_a = wt_ncv_cc.
ENDIF.

ENDIF.
DATA messtab LIKE STANDARD TABLE OF bdcmsgcoll WITH HEADER LINE.
CLEAR messtab[].

DATA: lv_char TYPE char1 VALUE 'E'.

DATA: BEGIN OF it_gcv_ncv,
tcode LIKE sy-tcode,
gcv   TYPE yyavg_gcv,
ncv   TYPE yyavg_ncv,

END OF it_gcv_ncv.

*     FREE MEMORY ID 'GCV'.
*     IF LS_OIJNOMI-SITYP = 'ZU'.
*       IT_GCV_NCV-TCODE = 'YRXU007'.
*       IT_GCV_NCV-NCV = WT_NCV_ZU.
*       IT_GCV_NCV-GCV = WT_GCV_ZU.
*    ELSE.
*      IT_GCV_NCV-TCODE = 'YRXU007'.
*      IT_GCV_NCV-NCV = WT_NCV_CC.
*      IT_GCV_NCV-GCV = WT_GCV_CC.
*    ENDIF.
*      EXPORT IT_GCV_NCV TO MEMORY ID 'GCV' .


CALL TRANSACTION 'O4TEN' USING bdcdata MODE lv_char MESSAGES INTO messtab.
REFRESH bdcdata.
DATA: wt_ncv_c TYPE char9.
wt_ncv_c = wt_ncv.
LOOP AT messtab WHERE msgtyp = 'E' OR msgtyp = 'A'.
EXIT.
ENDLOOP.

IF sy-subrc <> 0.
READ TABLE messtab WITH KEY msgtyp = 'S' msgid = 'OD' msgnr = '058 '.
IF sy-subrc = 0.
EXIT.
ENDIF.
WAIT UP TO 2 SECONDS.
DO 10 TIMES.
*Begin of changes by of VIPUL on 20241030 for ATC
SELECT * FROM oij_el_ticket_h INTO i_ticket_header UP TO 1 ROWS
WHERE ticketnr = w_ticket_number ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC
IF sy-subrc = 0.
EXIT.
ELSE.
WAIT UP TO 2 SECONDS.
ENDIF.
ENDDO.
DATA: lv_msg TYPE string.
IF i_ticket_header IS INITIAL.
CONCATENATE 'CHECK NOMINATION' ls_oijnomi-nomtk INTO lv_msg SEPA RATED BY space.
wa_msg-msg = lv_msg.
wa_msg-type = 'ERROR'.
APPEND wa_msg TO it_msg.
CLEAR: wa_msg.
"MESSAGE E001(00) WITH 'CHECK NOMINATION' LS_OIJNOMI-NOMTK.
CALL TRANSACTION 'YRXU007'.
ENDIF.

DATA: wt_ncv_c1     TYPE char15.
DATA: wt_ncv_pp(14) TYPE p DECIMALS 2.
wt_ncv_pp = wt_ncv_p.
WRITE wt_ncv_pp TO wt_ncv_c1.
CONDENSE wt_ncv_c1.
CLEAR sy-binpt.

DATA : lv_msg1 TYPE sy-msgv1,
lv_msg2 TYPE sy-msgv2,
lv_msg3 TYPE string,
lv_msg4 TYPE string.
CLEAR:w_ticket_number1.
w_ticket_number1 = w_ticket_number.
**      BOC 4000007694 TECHNICAL $RAVINDER SINGH FUNCTIONAL SHREYOSI DE ON 21.06.2024.
****      CONCATENATE 'TICKET NO: ' W_TICKET_NUMBER ' ' INTO LV_MSG1 R ESPECTING BLANKS.
****      CONCATENATE ' ' ' CREATED' INTO LV_MSG2 RESPECTING BLANKS..
****      CONCATENATE LV_MSG1 LV_MSG2 INTO LV_MSG3 SEPARATED BY SPACE.
****      CLEAR: WA_MSG.
****      WA_MSG-MSG = LV_MSG3.
****      WA_MSG-TYPE = 'SUCCESS'.
****      APPEND WA_MSG TO IT_MSG..
**      EOC 4000007694 TECHNICAL $RAVINDER SINGH FUNCTIONAL SHREYOSI DE ON 21.06.2024.
CLEAR: wa_msg.
"BOC 4000007694 TECHNICAL ANKUR FUNCTIONAL SHREYOSI DE ON 15.03.20 24.

PERFORM actualize_ticket.

"EOC 4000007694 TECHNICAL ANKUR FUNCTIONAL SHREYOSI DE ON 15.03.20 24.

"MESSAGE I001(00) WITH LV_MSG1 LV_MSG2.
ELSE.
MESSAGE ID messtab-msgid TYPE messtab-msgtyp NUMBER messtab-msgnr WITH
messtab-msgv1 messtab-msgv2 messtab-msgv3 messtab-msgv4.
ENDIF.
ELSE.
"MESSAGE E001(00) WITH 'CHECK NOMINATION' P_NOMTK 'WITH SCHEDULE TYP E ZU AND RDI S'.
CONCATENATE 'CHECK NOMINATION' p_nomtk 'WITH SCHEDULE TYPE ZU AND RD I S' INTO lv_msg4 SEPARATED BY space.
CLEAR: wa_msg.
wa_msg-msg = lv_msg3.
wa_msg-type = 'ERROR'.
APPEND wa_msg TO it_msg.
CLEAR: wa_msg.

ENDIF.

ENDFORM.                      " TICKET_ACTUALIZATE

*                                                                    *
*        INSERT FIELD                                                *
*                                                                    *
FORM bdc_field USING fnam fval.
CLEAR bdcdata.
bdcdata-fnam = fnam.
bdcdata-fval = fval.
APPEND bdcdata.
ENDFORM.                    "BDC_FIELD

*&                                                                   *
*&       FORM BDC_DYNPRO
*&                                                                   *
*        TEXT
*                                                                    *
*         >PROGRAM    TEXT
*         >DYNPRO     TEXT
*                                                                    *
FORM bdc_dynpro USING program dynpro.
CLEAR bdcdata.
bdcdata-program = program.
bdcdata-dynpro   = dynpro.
bdcdata-dynbegin = 'X'.
APPEND bdcdata.
ENDFORM.                     "BDC_DYNPRO
*&                                                                   *
*&       FORM CHECK_EXISTING_TICKET
*&                                                                   *
*        TEXT
*                                                                    *
FORM check_existing_ticket .
DATA: i_oijnomi TYPE TABLE OF oijnomi,
l_oijnomi TYPE oijnomi,
i_tic     TYPE TABLE OF oij_el_ticket_i,
i_tic1    TYPE TABLE OF oij_el_ticket_i,
wa_tic1   TYPE          oij_el_ticket_i.

DATA: it_order_inx TYPE TABLE OF bapisdh1x,
wa_order_inx TYPE bapisdh1x,
it_return    TYPE TABLE OF bapiret2,
wa_return    TYPE bapiret2.





SELECT *
FROM oijnomi INTO TABLE i_oijnomi
WHERE sityp IN ('ZA','ZU','ZJ')
AND idate EQ so_date-high
AND locid EQ p_locid
AND conpat EQ p_kunnr
AND docind NE 'X'
AND delind NE 'X'.
IF i_oijnomi IS NOT INITIAL.
SELECT *
INTO TABLE i_tic
FROM oij_el_ticket_i FOR ALL ENTRIES IN i_oijnomi
WHERE nomtk    = i_oijnomi-nomtk
AND nomit    = i_oijnomi-nomit
AND locid    = i_oijnomi-locid
AND ticket_purpose = 1
AND delind NE 'X'.
IF i_tic IS NOT INITIAL.
SELECT *
INTO TABLE i_tic1
FROM oij_el_ticket_i
FOR ALL ENTRIES IN i_tic
WHERE ticket_key = i_tic-ticket_key
AND ticket_purpose EQ 5.
IF sy-subrc = 0.
LOOP AT i_tic INTO wa_tic1.
*          UPDATE OIJ_EL_TICKET_I
*             SET DELIND = 'X'
*            WHERE TICKET_KEY     = WA_TIC1-TICKET_KEY
*              AND TICKET_ITEM    = WA_TIC1-TICKET_ITEM
*              AND TICKET_VERSION = WA_TIC1-TICKET_VERSION
*              AND TICKET_PURPOSE = WA_TIC1-TICKET_PURPOSE.

UPDATE oijnomi
SET delind = 'X'
WHERE nomtk = wa_tic1-nomtk
AND nomit = wa_tic1-nomit.
UPDATE oijnomh
SET delind = 'X'
WHERE nomtk = wa_tic1-nomtk.
ENDLOOP.

REFRESH: i_tic.",I_OIJNOMI.



ENDIF.

ENDIF.
ENDIF.

READ TABLE i_tic INTO wa_tic1 WITH KEY ticket_purpose = 1 status = 'C ' substatus = '6'.
IF sy-subrc = 0.
MESSAGE TEXT-002 TYPE 'E'.
ELSE.
READ TABLE i_tic INTO wa_tic1 WITH KEY ticket_purpose = 1 status = 'C' substatus = 'A'.
IF sy-subrc = 0.
MESSAGE TEXT-002 TYPE 'E'.
ELSE.
LOOP AT i_tic INTO wa_tic1.
UPDATE oij_el_ticket_i
SET delind = 'X'
WHERE ticket_key     = wa_tic1-ticket_key
AND ticket_item    = wa_tic1-ticket_item
AND ticket_version = wa_tic1-ticket_version
AND ticket_purpose = wa_tic1-ticket_purpose.

UPDATE oijnomi
SET delind = 'X'
WHERE nomtk = wa_tic1-nomtk
AND nomit = wa_tic1-nomit.
UPDATE oijnomh
SET delind = 'X'
WHERE nomtk = wa_tic1-nomtk.
ENDLOOP.
ENDIF.
ENDIF.

COMMIT WORK AND WAIT.
LOOP AT i_oijnomi INTO l_oijnomi WHERE sityp = 'ZU' AND docnr IS NOT I NITIAL.
wa_order_inx-updateflag = 'D'.
*    APPEND WA_ORDER_INX TO IT_ORDER_INX.
** -> BEGIN OF CHANGES BY KRITIKA ON 18.10.2024 FOR ATC
CALL FUNCTION 'BAPI_SALESORDER_CHANGE' "#EC CI_USAGE_OK[2438131]
** <- END OF CHANGES BY KRITIKA ON 18.10.2024 FOR ATC
EXPORTING
salesdocument     = l_oijnomi-docnr
*        ORDER_HEADER_IN = ORDER_HEADER_IN
order_header_inx = wa_order_inx
*        SIMULATION        = SIMULATION
*        BEHAVE_WHEN_ERROR            = ' '
*        INT_NUMBER_ASSIGNMENT        = ' '
*        LOGIC_SWITCH      = LOGIC_SWITCH
*        NO_STATUS_BUF_INIT           = ' '
TABLES
return            = it_return
*        ORDER_ITEM_IN     = ORDER_ITEM_IN
*        ORDER_ITEM_INX    = ORDER_ITEM_INX
*        PARTNERS          = PARTNERS
*        PARTNERCHANGES    = PARTNERCHANGES
*        PARTNERADDRESSES = PARTNERADDRESSES
*        ORDER_CFGS_REF    = ORDER_CFGS_REF
*        ORDER_CFGS_INST = ORDER_CFGS_INST
*        ORDER_CFGS_PART_OF           = ORDER_CFGS_PART_OF
*        ORDER_CFGS_VALUE = ORDER_CFGS_VALUE
*        ORDER_CFGS_BLOB = ORDER_CFGS_BLOB
*        ORDER_CFGS_VK     = ORDER_CFGS_VK
*        ORDER_CFGS_REFINST           = ORDER_CFGS_REFINST
*        SCHEDULE_LINES    = SCHEDULE_LINES
*        SCHEDULE_LINESX = SCHEDULE_LINESX
*        ORDER_TEXT        = ORDER_TEXT
*        ORDER_KEYS        = ORDER_KEYS
*        CONDITIONS_IN     = CONDITIONS_IN
*        CONDITIONS_INX    = CONDITIONS_INX
*        EXTENSIONIN       = EXTENSIONIN
.
READ TABLE it_return INTO wa_return WITH KEY type = 'E'.
IF sy-subrc <> 0.
CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
* EXPORTING
*   WAIT            = WAIT
* IMPORTING
*   RETURN          = RETURN
.
ELSE.
MESSAGE wa_return-message TYPE 'E'.

ENDIF.

REFRESH it_return.
ENDLOOP.

ENDFORM.                      " CHECK_EXISTING_TICKET
*&                                                                     *
*&       FORM ERROR_ALV
*&                                                              *
*        TEXT
*                                                               *
*     > P1         TEXT
* <      P2        TEXT
*                                                               *
FORM error_alv .
DATA:i_layout TYPE slis_layout_alv.
CLEAR: wa_fcat.
REFRESH:it_fcat[].

CLEAR wa_fcat.
wa_fcat-col_pos   = 1.
wa_fcat-fieldname = 'MSG'.
wa_fcat-tabname   = 'IT_MSG'.
wa_fcat-seltext_l = 'MESSAGE'.
wa_fcat-outputlen = 50.
APPEND wa_fcat TO it_fcat.

CLEAR wa_fcat.
wa_fcat-col_pos   = 2.
wa_fcat-fieldname = 'TYPE'.
wa_fcat-tabname   = 'IT_MSG'.
wa_fcat-seltext_l = 'TYPE'.
wa_fcat-outputlen = 50.
APPEND wa_fcat TO it_fcat.


IF it_msg IS NOT INITIAL.

DATA: v_repid1 TYPE sy-repid.
i_layout-colwidth_optimize = 'X'.
i_layout-zebra = 'X'.

CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
EXPORTING
*         I_INTERFACE_CHECK = ' '
*         I_BYPASSING_BUFFER = ' '
*         I_BUFFER_ACTIVE    = ' '
i_callback_program = v_repid1
*         I_CALLBACK_PF_STATUS_SET          = ' '
*         I_CALLBACK_USER_COMMAND           = ' '
*         I_CALLBACK_TOP_OF_PAGE            = ' '
*         I_CALLBACK_HTML_TOP_OF_PAGE       = ' '
*         I_CALLBACK_HTML_END_OF_LIST       = ' '
*         I_STRUCTURE_NAME   =
*         I_BACKGROUND_ID    = ' '
i_grid_title       = 'MESSAGE SCREEN'
*         I_GRID_SETTINGS    =
is_layout          = i_layout
it_fieldcat        = it_fcat
*         IT_EXCLUDING       =
*         IT_SPECIAL_GROUPS =
*         IT_SORT            =
*         IT_FILTER          =
*         IS_SEL_HIDE        =
*         I_DEFAULT          = 'X'
i_save             = 'X'
*         IS_VARIANT         =
*         IT_EVENTS          =
*          IT_EVENT_EXIT       =
*          IS_PRINT            =
*          IS_REPREP_ID        =
*          I_SCREEN_START_COLUMN              = 0
*          I_SCREEN_START_LINE                = 0
*          I_SCREEN_END_COLUMN                = 0
*          I_SCREEN_END_LINE = 0
*          I_HTML_HEIGHT_TOP = 0
*          I_HTML_HEIGHT_END = 0
*          IT_ALV_GRAPHICS     =
*          IT_HYPERLINK        =
*          IT_ADD_FIELDCAT     =
*          IT_EXCEPT_QINFO     =
*          IR_SALV_FULLSCREEN_ADAPTER         =
*       IMPORTING
*          E_EXIT_CAUSED_BY_CALLER            =
*          ES_EXIT_CAUSED_BY_USER             =
TABLES
t_outtab            = it_msg[]
EXCEPTIONS
program_error       = 1
OTHERS              = 2.
.
IF sy-subrc <> 0.
* IMPLEMENT SUITABLE ERROR HANDLING HERE
ENDIF.

LEAVE TO CURRENT TRANSACTION.

ENDIF.


ENDFORM.
*&                                                                     *
*&       FORM ACTUALIZE_TICKET
*&                                                                     *
*        TEXT
*                                                                      *
*    > P1         TEXT
* <     P2        TEXT
*                                                                      *
FORM actualize_ticket .

CLEAR: messtab[].
WAIT UP TO 2 SECONDS.
DATA l_config TYPE kzkfg.
REFRESH messtab.

CALL FUNCTION 'YRX_TICKET_ACTUAL'
EXPORTING
ticketnr   = w_ticket_number1
w_ncv_char = wt_gcv_a
TABLES
it_message = messtab.


LOOP AT messtab INTO wa_message WHERE msgtyp = 'E'..
CLEAR wa_message.
wa_message-id     = wa_msg1-msgid.
wa_message-number = wa_msg1-msgnr.
wa_message-message = wa_msg1-msgv1.
APPEND wa_message TO it_message.
ENDLOOP.

DATA it_oij_el_ticket_i TYPE TABLE OF oij_el_ticket_i.
DATA wa_oij_el_ticket_i TYPE oij_el_ticket_i.

REFRESH it_oij_el_ticket_i.

*Begin of changes by of VIPUL on 20241030 for ATC
SELECT ticket_key INTO w_ticket_nr FROM oij_el_ticket_h
UP TO 1 ROWS
WHERE ticketnr = w_ticket_number1 ORDER BY PRIMARY KEY .
ENDSELECT.
* *End of changes by of VIPUL on 20241030 for ATC

SELECT * INTO TABLE it_oij_el_ticket_i FROM oij_el_ticket_i
WHERE
ticket_key = w_ticket_nr.

LOOP AT it_oij_el_ticket_i INTO wa_oij_el_ticket_i.
IF wa_oij_el_ticket_i-status = 'C' AND wa_oij_el_ticket_i-substatus = '6'.
CLEAR wa_message.
wa_message-id = 'I'.
wa_message-number = '001'.
wa_message-message = 'TICKET ACTUALIZED COMPLETELY WITH NO ERRORS'.
APPEND wa_message TO it_message.
*       *    SOC CHARM ID :4000007694    TECHICAL : RAVINDER SINGH FUNCT IONAL : SHREYOSHI DT:13.08.2024
SHIFT wa_oij_el_ticket_i-ticket_key LEFT DELETING LEADING '0'.
SHIFT wa_oij_el_ticket_i-ticket_item LEFT DELETING LEADING '0'.
wa_msg-msg = 'TICKET (TICKET NO: ' && w_ticket_number1 && ' TICKET KEY: ' && wa_oij_el_ticket_i-ticket_key
&& ' TICKET ITEM: ' && wa_oij_el_ticket_i-ticket_item && ') ACTUALIZED COMPLETELY WITH NO ERRORS.'.
wa_msg-type = 'SUCCESS'.
APPEND wa_msg TO it_msg.
*       *    EOC CHARM ID :4000007694    TECHICAL : RAVINDER SINGH FUNCT IONAL : SHREYOSHI DT:13.08.2024

ENDIF.
IF wa_oij_el_ticket_i-status = 'C' AND wa_oij_el_ticket_i-substatus <> '6'.
CLEAR wa_message.
wa_message-id = 'E'.
wa_message-number = '001'.

CONCATENATE w_ticket_number1 'IS PARTIALLY ACTUALIZED WITH ERRORS'INTO wa_message-message
SEPARATED BY space.
APPEND wa_message TO it_message.
*    SOC CHARM ID :4000007694    TECHICAL : RAVINDER SINGH FUNCTIONAL : SHREYOSHI DT:21.06.2024
SHIFT wa_oij_el_ticket_i-ticket_key LEFT DELETING LEADING '0'.
SHIFT wa_oij_el_ticket_i-ticket_item LEFT DELETING LEADING '0'.
wa_msg-msg = ' TICKET (TICKET NO: ' && w_ticket_number1 && ' TICKE T KEY: ' && wa_oij_el_ticket_i-ticket_key
&& ' TICKET ITEM: ' && wa_oij_el_ticket_i-ticket_item && ') PARTIALLY ACTUALIZED WITH ERRORS'.
wa_msg-type = 'ERROR'.
APPEND wa_msg TO it_msg.
*      **      EOC 4000007694 TECHNICAL $RAVINDER SINGH FUNCTIONAL SHREY OSI DE ON 21.06.2024.
ENDIF.

ENDLOOP.


ENDFORM.
