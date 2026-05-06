*&---------------------------------------------------------------------*
*& Report  ZFIVMS_BRD_SEL_FILEGEN_DIGSIGN
*&---------------------------------------------------------------------*
*&  OLD Report ZFIVMS_BRD_FILEGEN
*&
*&  Sudhir Sharma <RD1K968710>                  Date 06-10-10
*&  Sudhir Sharma <RD1K976705> CR No.30005915   Date 13-06-11
*&  Lipsy Swain   <RD1K983162>                  Date 07-01-2013
*&
*&  New Report ZFIVMS_BRD_SEL_FILEGEN_DIGSIGN
*&  Author      : Alok kumar Singh
*&  Title       : SBI Composite Beneficiary File registration/de-reg. process
*&  Description : Program handles SBI Composite Beneficiary file
*&                generation, digital signing and File Transfer.
*&                1. Previously used 4 formats whereby 4 type of files for each
*&                   payment center was being generated
*&                   (old Prog:ZFIVMS_BRD_SEL_FILEGEN), have been discarded.
*&                   Now all the data is transferred in the new composite format,
*&                   so now only 1 file for each payment center will be generated.
*&                2. Program signs the data with Digital signature of the user
*&                3. Transfers data through PI to SBI
*&  CR: 30010502
*&  TP: RD1K991769
*&---------------------------------------------------------------------*
***********************************************************************
* CHANGE HISTORY
*
* Mod Date    CR No.       Changed by   Description                   Chng ID
* 21.10.2016  30014976     CAB_RAMA     SPAN key embeds sy-datum
***start of ONGC code
* xx.xx.xxxx  30016236     CAB_RAMA     SBIN exception set added
* 20.11.2018  6000000474   (trust)      ECPF payment center added
* 28.11.2022  RD1K9A0Q5P  (support)    S/4 HANA sort fix
***end of ONGC code
***********************************************************************
REPORT  ZFIVMS_BRD_SEL_FILEGEN_DIGSIGN.

TABLES : ZFIVMS_BRD , pa0001, pa0006 , pa0009 , pa9205 , zfi_path.

************************************************************************
* Begin RD1K991769 CAB_ALOK   CR 30010502
types: begin of TY_record,            " Old record
  record(200) ,
end of TY_record.
types: begin of TY_record_new,        " concatenated record in NEW_FORMAT, max length > 200
  record(210) ,
end of TY_record_new.
types: begin of TY_NEW_FORMAT,        "length 194 + 8 separators +EoL
  BEN_TYPE(1),            "Beneficiary Type * 1
  BEN_ACT_TYPE(1),        "Beneficiary Action Type * 1
  BEN_NAME(35),           "Beneficiary Name * 35
***start of ONGC code
  BEN_AC_NO(38),          "Beneficiary Account Number * 33//17 (expanded from 17)
***end of ONGC code
  BEN_CODE(24),           "Beneficiary Code 20 (back to 24)
  IFSC(11),               "IFS Code# 11
  ADDR1(35),              "Address1 35
  ADDR2(35),              "Address2 35
  ADDR3(35),              "Address3 35
end of TY_NEW_FORMAT.

data: IST_94427      TYPE STANDARD TABLE OF TY_NEW_FORMAT.  "cvp-mum
data: IST_93128      TYPE STANDARD TABLE OF TY_NEW_FORMAT.  "cep-ddn
data: IST_30523      TYPE STANDARD TABLE OF TY_NEW_FORMAT.  "ovl
data: IST_94427_FILE TYPE STANDARD TABLE OF TY_record_new.  "cvp
data: IST_93128_FILE TYPE STANDARD TABLE OF TY_record_new.  "ddn
data: IST_30523_FILE TYPE STANDARD TABLE OF TY_record_new.  "ovl
***start of ONGC code
**** START CR 6000000474   ON 20.11.18
data: IST_96726      TYPE STANDARD TABLE OF TY_NEW_FORMAT.  "ECPF
data: IST_96726_FILE TYPE STANDARD TABLE OF TY_record_new.  "ECPF
***END
***end of ONGC code
data: trans_string type string.
* End RD1K991769 CAB_ALOK   CR 30010502
***********************************************************************
data : begin of ist_rdata_sbi occurs 0,
  span(200),
*  koinh_new type zfivmsbank-koinh_new,
*  bankn_new type zfivmsbank-bankn_new,
*  bankl_new type zfivmsbank-bankl_new,
end of ist_rdata_sbi.
data : begin of ist_rdata_nonsbi occurs 0,
  span(200),
end of ist_rdata_nonsbi.
data : begin of ist_ddata_sbi occurs 0,
  span(200),
end of ist_ddata_sbi.
data : begin of ist_ddata_nonsbi occurs 0,
  span(200),
end of ist_ddata_nonsbi.
* Begin of < > on 02052011
data : begin of ist_rdata_sbi_ovl occurs 0,
  span(200),
end of ist_rdata_sbi_ovl.
data : begin of ist_rdata_nonsbi_ovl occurs 0,
  span(200),
end of ist_rdata_nonsbi_ovl.
data : begin of ist_ddata_sbi_ovl occurs 0,
  span(200),
end of ist_ddata_sbi_ovl.
data : begin of ist_ddata_nonsbi_ovl occurs 0,
  span(200),
end of ist_ddata_nonsbi_ovl.
* End of <> on 02052011
* Begin of < > on 16072012
data : begin of ist_rdata_sbi_s occurs 0,
  span(200),
end of ist_rdata_sbi_s.
data : begin of ist_ddata_sbi_s occurs 0,
  span(200),
end of ist_ddata_sbi_s.
data : begin of ist_rdata_nonsbi_s occurs 0,
  span(200),
end of ist_rdata_nonsbi_s.
data : begin of ist_ddata_nonsbi_s occurs 0,
  span(200),
end of ist_ddata_nonsbi_s.
* End of <> on 16072012
* Begin of < > on 28062011
data : begin of ist_rdata_sbi_ovl_e occurs 0,
  span(200),
end of ist_rdata_sbi_ovl_e.
data : begin of ist_ddata_sbi_ovl_e occurs 0,
  span(200),
end of ist_ddata_sbi_ovl_e.
data : begin of ist_rdata_nonsbi_ovl_e occurs 0,
  span(200),
end of ist_rdata_nonsbi_ovl_e.
data : begin of ist_ddata_nonsbi_ovl_e occurs 0,
  span(200),
end of ist_ddata_nonsbi_ovl_e.
* End of <> on 28062011
data : begin of ist_rdata_sbi_e occurs 0,
  span(200),
*  koinh_new type zfivmsbank-koinh_new,
*  bankn_new type zfivmsbank-bankn_new,
*  bankl_new type zfivmsbank-bankl_new,
end of ist_rdata_sbi_e.
data : begin of ist_rdata_nonsbi_e occurs 0,
  span(200),
end of ist_rdata_nonsbi_e.
data : begin of ist_ddata_sbi_e occurs 0,
  span(200),
end of ist_ddata_sbi_e.
data : begin of ist_ddata_nonsbi_e occurs 0,
  span(200),
end of ist_ddata_nonsbi_e.
data : wa_ddata_sbi    like ist_ddata_sbi,
       wa_ddata_nonsbi like ist_ddata_nonsbi,
       g_benf.
* Begin of <> on 10062011
data : ist_zfi_path type table of zfi_path,
       wa_path      type zfi_path.
* End of <> on 10062011
* Begin of <RD1K977143> on 14072011
data : g_filegen.
* End of <RD1K977143> on 14072011
data : ist_zfivms_resend   type table of zfivms_resend,
       wa_zfivms_resend    type zfivms_resend,
       ist_zfivms_brd      type table of zfivms_brd,
       wa_zfivms_brd       type zfivms_brd,
       ist_zfivms_brd1     type table of zfivms_brd,     "21062011
       ist_zfivms_brdt     type table of zfivms_brd,     "21062011
       ist_zfivms_brdtmp   type table of zfivms_brd,     "21062011
       ist_zfivms_brdtmp1  type table of zfivms_brd,     "21062011
       ist_zsdcust_bankupd type table of zfivms_brd,     "added by lipsy on 27.09.2012
       wa_zfivms_brdtmp    type zfivms_brd,              "21062011
       wa_zfivms_brdt      type zfivms_brd,              "21062011
       wa_zfivms_brd1      type zfivms_brd,              "21062011
       ist_pa0009          type table of pa0009,
       wa_pa0009           type pa0009,
       ist_pa0009as        type table of pa0009,
       ist_pa0009as1       type table of pa0009,
       wa_pa0009as         type pa0009,
       wa_pa0009t          type pa0009,
       ist_pa0006          type table of pa0006,
       wa_pa0006           type pa0006,
       wa_pa0001           type pa0001,
       wa_pa9205           type pa9205,
       g_date_rd(10),
       g_time_rd(06),
       l_rsbi    type i,
       l_dsbi    type i,
       l_stat    type i,
       g_filename1 like rlgrap-filename,
       g_filename  type string,
       g_fsnam     type zfi_path-fsnam,
       g_corpid    type zfi_path-corpid.
* Begin of <> on 18052012
data : l_spanemp(17),
       l_spanasa(24),
       l_span(7),
       l_strlen type n.
* End of <> on 18052012
data : valid_characters_as type string,
       valid_input_as      type string.

**Begin RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing
data g_cancel_pressed(1).
data: ok_code2000      TYPE sy-ucomm.
DATA: G_PASSWD_INPUT   TYPE SSFINFO-PASSWORD.
DATA  G_PASSWORD       TYPE SSFINFO-PASSWORD.
data ist_pa0009_BAK    type table of pa0009.
DATA: IST_pa0009_OVL   type table of pa0009,
      IST_pa0009_ONGC  type table of pa0009.
**End RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing

selection-screen begin of block rad1 with frame title text-001.
PARAMETERS : p_date   type zfivmsbank-VMC_APDATE.
* Begin of <> on 09052012
Select-OPTIONS : s_span for zfivms_brd-span.
* End of <> on 09052012
PARAMETERS : p_payctr type zpay_ctr OBLIGATORY.
selection-screen end of block rad1.

START-OF-SELECTION.
**Begin RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing
  PERFORM AUTH_CHECK.
  PERFORM SET_TRANS.
**End RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing
  perform check_req_value.
  PERFORM EXTRACT_DATA.
**Begin RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing
* PERFORM GENERATE_FILE.
  PERFORM GENERATE_NEW_FORMAT.
  PERFORM MERGE_FILE.
  PERFORM PROCESS.
  PERFORM CLEAR_VAR.
**End RD1K991769 CAB_ALOK   CR 30010502 Beneficiary file-digital signing

* NOTE: INCLUDE zbcm_class - keep as INCLUDE (system-level class, not part of this program)
INCLUDE zbcm_class.
* NOTE: INCLUDE zbcm_class_declare - keep as INCLUDE (system-level class, not part of this program)
INCLUDE zbcm_class_declare.
*&---------------------------------------------------------------------*
*& Inlined from INCLUDE zfivms_brd_sel_filegen_digso01
*&---------------------------------------------------------------------*
*& Module STATUS_0100 OUTPUT
*&---------------------------------------------------------------------*
MODULE STATUS_0100 OUTPUT.
  " No interactive screen - batch program
ENDMODULE.

*&---------------------------------------------------------------------*
*& Module USER_COMMAND_0100 INPUT
*&---------------------------------------------------------------------*
MODULE USER_COMMAND_0100 INPUT.
  " No interactive screen - batch program
ENDMODULE.

*&---------------------------------------------------------------------*
*& Inlined from INCLUDE zfivms_brd_sel_filegen_digsi01
*&---------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.

PARAMETERS:
  p_date   TYPE sy-datum OBLIGATORY DEFAULT sy-datum,
  p_payctr TYPE zpayctr  OBLIGATORY.

SELECTION-SCREEN END OF BLOCK b1.

SELECTION-SCREEN BEGIN OF BLOCK b2 WITH FRAME TITLE text-002.

PARAMETERS:
  p_test   TYPE c AS CHECKBOX DEFAULT ' '.

SELECTION-SCREEN END OF BLOCK b2.

*&---------------------------------------------------------------------*
*& AT SELECTION-SCREEN
*&---------------------------------------------------------------------*
AT SELECTION-SCREEN.

  AUTHORITY-CHECK OBJECT 'ZFIBENREG'
    FIELD 'ZPAY_CTR' ID 'ZPAY_CTR' FIELD p_payctr.
  IF sy-subrc <> 0.
    MESSAGE e001(zfi) WITH p_payctr.
  ENDIF.

*&---------------------------------------------------------------------*
*& Inlined from INCLUDE zfivms_brd_sel_filegen_digsf01
*&---------------------------------------------------------------------*
*& Form AUTH_CHECK
*&---------------------------------------------------------------------*
form auth_check .
* Check auth
* tcode: ZFIBENFILEGEN
*roles
*1. D:FI_FILEGEN_CVP
*2. D:FI_FILEGEN_CEP
*3. D:FI_FILEGEN_OVL
*
Object
:ZFIBENREG
*
Field :ZPAY_CTR (03,16)
*
Value :CVP / CEP / OVL

*
*
*
*

authority-check object 'ZFIBENREG'
id 'ZPAY_CTR' field p_payctr
id 'ACTVT' field '16'.
if sy-subrc <> 0.
message 'No authorization' type 'E'.
endif.
endform.

*&---------------------------------------------------------------------*
*& Form SET_TRANS
*&---------------------------------------------------------------------*
form set_trans .
concatenate ''' ~ ! ` ! @ # $ % ^ & * ( ) _ - '
/ , . < > ' into trans_string respecting blanks.
endform.

*&---------------------------------------------------------------------*
*& Form CHECK_REQ_VALUE
*&---------------------------------------------------------------------*
FORM CHECK_REQ_VALUE .
if p_date is initial and s_span is initial.
message e434(zfi).
endif.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EXTRACT_DATA
*&---------------------------------------------------------------------*
FORM EXTRACT_DATA .
data : l_begdate
type pa0009-begda,
l_begdateas type pa0009-begda,
l_strlen type n,
l_line(2),
l_enddate type pa0009-endda value '99991231',
l_endda(10),
l_begda(10),
l_tabix type sy-tabix,
"20062011
l_bukrs type pa0001-bukrs.
"16062011
concatenate 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
'abcdefghijklmnopqrstuvwxyz' into valid_characters_as.
* Begin of <> on 01022011
*
*

*
*
*
*

CONCATENATE p_date(4) p_date(4) p_date(4) into l_begda.
write p_date to p_date YYMMDD.

**Begin RD1K991769 CAB_ALOK
* select * from pa0009 into table ist_pa0009as where aedtm = p_date and
endda = l_enddate "p_date
*
and subty = '0'.
select * from pa0009

into table ist_pa0009as
where subty = '0'
and endda = l_enddate
and aedtm = p_date
and flag1 = ''.
*
and FLAG2 = ''.
**End RD1K991769 CAB_ALOK
* if sy-subrc = 0.
*
append ist_pa0009as to ist_pa0009.
*
sort ist_pa0009as descending by pernr.
*
read table ist_pa0009as into wa_pa0009as with key
*
l_begdateas = wa_pa0009as-begda - 1.
*
select * from pa0009 into table ist_pa0009as1 where endda = l_be
*
and subty = '0'.
*
endif.
* Begin of <> on 16072012
ist_pa0009[] = ist_pa0009as[].
**Begin RD1K991769 CAB_ALOK
ist_pa0009_BAK[] = ist_pa0009as[].
**End RD1K991769 CAB_ALOK

* select * from pa0009 into table ist_pa0009 where begda = p_date and e
ndda = l_enddate "'31.12.9999' "p_date
*
and subty = '0'." and zl
sch in ('S','N','8','9'). "20062011 20062011
*
** if sy-subrc = 0.
"16022012
** Begin of <> on 291211
*
* loop at ist_pa0009as into wa_pa0009as.
*
append wa_pa0009as to ist_pa0009 ." from . "index sy-tabix.
* endloop.
* End of <> on 16072012
* End of <> on 291211
* Begin of <> on 16062011 To take OVL employee data also
**Begin RD1K991769 CAB_ALOK
clear wa_pa0009 .
**End RD1K991769 CAB_ALOK
loop at ist_pa0009 into wa_pa0009.
l_tabix = sy-tabix.
select single bukrs into l_bukrs from pa0001 where pernr = wa_pa0009
-pernr
and begda <= p_date a
nd endda >= p_date . "p_date
if sy-subrc = 0.
*
loop at ist_pa0009 into wa_pa0009.
* End of <> on 16062011
if l_bukrs = 'OVL'.
perform get_ovl_emlpoyee_details.
***start of ONGC code
elseif l_bukrs = 'OGL'.
CONTINUE.
***end of ONGC code

else.
perform get_ongc_emlpoyee_details.
endif.
endif.
clear wa_pa0009.
endloop.
* endif.
* End of <>

"21062011

* Begin of <> on 231110
select * from zfivms_resend into table ist_zfivms_resend where status
= 'DEREGISTRATION FAILURE'.
* End of <>
* select * from zfivms_brd into table ist_zfivms_brd where vmc_apdate =
p_date and status = 'APPROVED BY VMC'.
* Begin of <> on 09052012
if not p_date is initial and not s_span is initial.
""""""""
"added by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
( vmc_apdate EQ p_date and span in s_span )
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 04.01.2013 for file generation even if
customer bank data is not there RD1K983162
""""""""""
select * from zfivms_brd into table ist_zfivms_brd where ( vmc_apda
te EQ p_date and span in s_span )
and status in ('RELEASE BY VMC' , 'RELEASE BY EMPLOYEE' , 'RELEASE B
Y VMC FOR OVL', 'RELEASE BY EMPLOYEE FOR OVL').
"commented by lipsy on 04.01.2013 for file generation even if cus
bank data is not there RD1K983162
"added by lipsy on 27.09.2012
*
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
( vmc_apdate = p_date and span in s_span )
*
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 27.09.2012
"end of comment by lipsy on 04.01.2013 for file generation even if
customer bank data is not there RD1K983162

elseif not p_date is initial.
"""""""""
"added by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
vmc_apdate = p_date
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 04.01.2013 for file generation even i
f customer bank data is not there RD1K983162
""""""""
select * from zfivms_brd into table ist_zfivms_brd where vmc_apdate
= p_date
and status in ('RELEASE BY VMC' , 'RELEASE BY EMPLOYEE' , 'RELEASE BY
VMC FOR OVL', 'RELEASE BY EMPLOYEE FOR OVL').
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162

"added by lipsy on 27.09.2012
*
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
( vmc_apdate = p_date and span in s_span )
*
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 27.09.2012
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
elseif not s_span is initial.
"""""""""
"added by lipsy on 04.01.2013 for file generation even if customer ban
k data is not there RD1K983162
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
s
pan in s_span
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 04.01.2013 for file generation even if cus
tomer bank data is not there RD1K983162
""""""""
"
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
""""added by lipsy on 28.12.2012 RD1K983099
"added by lipsy on 27.09.2012
*
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
( vmc_apdate = p_date and span in s_span )
*
and status in ('RELEASE BY CMC').
"end of addition by lipsy on 27.09.2012
"""""end of addition by lipsy on 28.12.2012 RD1K983099
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
""""""""""
select * from zfivms_brd into table ist_zfivms_brd where span in s_
span
and status in ('RELEASE BY VMC' , 'RELEASE BY EMPLOYEE' , 'RELEASE BY
VMC FOR OVL', 'RELEASE BY EMPLOYEE FOR OVL').
"commented by lipsy on 28.12.2012 RD1K983099
*"added by lipsy on 27.09.2012
*
select * from zsdcust_bankupd into table ist_zsdcust_bankupd where
( vmc_apdate = p_date and span in s_span )
*
and status in ('RELEASE BY CMC').
*"end of addition by lipsy on 27.09.2012
"commented by lipsy on 28.12.2012 RD1K983099
endif.
* End of <> on 09052012
*
status in ('APPROVED BY VMC' , 'APPROVED BY EMPLOYEE' , 'APPROVED BY
VMC FOR OVL', 'APPROVED BY EMPLOYEE FOR OVL'). 290911
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
* if sy-subrc = 0.
"commented by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
"""""""""""
"added by lipsy on 04.01.2013 for file generation even if customer
bank data is not there RD1K983162
if ist_zfivms_brd is not INITIAL or ist_zsdcust_bankupd is NOT INITI

AL.
"end of addition by lipsy on 04.01.2013 for file generation even i
f customer bank data is not there RD1K983162
""""""""
* Begin of <> on 21062011
sort ist_zfivms_brd DESCENDING by span .
ist_zfivms_brd1[] = ist_zfivms_brd[].
***start of ONGC code
***S/4 Start of Change
Support # TR RD1K9A0Q5P # 2022/11/28
sort ist_zfivms_brd1 DESCENDING by span .
***S/4 End of Change
Support # TR RD1K9A0Q5P # 2022/11/28
***end of ONGC code
Delete ADJACENT DUPLICATES FROM ist_zfivms_brd1 comparing span.
loop at ist_zfivms_brd1 into wa_zfivms_brd1.
ist_zfivms_brdt[] = ist_zfivms_brd[].
loop at ist_zfivms_brdt into wa_zfivms_brdt where span = wa_zfivm
s_brd1-span.
append wa_zfivms_brdt to ist_zfivms_brdtmp .
endloop.
*
delete ist_zfivms_brdt where span <> wa_zfivms_brd1-span.
if sy-subrc = 0.
clear l_line.
describe table ist_zfivms_brdtmp lines l_line.
shift l_line left deleting leading '0'.
"16072012
if l_line > 0.
read table ist_zfivms_brdtmp into wa_zfivms_brdtmp with key se
qno = l_line.
if sy-subrc <> 0.
read table ist_zfivms_brdtmp into wa_zfivms_brdtmp index 1.
endif.
endif.
*
sort ist_zfivms_brdtmp ascending by seqno span.
*
read table ist_zfivms_brdtmp into wa_zfivms_brdtmp index 1.
append wa_zfivms_brdtmp to ist_zfivms_brdtmp1.
*
modify ist_zfivms_brdtmp1 from wa_zfivms_brdtmp.
refresh ist_zfivms_brdtmp.
endif.
endloop.
refresh ist_zfivms_brd.
ist_zfivms_brd[] = ist_zfivms_brdtmp1[].
* End of <> on 21062011
* Begin of <> on 16072012
loop at ist_zfivms_resend into wa_zfivms_resend.
move-corresponding wa_zfivms_resend to wa_zfivms_brd.
append wa_zfivms_brd to ist_zfivms_brd..
*
append lines of ist_zfivms_resend to ist_zfivms_brd.
endloop.
* End of <> on 16072012
"added by lipsy on 27.09.2012
APPEND LINES OF ist_zsdcust_bankupd TO ist_zfivms_brd.
"end of addition by lipsy on 27.09.2012
* Vendor Registration - D
loop at ist_zfivms_brd into wa_zfivms_brd . "where status <> 'APPROV
ED BY EMPLOYEE'.
* Begin of <> on 03122010

**Begin RD1K991769 CAB_ALOK
*
if wa_zfivms_brd-bankl_new(4) = 'SBIN'.
*
valid_input_as = wa_zfivms_brd-bankl_new+6(5).
*
else.
*
valid_input_as = wa_zfivms_brd-bankl_new.
*
endif.
clear valid_input_as.
valid_input_as = wa_zfivms_brd-bankl_new.
**End RD1K991769 CAB_ALOK

* End of <> on 03122010
check valid_input_as is not INITIAL.
"28.08.2014
condense valid_input_as.
* NON SBI
*{
***start of ONGC code
*/..Begin of Change CR : 30016236 by CAB_RAMA
CONSTANTS: g_setclass LIKE setleaf-setclass VALUE '0000',
g_setname LIKE setleaf-setname VALUE 'ZFI_SBIN_EXCEPTION '
.
DATA :
ist_setleaf
LIKE setleaf OCCURS 0 WITH HEADER LINE,
L_SBI_OTH(1) TYPE C.
CLEAR : ist_setleaf, L_SBI_OTH.
REFRESH : ist_setleaf.
SELECT * FROM setleaf
INTO CORRESPONDING FIELDS OF TABLE ist_setleaf
WHERE setclass = g_setclass
AND setname = g_setname.
READ TABLE ist_setleaf WITH KEY setclass = g_setclass
setname = g_setname
valfrom = valid_input_as+0(4).
IF SY-SUBRC = 0.
L_SBI_OTH = 'X'.
ENDIF.
*}END OF CR 30016236
***end of ONGC code
**Begin RD1K991769 CAB_ALOK
*
if valid_input_as ca valid_characters_as.
***start of ONGC code
if valid_input_as+0(4) <> 'SBIN' AND L_SBI_OTH = ''.
***end of ONGC code
**End RD1K991769 CAB_ALOK
if wa_zfivms_brd-span ca 'R'.
* Begin of <> on 18052012
*
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn_
new '*' valid_input_as '*'
*
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zfi
vms_brd-city1 '*' wa_zfivms_brd-tel_number '*' wa_zfivms_brd-span '*' i
nto ist_rdata_nonsbi-span.
* End of <> on 18052012
if wa_zfivms_brd-status = 'RELEASE BY VMC FOR OVL'.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zf
ivms_brd-city1 '*' wa_zfivms_brd-tel_number '*' wa_zfivms_brd-span '*'

into ist_rdata_nonsbi-span.
MOVE-CORRESPONDING ist_rdata_nonsbi to ist_rdata_nonsbi_ovl.
"05052011
append ist_rdata_nonsbi_ovl.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE FOR OVL'. "
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zf
ivms_brd-city1 '*' wa_zfivms_brd-tel_number '*' l_spanasa '*' into ist_
rdata_nonsbi-span.
MOVE-CORRESPONDING ist_rdata_nonsbi to ist_rdata_nonsbi_ovl_
e. "28062011
append ist_rdata_nonsbi_ovl_e.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE'.
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zf
ivms_brd-city1 '*' wa_zfivms_brd-tel_number '*' l_spanasa '*' into ist_
rdata_nonsbi_s-span.
append ist_rdata_nonsbi_s.
elseif wa_zfivms_brd-status = 'RELEASE BY VMC'.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn_
new '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zf
ivms_brd-city1 '*' wa_zfivms_brd-tel_number '*' wa_zfivms_brd-span '*'
into ist_rdata_nonsbi-span.
append ist_rdata_nonsbi.
"added by lipsy on 27.09.2012
elseif wa_zfivms_brd-status = 'RELEASE BY CMC'.
if wa_zfivms_brd-span+13(1) = 'R'."r is registration
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn_
new '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zf
ivms_brd-city1 '*' wa_zfivms_brd-tel_number '*' wa_zfivms_brd-span '*'
into ist_rdata_nonsbi-span.
append ist_rdata_nonsbi.
endif.
"end of addition by lipsy on 27.09.2012
endif.
elseif wa_zfivms_brd-span ca 'D'.
* 18052012
if wa_zfivms_brd-status = 'RELEASE BY VMC FOR OVL'.
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#'
wa_zfivms_brd-koinh_new '#' wa_zfivms_brd-span '#' into ist_ddata_nons
bi-span.
MOVE-CORRESPONDING ist_ddata_nonsbi to ist_ddata_nonsbi_ovl.
"05052011
append ist_ddata_nonsbi_ovl.

elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE FOR OVL'. "
perform add_7x_emp_D.
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#'

wa_zfivms_brd-koinh_new '#' l_spanasa '#' into ist_ddata_nonsbi-span.
MOVE-CORRESPONDING ist_ddata_nonsbi to ist_ddata_nonsbi_ovl_
e. "28062011
append ist_ddata_nonsbi_ovl_e.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE'.
perform add_7x_emp_D.
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#'
wa_zfivms_brd-koinh_new '#' l_spanasa '#' into ist_ddata_nonsbi_s-span
.
append ist_ddata_nonsbi_s.
elseif wa_zfivms_brd-status = 'RELEASE BY VMC'.
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#'
wa_zfivms_brd-koinh_new '#' wa_zfivms_brd-span '#' into ist_ddata_nons
bi-span.
append ist_ddata_nonsbi.
"addition by lipsy on 27.09.2012
elseif wa_zfivms_brd-status = 'RELEASE BY CMC'.
if wa_zfivms_brd-span+13(1) = 'D'."d is for deregistration
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#'
wa_zfivms_brd-koinh_new '#' wa_zfivms_brd-span '#' into ist_ddata_nons
bi-span.
append ist_ddata_nonsbi.
endif.
"end of addition by lipsy on 27.09.2012
endif.
endif.
else.
* SBI
if wa_zfivms_brd-span ca 'R'.
* 18052012
* move-corresponding wa_zfivms_brd to ist_rdata_sbi.
if wa_zfivms_brd-status = 'RELEASE BY VMC FOR OVL'.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*' wa_zfivms_brd-span '*' into ist_rdata_sbispan.
MOVE-CORRESPONDING ist_rdata_sbi to ist_rdata_sbi_ovl. "0505
append ist_rdata_sbi_ovl.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE FOR OVL'. "
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*' l_spanasa '*' into ist_rdata_sbi-span.
MOVE-CORRESPONDING ist_rdata_sbi to ist_rdata_sbi_ovl_e.
append ist_rdata_sbi_ovl_e.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE'.
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*' l_spanasa '*' into ist_rdata_sbi_s-span.
append ist_rdata_sbi_s.
elseif wa_zfivms_brd-status = 'RELEASE BY VMC'.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*' wa_zfivms_brd-span '*' into ist_rdata_sbispan.
append ist_rdata_sbi.

"added by lipsy on 27.09.2012
elseif wa_zfivms_brd-status = 'RELEASE BY CMC'.
if wa_zfivms_brd-span+13(1) = 'R'."r is for registration
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn
_new '*' valid_input_as '*' wa_zfivms_brd-span '*' into ist_rdata_sbispan.
append ist_rdata_sbi.
endif.
"end of addition by lipsy on 27.09.2012
endif.
elseif wa_zfivms_brd-span ca 'D'.
* 18052012
if wa_zfivms_brd-status = 'RELEASE BY VMC FOR OVL'.
concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-b
ankn_new '#' valid_input_as '#' wa_zfivms_brd-span '#' into ist_ddata_s
bi-span.
MOVE-CORRESPONDING ist_ddata_sbi to ist_ddata_sbi_ovl. "0505
append ist_ddata_sbi_ovl.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE FOR OVL'. "
perform add_7x_emp_D.
concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-b
ankn_new '#' valid_input_as '#' l_spanasa '#' into ist_ddata_sbi-span.
MOVE-CORRESPONDING ist_ddata_sbi to ist_ddata_sbi_ovl_e.
append ist_ddata_sbi_ovl_e.
elseif wa_zfivms_brd-status = 'RELEASE BY EMPLOYEE'.
perform add_7x_emp_D.
concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-b
ankn_new '#' valid_input_as '#' l_spanasa '#' into ist_ddata_sbi_s-spa
n.
append ist_ddata_sbi_s.
elseif wa_zfivms_brd-status = 'RELEASE BY VMC'.
concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-b
ankn_new '#' valid_input_as '#' wa_zfivms_brd-span '#' into ist_ddata_s
bi-span.
append ist_ddata_sbi.

n

"added by lipsy on 27.09.2012
elseif wa_zfivms_brd-status = 'RELEASE BY CMC'.
if wa_zfivms_brd-span+13(1) = 'D'."D is for Deregistratio

concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-b
ankn_new '#' valid_input_as '#' wa_zfivms_brd-span '#' into ist_ddata_s
bi-span.
append ist_ddata_sbi.
endif.
"end of addition by lipsy on 27.09.2012
endif.
endif.
endif.
endloop.
* Employee Registration - D
loop at ist_zfivms_brd into wa_zfivms_brd where status = 'RELEASE BY
EMPLOYEE' .

**Begin RD1K991769 CAB_ALOK
*
if wa_zfivms_brd-bankl_new(4) = 'SBIN'.
*
valid_input_as = wa_zfivms_brd-bankl_new+6(5).
*
else.
*
valid_input_as = wa_zfivms_brd-bankl_new.
*
endif.
clear valid_input_as.
valid_input_as = wa_zfivms_brd-bankl_new.
**End RD1K991769 CAB_ALOK
check valid_input_as is not INITIAL.
"28.08.2014
condense valid_input_as.
* NON SBI
**Begin RD1K991769 CAB_ALOK
*
if valid_input_as ca valid_characters_as.
if valid_input_as+0(4) <> 'SBIN'.
**End RD1K991769 CAB_ALOK
if wa_zfivms_brd-span ca 'R'.
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn_n
ew '*' valid_input_as '*'
wa_zfivms_brd-street '*' wa_zfivms_brd-str_suppl1 '*' wa_zfiv
ms_brd-city1 '*' wa_zfivms_brd-tel_number '*' wa_zfivms_brd-span '*' i
nto ist_rdata_nonsbi_e-span.
*

append ist_rdata_nonsbi_e.
elseif wa_zfivms_brd-span ca 'D'.
*
perform add_7x_emp_D.
concatenate wa_zfivms_brd-bankn_new '#' valid_input_as '#' wa
_zfivms_brd-koinh_new '#' wa_zfivms_brd-span '#' into ist_ddata_nonsbi
_e-span.
append ist_ddata_nonsbi_e.
endif.
else.
* SBI
if wa_zfivms_brd-span ca 'R'.
*
perform add_7x_emp.
concatenate wa_zfivms_brd-koinh_new '*' wa_zfivms_brd-bankn_n
ew '*' valid_input_as '*' wa_zfivms_brd-span '*' into ist_rdata_sbi_e-s
pan.
*
move-corresponding wa_zfivms_brd to ist_rdata_sbi.
append ist_rdata_sbi_e.
elseif wa_zfivms_brd-span ca 'D'.
*
perform add_7x_emp_D.
concatenate wa_zfivms_brd-koinh_new '#' '#' wa_zfivms_brd-ban
kn_new '#' valid_input_as '#' wa_zfivms_brd-span '#' into ist_ddata_sbi
_e-span.
append ist_ddata_sbi_e.
endif.
endif.
endloop.
endif.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GENERATE_NEW_FORMAT
*&---------------------------------------------------------------------*
FORM GENERATE_NEW_FORMAT .

*
*
*

refresh: IST_94427 . "cvp-mum
refresh: IST_93128 . "cep-ddn
refresh:IST_30523 . "övl
***start of ONGC code
refresh: IST_96726 . "ECPF
***end of ONGC code
PERFORM vendor_filegen.
PERFORM employee_filegen.
*
*
*

if g_filegen = 'X'.
message s411(zfi).
endif.

refresh : ist_zfivms_brdtmp,ist_ddata_sbi_e,ist_rdata_sbi_e,ist_ddata_
nonsbi_e,ist_rdata_nonsbi_e,ist_zfivms_brd,ist_zfivms_resend.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form VENDOR_FILEGEN
*&---------------------------------------------------------------------*
FORM VENDOR_FILEGEN .
* Data Declaration
DATA : D_MSG_TEXT(50).

*
*
*
*

* SBI
* Begin of <> on 16072012
if not ist_rdata_sbi_s[] is initial.
append lines of ist_rdata_sbi_s to ist_rdata_sbi.
endif.
* End of <> on 16072012
describe table ist_rdata_sbi lines l_rsbi.
if l_rsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <RD1K976705> on 10062011 15062011
* Begin of <> on 24122010
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'MUM'.
if sy-subrc = 0.
concatenate g_fsnam '3P.' g_date_rd '.' g_corpid '.txt' into g_fi

lename1.
endif.
*
concatenate g_fsnam '3P.' g_date_rd '.94427' '.txt' into g_filenam
e1.
"16062011
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/m
um/' '3P.' g_date_rd '.94427' '.txt' into g_filename1. "04052011
* End of <RD1K976705> on 10062011
*
concatenate '/usr/sap/syst-sysid/DigSignApp/data/output/signed/mum/
' '3P.' g_date_rd '.94427' '.txt' into g_filename1. "11042011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' '3P.'
g_date_rd '.94427' '.txt' into g_filename1. "11042011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' '3P'
g_date_rd g_time_rd '.txt' into g_filename1.
* End of <> on 24122010
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CVP, MUM, 3P (SBI - Regis. )
*1=============================================================
IF p_payctr = 'CVP' .
** Opening the File
*
open dataset g_filename1 for output in text mode ENCODING DEFAULT M
ESSAGE D_MSG_TEXT.
** Begin of <> on 04052011
*
IF SY-SUBRC NE 0.
*
WRITE: 'File cannot be opened. Reason:', D_MSG_TEXT.
*
EXIT.
*
ENDIF.
** Transferring Data
** End of <>
*
LOOP AT ist_rdata_sbi.
*
transfer ist_rdata_sbi to g_filename1.
*
g_filegen = 'X'.
*
ENDLOOP.
** Closing the File
*
close dataset g_filename1.

*

if ist_rdata_sbi[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING '3P' '94427' ist_rdata_sbi[] .
PERFORM DO_DS tables ist_rdata_sbi using sbi_filename .
endif.
ENDIF. " p_payctr = 'CVP' .

**End RD1K991769 CAB_ALOK

"""""""""opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"opened commented part on 27.09.2012
* Begin of Comment on 07082012
*
concatenate 'C:\SPAN\' 'R_SBI' g_date_rd g_time_rd '.txt' into g_f
ilename. "21122010 11042011 21062011 05072011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'

*
*
*
**
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*
*

EXPORTING
FILENAME
FILETYPE
WRITE_FIELD_SEPARATOR
TABLES
DATA_TAB
EXCEPTIONS
FILE_WRITE_ERROR
NO_BATCH
GUI_REFUSE_FILETRANSFER
INVALID_TYPE
NO_AUTHORITY
UNKNOWN_ERROR
HEADER_NOT_ALLOWED
SEPARATOR_NOT_ALLOWED
FILESIZE_NOT_ALLOWED
HEADER_TOO_LONG
DP_ERROR_CREATE
DP_ERROR_SEND
DP_ERROR_WRITE
UNKNOWN_DP_ERROR
ACCESS_DENIED
DP_OUT_OF_MEMORY
DISK_FULL
DP_TIMEOUT
FILE_NOT_FOUND
DATAPROVIDER_EXCEPTION
CONTROL_FLUSH_ERROR
OTHERS

= g_filename
= 'ASC'
= '*'
= ist_rdata_sbi
= 1
= 2
= 3
= 4
= 5
= 6
= 7
= 8
= 9
= 10
= 11
= 12
= 13
= 14
= 15
= 16
= 17
= 18
= 19
= 20
= 21
= 22.

IF SY-SUBRC <> 0.
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
ENDIF.

"commented by lipsy on 28.12.2012 <RD1K983099>
"opened and commented by lipsy on 7.01.2013
"end of open 27.09.2012
* End of <> on 07082012
endif.
* Begin of <> on 16072012
if not ist_ddata_sbi_s[] is initial.
append lines of ist_ddata_sbi_s to ist_ddata_sbi.
endif.
* End of <> on 16072012
describe table ist_ddata_sbi lines l_dsbi.
if l_dsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"opened comment on 3.10.2012 for testing by lipsy
* Begin of Comment on 07082012
*
concatenate '/usr/sap/RP1/FIN/epay/send/mum/' 'D3P' g_date_rd g_t
ime_rd '.txt' into g_filename.
*
concatenate 'C:\SPAN\' 'D_SBI' g_date_rd g_time_rd '.txt' into g_f
ilename. "21122010
"11042011 21062011 05072011
*

*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_sbi
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"end of open of comment by lipsy on 3.10.2012
* End of <> on 07082012
* Begin of <> on 10062011
* Begin of <> on 24122010
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'MUM'.
if sy-subrc = 0.
concatenate g_fsnam 'D3P.' g_date_rd '.' g_corpid '.txt' into g_
filename1.
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
mum/' 'D3P.' g_date_rd '.94427' '.txt' into g_filename1. "11042011
* End of <> on 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'D3P.
' g_date_rd '.94427' '.txt' into g_filename1. "11042011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'D3P'
g_date_rd g_time_rd '.txt' into g_filename1.
* End of <> on 24212010
**Begin RD1K991769 CAB_ALOK

** ZVMSFILEGEN_CVP, MUM, D3P (SBI - DeRegis. )
*2=============================================================
IF p_payctr = 'CVP' .
**
**
**
**
**
**

open dataset g_filename1 for output in text mode ENCODING DEFAULT.
lOOP AT ist_ddata_sbi.
transfer ist_ddata_sbi to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_ddata_sbi[] is not INITIAL.

*

perform PREPARE_NEW_FORMAT USING 'D3P' '94427' ist_ddata_sbi[] .
PERFORM DO_DS tables ist_ddata_sbi using sbi_filename .

endif.
ENDIF. " p_payctr = 'CVP' .
**End RD1K991769 CAB_ALOK
loop at ist_ddata_sbi into wa_ddata_sbi.
split wa_ddata_sbi at '#' into : wa_zfivms_brd-koinh_new g_benf
wa_zfivms_brd-bankn_new wa_zfivms_brd-bankl_new wa_zfivms_brd-span.
*
split wa_ddata_sbi at '#' into : g_koinh g_benf g_bankn g_bankl
g_status g_remarks g_span.
update zfivms_resend set status = 'SENT TO SBI'
where span = wa_zfivms_brd-span.
endloop.
endif.
* NON SBI
clear : l_rsbi , l_dsbi.
* Begin of <> on 16072012
if not ist_rdata_nonsbi_s[] is initial.
append lines of ist_rdata_nonsbi_s to ist_rdata_nonsbi.
endif.
* End of <> on 16072012
describe table ist_rdata_nonsbi lines l_rsbi.
if l_rsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
* Begin of <> on 24122010
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'MUM'.
concatenate g_fsnam 'IBTP.' g_date_rd '.' g_corpid '.txt' into g_f
ilename1.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
mum/' 'IBTP.' g_date_rd '.94427' '.txt' into g_filename1. "11042011
* End of <> 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'IBTP
.' g_date_rd '.94427' '.txt' into g_filename1."11042011 04052011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'IBTP
' g_date_rd g_time_rd '.txt' into g_filename1.
* End of <> on 24212010

**Begin RD1K991769 CAB_ALOK
* ZVMSFILEGEN_CVP, MUM, IBTP (Non-SBI - Regis. )
*3==============================================================
IF p_payctr = 'CVP' .
***
.
***
***
***
***
***

.
*

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_rdata_nonsbi.
transfer ist_rdata_nonsbi to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_rdata_nonsbi[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'IBTP' '94427' ist_rdata_nonsbi[]
PERFORM DO_DS tables ist_rdata_nonsbi using sbi_filename .
endif.

ENDIF. " IF p_payctr = 'CVP'
**End RD1K991769 CAB_ALOK
* 14102011 17102011 09052012
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"opened commented part 27.09.2012
* Begin of Comment on 07082012
*
concatenate '/usr/sap/RP1/FIN/epay/send/mum/' 'IBTP' g_date_rd g_t
ime_rd '.txt' into g_filename.
*
concatenate 'C:\SPAN\' 'R_NONSBI' g_date_rd g_time_rd '.txt' into
g_filename.
"11042011 21062011 05072011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_nonsbi
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15

*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"end of open 27.09.2012
* End of <> on 07082012
* 14102011 17102011 090502012
endif.
* Begin of <> on 16072012
if not ist_ddata_nonsbi_s[] is initial.
append lines of ist_ddata_nonsbi_s to ist_ddata_nonsbi.
endif.
* End of <> on 16072012
describe table ist_ddata_nonsbi lines l_dsbi.
if l_dsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
* Begin of <> on 24122010
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'MUM'.
if sy-subrc = 0.
concatenate g_fsnam 'DIBTP.' g_date_rd '.' g_corpid '.txt' into
g_filename1.
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
mum/' 'DIBTP.' g_date_rd '.94427' '.txt' into g_filename1. "11042011
* End of <> 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'DIBT
P.' g_date_rd '.94427' '.txt' into g_filename1.
"11042011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/mum/' 'DIBT
P' g_date_rd g_time_rd '.txt' into g_filename1.
* End of <> on 24122010
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CVP, MUM, DIBTP (Non-SBI - DeRegis. )
*4===============================================================
IF p_payctr = 'CVP' .
***
.
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_ddata_nonsbi.
transfer ist_ddata_nonsbi to g_filename1.
g_filegen = 'X'.

***
***

] .
*

ENDLOOP.
close dataset g_filename1.
if ist_ddata_nonsbi[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'DIBTP' '94427' ist_ddata_nonsbi[
PERFORM DO_DS tables ist_ddata_nonsbi using sbi_filename .
endif.

ENDIF. " IF p_payctr = 'CVP' .
**End RD1K991769 CAB_ALOK
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"opened comment for testing by lipsy on 3.10.2012
* Begin of Comment on 07082012
*
concatenate '/usr/sap/RP1/FIN/epay/send/mum/' 'DIBTP' g_date_rd g
_time_rd '.txt' into g_filename.
* Begin of <> on 21122010
*
concatenate 'C:\SPAN\' 'D_NONSBI' g_date_rd g_time_rd '.txt' into
g_filename. "21122010 "11042011 21062011 05072011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
*
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_nonsbi
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
"opened and commented by lipsy on 7.01.2013
"commented by lipsy on 28.12.2012 <RD1K983099>
"end of open of comment by lipsy on 03.10.2012

loop at ist_ddata_nonsbi into wa_ddata_nonsbi.
split wa_ddata_nonsbi at '#' into : wa_zfivms_brd-bankn_new wa_zf
ivms_brd-bankl_new wa_zfivms_brd-koinh_new wa_zfivms_brd-span.
update zfivms_resend set status = 'SENT TO SBI'
where span = wa_zfivms_brd-span.
where span = wa_zfivms_brd-span.
endloop.

*

endif.
* Code Written for OVL Bank registration Date 02-05-2011
clear : l_rsbi, l_dsbi.
* OVL SBI - R
* Begin of <RD1K977143> on 14072011
if not ist_rdata_sbi_ovl_e[] is initial.
* End of <RD1K977143> on 14072011
append lines of ist_rdata_sbi_ovl_e to ist_rdata_sbi_ovl.
"06
072011
endif.
describe table ist_rdata_sbi_ovl lines l_rsbi.
if l_rsbi > 0.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'OVL'.
if sy-subrc = 0.
concatenate g_fsnam '3P.' g_date_rd '.' g_corpid '.txt' into g_f
ilename1. "16062011
*
concatenate g_fsnam '3P.' g_date_rd '.30523' '.txt' into g_filena
me1. "16062011
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ovl/' '3P.' g_date_rd '.30523' '.txt' into g_filename1.
* End of <> on 10062011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_OVL, OVL, 3P (SBI - Regis. )
*5=============================================================
IF p_payctr = 'OVL' .
***
.
***
***
***
***
***

.

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_rdata_sbi_ovl.
transfer ist_rdata_sbi_ovl to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_rdata_sbi_ovl[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING '3P' '30523' ist_rdata_sbi_ovl[]
endif.

ENDIF. " p_payctr = 'OVL' .
**End RD1K991769 CAB_ALOK

"opened and commented by lipsy on 7.01.2013
* Begin of Comment on 07082012
*
Concatenate 'C:\SPAN\' 'R_SBI_OVL' g_date_rd g_time_rd '.txt' into
g_filename. "05052011 "10062011 21062011 05072011 110711
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_sbi_ovl
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
"opened and commented by lipsy on 7.01.2013
endif.
* SBI - D
clear : l_rsbi, l_dsbi.
"14072011 RD1K9771
* Begin of <RD1K977143> on 14072011
if not ist_ddata_sbi_ovl_e[] is initial.
* End of <RD1K977143> on 14072011
append lines of ist_ddata_sbi_ovl_e to ist_ddata_sbi_ovl.
"06
072011
endif.
describe table ist_ddata_sbi_ovl lines l_dsbi.
if l_dsbi > 0.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011

select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'OVL'.
if sy-subrc = 0.
concatenate g_fsnam 'D3P.' g_date_rd '.' g_corpid '.txt' into g_
filename1.
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ovl/' 'D3P.' g_date_rd '.30523' '.txt' into g_filename1.
* End of <> on 10062011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_OVL, OVL, D3P (SBI - DeRegis. )
*6=============================================================
IF p_payctr = 'OVL' .
***
.
***
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_ddata_sbi_ovl.
transfer ist_ddata_sbi_ovl to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_ddata_sbi_ovl[] is not INITIAL.

.

perform PREPARE_NEW_FORMAT USING 'D3P' '30523' ist_ddata_sbi_ovl[]
endif.

ENDIF. " p_payctr = 'OVL' .
**End RD1K991769 CAB_ALOK
** Begin of Comment on 07082012
*
*
Concatenate 'C:\SPAN\' 'D_SBI_OVL' g_date_rd g_time_rd '.txt' into
g_filename. "05052011 21062011 05072011 11072011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_ddata_sbi_ovl
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8

*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
loop at ist_ddata_sbi_ovl into wa_ddata_sbi.
split wa_ddata_sbi at '#' into : wa_zfivms_brd-koinh_new g_benf
wa_zfivms_brd-bankn_new wa_zfivms_brd-bankl_new wa_zfivms_brd-span.
update zfivms_resend set status = 'SENT TO SBI'
where span = wa_zfivms_brd-span.
endloop.
endif.
* NON SBI - R
clear : l_rsbi , l_dsbi.
* Begin of <RD1K977143> on 14072011
if not ist_rdata_nonsbi_ovl_e[] is initial.
* End of <RD1K977143> on 14072011
append lines of ist_rdata_nonsbi_ovl_e to ist_rdata_nonsbi_ovl. "06
072011
endif.
describe table ist_rdata_nonsbi_ovl lines l_rsbi.
if l_rsbi > 0.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'OVL'.
if sy-subrc = 0.
concatenate g_fsnam 'IBTP.' g_date_rd '.' g_corpid '.txt' into g
_filename1.
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ovl/' 'IBTP.' g_date_rd '.30523' '.txt' into g_filename1.
* End of <> on 10062011

**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_OVL, OVL, IBTP (nonSBI - Regis. )
*7=============================================================

IF p_payctr = 'OVL' .
***
.
***
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_rdata_nonsbi_ovl. "ist_rdata_nonsbi. "19052011
transfer ist_rdata_nonsbi_ovl to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.

if ist_rdata_nonsbi_ovl[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'IBTP' '30523' ist_rdata_nonsbi_o
vl[] .
endif.
ENDIF. " p_payctr = 'OVL' .
**End RD1K991769 CAB_ALOK
"opened and commented by lipsy on 7.01.2013
* Begin of Comment on 07082012
*
Concatenate 'C:\SPAN\' 'R_NONSBI_OVL' g_date_rd g_time_rd '.txt' i
nto g_filename. "04052011 21062011 05072011 11072011
**
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_nonsbi_ovl
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.

*
ENDIF.
* End of <> on 07082012
"opened and commented by lipsy on 7.01.2013
endif.
* NON SBI - D
clear : l_rsbi, l_dsbi.
"14072011 RD1K9771
* Begin of <RD1K977143> on 14072011
if not ist_ddata_nonsbi_ovl_e[] is initial.
* End of <RD1K977143> on 14072011
append lines of ist_ddata_nonsbi_ovl_e to ist_ddata_nonsbi_ovl. "06
072011
endif.
describe table ist_ddata_nonsbi_ovl lines l_dsbi.
if l_dsbi > 0.
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh
ere sysid = syst-sysid
and zbukr = 'OVL'.
if sy-subrc = 0.
concatenate g_fsnam 'DIBTP.' g_date_rd '.' g_corpid '.txt' into
g_filename1.
endif.
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ovl/' 'DIBTP.' g_date_rd '.30523' '.txt' into g_filename1.
* End of <> on 10062011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_OVL, OVL, DIBTP (nonSBI - DeRegis. )
*8=============================================================
IF p_payctr = 'OVL' .
***
.
***
****
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_ddata_nonsbi_ovl. "ist_ddata_nonsbi. 19052011
transfer ist_ddata_nonsbi to g_filename1.
19052011
transfer ist_ddata_nonsbi_ovl to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.

if ist_ddata_nonsbi_ovl[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'DIBTP' '30523' ist_ddata_nonsbi_
ovl[] .
endif.
ENDIF. " p_payctr = 'OVL' .
**End RD1K991769 CAB_ALOK
* Begin of Comment on 07082012
*

Concatenate 'C:\SPAN\' 'D_NONSBI_OVL'

g_date_rd g_time_rd '.txt' i

nto g_filename. "05052011 21062011 11072011
**
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_ddata_nonsbi_ovl
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
loop at ist_ddata_nonsbi_ovl into wa_ddata_nonsbi.
split wa_ddata_nonsbi at '#' into : wa_zfivms_brd-bankn_new wa_zf
ivms_brd-bankl_new wa_zfivms_brd-koinh_new wa_zfivms_brd-span.
update zfivms_resend set status = 'SENT TO SBI'
where span = wa_zfivms_brd-span.
endloop.
endif.
* End of Logic for OVL
**Begin RD1K991769 CAB_ALOK
* describe table ist_zfivms_brd lines l_stat.
* if l_stat > 0.
** Begin of <> on 26082011
*
update zfivmsbank set status = 'SENT TO SBI'
*
where vmc_apdate = p_date
*
and status in ('RELEASE BY VMC' , 'RELEASE B
Y EMPLOYEE' , 'RELEASE BY VMC FOR OVL', 'RELEASE BY EMPLOYEE FOR OVL').
**
update zfivmsbank set status = 'SENT TO SBI'

**
where vmc_apdate = p_date.
** End of <> on 26082011
*
*
"added by lipsy on 27.09.2012
*
update zsdcust_bank set status = 'SENT TO SBI'
*
where chon = p_date
*
and status = 'RELEASE BY CMC'.
*
"end of addition by lipsy on 27.09.2012
* endif.
**End RD1K991769 CAB_ALOK

* End of <> on 21122010
ENDFORM.

*&---------------------------------------------------------------------*
*& Form EMPLOYEE_FILEGEN
*&---------------------------------------------------------------------*
FORM EMPLOYEE_FILEGEN .
* SBI
describe table ist_rdata_sbi_e lines l_rsbi.
if l_rsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'DDN'.
concatenate g_fsnam '3P.' g_date_rd '.93128' '.txt' into g_filenam
e1.
***start of ONGC code
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'ECPF'.
concatenate g_fsnam '3P.' g_date_rd '.96726' '.txt' into g_filenam
e1.
***end of ONGC code
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ddn/' '3P.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
* End of <> 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' '3P.'
g_date_rd '.93128' '.txt' into g_filename1. "11042011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CEP, CEP, 3P (SBI - Regis. )
*9=============================================================
IF p_payctr = 'CEP'.
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT

.
***
***
***
***
***

lOOP AT ist_rdata_sbi_e.
transfer ist_rdata_sbi_e to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_rdata_sbi_e[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING '3P' '93128' ist_rdata_sbi_e[] .
endif.

ENDIF. " p_payctr = 'CEP .
***start of ONGC code
IF p_payctr = 'ECPF'.
if ist_rdata_sbi_e[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING '3P' '96726' ist_rdata_sbi_e[] .
endif.
ENDIF. "
***end of ONGC code
**End RD1K991769 CAB_ALOK

"opened and commented by lipsy on 7.01.2013
* Begin of Comment on 07082012
*
concatenate 'C:\SPAN\' 'R_SBI_E' g_date_rd g_time_rd '.txt' into g
_filename.
"11042011 "21062011 291211 27012012
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_sbi_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17

*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
"opened and commented by lipsy on 7.01.2013
endif.
describe table ist_ddata_sbi_e lines l_dsbi.
if l_dsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of Comment on 07082012
*
concatenate 'C:\SPAN\' 'D_SBI_E' g_date_rd g_time_rd '.txt' into g
_filename. "21122010 "11042011 21060211 291211 27012012
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_sbi_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012

* Begin of <> on 10062011
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'DDN'.
concatenate g_fsnam 'D3P.' g_date_rd '.93128' '.txt' into g_filena
me1.
***start of ONGC code
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'ECPF'.
concatenate g_fsnam 'D3P.' g_date_rd '.96726' '.txt' into g_filena
me1.
***end of ONGC code
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ddn/' 'D3P.' g_date_rd '.93128' '.txt' into g_filename1.
* End of <> on 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'D3P.
' g_date_rd '.93128' '.txt' into g_filename1. "11042011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CEP, CEP, D3P (SBI - DeRegis. )
*10=============================================================
IF p_payctr = 'CEP'.
***
.
***
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_ddata_sbi_e.
transfer ist_ddata_sbi_e to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_ddata_sbi_e[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'D3P' '93128' ist_ddata_sbi_e[] .
endif.

ENDIF. " p_payctr = 'CEP' .
**End RD1K991769 CAB_ALOK
***start of ONGC code
IF p_payctr = 'ECPF'.
if ist_ddata_sbi_e[] is not INITIAL.
perform PREPARE_NEW_FORMAT USING 'D3P' '96726' ist_ddata_sbi_e[] .
endif.
ENDIF. "
***end of ONGC code
endif.
* NON SBI

clear : l_rsbi , l_dsbi.
describe table ist_rdata_nonsbi_e lines l_rsbi.
if l_rsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'DDN'.
concatenate g_fsnam 'IBTP.' g_date_rd '.93128' '.txt' into g_filen
ame1.
***start of ONGC code
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'ECPF'.
concatenate g_fsnam 'IBTP.' g_date_rd '.96726' '.txt' into g_filen
ame1.
***end of ONGC code
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ddn/' 'IBTP.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
* End of <> 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'IBTP
.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CEP, CEP, IBTP (nonSBI - Regis. )
*11=============================================================
IF p_payctr = 'CEP'.
***
.
***
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_rdata_nonsbi_e.
transfer ist_rdata_nonsbi_e to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_rdata_nonsbi_e[] is not INITIAL.

[] .

perform PREPARE_NEW_FORMAT USING 'IBTP' '93128' ist_rdata_nonsbi_e

endif.
ENDIF. " p_payctr = 'CEP' .
**End RD1K991769 CAB_ALOK
***start of ONGC code
IF p_payctr = 'ECPF'.
if ist_rdata_nonsbi_e[] is not INITIAL.
[] .

perform PREPARE_NEW_FORMAT USING 'IBTP' '96726' ist_rdata_nonsbi_e

endif.

ENDIF. "
***end of ONGC code
"opened and commented by lipsy on 7.01.2013
* Begin of Comment on 07082012
*
concatenate 'C:\SPAN\' 'R_NONSBI_E' g_date_rd g_time_rd '.txt' int
o g_filename. "11042011 21062011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_nonsbi_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
"opened and commented by lipsy on 7.01.2013
endif.
describe table ist_ddata_nonsbi_e lines l_dsbi.
if l_dsbi > 0.
*
write sy-datum to g_date_rd DDMMYY.
"14022011
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
g_time_rd = sy-uzeit.
* Begin of <> on 10062011
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'DDN'.
concatenate g_fsnam 'DIBTP.' g_date_rd '.93128' '.txt' into g_file

name1.
***start of ONGC code
select single fsnam into g_fsnam from zfi_path where sysid = syst-sy
sid
and zbukr = 'ECPF'.
concatenate g_fsnam 'DIBTP.' g_date_rd '.96726' '.txt' into g_file
name1.
***end of ONGC code
*
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed/
ddn/' 'DIBTP.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
* End of <> on 10062011
*
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'DIBT
P.' g_date_rd '.93128' '.txt' into g_filename1.
"11042011
**Begin RD1K991769 CAB_ALOK

* ZVMSFILEGEN_CEP, CEP, DIBTP (nonSBI - DeRegis. )
*12=============================================================
IF p_payctr = 'CEP'.
***
.
***
***
***
***
***

open dataset g_filename1 for output in text mode ENCODING DEFAULT
lOOP AT ist_ddata_nonsbi_e.
transfer ist_ddata_nonsbi_e to g_filename1.
g_filegen = 'X'.
ENDLOOP.
close dataset g_filename1.
if ist_ddata_nonsbi_e[] is not INITIAL.

e[] .

perform PREPARE_NEW_FORMAT USING 'DIBTP' '93128' ist_ddata_nonsbi_

endif.
ENDIF. " p_payctr = 'CEP' .
**End RD1K991769 CAB_ALOK
***start of ONGC code
IF p_payctr = 'ECPF'.
if ist_ddata_nonsbi_e[] is not INITIAL.
e[] .

perform PREPARE_NEW_FORMAT USING 'DIBTP' '96726' ist_ddata_nonsbi_

endif.
ENDIF.
***end of ONGC code
* Begin of Comment on 07082012
*
concatenate 'C:\SPAN\' 'D_NONSBI_E' g_date_rd g_time_rd '.txt' int
o g_filename.
"11042011 21062011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename

*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_nonsbi_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* End of <> on 07082012
endif.
* OVL Employee
clear : l_rsbi , l_dsbi.
* Begin of <> on 06072011
* SBI
* describe table ist_rdata_sbi_ovl_e lines l_rsbi.
* if l_rsbi > 0.
**
write sy-datum to g_date_rd DDMMYY.
"14022011
*
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
*
g_time_rd = sy-uzeit.
** Begin of <> on 10062011
*
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path
where sysid = syst-sysid
*
and zbukr = 'OVL'.
*
if sy-subrc = 0.
*
concatenate g_fsnam '3P.' g_date_rd '.' g_corpid '.txt' into g_f
ilename1.
*
endif.
**
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed
/ddn/' '3P.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
** End of <> 10062011
**
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' '3P.
' g_date_rd '.93128' '.txt' into g_filename1. "11042011
*
*
*
open dataset g_filename1 for output in text mode ENCODING DEFAULT.
*
lOOP AT ist_rdata_sbi_ovl_e.

*
transfer ist_rdata_sbi_ovl_e to g_filename1.
*
ENDLOOP.
*
close dataset g_filename1.
*
*
concatenate 'C:\SPAN\' 'R_SBI_OVL_E' g_date_rd g_time_rd '.txt' in
to g_filename.
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_sbi_ovl_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* endif.
*
*

describe table ist_ddata_sbi_ovl_e lines l_dsbi.
if l_dsbi > 0.

**
write sy-datum to g_date_rd DDMMYY.
"14022011
*
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
*
g_time_rd = sy-uzeit.
*
*
concatenate 'C:\SPAN\' 'D_SBI_OVL_E' g_date_rd g_time_rd '.txt' in
to g_filename. "21122010 "11042011 21060211
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_sbi_ovl_e

*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
** Begin of <> on 10062011
*
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path
where sysid = syst-sysid
*
and zbukr = 'OVL'.
*
if sy-subrc = 0.
*
concatenate g_fsnam 'D3P.' g_date_rd '.' g_corpid '.txt' into g_
filename1.
*
endif.
*
**
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed
/ddn/' 'D3P.' g_date_rd '.93128' '.txt' into g_filename1.
** End of <> on 10062011
**
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'D3P
.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
*
*
*
open dataset g_filename1 for output in text mode ENCODING DEFAULT.
*
lOOP AT ist_ddata_sbi_ovl_e.
*
transfer ist_ddata_sbi_ovl_e to g_filename1.
*
ENDLOOP.
*
close dataset g_filename1.
*
* endif.
* NON SBI
* clear : l_rsbi , l_dsbi.
* describe table ist_rdata_nonsbi_ovl_e lines l_rsbi.
* if l_rsbi > 0.
**
write sy-datum to g_date_rd DDMMYY.
"14022011
*
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
*
g_time_rd = sy-uzeit.
** Begin of <> on 10062011
*
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path wh

ere sysid = syst-sysid
*
and zbukr = 'OVL'.
*
if sy-subrc = 0.
*
concatenate g_fsnam 'IBTP.' g_date_rd '.' g_corpid '.txt' into g
_filename1.
*
endif.
*
**
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed
/ddn/' 'IBTP.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
** End of <> 10062011
**
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'IBT
P.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
*
*
open dataset g_filename1 for output in text mode ENCODING DEFAULT.
*
lOOP AT ist_rdata_nonsbi_ovl_e.
*
transfer ist_rdata_nonsbi_ovl_e to g_filename1.
*
ENDLOOP.
*
close dataset g_filename1.
*
concatenate 'C:\SPAN\' 'R_NONSBI_OVL_E' g_date_rd g_time_rd '.txt'
into g_filename. "11042011 21062011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= '*'
*
TABLES
*
DATA_TAB
= ist_rdata_nonsbi_ovl_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO
*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* endif.
*
* describe table ist_ddata_nonsbi_ovl_e lines l_dsbi.
* if l_dsbi > 0.

**
write sy-datum to g_date_rd DDMMYY.
"14022011
*
CONCATENATE SY-DATUM+6(2) SY-DATUM+4(2) SY-DATUM(4) into g_date_rd.
*
g_time_rd = sy-uzeit.
** Begin of <> on 10062011
*
select single fsnam corpid into (g_fsnam, g_corpid) from zfi_path w
here sysid = syst-sysid
*
and zbukr = 'OVL'.
*
if sy-subrc = 0.
*
concatenate g_fsnam 'DIBTP.' g_date_rd '.' g_corpid '.txt' into
g_filename1.
*
endif.
*
**
concatenate '/usr/sap/' syst-sysid '/DigSignApp/data/output/signed
/ddn/' 'DIBTP.' g_date_rd '.93128' '.txt' into g_filename1. "11042011
** End of <> on 10062011
**
concatenate '/usr/sap/RD1/DigSignApp/data/output/signed/ddn/' 'DIB
TP.' g_date_rd '.93128' '.txt' into g_filename1.
"11042011
*
*
open dataset g_filename1 for output in text mode ENCODING DEFAULT.
*
lOOP AT ist_ddata_nonsbi_ovl_e.
*
transfer ist_ddata_nonsbi_ovl_e to g_filename1.
*
ENDLOOP.
*
close dataset g_filename1.
*
*
*
concatenate 'C:\SPAN\' 'D_NONSBI_OVL_E' g_date_rd g_time_rd '.txt'
into g_filename.
"11042011 21062011
*
*
CALL FUNCTION 'GUI_DOWNLOAD'
*
EXPORTING
*
FILENAME
= g_filename
*
FILETYPE
= 'ASC'
**
WRITE_FIELD_SEPARATOR
= ''
*
TABLES
*
DATA_TAB
= ist_ddata_nonsbi_ovl_e
*
EXCEPTIONS
*
FILE_WRITE_ERROR
= 1
*
NO_BATCH
= 2
*
GUI_REFUSE_FILETRANSFER
= 3
*
INVALID_TYPE
= 4
*
NO_AUTHORITY
= 5
*
UNKNOWN_ERROR
= 6
*
HEADER_NOT_ALLOWED
= 7
*
SEPARATOR_NOT_ALLOWED
= 8
*
FILESIZE_NOT_ALLOWED
= 9
*
HEADER_TOO_LONG
= 10
*
DP_ERROR_CREATE
= 11
*
DP_ERROR_SEND
= 12
*
DP_ERROR_WRITE
= 13
*
UNKNOWN_DP_ERROR
= 14
*
ACCESS_DENIED
= 15
*
DP_OUT_OF_MEMORY
= 16
*
DISK_FULL
= 17
*
DP_TIMEOUT
= 18
*
FILE_NOT_FOUND
= 19
*
DATAPROVIDER_EXCEPTION
= 20
*
CONTROL_FLUSH_ERROR
= 21
*
OTHERS
= 22.
*
*
IF SY-SUBRC <> 0.
*
MESSAGE ID SY-MSGID TYPE SY-MSGTY NUMBER SY-MSGNO

*
WITH SY-MSGV1 SY-MSGV2 SY-MSGV3 SY-MSGV4.
*
ENDIF.
* endif.
* End of <> 06072011
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_ONGC_EMLPOYEE_DETAILS
*&---------------------------------------------------------------------*
FORM GET_ONGC_EMLPOYEE_DETAILS .
data : l_begdate type pa0009-begda,
l_strlen type n,
l_line(2) type n,
l_enddate type pa0009-endda value '99991231',
l_endda(10),
l_begda(10).
* Begin of <> on 15022012
*/..Begin of Change CR :30014976 by CAB_RAMA
*
CONCATENATE 'EMP0000000000R00' wa_pa0009-pernr into wa_zfivms_brd-s
pan.
CONCATENATE 'EM' sy-datum '000R00' wa_pa0009-pernr into wa_zfivms_brd-sp
an.
*/..End of Change CR :30014976 by CAB_RAMA
* CONCATENATE '0000000000000R00' wa_pa0009-pernr into wa_zfivms_brd-spa
n.
* End of <> on 15022012
**Begin RD1K991769 CAB_ALOK
* if wa_pa0009-bankl(4) = 'SBIN'.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl+6(5).
* else.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl.
* endif.
wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl.
**End RD1K991769 CAB_ALOK
wa_zfivms_brd-bankn_new = wa_pa0009-bankn.
if not wa_pa0009-emftx is initial.
wa_zfivms_brd-koinh_new = wa_pa0009-emftx.
else.
select single ename from pa0001 into wa_zfivms_brd-koinh_new where p
ernr = wa_pa0009-pernr.
endif.
**Begin 26.05.2014 RD1K991769 CAB_ALOK CR 30010502:Beneficiary file-digi
tal signing, Logging
*REPLACE ALL OCCURRENCES OF '.' IN wa_zfivms_brd-koinh_new WITH space.
*translate
TRANSLATE wa_zfivms_brd-koinh_new USING trans_string.
**End RD1K991769 CAB_ALOK
Logging

CR 30010502:Beneficiary file-digital signing,

select single zphone from pa9205 into wa_zfivms_brd-tel_number where p
ernr = wa_pa0009-pernr and subty = '01'.
wa_zfivms_brd-vmc_apdate = wa_pa0009-aedtm . " wa_pa0009-begda. 301211
wa_zfivms_brd-STATUS

= 'RELEASE BY EMPLOYEE'.

select * from pa0006 into table ist_pa0006 where pernr = wa_pa0009-per
nr
and anssa in ('1', '3' )
and endda = l_enddate. "'
31.12.9999'.
read table ist_pa0006 into wa_pa0006 with key anssa = '1'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2. 2
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01. 2
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
"wa_pa0006-stras. 2
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
read table ist_pa0006 into wa_pa0006 with key anssa = '3'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2. 2
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01. 2
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
290911
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01.
290911
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
290911
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
wa_zfivms_brd-STREET = 'ONGC'.
endif.
* Begin of <> on 30052012
else.
wa_zfivms_brd-STREET = 'ONGC'.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
* End of <> on 30052012
endif.
endif.
endif.
* Begin of <> on 20062011 21062011

refresh ist_zfivms_brd1.
select * from zfivms_brd into corresponding fields of table ist_zfivm
s_brd1 where span = wa_zfivms_brd-span.
if sy-subrc = 0.
* Begin of <> on 22052012
clear l_line.
describe table ist_zfivms_brd1 lines l_line.
shift l_line left deleting leading '0'.
"16072012
if l_line > 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 with key seqno = l_
line.
if sy-subrc <> 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
endif.
*
sort ist_zfivms_brd1 descending by seqno.
*
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
* End of <> on 22052012
if sy-subrc = 0.
wa_zfivms_brd-seqno = wa_zfivms_brd1-seqno + 1.
endif.
endif.
modify zfivms_brd from wa_zfivms_brd .
*
update zfivms_brd set seqno = wa_zfivms_brd-seqno
*
BANKL_NEW = wa_zfivms_brd-BANKL_NEW
*
BANKN_NEW = wa_zfivms_brd-BANKn_NEW
*
KOINH_NEW = wa_zfivms_brd-KOINH_NEW
*
STREET
= wa_zfivms_brd-STREET
*
STR_SUPPL1 = wa_zfivms_brd-STR_SUPPL1
*
TEL_NUMBER = wa_zfivms_brd-TEL_NUMBER
*
CITY1
= wa_zfivms_brd-CITY1
*
VMC_APDATE = wa_zfivms_brd-VMC_APDATE
*
VMC_APTIME = wa_zfivms_brd-VMC_APTIME
*
STATUS
= wa_zfivms_brd-STATUS
*
REMARKS
= wa_zfivms_brd-REMARKS
*
where span = wa_zfivms_brd-span .
else.
* End of <> on 20062011
wa_zfivms_brd-seqno = 1.
modify zfivms_brd from wa_zfivms_brd .
endif.
.
"ENDDA
**Begin RD1K991769 CAB_ALOK
* if sy-subrc = 0.
*
update pa0009 set flag1 = 'R' where pernr = wa_pa0009-pernr and end
da = l_enddate and subty = '0'.
* endif.
*
*
*
*
*
*

update pa0009
"FLAG2
set flag2 = 'X'
where pernr = wa_pa0009-pernr
and endda = l_enddate
and subty = '0'.
wa_pa0009t-FLAG2 = 'X'.

wa_pa0009-FLAG1 = 'R'.
APPEND wa_pa0009 TO IST_pa0009_ONGC.
**1234
* data: WA_ZPA0009 type ZPA0009.

* WA_ZPA0009 = wa_pa0009.
* WA_ZPA0009-flag_read = 'Q'.
* insert ZPA0009 FROM WA_ZPA0009.
**1234
**End RD1K991769 CAB_ALOK
*

clear : wa_zfivms_brd , wa_zfivms_brd1.
append wa_zfivms_brd to ist_zfivms_brd.

"ENDDA

l_begdate = wa_pa0009-begda - 1.
**Begin RD1K991769 CAB_ALOK
clear wa_pa0009t.
**End RD1K991769 CAB_ALOK
**Begin RD1K991769 CAB_ALOK
* select single * from pa0009 into wa_pa0009t where pernr = wa_pa0009pernr and endda = l_begdate "begda = p_date and
*
and subty = '0'." and zlsch
in ('S','N'). 20062011
select single *
from pa0009
into wa_pa0009t
where pernr = wa_pa0009-pernr
and subty = '0'
and endda = l_begdate
and ( flag1 = '' or flag1 = 'S' or flag1 = 'F').
*
and FLAG2 = ''.
**End RD1K991769 CAB_ALOK

if sy-subrc = 0.
* Begin of <> on 15022012
*/..Begin of Change CR :30014976 by CAB_RAMA
* CONCATENATE 'EMP0000000000D00' wa_pa0009t-pernr into wa_zfivms_brd-sp
an.
CONCATENATE 'EM' sy-datum '000D00' wa_pa0009t-pernr into wa_zfivms_brd-s
pan.
*/..End of Change CR :30014976 by CAB_RAMA
*
CONCATENATE '0000000000000D00' wa_pa0009t-pernr into wa_zfivms_brdspan.
* End of <> on 15022012
**Begin RD1K991769 CAB_ALOK
*
if wa_pa0009t-bankl(4) = 'SBIN'.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl+6(5).
*
else.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl.
*
endif.
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl.
**End RD1K991769 CAB_ALOK

wa_zfivms_brd-bankn_new = wa_pa0009t-bankn.
if not wa_pa0009t-emftx is initial.
wa_zfivms_brd-koinh_new = wa_pa0009t-emftx.
else.
select single ename from pa0001 into wa_zfivms_brd-koinh_new where
pernr = wa_pa0009t-pernr.
endif.
**Begin 26.05.2014 RD1K991769 CAB_ALOK CR 30010502:Beneficiary file-digi
tal signing, Logging
*REPLACE ALL OCCURRENCES OF '.' IN wa_zfivms_brd-koinh_new WITH space.
*translate
TRANSLATE wa_zfivms_brd-koinh_new USING trans_string.
**End RD1K991769 CAB_ALOK CR 30010502:Beneficiary file-digital signing,
Logging
select single zphone from pa9205 into wa_zfivms_brd-tel_number where
pernr = wa_pa0009-pernr and subty = '01'.

wa_zfivms_brd-vmc_apdate = wa_pa0009-aedtm . " wa_pa0009-begda. 3012
wa_zfivms_brd-STATUS = 'RELEASE BY EMPLOYEE'.

refresh ist_pa0006.
select * from pa0006 into table ist_pa0006 where pernr = wa_pa0009tpernr
and anssa in ('1', '3'
)
and endda = l_enddate.
"'31.12.9999'.
read table ist_pa0006 into wa_pa0006 with key anssa = '1'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras. 2
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
read table ist_pa0006 into wa_pa0006 with key anssa = '3'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2
.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01
.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras
.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01
.

if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.

"wa_pa0006-stras

.

else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
wa_zfivms_brd-STREET = 'ONGC'.
endif.
endif.
endif.
endif.
* Begin of <> on 20062011 21062011
refresh ist_zfivms_brd1.
select * from zfivms_brd into corresponding fields of table ist_zfi
vms_brd1 where span = wa_zfivms_brd-span.
if sy-subrc = 0.
* Begin of <> on 22052012
clear l_line.
describe table ist_zfivms_brd1 lines l_line.
shift l_line left deleting leading '0'.
"16072012
if l_line > 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 with key seqno =
l_line.
if sy-subrc <> 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
endif.
*
sort ist_zfivms_brd1 descending by seqno.
*
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
if sy-subrc = 0.
wa_zfivms_brd-seqno = wa_zfivms_brd1-seqno + 1.
endif.
endif.
modify zfivms_brd from wa_zfivms_brd .
*
update zfivms_brd set seqno = wa_zfivms_brd-seqno
*
BANKL_NEW = wa_zfivms_brd-BANKL_NEW
*
BANKN_NEW = wa_zfivms_brd-BANKn_NEW
*
KOINH_NEW = wa_zfivms_brd-KOINH_NEW
*
STREET
= wa_zfivms_brd-STREET
*
STR_SUPPL1 = wa_zfivms_brd-STR_SUPPL1
*
TEL_NUMBER = wa_zfivms_brd-TEL_NUMBER
*
CITY1
= wa_zfivms_brd-CITY1
*
VMC_APDATE = wa_zfivms_brd-VMC_APDATE
*
VMC_APTIME = wa_zfivms_brd-VMC_APTIME
*
STATUS
= wa_zfivms_brd-STATUS
*
REMARKS
= wa_zfivms_brd-REMARKS
*
where span = wa_zfivms_brd-span .
else.
* End of <> on 20062011
wa_zfivms_brd-seqno = 1.
modify zfivms_brd from wa_zfivms_brd .
"ENDDA
endif.
**Begin RD1K991769 CAB_ALOK
*
if sy-subrc = 0.
*
update pa0009 set flag1 = 'D' where pernr = wa_pa0009t-pernr and
endda = l_begdate and subty = '0'.
**
clear : wa_zfivms_brd , wa_zfivms_brd1.
*
endif.

*
*
*
*
*
*

update pa0009
set flag2 = 'X'
where pernr = wa_pa0009t-pernr
and endda = l_begdate
and subty = '0'.
wa_pa0009t-FLAG2 = 'X'.

wa_pa0009t-FLAG1 = 'D'.
APPEND wa_pa0009t TO IST_pa0009_ONGC.
*1234
* data: W_ZPA0009 type ZPA0009.
* W_ZPA0009 = wa_pa0009.
* W_ZPA0009-flag_read = 'Q'.
* insert ZPA0009 FROM W_ZPA0009.
*1234
**End RD1K991769 CAB_ALOK

clear : wa_zfivms_brd , wa_zfivms_brd1.
endif.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form GET_OVL_EMLPOYEE_DETAILS
*&---------------------------------------------------------------------*
FORM GET_OVL_EMLPOYEE_DETAILS .
data : l_begdate type pa0009-begda,
l_strlen type n,
l_line(2) type n,
l_enddate type pa0009-endda value '99991231',
l_endda(10),
l_begda(10).
* Begin of <> on 15022012

*
*
*
*

*/..Begin of Change CR :30014976 by CAB_RAMA
* CONCATENATE 'EMP0000000000R00' wa_pa0009-pernr into wa_zfivms_brd-spa
n.
CONCATENATE 'EM' sy-datum '000R00' wa_pa0009-pernr into wa_zfivms_brd-sp
an.
*/..End of Change CR :30014976 by CAB_RAMA
* CONCATENATE '0000000000000R00' wa_pa0009-pernr into wa_zfivms_brd-span
.
* End of <> on 15022012
**Begin RD1K991769 CAB_ALOK
* if wa_pa0009-bankl(4) = 'SBIN'.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl+6(5).
* else.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl.
* endif.

wa_zfivms_brd-BANKL_NEW = wa_pa0009-bankl.
**End RD1K991769 CAB_ALOK
wa_zfivms_brd-bankn_new = wa_pa0009-bankn.
if not wa_pa0009-emftx is initial.
wa_zfivms_brd-koinh_new = wa_pa0009-emftx.
else.
select single ename from pa0001 into wa_zfivms_brd-koinh_new where p
ernr = wa_pa0009-pernr.
endif.
**Begin 26.05.2014 RD1K991769 CAB_ALOK CR 30010502:Beneficiary file-digi
tal signing, Logging
*REPLACE ALL OCCURRENCES OF '.' IN wa_zfivms_brd-koinh_new WITH space.
*translate
TRANSLATE wa_zfivms_brd-koinh_new USING trans_string.
**End RD1K991769 CAB_ALOK
Logging

CR 30010502:Beneficiary file-digital signing,

select single zphone from pa9205 into wa_zfivms_brd-tel_number where p
ernr = wa_pa0009-pernr and subty = '01'.
.

wa_zfivms_brd-vmc_apdate = wa_pa0009-aedtm . " wa_pa0009-begda. 301211
wa_zfivms_brd-STATUS

= 'RELEASE BY EMPLOYEE FOR OVL'.

select * from pa0006 into table ist_pa0006 where pernr = wa_pa0009-per
nr
and anssa in ('1', '3' )
and endda = l_enddate. "'
31.12.9999'.
read table ist_pa0006 into wa_pa0006 with key anssa = '1'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
wa_zfivms_brd-STREET = 'ONGC'.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
"wa_pa0006-stras.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
endif.
else.
read table ist_pa0006 into wa_pa0006 with key anssa = '3'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
"wa_pa0006-ort01.
if not wa_pa0006-stras is initial.

wa_zfivms_brd-STR_SUPPL1 ='TEL BHAVAN'.

" wa_pa0006-stras

else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.

"wa_pa0006-ort01

.

.

if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
wa_zfivms_brd-STREET = 'ONGC'.
endif.
* Begin of <> on 30052012
else.
wa_zfivms_brd-STREET = 'ONGC'.
wa_zfivms_brd-CITY1 = 'DEHRADUN'.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
* End of <> on 30052012
endif.
endif.

"wa_pa0006-stras.

*Begin of <> on 20062011 21062011
refresh ist_zfivms_brd1.
select * from zfivms_brd into corresponding fields of table ist_zfivm
s_brd1 where span = wa_zfivms_brd-span.
if sy-subrc = 0.
* Begin of <> on 22052012
clear l_line.
describe table ist_zfivms_brd1 lines l_line.
shift l_line left deleting leading '0'.
"16072012
if l_line > 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 with key seqno = l_
line.
if sy-subrc <> 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
endif.
*
sort ist_zfivms_brd1 descending by seqno.
*
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
if sy-subrc = 0.
wa_zfivms_brd-seqno = wa_zfivms_brd1-seqno + 1.
endif.
endif.
modify zfivms_brd from wa_zfivms_brd .
else.
* End of <> on 20062011
wa_zfivms_brd-seqno = 1.
modify zfivms_brd from wa_zfivms_brd .
endif.
* modify zfivms_brd from wa_zfivms_brd .
"ENDDA
**Begin RD1K991769 CAB_ALOK
* if sy-subrc = 0.
*
update pa0009 set flag1 = 'R' where pernr = wa_pa0009-pernr and end
da = l_enddate and subty = '0'.
*
clear : wa_zfivms_brd .
"21062011
* endif.

*
*
*
*
*
*

update pa0009
set flag2 = 'X'
where pernr = wa_pa0009-pernr
and endda = l_enddate
and subty = '0'.
wa_pa0009-FLAG2 = 'X'.

wa_pa0009-FLAG1 = 'R'.
APPEND wa_pa0009 TO IST_pa0009_OVL.
**End RD1K991769 CAB_ALOK
*

clear : wa_zfivms_brd , wa_zfivms_brd1 .
append wa_zfivms_brd to ist_zfivms_brd.

"ENDDA

l_begdate = wa_pa0009-begda - 1.
**Begin RD1K991769 CAB_ALOK
clear wa_pa0009t.
**End RD1K991769 CAB_ALOK
* select single * from pa0009 into wa_pa0009t where pernr = wa_pa0009pernr "30062011
*
and endda = l_begdate
"begda = p_date and
*
and subty = '0'. " and
zlsch in ('S','N','8','9'). "('S','N'). 20062011 20062011
select single *
from pa0009
into wa_pa0009t
where pernr = wa_pa0009-pernr
and subty = '0'
and endda = l_begdate
and ( flag1 = '' or flag1 = 'S' or flag1 = 'F').
*
and FLAG2 = ''.
if sy-subrc = 0.
* Begin of <> on 15022012
*/..Begin of Change CR :30014976 by CAB_RAMA
* CONCATENATE 'EMP0000000000D00' wa_pa0009t-pernr into wa_zfivms_brd-sp
an.
CONCATENATE 'EM' sy-datum '000D00' wa_pa0009t-pernr into wa_zfivms_brd-s
pan.
*/..End of Change CR :30014976 by CAB_RAMA
*
CONCATENATE '0000000000000D00' wa_pa0009t-pernr into wa_zfivms_brdspan.
* End of <> on 15022012
**Begin RD1K991769 CAB_ALOK
*
if wa_pa0009t-bankl(4) = 'SBIN'.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl+6(5).
*
else.
*
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl.
*
endif.
wa_zfivms_brd-BANKL_NEW = wa_pa0009t-bankl.
**End RD1K991769 CAB_ALOK

wa_zfivms_brd-bankn_new = wa_pa0009t-bankn.
if not wa_pa0009t-emftx is initial.
wa_zfivms_brd-koinh_new = wa_pa0009t-emftx.
else.
select single ename from pa0001 into wa_zfivms_brd-koinh_new where
pernr = wa_pa0009t-pernr.
endif.
**Begin 26.05.2014 RD1K991769 CAB_ALOK CR 30010502:Beneficiary file-digi
tal signing, Logging
*REPLACE ALL OCCURRENCES OF '.' IN wa_zfivms_brd-koinh_new WITH space.
*translate
TRANSLATE wa_zfivms_brd-koinh_new USING trans_string.
**End RD1K991769 CAB_ALOK
Logging

CR 30010502:Beneficiary file-digital signing,

select single zphone from pa9205 into wa_zfivms_brd-tel_number where
pernr = wa_pa0009-pernr and subty = '01'.

wa_zfivms_brd-vmc_apdate = wa_pa0009-aedtm . " wa_pa0009-begda. 3012
wa_zfivms_brd-STATUS = 'RELEASE BY EMPLOYEE FOR OVL'.

refresh ist_pa0006.
select * from pa0006 into table ist_pa0006 where pernr = wa_pa0009tpernr
and anssa in ('1', '3'
)
and endda = l_enddate.
"'31.12.9999'.
read table ist_pa0006 into wa_pa0006 with key anssa = '1'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET = 'ONGC'.
"wa_pa0006-name2.
wa_zfivms_brd-CITY1 = 'DEHRADUN'. "wa_pa0006-ort01.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
read table ist_pa0006 into wa_pa0006 with key anssa = '3'.
if sy-subrc = 0.
if not wa_pa0006-name2 is initial.
wa_zfivms_brd-STREET ='ONGC'. " wa_pa0006-name2.
wa_zfivms_brd-CITY1 = 'DEHRADUN'. "wa_pa0006-ort01.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
else.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
else.
wa_zfivms_brd-CITY1 ='DEHRADUN'. " wa_pa0006-ort01.
if not wa_pa0006-stras is initial.
wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'. "wa_pa0006-stras.
else.

wa_zfivms_brd-STR_SUPPL1 = 'TEL BHAVAN'.
endif.
wa_zfivms_brd-STREET = 'ONGC'.
endif.
endif.
endif.
endif.
*Begin of <> on 20062011 21062011
refresh ist_zfivms_brd1.
select * from zfivms_brd into corresponding fields of table ist_zfi
vms_brd1 where span = wa_zfivms_brd-span.
if sy-subrc = 0.
* Begin of <> on 22052012
clear l_line.
describe table ist_zfivms_brd1 lines l_line.
shift l_line left deleting leading '0'.
"16072012
if l_line > 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 with key seqno =
l_line.
if sy-subrc <> 0.
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
endif.
*
sort ist_zfivms_brd1 descending by seqno.
*
read table ist_zfivms_brd1 into wa_zfivms_brd1 index 1.
if sy-subrc = 0.
wa_zfivms_brd-seqno = wa_zfivms_brd1-seqno + 1.
endif.
endif.
* End of <> on 22052012
modify zfivms_brd from wa_zfivms_brd .
else.
* End of <> on 20062011
wa_zfivms_brd-seqno = 1.
modify zfivms_brd from wa_zfivms_brd .
endif.
*
modify zfivms_brd from wa_zfivms_brd .
"ENDDA
**Begin RD1K991769 CAB_ALOK
*
if sy-subrc = 0.
*
update pa0009 set flag1 = 'D' where pernr = wa_pa0009t-pernr and
endda = l_begdate and subty = '0'.
*
clear : wa_zfivms_brd .
"21062011
*
endif.
*
*
*
*
*
*

update pa0009
set flag2 = 'X'
where pernr = wa_pa0009t-pernr
and endda = l_begdate
and subty = '0'.
wa_pa0009t-FLAG2 = 'X'.
wa_pa0009t-FLAG1 = 'D'.

APPEND wa_pa0009t TO IST_pa0009_OVL.
**End RD1K991769 CAB_ALOK
clear : wa_zfivms_brd , wa_zfivms_brd1 .
endif.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_7X_EMP
*&---------------------------------------------------------------------*
FORM ADD_7X_EMP .
* clear : l_spanemp,
*
l_spanasa,
*
l_span,
*
l_strlen.
* l_spanemp = wa_zfivms_brd-span(17).
l_spanasa = wa_zfivms_brd-span.
replace 'R0000' in l_spanasa with 'R0007'.
*
*
*
*
*
*
*

*
*
*
*

*
*
*
*

shift l_spanasa left deleting leading '0'.
l_strlen = strlen( l_spanasa ).
if l_strlen = '5'.
concatenate '70' l_spanasa into l_span.
elseif l_strlen = '6'.
concatenate '7' l_spanasa into l_span.
endif.

ENDFORM.

*&---------------------------------------------------------------------*
*& Form ADD_7X_EMP_D
*&---------------------------------------------------------------------*
FORM ADD_7X_EMP_D .
l_spanasa = wa_zfivms_brd-span.
replace 'D0000' in l_spanasa with 'D0007'.
ENDFORM.

*&---------------------------------------------------------------------*
*& Form CLEAR_VAR
*&---------------------------------------------------------------------*
form clear_var .

*
*
*
*

refresh: ist_94427 . "cvp-mum
refresh: ist_93128 . "cep-ddn
refresh: ist_30523 . "övl
refresh: ist_94427_file . "cvp-mum
refresh: ist_93128_file . "cep-ddn
refresh: ist_30523_file . "övl
***start of ONGC code
refresh: ist_96726 . " ECPF
refresh: ist_96726_file . "ECPF
***end of ONGC code
endform.

*&---------------------------------------------------------------------*
*& Form PREPARE_NEW_FORMAT
*&---------------------------------------------------------------------*
form prepare_new_format
TP

*
text

using p_type type zchar05

ep DDN 93128 and OVL 30523

p_corpid type zchar05

*
"3P, D3P, IBTP, DIB
" cvp MUM 94427, c

p_ist_record type standard table.

** final data in IST_94427 MUM
*
IST_93128 "ddn
*
IST_30523 cvp
*
*** 1.3P ( SBI, Registration)
*============================================
**AAA*10101010101*01234*0000009457001R0000876681*
**KOINH_NEW* BANKN_NEW * BANKL_NEW * SPAN *
=> need to be modified

** New Format
***Beneficiary Type
Beneficiary Action Type
Beneficiary Name
**
Beneficiary Account Number
Beneficiary Code IFS Code#
*
Address1 Address2 Add
ress3
***S
A
KOINH_NEW
**
BANKN_NEW
SPAN
BANKL_NEW
*
STREET STR_SUPPL1
CITY1
***2. D3P ( SBI, De-registration)
*============================================
**ORIENT ENTERPRISE GUJ PVT LTD#
____#00000030043632391#03993#00000024
00001R0000807379#
**KOINH_NEW #
# BANKN_NEW # BANKL_NEW # SPAN #
** New Format
**S D ComBenNSRegSBAdd1 00000037773091731 EDYWYE35 ICIC0000001 MIN NAGAR
WEST NETHAJI AV TAMILNADU INDIA
**
**S D KOINH_NEW BANKN_NEW
SPAN BANKL_NEW STREET STR_SUPPL1 CITY1
***3.0 IBTP ( Non-SBI, registration)
*============================================
**AS & Sons*361101010036053*UBIN0536113*ONGC*ONGC*ONGC**0000000001948R00
00100032*
**KOINH_NEW* BANKN_NEW * BANKL_NEW *STREET* STR_SUPPL1*CITY1*SPAN *
** New Format
**O A ComBenNSRegOBAdd1 111222333777 STSNS64636 ICIC0000001 MIN NAGAR WE
ST NETHAJI AV TAMILNADU INDIA
**O A KOINH_NEW BANKN_NEW
SPAN BANKL_NEW STREET STR_SUPPL1 CITY1
***4.0 DIBTP ( Non-SBI, De-registration)
*============================================
**AS & Sons*361101010036053*UBIN0536113*ONGC*ONGC*ONGC**0000000001948R00
00100032*
**KOINH_NEW* BANKN_NEW * BANKL_NEW *STREET* STR_SUPPL1*CITY1*SPAN *
** New Format
**O A ComBenNSRegOBAdd1 111222333777 STSNS64636 ICIC0000001 MIN NAGAR WE
ST NETHAJI AV TAMILNADU INDIA
**O A KOINH_NEW BANKN_NEW
SPAN BANKL_NEW STREET STR_SUPPL1 CITY1
data: l_koinh_new# type zfivms_brd-koinh_new, " KOINH_FI,
l_bankn_new type zfivms_brd-bankn_new, " BANKN,
l_bankl_new type zfivms_brd-bankl_new, " BANKK,
l_dummy(10),
l_span
type zfivms_brd-span, " ZSPAN,
l_street
type zfivms_brd-street,
l_str_suppl1 type zfivms_brd-str_suppl1,
l_city1
type zfivms_brd-city1,
l_phone(30).
data: wa_record type ty_record.
data: wa_94427 type ty_new_format.
data: wa_30523 type ty_new_format.
data: wa_93128 type ty_new_format.

data: wa_96726 type ty_new_format.
*BREAK-POINT.
if p_corpid = '94427'.

" 94427 - MUM " create IST_94427[]

case p_type.
when '3P'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_span .

l_bankn_new l_bankl_n

*

create new record
wa_94427-ben_type = 'S'.
"Beneficiary Type * 1
wa_94427-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_94427-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_94427-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
*now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_94427-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_94427-BEN_AC_NO.
wa_94427-ben_code = l_span .
"Beneficiary Code, 24
wa_94427-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_94427-addr1, wa_94427-addr2, wa_94427-addr3)
where span = l_span.
if wa_94427-addr1 is initial.
wa_94427-addr1 = 'ONGC'.
endif.
if wa_94427-addr2 is initial.
wa_94427-addr2 = 'ONGC'.
endif.
if wa_94427-addr3 is initial.
wa_94427-addr3 = 'ONGC'.
endif.
append wa_94427 to ist_94427.
clear: wa_record, wa_94427.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'D3P'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_koinh_new l_dummy l_bankn_new l_
bankl_new l_span .
*

create new record
wa_94427-ben_type = 'S'.

"Beneficiary Type * 1

wa_94427-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_94427-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_94427-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_94427-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_94427-BEN_AC_NO.
wa_94427-ben_code = l_span .
"Beneficiary Code, 24
wa_94427-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_94427-addr1, wa_94427-addr2, wa_94427-addr3)
where span = l_span.
if wa_94427-addr1 is initial.
wa_94427-addr1 = 'ONGC'.
endif.
if wa_94427-addr2 is initial.
wa_94427-addr2 = 'ONGC'.
endif.
if wa_94427-addr3 is initial.
wa_94427-addr3 = 'ONGC'.
endif.
append wa_94427 to ist_94427.
clear: wa_record, wa_94427.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'IBTP'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_street l_str_suppl1 l_city1 l_phone l_span .

l_bankn_new l_bankl_n

*

create new record
wa_94427-ben_type = 'O'.
"Beneficiary Type * 1
wa_94427-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_94427-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_94427-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
*Change Requirement: prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_94427-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_94427-BEN_AC_NO.
wa_94427-ben_code = l_span .
"Beneficiary Code, 24
wa_94427-ifsc
= l_bankl_new . "IFS Code# 11
***
***

SELECT SINGLE STREET STR_SUPPL1 CITY1
FROM ZFIVMS_BRD

***
***

INTO (WA_94427-ADDR1, WA_94427-ADDR2, WA_94427-ADDR3)
WHERE SPAN = L_SPAN.
wa_94427-addr1 = l_street .
wa_94427-addr2 = l_str_suppl1.
wa_94427-addr3 = l_city1.
if wa_94427-addr1 is initial.
wa_94427-addr1 = 'ONGC'.
endif.
if wa_94427-addr2 is initial.
wa_94427-addr2 = 'ONGC'.
endif.
if wa_94427-addr3 is initial.
wa_94427-addr3 = 'ONGC'.
endif.

append wa_94427 to ist_94427.
clear: wa_record, wa_94427.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1, l_phone.
endloop.
when 'DIBTP'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_bankn_new l_bankl_new l_koinh_n
ew l_span .
*

create new record
wa_94427-ben_type = 'O'.
"Beneficiary Type * 1
wa_94427-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_94427-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_94427-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
*now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_94427-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_94427-BEN_AC_NO.
wa_94427-ben_code = l_span .
"Beneficiary Code, 24
wa_94427-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_94427-addr1, wa_94427-addr2, wa_94427-addr3)
where span = l_span.
wa_94427-addr1 = l_street .
wa_94427-addr2 = l_str_suppl1.
wa_94427-addr3 = l_city1.
if wa_94427-addr1 is initial.
wa_94427-addr1 = 'ONGC'.
endif.
if wa_94427-addr2 is initial.
wa_94427-addr2 = 'ONGC'.
endif.
if wa_94427-addr3 is initial.
wa_94427-addr3 = 'ONGC'.

endif.
append wa_94427 to ist_94427.
clear: wa_record, wa_94427.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
endcase.
elseif p_corpid = '30523'.

" 30523 - ovl " prepare IST_30523[]

case p_type.
when '3P'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_span .

l_bankn_new l_bankl_n

*

create new record
wa_30523-ben_type = 'S'.
"Beneficiary Type * 1
wa_30523-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_30523-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_30523-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_30523-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_30523-BEN_AC_NO.
wa_30523-ben_code = l_span .
"Beneficiary Code, 24
wa_30523-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_30523-addr1, wa_30523-addr2, wa_30523-addr3)
where span = l_span.
if wa_30523-addr1 is initial.
wa_30523-addr1 = 'ONGC'.
endif.
if wa_30523-addr2 is initial.
wa_30523-addr2 = 'ONGC'.
endif.
if wa_30523-addr3 is initial.
wa_30523-addr3 = 'ONGC'.
endif.
append wa_30523 to ist_30523.
clear: wa_record, wa_30523.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'D3P'.

loop at p_ist_record into wa_record.
split wa_record at '#' into l_koinh_new l_dummy l_bankn_new l_
bankl_new l_span .
*

create new record
wa_30523-ben_type = 'S'.
"Beneficiary Type * 1
wa_30523-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_30523-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_30523-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_30523-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_30523-BEN_AC_NO.
wa_30523-ben_code = l_span .
"Beneficiary Code, 24
wa_30523-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_30523-addr1, wa_30523-addr2, wa_30523-addr3)
where span = l_span.
if wa_30523-addr1 is initial.
wa_30523-addr1 = 'ONGC'.
endif.
if wa_30523-addr2 is initial.
wa_30523-addr2 = 'ONGC'.
endif.
if wa_30523-addr3 is initial.
wa_30523-addr3 = 'ONGC'.
endif.
append wa_30523 to ist_30523.
clear: wa_record, wa_30523.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'IBTP'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_street l_str_suppl1 l_city1 l_phone l_span .

l_bankn_new l_bankl_n

*

create new record
wa_30523-ben_type = 'O'.
"Beneficiary Type * 1
wa_30523-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_30523-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_30523-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_30523-BEN_AC_NO
*
IMPORTING

*

OUTPUT = WA_30523-BEN_AC_NO.
wa_30523-ben_code = l_span .
"Beneficiary Code, 24
wa_30523-ifsc
= l_bankl_new . "IFS Code# 11

***
***
***
***

SELECT SINGLE STREET STR_SUPPL1 CITY1
FROM ZFIVMS_BRD
INTO (WA_30523-ADDR1, WA_30523-ADDR2, WA_30523-ADDR3)
WHERE SPAN = L_SPAN.
wa_30523-addr1 = l_street .
wa_30523-addr2 = l_str_suppl1.
wa_30523-addr3 = l_city1.
if wa_30523-addr1 is initial.
wa_30523-addr1 = 'ONGC'.
endif.
if wa_30523-addr2 is initial.
wa_30523-addr2 = 'ONGC'.
endif.
if wa_30523-addr3 is initial.
wa_30523-addr3 = 'ONGC'.
endif.

append wa_30523 to ist_30523.
clear: wa_record, wa_30523.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1, l_phone.
endloop.
when 'DIBTP'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_bankn_new l_bankl_new l_koinh_n
ew l_span .
*

create new record
wa_30523-ben_type = 'O'.
"Beneficiary Type * 1
wa_30523-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_30523-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_30523-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_30523-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_30523-BEN_AC_NO.
wa_30523-ben_code = l_span .
"Beneficiary Code, 24
wa_30523-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_30523-addr1, wa_30523-addr2, wa_30523-addr3)
where span = l_span.
wa_30523-addr1 = l_street .
wa_30523-addr2 = l_str_suppl1.
wa_30523-addr3 = l_city1.

if wa_30523-addr1 is initial.
wa_30523-addr1 = 'ONGC'.
endif.
if wa_30523-addr2 is initial.
wa_30523-addr2 = 'ONGC'.
endif.
if wa_30523-addr3 is initial.
wa_30523-addr3 = 'ONGC'.
endif.
append wa_30523 to ist_30523.
clear: wa_record, wa_30523.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
endcase.
elseif p_corpid = '93128'.

" 93128 - CEP " prepare IST_93128[]

case p_type.
when '3P'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_span .

l_bankn_new l_bankl_n

*

create new record
wa_93128-ben_type = 'S'.
"Beneficiary Type * 1
wa_93128-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_93128-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_93128-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_93128-ben_code = l_span .
"Beneficiary Code, 24
wa_93128-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_93128-addr1, wa_93128-addr2, wa_93128-addr3)
where span = l_span.
if wa_93128-addr1 is initial.
wa_93128-addr1 = 'ONGC'.
endif.
if wa_93128-addr2 is initial.
wa_93128-addr2 = 'ONGC'.
endif.
if wa_93128-addr3 is initial.
wa_93128-addr3 = 'ONGC'.
endif.

append wa_93128 to ist_93128.
clear: wa_record, wa_93128.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'D3P'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_koinh_new l_dummy l_bankn_new l_
bankl_new l_span .
*

create new record
wa_93128-ben_type = 'S'.
"Beneficiary Type * 1
wa_93128-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_93128-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_93128-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_93128-ben_code = l_span .
"Beneficiary Code, 24
wa_93128-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_93128-addr1, wa_93128-addr2, wa_93128-addr3)
where span = l_span.
if wa_93128-addr1 is initial.
wa_93128-addr1 = 'ONGC'.
endif.
if wa_93128-addr2 is initial.
wa_93128-addr2 = 'ONGC'.
endif.
if wa_93128-addr3 is initial.
wa_93128-addr3 = 'ONGC'.
endif.
append wa_93128 to ist_93128.
clear: wa_record, wa_93128.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'IBTP'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_street l_str_suppl1 l_city1 l_phone l_span .
*

l_bankn_new l_bankl_n

create new record
wa_93128-ben_type = 'O'.
"Beneficiary Type * 1
wa_93128-ben_act_type = 'A'.
"Beneficiary Action Type *
wa_93128-ben_name = l_koinh_new. "Beneficiary Name * 35

wa_93128-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_93128-ben_code = l_span .
"Beneficiary Code, 24
wa_93128-ifsc
= l_bankl_new . "IFS Code# 11
***
***
***
***

SELECT SINGLE STREET STR_SUPPL1 CITY1
FROM ZFIVMS_BRD
INTO (WA_93128-ADDR1, WA_93128-ADDR2, WA_93128-ADDR3)
WHERE SPAN = L_SPAN.
wa_93128-addr1 = l_street .
wa_93128-addr2 = l_str_suppl1.
wa_93128-addr3 = l_city1.
if wa_93128-addr1 is initial.
wa_93128-addr1 = 'ONGC'.
endif.
if wa_93128-addr2 is initial.
wa_93128-addr2 = 'ONGC'.
endif.
if wa_93128-addr3 is initial.
wa_93128-addr3 = 'ONGC'.
endif.

append wa_93128 to ist_93128.
clear: wa_record, wa_93128.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1, l_phone.
endloop.
when 'DIBTP'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_bankn_new l_bankl_new l_koinh_n
ew l_span .
*

create new record
wa_93128-ben_type = 'O'.
"Beneficiary Type * 1
wa_93128-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_93128-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_93128-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_93128-ben_code = l_span .
"Beneficiary Code, 24
wa_93128-ifsc
= l_bankl_new . "IFS Code# 11

select single street str_suppl1 city1
from zfivms_brd
into (wa_93128-addr1, wa_93128-addr2, wa_93128-addr3)
where span = l_span.
wa_93128-addr1 = l_street .
wa_93128-addr2 = l_str_suppl1.
wa_93128-addr3 = l_city1.
if wa_93128-addr1 is initial.
wa_93128-addr1 = 'ONGC'.
endif.
if wa_93128-addr2 is initial.
wa_93128-addr2 = 'ONGC'.
endif.
if wa_93128-addr3 is initial.
wa_93128-addr3 = 'ONGC'.
endif.
append wa_93128 to ist_93128.
clear: wa_record, wa_93128.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
endcase.
*****start trust
***start of ONGC code
elseif p_corpid = '96726'.

" 93128 - CEP " prepare IST_93128[]

case p_type.
when '3P'.
loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_span .

l_bankn_new l_bankl_n

*

create new record
wa_96726-ben_type = 'S'.
"Beneficiary Type * 1
wa_96726-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_96726-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_96726-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_96726-ben_code = l_span .
"Beneficiary Code, 24
wa_96726-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_96726-addr1, wa_96726-addr2, wa_96726-addr3)
where span = l_span.
if wa_96726-addr1 is initial.

wa_96726-addr1 = 'ECPF'.
endif.
***end of ONGC code
if wa_96726-addr2 is initial.
wa_96726-addr2 = 'ECPF'.
endif.
if wa_96726-addr3 is initial.
wa_96726-addr3 = 'ECPF'.
endif.
append wa_96726 to ist_96726.
clear: wa_record, wa_96726.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'D3P'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_koinh_new l_dummy l_bankn_new l_
bankl_new l_span .
*

create new record
wa_96726-ben_type = 'S'.
"Beneficiary Type * 1
wa_96726-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_96726-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_96726-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_93128-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_93128-BEN_AC_NO.
wa_96726-ben_code = l_span .
"Beneficiary Code, 24
wa_96726-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_96726-addr1, wa_96726-addr2, wa_96726-addr3)
where span = l_span.
if wa_96726-addr1 is initial.
wa_96726-addr1 = 'ECPF'.
endif.
if wa_96726-addr2 is initial.
wa_96726-addr2 = 'ECPF'.
endif.
if wa_96726-addr3 is initial.
wa_96726-addr3 = 'ECPF'.
endif.
append wa_96726 to ist_96726.
clear: wa_record, wa_96726.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
when 'IBTP'.

loop at p_ist_record into wa_record.
split wa_record at '*' into l_koinh_new
ew l_street l_str_suppl1 l_city1 l_phone l_span .

l_bankn_new l_bankl_n

*

create new record
wa_96726-ben_type = 'O'.
"Beneficiary Type * 1
wa_96726-ben_act_type = 'A'.
"Beneficiary Action Type * 1
wa_96726-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_96726-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33
**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_96726-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_96726-BEN_AC_NO.
wa_96726-ben_code = l_span .
"Beneficiary Code, 24
wa_96726-ifsc
= l_bankl_new . "IFS Code# 11
***
***
***
***

SELECT SINGLE STREET STR_SUPPL1 CITY1
FROM ZFIVMS_BRD
INTO (WA_96726-ADDR1, WA_96726-ADDR2, WA_96726-ADDR3)
WHERE SPAN = L_SPAN.
wa_96726-addr1 = l_street .
wa_96726-addr2 = l_str_suppl1.
wa_96726-addr3 = l_city1.
if wa_96726-addr1 is initial.
wa_96726-addr1 = 'ECPF'.
endif.
if wa_96726-addr2 is initial.
wa_96726-addr2 = 'ECPF'.
endif.
if wa_96726-addr3 is initial.
wa_96726-addr3 = 'ECPF'.
endif.

append wa_96726 to ist_96726.
clear: wa_record, wa_96726.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1, l_phone.
endloop.
when 'DIBTP'.
loop at p_ist_record into wa_record.
split wa_record at '#' into l_bankn_new l_bankl_new l_koinh_n
ew l_span .
*

create new record
wa_96726-ben_type = 'O'.
"Beneficiary Type * 1
wa_96726-ben_act_type = 'D'.
"Beneficiary Action Type * 1
wa_96726-ben_name = l_koinh_new. "Beneficiary Name * 35
wa_96726-ben_ac_no = l_bankn_new. "Beneficiary Account Number
* 17, max :33

**now prefix 0 not needed
*
CALL FUNCTION 'CONVERSION_EXIT_ALPHA_INPUT'
*
EXPORTING
*
INPUT = WA_96726-BEN_AC_NO
*
IMPORTING
*
OUTPUT = WA_96726-BEN_AC_NO.
wa_96726-ben_code = l_span .
"Beneficiary Code, 24
wa_96726-ifsc
= l_bankl_new . "IFS Code# 11
select single street str_suppl1 city1
from zfivms_brd
into (wa_96726-addr1, wa_96726-addr2, wa_96726-addr3)
where span = l_span.
wa_96726-addr1 = l_street .
wa_96726-addr2 = l_str_suppl1.
wa_96726-addr3 = l_city1.
if wa_96726-addr1 is initial.
wa_96726-addr1 = 'ECPF'.
endif.
if wa_96726-addr2 is initial.
wa_96726-addr2 = 'ECPF'.
endif.
if wa_96726-addr3 is initial.
wa_96726-addr3 = 'ECPF'.
endif.
append wa_96726 to ist_96726.
clear: wa_record, wa_96726.
clear: l_koinh_new, l_bankn_new, l_bankl_new, l_span, l_street
, l_str_suppl1, l_city1.
endloop.
endcase.
*** end trust
endif. " P_CORPID

endform.

*&---------------------------------------------------------------------*
*& Form MERGE_FILE
*&---------------------------------------------------------------------*
form merge_file .
** Prepare records with
separator.
refresh: ist_94427_file . "cvp-mum
refresh: ist_93128_file . "cep-ddn
refresh: ist_30523_file . "övl

*
*
*
*

data: wa_94427 type ty_new_format.
data: wa_93128 type ty_new_format.
data: wa_30523 type ty_new_format.
data: wa_record_new type ty_record_new.
*** start trust
refresh: ist_96726_file . "övl
data: wa_96726 type ty_new_format.
*** end trust
*
BEN_TYPE(1),
"Beneficiary Type * 1
*
BEN_ACT_TYPE(1), " Beneficiary Action Type * 1
*
BEN_NAME(35),
"Beneficiary Name * 35
*
BEN_AC_NO(17),
"Beneficiary Account Number * 33//17
*
BEN_CODE(24),
"Beneficiary Code 20 (back to 24)
*
IFSC(11),
"IFS Code# 11
*
ADDR1(35),
"Address1 35
*
ADDR2(35),
"Address2 35
*
ADDR3(35),
"Address3 35
if p_payctr = 'CVP' .
loop at ist_94427 into wa_94427.
_name

concatenate wa_94427-ben_type

wa_94427-ben_act_type

wa_94427-ben

wa_94427-ben_ac_no wa_94427-ben_code wa_94427-ifsc
wa_94427-addr1 wa_94427-addr2 wa_94427-addr3
into wa_record_new-record separated by ' '.
append wa_record_new to ist_94427_file.

endloop.
endif.
if p_payctr = 'CEP' .
loop at ist_93128 into wa_93128.
_name

concatenate wa_93128-ben_type

wa_93128-ben_act_type

wa_93128-ben

wa_93128-ben_ac_no wa_93128-ben_code wa_93128-ifsc
wa_93128-addr1 wa_93128-addr2 wa_93128-addr3
into wa_record_new-record separated by ' '.
append wa_record_new to ist_93128_file.

endloop.
endif.
*** start trust
if p_payctr = 'ECPF' .
loop at ist_96726 into wa_96726.
_name

concatenate wa_96726-ben_type

wa_96726-ben_act_type

wa_96726-ben

wa_96726-ben_ac_no wa_96726-ben_code wa_96726-ifsc
wa_96726-addr1 wa_96726-addr2 wa_96726-addr3
into wa_record_new-record separated by ' '.

append wa_record_new to ist_96726_file.
endloop.
endif.
** end trust
if p_payctr = 'OVL' .
loop at ist_30523 into wa_30523.
_name

concatenate wa_30523-ben_type

wa_30523-ben_act_type

wa_30523-ben

wa_30523-ben_ac_no wa_30523-ben_code wa_30523-ifsc
wa_30523-addr1 wa_30523-addr2 wa_30523-addr3
into wa_record_new-record separated by ' '.
append wa_record_new to ist_30523_file.

endloop.
endif.

endform.

*&---------------------------------------------------------------------*
*& Form PROCESS
*&---------------------------------------------------------------------*
form process .

*
*
*
*

* IST_94427_FILE . "cvp-mum
* IST_93128_FILE . "cep-ddn
* IST_30523_FILE . "övl
if p_payctr = 'CVP'.
perform digital_sign_save_logs using ist_94427_file.
elseif p_payctr = 'CEP' .
perform digital_sign_save_logs using ist_93128_file.
*** start trust
elseif p_payctr = 'ECPF' .
perform digital_sign_save_logs using ist_96726_file.
*end trust
elseif p_payctr = 'OVL'.
perform digital_sign_save_logs using ist_30523_file.
endif.
endform.

*&---------------------------------------------------------------------*
*& Form DIGITAL_SIGN_SAVE_LOGS
*&---------------------------------------------------------------------*
form digital_sign_save_logs using ist_sbi_file type standard table.

*

**RAW DATA IST:
* IST_94427_FILE . "cvp-mum
* IST_93128_FILE . "cep-ddn
* IST_30523_FILE . "övl
** TABLE: ZFIBEN_FILE_MONI
*MANDT MANDT CLNT 3
*VMC_APDATE DATUM DATS 8
*SENT_DATE DATUM DATS 8
*SENT_TIME UZEIT TIMS 6
*SENT_FILENAME ZBENE_FILE_NAME CHAR 150
*SENT_FLAG FLAG CHAR 1
*SENT_USERID UNAME CHAR 12
*RECEIVED_DATE DATUM DATS 8
*RECEIVED_TIME UZEIT TIMS 6
*RECEIVED_FILENAME ZBENE_FILE_NAME CHAR 150
*RECEIVED_FLAG FLAG CHAR 1
*SENT_ERROR
STRING 0
*RAW_DATA
RAWSTRING 0
*DATA_SENT
RAWSTRING 0
*DATA_RECEIVED
RAWSTRING 0
data: wa_zfiben_file_moni type zfiben_file_moni.
data: l_vmc_apdate
type zfiben_file_moni-vmc_apdate,
l_sent_date
type zfiben_file_moni-sent_date,
l_sent_time
type zfiben_file_moni-sent_time,
l_sent_filename type zfiben_file_moni-sent_filename,
l_sent_flag
type zfiben_file_moni-sent_flag,
l_sent_userid
type zfiben_file_moni-sent_userid,
l_sent_error
type zfiben_file_moni-sent_error,
l_raw_data
type xstring, "ZFIBEN_FILE_MONI-RAW_DATA,
l_data_sent
type zfiben_file_moni-data_sent.
*=============================================
******** Monitoring
l_vmc_apdate = p_date.
l_sent_date = sy-datum.
l_sent_time = sy-uzeit.
l_sent_userid = sy-uname.
** Derive File Name: L_SENT_FILENAME
data: curr_date(6),
corpfilename(23),
corpacno(17).
** CURR_DATE
19801216
concatenate sy-datum+6(2) sy-datum+4(2) sy-datum+2(2) into curr_date.
if p_payctr = 'CVP'.
* COMPBEN. CURR_DATE 070114.ONGC94427 VMC_APDATE SENT_TIME .000000303405
74145.txt
concatenate 'ONGC94427' l_vmc_apdate l_sent_time into corpfilename
.
*
IF sy-host = 'eccdci' OR sy-host = 'eccqci'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030003223534.t

xt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
concatenate 'COMPBEN' curr_date corpfilename '00000030340574145' int
o l_sent_filename separated by '.' .
"changes on 30.07.14
*
ELSEIF sy-host+0(6) = 'eccpap'.
"S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030340574145.t
xt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
*
CONCATENATE 'COMPBEN' curr_date corpfilename '00000030340574145'
INTO l_sent_filename SEPARATED BY '.' .
"changes on 30.07.14 "S4H F
P3 changes
*
ENDIF.
"S4H FP3 changes
endif.
if p_payctr = 'CEP'.
*COMPBEN. CURR_DATE 070114.ONGC93128 VMC_APDATE SENT_TIME .0000003071731
4079.txt for
concatenate 'ONGC93128' l_vmc_apdate l_sent_time into corpfilename
.
*
if sy-host = 'eccdci' or sy-host = 'eccqci'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030003079576
.txt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
concatenate 'COMPBEN' curr_date corpfilename '00000030717314079' int
o l_sent_filename separated by '.' .
"changes on 30.07.14
*
elseif sy-host+0(6) = 'eccpap'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030717314079
.txt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
*
concatenate 'COMPBEN' curr_date corpfilename '00000030717314079'
into l_sent_filename separated by '.' .
"changes on 30.07.14 "S4H FP
*
endif. "S4H FP3 changes
endif.
***start trust
if p_payctr = 'ECPF'.
*COMPBEN. CURR_DATE 070114.ONGC93128 VMC_APDATE SENT_TIME .0000003071731
4079.txt for
concatenate 'ECPF96726' l_vmc_apdate l_sent_time into corpfilename
.
*
if sy-host = 'eccdci' or sy-host = 'eccqci'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030003079576
.txt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
concatenate 'COMPBEN' curr_date corpfilename '00000010392857009' int
o l_sent_filename separated by '.' .
"changes on 30.07.14
*
elseif sy-host+0(6) = 'eccpap'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030717314079
.txt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
*
concatenate 'COMPBEN' curr_date corpfilename '00000010392857009'
into l_sent_filename separated by '.' .
"changes on 30.07.14 "S4H FP
*
endif. "S4H FP3 changes
endif.
***end trust
if p_payctr = 'OVL'.
* OVL : COMPBEN.CURR_DATE 070114.OVL30523 VMC_APDATE SENT_TIME . 0000001
0277786661.txt
concatenate 'OVL30523' l_vmc_apdate l_sent_time into corpfilename .

*
if sy-host = 'eccdci' or sy-host = 'eccqci'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000030003139979.t
xt' INTO L_SENT_FILENAME SEPARATED BY '.' . "changes on 30.07.14
*
concatenate 'COMPBEN' curr_date corpfilename '00000030003139979'
into l_sent_filename separated by '.' .
"changes on 30.07.14 "S4H FP
*
elseif sy-host+0(6) = 'eccpap'. "S4H FP3 changes
*
CONCATENATE 'COMPBEN' CURR_DATE CorpFileName '00000010277786661
.txt' INTO L_SENT_FILENAME SEPARATED BY '.' .
"changes on 30.07.14
concatenate 'COMPBEN' curr_date corpfilename '00000010277786661' int
o l_sent_filename separated by '.' .
"changes on 30.07.14
*
endif. "S4H FP3 changes
endif.
*=============================================
** Digital signing
data: in_data_table like ssfbin occurs 0 with header line.
data: params like ssfparms.
data: crc
type i.
data: doc_sig type xstring.
data: doc_ver type xstring.
data: doc_line type string.
data: doc_tab type ssftxttab.
data: sig_list type ssfsignertab.
data: go_file_verifier type ref to lcl_file_verifier,
l_sign
type ssfsigner.
data it_zuser_signer type table of zuser_signer.
data wa_zuser_signer type zuser_signer.
data: ev_owner
type string,
ev_email
type string,
ev_serial
type string,
ev_thumbprint
type string,
ev_validfrom
type string,
ev_validto
type string,
ev_issuer
type string,
ev_bindocument_out type xstring,
e_int
type i.
data: lo_bnk_detail
type ref to zfi_bnk_sbi_in_encco_si_sbiweb,
proxy_data
type zfi_bnk_sbi_in_encfile_upload2,
lt_input
type zfi_bnk_sbi_in_encfile_upload1,
lo_sys_exception type ref to cx_ai_system_fault.
data: err_string
type string.
data: wa_pa0009_bak type pa0009.
create object lo_bnk_detail.

call function 'SCMS_TEXT_TO_BINARY'
* EXPORTING
*
FIRST_LINE
= 0
*
LAST_LINE
= 0
*
APPEND_TO_TABLE
= ' '
*
MIMETYPE
= ' '
*
ENCODING
=
importing
output_length = params-indatalen

tables
text_tab
= ist_sbi_file
binary_tab
= in_data_table
* EXCEPTIONS
*
FAILED
= 1
*
OTHERS
= 2
.
if sy-subrc <> 0.
message e570(zfi).
endif.
call function 'SCMS_BINARY_TO_XSTRING'
exporting
input_length = params-indatalen
*
FIRST_LINE
= 0
*
LAST_LINE
= 0
importing
buffer
= l_raw_data
tables
binary_tab
= in_data_table
exceptions
failed
= 1
others
= 2.
if sy-subrc <> 0.
message e570(zfi).
endif.
call function 'SSFS_CALL_CONTROL'
exporting
*
TXTDOCUMENT
= doc_tab
bindocument
= l_raw_data
doc_type
= 'TXT'
*
ssf_id
= 'CN=TEST CLASS IIIB SHATWO, SP=Gujarat, postalC
ode=392015, OU=CID - 2431135, OU=Marketing, O=Gujarat Narmada Valley Fer
tilizers Company Limited, C=IN'."wa_ZFI_BATCH_SIGN-ssf_screen.
importing
crc
= crc
signature
= doc_sig
exceptions
parameter_error = 1
conversion_error = 2
control_error
= 3
frontend_error
= 4
others
= 5.
**verify signature wrt userID
if crc = 0. " 'SSFS_CALL_CONTROL'
refresh sig_list.
call function 'SSFS_SERVER_VERIFY'
exporting
*
SSFAPPLIC
=
signature
= doc_sig
importing
crc
= crc
signerlist
= sig_list
txtdocument_out = doc_tab
bindocument_out = doc_ver
exceptions
kernel_error
= 1

parameter_error = 2
others
= 3.
if sy-subrc <> 0.
write: / 'Error while verifying signture:'(erv), sy-subrc.
exit.
elseif crc = 0. " signature OK
go_file_verifier = lcl_file_verifier=>get_instance( ).
read table sig_list into l_sign index 1.
if sy-subrc = 0.
call method go_file_verifier->get_signer_info
exporting
is_signerlist = l_sign
importing
ev_owner
= ev_owner
ev_email
= ev_email
ev_serial
= ev_serial
ev_thumbprint = ev_thumbprint
ev_validfrom = ev_validfrom
ev_validto
= ev_validto
ev_issuer
= ev_issuer
e_int
= e_int.
endif.
select * into table it_zuser_signer
from zuser_signer where uname = sy-uname
and active = 'X'.
if sy-subrc = 0.
read table it_zuser_signer into wa_zuser_signer with key thumbpr
in = ev_thumbprint.
if sy-subrc = 0.
l_data_sent = doc_sig.
else.
message 'Wrong Signature used for signing' type 'I'.
exit.
endif.
else.
message 'No Signature Maintained for user' type 'I'.
exit.
endif.
endif.
proxy_data-parameters-arg0
proxy_data-parameters-arg1

= l_sent_filename.
= l_data_sent.

try.
call method lo_bnk_detail->si_sbiwebservice_enc
exporting
output = proxy_data
importing
input = lt_input.
catch cx_ai_system_fault into lo_sys_exception.
err_string = lo_sys_exception->get_text( ).
catch cx_ai_application_fault .
endtry.
** update Moniotoring logs

in table ZFIBEN_FILE_MONI

wa_zfiben_file_moni-vmc_apdate = l_vmc_apdate.
wa_zfiben_file_moni-sent_date = l_sent_date.
wa_zfiben_file_moni-sent_time = l_sent_time .
wa_zfiben_file_moni-sent_filename = l_sent_filename.
wa_zfiben_file_moni-sent_userid = l_sent_userid.
wa_zfiben_file_moni-raw_data = l_raw_data.
wa_zfiben_file_moni-data_sent = l_data_sent.
wa_zfiben_file_moni-sent_flag = 'X' .
if err_string is not initial.
wa_zfiben_file_moni-sent_error = 'X' .
modify zfiben_file_moni from wa_zfiben_file_moni.
message 'Error while sending data' type 'I'.
else.
modify zfiben_file_moni from wa_zfiben_file_moni.
message 'Data sent successfully' type 'I'.
if p_payctr = 'CVP'.
" 'RELEASE BY VMC' & 'RELEASE BY CMC'.
data : lit_zfivmsbank1 type table of zfivmsbank.
data : lit_zsdcust_bank1 type table of zsdcust_bank.
data : lwa_zfivmsbank1 type zfivmsbank.
data : lwa_zsdcust_bank1 type zsdcust_bank.
select * from zfivmsbank into table lit_zfivmsbank1 where vmc_ap
date = p_date
and status = 'RELEASE BY VMC'.
if sy-subrc eq 0.
loop at lit_zfivmsbank1 into lwa_zfivmsbank1.
lwa_zfivmsbank1-status = 'SENT TO SBI'.
modify lit_zfivmsbank1 from lwa_zfivmsbank1 index sy-tabix.
endloop.
modify zfivmsbank from table lit_zfivmsbank1 ."added by gaurav
*
*
*

endif.
update zfivmsbank set status = 'SENT TO SBI'
where vmc_apdate = p_date
and status = 'RELEASE BY VMC'.

select * from zsdcust_bank into table lit_zsdcust_bank1 where ch
on = p_date
and status = 'RELEASE BY CMC'.
if sy-subrc eq 0.
loop at lit_zsdcust_bank1 into lwa_zsdcust_bank1.
lwa_zsdcust_bank1-status = 'SENT TO SBI'.
modify lit_zsdcust_bank1 from lwa_zsdcust_bank1 index sy-tab
ix.
endloop.
modify zsdcust_bank from table lit_zsdcust_bank1."added by gau
rav
endif.
*
*
*

UPDATE zsdcust_bank SET status = 'SENT TO SBI'
WHERE chon = p_date
AND status = 'RELEASE BY CMC'.

*****************************************************changes by gaurav o
n 22 may 2018
*
BREAK-POINT.

50),

data: it_zfi_alerts type table of zfi_alerts,
wa_zfi_alerts type zfi_alerts,
wa_zfi_alerts1 type zfi_alerts,
wa_zfi_alerts2 type zfi_alerts,
wa_zfi_alerts3 type zfi_alerts,
wa_obj_head
type solisti1,
wa_obj_cont
type solisti1,
it_obj_cont
type table of solisti1,
it_obj_head
type table of solisti1,
mail_sub
type sodocchgi1,
it_reclist
type table of somlreci1,
wa_reclist
type somlreci1,
lwa_zfivmsbank type zfivmsbank,
lit_zfivmsbank type table of zfivmsbank,
wa_new1
type zfivmsbank.
data: http_client type ref to if_http_client.
data: messg(160),
l_enddate
type pa0009-endda value '99991231',
ist_pa9205
type table of pa9205,
wa_pa9205
type
pa9205,
wf_string
type string,
mob_no(12),
result
type string,l_reason_desc(35), l_result(
disp_txt

type string.

clear: mob_no, messg, wf_string, result.
clear: l_reason_desc, l_result.
select * from zfivmsbank into table lit_zfivmsbank where vmc_apd
ate = p_date
and status = 'SENT TO SBI'.
select * from zfi_alerts into table it_zfi_alerts.

ts3.

loop at lit_zfivmsbank into lwa_zfivmsbank.
clear: wa_zfi_alerts,wa_zfi_alerts1,wa_zfi_alerts2,wa_zfi_aler

SAPMAIL'

refresh:it_obj_cont,it_obj_head,it_reclist.
read table it_zfi_alerts into wa_zfi_alerts with key categ = '
action =

'SUBJECT'
'SENT_2_SBI'.
if sy-subrc = 0.

ident =

replace all occurrences of 'XXXXXXXXXX' in wa_zfi_alerts-mat
t with lwa_zfivmsbank-reqno.
mail_sub-obj_descr = wa_zfi_alerts-matt.
= 'SAPMAIL'

read table it_zfi_alerts into wa_zfi_alerts1 with key categ

action
= 'GESTURE'.
*
IF sy-subrc = 0.
wa_obj_cont-line = wa_zfi_alerts1-matt.
append wa_obj_cont to it_obj_cont.
read table it_zfi_alerts into wa_zfi_alerts2 with key categ
= 'SAPMAIL'
action
= 'MESSAGE'

ident
= 'SENT_2_SBI'.
*
REPLACE ALL OCCURRENCES OF 'XXXXXXXXXX' IN wa
_zfi_alerts2-matt WITH zfivmsbank-reqno.
replace all occurrences of 'XXXXXXXXXX' in wa_zfi_alerts2-ma
tt with lwa_zfivmsbank-reqno.
replace all occurrences of 'ZFIVMSBANK-LIFNR' in wa_zfi_aler
ts2-matt with lwa_zfivmsbank-lifnr.
replace all occurrences of 'ZFIVMSBANK-NAME1' in wa_zfi_aler
ts2-matt with lwa_zfivmsbank-name1.
wa_obj_cont-line = wa_zfi_alerts2-matt.
append wa_obj_cont to it_obj_cont.
wa_reclist-receiver = lwa_zfivmsbank-cpf.
wa_reclist-rec_type = 'B'.
append wa_reclist to it_reclist.
ENDIF.

*

read table it_zfi_alerts into wa_zfi_alerts3 with key categ

= 'SMS'

action = '

MESSAGE'
ENT_2_SBI'.

ident = 'S
shift lwa_zfivmsbank-reqno left deleting leading '0'.
shift lwa_zfivmsbank-lifnr left deleting leading '0'.
wa_new1-name1 = lwa_zfivmsbank-name1+0(19).

replace all occurrences of 'XXXXXXXXXX' in wa_zfi_alerts3-ma
tt with lwa_zfivmsbank-reqno.
replace all occurrences of 'ZFIVMSBANK-LIFNR' in wa_zfi_aler
ts3-matt with lwa_zfivmsbank-lifnr.
replace all occurrences of 'ZFIVMSBANK-NAME1' in wa_zfi_aler
ts3-matt with wa_new1-name1.
replace all occurrences of '&' in wa_zfi_alerts3-matt with '
and'.
messg = wa_zfi_alerts3-matt.
*
*
*
*

*
*
*
*
*
*
*

endif.

wa_obj_cont-line = 'testing'.
APPEND wa_obj_cont TO it_obj_cont.
wa_obj_head-line = 'heading'.
APPEND wa_obj_head TO it_obj_head.

call function 'SO_NEW_DOCUMENT_SEND_API1'
exporting
document_data
= mail_sub
DOCUMENT_TYPE
= 'RAW'
put_in_outbox
= ' '
commit_work
= 'X'
IP_ENCRYPT
=
IP_SIGN
=
IMPORTING
SENT_TO_ALL
=
NEW_OBJECT_ID
=
tables
object_header
= it_obj_head
object_content
= it_obj_cont

CONTENTS_HEX
=
OBJECT_PARA
=
OBJECT_PARB
=
receivers
= it_reclist
exceptions
too_many_receivers
= 1
document_not_sent
= 2
document_type_not_exist
= 3
operation_no_authorization = 4
parameter_error
= 5
x_error
= 6
enqueue_error
= 7
others
= 8.
if sy-subrc = 0.
COMMIT WORK.
endif.

*
*
*

*

select * from pa9205 into corresponding fields of table ist_pa
9205 where pernr = lwa_zfivmsbank-cpf"ZMM_VEND_UNBLOCK-ERNAM
and subty = '01'

"22112013 by Sudhir Sharma

and endda = l_enddate.
if sy-subrc = 0.
sort ist_pa9205 by begda descending.
read table ist_pa9205 into wa_pa9205 index 1.
if sy-subrc = 0.
shift wa_pa9205-zphone left deleting leading '0'.
concatenate '91' wa_pa9205-zphone into mob_no.
clear wf_string .
concatenate
'http://10.205.48.190:13013/cgi-bin/sendsms?'
'username=ongc&password=ongc12&from=ONGC-OL&to=' mob_no
'&text=' messg
'&remLen=400' "180' 20032013
into wf_string .
call method cl_http_client=>create_by_url
exporting
url
= wf_string
importing
client
= http_client
exceptions
argument_not_found = 1
plugin_not_active = 2
internal_error
= 3
others
= 4.
call method http_client->send
exceptions
http_communication_failure = 1
http_invalid_state
= 2.
call method http_client->receive
exceptions
http_communication_failure = 1
http_invalid_state
= 2

http_processing_failed
= 3.
clear result .
result = http_client->response->get_cdata( ).
move result to l_result .
concatenate 'Message from SMS gateway:' l_result+2 into l_

result.

endif.
endif.
endloop.

*****************************************************changes by gaurav o
n 22 may.

elseif p_payctr = 'OVL'.
" 'RELEASE BY VMC FOR OVL'
update zfivmsbank set status = 'SENT TO SBI'
where vmc_apdate = p_date
and status = 'RELEASE BY VMC FOR OVL'.
" ZFIVMS_BRD-'RELEASE BY EMPLOYEE FOR OVL' === pa0009
" set flag - R / D
clear wa_pa0009_bak.
loop at ist_pa0009_OVL into wa_pa0009_BAK.
endloop.
modify pa0009 from table ist_pa0009_ovl.

*
*

*
*

elseif p_payctr = 'CEP'.
" ZFIVMS_BRD-'RELEASE BY EMPLOYEE' === pa0009
" set flag - R / D
clear wa_pa0009_bak.
loop at ist_pa0009_ONGC into wa_pa0009_BAK.
endloop.
modify pa0009 from table ist_pa0009_ongc.
endif.
endif.
else.

"\\ 'SSFS_CALL_CONTROL'

case crc.
when '2'.
message 'Please insert E-token' type 'I'.
exit.
when others.
message 'Error while signing' type 'E'.
exit.
endcase.
endif.

"\\ 'SSFS_CALL_CONTROL'

clear: wa_zfiben_file_moni.

endform.


