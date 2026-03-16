*
***INCLUDE LZOIU_DELIVERY_NETWORKF01 .
*

*

*&
*&
Form GET_REF_DELIVERY_NETWORK
*&
*
text
*
*
>P_W_MESSAGE_DS text
*
FORM get_ref_delivery_network USING delivery_network
text1
text2
text3
w_ref_fg.

*

*

*
*
*

DATA:

temp_dn_tb LIKE oiu_pr_dn,
not_found
TYPE c.
* Get the reference delivery network for creation
************************************************************************
*
Select master data using full key
*
************************************************************************
CALL FUNCTION 'OIU_DN_MASTER_READ'
EXPORTING
dn_no
= delivery_network
force_db
= 'X'
IMPORTING
e_oiu_pr_dn = temp_dn_tb
EXCEPTIONS
none_found = 1
OTHERS
= 2.
IF sy-subrc NE 0.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'002'
space
space
text1
text2
text3
space
space
space
space.
ENDIF.
IF w_ref_fg = c_flag_on.
MOVE-CORRESPONDING temp_dn_tb TO ref_delivery_network_ds.
MOVE-CORRESPONDING ref_delivery_network_ds TO roiu_pr_dn.
ELSE.
MOVE-CORRESPONDING temp_dn_tb TO roiu_pr_dn.
delivery_network_tb1 = roiu_pr_dn.
ENDIF.
**PERFORM CHECK_SETID USING ROIU_PR_DN-DN_SET.
PERFORM get_setid_desc.
ENDFORM.

"SO7K011423 BT

" GET_REF_DELIVERY_NETWORK

*&
*&
Form SET_ERROR_MESSAGE
*&
*
text
*
*
>P_W_MESSAGE_DS text
*
*FORM set_error_message USING
p_message LIKE bapiret2.
*
* IF p_message-type = c_messagetype_e.
*
MESSAGE ID p_message-id
*
TYPE
p_message-type
*
NUMBER
p_message-number
*
WITH
p_message-message_v1
*
p_message-message_v2
*
p_message-message_v3
*
p_message-message_v4.
* ENDIF.
*
*ENDFORM.
" SET_ERROR_MESSAGE
*&
*&
Form SAVE_DEL_NET_DATA
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM save_del_net_data.

26
*
*
*
*

*
*
*
*

CALL FUNCTION 'ZOIU_UPDATE_DELIVERY_NETWORK' IN UPDATE TASK
EXPORTING
i_delivery_network = roiu_pr_dn.
*

IF w_mode = c_create.
create node table entry
CALL FUNCTION 'OIU_NODE_DN_INSERT' IN UPDATE TASK
EXPORTING
i_dn_no = roiu_pr_dn-dn_no.
ENDIF.

* update hierarchy?
* PERFORM f_update_hierarchy. "SOGK027671 VV
*create outbound idoc
CALL FUNCTION 'ZIDOC_OUT_OIU_DELIVERY_NETWK' IN UPDATE TASK
EXPORTING
i_roiu_pr_dn = roiu_pr_dn.
ENDFORM.
" SAVE_DEL_NET_DATA
*&
*&
Form PREPARE_CREATE_FOR_SAVE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM prepare_create_for_save.
IF w_mode = c_create.

*
*
*
*

roiu_pr_dn-shtxt = roiu_pr_dn-dn_de.
SET LOCALE LANGUAGE sy-langu.
TRANSLATE roiu_pr_dn-shtxt TO UPPER CASE.
SET LOCALE LANGUAGE space.
roiu_pr_dn-xxdbaction = 'I'.
roiu_pr_dn-oiu_cruser = sy-uname.
GET TIME STAMP FIELD roiu_pr_dn-oiu_timestamp.
delivery_network_tb1 = roiu_pr_dn.
APPEND delivery_network_tb1.
ENDIF.

"#EC TRANSLANG

ENDFORM.
" PREPARE_CREATE_FOR_SAVE
*&
*&
Form PREPARE_UPDATE_FOR_SAVE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM prepare_update_for_save.
roiu_pr_dn-xxdbaction = c_update_act.
roiu_pr_dn-shtxt = roiu_pr_dn-dn_de.
SET LOCALE LANGUAGE sy-langu.
TRANSLATE roiu_pr_dn-shtxt TO UPPER CASE.
SET LOCALE LANGUAGE space.

*
*
*
*
"SOGK026707 BT
"#EC TRANSLANG

ENDFORM.
" PREPARE_UPDATE_FOR_SAVE
*&
*&
Form CHECK_FOR_CHANGES
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_for_changes.

*
*
*
*

IF w_mode NE c_display.
IF delivery_network_tb1 NE roiu_pr_dn OR w_mode = c_create
OR w_long_text_written_fg NE space.
IF w_fcode = c_back.
PERFORM popup_confirm_step USING text-003 text-004 text-005.
ELSE.
PERFORM popup_confirm_step USING text-003 text-004 text-006.
ENDIF.
ENDIF.
ENDIF.
ENDFORM.
*&
*&
Form
*&
*
text
*

" CHECK_FOR_CHANGES
*
POPUP_CONFIRM_STEP
*
*

*
>P_TEXT_003 text
*
>P_TEXT_004 text
*
>P_TEXT_005 text
*
FORM popup_confirm_step USING

*
text1
text2
title.

CALL FUNCTION 'POPUP_TO_CONFIRM_STEP'
EXPORTING
defaultoption = 'Y'
textline1
= text1
textline2
= text2
titel
= title
start_column
= 15
start_row
= 3
cancel_display = 'X'
IMPORTING
answer
= w_answer.
IF w_answer = c_yes.
CASE w_mode.
WHEN c_create.
PERFORM create_delivery_network.
WHEN c_update.
PERFORM update_delivery_network.
ENDCASE.
ELSE.
PERFORM rollback_longtext.
PERFORM unlock_delivery_number.
ENDIF.
ENDFORM.
" POPUP_CONFIRM_STEP
*&
*&
Form CHANGE_DOCUMENT
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM change_document USING dn1
STRUCTURE roiu_pr_dn
dn2
STRUCTURE roiu_pr_dn.

*
*
*
*

utime
= sy-uzeit.
udate
= sy-datum.
username = sy-uname.
tcode
= sy-tcode.
upd_oiu_pr_dn = c_update_act.
objectid = roiu_pr_dn-dn_no.
MOVE-CORRESPONDING dn1 TO *oiu_pr_dn.
MOVE-CORRESPONDING dn2 TO oiu_pr_dn.
PERFORM cd_call_oiu_dn.
ENDFORM.
*&
*&
Form
*&

" CHANGE_DOCUMENT
*
NO_CHANGES_MADE
*

*
text
*
*
> p1
text
* <
p2
text
*
FORM no_changes_made.

*
*

PERFORM set_return_message USING c_messagetype_s
c_messageid_cm
'011'
space
space
space
space
space
space
space
space
space.
LEAVE TO TRANSACTION sy-tcode.
ENDFORM.
" NO_CHANGES_MADE
*&
*&
Form CHECK_FOR_NUMBER
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_for_number.

*
*
*
*

************************************************************************
*
Select master data using full key
*
************************************************************************
ENDFORM.
" CHECK_FOR_NUMBER
*&
*&
Form GET_NUMBER_RANGE_INFO
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM get_number_range_info.
DATA:

l_subobject TYPE nrsobj.

* see if it was read last time
CHECK oiu_pr_dn_grp-dn_grp <> roiu_pr_dn-dn_grp.
SELECT SINGLE * FROM oiu_pr_dn_grp WHERE dn_grp = roiu_pr_dn-dn_grp.
IF sy-subrc <> 0.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'002'
space
space
text-002
roiu_pr_dn-dn_grp
space

*
*
*
*

space
space
space
space.
ENDIF.
* CALL FUNCTION 'NUMBER_GET_INFO'
*
EXPORTING
*
nr_range_nr
= oiu_pr_dn_grp-dn_nrnr
*
object
= c_num_rng_obj
**
SUBOBJECT
= ' '
**
TOYEAR
= '0000'
*
IMPORTING
*
interval
= w_interval_ds
*
EXCEPTIONS
*
interval_not_found = 1
*
object_not_found
= 2
*
OTHERS
= 3.
l_subobject = oiu_pr_dn_grp-dn_grp.
CALL FUNCTION 'OIU_NUMBER_GET_INFO'
EXPORTING
tabname
= g_tabname
externind
= oiu_pr_dn_grp-externind
subobject
= l_subobject
IMPORTING
nriv
= w_interval_ds
EXCEPTIONS
table_not_found
= 1
interval_not_found = 2
object_not_found
= 3
other_error
= 4
OTHERS
= 5.
IF sy-subrc NE 0.
PERFORM set_return_message USING sy-msgty
sy-msgid
sy-msgno
space
space
sy-msgv1
sy-msgv2
sy-msgv3
sy-msgv4
space
space
space.
ENDIF.
ENDFORM.
" GET_NUMBER_RANGE_INFO
*&
*&
Form CHECK_FOR_VALIDITY
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM check_for_validity.
DATA:

del_number TYPE i,
l_dn_grp_tx LIKE oiu_pr_dn_grp_tx-dn_grp_tx.

*
*
*
*

CONSTANTS:

c_internal(2)

TYPE c

VALUE '02'.

IF w_interval_ds-externind IS INITIAL.
CHECK w_interval_ds-nrrangenr = c_internal.
ENDIF.
PERFORM get_dn_grp_tx USING roiu_pr_dn-dn_grp
l_dn_grp_tx.
* internal number assignments
IF w_interval_ds-externind IS INITIAL
AND NOT roiu_pr_dn-dn_no IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
'OIUPR'
'274'
space
space
l_dn_grp_tx
space
space
space
space
space
space.
* external number assignments
*
*
*

begin change note 196949 05/10/2001 lk
ELSEIF W_INTERVAL_DS-EXTERNIND = C_TRUE
and ROIU_PR_DN-DN_NO IS INITIAL.
ELSEIF w_interval_ds-externind = c_true.
IF
roiu_pr_dn-dn_no IS INITIAL
OR ( ( w_interval_ds-fromnumber GT roiu_pr_dn-dn_no )"#EC PORTABLE
OR
( w_interval_ds-tonumber
LT
roiu_pr_dn-dn_no ) ).
*
end change note 196949 05/10/2001 lk
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

begin change note 196947 05/07/2001 lk
the table should not be locked for all the networks
when creating a external delivery network.
PERFORM unlock_delivery_number.
end change note 196947 05/07/2001 lk
PERFORM SET_RETURN_MESSAGE USING C_MESSAGETYPE_E
C_MESSAGEID_CM
'017'
SPACE
SPACE
TEXT-002
L_DN_GRP_TX
TEXT-002
SPACE
SPACE
SPACE
SPACE.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'019'
space
space
w_interval_ds-fromnumber
w_interval_ds-tonumber
space
space

space
space
space.
ENDIF.
* not in range
* ELSEIF W_INTERVAL_DS-EXTERNIND = C_TRUE
*
AND ( ( W_INTERVAL_DS-FROMNUMBER GT ROIU_PR_DN-DN_NO )"#EC
*PORTABLE
*
OR
( W_INTERVAL_DS-TONUMBER
LT
ROIU_PR_DN-DN_NO ) ).
*
PERFORM SET_RETURN_MESSAGE USING C_MESSAGETYPE_E
*
C_MESSAGEID_CM
*
'019'
*
SPACE
*
SPACE
*
W_INTERVAL_DS-FROMNUMBER
*
W_INTERVAL_DS-TONUMBER
*
SPACE
*
SPACE
*
SPACE
*
SPACE
*
SPACE.
*
end change note 196949 05/10/2001 lk
ENDIF.
ENDFORM.
" CHECK_FOR_VALIDITY
*&
*&
Form GET_DEL_NETWORK_NUMBER
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM get_del_network_number.
* Changed by VV on 20020729 for Maintaining Production Number range
DATA:

l_subobject TYPE nrsobj.

IF w_interval_ds-externind IS INITIAL.
CALL FUNCTION 'NUMBER_GET_NEXT'
EXPORTING
nr_range_nr
= w_interval_ds-nrrangenr
object
= w_interval_ds-object
quantity
= '1'
SUBOBJECT
= ' '
TOYEAR
= '0000'
IGNORE_BUFFER
= ' '
IMPORTING
number
= roiu_pr_dn-dn_no
QUANTITY
=
RETURNCODE
=
EXCEPTIONS
interval_not_found
= 1
number_range_not_intern = 2
object_not_found
= 3
quantity_is_0
= 4
quantity_is_not_1
= 5
interval_overflow
= 6
OTHERS
= 7.

*
*
*
*
*
**
**
**
*
*
**
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
l_subobject = roiu_pr_dn-dn_grp.
CALL FUNCTION 'OIU_GET_NEXT_SYS_NO'
EXPORTING
tabname
= g_tabname
subobject
= l_subobject
IMPORTING
sys_no
= roiu_pr_dn-dn_no
EXCEPTIONS
table_not_found
= 1
object_not_found
= 2
interval_not_found
= 3
number_range_not_intern = 4
interval_overflow
= 5
OTHERS
= 6.
IF sy-subrc NE 0.
PERFORM set_return_message USING sy-msgty
sy-msgid
sy-msgno
space
space
sy-msgv1
sy-msgv2
sy-msgv3
sy-msgv4
space
space
space.
ENDIF.
SET PARAMETER ID 'OIU_DN' FIELD roiu_pr_dn-dn_no.
ENDIF.
ENDFORM.
" GET_DEL_NETWORK_NUMBER
*&
*&
Form CHECK_FOR_DUPLICATE
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM check_for_duplicate.

*
*
*
*

DATA:
not_found TYPE c.
If in create mode and using an external number range check to see
if a delivery network with that number already exists
IF w_mode = c_create AND NOT roiu_pr_dn-dn_no IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
CALL FUNCTION 'OIU_DN_MASTER_READ'
EXPORTING
dn_no
= roiu_pr_dn-dn_no
force_db
= 'X'
*
IMPORTING
*
E_OIU_PR_DN =
EXCEPTIONS
none_found = 1
*
*

OTHERS

= 2.

IF sy-subrc = 0.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'003'
space
space
text-002
roiu_pr_dn-dn_no
space
space
space
space
space.
ENDIF.
ENDIF.
ENDFORM.
" CHECK_FOR_DUPLICATE
*&
*&
Form ADMINISTRATIVE_DATA
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM administrative_data.

*
*
*
*

************************************************************************
*
Select data using full key
*
************************************************************************
SELECT SINGLE dn_grp_tx FROM oiu_pr_dn_grp_tx
INTO roiu_administrative-fl_grp_tx
WHERE dn_grp = roiu_pr_dn-dn_grp
AND
spras = sy-langu.
IF sy-subrc NE 0.
CLEAR roiu_administrative-fl_grp_tx.
ENDIF.
roiu_administrative-fl_grp
= roiu_pr_dn-dn_grp.
roiu_administrative-oiu_timestamp = roiu_pr_dn-oiu_timestamp.
roiu_administrative-cruser
= roiu_pr_dn-oiu_cruser.
CALL FUNCTION 'OIU_CM_CREATE_GROUP_INFO'
EXPORTING
i_roiu_admin = roiu_administrative.
ENDFORM.
" ADMINISTRATIVE_DATA
*&
*&
Form CHECK_FOR_GAS_PLANT
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM check_for_gas_plant.
DATA: l_type(132) TYPE c.
* Check that only type P delivery networks contain a company code/ JV
IF NOT roiu_pr_dn-bukrs IS INITIAL OR

*
*
*
*

NOT roiu_pr_dn-vname IS INITIAL.
IF roiu_pr_dn-type_cd EQ c_gathering_sys
OR roiu_pr_dn-type_cd EQ c_fuel_system
OR roiu_pr_dn-type_cd EQ c_cluster.
CASE roiu_pr_dn-type_cd.
WHEN c_gathering_sys.
l_type = text-033.
WHEN c_fuel_system.
l_type = text-032.
WHEN c_cluster.
l_type = text-034.
ENDCASE.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
text-030
space
space
l_type
space
space
space.
ENDIF.
ENDIF.
IF NOT roiu_pr_dn-gplt_dn_no IS INITIAL
AND roiu_pr_dn-type_cd = c_fuel_system.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
text-012
space
text-032
space
space
space
space.
ENDIF.
* When DN type is 'P' and company code or joint venture are blank error
IF roiu_pr_dn-type_cd EQ c_gas_plant
AND ( roiu_pr_dn-bukrs IS INITIAL
OR
roiu_pr_dn-vname IS INITIAL ).
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'009'
space
space
text-030
space
space
text-012
space
space

space.
ELSE.
PERFORM check_joint_venture.
ENDIF.
ENDFORM.
" CHECK_FOR_GAS_PLANT
*&
*&
Form CHECK_VALID_GASPLANT_NETWORK_I
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM check_valid_gasplant_ntwk_id.
DATA:

36

*
*
*
*

temp_gplt_dn_tb LIKE oiu_pr_dn.

************************************************************************
*
Select master data using full key
*
***********************************************************************
CALL FUNCTION 'OIU_DN_MASTER_READ'
EXPORTING
dn_no
= roiu_pr_dn-gplt_dn_no
force_db
= 'X'
IMPORTING
e_oiu_pr_dn = temp_gplt_dn_tb
EXCEPTIONS
none_found = 1
OTHERS
= 2.
IF sy-subrc NE 0.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'002'
space
space
text-012
text-002
roiu_pr_dn-gplt_dn_no
space
space
space
space.
ELSEIF roiu_pr_dn-dn_no = roiu_pr_dn-gplt_dn_no.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_pr
'010'
space
space
space
space
space
space
space
space
space.
ENDIF.

ENDFORM.
" CHECK_VALID_GASPLANT_NETWORK_I
*&
*&
Form CHECK_COUNTRY_AND_REGION
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_country_and_region.
CALL FUNCTION 'OIU_VERIFY_PRIMARY_GEO_LOC'
EXPORTING
land1
= roiu_pr_dn-cntry_cd
pri_loc_code
= roiu_pr_dn-pri_geo_loc
IMPORTING
pri_geo_loc_required = w_geo_required_fg
EXCEPTIONS
region_required
= 1
region_not_valid
= 2
country_not_found
= 3
*
OTHERS
= 4
.
IF sy-subrc <> 0.
IF sy-msgty = 'E'.
CLEAR w_fcode.
ENDIF.
PERFORM set_return_message USING sy-msgty
sy-msgid
sy-msgno
space
space
sy-msgv1
sy-msgv2
sy-msgv3
sy-msgv4
space
space
space.
ENDIF.
IF w_geo_required_fg = c_true.
CALL FUNCTION 'OIU_VERIFY_SECONDARY_GEO_LOC'
EXPORTING
land1
= roiu_pr_dn-cntry_cd
pri_loc_code
= roiu_pr_dn-pri_geo_loc
sec_loc_code
= roiu_pr_dn-sec_geo_loc
EXCEPTIONS
county_city_required = 1
county_city_not_valid = 2
country_not_found
= 3
pri_geo_loc_not_found = 4
*
OTHERS
= 5
.
IF sy-subrc <> 0.
IF sy-msgty = 'E'.
CLEAR w_fcode.
ENDIF.
PERFORM set_return_message USING sy-msgty

37

*
*
*
*

sy-msgid
sy-msgno
space
space
sy-msgv1
sy-msgv2
sy-msgv3
sy-msgv4
space
space
space.
ENDIF.
ENDIF.
ENDFORM.
" CHECK_COUNTRY_AND_REGION
*&
*&
Form LOCK_DELIVERY_NUMBER
*&
*
text
*
*
>P_W_RETURN_DS text
*
FORM lock_delivery_number.
CALL FUNCTION 'ENQUEUE_E_OIU_DN'
EXPORTING
mode_oiu_pr_dn = 'E'
mandt
= sy-mandt
dn_no
= roiu_pr_dn-dn_no
*
X_DN_NO
= ' '
*
_SCOPE
= '2'
*
_WAIT
= ' '
*
_COLLECT
= ' '
EXCEPTIONS
foreign_lock
= 1
system_failure = 2
OTHERS
= 3.
CASE sy-subrc.
WHEN 1.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'025'
space
space
text-002
roiu_pr_dn-dn_no
space
sy-msgv1
space
space
space.
WHEN 2.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'026'
space
space
space
space
space

*
*
*
*

space
space
space
space.
WHEN 3.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'027'
space
space
space
space
space
space
space
space
space.
ENDCASE.
ENDFORM.
" LOCK_DELIVERY_NUMBER
*&
*&
Form UNLOCK_DELIVERY_NUMBER
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM unlock_delivery_number.
CALL FUNCTION 'DEQUEUE_E_OIU_DN'
EXPORTING
mode_oiu_pr_dn = 'E'
mandt
= sy-mandt
dn_no
= roiu_pr_dn-dn_no
*
X_DN_NO
= ' '
*
_SCOPE
= '3'
*
_SYNCHRON
= ' '
*
_COLLECT
= ' '
.
ENDFORM.
" UNLOCK_DELIVERY_NUMBER
*&
*&
Form LEAVE_TO_TRANSACTION
*&
*
text
*
*
>P_W_RETURN_DS text
*
>P_C_CREATE text
*
FORM leave_to_transaction USING
mode.
CALL FUNCTION 'OIU_LEAVE_TXN_CTL'
EXPORTING
i_mcode
= mode
i_cprog
= sy-cprog
i_format
= w_format
EXCEPTIONS
none_found = 1
OTHERS
= 2.

*
*
*
*

*
*
*
*

IF sy-subrc <> 0.
PERFORM set_return_message USING sy-msgty
sy-msgid
sy-msgno
space
space
sy-msgv1
sy-msgv2
sy-msgv3
sy-msgv4
space
space
space.
ENDIF.
ENDFORM.
" LEAVE_TO_TRANSACTION
*&
*&
Form USE_LONG_TEXT_PROCESSING
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM use_long_text_processing USING long_text_read_fg
long_text_fg
c_ltxt
long_text_written_fg.
DATA: long_text_name

LIKE thead-tdname.

MOVE roiu_pr_dn-dn_no TO long_text_name(20).
IF long_text_read_fg = c_flag_off.
w_new_mode = c_display.
CALL FUNCTION 'OIU_LONG_TEXT_MAINTAIN'
EXPORTING
oiu_flg_only_check
= c_flag_on
oiu_modus
= w_new_mode
oiu_table_name
= c_dn_tab_name
oiu_text_id
= c_ltxt
oiu_text_name
= long_text_name
*
OIU_TEXT_NAME_REF
= '
'
IMPORTING
oiu_flg_text_exists = long_text_fg
oiu_flg_txt_function = long_text_written_fg.
long_text_read_fg = c_flag_on.
ENDIF.
CASE w_mode.
WHEN c_display.
IF long_text_fg <> c_flag_off.
w_new_mode = c_display.
CALL FUNCTION 'OIU_LONG_TEXT_MAINTAIN'
EXPORTING
oiu_flg_only_check
= c_flag_off
oiu_modus
= w_new_mode
oiu_table_name
= c_dn_tab_name
oiu_text_id
= c_ltxt
oiu_text_name
= long_text_name
*
oiu_text_name_ref
= '
'

*
*
*
*

*

IMPORTING
oiu_flg_text_exists =
oiu_flg_txt_function = long_text_written_fg.

ELSE.
MESSAGE i036.
ENDIF.
WHEN OTHERS.
IF long_text_fg <> c_flag_off.
w_new_mode = c_update.
CALL FUNCTION 'OIU_LONG_TEXT_MAINTAIN'
EXPORTING
oiu_flg_only_check
= c_flag_off
oiu_modus
= w_new_mode
oiu_table_name
= c_dn_tab_name
oiu_text_id
= c_ltxt
oiu_text_name
= long_text_name
*
OIU_TEXT_NAME_REF
= '
'
IMPORTING
*
OIU_FLG_TEXT_EXISTS =
oiu_flg_txt_function = long_text_written_fg.
ELSE.
w_new_mode = c_create.
CALL FUNCTION 'OIU_LONG_TEXT_MAINTAIN'
EXPORTING
oiu_flg_only_check
= c_flag_off
oiu_modus
= w_new_mode
oiu_table_name
= c_dn_tab_name
oiu_text_id
= c_ltxt
oiu_text_name
= long_text_name
*
OIU_TEXT_NAME_REF
= '
'
IMPORTING
*
OIU_FLG_TEXT_EXISTS =
oiu_flg_txt_function = long_text_written_fg.
ENDIF.
ENDCASE.
ENDFORM.
" USE_LONG_TEXT_PROCESSING
*&
*&
Form COMMIT_ALL_WORK
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM commit_all_work.

*
*
*
*

CALL FUNCTION 'COMMIT_TEXT'.
COMMIT WORK.
ENDFORM.
" COMMIT_ALL_WORK
*&
*&
Form PREPARE_LONG_TEXT
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM prepare_long_text.

*
*
*
*

*
*
*
*

Temporary long text change flags are used because if the call
function is entered more than once the value passed back to the
program will clear your original flag when it should be set since
a prior change had been made
SET TITLEBAR c_longtxt WITH w_ttltxt.
PERFORM use_long_text_processing USING w_long_text_read_fg
w_long_text_fg
c_ltxt
w_temp_long_text_written_fg.
IF NOT w_temp_long_text_written_fg IS INITIAL.
MOVE w_temp_long_text_written_fg TO w_long_text_written_fg.
ENDIF.

ENDFORM.
" PREPARE_LONG_TEXT
*&
*&
Form GET_COUNTRY_TEXTS
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM get_country_texts.

*
*
*
*

IF NOT roiu_pr_dn-cntry_cd IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
IF t005t-land1 NE roiu_pr_dn-cntry_cd.
SELECT SINGLE * FROM t005t WHERE spras = sy-langu
AND
land1 = roiu_pr_dn-cntry_cd.
ENDIF.
ENDIF.
IF NOT roiu_pr_dn-pri_geo_loc IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
IF oiu_cm_lxpgl_tx-pri_geo_loc NE roiu_pr_dn-pri_geo_loc.
SELECT SINGLE * FROM oiu_cm_lxpgl_tx
WHERE spras
= sy-langu
AND
land1
= roiu_pr_dn-cntry_cd
AND
pri_geo_loc = roiu_pr_dn-pri_geo_loc.
ENDIF.
ELSE.
CLEAR: oiu_cm_lxpgl_tx-pri_geo_desc.
ENDIF.
IF NOT roiu_pr_dn-sec_geo_loc IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
SELECT SINGLE * FROM oiu_cm_lxsgl_tx
WHERE spras
= sy-langu
AND
land1
= roiu_pr_dn-cntry_cd
AND
pri_geo_loc = roiu_pr_dn-pri_geo_loc
AND
sec_geo_loc = roiu_pr_dn-sec_geo_loc.
ELSE.
CLEAR: oiu_cm_lxsgl_tx-sec_geo_desc.
ENDIF.

ENDFORM.
" GET_COUNTRY_TEXTS
*&
*&
Form GET_TYPECD_TEXTS
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM get_typecd_texts.
DATA: domain_value

*
*
*
*

LIKE dd07v-domvalue_l.

domain_value = roiu_pr_dn-type_cd.
IF NOT roiu_pr_dn-type_cd IS INITIAL.
CALL FUNCTION 'DOMAIN_VALUE_GET'
EXPORTING
i_domname = c_dn_typecd_dom
i_domvalue = domain_value
IMPORTING
e_ddtext
= dd07v-ddtext
*
EXCEPTIONS
*
NOT_EXIST = 1
*
OTHERS
= 2
.
ELSE.
CLEAR: dd07v-ddtext.
ENDIF.
ENDFORM.

" GET_TYPECD_TEXTS

*&
*&
Form GET_GAS_PLANT_TEXTS
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM get_gas_plant_texts.

*
*
*
*

DATA: l_vname_nm LIKE oiu_do_jv-vname_nm.
IF NOT roiu_pr_dn-bukrs IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
IF t001-bukrs NE roiu_pr_dn-bukrs.
SELECT SINGLE * FROM t001
WHERE bukrs = roiu_pr_dn-bukrs.
roiu_pr_dn-butxt = t001-butxt.
ENDIF.
ELSE.
CLEAR roiu_pr_dn-butxt.
ENDIF.
IF NOT roiu_pr_dn-vname IS INITIAL.
SELECT SINGLE * FROM oiu_do_jv
WHERE bukrs = roiu_pr_dn-bukrs
AND
vname = roiu_pr_dn-vname.

IF sy-subrc NE 0.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'095'
space
space
space
space
space
space
space
space
space.
ENDIF.
oiu_do_jv-vname_nm = oiu_do_jv-vname_nm.
ELSE.
CLEAR oiu_do_jv-vname_nm.
ENDIF.
IF NOT roiu_pr_dn-ref_dn_no IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
IF oiu_pr_dn-dn_de NE roiu_pr_dn-ref_dn_de.
SELECT SINGLE dn_de FROM oiu_pr_dn INTO roiu_pr_dn-ref_dn_de
WHERE dn_no = roiu_pr_dn-dn_no.
ENDIF.
ELSE.
CLEAR roiu_pr_dn-ref_dn_de.
ENDIF.
IF NOT roiu_pr_dn-gplt_dn_no IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
IF oiu_pr_dn-dn_de NE roiu_pr_dn-gplt_dn_de.
SELECT SINGLE dn_de FROM oiu_pr_dn INTO roiu_pr_dn-gplt_dn_de
WHERE dn_no = roiu_pr_dn-gplt_dn_no.
ENDIF.
ENDIF.
ENDFORM.
" GET_GAS_PLANT_TEXTS
*&
*
*&
Form SAVE_SUCCESSFUL
*&
*
*
text
*
*
*
> p1
text
* <
p2
text
*
*
FORM save_successful.
DATA: l_text LIKE sy-msgv1.
DATA: f_dnno
LIKE roiu_pr_dn-dn_no,
f_dnde
LIKE roiu_pr_dn-dn_de,
f_dnal
LIKE roiu_pr_dnvd-al_fl,
f_dnrc(2) TYPE c,

f_dnid(8) TYPE c

VALUE 'OIUGT_DN'.

f_dnrc = '01'.
f_dnno = roiu_pr_dn-dn_no.
f_dnde = roiu_pr_dn-dn_de.
EXPORT f_dnrc f_dnno f_dnde f_dnal TO MEMORY ID f_dnid.
CASE w_mode.
WHEN c_update.
l_text = text-chg.
WHEN c_create.
l_text = text-crt.
ENDCASE.
PERFORM set_return_message USING c_messagetype_s
c_messageid_cm
'006'
space
space
text-002
roiu_pr_dn-dn_no
space
l_text
space
space
space.
ENDFORM.
" SAVE_SUCCESSFUL
*&
*&
Form TRANSLATE_TEXTS
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM translate_texts.
DATA:

descr

*
*
*
*

LIKE roiu_pr_dn-dn_de.

descr = roiu_pr_dn-dn_de.
TRANSLATE roiu_pr_dn-dn_de TO UPPER CASE.
roiu_pr_dn-dn_de = descr.

"#EC TRANSLANG

ENDFORM.
" TRANSLATE_TEXTS
*&
*&
Form NUMBER_RANGE_CHECK_BAPI
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM number_range_check_bapi.
*
*
*

Check to see if the number range group exists
PERFORM check_for_number.
Get the number range information in order to get the number range
PERFORM get_number_range_info.
*
If the number range is external lock the delivery network number

*
*
*
*

IF w_interval_ds-externind = c_flag_on.
PERFORM lock_delivery_number.
ENDIF.
*
Checks to make sure if an internal number is selected
*
the delvery network field is blank
PERFORM check_for_validity.
*
If the indicator is not external set del network number = INTER-001
IF w_interval_ds-externind NE c_true.
roiu_pr_dn-dn_no = c_internal.
ENDIF.
ENDFORM.
" NUMBER_RANGE_CHECK_BAPI
*&
*
*&
Form BAPI_EDIT_CHECKS
*&
*
*
text
*
*
*
> p1
text
* <
p2
text
*
*
FORM bapi_edit_checks.
PERFORM check_dn_setid USING roiu_pr_dn-dn_set.
PERFORM check_setid USING roiu_pr_dn-dn_set.
* Check for a duplicate delivery network
PERFORM check_for_duplicate.
* Check for a duplicate DN in Razorfish OIUH_PR_DN table
"N 1732281
PERFORM check_razorfish_dn_duplicate.
"N 1732281
* Check that required field description is populated
PERFORM check_description.
PERFORM verify_entity_name.
* Check that required field DN type is populated and with a valid value
PERFORM check_dn_type.
* Check that required field all. tol. is populated and with a valid valu
PERFORM check_allocation_tolerance.
* Check for valid country and city information
PERFORM check_country_and_region.
* Check that company code is populated when required and the value is
* valid
PERFORM check_company_code.
* Check that joint venture is populated when required and the populated
* value exists within the correct company code
PERFORM check_joint_venture.
* When the gas plant delivery network is filled make sure it is valid
IF NOT roiu_pr_dn-gplt_dn_no IS INITIAL.
PERFORM check_valid_gasplant_ntwk_id.
ENDIF.
ENDFORM.

" BAPI_EDIT_CHECKS

*&
*&
Form CHECK_DESCRIPTION
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_description.
IF roiu_pr_dn-dn_de IS INITIAL.
PERFORM set_return_message USING c_messagetype_e

*
*
*
*

c_messageid_cm
'008'
space
space
text-025
space
space
space
space
space
space.
ENDIF.
ENDFORM.

" CHECK_DESCRIPTION

*&
*&
Form CHECK_ALLOCATION_TOLERANCE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_allocation_tolerance.

*
*
*
*

IF roiu_pr_dn-tol_pc IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'008'
space
space
text-026
space
space
space
space
space
space.
* Check to see if the allocation tolerance falls within the valid range
ELSEIF ( roiu_pr_dn-tol_pc < 0 OR roiu_pr_dn-tol_pc > 999 ).
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'065'
space
space
text-026
c_zero
c_lt_thousand
space
space
space
space.
ENDIF.
ENDFORM.
*&
*&
*&
*
*

" CHECK_ALLOCATION_TOLERANCE
*
Form

CHECK_DN_TYPE
*

text
*

*
> p1
text
* <
p2
text
*
FORM check_dn_type.

*

IF roiu_pr_dn-type_cd IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'008'
space
space
text-027
space
space
space
space
space
space.
ELSEIF roiu_pr_dn-type_cd NE c_gas_plant
AND roiu_pr_dn-type_cd NE c_fuel_system
AND roiu_pr_dn-type_cd NE c_cluster
AND roiu_pr_dn-type_cd NE c_gathering_sys.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
roiu_pr_dn-type_cd
space
text-027
space
space
space
space.
ENDIF.
ENDFORM.
" CHECK_DN_TYPE
*&
*&
Form CHECK_COMPANY_CODE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_company_code.
DATA:

*
*
*
*

l_type(132) TYPE c.

IF roiu_pr_dn-type_cd = c_gas_plant
AND NOT roiu_pr_dn-bukrs IS INITIAL.
*
Check to see if the company code is valid.
************************************************************************
*
Select master data using full key
*
************************************************************************
SELECT SINGLE * FROM t001 WHERE bukrs = roiu_pr_dn-bukrs.
IF sy-subrc NE 0.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'005'
space

space
text-014
roiu_pr_dn-bukrs
space
text-fnd
space
space
space.
ENDIF.
ELSEIF roiu_pr_dn-type_cd = c_gas_plant
AND roiu_pr_dn-bukrs IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'008'
space
space
text-012
text-002
space
space
space
space
space.
ELSEIF roiu_pr_dn-type_cd NE c_gas_plant
AND NOT roiu_pr_dn-bukrs IS INITIAL.
CASE roiu_pr_dn-type_cd.
WHEN c_gathering_sys.
l_type = text-033.
WHEN c_fuel_system.
l_type = text-032.
WHEN c_cluster.
l_type = text-034.
ENDCASE.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
text-014
space
l_type
text-002
space
space
space.
ENDIF.
ENDFORM.

" CHECK_COMPANY_CODE

*&
*&
Form CHECK_JOINT_VENTURE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_joint_venture.
DATA: l_type(132)
TABLES: oiu_do_di.

TYPE c.

*
*
*
*

IF roiu_pr_dn-type_cd = c_gas_plant
AND NOT roiu_pr_dn-bukrs IS INITIAL
AND NOT roiu_pr_dn-vname IS INITIAL.
************************************************************************
*
Select master data using full key
*
************************************************************************
SELECT * FROM oiu_do_jv
WHERE bukrs = roiu_pr_dn-bukrs
AND vname = roiu_pr_dn-vname.
ENDSELECT.
IF sy-subrc NE 0.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
text-028
roiu_pr_dn-vname
text-014
roiu_pr_dn-bukrs
space
space
space.
ELSEIF oiu_do_jv-type_cd <> '04'.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
'OIUPR'
'283'
space
space
space
space
space
space
space
space
space.
ENDIF.
ELSEIF roiu_pr_dn-type_cd EQ c_gas_plant AND
NOT roiu_pr_dn-bukrs IS INITIAL AND
roiu_pr_dn-vname IS INITIAL.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'008'
space
space
text-028
space
space
space
space
space
space.
ELSEIF roiu_pr_dn-type_cd NE c_gas_plant
AND NOT roiu_pr_dn-bukrs IS INITIAL

AND NOT roiu_pr_dn-vname IS INITIAL.
CASE roiu_pr_dn-type_cd.
WHEN c_gathering_sys.
l_type = text-033.
WHEN c_fuel_system.
l_type = text-032.
WHEN c_cluster.
l_type = text-034.
ENDCASE.
CLEAR w_fcode.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'004'
space
space
text-030
space
space
l_type
space
space
space.
ENDIF.
ENDFORM.
" CHECK_JOINT_VENTURE
*&
*&
Form BAPI_EDIT_CHECKS_CHANGE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM bapi_edit_checks_change.

*
*
*
*

* Check that required field description is populated
PERFORM check_description.
* Check that required field all. tol. is populated and with a valid valu
PERFORM check_allocation_tolerance.
* Check for valid country and city information
PERFORM check_country_and_region.
* Check that company code is populated when required and the value is
* valid
PERFORM check_company_code.
* Check that joint venture is populated when required and the populated
* value exists within the correct company code
PERFORM check_joint_venture.
* When the gas plant delivery network is filled make sure it is valid
IF NOT roiu_pr_dn-gplt_dn_no IS INITIAL.
PERFORM check_valid_gasplant_ntwk_id.
ENDIF.
* Validate set id passed is a member of the standard hierarchy
PERFORM check_dn_setid USING roiu_pr_dn-dn_set.
PERFORM check_setid USING roiu_pr_dn-dn_set.
ENDFORM.
*&
*&
Form
*&
*
text
*

" BAPI_EDIT_CHECKS_CHANGE
*
SET_PARAMETER_ID
*
*

*
> p1
text
* <
p2
text
*
FORM set_parameter_id.

*

SET PARAMETER ID 'OIU_VENTURE_TYPE' FIELD c_gas_plant_jv.
IF w_mode NE c_create.
GET PARAMETER ID 'OIU_DN' FIELD roiu_pr_dn-dn_no.
IF sy-subrc = 0.
SELECT SINGLE dn_de FROM oiu_pr_dn INTO oiu_pr_dn-dn_de
WHERE dn_no = roiu_pr_dn-dn_no.
ENDIF.
ELSE.
CLEAR roiu_pr_dn-dn_de.
CLEAR setheadert-descript.
"SO7K011423 BT
GET PARAMETER ID 'OIU_DN_GRP' FIELD roiu_pr_dn-dn_grp.
IF sy-subrc = 0 AND
sy-dynnr = c_screen_1100.
* single select using key fields
SELECT SINGLE * FROM oiu_pr_dn_grp_tx
WHERE dn_grp = roiu_pr_dn-dn_grp
AND spras = sy-langu.
ENDIF.
*check count of available groups, if one - set the default
PERFORM check_single_grp.
GET PARAMETER ID 'OIU_DN_TYPE_CD' FIELD roiu_pr_dn-type_cd.
IF sy-subrc = 0.
PERFORM get_typecd_texts.
ENDIF.
IF roiu_pr_dn-type_cd IS INITIAL AND
w_mode = c_create.
SET PARAMETER ID 'OIU_DN_TYPE_CD' FIELD c_type_cd.
ENDIF.
GET PARAMETER ID 'OIU_DN_REF' FIELD roiu_pr_dn-ref_dn_no.
IF sy-subrc = 0.
SELECT SINGLE dn_de FROM oiu_pr_dn INTO roiu_pr_dn-ref_dn_de
WHERE dn_no = roiu_pr_dn-ref_dn_no.
ENDIF.
ENDIF.
ENDFORM.
" SET_PARAMETER_ID
*&
*&
Form ROLLBACK_LONGTEXT
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM rollback_longtext.
IF NOT w_long_text_written_fg IS INITIAL.
CALL FUNCTION 'DELETE_TEXT_FROM_CATALOG'
EXPORTING
object
= '*'
name
= '*'
id
= '*'
language = '*'

*
*
*
*

*
*
*

EXCEPTIONS
NOT_FOUND = 1
OTHERS
= 2
.
ENDIF.

ENDFORM.
" ROLLBACK_LONGTEXT
*&
*&
Form CREATE_DELIVERY_NETWORK
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM create_delivery_network.

*
*
*
*

*

Before allowing save - check enqueue of standard hierarchy set
PERFORM enqueue_setid USING space roiu_pr_dn-dn_set.
* Get unique delivery network number for an internal number
* assignment
PERFORM get_del_network_number.
* Check for Razorfish entry for internal Number range
"N 1736401
PERFORM check_razorfish_dn_duplicate .
"N 1736401
* Lock the assigned delivery network number
PERFORM lock_delivery_number.
* Fill remaining internal table info for the save
PERFORM prepare_create_for_save.
* Call the function module to insert new record
PERFORM save_del_net_data.
* Commits data to the DB and Rollsback any bad insert
PERFORM commit_all_work.
* User message stating that the save was successful
PERFORM save_successful.
* hierarchy update called separately
IF SY-SUBRC = 0.
"SOGK027671 VV
Perform F_UPDATE_HIERARCHY.
"SOGK027671 VV
COMMIT WORK.
"SOGK027671 VV
ENDIF.
"SOGK027671 VV
IF sy-calld = c_true OR sy-binpt = c_true.
LEAVE TO SCREEN 0.
ELSE.
LEAVE TO TRANSACTION sy-tcode.
ENDIF.
ENDFORM.
" CREATE_DELIVERY_NETWORK
*&
*&
Form UPDATE_DELIVERY_NETWORK
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM update_delivery_network.
*

Before allowing save - check enqueue of DN Standard Hierarchy
IF delivery_network_tb1-dn_set <> roiu_pr_dn-dn_set.
PERFORM enqueue_setid USING delivery_network_tb1-dn_set

*
*
*
*

roiu_pr_dn-dn_set.
ENDIF.
* Fill remaining internal table info for the save
PERFORM prepare_update_for_save.
* Call the function module to insert new record
PERFORM save_del_net_data.
* Calls the change document function module using the old record
* table and the changed record table
PERFORM change_document USING delivery_network_tb1
roiu_pr_dn.
* Commits all work to the DB and rollsback if bad update
PERFORM commit_all_work.
PERFORM save_successful.
* hierarchy update called separately
IF SY-SUBRC = 0.
Perform F_UPDATE_HIERARCHY.
COMMIT WORK.
ENDIF.

"SOGK027671 VV
"SOGK027671 VV
"SOGK027671 VV
"SOGK027671 VV

IF sy-calld = c_true OR sy-binpt = c_true.
LEAVE TO SCREEN 0.
ELSE.
LEAVE TO TRANSACTION sy-tcode.
ENDIF.
ENDFORM.
" UPDATE_DELIVERY_NETWORK
*&
*&
Form CHECK_INTITIAL_FIELDS
*&
*
text
*
*
> p1
text
* <
p2
text
*
*FORM check_intitial_fields.
*
* IF roiu_pr_dn-tol_pc IS INITIAL.
*
PERFORM set_return_message USING c_messagetype_w
*
c_messageid_cm
*
'005'
*
space
*
space
*
text-026
*
space
*
space
*
text-029
*
space
*
space
*
space.
* ENDIF.
*
*ENDFORM.
" CHECK_INTITIAL_FIELDS
*&
*&
Form CLEAR_INFORMATION
*&
*
text
*
*
> p1
text

*
*
*
*

*
*
*

* <
p2
text
*
FORM clear_information.

*

CLEAR: roiu_pr_dn.
ENDFORM.
" CLEAR_INFORMATION
*&
*&
Form CHANGE_TO_DN_DATED
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM change_to_dn_dated.

*
*
*
*

CLEAR w_answer.
IF w_mode NE c_display.
IF delivery_network_tb1 NE roiu_pr_dn OR w_mode = c_create
OR w_long_text_written_fg NE space.
PERFORM popup_confirm_step USING text-003 text-004 text-006.
ENDIF.
ENDIF.
IF w_answer <> c_cancel.
PERFORM leave_to_dn_dated_trans.
ENDIF.
ENDFORM.
" CHANGE_TO_DN_DATED
*&
*&
Form LEAVE_TO_DN_DATED_TRANS
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM leave_to_dn_dated_trans.

*
*
*
*

CASE w_fcode.
WHEN c_fcode_dnd1.
LEAVE TO TRANSACTION c_trans_dnd1.
WHEN c_fcode_dnd2.
LEAVE TO TRANSACTION c_trans_dnd2.
WHEN c_fcode_dnd3.
LEAVE TO TRANSACTION c_trans_dnd3.
WHEN c_fcode_dnd6.
LEAVE TO TRANSACTION c_trans_dnd6.
ENDCASE.
ENDFORM.
" LEAVE_TO_DN_DATED_TRANS
*&
*&
Form CHECK_COMBOBOX_STATUS
*&
* CHECKING THE STATUS OF THE COMBOBOXES ON THE SCREEN TO SEE IF THEY
* HAVE CHANGED BASED ON THE FUNCTION CODE. IF THE COMBOBOXES HAVE
* CHANGED, THEN DEPENDENT COMBOBOXES NEED TO BE CLEARED AND
* RE-POPULATED
*
FORM check_combobox_status.

*
*

*

IF w_cntry <> roiu_pr_dn-cntry_cd.
CLEAR roiu_pr_dn-pri_geo_loc.
CLEAR roiu_pr_dn-sec_geo_loc.
w_fcode = c_fcode_loc.
ELSEIF w_primary <> roiu_pr_dn-pri_geo_loc.
CLEAR roiu_pr_dn-sec_geo_loc.
w_fcode = c_fcode_loc.
ENDIF.
ENDFORM.

" CHECK_COMBOBOX_STATUS

*&
*&
Form CHECK_SINGLE_GRP
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM check_single_grp.
*This sets the group default if only one group exists
DATA: BEGIN OF grp_tb OCCURS 0.
INCLUDE STRUCTURE oiu_pr_dn_grp.
DATA: END OF grp_tb.

*
*
*
*

SELECT * FROM oiu_pr_dn_grp
APPENDING TABLE grp_tb.
IF sy-subrc = 0.
DESCRIBE TABLE grp_tb LINES sy-tfill.
IF sy-tfill = 1.
READ TABLE grp_tb.
IF g_bapi_fg EQ c_true.
roiu_pr_dn-dn_grp = grp_tb-dn_grp.
ELSE.
SET PARAMETER ID 'OIU_DN_GRP' FIELD grp_tb-dn_grp.
ENDIF.
ENDIF.
ENDIF.
ENDFORM.

" CHECK_SINGLE_GRP

*&
*&
Form CHECK_SETID
*&
* VALIDATE THE SET NAME THAT IS ENTERED BY THE USER.
*
1. CHECK THAT SET EXISTS.
*
2. CHECK THAT THE SET IS A MEMBER OF THE STANDARD HIERARCHY
*
*
>P_SET
SET NAME ENTERED BY THE USER ON SCREEN
*
FORM check_setid USING
set LIKE roiu_pr_dn-dn_set.
DATA: l_setid LIKE sethier-setid.
DATA: l_setname TYPE setnamenew.
DATA: ls_setinfo LIKE setinfo.
CLEAR setheadert-descript.
l_setname = set.

"SO7K011423 BT

*
*

*
*

CALL FUNCTION 'G_SET_ENCRYPT_SETID'
EXPORTING
setclass
= c_setclass
shortname
= l_setname
*
KOKRS
=
*
KTOPL
=
*
LIB
=
*
RNAME
=
*
ECCS_DIMEN
=
*
ECCS_ITCLG
=
*
ECCS_SITYP
=
IMPORTING
setid
= l_setid.
*
EXCEPTIONS
*
NO_CO_AREA_SPECIFIED = 1
*
ILLEGAL_SETCLASS
= 2
*
OTHERS
= 3
*
.
*
*

check the type
CALL FUNCTION 'G_SET_GET_INFO'
EXPORTING
setname = l_setid
IMPORTING
info
= ls_setinfo
EXCEPTIONS
OTHERS = 1.
IF sy-subrc <> 0.
PERFORM set_return_message
USING
c_messagetype_e
c_messageid_cm
'200'
space
space
set
space
space
space
space
space
space.
ENDIF.
setheadert-descript = ls_setinfo-title.
IF ls_setinfo-type NA 'BS'.
PERFORM set_return_message
USING
c_messagetype_e
c_messageid_cm
'201'
space
space
set
space
space
space

" SO7K011423 BT

57

space
space
space.
ELSE.
CALL FUNCTION 'OIU_CHECK_HIER_IN_STD'
EXPORTING
i_setclass
= c_setclass
i_setname
= l_setname
EXCEPTIONS
not_in_standard = 1
OTHERS
= 2.
IF sy-subrc <> 0.
PERFORM set_return_message
USING
c_messagetype_e
c_messageid_cm
'202'
space
space
set
space
space
space
space
space
space.
ENDIF.
ENDIF.
*

setheadert-descript = ls_setinfo-title.

ENDFORM.

SO7K011423 BT

" CHECK_SETID

*&
*&
Form CHECK_DN_SETID
*&
*
text
*
*
>P_ROIU_PR_DN_DN_SET text
*
FORM check_dn_setid USING
set LIKE roiu_pr_dn-dn_set.
IF set IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'008'
space
space
text-hie
space
space
space
space
space
space.
ENDIF.
ENDFORM.
*&
*&

*
*
*
*

" CHECK_DN_SETID
*
Form

GET_SETID_DESC

*&
*
1. CHECK THAT SET EXISTS.
*
2. CHECK THAT THE SET IS A MEMBER OF THE STANDARD HIERARCHY
*
3. Get the SETID's description
*
FORM get_setid_desc.
DATA: l_setid
LIKE sethier-setid.
DATA: l_setname TYPE setnamenew.
DATA: ls_setinfo LIKE setinfo.
CLEAR setheadert-descript.
l_setname = roiu_pr_dn-dn_set.
CALL FUNCTION 'G_SET_ENCRYPT_SETID'
EXPORTING
setclass = c_setclass
shortname = l_setname
IMPORTING
setid
= l_setid.
*

check the type
CALL FUNCTION 'G_SET_GET_INFO'
EXPORTING
setname = l_setid
IMPORTING
info
= ls_setinfo
EXCEPTIONS
OTHERS = 1.
IF sy-subrc <> 0.
EXIT.
ENDIF.
setheadert-descript = ls_setinfo-title.

ENDFORM.

" GET_SETID_DESC

59
*

*

*eject
*&
*&
Form ENQUEUE_SETID
*&
*
Lock Set ID's for Update - OLD and NEW prior to allowing save
*
*
> OLD_SET
Old Set ID from Record (Create = Blank)
*
> NEW_SET
New Set ID from Record
*
FORM enqueue_setid USING p_old
p_new.

*
*
*
*

DATA: l_setname TYPE setnamenew.
DATA: l_setid
TYPE setid.
* is the Hier. active?
m_hier_active.
* If "NO" then skip the hierarchy update.
IF w_hier_active IS INITIAL.
EXIT.
ENDIF.
IF NOT ( p_old IS INITIAL ).
l_setname = p_old.

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

CALL FUNCTION 'G_SET_ENCRYPT_SETID'
EXPORTING
setclass
= c_setclass
shortname
= l_setname
KOKRS
=
KTOPL
=
LIB
=
RNAME
=
ECCS_DIMEN
=
ECCS_ITCLG
=
ECCS_SITYP
=
IMPORTING
setid
= l_setid.
EXCEPTIONS
NO_CO_AREA_SPECIFIED = 1
ILLEGAL_SETCLASS
= 2
OTHERS
= 3
CALL FUNCTION 'G_SET_ENQUEUE_WITH_FATHERS'
EXPORTING
setid
= l_setid
EXCEPTIONS
foreign_lock
= 1
system_failure = 2
OTHERS
= 3.
IF sy-subrc <> 0.
CLEAR w_fcode.
PERFORM set_return_message
USING
c_messagetype_e
c_messageid_cm
'203'
space

" SOGK013053 EY
" SOGK013053 EY
" SOGK013053 EY
" SOGK013053 EY

space
space
space
space
space
space
space
space.

ENDIF.
ENDIF.
IF NOT ( p_new IS INITIAL ).
l_setname = p_new.

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

CALL FUNCTION 'G_SET_ENCRYPT_SETID'
EXPORTING
setclass
= c_setclass
shortname
= l_setname
KOKRS
=
KTOPL
=
LIB
=
RNAME
=
ECCS_DIMEN
=
ECCS_ITCLG
=
ECCS_SITYP
=
IMPORTING
setid
= l_setid.
EXCEPTIONS
NO_CO_AREA_SPECIFIED = 1
ILLEGAL_SETCLASS
= 2
OTHERS
= 3
CALL FUNCTION 'G_SET_ENQUEUE_WITH_FATHERS'
EXPORTING
setid
= l_setid
EXCEPTIONS
foreign_lock
= 1
system_failure = 2
OTHERS
= 3.
IF sy-subrc <> 0.
CLEAR w_fcode.
PERFORM set_return_message
USING
c_messagetype_e
c_messageid_cm
'203'
space
space
space
space
space
space
space
space
space.
ENDIF.

ENDIF.

ENDFORM.
" ENQUEUE_SETID
*&
*&
Form GET_DN_GRP_TX
*&
*
text
*
*
>P_ROIU_PR_DN_DN_GRP text
*
>P_L_DN_GRP_TX text
*
FORM get_dn_grp_tx USING
p_dn_grp
p_dn_grp_tx.

*
*
*
*

IF NOT p_dn_grp IS INITIAL.
SELECT SINGLE dn_grp_tx
INTO
p_dn_grp_tx
FROM
oiu_pr_dn_grp_tx
WHERE spras = sy-langu AND
dn_grp = p_dn_grp.
IF sy-subrc NE 0.
CLEAR p_dn_grp_tx.
ENDIF.
ELSE.
CLEAR p_dn_grp_tx.
ENDIF.
ENDFORM.

" GET_DN_GRP_TX

*&
*&
Form DEFAULT_HIER_NODE
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM default_hier_node.
DATA: l_single_dn_set TYPE setidlist,
l_setid
LIKE sethier-setid,
l_line_count
LIKE sy-index.

*
*
*
*

"SOGK005579 BT

CALL FUNCTION 'OIU_SET_STD_HIER_NODES'
EXPORTING
setclass
= c_setclass
IMPORTING
single_set
= l_single_dn_set
line_count
= l_line_count "SOGK005579 BT
EXCEPTIONS
default_node = 1
OTHERS
= 2.
CASE sy-subrc.
WHEN 1.
READ TABLE l_single_dn_set INTO l_setid INDEX 1.
roiu_pr_dn-dn_set = l_setid+4.
PERFORM check_setid USING roiu_pr_dn-dn_set. "SO7K011423 BT
IF l_line_count > 1.
"SOGK005579 BT

CLEAR roiu_pr_dn-dn_set.
ENDIF.
WHEN OTHERS.
CLEAR roiu_pr_dn-dn_set.
ENDCASE.
ENDFORM.

63

"SOGK005579 BT
"SOGK005579 BT

" DEFAULT_HIER_NODE

*&
*&
Form VERIFY_ENTITY_NAME
*&
*
text
*
*
> p1
text
* <
p2
text
*
FORM verify_entity_name.
DATA: l_oiu_cm_edit LIKE oiu_cm_edit.
*KH SELECT * FROM oiu_cm_edit
SELECT SINGLE * FROM oiu_cm_edit
"KH
INTO l_oiu_cm_edit
WHERE edit_no = '0009'.
*KH ENDSELECT.
CHECK l_oiu_cm_edit-status = c_true.
SET LOCALE LANGUAGE sy-langu.
roiu_pr_dn-shtxt = roiu_pr_dn-dn_de.
TRANSLATE roiu_pr_dn-shtxt TO UPPER CASE.
SET LOCALE LANGUAGE space.
SELECT * FROM oiu_pr_dn
WHERE shtxt = roiu_pr_dn-shtxt
AND
dn_no <> roiu_pr_dn-dn_no.
ENDSELECT.
IF sy-subrc = 0.
CLEAR w_fcode.
PERFORM set_return_message USING 'E'
'OIUCM'
'100'
space
space
text-cme
text-dup
oiu_pr_dn-dn_no
space
space
space
space.

*
*
*
*

"#EC TRANSLANG

ENDIF.
ENDFORM.

" VERIFY_ENTITY_NAME

*&
*&
Form F_UPDATE_HIERARCHY
*&
*
text
*
FORM f_update_hierarchy.
* SOGK013053

EY

3/13/02

* is the Hier. active?

OSS 126627/2002

*
*
*
Hierarchy performance review

m_hier_active.

" SOGK013053 EY

CHECK delivery_network_tb1-dn_set <> roiu_pr_dn-dn_set
OR w_mode
= c_create.
IF w_mode
= c_create.
w_old_set = space.
ELSE.
w_old_set = delivery_network_tb1-dn_set.
ENDIF.
w_new_set
= roiu_pr_dn-dn_set.
w_set_object = roiu_pr_dn-dn_no.
* If "Not Active" then skip the hierarchy update.
IF w_hier_active IS INITIAL.
ELSE.
CALL FUNCTION 'OIU_UPDATE_HIERARCHY' IN UPDATE TASK
EXPORTING
setclass = c_setclass
old_set = w_old_set
new_set = w_new_set
object
= w_set_object.
ENDIF.

" SOGK013053 EY
" SOGK013053 EY

" SOGK013053 EY

CALL FUNCTION 'OIU_UPDATE_CM_HIER' IN UPDATE TASK " SOGK013053 EY
EXPORTING
i_setcls = c_setclass
i_oldset = w_old_set
i_newset = w_new_set
i_object = w_set_object.
ENDFORM.
" F_UPDATE_HIERARCHY
*&
*&
Form CHECK_PREMAS_DN_DUPLICATE
*&
* checks the existence of DN number in razorfish table OIUH_PR_DN
* Note # 1732281
*
*
> p1
text
* <
p2
text
*
form CHECK_RAZORFISH_DN_DUPLICATE .
DATA : lv_dn_no LIKE OIUH_PR_DN-DN_DN_NO ,
lv_sub_dn LIKE
OIUH_PR_DN-DN_SUB_NWK_NO .
DATA : lt_oiuh_pr_dn TYPE TABLE OF OIUH_PR_DN .
DATA : ls_oiuh_pr_dn LIKE LINE OF lt_oiuh_pr_dn .
* the last 4 char as sub dn number
lv_sub_dn = roiu_pr_dn-dn_no+16(4).
* char 5 to 8 used as dn number
lv_dn_no = roiu_pr_dn-dn_no+12(4).
IF w_mode = c_create AND NOT roiu_pr_dn-dn_no IS INITIAL.
SELECT SINGLE *
INTO ls_oiuh_pr_dn
FROM OIUH_PR_DN
WHERE DN_DN_NO
= lv_dn_no AND
DN_SUB_NWK_NO = lv_sub_dn .
IF sy-subrc = 0.
*
Start of "N 1736401

*
*
*
*

IF w_interval_ds-externind IS INITIAL.
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'100'
space
space
text-037
space
space
space
space
space
space.
ELSE.
*
End of Note "N 1736401
PERFORM set_return_message USING c_messagetype_e
c_messageid_cm
'003'
space
space
text-036
text-002
roiu_pr_dn-dn_no
space
space
space
space.
ENDIF.
"N 1736401
ENDIF.
ENDIF.
endform.
" CHECK_PREMAS_DN_DUPLICATE
