*&---------------------------------------------------------------------*
*&  BUILD_BP_NODE - the business partner half of CVIS_EI_EXTERN
*&---------------------------------------------------------------------*
*  Taken from ZMMS_BP_MASS_UPLOAD, which creates business partners and
*  suppliers in S/4HANA through CL_MD_BP_MAINTAIN today.
*
*  The caller supplies nothing new. The account group is already in
*  LS_CENTRAL, the name, search terms, title and address are already in
*  LS_POSTAL_ADDR, and the grouping and roles are read here from the CVI
*  Customizing - the two tables the conversion filled - so no value has
*  to be known in advance or hard coded.
*
*  Call it once, immediately before the maintain call, then put the
*  result in the PARTNER component beside the VENDOR component the RFC
*  already builds.
*&---------------------------------------------------------------------*
FORM build_bp_node
  USING    iv_lifnr    TYPE lifnr                  " blank on a creation
           iv_task     TYPE cvis_ei_object_task    " 'I' or 'U' - never 'M'
           iv_category TYPE bu_type                " '2' organisation, '1' person
           is_central  TYPE vmds_ei_vmd_central
           is_postal   TYPE cvis_ei_1vl
  CHANGING cs_partner  TYPE bus_ei_extern
           cv_guid     TYPE bu_partner_guid.

  DATA: lt_g2b  TYPE SORTED TABLE OF cvic_vend_to_bp1 WITH NON-UNIQUE KEY account_group,
        lt_r2b  TYPE STANDARD TABLE OF cvic_vend_to_bp2 WITH EMPTY KEY,
        ls_role TYPE bus_ei_bupa_roles,
        ls_adr  TYPE bus_ei_bupa_address.

  CLEAR: cs_partner, cv_guid.

* --- 1. the partner has to be identified, on a creation too ------------
*     Without this every row comes back with "Specify at least one number
*     for the business partner" (message R11 123). A change names the
*     partner by the GUID the link table holds; a creation has no number
*     yet, so a GUID is generated here and becomes the new partner's.
  IF iv_task = 'I'.
    TRY.
        cv_guid = cl_system_uuid=>if_system_uuid_static~create_uuid_x16( ).
      CATCH cx_uuid_error.
        CLEAR cv_guid.
    ENDTRY.
  ELSE.
    SELECT SINGLE partner_guid FROM cvi_vend_link
      WHERE vendor = @iv_lifnr INTO @cv_guid.
  ENDIF.

  cs_partner-header-object_task = iv_task.
  IF cv_guid IS NOT INITIAL.
    cs_partner-header-object_instance-bpartnerguid = cv_guid.
  ENDIF.

* --- 2. category and grouping ------------------------------------------
*     BP_CONTROL has no DATAX counterpart, so these two are simply set.
*     The grouping is what gives the new partner its number, which is why
*     the vendor number range must not be drawn from any more.
  SELECT * FROM cvic_vend_to_bp1 INTO TABLE @lt_g2b.               "#EC CI_NOWHERE
  cs_partner-central_data-common-data-bp_control-category = iv_category.
  READ TABLE lt_g2b INTO DATA(ls_g2b)
       WITH KEY account_group = is_central-data-ktokk BINARY SEARCH.
  IF sy-subrc = 0.
    cs_partner-central_data-common-data-bp_control-grouping = ls_g2b-grouping.
  ENDIF.

* --- 3. name, search terms, title --------------------------------------
*     All of it is already in the address the RFC maps: BAPIAD1VL carries
*     NAME, NAME_2..4, SORT1, SORT2 and TITLE.
  IF iv_category = '2'.
    cs_partner-central_data-common-data-bp_organization-name1 = is_postal-data-name.
    cs_partner-central_data-common-data-bp_organization-name2 = is_postal-data-name_2.
    cs_partner-central_data-common-data-bp_organization-name3 = is_postal-data-name_3.
    cs_partner-central_data-common-data-bp_organization-name4 = is_postal-data-name_4.
    CALL FUNCTION 'ZGEN_UPDATE_X'
      EXPORTING data  = cs_partner-central_data-common-data-bp_organization
      IMPORTING datax = cs_partner-central_data-common-datax-bp_organization.
  ELSE.
*   An employee vendor is a person: the name is a first and a last name,
*   not an organisation name.
    cs_partner-central_data-common-data-bp_person-firstname = is_postal-data-name.
    cs_partner-central_data-common-data-bp_person-lastname  = is_postal-data-name_2.
    CALL FUNCTION 'ZGEN_UPDATE_X'
      EXPORTING data  = cs_partner-central_data-common-data-bp_person
      IMPORTING datax = cs_partner-central_data-common-datax-bp_person.
  ENDIF.

  cs_partner-central_data-common-data-bp_centraldata-searchterm1 = is_postal-data-sort1.
  cs_partner-central_data-common-data-bp_centraldata-searchterm2 = is_postal-data-sort2.
  cs_partner-central_data-common-data-bp_centraldata-title_key   = is_postal-data-title.
  CALL FUNCTION 'ZGEN_UPDATE_X'
    EXPORTING data  = cs_partner-central_data-common-data-bp_centraldata
    IMPORTING datax = cs_partner-central_data-common-datax-bp_centraldata.

* --- 4. roles ----------------------------------------------------------
*     Every account group maps to its own set - usually FLVN00 and
*     FLVN01 - and the Customizing says which, so nothing is hard coded.
  SELECT * FROM cvic_vend_to_bp2 INTO TABLE @lt_r2b               "#EC CI_NOWHERE
    WHERE account_group = @is_central-data-ktokk.
  LOOP AT lt_r2b INTO DATA(ls_r2b).
    CLEAR ls_role.
    ls_role-task     = 'I'.
    ls_role-data_key = ls_r2b-role.
    APPEND ls_role TO cs_partner-central_data-role-roles.
  ENDLOOP.

* --- 5. address --------------------------------------------------------
*     The same BAPIAD1VL the vendor node carries, so the partner and the
*     vendor cannot end up with two different addresses.
  CLEAR ls_adr.
  ls_adr-task = iv_task.
  MOVE-CORRESPONDING is_postal-data  TO ls_adr-data-postal-data.
  MOVE-CORRESPONDING is_postal-datax TO ls_adr-data-postal-datax.
  IF ls_adr-data-postal-datax IS NOT INITIAL.
    APPEND ls_adr TO cs_partner-central_data-address-addresses.
  ENDIF.

ENDFORM.

*&---------------------------------------------------------------------*
*&  How the RFC ends, in place of VMD_EI_API=>MAINTAIN_BAPI
*&---------------------------------------------------------------------*
*  DATA: ls_cvis  TYPE cvis_ei_extern,
*        lt_data  TYPE cvis_ei_extern_t,
*        lt_map   TYPE mdg_bs_bp_msgmap_t,
*        lt_ret   TYPE bapiretm,
*        lv_guid  TYPE bu_partner_guid,
*        lv_task  TYPE cvis_ei_object_task.
*
*  lv_task = COND #( WHEN lifnr IS INITIAL THEN 'I' ELSE 'U' ).
*
*  PERFORM build_bp_node USING    lifnr lv_task '2' ls_central ls_postal_addr
*                        CHANGING ls_cvis-partner lv_guid.
*
*  ls_cvis-vendor = ls_vendors_main.        " unchanged, as the RFC builds it
*  ls_cvis-vendor-header-object_task = lv_task.
*  APPEND ls_cvis TO lt_data.
*
*  cl_md_bp_maintain=>validate_single( EXPORTING i_data        = ls_cvis
*                                      IMPORTING et_return_map = lt_map ).
*  LOOP AT lt_map INTO DATA(ls_map) WHERE type CA 'EAX'.
*    "  ET_RETURN_MAP names the structure and field, so the response file
*    "  can point the portal at the column that is wrong.
*    APPEND VALUE bapiret2( type = ls_map-type id = ls_map-id
*                           number = ls_map-number message = ls_map-message )
*           TO lt_error.
*  ENDLOOP.
*  CHECK lt_error IS INITIAL.
*
*  cl_md_bp_maintain=>maintain( EXPORTING i_data   = lt_data
*                                         i_test_run = abap_false
*                               IMPORTING e_return = lt_ret ).
*
*  "  BAPI_TRANSACTION_COMMIT, not a bare COMMIT WORK: the business
*  "  partner hangs its own end of LUW processing off it. And the BP keeps
*  "  a memory for the LUW just closed - without clearing it the SECOND
*  "  row of the file is refused with "Parameter IV_X_SAVE is ' ' for FM
*  "  BUPA_CREATE_FROM_DATA".
*  CALL FUNCTION 'BAPI_TRANSACTION_COMMIT' EXPORTING wait = abap_true.
*  CALL FUNCTION 'BUP_MEMORY_CENTRAL_INIT'.
*
*  "  Both numbers for the response file. Unless the grouping is flagged
*  "  for the same number they are different, and the portal needs the BP.
*  SELECT SINGLE vendor  FROM cvi_vend_link WHERE partner_guid = @lv_guid INTO @lifnr.
*  SELECT SINGLE partner FROM but000        WHERE partner_guid = @lv_guid INTO @lv_bp.
