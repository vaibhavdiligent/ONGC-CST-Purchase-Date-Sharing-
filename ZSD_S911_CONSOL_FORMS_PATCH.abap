*&---------------------------------------------------------------------*
*& ZSD_S911_CONSOL  /  include ZSD_S911_CONSOL_FORMS  /  FORM get_data
*& Fix for TSV_TNEW_PAGE_ALLOC_FAILED 02.11.2026 11:02:55 (CRS, job
*& ZSD_S911_CONSOL 11022100, user INE17050)
*&---------------------------------------------------------------------*
*& Root cause (from ST22):
*&   TA_ZSD_S911           1,505,481 rows x 4,426 bytes  = 6.7 GB
*&   TA_ZSD_S911_SAKNR     full copy of it (line 37)     = 6.7 GB
*&   TA_ZSD_S911_WERKS     full copy of it (line 50)     = 6.7 GB  <- dump
*&   Session quota 14 GB: the second full copy, made only to find the
*&   distinct GL accounts / plants, cannot be allocated.
*&   The following FOR ALL ENTRIES statements also run on the full
*&   1.5-million-row table instead of on the distinct values.
*&
*& Change: replace lines 36-70 of FORM get_data (block between
*&   "***SOA by Gajanan - 11.11.2024 - CR#8000003569***" and the
*&   "IF NOT ta_plant_trd IS INITIAL. SORT ... ENDIF." after the
*&   zsd_plant_trd select) with the block below. No copy of the big
*&   table is made; the three lookups are driven by small, duplicate-
*&   free key tables (a few hundred rows). Result tables TA_S911_GL,
*&   TA_S911_PLANT, TA_PLANT_TRD are filled exactly as before.
*&---------------------------------------------------------------------*

***SOA by Gajanan - 11.11.2024 - CR#8000003569***
* **BEGIN OF CHANGE 02.11.2026 - TSV_TNEW_PAGE_ALLOC_FAILED:
* distinct keys collected in small tables instead of copying TA_ZSD_S911
    DATA: ls_s911_key LIKE LINE OF ta_zsd_s911.
    DATA: lt_key_sakn1  LIKE SORTED TABLE OF ls_s911_key-sakn1
                          WITH UNIQUE KEY table_line,
          lt_key_werks  LIKE SORTED TABLE OF ls_s911_key-werks
                          WITH UNIQUE KEY table_line,
          lt_key_remark LIKE SORTED TABLE OF ls_s911_key-remark
                          WITH UNIQUE KEY table_line.

    LOOP AT ta_zsd_s911 ASSIGNING FIELD-SYMBOL(<ls_s911_key>).
      INSERT <ls_s911_key>-sakn1  INTO TABLE lt_key_sakn1.
      INSERT <ls_s911_key>-werks  INTO TABLE lt_key_werks.
      INSERT <ls_s911_key>-remark INTO TABLE lt_key_remark.
    ENDLOOP.
*   (duplicate INSERTs are ignored, sy-subrc 4, by the unique key)

*    DATA(ta_zsd_s911_saknr) = ta_zsd_s911.
*    SORT ta_zsd_s911_saknr BY sakn1.
*    DELETE ADJACENT DUPLICATES FROM ta_zsd_s911_saknr COMPARING sakn1.

    "Fetch GLs
    IF lt_key_sakn1 IS NOT INITIAL.
      SELECT * FROM zsd_s911_gl INTO TABLE ta_s911_gl
        FOR ALL ENTRIES IN lt_key_sakn1
         WHERE saknr = lt_key_sakn1-table_line.
    ENDIF.

    IF NOT ta_s911_gl IS INITIAL.
      SORT ta_s911_gl BY saknr.
    ENDIF.

*    DATA(ta_zsd_s911_werks) = ta_zsd_s911.
*    SORT ta_zsd_s911_werks BY werks.
*    DELETE ADJACENT DUPLICATES FROM ta_zsd_s911_werks COMPARING werks.

    "Fetch Plants
    IF lt_key_werks IS NOT INITIAL.
      SELECT * FROM zsd_s911_plant INTO TABLE ta_s911_plant
        FOR ALL ENTRIES IN lt_key_werks
         WHERE werks = lt_key_werks-table_line.
    ENDIF.

    IF NOT ta_s911_plant IS INITIAL.
      SORT ta_s911_plant BY werks.
    ENDIF.

    "Fetch Remark
    IF lt_key_remark IS NOT INITIAL.
      SELECT * FROM zsd_plant_trd INTO TABLE ta_plant_trd
        FOR ALL ENTRIES IN lt_key_remark
         WHERE remark = lt_key_remark-table_line.
    ENDIF.

    IF NOT ta_plant_trd IS INITIAL.
      SORT ta_plant_trd BY remark.
    ENDIF.

    FREE: lt_key_sakn1, lt_key_werks, lt_key_remark.
* **END OF CHANGE 02.11.2026
