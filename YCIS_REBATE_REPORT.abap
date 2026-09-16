*&---------------------------------------------------------------------*
*& Report  YCIS_REBATE_REPORT
*&---------------------------------------------------------------------*
*& CIS 2026-27 - Rebate Order Report (grade-wise)
*&
*& GAIL enhancement (as discussed):
*&   1. Material Name / Grade  - the actual grade (KONDM) the rebate was
*&                               earned on (not the generic REBATE(POLYMER)).
*&   2. Actual Quantity        - the real qty the discount was applied on;
*&                               with capping it is split PROPORTIONATELY
*&                               across grades (per the split rule).
*&   3. One line per grade     - a rebate order expands into one row per
*&                               grade involved in the calculation.
*&
*& Source of truth:
*&   * Header / totals  -> YCIS_APPRVL (written by the workflow at L3):
*&       ORDER_NO, KUNNR, SALES_OFF, PERIOD_FROM/TO, PURCH_NO (reference),
*&       ELIG_QTY (eligible/discounted qty), LFT_QTY (lifted), REBATE_VAL,
*&       WAERS, REB_COND.  (The rebate ITEM in VBAP carries no qty, which is
*&       why the old report showed Discount Qty = 0.)
*&   * Grade split      -> S922 lifting for the customer + CIS period,
*&       grouped by grade (KONDM). ELIG_QTY and REBATE_VAL are then
*&       allocated to each grade in proportion to that grade's lifting.
*&       Capping is handled automatically because ELIG_QTY (already capped)
*&       is the amount being split.
*&   * Captured detail  -> if a grade-detail table YCIS_APPRVL_GRD exists
*&       and holds rows for the order, it is used as-is (captured at source);
*&       otherwise the split is reconstructed from S922 (works for old orders
*&       and needs no extra table).
*&
*&   Non-discount grades (PS/GS/Powder/Polyfines, table YCIS_NODISC_GRD)
*&   count for lifting/eligibility but earn NO discount, so they appear with
*&   their quantity and a ZERO rebate amount.
*&---------------------------------------------------------------------*
REPORT  ycis_rebate_report.

TYPE-POOLS: slis.

TABLES: ycis_apprvl.

*--------------------------------------------------------------------*
* Output structure - one row per grade
*--------------------------------------------------------------------*
TYPES: BEGIN OF ty_out,
         sel        TYPE flag,          " tick to print the Approval Note
         vbeln      TYPE vbeln_va,     " rebate order
         auart      TYPE auart,        " doc type
         erdat      TYPE erdat,        " created on
         bstkd      TYPE bstkd,        " reference no
         kunnr      TYPE kunnr,        " customer
         name1      TYPE name1_gp,     " customer name
         sales_off  TYPE vkbur,        " sales office
         kondm      TYPE kondm,        " grade
         kondm_txt  TYPE maktx,        " material name (from YRVA_GRADE_CISD -> MAKT)
         lft_qty    TYPE menge_d,      " lifted qty of this grade
         elig_qty   TYPE menge_d,      " discounted qty of this grade (capping-split)
         meins      TYPE meins,        " unit
         rebate_val TYPE ycis_apprvl-rebate_val,  " rebate amount of this grade
         waers      TYPE waers,        " currency
*        workflow status (visible for every record - pending / approved /
*        returned / completed), plus the last rejection detail
         scheme_type TYPE ycis_apprvl-scheme_type,
         status_txt  TYPE char32,
         rej_by      TYPE ycis_apprvl-rej_by,
         rej_remarks TYPE ycis_apprvl-rej_remarks,
*        approval trail - who approved at each level and when
         l1_user    TYPE ycis_apprvl-l1_user,
         l1_date    TYPE ycis_apprvl-l1_date,
         l1_time    TYPE ycis_apprvl-l1_time,
         l2_user    TYPE ycis_apprvl-l2_user,
         l2_date    TYPE ycis_apprvl-l2_date,
         l2_time    TYPE ycis_apprvl-l2_time,
         l3_user    TYPE ycis_apprvl-l3_user,
         l3_date    TYPE ycis_apprvl-l3_date,
         l3_time    TYPE ycis_apprvl-l3_time,
         l4_user    TYPE ycis_apprvl-l4_user,
         l4_date    TYPE ycis_apprvl-l4_date,
         l4_time    TYPE ycis_apprvl-l4_time,
         l5_user    TYPE ycis_apprvl-l5_user,
         l5_date    TYPE ycis_apprvl-l5_date,
         l5_time    TYPE ycis_apprvl-l5_time,
         l6_user    TYPE ycis_apprvl-l6_user,
         l6_date    TYPE ycis_apprvl-l6_date,
         l6_time    TYPE ycis_apprvl-l6_time,
       END OF ty_out.

TYPES: BEGIN OF ty_grade,
         kondm TYPE kondm,
         qty   TYPE menge_d,
       END OF ty_grade.

DATA: gt_out    TYPE STANDARD TABLE OF ty_out,
      gs_out    TYPE ty_out,
      gt_fcat   TYPE slis_t_fieldcat_alv,
      gs_fcat   TYPE slis_fieldcat_alv,
      gs_layout TYPE slis_layout_alv,
      gr_nodisc TYPE RANGE OF kondm,
      gs_nodisc LIKE LINE OF gr_nodisc.

*--------------------------------------------------------------------*
* Selection screen
*--------------------------------------------------------------------*
SELECTION-SCREEN BEGIN OF BLOCK b1 WITH FRAME TITLE text-001.
SELECT-OPTIONS: s_sptag FOR ycis_apprvl-period_from,   " CIS period
                s_vkbur FOR ycis_apprvl-sales_off,     " sales office
                s_kunnr FOR ycis_apprvl-kunnr,         " customer
                s_vbeln FOR ycis_apprvl-order_no,      " rebate order
                s_stat  FOR ycis_apprvl-wf_status.     " workflow status (blank = all)
SELECTION-SCREEN END OF BLOCK b1.

*--------------------------------------------------------------------*
START-OF-SELECTION.
  PERFORM load_nodisc.
  PERFORM get_data.
  IF gt_out IS INITIAL.
    MESSAGE 'No records found for the selection' TYPE 'I'.
    RETURN.
  ENDIF.
  PERFORM build_fieldcat.
  PERFORM display_alv.

*&---------------------------------------------------------------------*
*&      Form  load_nodisc   (non-discount grades - no rebate value)
*&---------------------------------------------------------------------*
FORM load_nodisc.
  DATA: lt_nod TYPE STANDARD TABLE OF ycis_nodisc_grd,
        ls_nod TYPE ycis_nodisc_grd.
  REFRESH gr_nodisc.
  SELECT * FROM ycis_nodisc_grd INTO TABLE lt_nod.
  LOOP AT lt_nod INTO ls_nod.
    gs_nodisc-sign = 'I'. gs_nodisc-option = 'EQ'.
    gs_nodisc-low  = ls_nod-kondm.
    APPEND gs_nodisc TO gr_nodisc.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  get_data
*&---------------------------------------------------------------------*
FORM get_data.
  DATA: lt_appr TYPE STANDARD TABLE OF ycis_apprvl,
        ls_appr TYPE ycis_apprvl.

*   Show EVERY record with its status (Pending L2 / Pending L3 / Completed /
*   Returned) - the completed-only filter (order_no <> space) was removed so
*   in-flight and rejected records are visible in the report. GAIL 31.07.2026.
  SELECT * FROM ycis_apprvl INTO TABLE lt_appr
    WHERE order_no    IN s_vbeln
      AND sales_off   IN s_vkbur
      AND kunnr       IN s_kunnr
      AND period_from IN s_sptag
      AND wf_status   IN s_stat.

  LOOP AT lt_appr INTO ls_appr.
    PERFORM emit_order USING ls_appr.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  emit_order   (expand one rebate order into grade rows)
*&---------------------------------------------------------------------*
FORM emit_order USING p_appr TYPE ycis_apprvl.
  DATA: lv_name  TYPE name1_gp,
        lv_erdat TYPE erdat,
        lv_auart TYPE auart,
        lv_bstkd TYPE bstkd,
        lt_grade TYPE STANDARD TABLE OF ty_grade,
        ls_grade TYPE ty_grade,
        lv_totl  TYPE menge_d,      " total lifted (all grades)
        lv_totd  TYPE menge_d,      " total discountable lifted
        lv_qty   TYPE menge_d,
        lv_val   TYPE ycis_apprvl-rebate_val.

* customer name
  SELECT SINGLE name1 FROM kna1 INTO lv_name WHERE kunnr = p_appr-kunnr.
* order header attributes (created-on / doc type)
  SELECT SINGLE erdat auart INTO (lv_erdat, lv_auart)
    FROM vbak WHERE vbeln = p_appr-order_no.
* reference no - prefer the one stored on the order (VBKD-BSTKD), else the
* value staged in YCIS_APPRVL (PURCH_NO)
  SELECT SINGLE bstkd FROM vbkd INTO lv_bstkd
    WHERE vbeln = p_appr-order_no AND posnr = '000000'.
  IF lv_bstkd IS INITIAL.
    lv_bstkd = p_appr-purch_no.
  ENDIF.

* ---- 1) grade detail captured at source (YCIS_APPRVL_GRD) ------------
  DATA: lt_cap TYPE STANDARD TABLE OF ycis_apprvl_grd,
        ls_cap TYPE ycis_apprvl_grd.
*   Key the grade detail by the business key (not by order_no) so it is found
*   for in-flight records too, whose order has not been created yet.
  SELECT * FROM ycis_apprvl_grd INTO TABLE lt_cap
    WHERE qais_no     = p_appr-qais_no
      AND scheme_type = p_appr-scheme_type
      AND period_from = p_appr-period_from
      AND period_to   = p_appr-period_to
      AND kunnr       = p_appr-kunnr
      AND kvgr2       = p_appr-kvgr2.
  IF lt_cap IS NOT INITIAL.
    LOOP AT lt_cap INTO ls_cap.
      CLEAR gs_out.
      gs_out-vbeln     = p_appr-order_no.
      gs_out-auart     = lv_auart.
      gs_out-erdat     = lv_erdat.
      gs_out-bstkd     = lv_bstkd.
      gs_out-kunnr     = p_appr-kunnr.
      gs_out-name1     = lv_name.
      gs_out-sales_off = p_appr-sales_off.
      gs_out-kondm     = ls_cap-kondm.
      PERFORM grade_name USING ls_cap-kondm CHANGING gs_out-kondm_txt.
      gs_out-lft_qty    = ls_cap-lft_qty.
      gs_out-elig_qty   = ls_cap-elig_qty.
      gs_out-rebate_val = ls_cap-rebate_val.
      gs_out-waers      = ls_cap-waers.
      PERFORM fill_appr_trail USING p_appr.
      APPEND gs_out TO gt_out.
    ENDLOOP.
    RETURN.
  ENDIF.

* ---- 2) else reconstruct from S922 lifting (customer + CIS period) ----
  SELECT kondm ummenge FROM s922 INTO (ls_grade-kondm, ls_grade-qty)
    WHERE sptag  BETWEEN p_appr-period_from AND p_appr-period_to
      AND pkunag = p_appr-kunnr.
    COLLECT ls_grade INTO lt_grade.
  ENDSELECT.

* totals for the proportional split
  CLEAR: lv_totl, lv_totd.
  LOOP AT lt_grade INTO ls_grade.
    lv_totl = lv_totl + ls_grade-qty.
    IF ls_grade-kondm NOT IN gr_nodisc.
      lv_totd = lv_totd + ls_grade-qty.
    ENDIF.
  ENDLOOP.

* no lifting found -> single summary line from the header totals
  IF lt_grade IS INITIAL OR lv_totl IS INITIAL.
    CLEAR gs_out.
    gs_out-vbeln      = p_appr-order_no.
    gs_out-auart      = lv_auart.
    gs_out-erdat      = lv_erdat.
    gs_out-bstkd      = lv_bstkd.
    gs_out-kunnr      = p_appr-kunnr.
    gs_out-name1      = lv_name.
    gs_out-sales_off  = p_appr-sales_off.
    gs_out-elig_qty   = p_appr-elig_qty.
    gs_out-lft_qty    = p_appr-lft_qty.
    gs_out-rebate_val = p_appr-rebate_val.
    gs_out-waers      = p_appr-waers.
    PERFORM fill_appr_trail USING p_appr.
    APPEND gs_out TO gt_out.
    RETURN.
  ENDIF.

* one output line per grade - allocate qty & value proportionately
  LOOP AT lt_grade INTO ls_grade.
    CLEAR gs_out.
    gs_out-vbeln     = p_appr-order_no.
    gs_out-auart     = lv_auart.
    gs_out-erdat     = lv_erdat.
    gs_out-bstkd     = lv_bstkd.
    gs_out-kunnr     = p_appr-kunnr.
    gs_out-name1     = lv_name.
    gs_out-sales_off = p_appr-sales_off.
    gs_out-kondm     = ls_grade-kondm.
    PERFORM grade_name USING ls_grade-kondm CHANGING gs_out-kondm_txt.
    gs_out-lft_qty   = ls_grade-qty.
*   discounted (eligible) qty for this grade = header eligible qty x share
    lv_qty = p_appr-elig_qty * ls_grade-qty / lv_totl.
    gs_out-elig_qty  = lv_qty.
*   rebate value only on discountable grades, split by discountable share
    IF ls_grade-kondm IN gr_nodisc OR lv_totd IS INITIAL.
      CLEAR gs_out-rebate_val.
    ELSE.
      lv_val = p_appr-rebate_val * ls_grade-qty / lv_totd.
      gs_out-rebate_val = lv_val.
    ENDIF.
    gs_out-waers     = p_appr-waers.
    PERFORM fill_appr_trail USING p_appr.
    APPEND gs_out TO gt_out.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  fill_appr_trail   (approval trail: who/when at L1/L2/L3)
*&---------------------------------------------------------------------*
FORM fill_appr_trail USING p_appr TYPE ycis_apprvl.
*   workflow status (shown for every record) + last rejection detail
  PERFORM status_text USING p_appr CHANGING gs_out-status_txt.
  gs_out-scheme_type = p_appr-scheme_type.
  gs_out-rej_by      = p_appr-rej_by.
  gs_out-rej_remarks = p_appr-rej_remarks.
  gs_out-l1_user = p_appr-l1_user.
  gs_out-l1_date = p_appr-l1_date.
  gs_out-l1_time = p_appr-l1_time.
  gs_out-l2_user = p_appr-l2_user.
  gs_out-l2_date = p_appr-l2_date.
  gs_out-l2_time = p_appr-l2_time.
  gs_out-l3_user = p_appr-l3_user.
  gs_out-l3_date = p_appr-l3_date.
  gs_out-l3_time = p_appr-l3_time.
  gs_out-l4_user = p_appr-l4_user.
  gs_out-l4_date = p_appr-l4_date.
  gs_out-l4_time = p_appr-l4_time.
  gs_out-l5_user = p_appr-l5_user.
  gs_out-l5_date = p_appr-l5_date.
  gs_out-l5_time = p_appr-l5_time.
  gs_out-l6_user = p_appr-l6_user.
  gs_out-l6_date = p_appr-l6_date.
  gs_out-l6_time = p_appr-l6_time.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  status_text   (readable workflow status for the report)
*&---------------------------------------------------------------------*
FORM status_text USING p_appr TYPE ycis_apprvl
              CHANGING p_txt TYPE any.
  CASE p_appr-wf_status.
    WHEN '40'. p_txt = 'Completed - Order Created'.
    WHEN '30'. p_txt = 'Pending L3 (Approved by L2)'.
    WHEN '20'.
      IF p_appr-rej_level = '3'.
        p_txt = 'Returned by L3 - Pending L2'.
      ELSE.
        p_txt = 'Pending L2 (Submitted by L1)'.
      ENDIF.
    WHEN '10'. p_txt = 'Returned by L2 - Pending L1'.
    WHEN OTHERS. p_txt = p_appr-wf_status.
  ENDCASE.
ENDFORM.
*&---------------------------------------------------------------------*
*&      Form  grade_name   (material name for a KONDM / grade)
*&   GAIL 31.07.2026: show the MATERIAL NAME only (YY_MATNR) from
*&   YRVA_GRADE_CISD (YY_GRADE = KONDM) - NOT the material description.
*&   Only if the grade is not maintained in YRVA_GRADE_CISD do we fall back
*&   to the price-group (grade) text (T178T).
*&---------------------------------------------------------------------*
FORM grade_name USING p_kondm TYPE kondm CHANGING p_txt TYPE any.
  DATA: lv_matnr TYPE matnr.
  CLEAR p_txt.
  IF p_kondm IS INITIAL.
    RETURN.
  ENDIF.
* material name mapped to this grade in YRVA_GRADE_CISD (YY_GRADE -> YY_MATNR)
  CLEAR lv_matnr.
  SELECT yy_matnr UP TO 1 ROWS INTO lv_matnr
    FROM yrva_grade_cisd WHERE yy_grade = p_kondm.
  ENDSELECT.
  IF lv_matnr IS NOT INITIAL.
    p_txt = lv_matnr.
    RETURN.
  ENDIF.
* fallback - price-group (grade) text
  SELECT SINGLE vtext FROM t178t INTO p_txt
    WHERE spras = sy-langu AND kondm = p_kondm.
ENDFORM.

*&---------------------------------------------------------------------*
FORM build_fieldcat.
*   Every column carries its DDIC reference (REF_TABNAME / REF_FIELDNAME) so
*   the ALV grid interactive Filter and Sort work. Without a type reference
*   the classic ALV cannot build the filter, which is why the funnel did
*   nothing. Amount / quantity / currency columns are left without a QUAN/CURR
*   reference on purpose (they carry no unit field here) - they still filter
*   as numeric values. STATUS_TXT is computed and gets an explicit CHAR type.
*   GAIL 06.08.2026.
  DEFINE add_fc.
    CLEAR gs_fcat.
    gs_fcat-fieldname     = &1.
    gs_fcat-seltext_l     = &2.
    gs_fcat-seltext_m     = &2.
    gs_fcat-seltext_s     = &2.
    gs_fcat-ref_tabname   = &3.
    gs_fcat-ref_fieldname = &4.
    APPEND gs_fcat TO gt_fcat.
  END-OF-DEFINITION.

* tick-box column to choose the line whose Approval Note is printed
  CLEAR gs_fcat.
  gs_fcat-fieldname = 'SEL'.
  gs_fcat-seltext_l = 'Print'.
  gs_fcat-seltext_m = 'Print'.
  gs_fcat-seltext_s = 'Print'.
  gs_fcat-checkbox  = 'X'.
  gs_fcat-edit      = 'X'.
  gs_fcat-outputlen = 5.
  APPEND gs_fcat TO gt_fcat.

  add_fc 'VBELN'       'Rebate Order'          'YCIS_APPRVL'     'ORDER_NO'.
  add_fc 'AUART'       'Doc Type'              'VBAK'            'AUART'.
  add_fc 'ERDAT'       'Created On'            'VBAK'            'ERDAT'.
  add_fc 'BSTKD'       'Reference No'          'VBKD'            'BSTKD'.
  add_fc 'KUNNR'       'Customer'              'YCIS_APPRVL'     'KUNNR'.
  add_fc 'NAME1'       'Customer Name'         'KNA1'            'NAME1'.
  add_fc 'SALES_OFF'   'Sales Office'          'YCIS_APPRVL'     'SALES_OFF'.
  add_fc 'SCHEME_TYPE' 'Scheme'                'YCIS_APPRVL'     'SCHEME_TYPE'.
*   STATUS_TXT is computed (no DDIC element of its own). The ALV grid Filter
*   needs a real DDIC reference - an explicit inttype alone is not enough -
*   so reference a plain CHAR field (CUST_NAME, char 35) purely to give the
*   column a filterable/sortable character type; the header stays 'Status'
*   via the seltext above. GAIL 06.08.2026.
  add_fc 'STATUS_TXT'  'Status'                'YCIS_APPRVL'     'CUST_NAME'.
  add_fc 'KONDM'       'MPG'                   'YCIS_APPRVL_GRD' 'KONDM'.
  add_fc 'KONDM_TXT'   'Material / Grade Name' 'MAKT'            'MAKTX'.
  add_fc 'LFT_QTY'     'Lifted Qty'            ''                ''.
  add_fc 'ELIG_QTY'    'Discount Qty'          ''                ''.
  add_fc 'REBATE_VAL'  'Rebate Amount'         ''                ''.
  add_fc 'WAERS'       'Currency'              'YCIS_APPRVL'     'WAERS'.
  add_fc 'L1_USER'     'L1 Approved By'        'YCIS_APPRVL'     'L1_USER'.
  add_fc 'L1_DATE'     'L1 Date'               'YCIS_APPRVL'     'L1_DATE'.
  add_fc 'L1_TIME'     'L1 Time'               'YCIS_APPRVL'     'L1_TIME'.
  add_fc 'L2_USER'     'L2 Approved By'        'YCIS_APPRVL'     'L2_USER'.
  add_fc 'L2_DATE'     'L2 Date'               'YCIS_APPRVL'     'L2_DATE'.
  add_fc 'L2_TIME'     'L2 Time'               'YCIS_APPRVL'     'L2_TIME'.
  add_fc 'L3_USER'     'L3 Executed By'        'YCIS_APPRVL'     'L3_USER'.
  add_fc 'L3_DATE'     'L3 Date'               'YCIS_APPRVL'     'L3_DATE'.
  add_fc 'L3_TIME'     'L3 Time'               'YCIS_APPRVL'     'L3_TIME'.
  add_fc 'L4_USER'     'L4 Vetted By'          'YCIS_APPRVL'     'L4_USER'.
  add_fc 'L4_DATE'     'L4 Date'               'YCIS_APPRVL'     'L4_DATE'.
  add_fc 'L4_TIME'     'L4 Time'               'YCIS_APPRVL'     'L4_TIME'.
  add_fc 'L5_USER'     'L5 Approved By'        'YCIS_APPRVL'     'L5_USER'.
  add_fc 'L5_DATE'     'L5 Date'               'YCIS_APPRVL'     'L5_DATE'.
  add_fc 'L5_TIME'     'L5 Time'               'YCIS_APPRVL'     'L5_TIME'.
  add_fc 'L6_USER'     'L6 Disbursed By'       'YCIS_APPRVL'     'L6_USER'.
  add_fc 'L6_DATE'     'L6 Date'               'YCIS_APPRVL'     'L6_DATE'.
  add_fc 'L6_TIME'     'L6 Time'               'YCIS_APPRVL'     'L6_TIME'.
  add_fc 'REJ_BY'      'Rejected By'           'YCIS_APPRVL'     'REJ_BY'.
  add_fc 'REJ_REMARKS' 'Reject Remark'         'YCIS_APPRVL'     'REJ_REMARKS'.

*   STATUS_TXT borrows CUST_NAME only for its (filterable) CHAR type, so the
*   grid would otherwise show that field's label 'Name 1'. Force the header
*   back to 'Status': DDICTXT = 'L' tells ALV to use SELTEXT_L over the DDIC
*   text. GAIL 06.08.2026.
  READ TABLE gt_fcat INTO gs_fcat WITH KEY fieldname = 'STATUS_TXT'.
  IF sy-subrc = 0.
    gs_fcat-ddictxt      = 'L'.
    gs_fcat-reptext_ddic = 'Status'.
    gs_fcat-seltext_l    = 'Status'.
    gs_fcat-seltext_m    = 'Status'.
    gs_fcat-seltext_s    = 'Status'.
    MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
  ENDIF.

*   The USER columns all reference data element UNAME, whose DDIC label is
*   'User Name' - which the grid would otherwise show for every one of them.
*   Force the column-specific header (DDICTXT = 'L' = use SELTEXT_L). GAIL.
  LOOP AT gt_fcat INTO gs_fcat.
    CASE gs_fcat-fieldname.
      WHEN 'L1_USER' OR 'L2_USER' OR 'L3_USER' OR 'L4_USER'
        OR 'L5_USER' OR 'L6_USER' OR 'REJ_BY'.
        gs_fcat-ddictxt      = 'L'.
        gs_fcat-reptext_ddic = gs_fcat-seltext_l.
        MODIFY gt_fcat FROM gs_fcat INDEX sy-tabix.
    ENDCASE.
  ENDLOOP.
ENDFORM.

*&---------------------------------------------------------------------*
FORM display_alv.
  gs_layout-colwidth_optimize = 'X'.
  gs_layout-zebra             = 'X'.
  gs_layout-box_fieldname     = 'SEL'.   " row-select tick box
  CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
    EXPORTING
      i_callback_program       = sy-repid
      i_callback_pf_status_set = 'SET_STATUS'
      i_callback_user_command  = 'USER_COMMAND'
      is_layout                = gs_layout
      it_fieldcat              = gt_fcat
    TABLES
      t_outtab                 = gt_out
    EXCEPTIONS
      program_error            = 1
      OTHERS                   = 2.
  IF sy-subrc <> 0.
    MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
            WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
  ENDIF.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  set_status
*&---------------------------------------------------------------------*
*&  GUI status 'STANDARD' must exist in this program - create it in SE41
*&  by COPYING status 'STANDARD' from program SAPLKKBL (the ALV standard
*&  toolbar) and adding one push button with function code 'PRNT'
*&  (suggested key F5, text "Print Approval Note", icon ICON_PRINT).
*&---------------------------------------------------------------------*
FORM set_status USING rt_extab TYPE slis_t_extab.            "#EC CALLED
  SET PF-STATUS 'STANDARD' EXCLUDING rt_extab.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  user_command
*&---------------------------------------------------------------------*
FORM user_command USING r_ucomm     LIKE sy-ucomm            "#EC CALLED
                        rs_selfield TYPE slis_selfield.
  DATA: lr_grid  TYPE REF TO cl_gui_alv_grid,
        lt_rows  TYPE lvc_t_row,
        ls_row   TYPE lvc_s_row,
        lv_vbeln TYPE vbeln_va.

  CASE r_ucomm.
    WHEN 'PRNT'.
*       commit the tick made in the grid so gt_out-SEL is up to date
      CALL FUNCTION 'GET_GLOBALS_FROM_SLVC_FULLSCR'
        IMPORTING
          e_grid = lr_grid.
      IF lr_grid IS NOT INITIAL.
        CALL METHOD lr_grid->check_changed_data.
      ENDIF.
*       target row: the ticked Print box; else a highlighted row; else cursor
      CLEAR gs_out.
      READ TABLE gt_out INTO gs_out WITH KEY sel = 'X'.
      IF sy-subrc <> 0 AND lr_grid IS NOT INITIAL.
        CALL METHOD lr_grid->get_selected_rows
          IMPORTING
            et_index_rows = lt_rows.
        READ TABLE lt_rows INTO ls_row INDEX 1.
        IF sy-subrc = 0.
          READ TABLE gt_out INTO gs_out INDEX ls_row-index.
        ELSEIF rs_selfield-tabindex > 0.
          READ TABLE gt_out INTO gs_out INDEX rs_selfield-tabindex.
        ENDIF.
      ENDIF.
      IF gs_out IS INITIAL.
        MESSAGE 'Tick the Print box on the line you want, then press Print Approval Note' TYPE 'I'.
        RETURN.
      ENDIF.
      lv_vbeln = gs_out-vbeln.
      IF lv_vbeln IS INITIAL.
        MESSAGE 'Approval Note is available once the rebate order is created (L3)' TYPE 'I'.
        RETURN.
      ENDIF.
      PERFORM print_note USING lv_vbeln.
*       clear the tick(s) after printing
      LOOP AT gt_out INTO gs_out WHERE sel = 'X'.
        CLEAR gs_out-sel.
        MODIFY gt_out FROM gs_out.
      ENDLOOP.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  print_note   (render the CIS PSD Discount Approval Note)
*&---------------------------------------------------------------------*
FORM print_note USING p_vbeln TYPE vbeln_va.
  DATA: lt_appr   TYPE STANDARD TABLE OF ycis_apprvl,
        ls_appr   TYPE ycis_apprvl,
        ls_note   TYPE zcis_appr_note_s,
        lv_total  TYPE ycis_apprvl-rebate_val,
        lv_fm     TYPE funcname,
        ls_outpar TYPE sfpoutputparams,
        ls_docpar TYPE sfpdocparams.

*   all approval rows that produced this rebate order (usually one)
  SELECT * FROM ycis_apprvl INTO TABLE lt_appr
    WHERE order_no = p_vbeln.
  IF lt_appr IS INITIAL.
    MESSAGE 'No approval record found for this rebate order' TYPE 'I'.
    RETURN.
  ENDIF.
  READ TABLE lt_appr INTO ls_appr INDEX 1.

*   total discount value across the order's approval rows
  CLEAR lv_total.
  LOOP AT lt_appr INTO ls_appr.
    lv_total = lv_total + ls_appr-rebate_val.
  ENDLOOP.
  READ TABLE lt_appr INTO ls_appr INDEX 1.

*   ---- fill the note structure from the approval record ----
  CLEAR ls_note.
  ls_note-appr_note_no     = ls_appr-qais_no.
  ls_note-ref_no           = 'GAIL/PMG/CPC/PSD Discount disbursement'.
  WRITE sy-datum TO ls_note-doc_date DD/MM/YYYY.
  PERFORM scheme_name USING ls_appr CHANGING ls_note-variant_name.
  WRITE lv_total TO ls_note-disc_total_value CURRENCY ls_appr-waers.
  CONDENSE ls_note-disc_total_value.

*   per-level signature lines: user + date + time (available for all levels)
  PERFORM sign_line USING ls_appr-l1_user ls_appr-l1_date ls_appr-l1_time
                    CHANGING ls_note-l1_sign.
  PERFORM sign_line USING ls_appr-l2_user ls_appr-l2_date ls_appr-l2_time
                    CHANGING ls_note-l2_sign.
  PERFORM sign_line USING ls_appr-l3_user ls_appr-l3_date ls_appr-l3_time
                    CHANGING ls_note-l3_sign.
  PERFORM sign_line USING ls_appr-l4_user ls_appr-l4_date ls_appr-l4_time
                    CHANGING ls_note-l4_sign.
  PERFORM sign_line USING ls_appr-l5_user ls_appr-l5_date ls_appr-l5_time
                    CHANGING ls_note-l5_sign.
  PERFORM sign_line USING ls_appr-l6_user ls_appr-l6_date ls_appr-l6_time
                    CHANGING ls_note-l6_sign.

*   per-level remarks (L1-L3 share the general REMARKS; L4-L6 have their own)
  ls_note-l3_remarks = ls_appr-remarks.
  ls_note-l4_remarks = ls_appr-rem_l4.
  ls_note-l5_remarks = ls_appr-rem_l5.
  ls_note-l6_remarks = ls_appr-rem_l6.

*   ---- render the Adobe form ----
  CALL FUNCTION 'FP_FUNCTION_MODULE_NAME'
    EXPORTING
      i_name     = 'ZCIS_PSD_APPR_NOTE'
    IMPORTING
      e_funcname = lv_fm
    EXCEPTIONS
      OTHERS     = 1.
  IF sy-subrc <> 0 OR lv_fm IS INITIAL.
    MESSAGE 'Approval Note form ZCIS_PSD_APPR_NOTE is not active' TYPE 'I'.
    RETURN.
  ENDIF.

  ls_outpar-nodialog = space.        " show the print/preview dialog
  ls_outpar-preview  = 'X'.
  CALL FUNCTION 'FP_JOB_OPEN'
    CHANGING
      ie_outputparams = ls_outpar
    EXCEPTIONS
      cancel          = 1
      usage_error     = 2
      system_error    = 3
      internal_error  = 4
      OTHERS          = 5.
  IF sy-subrc <> 0.
    RETURN.
  ENDIF.

  ls_docpar-langu   = sy-langu.
  ls_docpar-country = 'IN'.
  CALL FUNCTION lv_fm
    EXPORTING
      /1bcdwb/docparams = ls_docpar
      wa_note           = ls_note
    EXCEPTIONS
      usage_error       = 1
      system_error      = 2
      internal_error    = 3
      OTHERS            = 4.
  IF sy-subrc <> 0.
    MESSAGE 'Error while generating the Approval Note' TYPE 'I'.
  ENDIF.

  CALL FUNCTION 'FP_JOB_CLOSE'
    EXCEPTIONS
      usage_error    = 1
      system_error   = 2
      internal_error = 3
      OTHERS         = 4.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  scheme_name   (Subject / Variant = scheme text + period)
*&---------------------------------------------------------------------*
FORM scheme_name USING    ps_appr TYPE ycis_apprvl
                 CHANGING p_out   TYPE any.
  DATA: lv_txt TYPE char25, lv_f TYPE char10, lv_t TYPE char10.
  CASE ps_appr-scheme_type.
    WHEN 'M'. lv_txt = 'Monthly CIS'.
    WHEN 'Q'. lv_txt = 'Quarterly CIS'.
    WHEN 'A'. lv_txt = 'Annual CIS'.
    WHEN 'C'. lv_txt = 'Annual Consistency CIS'.
    WHEN 'U'. lv_txt = 'Upliftment Rebate (PSD)'.
    WHEN OTHERS. lv_txt = ps_appr-scheme_type.
  ENDCASE.
  WRITE ps_appr-period_from TO lv_f DD/MM/YYYY.
  WRITE ps_appr-period_to   TO lv_t DD/MM/YYYY.
  CONCATENATE lv_txt '(' lv_f 'to' lv_t ')' INTO p_out SEPARATED BY space.
  CONDENSE p_out.
ENDFORM.

*&---------------------------------------------------------------------*
*&      Form  sign_line   (build "USER  DD.MM.YYYY  HH:MM:SS")
*&---------------------------------------------------------------------*
FORM sign_line USING p_user TYPE any
                     p_date TYPE datum
                     p_time TYPE uzeit
               CHANGING p_out TYPE any.
  DATA: lv_d TYPE char10, lv_t TYPE char8.
  CLEAR p_out.
  CHECK p_user IS NOT INITIAL.
  IF p_date IS NOT INITIAL.
    WRITE p_date TO lv_d DD/MM/YYYY.
  ENDIF.
  IF p_time IS NOT INITIAL.
    WRITE p_time TO lv_t USING EDIT MASK '__:__:__'.
  ENDIF.
  CONCATENATE p_user lv_d lv_t INTO p_out SEPARATED BY space.
  CONDENSE p_out.
ENDFORM.
