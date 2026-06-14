# SAP S/4HANA ATC Remediation — Reference Guide
## Project: ONGC CST / OTC Programs | Author: EJX9007359 | Date: 2026

---

## 1. CHANGE MARKER FORMAT (use exactly)

```abap
*****Code Remediation changes S4 2025_1 Conversion **begin of change by EJX9007359 2026.05.22  for ATC
*  <old code commented out>
   <new code>
*****Code Remediation changes S4 2025_1 Conversion **End of change by EJX9007359 2026.05.22  for ATC
```
> For June 2026 work, date used: `2026.06.13`
> Always comment the OLD lines above the new ones — never delete them.

---

## 2. PSEUDO COMMENTS (ATC SUPPRESSIONS)

| ATC Check | Pseudo Comment | Note Number? |
|---|---|---|
| S/4HANA: Field Length Extensions | `"#EC CI_FLDEXT_OK[2438131]` | Yes — `[2438131]` |
| Search problematic statements (DELETE INDEX / EXIT in SELECT) | `"#EC CI_NOORDER` | No — bare |
| Direct table access (CDS not available / BW extractor) | `"#EC CI_USAGE_OK[<finding_id>]` | Yes — from ATC detail |

**Rule:** `CI_FLDEXT_OK` and `CI_USAGE_OK` carry bracketed note/item numbers. `CI_NOORDER` is always bare.
**Important:** "Exemptions disabled by system setup" in ATC does NOT affect `"#EC` pseudo comments — they work independently.

---

## 3. KEY REFERENCE FILES (in GitHub repo)

| File | Purpose |
|---|---|
| `ARS_API_SUCCESSOR.xlsx` | Official SAP table → CDS view mapping (columns: Object Type, Object Name, ..., Successor Object Name) |
| `DDLS_BASE_FIELDS.txt` | Maps CDS element names ↔ base table field names. Format: `CDS_VIEW|CDS_VIEW|CDS_ELEMENT|BASE_TABLE|BASE_FIELD|` |

**How to use together:** Find table in ARS xlsx → get CDS view name → look up CDS view in DDLS_BASE_FIELDS.txt → confirm each field exists before converting.

---

## 4. CONVERSION RULE — WHEN TO CONVERT vs SUPPRESS

**Convert SELECT to CDS** only when ALL of these are true:
1. Table has an official entry in ARS_API_SUCCESSOR.xlsx
2. ALL selected fields (SELECT list) exist in the CDS view (check DDLS_BASE_FIELDS.txt)
3. ALL WHERE-clause fields exist in the CDS view
4. The internal table TYPE can accommodate the CDS field names (or aliases restore them)

**Suppress with `"#EC CI_USAGE_OK`** when:
- Table has no ARS successor (customizing tables: T438A, TBP1C, TBP1T, TWICOSTORET, TFPROFT_CEP, etc.)
- BW extractor needing SELECT * across all 40+ fields (e.g. VBFA extractor — CDS only has 26/43 fields)
- Cross-client reads that CDS cannot reproduce (T048 pattern)
- Already has `"#EC CI_USAGE_OK[nnn]` in code — leave it

---

## 5. CONFIRMED TABLE → CDS MAPPINGS (from ARS xlsx)

| Legacy Table | CDS Successor | Notes |
|---|---|---|
| EBAN | I_PurchaseRequisitionItemAPI01 | |
| VBAP | I_SalesDocumentItem | |
| VBEP | I_SalesDocumentScheduleLine | |
| VBUP | V_VBUP_S4 | Same field names as VBUP — no CamelCase aliases needed |
| VBUK | V_VBUK_S4 | Same field names as VBUK — no CamelCase aliases needed |
| VBAK | I_SalesDocument | |
| LIPS | I_DeliveryDocumentItem | |
| LIKP | I_DeliveryDocument | |
| MARA | I_Product | |
| LFA1 | I_Supplier | |
| KNA1 | I_Customer | |
| EKKO | I_PurchaseOrderAPI01 | |
| EKPO | I_PurchaseOrderItemAPI01 | |
| QALS | I_InspectionLot | MANDT field in QALS is `MANDANT` not `MANDT`; CDS does not expose MANDT |
| VBFA | I_SDDocumentMultiLevelProcFlow | Only 26/43 fields available in CDS |
| KBED | I_CapacityRequirementItem | |

**Tables with NO official ARS successor (suppress, do not convert):**
COSS, KBKO, BPBK, BPEG, BPEJ, TBP1C, TBP1T, MDKP, VTTK, TVTKT, TVTFT, T438A, TWICOSTORET, TFPROFT_CEP

---

## 6. MODERN ABAP OPEN SQL RULES

```abap
" Comma-separated field list (mandatory in S/4)
SELECT field1,
       field2,
       field3
  FROM cds_view
  INTO TABLE @lt_table
  WHERE field1 = @lv_var.

" Host variables: prefix with @
WHERE ebeln = @ls_ekko-ebeln

" No USING CLIENT — CDS handles client implicitly
" No CLIENT SPECIFIED — CDS handles client implicitly
" No MANDT in WHERE clause for CDS views
```

**Never use:**
- `SELECT * FROM <cds_view>` if internal table has fewer fields
- `USING CLIENT @mandt1` with CDS views
- `CLIENT SPECIFIED` with CDS views
- `MANDT EQ @MANDT1` in WHERE clause for CDS views (except where CDS explicitly exposes MANDT as data)

---

## 7. CDS FIELD ALIAS RULES

```abap
" Use AS <legacy_name> to preserve existing internal table structure
SELECT SalesDocument AS vbeln,
       SalesDocumentItem AS posnr,
       Material AS matnr
  FROM I_SalesDocumentItem
  INTO TABLE @lt_vbap.

" V_VBUK_S4 and V_VBUP_S4 = same field names as original tables
" NO aliases needed — just SELECT field names directly:
SELECT vbeln, gbstk FROM V_VBUK_S4 INTO TABLE @lt_vbuk WHERE ...
```

---

## 8. SUPPLEMENTARY FETCH-BACK PATTERN

When a CDS view is missing some fields that are needed in the internal table:

```abap
" Step 1: Main SELECT from CDS (gets most fields)
SELECT SalesDocument AS vbeln, ...
  FROM I_SalesDocumentItem
  INTO TABLE @lt_vbap
  WHERE ...

" Step 2: Fetch missing fields from original table
SELECT vbeln, posnr, /scl/atwrt, /lot/tempkz
  FROM lips
  FOR ALL ENTRIES IN @lt_vbap
  WHERE vbeln = @lt_vbap-vbeln
    AND posnr = @lt_vbap-posnr
  INTO TABLE @lt_lips_supp.

" Step 3: Fill back via FIELD-SYMBOL loop + BINARY SEARCH
SORT lt_lips_supp BY vbeln posnr.
LOOP AT lt_vbap ASSIGNING <lfs_vbap>.
  READ TABLE lt_lips_supp WITH KEY vbeln = <lfs_vbap>-vbeln
                                    posnr = <lfs_vbap>-posnr
                           BINARY SEARCH
                           TRANSPORTING NO FIELDS.
  IF sy-subrc = 0.
    <lfs_vbap>-/scl/atwrt = lt_lips_supp[ sy-tabix ]-/scl/atwrt.
  ENDIF.
ENDLOOP.
```

---

## 9. JOIN DUPLICATE ALIAS PATTERN

When joining two tables that both have a field with the same legacy name:

```abap
" KNA1 + KNB1 both have LOEVM and SPERR
SELECT kna1~kunnr,
       kna1~loevm,           " customer-level
       kna1~sperr,
       knb1~loevm AS loevm_b1,   " company-code-level — renamed to avoid dup
       knb1~sperr AS sperr_b1
  FROM I_Customer AS kna1
  INNER JOIN I_CustomerCompany AS knb1 ON ...
```
Same pattern for LFA1 + LFB1 (`loevm_b1` / `sperr_b1`).

---

## 10. DELETE INDEX / EXIT IN SELECT — NOORDER PATTERN

This is the most common finding in pricing programs (condition tables A551–A999).
**Do NOT rewrite** to SELECT INTO TABLE — these use `PACKAGE SIZE` and deliberately exit after first packet.

```abap
" Before (ATC finding on delete … index and exit.):
select * from a757 ... package size pack_size ...
  describe table t_header_a757 lines ilines.
  if ilines > max_line.
    message s016(es) with max_line.
    delete t_header_a757 index ilines.
  endif.
  exit.
endselect.

" After (suppress findings with pragma):
*****Code Remediation changes S4 2025_S Conversion **begin of change by EJX9007359 2026.06.13 ATC
*   delete t_header_a757 index ilines.
*   endif.
*   exit.
    describe table t_header_a757 lines ilines.
    if ilines > max_line.
      message s016(es) with max_line.
      delete t_header_a757 index ilines.           "#EC CI_NOORDER
    endif.
    exit.                                          "#EC CI_NOORDER
*****Code Remediation changes S4 2025_S Conversion **End of change by EJX9007359 2026.06.13 ATC
  endselect.
```

---

## 11. CLIENT SPECIFIED REMOVAL PATTERN (solve2 style)

```abap
" OLD (ECC):
SELECT * FROM <table>
  CLIENT SPECIFIED
  WHERE MANDT EQ MANDT1
    AND other_field = value.

" NEW (S/4 CDS — client is implicit):
SELECT * FROM <cds_view>
  WHERE other_field = @value.
" Remove: CLIENT SPECIFIED, MANDT condition, USING CLIENT @MANDT1
```

---

## 12. VBFA → CDS (I_SDDocumentMultiLevelProcFlow) FIELD MAP

| CDS Element | VBFA Field |
|---|---|
| PrecedingDocument | VBELV |
| PrecedingDocumentItem | POSNV |
| SubsequentDocument | VBELN |
| SubsequentDocumentItem | POSNN |
| SubsequentDocumentCategory | VBTYP_N |
| PrecedingDocumentCategory | VBTYP_V |
| QuantityInBaseUnit | RFMNG |
| BaseUnit | MEINS |
| NetAmount | RFWRT |
| StatisticsCurrency | WAERS |
| CreationDate | ERDAT |
| CreationTime | ERZET |
| LastChangeDate | AEDAT |
| MaterialDocumentYear | MJAHR |
| GoodsMovementType | BWART |
| ProcessFlowLevel | STUFE |
| BillingPlan | FPLNR |
| BillingPlanItem | FPLTR |
| OrderQuantityUnit | VRKME |
| SDFulfillmentCalculationRule | PLMIN |
| TransferOrderInWrhsMgmtIsConfd | TAQUI |
| WarehouseNumber | LGNUM |

> VBFA has 43 fields; CDS only has 26. Missing: FKTYP, SOBKZ, KZBEF, ABGES, etc. — for BW extractors, suppress with CI_USAGE_OK rather than partial CDS conversion.

---

## 13. FILES CONVERTED (branch: claude/exciting-darwin-rrmfvz)

| File | Program | What was done |
|---|---|---|
| `solve.txt` | `/CCEJ/RUOSD_ATP_STK_F01` | EBAN, VBAP+VBEP+VBUP, LIPS+LIKP, MARA, VBUK, VBUP → CDS with supplementary fetches |
| `solve1.txt` | `/CCEJ/RUOSD_ATP_STK_F01_NEW` | Same tables + F_GET_VBUKP form; V_VBUK_S4 / V_VBUP_S4 used |
| `solve2.txt` | `/CCC/RDTBCR_1BRBES2RIA_PGM1` | 86 tables with successors from ARS; 30 converted; CLIENT SPECIFIED removed; duplicate alias fix; QALS MANDANT fix; VBFA MANDT as @sy-mandt |
| `RDOTCSLSR_CCCIL_PRICNG_S5.txt` | `/CCC/RDOTCSLSR_CCCIL_PRICNG_S5` | 109×2 CI_NOORDER (DELETE/EXIT) + 154 CI_FLDEXT_OK[2438131]; no CDS conversions (no successors for pricing tables) |

