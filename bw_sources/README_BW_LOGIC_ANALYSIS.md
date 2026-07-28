# BW Source Extracts — Support Fee (GAP-1000002273)

Extracted from BW system DJB, schema CCEJ_VIRTUAL / package ZJ_AccountPL.SupportFee.
These are the reference sources for the S/4HANA rebuild (see TSD_CCBJI_SupportFee_S4HANA.md).

## Contents

* `PROC_*.sql` — all 8 HANA procedures (complete).
* `VIEW_ZJAPL_SU_FEE_FAGLFLEXA_from_char15001.xml` — chars 15,001–61,058 of the
  view XML (chunks 2–5). Chunk 1 (chars 1–15,000: header, data sources,
  nodes PN_FAGLFLEXA / Projection_3 / PN_MAP_GL / Aggregation_1 / Join_2 /
  Aggregation_2 / Join_1 / start of Projection_4) was delivered via chat and is
  summarized below.

## Key logic findings (validated against TSD)

### Source routing (PROC_SUPPORT_FEE_STATGING_CALCULATION — the master)
Staging table ZJ_SUPPORT_FEE_PER is truncated and filled per run:

| Flow | View | BLART set |
|---|---|---|
| FI | DATA_001 | NOT IN (BA, RV, RW) |
| FI | DATA_BA_001 | = BA |
| FI | DATA_F_001 | IN (RV, RW, RJ)  ← RJ added 19.05.2023 Defect 9000021581 |
| COPA | DATA_001 | NOT IN (RV, RW, BA) |
| COPA | DATA_BA_001 | = BA |
| COPA | DATA_F_001 | IN (RV, RW, RJ, AB) ← AB added 25.05.2023 GAP-1000003176 |

RJ and AB are NOT in FS v2.0 nor in the 2022 BW ABAP — S/4 build must include them.

### De-duplication (three mechanisms)
1. FI flow excludes EXPENSE_GL and AC_DOC_NR values found in DATA_F_001 with
   ZJ_FLAG='X' (staging tables ZJAPL_STAGING_EXPENSEGL / _ACDOCNR).
2. Amounts: SUM(DEB_CRE_LC) = FI cost vs SUM(CA_COPA_VF_SUM) = COPA cost per doc.
3. COPA insert keeps only ROW_NUM='1' per (AC_DOC_NR, RBELN, BELNR) partition
   (origin of `row_num EQ '1'` in the BW ABAP).

### FI↔COPA match key (VIEW ZJAPL_SU_FEE_FAGLFLEXA, Join_4 leftOuter)
AC_DOC_NR + GL_ACCOUNT + COMP_CODE + FISCYEAR (consistent with the FS Excel
matrix for the DATA_001 view).

### Formulas (from the view XML)
* CA_COPA_VF_SUM = VV518+VV520+VV572+VV529+VV573+VV575+VV576+VV577+VV578+VV583+VV584+VV608+VV622
* CA_Incidence_per = INCIDENCE_PER; sentinel **999** when FI-side keys incomplete
* CA_AMT_Support_Fee = DEB_CRE_LC × CA_Incidence_per / 100
* CA_AMT_COPA = CA_COPA_VF_SUM × CA_Incidence_per / 100
* Incidence join (Join_2): /BIC/AZJDMAPGL2 ⋈ /BIC/AZJDSUPFEE2 INNER ON
  REF_GL_ACCOUNT = RACCT AND SOURCE = SOURCE

### COPA staging write-back (PROC_ZJ_SUPPORTFEE_RES_UPDATE)
Delete-per-period+company (VRGAR='N') then UPSERT into ZJ_SUPP_COPA with
VV557/100, VV582/100, VV529/100 (×100 storage scaling removed at boundary).
Precedent for S/4 re-run handling: COPA side was replace-per-period.

## Still outstanding from BW (optional, decreasing priority)
1. CF_001 + CF_TYPE_F_001 view XMLs (4 chunks each)
2. DATA_F_001 / DATA_001 / DATA_BA_001 XMLs (13–14 chunks each, or HANA Studio
   Developer-Mode export of the package in one action)
Their consumption pattern and output columns are already fully known from the
procedures; the XMLs would only confirm internal node details.
