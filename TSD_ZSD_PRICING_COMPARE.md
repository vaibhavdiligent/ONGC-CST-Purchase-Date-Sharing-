# TSD — ZSD_PRICING_COMPARE

**CCBJI (Coca-Cola Bottlers Japan) — Pricing regression validation after ECC → S/4HANA migration**

| | |
|---|---|
| Program | `ZSD_PRICING_COMPARE` (executable report, SE38) |
| Module | SD — Pricing |
| System | S/4HANA (post ECC migration) |
| Author | Diligent Consulting |
| Version | 1.0 |

## 1. Business background

CCBJI migrated from ECC to S/4HANA. Open sales orders were converted with the
pricing result calculated by the **ECC** pricing engine (the KONV content was
moved to **PRCD_ELEMENTS** during conversion; KONV is obsolete/empty in S/4HANA).

To prove that the S/4HANA pricing configuration (pricing procedure
determination, condition records, access sequences, VOFM requirements/formulas)
reproduces the ECC pricing result, the user enters a **document type and a
creation date range** (mandatory), optionally a **customer range**, and **how
many orders to check** (default 1). The program picks the top-N sales orders
**X** with the **highest net value** (`VBAK-NETWR`) in that period — overall
when no customer is entered, or **per customer** when a customer range is
entered (e.g. 5 customers × top 5 orders = 25 copy orders). For each X it
creates a copy order **Y** on which S/4HANA re-derives the pricing from
scratch and compares the pricing conditions of X and Y line by line. Any
delta indicates a configuration or condition-record migration defect.

All details of X (org data, partners, items, quantities, pricing date,
conditions) are read from the database — `VBAK`, `VBAP`, `VBPA`, `VBKD` and
`PRCD_ELEMENTS` (equivalent to what `BAPISDORDER_GETDETAILEDLIST` would
return, but without the external-format conversions).

## 2. Processing

1. Order Y is created with `BAPI_SALESORDER_CREATEFROMDAT2`,
   `LOGIC_SWITCH-PRICING = 'B'` (carry out new pricing).
2. Y's header/org data, sold-to (AG), ship-to (WE), items (material, plant,
   quantity, sales unit) are copied from X — an exact replication. The PO
   number of Y is stamped **`PRCVAL-<X order number>`** for traceability.
3. After `BAPI_TRANSACTION_COMMIT` (wait), Y's freshly calculated conditions
   are read from **PRCD_ELEMENTS** via Y's `VBAK-KNUMV` — an
   internal-to-internal comparison against X's PRCD_ELEMENTS records.
4. **Y remains in the system** (no rejection step, per CCBJI decision). The
   `PRCVAL-*` PO number makes the test copies easy to find (VA05 /
   `VBAK-BSTNK`) and clean up manually once the validation cycle is finished.

> Note: creating Y consumes number ranges and may trigger outputs / credit
> checks / ATP depending on configuration, and the Y orders stay open until
> cleaned up. Run in the QA client that holds the migrated production data,
> not in production.

## 3. Pricing date

Y is always priced on **X's original pricing date** — read from `VBKD-PRSDT`
(header record, fallback first VBKD record / `VBAK-ERDAT`). This makes exactly
the same condition-record validity periods apply as when X was priced in ECC,
so the comparison is a true ECC-engine-vs-S/4-engine equivalence test of the
migrated configuration, unaffected by condition records changed since then.

## 4. Comparison logic

- X's conditions: `PRCD_ELEMENTS` via `VBAK-KNUMV` (alternative access in
  S/4HANA would be CDS view `V_KONV`).
- Match key: **item (KPOSN) + condition type (KSCHL) + occurrence** (n-th
  appearance of the type within the item, counted over the whole item in
  STUNR/ZAEHK order) — so condition types appearing twice are compared
  pairwise. Inactive (`KINAK`) and statistical (`KSTAT`) lines are excluded
  **before** occurrence numbering on both sides, so both sides number on the
  same basis.
- **Matching order** (prevents false MISSING/MISMATCH when the step sequence
  differs between the ECC and S/4 pricing procedures): for each X line the
  program takes ① an unused Y line of the same item + type with **identical
  values**, else ② the positional match (same occurrence), else ③ any unused
  line of that type (compared, delta shown). `MISSING_S4` is reported only
  when the condition type does not exist on Y at all for that item.
- **Two-pass processing:** normal condition lines claim their Y partners
  first; manual lines (`KHERK = 'C'` / `KMPRS`) are processed in a second
  pass and consume only leftover Y lines — so a manual line can neither steal
  a regular line's partner nor leave its own Y counterpart to be misreported
  as `NEW_IN_S4`. Manual rows display both the X value and the freshly
  determined Y value.
- Compared per condition line: rate **KBETR**, pricing unit **KPEIN**,
  condition unit **KMEIN**, condition value **KWERT**.
- **Stored value fields** compared between X and Y (customer requirement),
  shown as extra rows with the field name in the Cond.Type/Field column:
  - `VBAP` per item: `NETWR`, `NETPR`, `SKTOF`, `WAVWR`, `KZWI1`–`KZWI6`
    (pricing subtotals), `MWSBP` (tax)
  - `VBAK` header: `NETWR`
  - Amount fields are TCURX-converted before comparison; non-amount fields
    (e.g. `SKTOF`) are compared as raw values with X/Y shown in the remark.
- Zero tolerance — with JPY even ¥1 deltas are relevant.

### Amount normalisation (critical for JPY)

All amounts are converted to external format before comparison:

- **Currency decimal shift**: internal CURR fields always carry 2 decimals;
  TCURX defines the real decimals per currency. JPY has 0 decimals, so real
  amount = stored amount × 100 (`× 10^(2 − TCURX-CURRDEC)`).
- **Percentage conditions** (`KRECH = 'A'`): KBETR is stored with one implied
  extra decimal (KBETR 100.00 = 10.000 %) → divided by 10. A factor-10 delta
  on a percentage condition is flagged with a dedicated remark (cf. SAP KBA
  **2333377**).

### Classification

| Status | Meaning |
|---|---|
| `OK` | Values identical |
| `MISMATCH` | S/4 pricing deviates from the ECC result — config/record defect candidate |
| `MISSING_S4` | Condition on X not re-determined on Y → missing/wrong condition record or access sequence. **Red** only if X carried a rate/value ≠ 0; **yellow** when both are zero (no impact on the pricing outcome) |
| `NEW_IN_S4` | Condition determined on Y but absent on X. **Red** only if Y carries a rate/value ≠ 0; **yellow** when zero |
| `MANUAL` | Manually entered on X (`KHERK = 'C'` / `KMPRS = 'X'`) — cannot be re-derived by repricing; info only |
| `ERROR` | Y could not be created (BAPI messages shown in remark) |

Filtering rules: inactive lines (`KINAK ≠ space`) and statistical lines
(`KSTAT = 'X'`) are ignored; fully rejected items of X (`ABGRU ≠ space`) are
skipped. Header conditions (`KHERK = 'D'`) get an explanatory remark on
mismatch, since header distribution across items can legitimately differ.

## 5. Selection screen

Two modes via radio buttons:

**R1 — Automatic selection (default)**

| Field | Description |
|---|---|
| `S_AUART` (mandatory in R1) | Sales document type(s) |
| `S_ERDAT` (mandatory in R1) | Creation date range |
| `S_KUNNR` (optional) | Customer (sold-to) range — when filled, the top-N orders are determined **per customer** |
| `P_TOPN` (default 1) | How many orders to check: the N highest-value orders (`VBAK-NETWR` descending) overall, or per customer when `S_KUNNR` is filled |

Example: 5 customers in `S_KUNNR` and `P_TOPN` = 5 → up to 25 orders are
replicated and compared in one run.

**R2 — Specific sales orders**

| Field | Description |
|---|---|
| `S_VBELN` (mandatory in R2) | Sales order number(s) — every listed order is copied and compared, regardless of document type/date |

Mandatory fields are validated at runtime per mode (R1 needs document type +
date range; R2 needs at least one order number).

**Dynamic screen:** only the fields of the chosen mode are visible — selecting
"Give order No" hides the automatic-selection fields and shows only the Sales
Document range, and vice versa (radio buttons with `USER-COMMAND` +
`AT SELECTION-SCREEN OUTPUT` / `MODIF ID`). The mandatory fields of the active
mode carry the required-entry indicator (`SCREEN-REQUIRED = '2'`).

Everything else is fixed: create-order mode, X's original pricing date, zero
tolerance, statistical lines excluded, all comparison rows shown. The ALV
header shows how many orders were selected; a Customer column identifies the
sold-to party of each row. All BAPI input/output variables are cleared
explicitly at the start of each order so no values carry over between the
orders of one run.

## 6. Output — two screens

**Screen 1 — Order overview** (one row per order): customer, old/new order
numbers, item count, net value X / Y / delta, check counters (total, OK,
differences, warnings) and a color-coded verdict:

| Verdict | Meaning |
|---|---|
| `ALL OK` (green) | Every check passed — S/4 reproduces the ECC pricing (warnings, if any, are informational) |
| `CHECK` (red) | At least one real difference — double-click the row for detail |
| `ERROR` (red) | Copy order Y could not be created (BAPI messages in remark) |

The report header shows the overall RESULT line ("ALL OK" or "N differences")
so one glance answers "is pricing correct or not".

**Screen 2 — Pricing detail** (click the order number hotspot or double-click
the row): popup ALV designed for end users —

- Rows are grouped by a **Section** column and sorted in reading order:
  *Order total* (VBAK-NETWR) → *Item values* (NETWR, NETPR, subtotals, tax,
  cost…) → *Pricing conditions* per item.
- A **Description** column translates every row into plain language: condition
  type texts from T685T ("Output Tax") and fixed texts for value fields
  ("Pricing subtotal 3", "Cost (moving average price)").
- **Rate columns** carry only condition rates; **Amount columns** carry all
  money values (condition values and field values) — the two are never mixed.
- Technical columns (pricing unit, UoM) are hidden by default and can be
  added back via the ALV layout.
- The condition type is rendered as a **hotspot** — a single click (or
  double-click) opens VK13 (level 3). The popup header explains the colors
  and the click behaviour.

Layout save and Excel export available on both grids.

**Level 3 — VK13 jump** (double-click a condition row in the detail): the
program validates the row is a real pricing condition type (T685, usage A,
application V) and calls transaction **VK13** with the condition type pre-set
(parameter ID `VKS`, `WITH AUTHORITY-CHECK`). The record to inspect is the one
valid on the **pricing date shown in the row**. Double-clicking a value-field
row (NETPR, KZWI1, …) shows an explanatory message instead — there is no
condition record behind those.

## 7. Setup / transport notes

- Maintain text elements: `TEXT-001` (block title), selection texts, and the
  message/remark text symbols (`M02`, `R01…R06`).
- Created Y orders are **not rejected automatically**. Plan a periodic manual
  clean-up: find them via VA05 / `VBAK-BSTNK = PRCVAL-*` and reject or delete
  them so they do not distort open-order reporting or get delivered.
- Authorization: standard checks of the BAPIs apply (VA01 equivalent);
  run with a user authorized for order creation in the test client.
- Recommended test sequence: run one document type / period at a time, review
  MANUAL/NEW noise, then repeat per document type / period.

## 8. Known limitations

- Manual conditions of X are not injected into Y — reported as `MANUAL`.
- Group conditions / scale-based conditions may legitimately differ when Y's
  cumulative base differs from X's original document context.
- The N highest-value orders (overall or per customer) are validated per run;
  vary document type / period / customers for broader coverage.

## 9. References

- New table PRCD_ELEMENTS in S/4HANA (KONV obsolete):
  https://saplearners.com/new-table-prcd_elements-in-s-4-hana-konv-table-is-obsolete/
- CDS view V_KONV in S/4HANA:
  https://community.sap.com/t5/enterprise-resource-planning-blog-posts-by-sap/cds-view-quot-v-konv-quot-in-sap-s-4hana-on-premise/ba-p/13444713
- SAP KBA 2333377 — Issues with values returned in SD BAPI (percentage /10):
  https://userapps.support.sap.com/sap/support/knowledge/en/2333377
