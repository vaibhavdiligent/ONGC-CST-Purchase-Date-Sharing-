# TSD — /CCBJI/SD_PRICING_VALIDATION

**CCBJI (Coca-Cola Bottlers Japan) — Pricing regression validation after ECC → S/4HANA migration**

| | |
|---|---|
| Program | `/CCBJI/SD_PRICING_VALIDATION` (executable report, SE38, customer namespace /CCBJI/) |
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
reproduces the ECC pricing result, the user enters only a **document type and a
creation date range**. The program automatically picks the sales order **X**
with the **highest net value** (`VBAK-NETWR`) in that period, creates a copy
order **Y** on which S/4HANA re-derives the pricing from scratch, and compares
the pricing conditions of X and Y line by line. Any delta indicates a
configuration or condition-record migration defect.

All details of X (org data, partners, items, quantities, pricing date,
conditions) are read from the database — `VBAK`, `VBAP`, `VBPA`, `VBKD` and
`PRCD_ELEMENTS` (equivalent to what `BAPISDORDER_GETDETAILEDLIST` would
return, but without the external-format conversions).

## 2. Processing modes

### Mode 1 — Create order Y (default, chosen by CCBJI)

1. Order Y is really created with `BAPI_SALESORDER_CREATEFROMDAT2`,
   `LOGIC_SWITCH-PRICING = 'B'` (carry out new pricing).
2. Y's header/org data, sold-to (AG), ship-to (WE), items (material, plant,
   quantity, sales unit) are copied from X. The PO number of Y is stamped
   **`PRCVAL-<X order number>`** for traceability.
3. After `BAPI_TRANSACTION_COMMIT` (wait), Y's freshly calculated conditions
   are read from **PRCD_ELEMENTS** via Y's `VBAK-KNUMV` — an
   internal-to-internal comparison against X's PRCD_ELEMENTS records.
4. **Y remains in the system** (no rejection step, per CCBJI decision). The
   `PRCVAL-*` PO number makes the test copies easy to find (VA05 /
   `VBAK-BSTNK`) and clean up manually once the validation cycle is finished.

> Note: create mode consumes number ranges and may trigger outputs / credit
> checks / ATP depending on configuration, and the Y orders stay open until
> cleaned up. Run it in the QA client that holds the migrated production
> data, not in production.

### Mode 2 — Simulate (optional)

`BAPI_SALESORDER_SIMULATE` re-prices without saving anything; the calculated
conditions are returned in `ORDER_CONDITION_EX`. No number ranges, no
follow-on side effects — suitable for validating thousands of orders.

## 3. Pricing date

Radio buttons:
- **X's original pricing date** (default) — read from `VBKD-PRSDT` (header
  record, fallback first VBKD / `VBAK-ERDAT`). This makes the same
  condition-record validity periods apply as on X, i.e. a true
  ECC-engine-vs-S/4-engine equivalence test.
- **Today's date** — validates today's condition records instead
  (config-currency test, not an equivalence test).

## 4. Comparison logic

- X's conditions: `PRCD_ELEMENTS` via `VBAK-KNUMV` (alternative access in
  S/4HANA would be CDS view `V_KONV`).
- Match key: **item (KPOSN) + condition type (KSCHL) + occurrence** (n-th
  appearance of the type within the item, in STUNR/ZAEHK order) — so condition
  types appearing twice are compared pairwise.
- Compared fields: rate **KBETR**, pricing unit **KPEIN**, condition unit
  **KMEIN**, condition value **KWERT** (create mode only), plus one row per
  item for the item net value **NETWR**.
- Tolerance `P_TOL` (absolute, external units; default 0 — with JPY even ¥1
  deltas are relevant).

### Amount normalisation (critical for JPY)

All amounts are converted to external format before comparison:

- **Currency decimal shift**: internal CURR fields always carry 2 decimals;
  TCURX defines the real decimals per currency. JPY has 0 decimals, so real
  amount = stored amount × 100 (`× 10^(2 − TCURX-CURRDEC)`).
- **Percentage conditions** (`KRECH = 'A'`): KBETR is stored with one implied
  extra decimal (KBETR 100.00 = 10.000 %) → divided by 10.
- BAPI outputs (`ORDER_CONDITION_EX`) are already external; per SAP KBA
  **2333377** percentage rates are returned divided by 10 (true percent). A
  factor-10 delta on a percentage condition is flagged with a dedicated remark.

### Classification

| Status | Meaning |
|---|---|
| `OK` | Values identical within tolerance (suppressed when "differences only" is set) |
| `MISMATCH` | S/4 pricing deviates from the ECC result — config/record defect candidate |
| `MISSING_S4` | Condition on X not re-determined on Y → missing/wrong condition record or access sequence |
| `NEW_IN_S4` | Condition determined on Y but absent on X |
| `MANUAL` | Manually entered on X (`KHERK = 'C'` / `KMPRS = 'X'`) — cannot be re-derived by repricing; info only |
| `ERROR` | Y could not be created/simulated (BAPI messages shown in remark) |

Filtering rules: inactive lines (`KINAK ≠ space`) are ignored; statistical
lines (`KSTAT = 'X'`) are ignored unless `P_STAT` is set; fully rejected items
of X (`ABGRU ≠ space`) are skipped; condition types can be restricted via
`S_KSCHL`. Header conditions (`KHERK = 'D'`) get an explanatory remark on
mismatch, since header distribution across items can legitimately differ.

## 5. Selection screen

| Field | Description |
|---|---|
| `S_AUART` (obligatory) | Sales document type(s) |
| `S_ERDAT` (obligatory) | Creation date range — the order with the highest `VBAK-NETWR` in this period is selected as X |
| `P_CRT` / `P_SIM` | Mode: create order Y (default) / simulate |
| `P_DTOLD` / `P_DTTOD` | Pricing date for Y: X's PRSDT (default) / today |
| `S_KSCHL` | Restrict condition types |
| `P_TOL` | Absolute tolerance (default 0) |
| `P_STAT` | Include statistical conditions |
| `P_ONLYER` | Show differences only (default on) |

The ALV header shows which order was picked as X and its net value.

## 6. Output

SALV grid (layout save enabled, Excel export via standard ALV functions):
X order, Y order, item, material, condition type, status (color-coded),
rate X/Y/delta, pricing unit X/Y, UoM X/Y, condition value X/Y/delta, remark.
Header block shows run totals (orders, errors, OK / mismatch / missing / new /
manual counts).

## 7. Setup / transport notes

- The program lives in the customer namespace **/CCBJI/** — the namespace must
  exist in the system with a valid developer license key (SE03 → Administration
  → Display/Change Namespaces) and the target package must allow it.
- Maintain text elements: `TEXT-001…004` (block titles), selection texts, and
  the message/remark text symbols (`M01/M02`, `R01…R06`).
- Created Y orders are **not rejected automatically**. Plan a periodic manual
  clean-up: find them via VA05 / `VBAK-BSTNK = PRCVAL-*` and reject or delete
  them so they do not distort open-order reporting or get delivered.
- Authorization: standard checks of the BAPIs apply (VA01 equivalent);
  run with a user authorized for order creation in the test client.
- Recommended test sequence: run one document type at a time with
  "differences only" off first, review MANUAL/NEW noise, then repeat per
  document type / period.

## 8. Known limitations

- Manual conditions of X are not injected into Y — reported as `MANUAL`.
- Group conditions / scale-based conditions may legitimately differ when Y's
  cumulative base differs from X's original document context.
- In simulate mode KWERT is not compared per condition (rate + item net value
  only); create mode compares KWERT as well.
- Free-goods items may return no conditions in simulate mode (known BAPI
  behaviour).

## 9. References

- New table PRCD_ELEMENTS in S/4HANA (KONV obsolete):
  https://saplearners.com/new-table-prcd_elements-in-s-4-hana-konv-table-is-obsolete/
- CDS view V_KONV in S/4HANA:
  https://community.sap.com/t5/enterprise-resource-planning-blog-posts-by-sap/cds-view-quot-v-konv-quot-in-sap-s-4hana-on-premise/ba-p/13444713
- SAP KBA 2333377 — Issues with values returned in SD BAPI (percentage /10):
  https://userapps.support.sap.com/sap/support/knowledge/en/2333377
- BAPI_SALESORDER_SIMULATE conditions in ORDER_CONDITION_EX:
  https://community.sap.com/t5/application-development-discussions/bapi-or-fm-to-simulate-pricing-for-sales-order/td-p/3854788
