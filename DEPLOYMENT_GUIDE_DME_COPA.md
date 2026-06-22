# Deployment & Object Creation Guide — DME Month-End CO-PA

How to create the new objects in S/4HANA from this repo. Create them in the
order below (the program consumes the CDS view, so create the CDS first).

| # | Object | Type | Name | Source file |
|---|---|---|---|---|
| 1 | CDS view | DDL source | `ZC_DME_SALES_COPA` (SQL view `ZCDMESALESCOPA`) | `ZC_DME_SALES_COPA.ddls.asddls` |
| 2 | Report | Executable program | `ZRDME_MONTH_END_COPA` | `ZRDME_MONTH_END_COPA.abap` |
| 3 | Table change | DDIC (optional/future) | append `GL_ACCT_ACCTBSD` to GL map | — |
| 4 | TVARVC entries | Config | see §4 | — |
| 5 | Transaction code | Tcode | e.g. `ZRDME_ACC_DME` | — |

---

## 1. Create the CDS view `ZC_DME_SALES_COPA`

> **Must be created in ADT (Eclipse) — CDS views cannot be created in SE80/SE11.**

**Steps (ADT / Eclipse):**
1. Install **ABAP Development Tools (ADT)** in Eclipse and connect to the S/4 system.
2. In **Project Explorer** → expand your package → right-click **Core Data Services** → **New** → **Data Definition**.
3. Enter:
   - **Name:** `ZC_DME_SALES_COPA`
   - **Description:** DME Month-End CO-PA Sales Source (CE2/ACDOCA)
   - **Package:** your DME package (e.g. `/CCBJI/RTR` or the S/4 Z-package)
4. Choose transport request when prompted.
5. On the template screen pick **"Define View"** (or just clear it).
6. **Paste the entire contents** of `ZC_DME_SALES_COPA.ddls.asddls` from this repo, replacing the template.
7. **Before activating — confirm two things** (see §1.1).
8. **Activate** (Ctrl+F3).

### 1.1 Field-name checks before activating the CDS

The view has one parameter and two source branches (CE2JP00 + ACDOCA):

- **Parameter** `p_cutover : jahrper` — the CE2→ACDOCA switch period. The
  program passes this at runtime; nothing to change.
- **Branch 1 (CE2JP00):** standard fields — no change expected.
- **Branch 2 (ACDOCA):** the CO-PA characteristics `WW207, WW214, WW228,
  WW229, VKAUS` are read from ACDOCA. These were **CONFIRMED present in
  account-based CO-PA (ACDOCA)** by the functional team (16-Jun-2026). If a
  field activation error occurs, the only edit needed is the **"T2 EDIT
  POINT"** block in the DDL — adjust the ACDOCA append field name and
  re-activate. No other change is required.

> If a customer ACDOCA append uses different technical names, that is the only
> place to fix them.

---

## 2. Create the report `ZRDME_MONTH_END_COPA`

**Steps (SE38 / SE80 or ADT):**
1. **SE38** → program `ZRDME_MONTH_END_COPA` → **Create**.
2. Attributes: **Type = Executable program (1)**, **Status = Customer Program**,
   **Application = blank**, assign your package + transport.
3. **Paste the entire contents** of `ZRDME_MONTH_END_COPA.abap` from this repo.
4. Maintain **text symbols** (Goto → Text Elements → Text Symbols):
   - `001` Input parameters
   - `002` Test / Production
   - `012` Posting Parameters
   - `016` Payment Type
   - `027` Sales Update
   - plus the inline column / message texts (the editor flags any missing).
5. **Check the WCOCOH field names (T1)** — see §2.1.
6. **Activate** (Ctrl+F3).
7. Create the transaction code (§5).

### 2.1 Field-name check before activating the report

Open the banner **"EXTERNAL FIELD-NAME MAPPING — SINGLE EDIT POINT"** at the
top of the program. The only unconfirmed item is **T1 (WCOCOH append names)**:

- In `F_GET_DATA`, the **"T1 EDIT POINT"** SELECT reads `WCOCOH-ZZBOTACC`
  (bottler account) and `WCOCOH-CUST_OWNER` (dealer/owner).
- **Pending confirmation with Pankaj-san.** If the real append names differ,
  change them only in that SELECT and re-activate. This is the **only**
  remaining activation blocker.

---

## 3. (Optional / future) GL mapping table

Per client decision **B1**, GLs are read from the existing `/CCBJI/T_DME_GL`
and the **same GL is used for CB and AB** CO-PA — so **no DDIC change is
required now**.

When a separate account-based GL is later confirmed:
1. Add field `GL_ACCT_ACCTBSD : SAKNR` to the GL mapping (append or
   `ZDME_GL_MAPPING`).
2. In `F_GET_DATA`, point the GL `SELECT` at that table/field.
3. `gv_abgl` then automatically uses the new column (code already handles it).

---

## 4. TVARVC configuration (STVARV)

| Name | Type | Value | Purpose |
|---|---|---|---|
| `/CCBJI/DME_ACDOCA_FROM` | Parameter (P) | `2026001` (test) / `2027012` (prod) | Sales-source CE2→ACDOCA cut-over period passed to the CDS view (F3) |
| `/CCBJI/RTR_DME_CRGL` | Parameter (P) | credit GL account | Offsetting credit GL |

Maintain in transaction **STVARV** (numb `0000`).

---

## 5. Transaction code

1. **SE93** → create e.g. `ZRDME_ACC_DME`.
2. Type: **Program and selection screen (report transaction)**.
3. Program: `ZRDME_MONTH_END_COPA`. (Final tcode name to be confirmed — item T4.)

---

## 6. Activation order & smoke test

1. Activate CDS `ZC_DME_SALES_COPA`.
2. Activate report `ZRDME_MONTH_END_COPA`.
3. Maintain TVARVC (`/CCBJI/DME_ACDOCA_FROM = 2026001` for testing).
4. Run the report with **RB_TEST = X** (test run — uses
   `BAPI_ACC_DOCUMENT_CHECK`, no database update) for a known Spot-Pay
   contract; verify the ALV output and messages.
5. After **F1** sandbox sign-off (BAPI posts both CB + AB), switch to
   **RB_POST** for production posting.

> See `BUILD_READINESS_DME_COPA.md` for the full activation / test /
> production gating checklist.
