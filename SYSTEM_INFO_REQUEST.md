# System information request — YGMS_BP_MASS_UPLOAD

What I need from the S/4HANA system to close the open points in
`PLAN_YGMS_BP_MASS_UPLOAD.md` and Annex A.

Ordered by how much it blocks. **Tier 1 blocks the design. Tier 2 confirms the 220
mapped fields. Tier 3 is needed to build and test.** Tier 4 cannot be answered by the
system at all — it needs a person.

How to send: SE16N → `Ctrl+Shift+F9` (or *List → Export → Spreadsheet*) → save as
`.xlsx`, drop the files in this repo or attach them. Screenshots are fine where noted.

---

## Tier 1 — blocks the design (5 items, ~15 minutes)

### 1.1 Release and support pack
- **System → Status** → screenshot of the *Component version* / *SAP System data* popup.
- Or: SE16 → table **`CVERS`**, all rows (it is tiny).

*Answers Q1.* Decides whether `CL_MD_BP_MAINTAIN` is the right API or whether we
fall back to `RFC_CVI_EI_INBOUND_MAIN`.

### 1.2 Does the API class exist, and what is its signature?
- SE24 → **`CL_MD_BP_MAINTAIN`** → screenshot of the **Methods** tab.
- Then double-click method **`MAINTAIN`** → screenshot of its **Parameters** tab.
- Same for **`VALIDATE_SINGLE`** if it exists.

*This is the single most important item.* The whole design rests on this class being
present with an `I_DATA TYPE CVIS_EI_EXTERN` importing parameter.

### 1.3 Are the old transactions really gone?
- SE93 → enter **`XK01`**, then **`XK02`**, then **`XK05`** → screenshot what each shows
  (a redirect to `BP`, a "transaction does not exist", or a normal dialog transaction).

*Confirms §2 of the plan against your system specifically, rather than against the
general S/4HANA note.* If any of these still runs natively, part of the rework argument
weakens and we should know before build.

### 1.4 India tax number categories
- SE16 → **`TFKTAXNUMTYPE`**, filter `TAXTYPE = IN*` → all rows.
- Also **`TFKTAXNUMTYPET`** (the text table) for the same rows, language `EN`.

*Answers Q3.* I need to know which category holds **PAN** and confirm `IN3` = GSTIN in
your config. Right now Annex A has PAN as class C (no API) purely because I can't confirm
it has a BP tax category — if it does, PAN moves to class A and Scenario 7 gets much simpler.

### 1.5 Does `J_1IMOVEND` still exist and is it populated?
- SE11 → **`J_1IMOVEND`** → does it exist? Screenshot the field list.
- SE16 → `J_1IMOVEND` → **number of entries** (just the count, not the data).
- If it exists: SE16 → 10 sample rows with `J_1IPANNO` or `J_1ISSIST` filled.

*Answers Q4, and decides whether Scenario 7 is built, descoped, or done by direct table
update.* If the table is empty or absent, the whole `CIN MSME` sheet drops out and 18
class-C columns disappear from the problem.

---

## Tier 2 — confirms the 220 mapped fields (one extract, ~5 minutes)

One SE16N extract from **`DD03L`** answers almost every DDIC question in Annex A at once.

- SE16N → table **`DD03L`**
- Field **`TABNAME`** → paste this list into the multiple-selection (green arrow → *Select Single Values*):

```
CVIS_EI_EXTERN
VMDS_EI_EXTERN
VMDS_EI_VMD_CENTRAL
VMDS_EI_CENTRAL_DATA
VMDS_EI_COMPANY
VMDS_EI_COMPANY_DATA
VMDS_EI_COMPANY_DATAX
VMDS_EI_PURCHASING
VMDS_EI_PURCHASING_DATA
VMDS_EI_PURCHASING_DATAX
VMDS_EI_WTAX_TYPE
VMDS_EI_FUNCTIONS
VMDS_EI_BANKDETAIL
BUS_EI_EXTERN
BUS_EI_BUPA_CENTRAL_DATA
BUS_EI_BUPA_ADDRESS
BUS_EI_BUPA_TAXNUMBER
BUS_EI_BUPA_BANKDETAIL
BUS_EI_BUPA_ROLES
LFA1
LFB1
LFM1
LFBK
LFBW
WYT3
J_1IMOVEND
BNKA
```

- Restrict to `AS4LOCAL = 'A'` (active versions only) to halve the row count.
- Columns to keep: `TABNAME`, `FIELDNAME`, `POSITION`, `ROLLNAME`, `DATATYPE`, `LENG`, `DECIMALS`.

*This confirms or corrects every class-A target path in Annex A in one go*, including
the two I flagged as assumed: **`LFM1-BSTAE`** and **`LFM1-KZABS`** (Q7).

---

## Tier 3 — needed to build and test

### 3.1 A "golden record" — the highest-value item here

Pick **one vendor that was created correctly and completely** in this system (ideally an
Indian domestic vendor with bank details, TDS and PAN maintained). Give me its **BP number
and `LIFNR`**, then extract just that one record's rows from:

| Table | Why |
|---|---|
| `BUT000` | BP header — category, grouping, name fields |
| `BUT100` | BP roles actually assigned (confirms `FLVN00` / `FLVN01`) |
| `BUT020` + `ADRC` + `ADR6` | address and e-mail as actually stored |
| `BUT0ID` | BP identification numbers |
| `BUT0BK` | BP bank details |
| `LFA1`, `LFB1`, `LFM1` | supplier general / CC / purchasing |
| `LFBK`, `LFBW`, `LFB5`, `WYT3` | bank, withholding tax, dunning, partner functions |
| `J_1IMOVEND` | CIN, if the table exists |
| `CVI_VEND_LINK` | the BP ↔ vendor link row |

**Why this matters more than anything else in Tier 3:** it lets me reverse-engineer
exactly which fields your business actually populates and with what values, instead of
me inferring from a template's guideline column. It will catch mistakes in Annex A that
no DDIC extract can.

### 3.2 Configuration — for the validation layer

Full extracts (all are small):

| Table | Contents | Validates |
|---|---|---|
| `T001` | Company codes | `BUKRS` |
| `T024E` | Purchasing organisations | `EKORG` |
| `T077K` | Vendor account groups | `KTOKK` |
| `TB001` | BP groupings / number ranges | BP grouping (Q2) |
| `TB003` | BP roles | role assignment |
| `T005` / `T005S` | Countries / regions | `COUNTRY`, `REGION` |
| `T052` | Payment terms | `ZTERM` |
| `T042Z` | Payment methods | `ZWELS` |
| `T059P` | Withholding tax types (filter `LAND1 = 'IN'`) | `WITHT` |
| `T059Z` | Withholding tax codes (filter `LAND1 = 'IN'`) | `WT_WITHCD` |
| `TPAR` | Partner functions | `PARVW` |
| `T016` | Industry keys | `BRSCH` |
| `T035` | Cash management / planning groups | `FDGRV` |
| `T012K` | House banks | `HBKID` |
| `TINC` | Incoterms | `INCO1` |
| `TSAD3T` | Title keys + texts (language `EN`) | `TITLE_MEDI` — the template holds the *text* "Company", I need the key behind it |
| `SKA1` | G/L accounts, recon accounts only (`XBILK`/`MITKZ = 'K'`) | `AKONT` |

`BNKA` is likely large — **just the row count** plus 20 sample rows is enough.

### 3.3 CVI account-group mapping (Q2)

I couldn't safely name the table for this, so please go by IMG path:

> SPRO → Cross-Application Components → Master Data Synchronization →
> Customer/Vendor Integration → Business Partner Settings → Settings for Vendor
> Integration → Field Assignment for Vendor Integration → Assign Keys

Screenshots of:
- **Define Number Assignment for Direction Vendor to BP**
- **Define BP Role for Direction Vendor to BP** (or similarly named node)

*This tells me which BP grouping and role each vendor account group maps to*, which
Scenario 1 cannot set without.

### 3.4 Is there already a Z/Y upload program for this?

- SE80 / SE38 → search program names `Y*BP*`, `Z*BP*`, `Y*VEND*`, `Z*VEND*`, `*UPLOAD*`.

Worth two minutes to avoid rebuilding something that exists.

### 3.5 Authorisations

Which authorisation objects the intended users hold — particularly `B_BUPA_RLT`,
`B_BUPA_GRP`, `F_LFA1_BUK`, `M_LIEF_EKO`, `F_BNKA_MAN`. A screenshot of a representative
role is fine.

---

## Tier 4 — the system cannot answer these; a person must

These are the real blockers on Scenarios 6 and 7. No extract will resolve them.

| # | Question | Who |
|---|---|---|
| **Q4** | Are the pre-GST excise fields (ECC no., excise registration / range / division / commissionerate, CST, LST, service tax) still used? Where does **MSME / Udyam** status live today? | FI / Taxation |
| **Q6** | In `TAN details update.xlsx`: what do **`Section_code`**, **`threshold`** and **`Currency`** actually drive? None has an `LFBW` field, and in your sample data `Section_code` equals the company code. Also — what is the **unheaded 22nd column** holding `0`? | FI / Taxation |
| **Q8** | Block templates document `B` as the block value but the sample data uses `X`. And how should **unblock** be signalled? An empty cell cannot express it — see below. | Purchase / Master Data |
| **Q9** | The ZIP is named "vendor **and customer** master" but contains only vendor templates. Is customer creation in scope? | Project |
| **Q11** | Is there an approval / maker-checker step before a vendor goes live, or does the program post directly? | Project |
| — | Vendor bank details: **append or replace**? And how should `BVTYP` (partner bank type) be derived — it is absent from the template. | Finance / Payments |

### On Q8 specifically — this needs a decision, not an extract

Clearing a field through this API means writing `DATA-<field> = space` **together with**
`DATAX-<field> = 'X'`. An empty spreadsheet cell cannot distinguish "leave this alone"
from "clear it". So **unblock is not implementable with the template as it stands.**

My proposal: add an explicit `Action` column with values `BLOCK` / `UNBLOCK`. That is a
template change and needs sign-off from whoever owns these files.

---

## Summary of what unblocks what

| Give me | Unblocks |
|---|---|
| Tier 1 (1.1–1.5) | Whether the design is correct at all; Scenario 7 scope |
| Tier 2 (`DD03L`) | Confirmation of all 220 class-A mappings |
| Tier 3.1 (golden record) | Catches mapping errors nothing else will |
| Tier 3.2–3.3 | The validation layer and Scenario 1 |
| Tier 4 | Scenarios 6 and 7, and unblock in Scenario 11 |

**Tier 1 alone is enough for me to start Phase 1** (framework + Scenarios 8 and 9).
