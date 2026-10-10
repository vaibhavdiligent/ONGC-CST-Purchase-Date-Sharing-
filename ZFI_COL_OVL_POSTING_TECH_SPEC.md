# ZFI_COL_OVL_POSTING — Corporate Colombia: OVC → OVL posting

Source FS: `FW FS for Corporate - Colombia.msg` (Raju Ashar). Decisions confirmed by the user on 10.10.2026 are listed in §6.

| Deliverable | File |
|---|---|
| Report source | `ZFI_COL_OVL_POSTING.abap` |
| abapGit package (report + 2 tables + texts) | `ZFI_COL_OVL_POSTING_abapGit.zip` |
| This document | `ZFI_COL_OVL_POSTING_TECH_SPEC.md` |

---

## 1. DDIC objects (create before activating the report)

### 1.1 Table `ZFI_COL_GLMAP`: mapping (the FS "ZTable")
Transparent, delivery class **C**, *Display/Maintenance allowed*. Generate table maintenance in **SE54** (authorisation group e.g. `&NC&` or the FI group, one-step, function group `ZFI_COL_GLMAP`), then maintain the table in **SM30**.

| Field | Key | Data element | FS column | Check (SE11 foreign key / runtime) |
|---|---|---|---|---|
| MANDT | ✔ | MANDT | — | |
| FROM_BUKRS | ✔ | BUKRS | From Co.Code | T001 |
| FROM_HKONT | ✔ | HKONT | GL for From Co.Code | SKB1 (BUKRS = FROM_BUKRS) |
| TO_BUKRS | ✔ | BUKRS | To Co.Code | T001 |
| TO_HKONT | | HKONT | GL for To Co.Code | SKB1 (BUKRS = **TO_BUKRS**). Confirmed: the FS saying "From" was a typo |
| KOSTL | | KOSTL | To Co.Code Cost Center | CSKS (BUKRS = TO_BUKRS) |
| PRCTR | | PRCTR | To Co.Code Profit Center | CEPC (BUKRS = TO_BUKRS) |
| GSBER | | GSBER | Business Area | TGSB. Confirmed: **GSBER**, not BUPLA |
| OFFSET_HKONT | | HKONT | OFFSET GL | SKB1 (BUKRS = TO_BUKRS) |

The report checks every value again at run time (SKB1 / CSKS / CEPC valid on the posting date / TGSB) and lists all invalid entries in one message.
Rule: within one From Co.Code, all mapped GLs must point to the **same** To Co.Code, because everything goes into one document.

### 1.2 Table `ZFI_COL_POSTLOG`: success log / duplicate block
Transparent, delivery class **A**, *Display allowed, maintenance not allowed* (written only by the report).

| Field | Key | Data element | Meaning |
|---|---|---|---|
| MANDT | ✔ | MANDT | |
| BUKRS | ✔ | BUKRS | OVC company code (selection) |
| GJAHR | ✔ | GJAHR | Fiscal year (selection) |
| POPER | ✔ | POPER | Period (selection) |
| TO_BUKRS | | BUKRS | OVL company code |
| BELNR | | BELNR_D | OVL document number |
| TO_GJAHR | | GJAHR | OVL fiscal year |
| BUDAT | | BUDAT | Posting date used |
| ERNAM / ERDAT / ERZET | | ERNAM / ERDAT / ERZET | Posted by / on / at |

If a posting has to be redone (e.g. after reversing the OVL document), the log row must be deleted first, through SE16N or an authorised utility, on purpose.

### 1.3 Text elements (already included in the abapGit `.prog.xml`; add them manually only if you paste the source)
- Text symbols: `B01` Selection, `B02` Processing
- Selection texts: `P_BUKRS` From Co.Code (OVC), `P_POPER` Period, `P_GJAHR` Fiscal Year, `P_TEST` Test Run (no posting)

Optional: create a transaction code (e.g. `ZFI_COL_POST`) in SE93.

---

## 2. Selection screen
| Parameter | Type | Note |
|---|---|---|
| P_BUKRS | BUKRS | From Co.Code (OVC). Must exist; display authorisation F_BKPF_BUK 03 |
| P_POPER | POPER | One period, 001–016 |
| P_GJAHR | GJAHR | Fiscal year |
| P_TEST | Checkbox, default **ON** | Test run: `BAPI_ACC_DOCUMENT_CHECK` only, nothing posted, nothing logged |

If OVC/year/period is already in `ZFI_COL_POSTLOG`, the selection screen stops with an error that shows the OVL document number.

---

## 3. Processing logic

**Step 1: documents (ACDOCA)**
`SELECT DISTINCT BELNR, GJAHR FROM ACDOCA WHERE RLDNR = '0L' AND RRCTY = '0' AND RBUKRS = P_BUKRS AND GJAHR = P_GJAHR AND POPER = P_POPER AND RACCT IN ('0000095671', '0000956712')`. The GLs are hard-coded and converted with ALPHA, so they carry leading zeros.

**Step 2: subtotals (BSEG)**
`BSEG` for those documents, BUKRS = P_BUKRS, HKONT **not** 95671 / 956712.
Each line's DMBE2 gets a sign (SHKZG `H` = negative), then the lines are **subtotalled per HKONT**. GLs that net to zero are dropped.

**Mapping**: every subtotal GL must exist in `ZFI_COL_GLMAP` for P_BUKRS. Unmapped GLs are listed and the run stops.

**Dates**
- `LAST_DAY_IN_PERIOD_GET` (OVC fiscal-year variant `T001-PERIV`, P_GJAHR, P_POPER) gives the last day of the OVC period. This is both the **posting date and the document date** in OVL.
- `DATE_TO_PERIOD_CONVERT` (OVL `T001-PERIV`) gives the OVL period/year. It is only shown in the ALV header, because the BAPI derives the period from the posting date.

**Posting: ONE document via `BAPI_ACC_DOCUMENT_POST`**
- Header: company code = To Co.Code, doc type **CL** (hard-coded), BUS_ACT `RFBU`, reference `OVC/PPP/YYYY`, header text.
- For **each** subtotal GL, two lines:
  1. Mapped GL (TO_HKONT) with cost center, profit center and business area from the table, amount = DMBE2.
  2. Offset GL (OFFSET_HKONT) with profit center and business area, amount = −DMBE2.
- Posting key: the BAPI derives it from the sign. A positive amount gives **40** on the mapped GL and **50** on the offset; a negative amount gives the reverse. The ALV shows both keys.
- Currency: `CURRENCYAMOUNT` has only CURR_TYPE `00` (document currency) = **USD**. The system translates it to the OVL local currency (**INR**) at the posting-date rate. The BAPI does this conversion itself when no local-currency row is supplied.
- Limit: 999 lines per FI document, i.e. at most 499 GLs.

**Duplicate / concurrency control**
- A lock (`ENQUEUE_E_TABLE` on `ZFI_COL_POSTLOG` + OVC/year/period) is held during the check and post, and the log is re-read under that lock.
- After a successful post, the log row is inserted **in the same LUW**, then `BAPI_TRANSACTION_COMMIT` (WAIT). If the BAPI or the insert fails, `BAPI_TRANSACTION_ROLLBACK` runs, so there is never a document without a log row or a log row without a document.

**Authorisation**: F_BKPF_BUK ACTVT 03 on OVC (selection), ACTVT 01 on OVL (real run only).

---

## 4. Output
ALV, one row per OVC GL: status light, item no., OVC GL, amount (USD, with total), PK, mapped GL, CC, PC, BA, offset item, offset GL, offset PK, message.
The header shows OVC/period → OVL, the posting date and the OVL period. When there are errors, a popup lists all BAPI return messages; line-level errors are also shown on the matching ALV row.

---

## 5. Unit test cases
| # | Scenario | Expected |
|---|---|---|
| 1 | Test run, valid period, all GLs mapped | Yellow lights, "Test run OK", nothing posted, no log row |
| 2 | Real run, same input | Green lights, OVL document type CL, posting = document date = last day of OVC period, USD amounts with INR translated, row in ZFI_COL_POSTLOG |
| 3 | Run test 2 again | Selection-screen error "Period already posted…" with the doc number |
| 4 | One GL not in ZFI_COL_GLMAP | Stops and lists the unmapped GL(s) |
| 5 | Mapped GL / CC / PC / BA not valid in OVL | Stops: "Invalid mapping for …" lists them all |
| 6 | Mapping points to two different To Co.Codes | Stops: one document needs one company code |
| 7 | No ACDOCA documents on 95671 / 956712 | Message "No documents found…" |
| 8 | GL whose net DMBE2 is negative | Mapped GL PK 50, offset PK 40 |
| 9 | OVL period closed / BAPI error | Red lights, popup with messages, rollback, no log row |
| 10 | Two users post the same period at the same time | The second gets "Period is locked…" or "already posted" |
| 11 | OVC and OVL with different fiscal-year variants | Posting date = OVC period end; OVL period derived correctly (check the ALV header) |

---

## 6. Decisions confirmed by the user (10.10.2026)
1. GL for To Co.Code is checked against SKB1 of the **To** company code.
2. Business Area = **GSBER**.
3. Hard-coded GLs **95671** and **956712** confirmed, with leading zeros added internally.
4. Post in **USD**; the system converts to INR.
5. Each GL line has its own offset line, and **everything goes in one document**.
6. Document type **CL**, hard-coded.
7. Success log table: OVC Co.Code / fiscal year / period. If an entry exists, the next run is blocked.
8. Names `ZFI_COL_GLMAP`, `ZFI_COL_POSTLOG`, `ZFI_COL_OVL_POSTING` approved.

### Assumptions (tell us if any are wrong)
- The **offset line** carries the profit center and business area of the mapping row but **no cost center**, because the FS gives only the offset GL. If the offset GL is a cost element, a cost center is needed. In that case add one in the table/report, or use a balance-sheet offset GL.
- DMBE2 (OVC 2nd local currency) is USD.
- The profit-center check accepts `CEPC-BUKRS = To Co.Code` **or** blank, because many systems leave CEPC-BUKRS empty and keep the assignment elsewhere.

### References checked
- BAPI_ACC_DOCUMENT_POST derives local currency from the document-currency amount and the exchange rate: [SAP Community](https://community.sap.com/t5/application-development-and-automation-discussions/bapi-mega-urgent/m-p/1382555), [CURR_TYPE 00/10 usage](https://blogs.sap.com/t5/technology-blogs-by-members/posting-journal-entry-document-in-sap-using-bapi/ba-p/13470400)
- LAST_DAY_IN_PERIOD_GET interface (I_GJAHR / I_PERIV / I_POPER → E_DATE): [SAP Community](https://community.sap.com/t5/application-development-and-automation-discussions/function-module/m-p/4327918)
- DATE_TO_PERIOD_CONVERT interface (I_DATE / I_PERIV → E_BUPER / E_GJAHR): [se80.co.uk](https://www.se80.co.uk/sapfms/d/date/date_to_period_convert.htm)
- CEPC has field BUKRS (check table T001): [leanx.eu](https://leanx.eu/en/sap/table/cepc.html)
