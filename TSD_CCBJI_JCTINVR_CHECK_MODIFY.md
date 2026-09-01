# Technical Specification — NTA Invoice Number Check & Modify (S/4HANA)

| | |
|---|---|
| Program | `/CCBJI/JCTINVR_CHECK_MODIFY` |
| Report title | NTA Invoice Reg.No. - Daily Status Check and Modify |
| Transaction | `/CCBJI/JCTINVRCHK` |
| Replaces | Ab Initio / PI graph `05228MD_NationalTaxAgency_C1_InvoiceNumber_CheckModify` |
| Package | `/CCBJI/ODATA_DYNAMIC` (same package as the JCTINVR OData objects) |
| Table | `/CCBJI/T_JCTINVR` — Invoice registration number (qualified invoicing business) |
| Schedule | Daily background job, legacy slot 10:00 JST |
| Author | Claude (Diligent Consulting) — for Vaibhav Maheshwari |

---

## 1. Why the graph disappears

In the legacy landscape the graph read `"SAPCOK"//CCBJI/T_JCTINVR` on DB2, split the
stream into a "delete" and an "update/active" branch, reformatted the delta fields and
wrote the rows back through PI.

In S/4HANA `/CCBJI/T_JCTINVR` is a local transparent table, so read → decide → write is
one ABAP report. No PI channel, no DB2 connection, no gather component.

| Ab Initio component | S/4HANA equivalent |
|---|---|
| Read DB2 source table | `SELECT` on `/CCBJI/T_JCTINVR` (`FORM read_candidates`) |
| Filter "Mark as Deleted (D)" | `FORM classify_records`, branch 1 |
| Filter "Mark as Updated/Active (U)" | `FORM classify_records`, branch 2 |
| Reformat output (`zupdind/mandt/zaenam/zupdat/zuptim`) | field assignment in `FORM update_database` |
| Gather | single internal table `gt_log` |
| Update DB2 target (key `mandt + invoice_cd`) | `UPDATE /ccbji/t_jctinvr … WHERE invoice_cd = …` |
| Daily 10:00 JST schedule | SM36 job on `/CCBJI/JCTINVRCHK` |

## 2. Selection screen

| Field | Type | Default | Purpose |
|---|---|---|---|
| `P_DATE` | `SY-DATUM` (obligatory) | `SY-DATUM` | Process date the revocation / expiration dates are compared against |
| `S_INVCD` | select-option on `INVOICE_CD` | empty | Optional restriction (testing / re-runs) |
| `P_CHGBY` | `ZAENAM` (CHAR 12) | `SY-UNAME` | Value written to `ZAENAM` (legacy constant was `AbInitio`) |
| `P_TEST` | checkbox | `X` | Test run — classify and list, no database update |
| `P_CMTSZ` | INT4 | 5000 | `COMMIT WORK` after n updated rows |
| `P_ALV` | checkbox | `X` | ALV in dialog; classic list is always used in background |

## 3. Processing logic

A registration number is **no longer valid** on the process date when

```
( REVOCATION_DATE <> '00000000' AND REVOCATION_DATE <= P_DATE )
OR
( EXPIRATION_DATE <> '00000000' AND EXPIRATION_DATE <= P_DATE )
```

| Case | Condition | Action |
|---|---|---|
| 1 | no longer valid **and** `ZUPDIND <> 'D'` | `ZUPDIND = 'D'` |
| 2 | still valid **and** `ZUPDIND = 'D'` **and** at least one of the two dates filled | `ZUPDIND = 'U'` |
| 3 | anything else | untouched |

Case 2 keeps the legacy safeguard that a row flagged `D` **without** any revocation or
expiration date was deleted by another process and must not be reactivated.

For every changed row the delta/audit block is refreshed exactly as the legacy reformat
component did: `ZUPDIND`, `ZAENAM` (= `P_CHGBY`), `ZUPDAT` (= `SY-DATUM`),
`ZUPTIM` (= `SY-UZEIT`). `MANDT` is not written explicitly — Open SQL works in the logon
client of the job (the graph hard-coded `100`).

### Interpretation note

The graph draws the revocation filter and the expiration filter stacked inside each
branch. They are implemented here as **OR** (either date makes the number invalid),
which is the only reading consistent with the NTA rules — a number that is revoked is
invalid whether or not it also carries an expiration date. If the legacy graph really
chained them as AND (both dates required), change the `OR` in `FORM classify_records`
to `AND`; nothing else moves.

## 4. Performance

* Only rows carrying a revocation or an expiration date are read
  (`WHERE revocation_date <> '00000000' OR expiration_date <> '00000000'`), and only
  four columns are transferred — the bulk of the registry never leaves the database.
* Updates touch four fields per row and commit every `P_CMTSZ` rows, so a large first
  run does not build one huge transaction.
* If the candidate set ever grows beyond comfortable memory, restrict with `S_INVCD`
  or add a secondary index on `REVOCATION_DATE` / `EXPIRATION_DATE`.

## 5. Output

* Dialog: ALV (`CL_SALV_TABLE`) — invoice number, indicator before/after, both dates,
  action, status; run parameters and counters as top-of-page.
* Background: classic list to the spool — the same columns plus the counter block
  (records read, flagged `D`, flagged `U`, rows updated, rows in error).

## 6. Test plan

1. `P_TEST = 'X'` with `P_DATE = SY-DATUM` — check the classification against a manual
   `SE16N` sample before any update run.
2. Row with `REVOCATION_DATE` = yesterday and `ZUPDIND = 'U'` → becomes `D`.
3. Row with `EXPIRATION_DATE` = yesterday and `ZUPDIND` initial → becomes `D`.
4. Row with `REVOCATION_DATE` = tomorrow and `ZUPDIND = 'D'` → becomes `U`.
5. Row with `ZUPDIND = 'D'` and both dates `00000000` → untouched.
6. Re-run the same day → zero changes (the job is idempotent).
7. Back-date `P_DATE` to reproduce a historical run and compare with the legacy DB2 result.

## 7. Naming convention

The object names follow the objects already deployed for this same table and the same
NTA interface (see `ZCCBJI_JCTINVR_ODATA_GUIDE.md` on branch
`claude/odata-ccbji-t-jctinvr-zntu9g`): the productive import goes into package
`/CCBJI/ODATA_DYNAMIC` and the objects carry the `/CCBJI/` namespace —
`/CCBJI/CL_JCTINVR_MPC`, `/CCBJI/CL_JCTINVR_DPC`, `/CCBJI/JCTINVR_MDL`,
`/CCBJI/JCTINVR_SRV`. This report is the batch counterpart of that inbound service,
so it joins the same `JCTINVR` family:

| Object | Name | abapGit file |
|---|---|---|
| Report | `/CCBJI/JCTINVR_CHECK_MODIFY` (27 chars, limit 30) | `src/#ccbji#jctinvr_check_modify.prog.*` |
| Transaction | `/CCBJI/JCTINVRCHK` (17 chars, limit 20) | `src/#ccbji#jctinvrchk.tran.xml` |

CCBJI also runs an older report convention of the form
`/CCBJI/R<U|D><module><submodule>R_<name>` (`/CCBJI/RUFIGLR_REPORTING_SUPP`,
`/CCBJI/RUFIAPR_ACCURAL_DME`, `/CCBJI/RUSDSLSR_LSA_UPLOAD`). If the CCBJI object
register requires that form for this interface (05228**MD** = master data), the
equivalent name is `/CCBJI/RUFIMDR_INVNUM_CHKMOD` with transaction
`/CCBJI/RUFIMD_INVCHK` — rename the two files, the `REPORT` statement, `<NAME>` in
the `.prog.xml` and `TCODE`/`PGMNA` in the `.tran.xml`; the logic is unaffected.

Creating objects in `/CCBJI/` requires the namespace to be set to *modifiable* in
SE03 on the target system (repair licence for the namespace), which is already the
case there — the OData classes above were imported the same way.
