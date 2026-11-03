# Custom Data Monitor Task: Reverse Group Journal Entries of a Document Type

Code: `src/zcl_fincs_cdm_reverse_badi.clas.abap` (BAdI implementation) and
`src/zcl_fincs_gje_reversal.clas.abap` (reversal logic).

## 1. What is publicly known (SAP Help / SAP Press / SAP Community)

| Item | Fact |
|---|---|
| BAdI | `BADI_FINCS_CUST_DATAMON` (custom task in Data Monitor, CXCD) |
| Filter | `CONSOLIDATIONTASK` = your custom task ID |
| Calls | Called for the base version **and** each extension version |
| Status | The implementation **must set** `CONSOLIDATIONTASKSTATUS` on every call |
| Doc types | A custom task can have a document type on posting level 0C/01/10/20/30, but **not 00** |
| Task log | The BAdI cannot write to the task log directly. Check whether your release's signature has a message table |
| CXA0 | Runs report `FICRVS00` (package FC00), "Mass Reversal" of group journal entries |
| `FINCS_JRNLENTR_POST_SRV` | OData service behind the Fiori app *Post Group Journal Entries*. That app has a **Reverse** action for single entries |

The BAdI's interface and method signature is **not published** outside the system
(SAP Help and KBAs on support.sap.com need an S-user login). Read it in SE18/SE19.

## 2. Options for doing the reversal

| Option | How | Verdict |
|---|---|---|
| **A. SUBMIT FICRVS00 (CXA0)** | `SUBMIT FICRVS00 WITH SELECTION-TABLE ... EXPORTING LIST TO MEMORY AND RETURN` | **Implemented.** Uses the standard reversal: number ranges, reversal flags and links |
| B. Reuse the logic behind `FINCS_JRNLENTR_POST_SRV` | Find the DPC_EXT class of the service and the class it calls for "Reverse". Call that class directly | Good if A has problems. Internal (unreleased) API, so check it after upgrades |
| C. Call the OData service from ABAP | HTTP call to your own Gateway | Not recommended: CSRF, authorisation and its own LUW |
| D. Post the inverted lines yourself | Read ACDOCU, flip signs, post with the same doc type | Last resort. You lose the "reversed" link |

### Finding the API behind `FINCS_JRNLENTR_POST_SRV` (option B)
1. `/IWFND/MAINT_SERVICE`: find `FINCS_JRNLENTR_POST_SRV` and note the technical model/service.
2. `SEGW` or `/IWBEP/REG_SERVICE`: get the **DPC_EXT** class and the function import / action used by **Reverse**.
3. SE24: open `EXECUTE_ACTION` (or `/IWBEP/IF_MGW_APPL_SRV_RUNTIME~EXECUTE_ACTION`) and follow the call down to the class or function module that does the reversal.
4. Or set an external breakpoint, press **Reverse** in the Fiori app, and read the call stack. This is the quickest way.
5. Replace `SUBMIT_MASS_REVERSAL` in `ZCL_FINCS_GJE_REVERSAL` with a call to that API. The rest of the code stays the same.

## 3. Checklist before activating
1. **SE18 `BADI_FINCS_CUST_DATAMON`**: read the interface, method and parameters, and the status values for `CONSOLIDATIONTASKSTATUS`.
2. **SE19**: create the implementation with filter `CONSOLIDATIONTASK = <task>`. Keep the generated method and put the *mapping block* from `ZCL_FINCS_CDM_REVERSE_BADI` into it.
3. **SE11 ACDOCU**: check field names (`RLDNR, RDIMEN, RVERS, RYEAR, POPER, RBUNIT, DOCTY, DOCCT, DOCNR`). Add the "already reversed / is reversal" filter in `GET_DOCUMENTS`.
4. **SE38 FICRVS00**: copy the selection-screen names into the `GC_SEL` constants. Run CXA0 once by hand with the same values, compare, and save a variant.
5. Set `GC_DOCTYPE_TO_REVERSE`, `GC_LEDGER` and `GC_DIMENSION`.
6. Handle **extension versions**: only reverse in the version that owns the document, or the second call will find nothing (harmless) or reverse twice.
7. Test with a test run first. Then run it for real and check in *Display Group Journal Entries*.

## 4. Watch-outs
- FICRVS00 does its own `COMMIT WORK`. If the monitor complains about a commit inside the BAdI, wrap the SUBMIT in a
  background job (`JOB_OPEN` / `SUBMIT ... VIA JOB` / `JOB_CLOSE`) or use option B.
- Authorisation: the user running the monitor needs the CXA0 / group journal reversal authorisation.
- Period locks: the reversal period must be open for the reversal document type.
