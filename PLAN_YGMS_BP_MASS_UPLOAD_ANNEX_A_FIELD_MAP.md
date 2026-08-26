# Annex A — Field-level mapping matrix (all 11 templates)

Generated from the workbooks in `revendorandcustomermassuploadtempla.zip`. Companion to `PLAN_YGMS_BP_MASS_UPLOAD.md`.

**Class key**

| Class | Meaning |
|---|---|
| **A** | Maps to `CL_MD_BP_MAINTAIN=>MAINTAIN( i_data TYPE cvis_ei_extern )` |
| **B** | Maps to a *different* API — `BAPI_BANK_CREATE` / `BAPI_BANK_CHANGE`. Bank keys are not BP objects |
| **C** | **No standard API** — `J_1IMOVEND` (CIN). Direct table update or BDC on `BP` only |
| **D** | Dead LSMW/dynpro artifact — read and ignored |
| **E** | **Unmapped** — needs functional clarification before it can be specified |

Paths under class A are relative to `CVIS_EI_EXTERN`; `COMPANY[]` = `VENDOR-COMPANY_DATA-COMPANY`, `PURCHASING[]` = `VENDOR-PURCHASING_DATA-PURCHASING`, `ADDRESS` = `PARTNER-CENTRAL_DATA-ADDRESS-ADDRESSES[]-DATA`.


## 1 Vendor/BP Create — `Vendor Creation Template_All CC_Sample.xlsx` › `Sheet1` (65 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key; blank on create |
| 3 | `BUKRS` | **A** | `VENDOR-COMPANY_DATA-COMPANY-DATA_KEY-BUKRS` | org key |
| 4 | `EKORG` | **A** | `VENDOR-PURCHASING_DATA-PURCHASING-DATA_KEY-EKORG` | org key |
| 5 | `KTOKK` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-KTOKK` |  |
| 6 | `TITLE_MEDI` | **A** | `PARTNER-CENTRAL_DATA-COMMON-DATA-BP_CENTRALDATA-TITLE_KEY` | CONV: template holds text "Company", API needs TITLE key |
| 7 | `NAME1` | **A** | `PARTNER-...-BP_ORGANIZATION-NAME1` |  |
| 8 | `NAME2` | **A** | `PARTNER-...-BP_ORGANIZATION-NAME2` |  |
| 9 | `NAME3` | **A** | `PARTNER-...-BP_ORGANIZATION-NAME3` |  |
| 10 | `NAME4` | **A** | `PARTNER-...-BP_ORGANIZATION-NAME4` |  |
| 11 | `SORT1` | **A** | `PARTNER-...-BP_CENTRALDATA-SEARCHTERM1` |  |
| 12 | `SORT2` | **A** | `PARTNER-...-BP_CENTRALDATA-SEARCHTERM2` |  |
| 13 | `STR_SUPPL1` | **A** | `ADDRESS-POSTAL-DATA-STR_SUPPL1` |  |
| 14 | `STR_SUPPL2` | **A** | `ADDRESS-POSTAL-DATA-STR_SUPPL2` |  |
| 15 | `STREET` | **A** | `ADDRESS-POSTAL-DATA-STREET` |  |
| 16 | `STR_SUPPL3` | **A** | `ADDRESS-POSTAL-DATA-STR_SUPPL3` |  |
| 17 | `CITY2` | **A** | `ADDRESS-POSTAL-DATA-DISTRICT` |  |
| 18 | `POST_CODE1` | **A** | `ADDRESS-POSTAL-DATA-POSTL_COD1` |  |
| 19 | `CITY1` | **A** | `ADDRESS-POSTAL-DATA-CITY` |  |
| 20 | `COUNTRY` | **A** | `ADDRESS-POSTAL-DATA-COUNTRY` |  |
| 21 | `REGION` | **A** | `ADDRESS-POSTAL-DATA-REGION` |  |
| 22 | `LANGU` | **A** | `ADDRESS-POSTAL-DATA-LANGU` |  |
| 23 | `TEL_NUMBER` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[]-TELEPHONE` |  |
| 24 | `TEL_EXTENS` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[]-EXTENSION` |  |
| 25 | `TEL_NUMBER2` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[]-TELEPHONE (2nd)` |  |
| 26 | `TEL_EXTENS2` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[]-EXTENSION (2nd)` |  |
| 27 | `MOB_NUMBER` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[] R_3_USER=3` |  |
| 28 | `MOB_NUMBER2` | **A** | `ADDRESS-COMMUNICATION-PHONE-PHONE[] R_3_USER=3 (2nd)` |  |
| 29 | `FAX_NUMBER` | **A** | `ADDRESS-COMMUNICATION-FAX-FAX[]-FAX` |  |
| 30 | `SMTP_ADDR` | **A** | `ADDRESS-COMMUNICATION-SMTP-SMTP[]-E_MAIL` |  |
| 31 | `SMTP_ADDR2` | **A** | `ADDRESS-COMMUNICATION-SMTP-SMTP[]-E_MAIL (2nd)` |  |
| 32 | `KUNNR` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-KUNNR` |  |
| 33 | `VBUND` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-VBUND` |  |
| 34 | `KONZS` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-KONZS` |  |
| 35 | `STCD3` | **A** | `VENDOR-...-CENTRAL-DATA-STCD3 + PARTNER TAXNUMBERS[] IN3` | dual write; IN3=GSTIN |
| 36 | `STCD5` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-STCD5` |  |
| 37 | `STCEG` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-STCEG` |  |
| 38 | `J_1KFTBUS` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-J_1KFTBUS` | Argentina field; likely unused |
| 39 | `STENR` | **A** | `VENDOR-CENTRAL_DATA-CENTRAL-DATA-STENR` | likely unused |
| 40 | `BRSCH` | **A** | `VENDOR-...-CENTRAL-DATA-BRSCH + PARTNER INDUSTRYSECTOR` |  |
| 41 | `BANKS_01` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKS` |  |
| 42 | `BANKL_01` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKL` | pre-check BNKA |
| 43 | `BANKN_01` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKN` |  |
| 44 | `KOINH_01` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-KOINH` |  |
| 45 | `BKONT` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BKONT` |  |
| 46 | `IBAN` | **A** | `VENDOR-CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-IBAN` |  |
| 47 | `AKONT` | **A** | `COMPANY[]-DATA-AKONT` |  |
| 48 | `FDGRV` | **A** | `COMPANY[]-DATA-FDGRV` |  |
| 49 | `ALTKN` | **A** | `COMPANY[]-DATA-ALTKN` |  |
| 50 | `ZTERM` | **A** | `COMPANY[]-DATA-ZTERM` | CC level |
| 51 | `REPRF` | **A** | `COMPANY[]-DATA-REPRF` |  |
| 52 | `ZWELS` | **A** | `COMPANY[]-DATA-ZWELS` |  |
| 53 | `ZAHLS` | **A** | `COMPANY[]-DATA-ZAHLS` |  |
| 54 | `HBKID` | **A** | `COMPANY[]-DATA-HBKID` |  |
| 55 | `VEN_CLASS` | **C** | `J_1IMOVEND-VEN_CLASS` | NO standard API - see Q4 |
| 56 | `J_1ISSIST` | **C** | `J_1IMOVEND-J_1ISSIST` | NO standard API - see Q4 |
| 57 | `J_1IPANNO` | **C** | `J_1IMOVEND-J_1IPANNO or BP TAXNUMBERS[]` | category unconfirmed - see Q3 |
| 58 | `QLAND` | **A** | `COMPANY[]-DATA-QLAND` |  |
| 59 | `WITHT` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 60 | `WT_WITHCD` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 61 | `WAERS` | **A** | `PURCHASING[]-DATA-WAERS` |  |
| 62 | `ZTERM` | **A** | `PURCHASING[]-DATA-ZTERM` | POrg level - same tech name as col 50 |
| 63 | `KALSK` | **A** | `PURCHASING[]-DATA-KALSK` |  |
| 64 | `WEBRE` | **A** | `PURCHASING[]-DATA-WEBRE` |  |
| 65 | `INCO1` | **A** | `PURCHASING[]-DATA-INCO1` |  |
| 66 | `INCO2` | **A** | `PURCHASING[]-DATA-INCO2` |  |

## 2 Vendor Extend — `Vendor Extend Template.Ver1.xlsx` › `Vendor Extend` (13 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | target |
| 3 | `BUKRS` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | target |
| 4 | `EKORG` | **A** | `PURCHASING[]-DATA_KEY-EKORG` | target |
| 5 | `LIFNR` | **A** | `(read source)` | reference vendor - drives a read, not a write |
| 6 | `BUKRS` | **A** | `(read source)` | reference CC |
| 7 | `EKORG` | **A** | `(read source)` | reference POrg |
| 8 | `<no tech name>` | **D** | `-` | "Always X" - LSMW artifact, ignored |
| 9 | `AKONT` | **A** | `COMPANY[]-DATA-AKONT` |  |
| 10 | `<no tech name>` | **A** | `COMPANY[]-DATA-ZWELS` | tech name MISSING in template; inferred from description "Payment Method" |
| 11 | `<no tech name>` | **A** | `COMPANY[]-DATA-REPRF` | tech name MISSING in template; inferred from "Chk double Invoice" |
| 12 | `WAERS` | **A** | `PURCHASING[]-DATA-WAERS` |  |
| 13 | `KALSK` | **A** | `PURCHASING[]-DATA-KALSK` |  |
| 14 | `WEBRE` | **A** | `PURCHASING[]-DATA-WEBRE` |  |

## 4 Vendor Bank Details — `Bank Details and Bank Key Template_V1.0.xlsx` › `Bank Details` (7 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 3 | `BUKRS` | **A** | `(authorisation check only)` | LFBK is client-level |
| 4 | `BANKS` | **A** | `CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKS` |  |
| 5 | `BANKL` | **A** | `CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKL` |  |
| 6 | `BANKN` | **A** | `CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-BANKN` |  |
| 7 | `KOINH` | **A** | `CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-KOINH` |  |
| 8 | `IBAN` | **A** | `CENTRAL_DATA-BANKDETAIL-BANKDETAILS[]-IBAN` |  |

## 3 Bank Key (BNKA) — `Bank Details and Bank Key Template_V1.0.xlsx` › `Bank Key` (8 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `BANKS` | **B** | `BAPI_BANK_CREATE BANK_CTRY` |  |
| 3 | `BANKL` | **B** | `BAPI_BANK_CREATE BANK_KEY` |  |
| 4 | `BANKA` | **B** | `BANK_ADDRESS-BANK_NAME` |  |
| 5 | `PROVZ` | **B** | `BANK_ADDRESS-REGION` |  |
| 6 | `STRAS` | **B** | `BANK_ADDRESS-STREET` |  |
| 7 | `ORT01` | **B** | `BANK_ADDRESS-CITY` |  |
| 8 | `BRNCH` | **B** | `BANK_ADDRESS-BANK_BRANCH` |  |
| 9 | `SWIFT` | **B** | `BANK_ADDRESS-SWIFT_CODE` |  |

## 3 Bank Key (BNKA) — `Bank key format_Uploadable File.xlsx` › `Sheet1` (9 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `BANKS` | **B** | `BAPI_BANK_CREATE BANK_CTRY` |  |
| 3 | `BANKL` | **B** | `BAPI_BANK_CREATE BANK_KEY` |  |
| 4 | `BANKA` | **B** | `BANK_ADDRESS-BANK_NAME` |  |
| 5 | `PROVZ` | **B** | `BANK_ADDRESS-REGION` |  |
| 6 | `STRAS` | **B** | `BANK_ADDRESS-STREET` |  |
| 7 | `ORT01` | **B** | `BANK_ADDRESS-CITY` |  |
| 8 | `BRNCH` | **B** | `BANK_ADDRESS-BANK_BRANCH` |  |
| 9 | `SWIFT` | **B** | `BANK_ADDRESS-SWIFT_CODE` |  |
| 10 | `BNKLZ` | **B** | `BANK_ADDRESS-BANK_NO` | template says keep blank |

## 5 Withholding Tax / TDS — `TDS for upload Format_V.01.xlsx` › `Sheet1` (64 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 3 | `BUKRS` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 4 | `D0610` | **D** | `-` | XK02 screen flag - ignored |
| 5 | `QLAND` | **A** | `COMPANY[]-DATA-QLAND` |  |
| 6 | `WITHT_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 7 | `WITHT_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 8 | `WITHT_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 9 | `WITHT_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 10 | `WITHT_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 11 | `WITHT_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 12 | `WT_WITHCD_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 13 | `WT_WITHCD_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 14 | `WT_WITHCD_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 15 | `WT_WITHCD_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 16 | `WT_WITHCD_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 17 | `WT_WITHCD_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 18 | `WT_SUBJCT_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 19 | `WT_SUBJCT_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 20 | `WT_SUBJCT_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 21 | `WT_SUBJCT_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 22 | `WT_SUBJCT_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 23 | `WT_SUBJCT_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_SUBJCT` |  |
| 24 | `QSREC_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 25 | `QSREC_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 26 | `QSREC_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 27 | `QSREC_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 28 | `QSREC_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 29 | `QSREC_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-QSREC` |  |
| 30 | `WT_WTSTCD_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 31 | `WT_WTSTCD_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 32 | `WT_WTSTCD_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 33 | `WT_WTSTCD_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 34 | `WT_WTSTCD_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 35 | `WT_WTSTCD_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTSTCD` |  |
| 36 | `WT_EXNR_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 37 | `WT_EXNR_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 38 | `WT_EXNR_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 39 | `WT_EXNR_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 40 | `WT_EXNR_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 41 | `WT_EXNR_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 42 | `WT_EXRT_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 43 | `WT_EXRT_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 44 | `WT_EXRT_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 45 | `WT_EXRT_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 46 | `WT_EXRT_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 47 | `WT_EXRT_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 48 | `WT_WTEXRS_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 49 | `WT_WTEXRS_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 50 | `WT_WTEXRS_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 51 | `WT_WTEXRS_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 52 | `WT_WTEXRS_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 53 | `WT_WTEXRS_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WTEXRS` |  |
| 54 | `WT_EXDF_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 55 | `WT_EXDF_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 56 | `WT_EXDF_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 57 | `WT_EXDF_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 58 | `WT_EXDF_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 59 | `WT_EXDF_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` | DD.MM.YYYY -> YYYYMMDD |
| 60 | `WT_EXDT_01` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |
| 61 | `WT_EXDT_02` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |
| 62 | `WT_EXDT_03` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |
| 63 | `WT_EXDT_04` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |
| 64 | `WT_EXDT_05` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |
| 65 | `WT_EXDT_06` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` | DD.MM.YYYY -> YYYYMMDD |

## 6 TDS Exemption / TAN — `TAN details update.xlsx` › `CIN TAN EXEMPTION` (22 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `Company` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 3 | `Address` | **E** | `?` | EMPTY in every data row - purpose unknown |
| 4 | `Section_code_1` | **E** | `?` | no LFBW field; sample value = company code -> Business Place? See Q6 |
| 5 | `Section_code_2` | **E** | `?` | as above |
| 6 | `Certificate_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 7 | `Certificate_2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXNR` |  |
| 8 | `Exemption_rate_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 9 | `Exemption_rate_2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXRT` |  |
| 10 | `Validfrom_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` |  |
| 11 | `Validfrom2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDF` |  |
| 12 | `Validto_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` |  |
| 13 | `Validto_2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_EXDT` |  |
| 14 | `taxtype_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 15 | `Taxtype_2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA_KEY-WITHT` |  |
| 16 | `taxcode_1` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 17 | `Taxcode_2` | **A** | `COMPANY[]-WTAX_TYPE[]-DATA-WT_WITHCD` |  |
| 18 | `threshold_1` | **E** | `?` | no LFBW field - TDS threshold is config, not master data. See Q6 |
| 19 | `threshold_2` | **E** | `?` | as above |
| 20 | `Currency_1` | **E** | `?` | no LFBW field. See Q6 |
| 21 | `Currency_2` | **E** | `?` | as above |
| 22 | `<no header>` | **E** | `?` | unheaded column, value 0 in 2 rows - purpose unknown |

## 7 CIN / PAN / MSME — `CIN_Email_MSME upload.xlsx` › `CIN MSME` (15 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor Account Number` | **C** | `J_1IMOVEND-LIFNR` | key |
| 2 | `Company Code` | **C** | `(auth check)` |  |
| 3 | `Address View` | **D** | `-` | screen view flag - ignored |
| 4 | `ECC Number` | **C** | `J_1IMOVEND-J_1IEXCD` | NO standard API; pre-GST field |
| 5 | `Excise Registration Number` | **C** | `J_1IMOVEND-J_1IEXRN` | NO standard API; pre-GST |
| 6 | `Excise Range` | **C** | `J_1IMOVEND-J_1IEXRG` | NO standard API; pre-GST |
| 7 | `Excise Division` | **C** | `J_1IMOVEND-J_1IEXDI` | NO standard API; pre-GST |
| 8 | `Excise Commissionerate` | **C** | `J_1IMOVEND-J_1ICOMM` | NO standard API; pre-GST |
| 9 | `Central Sales Tax Number` | **C** | `J_1IMOVEND-J_1ICSTNO` | NO standard API; pre-GST |
| 10 | `Local Sales Tax Number` | **C** | `J_1IMOVEND-J_1ILSTNO` | NO standard API; pre-GST |
| 11 | `Service Tax Registration Number` | **C** | `J_1IMOVEND-J_1ISERN` | NO standard API; pre-GST |
| 12 | `Permanent Account Number` | **C** | `J_1IMOVEND-J_1IPANNO or BP TAXNUMBERS[]` | see Q3 |
| 13 | `SSI status` | **C** | `J_1IMOVEND-J_1ISSIST` | NO standard API |
| 14 | `Exc.Tax Ind. Vendor` | **C** | `J_1IMOVEND-J_1IEXCIVE` | NO standard API |
| 15 | `Type of Vendor` | **C** | `J_1IMOVEND-J_1IVTYP` | NO standard API |

## 7 CIN / PAN / MSME — `CIN_Email_MSME upload.xlsx` › `PAN Update` (2 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `Sap V Code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 3 | `PAN No.` | **C** | `J_1IMOVEND-J_1IPANNO or BP TAXNUMBERS[]` | see Q3 |

## 7 CIN / PAN / MSME — `CIN_Email_MSME upload.xlsx` › `Email` (8 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `<no header>` | **D** | `-` | transaction code XK02 - ignored |
| 2 | `<no header>` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key (vendor number) |
| 3 | `<no header>` | **D** | `-` | screen view flag X |
| 4 | `<no header>` | **D** | `-` | screen view flag X |
| 5 | `<no header>` | **D** | `-` | screen view flag X |
| 6 | `<no header>` | **D** | `-` | screen view flag X |
| 7 | `<no header>` | **A** | `ADDRESS-COMMUNICATION-SMTP-SMTP[]-E_MAIL` |  |
| 8 | `<no header>` | **A** | `ADDRESS-COMMUNICATION-SMTP-SMTP[]-E_MAIL (2nd)` |  |

## 8 Payment Term / Method — `Payment Term and Method update_V.01.xlsx` › `Payment Term` (4 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor Code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `Company Code` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 3 | `Pur Org` | **A** | `PURCHASING[]-DATA_KEY-EKORG` | key |
| 4 | `Payment Term` | **A** | `COMPANY[]-DATA-ZTERM + PURCHASING[]-DATA-ZTERM` | written at both org levels |

## 8 Payment Term / Method — `Payment Term and Method update_V.01.xlsx` › `Payment Method` (3 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor Code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `Company Code` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 3 | `Payment Method` | **A** | `COMPANY[]-DATA-ZWELS` |  |

## 9 Purchasing Org Data — `Purchase Org Data update.xlsx` › `Sheet1` (4 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 2 | `Vendor code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 3 | `Pur Org` | **A** | `PURCHASING[]-DATA_KEY-EKORG` | key |
| 4 | `CONFIRMATION CONTROL` | **A** | `PURCHASING[]-DATA-BSTAE` | tech name ASSUMED - confirm on LFM1, Q7 |
| 5 | `CHECK ACKNOWLEDGEMENT` | **A** | `PURCHASING[]-DATA-KZABS` | tech name ASSUMED - confirm on LFM1, Q7 |

## 10 Partner Functions — `Vendor Partner Function Template.xlsx` › `Sheet1` (35 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `BUKRS` | **A** | `(auth check)` |  |
| 3 | `EKORG` | **A** | `PURCHASING[]-DATA_KEY-EKORG` | key |
| 4 | `D0320` | **D** | `-` | XK01/XK02 screen flag - ignored |
| 5 | `USE_ZAV` | **D** | `-` | LSMW artifact "Always X" - ignored |
| 6 | `PARVW_05` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 7 | `PARVW_06` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 8 | `PARVW_07` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 9 | `PARVW_08` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 10 | `PARVW_09` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 11 | `PARVW_10` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 12 | `PARVW_11` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 13 | `PARVW_12` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 14 | `PARVW_13` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 15 | `PARVW_14` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 16 | `PARVW_15` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 17 | `GPARN_05` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 18 | `GPARN_06` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 19 | `GPARN_07` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 20 | `GPARN_08` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 21 | `GPARN_09` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 22 | `GPARN_10` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 23 | `GPARN_11` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 24 | `GPARN_12` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 25 | `GPARN_13` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 26 | `GPARN_14` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 27 | `GPARN_15` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 28 | `PARVW_01` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 29 | `PARVW_02` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 30 | `PARVW_03` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 31 | `PARVW_04` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA_KEY-PARVW` | note: file orders 05-15 BEFORE 01-04 |
| 32 | `GPARN_01` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 33 | `GPARN_02` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 34 | `GPARN_03` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |
| 35 | `GPARN_04` | **A** | `PURCHASING[]-FUNCTIONS[]-DATA-LIFN2` | note: file orders 05-15 BEFORE 01-04 |

## 11 Block / Unblock — `Vendor Block_Unblocked.xlsx` › `Total Block` (9 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `<tcode>` | **D** | `-` | "XK05" - LSMW artifact, ignored |
| 2 | `LIFNR` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 3 | `BUKRS` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 4 | `EKORG` | **A** | `PURCHASING[]-DATA_KEY-EKORG` | key |
| 5 | `SPERR` | **A** | `CENTRAL_DATA-CENTRAL-DATA-SPERR` | central posting block |
| 6 | `SPERR_1` | **A** | `COMPANY[]-DATA-SPERR` | CC posting block |
| 7 | `SPERM` | **A** | `CENTRAL_DATA-CENTRAL-DATA-SPERM` | central purchasing block |
| 8 | `SPERM_1` | **A** | `PURCHASING[]-DATA-SPERM` | POrg purchasing block |
| 9 | `SPERQ` | **A** | `CENTRAL_DATA-CENTRAL-DATA-SPERQ` |  |

## 11 Block / Unblock — `Vendor Block_Unblocked.xlsx` › `Payment block` (3 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `Comp code` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 3 | `Block / unblock` | **A** | `COMPANY[]-DATA-ZAHLS` | UNBLOCK not expressible without template change - Q8 |

## 11 Block / Unblock — `Vendor Block_Unblocked.xlsx` › `Deletion Flag` (3 data columns)

| Col | Template field | Class | Target | Note |
|---:|---|:---:|---|---|
| 1 | `Vendor code` | **A** | `VENDOR-HEADER-OBJECT_INSTANCE-LIFNR` | key |
| 2 | `Comp code` | **A** | `COMPANY[]-DATA_KEY-BUKRS` | key |
| 3 | `Block / unblock` | **A** | `COMPANY[]-DATA-LOEVM` | UNBLOCK not expressible without template change - Q8 |

---

## Totals

| Class | Meaning | Columns | Share |
|:---:|---|---:|---:|
| **A** | CL_MD_BP_MAINTAIN | 220 | 80.3% |
| **B** | BAPI_BANK_* | 17 | 6.2% |
| **C** | no standard API | 18 | 6.6% |
| **D** | ignored (artifact) | 11 | 4.0% |
| **E** | UNMAPPED | 8 | 2.9% |
| | **Total data columns** | **274** | |

**Answer to "does everything map to the class?" — no.** 220 of 274 data columns (80%) map to `CL_MD_BP_MAINTAIN`. 17 go to `BAPI_BANK_*`, 18 have no standard API, 11 are dead artifacts, and 8 are genuinely unmapped pending functional input.
