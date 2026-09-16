# Customer template download - what the workbook needs, and where every field lives

Research only. No program has been written yet.

Sources: `customer code templates.xlsx` (the workbook Cipla supplied), the two programs
already built and running - `ZSDS_CUST_MASS_UPLOAD` and `ZBCS_MASS_UPLOAD_EXTRACT` - and
`tools/ddic.json`, the DD03L extract taken from system CRS (577 objects, 20 753 rows).

## 1. What the workbook is

LSMW shaped, because LSMW is what it is replacing: `XD01` is gone in S/4HANA and the
customer is maintained as a Business Partner. Each sheet is a country or legal entity;
inside a sheet the templates are stacked one under the other, one per account group.

A block is: Project / Subproject / Object rows (sometimes absent), an M/O row (sometimes
absent), the technical field names, the data type, the length, the description, then
sample data. The two "sometimes absent" rows are why the heading sits on row 4 in some
sheets and row 5 in others.

| | |
|---|---|
| Sheets | Australia, Dubai, Europe, Exelan, India, Invagen, Kenya, Moroccco *(sic)*, QCIL, SAGA, plus `cust extn` and `block unblock` |
| Template blocks | 60 |
| Distinct column lists | 23 |
| Country / account-group combinations | 62 |
| Distinct field names | 172 |
| Block width | 24 to 140 columns |

`tools/cipla/parse_customer_templates.py` reads the workbook and writes
`docs/cipla/customer_template_registry.json`, which is what the program will be generated
from. A template change is then a re-run, not a hand edit.

## 2. Where the fields live

The node codes are the ones the two existing programs already use.

| Node | Target | Notes |
|---|---|---|
| K | key | KUNNR, BUKRS, VKORG, VTWEG, SPART, KTOKD |
| A | address, `BAPIAD1VL` | the workbook uses ADRC names, the API does not - see 4 |
| M | communication | ADR2 / ADR3 / ADR6 through the address node |
| C | KNA1 | general data, through `CMDS_EI_VMD_CENTRAL_DATA` |
| B | KNB1 | company code |
| S | KNVV | sales area |
| T | KNVI | tax classification, one row per tax category |
| Z | `ZSD_LICENSE_CHK` | 79 fields, key MANDT + KUNNR |
| I | BP identification (`BUT0ID`) | Aadhaar |
| U | FSCM credit (`CL_UKM_FACADE`) | credit limit, risk class |

### The 52 fields that are not customer-master fields

All 52 are accounted for.

- **50 are `ZSD_LICENSE_CHK`** - the customer-owned table that holds the drug licences,
  the bank guarantee, the routing and the GLN data. Confirmed field by field against the
  DDIC extract. `TIN`, `TAXP_TYPE`, `NINBRN` and `LEGL_NAME` are in the table at positions
  74-77 but were absent from the templates the earlier work was built against, so they are
  new to the map but not new to the table.
- **`GST_TDS` is `KNA1-GST_TDS`**, data element `J_1IGSTTDS` - standard India
  localisation, carried in the CVI interface as
  `CMDS_EI_VMD_CENTRAL_DATA-GST_TDS`. Not custom at all.
- **`AADHAAR_NO` is a BP identification number**, category `X90003`, created by Cipla.
  It is not on KNA1 and not in `ZSD_LICENSE_CHK`. `ZSDS_CUST_MASS_UPLOAD` already writes
  it and `ZBCS_MASS_UPLOAD_EXTRACT` already reads it back from `BUT0ID`.

`ZSD_LICENSE_CHK` has change logging on and no standard API, so the upload writes it
directly - read, merge, modify, never blanking a column the template does not carry. That
write was explicitly authorised. The download only reads it.

## 3. How much is already solved

Of the 172 field names in the workbook, **137 already have a target** in
`ZSDS_CUST_MASS_UPLOAD`. The 35 that do not fall into five groups:

| Count | Group | Fields |
|---|---|---|
| 13 | Standard master data, new to the map | FISKN, KATRA4, KVERM, KZTLF, PLTYP, STCD1, STCD2, STKZU, TAXKD_05, TAXKD_06, UEBTO, UNTTO, WBRSL |
| 9 | Copy from reference - an LSMW/XD01 feature with no API equivalent | REF_KUNNR, REF_BUKRS, REF_VKORG, REF_VTWEG, REF_SPART, BUKRS1, VKORG1, VTWEG1, SPART1 |
| 5 | Contact person (KNVK) | PARNR, NAME1_01, NAMEV_01, ABTNR_01, PAFKT_01 |
| 4 | LSMW control, no SAP target | TCODE, USE_ZAV, ZAV, KNA1 |
| 4 | `ZSD_LICENSE_CHK`, absent from the old templates | TIN, TAXP_TYPE, NINBRN, LEGL_NAME |

The copy-from-reference group is the same problem met on the Cimmra vendor extension and
it has the same answer: read the reference with `CMD_EI_API_EXTRACT=>GET_DATA` and carry
the values across, rather than asking the interface for a feature it does not have. On the
download side these columns simply come out empty, because a created customer has no
reference.

## 4. The workbook uses ADRC names, the API uses BAPIAD1VL names

The translation already exists in `ZSDS_CUST_MASS_UPLOAD` and carries over unchanged.

| Workbook | API |
|---|---|
| NAME1 / NAME2 / NAME3 / NAME4 | NAME / NAME_2 / NAME_3 / NAME_4 |
| NAME_CO | C_O_NAME |
| CITY1 | CITY |
| CITY2 | DISTRICT |
| POST_CODE1 | POSTL_COD1 |
| HOUSE_NUM1 | HOUSE_NO |
| TITLE_MEDI | TITLE |
| LOCATION (Street 5) | LOCATION |
| STR_SUPPL1 / 2 / 3 (Street 2 / 3 / 4) | STR_SUPPL1 / 2 / 3 |
| TEL_NUMBER / MOB_NUMBER / FAX_NUMBER / SMTP_ADDR | the communication node |
| TAXKD_01 .. TAXKD_06 | KNVI rows, by tax category (JOCG, JTC1, JTX1..JTX4, MWST, UTX2, UTX3, UTXJ) |

## 5. Faults in the workbook

1. **QCIL, third block (technical row 21).** The technical row carries an extra `KUNNR`,
   so it is shifted one column against the description and data rows - 64 of its 83
   columns disagree. The description and data rows agree with each other, so the
   description row is the correct one.
2. **Five blocks carry no technical field row** - India `ZSHM`, `ZDOD`, `ZDOF`, `ZDOC`,
   and the whole `cust extn` sheet. Their fields have to be resolved from the
   descriptions.
3. **`block unblock` has neither**, and is `XD05`, a different transaction.
4. **Description wording drifts** between copies of the same format - "Always X" against
   "aLWAYS x", "Tax Number 3" against "Tax Number 3 ( GST Number)", "20B. Lic. No"
   against "20B. Lic. No.". Harmless as long as the file is matched on the technical
   name, never on the description.

## 6. What the download program will do

Confirmed with Cipla:

- The user picks a country and an account group, then a radio button for the format. One
  combination, one file.
- Selection by Business Partner number or customer number, as the existing programs do.
- An empty template, or the template filled with the data of the numbers given.
- Every column of the workbook is in scope.

Carried over from `ZBCS_MASS_UPLOAD_EXTRACT` unchanged: the nine-part xlsx package with
shared strings and column A always written, the heading matcher, and the extract through
`CMD_EI_API_EXTRACT=>GET_DATA` alongside the `ZSD_LICENSE_CHK` and `BUT0ID` reads.

## 7. Still to settle

1. The five description-only blocks: their technical fields have to be agreed.
2. QCIL block 3: confirm the description row is the correct one.
3. `cust extn` and `block unblock`: in scope for this program, or separate?
4. Sheet-to-country mapping: the sheets are named for entities (Exelan, Invagen, QCIL,
   SAGA) as much as for countries. The selection screen needs the country key and the
   company code behind each, which the sample data gives (6700 Australia, 8300 Dubai,
   7450/7000 Europe, 5400 Exelan, 1000 India, 5200 Invagen, 4900 Kenya, 6600 Morocco,
   4500 QCIL, 4100 SAGA) but which should be confirmed rather than inferred.
