# ZFI_RGGBR000_USEREXIT — BDP validation: role based → PERSK based

## Requirement

The BDP (Book of Delegated Powers) financial-power validation in the validation
exit program `ZFI_RGGBR000_USEREXIT` decided a user's posting limit from the
**roles assigned to the user** (`AGR_USERS`), and read the limit from
`ZFI_OVL_BDP` / `ZFI_OVC_BDP` using the role name as key.

It must instead be decided from the **employee subgroup (grade)** of the posting
user — field `PERSK` of table `ZPA0001` (HR Master Record: Infotype 0001,
Org. Assignment).

## Scope of change

| Routine | Company | Limit table | Called from |
|---|---|---|---|
| `FORM u102` | OVL | `ZFI_OVL_BDP` | FI validation, call-up points 1/2/3 |
| `FORM u252` | OVC | `ZFI_OVC_BDP` | FI validation, call-up points 1/2/3 |

Two new helper subroutines were added at the end of the program:

* `FORM get_user_grade` — returns the posting user's `PERSK`
* `FORM is_full_power_grade` — flags grades E4 and above (no amount limit)
* `FORM raise_bdp_limit_msg` — raises the limit-exceeded error message

No other exit routine was touched. `FORM us001` still uses `C:FI_PCS_E*` /
`FI_PCS_E*` roles; it is a separate PCS validation and is not driven by the two
BDP tables.

## Before

```abap
SELECT * INTO TABLE ist_role FROM agr_users WHERE uname = sy-uname
                                              AND ( agr_name LIKE 'FI_AP_E%' OR ... ).
SELECT * FROM zfi_ovl_bdp INTO TABLE it_bdp WHERE ( agr_name_post LIKE 'FI_AP_E%' OR ... ).
...
LOOP AT ist_role.
  IF ist_role-agr_name CS prf0 OR ... CS nprf11.
    READ TABLE it_bdp INTO wa_bdp WITH KEY agr_name_post = ist_role-agr_name.
    IF gross > wa_bdp-bdp_limit.
      IF ist_role-agr_name CS prf0 OR ... .  MESSAGE e146(zfi). ENDIF.
      ...
```

* The limit came from the role name (`..._E0_...` = 50,000, `E1` = 10,00,000,
  `E2` = 50,00,000, `E3` = 1,00,00,000).
* A user with no matching role was **not checked at all**.
* The check ran once per assigned role.

## After

```abap
PERFORM get_user_grade CHANGING lv_persk.
IF lv_persk IS INITIAL.
  MESSAGE 'You are not authorized to post - no valid HR record (ZPA0001) exists for your user id' TYPE 'E'.
ENDIF.

*     Employee subgroup E4 and above has full financial power.
PERFORM is_full_power_grade USING lv_persk CHANGING lv_full_power.

CLEAR lv_bdp_limit.
IF lv_full_power IS INITIAL.
  SELECT SINGLE bdp_limit FROM zfi_ovl_bdp INTO lv_bdp_limit
                                WHERE agr_name_post = lv_persk.
  IF sy-subrc NE 0.
    " 'BDP limit is not maintained in table ZFI_OVL_BDP for employee subgroup <PERSK>'
  ENDIF.
ENDIF.
...
IF lv_full_power IS INITIAL AND gross > lv_bdp_limit.
  PERFORM raise_bdp_limit_msg USING lv_persk lv_bdp_limit gross.
ENDIF.
```

### Grades with full financial power — `FORM is_full_power_grade`

Employee subgroup **E4 and above** (E4 … E9) approves any amount: no limit is
read and no limit is applied. Only executive grades are recognised — the first
character must be `E` — so any other employee subgroup stays subject to the
limit in the BDP table.

```abap
CONSTANTS : c_full_power TYPE zpa0001-persk VALUE 'E4'.
IF p_persk(1) = 'E' AND p_persk GE c_full_power.
  p_flag = 'X'.
ENDIF.
```

The threshold is a single constant in one subroutine shared by `u102` and
`u252`, so a future change to the BDP is a one-line change.

### Determining the user's grade — `FORM get_user_grade`

Follows the pattern already used by `FORM u254` / `FORM uf246` in this program:

1. `ZPA0001` where `PERNR = SY-UNAME`, `SPRPS = ' '`, `BEGDA <= SY-DATUM <= ENDDA`
   (the SAP user id is the CPF number for employees).
2. If not found — `ZMM_VMS_CR_NEW-CORE_USER_ID = SY-UNAME` → `CPF_NO`, then
   `ZPA0001` on that `PERNR` with the same validity conditions.
3. Otherwise `PERSK` is returned empty and the posting is **blocked**.

### Error message

A single dynamic message replaces the four grade/level specific messages
(`e146` / `e147` / `e148` / `e895` for OVL and `e146` / `e908` / `e909` / `e910`
for OVC):

```
Document amount <gross> exceeds the BDP financial power of <limit> for employee subgroup <PERSK>
```

It is raised as `MESSAGE <text> TYPE 'E'`, the same style already used elsewhere
in this program (`u254`, `u259`, `u260`), so **no new entry in message class ZFI
is required**.

### Cash / Bank flag retained

`cash_bank_flg` is **not** a limit — it selects which document-type rule set
applies (the `BP/BR/CP/CR/CC/ZP/XE/XD/XF/ZV` exclusion list versus the KM
10-lakh F-02 rule). It is therefore still derived from the user's
`C:FI_CASH*` / `C:FI_BANK*` / `FI_CASH_OFFICER*` / `FI_BANK_ASSTT*` roles, and
the `AGR_USERS` read is retained **only** for that purpose. All limit logic is
now grade driven.

The KM / 10-lakh direct-F-02 rule in `u102` is unchanged. In `u252` it stays
commented out, exactly as it was before this change.

## Configuration required before transport to production

**This is mandatory — the code change alone is not sufficient.**

`ZFI_OVL_BDP` and `ZFI_OVC_BDP` keep their existing structure
(`MANDT`, `AGR_NAME_POST`, `BDP_LIMIT`) — no DDIC change. Their **content**
must be re-maintained so that `AGR_NAME_POST` holds the **employee subgroup
(PERSK)** instead of a role name, one row per grade.

Old content (24 rows, role names):

| AGR_NAME_POST | BDP_LIMIT |
|---|---|
| `C:FI_AP_E0_OVL` | 50,000 |
| `C:FI_AP_E1_OVL` | 10,00,000 |
| `FI_AP_E0_OVL_YY` | 50,000 |
| … (AP / ASSET / GL × E0–E3, `C:` and `_YY` variants) | |

New content — one row per **limited** grade. Grades E4 and above are handled in
code and need no row:

| AGR_NAME_POST | BDP_LIMIT |
|---|---|
| `E0` | 50,000 |
| `E1` | 10,00,000 |
| `E2` | 50,00,000 |
| `E3` | 1,00,00,000 |

Because the module dimension (AP / GL / ASSET) carried the same limit for a
given level in the old table, dropping it loses nothing. The legacy role rows
may be left in place — they are simply never read any more — or deleted once
the change is confirmed in production.

`ZFI_OVC_BDP` must be maintained the same way; the OVC routine `u252` uses the
identical logic.

Note: `PERSK` is `CHAR 2` and `AGR_NAME_POST` is longer, so the value must be
maintained left-justified with no leading blanks.

## Behaviour changes to cover in UAT

1. A user **without** any FI posting role but **with** a valid `ZPA0001` record is
   now subject to the BDP limit (previously unchecked).
2. A user **without** a valid `ZPA0001` record (and no `ZMM_VMS_CR_NEW` mapping)
   is now **blocked** from posting. Batch / RFC / service users that post FI
   documents must be reviewed before go-live.
3. A grade **below E4** with no row in `ZFI_OVL_BDP` / `ZFI_OVC_BDP` blocks
   posting with an explicit "limit not maintained" message — so a configuration
   gap is visible rather than silently allowing the posting. Grades E4 and above
   need no row and are never blocked on amount.
4. Confirm with Finance which grades actually occur in `ZPA0001-PERSK` for FI
   posting users. If the population is predominantly E5 and above, the amount
   limit will in practice apply to very few users.
5. Document types listed in set `BDP_DOCTYPE_NOLIMIT` (OVL) /
   `BDP_DOCTYPE_NOLIMIT_OVC` (OVC) and transaction codes in set
   `ZJVA_NOP_TCODE` remain exempt — unchanged.

## Objects to transport

* `ZFI_RGGBR000_USEREXIT` (program)
* `ZFI_OVL_BDP`, `ZFI_OVC_BDP` (table **contents** — customizing/data transport)
