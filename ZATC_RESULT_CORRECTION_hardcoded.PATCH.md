# Cursor-drop fix for the hardcoded (self-contained) ZATC_RESULT_CORRECTION variant

There are two variants of `ZATC_RESULT_CORRECTION` in use:

1. **Engine variant** (`ZATC_RESULT_CORRECTION.abap` in this repo) — reads its
   lookup data from the custom Z-tables via:
   ```abap
   FORM zatc_process_all. SELECT * INTO TABLE it_zatc_process_all FROM zatc_process_all. ENDFORM.
   FORM zatc_process_dte. SELECT * INTO TABLE it_zatc_process_dte FROM zatc_process_dte. ENDFORM.
   FORM zatc_process1.    SELECT * INTO TABLE it_zatc_process1   FROM zatc_process1.    ENDFORM.
   ```
   This variant ALREADY contains the full cursor fix.

2. **Hardcoded variant** — same program, but `ZATC_PROCESS_DTE` is filled with
   hundreds of inline `append wa_zatc_process_dte to it_zatc_process_dte`
   entries and the `SELECT`s are commented out, so it needs no Z-tables. This
   variant is missing the cursor fix.

This file documents the EXACT edit to add the cursor fix to the **hardcoded
variant** without touching its hardcoded data.

## Root cause

In the `WHEN 'S/4HANA: SEARCH FOR DATABASE OPERATIONS'.` handler, the program
only acts when:
```abap
IF wa_final-message1 = 'DB OPERATION SELECT FOUND'
  OR wa_final-message1 = 'DB OPERATION JOIN FOUND'.
```
For `DB OPERATION OPEN CURSOR FOUND`, `FETCH NEXT CURSOR FOUND`, and
`CLOSE CURSOR FOUND` (all reported on table VBUP), the IF is false and — with
no `ELSE` — the flagged line is never appended to `repos_tab_new`. It is
silently dropped, producing:
- `Field list without INTO clause is not allowed` (OPEN CURSOR dropped)
- `The statement "APPENDING" is not expected`     (FETCH NEXT CURSOR dropped)
- `Incorrect nesting …`                            (CLOSE CURSOR dropped)

## The fix (essential — 4 added lines)

In the DATABASE OPERATIONS handler, locate this block (it sits immediately
before `WHEN 'S/4HANA: FIELD LENGTH EXTENSIONS'.`):

### FIND
```abap
                ELSE.
                  APPEND wa_repos_tab TO repos_tab_new.
                ENDIF.
              ENDIF.
              REFRESH : it_query,it_query_new.
            WHEN 'S/4HANA: FIELD LENGTH EXTENSIONS'.
```

### REPLACE WITH
```abap
                ELSE.
                  APPEND wa_repos_tab TO repos_tab_new.
                ENDIF.
              ELSE.
                " Non-SELECT/JOIN DB operations (OPEN CURSOR / FETCH NEXT CURSOR /
                " CLOSE CURSOR / UPDATE / INSERT / MODIFY / DELETE) are not auto-
                " converted here - keep the original line instead of dropping it.
                APPEND wa_repos_tab TO repos_tab_new.
              ENDIF.
              REFRESH : it_query,it_query_new.
            WHEN 'S/4HANA: FIELD LENGTH EXTENSIONS'.
```

The only functional change is the added:
```abap
              ELSE.
                APPEND wa_repos_tab TO repos_tab_new.
```

After applying: Activate (Ctrl+F3) and re-run. The two `vbup`
`OPEN CURSOR … FETCH … CLOSE CURSOR` blocks are kept intact and the five
syntax errors (lines 544/549/735/746/755) disappear.

## Optional hardening (defense in depth)

The engine variant also adds a global guard so cursor lines can never be
touched by ANY handler, regardless of which ATC check fires:

1. Just before `CASE wa_final-check_title.`:
   ```abap
   DATA l_is_cursor_line TYPE abap_bool.
   PERFORM check_cursor_line USING l_tabix CHANGING l_is_cursor_line.
   IF l_is_cursor_line = abap_true.
     APPEND wa_repos_tab TO repos_tab_new.
   ELSE.
   ```
2. After the matching `ENDCASE.` add `ENDIF. "l_is_cursor_line guard`.
3. Add the FORM (place it just before `FORM change_table`):
   ```abap
   FORM check_cursor_line USING    p_tabix     TYPE sy-tabix
                          CHANGING p_is_cursor TYPE abap_bool.
     DATA l_scan TYPE sy-tabix.
     DATA l_ln   TYPE string.
     DATA wa_c   TYPE abaptxt255.
     CLEAR p_is_cursor.
     READ TABLE repos_tab INTO wa_c INDEX p_tabix.
     IF sy-subrc <> 0. RETURN. ENDIF.
     IF wa_c-line CS 'OPEN CURSOR'
       OR wa_c-line CS 'CLOSE CURSOR'
       OR ( wa_c-line CS 'FETCH' AND wa_c-line CS 'CURSOR' ).
       p_is_cursor = abap_true.
       RETURN.
     ENDIF.
     l_scan = p_tabix.
     DO 25 TIMES.
       READ TABLE repos_tab INTO wa_c INDEX l_scan.
       IF sy-subrc <> 0. EXIT. ENDIF.
       IF wa_c-line CS 'OPEN CURSOR'.
         p_is_cursor = abap_true. EXIT.
       ENDIF.
       l_ln = wa_c-line.
       IF l_ln CS '"'. l_ln = l_ln(sy-fdpos). ENDIF.
       CONDENSE l_ln.
       IF l_scan < p_tabix AND l_ln CS '.'. EXIT. ENDIF.
       l_scan = l_scan - 1.
     ENDDO.
   ENDFORM.
   ```

The 4-line ELSE alone resolves all five errors; the global guard is extra
safety. The full, ready-to-use engine variant (with both) is
`ZATC_RESULT_CORRECTION.abap` in this repo.
