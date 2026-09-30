"""Every error these programs have shown in the system, put back and caught.

Each case below is an error that reached the ABAP editor, a short dump or a
wrong result during testing. It re-creates that error in the current source
of both programs - the original faulty statement, or its exact twin where
the program never had that statement - runs the audit that guards against
it, and requires the audit to FAIL. The sources are restored after every
case, whatever happens.

A case that passes here means: if that mistake is made again, in either
program, the audits say so before the code reaches SAP.

    python3 tools/regress_past_errors.py
"""
import os, shutil, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
V = 'src/zmms_bp_mass_upload.prog.abap'
VX = 'src/zmms_bp_mass_upload.prog.xml'
C = 'src/zsds_cust_tmpl_download.prog.abap'

# (seen as, program, audit, [(file, old, new), ...])
CASES = [
    # ---- syntax errors on activation ---------------------------------
    ('Names may consist only of A-Z, _, 0-9 - METHODS lif_h~key_col (screenshot 6)', V,
     'audit_declarations.py',
     [(V, '    INTERFACES lif_h ABSTRACT METHODS sheet run.\n',
          '    INTERFACES lif_h ABSTRACT METHODS sheet run.\n    METHODS lif_h~key_col.\n')]),
    ('C(255) of P_FILE is not compatible with STRING of IV_FILE (screenshot 7)', V,
     'audit_param_types.py',
     [(V, 'EXPORTING iv_file    = CONV string( p_file )', 'EXPORTING iv_file    = p_file')]),
    ('C(255) of P_FILE is not compatible with STRING of IV_FILE (screenshot 7)', C,
     'audit_param_types.py',
     [(C, 'EXPORTING iv_file    = CONV string( p_file )', 'EXPORTING iv_file    = p_file')]),
    ('A SECTION specification cannot be used more than once (screenshot 11)', C,
     'audit_nesting.py',
     [(C, 'CLASS lcl_util DEFINITION FINAL.\n  PUBLIC SECTION.\n',
          'CLASS lcl_util DEFINITION FINAL.\n  PUBLIC SECTION.\n  PUBLIC SECTION.\n')]),
    ('A SECTION specification cannot be used more than once (screenshot 11)', V,
     'audit_nesting.py',
     [(V, 'CLASS lcl_util DEFINITION FINAL.\n  PUBLIC SECTION.\n',
          'CLASS lcl_util DEFINITION FINAL.\n  PUBLIC SECTION.\n  PUBLIC SECTION.\n')]),
    ('"ENDCLASS" does not have an open control structure (screenshot 11)', C,
     'audit_nesting.py',
     [(C, 'CLASS lcl_util DEFINITION FINAL.', 'ENDCLASS.\nCLASS lcl_util DEFINITION FINAL.')]),
    ('"ENDCLASS" does not have an open control structure (screenshot 11)', V,
     'audit_nesting.py',
     [(V, 'CLASS lcl_util DEFINITION FINAL.', 'ENDCLASS.\nCLASS lcl_util DEFINITION FINAL.')]),
    ('Field "GO_LOG" is unknown (screenshot 12)', V,
     'audit_event_scope.py',
     [(V, 'START-OF-SELECTION.\n', 'DATA gv_lost TYPE i.\n\nSTART-OF-SELECTION.\n  gv_lost = 1.\n')]),
    ('Field "GO_LOG" is unknown (screenshot 12)', C,
     'audit_event_scope.py',
     [(C, 'START-OF-SELECTION.\n', 'DATA gv_lost TYPE i.\n\nSTART-OF-SELECTION.\n  gv_lost = 1.\n')]),
    ('A class used before its definition has been read', V,
     'audit_class_order.py',
     [(V, "    LOOP AT lcl_map=>for( iv_scen ) INTO DATA(ls_m).",
          "    DATA(lv_early) = lcl_dl=>scenario( ).\n    LOOP AT lcl_map=>for( iv_scen ) INTO DATA(ls_m).")]),
    ('A name declared twice in one method', C,
     'audit_nesting.py',
     [(C, "    DATA(lv_regn) = screen_regn( ).\n", "    DATA(lv_regn) = screen_regn( ).\n    DATA(lv_regn) = screen_regn( ).\n")]),
    ('MESSAGE / READ TABLE given an expression instead of a data object', V,
     'audit_operand_positions.py',
     [(V, "    gt_kh = lcl_hdr=>for( gv_scen ).\n    READ TABLE gt_kh INTO",
          "    READ TABLE lcl_hdr=>for( gv_scen ) INTO")]),
    # ---- short dumps --------------------------------------------------
    ('CALL_FUNCTION_CONFLICT_GEN_TYP - a STRING to WINDOW_TITLE of F4IF_INT_TABLE_VALUE_REQUEST (screenshots 8, 9)', C,
     'audit_fm_string.py',
     [(C, '                 window_title = lv_title\n',
          '                 window_title = |Account groups|\n')]),
    ('MESSAGE_TYPE_X - Internal error, value range of ADTEL-R3_USER (screenshot 10)', V,
     'audit_mobile_flag.py',
     [(V, "CONSTANTS gc_mobile TYPE c LENGTH 1 VALUE '3'.", "CONSTANTS gc_mobile TYPE c LENGTH 1 VALUE 'X'.")]),
    ('MESSAGE_TYPE_X - Internal error, value range of ADTEL-R3_USER (screenshot 10)', C,
     'audit_mobile_flag.py',
     [(C, "CONSTANTS gc_mobile TYPE c LENGTH 1 VALUE '3'.", "CONSTANTS gc_mobile TYPE c LENGTH 1 VALUE 'X'.")]),
    ('ITAB_DUPLICATE_KEY - SELECT from T052 into a unique table', V,
     'audit_unique_keys.py',
     [(V, 'SELECT DISTINCT ekorg FROM t024e', 'SELECT ekorg FROM t024e')]),
    ('ITAB_DUPLICATE_KEY - SELECT from T052 into a unique table', C,
     'audit_unique_keys.py',
     [(C, 'SELECT DISTINCT kvgr3 FROM tvv3', 'SELECT kvgr3 FROM tvv3')]),
    ('CX_SY_REPLACE_INFINITE_LOOP - a blank quoted literal as REPLACE pattern', V,
     'audit_literals.py',
     [(V, "REPLACE ALL OCCURRENCES OF ` ` IN rv WITH `_`.", "REPLACE ALL OCCURRENCES OF ' ' IN rv WITH '_'.")]),
    # ---- wrong behaviour ----------------------------------------------
    ('".xlsx is not supported" for an .xlsx - the path cut at 128 characters', V,
     'audit_file_path.py',
     [(V, 'PARAMETERS: p_file  TYPE ty_path LOWER CASE,', 'PARAMETERS: p_file  TYPE rlgrap-filename LOWER CASE,')]),
    ('".xlsx is not supported" for an .xlsx - the path cut at 128 characters', C,
     'audit_file_path.py',
     [(C, 'PARAMETERS: p_file  TYPE ty_path LOWER CASE,', 'PARAMETERS: p_file  TYPE rlgrap-filename LOWER CASE,')]),
    ('"No data rows were found to process" - the real reason overwritten', V,
     'audit_fatal_message.py',
     [(V, "    go_log->add( iv_row = 0 iv_ty = 'E' iv_txt = 'No scenario selected.' ).\n",
          "    MESSAGE 'No scenario selected.' TYPE 'E'.\n")]),
    ('Spanish address filed under English - language cut to one letter', V,
     'audit_language_key.py',
     [(V, "CALL FUNCTION 'CONVERSION_EXIT_ISOLA_INPUT'", "CALL FUNCTION 'CONVERSION_EXIT_ISOLX_INPUT'")]),
    ('Spanish address filed under English - language cut to one letter', C,
     'audit_language_key.py',
     [(C, "( tmpl = '06551ad0' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = 'LG' )",
          "( tmpl = '06551ad0' col = 34  hdr = 'Language Key' node = 'A' fld = 'LANGU' fmt = '' cnv = '' )")]),
    ('Account group F4 using the region before the screen was read (screenshots 2-4)', C,
     'audit_screen_and_texts.py',
     [(C, "    DATA(lv_regn) = screen_regn( ).\n", "    DATA(lv_regn) = p_regn.\n")]),
    ('Red and green on one row - a green light for a line that is not an outcome', V,
     'audit_status_icons.py',
     [(V, "                        WHEN iv_ty = 'S'    THEN icon_green_light\n                        ELSE                     icon_information )",
          "                        ELSE                     icon_green_light )")]),
    ('TAN rows "sent" with no word on whether they were saved', V,
     'audit_commit_verdict.py',
     [(V, "      EXPORTING wait   = abap_true\n      IMPORTING return = ls_ret.\n    rv = xsdbool( ls_ret-type NA 'EAX' ).",
          "      EXPORTING wait   = abap_true.\n    rv = abap_true.")]),
    ('A selection text SAP cuts short - "Vendor / BP creation - all com"', VX,
     'audit_screen_and_texts.py',
     [(VX, '.Vendor / BP creation - all CC<', '.Vendor / BP creation - all company codes<')]),
]


def run(audit):
    return subprocess.run([sys.executable, os.path.join('tools', audit)], cwd=ROOT,
                          capture_output=True, text=True)


def main():
    files = {f for c in CASES for f, *_ in c[3]}
    backup = tempfile.mkdtemp()
    for f in files:
        shutil.copy(os.path.join(ROOT, f), os.path.join(backup, os.path.basename(f)))
    bad = []
    try:
        # the clean source has to pass every audit first, or a failure below
        # proves nothing
        for audit in sorted({c[2] for c in CASES}):
            r = run(audit)
            if r.returncode != 0:
                bad.append(f'{audit} fails on the clean source - fix that first:\n{r.stdout[-400:]}')
        if bad:
            raise SystemExit
        for seen, prog, audit, edits in CASES:
            try:
                for f, old, new in edits:
                    path = os.path.join(ROOT, f)
                    s = open(path, encoding='utf-8').read()
                    if s.count(old) != 1:
                        bad.append(f'{os.path.basename(prog)} | {seen}: the statement to put back '
                                   f'is not in the source any more ({s.count(old)} matches) - '
                                   f'update this case')
                        raise KeyError
                    open(path, 'w', encoding='utf-8').write(s.replace(old, new))
                r = run(audit)
                caught = r.returncode != 0
                print(f'  {"caught " if caught else "MISSED "} {os.path.basename(prog):38} {seen}')
                if not caught:
                    bad.append(f'{audit} did not catch: {seen} in {prog}')
            except KeyError:
                pass
            finally:
                for f in files:
                    shutil.copy(os.path.join(backup, os.path.basename(f)), os.path.join(ROOT, f))
    except SystemExit:
        pass
    finally:
        for f in files:
            shutil.copy(os.path.join(backup, os.path.basename(f)), os.path.join(ROOT, f))
        shutil.rmtree(backup)
    print()
    if bad:
        print('\n'.join(bad))
        sys.exit(1)
    print(f'clean - all {len(CASES)} past errors, put back into the current source, are '
          f'caught by an audit')


if __name__ == '__main__':
    main()
