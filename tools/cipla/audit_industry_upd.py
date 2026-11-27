"""ZMMS_BP_INDUSTRY_UPD keeps what Cipla's go-live taught (November 2026).

    python3 tools/cipla/audit_industry_upd.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
S = open(os.path.join(ROOT, 'src/zmms_bp_industry_upd.prog.abap'), encoding='utf-8').read()
code = '\n'.join(l for l in S.splitlines() if not l.lstrip().startswith(('*', '"')))
fail = []

# No standard table is written directly.
if re.search(r'^\s*(INSERT|UPDATE|MODIFY|DELETE)\s+(but0is|lfa1|kna1|but000)\b', code, re.I | re.M):
    fail.append('a standard table is written directly - the industry must go through CL_MD_BP_MAINTAIN')
# Validation leftovers discarded before the real posting (HONC 20.11.2026, ZMMS 27.11.2026).
if not re.search(r'validate_single\([\s\S]*?ENDTRY\.\s*\n(\s*"[^\n]*\n)*\s*discard\( \)\.', code):
    fail.append('what VALIDATE_SINGLE left in the buffers is not discarded before MAINTAIN')
if re.search(r'^\s*ROLLBACK WORK', code, re.M):
    fail.append('bare ROLLBACK WORK - the partner buffers stay; use BAPI_TRANSACTION_ROLLBACK')
if not re.search(r"CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'", code):
    fail.append('DISCARD does not call BAPI_TRANSACTION_ROLLBACK')
# The commit is waited for and its RETURN read.
if not re.search(r"CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'\s*\n\s*EXPORTING wait\s*= abap_true\s*\n\s*IMPORTING return = ls_cret\.", code):
    fail.append('BAPI_TRANSACTION_COMMIT without WAIT / without reading its RETURN')
if "ls_cret-type CA 'EAX'" not in code:
    fail.append('the RETURN of the commit is not checked')
# The supplier header goes with the partner: CVI synchronises in the call.
if 'ls_data-vendor-header-object_task           = gc_upd.' not in code:
    fail.append('the supplier header is not sent with the partner - the synchronisation runs at '
                'COMMIT WORK and its refusal is not reported (CX_FSBP_PARTNER_INCONSISTENT, HONC)')
# Rows of TB038A without an industry system are not industries (CRS, BP 100154602).
for m in re.finditer(r'SELECT [^.]*?FROM tb038a\b([^.]*?)INTO', code):
    if not re.search(r'WHERE[^.]*istype (<> @space|= @iv_istype)', m.group(1)):
        fail.append('TB038A read without ignoring rows that have no industry system')
# Excel drops leading zeros: "0008" arrives as 8.
if 'CONV i( lv_s ) = CONV i( iv_in )' not in code:
    fail.append('an industry key Excel shortened (8 for 0008) is not matched to the full key')
# The new industry takes over the standard flag.
if not re.search(r'ls_new-data-ind_default\s*= lv_make_std\.\s*\n\s*ls_new-datax-ind_default\s*= abap_true\.', code):
    fail.append('the new industry does not take over the standard flag - the supplier keeps industry 92')
# The result is read back from the partner after the save.
if not re.search(r'DATA\(lt_after\) = industries\( ls_out-partner \)\.', code):
    fail.append('the industries are not read back after the save')
# The test run changes nothing.
if 'i_test_run = p_test' not in code:
    fail.append('MAINTAIN is not given the test-run flag')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - industry update: API only, validation discarded, commit waited for and read, supplier '
      'header sent, blank industry systems ignored, shortened keys matched, read back')
