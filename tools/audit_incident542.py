"""The faults Cipla's go-live tests found (Incident 542, November 2026) stay fixed.

Each check reads the source for the shape of the fix, so taking a fix out -
or writing the old statement back - fails here. regress_past_errors.py puts
each old statement back and requires this audit to catch it.

    python3 tools/audit_incident542.py
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
V = open(os.path.join(ROOT, 'src/zmms_bp_mass_upload.prog.abap'), encoding='utf-8').read()
C = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()

fail = []


def method(src, cls, name):
    start = src.index(f'CLASS {cls} IMPLEMENTATION.')
    end = src.index('\nENDCLASS.', start)
    m = re.search(rf'\n  METHOD {re.escape(name)}\.\n(.*?)\n  ENDMETHOD\.', src[start:end], re.S)
    return m.group(1) if m else ''


# ---- supplier -------------------------------------------------------------
central = method(V, 'lcl_h_create', 'fill_central')
if not central:
    fail.append('supplier: LCL_H_CREATE has no FILL_CENTRAL any more - update this audit')
# Tax number 3 of the AU supplier was in LFA1 and not in BP: it had been
# written into the supplier, where BP does not look.
for f in ('STCD3', 'STCD5', 'STCEG'):
    if re.search(rf'\|{f};\d+;', central):
        fail.append(f'supplier: {f} is written into the supplier record (FILL_CENTRAL) - it '
                    f'reaches LFA1 only and BP shows no tax number; send it as a BP tax number')
tax = method(V, 'lcl_h_create', 'fill_tax')
if 'cs_data-partner-central_data-taxnumber-taxnumbers' not in tax:
    fail.append('supplier: FILL_TAX does not send the tax numbers to the business partner')
if 'tax_type_ok' not in tax:
    fail.append('supplier: FILL_TAX does not check that the tax number category exists')
run = method(V, 'lcl_h_create', 'lif_h~run')
if 'fill_tax(' not in run:
    fail.append('supplier: the creation tab never calls FILL_TAX')
# "Posted, but the new vendor number could not be read back" was a warning
# under a green "Posted successfully", and nobody could tell what was missing.
if re.search(r"iv_ty = 'W'\s*\n\s*iv_txt = 'Posted, but the new vendor number", V):
    fail.append('supplier: a creation that left no supplier behind is only a warning')

# ---- customer -------------------------------------------------------------
master = method(C, 'lcl_engine', 'master')
if not master:
    fail.append('customer: LCL_ENGINE has no MASTER any more - update this audit')
# Ship-to, ZREM and ZNOT: a bare company code key created KNB1 without a
# reconciliation account.
m = re.search(r'\n(.*)\n\s*APPEND ls_comp TO ls_cust-company_data-company\.', master)
if not m or 'ls_comp-datax IS NOT INITIAL' not in m.group(1):
    fail.append('customer: the company code is sent whether or not the row has company code '
                'data - ship-to rows fail on "Reconciliation acct is not filled"')
# A row with a red line in the log was still posted.
post = master.find('mo_cvis->post(')
guard = master.find('mo_log->has_error( is_row-row )')
if post < 0 or guard < 0 or guard > post:
    fail.append('customer: a row with errors is still posted')
# Unblocking changed nothing: an empty block cell meant "leave alone".
if not re.search(r'lv_unblk = abap_true AND lv_cell IS INITIAL[\s\S]{0,200}lv_cell = gc_clear', master):
    fail.append('customer: an unblocking row does not clear the blocks')
# Invagen's tax columns carry India's categories.
if not re.search(r'tax_cat_ok\([^)]*\) = abap_false\.\s*\n\s*DATA\(lv_named\)[\s\S]{0,120}tax_cat_nth\(', master):
    fail.append('customer: a tax column headed with another country\'s category is not read '
                'by position')
# The extension tab relies on its reference columns.
for k in ('REFERENCECOMPANYCODE', 'REFERENCESALESORGANIZATION'):
    if k not in master:
        fail.append(f'customer: the reference column {k} is not read - an extension gets no '
                    f'reconciliation account, sort key or Incoterms')
# The customer had the supplier's tax number fault too: STCD* and STCEG
# written into KNA1, where BP does not show them.
if not re.search(r"WHEN gc_n_cent\.\s*\n\s*IF ls_m-fld = 'STCEG'[^\n]*\n\s*APPEND[^\n]*lt_txn\.\s*\n\s*CONTINUE\.", master):
    fail.append('customer: tax numbers (STCD1-6, STCEG) are written into KNA1 instead of the business '
                'partner - BP shows no tax number')
if 'ls_bp-central_data-taxnumber-taxnumbers' not in master:
    fail.append('customer: no tax number reaches the business partner')
# ZREM / ZNOT rows name a reference customer with no company code data -
# that must not stop the row.
if re.search(r"IF ls_rknb1 IS INITIAL\.\s*\n(?:\s*\".*\n)*\s*mo_log->add\([^)]*iv_type = 'E'", master):
    fail.append('customer: a reference customer without that company code stops the row - '
                'ZREM and ZNOT rows fail although they carry no company code data')
# Excel turns 01 into 1.
setc = method(C, 'lcl_engine', 'set_comp')
if 'code_padded(' not in setc:
    fail.append('customer: a code Excel stripped of its leading zero (shipping conditions 1 for 01) '
                'reaches SAP unpadded')
if 'ref_fill(' not in master:
    fail.append('customer: nothing is copied from the reference company code and sales area')

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - tax numbers go to the business partner, a half-made supplier is an error, '
      'company codes go only with data, faulty rows are not posted, unblocking clears, '
      'foreign tax headings are read by position and reference columns are copied')
