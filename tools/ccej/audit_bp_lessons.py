"""The CCBJI customer and supplier programs keep what Cipla's go-live taught.

Both programs are generated from tools/ccej/bp_maint/*.abap. In November 2026
the same kind of upload went live at Cipla, and each of these faults reached
production there before it was found. This audit reads the CCBJI source for
the shape of each fix, so the two programs cannot fall back:

  - bank details belong to the business partner (BUT0BK, by bank details ID);
    sent on the customer / supplier they are ignored - CVI_EI 004 "Bank data
    is provided from the business partner";
  - a new record's telephone, mobile, fax and e-mail go on the partner's
    address, or they are dropped;
  - tax numbers go to the business partner under their category, where the
    country has one;
  - what the validation leaves in the partner's buffers is discarded before
    posting, through BAPI_TRANSACTION_ROLLBACK (a bare ROLLBACK WORK does not
    refresh them) - else a later COMMIT dumps with CX_FSBP_PARTNER_INCONSISTENT;
  - a creation is checked afterwards: with postprocessing active SAP saves the
    partner and parks the customer / supplier error in MDS_PPO2.

    python3 tools/ccej/audit_bp_lessons.py
"""
import glob, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
RAW = '\n'.join(open(f, encoding='utf-8').read()
                for f in sorted(glob.glob(os.path.join(ROOT, 'tools/ccej/bp_maint/*.abap'))))
SRC = '\n'.join(re.sub(r'(?<![`|\'])\s"\s.*$', '', l) for l in RAW.split('\n')
                if not l.lstrip().startswith('*') and not l.lstrip().startswith('"'))

fail = []


def need(what, pat, flags=re.S):
    if not re.search(pat, SRC, flags):
        fail.append(what)


if re.search(r"WHEN 'BANK'\. lv_path = `CENTRAL_DATA-BANKDETAIL-BANKDETAILS`", SRC):
    pass  # LINE_OF still knows the path; what matters is that BUILD never takes it for BANK
need('bank details are not put on the business partner (PARTNER-CENTRAL_DATA-BANKDETAIL) - on the '
     'customer / supplier CVI ignores them',
     r"METHOD bank_line\..*?`PARTNER-CENTRAL_DATA-BANKDETAIL-BANKDETAILS`.*?ENDMETHOD\.")
need('a bank entry of the file still goes through LINE_OF (the customer / supplier side) in BUILD',
     r"IF ls_e-grp = 'BANK'\..*?CONTINUE\.\s*\n\s*ENDIF\.\s*\n\s*DATA\(lr_l\) = line_of\(")
need('a changed bank field is not written to the partner\'s bank line',
     r"WHEN 'BANK'\.\s*\n.*?bank_line\( ir_ext = lr_ext is_ent = ls_bke \)")
need('a new account does not get the next free bank details ID of the partner',
     r"lv_bkvid = \|\{ mv_bknext WIDTH = 4 ALIGN = RIGHT PAD = '0' \}\|\.")
need('bank details that are no longer valid are counted as the partner\'s accounts',
     r"METHOD bp_banks\..*?bk_valid_to\s+< lv_now.*?CONTINUE\.")
need('a new record\'s telephone / mobile is not put on the business partner\'s address',
     r"APPEND ls_nph TO ls_adr-data-communication-phone-phone\.")
need('a new record\'s e-mail is not put on the business partner\'s address',
     r"APPEND ls_nsm TO ls_adr-data-communication-smtp-smtp\.")
need('tax numbers are written into KNA1 / LFA1 only - they belong to the business partner where the '
     'country has the category',
     r"APPEND ls_tn TO es_ext-partner-central_data-taxnumber-taxnumbers\.")
need('a tax number without a category in the system is lost instead of staying on the customer / supplier',
     r"IF lv_tcat IS INITIAL\.\s*\n\s*set_field\( ir_line = lr_cd is_chg = ls_tx-chg \)\.")
need('what the validation left in the buffers is not discarded before posting',
     r"ENDTRY\.\s*\n\s*discard\( \)\.\s*\n\s*LOOP AT lt_map")
need('DISCARD does not roll back through BAPI_TRANSACTION_ROLLBACK',
     r"METHOD discard\.\s*\n\s*CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'\.")
if re.search(r'^\s*ROLLBACK WORK\.', SRC, re.M):
    fail.append('a bare ROLLBACK WORK is left - it does not refresh the business partner\'s buffers; use DISCARD')
need('a creation is not checked afterwards - a partner saved without its customer / supplier (MDS_PPO2) '
     'would be reported as created',
     r"IF is_obj-exists = abap_false\.\s*\n.*?num_by_guid\( lv_ng \) IS INITIAL.*?MDS_PPO2")

# Reviewed field by field, 27.11.2026.
need('the catalogue takes no fields for a part whose structure name gives none - it falls back to the '
     'structure the upload writes to', r"IF lt_c IS INITIAL\.\s*\n(?:\s*\".*\n)*\s*lt_c = path_comps\( is_g-grp \)\.")
need('the partner\'s own name, search terms and title are not sent - BP keeps the old ones after a change',
     r"LOOP AT is_obj-chg INTO DATA\(ls_nb\) WHERE grp = 'ADDR'\..*?`SEARCHTERM1`.*?`TITLE_KEY`")
need('organisation names are sent to a partner that is a person',
     r"DATA\(lv_isorg\) = xsdbool\( lv_create = abap_true OR lcl_cfg=>bp_is_org\( lv_bp0 \) = abap_true \)\.")
need('a new mobile is made the standard number next to an existing landline - two standard numbers',
     r"IF ls_cm-kind = `MOB`\.\s*\n\s*IF line_exists\( lt_com\[ kind = `TEL` del = abap_false \] \)\.")
need('a company code added to an existing record does not get the FI role (FLCU00 / FLVN00)',
     r"line_exists\( is_obj-ent\[ grp = 'COMP' task = gc_ins \] \).*?THEN 'FLCU00' ELSE 'FLVN00'")
need('a number given in the file is not passed as the partner number for an external grouping',
     r"lcl_cfg=>bp_ext\(.*?es_ext-partner-header-object_instance-bpartner = is_obj-num\.")
need('BVTYP of the file is not used as the bank details ID of a new account',
     r"IF ls_bv-fld-api = 'BVTYP'\..*?bank_line\( ir_ext = lr_ext is_ent = ls_e iv_bkvid = lv_bvask \)")

if fail:
    print(f'{len(fail)} problem(s):')
    for f in fail:
        print('  -', f)
    sys.exit(1)
print('clean - the CCBJI programs put bank details, new communication and tax numbers on the business '
      'partner, discard after validation and check a creation afterwards')
