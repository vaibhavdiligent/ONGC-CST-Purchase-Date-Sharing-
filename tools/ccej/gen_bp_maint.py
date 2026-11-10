"""Generates the two CCEJ master data programs from one source.

    python3 tools/ccej/gen_bp_maint.py

The source is tools/ccej/bp_maint/*.abap, joined in name order. The
customer and the supplier program differ only in the values substituted
below; everything else is the same code, which branches on GC_SIDE where the
two masters differ. Edit the parts, never the generated programs - the
audits compare them with what this script produces.
"""
import glob, os, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
PARTS = sorted(glob.glob(os.path.join(ROOT, 'tools', 'ccej', 'bp_maint', '*.abap')))

SIDES = {
    # CCBJI's report convention /CCBJI/R<U|D><module><submodule>R_<name>, as in
    # /CCBJI/RUSDSLSR_LSA_UPLOAD: U for a report that updates, SD or MM, MD for
    # master data. abapGit writes the namespace slashes as # in the file name.
    'C': dict(PROG='/ccbji/rusdmdr_cust_bp_maint', PROGU='/CCBJI/RUSDMDR_CUST_BP_MAINT',
              FILE='#ccbji#rusdmdr_cust_bp_maint', FILEBASE='CUST_BP_MAINT', SIDE='C',
              NUMTYPE='kunnr', ORGTYPE='vkorg', KTOTYPE='ktokd',
              TITLE='Customer Master - Dynamic Download / Upload', MODULE='SD',
              TCODE='/CCBJI/RUSDMD_CUSBP', TTEXT='Customer master download / upload',
              WHAT1='customer',
              NUMTEXT='Customer', ORGTEXT='Sales organization', WHO='Which customers (download)'),
    'V': dict(PROG='/ccbji/rummmdr_vend_bp_maint', PROGU='/CCBJI/RUMMMDR_VEND_BP_MAINT',
              FILE='#ccbji#rummmdr_vend_bp_maint', FILEBASE='VEND_BP_MAINT', SIDE='V',
              NUMTYPE='lifnr', ORGTYPE='ekorg', KTOTYPE='ktokk',
              TITLE='Supplier Master - Dynamic Download / Upload', MODULE='MM',
              TCODE='/CCBJI/RUMMMD_VENBP', TTEXT='Supplier master download / upload',
              WHAT1='supplier',
              NUMTEXT='Supplier', ORGTEXT='Purchasing organization', WHO='Which suppliers (download)'),
}


def source(side):
    v = SIDES[side]
    out = '\n'.join(open(p, encoding='utf-8').read().rstrip('\n') + '\n' for p in PARTS)
    for k, val in v.items():
        out = out.replace('@@' + k + '@@', val)
    left = [l for l in out.split('\n') if '@@' in l]
    if left:
        sys.exit(f'unreplaced placeholder: {left[0]}')
    return out


def xml(side):
    v = SIDES[side]
    syms = [('001', 'Create or change'), ('002', 'Download or upload'),
            ('003', 'New records (create only)'), ('004', 'Fields in the file'),
            ('005', v['WHO']), ('006', 'Upload'), ('007', 'File')]
    sels = [('P_CRT', 'Create new records'), ('P_CHG', 'Change existing records'),
            ('P_DOWN', 'Download a file'), ('P_UP', 'Upload a filled file'),
            ('P_KTOGR', 'Account group'), ('P_BPGRP', 'BP grouping (optional)'),
            ('S_FLD', 'Chosen fields'), ('P_TLANG', 'Language of texts'),
            ('S_NUM', v['NUMTEXT']), ('S_BP', 'Business partner'), ('S_BUKRS', 'Company code'),
            ('S_ORG', v['ORGTEXT']), ('P_MAX', 'Records at most'),
            ('P_TEST', 'Test run (nothing is posted)'), ('P_FILE', 'File'),
            ('P_XLSX', 'Excel (.xlsx)'), ('P_TXT', 'Text, tab-separated (.txt)')]
    items = [f'    <item>\n     <ID>R</ID>\n     <ENTRY>{v["TITLE"]}</ENTRY>\n     <LENGTH>{len(v["TITLE"])}</LENGTH>\n    </item>']
    for k, t in syms:
        items.append(f'    <item>\n     <ID>I</ID>\n     <KEY>{k}</KEY>\n     <ENTRY>{t}</ENTRY>\n     <LENGTH>{len(t)}</LENGTH>\n    </item>')
    for k, t in sels:
        assert len(t) <= 30, t
        e = 'D       .' + t
        items.append(f'    <item>\n     <ID>S</ID>\n     <KEY>{k}</KEY>\n     <ENTRY>{e}</ENTRY>\n     <LENGTH>{len(e)}</LENGTH>\n    </item>')
    return ('<?xml version="1.0" encoding="utf-8"?>\n'
            '<abapGit version="v1.0.0" serializer="LCL_OBJECT_PROG" serializer_version="v1.0.0">\n'
            ' <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n'
            '  <asx:values>\n'
            '   <PROGDIR>\n'
            f'    <NAME>{v["PROGU"]}</NAME>\n'
            '    <STATE>A</STATE>\n    <DTEFPS>U</DTEFPS>\n    <UCCHECK>X</UCCHECK>\n'
            '    <SUBC>1</SUBC>\n    <RSTAT>K</RSTAT>\n    <FIXPT>X</FIXPT>\n'
            '   </PROGDIR>\n'
            '   <TPOOL>\n' + '\n'.join(items) + '\n   </TPOOL>\n'
            '  </asx:values>\n'
            ' </asx:abap>\n'
            '</abapGit>\n')


def tran_file(side):
    return SIDES[side]['TCODE'].lower().replace('/', '#') + '.tran.xml'


def tran_xml(side):
    v = SIDES[side]
    assert len(v['TCODE']) <= 20 and len(v['PROGU']) <= 30 and len(v['TTEXT']) <= 36, v
    return ('<?xml version="1.0" encoding="utf-8"?>\n'
            '<abapGit version="v1.0.0" serializer="LCL_OBJECT_TRAN" serializer_version="v1.0.0">\n'
            ' <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n'
            '  <asx:values>\n'
            '   <TSTC>\n'
            f'    <TCODE>{v["TCODE"]}</TCODE>\n'
            f'    <PGMNA>{v["PROGU"]}</PGMNA>\n'
            '    <DTEFPS>U</DTEFPS>\n'
            '    <CINFO>00000000000000000010000000000000</CINFO>\n'
            '   </TSTC>\n'
            '   <TSTCC>\n'
            f'    <TCODE>{v["TCODE"]}</TCODE>\n'
            '    <S_WEBGUI>1</S_WEBGUI>\n'
            '    <S_EASYWEB>0</S_EASYWEB>\n'
            '    <S_SERVICE>0</S_SERVICE>\n'
            '   </TSTCC>\n'
            '   <TSTCT>\n'
            f'    <TCODE>{v["TCODE"]}</TCODE>\n'
            f'    <TTEXT>{v["TTEXT"]}</TTEXT>\n'
            '   </TSTCT>\n'
            '  </asx:values>\n'
            ' </asx:abap>\n'
            '</abapGit>\n')


def main():
    for side, v in SIDES.items():
        base = os.path.join(ROOT, 'src', v['FILE'])
        open(base + '.prog.abap', 'w', encoding='utf-8').write(source(side))
        open(base + '.prog.xml', 'w', encoding='utf-8').write(xml(side))
        tran = os.path.join(ROOT, 'src', tran_file(side))
        open(tran, 'w', encoding='utf-8').write(tran_xml(side))
        print('wrote', base + '.prog.abap', len(source(side).split('\n')), 'lines')


if __name__ == '__main__':
    main()
