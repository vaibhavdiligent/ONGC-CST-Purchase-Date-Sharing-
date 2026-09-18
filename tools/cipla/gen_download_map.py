"""Turn the template registry into the column map of ZSDS_CUST_TMPL_DOWNLOAD.

Every column of every template is resolved to the part of the customer master that
holds it, using the DD03L extract taken from system CRS rather than guesswork:

    K  key                 KUNNR BUKRS VKORG VTWEG SPART KTOKD
    A  address             BAPIAD1VL, under the workbook's ADRC names
    M  communication       telephone, mobile, fax, e-mail
    C  general data        CMDS_EI_VMD_CENTRAL_DATA
    B  company code        KNB1
    S  sales area          KNVV
    T  tax classification  by category, or by position
    Z  ZSD_LICENSE_CHK     licence, bank guarantee, routing, GLN
    I  BP identification   Aadhaar, category X90003
    P  contact person      KNVK
    X  a constant          the transaction code, and the "always X" flag
    -  nothing to read     the copy-from-reference columns
"""
import json, re, sys, collections

DD   = json.load(open('tools/ddic.json'))
REG  = json.load(open('docs/cipla/customer_template_registry.json'))

KEY  = {'KUNNR', 'BUKRS', 'VKORG', 'VTWEG', 'SPART', 'KTOKD'}

# The workbook uses ADRC names; the address node of the interface is BAPIAD1VL.
ADDR = {'NAME1': 'NAME', 'NAME2': 'NAME_2', 'NAME3': 'NAME_3', 'NAME4': 'NAME_4',
        'NAME_CO': 'C_O_NAME', 'SORT1': 'SORT1', 'SORT2': 'SORT2',
        'STREET': 'STREET', 'STR_SUPPL1': 'STR_SUPPL1', 'STR_SUPPL2': 'STR_SUPPL2',
        'STR_SUPPL3': 'STR_SUPPL3', 'LOCATION': 'LOCATION', 'HOUSE_NUM1': 'HOUSE_NO',
        'CITY1': 'CITY', 'CITY2': 'DISTRICT', 'POST_CODE1': 'POSTL_COD1',
        'COUNTRY': 'COUNTRY', 'REGION': 'REGION', 'LANGU': 'LANGU',
        'TITLE_MEDI': 'TITLE'}

COMM = {'TEL_NUMBER': 'TEL', 'MOB_NUMBER': 'MOB', 'FAX_NUMBER': 'FAX', 'SMTP_ADDR': 'SMT'}

# Contact person, KNVK. The numbered suffix is the first contact person only.
CONT = {'PARNR': 'PARNR', 'NAME1_01': 'NAME1', 'NAMEV_01': 'NAMEV',
        'ABTNR_01': 'ABTNR', 'PAFKT_01': 'PAFKT'}

# An LSMW control column. It carries a constant, not master data.
CONST = {'USE_ZAV': 'X', 'ZAV': 'X'}

# Copying from a reference is an XD01 feature. A customer that exists has no
# reference, so these columns are written empty.
NONE = {'REF_KUNNR', 'REF_BUKRS', 'REF_VKORG', 'REF_VTWEG', 'REF_SPART',
        'BUKRS1', 'VKORG1', 'VTWEG1', 'SPART1'}

# Where the same name lives in two places, the workbook's own usage decides.
FIXED = {
    # The XD05 block / unblock template. The central flag and the one that belongs
    # to the company code or the sales area carry the same name, so the sheet's own
    # suffix decides which is which.
    # Whether the row blocks or unblocks is the user's choice, not something
    # that can be read out of the customer, so the column is left empty.
    'LABEL':   ('-', ''),
    'SPERR':   ('C', 'SPERR'),   'SPERR_B': ('B', 'SPERR'),
    'AUFSD':   ('C', 'AUFSD'),   'AUFSD_S': ('S', 'AUFSD'),
    'LIFSD':   ('C', 'LIFSD'),   'LIFSD_S': ('S', 'LIFSD'),
    'FAKSD':   ('C', 'FAKSD'),   'FAKSD_S': ('S', 'FAKSD'),
    'CASSD':   ('C', 'CASSD'),   'CASSD_S': ('S', 'CASSD'),
    'ZTERM':   ('B', 'ZTERM'),    # company code terms
    'ZTERM1':  ('S', 'ZTERM'),    # sales area terms
    'ZTERM_1': ('S', 'ZTERM'),
    'KDGRP':   ('S', 'KDGRP'),    # sales area customer group
    'KDGRP1':  ('Z', 'KDGRP'),    # the one on the licence record
    'WERKS':   ('Z', 'WERKS'),
    'VWERK':   ('S', 'VWERK'),
    'CURRENCY': ('Z', 'CURRENCY'),
    'WAERS':   ('S', 'WAERS'),
    'AADHAAR_NO': ('I', 'X90003'),
    'KATRA4': ('C', 'KATR4'),     # the workbook's spelling of the KNA1 attribute
    # The QCIL templates head the customer number column KNA1. Left unmapped it
    # would come out empty, which is the one column the file cannot do without.
    'KNA1':   ('K', 'KUNNR'),
    'MWST': ('T', 'MWST'), 'UTXJ': ('T', 'UTXJ'),
    'UTX2': ('T', 'UTX2'), 'UTX3': ('T', 'UTX3'),
}

# How a stored value is written so that the upload reads it back unchanged.
#   DT date   NM whole number   AL leading zeros   TT title key
FMT = {'KUNNR': 'AL', 'LIFNR': 'AL', 'AKONT': 'GL', 'TITLE_MEDI': 'TT',
       'FISKN': 'AL', 'ALTKN': 'AL', 'KNA1': 'AL'}


def fields_of(tab):
    return {x['f'] for x in DD.get(tab, [])}


LIC, CENT, KNB1, KNVV = (fields_of('ZSD_LICENSE_CHK'),
                         fields_of('CMDS_EI_VMD_CENTRAL_DATA'),
                         fields_of('KNB1'), fields_of('KNVV'))


def resolve(f):
    """(node, field) for one workbook column name."""
    if f in FIXED:                    return FIXED[f]
    if f in CONST:                    return ('X', CONST[f])
    if f in NONE:                     return ('-', '')
    if f in KEY:                      return ('K', f)
    if f in ADDR:                     return ('A', ADDR[f])
    if f in COMM:                     return ('M', COMM[f])
    if f in CONT:                     return ('P', CONT[f])
    m = re.match(r'^TAXKD_(\d+)$', f)
    if m:                             return ('T', '#%d' % int(m.group(1)))
    if f in LIC:                      return ('Z', f)
    if f in CENT:                     return ('C', f)
    if f in KNB1:                     return ('B', f)
    if f in KNVV:                     return ('S', f)
    return ('?', f)


# ---------------------------------------------------------------- descriptions
# Five blocks carry no technical field row. Their columns are resolved from the
# description, learned from the 55 blocks that do name their fields. Seven
# descriptions are used for more than one field; the reading of each was confirmed
# with Cipla and follows the order those blocks use.
ORDINAL = {
    'taxclassificationforcustomer': ['TAXKD_01', 'TAXKD_02', 'TAXKD_03',
                                     'TAXKD_04', 'TAXKD_05', 'TAXKD_06'],
    'customergroup':      ['KDGRP', 'KDGRP1'],
    'termsofpaymentkey':  ['ZTERM', 'ZTERM1'],
}
SINGLE = {'alwaysx': 'USE_ZAV', 'name1': 'NAME1', 'attribute4': 'KATR4',
          'customercode': 'KUNNR'}

# Where the heading names the tax category, the column is read by category rather
# than by position. The India heading reads JOIG and the category is JOCG - SAP's
# own inconsistency, and the upload program already follows it.
INDIA_TAX = ['JOCG', 'JTC1', 'JTX1', 'JTX2', 'JTX3', 'JTX4']

CATEGORY = {'JOIG': 'JOCG', 'JTC1': 'JTC1', 'JTX1': 'JTX1', 'JTX2': 'JTX2',
            'JTX3': 'JTX3', 'JTX4': 'JTX4', 'UTXJ': 'UTXJ', 'UTX2': 'UTX2',
            'UTX3': 'UTX3', 'MWST': 'MWST'}


def squash(s):
    return re.sub(r'[^a-z0-9]', '', str(s).lower())


def learn_descriptions():
    seen = collections.defaultdict(collections.Counter)
    for fmt in REG['formats'].values():
        if not fmt['fields']:
            continue
        for fld, desc in zip(fmt['fields'], fmt['desc']):
            if desc:
                seen[squash(desc)][fld] += 1
    return {k: c.most_common(1)[0][0] for k, c in seen.items()}, list(seen)


CANON, KNOWN = learn_descriptions()


def from_description(desc, nth):
    """The technical name behind one description, nth being its occurrence."""
    k = squash(desc)
    if k in ORDINAL:
        return ORDINAL[k][nth - 1] if nth <= len(ORDINAL[k]) else ORDINAL[k][-1]
    if k in SINGLE:
        return SINGLE[k]
    if k in CANON:
        return CANON[k]
    # Excel truncates a long heading; a single prefix match is unambiguous.
    hit = [c for c in KNOWN if c.startswith(k)]
    return CANON[hit[0]] if len(hit) == 1 else ''


def main():
    lines, unresolved = [], collections.Counter()
    order = sorted(REG['formats'])
    for i, sig in enumerate(order, 1):
        fmt = REG['formats'][sig]
        users = sorted({f"{c['country']}/{c['ktokd']}" for c in REG['combinations']
                        if c['format'] == sig})
        lands = {c['country'] for c in REG['combinations'] if c['format'] == sig}
        # Cipla confirmed the order of India's six tax categories, so a template
        # used only by India can name them even where its headings do not.
        india_only = lands == {'IN'}
        nth_tax = 0
        lines.append(f"    \"  {sig} - {fmt['ncol']} columns - {', '.join(users)}")
        names = fmt['fields'] or ['' for _ in fmt['desc']]
        seen_desc = collections.Counter()
        for col, (fld, desc) in enumerate(zip(names, fmt['desc']), 1):
            if not fld:                      # a description-only block
                seen_desc[squash(desc)] += 1
                fld = from_description(desc, seen_desc[squash(desc)]) or desc
            if fld == 'TCODE':
                node, target = 'X', fmt.get('tcode') or 'XD01'
            else:
                node, target = resolve(fld)
            # A tax column whose heading names its category is read by category.
            if node == 'T' and target.startswith('#'):
                head = re.sub(r'[^A-Z0-9]', '', (desc or '').upper())[:4]
                if head in CATEGORY:
                    target = CATEGORY[head]
                elif india_only and nth_tax < len(INDIA_TAX):
                    target = INDIA_TAX[nth_tax]
            if node == 'T':
                nth_tax += 1
            if node == '?':
                unresolved[fld] += 1
            hdr = (desc or fld).replace("'", "''")
            lines.append(f"      ( tmpl = '{sig}' col = {col:<3} hdr = '{hdr[:60]}' "
                         f"node = '{node}' fld = '{target}' fmt = '{FMT.get(fld, '')}' )")
    if unresolved:
        print('columns that could not be resolved:', file=sys.stderr)
        for f, n in unresolved.most_common():
            print(f'   {n:3}  {f}', file=sys.stderr)
        return 1
    open('docs/cipla/download_map.abap', 'w').write('\n'.join(lines) + '\n')
    print(f'{len(order)} templates, {len(lines) - len(order)} columns -> '
          f'docs/cipla/download_map.abap')
    return 0


if __name__ == '__main__':
    sys.exit(main())
