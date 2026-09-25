"""Read "customer code templates.xlsx" and write the format registry the download
program is built from.

The workbook is LSMW shaped. Every sheet is a country or legal entity, and inside a
sheet the templates are stacked one under the other, one per customer account group.
A block is:

    Project / Subproject / Object      (optional, 3 rows)
    M / O                              (optional, 1 row - mandatory or optional)
    TCODE KUNNR BUKRS ...              technical field names   <- the anchor
    C C C ...                          data type
    20 16 4 ...                        length
    Transaction Code, Customer ...     description
    XD01 ...                           sample data rows

Five blocks carry no technical row at all and are anchored on the description row
instead; those are reported separately because their fields have to be resolved by
hand before they can be built.
"""
import json, re, sys, collections, hashlib
import openpyxl

TCODE = re.compile(r'^X[A-Z]\d{2}$')
MARK = {'PROJECT', 'SUBPROJECT', 'OBJECT', 'M', 'O', 'TCODE'}

# The country each sheet serves. The workbook states it in the sheet name and in the
# Project row above the first block - "DOM customer creation for Australia",
# "Ship to party Dubai", "Europe customer master", "ZEXP customer upload / India export",
# "YDOM- Kenya", "Morocco customer code creation", "New customer for Uganda / Uganda
# domestick". Exelan and Invagen are the two United States entities, and their Project
# rows read "US Sold to's". SAGA carries no country word; its sample rows are ZAR and ZA.
# Europe is a region rather than a country and is the one entry Cipla has to confirm.
SHEET_COUNTRY = {
    'Australia': ['AU'],
    'Dubai':     ['AE'],
    # Confirmed by Cipla: the Europe template serves four countries and is the same for
    # all of them - United Kingdom 7101, Belgium 7001, Spain 7451, Netherlands 7501.
    'Europe':    ['GB', 'BE', 'ES', 'NL'],
    'Exelan':    ['US'],
    'India':     ['IN'],
    'Invagen':   ['US'],
    'Kenya':     ['KE'],
    'Moroccco':  ['MA'],
    'QCIL':      ['UG'],
    'SAGA':      ['ZA'],    # inferred from the sample rows, no country word on the sheet
    'cust extn': ['*'],     # extension template, valid for any country
    'block unblock': ['*'],  # XD05, valid for any country
}

# The workbook is organised by REGION, not by country, and the two do not line up:
# Europe is one template for four countries, and the United States has two entities -
# Exelan and Invagen - with three account groups in common. A country alone therefore
# cannot say which template is wanted, which is why the selection screen offers the
# region and derives the country from it. The code is what the screen stores; the text
# is what it shows, with the country or countries in brackets.
SHEET_REGION = {
    'Australia':     ('AU', 'Australia'),
    'Dubai':         ('AE', 'Dubai'),
    'Europe':        ('EU', 'Europe'),
    'Exelan':        ('EX', 'Exelan'),
    'India':         ('IN', 'India'),
    'Invagen':       ('IV', 'Invagen'),
    'Kenya':         ('KE', 'Kenya'),
    'Moroccco':      ('MA', 'Morocco'),     # the sheet name has the typo, not the country
    'QCIL':          ('UG', 'QCIL'),
    'SAGA':          ('ZA', 'SAGA'),
    'cust extn':     ('CX', 'Customer extension'),
    'block unblock': ('BU', 'Block / unblock'),
}

# The QCIL export block holds two layouts at once: a technical row copied from the
# 83-column Australia/Morocco template, and a description and data row copied from the
# QCIL domestic template and pasted one column out. Cipla confirmed it is the QCIL
# template - 63 fields, plus the transaction code and the customer code, 65 columns in
# all - so the block takes the QCIL domestic field list.
BLOCK_FROM = {('QCIL', 'ZEXP'): ('QCIL', 'ZDOM')}


def blocks_of(ws):
    """Every template block on one sheet, in row order."""
    tech = [r for r in range(1, ws.max_row + 1)
            if str(ws.cell(r, 1).value or '').strip().upper() == 'TCODE']
    desc = [r for r in range(1, ws.max_row + 1)
            if str(ws.cell(r, 1).value or '').strip().lower() == 'transaction code']
    out = []
    for d in desc:
        h = d - 3 if (d - 3) in tech else None          # technical row, when there is one
        anchor = h or d
        cols = [c for c in range(1, ws.max_column + 1)
                if str(ws.cell(anchor, c).value or '').strip()]
        nxt = min([t for t in tech if t > d] + [r for r in desc if r > d] + [ws.max_row + 1])
        rows = [r for r in range(d + 1, nxt) if TCODE.match(str(ws.cell(r, 1).value or '').strip())]
        # A block whose description row has drifted out of step with its technical row is
        # flagged rather than silently resolved either way. The technical row is kept,
        # because it is the one that is complete, but the block is reported.
        inconsistent = False
        if h:
            fl = [str(ws.cell(h, c).value).strip() for c in cols]
            ds = [str(ws.cell(d, c).value or '').strip() for c in cols]
            pairs = {'TCODE': 'transaction code', 'KTOKD': 'customer account group',
                     'BUKRS': 'company code', 'VKORG': 'sales organization'}
            inconsistent = sum(1 for f, x in zip(fl, ds)
                               if f in pairs and x and x.lower() != pairs[f]) >= 2

        # The transaction code the template is for. Every create template is
        # XD01; the block and unblock sheet is XD05, and writing XD01 into it
        # would hand the user a file for the wrong transaction.
        tcode = ''
        for r in rows:
            v = str(ws.cell(r, 1).value or '').strip()
            if TCODE.match(v):
                tcode = v
                break
        if not tcode and ws.title == 'block unblock':
            tcode = 'XD05'

        rec = dict(sheet=ws.title, tech_row=h, desc_row=d, ncol=len(cols),
                   inconsistent=inconsistent, tcode=tcode,
                   fields=[str(ws.cell(h, c).value).strip() for c in cols] if h else [],
                   desc=[str(ws.cell(d, c).value or '').strip() for c in cols],
                   typ=[str(ws.cell(h + 1, c).value or '').strip() for c in cols] if h else [],
                   length=[str(ws.cell(h + 2, c).value or '').strip() for c in cols] if h else [],
                   mo=([str(ws.cell(h - 1, c).value or '').strip() for c in cols]
                       if h and str(ws.cell(h - 1, 1).value or '').strip() in ('M', 'O') else []),
                   sample_rows=rows)
        # A block whose rows disagree is keyed from the description row, because the
        # data follows the description.
        if inconsistent and 'Customer Account Group' in rec['desc']:
            kc = cols[rec['desc'].index('Customer Account Group')]
            rec['ktokd'] = sorted({str(ws.cell(r, kc).value).strip()
                                   for r in rows if ws.cell(r, kc).value})
        elif rec['fields'] and 'KTOKD' in rec['fields']:
            kc = cols[rec['fields'].index('KTOKD')]
            rec['ktokd'] = sorted({str(ws.cell(r, kc).value).strip()
                                   for r in rows if ws.cell(r, kc).value})
        elif 'Customer Account Group' in rec['desc']:
            kc = cols[rec['desc'].index('Customer Account Group')]
            rec['ktokd'] = sorted({str(ws.cell(r, kc).value).strip()
                                   for r in rows if ws.cell(r, kc).value})
        else:
            rec['ktokd'] = []
        out.append(rec)
    return out


def block_unblock(ws):
    """The XD05 sheet, which has no Project rows and no technical field row.

    Row 1 groups the columns, row 2 describes them, rows 3 and 4 are the blocking
    and the unblocking example. Column 1 is a label rather than a field.
    """
    cols = [c for c in range(1, ws.max_column + 1)
            if str(ws.cell(2, c).value or '').strip()]
    fields = ['LABEL', 'KUNNR', 'BUKRS', 'VKORG', 'VTWEG', 'SPART',
              'SPERR', 'SPERR_B', 'AUFSD', 'AUFSD_S', 'LIFSD', 'LIFSD_S',
              'FAKSD', 'FAKSD_S', 'CASSD', 'CASSD_S']
    return dict(sheet=ws.title, tech_row=None, desc_row=2, ncol=len(fields),
                inconsistent=False, tcode='XD05', fields=fields,
                desc=['Blocking or unblocking'] + [str(ws.cell(2, c).value).strip()
                                                   for c in cols],
                typ=[], length=[], mo=[], sample_rows=[3, 4], ktokd=['*'])


def main(path, out):
    wb = openpyxl.load_workbook(path, data_only=True)
    all_blocks = []
    for ws in wb.worksheets:
        if ws.title == 'block unblock':
            all_blocks.append(block_unblock(ws))
            continue
        all_blocks += blocks_of(ws)

    # A block that borrows another block's layout takes its field list before the
    # formats are worked out.
    by_key = {(b['sheet'], k): b for b in all_blocks for k in (b['ktokd'] or [])}
    for dst, src in BLOCK_FROM.items():
        if dst in by_key and src in by_key:
            for f in ('fields', 'desc', 'typ', 'length', 'ncol'):
                by_key[dst][f] = by_key[src][f]
            by_key[dst]['borrowed_from'] = f'{src[0]}/{src[1]}'
            by_key[dst]['inconsistent'] = False

    # One format per distinct column list. A format is shared by every
    # country/account group combination that uses the same columns.
    fmt, combo = {}, []
    for b in all_blocks:
        key = '|'.join(b['fields']) if b['fields'] else 'DESC:' + '|'.join(b['desc'])
        sig = hashlib.md5(key.encode()).hexdigest()[:8]
        fmt.setdefault(sig, dict(id=sig, ncol=b['ncol'], fields=b['fields'],
                                 desc=b['desc'], typ=b['typ'], length=b['length'],
                                 tcode=b.get('tcode') or 'XD01',
                                 has_tech=bool(b['fields'])))
        rg, rtext = SHEET_REGION.get(b['sheet'], ('??', b['sheet']))
        for k in (b['ktokd'] or ['?']):
            for ctry in SHEET_COUNTRY.get(b['sheet'], ['?']):
                combo.append(dict(sheet=b['sheet'], region=rg, country=ctry, ktokd=k,
                                  format=sig, tech_row=b['tech_row'],
                                  desc_row=b['desc_row']))

    # The region list the selection screen shows, each with the country or
    # countries behind it, so the user picks a place and not a key.
    regions = []
    for sheet, (rg, rtext) in SHEET_REGION.items():
        lands = [c for c in SHEET_COUNTRY.get(sheet, []) if c != '*']
        regions.append(dict(region=rg, sheet=sheet, countries=lands,
                            text=f'{rtext} ({"/".join(lands)})' if lands else rtext))
    regions.sort(key=lambda r: r['text'])

    json.dump(dict(formats=fmt, combinations=combo, sheet_country=SHEET_COUNTRY,
                   regions=regions), open(out, 'w'), indent=1)

    # A country and an account group must name exactly one format, or the selection
    # screen cannot resolve what the user asked for.
    key = {}
    for c in combo:
        key.setdefault((c['country'], c['ktokd']), set()).add(c['format'])
    clash = {k: v for k, v in key.items() if len(v) > 1}
    print(f'{len(key)} country + account-group keys, {len(clash)} ambiguous')
    for k, v in clash.items():
        print(f'   {k} -> {sorted(v)}')
    print(f'{len(all_blocks)} blocks, {len(fmt)} distinct formats, '
          f'{len(combo)} country/account-group combinations -> {out}')
    odd = [b for b in all_blocks if b.get('inconsistent')]
    if odd:
        print(f'{len(odd)} block(s) whose description row disagrees with the technical row:')
        for b in odd:
            print(f"   {b['sheet']:12} technical row {b['tech_row']}, description row "
                  f"{b['desc_row']}  {b['ncol']} columns")

    missing = [b for b in all_blocks if not b['fields']]
    if missing:
        print(f'{len(missing)} block(s) carry no technical field row:')
        for b in missing:
            print(f"   {b['sheet']:12} desc row {b['desc_row']:3}  "
                  f"{b['ncol']:3} columns  {','.join(b['ktokd'])}")


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else 'docs/cipla/customer_code_templates.xlsx',
         sys.argv[2] if len(sys.argv) > 2 else 'docs/cipla/customer_template_registry.json')
