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
        rec = dict(sheet=ws.title, tech_row=h, desc_row=d, ncol=len(cols),
                   fields=[str(ws.cell(h, c).value).strip() for c in cols] if h else [],
                   desc=[str(ws.cell(d, c).value or '').strip() for c in cols],
                   typ=[str(ws.cell(h + 1, c).value or '').strip() for c in cols] if h else [],
                   length=[str(ws.cell(h + 2, c).value or '').strip() for c in cols] if h else [],
                   mo=([str(ws.cell(h - 1, c).value or '').strip() for c in cols]
                       if h and str(ws.cell(h - 1, 1).value or '').strip() in ('M', 'O') else []),
                   sample_rows=rows)
        if rec['fields'] and 'KTOKD' in rec['fields']:
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


def main(path, out):
    wb = openpyxl.load_workbook(path, data_only=True)
    all_blocks = []
    for ws in wb.worksheets:
        all_blocks += blocks_of(ws)

    # One format per distinct column list. A format is shared by every
    # country/account group combination that uses the same columns.
    fmt, combo = {}, []
    for b in all_blocks:
        key = '|'.join(b['fields']) if b['fields'] else 'DESC:' + '|'.join(b['desc'])
        sig = hashlib.md5(key.encode()).hexdigest()[:8]
        fmt.setdefault(sig, dict(id=sig, ncol=b['ncol'], fields=b['fields'],
                                 desc=b['desc'], typ=b['typ'], length=b['length'],
                                 has_tech=bool(b['fields'])))
        for k in (b['ktokd'] or ['?']):
            combo.append(dict(sheet=b['sheet'], ktokd=k, format=sig,
                              tech_row=b['tech_row'], desc_row=b['desc_row']))

    json.dump(dict(formats=fmt, combinations=combo), open(out, 'w'), indent=1)
    print(f'{len(all_blocks)} blocks, {len(fmt)} distinct formats, '
          f'{len(combo)} country/account-group combinations -> {out}')
    missing = [b for b in all_blocks if not b['fields']]
    if missing:
        print(f'{len(missing)} block(s) carry no technical field row:')
        for b in missing:
            print(f"   {b['sheet']:12} desc row {b['desc_row']:3}  "
                  f"{b['ncol']:3} columns  {','.join(b['ktokd'])}")


if __name__ == '__main__':
    main(sys.argv[1] if len(sys.argv) > 1 else 'docs/cipla/customer_code_templates.xlsx',
         sys.argv[2] if len(sys.argv) > 2 else 'docs/cipla/customer_template_registry.json')
