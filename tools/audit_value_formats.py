"""Every shape a value can arrive in from a spreadsheet, and what the upload
   programs make of it.

CL_FDT_XL_SPREADSHEET hands over whatever the cell held, and that depends on
how the file was made and on what Excel did to it in between: a date can come
as 31.12.2026, as 2026-12-31, with a time behind it, or as the serial number
46022; an amount as 500,000.00 or 500.000,00 or with a trailing minus; a
ticked box as X or as TRUE.

This mirrors TO_DATE, TO_DEC, ALPHA and FLAG from the two programs and checks
each shape. A shape must either come out right or be rejected outright - a
wrong value that posts silently is the one outcome that must not happen.
"""
import datetime, re, sys

def to_date(s):
    lv = s.strip()
    if not lv:
        return ''
    if ' ' in lv:
        lv = lv.split(' ')[0]
    lv = lv.replace('/', '.').replace('-', '.')
    rv = ''
    if '.' in lv:
        parts = (lv.split('.') + ['', '', ''])[:3]
        p1, p2, p3 = parts
        if not (p1 and p2 and p3) or not (p1.isdigit() and p2.isdigit() and p3.isdigit()):
            return ''
        if len(p1) == 4:
            y, m, d = p1, p2, p3
        elif int(p1) > 12 or int(p2) <= 12:
            d, m, y = p1, p2, p3
        else:
            m, d, y = p1, p2, p3
        if len(y) == 2:
            y = '20' + y
        if len(y) != 4:
            return ''
        rv = f'{y}{int(m):02d}{int(d):02d}'
    elif len(lv) == 8 and lv.isdigit():
        rv = lv
    elif 4 <= len(lv) <= 6 and lv.isdigit() and 20000 <= int(lv) <= 80000:
        rv = (datetime.date(1899, 12, 30) + datetime.timedelta(days=int(lv))).strftime('%Y%m%d')
    if rv:
        try:
            datetime.date(int(rv[:4]), int(rv[4:6]), int(rv[6:]))
        except ValueError:
            rv = ''
    return rv

def to_dec(s):
    lv = ''.join(s.split())
    if not lv:
        return None
    neg = lv.endswith('-')
    if neg:
        lv = lv[:-1]
    dot, com = lv.rfind('.'), lv.rfind(',')
    if dot >= 0 and com >= 0:
        if com > dot:
            lv = lv.replace('.', '').replace(',', '.')
        else:
            lv = lv.replace(',', '')
    elif lv.count(',') == 1:
        lv = lv.replace(',', '.') if len(lv) - com - 1 == 2 else lv.replace(',', '')
    elif lv.count(',') > 1:
        lv = lv.replace(',', '')
    try:
        v = float(lv)
    except ValueError:
        return 'ERROR'
    return -v if neg else v

def flag(s):
    lv = s.strip().upper()
    if len(lv) <= 1:
        return lv
    if lv in ('TRUE', 'YES', 'JA', 'Y', 'J', '1', 'SET', 'CHECKED'):
        return 'X'
    if lv in ('FALSE', 'NO', 'NEIN', 'N', '0', 'UNCHECKED'):
        return ''
    return lv

def alpha(s, n=10):
    rv = s.strip()
    return rv.rjust(n, '0') if rv.isdigit() and len(rv) < n else rv

CASES = [
    ('date', '31.12.2026', '20261231'), ('date', '01.01.2012', '20120101'),
    ('date', '31/12/2026', '20261231'), ('date', '31-12-2026', '20261231'),
    ('date', '20261231', '20261231'),   ('date', '31.12.26', '20261231'),
    ('date', '12/31/2026', '20261231'), ('date', '2026-12-31', '20261231'),
    ('date', '2026.12.31', '20261231'), ('date', '46022', '20251231'),
    ('date', '31.12.2026 00:00:00', '20261231'), ('date', '1.1.2026', '20260101'),
    ('date', '31.02.2026', ''),         ('date', 'not a date', ''),
    ('dec', '500000', 500000.0),        ('dec', '500000.00', 500000.0),
    ('dec', '500,000.00', 500000.0),    ('dec', '500.000,00', 500000.0),
    ('dec', '500 000', 500000.0),       ('dec', '500000.5', 500000.5),
    ('dec', '1,234,567.89', 1234567.89), ('dec', '1.234.567,89', 1234567.89),
    ('dec', '2500-', -2500.0),          ('dec', '-2500', -2500.0),
    ('dec', '1500,50', 1500.50),        ('dec', '1,500', 1500.0),
    ('dec', 'abc', 'ERROR'),
    ('flag', 'X', 'X'), ('flag', 'TRUE', 'X'), ('flag', 'Yes', 'X'),
    ('flag', 'FALSE', ''), ('flag', 'No', ''), ('flag', '1', '1'),   # one character is left alone: CHAR1 fields like KALKS hold digits
    ('alpha', '0000144700', '0000144700'), ('alpha', '144700', '0000144700'),
    ('alpha', 'YVMI', 'YVMI'),
]

wrong, rejected = [], []
print(f'  {"arrives as":<24} {"kind":<6} {"becomes":<14} {"should be":<12} verdict')
for kind, raw, exp in CASES:
    got = {'date': to_date, 'dec': to_dec, 'flag': flag, 'alpha': alpha}[kind](raw)
    if kind == 'dec' and isinstance(got, float) and isinstance(exp, float):
        ok = abs(got - exp) < 0.001
    else:
        ok = str(got) == str(exp)
    if ok:
        verdict = 'ok'
    elif got in ('', 'ERROR', None):
        verdict = 'rejected - the row is reported, not posted'
        rejected.append(raw)
    else:
        verdict = '*** WRONG VALUE ***'
        wrong.append((raw, got, exp))
    print(f'  {raw!r:<24} {kind:<6} {str(got):<14} {str(exp):<12} {verdict}')

print()
if wrong:
    print('SILENTLY WRONG:')
    for raw, got, exp in wrong:
        print(f'   {raw!r} becomes {got} but means {exp}')
else:
    print('clean - no spreadsheet value shape is read as the wrong number, date or flag')
sys.exit(1 if wrong else 0)
