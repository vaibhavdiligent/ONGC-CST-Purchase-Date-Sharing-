"""Data declared in an AT SELECTION-SCREEN block is local to that block.

SAP runs the AT SELECTION-SCREEN event blocks as procedures, so a DATA
statement in one of them - or anywhere after one, up to the next event
keyword, which is still inside it - declares a local variable. The same
statement in INITIALIZATION, START-OF-SELECTION or END-OF-SELECTION, which
are not procedures, declares a global one.

ZMMS_BP_MASS_UPLOAD declared GO_LOG between its last AT SELECTION-SCREEN
block and START-OF-SELECTION, meaning it for the whole program. It was
local to AT SELECTION-SCREEN, and activation stopped with

    Field "GO_LOG" is unknown.

This reports every name declared in an AT SELECTION-SCREEN block that is
used in another event block, where no global declaration makes it visible.
"""
import os, re, sys

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from abap_parse import statements, unchain

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_tmpl_download.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'tools/cipla/download_skeleton.abap']

EVENTS = re.compile(r'(INITIALIZATION|START-OF-SELECTION|END-OF-SELECTION|LOAD-OF-PROGRAM|'
                    r'TOP-OF-PAGE|END-OF-PAGE|AT\s+LINE-SELECTION|AT\s+USER-COMMAND|'
                    r'AT\s+SELECTION-SCREEN)\b')
DECL = re.compile(r'^(?:DATA|CLASS-DATA|STATICS|CONSTANTS|FIELD-SYMBOLS|PARAMETERS|'
                  r'SELECT-OPTIONS|TABLES)\s+(<?\w+>?)')
INLINE = re.compile(r'\b(?:DATA|FIELD-SYMBOL)\((<?\w+>?)\)')

findings = []
for p in PROGS:
    src = open(os.path.join(ROOT, p), encoding='utf-8').read()
    block = ('global', 0)      # the event block a top-level statement belongs to
    depth = 0                  # inside CLASS ... ENDCLASS / FORM ... ENDFORM
    local = {}                 # (block) -> {name: line}
    glob = set()
    uses = []                  # (block, line, statement)
    for ln, text in statements(src):
        for s in unchain(text):
            up = ' '.join(s.split()).upper()
            w = up.split()
            if not w:
                continue
            if ((w[0] == 'CLASS' and len(w) > 2) or (w[0] == 'INTERFACE' and len(w) > 1)) and \
                    not re.search(r'\b(DEFERRED|LOAD)\b', up):
                depth += 1
                # a class definition ends the event block before it
                block = ('global', ln)
                continue
            if w[0] == 'FORM':
                depth += 1
                block = ('global', ln)
                continue
            if w[0] in ('ENDCLASS', 'ENDINTERFACE', 'ENDFORM'):
                depth -= 1
                if depth < 0:
                    sys.exit(f'{p}:{ln}: {w[0]} with nothing open - this audit lost count')
                continue
            if depth:
                continue
            m = EVENTS.match(up)
            if m:
                kind = 'ass' if m.group(1).startswith('AT SELECTION') else 'event'
                block = (kind, ln)
                continue
            names = []
            d = DECL.match(up)
            if d:
                names.append(d.group(1))
            names += INLINE.findall(up)
            for n in names:
                if block[0] == 'ass':
                    local.setdefault(block, {})[n] = ln
                else:
                    glob.add(n)
            uses.append((block, ln, up))

    for blk, names in local.items():
        for n, dln in names.items():
            if n in glob:
                continue
            pat = re.compile(r'(?<![\w<-])' + re.escape(n) + r'(?![\w>])')
            for b2, ln, up in uses:
                if b2 == blk:
                    continue
                if pat.search(up):
                    findings.append(f'{p}:{ln}: {n} is used here, but it was declared at line '
                                    f'{dln} inside AT SELECTION-SCREEN, where it is local - '
                                    f'"Field {n} is unknown"')
                    break

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - nothing declared in an AT SELECTION-SCREEN block is used outside it')
