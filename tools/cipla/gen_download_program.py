"""Assemble src/zsds_cust_tmpl_download.prog.abap from the skeleton and the map.

The skeleton holds the logic and carries two markers; this fills them from the
template registry, so a change to the workbook is a regeneration rather than an
edit to the program.
"""
import json, re, subprocess, sys

SKEL = 'tools/cipla/download_skeleton.abap'
OUT  = 'src/zsds_cust_tmpl_download.prog.abap'
CHUNKS = 6

# The two templates that do not belong to a country carry a name the program
# refers to directly.
RENAME = {'cust extn': 'EXTN', 'block unblock': 'BLOCK'}


def main():
    if subprocess.call([sys.executable, 'tools/cipla/gen_download_map.py']) != 0:
        return 1
    reg = json.load(open('docs/cipla/customer_template_registry.json'))
    rows = open('docs/cipla/download_map.abap').read().splitlines()

    # The generated id of a template, renamed where the program names it.
    alias = {}
    for c in reg['combinations']:
        if c['sheet'] in RENAME:
            alias[c['format']] = RENAME[c['sheet']]
    for old, new in alias.items():
        rows = [r.replace(f"tmpl = '{old}'", f"tmpl = '{new}'") for r in rows]

    # The map, split so that no single VALUE statement grows unreasonable.
    body = [r for r in rows if r.strip().startswith('(')]
    comment = {i: r for i, r in enumerate(rows) if not r.strip().startswith('(')}
    per = -(-len(rows) // CHUNKS)
    methods = []
    for n in range(CHUNKS):
        part = rows[n * per:(n + 1) * per]
        if not part:
            part = ['      " nothing in this part']
        methods.append('  METHOD map_%d.\n    rt = VALUE tt_col(\n%s\n    ).\n  ENDMETHOD.'
                       % (n + 1, '\n'.join(part)))
    mapping = '\n\n'.join(methods)

    # A country and an account group name one template. The two that belong to
    # no country are reached by their own radio button, not through this table.
    seen, combi = set(), []
    for c in sorted(reg['combinations'], key=lambda x: (x['country'], x['ktokd'])):
        if c['country'] == '*':
            continue
        key = (c['country'], c['ktokd'])
        if key in seen:
            continue
        seen.add(key)
        combi.append(f"      ( land = '{c['country']}' ktokd = '{c['ktokd']}' "
                     f"tmpl = '{alias.get(c['format'], c['format'])}' )")

    text = open(SKEL).read()
    text = text.replace('*<<COMBI>>', '\n'.join(combi))
    text = text.replace('*<<MAP>>', mapping)
    open(OUT, 'w').write(text)
    print(f'{OUT}: {len(text.splitlines())} lines, {len(body)} columns, '
          f'{len(combi)} combinations, {CHUNKS} map methods')
    return 0


if __name__ == '__main__':
    sys.exit(main())
