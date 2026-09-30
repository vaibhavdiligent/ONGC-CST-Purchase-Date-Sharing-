"""The template map against the customer maps it replaced.

ZSDS_CUST_MASS_UPLOAD and the customer scenarios of ZBCS_MASS_UPLOAD_EXTRACT
mapped the same template headings onto the same customer master. They were
built separately and were in use, so every heading they and the template map
share is an independent opinion about where that column belongs, and the two
opinions have to agree. Both are retired; their maps are kept, frozen, in
tools/cipla/legacy_customer_map.txt for this comparison.

Four differences are deliberate and are listed here rather than reported, each
with the reason. Anything else is a finding.
"""
import collections, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
LEGACY = os.path.join(ROOT, 'tools/cipla/legacy_customer_map.txt')
NEW = open(os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap'), encoding='utf-8').read()


def squash(s):
    return re.sub(r'[^A-Z0-9]', '', s.upper())[:40]


def allowed(head, mine, theirs):
    """The differences that are meant to be there."""
    node, fld = mine
    # The upload ignores the two LSMW control columns; a download writes them,
    # so the file that comes out is ready to be uploaded again.
    if node == 'X' and squash(head) in ('TRANSACTIONCODE', 'ALWAYSX'):
        return True
    # "Tax classification for customer" names no category, and which category
    # it is depends on the country the template serves. Comparing an India
    # template against the upload's US mapping says nothing.
    if node == 'T' and squash(head) == 'TAXCLASSIFICATIONFORCUSTOMER':
        return True
    # "Name 1" is the customer's name on most templates and the contact
    # person's on the one that carries a contact person.
    if node == 'P' and squash(head) == 'NAME1':
        return True
    return False


def main():
    ref = collections.defaultdict(set)
    for line in open(LEGACY, encoding='utf-8'):
        if line.startswith('#') or not line.strip():
            continue
        _prog, _lay, _col, head, node, fld = line.rstrip('\n').split('|')
        ref[head].add((node, fld))
    if not ref:
        print('tools/cipla/legacy_customer_map.txt holds no rows - nothing to compare')
        return 1

    rows = re.findall(r"tmpl = '(\w+)'\s+col = (\d+)\s+hdr = '(.*?)'\s+node = '(.)'\s+"
                      r"fld = '(.*?)'\s+fmt", NEW)
    checked = agree = waived = 0
    findings = collections.Counter()
    for tmpl, col, head, node, fld in rows:
        key = squash(head.replace("''", "'"))
        if key not in ref:
            continue
        checked += 1
        if (node, fld) in ref[key]:
            agree += 1
        elif allowed(head, (node, fld), ref[key]):
            waived += 1
        else:
            findings[(head, (node, fld), tuple(sorted(ref[key])))] += 1

    if findings:
        print(f'{sum(findings.values())} column(s) where the template map and the '
              f'retired maps disagree:')
        for (head, mine, theirs), n in findings.most_common():
            print(f'  {n:4}x "{head[:44]}"')
            print(f'          template {mine}   retired {theirs}')
        return 1
    print(f'clean - {checked} columns carry a heading the retired maps also map; '
          f'{agree} agree outright and {waived} differ for a stated reason')
    return 0


if __name__ == '__main__':
    sys.exit(main())
