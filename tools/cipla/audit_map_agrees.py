"""The download map against the two programs already running.

ZSDS_CUST_MASS_UPLOAD and ZBCS_MASS_UPLOAD_EXTRACT map the same template
headings onto the same customer master, and they were built separately and are
in use. So every heading they and the download share is an independent opinion
about where that column belongs, and the two opinions have to agree.

Four differences are deliberate and are listed here rather than reported, each
with the reason. Anything else is a finding.
"""
import collections, os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
UP = open(os.path.join(ROOT, 'src/zsds_cust_mass_upload.prog.abap'), encoding='utf-8').read()
EX = open(os.path.join(ROOT, 'src/zbcs_mass_upload_extract.prog.abap'), encoding='utf-8').read()
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
    for m in re.finditer(r"node = '(.)' fld = '([A-Z0-9_#]+)'\s+cnv = '(\w*)'\s+"
                         r"hdr = '([A-Z0-9]*)'", UP):
        ref[m.group(4)].add((m.group(1), m.group(2)))
    for m in re.finditer(r"hdr = '(.*?)' node = '(.)' fld = '([A-Z0-9_#]*)'", EX):
        ref[squash(m.group(1))].add((m.group(2), m.group(3)))

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
        print(f'{sum(findings.values())} column(s) where the download and the running '
              f'programs disagree:')
        for (head, mine, theirs), n in findings.most_common():
            print(f'  {n:4}x "{head[:44]}"')
            print(f'          download {mine}   running {theirs}')
        return 1
    print(f'clean - {checked} columns carry a heading the running programs also map; '
          f'{agree} agree outright and {waived} differ for a stated reason')
    return 0


if __name__ == '__main__':
    sys.exit(main())
