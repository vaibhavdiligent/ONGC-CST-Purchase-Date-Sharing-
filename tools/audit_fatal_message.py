"""A fatal message in START-OF-SELECTION is overwritten before it is read.

MESSAGE ... TYPE 'E' inside START-OF-SELECTION writes the status bar and ends
that event block.  END-OF-SELECTION still runs, and the first thing these
programs do there is write the status bar again - a summary line, or, when
nothing was logged, a stock sentence.  The real reason is gone by the time
the user looks.

That is how Cipla's TDS run came back reported as

    No data rows were found to process.

when the reader had in fact refused the file and said why: the reason went to
the status bar, END-OF-SELECTION overwrote it with the stock line, and the
stock line is what was screenshotted and sent back.

So: inside START-OF-SELECTION nothing may stop with a message.  A reason that
must reach the user goes into the LOG, which is displayed as a list and stays
on screen.  Checked here:

  * no MESSAGE of type E, A or X between START-OF-SELECTION and
    END-OF-SELECTION;
  * the log is displayed exactly once, so an early return does not put the
    list on screen twice;
  * the stock sentence for an empty log does not claim to know why the run
    was empty.
"""
import os, re, sys

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
PROGS = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap',
         'src/zsds_cust_tmpl_download.prog.abap']

# a diagnosis the display method is in no position to make
CLAIMS = ('no data rows were found', 'nothing was processed',
          'no data rows found')

findings = []

for prog in PROGS:
    path = os.path.join(ROOT, prog)
    if not os.path.exists(path):
        continue
    name = os.path.basename(prog).split('.')[0].upper()
    src = open(path, encoding='utf8').read()

    m = re.search(r'^START-OF-SELECTION\.', src, re.M)
    if not m:
        continue
    e = re.search(r'^END-OF-SELECTION\.', src[m.end():], re.M)
    body = src[m.end():m.end() + e.start()] if e else src[m.end():]
    offset = m.end()

    for f in re.finditer(r"MESSAGE\b[^.]*?TYPE\s+'([EAX])'", body, re.S):
        line = src[:offset + f.start()].count('\n') + 1
        findings.append(f'{name}:{line}: MESSAGE TYPE \'{f.group(1)}\' in '
                        f'START-OF-SELECTION - END-OF-SELECTION writes the '
                        f'status bar after it, so this reason never reaches '
                        f'the user. Put it in the log and RETURN')

    # the log is put on screen once
    shown = len(re.findall(r'\bgo_log->display\( \)', src))
    if shown > 1:
        findings.append(f'{name}: the log is displayed {shown} times - an '
                        f'early return would put the list on screen twice')

    # Only the stock sentence the DISPLAY method falls back on when the log
    # is empty. The same words earned elsewhere - in a message that counted
    # the rows and named the column - are a real diagnosis, not a guess.
    for d in re.finditer(r'METHOD display\..*?ENDMETHOD\.', src, re.S):
        for f in re.finditer(r"MESSAGE\s+'([^']*)'", d.group(0)):
            if any(c in f.group(1).lower() for c in CLAIMS):
                line = src[:d.start() + f.start()].count('\n') + 1
                findings.append(f'{name}:{line}: the empty-log fallback says '
                                f'"{f.group(1)}" - a diagnosis made where the '
                                f'file may never have been read. Say what is '
                                f'known instead')

if findings:
    print('\n'.join(findings))
    sys.exit(1)
print('clean - no fatal message is stranded in START-OF-SELECTION, the log is '
      'displayed once, and no stock sentence diagnoses a file that was not read')
