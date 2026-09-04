"""Writing into a field the program only names at runtime.

Both upload programs ASSIGN a component by name and then write the cell
into it. The field behind that name can be anything the dictionary allows,
and two statements do not survive that:

  DESCRIBE FIELD <fs> LENGTH n IN CHARACTER MODE
      needs a character-like operand. KNVV-ANTLF is packed, so a domestic
      customer file with column 72 filled ended in OBJECTS_NOT_CHAR - the
      program terminated before a single row was posted.

  <fs> = value
      converts, and a cell that is not a number reaching a packed or date
      field raises CX_SY_CONVERSION_NO_NUMBER. Unhandled, that is a short
      dump for what is really a bad cell, and the run stops instead of
      logging the row and going on.

So: DESCRIBE ... IN CHARACTER MODE belongs only in CHAR_LEN( ), which
checks the type first, and a write through a field symbol has to sit inside
a TRY that catches the conversion error.
"""
import os, re, sys

ROOT  = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
FILES = ['src/zsds_cust_mass_upload.prog.abap',
         'src/zmms_bp_mass_upload.prog.abap',
         'src/zbcs_mass_upload_extract.prog.abap']

def methods(src):
    out, cur, name = [], [], None
    for i, line in enumerate(src.split('\n'), start=1):
        m = re.match(r'\s*METHOD\s+([\w~]+)\s*\.', line)
        if m:
            name, cur = m.group(1), []
            continue
        if re.match(r'\s*ENDMETHOD', line) and name:
            out.append((name, cur)); name = None
            continue
        if name is not None:
            cur.append((i, line))
    return out

bad = []
for path in FILES:
    src = open(os.path.join(ROOT, path), encoding='utf-8').read()
    for name, lines in methods(src):
        body = '\n'.join(re.sub(r'".*$', '', l) for _, l in lines)

        for lineno, line in lines:
            code = re.sub(r'".*$', '', line)
            if re.search(r'DESCRIBE\s+FIELD\b.*\bIN\s+CHARACTER\s+MODE', code, re.I) \
               and name.lower() != 'char_len':
                bad.append(f'{path}:{lineno}: {name}( ) describes a field in character '
                           f'mode outside CHAR_LEN( ) - a packed or string target '
                           f'terminates with OBJECTS_NOT_CHAR')

        # a write through a field symbol that came from ASSIGN COMPONENT
        if 'ASSIGN COMPONENT' not in body:
            continue
        for lineno, line in lines:
            code = re.sub(r'".*$', '', line)
            m = re.match(r'\s*(<[\w_]+>)\s*=\s*(.+?)\s*\.\s*$', code)
            if not m:
                continue
            rhs = m.group(2).strip()
            # A copy between two components of the same type, or a flag put
            # into a DATAX field, converts nothing and cannot fail.
            if rhs.startswith('<') or rhs.startswith("'") or rhs.startswith('`') \
               or rhs.lower() in ('abap_true', 'abap_false', 'space'):
                continue
            # is this line inside a TRY that catches a conversion error?
            before = '\n'.join(re.sub(r'".*$', '', l) for i, l in lines if i < lineno)
            after  = '\n'.join(re.sub(r'".*$', '', l) for i, l in lines if i > lineno)
            open_try = before.count('TRY.') - before.count('ENDTRY')
            caught   = re.search(r'CATCH[^.]*cx_sy_conversion', after, re.I)
            if open_try <= 0 or not caught:
                bad.append(f'{path}:{lineno}: {name}( ) writes {m.group(1)} with no '
                           f'CATCH CX_SY_CONVERSION_ERROR - a cell that will not '
                           f'convert becomes a short dump\n      {code.strip()}')

print('\n'.join(bad) if bad else
      'clean - nothing describes an unknown field in character mode, and every '
      'write through a field symbol catches a conversion that fails')
sys.exit(1 if bad else 0)
