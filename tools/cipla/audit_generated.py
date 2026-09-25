"""The generated program must be what the generator generates.

ZSDS_CUST_TMPL_DOWNLOAD is assembled from download_skeleton.abap and the
template registry. Editing the generated file instead of the skeleton looks
like it works, and then the next regeneration throws the edit away without
a word - which is exactly what happened to the file-path fix: it was made in
src/, the skeleton never got it, and the first regeneration put RLGRAP-FILENAME
back.

So: regenerate into a copy and compare. Any difference means someone edited
the output, and the change belongs in the skeleton.

The file is restored either way; this reports, it does not fix.
"""
import os, shutil, subprocess, sys, tempfile

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
OUT  = os.path.join(ROOT, 'src/zsds_cust_tmpl_download.prog.abap')

kept = tempfile.NamedTemporaryFile(delete=False, suffix='.abap').name
shutil.copy(OUT, kept)
try:
    r = subprocess.run([sys.executable, 'tools/cipla/gen_download_program.py'],
                       cwd=ROOT, capture_output=True, text=True)
    if r.returncode != 0:
        print('the generator failed:\n' + (r.stderr or r.stdout))
        sys.exit(1)
    fresh = open(OUT, encoding='utf8').read().splitlines()
    have  = open(kept, encoding='utf8').read().splitlines()
finally:
    shutil.copy(kept, OUT)
    os.unlink(kept)

if fresh != have:
    import difflib
    diff = list(difflib.unified_diff(have, fresh, 'in src/', 'regenerated',
                                     lineterm='', n=1))
    print('src/zsds_cust_tmpl_download.prog.abap is not what the generator '
          'produces - the change belongs in tools/cipla/download_skeleton.abap:')
    print('\n'.join(diff[:40]))
    if len(diff) > 40:
        print(f'  ... and {len(diff) - 40} more lines')
    sys.exit(1)
print('clean - the generated program is exactly what the generator produces')
