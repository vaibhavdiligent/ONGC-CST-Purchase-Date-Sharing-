"""The two CCEJ programs are generated - they must be exactly what the
generator makes from tools/ccej/bp_maint/*.abap.

A change made straight in src/zsds_cust_bp_maint.prog.abap or
src/zmms_vend_bp_maint.prog.abap would be lost at the next generation, and a
change made only to the parts would never reach SAP.
"""
import importlib.util, os, sys

ROOT = os.path.dirname(os.path.dirname(os.path.dirname(os.path.abspath(__file__))))
spec = importlib.util.spec_from_file_location('gen', os.path.join(ROOT, 'tools', 'ccej', 'gen_bp_maint.py'))
gen = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gen)

bad = []
for side, v in gen.SIDES.items():
    base = os.path.join(ROOT, 'src', v['PROG'])
    if open(base + '.prog.abap', encoding='utf-8').read() != gen.source(side):
        bad.append(f"{v['PROGU']}.prog.abap differs from the generator's output - edit the parts "
                   f"in tools/ccej/bp_maint and run tools/ccej/gen_bp_maint.py")
    if open(base + '.prog.xml', encoding='utf-8').read() != gen.xml(side):
        bad.append(f"{v['PROGU']}.prog.xml differs from the generator's output")
if bad:
    print('\n'.join(bad))
    sys.exit(1)
print('clean - both CCEJ programs are exactly what the generator makes')
