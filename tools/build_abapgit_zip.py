#!/usr/bin/env python3
"""Build the abapGit-offline ZIP of package ZPR_DPR_RAP from src/rap/.

Usage:  python3 tools/build_abapgit_zip.py [--with-ddlx]
Output: deploy/ZPR_DPR_RAP_abapgit.zip

Default excludes the 5 metadata extensions (they are created in ADT on the
customer system - abapGit could not import DDLX there) and always excludes
the service binding (created with the ADT wizard). Sidecar XML formats are
copied from SAP-samples/abap-platform-rap100 as serialised by abapGit.
"""
import os, re, sys, zipfile

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
SRC = os.path.join(ROOT, "src", "rap")
OUT = os.path.join(ROOT, "deploy", "ZPR_DPR_RAP_abapgit.zip")
WITH_DDLX = "--with-ddlx" in sys.argv
BOM = "﻿"

def read(fn):
    with open(os.path.join(SRC, fn), encoding="utf-8") as f:
        return f.read()

def label(src, fallback):
    m = re.search(r"@EndUserText\.label:\s*'([^']*)'", src)
    return (m.group(1) if m else fallback)[:60].replace("&", "&amp;") \
        .replace("<", "&lt;").replace(">", "&gt;")

def wrap(serializer, inner):
    return (BOM + '<?xml version="1.0" encoding="utf-8"?>\n'
            f'<abapGit version="v1.0.0" serializer="{serializer}" serializer_version="v1.0.0">\n'
            ' <asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n'
            '  <asx:values>\n' + inner +
            '  </asx:values>\n </asx:abap>\n</abapGit>\n')

def ddls_xml(name, ddtext, source_type):
    st = f"    <SOURCE_TYPE>{source_type}</SOURCE_TYPE>\n" if source_type else ""
    return wrap("LCL_OBJECT_DDLS",
        "   <DDLS>\n"
        f"    <DDLNAME>{name}</DDLNAME>\n"
        "    <DDLANGUAGE>E</DDLANGUAGE>\n"
        f"    <DDTEXT>{ddtext}</DDTEXT>\n" + st +
        "   </DDLS>\n")

def ddlx_xml(name, desc):
    return wrap("LCL_OBJECT_DDLX",
        "   <DDLX>\n    <METADATA>\n"
        f"     <NAME>{name}</NAME>\n"
        f"     <DESCRIPTION>{desc}</DESCRIPTION>\n"
        "     <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>\n"
        "    </METADATA>\n   </DDLX>\n")

def bdef_xml(name, desc):
    return wrap("LCL_OBJECT_BDEF",
        "   <BDEF>\n"
        f"    <NAME>{name}</NAME>\n"
        "    <TYPE>BDEF/BDO</TYPE>\n"
        f"    <DESCRIPTION>{desc}</DESCRIPTION>\n"
        "    <DESCRIPTION_TEXT_LIMIT>60</DESCRIPTION_TEXT_LIMIT>\n"
        "    <LANGUAGE>EN</LANGUAGE>\n"
        "    <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>\n"
        "    <SOURCE_TYPE>ABAP_SOURCE</SOURCE_TYPE>\n"
        "    <SOURCE_FIXED_POINT_ARITHMETIC>true</SOURCE_FIXED_POINT_ARITHMETIC>\n"
        "    <SOURCE_UNICODE_CHECKS_ACTIVE>true</SOURCE_UNICODE_CHECKS_ACTIVE>\n"
        "   </BDEF>\n")

def srvd_xml(name, desc):
    return wrap("LCL_OBJECT_SRVD",
        "   <SRVD>\n"
        f"    <NAME>{name}</NAME>\n"
        "    <TYPE>SRVD/SRV</TYPE>\n"
        f"    <DESCRIPTION>{desc}</DESCRIPTION>\n"
        "    <LANGUAGE>EN</LANGUAGE>\n"
        "    <MASTER_LANGUAGE>EN</MASTER_LANGUAGE>\n"
        "    <SOURCE_TYPE>ABAP_SOURCE</SOURCE_TYPE>\n"
        "    <SRVD_SOURCE_TYPE>S</SRVD_SOURCE_TYPE>\n"
        "    <SRVD_SOURCE_TYPE_DESC>Definition</SRVD_SOURCE_TYPE_DESC>\n"
        "   </SRVD>\n")

def clas_xml(name, desc, category=None, clsdefint=None):
    extra = f"    <CATEGORY>{category}</CATEGORY>\n" if category else ""
    inner = ("   <VSEOCLASS>\n"
        f"    <CLSNAME>{name}</CLSNAME>\n"
        "    <LANGU>E</LANGU>\n"
        f"    <DESCRIPT>{desc}</DESCRIPT>\n" + extra +
        "    <STATE>1</STATE>\n"
        "    <CLSCCINCL>X</CLSCCINCL>\n"
        "    <FIXPT>X</FIXPT>\n"
        "    <UNICODE>X</UNICODE>\n")
    if clsdefint:
        inner += f"    <CLSDEFINT>{clsdefint}</CLSDEFINT>\n"
    return wrap("LCL_OBJECT_CLAS", inner + "   </VSEOCLASS>\n")

files = {}

files[".abapgit.xml"] = (BOM + '<?xml version="1.0" encoding="utf-8"?>\n'
    '<asx:abap xmlns:asx="http://www.sap.com/abapxml" version="1.0">\n'
    ' <asx:values>\n  <DATA>\n'
    '   <MASTER_LANGUAGE>E</MASTER_LANGUAGE>\n'
    '   <STARTING_FOLDER>/src/</STARTING_FOLDER>\n'
    '   <FOLDER_LOGIC>PREFIX</FOLDER_LOGIC>\n'
    '  </DATA>\n </asx:values>\n</asx:abap>\n')

files["src/package.devc.xml"] = wrap("LCL_OBJECT_DEVC",
    "   <DEVC>\n    <CTEXT>ONGC Videsh DPR - Analytical RAP</CTEXT>\n   </DEVC>\n")

# every DDLS in src/rap - view entities get SOURCE_TYPE W, classic views and
# abstract entities none (the DDL parser derives the kind)
for fn in sorted(os.listdir(SRC)):
    if not fn.endswith(".ddls.asddls"):
        continue
    name = fn[:-len(".ddls.asddls")]
    src = read(fn)
    st = "W" if re.search(r"define\s+(root\s+)?view\s+entity", src) else ""
    low = name.lower()
    files[f"src/{low}.ddls.asddls"] = src
    files[f"src/{low}.ddls.xml"] = ddls_xml(name, label(src, name), st)

if WITH_DDLX:
    for fn in sorted(os.listdir(SRC)):
        if fn.endswith(".ddlx.asddlx"):
            name = fn[:-len(".ddlx.asddlx")]
            files[f"src/{name.lower()}.ddlx.asddlx"] = read(fn)
            files[f"src/{name.lower()}.ddlx.xml"] = ddlx_xml(name, f"UI annotations for {name}"[:60])

# BDEF: object name = root entity
files["src/zdpr_i_excel_dl.bdef.asbdef"] = read("ZDPR_I_EXCEL_DL.bdef.asbdef")
files["src/zdpr_i_excel_dl.bdef.xml"] = bdef_xml("ZDPR_I_EXCEL_DL", "DPR Excel/PDF download actions")

for name, desc in [("ZCL_ZDPR_EXCEL", "DPR Excel export (abap2xlsx)"),
                   ("ZCL_ZDPR_PDF", "DPR PDF export")]:
    files[f"src/{name.lower()}.clas.abap"] = read(name + ".clas.abap")
    files[f"src/{name.lower()}.clas.xml"] = clas_xml(name, desc)

# behavior implementation: global class -> main include, lhc_* -> locals_imp
zbp = read("ZBP_ZDPR_EXCEL_DL.clas.abap")
m = re.search(r"^CLASS\s+lhc_", zbp, re.M)
files["src/zbp_zdpr_excel_dl.clas.abap"] = zbp[:m.start()].rstrip() + "\n"
files["src/zbp_zdpr_excel_dl.clas.locals_imp.abap"] = zbp[m.start():]
files["src/zbp_zdpr_excel_dl.clas.xml"] = clas_xml(
    "ZBP_ZDPR_EXCEL_DL", "Behavior implementation for ZDPR_I_EXCEL_DL",
    category="06", clsdefint="ZDPR_I_EXCEL_DL")

files["src/zdpr_sd_analytics.srvd.srvdsrv"] = read("ZDPR_SD_ANALYTICS.srvd.asddls")
files["src/zdpr_sd_analytics.srvd.xml"] = srvd_xml("ZDPR_SD_ANALYTICS", "DPR Analytics - Service Definition")

os.makedirs(os.path.dirname(OUT), exist_ok=True)
with zipfile.ZipFile(OUT, "w", zipfile.ZIP_DEFLATED) as z:
    for p in sorted(files):
        z.writestr(p, files[p].encode("utf-8"))
print(f"{OUT}: {len(files)} files, {os.path.getsize(OUT)} bytes"
      f" ({'with' if WITH_DDLX else 'without'} DDLX)")
