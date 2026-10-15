#!/usr/bin/env python3
"""Generate sample Fiori Overview Page mockups (HTML) for FM, P2P and O2C.
Illustrative figures only - no system data, no programs. Output: deploy/Sample_Dashboard_<AREA>.html
Screenshots are taken separately with Playwright (tools/shot_samples.js)."""
import os, json, random

ROOT = os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
OUT = os.path.join(ROOT, "deploy")

CSS = """
  :root{--bg:#f5f6f7;--card:#fff;--line:#d9d9d9;--txt:#32363a;--muted:#6a6d70;--brand:#0a6ed1;--good:#107e3e;--warn:#e9730c;--bad:#bb0000;--shell:#354a5f;--s1:#5899da;--s2:#e8743b;--s3:#9fb96b;--s4:#8f79c5}
  *{box-sizing:border-box}
  body{margin:0;background:var(--bg);color:var(--txt);font:13px/1.35 "72","72full",Arial,Helvetica,sans-serif}
  .shell{height:44px;background:var(--shell);color:#fff;display:flex;align-items:center;padding:0 16px;gap:14px;font-size:15px}
  .shell .home{width:22px;height:22px;border:2px solid #fff;border-radius:3px}
  .shell .sp{flex:1}.shell .av{width:26px;height:26px;border-radius:50%;background:#7fa4c7}
  .head{background:#fff;border-bottom:1px solid var(--line);padding:10px 20px 8px}
  .head h1{margin:0 0 6px;font:400 20px/1.2 inherit}
  .fb{display:flex;gap:12px;flex-wrap:wrap;align-items:flex-end}
  .f{display:flex;flex-direction:column;gap:3px;font-size:12px;color:var(--muted)}
  .f input,.f select{height:30px;min-width:150px;border:1px solid #89919a;border-radius:4px;padding:0 8px;background:#fff;font:inherit;color:var(--txt)}
  .btns{margin-left:auto;display:flex;gap:8px;align-items:center}
  .b{height:30px;padding:0 10px;border:1px solid #0854a0;border-radius:4px;background:#fff;color:#0854a0;font:inherit;font-weight:600}
  .b.go{background:var(--brand);color:#fff;border-color:var(--brand)}
  .grid{padding:14px 20px;display:grid;grid-template-columns:repeat(auto-fill,320px);gap:14px;align-items:start}
  .card{background:var(--card);border:1px solid var(--line);border-radius:4px;box-shadow:0 0 2px rgba(0,0,0,.1);overflow:hidden}
  .card.w2{grid-column:span 2}
  .ch{padding:10px 12px 6px;border-bottom:1px solid #eee}
  .ch .t{font-size:15px;font-weight:600;color:var(--brand)}
  .ch .st{font-size:12px;color:var(--muted);margin-top:2px}
  .kpi{display:flex;align-items:baseline;gap:8px;padding:6px 12px 4px}
  .kpi .v{font-size:26px;font-weight:300}.kpi .u{font-size:12px;color:var(--muted)}
  .kpi .d{font-size:12px;margin-left:auto}
  .cb{padding:8px 12px 10px}
  .legend{display:flex;gap:14px;font-size:11px;color:var(--muted);margin:2px 0 4px;flex-wrap:wrap}
  .legend i{display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:4px;vertical-align:-1px}
  svg text{font-size:10px;fill:#6a6d70}
  table{width:100%;border-collapse:collapse;font-size:12px}
  th{text-align:left;color:var(--muted);font-weight:400;padding:4px 2px;border-bottom:1px solid #e5e5e5}
  td{padding:6px 2px;border-bottom:1px solid #f0f0f0;white-space:nowrap;overflow:hidden;text-overflow:ellipsis;max-width:160px}
  td.n,th.n{text-align:right}
  .g{color:var(--good)}.w{color:var(--warn)}.r{color:var(--bad)}
  .vm{font-size:12px;color:var(--brand);padding:6px 12px 10px;border-top:1px solid #eee}
  .list{margin:0;padding:0}.list li{list-style:none;display:flex;justify-content:space-between;padding:6px 0;border-bottom:1px solid #f0f0f0}
  .list .l{color:var(--muted);font-size:11px}
  .bar{height:6px;background:#e5e5e5;border-radius:3px;margin-top:4px}.bar i{display:block;height:6px;border-radius:3px;background:var(--s1)}
  .note{margin:6px 20px 18px;padding:10px 12px;background:#fff8e1;border:1px solid #f0d58c;border-radius:4px;font-size:12px}
  .note b{display:block;margin-bottom:3px}
  @media (max-width:700px){.grid{grid-template-columns:1fr}.card.w2{grid-column:span 1}}
"""

def head(title, filters):
    fs = "".join(f'<label class="f">{l}<input value="{v}"></label>' for l, v in filters)
    return f"""<div class="shell"><div class="home"></div><span>{title}</span><span class="sp"></span><div class="av"></div></div>
<div class="head"><h1>{title}</h1><div class="fb">{fs}<div class="btns"><button class="b">Adapt Filters</button><button class="b go">Go</button></div></div></div>"""

def card(title, sub, body, wide=False, more=True, kpi=None):
    k = ""
    if kpi:
        v, u, d, cls = kpi
        k = f'<div class="kpi"><span class="v">{v}</span><span class="u">{u}</span><span class="d {cls}">{d}</span></div>'
    return f"""<div class="card{' w2' if wide else ''}"><div class="ch"><div class="t">{title}</div><div class="st">{sub}</div></div>{k}<div class="cb">{body}</div>{'<div class="vm">View More ›</div>' if more else ''}</div>"""

def legend(items):
    return '<div class="legend">' + "".join(f'<span><i style="background:{c}"></i>{n}</span>' for n, c in items) + "</div>"

def table(headers, rows, num_cols):
    h = "".join(f'<th class="{"n" if i in num_cols else ""}">{x}</th>' for i, x in enumerate(headers))
    body = ""
    for r in rows:
        body += "<tr>" + "".join(
            f'<td class="{"n" if i in num_cols else ""}{(" " + c[1]) if isinstance(c, tuple) else ""}">{c[0] if isinstance(c, tuple) else c}</td>'
            for i, c in enumerate(r)) + "</tr>"
    return f"<table><tr>{h}</tr>{body}</table>"

def lines(series, labels, w=616, h=190, ymin=0, ymax=100, fmt=lambda v: str(v)):
    L, R, T, B = 44, 8, 8, 28
    n = len(labels)
    def x(i): return L + (w - L - R) * i / (n - 1)
    def y(v): return T + (h - T - B) * (1 - (v - ymin) / (ymax - ymin))
    s = ""
    for k in range(5):
        v = ymin + (ymax - ymin) * k / 4
        s += f'<line x1="{L}" y1="{y(v):.1f}" x2="{w-R}" y2="{y(v):.1f}" stroke="#e5e5e5"/><text x="{L-6}" y="{y(v)+3:.1f}" text-anchor="end">{fmt(v)}</text>'
    step = max(1, n // 6)
    for i in range(0, n, step):
        s += f'<text x="{x(i):.1f}" y="{h-10}" text-anchor="middle">{labels[i]}</text>'
    for vals, col in series:
        pts = " ".join(f"{x(i):.1f},{y(v):.1f}" for i, v in enumerate(vals))
        s += f'<polyline fill="none" stroke="{col}" stroke-width="2" points="{pts}"/>'
    return f'<svg width="{w}" height="{h}" viewBox="0 0 {w} {h}">{s}</svg>'

def columns(groups, labels, colors, w=296, h=150, ymax=100, fmt=lambda v: str(v)):
    L, R, T, B = 34, 6, 8, 22
    n = len(labels); m = len(colors)
    gw = (w - L - R) / n; bw = min(22, gw / (m + 1))
    def y(v): return T + (h - T - B) * (1 - v / ymax)
    s = ""
    for k in range(3):
        v = ymax * k / 2
        s += f'<line x1="{L}" y1="{y(v):.1f}" x2="{w-R}" y2="{y(v):.1f}" stroke="#e5e5e5"/><text x="{L-4}" y="{y(v)+3:.1f}" text-anchor="end">{fmt(v)}</text>'
    for i, g in enumerate(groups):
        x0 = L + gw * i + (gw - bw * m) / 2
        for j, v in enumerate(g):
            s += f'<rect x="{x0+bw*j:.1f}" y="{y(v):.1f}" width="{bw-2:.1f}" height="{h-B-y(v):.1f}" fill="{colors[j]}"/>'
        s += f'<text x="{L+gw*i+gw/2:.1f}" y="{h-6}" text-anchor="middle">{labels[i]}</text>'
    return f'<svg width="{w}" height="{h}" viewBox="0 0 {w} {h}">{s}</svg>'

def donut(parts, w=296, h=150):
    import math
    cx, cy, r, ri = 80, 75, 60, 38
    total = sum(v for _, v, _ in parts); a = -math.pi / 2; s = ""
    for name, v, col in parts:
        a2 = a + 2 * math.pi * v / total
        x1, y1 = cx + r * math.cos(a), cy + r * math.sin(a); x2, y2 = cx + r * math.cos(a2), cy + r * math.sin(a2)
        xi1, yi1 = cx + ri * math.cos(a2), cy + ri * math.sin(a2); xi2, yi2 = cx + ri * math.cos(a), cy + ri * math.sin(a)
        large = 1 if (a2 - a) > math.pi else 0
        s += f'<path d="M{x1:.1f},{y1:.1f} A{r},{r} 0 {large} 1 {x2:.1f},{y2:.1f} L{xi1:.1f},{yi1:.1f} A{ri},{ri} 0 {large} 0 {xi2:.1f},{yi2:.1f} Z" fill="{col}"/>'
        a = a2
    leg = "".join(f'<g transform="translate(160,{28+i*22})"><rect width="10" height="10" fill="{c}"/><text x="16" y="9">{n} · {v}%</text></g>' for i, (n, v, c) in enumerate(parts))
    return f'<svg width="{w}" height="{h}" viewBox="0 0 {w} {h}">{s}{leg}</svg>'

def listcard(items):
    return '<ul class="list">' + "".join(
        f'<li><span>{t}<br><span class="l">{d}</span></span><span class="{cls}">{v}</span></li>' for t, d, v, cls in items) + "</ul>"

def page(title, filters, cards, note, fname):
    html = f"""<!DOCTYPE html><html lang="en"><head><meta charset="utf-8"><title>{title}</title>
<meta name="viewport" content="width=device-width, initial-scale=1"><style>{CSS}</style></head><body>
{head(title, filters)}<div class="grid">{''.join(cards)}</div>
<div class="note"><b>Sample only.</b> {note} All figures are illustrative and not taken from any system.</div></body></html>"""
    with open(os.path.join(OUT, fname), "w", encoding="utf-8") as f:
        f.write(html)
    print("written", fname)

random.seed(11)
months = ["Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec", "Jan", "Feb", "Mar"]
K = lambda v: f"{v/1000:.0f}K" if v >= 1000 else f"{v:.0f}"
CR = lambda v: f"{v:.0f} Cr"

# ── 1. Finance / Funds Management ─────────────────────────────────────────────
budget = [520, 530, 540, 560, 580, 600, 610, 630, 650, 670, 690, 720]
commit = [410, 445, 470, 505, 535, 560, 575, 590, 0, 0, 0, 0]
actual = [380, 412, 440, 470, 498, 522, 0, 0, 0, 0, 0, 0]
fm_cards = [
    card("Budget vs Commitment vs Actual (cumulative)", "FY 2026-27 · all fund centres · INR Crore",
         legend([("Budget", "#5899da"), ("Commitment", "#e8743b"), ("Actual (invoiced)", "#9fb96b")]) +
         lines([(budget, "#5899da"), (commit[:8] + [None]*0, "#e8743b"), (actual[:6], "#9fb96b")][:1] +
               [(commit[:8], "#e8743b"), (actual[:6], "#9fb96b")], months, ymax=800, fmt=CR), wide=True, more=False),
    card("My Budget Overview", "Budget · consumed · available · FY 2026-27 YTD (as in SAP app My Budget Overview)",
         table(["Fund centre", "Consumed", "% of budget"],
               [["Exploration", "312 Cr", ("81 %", "r")], ["Development", "498 Cr", ("64 %", "w")], ["Corporate", "77 Cr", ("42 %", "g")]], {1, 2}),
         kpi=("887", "Cr consumed", "68 % of 1,305 Cr", "w")),
    card("Open Commitments by Type", "Purchase orders and contracts · INR Crore",
         legend([("PO", "#5899da"), ("Contract", "#e8743b"), ("Earmarked", "#9fb96b")]) +
         columns([[142, 88, 21], [96, 115, 12], [31, 12, 8]], ["Expl.", "Devel.", "Corp."], ["#5899da", "#e8743b", "#9fb96b"], ymax=160, fmt=CR)),
    card("Cash Position", "Bank balances by currency · today (as in SAP Cash Management)",
         donut([("USD", 58, "#5899da"), ("INR", 27, "#e8743b"), ("RUB", 9, "#9fb96b"), ("Other", 6, "#8f79c5")]),
         kpi=("1,942", "Cr equivalent", "▲ 3.1 % vs last week", "g")),
    card("Overdue Receivables from Partners", "Joint-venture cash calls · days overdue",
         listcard([("Sakhalin-1 JV", "Cash call CC-2026-014 · 45 days", "18.4 Cr", "r"),
                   ("ACG Consortium", "Cash call CC-2026-011 · 12 days", "6.2 Cr", "w"),
                   ("BC-10 JV", "Cash call CC-2026-016 · 3 days", "2.9 Cr", "")])),
    card("My Budget Alerts", "Fund centres above the 80 % consumption threshold (as in SAP app My Budget Alerts)",
         table(["Fund centre / item", "Consumed", "% of budget"],
               [["Exploration / Drilling", "221 Cr", ("94 %", "r")], ["Exploration / Seismic", "64 Cr", ("86 %", "w")], ["Development / Facilities", "138 Cr", ("79 %", "g")]], {1, 2})),
]
page("Finance & Funds Management Dashboard",
     [("Fiscal year*", "2026"), ("Fund*", "OVL-GENERAL"), ("Fund centre", "All"), ("Period to", "006 / 2026")],
     fm_cards, "Standard Fiori Overview Page cards on CDS analytical queries over FM (budget, commitment, actual), FI-GL and bank data.",
     "Sample_Dashboard_FM.html")

# ── 2. Procure-to-Pay ────────────────────────────────────────────────────────
days = [f"W{w}" for w in range(1, 13)]
p2p_cards = [
    card("PR to PO Cycle Time", "Weekly average · calendar days · FY 2026-27",
         legend([("Cycle time (days)", "#5899da"), ("Target 10 days", "#e8743b")]) +
         lines([([14, 13, 15, 12, 11, 12, 10, 11, 9, 10, 9, 8], "#5899da"), ([10]*12, "#e8743b")], days, ymax=20, fmt=lambda v: f"{v:.0f}"),
         wide=True, more=False),
    card("Purchase Requisition Items to be Processed", "By age · count (standard card of the SAP Procurement Overview Page)",
         table(["Age", "PRs", "Value"], [["> 30 days", ("42", "r"), "18.6 Cr"], ["15–30 days", ("67", "w"), "22.1 Cr"], ["< 15 days", "118", "31.4 Cr"]], {1, 2}),
         kpi=("227", "open PRs", "▼ 8 % vs last month", "g")),
    card("Purchasing Spend by Purchasing Group", "Released this fiscal year · INR Crore (standard card: Purchasing Spend)",
         columns([[310], [245], [172], [96]], ["Drilling", "Subsea", "Services", "IT"], ["#5899da"], ymax=400, fmt=CR)),
    card("Invoices Blocked for Payment", "By block reason",
         donut([("Price variance", 46, "#e8743b"), ("Qty variance", 31, "#5899da"), ("Missing GR", 17, "#9fb96b"), ("Other", 6, "#8f79c5")]),
         kpi=("138", "blocked invoices", "41.7 Cr held", "w")),
    card("Supplier Evaluation – On-Time Delivery", "Last 90 days · top suppliers by PO value (standard card: Supplier Evaluation)",
         table(["Vendor", "POs", "On time"], [["Halliburton", "38", ("92 %", "g")], ["Schlumberger", "31", ("88 %", "w")], ["TechnipFMC", "12", ("71 %", "r")]], {1, 2})),
    card("Overdue Purchase Order Items / Payments Due", "Next 7 days · cash discount at risk",
         listcard([("Baker Hughes", "Inv 90012345 · due 19.09 · 2 % discount", "6.8 Cr", ""),
                   ("Wood Group", "Inv 90012401 · due 21.09", "3.1 Cr", ""),
                   ("Aker Solutions", "Inv 90012388 · overdue 4 days", "1.9 Cr", "r")])),
]
page("Procure-to-Pay Dashboard",
     [("Fiscal year*", "2026"), ("Purchasing org*", "OVL1"), ("Purchasing group", "All"), ("Vendor", "All")],
     p2p_cards, "Standard Fiori Overview Page cards on CDS analytical queries over MM purchasing (EBAN, EKKO/EKPO), goods receipts and MM/FI invoices.",
     "Sample_Dashboard_P2P.html")

# ── 3. Order-to-Cash ─────────────────────────────────────────────────────────
o2c_cards = [
    card("Sales Volume vs Plan", "Crude and gas · monthly · USD million (as in SAP Sales Management Overview)",
         legend([("Revenue", "#5899da"), ("Plan", "#e8743b")]) +
         lines([([118, 124, 131, 127, 135, 142], "#5899da"), ([120, 122, 125, 128, 130, 133, 135, 138, 140, 142, 145, 148], "#e8743b")], months, ymax=200, fmt=lambda v: f"{v:.0f} M"),
         wide=True, more=False),
    card("Overdue Receivables and DSO", "Open customer items · USD million (as in SAP Accounts Receivable Overview)",
         table(["Bucket", "Amount", "Share"], [["Not due", "212 M", ("61 %", "g")], ["1–30 days", "84 M", ("24 %", "w")], ["> 30 days", "52 M", ("15 %", "r")]], {1, 2}),
         kpi=("348", "M open", "DSO 41 days", "w")),
    card("Liftings vs Nomination", "Current month · thousand barrels",
         legend([("Nominated", "#e8743b"), ("Lifted", "#5899da")]) +
         columns([[980, 940], [620, 655], [410, 380]], ["Sakhalin", "ACG", "Brazil"], ["#e8743b", "#5899da"], ymax=1200, fmt=K)),
    card("Sales by Customer", "Fiscal year to date · share of revenue",
         donut([("Refinery A", 38, "#5899da"), ("Trader B", 27, "#e8743b"), ("Refinery C", 21, "#9fb96b"), ("Others", 14, "#8f79c5")])),
    card("Sales Order Fulfillment Issues", "Orders with delivery or billing issues (standard SAP app Sales Order Fulfillment)",
         table(["Status", "Orders", "Value"], [["Delivered, not billed", ("14", "w"), "96 M"], ["Blocked", ("3", "r"), "27 M"], ["Open", "22", "184 M"]], {1, 2}),
         kpi=("39", "orders", "307 M", "")),
    card("Overdue Customers", "Top overdue balances",
         listcard([("Trader B", "Inv 4500123 · 38 days overdue", "18.2 M", "r"),
                   ("Refinery C", "Inv 4500119 · 12 days overdue", "9.4 M", "w"),
                   ("Refinery A", "Inv 4500130 · 2 days overdue", "4.1 M", "")])),
]
page("Order-to-Cash Dashboard",
     [("Fiscal year*", "2026"), ("Sales org*", "OVL1"), ("Customer", "All"), ("Material", "Crude / Gas")],
     o2c_cards, "Standard Fiori Overview Page cards on CDS analytical queries over SD sales orders, deliveries, billing and FI-AR open items.",
     "Sample_Dashboard_O2C.html")
