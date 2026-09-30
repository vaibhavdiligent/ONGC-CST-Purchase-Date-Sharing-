"""Reads ABAP source as statements - comments, literals and string templates
blanked, chains split - for the audits that need more than a line at a time.
"""
import re


def statements(src):
    """(line, text) per statement, literals blanked and comments dropped."""
    out, buf, line0 = [], [], None
    lines = src.split('\n')
    mode, depth = 'code', 0          # code | lit ' | bq ` | tpl |
    stack = []                       # template nesting: brace depth per level
    for ln, raw in enumerate(lines, 1):
        if mode == 'code' and not stack and raw.startswith('*'):
            continue
        i = 0
        while i < len(raw):
            ch = raw[i]
            if mode == 'lit':
                if ch == "'":
                    if raw[i + 1:i + 2] == "'":
                        i += 2; continue
                    mode = 'code'
                i += 1; continue
            if mode == 'bq':
                if ch == '`':
                    if raw[i + 1:i + 2] == '`':
                        i += 2; continue
                    mode = 'code'
                i += 1; continue
            if mode == 'tpl':
                if ch == '\\':
                    i += 2; continue
                if ch == '|':
                    mode = 'code'; stack.pop()
                    buf.append(' '); i += 1; continue
                if ch == '{':
                    mode = 'code'; stack[-1] += 1
                i += 1; continue
            # code
            if ch == '"':
                break
            if ch == "'":
                mode = 'lit'; buf.append('L'); i += 1; continue
            if ch == '`':
                mode = 'bq'; buf.append('L'); i += 1; continue
            if ch == '|':
                mode = 'tpl'; stack.append(0); buf.append('T '); i += 1; continue
            if ch == '}' and stack and stack[-1] > 0:
                stack[-1] -= 1; mode = 'tpl'; i += 1; continue
            if ch == '.' and not stack:
                # a decimal point is not a full stop
                nxt = raw[i + 1:i + 2]
                if not (nxt.isdigit() and buf and buf[-1][-1:].isdigit()):
                    text = ''.join(buf).strip()
                    if text:
                        out.append((line0 or ln, text))
                    buf, line0 = [], None
                    i += 1; continue
            if line0 is None and not ch.isspace():
                line0 = ln
            buf.append(ch)
            i += 1
        if mode == 'lit' or mode == 'bq':
            mode = 'code'            # a literal never spans lines
        buf.append(' ')
    return out


def unchain(text):
    """A chained statement is several: DATA: a TYPE i, b TYPE i."""
    m = re.match(r'([\w-]+(?:\s+[\w-]+)?)\s*:(?!=)(.*)$', text, re.S)
    if not m:
        return [text]
    head, rest = m.group(1), m.group(2)
    parts, depth, cur = [], 0, ''
    for ch in rest:
        if ch in '([':
            depth += 1
        elif ch in ')]':
            depth -= 1
        if ch == ',' and depth == 0:
            parts.append(cur); cur = ''
        else:
            cur += ch
    parts.append(cur)
    return [f'{head} {p.strip()}' for p in parts if p.strip()]
