import re
from math import gcd


def normalize_line(s: str) -> str:
    return re.sub(r"\s+", " ", s.strip().lower())


def is_affine_term(expr: str, var: str):
    m = re.fullmatch(rf"{var}\s*([+-]\s*\d+)?", expr.strip(), re.IGNORECASE)
    if m:
        if m.group(1):
            sign = 1 if "+" in m.group(1) else -1
            return 1, sign * int(re.sub(r"[^0-9]", "", m.group(1)))
        return 1, 0
    m2 = re.fullmatch(rf"(\d+)\s*\*\s*{var}\s*([+-]\s*\d+)?", expr.strip(), re.IGNORECASE)
    if m2:
        coeff = int(m2.group(1))
        if m2.group(2):
            sign = 1 if "+" in m2.group(2) else -1
            return coeff, sign * int(re.sub(r"[^0-9]", "", m2.group(2)))
        return coeff, 0
    return None


def is_variable_coeff_term(expr: str, var: str):
    m = re.fullmatch(rf"([A-Za-z_]\w*)\s*\*\s*{var}\s*([+-]\s*\d+)?", expr.strip(), re.IGNORECASE)
    if m:
        coef_var = m.group(1)
        return coef_var
    return None


def parse_subscripts_text(section_text: str) -> list:
    inner = section_text.strip()
    if inner.startswith("(") and inner.endswith(")"):
        inner = inner[1:-1]
    parts = []
    depth = 0
    buf = []
    for c in inner:
        if c == "," and depth == 0:
            parts.append("".join(buf).strip())
            buf = []
            continue
        if c in "(":
            depth += 1
        elif c in ")":
            depth -= 1
        buf.append(c)
    if buf:
        parts.append("".join(buf).strip())
    return parts


def find_k_feasible(cw: int, kw: int, cr: int, kr: int, lb: int | None, ub: int | None, max_k: int = 16):
    if cw == cr:
        k = kw - kr
        return k if k != 0 else 0
    for k in range(-max_k, max_k + 1):
        if k == 0:
            continue
        num = cr * k + (kr - kw)
        den = (cw - cr)
        if den == 0:
            continue
        if num % den == 0:
            i = num // den
            if lb is None or ub is None or (lb <= i <= ub):
                return k
    return None