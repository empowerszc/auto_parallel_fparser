import re
from math import gcd


def normalize_line(s: str) -> str:
    """Normalize a source line for textual matching.

    Args:
        s: A single line of Fortran source.

    Returns:
        The line lowercased with consecutive spaces collapsed to one.

    Example:
        >>> normalize_line('  DO  i = 1, N  ')
        'do i = 1, n'
    """
    return re.sub(r"\s+", " ", s.strip().lower())


def is_affine_term(expr: str, var: str):
    """Check whether an index expression is affine in a loop variable.

    Affine form supported: `var [+/- c]` or `c*var [+/- d]` where c,d are integers.

    Args:
        expr: Index expression text, e.g. 'i+1', '2*j-3'.
        var: Loop variable name, e.g. 'i'.

    Returns:
        Tuple `(coeff, offset)` if affine, else None.

    Example:
        >>> is_affine_term('i+2', 'i')
        (1, 2)
        >>> is_affine_term('3*i-1', 'i')
        (3, -1)
        >>> is_affine_term('i*j', 'i') is None
        True
    """
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
    """Detect non-constant coefficient terms like 'a*i' or 'i*a'.

    Args:
        expr: Index expression text.
        var: Loop variable name.

    Returns:
        The symbolic coefficient token (e.g., 'a' or '(k+1)') when found; otherwise None.

    Example:
        >>> is_variable_coeff_term('a*i+1', 'i')
        'a'
        >>> is_variable_coeff_term('i*(k+1)', 'i')
        '(k+1)'
    """
    s = expr.strip()
    m1 = re.fullmatch(rf"([A-Za-z_]\w*|\([^\)]*\))\s*\*\s*{var}(\s*[+-]\s*\d+)?", s, re.IGNORECASE)
    if m1:
        return m1.group(1)
    m2 = re.fullmatch(rf"{var}\s*\*\s*([A-Za-z_]\w*|\([^\)]*\))(\s*[+-]\s*\d+)?", s, re.IGNORECASE)
    if m2:
        return m2.group(1)
    return None


def has_multiplicative_var(expr: str, var: str):
    """Return True if expression contains '*' and references the loop variable.

    This flags non-affine multiplicative coupling like 'i*j'.

    Args:
        expr: Index expression text.
        var: Loop variable name.

    Example:
        >>> has_multiplicative_var('i*j', 'i')
        True
    """
    s = expr.strip()
    if "*" in s and re.search(rf"\b{var}\b", s, re.IGNORECASE):
        return True
    return False


def is_symbolic_offset_term(expr: str, var: str):
    """Detect symbolic offsets added to a loop variable, e.g. 'sym + i' or 'i - sym'.

    Args:
        expr: Index expression.
        var: Loop variable.

    Returns:
        Tuple `(symbol, op)` where op is '+' or '-' when matched; otherwise None.

    Example:
        >>> is_symbolic_offset_term('k+i', 'i')
        ('k', '+')
    """
    s = expr.strip()
    patt = rf"(\([^\)]*\)|[A-Za-z_]\w*)\s*(\+|-)\s*{var}"
    m1 = re.fullmatch(patt, s, re.IGNORECASE)
    if m1:
        return m1.group(1), m1.group(2)
    patt2 = rf"{var}\s*(\+|-)\s*(\([^\)]*\)|[A-Za-z_]\w*)"
    m2 = re.fullmatch(patt2, s, re.IGNORECASE)
    if m2:
        return m2.group(2), m2.group(1)
    return None


def apply_constants(expr: str, consts: dict) -> str:
    """Substitute known scalar constants into an index expression.

    Args:
        expr: Original expression text.
        consts: Mapping from symbol name to literal value (strings).

    Returns:
        Expression with occurrences of keys replaced by their values.

    Example:
        >>> apply_constants('i+k', {'k': '2'})
        'i+2'
    """
    s = expr
    try:
        for k, v in consts.items():
            s = re.sub(rf"\b{re.escape(k)}\b", str(v), s)
    except Exception:
        return expr
    return s


def is_function_like(expr: str) -> bool:
    """Heuristically detect function-like calls inside subscripts.

    Args:
        expr: Expression text.

    Returns:
        True if it matches `<ident>(...)`, else False.
    """
    s = expr.strip()
    return bool(re.match(r"^[A-Za-z_]\w*\s*\(.*\)$", s))


def parse_subscripts_text(section_text: str) -> list:
    """Split a Fortran Section_Subscript_List into individual index expressions.

    Handles nested parentheses so commas inside function calls or slices are preserved.

    Args:
        section_text: Raw text like '(i, j+1, idx(k,l))'.

    Returns:
        List of index expression strings, without outer parentheses.

    Example:
        >>> parse_subscripts_text('(i, j+1, idx(k,l))')
        ['i', 'j+1', 'idx(k,l)']
    """
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


def find_k_feasible(cw: int, kw: int, cr: int, kr: int, lb: int | None, ub: int | None, stride: int | None = 1, max_k: int = 64):
    """Search for a feasible dependence distance `k` when coefficients differ.

    Solves `cw*i + kw = cr*(i + k) + kr` for integer `i` within loop bounds.

    Args:
        cw, kw: Coefficient and offset for write index.
        cr, kr: Coefficient and offset for read index.
        lb, ub: Loop bounds for `i` (inclusive), if known.
        stride: Loop stride. When >1, enforces `(i - lb) % stride == 0`.
        max_k: Search window for k (symmetric range).

    Returns:
        Integer k when feasible; 0 when equal coefficients yield k=kw-kr; None if unknown.

    Example:
        >>> find_k_feasible(2, 0, 1, 0, 1, 10)
        1
    """
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
                if stride and stride > 1 and lb is not None:
                    if (i - lb) % stride != 0:
                        continue
                return k
    return None