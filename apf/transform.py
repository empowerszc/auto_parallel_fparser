from typing import List, Tuple, Optional

from .ir import AnalysisResult
from .utils import normalize_line


class TransformOptions:
    """Options for generating OpenMP directives.

    Args:
        style: One of 'parallel_do', 'do', 'parallel_region'.
        schedule: OpenMP schedule clause (e.g., 'static', 'dynamic', None).
        collapse: Integer for collapse clause when nesting > 1.
    """
    def __init__(self, style: str = "parallel_do", schedule: Optional[str] = "static", collapse: Optional[int] = None):
        self.style = style
        self.schedule = schedule
        self.collapse = collapse


def insert_openmp_directives(src_lines: List[str], loop_start: str, loop_end: str, clauses: str, options: Optional[TransformOptions] = None, nest_depth: int = 1) -> List[str]:
    """Insert OpenMP directives around a DO loop.

    Args:
        src_lines: Original source lines.
        loop_start: Exact DO header text (from AST).
        loop_end: Exact END DO text (from AST).
        clauses: Clauses string (from `build_omp_clauses`).
        options: TransformOptions controlling style/schedule/collapse.
        nest_depth: Nesting depth for potential collapse.

    Returns:
        New list of source lines with directives inserted.

    Example:
        >>> insert_openmp_directives(['do i=1,2','end do'], 'DO i = 1, 2', 'END DO', 'private(i)')
        ['!$omp parallel do private(i)', 'do i=1,2', 'end do', '!$omp end parallel do']
    """
    nsrc = [l.rstrip("\n") for l in src_lines]
    start_idx, end_idx = find_loop_range(src_lines, loop_start, loop_end)
    if start_idx is None or end_idx is None:
        return src_lines
    options = options or TransformOptions()
    extra = []
    if options.schedule:
        extra.append(f"schedule({options.schedule})")
    if options.collapse and options.collapse > 1:
        extra.append(f"collapse({options.collapse})")
    extra_str = (" " + " ".join(extra)) if extra else ""
    if options.style == "parallel_do":
        omp_start = f"!$omp parallel do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_end = "!$omp end parallel do"
        nsrc.insert(start_idx, omp_start)
        nsrc.insert(end_idx + 2, omp_end)
    elif options.style == "do":
        omp_start = f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_end = "!$omp end do"
        nsrc.insert(start_idx, omp_start)
        nsrc.insert(end_idx + 2, omp_end)
    elif options.style == "parallel_region":
        omp_par_start = "!$omp parallel"
        omp_par_end = "!$omp end parallel"
        omp_do_start = f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_do_end = "!$omp end do"
        nsrc.insert(start_idx, omp_par_start)
        nsrc.insert(start_idx + 1, omp_do_start)
        nsrc.insert(end_idx + 2, omp_do_end)
        nsrc.insert(end_idx + 3, omp_par_end)
    return [l + "\n" for l in nsrc]


def build_omp_clauses(ar: AnalysisResult) -> str:
    """Build OpenMP clauses from analysis result.

    Args:
        ar: AnalysisResult for a loop.

    Returns:
        String of space-separated clauses (private, firstprivate, lastprivate, reduction).
    """
    clauses = []
    if ar.private_vars:
        clauses.append(f"private({', '.join(ar.private_vars)})")
    if ar.firstprivate_vars:
        clauses.append(f"firstprivate({', '.join(ar.firstprivate_vars)})")
    if ar.lastprivate_vars:
        clauses.append(f"lastprivate({', '.join(ar.lastprivate_vars)})")
    if ar.reduction_vars:
        red = []
        for v, kind in ar.reduction_vars.items():
            op = "+" if kind == "sum" else "*"
            red.append(f"{op}:{v}")
        clauses.append(f"reduction({', '.join(red)})")
    return " ".join(clauses)
def find_loop_range(src_lines: List[str], loop_start: str, loop_end: str):
    """Locate DO/END DO lines by matching normalized text.

    Args:
        src_lines: Source lines.
        loop_start: DO header from AST.
        loop_end: END DO from AST.

    Returns:
        Tuple `(start_idx, end_idx)` or `(None, None)` if not found.
    """
    nsrc = [l.rstrip("\n") for l in src_lines]
    target_start = normalize_line(loop_start)
    start_idx = None
    for idx, line in enumerate(nsrc):
        if normalize_line(line).startswith(target_start):
            start_idx = idx
            break
    if start_idx is None:
        return None, None
    end_idx = None
    for idx in range(start_idx + 1, len(nsrc)):
        if normalize_line(nsrc[idx]).startswith(normalize_line(loop_end)):
            end_idx = idx
            break
    return start_idx, end_idx