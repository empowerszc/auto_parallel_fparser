from typing import List, Tuple, Optional
from fparser.two.Fortran2003 import Comment, Directive, Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt, Assignment_Stmt, Data_Ref
from fparser.common.readfortran import FortranStringReader

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


def insert_openmp_directives(src_lines: List[str], loop_start: str, loop_end: str, clauses: str, options: Optional[TransformOptions] = None, nest_depth: int = 1, nth: Optional[int] = None) -> List[str]:
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
    if nth is not None:
        start_idx, end_idx = find_loop_range_nth(src_lines, loop_start, loop_end, nth)
    else:
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
    def has_omp_at(idx: int) -> bool:
        if idx < 0 or idx >= len(nsrc):
            return False
        return nsrc[idx].strip().lower().startswith("!$omp")
    if options.style == "parallel_do":
        omp_start = f"!$omp parallel do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_end = "!$omp end parallel do"
        if not has_omp_at(start_idx - 1):
            nsrc.insert(start_idx, omp_start)
            end_idx += 1
        if not has_omp_at(end_idx + 1):
            nsrc.insert(end_idx + 2, omp_end)
    elif options.style == "do":
        omp_start = f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_end = "!$omp end do"
        if not has_omp_at(start_idx - 1):
            nsrc.insert(start_idx, omp_start)
            end_idx += 1
        if not has_omp_at(end_idx + 1):
            nsrc.insert(end_idx + 2, omp_end)
    elif options.style == "parallel_region":
        omp_par_start = "!$omp parallel"
        omp_par_end = "!$omp end parallel"
        omp_do_start = f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_do_end = "!$omp end do"
        if not has_omp_at(start_idx - 1):
            nsrc.insert(start_idx, omp_par_start)
            start_idx += 1
            end_idx += 1
        if not has_omp_at(start_idx):
            nsrc.insert(start_idx, omp_do_start)
            end_idx += 1
        if not has_omp_at(end_idx + 1):
            nsrc.insert(end_idx + 2, omp_do_end)
        if not has_omp_at(end_idx + 3):
            nsrc.insert(end_idx + 3, omp_par_end)
    return [l + "\n" for l in nsrc]


def _build_omp_line(kind: str, clauses: str, options: TransformOptions) -> str:
    extra = []
    if options.schedule:
        extra.append(f"schedule({options.schedule})")
    if options.collapse and options.collapse > 1:
        extra.append(f"collapse({options.collapse})")
    extra_str = (" " + " ".join(extra)) if extra else ""
    if kind == "parallel_do_start":
        return f"!$omp parallel do{(' ' + clauses) if clauses else ''}{extra_str}"
    if kind == "parallel_do_end":
        return "!$omp end parallel do"
    if kind == "do_start":
        return f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
    if kind == "do_end":
        return "!$omp end do"
    if kind == "parallel_start":
        return "!$omp parallel"
    if kind == "parallel_end":
        return "!$omp end parallel"
    return ""


def insert_openmp_directives_ast(loop_node: Block_Nonlabel_Do_Construct, clauses: str, options: Optional[TransformOptions] = None):
    # 将 OpenMP 指令以 AST 节点插入到 DO 循环周围，保持树结构可序列化
    options = options or TransformOptions()
    # 上溯找到拥有 content 列表的父节点（通常为 Execution_Part）
    parent = getattr(loop_node, "parent", None)
    while parent is not None and not hasattr(parent, "content"):
        parent = getattr(parent, "parent", None)
    if parent is None:
        return
    # 复制 content 列表并定位 DO 构造所在索引
    pc = list(getattr(parent, "content", []))
    idx = None
    for i, n in enumerate(pc):
        if n is loop_node:
            idx = i
            break
    if idx is None:
        return
    # 通过 FortranStringReader + Directive 保证插入的是合法可序列化的指令节点
    def _dir(text: str):
        r = FortranStringReader(text + "\n", ignore_comments=False, process_directives=True)
        return Directive(r)
    if options.style == "parallel_do":
        s = _build_omp_line("parallel_do_start", clauses, options)
        e = _build_omp_line("parallel_do_end", clauses, options)
        # 在 DO 前插入开始指令，在整个循环节点之后插入结束指令
        pc.insert(idx, _dir(s))
        pc.insert(idx + 2, _dir(e))
    elif options.style == "do":
        s = _build_omp_line("do_start", clauses, options)
        e = _build_omp_line("do_end", clauses, options)
        pc.insert(idx, _dir(s))
        pc.insert(idx + 2, _dir(e))
    elif options.style == "parallel_region":
        ps = _build_omp_line("parallel_start", clauses, options)
        ds = _build_omp_line("do_start", clauses, options)
        de = _build_omp_line("do_end", clauses, options)
        pe = _build_omp_line("parallel_end", clauses, options)
        # 插入顺序：parallel -> do -> 循环 -> end do -> end parallel
        pc.insert(idx, _dir(ps))
        pc.insert(idx + 1, _dir(ds))
        pc.insert(idx + 3, _dir(de))
        pc.insert(idx + 4, _dir(pe))
    parent.content = pc


def insert_atomic_update_for_derived(loop_node: Block_Nonlabel_Do_Construct):
    # 对派生类型的自增/自加赋值在语句前插入 OpenMP atomic update（AST 方式）
    content = list(getattr(loop_node, "content", []))
    new_content = []
    for n in content:
        new_content.append(n)
        if isinstance(n, Assignment_Stmt):
            items = getattr(n, "items", [])
            lhs = items[0] if items else None
            rhs = items[2] if len(items) > 2 else None
            if isinstance(lhs, Data_Ref):
                txt = str(n).lower()
                if "+" in txt and str(lhs).lower() in txt:
                    r = FortranStringReader("!$omp atomic update\n", ignore_comments=False, process_directives=True)
                    new_content.insert(len(new_content) - 1, Directive(r))
    loop_node.content = new_content


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


def find_loop_range_nth(src_lines: List[str], loop_start: str, loop_end: str, nth: int):
    nsrc = [l.rstrip("\n") for l in src_lines]
    target_start = normalize_line(loop_start)
    count = 0
    start_idx = None
    for idx, line in enumerate(nsrc):
        if normalize_line(line).startswith(target_start):
            count += 1
            if count == nth:
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