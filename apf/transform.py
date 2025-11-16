from typing import List, Tuple, Optional
from fparser.two.Fortran2003 import Comment, Directive, Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt, Assignment_Stmt, Data_Ref, Type_Declaration_Stmt
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
            nsrc.insert(end_idx + 1, omp_end)
    elif options.style == "do":
        omp_start = f"!$omp do{(' ' + clauses) if clauses else ''}{extra_str}"
        omp_end = "!$omp end do"
        if not has_omp_at(start_idx - 1):
            nsrc.insert(start_idx, omp_start)
            end_idx += 1
        if not has_omp_at(end_idx + 1):
            nsrc.insert(end_idx + 1, omp_end)
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
        if isinstance(n, Assignment_Stmt):
            items = getattr(n, "items", [])
            lhs = items[0] if items else None
            rhs = items[2] if len(items) > 2 else None
            if isinstance(lhs, Data_Ref):
                lhs_txt = str(lhs).lower()
                rhs_txt = str(rhs).lower() if rhs is not None else ""
                if "+" in rhs_txt and lhs_txt in rhs_txt:
                    r = FortranStringReader("!$omp atomic update\n", ignore_comments=False, process_directives=True)
                    new_content.append(Directive(r))
                    new_content.append(n)
                    continue
        new_content.append(n)
    loop_node.content = new_content


def _find_parent_with_content(node):
    p = getattr(node, "parent", None)
    while p is not None and not hasattr(p, "content"):
        p = getattr(p, "parent", None)
    return p


def _infer_component_type(root, comp_name: str) -> str:
    # 尝试从类型定义中推断成员的基本类型（REAL/INTEGER 等），失败则默认 REAL
    try:
        from fparser.two.utils import walk
        from fparser.two.Fortran2003 import Type_Declaration_Stmt, Entity_Decl
        for td in walk(root, Type_Declaration_Stmt):
            txt = str(td).strip().upper()
            for e in walk(td, Entity_Decl):
                try:
                    nm = e.items[0].string.lower()
                except Exception:
                    nm = None
                if nm == comp_name.lower():
                    # 形如 "REAL, ALLOCATABLE :: data_arr(:)" 提取前半部分基本类型
                    base = txt.split("::")[0].strip()
                    # 移除属性，仅保留基本类型名
                    base = base.split(",")[0].strip()
                    return base.capitalize()
    except Exception:
        pass
    return "REAL"


def rewrite_derived_members_to_temps(loop_node: Block_Nonlabel_Do_Construct) -> dict:
    # 识别循环体内赋值语句的派生类型成员（含标量与数组），引入局部临时变量并替换，循环后写回
    from fparser.two.utils import walk
    from fparser.two.Fortran2003 import Data_Ref, Part_Ref, Section_Subscript_List, Name
    parent = _find_parent_with_content(loop_node)
    if parent is None:
        return {}
    root = parent
    while hasattr(root, "parent") and root.parent is not None:
        root = root.parent
    # 收集要处理的成员：仅针对作为 LHS 的派生成员（需要写回）
    members = {}
    for a in walk(loop_node, Assignment_Stmt):
        items = getattr(a, "items", [])
        lhs = items[0] if items else None
        if isinstance(lhs, (Data_Ref, Part_Ref)):
            txt_full = str(lhs).strip()
            names = [n.string for n in walk(lhs, Name)]
            # names: [obj, member, (subscripts...)]
            if len(names) >= 2:
                objn, comp = names[0], names[1]
                base_arr = f"{objn} % {comp}"
                is_array = bool(list(walk(lhs, Section_Subscript_List)))
                key = base_arr
                members.setdefault(key, {"component": comp.lower(), "is_array": is_array, "full": txt_full})
    if not members:
        return {}
    # 生成唯一的临时变量名，并在循环前插入声明与 copy-in
    pc = list(getattr(parent, "content", []))
    idx = None
    for i, n in enumerate(pc):
        if n is loop_node:
            idx = i
            break
    if idx is None:
        return {}
    name_map = {}
    decl_nodes = []
    copyin_nodes = []
    existing_names = set([n.string.lower() for n in walk(parent, Name)])
    for base_txt, meta in members.items():
        comp = meta["component"]
        is_array = meta["is_array"]
        tmp = f"apf_tmp_{comp}"
        si = 1
        while tmp.lower() in existing_names:
            si += 1
            tmp = f"apf_tmp_{comp}_{si}"
        basetype = _infer_component_type(root, comp)
        if is_array:
            decl_text = f"{basetype} :: {tmp}(SIZE({base_txt}))"
            copyin_text = f"{tmp} = {base_txt}"
        else:
            decl_text = f"{basetype} :: {tmp}"
            copyin_text = f"{tmp} = {base_txt}"
        try:
            decl_nodes.append(Type_Declaration_Stmt(decl_text))
            copyin_nodes.append(Assignment_Stmt(copyin_text))
            name_map[base_txt] = tmp
        except Exception:
            # 若构造失败则跳过该成员
            continue
    # 在循环之前插入声明和 copy-in
    for dn in decl_nodes:
        pc.insert(idx, dn)
        idx += 1
    for cn in copyin_nodes:
        pc.insert(idx, cn)
        idx += 1
    # 重写循环体内的赋值语句：将 base_txt 替换为 tmp 名称
    new_loop_content = []
    for n in list(getattr(loop_node, "content", [])):
        if isinstance(n, Assignment_Stmt):
            txt = str(n)
            new_txt = txt
            for base_txt, tmp in name_map.items():
                # 大小写与空白不敏感替换：替换基数组名或标量名
                new_txt = new_txt.replace(base_txt, tmp)
                new_txt = new_txt.replace(base_txt.upper(), tmp)
                new_txt = new_txt.replace(base_txt.lower(), tmp)
            if new_txt != txt:
                try:
                    n = Assignment_Stmt(new_txt)
                except Exception:
                    pass
        new_loop_content.append(n)
    loop_node.content = new_loop_content
    # 对嵌套结构中的赋值语句也进行替换（如 IF 块内）
    for a in walk(loop_node, Assignment_Stmt):
        txt = str(a)
        new_txt = txt
        for base_txt, tmp in name_map.items():
            new_txt = new_txt.replace(base_txt, tmp)
            new_txt = new_txt.replace(base_txt.upper(), tmp)
            new_txt = new_txt.replace(base_txt.lower(), tmp)
        if new_txt != txt:
            try:
                na = Assignment_Stmt(new_txt)
                p = getattr(a, "parent", None)
                pc = list(getattr(p, "content", [])) if p is not None else []
                for i, node in enumerate(pc):
                    if node is a:
                        pc[i] = na
                        break
                if p is not None:
                    p.content = pc
            except Exception:
                pass
    # 在循环之后插入 copy-out
    copyout_nodes = []
    for base_txt, tmp in name_map.items():
        copyout_text = f"{base_txt} = {tmp}"
        try:
            copyout_nodes.append(Assignment_Stmt(copyout_text))
        except Exception:
            continue
    # 将 copy-out 插入在循环节点之后
    ins_at = None
    for i, n in enumerate(pc):
        if n is loop_node:
            ins_at = i + 1
            break
    if ins_at is None:
        parent.content = pc
        return name_map
    for cn in copyout_nodes:
        pc.insert(ins_at, cn)
        ins_at += 1
    parent.content = pc
    return name_map


def detect_derived_members_in_lines(lines: List[str], sidx: int, eidx: int):
    import re
    res = {}
    pat_chain = re.compile(r"\b([A-Za-z_]\w*)(?:\s*%\s*[A-Za-z_]\w+)+")
    for i in range(sidx, eidx + 1):
        line = lines[i]
        if "=" in line:
            lhs, rhs = line.split("=", 1)
        else:
            lhs, rhs = line, ""
        # 扫描 LHS 与 RHS 的派生成员链
        for part_text, is_write in ((lhs, True), (rhs, False)):
            for m in pat_chain.finditer(part_text):
                chain_text = m.group(0)
                parts = [p.strip() for p in chain_text.split('%')]
                if len(parts) < 2:
                    continue
                obj = parts[0].split()[-1]
                comps = []
                for c in parts[1:]:
                    c = re.sub(r"\(.*\)$", "", c).strip()
                    c = c.split()[0]
                    if c:
                        comps.append(c)
                if not comps:
                    continue
                final = comps[-1].lower()
                base_sig = obj + " % " + " % ".join(comps)
                # 是否数组：检查链末的组件后是否紧跟括号（在当前片段内）
                tail_pat = re.compile(rf"\b{re.escape(comps[-1])}\s*\(")
                has_paren = bool(tail_pat.search(chain_text))
                comp_chain = r"\s*%\s*".join([re.escape(c) for c in comps])
                patt_all = re.compile(rf"\b{re.escape(obj)}\s*%\s*{comp_chain}\b")
                patt_arr = re.compile(rf"\b{re.escape(obj)}\s*%\s*{comp_chain}\s*\(")
                entry = res.get(base_sig)
                meta = {"component": final, "is_array": has_paren, "patt_all": patt_all, "patt_arr": patt_arr, "write": is_write}
                if entry is None:
                    res[base_sig] = meta
                else:
                    # 累积属性：只要有任一写入，则标记 write=True；数组属性若任一出现为数组则为 True
                    entry["write"] = entry.get("write", False) or is_write
                    entry["is_array"] = entry.get("is_array", False) or has_paren
                    res[base_sig] = entry
    return res


def sanitize_omp_directives(lines: List[str], nsidx: int, neidx: int) -> List[str]:
    nsrc = lines
    def _is_start(s: str) -> bool:
        t = s.strip().lower()
        return t.startswith("!$omp parallel do") or t.startswith("!$omp do")
    def _is_end(s: str) -> bool:
        t = s.strip().lower()
        return t.startswith("!$omp end parallel do") or t.startswith("!$omp end do")
    win_start = max(0, nsidx - 3)
    win_end = min(len(nsrc) - 1, neidx + 6)
    start_idxs = [i for i in range(win_start, win_end + 1) if _is_start(nsrc[i])]
    end_idxs = [i for i in range(win_start, win_end + 1) if _is_end(nsrc[i])]
    keep = set()
    # 保留靠近 DO 的唯一开始指令
    if start_idxs:
        cand = [i for i in start_idxs if i <= nsidx]
        target = max(cand) if cand else start_idxs[0]
        keep.add(target)
    # 保留紧随 END DO 的唯一结束指令
    if end_idxs:
        cand = [i for i in end_idxs if i >= neidx + 1]
        target = min(cand) if cand else end_idxs[-1]
        keep.add(target)
    # 删除窗口内除 keep 外的重复 OMP 指令
    to_delete = [i for i in (start_idxs + end_idxs) if i not in keep]
    for i in sorted(to_delete, reverse=True):
        del nsrc[i]
        if i <= nsidx:
            nsidx -= 1
        if i <= neidx:
            neidx -= 1
    return nsrc


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