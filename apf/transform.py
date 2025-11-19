from typing import List, Tuple, Optional
from fparser.two.Fortran2003 import Comment, Directive, Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt, Assignment_Stmt, Data_Ref, Type_Declaration_Stmt, Allocate_Stmt, Deallocate_Stmt, Call_Stmt, Subroutine_Subprogram, Function_Subprogram
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
        # 结束指令需位于 loop_node 之后、紧随其后的 copy-out/deallocate 之后
        end_insert = idx + 1
        from fparser.two.Fortran2003 import Assignment_Stmt
        while end_insert < len(pc) and isinstance(pc[end_insert], (Assignment_Stmt, Deallocate_Stmt)):
            end_insert += 1
        pc.insert(idx, _dir(s))
        pc.insert(end_insert + 1, _dir(e))
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


def _find_enclosing_subprogram(node):
    q = getattr(node, "parent", None)
    from fparser.two.Fortran2003 import Subroutine_Subprogram, Function_Subprogram
    while q is not None and not isinstance(q, (Subroutine_Subprogram, Function_Subprogram)):
        q = getattr(q, "parent", None)
    return q


def _find_spec_part(subp):
    if subp is None:
        return None
    from fparser.two.Fortran2003 import Specification_Part
    for n in list(getattr(subp, "content", [])):
        if isinstance(n, Specification_Part):
            return n
    return None


def _unique_name(base: str, existing: set) -> str:
    n = base
    i = 2
    while n.lower() in existing:
        n = f"{base}_{i}"
        i += 1
    return n


def _infer_component_type(root, comp_name: str) -> str:
    # 尝试从类型定义中推断成员的基本类型（REAL/INTEGER 等），失败则默认 REAL
    # 说明：优先遍历 Type_Declaration_Stmt，其次解析 Derived_Type_Def 文本以获取组件声明头
    try:
        from fparser.two.utils import walk
        from fparser.two.Fortran2003 import Type_Declaration_Stmt, Entity_Decl, Derived_Type_Def
        for td in walk(root, Type_Declaration_Stmt):
            txt = str(td).strip().upper()
            for e in walk(td, Entity_Decl):
                try:
                    nm = e.items[0].string.lower()
                except Exception:
                    nm = None
                if nm == comp_name.lower():
                    base = txt.split("::")[0].strip()
                    base = base.split(",")[0].strip()
                    return base.capitalize()
        for dt in walk(root, Derived_Type_Def):
            dtxt = dt.tofortran() if hasattr(dt, "tofortran") else str(dt)
            lines = [l.strip() for l in dtxt.splitlines()]
            for l in lines:
                if "::" in l and comp_name.lower() in l.lower():
                    head = l.split("::")[0].strip()
                    head_u = head.upper()
                    for kw in ["REAL", "INTEGER", "DOUBLE PRECISION", "LOGICAL", "COMPLEX"]:
                        if head_u.startswith(kw):
                            return kw.title() if kw != "DOUBLE PRECISION" else "Double Precision"
    except Exception:
        pass
    return "REAL"


def rewrite_derived_members_to_temps(loop_node: Block_Nonlabel_Do_Construct) -> dict:
    # 识别循环体内赋值语句的派生类型成员（含标量与数组），引入局部临时变量并替换，循环后写回
    # 策略：
    # 1) 在循环所属子程序的规范区插入声明（数组声明为 allocatable）与在循环前插入 allocate 与 copy-in
    # 2) 循环体中将所有派生成员引用替换为临时变量（包含嵌套块内赋值）
    # 3) 在循环后插入 copy-out；若为数组则追加 deallocate
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
    subp = _find_enclosing_subprogram(loop_node)
    spec = _find_spec_part(subp)
    spc = list(getattr(spec, "content", [])) if spec is not None else None
    # 规范区插入位置：implicit none 之后；若没有则在规范区开头
    decl_insert = 0
    if spc is not None:
        for i, n in enumerate(spc[:30]):
            s = str(n).strip().lower()
            if s.startswith("implicit none"):
                decl_insert = i + 1
                break
    # 循环在执行区中的位置
    idx_loop = None
    for i, n in enumerate(pc):
        if n is loop_node:
            idx_loop = i
            break
    if idx_loop is None:
        return {}
    name_map = {}
    decl_nodes = []
    alloc_nodes = []
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
            decl_text = f"{basetype}, ALLOCATABLE :: {tmp}(:)"
            alloc_text = f"ALLOCATE({tmp}(SIZE({base_txt})))"
            copyin_text = f"{tmp} = {base_txt}"
        else:
            decl_text = f"{basetype} :: {tmp}"
            alloc_text = None
            copyin_text = f"{tmp} = {base_txt}"
        try:
            decl_nodes.append(Type_Declaration_Stmt(decl_text))
            if alloc_text:
                alloc_nodes.append(Allocate_Stmt(alloc_text))
            copyin_nodes.append(Assignment_Stmt(copyin_text))
            name_map[base_txt] = tmp
        except Exception:
            # 若构造失败则跳过该成员
            continue
    # 插入声明到规范区
    if spc is not None:
        for dn in decl_nodes:
            spc.insert(decl_insert, dn)
            decl_insert += 1
        spec.content = spc
    else:
        # 回退：若未找到规范区，则插入到当前父 content 的顶部
        for dn in decl_nodes:
            pc.insert(0, dn)
    # 在循环之前插入 allocate/copy-in（执行区）
    exec_insert = idx_loop
    for an in alloc_nodes:
        pc.insert(exec_insert, an)
        exec_insert += 1
    for cn in copyin_nodes:
        pc.insert(exec_insert, cn)
        exec_insert += 1
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
    # 对嵌套结构中的赋值语句也进行替换（如 IF 块内），保持一致性
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
    # 对数组插入 deallocate
    for base_txt, meta in members.items():
        tmp = name_map.get(base_txt)
        if tmp and meta.get("is_array"):
            try:
                copyout_nodes.append(Deallocate_Stmt(f"DEALLOCATE({tmp})"))
            except Exception:
                pass
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


def _collect_calls_in_loop(loop_node: Block_Nonlabel_Do_Construct):
    from fparser.two.utils import walk
    calls = []
    for c in walk(loop_node, Call_Stmt):
        try:
            callee = c.items[0].string
        except Exception:
            callee = str(c).split("(")[0].strip()
        calls.append((c, callee))
    return calls


def _find_subprogram(root, name: str):
    from fparser.two.utils import walk
    for s in walk(root, Subroutine_Subprogram):
        try:
            sn = s.content[0].items[1].string
        except Exception:
            sn = None
        if sn and sn.lower() == name.lower():
            return s
    for f in walk(root, Function_Subprogram):
        try:
            fn = f.content[0].items[1].string
        except Exception:
            fn = None
        if fn and fn.lower() == name.lower():
            return f
    return None


def _detect_derived_writes_in_subprogram(subp):
    from fparser.two.utils import walk
    from fparser.two.Fortran2003 import Assignment_Stmt, Data_Ref, Part_Ref, Section_Subscript_List
    import re
    chains = {}
    for a in walk(subp, Assignment_Stmt):
        items = getattr(a, "items", [])
        lhs = items[0] if items else None
        if isinstance(lhs, (Data_Ref, Part_Ref)):
            txt = str(lhs).strip()
            # 去除数组下标，保留派生成员链
            chain_txt = re.sub(r"\([^()]*\)", "", txt)
            if '%' not in chain_txt:
                continue
            parts = [p.strip() for p in chain_txt.split('%')]
            if len(parts) >= 2:
                objn = parts[0]
                comps = parts[1:]
                chain = objn + " % " + " % ".join(comps)
                is_array = bool(list(walk(lhs, Section_Subscript_List)))
                chains[chain] = {"component": comps[-1].lower(), "is_array": is_array}
    return chains


def _duplicate_subprogram_with_args(root, subp, add_args_map: dict):
    # 复制子程序，追加形式参数，并将派生成员链替换为传入参数名
    import re
    text = subp.tofortran()
    # 取得名称并构造新名称
    header = subp.content[0]
    try:
        name = header.items[1].string
    except Exception:
        name = re.findall(r"\b(subroutine|function)\s+([A-Za-z_]\w+)", text, re.I)[0][1]
    new_name = name + "_apf"
    # 形式参数列表：追加参数名（按组件去重，确保唯一）
    comps = []
    for meta in add_args_map.values():
        c = meta["component"]
        if c not in comps:
            comps.append(c)
    # 先构造占位名，稍后在 AST 中保证唯一
    add_list = [f"apf_arg_{v}" for v in comps]
    # 重写头部：在名称与实参之间插入追加参数
    if "subroutine" in text.lower().splitlines()[0].lower():
        text = re.sub(rf"(\bsubroutine\s+){re.escape(name)}\s*\((.*?)\)",
                      lambda m: f"{m.group(1)}{new_name}({m.group(2)}{(',' if m.group(2).strip() else '')}{', '.join(add_list)})",
                      text, count=1, flags=re.I|re.S)
    else:
        text = re.sub(rf"(\bfunction\s+){re.escape(name)}\s*\((.*?)\)",
                      lambda m: f"{m.group(1)}{new_name}({m.group(2)}{(',' if m.group(2).strip() else '')}{', '.join(add_list)})",
                      text, count=1, flags=re.I|re.S)
    # 在规范区追加追加参数的声明（按标量/数组）
    decls = []
    # 解析并返回新子程序节点，将其插入到根的内容末尾
    r = FortranStringReader(text, ignore_comments=False, process_directives=True)
    from fparser.two.Fortran2003 import Subroutine_Subprogram, Function_Subprogram
    try:
        new_node = Subroutine_Subprogram(r)
    except Exception:
        new_node = Function_Subprogram(r)
    # 在新子程序的规范区插入追加参数声明，确保名称唯一
    spec = _find_spec_part(new_node)
    spc = list(getattr(spec, "content", [])) if spec is not None else []
    from fparser.two.utils import walk
    existing_names = set([n.string.lower() for n in walk(new_node, Name)])
    arg_map = {}
    for comp in comps:
        base = f"apf_arg_{comp}"
        argn = _unique_name(base, existing_names)
        existing_names.add(argn.lower())
        arg_map[comp] = argn
        basetype = _infer_component_type(root, comp)
        is_arr = any((meta.get("component") == comp and meta.get("is_array")) for meta in add_args_map.values())
        decl_text = f"{basetype}, INTENT(INOUT) :: {argn}(:)" if is_arr else f"{basetype}, INTENT(INOUT) :: {argn}"
        spc.append(Type_Declaration_Stmt(decl_text))
    if spec is not None:
        spec.content = spc
    # 使用 AST 级替换：仅替换 LHS 写入链为参数名
    _replace_lhs_with_args_in_subprogram(new_node, arg_map, add_args_map)
    parent = _find_parent_with_content(subp)
    pc = list(getattr(parent, "content", []))
    ins = None
    for i, n in enumerate(pc):
        if n is subp:
            ins = i + 1
            break
    if ins is None:
        pc.append(new_node)
    else:
        pc.insert(ins, new_node)
    parent.content = pc
    return new_name


def _replace_lhs_with_args_in_subprogram(subp_node, arg_map: dict, add_args_map: dict):
    from fparser.two.utils import walk
    from fparser.two.Fortran2003 import Assignment_Stmt
    import re
    chains = list(add_args_map.items())
    for a in walk(subp_node, Assignment_Stmt):
        txt = str(a)
        new_txt = txt
        for chain, meta in chains:
            comp = meta.get("component")
            argn = arg_map.get(comp)
            if not argn:
                continue
            lhs_pat = re.compile(rf"^(\s*)({re.escape(chain)}\s*(\([^)]*\))?)\s*=\s*", re.I)
            m = lhs_pat.match(new_txt)
            if m:
                # 保留原左值下标（若有）
                idx = m.group(3) or ""
                new_txt = lhs_pat.sub(f"\\g<1>{argn}{idx} = ", new_txt, count=1)
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


def rewrite_calls_with_temps(loop_node: Block_Nonlabel_Do_Construct):
    # 在循环体中查找过程调用，若被调用过程内部包含派生成员引用，则：
    # 1) 在当前子程序规格区声明临时变量（数组 allocatable）并在循环前 copy-in/allocate
    # 2) 复制被调用过程为 _apf 版本，追加相应的 INOUT 参数并替换内部派生成员引用
    # 3) 修改调用语句为调用 _apf 版本并传入临时变量；循环后对临时变量进行 copy-out/deallocate
    root = loop_node
    while hasattr(root, "parent") and root.parent is not None:
        root = root.parent
    calls = _collect_calls_in_loop(loop_node)
    if not calls:
        return {}
    parent = _find_parent_with_content(loop_node)
    if parent is None:
        return {}
    pc = list(getattr(parent, "content", []))
    # 在规范区插入声明
    subp = _find_enclosing_subprogram(loop_node)
    spec = _find_spec_part(subp)
    spc = list(getattr(spec, "content", [])) if spec is not None else None
    decl_insert = 0
    if spc is not None:
        for i, n in enumerate(spc[:30]):
            s = str(n).strip().lower()
            if s.startswith("implicit none"):
                decl_insert = i + 1
                break
    name_map = {}
    from fparser.two.utils import walk
    existing_names_parent = set([n.string.lower() for n in walk(parent, Name)])
    for call_node, cname in calls:
        subp = _find_subprogram(root, cname)
        if not subp:
            continue
        chains = _detect_derived_writes_in_subprogram(subp)
        if not chains:
            continue
        # 为每个链在当前过程声明临时变量并在循环前 copy-in/allocate
        for chain, meta in chains.items():
            comp = meta["component"]
            base_tmp = f"apf_tmp_{comp}"
            tmp = _unique_name(base_tmp, existing_names_parent)
            existing_names_parent.add(tmp.lower())
            # 推断成员基本类型
            basetype = _infer_component_type(root, comp)
            if meta["is_array"]:
                decl = Type_Declaration_Stmt(f"{basetype}, ALLOCATABLE :: {tmp}(:)")
                alloc = Allocate_Stmt(f"ALLOCATE({tmp}(SIZE({chain})))")
                copyin = Assignment_Stmt(f"{tmp} = {chain}")
                if spc is not None:
                    spc.insert(decl_insert, decl)
                    decl_insert += 1
                    spec.content = spc
                else:
                    pc.insert(0, decl)
                # 在循环之前插入分配与 copy-in
                idx = None
                for i, n in enumerate(pc):
                    if n is loop_node:
                        idx = i
                        break
                if idx is not None:
                    pc.insert(idx, alloc)
                    pc.insert(idx + 1, copyin)
            else:
                decl = Type_Declaration_Stmt(f"{basetype} :: {tmp}")
                copyin = Assignment_Stmt(f"{tmp} = {chain}")
                if spc is not None:
                    spc.insert(decl_insert, decl)
                    decl_insert += 1
                    spec.content = spc
                else:
                    pc.insert(0, decl)
                idx = None
                for i, n in enumerate(pc):
                    if n is loop_node:
                        idx = i
                        break
                if idx is not None:
                    pc.insert(idx, copyin)
            name_map[chain] = tmp
        # 复制被调用过程，替换内部派生成员引用，追加参数
        new_name = _duplicate_subprogram_with_args(root, subp, chains)
        # 替换调用语句名称并在实参列表后追加临时变量
        ctxt = str(call_node)
        # 构造追加参数文本
        add_args = [name_map.get(chain, f"apf_tmp_{meta['component']}") for chain, meta in chains.items()]
        import re
        if "(" in ctxt:
            ctxt = re.sub(rf"\b{re.escape(cname)}\s*\((.*?)\)", lambda m: f"{new_name}({m.group(1)}{(',' if m.group(1).strip() else '')}{', '.join(add_args)})", ctxt, count=1)
        else:
            ctxt = re.sub(rf"\b{re.escape(cname)}\b", f"{new_name}({', '.join(add_args)})", ctxt, count=1)
        try:
            new_call = Call_Stmt(FortranStringReader(ctxt, ignore_comments=False, process_directives=True))
            # 替换父 content 中的节点
            for i, n in enumerate(pc):
                if n is call_node:
                    pc[i] = new_call
                    break
        except Exception:
            pass
        # 循环后写回与释放
        idx_end = None
        for i, n in enumerate(pc):
            if n is loop_node:
                idx_end = i + 1
                break
        if idx_end is not None:
            for chain, meta in chains.items():
                tmp = name_map.get(chain, f"apf_tmp_{meta['component']}")
                pc.insert(idx_end, Assignment_Stmt(f"{chain} = {tmp}"))
                idx_end += 1
                if meta["is_array"]:
                    pc.insert(idx_end, Deallocate_Stmt(f"DEALLOCATE({tmp})"))
                    idx_end += 1
    parent.content = pc
    return name_map


def detect_derived_members_in_lines(lines: List[str], sidx: int, eidx: int):
    # 文本模式下识别 LHS/RHS 的派生成员链（支持嵌套与数组下标），并构造替换所需的模式
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
    # 清理预插或重复的 OpenMP 指令：
    # - DO 前的连续 OMP 开始指令块，仅保留最靠近 DO 的一条
    # - END DO 后的连续 OMP 结束指令块，仅保留紧随 END DO 的一条
    # - 跨越我们插入的临时声明、copy-in/out 语句进行窗口合并
    nsrc = lines
    def _is_start(s: str) -> bool:
        t = s.strip().lower()
        return t.startswith("!$omp parallel do") or t.startswith("!$omp do") or t.startswith("!$omp parallel")
    def _is_end(s: str) -> bool:
        t = s.strip().lower()
        return t.startswith("!$omp end parallel do") or t.startswith("!$omp end do") or t.startswith("!$omp end parallel")
    # 清理 DO 前的连续 OMP 开始指令块，仅保留最靠近 DO 的一条
    i = nsidx - 1
    start_block = []
    def _is_temp_decl_or_copyin(s: str) -> bool:
        t = s.strip().upper()
        return t.startswith("REAL ::") or t.startswith("INTEGER ::") or t.startswith("DOUBLE PRECISION ::") or ("APF_TMP_" in t)
    # 允许跨越我们插入的临时变量声明/赋值，继续向上合并 OMP 开始指令块
    while i >= 0:
        line = nsrc[i]
        if line.strip().lower().startswith("!$omp"):
            start_block.append(i)
            i -= 1
            continue
        if _is_temp_decl_or_copyin(line):
            i -= 1
            continue
        break
    if len(start_block) > 1:
        keep = min([idx for idx in start_block if _is_start(nsrc[idx])] or [start_block[-1]])
        for idx in sorted([x for x in start_block if x != keep], reverse=True):
            del nsrc[idx]
            if idx <= nsidx:
                nsidx -= 1
            if idx <= neidx:
                neidx -= 1
    # 清理 END DO 后的连续 OMP 结束指令块，仅保留紧随 END DO 的一条
    j = neidx + 1
    end_block = []
    while j < len(nsrc):
        line = nsrc[j]
        if line.strip().lower().startswith("!$omp"):
            end_block.append(j)
            j += 1
            continue
        if _is_temp_decl_or_copyin(line):
            j += 1
            continue
        break
    if len(end_block) > 1:
        keep = min([idx for idx in end_block if _is_end(nsrc[idx])] or [end_block[0]])
        for idx in sorted([x for x in end_block if x != keep], reverse=True):
            del nsrc[idx]
            if idx <= nsidx:
                nsidx -= 1
            if idx <= neidx:
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