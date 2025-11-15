from typing import List, Dict, Tuple

from fparser.two.utils import walk
from fparser.two.Fortran2003 import Assignment_Stmt, Part_Ref, Name, Section_Subscript_List, Call_Stmt, Block_Nonlabel_Do_Construct, Nonlabel_Do_Stmt, End_Do_Stmt

from .ir import ArrayAccess, StatementIR, LoopIR, Dependence, AnalysisResult
from .utils import is_affine_term, parse_subscripts_text, is_variable_coeff_term
from .functions import find_pure_functions
from .symbols import collect_intent_in, collect_intent_out, collect_intent_inout, collect_value_attr, collect_scalar_constants_before


def extract_loop_ir(parse_tree) -> List[LoopIR]:
    loops: List[LoopIR] = []
    parent_map: Dict[str, str] = {}
    for d in walk(parse_tree, Block_Nonlabel_Do_Construct):
        inner_blocks = [c for c in getattr(d, "content", [])[1:-1] if isinstance(c, Block_Nonlabel_Do_Construct)]
        if inner_blocks:
            ost = None
            for child in walk(d, Nonlabel_Do_Stmt):
                ost = str(child)
                break
            for ib in inner_blocks:
                ist = None
                for c in walk(ib, Nonlabel_Do_Stmt):
                    ist = str(c)
                    break
                if ost and ist:
                    parent_map[ist] = ost
    for d in walk(parse_tree, Block_Nonlabel_Do_Construct):
        # Loop var and bounds from Do_Stmt
        dostmt = None
        for child in walk(d, Nonlabel_Do_Stmt):
            dostmt = child
            break
        loop_vars: List[str] = []
        bounds: Dict[str, Tuple[str, str, str]] = {}
        start_text = str(dostmt) if dostmt else "DO"
        end_text = str(list(walk(d, End_Do_Stmt))[0]) if list(walk(d, End_Do_Stmt)) else "END DO"
        if dostmt:
            s = str(dostmt)
            # expecting: DO i = lb, ub [, step]
            if "=" in s and "," in s:
                head = s[s.find("DO") + 2:].strip()
                var = head.split("=")[0].strip()
                rest = head.split("=")[1].strip()
                parts = [p.strip() for p in rest.split(",")]
                lb = parts[0]
                ub = parts[1] if len(parts) > 1 else None
                step = parts[2] if len(parts) > 2 else None
                loop_vars = [var]
                bounds[var] = (lb, ub, step)
        def collect_inner_loops(node):
            for inner in getattr(node, "content", [])[1:-1]:
                if isinstance(inner, Block_Nonlabel_Do_Construct):
                    istmt = None
                    for c in walk(inner, Nonlabel_Do_Stmt):
                        istmt = c
                        break
                    if istmt:
                        s2 = str(istmt)
                        if "=" in s2 and "," in s2:
                            head2 = s2[s2.find("DO") + 2:].strip()
                            var2 = head2.split("=")[0].strip()
                            rest2 = head2.split("=")[1].strip()
                            parts2 = [p.strip() for p in rest2.split(",")]
                            lb2 = parts2[0]
                            ub2 = parts2[1] if len(parts2) > 1 else None
                            step2 = parts2[2] if len(parts2) > 2 else None
                            if var2 not in loop_vars:
                                loop_vars.append(var2)
                            bounds[var2] = (lb2, ub2, step2)
                    collect_inner_loops(inner)
        collect_inner_loops(d)
        nest_depth = len(loop_vars) if loop_vars else 1
        # Collect statements in body (include nested assignments within inner loops)
        stmt_irs: List[StatementIR] = []
        consts = {}
        try:
            consts = collect_scalar_constants_before(d)
        except Exception:
            consts = {}
        for s in walk(d, Assignment_Stmt):
            text = str(s)
            ir = StatementIR(raw=text)
            # LHS
            lhs = s.items[0]
            if isinstance(lhs, Part_Ref):
                name = lhs.items[0].string if hasattr(lhs, "items") else str(lhs).split("(")[0]
                subsec = list(walk(lhs, Section_Subscript_List))
                subscripts = []
                affine_map = {}
                nonconst = {}
                if subsec:
                    subscripts = parse_subscripts_text(str(subsec[0]))
                    for sub in subscripts:
                        for lv in loop_vars:
                            from .utils import apply_constants
                            subx = apply_constants(sub, consts)
                            at = is_affine_term(subx, lv)
                            if at:
                                affine_map[lv] = at
                            else:
                                vc = is_variable_coeff_term(subx, lv)
                                if vc:
                                    nonconst[lv] = vc
                                else:
                                    from .utils import has_multiplicative_var
                                    if has_multiplicative_var(subx, lv):
                                        nonconst[lv] = sub
                                    else:
                                        from .utils import is_symbolic_offset_term
                                        sym = is_symbolic_offset_term(subx, lv)
                                        if sym:
                                            nonconst[lv] = sub
                ir.writes.append(ArrayAccess(name=name.lower(), subscripts=subscripts, affine_map=affine_map, nonconst_coeffs=nonconst))
            # RHS reads (array parts only)
            for ref in walk(s.items[2], Part_Ref):
                name = ref.items[0].string if hasattr(ref, "items") else str(ref).split("(")[0]
                subsec = list(walk(ref, Section_Subscript_List))
                subscripts = []
                affine_map = {}
                nonconst = {}
                if subsec:
                    subscripts = parse_subscripts_text(str(subsec[0]))
                    for sub in subscripts:
                        for lv in loop_vars:
                            from .utils import apply_constants
                            subx = apply_constants(sub, consts)
                            at = is_affine_term(subx, lv)
                            if at:
                                affine_map[lv] = at
                            else:
                                vc = is_variable_coeff_term(subx, lv)
                                if vc:
                                    nonconst[lv] = vc
                                else:
                                    from .utils import has_multiplicative_var
                                    if has_multiplicative_var(subx, lv):
                                        nonconst[lv] = sub
                                    else:
                                        from .utils import is_symbolic_offset_term
                                        sym = is_symbolic_offset_term(subx, lv)
                                        if sym:
                                            nonconst[lv] = sub
                ir.reads.append(ArrayAccess(name=name.lower(), subscripts=subscripts, affine_map=affine_map, nonconst_coeffs=nonconst))
            # simple reductions: scalar x = x op expr
            if isinstance(lhs, Name):
                lhs_name = lhs.string.lower()
                rhs_text = str(s.items[2]).lower()
                if lhs_name in rhs_text:
                    if "+" in rhs_text:
                        ir.reductions[lhs_name] = "sum"
                    elif "*" in rhs_text:
                        ir.reductions[lhs_name] = "product"
            stmt_irs.append(ir)
        loop_ir = LoopIR(loop_vars=loop_vars, bounds=bounds, body_statements=stmt_irs, node_ref=d, start_text=start_text, end_text=end_text, nest_depth=nest_depth, parent_start_text=parent_map.get(start_text))
        loops.append(loop_ir)
    return loops


def compute_dependences(loop: LoopIR) -> List[Dependence]:
    deps: List[Dependence] = []
    seen_trivial_shapes = set()
    for i, s1 in enumerate(loop.body_statements):
        for j, s2 in enumerate(loop.body_statements):
            # 允许同一语句内的读写对参与跨迭代依赖判断（例如 A(i) = A(i+1)）
            if j < i:
                continue
            for w in s1.writes:
                for r in s2.reads:
                    if w.name == r.name:
                        dist_vec = []
                        dir_vec = []
                        carried_by = []
                        unknown_lvs = []
                        for lv in loop.loop_vars:
                            if (lv in w.affine_map and lv in r.affine_map):
                                cw, kw = w.affine_map[lv]
                                cr, kr = r.affine_map[lv]
                                if cw == cr:
                                    k = kw - kr
                                    dist_vec.append(k)
                                    dir_vec.append("<" if k > 0 else (">" if k < 0 else "="))
                                    if k != 0:
                                        carried_by.append(lv)
                                else:
                                    lb_s, ub_s, step_s = loop.bounds.get(lv, (None, None, None))
                                    try:
                                        lb = int(str(lb_s)) if lb_s is not None else None
                                        ub = int(str(ub_s)) if ub_s is not None else None
                                        stride = int(str(step_s)) if step_s is not None else 1
                                    except Exception:
                                        lb, ub, stride = None, None, 1
                                    from .utils import find_k_feasible
                                    kfound = find_k_feasible(cw, kw, cr, kr, lb, ub, stride=stride)
                                    if kfound is not None:
                                        dist_vec.append(kfound)
                                        dir_vec.append("<" if kfound > 0 else (">" if kfound < 0 else "="))
                                        carried_by.append(lv)
                                    else:
                                        unknown_lvs.append(lv)
                                        dist_vec.append("?")
                                        dir_vec.append("?")
                            elif (lv in getattr(w, 'nonconst_coeffs', {}) or lv in getattr(r, 'nonconst_coeffs', {})):
                                unknown_lvs.append(lv)
                                dist_vec.append("?")
                                dir_vec.append("?")
                            else:
                                dist_vec.append(0)
                                dir_vec.append("=")
                        if unknown_lvs:
                            carried_by = list(set(carried_by + unknown_lvs))
                        is_trivial = all((d == 0 for d in dist_vec if not isinstance(d, str))) and all((dr == "=" for dr in dir_vec))
                        key = (w.name, tuple(dist_vec), tuple(dir_vec))
                        if is_trivial and key in seen_trivial_shapes:
                            pass
                        else:
                            if is_trivial:
                                seen_trivial_shapes.add(key)
                            deps.append(Dependence(src_stmt=i, dst_stmt=j, array=w.name, distance_vector=dist_vec, direction_vector=dir_vec, carried_by=carried_by))
            # 反依赖：读-写
            for r in s1.reads:
                for w in s2.writes:
                    if w.name != r.name:
                        continue
                    dist_vec = []
                    dir_vec = []
                    carried_by = []
                    unknown_lvs = []
                    for lv in loop.loop_vars:
                        if (lv in w.affine_map and lv in r.affine_map):
                            cw, kw = w.affine_map[lv]
                            cr, kr = r.affine_map[lv]
                            if cw == cr:
                                k = kr - kw
                                dist_vec.append(k)
                                dir_vec.append("<" if k > 0 else (">" if k < 0 else "="))
                                if k != 0:
                                    carried_by.append(lv)
                            else:
                                lb_s, ub_s, step_s = loop.bounds.get(lv, (None, None, None))
                                try:
                                    lb = int(str(lb_s)) if lb_s is not None else None
                                    ub = int(str(ub_s)) if ub_s is not None else None
                                    stride = int(str(step_s)) if step_s is not None else 1
                                except Exception:
                                    lb, ub, stride = None, None, 1
                                from .utils import find_k_feasible
                                kfound = find_k_feasible(cr, kr, cw, kw, lb, ub, stride=stride)
                                if kfound is not None:
                                    dist_vec.append(kfound)
                                    dir_vec.append("<" if kfound > 0 else (">" if kfound < 0 else "="))
                                    carried_by.append(lv)
                                else:
                                    unknown_lvs.append(lv)
                                    dist_vec.append("?")
                                    dir_vec.append("?")
                        elif (lv in getattr(w, 'nonconst_coeffs', {}) or lv in getattr(r, 'nonconst_coeffs', {})):
                            unknown_lvs.append(lv)
                            dist_vec.append("?")
                            dir_vec.append("?")
                        else:
                            dist_vec.append(0)
                            dir_vec.append("=")
                    if unknown_lvs:
                        carried_by = list(set(carried_by + unknown_lvs))
                    is_trivial = all((d == 0 for d in dist_vec if not isinstance(d, str))) and all((dr == "=" for dr in dir_vec))
                    key = (w.name, tuple(dist_vec), tuple(dir_vec))
                    if is_trivial and key in seen_trivial_shapes:
                        pass
                    else:
                        if is_trivial:
                            seen_trivial_shapes.add(key)
                        deps.append(Dependence(src_stmt=i, dst_stmt=j, array=w.name, distance_vector=dist_vec, direction_vector=dir_vec, carried_by=carried_by))
            # 输出依赖：写-写
            for w1 in s1.writes:
                for w2 in s2.writes:
                    if w1.name != w2.name:
                        continue
                    dist_vec = []
                    dir_vec = []
                    carried_by = []
                    unknown_lvs = []
                    for lv in loop.loop_vars:
                        if (lv in w1.affine_map and lv in w2.affine_map):
                            c1, k1 = w1.affine_map[lv]
                            c2, k2 = w2.affine_map[lv]
                            if c1 == c2:
                                k = k1 - k2
                                dist_vec.append(k)
                                dir_vec.append("<" if k > 0 else (">" if k < 0 else "="))
                                if k != 0:
                                    carried_by.append(lv)
                            else:
                                lb_s, ub_s, step_s = loop.bounds.get(lv, (None, None, None))
                                try:
                                    lb = int(str(lb_s)) if lb_s is not None else None
                                    ub = int(str(ub_s)) if ub_s is not None else None
                                    stride = int(str(step_s)) if step_s is not None else 1
                                except Exception:
                                    lb, ub, stride = None, None, 1
                                from .utils import find_k_feasible
                                kfound = find_k_feasible(c1, k1, c2, k2, lb, ub, stride=stride)
                                if kfound is not None:
                                    dist_vec.append(kfound)
                                    dir_vec.append("<" if kfound > 0 else (">" if kfound < 0 else "="))
                                    carried_by.append(lv)
                                else:
                                    unknown_lvs.append(lv)
                                    dist_vec.append("?")
                                    dir_vec.append("?")
                        elif (lv in getattr(w1, 'nonconst_coeffs', {}) or lv in getattr(w2, 'nonconst_coeffs', {})):
                            unknown_lvs.append(lv)
                            dist_vec.append("?")
                            dir_vec.append("?")
                        else:
                            dist_vec.append(0)
                            dir_vec.append("=")
                    if unknown_lvs:
                        carried_by = list(set(carried_by + unknown_lvs))
                    is_trivial = all((d == 0 for d in dist_vec if not isinstance(d, str))) and all((dr == "=" for dr in dir_vec))
                    key = (w1.name, tuple(dist_vec), tuple(dir_vec))
                    if is_trivial and key in seen_trivial_shapes:
                        pass
                    else:
                        if is_trivial:
                            seen_trivial_shapes.add(key)
                        deps.append(Dependence(src_stmt=i, dst_stmt=j, array=w1.name, distance_vector=dist_vec, direction_vector=dir_vec, carried_by=carried_by))
    return deps


def analyze_loop(loop: LoopIR) -> AnalysisResult:
    deps = compute_dependences(loop)
    reduction_vars = {}
    for s in loop.body_statements:
        for k, v in s.reductions.items():
            reduction_vars[k] = v
    private_vars = list(loop.loop_vars)
    firstprivate_vars = []
    shared_vars = []
    rhs_scalars = set()
    lhs_scalars = set()
    arrays = set()
    for s in loop.body_statements:
        for a in s.reads:
            arrays.add(a.name)
        for a in s.writes:
            arrays.add(a.name)
    from fparser.two.Fortran2003 import Assignment_Stmt
    for node in walk(loop.node_ref, Assignment_Stmt):
        items = getattr(node, "items", [])
        lhs = items[0] if items else None
        rhs = items[2] if len(items) > 2 else None
        for n in walk(rhs, Name):
            nm = n.string.lower()
            if nm not in arrays and nm not in loop.loop_vars:
                rhs_scalars.add(nm)
        if isinstance(lhs, Name):
            nm = lhs.string.lower()
            if nm not in arrays and nm not in loop.loop_vars:
                lhs_scalars.add(nm)
    decl_in = collect_intent_in(loop.node_ref)
    decl_out = collect_intent_out(loop.node_ref)
    decl_inout = collect_intent_inout(loop.node_ref)
    decl_value = collect_value_attr(loop.node_ref)
    firstprivate_vars = sorted(list(((rhs_scalars | decl_value | decl_in) - lhs_scalars)))
    lastprivate_vars = sorted(list(lhs_scalars - rhs_scalars))
    is_parallel = True
    reason = None
    current_lv = loop.loop_vars[0] if loop.loop_vars else None
    idx_current = 0
    for d in deps:
        try:
            val = d.distance_vector[idx_current] if isinstance(d.distance_vector, list) else None
            direc = d.direction_vector[idx_current] if isinstance(d.direction_vector, list) else None
        except Exception:
            val, direc = None, None
        if val not in (0, None) or direc == "?":
            is_parallel = False
            reason = f"loop-carried/unknown dependence on {d.array} along {current_lv} with distance {d.distance_vector}"
            if d.array in reduction_vars:
                is_parallel = True
                reason = None
    # check function calls' purity
    parse_tree = loop.node_ref
    root = None
    # get root by walking up via .parent if available, else use node_ref.root when present
    try:
        node = parse_tree
        while hasattr(node, "parent") and node.parent is not None:
            node = node.parent
        root = node
    except Exception:
        root = None
    pure = set()
    try:
        root_for_pure = root if root is not None else parse_tree
        pure = find_pure_functions(root_for_pure)
    except Exception:
        pure = set()
    for s in walk(loop.node_ref, Call_Stmt):
        text = str(s).lower()
        name = None
        names = [n.string.lower() for n in walk(s, Name)]
        if names:
            name = names[0]
        if name and name not in pure:
            is_parallel = False
            if reason is None:
                reason = f"impure function call '{name}' inside loop"
    return AnalysisResult(loop=loop, dependences=deps, is_parallel=is_parallel, reason=reason, reduction_vars=reduction_vars, private_vars=private_vars, firstprivate_vars=firstprivate_vars, shared_vars=list(arrays), lastprivate_vars=lastprivate_vars)