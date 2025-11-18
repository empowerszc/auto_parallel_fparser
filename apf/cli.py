"""Command-line interface for the auto-parallel analysis and transform tool.

Usage:
    python3 -m apf.cli <file.f90> --report [--no-trivial]
    python3 -m apf.cli <file.f90> --apply --output out.f90 [--analyze-derived]

Flags:
    --no-trivial       Hide trivial (=, zero-distance) dependencies in report.
    --analyze-derived  Enable derived-type member analysis and basic transforms.
"""
import argparse
import re
from typing import List

from .parser import parse_file
from .dependence import extract_loop_ir, analyze_loop
from .transform import insert_openmp_directives, build_omp_clauses, TransformOptions


def analyze_file(path: str, no_trivial: bool = False) -> str:
    """Analyze a Fortran file and return a human-readable dependence report.

    Args:
        path: Path to a Fortran source file.
        no_trivial: When True, hides trivial dependencies in the output.

    Returns:
        Multi-line string report per loop including dependences and decision.
    """
    tree = parse_file(path, ignore_comments=False)
    loops = extract_loop_ir(tree)
    report_lines: List[str] = []
    from .transform import find_loop_range, find_loop_range_nth
    seen = {}
    for idx, loop in enumerate(loops):
        ar = analyze_loop(loop)
        try:
            with open(path, "r") as f:
                lines = f.readlines()
            key = loop.start_text
            seen[key] = seen.get(key, 0) + 1
            sidx, eidx = find_loop_range_nth(lines, loop.start_text, loop.end_text, seen[key])
        except Exception:
            sidx, eidx = (None, None)
        loc = f"{path}:{(sidx+1) if sidx is not None else '?'}"
        report_lines.append(f"Loop {idx+1} @ {loc}: {loop.start_text}")
        for d in ar.dependences:
            trivial = all((x == 0 for x in d.distance_vector if not isinstance(x, str))) and all((dr == '=' for dr in d.direction_vector))
            if no_trivial and trivial:
                continue
            line = f"  dep {d.array}: distance={d.distance_vector}, direction={d.direction_vector}, carried_by={d.carried_by}"
            if getattr(d, 'notes', None):
                line += f"; notes={d.notes}"
            report_lines.append(line)
        if ar.is_parallel:
            report_lines.append("  parallelizable: yes")
            if ar.reduction_vars:
                report_lines.append(f"  reductions: {ar.reduction_vars}")
        else:
            report_lines.append(f"  parallelizable: no, reason: {ar.reason}")
    return "\n".join(report_lines) + ("\n" if report_lines else "")


def transform_file(path: str, output: str, style: str = "parallel_do", schedule: str = "static", collapse: str = "auto", analyze_derived: bool = False) -> str:
    return transform_file_line_fixed(path, output, style=style, schedule=schedule, collapse=collapse, analyze_derived=analyze_derived)


def transform_file_ast(path: str, output: str, style: str = "parallel_do", schedule: str = "static", collapse: str = "auto", analyze_derived: bool = False) -> str:
    """基于 AST 的 OpenMP 指令插入与写回。

    流程概览：
    1) 解析 Fortran 源码并提取循环 IR（包含 AST 引用）。
    2) 根据并行性分析构造 OpenMP 子句与策略，并在 AST 上插入指令。
    3) 使用 AST 的 tofortran() 序列化回源码（保持格式规范）。
    """
    tree = parse_file(path, ignore_comments=False)
    loops = extract_loop_ir(tree)
    applied = 0
    from .transform import insert_openmp_directives_ast, build_omp_clauses, TransformOptions
    # 构建父子关系映射（用于 collapse 策略）：本地判断内层并行性以决定是否合并
    loop_by_start = {l.start_text: l for l in loops}
    children = {}
    for l in loops:
        if l.parent_start_text:
            children.setdefault(l.parent_start_text, []).append(l)
    # 1) 派生类型：采用“局部临时变量 + 循环后写回”的重写策略，不插入 atomic
    # 2) 重写派生成员到临时变量（按循环体 LHS 成员）：并在插入 OpenMP 前替换归约名
    for loop in loops:
        ar = analyze_loop(loop)
        name_map = {}
        call_nm = {}
        if analyze_derived:
            try:
                from .transform import rewrite_derived_members_to_temps, rewrite_calls_with_temps
                nm1 = rewrite_derived_members_to_temps(loop.node_ref)
                call_nm = rewrite_calls_with_temps(loop.node_ref)
                name_map = {**(nm1 or {}), **(call_nm or {})}
            except Exception:
                name_map = {}
        clauses = build_omp_clauses(ar)
        for k, v in name_map.items():
            clauses = clauses.replace(k, v)
        allow_insert = (ar.is_parallel and not loop.has_complex_index) or (call_nm and not loop.has_complex_index)
        if not allow_insert and getattr(ar, 'reason', ''):
            if 'impure function call' in ar.reason and not loop.has_complex_index:
                allow_insert = True
        if not allow_insert:
            continue
        # 自适应 collapse 策略（同 transform_file_line_fixed）：外层并行且所有内层并行时合并
        local_style = style
        local_collapse = None
        if collapse == "auto":
            if loop.parent_start_text is None and loop.nest_depth and loop.nest_depth > 1:
                cs = children.get(loop.start_text, [])
                all_inner_parallel = True
                for c in cs:
                    car = analyze_loop(c)
                    if not car.is_parallel:
                        all_inner_parallel = False
                        break
                if all_inner_parallel:
                    local_style = "parallel_do"
                    local_collapse = loop.nest_depth
                else:
                    local_style = "parallel_region"
                    local_collapse = None
        else:
            try:
                ci = int(collapse)
                local_collapse = ci if ci > 1 else None
            except Exception:
                local_collapse = None
        opts = TransformOptions(style=local_style, schedule=schedule, collapse=local_collapse)
        # 在 AST 节点上插入指令：失败时不中断整体流程，记录警告
        try:
            insert_openmp_directives_ast(loop.node_ref, clauses, opts)
            applied += 1
        except Exception as e:
            print(f"Warning: Failed to insert OpenMP directives for loop at {loop.start_text}: {e}")
            continue
    # 3) 序列化回源码并写出：优先使用 AST 的 tofortran()，异常回退到 str(tree)
    try:
        result = tree.tofortran()
        with open(output, 'w') as f:
            f.write(result)
    except Exception as e:
        print(f"Error: Failed to serialize AST to Fortran: {e}")
        print("Falling back to string representation...")
        with open(output, 'w') as f:
            f.write(str(tree))
    return f"Applied OpenMP to {applied} loop(s). Output: {output}"


def transform_file_line_fixed(path: str, output: str, style: str = "parallel_do", schedule: str = "static", collapse: str = "auto", analyze_derived: bool = False) -> str:
    tree = parse_file(path, ignore_comments=False)
    loops = extract_loop_ir(tree)
    with open(path, "r") as f:
        lines = f.readlines()
    transformed = lines[:]
    applied = 0
    from .transform import find_loop_range_nth
    seen = {}
    protected = []
    for loop in loops:
        ar = analyze_loop(loop)
        clauses = build_omp_clauses(ar)
        key = loop.start_text
        seen[key] = seen.get(key, 0) + 1
        sidx, eidx = find_loop_range_nth(transformed, loop.start_text, loop.end_text, seen[key])
        if sidx is None or eidx is None:
            continue
        copyouts = []
        if analyze_derived:
            from .transform import detect_derived_members_in_lines, _infer_component_type
            dmem = detect_derived_members_in_lines(transformed, sidx, eidx)
            name_map = {}
            # 找到所属子程序规范区插入位置：优先在 implicit none 之后，否则在 subroutine/function 头之后
            sub_start = None
            for bi in range(sidx, -1, -1):
                low = transformed[bi].strip().lower()
                if low.startswith("subroutine") or low.startswith("function"):
                    sub_start = bi
                    break
            sub_end = None
            for bi in range(eidx, len(transformed)):
                low = transformed[bi].strip().lower()
                if low.startswith("end subroutine") or low.startswith("end function"):
                    sub_end = bi
                    break
            decl_insert = (sub_start + 1) if sub_start is not None else sidx
            for bi in range(sub_start + 1 if sub_start is not None else sidx, min(sub_start + 20 if sub_start is not None else sidx + 20, len(transformed))):
                low = transformed[bi].strip().lower()
                if low.startswith("implicit none"):
                    decl_insert = bi + 1
            declared = set()
            for base_txt, meta in dmem.items():
                comp = meta["component"]
                is_array = meta["is_array"]
                tmp = f"apf_tmp_{comp}"
                basetype = _infer_component_type(tree, comp)
                decl = f"{basetype}, allocatable :: {tmp}(:)" if is_array else f"{basetype} :: {tmp}"
                copyin = f"{tmp} = {base_txt}"
                if is_array:
                    # 先声明到规范区（去重）
                    if decl not in declared:
                        transformed.insert(decl_insert, decl + "\n")
                        decl_insert += 1
                        declared.add(decl)
                    # 在循环前分配与 copy-in
                    transformed.insert(sidx, f"allocate({tmp}(size({base_txt})))\n")
                    transformed.insert(sidx + 1, copyin + "\n")
                    sidx += 2
                    eidx += 2
                else:
                    if decl not in declared:
                        transformed.insert(decl_insert, decl + "\n")
                        decl_insert += 1
                        declared.add(decl)
                    transformed.insert(sidx, copyin + "\n")
                    sidx += 1
                    eidx += 1
                import re
                pat_arr = meta.get("patt_arr")
                pat_all = meta.get("patt_all")
                for i in range(sidx, eidx + 1):
                    line = transformed[i]
                    if pat_arr:
                        line = pat_arr.sub(f"{tmp}(", line)
                    if pat_all:
                        line = pat_all.sub(tmp, line)
                    transformed[i] = line
                if meta.get("write", False):
                    copyouts.append(f"{base_txt} = {tmp}\n")
                    if is_array:
                        copyouts.append(f"deallocate({tmp})\n")
                name_map[base_txt] = tmp
            for k, v in name_map.items():
                clauses = clauses.replace(k, v)
        local_style = style
        local_collapse = None
        if collapse == "auto":
            if loop.parent_start_text is None:
                local_collapse = loop.nest_depth if loop.nest_depth and loop.nest_depth > 1 else None
        else:
            try:
                ci = int(collapse)
                local_collapse = ci if ci > 1 else None
            except Exception:
                local_collapse = None
        opts = TransformOptions(style=local_style, schedule=schedule, collapse=local_collapse)
        # sidx/eidx 已在上方计算并随插入更新
        skip = False
        for ps, pe in protected:
            if ps is not None and pe is not None and sidx >= ps and eidx <= pe:
                skip = True
                break
        if skip:
            continue
        do_insert = ar.is_parallel and not loop.has_complex_index
        if do_insert:
            transformed = insert_openmp_directives(transformed, loop.start_text, loop.end_text, clauses, options=opts, nest_depth=loop.nest_depth, nth=seen[key])
            nsidx, neidx = find_loop_range_nth(transformed, loop.start_text, loop.end_text, seen[key])
            from .transform import sanitize_omp_directives
            transformed = sanitize_omp_directives(transformed, nsidx, neidx)
            nsidx, neidx = find_loop_range_nth(transformed, loop.start_text, loop.end_text, seen[key])
            # 插入 copy-out 到 'omp end parallel do' 之后，确保串行写回
            if copyouts:
                insert_after = None
                for j in range(neidx + 1, min(len(transformed), neidx + 8)):
                    if transformed[j].strip().lower().startswith("!$omp end parallel do") or transformed[j].strip().lower().startswith("!$omp end do"):
                        insert_after = j
                        break
                if insert_after is None:
                    insert_after = neidx + 1
                for co in copyouts:
                    insert_after += 1
                    transformed.insert(insert_after, co)
            if local_collapse and nsidx is not None and neidx is not None:
                protected.append((nsidx, neidx))
            applied += 1
        else:
            # 不并行时仍执行成员写回：紧随 END DO 后插入
            if copyouts:
                insert_after = eidx + 1
                for co in copyouts:
                    transformed.insert(insert_after, co)
                    insert_after += 1
    with open(output, "w") as f:
        f.writelines(transformed)
    return f"Applied OpenMP to {applied} loop(s). Output: {output}"


def main():
    """Entry point for CLI invocation."""
    p = argparse.ArgumentParser(description="Fortran auto-parallel checker and transformer (fparser-based)")
    p.add_argument("input", help="input Fortran file (.f90)")
    p.add_argument("--report", action="store_true", help="only run analysis and print report")
    p.add_argument("--no-trivial", action="store_true", help="hide trivial (=, zero-distance) dependencies in report")
    p.add_argument("--analyze-derived", action="store_true", help="enable derived-type member analysis and transforms")
    p.add_argument("--apply", action="store_true", help="apply OpenMP transformation")
    p.add_argument("--apply-ast", action="store_true", help="apply OpenMP transformation via AST editing (experimental)")
    p.add_argument("--output", default=None, help="output file when applying transformation")
    p.add_argument("--omp-style", choices=["parallel_do", "do", "parallel_region"], default="parallel_do")
    p.add_argument("--schedule", default="static")
    p.add_argument("--collapse", default="auto", help="auto or integer >=2")
    args = p.parse_args()
    if args.report or not args.apply:
        print(analyze_file(args.input, no_trivial=args.no_trivial))
    if args.apply_ast:
        out = args.output or (args.input + ".omp_ast.f90")
        print(transform_file_ast(args.input, out, style=args.omp_style, schedule=args.schedule, collapse=args.collapse, analyze_derived=args.analyze_derived))
    if args.apply:
        out = args.output or (args.input + ".omp.f90")
        print(transform_file_line_fixed(args.input, out, style=args.omp_style, schedule=args.schedule, collapse=args.collapse, analyze_derived=args.analyze_derived))


if __name__ == "__main__":
    main()