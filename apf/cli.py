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
    return transform_file_line_fixed(path, output, style=style, schedule=schedule, collapse=collapse)


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
    # 1) 先处理派生类型原子更新（保持原有逻辑）
    if analyze_derived:
        from .transform import find_loop_range_nth
        seen = {}
        for loop in loops:
            key = loop.start_text
            seen[key] = seen.get(key, 0) + 1
            with open(path, "r") as f:
                lines = f.readlines()
            sidx, eidx = find_loop_range_nth(lines, loop.start_text, loop.end_text, seen[key])
            if sidx is None or eidx is None:
                continue
            for i in range(sidx, eidx + 1):
                low = lines[i].lower()
                if re.search(r"[a-z_]\w*%[a-z_]\w*\s*=\s*[a-z_]\w*%[a-z_]\w*\s*\+", low):
                    prev = lines[i-1].strip().lower() if i > 0 else ""
                    if not prev.startswith("!$omp atomic"):
                        lines.insert(i, "!$omp atomic update\n")
                        eidx += 1
                        applied += 1
                    break
    # 2) 对可并行循环在 AST 上插入 OpenMP：保持与行级逻辑一致的 collapse/样式选择
    for loop in loops:
        ar = analyze_loop(loop)
        if not ar.is_parallel or loop.has_complex_index:
            continue
        clauses = build_omp_clauses(ar)
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


def transform_file_line_fixed(path: str, output: str, style: str = "parallel_do", schedule: str = "static", collapse: str = "auto") -> str:
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
        if not ar.is_parallel or loop.has_complex_index:
            continue
        clauses = build_omp_clauses(ar)
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
        key = loop.start_text
        seen[key] = seen.get(key, 0) + 1
        sidx, eidx = find_loop_range_nth(transformed, loop.start_text, loop.end_text, seen[key])
        if sidx is None or eidx is None:
            continue
        skip = False
        for ps, pe in protected:
            if ps is not None and pe is not None and sidx >= ps and eidx <= pe:
                skip = True
                break
        if skip:
            continue
        transformed = insert_openmp_directives(transformed, loop.start_text, loop.end_text, clauses, options=opts, nest_depth=loop.nest_depth, nth=seen[key])
        nsidx, neidx = find_loop_range_nth(transformed, loop.start_text, loop.end_text, seen[key])
        if local_collapse and nsidx is not None and neidx is not None:
            protected.append((nsidx, neidx))
        applied += 1
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
        print(transform_file_line_fixed(args.input, out, style=args.omp_style, schedule=args.schedule, collapse=args.collapse))


if __name__ == "__main__":
    main()