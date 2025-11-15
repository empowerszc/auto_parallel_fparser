import argparse
from typing import List

from .parser import parse_file
from .dependence import extract_loop_ir, analyze_loop
from .transform import insert_openmp_directives, build_omp_clauses, TransformOptions


def analyze_file(path: str) -> str:
    tree = parse_file(path, ignore_comments=False)
    loops = extract_loop_ir(tree)
    report_lines: List[str] = []
    for idx, loop in enumerate(loops):
        ar = analyze_loop(loop)
        report_lines.append(f"Loop {idx+1}: {loop.start_text}")
        for d in ar.dependences:
            report_lines.append(
                f"  dep {d.array}: distance={d.distance_vector}, direction={d.direction_vector}, carried_by={d.carried_by}"
            )
        if ar.is_parallel:
            report_lines.append("  parallelizable: yes")
            if ar.reduction_vars:
                report_lines.append(f"  reductions: {ar.reduction_vars}")
        else:
            report_lines.append(f"  parallelizable: no, reason: {ar.reason}")
    return "\n".join(report_lines) + ("\n" if report_lines else "")


def transform_file(path: str, output: str, style: str = "parallel_do", schedule: str = "static", collapse: str = "auto") -> str:
    tree = parse_file(path, ignore_comments=False)
    loops = extract_loop_ir(tree)
    with open(path, "r") as f:
        lines = f.readlines()
    transformed = lines[:]
    applied = 0
    protected = []
    # 构建父子关系映射
    loop_by_start = {l.start_text: l for l in loops}
    children = {}
    for l in loops:
        if l.parent_start_text:
            children.setdefault(l.parent_start_text, []).append(l)
    for loop in loops:
        ar = analyze_loop(loop)
        if ar.is_parallel:
            clauses = build_omp_clauses(ar)
            # 自适应并行策略
            local_style = style
            local_collapse = None
            if collapse == "auto":
                if loop.nest_depth and loop.nest_depth > 1:
                    cs = children.get(loop.start_text, [])
                    all_inner_parallel = True
                    any_inner_reduction = False
                    for c in cs:
                        car = analyze_loop(c)
                        if not car.is_parallel:
                            all_inner_parallel = False
                        if car.reduction_vars:
                            any_inner_reduction = True
                    if all_inner_parallel:
                        local_style = "parallel_do"
                        local_collapse = loop.nest_depth
                    else:
                        local_style = "parallel_region"
                        local_collapse = None
                else:
                    local_collapse = None
            else:
                try:
                    ci = int(collapse)
                    local_collapse = ci if ci > 1 else None
                except Exception:
                    local_collapse = None
            opts = TransformOptions(style=local_style, schedule=schedule, collapse=local_collapse)
            from .transform import find_loop_range
            sidx, eidx = find_loop_range(transformed, loop.start_text, loop.end_text)
            if sidx is None or eidx is None:
                continue
            skip = False
            for ps, pe in protected:
                if ps is not None and pe is not None and sidx >= ps and eidx <= pe:
                    skip = True
                    break
            if skip:
                continue
            transformed = insert_openmp_directives(transformed, loop.start_text, loop.end_text, clauses, options=opts, nest_depth=loop.nest_depth)
            # 变换后根据AST文本重新定位保护区间，避免 collapse 外层后内层重复并行
            nsidx, neidx = find_loop_range(transformed, loop.start_text, loop.end_text)
            if local_collapse and nsidx is not None and neidx is not None:
                protected.append((nsidx, neidx))
            applied += 1
    with open(output, "w") as f:
        f.writelines(transformed)
    return f"Applied OpenMP to {applied} loop(s). Output: {output}"


def main():
    p = argparse.ArgumentParser(description="Fortran auto-parallel checker and transformer (fparser-based)")
    p.add_argument("input", help="input Fortran file (.f90)")
    p.add_argument("--report", action="store_true", help="only run analysis and print report")
    p.add_argument("--apply", action="store_true", help="apply OpenMP transformation")
    p.add_argument("--output", default=None, help="output file when applying transformation")
    p.add_argument("--omp-style", choices=["parallel_do", "do", "parallel_region"], default="parallel_do")
    p.add_argument("--schedule", default="static")
    p.add_argument("--collapse", default="auto", help="auto or integer >=2")
    args = p.parse_args()
    if args.report or not args.apply:
        print(analyze_file(args.input))
    if args.apply:
        out = args.output or (args.input + ".omp.f90")
        print(transform_file(args.input, out, style=args.omp_style, schedule=args.schedule, collapse=args.collapse))


if __name__ == "__main__":
    main()