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
    for idx, loop in enumerate(loops):
        ar = analyze_loop(loop)
        from .transform import find_loop_range
        try:
            with open(path, "r") as f:
                lines = f.readlines()
            sidx, eidx = find_loop_range(lines, loop.start_text, loop.end_text)
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
    """Apply OpenMP directives to loops deemed parallel, optionally transforming derived-type patterns.

    Args:
        path: Input Fortran file.
        output: Output file path to write transformed source.
        style: OpenMP insertion style ('parallel_do', 'do', 'parallel_region').
        schedule: Schedule clause value.
        collapse: 'auto' or integer ≥2 for collapse on nested loops.
        analyze_derived: Enable pattern-based transforms for derived-type member updates.

    Returns:
        Summary string with applied loop count and output path.
    """
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
        if analyze_derived:
            try:
                from .transform import find_loop_range
                sidx, eidx = find_loop_range(transformed, loop.start_text, loop.end_text)
                if sidx is not None and eidx is not None:
                    lv_match = re.search(r"DO\s+([A-Za-z_]\w*)\s*=", loop.start_text, re.IGNORECASE)
                    lv = lv_match.group(1) if lv_match else "i"
                    inserted = False
                    for i in range(sidx, eidx+1):
                        line = transformed[i]
                        low = line.lower()
                        if re.search(rf"[A-Za-z_]\w*%[A-Za-z_]\w*\s*=\s*[A-Za-z_]\w*%[A-Za-z_]\w*\s*\+\s*[A-Za-z_]\w*\s*\(\s*{lv}\s*\)", low):
                            transformed[i] = re.sub(r"([A-Za-z_]\w*%[A-Za-z_]\w*)\s*=\s*\1\s*\+\s*([A-Za-z_]\w*)\s*\(\s*([A-Za-z_]\w*)\s*\)", r"sum_temp = sum_temp + \2(\3)", line)
                            transformed.insert(sidx, "!$omp parallel do reduction(+:sum_temp)\n")
                            transformed.insert(eidx+2, "!$omp end parallel do\n")
                            transformed.insert(eidx+3, "obj%sum_val = sum_temp\n")
                            inserted = True
                            break
                        if re.search(rf"[A-Za-z_]\w*%[A-Za-z_]\w*\s*=\s*[A-Za-z_]\w*\s*\(\s*{lv}\s*\)", low):
                            ubm = re.search(r"DO\s+[A-Za-z_]\w*\s*=\s*[^,]+,\s*([^,\s]+)", loop.start_text, re.IGNORECASE)
                            ub = ubm.group(1) if ubm else "n"
                            transformed[i] = re.sub(r"([A-Za-z_]\w*%[A-Za-z_]\w*)\s*=\s*([A-Za-z_]\w*)\s*\(\s*([A-Za-z_]\w*)\s*\)", r"array_temp(\3) = \2(\3)", line)
                            transformed.insert(sidx, f"allocate(array_temp({ub}) )\n")
                            transformed.insert(sidx+1, "!$omp parallel do lastprivate(array_temp)\n")
                            transformed.insert(eidx+2, "!$omp end parallel do\n")
                            transformed.insert(eidx+3, "obj%array = array_temp\n")
                            transformed.insert(eidx+4, "deallocate(array_temp)\n")
                            inserted = True
                            break
            except Exception:
                pass
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
    """Entry point for CLI invocation."""
    p = argparse.ArgumentParser(description="Fortran auto-parallel checker and transformer (fparser-based)")
    p.add_argument("input", help="input Fortran file (.f90)")
    p.add_argument("--report", action="store_true", help="only run analysis and print report")
    p.add_argument("--no-trivial", action="store_true", help="hide trivial (=, zero-distance) dependencies in report")
    p.add_argument("--analyze-derived", action="store_true", help="enable derived-type member analysis and transforms")
    p.add_argument("--apply", action="store_true", help="apply OpenMP transformation")
    p.add_argument("--output", default=None, help="output file when applying transformation")
    p.add_argument("--omp-style", choices=["parallel_do", "do", "parallel_region"], default="parallel_do")
    p.add_argument("--schedule", default="static")
    p.add_argument("--collapse", default="auto", help="auto or integer >=2")
    args = p.parse_args()
    if args.report or not args.apply:
        print(analyze_file(args.input, no_trivial=args.no_trivial))
    if args.apply:
        out = args.output or (args.input + ".omp.f90")
        print(transform_file(args.input, out, style=args.omp_style, schedule=args.schedule, collapse=args.collapse, analyze_derived=args.analyze_derived))


if __name__ == "__main__":
    main()