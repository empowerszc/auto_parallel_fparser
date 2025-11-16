# auto_parallel_fparser

基于 fparser2 的 Fortran 代码自动并行分析与改写工具。提供循环依赖分析、OpenMP 指令插入（行级与 AST 两种模式），并针对派生类型成员进行安全改写以兼容 OpenMP 子句。

## 核心能力

- 依赖分析：识别流/输出/反依赖，距离向量与方向向量计算，函数纯度判定
- 指令插入：支持 `parallel do` / `do` / `parallel region` 风格，支持 `schedule` 与 `collapse`
- 两种改写模式：
  - 行级改写：按文本定位插入指令，具备重复指令去重与派生成员临时变量策略
  - AST 改写：在语法树上插入指令与改写成员，保持格式与结构
- 派生类型成员处理：引入同类型临时变量，循环前 `copy-in`，循环体内替换，循环后 `copy-out`（数组成员使用 `allocatable + allocate/deallocate`）

## 快速开始

```bash
python -m apf.cli examples/derived_vars.f90 --report
python -m apf.cli --apply --output examples/derived_vars.f90.omp.f90 examples/derived_vars.f90
python -m apf.cli --apply-ast --output examples/derived_vars.f90.omp_ast.f90 examples/derived_vars.f90
```

常用参数：

- `--report` 仅分析打印报告
- `--apply` 行级改写（支持 `--analyze-derived`）
- `--apply-ast` AST 改写（支持 `--analyze-derived`）
- `--omp-style` 插入风格：`parallel_do` / `do` / `parallel_region`
- `--schedule` 调度策略（默认 `static`）
- `--collapse` 折叠层数或 `auto`

## 设计要点

- 行级与 AST 路径的策略对齐：相同的派生成员临时变量策略与子句替换
- 对已有 OMP 指令进行窗口去重，保持唯一开始/结束指令与顺序合理性
- 类型推断尽可能从声明中获取基本类型，失败时回退 `REAL`

## 目录结构

- `apf/cli.py` 命令行入口、行级与 AST 改写流程
- `apf/transform.py` 指令插入、成员改写与辅助工具
- `apf/dependence.py` 依赖分析与并行性判定
- `apf/symbols.py` 声明属性收集与常量收集
- `examples/` 示例代码

## 已知限制

- 极端复杂的表达式与过程调用的嵌套参数替换可能需要更强正则与 AST 路径覆盖
- 类型推断未解析自定义 `TYPE` 定义体，仅基于 `Type_Declaration_Stmt`