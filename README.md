# auto_parallel_fparser

基于 fparser2 的 Fortran 代码自动并行分析与改写工具。提供循环依赖分析、OpenMP 指令插入（行级与 AST 两种模式），并针对派生类型成员进行安全改写以兼容 OpenMP 子句；在开发模式下支持对循环体过程调用的复制改写（*_apf）以隔离派生成员写入。

## 核心能力

- 依赖分析：识别流/输出/反依赖，距离向量与方向向量计算，函数纯度判定
- 指令插入：支持 `parallel do` / `do` / `parallel region` 风格，支持 `schedule` 与 `collapse`
- 两种改写模式：
  - 行级改写：按文本定位插入指令，具备重复指令去重与派生成员临时变量策略
  - AST 改写：在语法树上插入指令与改写成员，保持格式与结构
- 派生类型成员处理：引入同类型临时变量，循环前 `copy-in`，循环体内替换，循环后 `copy-out`（数组成员使用 `allocatable + allocate/deallocate`）
- 类型推断：除扫描声明语句外，支持解析自定义 `TYPE` 定义体，识别 `REAL/INTEGER/DOUBLE PRECISION/LOGICAL/COMPLEX`
- 开发模式（dev 分支）：可选 `--rewrite-calls`，复制循环体内被调用过程为 *_apf 版本，追加临时参数并仅改写 LHS 写入（AST 级），以允许并行插入和成员写回

## 快速开始

```bash
python -m apf.cli examples/derived_vars.f90 --report
python -m apf.cli --apply --output examples/derived_vars.f90.omp.f90 examples/derived_vars.f90
python -m apf.cli --apply-ast --output examples/derived_vars.f90.omp_ast.f90 examples/derived_vars.f90
python -m apf.cli examples/derived_calls_update.f90 --apply-ast --analyze-derived --rewrite-calls --output examples/derived_calls_update.omp_ast.f90
```

常用参数：

- `--report` 仅分析打印报告
- `--apply` 行级改写（支持 `--analyze-derived`）
- `--apply-ast` AST 改写（支持 `--analyze-derived`）
- `--rewrite-calls` 在 AST 改写路径下启用对过程调用的复制改写（dev 分支），仅改写过程内部 LHS 派生成员写入，并插入临时参数
- `--omp-style` 插入风格：`parallel_do` / `do` / `parallel_region`
- `--schedule` 调度策略（默认 `static`）
- `--collapse` 折叠层数或 `auto`

## 设计要点

- 行级与 AST 路径的策略对齐：相同的派生成员临时变量策略与子句替换
- 对已有 OMP 指令进行窗口去重，保持唯一开始/结束指令与顺序合理性
- 类型推断：优先声明语句，其次解析 `Derived_Type_Def`；失败时回退 `REAL`
- 规范区声明与执行区分离：临时变量与追加形参声明统一插入 `Specification_Part`；`allocate/copy-in` 在循环前执行区，`copy-out/deallocate` 在循环后

## 目录结构

- `apf/cli.py` 命令行入口、行级与 AST 改写流程
- `apf/transform.py` 指令插入、成员改写与辅助工具
- `apf/dependence.py` 依赖分析与并行性判定
- `apf/symbols.py` 声明属性收集与常量收集
- `examples/` 示例代码（包含复杂分支与调用的 `derived_calls_update.f90`）

## 已知限制

- 极端复杂的表达式与过程调用的嵌套参数替换可能需要更强 AST 操作与覆盖（`--rewrite-calls` 仍为开发特性，当前仅改写 LHS 写入）
- `--rewrite-calls` 暂仅在 AST 路径生效，行级路径不启用；复制过程的序列化失败将被保护处理并记录告警