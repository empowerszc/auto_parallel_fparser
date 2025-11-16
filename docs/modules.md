# 模块介绍

## apf/cli.py
- 提供命令行入口与两种改写模式：`--apply`（行级）与 `--apply-ast`（AST）
- 行级改写流程：
  - 计算并行性与子句
  - 派生成员临时变量策略（声明位置、allocate/copy-in、循环体替换、end 后 copy-out + deallocate）
  - 插入 OpenMP 指令与窗口去重

## apf/transform.py
- 指令插入（行级与 AST）：构造 `Directive` 节点或文本插入
- `rewrite_derived_members_to_temps`：AST 路径下的派生成员改写
- `detect_derived_members_in_lines`：行级路径的派生成员链识别（嵌套与数组）
- `sanitize_omp_directives`：清理重复 OMP 指令块，确保唯一且顺序正确
- `_infer_component_type`：成员的基本类型推断

## apf/dependence.py
- 从 `LoopIR` 抽取读写集与索引表达式，计算距离/方向向量
- 并行性判定：复杂索引、未知方向、函数纯度检查
- 归约变量识别（支持派生成员名）

## apf/symbols.py
- 收集形参属性与 `VALUE` 属性、常量传播辅助