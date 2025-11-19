# 模块介绍

## apf/cli.py
- 提供命令行入口与两种改写模式：`--apply`（行级）与 `--apply-ast`（AST）
- 行级改写流程：
  - 计算并行性与子句
  - 派生成员临时变量策略（声明位置、allocate/copy-in、循环体替换、end 后 copy-out + deallocate）
  - 插入 OpenMP 指令与窗口去重
- 开发开关：`--rewrite-calls`（dev 分支）用于启用 AST 路径下的调用复制改写，仅改写过程内部 LHS 派生成员写入

## apf/transform.py
- 指令插入（行级与 AST）：构造 `Directive` 节点或文本插入
- `rewrite_derived_members_to_temps`：AST 路径下的派生成员改写
- `rewrite_calls_with_temps`：在循环所在过程声明临时变量，复制被调用过程为 *_apf 并追加参数，循环后写回与释放
- `_detect_derived_writes_in_subprogram`：仅识别赋值语句左值为派生成员的链（支持数组索引判定）
- `_duplicate_subprogram_with_args`：复制过程，插入规范区参数声明（唯一命名，类型与数组形态推断），并进行 AST 级左值替换
- `_replace_lhs_with_args_in_subprogram`：在复制过程 AST 中替换赋值左值的派生成员链为追加参数名
- `_find_enclosing_subprogram` / `_find_spec_part`：定位过程以及其 `Specification_Part`，用于声明插入
- `_unique_name`：基于 `Name` 集合的唯一名生成，用于 `apf_arg_*` 与 `apf_tmp_*`
- `detect_derived_members_in_lines`：行级路径的派生成员链识别（嵌套与数组）
- `sanitize_omp_directives`：清理重复 OMP 指令块，确保唯一且顺序正确
- `_infer_component_type`：成员的基本类型推断（支持解析 `Derived_Type_Def` 文本）

## apf/dependence.py
- 从 `LoopIR` 抽取读写集与索引表达式，计算距离/方向向量
- 并行性判定：复杂索引、未知方向、函数纯度检查
- 归约变量识别（支持派生成员名）

## apf/symbols.py
- 收集形参属性与 `VALUE` 属性、常量传播辅助