# 设计思路

## 目标
- 为 Fortran DO 循环进行自动并行分析与 OpenMP 改写
- 在派生类型成员场景下保证语义安全与编译兼容

## 总体架构
- 解析：`fparser2` 生成语法树与循环 IR
- 分析：依赖、纯度与归约识别，输出并行性与子句建议
- 改写：行级与 AST 两条路径，保持策略一致

## 关键策略
- 派生成员临时变量：
  - 标量：`basetype :: tmp`，循环前 `copy-in`，循环后 `copy-out`
  - 数组：`basetype, allocatable :: tmp(:)`，循环前 `allocate + copy-in`，循环后 `copy-out + deallocate`
  - 替换范围：LHS 与 RHS，嵌套块内赋值也替换
  - 子句替换：将派生成员名替换为临时变量名（如 `reduction(+:obj % sum_val)` → `reduction(+:apf_tmp_sum_val)`）
- 指令插入与去重：
  - 一对唯一的开始/结束指令；结束指令紧随 `END DO`
  - 在 DO 前后窗口内跨越临时声明/赋值/写回，清理冗余指令行

## 失败回退
- 类型推断失败时使用 `REAL`
- AST 插入失败时记录告警并继续其他循环

## 扩展方向
- 更强类型推断（解析 `TYPE` 定义体）与过程纯度自动识别
- RHS 参数列表中的复杂表达式替换与 AST 全面覆盖