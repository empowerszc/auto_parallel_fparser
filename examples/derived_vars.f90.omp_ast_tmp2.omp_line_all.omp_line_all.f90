MODULE custom_type_mod
  IMPLICIT NONE

  ! 定义自定义类型：包含标量（用于归约结果）和数组（用于数据存储）
  TYPE :: MyType
    REAL :: sum_val
    ! 标量：用于求和归约
    REAL :: max_val
    ! 标量：用于最大值归约
    REAL, ALLOCATABLE :: data_arr(:)
    ! 数组：原始数据
    REAL, ALLOCATABLE :: prod_arr(:)
    ! 数组：用于乘积归约
  END TYPE MyType

  CONTAINS

    ! 初始化自定义类型对象：分配数组并设置初始值
    SUBROUTINE init_obj(obj, n)
    TYPE(MyType), INTENT(OUT) :: obj
    INTEGER, INTENT(IN) :: n
    INTEGER :: i
    ALLOCATE(obj % data_arr(n), obj % prod_arr(n))
    REAL :: apf_tmp_data_arr(SIZE(obj % data_arr))
    apf_tmp_data_arr = obj % data_arr
    !$omp parallel do private(i) schedule(static)
    ! 初始化原始数据数组（1到n的平方）
!$omp parallel do private(i) schedule(static)
      DO i = 1, n
      apf_tmp_data_arr(i) = REAL(i ** 2)
    END DO
    !$omp end parallel do
    obj % data_arr = apf_tmp_data_arr
    obj % sum_val = 0.0
    ! 归约变量初始化
    obj % max_val = - HUGE(0.0)
    ! 初始化为极小值
    obj % prod_arr = 1.0
    ! 乘积归约初始化（乘法单位元）
  END SUBROUTINE init_obj

    ! 对自定义类型成员执行归约计算和依赖赋值
    SUBROUTINE process_obj(obj)
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_prod_arr
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_prod_arr
REAL :: apf_tmp_sum_val
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_max_val
REAL :: apf_tmp_prod_arr
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_data_arr
REAL :: apf_tmp_sum_val
    TYPE(MyType), INTENT(INOUT) :: obj
    INTEGER :: i, n
    TYPE(MyType) :: temp_obj
    ! 临时对象，用于跨对象赋值

    n = SIZE(obj % data_arr)
    ALLOCATE(temp_obj % data_arr(n), temp_obj % prod_arr(n))
    temp_obj % sum_val = 0.0
    temp_obj % max_val = - HUGE(0.0)
    REAL :: apf_tmp_sum_val
    apf_tmp_sum_val = obj % sum_val
    !$omp parallel do private(i) reduction(+:apf_tmp_sum_val) schedule(static)

      ! 1. 标量归约：求和（仅累加正数，条件归约）
apf_tmp_sum_val = obj % sum_val
apf_tmp_data_arr = obj % data_arr
      !    sum_val 是归约变量（多次写入，输出依赖）
!$omp parallel do private(i) reduction(+:apf_tmp_sum_val) schedule(static)
      DO i = 1, n
      IF (apf_tmp_data_arr(i) > 5.0) THEN
        ! 条件筛选
        apf_tmp_sum_val = apf_tmp_sum_val + apf_tmp_data_arr(i)
        ! 归约：输出依赖（i-1 → i）
      END IF
    END DO
    !$omp end parallel do
obj % data_arr = apf_tmp_data_arr
obj % sum_val = apf_tmp_sum_val
    obj % sum_val = apf_tmp_sum_val

      ! 2. 数组归约：乘积（prod_arr(i) 是前i个元素的乘积）
apf_tmp_data_arr = obj % data_arr
apf_tmp_prod_arr = obj % prod_arr
apf_tmp_prod_arr = obj % prod_arr
      !    流依赖：prod_arr(i) 依赖 prod_arr(i-1)
      DO i = 1, n
      IF (i == 1) THEN
        apf_tmp_prod_arr(i) = apf_tmp_data_arr(i)
        ! 初始值
      ELSE
        apf_tmp_prod_arr(i) = apf_tmp_prod_arr(i - 1) * apf_tmp_data_arr(i)
        ! 流依赖（i-1 → i）
apf_tmp_prod_arr = apf_tmp_prod_arr
      END IF
obj % prod_arr = apf_tmp_prod_arr
    END DO
    REAL :: apf_tmp_max_val
    apf_tmp_max_val = obj % max_val
    !$omp parallel do private(i) schedule(static)

      ! 3. 标量归约：最大值（跟踪全局最大值）
apf_tmp_max_val = obj % max_val
apf_tmp_data_arr = obj % data_arr
      !    输出依赖：max_val 可能被多次更新
!$omp parallel do private(i) schedule(static)
      DO i = 1, n
      IF (apf_tmp_data_arr(i) > apf_tmp_max_val) THEN
        apf_tmp_max_val = apf_tmp_data_arr(i)
        ! 归约：输出依赖（i-1 → i，条件性）
      END IF
    END DO
    !$omp end parallel do
obj % data_arr = apf_tmp_data_arr
obj % max_val = apf_tmp_max_val
    obj % max_val = apf_tmp_max_val
    REAL :: apf_tmp_data_arr(SIZE(temp_obj % data_arr))
    apf_tmp_data_arr = temp_obj % data_arr

      ! 4. 依赖赋值：基于当前对象成员给临时对象赋值（跨对象依赖）
apf_tmp_sum_val = obj % sum_val
apf_tmp_prod_arr = obj % prod_arr
      !    流依赖：temp_obj 成员依赖 obj 成员
!$omp parallel do private(i) schedule(static)
      DO i = 1, n
      apf_tmp_data_arr(i) = apf_tmp_prod_arr(i) + apf_tmp_sum_val
      ! 流依赖（obj → temp_obj）
    END DO
    !$omp end parallel do
    temp_obj % data_arr = apf_tmp_data_arr

apf_tmp_data_arr = obj % data_arr
apf_tmp_data_arr = obj % data_arr
      ! 5. 自依赖赋值：当前对象数组元素依赖自身前序元素（含步长2的跨迭代依赖）
      DO i = 3, n, 2
      ! 步长为2：i=3,5,7...
      apf_tmp_data_arr(i) = apf_tmp_data_arr(i - 2) * 0.5
      ! 流依赖（i-2 → i）
apf_tmp_data_arr = apf_tmp_data_arr
obj % data_arr = apf_tmp_data_arr
    END DO
    REAL :: apf_tmp_prod_arr(SIZE(obj % prod_arr))
    apf_tmp_prod_arr = obj % prod_arr
    !$omp parallel do private(i) schedule(static)

apf_tmp_data_arr = obj % data_arr
      ! 6. 输出依赖：同一元素被多次写入（覆盖赋值）
!$omp parallel do private(i) schedule(static)
      DO i = 1, n
      apf_tmp_prod_arr(i) = 2.0 * apf_tmp_data_arr(i)
      ! 输出依赖（与步骤2的prod_arr(i)冲突）
    END DO
    !$omp end parallel do
    obj % prod_arr = apf_tmp_prod_arr

    DEALLOCATE(temp_obj % data_arr, temp_obj % prod_arr)
  END SUBROUTINE process_obj

END MODULE custom_type_mod
