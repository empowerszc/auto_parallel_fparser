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
    INTEGER :: apf_tmp_i(SIZE(obj % data_arr(i)))
    apf_tmp_i = obj % data_arr(i)
    !$omp parallel do private(i) lastprivate(apf_tmp_i) schedule(static)
    !$omp parallel do private(i) lastprivate(apf_tmp_i) schedule(static)
      !$omp parallel do private(i) schedule(static)
      ! 初始化原始数据数组（1到n的平方）
      DO i = 1, n
      apf_tmp_i = REAL(i ** 2)
    END DO
    !$omp end parallel do
    !$omp end parallel do
    !$omp end parallel do
    obj % data_arr(i) = apf_tmp_i
    obj % sum_val = 0.0
    ! 归约变量初始化
    obj % max_val = - HUGE(0.0)
    ! 初始化为极小值
    obj % prod_arr = 1.0
    ! 乘积归约初始化（乘法单位元）
  END SUBROUTINE init_obj

    ! 对自定义类型成员执行归约计算和依赖赋值
    SUBROUTINE process_obj(obj)
    TYPE(MyType), INTENT(INOUT) :: obj
    INTEGER :: i, n
    TYPE(MyType) :: temp_obj
    ! 临时对象，用于跨对象赋值

    n = SIZE(obj % data_arr)
    ALLOCATE(temp_obj % data_arr(n), temp_obj % prod_arr(n))
    temp_obj % sum_val = 0.0
    temp_obj % max_val = - HUGE(0.0)
    REAL :: apf_tmp_sum_val
    IF (obj % data_arr(i) > 5.0) THEN
      ! 条件筛选
      apf_tmp_sum_val_2 = apf_tmp_sum_val_2 + obj % data_arr(i)
      ! 归约：输出依赖（i-1 → i）
    END IF
    REAL :: apf_tmp_max_val
    IF (obj % data_arr(i) > obj % max_val) THEN
      apf_tmp_max_val_2 = obj % data_arr(i)
      ! 归约：输出依赖（i-1 → i，条件性）
    END IF
    INTEGER :: apf_tmp_i(SIZE(temp_obj % data_arr(i)))
    apf_tmp_i = temp_obj % data_arr(i)
    !$omp parallel do private(i) lastprivate(apf_tmp_i) schedule(static)
    !$omp parallel do private(i) lastprivate(apf_tmp_i) schedule(static)
      !$omp parallel do private(i) schedule(static)

      ! 4. 依赖赋值：基于当前对象成员给临时对象赋值（跨对象依赖）
      !    流依赖：temp_obj 成员依赖 obj 成员
      DO i = 1, n
      apf_tmp_i = obj % prod_arr(i) + obj % sum_val
      ! 流依赖（obj → temp_obj）
    END DO
    !$omp end parallel do
    !$omp end parallel do
    !$omp end parallel do
    temp_obj % data_arr(i) = apf_tmp_i
    REAL, ALLOCATABLE :: apf_tmp_data_arr(:)
    ALLOCATE(apf_tmp_data_arr(SIZE(obj % data_arr)))
    apf_tmp_data_arr = obj % data_arr
    INTEGER, ALLOCATABLE :: apf_tmp_i(:)
    ALLOCATE(apf_tmp_i(SIZE(apf_tmp_data_arr % i)))
    apf_tmp_i = apf_tmp_data_arr % i

      ! 5. 自依赖赋值：当前对象数组元素依赖自身前序元素（含步长2的跨迭代依赖）
      DO i = 3, n, 2
      ! 步长为2：i=3,5,7...
      apf_tmp_data_arr(i) = apf_tmp_data_arr(i - 2) * 0.5
      ! 流依赖（i-2 → i）
    END DO
    apf_tmp_data_arr % i = apf_tmp_i
    DEALLOCATE(apf_tmp_i)
    obj % data_arr = apf_tmp_data_arr
    DEALLOCATE(apf_tmp_data_arr)
    INTEGER :: apf_tmp_i_2(SIZE(obj % prod_arr(i)))
    apf_tmp_i_2 = obj % prod_arr(i)
    !$omp parallel do private(i) lastprivate(apf_tmp_i_2) schedule(static)
    !$omp parallel do private(i) lastprivate(apf_tmp_i_2) schedule(static)
      !$omp parallel do private(i) schedule(static)

      ! 6. 输出依赖：同一元素被多次写入（覆盖赋值）
      DO i = 1, n
      apf_tmp_i_2 = 2.0 * obj % data_arr(i)
      ! 输出依赖（与步骤2的prod_arr(i)冲突）
    END DO
    !$omp end parallel do
    !$omp end parallel do
    !$omp end parallel do
    obj % prod_arr(i) = apf_tmp_i_2

    DEALLOCATE(temp_obj % data_arr, temp_obj % prod_arr)
  END SUBROUTINE process_obj

END MODULE custom_type_mod