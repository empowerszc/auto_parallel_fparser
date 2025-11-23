MODULE m_call_rewrite_demo
  ! 示例模块：演示循环内调用改写（有参/无参调用）
  IMPLICIT NONE
  TYPE :: TInner
    ! 派生类型，包含标量成员与可分配数组成员
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
  END TYPE TInner
  TYPE :: TObj
    ! 外层对象，嵌套 TInner
    TYPE(TInner) :: in
  END TYPE TObj
  TYPE(TObj) :: x_global
  CONTAINS
  SUBROUTINE callee(obj, i, a)
    ! 有参被调过程：内部修改 obj%in%arr(i) 与 obj%in%s
    IMPLICIT NONE
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: a
    IF (ALLOCATED(obj % in % arr)) THEN
      obj % in % arr(i) = obj % in % arr(i) + a * REAL(i)
    END IF
    obj % in % s = obj % in % s + a
  END SUBROUTINE callee

    SUBROUTINE callee0
    ! 无参被调过程：修改模块全局对象 x_global 的派生成员
    IMPLICIT NONE
    INTEGER :: i
    IF (ALLOCATED(x_global % in % arr)) THEN
      i = 1
      x_global % in % arr(i) = x_global % in % arr(i) + 1.0
    END IF
    x_global % in % s = x_global % in % s + 1.0
  END SUBROUTINE callee0

    SUBROUTINE driver(n)
    ! 驱动子程序：循环内调用 callee 与 callee0
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(TObj) :: x
    INTEGER :: i
    ALLOCATE(x % in % arr(n))
    x % in % s = 0.0
    x_global % in % s = 0.0
    ALLOCATE(x_global % in % arr(n))
    !$omp parallel do private(i) schedule(static)
    DO i = 1, n
      CALL callee(x, i, 0.1)
      CALL callee0
    END DO
    !$omp end parallel do
  END SUBROUTINE driver
END MODULE m_call_rewrite_demo