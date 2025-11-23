MODULE m_call_rewrite_demo
  IMPLICIT NONE
  TYPE :: TInner
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
  END TYPE TInner
  TYPE :: TObj
    TYPE(TInner) :: in
  END TYPE TObj
  CONTAINS
  SUBROUTINE callee(obj, i, a)
    IMPLICIT NONE
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: a
    IF (ALLOCATED(obj % in % arr)) THEN
      obj % in % arr(i) = obj % in % arr(i) + a * REAL(i)
    END IF
    obj % in % s = obj % in % s + a
  END SUBROUTINE callee

    SUBROUTINE driver(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(TObj) :: x
    INTEGER :: i
    ALLOCATE(x % in % arr(n))
    x % in % s = 0.0
    !$omp parallel do private(i) schedule(static)
    DO i = 1, n
      CALL callee(x, i, 0.1)
    END DO
    !$omp end parallel do
  END SUBROUTINE driver
END MODULE m_call_rewrite_demo