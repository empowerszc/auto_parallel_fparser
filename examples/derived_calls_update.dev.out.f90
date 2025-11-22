MODULE m_calls_update
  IMPLICIT NONE
  TYPE :: TInner
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
  END TYPE TInner
  TYPE :: TObj
    TYPE(TInner) :: in
    REAL :: t
  END TYPE TObj
  CONTAINS
  SUBROUTINE upd1(obj, i, alpha)
    IMPLICIT NONE
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: alpha
    IF (obj % in % s > 0.0) THEN
      obj % in % s = obj % in % s + alpha
    ELSE
      obj % in % s = obj % in % s - alpha
    END IF
    IF (ALLOCATED(obj % in % arr)) THEN
      IF (MOD(i, 2) == 0) THEN
        obj % in % arr(i) = obj % in % arr(i) + alpha * REAL(i)
      ELSE
        obj % in % arr(i) = obj % in % arr(i) - alpha * REAL(i)
      END IF
    END IF
    obj % t = obj % t + 1.0
  END SUBROUTINE upd1

    SUBROUTINE upd2(obj, i, beta)
    IMPLICIT NONE
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: beta
    IF (beta > 1.0) THEN
      obj % in % s = obj % in % s + beta
    END IF
    IF (ALLOCATED(obj % in % arr)) THEN
      obj % in % arr(i) = obj % in % arr(i) + beta
    END IF
    obj % t = obj % t + beta
  END SUBROUTINE upd2

    SUBROUTINE driver(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(TObj) :: x
    INTEGER :: i
    ALLOCATE(x % in % arr(n))
    x % in % s = 0.0
    x % t = 0.0
    !$omp parallel do private(i) schedule(static)
    DO i = 1, n
      IF (i < n / 2) THEN
        CALL upd1(x, i, 0.5)
      ELSE
        CALL upd2(x, i, 1.5)
      END IF
    END DO
    !$omp end parallel do
  END SUBROUTINE driver
END MODULE m_calls_update