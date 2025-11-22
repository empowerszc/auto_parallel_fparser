MODULE m_calls_update
  IMPLICIT NONE
  TYPE :: TInner
    REAL :: s
    REAL, ALLOCATABLE :: arr(:)
    INTEGER :: k
    DOUBLE PRECISION :: d
  END TYPE TInner
  TYPE :: TObj
    TYPE(TInner) :: in
    REAL :: t
    REAL, ALLOCATABLE :: buf(:)
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
      obj % in % d = obj % in % d + DBLE(beta)
    ELSE IF (beta == 1.0) THEN
      obj % in % s = obj % in % s - beta
    ELSE
      obj % in % k = obj % in % k + i
    END IF
    IF (ALLOCATED(obj % in % arr)) THEN
      obj % in % arr(i) = obj % in % arr(i) + beta
    END IF
    obj % t = obj % t + beta
  END SUBROUTINE upd2

    SUBROUTINE upd3(obj, i, gamma)
    IMPLICIT NONE
    REAL, ALLOCATABLE :: apf_tmp_buf(:)
    REAL, ALLOCATABLE :: apf_tmp_arr(:)
    REAL :: apf_tmp_s
    INTEGER :: apf_tmp_k
    TYPE(TObj), INTENT(INOUT) :: obj
    INTEGER, INTENT(IN) :: i
    REAL, INTENT(IN) :: gamma
    INTEGER :: j
    SELECT CASE (MOD(i, 4))
      CASE (0)
      IF (ALLOCATED(obj % buf)) THEN
        apf_tmp_buf(i) = gamma * REAL(i + j)
      END IF
      CASE (1)
      IF (ALLOCATED(obj % in % arr)) THEN
        apf_tmp_arr(i) = apf_tmp_arr(i) + gamma
      END IF
      CASE (2)
      apf_tmp_s = apf_tmp_s + gamma
      CASE DEFAULT
      apf_tmp_k = apf_tmp_k + j
    END SELECT
  END SUBROUTINE upd3

    SUBROUTINE driver(n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    TYPE(TObj) :: x
    INTEGER :: i, j
    ALLOCATE(x % in % arr(n))
    ALLOCATE(x % buf(n))
    x % in % s = 0.0
    x % in % k = 0
    x % in % d = 0.0D0
    x % t = 0.0
    !$omp parallel
    !$omp do private(i, j) schedule(static)
    DO i = 1, n
      CALL upd1(x, i, 0.5)
      IF (i < n / 3) THEN
        CALL upd2(x, i, 1.5)
      ELSE IF (i < 2 * n / 3) THEN
        CALL upd3(x, i, 2.0)
      ELSE
        CALL upd2(x, i, 1.0)
        CALL upd3(x, i, 1.0)
      END IF
      !$omp parallel do private(j) schedule(static)
      DO j = 1, 2
        IF (MOD(i + j, 2) == 0) THEN
          CALL upd1(x, i, REAL(j))
        ELSE
          CALL upd2(x, i, REAL(j))
        END IF
      END DO
      !$omp end parallel do
      SELECT CASE (MOD(i, 3))
      CASE (0)
        CALL upd3(x, i, 0.25)
      CASE (1)
        CALL upd1(x, i, 0.25)
      CASE DEFAULT
        CALL upd2(x, i, 0.25)
      END SELECT
    END DO
    !$omp end do
    !$omp end parallel
  END SUBROUTINE driver
END MODULE m_calls_update